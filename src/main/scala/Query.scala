package com.github.distributedclocks.tracechecker

/**
 * A Query, which either logically succeeds or does not.
 *
 * A Query's result is modeled via Result[T], which should include an explanation
 * of what happened. This explanation would be especially interesting on failure, and
 * should list what data was touched, and which causal relations were referenced.
 *
 * @tparam T the query result type
 */
abstract class Query[+T] { self =>
  /**
   * Perform the query, yielding the Result[T].
   */
  def apply(ctx: QueryContext): Result[T]

  /**
   * If the underlying Query[T] is successful, transform the result via fn.
   * If the underlying Query[T] is unsuccessful, forward that failure.
   */
  final def map[U](fn: T => U): Query[U] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) => Accept(fn(value), ctx)
        case r@Reject(_, _, _, _) => r
      }
    }

  final def asUnit: Query[Unit] =
    self.map(_ => ())

  /**
   * Allows monadic chaining of queries.
   * Produces a query that, if this query succeeds with result t, produces the result of the query fn(t).
   * If this query fails, it will short-circuit any further processing.
   */
  final def flatMap[U](fn: T => Query[U]): Query[U] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) => fn(value)(ctx)
        case r@Reject(_, _, _, _) => r
      }
    }

  /**
   * A short-cut to Queries.label, which labels the result of the current query as name in the query execution context.
   */
  final def label(name: String): Query[T] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) => Accept(value, ctx.withObservation(name, value))
        case r@Reject(_, _, _, _) => r
      }
    }

  /**
   * Assert that fn(t) must hold for any successful result of this query.
   * If fn(t) == true, succeed. Otherwise, fail and include descr(t) as explanation.
   */
  final def require(descr: T=>String)(fn: T => Boolean)(implicit positionInfo: PositionInfo): Query[T] =
    self.flatMap { t =>
      Queries.require(descr(t))(fn(t))
        .map(_ => t)
    }

  /**
   * A shortcut for an expected-common case: a query returns potentially many results, structurally,
   * but we know things are only going well if it returns exactly one.
   *
   * Transforms the underlying query to return the singleton element of T, or fail.
   */
  final def requireOne[U](implicit ev: T <:< Iterable[U], positionInfo: PositionInfo): Query[U] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) =>
          if(value.size == 1) {
            Accept(value.head, ctx)
          } else if(value.size > 1) {
            Reject("more than one value matched; see relevant values", ctx, value.toList, positionInfo = positionInfo)
          } else {
            Reject("no values matched", ctx, Nil, positionInfo = positionInfo)
          }
        case r@Reject(_, _, _, _) => r
      }
    }

  /**
   * Helper for asserting that a query should produce an empty collection.
   * Fails if the collection is non-empty, succeeds with a unit value otherwise.
   */
  final def requireEmpty(implicit ev: T <:< Iterable[Any], positionInfo: PositionInfo): Query[Unit] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) =>
          if (value.isEmpty) {
            Accept((), ctx)
          } else {
            Reject("values were matched that should not have been; see relevant values", ctx, value.toList, positionInfo = positionInfo)
          }
        case r@Reject(_, _, _, _) => r
      }
    }

  /**
   * Helper for asserting that a query returns a non-empty collection.
   * Yields the came collection untouched if it is non-empty, fails otherwise.
   *
   * Lists the collection as a "relevant value".
   */
  final def requireSome(implicit ev: T <:< Iterable[Any], positionInfo: PositionInfo): Query[T] =
    self.flatMap { collection =>
      Query { ctx =>
        if(collection.nonEmpty) {
          Accept(collection, ctx)
        } else {
          Reject("collection should not be empty", ctx, List(collection), positionInfo)
        }
      }
    }

  /**
   * Helper for building logical quantifications out of queries producing collections.
   * Returns a Quantifying builder, which provides methods for building forall or exists quantifications.
   */
  final def quantifying[E](name: String)(implicit ev: T <:< Iterable[E], positionInfo: PositionInfo): Query.Quantifying[T,E] =
    new Query.Quantifying[T,E](name = name, query = self)

  /**
   * Helper for easily invoking CausalRelation.latestPredecessors on Query[CausalRelation].
   */
  final def latestPredecessors[U <: AnyRef](from: Element)(fn: PartialFunction[Element,U])(implicit ev: T <:< CausalRelation): Query[LazyList[U]] =
    self.flatMap(_.latestPredecessors(from)(fn))

  /**
   * Helper for easily invoking CausalRelation.earliestSuccessors on Query[CausalRelation]
   */
  final def earliestSuccessors[U <: AnyRef](from: Element)(fn: PartialFunction[Element,U])(implicit ev: T <:< CausalRelation): Query[LazyList[U]] =
    self.flatMap(_.earliestSuccessors(from)(fn))
}

object Query {
  /**
   * For building custom queries. Most queries are built out of this at some level, look around the source code for examples.
   */
  def apply[T](fn: QueryContext => Result[T]): Query[T] = fn(_)

  final class Quantifying[+T,E](name: String, query: Query[T])(implicit ev: T <:< Iterable[E], positionInfo: PositionInfo) {
    /**
     * Short-cut for invoking Queries.forall.
     */
    def forall(fn: PartialFunction[E,Query[Any]]): Query[Unit] =
      for { data <- query; _ <- Queries.forall(name)(data)(fn) } yield ()

    /**
     * Short-cut for invoking Queries.exists.
     */
    def exists(fn: PartialFunction[E,Query[Any]]): Query[Unit] =
      for { data <- query; _ <- Queries.exists(name)(data)(fn) } yield ()
  }
}
