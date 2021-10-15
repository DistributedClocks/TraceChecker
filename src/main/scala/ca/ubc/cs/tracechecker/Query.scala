package ca.ubc.cs.tracechecker

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

  /**
   * The typical error short-circuiting "bind" operator.
   * Allows chaining of queries
   */
  final def flatMap[U](fn: T => Query[U]): Query[U] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) => fn(value)(ctx)
        case r@Reject(_, _, _, _) => r
      }
    }

  final def label(name: String): Query[T] =
    Query { ctx =>
      self(ctx) match {
        case Accept(value, ctx) => Accept(value, ctx.withObservation(name, value))
        case r@Reject(_, _, _, _) => r
      }
    }

  /**
   * Essentially an assertion. Transform the existing query, applying the predicate fn to any successful result.
   * If fn(t) == true, succeed, including descr in the explanation. Otherwise, fail, also including descr
   * in the explanation.
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
          } else {
            Reject("more than one value matched; see relevant values", ctx, value.toList, positionInfo = positionInfo)
          }
        case r@Reject(_, _, _, _) => r
      }
    }

  final def forall[E](name: String)(implicit ev: T <:< Iterable[E], positionInfo: PositionInfo) =
    new {
      def apply(fn: PartialFunction[E,Query[Any]]): Query[Unit] =
        for { data <- self; _ <- Queries.forall(name)(data)(fn) } yield ()
    }

  final def exists[E](name: String)(fn: PartialFunction[E,Query[Any]])(implicit ev: T <:< Iterable[E], positionInfo: PositionInfo): Query[Unit] =
    for { data <- self; _ <- Queries.exists(name)(data)(fn) } yield ()
}

object Query {
  def apply[T](fn: QueryContext => Result[T]): Query[T] = fn(_)
}
