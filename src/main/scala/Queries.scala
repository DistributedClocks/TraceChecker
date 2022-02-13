package com.github.distributedclocks.tracechecker

trait Queries {
  /**
   * Provides access to the list of elements provided to a given execution of a specification.
   */
  final val rawElements: Query[List[Element]] =
    Query { ctx =>
      Accept(ctx.state.elements, ctx)
    }

  /**
   * Provides access to a singleton CausalRelation instance, constructed using the result of rawElements and then cached
   */
  final val causalRelation: Query[CausalRelation] =
    materialize {
      rawElements.map(CausalRelation(_))
    }

  /**
   * Ensures the contained query's result is only computed once.
   * Useful if the underlying query is heavyweight but its result is not expected to change, e.g some aggregate property of rawElements.
   *
   * It caches based on object identity of the query, so if you want caching to work, store the entire materialized query
   * and use that query to access the cached result.
   */
  final def materialize[T](query: Query[T])(implicit positionInfo: PositionInfo): Query[T] =
    Query { ctx =>
      val groupName = s"materialize at $positionInfo"
      ctx.state.materializedState.get(ById(query)) match {
        case Some(result) =>
          result match {
            case Accept(value, _) => Accept(value.asInstanceOf[T], ctx)
            case Reject(_, _, _, _) => Reject(s"materialize already failed", ctx, Nil, positionInfo)
          }
        case None =>
          val freshResult = query(ctx.withoutEntries)
          ctx.state.materializedState(ById(query)) = freshResult
          freshResult match {
            case Accept(value, _) => Accept(value, ctx)
            case Reject(msg, innerCtx, relatedValues, innerPositionInfo) =>
              Reject(msg, ctx.withGroup(groupName, innerCtx), relatedValues, innerPositionInfo)
          }
      }
    }

  /**
   * Stores the given value in the query's execution context under the given name.
   * Has no other effect on query execution.
   *
   * When looking at a query failure, labels produced by this query will look like:
   * ``
   * name := <prettyprinted value>
   * ``
   */
  final def label(name: String)(value: Any): Query[Unit] =
    Query { ctx =>
      Accept((), ctx.withObservation(name, value))
    }

  /**
   * Wraps the underlying query such that its results appear as a named subgroup.
   *
   * If the underlying query fails, this will look like:
   * ``
   * subgroup name:
   *   ... <nested labels and subgroups>
   * ``
   */
  final def group[T](name: String)(query: Query[T]): Query[T] =
    Query { ctx =>
      val outerCtx = ctx
      query(ctx.withoutEntries) match {
        case Accept(value, _) => Accept(value, outerCtx)
        case Reject(msg, ctx, relatedValues, positionInfo) =>
          Reject(msg, outerCtx.withGroup(name, ctx), relatedValues, positionInfo = positionInfo)
      }
    }

  /**
   * Wraps the underlying query with group, using the file/line position info as group name.
   *
   * This makes clear when a property uses recursion or helpers. You should use it as: `call(helper(...))`
   */
  final def call[T](query: Query[T])(implicit positionInfo: PositionInfo): Query[T] =
    group(positionInfo.toString)(query)

  /**
   * A Query[Unit] that trivially succeeds.
   */
  object accept extends Query[Unit] {
    /**
     * Optionally, accept may be applied to a custom result result value to produce a Query[T] that trivially succeeds.
     */
    def apply[T](result: =>T): Query[T] =
      Query { ctx =>
        Accept(result, ctx)
      }

    override def apply(ctx: QueryContext): Result[Unit] =
      Accept((), ctx)
  }

  /**
   * A Query[Nothing] that trivially fails with the given message
   * (thus having effectively no result type).
   */
  final def reject(msg: =>String, contextualValues: List[Any] = Nil)(implicit positionInfo: PositionInfo): Query[Nothing] =
    Query { ctx =>
      Reject(msg, ctx, contextualValues, positionInfo = positionInfo)
    }

  /**
   * A query that wraps a boolean condition. If the condition is true, the query succeeds. If the condition is false,
   * the query fails with the provided message.
   */
  final def require(msg: =>String)(body: =>Boolean)(implicit positionInfo: PositionInfo): Query[Unit] =
    Query { ctx =>
      if(body) {
        Accept((), ctx)
      } else {
        Reject(msg, ctx, Nil, positionInfo = positionInfo)
      }
    }

  /**
   * Perform logical forall quantification.
   *
   * It checks fn(elem) for every elem in data that fn accepts.
   * If one element's check fails, it is reported as a counter-example and processing stops.
   *
   * @param name an alias by which to label a piece of data in results
   * @param data the "set" of data over which to quantify
   * @param fn the "condition". Only elements that fn accepts will be considered.
   */
  final def forall[T](name: String)(data: IterableOnce[T])(fn: PartialFunction[T,Query[Any]])(implicit positionInfo: PositionInfo): Query[Unit] =
    Query { ctx =>
      data.iterator
        .foldLeft(None: Option[Reject]) { (acc, t) =>
          acc.orElse {
            fn.unapply(t).flatMap { q =>
              acc match {
                case Some(_) => acc
                case None =>
                  q(ctx.withObservation(name, t)) match {
                    case Accept(_, _) => None
                    case r@Reject(_, _, _, _) => Some(r)
                  }
              }
            }
          }
        }
        .getOrElse(Accept((), ctx))
    }

  /**
   * The dual to forall, performs existential quantification.
   *
   * If an element is accepted by fn and passes fn(elem), it is considered proof by example and processing stops.
   * Otherwise, if all possible elements are exhausted, the query fails.
   */
  final def exists[T](name: String)(data: IterableOnce[T])(fn: PartialFunction[T,Query[Any]])(implicit positionInfo: PositionInfo): Query[Unit] =
    Query { ctx =>
      (data.foldLeft(Left(Nil): Either[List[T],Accept[Unit]]) { (acc, t) =>
        acc match {
          case Left(rejected) =>
            fn.unapply(t) match {
              case None => Left(rejected)
              case Some(q) =>
                val ctxWithObs = ctx.withObservation(name, t)
                q(ctxWithObs) match {
                  case Accept(_, _) => Right(Accept((), ctxWithObs))
                  case Reject(_, _, _, _) => Left(t :: rejected)
                }
            }
          case Right(example) => Right(example)
        }
      }) match {
        case Left(rejected) =>
          Reject(s"no satisfying assignment exists for $name", ctx, relatedValues = rejected.reverse, positionInfo = positionInfo)
        case Right(example) => example
      }
    }
}

object Queries extends Queries
