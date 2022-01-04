package ca.ubc.cs.tracechecker

import ca.ubc.cs.tracechecker.Queries.MaterializeError

trait Queries {
  final val rawElements: Query[List[Element]] =
    Query { ctx =>
      Accept(ctx.state.elements, ctx)
    }

  final val causalRelation: Query[CausalRelation] =
    materialize {
      rawElements.map(CausalRelation(_))
    }

  final def materialize[T](query: Query[T])(implicit positionInfo: PositionInfo): Query[T] =
    Query { ctx =>
      val resultValue = ctx.state.materializedState.getOrElseUpdate(ById(query), {
        query(ctx.withoutEntries) match {
          case Accept(value, _) => value
          case Reject(msg, ctx, relatedValues, innerPositionInfo) =>
            throw MaterializeError(
              msg = msg,
              materializePositionInfo = positionInfo,
              positionInfo = innerPositionInfo,
              ctx = ctx,
              relatedValues = relatedValues)
        }
      })
      Accept(resultValue.asInstanceOf[T], ctx)
    }

  final def label(name: String)(value: Any): Query[Unit] =
    Query { ctx =>
      Accept((), ctx.withObservation(name, value))
    }

  final def group[T](name: String)(query: Query[T]): Query[T] =
    Query { ctx =>
      val outerCtx = ctx
      query(ctx.withoutEntries) match {
        case Accept(value, ctx) => Accept(value, outerCtx)
        case Reject(msg, ctx, relatedValues, positionInfo) =>
          Reject(msg, outerCtx.withGroup(name, ctx), relatedValues, positionInfo = positionInfo)
      }
    }

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

  final def require(msg: =>String)(body: =>Boolean)(implicit positionInfo: PositionInfo): Query[Unit] =
    Query { ctx =>
      if(body) {
        Accept((), ctx)
      } else {
        Reject(msg, ctx, Nil, positionInfo = positionInfo)
      }
    }

  final def forall[T](name: String)(data: Iterable[T])(fn: PartialFunction[T,Query[Any]])(implicit positionInfo: PositionInfo): Query[Unit] =
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

  final def exists[T](name: String)(data: Iterable[T])(fn: PartialFunction[T,Query[Any]])(implicit positionInfo: PositionInfo): Query[Unit] =
    Query { ctx =>
      data.iterator
        .foldLeft(None: Option[Accept[Unit]]) { (acc, t) =>
          acc.orElse {
            fn.unapply(t) match {
              case None => None
              case Some(q) =>
                q(ctx.withObservation(name, t)) match {
                  case Accept(_, ctx) => Some(Accept((), ctx))
                  case Reject(_, _, _, _) => None
                }
            }
          }
        }
        .getOrElse {
          Reject(s"no satisfying assignment exists for $name", ctx, relatedValues = data.toList, positionInfo = positionInfo)
        }
    }
}

object Queries extends Queries {
  final case class MaterializeError(msg: String, materializePositionInfo: PositionInfo, positionInfo: PositionInfo,
                                    ctx: QueryContext, relatedValues: List[Any]) extends RuntimeException(
    s"""materialize at $materializePositionInfo failed:
       |  $msg at $positionInfo.
       |dumping context: ${pprint.apply(ctx)}
       |dumping related values: ${pprint.apply(relatedValues)}""".stripMargin)
}
