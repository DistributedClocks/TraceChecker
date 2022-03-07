//import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker_2.13:0.1.0-SNAPSHOT`

import com.github.distributedclocks.tracechecker._

import java.io.PrintWriter
import java.util.Base64

// category of traces
sealed trait KTraceAction
sealed trait CTraceAction
sealed trait STraceAction
sealed trait PTraceAction
sealed trait GTraceAction

// category of the chained-kvs components
sealed trait ServerOp
sealed trait KvslibOp
sealed trait CoordOp

// traits with that caputre certain fields
sealed trait ClientIdOp {
  val clientId: String
}

sealed abstract class Record extends Element
// server-related actions
final case class ServerStart(serverId: Int) extends Record with ServerOp with STraceAction
final case class ServerJoining(serverId: Int) extends Record with ServerOp with STraceAction
final case class NextServerJoining(nextServerId: Int) extends Record with ServerOp with STraceAction
final case class NewJoinedSuccessor(nextServerId: Int) extends Record with ServerOp with STraceAction
final case class ServerJoined(serverId: Int) extends Record with ServerOp with STraceAction
final case class ServerFailedRecvd(failedServerId: Int) extends Record with ServerOp with CTraceAction
final case class NewFailoverSuccessor(newNextServerId: Int) extends Record with ServerOp with CTraceAction
final case class NewFailoverPredecessor(newPrevServerId: Int) extends Record with ServerOp with CTraceAction
final case class ServerFailHandled(FailServerId: Int) extends Record with ServerOp with CTraceAction
final case class PutRecvd(clientId: String, opId: Long, key: String, value: String) extends Record with ServerOp with PTraceAction with ClientIdOp
final case class PutOrdered(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ServerOp with PTraceAction with ClientIdOp
final case class PutFwd(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ServerOp with PTraceAction with ClientIdOp
final case class PutFwdRecvd(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ServerOp with PTraceAction with ClientIdOp
final case class PutResult(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ServerOp with PTraceAction with ClientIdOp
final case class GetRecvd(clientId: String, opId: Long, key: String) extends Record with ServerOp with GTraceAction with ClientIdOp
final case class GetOrdered(clientId: String, opId: Long, gId: Long, key: String) extends Record with ServerOp with GTraceAction with ClientIdOp
final case class GetResult(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ServerOp with GTraceAction with ClientIdOp

// coord-related actions
final case class CoordStart() extends Record with CoordOp with CTraceAction
final case class ServerFail(serverId: Int) extends Record with CoordOp with CTraceAction
final case class ServerFailHandledRecvd(failedServerId: Int, adjacentServerId: Int) extends Record with CoordOp with CTraceAction
final case class NewChain(chain: String) extends Record with CoordOp with CTraceAction
final case class AllServersJoined() extends Record with CoordOp with CTraceAction
final case class HeadReqRecvd(clientId: String) extends Record with CoordOp with KTraceAction with ClientIdOp
final case class HeadRes(clientId: String, serverId: Int) extends Record with CoordOp with KTraceAction with ClientIdOp
final case class TailReqRecvd(clientId: String) extends Record with CoordOp with KTraceAction with ClientIdOp
final case class TailRes(clientId: String, serverId: Int) extends Record with CoordOp with KTraceAction with ClientIdOp
final case class ServerJoiningRecvd(serverId: Int) extends Record with CoordOp with STraceAction
final case class ServerJoinedRecvd(serverId: Int) extends Record with CoordOp with STraceAction

// kvslib-related actions
final case class KvslibStart(clientId: String) extends Record with KvslibOp with KTraceAction with ClientIdOp
final case class KvslibStop(clientId: String) extends Record with KvslibOp with KTraceAction with ClientIdOp
final case class HeadReq(clientId: String) extends Record with KvslibOp with KTraceAction with ClientIdOp
final case class HeadResRecvd(clientId: String, serverId: Int) extends Record with KvslibOp with KTraceAction with ClientIdOp
final case class TailReq(clientId: String) extends Record with KvslibOp with KTraceAction with ClientIdOp
final case class TailResRecvd(clientId: String, serverId: Int) extends Record with KvslibOp with KTraceAction with ClientIdOp
final case class Put(clientId: String, opId: Long, key: String, value: String) extends Record with KvslibOp with PTraceAction with ClientIdOp
final case class PutResultRecvd(opId: Long, gId: Long, key: String) extends Record with KvslibOp with PTraceAction
final case class Get(clientId: String, opId: Long, key: String) extends Record with KvslibOp with GTraceAction with ClientIdOp
final case class GetResultRecvd(opId: Long, gId: Long, key: String, value: String) extends Record with KvslibOp with GTraceAction

def decodeChain(chain: String): Array[Byte] =
  Base64.getDecoder.decode(chain)

def chainPp(gameState: String): List[Int] =
  decodeChain(gameState).map(b => b.toInt).toList

class Spec(N: Int) extends Specification[Record] {
  import Specification._

  val orderedTraces: Query[List[(String,List[Record])]] = {
    materialize {
      call(traces).map { traces =>
        traces.map{ trace =>
          (trace._1, trace._2.sorted(Element.VectorClockOrdering))
        }
      }
    }
  }

  val kvslibStarts: Query[List[KvslibStart]] =
    materialize {
      call(elements).map(_.collect{ case a: KvslibStart => a })
    }

  val kvslibStops: Query[List[KvslibStop]] =
    materialize {
      call(elements).map(_.collect{ case a: KvslibStop => a })
    }

  val headReqs: Query[List[HeadReq]] =
    materialize {
      call(elements).map(_.collect{ case a: HeadReq => a })
    }

  val tailReqs: Query[List[TailReq]] =
    materialize {
      call(elements).map(_.collect{ case a: TailReq => a })
    }

  val puts: Query[List[Put]] =
    materialize {
      call(elements).map(_.collect{ case a: Put => a })
    }

  val gets: Query[List[Get]] =
    materialize {
      call(elements).map(_.collect{ case a: Get => a })
    }

  val theCoordStart: Query[CoordStart] =
    call(elements).map(_.collect{ case a: CoordStart => a })
      .requireOne

  // FIXME: should we require only one?
  val allServersJoined: Query[List[AllServersJoined]] =
    call(elements).map(_.collect{ case a: AllServersJoined => a})

  val serverJoiningRecvd: Query[List[ServerJoiningRecvd]] =
    call(elements).map(_.collect{ case a: ServerJoiningRecvd => a })

  val serverStart: Query[List[ServerStart]] =
    materialize{ call(elements).map(_.collect{ case a: ServerStart => a }) }

  val serverJoining: Query[List[ServerJoining]] =
    materialize{ call(elements).map(_.collect{ case a: ServerJoining => a }) }

  val opsWithClientId: Query[List[Record with ClientIdOp]] =
    materialize{ call(elements).map(_.collect{ case a: ClientIdOp => a }) }

  def requireTraceType[T](trace: List[Record]): Query[Unit] = {
    val idx = trace.indexWhere(_.isInstanceOf[T])
    if (idx == -1) {
      accept
    } else {
      reject(s"Action ${trace(idx)} is in the wrong trace")
    }
  }

//  val ktraces: Query[Map[String, List[Record]]] = ???
//    materialize {
//    }

  override def rootRule: RootRule = RootRule(
    multiRule("Initialization", pointValue = 4)(
      rule("KvslibStart exists and happens before KvslibStop/HeadReq/TailReq/Put/Get", pointValue = 1) {
        call(kvslibStarts).quantifying("KvslibStart").forall { kstart =>
          for {
            _ <- call(kvslibStops).quantifying("KvslibStop").forall {
              case kstop if kstart.clientId == kstop.clientId =>
                if (kstart <-< kstop) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before KvslibStop")
                }
            }
            _ <- call(headReqs).quantifying("HeadReq").forall {
              case hreq if kstart.clientId == hreq.clientId =>
                if (kstart <-< hreq) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before HeadReq")
                }
            }
            _ <- call(tailReqs).quantifying("TailReq").forall {
              case treq if kstart.clientId == treq.clientId =>
                if (kstart <-< treq) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before TailReq")
                }
            }
            _ <- call(puts).quantifying("Put").forall {
              case put if kstart.clientId == put.clientId =>
                if (kstart <-< put) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before Put")
                }
            }
            _ <- call(gets).quantifying("Get").forall {
              case get if kstart.clientId == get.clientId =>
                if (kstart <-< get) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before Get")
                }
            }
          } yield ()
        }
      },

      rule("CoordStart recorded exactly once and happens before ServerJoiningRecvd and AllServersJoined", pointValue = 1) {
        for {
          cstart <- call(theCoordStart).label("The CoordStart")
          _ <- call(serverJoiningRecvd).label("ServerJoiningRecvd")
            .require(sjr => s"ServerJoiningRecvd should happen after CoordStart: $sjr") { _.forall(cstart <-< _) }
          _ <- call(allServersJoined).label("AllServerJoined")
            .require(sjr => s"ServerJoiningRecvd should happen after CoordStart: $sjr") { _.forall(cstart <-< _) }
        } yield ()
      },

      rule("Exactly N SeverStart", pointValue = 1) {
        call(serverStart).label("ServerStarts")
          .require(ss => s"There must be exactly N ServerStart actions, $ss") { _.size == N }
      },

      rule("ServerStart happens before ServerJoining", pointValue = 1) {
        call(serverStart).quantifying("ServerStarts").forall { ss =>
          call(serverJoining).quantifying("ServerJoinings").forall { sj =>
            if (ss <-< sj) {
              accept
            } else {
              reject("ServerJoining does not happen after ServerStart")
            }
          }
        }
      }
    ),

    multiRule("Termination", pointValue = 4)(
      rule("KvslibStop cannot followed by any actions by the same client", pointValue = 1) {
        call(kvslibStops).quantifying("KvslibStops").forall { kstop =>
          call(opsWithClientId).quantifying("Actions recorded with the same ClientId as KvslibStop")
            .forall {
              case op if op.clientId == kstop.clientId =>
                if (op <-< kstop) {
                  accept
                } else {
                  reject("Action with the same clientId happens after KvslibStop")
                }
            }
        }
      }
    ),

    multiRule("Join Handling", pointValue = 4)(
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Failure Handling", pointValue = 5)(
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Join/Failure Handling", pointValue = 1)(
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Head Server Requests", pointValue = 4)(
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Tail Server Requests", pointValue = 4)(
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Put Handling", pointValue = 2)(
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Get Handling", pointValue = 2)(
      rule("", pointValue = 1) {
        accept
      },
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Put-Get Data Consistency", pointValue = 1)(
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("Get Before Any Put Data Consistency", pointValue = 1)(
      rule("", pointValue = 1) {
        accept
      }
    ),

    multiRule("OpID-GID Consistency", pointValue = 1)(
      rule("", pointValue = 1) {
        accept
      }
    ),
  )
}

// do not remove this. it is here to force Ammonite to read the code ^^^ and the code vvv separately,
// which it turns out is necessary for the @main bit to actually work (and not disappear silently, making this script a no-op)
@

@main
def a3spec(@arg(doc = "the number of servers on the chain") n: Int,
           @arg(doc = "path to the trace file to analyse. this file will the one you told the tracing server to generate, and should contain exactly one trace") traceFiles: os.Path*): Unit = {
  val spec = new Spec(n)
  val results = spec.checkRules(traceFiles:_*)
  if (results.success) {
    println("all checks passed!")
    println()
    println("summary:")
    results.ruleList().foreach(print)
  } else {
    println(s"some checks failed... approximate grade: ${results.grade}/${spec.rootRule.availablePts}")
    println()
    println("summary:")
    results.ruleList().foreach(print)
    println()
    println("details:")
    results.counterExamples().foreach(print)
  }
  println("Score: " + results.grade)
  results.dump().foreach(print)
  val p = new PrintWriter("grade_out.log")
  try {
    p.println(results.grade)
    results.dump().foreach(p.print)
  } finally {p.close()}
}
