//import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker_2.13:0.1.0-SNAPSHOT`

import com.github.distributedclocks.tracechecker._

import java.util.Base64

// traits with that caputre certain fields
sealed trait ClientIdOp {
  val clientId: String
}

sealed trait FailoverOp {
  val serverId: Int
}

sealed trait GIdOp {
  val gId: Long
}

sealed trait Req {
  val clientId: String
  val opId: Long
}

sealed trait Res {
  val opId: Long
  val gId: Long
}

object GIdOrdering extends Ordering[GIdOp] {
  override def compare(x: GIdOp, y: GIdOp): Int = x.gId.compareTo(y.gId)
}

sealed abstract class Record extends Element
// server-related actions
final case class ServerStart(serverId: Int) extends Record
final case class ServerJoining(serverId: Int) extends Record
final case class NextServerJoining(nextServerId: Int) extends Record
final case class NewJoinedSuccessor(nextServerId: Int) extends Record
final case class ServerJoined(serverId: Int) extends Record
final case class ServerFailRecvd(failedServerId: Int) extends Record
final case class NewFailoverSuccessor(newNextServerId: Int) extends Record with FailoverOp {
  override val serverId = newNextServerId
}
final case class NewFailoverPredecessor(newPrevServerId: Int) extends Record with FailoverOp {
  override val serverId = newPrevServerId
}
final case class ServerFailHandled(failedServerId: Int) extends Record
final case class PutRecvd(clientId: String, opId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutOrdered(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutFwd(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutFwdRecvd(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class PutResult(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp
final case class GetRecvd(clientId: String, opId: Long, key: String) extends Record with ClientIdOp
final case class GetOrdered(clientId: String, opId: Long, gId: Long, key: String) extends Record with ClientIdOp
final case class GetResult(clientId: String, opId: Long, gId: Long, key: String, value: String) extends Record with ClientIdOp

// coord-related actions
final case class CoordStart() extends Record
final case class ServerFail(serverId: Int) extends Record
final case class ServerFailHandledRecvd(failedServerId: Int, adjacentServerId: Int) extends Record
final case class NewChain(chain: String) extends Record
final case class AllServersJoined() extends Record
final case class HeadReqRecvd(clientId: String) extends Record with ClientIdOp
final case class HeadRes(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class TailReqRecvd(clientId: String) extends Record with ClientIdOp
final case class TailRes(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class ServerJoiningRecvd(serverId: Int) extends Record
final case class ServerJoinedRecvd(serverId: Int) extends Record

// kvslib-related actions
final case class KvslibStart(clientId: String) extends Record with ClientIdOp
final case class KvslibStop(clientId: String) extends Record with ClientIdOp
final case class HeadReq(clientId: String) extends Record with ClientIdOp
final case class HeadResRecvd(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class TailReq(clientId: String) extends Record with ClientIdOp
final case class TailResRecvd(clientId: String, serverId: Int) extends Record with ClientIdOp
final case class Put(clientId: String, opId: Long, key: String, value: String) extends Record with ClientIdOp with Req
final case class PutResultRecvd(opId: Long, gId: Long, key: String) extends Record with GIdOp with Res
final case class Get(clientId: String, opId: Long, key: String) extends Record with ClientIdOp with Req
final case class GetResultRecvd(opId: Long, gId: Long, key: String, value: String) extends Record with Res

def decodeChain(chain: String): Array[Byte] =
  Base64.getDecoder.decode(chain)

def chainPp(chain: String): List[Int] =
  decodeChain(chain).map(b => b.toInt).toList

def chainContains(chain: String, serverId: Int): Boolean = chainPp(chain).contains(serverId)

// This is a spec implementation with more sanity checks
// based on the testing scenarios we are using for A3 grading:
// 1. There must be some Get requests
// 2. There must be some Put requests
// 3. There must be some KvslibStart
// 4. There must be some KvslibStop
// 5. There must be HeadReq and HeadRes
// 6. There must be TailReq and TailRes
// 7. If there are some server failures detected, then they must be handled
class Spec(N: Int) extends Specification[Record] {
  import Specification._

  val orderedTraces: Query[Map[String,List[Record]]] = {
    materialize {
      call(traces).map { traces =>
        traces.foldLeft(Map.empty: Map[String, List[Record]]){ (m, trace) =>
          m + (trace._1 -> trace._2.sorted(Element.VectorClockOrdering))
        }
      }
    }
  }

  val kvslibStarts: Query[List[KvslibStart]] =
    materialize {
      elements.map(_.collect{ case a: KvslibStart => a })
        .requireSome
    }

  val kvslibStops: Query[List[KvslibStop]] =
    materialize {
      elements.map(_.collect{ case a: KvslibStop => a })
        .requireSome
    }

  val puts: Query[List[Put]] =
    materialize {
      elements.map(_.collect{ case a: Put => a })
        .requireSome
    }

  val gets: Query[List[Get]] =
    materialize {
      elements.map(_.collect{ case a: Get => a })
        .requireSome
    }

  val theCoordStart: Query[CoordStart] =
    elements.map(_.collect{ case a: CoordStart => a })
      .requireOne

  // FIXME: should we require only one?
  val allServersJoined: Query[List[AllServersJoined]] =
    elements.map(_.collect{ case a: AllServersJoined => a})

  val serverStart: Query[List[ServerStart]] =
    materialize{ elements.map(_.collect{ case a: ServerStart => a }).requireSome }

  val serverJoining: Query[List[ServerJoining]] =
    materialize{ elements.map(_.collect{ case a: ServerJoining => a }).requireSome }

  val opsWithClientId: Query[List[Record with ClientIdOp]] =
    materialize{ elements.map(_.collect{ case a: ClientIdOp => a }) }

  val serverJoiningRecvd: Query[List[ServerJoiningRecvd]] =
    materialize{ elements.map(_.collect{ case a: ServerJoiningRecvd => a }) }

  val nextServerJoining: Query[List[NextServerJoining]] =
    materialize{ elements.map(_.collect{ case a: NextServerJoining => a }) }

  val newJoinedSuccessor: Query[List[NewJoinedSuccessor]] =
    materialize{ elements.map(_.collect{ case a: NewJoinedSuccessor => a }) }

  val serverJoined: Query[List[ServerJoined]] =
    materialize{ elements.map(_.collect{ case a: ServerJoined => a }) }

  val serverJoinedRecvd: Query[List[ServerJoinedRecvd]] =
    materialize{ elements.map(_.collect{ case a: ServerJoinedRecvd => a }) }

  val newChain: Query[List[NewChain]] =
    materialize{ elements.map(_.collect{ case a: NewChain => a }).requireSome }

  val putRecvd: Query[List[PutRecvd]] =
    materialize{ elements.map(_.collect{ case a: PutRecvd => a }) }

  val getRecvd: Query[List[GetRecvd]] =
    materialize{ elements.map(_.collect{ case a: GetRecvd => a }) }

  val serverFail: Query[List[ServerFail]] =
    materialize{ elements.map(_.collect{ case a: ServerFail => a }) }

  val serverFailRecvd: Query[List[ServerFailRecvd]] =
    materialize{ elements.map(_.collect{ case a: ServerFailRecvd => a }) }

  val failover: Query[List[Record with FailoverOp]] =
    materialize{ elements.map(_.collect{ case a: FailoverOp => a }) }

  val serverFailHandled: Query[List[ServerFailHandled]] =
    materialize{ elements.map(_.collect({ case a: ServerFailHandled => a })) }

  val serverFailHandledRecvd: Query[List[ServerFailHandledRecvd]] =
    materialize{ elements.map(_.collect({ case a: ServerFailHandledRecvd => a })) }

  val headReq: Query[List[HeadReq]] =
    materialize{ elements.map(_.collect({ case a: HeadReq => a })).requireSome }

  val headReqRecvd: Query[List[HeadReqRecvd]] =
    materialize{ elements.map(_.collect({ case a: HeadReqRecvd => a })) }

  val headRes: Query[List[HeadRes]] =
    materialize{ elements.map(_.collect({ case a: HeadRes => a })).requireSome }

  val headResRecvd: Query[List[HeadResRecvd]] =
    materialize{ elements.map(_.collect({ case a: HeadResRecvd => a })) }

  val tailReq: Query[List[TailReq]] =
    materialize{ elements.map(_.collect({ case a: TailReq => a })).requireSome }

  val tailReqRecvd: Query[List[TailReqRecvd]] =
    materialize{ elements.map(_.collect({ case a: TailReqRecvd => a })) }

  val tailRes: Query[List[TailRes]] =
    materialize{ elements.map(_.collect({ case a: TailRes => a })).requireSome }

  val tailResRecvd: Query[List[TailResRecvd]] =
    materialize{ elements.map(_.collect({ case a: TailResRecvd => a })) }

  val putResultRecvd: Query[List[PutResultRecvd]] =
    materialize{ elements.map(_.collect({ case a: PutResultRecvd => a })) }

  val getResultRecvd: Query[List[GetResultRecvd]] =
    materialize{ elements.map(_.collect({ case a: GetResultRecvd => a })) }

  def requireTraceType[T](trace: List[Record]): Query[Unit] = {
    val idx = trace.indexWhere(_.isInstanceOf[T])
    if (idx == -1) {
      accept
    } else {
      reject(s"Action ${trace(idx)} is in the wrong trace")
    }
  }

  def sanityCheck(): Query[Unit] =
    for {
      failed <- serverFail.map(_.nonEmpty)
      handled <- serverFailHandledRecvd.map(_.nonEmpty)
      _ <- if (failed != handled) {
          reject("Not all failed server are handled, or fcheck has reported false positives")
        } else {
          accept
        }
    } yield ()

  override def rootRule: RootRule = RootRule(
    multiRule("[10] Initialization", pointValue = 10)(
      rule("[2] KvslibStart exists and happens before KvslibStop/HeadReq/TailReq/Put/Get", pointValue = 2) {
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
            _ <- call(headReq).quantifying("HeadReq").forall {
              case hreq if kstart.clientId == hreq.clientId =>
                if (kstart <-< hreq) {
                  accept
                } else {
                  reject("KvslibStart doesn't happen before HeadReq")
                }
            }
            _ <- call(tailReq).quantifying("TailReq").forall {
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

      rule("[2] CoordStart recorded exactly once and happens before ServerJoiningRecvd and AllServersJoined", pointValue = 2) {
        for {
          cstart <- call(theCoordStart).label("The CoordStart")
          _ <- call(serverJoiningRecvd).label("ServerJoiningRecvd")
            .require(sjr => s"ServerJoiningRecvd should happen after CoordStart: $sjr") { _.forall(cstart <-< _) }
          _ <- call(allServersJoined).label("AllServerJoined")
            .require(sjr => s"ServerJoiningRecvd should happen after CoordStart: $sjr") { _.forall(cstart <-< _) }
        } yield ()
      },

      rule("[3] Exactly N SeverStart", pointValue = 3) {
        call(serverStart).label("ServerStart")
          .require(ss => s"There must be exactly N ServerStart actions, $ss") { _.size == N }
      },

      rule("[3] ServerStart happens before ServerJoining", pointValue = 3) {
        call(serverStart).quantifying("ServerStart").forall { ss =>
          call(serverJoining).quantifying("ServerJoining").forall {
            case sj if ss.serverId == sj.serverId =>
              if (ss <-< sj) {
                accept
              } else {
                reject("ServerJoining does not happen after ServerStart")
              }
          }
        }
      }
    ),

    multiRule("[10] Termination", pointValue = 10)(
      rule("[10] KvslibStop(C) cannot be followed by any actions recorded by C", pointValue = 10) {
        call(kvslibStops.requireSome).quantifying("KvslibStop(C)").forall { kstop =>
          call(elements).quantifying("Action recorded by C ")
            .forall {
              case elem if elem.tracerIdentity == kstop.tracerIdentity && elem != kstop =>
                if (elem <-< kstop) {
                  accept
                } else {
                  reject("The action recorded by C does not happens before KvslibStop")
                }
            }
        }
      }
    ),

    multiRule("[20] Join Handling", pointValue = 20)(
      rule("[2] Exactly one ServerJoining for each serverId", pointValue = 2) {
        call(serverJoining)
          .require(sjs => s"No duplicated serverId in ServerJoining actions: $sjs") { sjs =>
            sjs.forall { sj =>
              sjs.count(_.serverId == sj.serverId) == 1
            }
          }
      },
      rule("[10] ServerJoining behaves correctly", pointValue = 10) {
        call(serverJoining).quantifying("ServerJoining").forall{ sj =>
          for {
            _ <- call(serverJoiningRecvd)
              .map(_.collect{ case a if (a.serverId == sj.serverId) && (sj <-< a) => a }).requireOne
              .label("ServerJoiningRecvd")
            nsj <- call(nextServerJoining)
              .map(_.collect{ case a if (a.nextServerId == sj.serverId) && (sj <-< a) => a }).requireAtMostOne
              .label("NextServerJoining")
            _ <- nsj match {
              case Some(next) => if (next.tracerIdentity != sj.tracerIdentity) {
                accept
              } else {
                reject("NextServerJoining is not recorded by a different tracer")
              }
              case _ => accept
            }
            njs <- call(newJoinedSuccessor)
              .map(_.collect{ case a if (a.nextServerId == sj.serverId) && (sj <-< a) => a }).requireAtMostOne
              .label("NewJoinedSuccessor")
            _ <- njs match {
              case Some(next) => if (next.tracerIdentity != sj.tracerIdentity) {
                accept
              } else {
                reject("NewJoinedSuccessor is not recorded by a different tracer")
              }
              case _ => accept
            }
            _ <- call(serverJoined)
              .map(_.collect{ case a if (a.serverId == sj.serverId) && (sj <-< a) => a })
              .label("ServerJoined")
              .requireOne
            _ <- call(serverJoinedRecvd)
              .map(_.collect{ case a if (a.serverId == sj.serverId) && (sj <-< a) => a })
              .label("ServerJoinedRecvd")
              .requireOne
            _ <- call(newChain).quantifying("NewChains").exists {
              case nc if (sj <-< nc) && chainContains(nc.chain, sj.serverId) => accept
            }
          } yield ()
        }
      },
      rule("[4] ServerJoining eventually followed by AllServersJoined", pointValue = 4) {
        call(serverJoining).quantifying("ServerJoinings").forall{ sj =>
          call(allServersJoined).quantifying("AllServersJoined").exists{ allJoined =>
            if (sj <-< allJoined) {
              accept
            } else {
              reject("No AllServersJoined follows ServerJoining")
            }
          }
        }
      },
      rule("[4] AllServersJoined must exist and happen before PutRecvd/GetRecvd", pointValue = 4) {
        call(allServersJoined).requireSome.quantifying("AllServersJoined").forall{ allJoined =>
          for {
            _ <- call(putRecvd).quantifying("PutRecvd").forall{ pr =>
              if (allJoined <-< pr) accept else reject("AllServersJoined doesn't happen before PutRecvd")
            }
            _ <- call(getRecvd).quantifying("GetRecvd").forall{ gr =>
              if (allJoined <-< gr) accept else reject("AllServersJoined doesn't happen before GetRecvd")
            }
          } yield ()
        }
      }
    ),

    multiRule("[20] Failure Handling", pointValue = 20)(
      rule("[4] ServerFail(S) followed by at most two ServerFailRecvd(S)", pointValue = 4) {
        for {
          _ <- sanityCheck()
          _ <- call(serverFail).quantifying("ServerFail").forall { sf =>
            call(serverFailRecvd).map(_.collect{ case a if sf.serverId == a.failedServerId && sf <-< a => a })
              .require(l => s"ServerFail should only be followed by one or two ServerFailedRecvd, found: $l") { sfr =>
                sfr.size <= 2
              }
          }
        } yield ()
      },
      rule("[4] At most one NewFailoverSuccessor(X) or NewFailoverPredecessor(X) happens between a ServerFailRecvd(S) and the next ServerFailRecvd(S') recorded by the same server, where X!=S, S!=S'", pointValue = 4) {
        for {
          _ <- sanityCheck()
          _ <- call(serverFailRecvd).quantifying("ServerFailRecvd").forall { sfr =>
            for {
              // the next ServerFailRecvd (or None if we already at the last one) recorded by the same server (identified using tracerIdentity)
              nextOpt <- serverFailRecvd.map(_.collectFirst{ case x if x.tracerIdentity == sfr.tracerIdentity && sfr <-< x => x })
              _ <- nextOpt match {
                case Some(next) => call(failover).map(_.collect{
                  case a if sfr.tracerIdentity == a.tracerIdentity &&
                    sfr.failedServerId != a.serverId &&
                    sfr <-< a &&
                    a <-< next
                  => a
                })
                  .label("NewFailoverSuccessor or NewFailoverPredecessor")
                  .requireAtMostOne
                case None => call(failover).map(_.collect{ case a if sfr.tracerIdentity == a.tracerIdentity && sfr.failedServerId != a.serverId && sfr <-< a=> a })
                  .label("NewFailoverSuccessor or NewFailoverPredecessor")
                  .requireAtMostOne
              }
            } yield ()
          }
        } yield ()
      },
      rule("[4] ServerFailRecvd(S) must be followed by at most two ServerFailHandled(S)", pointValue = 4) {
        for {
          _ <- sanityCheck()
          _ <- call(serverFailRecvd).quantifying("ServerFailRecvd").forall { sfr =>
            call(serverFailHandled).map(_.collect{ case a if sfr.failedServerId == a.failedServerId && sfr <-< a => a })
              .label("succeeding ServerFailHanlded")
              .require(_ => "At most two ServerFailHandled(S) happens after ServerFailRecvd(S)") { failHandled =>
                failHandled.size <= 2
              }
          }
        } yield ()
      },
      rule("[4] ServerFailHandledRecvd(S) must be preceded by ServerFailHandled(S)", pointValue = 4) {
        for {
          _ <- sanityCheck()
          _ <- call(serverFailHandledRecvd).quantifying("ServerFailHandledRecvd").forall{ sfhr =>
            call(serverFailHandled).map(_.collect{ case a if sfhr.failedServerId == a.failedServerId && a <-< sfhr => a })
              .label("preceding ServerFailHandled")
              .requireSome
          }
        } yield ()
      },
      rule("[4] ServerFail(S) must be eventually followed by NewChain(C) without S", pointValue = 4) {
        for {
          _ <- sanityCheck()
          _ <- call(serverFail).quantifying("ServerFail").forall { sf =>
            call(newChain).quantifying("NewChain").exists {
              case c if sf <-< c && !chainContains(c.chain, sf.serverId) => accept
            }
          }
        } yield ()
      }
    ),

    multiRule("[10] Join/Failure Handling", pointValue = 10)(
      rule("[10] NewChain must be preceded by either ServerFail or ServerJoined", pointValue = 10) {
        call(newChain).quantifying("NewChain").forall { nc =>
          call(serverFail).map(_.collect{ case a if a <-< nc => a })
            .flatMap{ sfs =>
              if (sfs.isEmpty) {
                call(serverJoined).map(_.collect{ case a if a <-< nc => a }).requireSome
              } else {
                accept
              }
            }
        }
      }
    ),

    multiRule("[20] Head Server Requests", pointValue = 20)(
      rule("[5] The number of HeadReq(C) and HeadReqRecvd(C) must be identical", pointValue = 5) {
        for {
          hrs <- call(headReq).label("HeadReq")
          hrrs <- call(headReqRecvd).label("HeadReqRecvd")
          _ <- if (hrs.size == hrrs.size) accept else reject("Different number of HeadReq and HeadReqRecvd")
        } yield ()
      },
      rule("[5] HeadReq(C) must happen before HeadReqRecvd(C)", pointValue = 5) {
        call(headReq).quantifying("HeadReq").forall { hreq =>
          for {
            recvd <- call(headReqRecvd).map(_.find(x => x.clientId == hreq.clientId && hreq <-< x)).label("HeadReqRecvd")
            _ <- recvd match {
              case Some(r) => accept
              case None => reject("HeadReq does not happen before HeadReqRecvd")
            }
          } yield ()
        }
      },
      rule("[5] The number of HeadRes(C,S) and HeadResRecvd(C,S) must be identical", pointValue = 5) {
        for {
          hrs <- call(headRes).label("HeadRes")
          hrrs <- call(headResRecvd).label("HeadResRecvd")
          _ <- if (hrs.size == hrrs.size) accept else reject("Different number of HeadRes and HeadResRecvd")
        } yield ()
      },
      rule("[5] HeadRes(C,S) must happen before HeadResRecvd(C,S)", pointValue = 5) {
        call(headRes).quantifying("HeadRes").forall { hres =>
          for {
            recvd <- call(headResRecvd).map(_.find(x => x.clientId == hres.clientId && hres <-< x)).label("HeadResRecvd")
            _ <- recvd match {
              case Some(r) => accept
              case None => reject("HeadRes does not happen before HeadResRecvd")
            }
          } yield ()
        }
      }
    ),

    multiRule("[20] Tail Server Requests", pointValue = 20)(
      rule("[5] The number of TailReq(C) and TailReqRecvd(C) must be identical", pointValue = 5) {
        for {
          trs <- call(tailReq).label("all TailReq")
          trrs <- call(tailReqRecvd).label("all TailReqRecvd")
          _ <- if (trs.size == trrs.size) accept else reject("Different number of TailReq and TailReqRecvd")
        } yield ()
      },
      rule("[5] TailReq(C) must happen before TailReqRecvd(C)", pointValue = 5) {
        call(tailReq).quantifying("TailReq").forall { treq =>
          for {
            recvd <- call(tailReqRecvd).map(_.find(x => x.clientId == treq.clientId && treq <-< x)).label("TailReqRecvd")
            _ <- recvd match {
              case Some(r) => accept
              case None => reject("TailReq does not happen before TailReqRecvd")
            }
          } yield ()
        }
      },
      rule("[5] The number of TailRes(C) and TailResRecvd(C) must be identical", pointValue = 5) {
        for {
          trs <- call(tailRes).label("TailRes")
          trrs <- call(tailResRecvd).label("TailResRecvd")
          _ <- if (trs.size == trrs.size) accept else reject("Different number of TailRes and TailResRecvd")
        } yield ()
      },
      rule("[5] TailRes(C) must happen before TailResRecvd(C)", pointValue = 5) {
        call(tailRes).quantifying("TailRes").forall { tres =>
          for {
            recvd <- call(tailResRecvd).map(_.find(x => x.clientId == tres.clientId && tres <-< x)).label("TailResRecvd")
            _ <- recvd match {
              case Some(r) => accept
              case None => reject("TailRes does not happen before TailResRecvd")
            }
          } yield ()
        }
      },
    ),

    multiRule("[30] Put Handling", pointValue = 30)(
      rule("[10] Put(C) must be preceded by HeadResRecvd(C,S)", pointValue = 10) {
        call(puts).quantifying("Put").forall { p =>
          call(headResRecvd).quantifying("headResRecvd").exists { hrr =>
            if (hrr <-< p && hrr.clientId == p.clientId && hrr.tracerIdentity == p.tracerIdentity)
              accept
            else
              reject("No corresponding HeadResRecvd before Put")
          }
        }
      },
      rule("[20] The semantics of Put all recorded in a single Put-Trace", pointValue = 20) {
        call(puts).quantifying("Put").forall { p =>
          val ptrace = orderedTraces.map(_.get(p.traceId).toList).requireOne
          for {
            presRecvd <- ptrace.map(_.collect{ case a: PutResultRecvd if a.tracerIdentity == p.tracerIdentity => a })
              .label("The PutResultRecvd")
              .requireOne
            pOrdered <- ptrace.map(_.collect{ case a: PutOrdered if a <-< presRecvd && presRecvd.gId == a.gId => a }.toList)
              .label("PutOrdered at S")
              .requireOne
            _ <- ptrace.map(_.collect{ case a: PutRecvd if a <-< pOrdered && a.tracerIdentity == pOrdered.tracerIdentity => a })
              .label("PutRecvd")
              .requireSome
            _ <- ptrace.map(_.collect{
              case a: PutFwdRecvd if pOrdered <-< a &&
                a <-< presRecvd &&
                a.tracerIdentity != pOrdered.tracerIdentity &&
                a.gId == presRecvd.gId
              => a })
              .quantifying("PutFwdRecvd").forall { fwdRecvd =>
              ptrace.map(_.collect{ case a: PutFwd if pOrdered <-< a &&
                a <-< fwdRecvd &&
                presRecvd.gId == a.gId &&
                pOrdered.tracerIdentity == a.tracerIdentity => a })
                .label("PutFwd")
                .requireSome
            }
            _ <- ptrace.map(_.collectFirst{ case a: PutResult if a.gId == presRecvd.gId && pOrdered <-< a && a <-< presRecvd => a }.toList)
              .label("PutResult")
              .require(_ => "There should be at least one PutResult with the same gid happens between PutOrdered and PutResultRecvd") { pres =>
                pres.nonEmpty
              }
          } yield ()
        }
      }
    ),

    multiRule("[30] Get Handling", pointValue = 30)(
      rule("[10] Get(C) must be preceded by TailResRecvd(C,S)", pointValue = 10) {
        call(gets).quantifying("Get").forall { g =>
          call(tailResRecvd).quantifying("tailResRecvd").exists { trr =>
            if (trr <-< g && trr.clientId == g.clientId && trr.tracerIdentity == g.tracerIdentity)
              accept
            else
              reject("No corresponding TailResRecvd before Get")
          }
        }
      },
      rule("[20] The semantics of Get all recorded in a single Get-Trace", pointValue = 20) {
        call(gets).quantifying("Get").forall { g =>
          val gtrace = orderedTraces.map(_.get(g.traceId).toList).requireOne
          for {
            gresRecvd <- gtrace.map(_.collect{ case a: GetResultRecvd if a.tracerIdentity == g.tracerIdentity => a })
              .label("The GetResultRecvd")
              .requireOne
            gOrdered <- gtrace.map(_.collectFirst{ case a: GetOrdered if a <-< gresRecvd && gresRecvd.gId == a.gId => a }.toList)
              .label("GetOrdered at S")
              .requireOne
            _ <- gtrace.map(_.collect{ case a: GetRecvd if a <-< gOrdered && a.tracerIdentity == gOrdered.tracerIdentity => a })
              .label("GetRecvd")
              .requireSome
            _ <- gtrace.map(_.collectFirst{ case a: GetResult if a.gId == gresRecvd.gId && gOrdered <-< a && a <-< gresRecvd => a }.toList)
              .label("GetResult")
              .requireOne
          } yield ()
        }
      }
    ),

    multiRule("[25] Put-Get Data Consistency", pointValue = 25)(
      rule("[25] Get must have the same value as its latest preceding Put", pointValue = 25) {
        val putResultRecvdSorted = putResultRecvd.map(_.sorted(GIdOrdering))
        call(puts).quantifying("Put").forall { p =>
          call(putResultRecvdSorted).quantifying("corresponding PutResultRecvd").forall {
            case presRecvd if p.traceId == presRecvd.traceId && p.key == presRecvd.key =>
              call(getResultRecvd).quantifying("corresponding GetResultRecvd").forall {
                case gresRecvd if p.key == gresRecvd.key && presRecvd.gId < gresRecvd.gId =>
                  for {
                    pResultRecvdSorted <- putResultRecvdSorted
                    idx = pResultRecvdSorted.indexOf(presRecvd)
                    nextOpt = pResultRecvdSorted.lift(idx+1)
                    _ = nextOpt match {
                      case Some(next) =>
                        if (gresRecvd.gId < next.gId && gresRecvd.value != p.value)
                          reject("GetResultRecvd doesn't have the same value as its latest preceding Put")
                        else
                          accept
                      case None =>
                        if (gresRecvd.value == p.value)
                          accept
                        else
                          reject("GetResultRecvd doesn't have the same value as its latest preceding Put")
                    }
                  } yield()
              }
          }
        }
      }
    ),

    multiRule("[10] Get Before Any Put Data Consistency", pointValue = 10)(
      rule("[10] Get with no preceding Put should return empty string", pointValue = 10) {
        for {
          _ <- gets
          earliestPutResRecvdOpt <- putResultRecvd.map(_.sorted(GIdOrdering).headOption)
          _ = call(getResultRecvd).quantifying("GetResultRecvd").forall { gresRecvd =>
            earliestPutResRecvdOpt match {
              case Some(earliestPutResRecvd) if gresRecvd.gId < earliestPutResRecvd.gId && gresRecvd.value != "" =>
                reject("GetResultRecvd with not preceding PutResultRecvd has non-empty value")
              case None if gresRecvd.value != "" =>
                reject("GetResultRecvd with not preceding PutResultRecvd has non-empty value")
              case _ => accept
            }
          }
        } yield ()
      }
    ),

    multiRule("[25] OpID-GID Consistency", pointValue = 25)(
      rule("[25] The OpId for each client should have the same ordering as their corresponding GId", pointValue = 25) {
        import scala.collection.mutable
        val res = for {
          preq <- call(puts).label("Put")
          pres <- call(putResultRecvd).label("PutResultRecvd")
          greq <- call(gets).label("Get")
          gres <- call(getResultRecvd).label("GetResultRecvd")
          res = mutable.Map.empty[String, mutable.ListBuffer[Record with Res]]
          _ = preq.foreach { req =>
            val r = pres.find(x => req.traceId == x.traceId)
            if (r.nonEmpty) {
              if (!res.contains(req.clientId)) res += req.clientId -> mutable.ListBuffer(r.get)
              res(req.clientId) += r.get
            }
          }
          _ = greq.foreach { req =>
            val r = gres.find(x => req.traceId == x.traceId)
            if (r.nonEmpty) {
              if (!res.contains(req.clientId)) res += req.clientId -> mutable.ListBuffer(r.get)
              res(req.clientId) += r.get
            }
          }
          _ = println(res)
          resL = res.toList.map(_._2)
        } yield resL
        res.quantifying("PutResultRecvd/GetResultRecvd recorded by the same client").forall { m =>
          var errorPair: mutable.ListBuffer[Record with Res] = null
          val allRes = m.combinations(2).forall { comb =>
            val res = comb.head.opId.compareTo(comb(1).opId) == comb.head.gId.compareTo(comb(1).gId)
            if (!res) errorPair = comb
            res
          }
          if (allRes) accept else reject("find a pair violating OpID-GID consistency", errorPair.toList)
        }
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
  if (traceFiles.isEmpty) {
    println("No trace file provided.")
    sys.exit(0)
  }
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
//  println("Score: " + results.grade)
//  results.dump().foreach(print)
//  val p = new PrintWriter("grade_out.log")
//  try {
//    p.println(results.grade)
//    results.dump().foreach(p.print)
//  } finally {p.close()}
}
