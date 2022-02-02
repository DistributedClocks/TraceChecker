//import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker_2.13:0.1.0-SNAPSHOT`

import com.github.distributedclocks.tracechecker._

import java.io.PrintWriter
import java.util.Base64

sealed trait StateMoveMessage {
  val gameState: Option[String]
  val moveRow: Int
  val moveCount: Int
  val tracingServerAddr: String
  // FIXME: the type of token may need to change
  val token: String
}

def getGameStateBytes(gameState: String): Array[Byte] =
  Base64.getDecoder.decode(gameState)

object StateMoveMessage {
  def unapply(candidate: Any): Option[(Option[String],Int,Int)] =
    candidate match {
      case stateMoveMessage: StateMoveMessage =>
        Some((stateMoveMessage.gameState, stateMoveMessage.moveRow, stateMoveMessage.moveCount))
      case _ => None
    }
}

sealed abstract class Record extends Element
final case class GameStart(seed: Int) extends Record
final case class ClientMove(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: String) extends Record with StateMoveMessage
final case class ServerMoveReceive(gameState: Option[String], moveRow: Int, moveCount: Int, tracingServerAddr: String, token: String) extends Record with StateMoveMessage
final case class GameComplete(winner: String) extends Record

// new actions in A2
final case class NewNimServer(nimServerAddr: String) extends Record
final case class NimServerFailed(nimServerAddr: String) extends Record
final case class AllNimServersDown() extends Record

class Spec(expectedSeed: String) extends Specification[Record] {
  import Specification._

  val theTrace: Query[List[Record]] =
    traces.requireOne.map(_._2)

  val theTraceInOrder: Query[List[Record]] =
    materialize {
      // in this specific assignment, the vector clocks should form a total order, which we can use to sort them
      call(theTrace).map(_.sorted(Element.VectorClockOrdering))
        .flatMap { trace =>
          // sanity check: under some weird conditions where the tracing library is not used properly (branching),
          // the statement about total order above may not actually be true.
          // let's just check this to be sure, rather than give confusing feedback based on ambiguous information,
          // and hopefully no-one will ever fail this condition
          accept(trace zip trace.tail).quantifying("sequential pair").forall {
            case (before, after) if before <-< after => accept
            case (before, after) =>
              for {
                _ <- label("before")(before)
                _ <- label("after")(after)
                _ <- reject("before should happen-before after, but doesn't. your vector clocks are probably corrupted")
              } yield ()
          }
            .map(_ => trace)
        }
    }

  val theGameStart: Query[GameStart] =
    call(theTrace)
      .map(_.collect { case gs: GameStart => gs })
      .requireOne

  val theFirstClientMove: Query[ClientMove] =
    call(theTraceInOrder)
      .map(_.collectFirst { case cm: ClientMove => cm }.toList)
      .requireOne

  val theFirstServerMove: Query[ServerMoveReceive] =
    call(theTraceInOrder)
      .map(_.collectFirst { case sm: ServerMoveReceive => sm }.toList)
      .requireOne

  val theLastMove: Query[StateMoveMessage] =
    call(theTraceInOrder)
      .map(_.view.collect { case m: StateMoveMessage => m }.lastOption.toList)
      .requireOne

  val duplicatedMsgs: Query[Set[ById[ServerMoveReceive]]] =
    materialize {
      call(theTraceInOrder)
        .map(_.collect {
          case m: ServerMoveReceive => m
        })
        .map(_.foldLeft((Set.empty[ServerMoveReceive], Set.empty[ById[ServerMoveReceive]])){ (tuple, item) =>
          val (seen, dups) = tuple
          val newdup =
            if (seen(item)) {
              dups.incl(ById(item))
            } else {
              dups
            }
          (seen.incl(item), newdup)
        })
        .map(_._2)
    }

  def requireLegalOnReceive(m: StateMoveMessage): Query[Unit] =
    m match {
      case ClientMove(None, -1, seed, _, _) if seed.toString == expectedSeed =>
        accept // always legal on receive. we will try to figure out game begin semantics elsewhere
      case sm: ServerMoveReceive =>
        // we check that ServerMove happened in response to at least _something_ the client sent.
        // while a lot of things can happen due to retries/delays/etc, you can never have a server response without at
        // least one client request.
        causalRelation.latestPredecessors(sm) { case cm: ClientMove => cm }
          .label("latest predecessors")
          .requireOne
          .asUnit
      case cm@ClientMove(Some(gameStateAfterStr), moveRow, moveCount, _, _) if moveRow >= 0 && moveCount >= 0 =>
        // for non-initial client moves, we look back to the "last" ServerMove the client reported seeing to determine
        // if the client's request was reasonable. this avoids all the messy UDP semantics, as well as second-guessing
        // what the server might be seeing (which may include UDP-induced invalid moves!)
        duplicatedMsgs.flatMap { duplicatedMsgs =>
          causalRelation.latestPredecessors(cm) { case sm@ServerMoveReceive(Some(_), _, _, _, _) if !duplicatedMsgs(ById(sm)) => sm }
            .label("latest predecessors")
            .requireOne
            .flatMap {
              case sm@ServerMoveReceive(Some(gameStateBeforeStr), _, _, _, _) =>
                val gameStateAfter = getGameStateBytes(gameStateAfterStr)
                val gameStateBefore = getGameStateBytes(gameStateBeforeStr)
                for {
                  _ <- label("gameStateBefore")(gameStateBefore)
                  _ <- label("gameStateAfter")(gameStateAfter)
                  _ <- if(moveCount == 0) {
                    reject(s"$cm has a move count of 0, which Nim does not allow")
                  } else if(!gameStateBefore.indices.contains(moveRow)) {
                    reject(s"$cm lists a moveRow that does not index into the board in $sm")
                  } else {
                    val nextRowVal = gameStateBefore(moveRow) - moveCount
                    if (nextRowVal < 0) {
                      reject(s"$cm implies a game board with a negative value, relative to $sm")
                    } else {
                      if(gameStateAfter.corresponds(gameStateBefore.view.updated(moveRow, nextRowVal))(_ == _)) {
                        accept
                      } else {
                        reject(s"the game board in $cm is not consistent with the one in $sm, according to Nim rules")
                      }
                    }
                  }
                } yield ()
            }
            .asUnit
        }
      case _ =>
        reject(s"the move did not fit any recognised pattern. maybe it's a checker bug or a corrupt trace?")
    }

  override def rootRule: RootRule = RootRule(
    multiRule("[25%] Distributed tracing works", pointValue = 25)(
      rule("[5%] ServerGameStart is collected", pointValue = 5){reject("")},
      rule("[10%] ClientMoveReceives are collected", pointValue = 10){reject("")},
      rule("[10%] ServerMoves are collected", pointValue = 10){reject("")}
    ),

    multiRule("[25%] Nim server failures are detected by fcheck", pointValue = 25)(
      rule("[10%] If NimServerFailed is recorded, then there's a NewNimServer happens before it with the identical address", pointValue = 10){reject("")},
      rule("[15%] NimServerFailed is recorded when there's a corresponding ServerFailed", pointValue = 15){reject("")}
    ),

    multiRule("[40%] Transparent nim server fail-over works", pointValue = 40)(
      rule("[10%] NewNimServer recorded after NimServerFailed", pointValue = 10){reject("")},
      rule("[15%] For each NimServerFailed, GameResume is collected after the first ClientMoveReceive", pointValue = 15){reject("")},
      rule("[15%] The game progress normally, like A1", pointValue = 15){reject("")}
    ),

    rule("[10%] Nim servers total failure handled properly", pointValue = 10){reject("")}
  )
}

// do not remove this. it is here to force Ammonite to read the code ^^^ and the code vvv separately,
// which it turns out is necessary for the @main bit to actually work (and not disappear silently, making this script a no-op)
@

@main
def a1spec(@arg(doc = "the seed passed to the client which produced the trace being checked, in decimal with no leading zeroes") expectedSeed: String,
           @arg(doc = "path to the trace file to analyse. this file will the one you told the tracing server to generate, and should contain exactly one trace") traceFiles: os.Path*): Unit = {
  val spec = new Spec(expectedSeed = expectedSeed)
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
  val p = new PrintWriter("grade_out.log")
  try {
    p.println(results.grade)
    results.dump().foreach(p.print)
  } finally {p.close()}
}
