//import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker_2.13:0.1.0-SNAPSHOT`

import com.github.distributedclocks.tracechecker._

import java.io.PrintWriter
import java.util.Base64

sealed trait StateMoveMessage {
  val gameState: Option[String]
  val moveRow: Int
  val moveCount: Int
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
final case class ClientMove(gameState: Option[String], moveRow: Int, moveCount: Int) extends Record with StateMoveMessage
final case class ServerMoveReceive(gameState: Option[String], moveRow: Int, moveCount: Int) extends Record with StateMoveMessage
final case class GameComplete(winner: String) extends Record

class Spec() extends Specification[Record] {
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
      case ClientMove(None, -1, seed) if expectedSeed.contains(seed.toString) =>
        accept // always legal on receive. we will try to figure out game begin semantics elsewhere
      case sm: ServerMoveReceive =>
        // we check that ServerMove happened in response to at least _something_ the client sent.
        // while a lot of things can happen due to retries/delays/etc, you can never have a server response without at
        // least one client request.
        causalRelation.latestPredecessors(sm) { case cm: ClientMove => cm }
          .label("latest predecessors")
          .requireOne
          .asUnit
      case cm@ClientMove(Some(gameStateAfterStr), moveRow, moveCount) if moveRow >= 0 && moveCount >= 0 =>
        // for non-initial client moves, we look back to the "last" ServerMove the client reported seeing to determine
        // if the client's request was reasonable. this avoids all the messy UDP semantics, as well as second-guessing
        // what the server might be seeing (which may include UDP-induced invalid moves!)
        duplicatedMsgs.flatMap { duplicatedMsgs =>
          causalRelation.latestPredecessors(cm) { case sm@ServerMoveReceive(Some(_), _, _) if !duplicatedMsgs(ById(sm)) => sm }
            .label("latest predecessors")
            .requireOne
            .flatMap {
              case sm@ServerMoveReceive(Some(gameStateBeforeStr), _, _) =>
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
    rule("[5%] The client always win", pointValue = 5) {
      for {
        _ <- call(theTrace)
          .map(_.collect { case gc: GameComplete => gc })
          .requireOne
          .require(gc => s"winner should be the client $gc") { gc =>
            gc.winner.toLowerCase == "client"
          }
        _ <- call(theLastMove).label("the last move")
          .require(sm => s"the last move $sm should be a ClientMove with empty board") {
            case ClientMove(Some(board), _, _) if getGameStateBytes(board).forall(_ == 0) => true
            case _ => false
          }
          .flatMap(requireLegalOnReceive)
      } yield ()
    }
  )
}

// do not remove this. it is here to force Ammonite to read the code ^^^ and the code vvv separately,
// which it turns out is necessary for the @main bit to actually work (and not disappear silently, making this script a no-op)
@

@main
def a1spec(@arg(doc = "output file path") out: String,
           @arg(doc = "path to the trace file to analyse. this file will the one you told the tracing server to generate, and should contain exactly one trace") traceFiles: os.Path*): Unit = {
  val spec = new Spec()
  val results = spec.checkRules(traceFiles:_*)
  results.dump().foreach(print)
  val p = new PrintWriter(out)
  try {
    p.println(results.grade)
    results.dump().foreach(p.print)
  } finally {p.close()}
}
