package ca.ubc.cs.tracechecker

import org.scalatest.funsuite.AnyFunSuite

sealed abstract class Record extends Element
object Record {
  final case class ServerStart() extends Record
  final case class ServerEcho(kill: Boolean) extends Record
  final case class ServerStop() extends Record

  final case class ClientStart(kill: Boolean, requestCount: Int) extends Record
  final case class ClientSend() extends Record
  final case class ClientReceive() extends Record
  final case class ClientStop() extends Record
}

class SimpleTest extends AnyFunSuite {
  test("try to read log") {
    object ContainsClientStart {
      def unapply(trace: List[Record]): Option[Record.ClientStart] =
        trace.collectFirst { case cs@Record.ClientStart(_, _) => cs }
    }

    object Spec extends Specification[Record] {
      import Specification._

      val rootRule: RootRule = RootRule(
        rule("r1") {
          traces.quantifying("trace").forall {
            case (id, trace@ContainsClientStart(clientStart)) =>
              for {
                _ <- label("id")(id)
                _ <- label("clientStart")(clientStart)
                _ <- accept(trace.collect { case cs@Record.ClientStart(_, _) => cs })
                  .requireOne
                _ <- exists("clientStop")(trace) {
                  case clientStop@Record.ClientStop() =>
                    for {
                      _ <- require("clientStop happens-after clientStart")(clientStart <-< clientStop)
                    } yield ()
                }
              } yield ()
          }
        },
      )
    }

    val results = Spec.checkRules(os.pwd / "src" / "test" / "files" / "echo" / "trace_log.txt")

//    results.ruleList().foreach(System.out.print)
//    println()
//    results.counterExamples().foreach(System.out.print)
//    println()
//    println(s"ok? ${results.success}")
    assert(results.success)
  }
}
