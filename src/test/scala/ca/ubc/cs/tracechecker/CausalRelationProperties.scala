package ca.ubc.cs.tracechecker

import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Properties

class CausalRelationProperties extends AnyFunSuite {
  CausalRelationProperties.properties.foreach {
    case (name, prop) =>
      test(name) {
        import org.scalacheck._
        val result = Test.check(prop)(identity)
        withClue(pprint.apply(result)) {
          assert(result.passed)
        }
      }
  }
}

object CausalRelationProperties extends Properties("CausalRelation") {
  import org.scalacheck._

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(100000)

  final case class ElementsData(countRemaining: Int, possibleNextVectorClocks: List[(String,Boolean,Map[String,Long])], elementsAcc: List[Element]) {
    def result: List[Element] = elementsAcc.reverse

    def withElement(element: Element): ElementsData =
      copy(
        countRemaining = countRemaining - 1,
        elementsAcc = element :: elementsAcc)

    def withNextVCs(options: List[(String,Boolean,Map[String,Long])]): ElementsData =
      copy(possibleNextVectorClocks = options)
  }

  final case class TestElement() extends Element

  private def vcHappensBefore(left: Map[String,Long], right: Map[String,Long]): Boolean =
    (left.keysIterator ++ right.keysIterator).forall { key => right.getOrElse(key, 0L) >= left.getOrElse(key, 0L) } &&
      right.exists { case (key, clock) => clock > left.getOrElse(key, 0L) }

  private def updateVC(tracerId: String, vc: Map[String,Long]): Map[String,Long] =
    vc.updatedWith(tracerId)(_.map(_ + 1).orElse(Some(1)))

  val anyElementList: Gen[List[Element]] =
    for {
      tracerIds <- Gen.nonEmptyListOf(Gen.identifier)
      elementCount <- Gen.chooseNum(0, 20)
      elementsDataInit = ElementsData(
        countRemaining = elementCount,
        // start vector clocks at 1, because the null vector clock happens-before everything,
        // which means we expect "@t1{}" <-< "@t2{t2 -> 1}", even though there was logically no communication there
        possibleNextVectorClocks = tracerIds.view.map(tracerId => (tracerId, true, Map(tracerId -> 1L))).toList,
        elementsAcc = Nil)
      elements <- Gen.tailRecM(elementsDataInit) {
        case ed: ElementsData if ed.countRemaining == 0 =>
          Gen.const(Right(ed.result))
        case ed: ElementsData =>
          for {
            (tracerId, _, nextVC) <- Gen.oneOf(ed.possibleNextVectorClocks)
            isSend <- Gen.oneOf(true, false)
            remoteVCs = ed.possibleNextVectorClocks.filter(_._1 != tracerId)
            updatedVC = updateVC(tracerId, nextVC)
            updatedRemoteVCs = if(isSend) {
              // either the update has been received, or it hasn't. track both options as future possibilities
              // additionally: we make sure it's not possible to receive twice in the same step by toggling canReceive.
              //               initially, canReceive is true, but if we've modeled receiving on that step already,
              //               it becomes false and we don't consider more than one receive to it
              (remoteVCs ::: remoteVCs.flatMap {
                case (_, false, _) => None
                case (remoteTracerId, true, vc) =>
                  Some((remoteTracerId, false, vc.updatedWith(tracerId)(_.map(_ max nextVC.getOrElse(tracerId, 0)).orElse(nextVC.get(tracerId)))))
              }).distinctBy { case (tracerId, _, vc) => (tracerId, vc) }
            } else remoteVCs
          } yield Left(ed
            .withElement {
              val mkElement = if(isSend) () => Element.GenerateTokenTrace("") else () => TestElement()
              mkElement()
                .setVectorClock(nextVC)
                .setTraceId("")
                .setTracerIdentity(tracerId)
                .setLineNumber(ed.elementsAcc.size)
            }
            .withNextVCs((tracerId, true, updatedVC) :: updatedRemoteVCs))
      }
    } yield elements

  def mustHold(query: Query[Any]): Prop =
    query(QueryContext(state = new QueryContext.State(elements = Nil))) match {
      case Accept(_, _) => Prop.passed
      case r@Reject(_, _, _, _) =>
        Prop.collect(r)(Prop.falsified)
    }

  property("latestPredecessors matches happens-before") = Prop.forAll(anyElementList) { elementList =>
    val cr = CausalRelation(elementList)

    Prop.all(elementList.map { elem1 =>
      Prop.all(elementList.view
        .filter(_ ne elem1)
        .filter(elem1 <-< _)
        .map { elem2 =>
          mustHold {
            cr.latestPredecessors(elem2) {
              case e if e eq elem1 => e
            }
              .requireOne
              .require(e => s"$e eq $elem1")(_ eq elem1)
          }
        }
        .toSeq:_*)
    }:_*)
  }

  property("earliestSuccessors matches happens-before") = Prop.forAll(anyElementList) { elementList =>
    val cr = CausalRelation(elementList)

    Prop.all(elementList.map { elem1 =>
      Prop.all(elementList.view
        .filter(_ ne elem1)
        .filter(elem1 <-< _)
        .map { elem2 =>
          mustHold {
            cr.earliestSuccessors(elem1) {
              case e if e eq elem2 => e
            }
              .requireOne
              .require(e => s"$e eq $elem2")(_ eq elem2)
          }
        }
        .toSeq:_*)
    }:_*)
  }
}
