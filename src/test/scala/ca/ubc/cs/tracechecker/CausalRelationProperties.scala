package ca.ubc.cs.tracechecker

import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Properties

class CausalRelationProperties extends AnyFunSuite {
  CausalRelationProperties.properties.foreach {
    case (name, prop) =>
      test(name) {
        import org.scalacheck._
        val result = Test.check(prop)(CausalRelationProperties.overrideParameters)
        withClue(pprint.apply(result)) {
          assert(result.passed)
        }
      }
  }
}

object CausalRelationProperties extends Properties("CausalRelation") {
  import org.scalacheck._

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)

  final case class ElementsData(countRemaining: Int, possibleNextVectorClocks: Map[String,(Map[String,Long],List[Map[String,Long]])], elementsAcc: List[Element]) {
    def result: List[Element] = elementsAcc.reverse

    def withElement(element: Element): ElementsData =
      copy(
        countRemaining = countRemaining - 1,
        elementsAcc = element :: elementsAcc)

    def withUpdatedVC(tracerId: String, vc: Map[String,Long], possibleReceives: List[Map[String,Long]]): ElementsData =
      copy(possibleNextVectorClocks = possibleNextVectorClocks.updated(tracerId, (vc, possibleReceives)))

    def withSentMsg(tracerId: String, vc: Map[String,Long]): ElementsData =
      copy(possibleNextVectorClocks = possibleNextVectorClocks.view
        .map {
          case (`tracerId`, v) => (tracerId, v)
          case (currId, (currVc, possibleReceives)) => (currId, (currVc, vc :: possibleReceives))
        }
        .toMap)

    def withReceivedVC(tracerId: String, vc: Map[String,Long]): ElementsData =
      copy(possibleNextVectorClocks = possibleNextVectorClocks.updatedWith(tracerId)(_.map {
        case (currVc, possibleReceives) => (currVc, possibleReceives.filterNot(_ eq vc))
      }))
  }

  final case class TestElement() extends Element

  private def updateVC(tracerId: String, vc: Map[String,Long]): Map[String,Long] =
    vc.updatedWith(tracerId)(_.map(_ + 1).orElse(Some(1)))

  private def combineVCs(left: Map[String,Long], right: Map[String,Long]): Map[String,Long] =
    (left.keysIterator ++ right.keysIterator)
      .map { key =>
        key -> math.max(left.getOrElse(key, 0L), right.getOrElse(key, 0L))
      }
      .toMap

  val anyElementList: Gen[List[Element]] =
    for {
      tracerIds <- Gen.nonEmptyListOf(Gen.identifier)
      elementCount <- Gen.chooseNum(0, 20)
      elementsDataInit = ElementsData(
        countRemaining = elementCount,
        // this implies each starting vector will contain a 1, to prevent confusion due to empty VCs:
        // that would mean we expect "@t1{}" <-< "@t2{t2 -> 1}", even though there was logically no communication there
        possibleNextVectorClocks = tracerIds.view.map(tracerId => tracerId -> (Map.empty[String,Long], Nil)).toMap,
        elementsAcc = Nil)
      elements <- Gen.tailRecM(elementsDataInit) {
        case ed: ElementsData if ed.countRemaining == 0 =>
          Gen.const(Right(ed.result))
        case ed: ElementsData =>
          Gen.oneOf(tracerIds)
            .flatMap { tracerId =>
              val (currentVC, possibleReceives) = ed.possibleNextVectorClocks(tracerId)
              val basicNextVC = updateVC(tracerId, currentVC)
              Gen.oneOf(
                Gen.delay(Gen.const(
                  ed.withUpdatedVC(tracerId, basicNextVC, possibleReceives)
                    .withElement(
                      TestElement()
                        .setVectorClock(basicNextVC)
                        .setLineNumber(ed.elementsAcc.size)
                        .setTraceId("")
                        .setTracerIdentity(tracerId)))
                ),
                Gen.delay(Gen.const(
                  ed.withUpdatedVC(tracerId, basicNextVC, possibleReceives)
                    .withSentMsg(tracerId, basicNextVC)
                    .withElement(
                      Element.GenerateTokenTrace("")
                        .setVectorClock(basicNextVC)
                        .setLineNumber(ed.elementsAcc.size)
                        .setTraceId("")
                        .setTracerIdentity(tracerId))
                )),
                possibleReceives.map { recvVC =>
                  Gen.delay(Gen.const {
                    val nextVC = combineVCs(basicNextVC, recvVC)
                    ed.withUpdatedVC(tracerId, nextVC, possibleReceives)
                      .withReceivedVC(tracerId, recvVC)
                      .withElement(
                        Element.ReceiveTokenTrace("")
                          .setVectorClock(nextVC)
                          .setLineNumber(ed.elementsAcc.size)
                          .setTraceId("")
                          .setTracerIdentity(tracerId)
                      )
                  })
                }:_*)
            }
            .map(Left(_))
      }
    } yield elements

  def mustHold(query: Query[Any]): Prop =
    query(QueryContext(state = new QueryContext.State(elements = Nil))) match {
      case Accept(_, _) => Prop.passed
      case r@Reject(_, _, _, _) =>
        Prop.collect(r)(Prop.falsified)
    }

  property("latestPredecessors matches happens-before (single)") = Prop.forAllNoShrink(anyElementList) { elementList =>
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

  property("earliestSuccessors matches happens-before (single)") = Prop.forAllNoShrink(anyElementList) { elementList =>
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

  property("latestPredecessors matches happens-before (all)") = Prop.forAllNoShrink(anyElementList) { elementList =>
    val cr = CausalRelation(elementList)

    Prop.all(elementList.map { elem1 =>
      mustHold {
        cr.latestPredecessors(elem1) { case elem => elem }
          .map(_.view.map(ById(_)).toSet)
          .flatMap { results =>
            // calculate set of "maximum" elements to expect
            val expectedResults = elementList.view
              .filter(_ ne elem1)
              .filter(_ <-< elem1)
              .foldLeft(Set.empty[ById[Element]]) { (acc, elem) =>
                val filteredAcc = acc.filterNot(_.ref <-< elem)
                if(!filteredAcc.exists(elem <-< _.ref)) {
                  filteredAcc.incl(ById(elem))
                } else {
                  filteredAcc
                }
              }

            for {
              _ <- Queries.label("results")(results)
              _ <- Queries.label("expectedResults")(expectedResults)
              _ <- Queries.require("results must match expectedResults")(results == expectedResults)
            } yield ()
          }
      }
    }:_*)
  }

  property("earliestSuccessors matches happens-before (all)") = Prop.forAllNoShrink(anyElementList) { elementList =>
    val cr = CausalRelation(elementList)

    Prop.all(elementList.map { elem1 =>
      mustHold {
        cr.earliestSuccessors(elem1) { case elem => elem }
          .map(_.view.map(ById(_)).toSet)
          .flatMap { results =>
            // calculate set of "minimum" elements to expect
            val expectedResults = elementList.view
              .filter(_ ne elem1)
              .filter(elem1 <-< _)
              .foldLeft(Set.empty[ById[Element]]) { (acc, elem) =>
                val filteredAcc = acc.filterNot(elem <-< _.ref)
                if(!filteredAcc.exists(_.ref <-< elem)) {
                  filteredAcc.incl(ById(elem))
                } else {
                  filteredAcc
                }
              }

            for {
              _ <- Queries.label("results")(results)
              _ <- Queries.label("expectedResults")(expectedResults)
              _ <- Queries.require("results must match expectedResults")(results == expectedResults)
            } yield ()
          }
      }
    }:_*)
  }
}
