package ca.ubc.cs.tracechecker

import scala.collection.mutable

final class CausalRelation private(private val predecessors: Map[ById[Element],List[Element]], private val successors: Map[ById[Element],List[Element]], private val nodes: List[Element]) {
  private def vClockMax(lhs: Map[String,Long], rhs: Map[String,Long]): Map[String,Long] =
    (lhs.keysIterator ++ rhs.keysIterator)
      .map(key => key -> math.max(lhs.getOrElse(key, 0L), rhs.getOrElse(key, 0L)))
      .toMap

  private def vClockMin(lhs: Map[String,Long], rhs: Map[String,Long]): Map[String,Long] =
    (lhs.keysIterator ++ rhs.keysIterator)
      .map(key => key -> math.min(lhs.getOrElse(key, 0L), rhs.getOrElse(key, 0L)))
      .toMap

  private lazy val maxVClock: Map[String,Long] =
    nodes.view
      .map(_.vectorClock)
      .reduceOption(vClockMax)
      .getOrElse(Map.empty)

  /**
   * Starting at member from, work "backwards" through the relation's implied graph.
   * For all instances x --> from, where there does not exist any x --> y -->* from, yield x.
   *
   * A similar "forwards" utility should exist.
   */
  def latestPredecessors[U <: AnyRef](from: Element)(fn: PartialFunction[Element,U]): Query[LazyList[U]] = {
    // the vector clock you are not allowed to happen-before.
    // it is possible in more complex graphs for the crawler below to go "the long way round" and get "behind"
    // an existing match for fn. if we record a minimum allowable clock, combining the vector clocks of all
    // nodes we've accepted so far, then the crawler will disregard any such node it finds, alongside anything that
    // happens-before it.
    var minVClock = Map.empty[String,Long]

    val visitedNodes = mutable.HashSet.empty[ById[Element]]

    def crawler(from: Element): Iterator[U] =
      if(visitedNodes(ById(from))) {
        //println(s"visited: $from")
        Iterator.empty
      } else {
        visitedNodes += ById(from)
        //println(s"$from >>=${predecessors.getOrElse(ById(from), Nil).mkString("\n  ", "\n  ", "")}")
        predecessors.getOrElse(ById(from), Nil)
          .iterator
          //.tapEach(elem => println(s"pred: $elem"))
          .filterNot(_ <-< minVClock)
          //.tapEach(elem => println(s"cons: $elem"))
          .flatMap { to =>
            fn.unapply(to) match {
              case None => crawler(to)
              case s@Some(_) =>
                minVClock = vClockMax(minVClock, from.vectorClock)
                s
            }
          }
      }

    //println("---")
    Queries.accept(
      crawler(from)
        .distinctBy(ById(_))
        //.tapEach(println)
        .to(LazyList))
  }

  def earliestSuccessors[U <: AnyRef](from: Element)(fn: PartialFunction[Element,U]): Query[LazyList[U]] = {
    var maxVClock = Map.empty[String,Long]

    def crawler(from: Element): Iterator[U] =
      successors.getOrElse(ById(from), Nil)
        .iterator
        .filter(_.vectorClock.forall { case (k, clock) => clock <= maxVClock.getOrElse(k, 0L) })
        .flatMap { to =>
          fn.unapply(to) match {
            case None => crawler(to)
            case s@Some(_) =>
              maxVClock = (maxVClock.keysIterator ++ to.vectorClock.keysIterator)
                .map(k => k -> Math.min(maxVClock.getOrElse(k, 0L), to.vectorClock.getOrElse(k, 0L)))
                .toMap

              s
          }
        }

    Queries.accept(
      crawler(from)
        .distinctBy(ById(_))
        .to(LazyList))
  }

  def toDotPredecessors: String =
    s"digraph {\n${
      predecessors.view.flatMap {
        case (succ, preds) =>
          preds.view.map(_ -> succ.ref)
      }
        .map { case pred -> succ => s"\"$pred\" -> \"$succ\";" }
        .mkString("\n")
    }\n}"
}

object CausalRelation {
  def apply(elements: IterableOnce[Element]): CausalRelation = {
    val sortedElements = elements.iterator.toArray.sortInPlace()(Element.VectorClockOrdering)

    val wavefront = mutable.HashMap.empty[String,Element]
    val sends = mutable.HashMap.empty[String,mutable.HashMap[Long,Element]]

    val pairs = mutable.ListBuffer.empty[(Element,Element)]

    sortedElements.foreach { elem =>
      wavefront.get(elem.tracerIdentity) match {
        case Some(prevElem) =>
          // sanity asserts
          assert(prevElem.tracerIdentity == elem.tracerIdentity)
          assert(prevElem.vectorClockSelf + 1 == elem.vectorClockSelf)

          // necessarily, this elem happens-after the last elem we saw from that node
          assert(prevElem <-< elem)
          pairs += prevElem -> elem
        case None => // nothing special: just add to wavefront below and wait for event 2 from that node
      }
      wavefront.update(elem.tracerIdentity, elem)

      elem.vectorClock.foreach {
        case (otherId, otherClock) =>
          sends.getOrElseUpdate(otherId, mutable.HashMap.empty).get(otherClock) match {
            case Some(otherElem) =>
              assert(otherElem <-< elem)
              pairs += otherElem -> elem
            case None =>
          }
      }

      if(elem.isSend) {
        val sendsAtTracer = sends.getOrElseUpdate(elem.tracerIdentity, mutable.HashMap.empty)
        sendsAtTracer.update(elem.vectorClockSelf, elem)
      }
    }

    new CausalRelation(
      predecessors = pairs.result().groupMap(pair => ById(pair._2))(_._1),
      successors = pairs.result().groupMap(pair => ById(pair._1))(_._2),
      nodes = sortedElements.toList)
  }
}
