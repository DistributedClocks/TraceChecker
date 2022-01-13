package ca.ubc.cs.tracechecker

import scala.collection.mutable

final class CausalRelation private(private val predecessors: Map[ById[Element],List[Element]], private val successors: Map[ById[Element],List[Element]], private val nodes: List[Element]) {

  private def crawlGraph[U](from: Element, graph: Map[ById[Element],List[Element]], fn: PartialFunction[Element,U]): Iterator[(Element,U)] = {
    val visitedNodes = mutable.HashSet.empty[ById[Element]]

    def crawler(from: Element): Iterator[(Element,U)] =
      if(visitedNodes(ById(from))) {
        Iterator.empty
      } else {
        visitedNodes += ById(from)
        graph.getOrElse(ById(from), Nil)
          .iterator
          .flatMap { to =>
            fn.unapply(to) match {
              case None => crawler(to)
              case Some(u) => Some(to -> u)
            }
          }
      }

    crawler(from)
  }

  /**
   * Starting at member from, work "backwards" through the relation's implied graph.
   * For all instances x --> from, where x satisfies fn, and here does not exist any x --> y -->* from, yield x.
   *
   * This query yields a sequence of matching elements, none of which happens-before any other, to account for ambiguous/branching causality graphs.
   * Use fn's precondition to manipulate which elements the query considers "interesting",
   * e.g "give me the most recent instance(s) of record X".
   */
  def latestPredecessors[U <: AnyRef](from: Element)(fn: PartialFunction[Element,U]): Query[LazyList[U]] =
    Queries.accept(
      crawlGraph(from, predecessors, fn)
        .distinctBy(_._1.lineNumber)
        .foldLeft(Nil: List[(Element,U)]) { (pairs, p) =>
          val filteredPairs = pairs.filterNot(_._1 <-< p._1)
          if(!filteredPairs.exists(p._1 <-< _._1)) {
            p :: filteredPairs
          } else {
            filteredPairs
          }
        }
        .view.map(_._2)
        .to(LazyList))

  /**
   * This query operates as co-query to latestPredecessors. All the configuration is the same, but the causality graph
   * is navigated in the opposite direction.
   */
  def earliestSuccessors[U <: AnyRef](from: Element)(fn: PartialFunction[Element,U]): Query[LazyList[U]] =
    Queries.accept(
      crawlGraph(from, successors, fn)
        .distinctBy(_._1.lineNumber)
        .foldLeft(Nil: List[(Element,U)]) { (pairs, p) =>
          val filteredPairs = pairs.filterNot(p._1 <-< _._1)
          if(!filteredPairs.exists(_._1 <-< p._1)) {
            p :: filteredPairs
          } else {
            filteredPairs
          }
        }
        .view.map(_._2)
        .to(LazyList))

  /**
   * Generates a description of the happens-before graph in DOT language, which can then be displayed via graphviz.
   * Uses the internal adjacency map in the "before" direction.
   * May help debugging.
   */
  def toDotPredecessors: String =
    s"digraph {\n${
      predecessors.view.flatMap {
        case (succ, preds) =>
          preds.view.map(_ -> succ.ref)
      }
        .map { case pred -> succ => s"\"$pred\" -> \"$succ\";" }
        .mkString("\n")
    }\n}"

  /**
   * Same as toDotPredecessors, but using the internal adjacency map in the "after" direction.
   * May help debugging.
   */
  def toDotSuccessors: String =
    s"digraph {\n${
      successors.view.flatMap {
        case (pred, succs) =>
          succs.view.map(pred.ref -> _)
      }
        .map { case pred -> succ => s"\"$pred\" -> \"$succ\";" }
        .mkString("\n")
    }\n}"
}

object CausalRelation {
  /**
   * Constructs an instance of CausalRelation from a sequence of Element instances, which can be used to express
   * concepts of "the most recent" or "the earliest following".
   *
   * Assumptions (checked via assertions):
   * - there are no "gaps" in the vector clocks, that is, every traceId should produce a strictly increasing sequence of local vector clock indices
   * - the same element shall not appear twice, that is, no two elements shall have the same line number or vector clock
   * - basic vector clock rules: vector clocks reported in sequence by the same node will happen-before/after each other
   */
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
