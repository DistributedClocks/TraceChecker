package com.github.distributedclocks.tracechecker

trait Element extends Product {
  private[this] var _lineNumber: Option[Int] = None
  final def lineNumber: Int = _lineNumber.get
  final def setLineNumber(lineNumber: Int): this.type = {
    _lineNumber = Some(lineNumber)
    this
  }

  private[this] var _vectorClock: Option[Map[String,Long]] = None
  final def vectorClock: Map[String,Long] = _vectorClock.get
  final def setVectorClock(vectorClock: Map[String,Long]): this.type = {
    assert(_vectorClock.isEmpty)
    _vectorClock = Some(vectorClock)
    this
  }

  private[this] var _tracerIdentity: Option[String] = None
  final def tracerIdentity: String = _tracerIdentity.get
  final def setTracerIdentity(tracerIdentity: String): this.type = {
    assert(_tracerIdentity.isEmpty)
    _tracerIdentity = Some(tracerIdentity)
    this
  }

  private[this] var _traceId: Option[String] = None
  final def traceId: String = _traceId.get
  final def setTraceId(traceId: String): this.type = {
    assert(_traceId.isEmpty)
    _traceId = Some(traceId)
    this
  }

  def isSend: Boolean = false

  final def vectorClockSelf: Long = vectorClock.getOrElse(tracerIdentity, 0)

  override def toString: String =
    s"[$lineNumber] $productPrefix(${
      (productElementNames zip productIterator)
        .map { case name -> value => s"$name = $value" }
        .mkString(", ")
    })@$tracerIdentity{${
      vectorClock
        .keysIterator
        .toArray
        .sortInPlace()
        .iterator
        .map(key => s"$key -> ${vectorClock(key)}")
        .mkString(", ")
    }}#$traceId"

  final def <-<(other: Element): Boolean = vcLessThan(vectorClock, other.vectorClock)

  final def <-<(other: Map[String,Long]): Boolean = vcLessThan(vectorClock, other)

  final def :<-<:(other: Map[String,Long]): Boolean = vcLessThan(other, vectorClock)

  private def vcLessThan(left: Map[String,Long], right: Map[String,Long]): Boolean =
    (left.keysIterator ++ right.keysIterator).forall { key => right.getOrElse(key, 0L) >= left.getOrElse(key, 0L) } &&
      right.exists { case (key, clock) => clock > left.getOrElse(key, 0L) }
}

object Element {
  case class CreateTrace() extends Element
  case class GenerateTokenTrace(token: String) extends Element {
    override def isSend: Boolean = true
  }
  case class ReceiveTokenTrace(token: String) extends Element

  implicit object VectorClockPartialOrdering extends PartialOrdering[Element] {
    override def lteq(x: Element, y: Element): Boolean =
      x.vectorClock.keysIterator.forall(k => x.vectorClock(k) <= y.vectorClock.getOrElse(k, 0L))

    override def tryCompare(x: Element, y: Element): Option[Int] = {
      val xley = lteq(x, y)
      val ylex = lteq(y, x)
      if(xley && ylex) {
        Some(0)
      } else if(xley) {
        Some(-1)
      } else if(ylex) {
        Some(1)
      } else {
        None
      }
    }
  }

  /**
   * An unsound ordering on vector clocks: incomparable vector clocks are considered equal.
   * Useful for sorting, but no guarantees.
   */
  object VectorClockOrdering extends Ordering[Element] {
    private val partialOrdering = implicitly[PartialOrdering[Element]]

    override def compare(x: Element, y: Element): Int =
      partialOrdering.tryCompare(x, y).getOrElse(0)
  }
}
