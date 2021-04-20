package ca.ubc.cs.tracechecker

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

class Scanner[+A] private (private val seq: IndexedSeq[A], val isFlipped: Boolean, private val idx: Int) { self =>
  def head: A = seq(idx)

  def headOption: Option[A] = if(seq.isDefinedAt(idx)) Some(seq(idx)) else None

  private def nextIdx: Int =
    if(isFlipped) idx - 1 else idx + 1

  def tail: Scanner[A] = new Scanner(seq = seq, isFlipped = isFlipped, idx = nextIdx)

  def flipped: Scanner[A] =
    new Scanner(seq, !isFlipped,
      idx = if(isFlipped) idx + 1 else idx - 1)

  def isEmpty: Boolean = !seq.isDefinedAt(idx)

  def tails: Iterator[Scanner[A]] = new Iterator[Scanner[A]] {
    private var curr = self
    override def hasNext: Boolean = curr ne null
    override def next(): Scanner[A] = {
      val result = curr
      if(curr.isEmpty) {
        curr = null
      } else {
        curr = curr.tail
      }
      result
    }
  }
}

object Scanner {
  def apply[A](elems: A*)(implicit ct: ClassTag[A]): Scanner[A] =
    apply(ArraySeq.from(elems))

  def apply[A](seq: IndexedSeq[A]): Scanner[A] =
    new Scanner[A](seq = seq, isFlipped = false, idx = 0)

  def empty: Scanner[Nothing] = apply(IndexedSeq.empty)
}
