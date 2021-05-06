package ca.ubc.cs.tracechecker

import scala.collection.{IterableFactoryDefaults, SeqFactory, StrictOptimizedLinearSeqOps, StrictOptimizedSeqFactory, mutable}
import scala.collection.immutable.{AbstractSeq, LinearSeq, LinearSeqOps, StrictOptimizedSeqOps}

class Scanner[+A] private (private val underlying: IndexedSeq[A], val isFlipped: Boolean, private val idx: Int) extends AbstractSeq[A]
  with LinearSeq[A]
  with LinearSeqOps[A, Scanner, Scanner[A]]
  with StrictOptimizedLinearSeqOps[A, Scanner, Scanner[A]]
  with StrictOptimizedSeqOps[A, Scanner, Scanner[A]]
  with IterableFactoryDefaults[A, Scanner]
{ self =>
  override def head: A = underlying(idx)

  override def knownSize: Int =
    if(isFlipped) (idx + 1) else underlying.size - idx

  override def headOption: Option[A] = if(underlying.isDefinedAt(idx)) Some(underlying(idx)) else None

  private def nextIdx: Int =
    if(isFlipped) idx - 1 else idx + 1

  override def tail: Scanner[A] = new Scanner(underlying = underlying, isFlipped = isFlipped, idx = nextIdx)

  def flipped: Scanner[A] =
    new Scanner(underlying, !isFlipped,
      idx = if(isFlipped) idx + 1 else idx - 1)

  override def isEmpty: Boolean = !underlying.isDefinedAt(idx)

  override def tails: Iterator[Scanner[A]] = new Iterator[Scanner[A]] {
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

  override def iterableFactory: SeqFactory[Scanner] = Scanner
}

object Scanner extends StrictOptimizedSeqFactory[Scanner] {
  override def from[A](source: IterableOnce[A]): Scanner[A] =
    new Scanner[A](underlying = IndexedSeq.from(source), isFlipped = false, idx = 0)

  override def empty[A]: Scanner[A] =
    new Scanner[A](underlying = IndexedSeq.empty, isFlipped = false, idx = 0)

  override def newBuilder[A]: mutable.Builder[A, Scanner[A]] = new mutable.Builder[A, Scanner[A]] {
    private val buf = IndexedSeq.newBuilder[A]

    override def clear(): Unit = buf.clear()

    override def result(): Scanner[A] = new Scanner[A](underlying = buf.result(), isFlipped = false, idx = 0)

    override def addOne(elem: A): this.type = {
      buf.addOne(elem)
      this
    }
  }
}
