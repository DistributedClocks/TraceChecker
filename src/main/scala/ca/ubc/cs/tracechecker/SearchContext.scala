package ca.ubc.cs.tracechecker

abstract class SearchContext[-TraceElement] extends (TraceElement=>Boolean) { self =>
  override def apply(elem: TraceElement): Boolean

  final def restrict[U <: TraceElement](condition: U=>Boolean*): SearchContext[U] = { elem =>
    self(elem) && condition.forall(_(elem))
  }
}

object SearchContext {
  object Default extends SearchContext[Any] {
    override def apply(elem: Any): Boolean = true
  }
}
