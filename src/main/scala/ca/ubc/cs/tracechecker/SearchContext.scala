package ca.ubc.cs.tracechecker

/**
 * A refinable predicate, roughly speaking.
 *
 * Used by the TraceChecker to express implicit contextual restrictions on what may match a condition, like
 * similar constructs made in human language.
 *
 * @tparam TraceElement the type of trace element to restrict
 */
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
