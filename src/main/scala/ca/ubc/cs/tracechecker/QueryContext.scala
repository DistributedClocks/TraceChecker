package ca.ubc.cs.tracechecker

import scala.collection.mutable

final case class QueryContext private (entries: Map[String,QueryContext.Entry] = Map.empty, entrySeq: List[String] = Nil, state: QueryContext.State) {
  def withoutEntries: QueryContext = copy(entries = Map.empty, entrySeq = Nil)

  override def toString: String = s"QueryContext(annotations = $entries)"

  def withGroup(name: String, ctx: QueryContext): QueryContext = {
    assert(!entries.contains(name))
    copy(
      entries = entries.updated(name, QueryContext.GroupEntry(ctx)),
      entrySeq = name :: entrySeq)
  }

  def withObservation(name: String, value: Any): QueryContext = {
    assert(!entries.contains(name))
    copy(
      entries = entries.updated(name, QueryContext.ValueEntry(value)),
      entrySeq = name :: entrySeq)
  }
}

object QueryContext {
  sealed abstract class Entry
  final case class ValueEntry(value: Any) extends Entry
  final case class GroupEntry(ctx: QueryContext) extends Entry

  final class State(val elements: List[Element]) {
    val materializedState: mutable.HashMap[ById[Query[_]],Any] = mutable.HashMap.empty
  }
}
