package com.github.distributedclocks.tracechecker

sealed abstract class Result[+T] {
  val ctx: QueryContext
}

final case class Accept[T](value: T, ctx: QueryContext) extends Result[T]

final case class Reject(msg: String, ctx: QueryContext, relatedValues: List[Any], positionInfo: PositionInfo) extends Result[Nothing]
