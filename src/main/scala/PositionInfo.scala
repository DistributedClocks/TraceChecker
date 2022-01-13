package com.github.distributedclocks.tracechecker

final case class PositionInfo(lineNumber: Int, file: String) {
  override def toString: String = s"$file:$lineNumber"
}

object PositionInfo {
  implicit def findPositionInfo(implicit lineNumber: sourcecode.Line, file: sourcecode.File): PositionInfo =
    PositionInfo(lineNumber = lineNumber.value, file = file.value)
}
