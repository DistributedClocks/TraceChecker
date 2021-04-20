package ca.ubc.cs.tracechecker

import com.github.difflib.{DiffUtils, UnifiedDiffUtils}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object TestingUtils {
  def mkDiff(actualLines: mutable.Buffer[String], expectedLines: mutable.Buffer[String]): mutable.Buffer[String] = {
    val patch = DiffUtils.diff(expectedLines.asJava, actualLines.asJava)
    UnifiedDiffUtils.generateUnifiedDiff("expected", "actual", expectedLines.asJava, patch, 3).asScala
  }
}
