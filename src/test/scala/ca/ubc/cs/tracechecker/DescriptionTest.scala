package ca.ubc.cs.tracechecker

import org.scalatest.funspec.AnyFunSpec

class DescriptionTest extends AnyFunSpec {
  import Description._

  def checkOutput(desc: Description)(expected: String): Unit = {
    val expectedLines = expected.linesIterator.toBuffer
    val actualLines = desc.linesIterator.toBuffer
    val diff = TestingUtils.mkDiff(expectedLines = expectedLines, actualLines = actualLines)
    withClue(diff.mkString("\n") + "\n") {
      assert(expectedLines == actualLines)
    }
  }

  describe("indented") {
    it("should indent each new line that it contains") {
      checkOutput(d"a${d"b\nc".indented}\nd") {
        s"""ab
           |  c
           |d
           |""".stripMargin
      }
    }
    it("should work on the first line") {
      checkOutput(d"a".indented) {
        s"""  a
           |""".stripMargin
      }
    }
    it("should be nestable") {
      checkOutput(d"a\n${d"b\n${d"c".indented}".indented}") {
        s"""a
           |  b
           |    c
           |""".stripMargin
      }
    }
  }

  describe("ensureLineBreakBefore") {
    it("should add a line break if there isn't one") {
      checkOutput(d"a${d"b".ensureLineBreakBefore}") {
        s"""a
           |b
           |""".stripMargin
      }
    }
    it("should not duplicate existing line breaks") {
      checkOutput(d"a\n${d"b".ensureLineBreakBefore}") {
        s"""a
           |b
           |""".stripMargin
      }
    }
    it("should work at the beginning of a description") {
      checkOutput(d"a".ensureLineBreakBefore) {
        s"""
           |a
           |""".stripMargin
      }
    }
  }

  describe("ensureLineBreakAfter") {
    it("should add a line break if there isn't one") {
      checkOutput(d"a".ensureLineBreakAfter) {
        s"""a
           |
           |""".stripMargin
      }
    }
    it("shouldn't duplicate existing line breaks") {
      checkOutput(d"a\n".ensureLineBreakAfter) {
        s"""a
           |
           |""".stripMargin
      }
    }
    it("should work in the middle of a description") {
      checkOutput(d"a${d"b".ensureLineBreakAfter}c") {
        s"""ab
           |c
           |""".stripMargin
      }
    }
  }

  describe("the d string interpolator") {
    it("should detect literal new lines") {
      checkOutput(d"a\nb".indented) {
        s"""  a
           |  b
           |""".stripMargin
      }
    }
  }

  describe("the toDescription extension method") {
    it("should detect literal new lines") {
      checkOutput("a\nb".toDescription.indented) {
        s"""  a
           |  b
           |""".stripMargin
      }
    }
  }

  describe("the flattenDescriptions extension method") {
    it("should concatenate all the descriptions in an iterable") {
      checkOutput(List(d"a", d"b", d"c").flattenDescriptions) {
        s"""abc"""
      }
    }
    it("should work with indentation and new lines") {
      checkOutput(List(
        d"a".ensureLineBreakBefore.ensureLineBreakAfter,
        d"b".ensureLineBreakBefore.ensureLineBreakBefore.indented,
        d"c".ensureLineBreakBefore.ensureLineBreakAfter).flattenDescriptions) {
        s"""
           |a
           |  b
           |c
           |
           |""".stripMargin
      }
    }
  }
}
