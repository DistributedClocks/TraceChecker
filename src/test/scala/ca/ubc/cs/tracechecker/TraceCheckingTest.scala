package ca.ubc.cs.tracechecker

import org.scalatest.funspec.AnyFunSpec

class TraceCheckingTest extends AnyFunSpec with TraceChecking {
  import Description._

  type TraceElement = Int

  private val trace1 = Scanner.from(1 to 10)

  def checkPass[T](tc: TraceChecker[T], input: Input)(expected: (String,List[(T,Option[TraceElement])])): Unit = {
    tc(input) match {
      case TracePassed(actualDesc, actualOuts) =>
        val (expectedDesc, expectedOuts) = expected
        // intentionally fully evaluate the outputs
        val outList = actualOuts.toList
        val actualLines = actualDesc.linesIterator.toBuffer
        val expectedLines = expectedDesc.linesIterator.toBuffer
        val diff = TestingUtils.mkDiff(actualLines, expectedLines)
        withClue(diff.mkString("\n") + "\n") {
          assert(actualLines == expectedLines)
          assert(outList.map(ts => (ts.result, ts.rest.headOption)) == expectedOuts)
        }
      case TraceViolation(description) =>
        fail(s"failed with message:\n${description.linesIterator.mkString("\n")}")
    }
  }

  def checkFail[T](tc: TraceChecker[T], input: Input)(expected: String): Unit = {
    tc(input) match {
      case TracePassed(description, _) =>
        fail(s"passed with message:\n${description.linesIterator.mkString("\n")}")
      case TraceViolation(actualDesc) =>
        val expectedLines = expected.linesIterator.toBuffer
        val actualLines = actualDesc.linesIterator.toBuffer
        val diff = TestingUtils.mkDiff(actualLines, expectedLines)
        withClue(diff.mkString("\n") + "\n") {
          assert(actualLines == expectedLines)
        }
    }
  }

  describe("check combinator") {
    // with default context
    locally {
      implicit val ctx = SearchContext.Default
      it("passes") {
        checkPass(check(d"a")(_ == 1), trace1) {
          "a: matched at 1" -> List(1 -> Some(2))
        }
      }
      it("fails") {
        checkFail(check(d"a")(_ == 2), trace1) {
          "a: failed at 1"
        }
      }
    }
    // with non-default context
    locally {
      implicit val ctx = SearchContext.Default.restrict[Int](_ != 1)
      it("fails when the context rejects an element") {
        checkFail(check(d"a")(_ == 1), trace1) {
          "a: irrelevant at 1"
        }
      }
    }
  }

  describe("AnyElement") {
    it("works for first element of trace1, at least") {
      checkPass(AnyElement, trace1) {
        "any trace element: 1" -> List(1 -> Some(2))
      }
    }
  }

  describe("eventually combinator") {
    it("can capture all elements") {
      checkPass(eventually(AnyElement), trace1) {
        s"""eventually holds:
           |  any trace element: 1
           |  any trace element: 2
           |  any trace element: 3
           |  any trace element: 4
           |  any trace element: 5
           |  any trace element: 6
           |  any trace element: 7
           |  any trace element: 8
           |  any trace element: 9
           |  any trace element: 10
           |""".stripMargin ->
          List(
            1 -> Some(2),
            2 -> Some(3),
            3 -> Some(4),
            4 -> Some(5),
            5 -> Some(6),
            6 -> Some(7),
            7 -> Some(8),
            8 -> Some(9),
            9 -> Some(10),
            10 -> None)
      }
    }
    it("is lazy, will only capture all elements if it needs to") {
      checkPass(eventually(AnyElement).first, trace1) {
        s"""first of:
           |  eventually holds:
           |    any trace element: 1
           |    ...
           |""".stripMargin ->
          List(1 -> Some(2))
      }
    }
    it("fails if there are no matching elements") {
      implicit val ctx = SearchContext.Default
      checkFail(eventually(check(d"no")(_ => false)), trace1) {
        s"""does not eventually hold:
           |  no: failed at 1
           |  ...
           |  no: end of trace
           |""".stripMargin
      }
    }
  }

  describe("anyOf combinator") {
    it("can capture all elements") {
      checkPass(anyOf(AnyElement), trace1) {
        s"""unconditional search:
           |  any trace element: 1
           |  any trace element: 2
           |  any trace element: 3
           |  any trace element: 4
           |  any trace element: 5
           |  any trace element: 6
           |  any trace element: 7
           |  any trace element: 8
           |  any trace element: 9
           |  any trace element: 10
           |""".stripMargin ->
          List(
            1 -> Some(2),
            2 -> Some(3),
            3 -> Some(4),
            4 -> Some(5),
            5 -> Some(6),
            6 -> Some(7),
            7 -> Some(8),
            8 -> Some(9),
            9 -> Some(10),
            10 -> None)
      }
    }
    it("is lazy, will only capture all elements if it needs to") {
      checkPass(anyOf(AnyElement).first, trace1) {
        s"""first of:
           |  unconditional search:
           |    any trace element: 1
           |    ...
           |""".stripMargin ->
          List(1 -> Some(2))
      }
    }
    it("succeeds vacuously if no elements match") {
      implicit val ctx = SearchContext.Default
      checkPass(anyOf(check(d"no")(_ => false)), trace1) {
        s"""unconditional search: no results
           |""".stripMargin -> Nil
      }
    }
  }

  describe("once combinator") {
    it("succeeds with exactly one result") {
      implicit val ctx = SearchContext.Default
      checkPass(once(eventually(check(d"five")(_ == 5))), trace1) {
        s"""exactly once:
           |  eventually holds:
           |    five: matched at 5
           |""".stripMargin ->
          List(5 -> Some(6))
      }
    }
    it("fails with no results") {
      implicit val ctx = SearchContext.Default
      checkFail(once(anyOf(check(d"no")(_ => false))), trace1) {
        s"""exactly once:
           |  unconditional search: no results
           |""".stripMargin
      }
    }
    it("fails with more than one result") {
      implicit val ctx = SearchContext.Default
      checkFail(once(anyOf(check(d"2 or 4")(x => x == 2 || x == 4))), trace1) {
        s"""exactly once:
           |  unconditional search:
           |    2 or 4: matched at 2
           |    2 or 4: matched at 4
           |""".stripMargin
      }
    }
  }

  describe("not combinator") {
    it("fails when the underlying combinator passes") {
      checkFail(not(pass(d"yes", ())), trace1) {
        s"""not [violation]:
           |  yes
           |""".stripMargin
      }
    }
    it("passes when the underlying combinator fails") {
      checkPass(not(violation(d"no")), trace1) {
        s"""not [success]:
           |  no
           |""".stripMargin ->
          List(() -> Some(1))
      }
    }
    it("passes without consuming input") {
      checkPass(not(eventually(violation(d"no"))), trace1) {
        s"""not [success]:
           |  does not eventually hold:
           |    no
           |    ...
           |    no
           |""".stripMargin ->
          List(() -> Some(1))
      }
    }
  }

  describe("guard combinator") {
    it("passes when the underlying combinator passes, without consuming input, or forcing") {
      implicit val ctx = SearchContext.Default
      checkPass(guard(anyOf(check(d"2 or 4")(x => x == 2 || x == 4))), trace1) {
        s"""guarding (check without consuming input):
           |  unconditional search:
           |    2 or 4: matched at 2
           |    ...
           |""".stripMargin ->
          List(LazyList(2, 4) -> Some(1))
      }
    }
    it("fails when the underlying combinator fails") {
      checkFail(guard(violation(d"no")), trace1) {
        s"""guarding (check without consuming input) failed:
           |  no
           |""".stripMargin
      }
    }
  }

  describe("guards combinator") {
    it("passes when all the underlying combinators pass, without consuming input") {
      checkPass(guards(pass(d"yes 1", 1), pass(d"yes 2", 2), pass(d"yes 3", 3)), trace1) {
        s"""guarding (check without consuming input):
           |  yes 1
           |  yes 2
           |  yes 3
           |""".stripMargin ->
          List(List(LazyList(1), LazyList(2), LazyList(3)) -> Some(1))
      }
    }
    it("fails when any one underlying combinator fails") {
      checkFail(guards(pass(d"yes 1", 1), violation(d"no 2"), pass(d"yes 3", 3)), trace1) {
        s"""guarding (check without consuming input) failed:
           |  no 2
           |""".stripMargin
      }
    }
    it("does not force underlying combinators") {
      implicit val ctx = SearchContext.Default
      checkPass(guards(anyOf(check(d"2 or 4")(x => x == 2 || x == 4))).map(_.map(_.head)), trace1) {
        s"""guarding (check without consuming input):
           |  unconditional search:
           |    2 or 4: matched at 2
           |    ...
           |""".stripMargin ->
          List(List(2) -> Some(1))
      }
    }
  }

  describe("flatMap combinator") {
    it("passes when both the initial combinator and all consequences pass") {
      implicit val ctx = SearchContext.Default
      val combinator = eventually(check(d"element < 4")(x => x < 4)).flatMap { x =>
        eventually(check(d"element > x")(_ > x))
      }
      checkPass(combinator, trace1) {
        s"""initial success:
           |  eventually holds:
           |    element < 4: matched at 1
           |    element < 4: matched at 2
           |    element < 4: matched at 3
           |followed by success(es) based on [1]:
           |  eventually holds:
           |    element > x: matched at 2
           |    element > x: matched at 3
           |    element > x: matched at 4
           |    element > x: matched at 5
           |    element > x: matched at 6
           |    element > x: matched at 7
           |    element > x: matched at 8
           |    element > x: matched at 9
           |    element > x: matched at 10
           |followed by success(es) based on [2]:
           |  eventually holds:
           |    element > x: matched at 3
           |    element > x: matched at 4
           |    element > x: matched at 5
           |    element > x: matched at 6
           |    element > x: matched at 7
           |    element > x: matched at 8
           |    element > x: matched at 9
           |    element > x: matched at 10
           |followed by success(es) based on [3]:
           |  eventually holds:
           |    element > x: matched at 4
           |    element > x: matched at 5
           |    element > x: matched at 6
           |    element > x: matched at 7
           |    element > x: matched at 8
           |    element > x: matched at 9
           |    element > x: matched at 10
           |""".stripMargin ->
          ((2 to 10).toList.map {
            case 10 => (10, None)
            case x => (x, Some(x+1))
          } ::: (3 to 10).toList.map {
            case 10 => (10, None)
            case x => (x, Some(x+1))
          } ::: (4 to 10).toList.map {
            case 10 => (10, None)
            case x => (x, Some(x+1))
          })
      }
    }
    it("passes without forcing unnecessary checks, if it itself is not forced") {
      implicit val ctx = SearchContext.Default
      val combinator = eventually(check(d"element < 4")(x => x < 4)).flatMap { x =>
        eventually(check(d"element > x")(_ > x))
      }.first
      checkPass(combinator, trace1) {
        s"""first of:
           |  initial success:
           |    eventually holds:
           |      element < 4: matched at 1
           |      element < 4: matched at 2
           |      element < 4: matched at 3
           |  followed by success(es) based on [1]:
           |    eventually holds:
           |      element > x: matched at 2
           |      ...
           |  followed by success(es) based on [2]:
           |    eventually holds:
           |      element > x: matched at 3
           |      ...
           |  followed by success(es) based on [3]:
           |    eventually holds:
           |      element > x: matched at 4
           |      ...
           |""".stripMargin ->
          List(2 -> Some(3))
      }
    }
    it("fails if the initial combinator fails") {
      implicit val ctx = SearchContext.Default
      val combinator = eventually(check(d"no")(_ => false)).flatMap { x =>
        eventually(check(d"element > x")(_ > x))
      }
      checkFail(combinator, trace1) {
        s"""does not eventually hold:
           |  no: failed at 1
           |  ...
           |  no: end of trace
           |""".stripMargin
      }
    }
    it("fails if the any consequence leads to failure") {
      implicit val ctx = SearchContext.Default
      val combinator = eventually(check(d"2 or 4")(x => x == 2 || x == 4)).flatMap { x =>
        check(d"element == 3")(_ == 3)
      }
      checkFail(combinator, trace1) {
        s"""initial success:
           |  eventually holds:
           |    2 or 4: matched at 2
           |    2 or 4: matched at 4
           |followed by failure based on [4]:
           |  element == 3: failed at 5
           |""".stripMargin
      }
    }
    it("fails fast if an early consequence leads to failure") {
      implicit val ctx = SearchContext.Default
      val combinator = eventually(check(d"2 or 4")(x => x == 2 || x == 4)).flatMap { x =>
        check(d"element == 5")(_ == 5)
      }
      checkFail(combinator, trace1) {
        s"""initial success:
           |  eventually holds:
           |    2 or 4: matched at 2
           |    ...
           |followed by failure based on [2]:
           |  element == 5: failed at 3
           |""".stripMargin
      }
    }
  }

  describe("withLabel combinator") {
    it("labels passes") {
      checkPass(withLabel(d"label")(pass(d"yes", ())), trace1) {
        s"""label:
           |  yes
           |""".stripMargin ->
          List(() -> Some(1))
      }
    }
    it("labels violations") {
      checkFail(withLabel(d"label")(violation(d"no")), trace1) {
        s"""label:
           |  no
           |""".stripMargin
      }
    }
  }

  describe("disjunction combinator") {
    it("passes if the left-hand side passes") {
      checkPass(pass(d"yes L", '@') | violation(d"no R"), trace1) {
        s"""inclusive OR partial success:
           |  success:
           |    yes L
           |  failure:
           |    no R
           |""".stripMargin ->
          List('@' -> Some(1))
      }
    }
    it("passes if the right-hand side passes") {
      checkPass(violation(d"no L") | pass(d"yes R", '@'), trace1) {
        s"""inclusive OR partial success:
           |  failure:
           |    no L
           |  success:
           |    yes R
           |""".stripMargin ->
          List('@' -> Some(1))
      }
    }
    it("passes if both sides pass") {
      checkPass(pass(d"yes L", "@1") | pass(d"yes R", "@2"), trace1) {
        s"""inclusive OR:
           |  yes L
           |  yes R
           |""".stripMargin ->
          List(
            "@1" -> Some(1),
            "@2" -> Some(1))
      }
    }
    it("fails if neither side passes") {
      checkFail(violation(d"no L") | violation(d"no R"), trace1) {
        s"""inclusive OR failed:
           |  no L
           |  no R
           |""".stripMargin
      }
    }
  }

  describe("accumulated combinator") {
    it("provides access to the complete lazy list of results, while consuming no input") {
      implicit val ctx = SearchContext.Default
      checkPass(anyOf(check(d"2 or 4")(x => x == 2 || x == 4)).accumulated.map(_.mkString(", ")), trace1) {
        s"""accumulating (without consuming trace):
           |  unconditional search:
           |    2 or 4: matched at 2
           |    2 or 4: matched at 4
           |""".stripMargin ->
          List("2, 4" -> Some(1))
      }
    }
    it("does not force the underlying list") {
      implicit val ctx = SearchContext.Default
      checkPass(anyOf(check(d"2 or 4")(x => x == 2 || x == 4)).accumulated.map(_ => ()), trace1) {
        s"""accumulating (without consuming trace):
           |  unconditional search:
           |    2 or 4: matched at 2
           |    ...
           |""".stripMargin ->
          List(() -> Some(1))
      }
    }
    it("fails if the underlying combinator fails") {
      checkFail(violation(d"no").accumulated, trace1) {
        s"""accumulating (without consuming trace):
           |  no
           |""".stripMargin
      }
    }
  }

  describe("flatten combinator") {
    it("flattens an iterable result into multiple results at the same position") {
      checkPass(pass(d"pass 1, 2, 3", List(1, 2, 3)).flatten, trace1) {
        s"""flattening:
           |  pass 1, 2, 3
           |""".stripMargin ->
          List(1 -> Some(1), 2 -> Some(1), 3 -> Some(1))
      }
    }
    it("preserves laziness, within reason") {
      implicit val ctx = SearchContext.Default
      checkPass(guard(anyOf(check(d"2 or 4")(x => x == 2 || x == 4))).flatten.first, trace1) {
        s"""first of:
           |  flattening:
           |    guarding (check without consuming input):
           |      unconditional search:
           |        2 or 4: matched at 2
           |        ...
           |""".stripMargin ->
          List(2 -> Some(1))
      }
    }
  }

  describe("backwards combinator") {
    it("allows look-behind") {
      implicit val ctx = SearchContext.Default
      checkPass(check(d"1")(_ == 1) ~> backwards(check(d"1 backwards")(_ == 1)), trace1) {
        s"""initial success:
           |  1: matched at 1
           |followed by success(es) based on [1]:
           |  backwards search:
           |    1 backwards: matched at 1""".stripMargin ->
          List(1 -> None)
      }
    }
    it("is self-inverse") {
      implicit val ctx = SearchContext.Default
      checkPass(check(d"1")(_ == 1) ~> backwards(backwards(check(d"2 backwards backwards")(_ == 2))), trace1) {
        s"""initial success:
           |  1: matched at 1
           |followed by success(es) based on [1]:
           |  backwards search:
           |    backwards search:
           |      2 backwards backwards: matched at 2""".stripMargin ->
          List(2 -> Some(3))
      }
    }
    it("handles edge case: beginning of trace") {
      checkFail(backwards(AnyElement), trace1) {
        s"""backwards search:
           |  beginning of trace
           |""".stripMargin
      }
    }
    it("handles edge case: end of trace") {
      implicit val ctx = SearchContext.Default
      checkPass(eventually(check(d"10")(_ == 10)) ~> backwards(check(d"10 backwards")(_ == 10)), trace1) {
        s"""initial success:
           |  eventually holds:
           |    10: matched at 10
           |followed by success(es) based on [10]:
           |  backwards search:
           |    10 backwards: matched at 10
           |""".stripMargin ->
          List(10 -> Some(9))
      }
    }
  }

  describe("induct combinator") {
    implicit val ctx = SearchContext.Default
    it("can prove that every trace element is one greater than the last") {
      checkPass(induct(0) { prev =>
        check(d"$prev + 1")(_ == prev + 1)
      }, trace1) {
        s"""reasoning by induction, starting at 0:
           |  initial success:
           |    0 + 1: matched at 1
           |  followed by success(es) based on [1]:
           |    initial success:
           |      1 + 1: matched at 2
           |    followed by success(es) based on [2]:
           |      initial success:
           |        2 + 1: matched at 3
           |      followed by success(es) based on [3]:
           |        initial success:
           |          3 + 1: matched at 4
           |        followed by success(es) based on [4]:
           |          initial success:
           |            4 + 1: matched at 5
           |          followed by success(es) based on [5]:
           |            initial success:
           |              5 + 1: matched at 6
           |            followed by success(es) based on [6]:
           |              initial success:
           |                6 + 1: matched at 7
           |              followed by success(es) based on [7]:
           |                initial success:
           |                  7 + 1: matched at 8
           |                followed by success(es) based on [8]:
           |                  initial success:
           |                    8 + 1: matched at 9
           |                  followed by success(es) based on [9]:
           |                    initial success:
           |                      9 + 1: matched at 10
           |                    followed by success(es) based on [10]:
           |                      successfully inducted to end of trace
           |""".stripMargin -> Nil
      }
    }

    it("can disprove that every trace element is one greater than the last") {
      checkFail(induct(0) { prev =>
        check(d"$prev + 1")(_ == prev + 1)
      }, Scanner(1, 2, 4, 5, 6, 7)) {
        s"""reasoning by induction, starting at 0:
           |  initial success:
           |    0 + 1: matched at 1
           |  followed by failure based on [1]:
           |    initial success:
           |      1 + 1: matched at 2
           |    followed by failure based on [2]:
           |      2 + 1: failed at 4
           |""".stripMargin
      }
    }

    it("can stop early, if a condition succeeds with no further examples") {
      checkPass(induct(0) { prev =>
        anyOf(check(d"$prev + 1")(_ == prev + 1)).first
      }, Scanner(1, 2, 4, 5, 6, 7)) {
        s"""reasoning by induction, starting at 0:
           |  initial success:
           |    first of:
           |      unconditional search:
           |        0 + 1: matched at 1
           |  followed by success(es) based on [1]:
           |    initial success:
           |      first of:
           |        unconditional search:
           |          1 + 1: matched at 2
           |    followed by success(es) based on [2]:
           |      initial success:
           |        first of:
           |          unconditional search: no results
           |""".stripMargin -> Nil
      }
    }
  }

  describe("opt combinator") {
    implicit val ctx = SearchContext.Default
    it("converts success to Some") {
      checkPass(opt(pass(d"yes", 42)), trace1) {
        s"""optionally:
           |  yes
           |""".stripMargin ->
          List(Some(42) -> Some(1))
      }
    }
    it("converts failure to None") {
      checkPass(opt(violation(d"no")), trace1) {
        s"""optionally:
           |  no
           |""".stripMargin ->
          List(None -> Some(1))
      }
    }
  }
}
