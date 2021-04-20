package ca.ubc.cs.tracechecker

import scala.annotation.showAsInfix
import scala.collection.{View, mutable}

trait TraceChecking {
  type TraceElement
  type Input = Scanner[TraceElement]

  import Description._

  def verbose: Boolean = false

  @showAsInfix
  final case class ~[+T, +U](_1: T, _2: U) {
    override def toString: String = s"(${_1}~${_2})"
  }

  sealed abstract class TraceOutcome[+T] {
    def description: Description
    def map[U](fn: T=>U): TraceOutcome[U]
    def flatMapWithNext[U](fn: T => Input => TraceOutcome[U]): TraceOutcome[U]
    def mapPassDescription(fn: Description => Description): TraceOutcome[T]
    def mapViolationDescription(fn: Description => Description): TraceOutcome[T]
  }

  final case class TraceSuccess[+T](result: T, rest: Input) {
    def map[U](fn: T => U): TraceSuccess[U] =
      copy(result = fn(result))
  }

  final case class TracePassed[+T](description: Description, successes: LazyList[TraceSuccess[T]]) extends TraceOutcome[T] {
    override def map[U](fn: T => U): TraceOutcome[U] =
      copy(successes = successes.map(_.map(fn)))

    override def flatMapWithNext[U](fn: T => Input => TraceOutcome[U]): TraceOutcome[U] = {
      val mappedResults = successes.map(succ => (succ.result, fn(succ.result)(succ.rest)))
      val mappedSuccesses = mappedResults.collect { case (t, tp: TracePassed[U]) => (t, tp) }
      val mappedViolation = mappedResults.collectFirst { case (t, tv: TraceViolation) => (t, tv) }
      mappedViolation match {
        case Some((violationCause, violation)) =>
          violation.copy(description = d"initial success:\n${
            description.indented
          }\nfollowed by failure based on [$violationCause]:\n${violation.description.indented}")
        case None =>
          TracePassed(
            d"initial success:\n${description.indented}${
              mappedSuccesses.map { succ =>
                d"followed by success(es) based on [${succ._1}]:\n${succ._2.description.indented}"
                  .ensureLineBreakBefore
              }.flattenDescriptions
            }",
            mappedSuccesses.flatMap(_._2.successes))
      }
    }

    override def mapPassDescription(fn: Description => Description): TraceOutcome[T] =
      copy(description = fn(description))

    override def mapViolationDescription(fn: Description => Description): TraceOutcome[T] = this
  }

  final case class TraceViolation(description: Description) extends TraceOutcome[Nothing] {
    override def map[U](fn: Nothing => U): TraceOutcome[U] = this

    override def flatMapWithNext[U](fn: Nothing => Input => TraceOutcome[U]): TraceOutcome[U] = this

    override def mapPassDescription(fn: Description => Description): TraceOutcome[Nothing] = this

    override def mapViolationDescription(fn: Description => Description): TraceOutcome[Nothing] =
      copy(description = fn(description))
  }

  /**
   * A TraceChecker[T] is somewhat similar to a Parser[T] from a parser combinator library.
   *
   * The key differences are:
   * 1. TraceChecker will yield all possible interpretations of an input, always, whereas a Parser tends to seek one answer.
   * 2. TraceChecker will output a complete description of the "tree" it navigated to reach a conclusion, whereas a Parser is more concerned with the result T.
   *
   * The reason for these differences is that, unlike in parsing proper, where the grammar being parser is considered "known",
   * it is not necessarily obvious what specification (or part of specification) a TraceChecker is checking.
   * Therefore, it is important to encode as much information as possible about what happened when giving a result.
   *
   * @tparam T the element type yielded by the checker
   */
  abstract class TraceChecker[+T] extends (Input => TraceOutcome[T]) { self =>
    override def apply(input: Input): TraceOutcome[T]

    def map[U](fn: T => U): TraceChecker[U] =
      TraceChecker { input => self(input).map(fn) }

    def flatMap[U](fn: T => TraceChecker[U]): TraceChecker[U] =
      TraceChecker { input => self(input).flatMapWithNext(fn) }

    def mapPassDescription(fn: Description => Description): TraceChecker[T] =
      TraceChecker { input => self(input).mapPassDescription(fn) }

    def mapViolationDescription(fn: Description => Description): TraceChecker[T] =
      TraceChecker { input => self(input).mapViolationDescription(fn) }

    def mapDescription(fn: Description => Description): TraceChecker[T] =
      self.mapPassDescription(fn).mapViolationDescription(fn)

    /**
     * @param description a human-readable description of the condition being required
     * @param pred a predicate to apply to the underling checker's result
     * @return a new checker that performs all checks in self, then checks that all results satisfy pred
     */
    def require(description: Description)(pred: T => Boolean): TraceChecker[T] =
      self.flatMap {
        case result if pred(result) => pass(d"requirement [$description] succeeded", result)
        case _ => violation(d"requirement [$description] failed")
      }

    /**
     * Converts between a checker with many results, and a checker with one result, which is a list of many results.
     */
    def accumulated: TraceChecker[LazyList[T]] =
      TraceChecker { input =>
        self(input) match {
          case TracePassed(description, successes) =>
            val results = successes.map(_.result)
            TracePassed(d"accumulating (without consuming trace):\n${description.indented}",
              LazyList(TraceSuccess(results, input)))
          case TraceViolation(description) =>
            TraceViolation(d"accumulating (without consuming trace):\n${description.indented}")
        }
      }

    /**
     * The conceptual inverse of .accumulated. Maps from a checker that results in some iterable, to
     * a checker with many results, each of which is an element of the iterable.
     */
    def flatten[U](implicit ev: T <:< Iterable[U]): TraceChecker[U] =
      TraceChecker { input =>
        self(input) match {
          case TracePassed(description, successes) =>
            TracePassed(d"flattening:\n${description.indented}",
              successes.flatMap(succ => succ.result.map(TraceSuccess(_, succ.rest))))
          case TraceViolation(description) =>
            TraceViolation(d"flattening:\n${description.indented}")
        }
      }

    /**
     * Yield (and therefore check up to) only the first result of the underlying checker.
     */
    def first: TraceChecker[T] =
      TraceChecker { input =>
        self(input) match {
          case TracePassed(description, successes) =>
            TracePassed(d"first of:\n${description.indented}", if(successes.nonEmpty) LazyList(successes.head) else LazyList.empty)
          case TraceViolation(description) =>
            TraceViolation(d"first of:\n${description.indented}")
        }
      }

    def requireCount(description: Description)(pred: Int => Boolean): TraceChecker[T] = {
      TraceChecker { input =>
        self(input) match {
          case TracePassed(desc, successes) if pred(successes.size) =>
            TracePassed(d"$description:\n${desc.indented}", successes)
          case TracePassed(desc, _) => TraceViolation(d"$description:\n${desc.indented}")
          case TraceViolation(desc) =>
            TraceViolation(d"$description:\n${desc.indented}")
        }
      }
    }

    def ~[U](other: => TraceChecker[U]): TraceChecker[T ~ U] = {
      lazy val other_ = other
      for {t <- this; u <- other_} yield new ~(t, u)
    }

    def ~>[U](other: => TraceChecker[U]): TraceChecker[U] = {
      lazy val other_ = other
      for {_ <- this; u <- other_} yield u
    }

    def <~(other: => TraceChecker[Any]): TraceChecker[T] = {
      lazy val other_ = other
      for {t <- this; _ <- other_} yield t
    }

    /**
     * Combines two checkers, such that either checker producing a positive result is sufficient for the overall checker
     * to produce a positive result. Roughly equivalent to logical OR.
     */
    def |[U >: T](other: => TraceChecker[U]): TraceChecker[U] = {
      lazy val other_ = other
      TraceChecker { input =>
        self(input) match {
          case TracePassed(descriptionL, successesL) =>
            other_(input) match {
              case TracePassed(descriptionR, successesR) =>
                TracePassed(d"inclusive OR:\n${descriptionL.indented}\n${descriptionR.indented}", successesL #::: successesR)
              case TraceViolation(descriptionR) =>
                TracePassed(d"inclusive OR partial success:\n${d"success:\n${descriptionL.indented}\nfailure:\n${descriptionR.indented}".indented}",
                  successesL)
            }
          case TraceViolation(descriptionL) =>
            other_(input) match {
              case TracePassed(descriptionR, successesR) =>
                TracePassed(d"inclusive OR partial success:\n${d"failure:\n${descriptionL.indented}\nsuccess:\n${descriptionR.indented}".indented}",
                  successesR)
              case TraceViolation(descriptionR) =>
                TraceViolation(d"inclusive OR failed:\n${descriptionL.indented}\n${descriptionR.indented}")
            }
        }
      }
    }
  }

  object TraceChecker {
    def apply[T](fn: Input => TraceOutcome[T]): TraceChecker[T] = (input: Input) => fn(input)
  }

  def withLabel[T](description: Description)(tc: TraceChecker[T]): TraceChecker[T] =
    tc.mapDescription(desc => d"$description:\n${desc.indented}")

  def pass[T](description: Description, result: T): TraceChecker[T] =
    TraceChecker { input => TracePassed(description, LazyList(TraceSuccess(result, input))) }

  def violation(description: Description): TraceChecker[Nothing] =
    TraceChecker { _ => TraceViolation(description) }

  /**
   * Succeeds if, scanning along the underlying trace, the checker tc is satisfied at any position.
   * Yields all positions at which tc is satisfied, along with any related values.
   */
  def anyOf[T](tc: =>TraceChecker[T]): TraceChecker[T] = {
    lazy val tc_ = tc
    TraceChecker { input =>
      // use side-effects to only list descriptions of results that were actually used
      val passDescriptions = mutable.ListBuffer[Description]()
      val passIterator = input.tails.map(tc_).collect {
        case tp: TracePassed[T] =>
          passDescriptions += tp.description
          tp
      }
      val passes = passIterator.to(LazyList)
      if(passes.isEmpty) {
        TracePassed(d"unconditional search: no results", LazyList.empty)
      } else {
        TracePassed(d"unconditional search:\n${
          val maybeEllipsis = View.fromIteratorProvider { () =>
            Iterator.single(if(passIterator.hasNext) d"...".indented.ensureLineBreakBefore else d"")
          }
          (passDescriptions.view.map(_.indented.ensureLineBreakBefore) ++ maybeEllipsis).flattenDescriptions
        }", passes.flatMap(_.successes))
      }
    }
  }

  /**
   * Like anyOf, but requires that tc be satisfied by at least one position. Will either fail, or will yield at least one result.
   */
  def eventually[T](tc: =>TraceChecker[T]): TraceChecker[T] = {
    lazy val tc_ = tc
    TraceChecker { input =>
      val outcomes = input.tails.map(tc_).to(LazyList)
      val passDescriptions = mutable.ListBuffer[Description]()
      val passesIterator = outcomes.iterator.collect {
        case tp: TracePassed[T] =>
          passDescriptions += tp.description
          tp
      }
      val passes = passesIterator.to(LazyList)
      if(passes.isEmpty) {
        TraceViolation(d"does not eventually hold:\n${
          if(outcomes.size > 3 && !verbose) {
            d"${outcomes.head.description}\n...\n${outcomes.last.description}".indented
          } else {
            outcomes.view.map(_.description.indented.ensureLineBreakBefore).flattenDescriptions
          }
        }")
      } else {
        TracePassed(d"eventually holds:\n${
          val maybeEllipsis = View.fromIteratorProvider { () =>
            Iterator.single(if(passesIterator.hasNext) d"...".indented.ensureLineBreakBefore else d"")
          }
          (passDescriptions.view.map(_.indented.ensureLineBreakBefore) ++ maybeEllipsis).flattenDescriptions
        }", passes.flatMap(_.successes))
      }
    }
  }

  object AnyElement extends TraceChecker[TraceElement] {
    private val tv1 = TraceViolation(d"end of trace")
    private val tv2 = TraceViolation(d"beginning of trace")
    override def apply(input: Input): TraceOutcome[TraceElement] = {
      if(input.isEmpty) {
        if(input.isFlipped) tv2 else tv1
      } else {
        TracePassed(d"any trace element: ${input.head}", LazyList(TraceSuccess(input.head, input.tail)))
      }
    }
  }

  /**
   * Apply tc, but yield a list of tc's results without consuming any input. Related to parser look-ahead.
   */
  def guard[T](tc: TraceChecker[T]): TraceChecker[LazyList[T]] = {
    lazy val tc_ = tc
    TraceChecker { input =>
      tc_(input) match {
        case TracePassed(description, successes) =>
          TracePassed(d"guarding (check without consuming input):\n${description.indented}",
            LazyList(TraceSuccess(successes.map(_.result), input)))
        case TraceViolation(description) =>
          TraceViolation(d"guarding (check without consuming input) failed:\n${description.indented}")
      }
    }
  }

  /**
   * Same as guard, but operating on a list of underlying checkers, somewhat like a logical AND.
   * All of tc must be satisfied in order for the resulting checker to be satisfied.
   */
  def guards[T](tc: TraceChecker[T]*): TraceChecker[List[LazyList[T]]] =
    TraceChecker { input =>
      val results = tc.map(_(input))
      val violations = results.collect { case tv: TraceViolation => tv }
      if(violations.nonEmpty) {
        TraceViolation(d"guarding (check without consuming input) failed:\n${
          violations.map(_.description.indented.ensureLineBreakBefore).flattenDescriptions
        }")
      } else {
        val collectedResults = results.view.collect {
          case TracePassed(_, successes) => successes.map(_.result)
        }.toList
        TracePassed(d"guarding (check without consuming input):\n${results.map(_.description.indented.ensureLineBreakBefore).flattenDescriptions}",
          LazyList(TraceSuccess(collectedResults, input)))
      }
    }

  /**
   * Checks, at a single position, that the underlying element satisfied each of pred.
   *
   * Additionally, ctx is used to contextually rule out irrelevant elements that would otherwise have matched.
   */
  def check(description: Description)(pred: TraceElement=>Boolean*)(implicit ctx: SearchContext[TraceElement]): TraceChecker[TraceElement] =
    TraceChecker {
      case input if input.isEmpty =>
        if(input.isFlipped) {
          TraceViolation(d"$description: beginning of trace")
        } else {
          TraceViolation(d"$description: end of trace")
        }
      case input if !ctx(input.head) =>
        TraceViolation(d"$description: irrelevant at ${input.head}")
      case input if ctx(input.head) && pred.forall(_(input.head)) =>
        TracePassed(d"$description: matched at ${input.head}", LazyList(TraceSuccess(input.head, input.tail)))
      case input =>
        TraceViolation(d"$description: failed at ${input.head}")
    }

  def once[T](tc: TraceChecker[T]): TraceChecker[T] =
    tc.requireCount(d"exactly once")(_ == 1)

  /**
   * Similar to logical negation. If tc fails, it succeeds. If tc succeeds, it fails.
   * It does not consume input.
   */
  def not(tc: => TraceChecker[Any]): TraceChecker[Unit] = {
    lazy val tc_ = tc
    TraceChecker { input =>
      tc_(input) match {
        case TracePassed(description, _) =>
          TraceViolation(d"not [violation]:\n${description.indented}")
        case TraceViolation(description) =>
          TracePassed(d"not [success]:\n${description.indented}", LazyList(TraceSuccess((), input)))
      }
    }
  }

  /**
   * Checks tc "backwards", i.e passing to tc a flipped version of the underlying scanner.
   *
   * This allows for look-behind assertions, and should generally be used in conjunction with guard, to avoid
   * confusion about checking "direction".
   */
  def backwards[T](tc: =>TraceChecker[T]): TraceChecker[T] = {
    lazy val tc_ = tc.mapDescription(desc => d"backwards search:\n${desc.indented}")
    TraceChecker(input => tc_(input.flipped))
  }

  /**
   * Optionally matches tc. If tc does not match, yields a single None without consuming input.
   * If tc does match, yields Some of all tc's results.
   */
  def opt[T](tc: =>TraceChecker[T]): TraceChecker[Option[T]] = {
    lazy val tc_ = tc.mapDescription(desc => d"optionally:\n${desc.indented}")
    TraceChecker { input =>
      tc_(input) match {
        case TracePassed(description, successes) =>
          TracePassed(description, successes.map { case TraceSuccess(v, input) => TraceSuccess(Some(v), input) })
        case TraceViolation(description) =>
          TracePassed(description, LazyList(TraceSuccess(None, input)))
      }
    }
  }

  /**
   * A rough parallel to logical induction. Starting with the value init, checks fn(init), and,
   * for any result elem, checks fn(elem), then, for any result elem', checks fn(elem'), and so forth.
   *
   * This, combined with careful use of .first, allows expressing more sophisticated patterns that depend on previous matches, such as alternating ones,
   * for which the condition might be "not like the previous element".
   */
  def induct[T](init: =>T)(fn: T => TraceChecker[T]): TraceChecker[T] = {
    lazy val init_ = init
    withLabel(d"reasoning by induction, starting at $init_") {
      def impl(init: T): TraceChecker[T] =
        TraceChecker {
          case input if input.isEmpty =>
            TracePassed(d"successfully inducted to end of trace", LazyList.empty)
          case input =>
            fn(init).flatMap(impl)(input)
        }

      impl(init_)
    }
  }
}
