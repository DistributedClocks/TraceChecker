package com.github.distributedclocks.tracechecker

import fansi._

import scala.collection.{View, mutable}
import scala.reflect.ClassTag

abstract class Specification[E <: Element : ElementParser : ClassTag] extends Queries {
  /**
   * Query for accessing all elements of type E in all traces loaded from the current log.
   */
  final val elements: Query[List[E]] =
    materialize {
      val eTag = implicitly[ClassTag[E]]
      rawElements.map(_.collect { case eTag(e) => e })
    }

  /**
   * Query for accessing all traces in the current log, encoded as pairs of trace ID and trace elements.
   * Like elements, this will contain only elements of type E.
   */
  final val traces: Query[List[(String,List[E])]] =
    materialize {
      elements.map { elements =>
        elements.groupBy(_.traceId)
          .toList
          .sortBy(_._1) // sort for deterministic output
      }
    }

  import Specification._

  def rootRule: RootRule

  /**
   * Specifies a single rule, whose semantics are defined by an underlying query.
   * If the query succeeds, the rule is satisfied. Otherwise, the rule is not satisfied.
   *
   * Optionally, a long-form description may be provided, as well as a point value.
   * Not providing a point value means operations involving points will be non-functional, and should not be used.
   */
  final def rule(name: String, desc: String = "", pointValue: Double = -1)(query: Query[Any])(implicit positionInfo: PositionInfo): Rule =
    new SingleRule(
      name = name,
      query = query,
      desc = if(desc != "") Some(desc) else None,
      pointValue = if(pointValue >= 0) Some(pointValue) else None)

  /**
   * Specifies a compound rule, whose semantics depends on all of the subRules.
   *
   * A long-form description may optionally be provided.
   * Additionally, a pointValue may be provided. If the pointValue is provided, it will be used as a scaling factor
   * out of which all the sub-rules will be counted.
   * If no pointValue is provided, the rule will count the sum of all points scored by the sub-rules.
   */
  final def multiRule(name: String, desc: String = "", pointValue: Double = -1)(subRules: Rule*): Rule =
    new MultiRule(
      name = name,
      desc = if(desc != "") Some(desc) else None,
      pointValue = if(pointValue >= 0) Some(pointValue) else None,
      rules = subRules.toList)

  /**
   * Check each log path against rootRule, and report the outcome using RuleResults.
   */
  final def checkRules(logPaths: os.Path*): RuleResults = {
    val elementParser = implicitly[ElementParser[E]]
    rootRule.exec(logPaths.iterator.map { path =>
      RuleConfig(
        logName = path.last,
        queryState = new QueryContext.State(elements = elementParser(os.read.lines(path))))
    }.toList)
  }
}

object Specification {
  val checkMark: Str = Str("✓").overlay(Color.Green)
  val crossMark: Str = Str("❌").overlay(Color.Red)
  val asciiCheckMark: String = "[x]"
  val asciiCrossMark: String = "[ ]"

  final case class RuleConfig(logName: String, queryState: QueryContext.State)

  sealed abstract class Rule {
    def name: String
    def positionInfo: PositionInfo
    def availablePts: Double
    def exec(configs: List[RuleConfig]): RuleResults
  }

  sealed abstract class RuleResults {
    def success: Boolean
    def ruleList(indent: Int = 0): Iterator[Str]
    def counterExamples(prefix: String = "", indent: Int = 0): Iterator[Str]
    def grade: Double
    def dump(indent: Int = 0): Iterator[String]
  }

  trait CommonRuleOps {
    def name: String
    def desc: Option[String]

    def renderRuleListHeading(indent: Int, success: Boolean): Iterator[Str] =
      Iterator.fill(indent)(Str("  ")) ++
        Iterator[Str](if(success) checkMark else crossMark, " ", name, desc.map(desc => Str(s": $desc")).getOrElse(Str("")), "\n")

    def renderRuleListAscii(indent: Int, success: Boolean): Iterator[String] =
        Iterator[String](if(success) asciiCheckMark else asciiCrossMark, " ", "  " * indent, name, desc.getOrElse(""), "\n")
  }

  trait CommonMultiRuleOps {
    protected val pointValue: Option[Double]
    protected val naiveAvailablePts: Double

    protected def calculateGrade(results: List[RuleResults]): Double = {
      val rawGrade = results.iterator.map(_.grade).sum
      pointValue match {
        case None => rawGrade
        case Some(pointValue) => rawGrade / naiveAvailablePts * pointValue
      }
    }
  }

  final class RootRule private (rules: List[Rule], protected val pointValue: Option[Double]) extends CommonMultiRuleOps {
    protected lazy val naiveAvailablePts: Double = rules.iterator.map(_.availablePts).sum

    lazy val availablePts: Double = pointValue.getOrElse(naiveAvailablePts)

    def exec(configs: List[RuleConfig]): RuleResults = {
      val results = rules.map(_.exec(configs))
      new RuleResults {
        val success: Boolean = results.forall(_.success)

        override def ruleList(indent: Int): Iterator[Str] =
          Iterator.fill(indent)(Str("  ")) ++ Iterator[Str]("rule list (", checkMark, " for ok, ", crossMark, " for not ok):\n") ++
            results.iterator.flatMap(_.ruleList(indent = indent + 1))

        override def counterExamples(prefix: String, indent: Int): Iterator[Str] =
          results.iterator.flatMap(_.counterExamples(prefix = prefix, indent = indent))

        override def grade: Double = calculateGrade(results)

        override def dump(indent: Int): Iterator[String] = results.iterator.flatMap(_.dump(indent))
      }
    }
  }

  object RootRule {
    def apply(rules: Rule*): RootRule =
      new RootRule(rules = rules.toList, pointValue = None)

    def apply(pointValue: Double)(rules: Rule*): RootRule =
      new RootRule(rules = rules.toList, pointValue = Some(pointValue))
  }

  private final class SingleRule(val name: String, query: Query[Any], val desc: Option[String], pointValue: Option[Double])(implicit val positionInfo: PositionInfo) extends Rule with CommonRuleOps {
    def availablePts: Double = {
      require(pointValue.nonEmpty, s"rule $name defined at $positionInfo does not have a point value")
      pointValue.get
    }

    def exec(configs: List[RuleConfig]): RuleResults = {
      val results = configs.map { config =>
        query(QueryContext(state = config.queryState))
      }

      new RuleResults {
        val success: Boolean = results.forall { case Accept(_, _) => true; case _ => false }

        override def ruleList(indent: Int): Iterator[Str] = renderRuleListHeading(indent, success)

        override def counterExamples(prefix: String, indent: Int): Iterator[Str] =
          if(success) {
            Iterator.empty
          } else {
            // the pprinter's usual methods won't pprint starting at a set indentation; this rough copy-paste of
            // tokenize forces the matter by providing a non-0 indentCount to start (relying on our indents and their indents both == 2)
            def wranglePPrinter(x: Any, indentCount: Int): Iterator[Str] = {
              val tree = prettyprint.treeify(x, prettyprint.defaultEscapeUnicode, prettyprint.defaultShowFieldNames)
              val renderer = new pprint.Renderer(prettyprint.defaultWidth, prettyprint.colorApplyPrefix, prettyprint.colorLiteral, prettyprint.defaultIndent)
              val rendered = renderer.rec(tree, 0, indentCount = indentCount).iter
              // Truncate the output stream once it's wrapped-at-width height goes
              // beyond the desired height
              val truncated = new pprint.Truncated(rendered, prettyprint.defaultWidth, prettyprint.defaultHeight)
              truncated
            }

            val (configIdx, Reject(msg, ctx, relatedValues, positionInfo)) =
              results.view.zipWithIndex.collectFirst { case (r@Reject(_, _, _, _), idx) => (idx, r) }.get

            var maxIndent: Int = indent

            def renderContext(ctx: QueryContext, indent: Int): Iterator[Str] = {
              maxIndent = Math.max(maxIndent, indent)
              ctx.entrySeq.reverseIterator.flatMap { entryName =>
                Iterator.fill(indent)(Str("  ")) ++ (ctx.entries(entryName) match {
                  case QueryContext.ValueEntry(value) =>
                    Iterator.single(Str(s"$entryName := ")) ++ wranglePPrinter(value, indentCount = indent) ++ Iterator.single(Str("\n"))
                  case QueryContext.GroupEntry(ctx) =>
                    Iterator.single(Str(s"$entryName:\n")) ++
                      renderContext(ctx, indent = indent + 1)
                })
              }
            }

            Iterator.fill(indent)(Str("  ")) ++ Iterator[Str](crossMark, " ", prefix, name, " (file ", configs(configIdx).logName, "):\n") ++
              renderContext(ctx, indent = indent + 1) ++
              View.fromIteratorProvider(() => Iterator.fill(maxIndent + 1)(Str("  "))) ++ Iterator[Str](Str(msg).overlay(Color.Red).overlay(Bold.On), " at ", positionInfo.toString, "\n") ++
              (if(relatedValues.nonEmpty) {
                (View.fromIteratorProvider(() => Iterator.fill(maxIndent + 1)(Str("  "))) ++ Iterator[Str]("contextually relevant values: ") ++ wranglePPrinter(relatedValues, indentCount = maxIndent + 1) ++ Iterator[Str]("\n"))
                  .iterator
              } else Iterator.empty[Str])
          }

        override def grade: Double = {
          val pts = availablePts
          if(success) pts else 0
        }

        override def dump(indent: Int): Iterator[String] = renderRuleListAscii(indent, success)
      }
    }
  }

  private final class MultiRule(val name: String, rules: List[Rule], val desc: Option[String], protected val pointValue: Option[Double])(implicit val positionInfo: PositionInfo) extends Rule with CommonRuleOps with CommonMultiRuleOps {
    locally {
      val namesSeen = mutable.HashMap[String,Rule]()
      rules.foreach { rule =>
        require(!namesSeen.contains(rule.name), s"rule ${rule.name} (defined at ${rule.positionInfo}) conflicts with name of rule defined at ${namesSeen(rule.name).positionInfo}")
      }
    }

    protected lazy val naiveAvailablePts: Double = rules.iterator.map(_.availablePts).sum

    lazy val availablePts: Double = pointValue.getOrElse(naiveAvailablePts)

    def exec(configs: List[RuleConfig]): RuleResults = {
      val results = rules.map(_.exec(configs))
      new RuleResults {
        val success: Boolean = results.forall(_.success)

        override def ruleList(indent: Int): Iterator[Str] =
          renderRuleListHeading(indent, success) ++
            results.iterator.flatMap(_.ruleList(indent = indent + 1))

        override def counterExamples(prefix: String, indent: Int): Iterator[Str] =
          results.iterator.flatMap(_.counterExamples(prefix = s"$prefix$name -:- ", indent = indent))

        override def grade: Double = calculateGrade(results)

        override def dump(indent: Int): Iterator[String] = renderRuleListAscii(indent, success) ++ results.iterator.flatMap(_.dump(indent+1))
      }
    }
  }

  // TODO: add another class called EitherRule
}
