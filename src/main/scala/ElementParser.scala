package com.github.distributedclocks.tracechecker

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.language.experimental.macros
import scala.reflect.api
import scala.reflect.api.{TreeCreator, Universe}
import scala.reflect.macros.blackbox

abstract class ElementParser[E <: Element] {
  def apply(lines: IterableOnce[String]): List[Element]
}

object ElementParser {
  final case class ParsingException(badValue: JsValue, validTags: List[String]) extends Exception(s"could not find valid tag in $badValue. valid tags are: ${validTags.mkString(", ")}")

  sealed abstract class RecordParser {
    def tagName: String
    def tryParse(v: JsObject, lineNumber: Int): Option[Element]
  }

  final class SpecialisedRecordParser[E <: Element : JsonFormat](val tagName: String) extends RecordParser {
    override def tryParse(v: JsObject, lineNumber: Int): Option[Element] =
      v match {
        case js if js.fields("Tag").convertTo[String] == tagName =>
          Some(
            js.fields("Body").convertTo[E]
              .setLineNumber(lineNumber)
              .setTraceId(js.fields("TraceID").convertTo[Long].toString)
              .setVectorClock(js.fields("VectorClock").convertTo[Map[String,Long]])
              .setTracerIdentity(js.fields("TracerIdentity").convertTo[String]))
        case _ => None
      }
  }

//  def applyParsers(line: String, lineNumber: Int)(parsers: List[RecordParser]): Element = {
//    val obj = line.parseJson.asJsObject
//    parsers.foldLeft(None: Option[Element]) { (acc, parser) =>
//      acc.orElse(parser.tryParse(obj, lineNumber))
//    }.getOrElse {
//      throw ParsingException(obj, parsers.map(_.tagName))
//    }
//  }

  implicit def findElementParser[E <: Element]: ElementParser[E] = macro findElementParserImpl[E]

  def findElementParserImpl[E <: Element: c.WeakTypeTag](c: blackbox.Context): c.Expr[ElementParser[E]] = {
    import c.universe._

    val sym = implicitly[c.WeakTypeTag[E]].tpe.typeSymbol
    if(!sym.isClass) {
      c.abort(c.enclosingPosition, s"${sym.fullName} must be a class or trait")
    }
    val clsSym = sym.asClass
    if(!clsSym.isSealed) {
      c.abort(c.enclosingPosition, s"${sym.fullName} must be sealed")
    }

    val classesToParse: List[ClassSymbol] =
      List(typeOf[Element.CreateTrace], typeOf[Element.ReceiveTokenTrace], typeOf[Element.GenerateTokenTrace]).map(_.typeSymbol.asClass) :::
        clsSym.knownDirectSubclasses.iterator.map(_.asClass).toList

    val listOfRecordParserExprs: List[Expr[RecordParser]] =
      classesToParse.map { cls =>
        val constructor = cls.primaryConstructor.asMethod
        val argss = constructor.paramLists
        if(argss.size != 1) {
          c.error(c.enclosingPosition, s"one or more subclasses of ${sym.fullName} have more than one argument list. this is not currently supported")
          c.error(cls.pos, "note: this class must have exactly one argument list")
        }
        val args = argss.headOption.getOrElse(Nil)
        val argNames = args.map(_.name.toString.capitalize) // little trick to make Scala and Go naming conventions match

        if(!cls.isStatic) {
          c.error(c.enclosingPosition, s"one or more subclasses of ${sym.fullName} are not statically accessible")
          c.error(cls.pos, "note: this class needs to be statically accessible")
        }

        val companionRef = c.internal.gen.mkAttributedRef(cls.companion)

        // find and synthesize a call to the correct jsonFormat helper
        val protocolSym = symbolOf[DefaultJsonProtocol].asClass
        val jsonFormatName = TermName(if(argNames.isEmpty) "jsonFormat0" else "jsonFormat")
        val implicitTree: c.Tree = c.internal.gen.mkMethodCall(protocolSym.companion, jsonFormatName, companionRef :: argNames.map(name => q"$name"))

        val specialisedRecordParserTree =
          q"new _root_.com.github.distributedclocks.tracechecker.ElementParser.SpecialisedRecordParser[${cls.toType}](${cls.name.toString})($implicitTree)"

        Expr.apply[RecordParser](c.mirror, new TreeCreator {
          override def apply[U <: Universe with Singleton](m: api.Mirror[U]): U#Tree =
            m.universe.internal
              .createImporter(c.universe)
              .importTree(specialisedRecordParserTree)
        })
      }

    val listOfRecordParsers: Expr[List[RecordParser]] =
      listOfRecordParserExprs.foldLeft(reify[List[RecordParser]](Nil)) { (acc, pExpr) =>
        reify(pExpr.splice :: acc.splice)
      }

    reify {
      import spray.json._
      import DefaultJsonProtocol._

      new ElementParser[E] {
        override def apply(lines: IterableOnce[String]): List[Element] = {
          val parsers: List[RecordParser] = listOfRecordParsers.splice

          lines.iterator
            .zipWithIndex
            .map {
              case (line, lineNum) =>
                val obj = line.parseJson.asJsObject
                parsers.foldLeft(None: Option[Element]) { (acc, parser) =>
                  acc.orElse(parser.tryParse(obj, lineNum))
                }.getOrElse {
                  throw ParsingException(obj, parsers.map(_.tagName))
                }
            }
            .toList
        }
      }
    }
  }
}
