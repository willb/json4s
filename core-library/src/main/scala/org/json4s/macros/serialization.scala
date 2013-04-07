package org.json4s
package macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.json4s.JsonAST._
import org.json4s.reflect.TypeInfo
import org.json4s.Parser._
import scala.annotation.tailrec

object MacroImpls {

  case class H1(id: Int, name: String, age: Int)

  trait Json4sBuilder[T] {
    def result: T
  }
  class H1Json4sBuilder1 extends Json4sBuilder[H1] {
    var id: Int = _
    var name: String = _
    var age: Int = _
    def result: H1 = H1(id, name, age)
  }

  private[this] def findByName(jv: JValue, nameToFind: String): JValue =
    findDirectByName(List(jv), nameToFind) match {
      case Nil ⇒ JNothing
      case x :: Nil ⇒ x
      case x ⇒ JArray(x)
    }

  private[this] def findDirectByName(xs: List[JValue], name: String): List[JValue] = xs.flatMap {
    case JObject(l) ⇒ l.filter {
      case (n, _) if n == name ⇒ true
      case _ ⇒ false
    } map (_._2)
    case JArray(l) ⇒ findDirectByName(l, name)
    case _ ⇒ Nil
  }

  def rdr(implicit formats: Formats) = new Reader[H1] {
    def read(value: JValue): H1 = {
      // Create class
      val k = new H1Json4sBuilder1
      k.id = findByName(value, "id") match {
        case JInt(v) => v.intValue()
        case JDouble(v) => v.intValue()
        case JDecimal(v) => v.intValue()
        case _ => sys.error(s"Could not find a value to convert to Int for id in $value")
      }
      k.name = findByName(value, "name") match {
        case JInt(x) => x.toString
        case JDecimal(x) => x.toString
        case JDouble(x) => x.toString
        case JBool(x) => x.toString
        case JString(s) => s
        case JNull => null
        case _ => sys.error(s"Could not find a value to convert to String for name in $value")
      }
      k.age = findByName(value, "age") match {
        case JInt(v) => v.intValue()
        case JDouble(v) => v.intValue()
        case JDecimal(v) => v.intValue()
        case _ => sys.error(s"Could not find a value to convert to Int for age in $value")
      }
      k.result
    }
  }

  trait ParserReader[T] {
    def read(parser: Parser): T
  }

  trait H1ParserReader extends ParserReader[H1] {

    def read(parser: Parser): H1 = {
      def readIntField(name: String, tok: Token, setter: Int => Unit) = tok match {
        case IntVal(id) =>
          setter(id.intValue())
        case DoubleVal(id) =>
          setter(id.toInt)
        case BigDecimalVal(id) =>
          setter(id.intValue())
        case _ => parser.fail(s"Expected int for $name")
      }

      def readStringField(name: String, tok: Token, setter: String => Unit) = tok match {
        case IntVal(nm) =>
          setter(nm.toString())
        case DoubleVal(nm) =>
          setter(nm.toString())
        case BigDecimalVal(nm) =>
          setter(nm.toString())
        case BoolVal(nm) =>
          setter(nm.toString)
        case NullVal =>
          setter(null)
        case StringVal(nm) =>
          setter(nm)
        case _ => parser.fail(s"Expected string for $name")

      }

      val h1 = new H1Json4sBuilder1
      @tailrec def parse: H1 = parser.nextToken match {
        case FieldStart("id") =>
          readIntField("id", parser.nextToken, h1.id = _)
          parse
        case FieldStart("name") =>
          readStringField("name", parser.nextToken, h1.name = _)
          parse
        case FieldStart("age") =>
          readIntField("age", parser.nextToken, h1.age = _)
          parse
        case End => h1.result
        case _ => parse
      }
      parse
    }
  }

  class Helper[T <: Context](c: T)
}