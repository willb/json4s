package org.json4s

import org.json4s.ParserUtil.{Buffer, ParseException}
import java.io.StringReader
import scala.util.Try

object Parser {
  /** Parsed tokens from low level pull parser.
   */
  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token
  case class IntVal(value: BigInt) extends Token
  case class DoubleVal(value: Double) extends Token
  case class BigDecimalVal(value: BigDecimal) extends Token
  case class BoolVal(value: Boolean) extends Token
  case object NullVal extends Token
  case object OpenArr extends Token
  case object CloseArr extends Token
}

private object ParserMeta {
  class ValStack(parser: Parser) {
    import java.util.LinkedList
    private[this] val stack = new LinkedList[Any]()

    def popAny = stack.poll
    def pop[A](expectedType: Class[A]) = convert(stack.poll, expectedType)
    def push(v: Any) = stack.addFirst(v)
    def peekAny = stack.peek
    def peek[A](expectedType: Class[A]) = convert(stack.peek, expectedType)
    def replace[A](newTop: Any) = stack.set(0, newTop)

    private def convert[A](x: Any, expectedType: Class[A]): A = {
      if (x == null) parser.fail("expected object or array")
      try { x.asInstanceOf[A] } catch { case _: ClassCastException => parser.fail(s"unexpected $x") }
    }

    def peekOption = if (stack isEmpty) None else Some(stack.peek)
  }
}

trait ParserMeta {
  import JsonAST._
  import ParserMeta.ValStack

  // Added forwarders so it doesn't break existing code
  type Token = org.json4s.Parser.Token
  val OpenObj = org.json4s.Parser.OpenObj
  val CloseObj = org.json4s.Parser.CloseObj
  val FieldStart = org.json4s.Parser.FieldStart
  type FieldStart = org.json4s.Parser.FieldStart
  val End = org.json4s.Parser.End
  val StringVal = org.json4s.Parser.StringVal
  type StringVal = org.json4s.Parser.StringVal
  val IntVal = org.json4s.Parser.IntVal
  type IntVal = org.json4s.Parser.IntVal
  val DoubleVal = org.json4s.Parser.DoubleVal
  type DoubleVal = org.json4s.Parser.DoubleVal
  val BigDecimalVal = org.json4s.Parser.BigDecimalVal
  type BigDecimalVal = org.json4s.Parser.BigDecimalVal
  val BoolVal = org.json4s.Parser.BoolVal
  type BoolVal = org.json4s.Parser.BoolVal
  val NullVal = org.json4s.Parser.NullVal
  val OpenArr = org.json4s.Parser.OpenArr
  val CloseArr = org.json4s.Parser.CloseArr

  def parse(in: JsonInput): JValue =
    parse(in, useBigDecimalForDouble = false)

  def parseOpt(in: JsonInput): Option[JValue] =
    parseOpt(in, useBigDecimalForDouble = false)

  def tryParse(in: JsonInput): Try[JValue] =
    tryParse(in, useBigDecimalForDouble = false)

  def parse(in: JsonInput, useBigDecimalForDouble: Boolean): JValue =
    parse(in, astParser(_, useBigDecimalForDouble), useBigDecimalForDouble)

  def parseOpt(in: JsonInput, useBigDecimalForDouble: Boolean): Option[JValue] =
    parseOpt(in, astParser(_, useBigDecimalForDouble), useBigDecimalForDouble)

  def tryParse(in: JsonInput, useBigDecimalForDouble: Boolean): Try[JValue] =
    tryParse(in, astParser(_, useBigDecimalForDouble), useBigDecimalForDouble)

  def parse(in: JsonInput, useBigDecimalForDouble: Boolean, closeAutomatically: Boolean): JValue =
    parse(in, astParser(_, useBigDecimalForDouble), useBigDecimalForDouble, closeAutomatically)

  def parseOpt(in: JsonInput, useBigDecimalForDouble: Boolean, closeAutomatically: Boolean): Option[JValue] =
    parseOpt(in, astParser(_, useBigDecimalForDouble), useBigDecimalForDouble, closeAutomatically)

  def tryParse(in: JsonInput, useBigDecimalForDouble: Boolean, closeAutomatically: Boolean): Try[JValue] =
    tryParse(in, astParser(_, useBigDecimalForDouble), useBigDecimalForDouble, closeAutomatically)

  /**
   * Return parsed JSON.
   * @param in The input source of the json
   * @param parser The pull parser to use to produce the result
   * @param useBigDecimalForDouble use bigdecimal when a double is encountered
   * @tparam A The result type
   * @return the parsed json
   * @throws ParseException is thrown if parsing fails
   */
  def parse[A](in: JsonInput, parser: Parser => A, useBigDecimalForDouble: Boolean = false, closeAutomatically: Boolean = true): A

  def parseOpt[A](in: JsonInput, parser: Parser => A, useBigDecimalForDouble: Boolean = false, closeAutomatically: Boolean = true): Option[A] =
    tryParse(in, parser, useBigDecimalForDouble).toOption

  def tryParse[A](in: JsonInput, parser: Parser => A, useBigDecimalForDouble: Boolean = false, closeAutomatically: Boolean = true): Try[A] =
    Try(parse(in, parser, useBigDecimalForDouble))

  protected val astParser = (p: Parser, useBigDecimal: Boolean) => {
    val vals = new ValStack(p)
    var token: Token = null
    var root: Option[JValue] = None

    // This is a slightly faster way to correct order of fields and arrays than using 'map'.
    def reverse(v: JValue): JValue = v match {
      case JObject(l) => JObject((l.map { case (n, v) => (n, reverse(v)) }).reverse)
      case JArray(l) => JArray(l.map(reverse).reverse)
      case x => x
    }

    def closeBlock(v: Any) {
      @inline def toJValue(x: Any) = x match {
        case json: JValue => json
        case _ => p.fail(s"unexpected field $x")
      }

      vals.peekOption match {
        case Some((name: String, value)) =>
          vals.pop(classOf[JField])
          val obj = vals.peek(classOf[JObject])
          vals.replace(JObject((name, toJValue(v)) :: obj.obj))
        case Some(o: JObject) =>
          vals.replace(JObject(vals.peek(classOf[JField]) :: o.obj))
        case Some(a: JArray) => vals.replace(JArray(toJValue(v) :: a.arr))
        case Some(x) => p.fail(s"expected field, array or object but got $x")
        case None => root = Some(reverse(toJValue(v)))
      }
    }

    def newValue(v: JValue) {
      vals.peekAny match {
        case (name: String, value) =>
          vals.pop(classOf[JField])
          val obj = vals.peek(classOf[JObject])
          vals.replace(JObject((name, v) :: obj.obj))
        case a: JArray => vals.replace(JArray(v :: a.arr))
        case _ => p.fail("expected field or array")
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(JObject(Nil))
        case FieldStart(name) => vals.push(JField(name, null))
        case StringVal(x)     => newValue(JString(x))
        case IntVal(x)        => newValue(JInt(x))
        case DoubleVal(x)     => newValue(JDouble(x))
        case BigDecimalVal(x) => newValue(JDecimal(x))
        case BoolVal(x)       => newValue(JBool(x))
        case NullVal          => newValue(JNull)
        case CloseObj         => closeBlock(vals.popAny)
        case OpenArr          => vals.push(JArray(Nil))
        case CloseArr         => closeBlock(vals.pop(classOf[JArray]))
        case End              =>
      }
    } while (token != End)

    root getOrElse JNothing
  }





}

trait Parser {
  import Parser._

  def fail(msg: String): Nothing
  def nextToken: Token
}