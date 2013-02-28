package org.json4s.macroimpls.macrohelpers

import java.util.Date
import scala.reflect.macros.Context

import org.json4s.ParserUtil.ParseException
import org.json4s._

class NothingException(name: String) extends ParseException(s"JValue $name is a JNothing", null)

object PrimativeHelpers {
  import org.json4s.DateFormat

  def getValueByName(obj: JValue, name: String): Option[JValue] = obj match {
    case JObject(obj) =>
      obj.find{ case (k, _) => k == name}
        .map{ case (_, v) => v }
    case _ => None  // TODO: Is this desired?
  }


  // Some helpers to make things a little more simple in the generated code
  def getInt(name: String, in: JValue): Int = in match {
    case JInt(int) => int.intValue()
    case JNothing => throw new NothingException(name)
    case e => throw new ParseException(s"JValue '$name' is not a JInt. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getLong(name: String, in: JValue): Long = in match {
    case JInt(int) => int.longValue()
    case JNothing => throw new NothingException(name)
    case e => throw new ParseException(s"JValue '$name' is not a Long. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getFloat(name: String, in: JValue): Float = in match {
    case JDouble(num)   => num.asInstanceOf[Float]
    case JDecimal(num)  => num.floatValue()
    case JNothing => throw new NothingException(name)
    case e => throw new ParseException(s"JValue '$name' is not a Float. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getDouble(name: String, in: JValue): Double = in match {
    case JDouble(num)   => num.asInstanceOf[Double]
    case JDecimal(num)  => num.doubleValue()
    case JNothing => throw new NothingException(name)
    case e => throw new ParseException(s"JValue '$name' is not a Double. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getString(name: String, in: JValue): String = in match {
    case JString(s) => s
    case JNothing => throw new NothingException(name)
    case e => throw new ParseException(s"JValue '$name' is not a JString. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getDate(name: String, in: Any, dateFormat: DateFormat): Date = in match {
    case JString(s) => dateFormat.parse(s).get
    case JNothing => throw new NothingException(name)
    case e => throw new ParseException(s"JValue '$name' is not a Date. Value: $e, Type: ${e.getClass}",null)
  }
  
  def optIdent[U](opt: Option[U]):Option[U] = opt
}

