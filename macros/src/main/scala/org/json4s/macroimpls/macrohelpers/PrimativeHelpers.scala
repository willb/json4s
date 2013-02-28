package org.json4s.macroimpls.macrohelpers

import java.util.Date
import scala.reflect.macros.Context

import org.json4s.ParserUtil.ParseException
import org.json4s._

class NothingException extends ParseException("JValue is a JNothing", null)

object PrimativeHelpers {
  import org.json4s.DateFormat

  def getValueByName(obj: JValue, name: String): Option[JValue] = obj match {
    case JObject(obj) =>
      obj.find{ case (k, _) => k == name}
        .map{ case (_, v) => v }
    case _ => None  // TODO: Is this desired?
  }


  // Some helpers to make things a little more simple in the generated code
  def getInt(in: JValue): Int = in match {
    case JInt(int) => int.intValue()
    case _ => throw new NothingException
  }
  
  def getLong(in: JValue): Long = in match {
    case JInt(int) => int.longValue()
    case _ => throw new NothingException
  }
  
  def getFloat(in: JValue): Float = in match {
    case JDouble(num)   => num.asInstanceOf[Float]
    case JDecimal(num)  => num.floatValue()
    case _ => throw new NothingException
  }
  
  def getDouble( in: JValue): Double = in match {
    case JDouble(num)   => num.asInstanceOf[Double]
    case JDecimal(num)  => num.doubleValue()
    case _ => throw new NothingException
  }
  
  def getString(in: JValue): String = in match {
    case JString(s) => s
    case _ => throw new NothingException
  }
  
  def getDate(in: JValue, dateFormat: DateFormat): Date = in match {
    case JString(s) => dateFormat.parse(s).get
    case _ => throw new NothingException
  }
  
  def optIdent[U](opt: Option[U]):Option[U] = opt
}

