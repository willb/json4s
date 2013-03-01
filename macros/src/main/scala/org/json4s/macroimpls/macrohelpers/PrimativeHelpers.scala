package org.json4s.macroimpls.macrohelpers

import java.util.Date
import scala.reflect.macros.Context

import org.json4s.ParserUtil.ParseException
import org.json4s._
import annotation.tailrec

class JsonStructureException(expected: Class[_], received: Class[_]) extends ParseException(
  s"Received JValue type ($received) but expected ($expected).", null)

object PrimativeHelpers {
  import org.json4s.DateFormat

  def getFieldByName(obj: JValue, name: String): Option[JValue] = obj match {
    case JObject(fields) =>
     @tailrec
      def findField(name: String, lst: List[JField]): Option[JValue] = lst match {
        case h::t if (h._1 == name) => Some(h._2)
        case h::t => findField(name, t)
        case Nil => None
      }; findField(name, fields)
    case _ => None  // TODO: Is this the desired result?
  }

  // Some helpers to make things a little more simple in the generated code
  def getInt(in: JValue): Int = in match {
    case JInt(int) => int.intValue()
    case _ => throw new JsonStructureException(JInt.getClass(), in.getClass())
  }
  
  def getLong(in: JValue): Long = in match {
    case JInt(int) => int.longValue()
    case _ => throw new JsonStructureException(JInt.getClass(), in.getClass())
  }
  
  def getFloat(in: JValue): Float = in match {
    case JDouble(num)   => num.asInstanceOf[Float]
    case JDecimal(num)  => num.floatValue()
    case _ => throw new JsonStructureException(JDecimal.getClass(), in.getClass())
  }
  
  def getDouble( in: JValue): Double = in match {
    case JDouble(num)   => num.asInstanceOf[Double]
    case JDecimal(num)  => num.doubleValue()
    case _ => throw new JsonStructureException(JDecimal.getClass(), in.getClass())
  }
  
  def getString(in: JValue): String = in match {
    case JString(s) => s
    case _ => throw new JsonStructureException(JString.getClass(), in.getClass())
  }
  
  def getDate(in: JValue, dateFormat: DateFormat): Date = in match {
    case JString(s) => dateFormat.parse(s).get
    case _ => throw new JsonStructureException(JString.getClass(), in.getClass())
  }

  def optIdent[U](opt: Option[U]):Option[U] = opt
}

