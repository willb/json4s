package org.json4s.macroimpls.macrohelpers

import java.util.Date
import scala.reflect.macros.Context

import org.json4s.ParserUtil.ParseException

object PrimativeHelpers {
  import org.json4s.DateFormat
  
  // Some helpers to make things a little more simple in the generated code
  def getInt(name:String,in:Any):Int = in match {
    case n:Long => n.asInstanceOf[Int]
    case n:Int => n
    case bi: math.BigInt => bi.toInt
    case s:String => try { s.toInt } catch {
      case e:java.lang.NumberFormatException => 
      throw new ParseException(s"Error parsing value '$name' to Int.",e)
    }
    case e => throw new ParseException(s"Error converting item '$name' to Int. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getLong(name:String,in:Any):Long = in match {
    case n:Long => n
    case n:Int => n.asInstanceOf[Long]
    case bi: math.BigInt => bi.toLong
    case s:String => try { s.toLong } catch {
      case e:java.lang.NumberFormatException => 
      throw new ParseException(s"Error parsing value '$name' to Long",e)
    }
    case e => throw new ParseException(s"Error converting item '$name' to Long. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getFloat(name:String,in:Any):Float = in match {
    case n:Float => n
    case n:Double => n.asInstanceOf[Float]
    case b:math.BigDecimal => b.toFloat
    case n:Long =>   n.asInstanceOf[Float]
    case n:Int =>    n.asInstanceOf[Float]
    case s:String => s.toFloat
    case bi: math.BigInt => bi.toFloat
    case e => throw new ParseException(s"Error converting item '$name' to Float. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getDouble(name:String,in:Any):Double = in match {
    case n:Double => n
    case n:Float => n.asInstanceOf[Double]
    case b:math.BigDecimal => b.toDouble
    case n:Long =>   n.asInstanceOf[Double]
    case n:Int =>    n.asInstanceOf[Double]
    case s:String => try { s.toDouble } catch {
      case e:java.lang.NumberFormatException => 
      throw new ParseException(s"Error parsing value '$name' to Double",e)
    }
    case bi: math.BigInt => bi.toDouble
    case e => throw new ParseException(s"Error converting item '$name' to Double. Value: $e, Type: ${e.getClass}",null)
  }
  
  def getString(name:String,in:Any):String = in match {
    case s:String => s
    case s => s.toString
  }
  
  def getDate(name:String, in:Any, dateFormat: DateFormat):Date = in match {
    case s:Date => s
    case s:String => dateFormat.parse(s).get
    case e => throw new ParseException(s"Error converting item '$name' to Date. Value: $e, Type: ${e.getClass}",null)
  }
  
  def optIdent[U](opt: Option[U]):Option[U] = opt
  
}

