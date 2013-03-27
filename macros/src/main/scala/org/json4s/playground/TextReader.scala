package org.json4s
package playground

import java.lang.Character
import annotation.tailrec

/**
 * @author Bryce Anderson
 * Created on 3/25/13 at 8:37 PM
 */

sealed trait JsonField

case object END extends JsonField
case object Null extends JsonField
case class JsonBool(v: Boolean) extends JsonField
case class JsonString(str: String) extends JsonField
case class JsonObject(str: String) extends JsonField
case class JsonArray(str: String) extends JsonField
case class JsonNumber(str: String) extends JsonField {
  def toInt = str.toInt
  def toLong = str.toLong
  def toBigInt = BigInt(str)
  def toDouble = str.toDouble
  def toFloat = str.toFloat
  def toBigDecimal = BigDecimal(str)
}

private[json4s] object TextReaderHelpers {

  def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'
  def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  def invalidJson(msg: String) = throw new java.lang.IllegalStateException(msg)


  def unescapeString(txt: String): String = {
    var builder: StringBuilder = null
    var begin = 0
    var end = 0
    while(end < txt.length) {
      if(txt.charAt(end) == '\\') {
        if (builder == null) builder = new StringBuilder(txt.length) // Lazy is good

        builder.append(txt.substring(begin, end))
        end +=1
        val c = txt.charAt(end)
        if(c == '"' || c == '\\' || c == '/') { builder.append(c) }
        else if (c == 'b') { builder.append("\b") }
        else if (c == 'f') { builder.append("\f") }
        else if (c == 'n') { builder.append("\n") }
        else if (c == 'r') { builder.append("\r") }
        else if (c == 't') { builder.append("\t") }
        else if (c == 'u' ) { builder.append(Integer.parseInt(txt.substring(end+1, end+5), 16).toChar); end +=5 }

        begin = end + 1
      }

      end += 1
    }

    if (begin == 0) txt
    else {
      if (begin < txt.length) builder.append(txt.substring(begin, txt.length))
      builder.result()
    }
  }

  def findNextString(txt: String): (String, String) = {
    var begin = 0
    while(txt.charAt(begin) != '"') begin +=1
    begin += 1
    var end = begin + 1
    while(!(txt.charAt(end) == '"' && txt.charAt(end-1) != '\\')) end +=1

    (unescapeString(txt.substring(begin, end)), txt.substring(end+1))
  }

  def findNextNumber(txt: String): (String, String) = {
    var end = 0
    while(end < txt.length && isNumberChar(txt.charAt(end))) end += 1
    (txt.substring(0, end), txt.substring(end))
  }

  def findNextBoolean(txt: String): (String, String) = {
    if (txt.startsWith("true")) (txt.substring(0, 4), txt.substring(4))
    else if (txt.startsWith("false")) (txt.substring(0, 5), txt.substring(5))
    else throw new java.lang.IllegalStateException(s"Next string is not of type boolean: $txt")
  }

  def findNextObject(txt: String) = findNextBlock(txt, '{', '}')
  def findNextArray(txt: String) = findNextBlock(txt, '[', ']')

  private def findNextBlock(txt: String, start: Char, finish: Char): (String, String) = {
    var beginning = 0
    var current = ' '
    while({current = txt.charAt(beginning); current != start}) {
      if (current == '"') {
        beginning +=1
        while(!(txt.charAt(beginning) == '"' &&  txt.charAt(beginning-1) != '\\')) beginning +=1
      }
      beginning += 1
    }

    var end = beginning + 1
    var innerObjs = 0
    while({current = txt.charAt(end); current != finish || innerObjs > 0}) {
      if (current == '"') {
        end +=1
        while(!(txt.charAt(end) == '"' &&  txt.charAt(end-1) != '\\')) end +=1
      }
      else if (current == start) innerObjs += 1
      else if (current == finish) innerObjs -= 1
      end +=1
    }
    (txt.substring(beginning+1, end), if(end != txt.length) txt.substring(end+1) else "")
  }

  def extractField(str: String): (String, JsonField) =
    if(str.length == 0) {
      invalidJson(s"Tried to extract field that doesn't exist!")
    } else str.charAt(0) match {
    case '"'                    =>
      val (value, remainder) = findNextString(str)
      (remainder, JsonString(value))

    case '{'                    =>
      val (value, remainder) = findNextObject(str)
      (remainder, JsonObject(value))

    case '['                    =>
      val (value, remainder) = findNextArray(str)
      (remainder, JsonArray(value))

    case c if (isNumberChar(c)) =>
      val (value, remainder) = findNextNumber(str)
      (remainder, JsonNumber(value))

    case 't' if (str.charAt(1) == 'r' && str.charAt(2) == 'u' && str.charAt(3) == 'e') =>
      (str.substring(4), JsonBool(true))


    case 'f' if (str.charAt(1) == 'a' && str.charAt(2) == 'l' && str.charAt(3) == 's' && str.charAt(4) == 'e') =>
      (str.substring(5), JsonBool(false))

    case 'n' if (str.charAt(1) == 'u' && str.charAt(2) == 'l' && str.charAt(3) == 'l') =>
      (str.substring(4), Null)

    case _ => invalidJson(s"Failed to extract field from remaining json: $str")

  }

}

final class TextObjectReader(txt: String) extends JsonObjectReader {
  import TextReaderHelpers._

  // txt should not include the brackets {}, but everything just inside them
  def isSeparator(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n' || in == ',' || in == ':')
  val fields: List[(String, JsonField)] = {
    val builder = new collection.mutable.ListBuffer[(String, JsonField)]()

    @tailrec
    def looper(str: String): Unit = {


      val (key, rest) = findNextString(str)

      var pos = 0
      while(isWhitespace(rest.charAt(pos))) pos += 1     // zoom
      if (rest.charAt(pos) != ':') invalidJson(s"Object separator '${rest.charAt(pos)}' is not ':'")

      pos += 1
      while(isWhitespace(rest.charAt(pos))) pos += 1

      val (remainder, value) = extractField(rest.substring(pos))
      builder += ((key, value))

      pos = 0
      while(pos < remainder.length && isWhitespace(remainder.charAt(pos))) pos += 1
      if (pos == remainder.length) return
      else if (remainder.charAt(pos) == ',') looper(remainder.substring(pos+1))
      else invalidJson(s"Failed to find end or valid separator of Object fields: ${remainder.charAt(pos)}")
    }

    var pos = 0
    while(pos < txt.length && isWhitespace(txt.charAt(pos))) pos += 1
    if (pos != txt.length) looper(txt.substring(pos))
    builder.result
  }

  def getField(name: String) = fields.find(_._1 == name).map(_._2)

  lazy val getKeys: Seq[String] = fields.map(_._1)

  // Option forms
  def optObjectReader(key: String): Option[JsonObjectReader] = getField(key).map ( _ match {
    case JsonObject(str) => new TextObjectReader(str)
    case e => invalidJson(s"Field '$key' doesn't contain object: ${e.toString}")
  })

  def optArrayReader(key: String): Option[JsonArrayIterator] = getField(key).map ( _ match {
    case JsonArray(str) => new TextArrayIterator(str)
    case e => invalidJson(s"Field '$key' doesn't contain array: ${e.toString}")
  })

  def optInt(key: String): Option[Int] = getField(key).map ( _ match {
    case n: JsonNumber => n.toInt
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optLong(key: String): Option[Long] = getField(key).map ( _ match {
    case n: JsonNumber => n.toLong
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optFloat(key: String): Option[Float] = getField(key).map ( _ match {
    case n: JsonNumber => n.toFloat
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optDouble(key: String): Option[Double] = getField(key).map ( _ match {
    case n: JsonNumber => n.toDouble
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigInt(key: String): Option[BigInt] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigInt
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigDecimal(key: String): Option[BigDecimal] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigDecimal
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBool(key: String): Option[Boolean] = getField(key).map ( _ match {
    case JsonBool(v) => v
    case e => invalidJson(s"Field '$key' doesn't contain Boolean: ${e.toString}")
  })

  def optString(key: String): Option[String] = getField(key).map ( _ match {
    case JsonString(str) => str
    case e => invalidJson(s"Field '$key' doesn't contain number: ${e.toString}")
  })
}

final class TextArrayIterator(txt: String) extends JsonArrayIterator {
  import TextReaderHelpers._

  private var current = {
    var pos = 0
    while(pos < txt.length && isWhitespace(txt.charAt(pos))) pos += 1
    txt.substring(pos)
  }

  private def zoomToNext() {
    var pos = 0
    while(pos < current.length && isWhitespace(current.charAt(pos))) pos += 1   // zoom over whitespace
    if (pos == current.length) current = ""
    else if (current.charAt(pos) != ',')
      invalidJson(s"Array parser ran into illegal delimeter: ${current.charAt(pos)} as position $pos in json: $current")
    else {
      pos += 1
      while(pos < current.length && isWhitespace(current.charAt(pos))) pos += 1  // zoom again
      if (pos == current.length) current = ""
      else current = current.substring(pos)
    }
  }

  private def nextField: JsonField = {
    val (remainder, value) = extractField(current)
    current = remainder
    zoomToNext()
    value
  }

  def hasNext: Boolean = (current.length != 0)

  // Option forms
  def getNextObjectReader: Option[JsonObjectReader] = nextField match {
    case JsonObject(str) => Some(new TextObjectReader(str))
    case _ => None
  }

  def getNextArrayReader: Option[JsonArrayIterator] = nextField match {
    case JsonArray(str) => Some(new TextArrayIterator(str))
    case _ => None
  }

  def getNextInt: Option[Int] = nextField match {
    case n: JsonNumber => Some(n.toInt)
    case _ => None
  }

  def getNextLong: Option[Long] = nextField match {
    case n: JsonNumber => Some(n.toLong)
    case _ => None
  }

  def getNextFloat: Option[Float] = nextField match {
    case n: JsonNumber => Some(n.toFloat)
    case _ => None
  }

  def getNextDouble: Option[Double] = nextField match {
    case n: JsonNumber => Some(n.toDouble)
    case _ => None
  }

  def getNextBigInt: Option[BigInt] = nextField match {
    case n: JsonNumber => Some(n.toBigInt)
    case _ => None
  }

  def getNextBigDecimal: Option[BigDecimal] = nextField match {
    case n: JsonNumber => Some(n.toBigDecimal)
    case _ => None
  }

  def getNextBool: Option[Boolean] = nextField match {
    case JsonBool(v) => Some(v)
    case _ => None
  }

  def getNextString: Option[String] = nextField match {
    case JsonString(str) => Some(str)
    case _ => None
  }
}
