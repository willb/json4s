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

protected abstract class TextReader {

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

  // Will fail if the string ends with a number char! Shouldn't happen in json.
  def findNextNumber(txt: String): (String, String) = {
    var end = 1
    while(isNumberChar(txt.charAt(end))) end += 1
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
    var inQuote = false
    var beginning = 0
    var current = ' '
    while({current = txt.charAt(beginning); current != start || inQuote}) {
      if ((current == '"') && (txt.charAt(beginning-1) != '\\')) inQuote = !inQuote
      beginning +=1
    }

    var end = beginning + 1
    var innerObjs = 0
    while({current = txt.charAt(end); current != finish || innerObjs > 0} || inQuote) {
      if ((current == '"') && (txt.charAt(end-1) != '\\')) inQuote = !inQuote
      else if (current == start) innerObjs += 1
      else if (current == finish) innerObjs -= 1
      end +=1
    }
    (txt.substring(beginning+1, end), if(end != txt.length) txt.substring(end+1) else "")
  }

}

final class TextObjectReader(txt: String, strict: Boolean = false) extends TextReader with JsonObjectReader {

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

      val remainder = rest.charAt(pos) match {
        case '"'                    =>
          val (value, remainder) = findNextString(rest.substring(pos))
          builder += ((key, JsonString(value)))
          remainder

        case '{'                    =>
          val (value, remainder) = findNextObject(rest.substring(pos))
          builder += ((key, JsonObject(value)))
          remainder

        case '['                    =>
          val (value, remainder) = findNextArray(rest.substring(pos))
          builder += ((key, JsonArray(value)))
          remainder

        case c if (isNumberChar(c)) =>
          val (value, remainder) = findNextNumber(rest.substring(pos))
          builder += ((key, JsonNumber(value)))
          remainder

        case 't' if (rest.charAt(pos+1) == 'r' && rest.charAt(pos+2) == 'u' && rest.charAt(pos+3) == 'e') =>
          builder += ((key, JsonBool(true)))
          rest.substring(pos + 4)

        case 'f' if (rest.charAt(pos+1) == 'a' && rest.charAt(pos+2) == 'l' && rest.charAt(pos+3) == 's' && rest.charAt(pos+4) == 'e') =>
          builder += ((key, JsonBool(true)))
          rest.substring(pos + 5)

        case 'n' if (rest.charAt(pos+1) == 'u' && rest.charAt(pos+2) == 'l' && rest.charAt(pos+3) == 'l') =>
          builder += ((key, Null))
          rest.substring(pos + 4)
      }

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

final class TextArrayIterator(txt: String, strict: Boolean = false) extends JsonArrayIterator {
  def hasNext: Boolean = ???

  // Option forms
  def getNextObjectReader: Option[JsonObjectReader] = ???

  def getNextArrayReader: Option[JsonArrayIterator] = ???

  def getNextInt: Option[Int] = ???

  def getNextLong: Option[Long] = ???

  def getNextFloat: Option[Float] = ???

  def getNextDouble: Option[Double] = ???

  def getNextBigInt: Option[BigInt] = ???

  def getNextBigDecimal: Option[BigDecimal] = ???

  def getNextBool: Option[Boolean] = ???

  def getNextString: Option[String] = ???
}
