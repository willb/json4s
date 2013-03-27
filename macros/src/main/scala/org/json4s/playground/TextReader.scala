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

private[json4s] class JsonTextCursor(txt: String) {
  private var current = 0
  private val maxLength = txt.length

  def empty = (current == maxLength)

  def remainder = txt.substring(current)

  def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'
  def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  def fail(msg: String) = throw new java.lang.IllegalStateException(msg)


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

  // TODO: Will fail on strings like '"Hello world \\"'
  def findNextString(): String = {
    if(txt.charAt(current) != '"') fail(s"Failed to find string next in '$remainder'")
    var end = current + 1
    while(!(txt.charAt(end) == '"' && txt.charAt(end-1) != '\\' )) end +=1

    val str = unescapeString(txt.substring(current + 1, end))
    current = end + 1
    str
  }

  @inline final def trim() { while(current < maxLength && isWhitespace(txt.charAt(current))) current += 1 }

  // Requires a separator
  def zoomPastSeparator(sep: Char, required: Boolean = false) {
    trim()
    if (empty) {
      if(required) fail(s"Didn't find separator symbol '$sep'")
    }
    else {
      if (txt.charAt(current) != sep) fail(s"Separator '${txt.charAt(current)}' is not '$sep'")
      current += 1
      trim()
    }
  }

  def findNextNumber(): String = {
    var end = current
    while(end < maxLength && isNumberChar(txt.charAt(end))) end += 1

    val str = txt.substring(current, end)
    current = end
    str
  }

  def findNextBoolean(): Boolean = {
    if (txt.charAt(current)     == 't' &&
        txt.charAt(current + 1) == 'r' &&
        txt.charAt(current + 2) == 'u' &&
        txt.charAt(current + 3) == 'e') {
      current = current + 4
      true
    }
    else if ( txt.charAt(current)     == 'f' &&
              txt.charAt(current + 1) == 'a' &&
              txt.charAt(current + 2) == 'l' &&
              txt.charAt(current + 3) == 's' &&
              txt.charAt(current + 4) == 'e') {
      current = current + 5
      false
    }
    else fail(s"Next token is not of type boolean: ${txt.substring(current)}")
  }

  def findNextObject() = findNextBlock('{', '}')
  def findNextArray() = findNextBlock('[', ']')

  private def findNextBlock(start: Char, finish: Char): String = {
    var c = ' '
    while({c = txt.charAt(current); c != start}) {
      if (c == '"') {
        current +=1
        while(!(txt.charAt(current) == '"' &&  txt.charAt(current-1) != '\\')) current +=1
      }
      current += 1
    }

    var end = current + 1
    var innerObjs = 0
    while({c = txt.charAt(end); c != finish || innerObjs > 0}) {
      if (c == '"') {
        end +=1
        while(!(txt.charAt(end) == '"' &&  txt.charAt(end-1) != '\\')) end +=1
      }
      else if (c == start) innerObjs += 1
      else if (c == finish) innerObjs -= 1
      end +=1
    }
    val str = txt.substring(current+1, end)
    current = end + 1
    str
  }

  def extractField(): JsonField =
    if(maxLength == current) {
      fail(s"Tried to extract field that doesn't exist!")
    } else txt.charAt(current) match {
    case '"'                    => JsonString(findNextString)
    case '{'                    => JsonObject(findNextObject)
    case '['                    => JsonArray(findNextArray)
    case c if (isNumberChar(c)) => JsonNumber(findNextNumber)
    case c if(c == 't' || c == 'f') => JsonBool(findNextBoolean)
    case 'n' if (txt.charAt(current + 1) == 'u' && txt.charAt(current + 2) == 'l' && txt.charAt(current + 3) == 'l') =>
       current += 4
       Null

    case _ => fail(s"Failed to extract field from remaining json: ${txt.substring(current)}")
  }

}

final class TextObjectReader(txt: String) extends JsonTextCursor(txt) with JsonObjectReader {

  // txt should not include the brackets {}, but everything just inside them
  //def isSeparator(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n' || in == ',' || in == ':')
  val fields: List[(String, JsonField)] = {
    val builder = new collection.mutable.ListBuffer[(String, JsonField)]()

    @tailrec
    def looper(): Unit = {
      val key = findNextString()
      zoomPastSeparator(':', true)
      val value = extractField()
      builder += ((key, value))
      zoomPastSeparator(',')
      if (!empty) looper()
    }

    trim()
    if (!empty) looper()
    builder.result
  }

  def getField(name: String) = fields.find(_._1 == name).map(_._2)

  lazy val getKeys: Seq[String] = fields.map(_._1)

  // Option forms
  def optObjectReader(key: String): Option[JsonObjectReader] = getField(key).map ( _ match {
    case JsonObject(str) => new TextObjectReader(str)
    case e => fail(s"Field '$key' doesn't contain object: ${e.toString}")
  })

  def optArrayReader(key: String): Option[JsonArrayIterator] = getField(key).map ( _ match {
    case JsonArray(str) => new TextArrayIterator(str)
    case e => fail(s"Field '$key' doesn't contain array: ${e.toString}")
  })

  def optInt(key: String): Option[Int] = getField(key).map ( _ match {
    case n: JsonNumber => n.toInt
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optLong(key: String): Option[Long] = getField(key).map ( _ match {
    case n: JsonNumber => n.toLong
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optFloat(key: String): Option[Float] = getField(key).map ( _ match {
    case n: JsonNumber => n.toFloat
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optDouble(key: String): Option[Double] = getField(key).map ( _ match {
    case n: JsonNumber => n.toDouble
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigInt(key: String): Option[BigInt] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigInt
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigDecimal(key: String): Option[BigDecimal] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigDecimal
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBool(key: String): Option[Boolean] = getField(key).map ( _ match {
    case JsonBool(v) => v
    case e => fail(s"Field '$key' doesn't contain Boolean: ${e.toString}")
  })

  def optString(key: String): Option[String] = getField(key).map ( _ match {
    case JsonString(str) => str
    case e => fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })
}

final class TextArrayIterator(txt: String) extends JsonTextCursor(txt) with JsonArrayIterator {

  trim()

  private def nextField: JsonField = {
    val value = extractField()
    zoomPastSeparator(',', false)
    value
  }

  def hasNext: Boolean = !empty

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
