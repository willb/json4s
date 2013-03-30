package org.json4s
package playground

/**
 * @author Bryce Anderson
 * Created on 3/28/13 at 8:28 PM
 */

object JsonTextReader {
  def bindText(str: String): JsonReader = {
    val cursor = new JsonTextCursor(str)
    cursor.trim()
    cursor.extractField() match {
      case JsonObject(obj) => obj
      case JsonArray(obj)  => obj
      case e => throw new java.lang.IllegalStateException(s"Invalid starting json structure: $e")
    }
  }
}

private[json4s] sealed trait JsonField
private[json4s] case object Null extends JsonField
private[json4s] case class JsonBool(v: Boolean) extends JsonField
private[json4s] case class JsonString(str: String) extends JsonField
private[json4s] case class JsonObject(reader: JsonObjectReader) extends JsonField
private[json4s] case class JsonArray(reader: JsonArrayIterator) extends JsonField
private[json4s] case class JsonNumber(str: String) extends JsonField {
  def toInt = str.toInt
  def toLong = str.toLong
  def toBigInt = BigInt(str)
  def toDouble = str.toDouble
  def toFloat = str.toFloat
  def toBigDecimal = BigDecimal(str)
}

trait CursorFailure {
  def remainder: String
  def failStructure(msg: String) = throw new MappingException(msg, null)
  def failParse(msg: String) = throw new ParserUtil.ParseException(msg + "Remainder: " + remainder, null)
}


private[json4s] class JsonTextCursor(txt: String) extends CursorFailure { self =>

  def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  private var current = 0
  private val maxLength = txt.length



  def empty = (current >= maxLength)

  def remainder = txt.substring(current)

  final def nextChar() = { current += 1; txt.charAt(current-1)}

  def findNextString(): JsonString = {
    if(txt.charAt(current) != '"') failStructure(s"Failed to find string next in '$remainder'")
    var begin = current + 1
    var end = begin

    var builder: StringBuilder = null
    var chr = txt.charAt(end)
    while(chr != '"') {
      if(txt.charAt(end) == '\\') {
        if (builder == null) builder = new StringBuilder(txt.length)

        builder.append(txt.substring(begin, end))
        end +=1
        txt.charAt(end) match {
          case c if(c == '"' || c == '\\' || c == '/') => builder.append(c)
          case c if (c == 'b') => builder.append("\b")
          case c if (c == 'f') => builder.append("\f")
          case c if (c == 'n') => builder.append("\n")
          case c if (c == 'r') => builder.append("\r")
          case c if (c == 't') => builder.append("\t")
          case c if (c == 'u') => { builder.append(Integer.parseInt(txt.substring(end+1, end+5), 16).toChar); end +=4 }
          case c => failParse(s"Bad escaped character: '$c'.")
        }
        begin = end + 1
      }

      end += 1
      require(end < txt.length)
      chr = txt.charAt(end)
    }

    if (begin == current + 1) {
      current = end + 1
      JsonString(txt.substring(begin, end))
    }
    else {
      current = end + 1
      if (begin < end) builder.append(txt.substring(begin, end))
      JsonString(builder.result())
    }
  }

  def trim() { while(current < maxLength && isWhitespace(txt.charAt(current))) current += 1 }

  // returns true if finished, otherwise false
  def zoomPastSeparator(sep: Char, end: Char): Boolean = {
    trim()
    val chr = txt.charAt(current)
    if (chr == end) true
    else if (chr != sep) failParse(s"Separator '${txt.charAt(current)}' is not '$sep' or '$end'")
    else {
      current += 1
      trim()
      false
    }
  }

  def findNextNumber(): JsonNumber = {
    var end = current
    while(end < maxLength && isNumberChar(txt.charAt(end))) end += 1

    if(end == current) failStructure(s"Next token is not number: ${txt.charAt(end)}")
    else {
    val str = txt.substring(current, end)
    current = end
    JsonNumber(str)
    }
  }

  def findNextBoolean(): JsonBool = {
    if (txt.charAt(current)     == 't' &&
      txt.charAt(current + 1) == 'r' &&
      txt.charAt(current + 2) == 'u' &&
      txt.charAt(current + 3) == 'e') {
      current = current + 4
      JsonBool(true)
    }
    else if ( txt.charAt(current)     == 'f' &&
      txt.charAt(current + 1) == 'a' &&
      txt.charAt(current + 2) == 'l' &&
      txt.charAt(current + 3) == 's' &&
      txt.charAt(current + 4) == 'e') {
      current = current + 5
      JsonBool(false)
    }
    else failStructure(s"Next token is not of type boolean: ${txt.substring(current, 5)}")
  }

  def extractField(): JsonField =
    if(maxLength == current) {
      failStructure(s"Tried to extract field that doesn't exist!")
    } else txt.charAt(current) match {
      case '"'                    => findNextString()
      case '{'                    => JsonObject(new TextObjectReader(self))
      case '['                    => JsonArray(new TextArrayIterator(self))
      case c if (isNumberChar(c)) => findNextNumber()
      case c if(c == 't' || c == 'f') => findNextBoolean()
      case 'n' if (txt.charAt(current + 1) == 'u' && txt.charAt(current + 2) == 'l' && txt.charAt(current + 3) == 'l') =>
        current += 4
        Null

      case _ => failParse(s"Cursor not at valid field.")
    }
}