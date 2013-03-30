package org.json4s
package playground

import collection.BitSet
import java.nio.CharBuffer
import annotation.switch

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
      case e => throw new InvalidStructure(s"Invalid starting json structure: $e", null)
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


private[json4s] class JsonTextCursor(txt: String) { self =>

  private[this] val numbers = BitSet((('0' to '9') ++ ".eE-+").map(_.toInt):_*)
  private[this] val whiteSpace = BitSet(Seq(' ', '\r', '\t', '\n').map(_.toInt):_*)

  def isNumberChar(c: Char) = numbers contains c
  def isWhitespace(in: Char) = whiteSpace contains in

  private[this] var current = 0
  private[this] val maxLength = txt.length

  def fail(msg: String, cause: Exception = null) = throw new ParserUtil.ParseException(msg, cause)

  def empty = (current >= maxLength)

  def remainder = txt.substring(current)

  final def nextChar() = { current += 1; txt.charAt(current-1)}

  def findNextString(): JsonString = {
    if(txt.charAt(current) != '"') fail(s"Failed to find string next in '$remainder'")
    var begin = current + 1
    var end = begin


    var builder: CharBuffer = null
    var chr = txt.charAt(end)
    while(chr != '"') {
      if(txt.charAt(end) == '\\') {
        if (builder == null) builder = CharBuffer.allocate(txt.size * 2)

        builder.append(txt.substring(begin, end))
        end +=1
        (txt.charAt(end): @switch) match {
          case '"' => builder.append('"')
          case '\\' => builder.append('\\')
          case '/' => builder.append('/')
          case 'b' => builder.append('\b')
          case 'f' => builder.append('\f')
          case 'n' => builder.append('\n')
          case 'r' => builder.append('\r')
          case 't' => builder.append('\t')
          case 'u' =>
            builder.append(Integer.parseInt(txt.substring(end+1, end+5), 16).toChar)
            end += 4
          case c => fail(s"Bad escaped character: '$c'. Remainder: $remainder")
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
      JsonString(builder.flip().toString)
    }
  }

  def trim() { while(current < maxLength && isWhitespace(txt.charAt(current))) current += 1 }

  // returns true if finished, otherwise false
  def zoomPastSeparator(sep: Char, end: Char): Boolean = {
    trim()
    val chr = txt.charAt(current)
    if (chr == end) true
    else if (chr != sep) fail(s"Separator '${txt.charAt(current)}' is not '$sep' or '$end'")
    else {
      current += 1
      trim()
      false
    }
  }

  def findNextNumber(): JsonNumber = {
    var end = current
    while(end < maxLength && isNumberChar(txt.charAt(end))) end += 1

    val str = txt.substring(current, end)
    current = end
    JsonNumber(str)
  }

  def findNextBoolean(): JsonBool = {
    if (txt.substring(0, 4) == "true"){
      current = current + 4
      JsonBool(v = true)
    }
    else if ( txt.substring(0, 5) == "false") {
      current = current + 5
      JsonBool(v = false)
    }
    else fail(s"Next token is not of type boolean: ${txt.substring(current)}")
  }

  def extractField(): JsonField =
    if(maxLength == current) {
      fail(s"Tried to extract field that doesn't exist!")
    } else {
      (txt.charAt(current): @switch) match {
        case '"' => findNextString()
        case '{' => JsonObject(new TextObjectReader(self))
        case '[' => JsonArray(new TextArrayIterator(self))
        case '0' => findNextNumber()
        case '1' => findNextNumber()
        case '2' => findNextNumber()
        case '3' => findNextNumber()
        case '4' => findNextNumber()
        case '5' => findNextNumber()
        case '6' => findNextNumber()
        case '7' => findNextNumber()
        case '8' => findNextNumber()
        case '9' => findNextNumber()
        case '.' => findNextNumber()
        case 'e' => findNextNumber()
        case 'E' => findNextNumber()
        case '-' => findNextNumber()
        case '+' => findNextNumber()
        case 't' => findNextBoolean()
        case 'f' => findNextBoolean()
        case 'n' =>
          if (txt.charAt(current + 1) == 'u' && txt.charAt(current + 2) == 'l' && txt.charAt(current + 3) == 'l') {
            current += 4
            Null
          } else fail(s"Failed to extract field from remaining json: ${txt.substring(current)}")

        case _ => fail(s"Failed to extract field from remaining json: ${txt.substring(current)}")
      }
    }
}