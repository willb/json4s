package org.json4s
package macro_readers

import scala.annotation.tailrec

/**
 * @author Bryce Anderson
 * Created on 3/28/13 at 8:28 PM
 */

object JsonTextReader {
  def bindText(str: String): JsonReader = {
    val cursor = new JsonStringCursor(str)
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

trait JsonTextCursor {
  def empty: Boolean
  def findNextString(): JsonString
  def zoomPastSeparator(sep: Char, end: Char): Boolean
  def findNextNumber(): JsonNumber
  def extractField(): JsonField
  def findNextBoolean(): JsonBool
  def nextChar(): Char

  protected def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  protected def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  def failStructure(msg: String) = throw new MappingException(msg, null)
  def failParse(msg: String) = throw new ParserUtil.ParseException(msg, null)
}


