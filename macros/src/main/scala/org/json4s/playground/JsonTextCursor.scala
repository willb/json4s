package org.json4s.playground

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

private[json4s] class JsonTextCursor(text: String) extends TextCursor {
  def txt = text
  def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  def newObjectReader(cursor: TextCursor)  = new TextObjectReader(cursor)
  def newArrayIterator(cursor: TextCursor) = new TextArrayIterator(cursor)
}