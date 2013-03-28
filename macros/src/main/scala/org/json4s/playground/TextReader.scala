package org.json4s
package playground

import java.lang.Character
import annotation.tailrec
import collection.mutable.ListBuffer

/**
 * @author Bryce Anderson
 * Created on 3/25/13 at 8:37 PM
 */


object TextReader {
  def bindText(str: String): JsonReader = {
    val cursor = new JsonTextCursor(str)
    cursor.trim()
    cursor.extractField() match {
      case JsonObject(obj) => obj
      case JsonArray(obj)  => obj
      case e => fail(s"Invalid starting json structure: $e")
    }
  }

  private[json4s] def fail(msg: String) = throw new java.lang.IllegalStateException(msg)
}

private[json4s] sealed trait JsonField
private[json4s] case object END extends JsonField
private[json4s] case object Null extends JsonField
private[json4s] case class JsonBool(v: Boolean) extends JsonField
private[json4s] case class JsonString(str: String) extends JsonField
private[json4s] case class JsonObject(reader: TextObjectReader) extends JsonField
private[json4s] case class JsonArray(reader: TextArrayIterator) extends JsonField
private[json4s] case class JsonNumber(str: String) extends JsonField {
  def toInt = str.toInt
  def toLong = str.toLong
  def toBigInt = BigInt(str)
  def toDouble = str.toDouble
  def toFloat = str.toFloat
  def toBigDecimal = BigDecimal(str)
}


private[json4s] class JsonTextCursor(txt: String) { self =>

  private var current = 0
  private val maxLength = txt.length

  def empty = (current >= maxLength)

  def remainder = txt.substring(current)

  final def nextChar() = { current += 1; txt.charAt(current-1)}

  def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'
  def isNumberChar(c: Char) = (Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
  def isWhitespace(in: Char) = ( in == ' ' || in == '\r' || in == '\t' || in == '\n')

  def findNextString(): JsonString = {
    if(txt.charAt(current) != '"') TextReader.fail(s"Failed to find string next in '$remainder'")
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
          case c => TextReader.fail(s"Bad escaped character: '$c'. Remainder: ${remainder}")
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

  @inline
  final def checkEscaped(end: Int) =
    if(txt.charAt(end-1) != '\\') true
    else {   // Need to make sure we didn't escape the slash that is escaping the slash etc...
    var slashes = 1
      while(txt.charAt(end - slashes - 1) == '\\') slashes += 1
      slashes % 2 == 0
    }

  @inline final def trim() { while(current < maxLength && isWhitespace(txt.charAt(current))) current += 1 }

  // Requires a separator
  // returns true if finished, otherwise false
  def zoomPastSeparator(sep: Char, end: Char): Boolean = {
    trim()
    val chr = txt.charAt(current)
    if (chr == end) true
    else if (chr != sep) TextReader.fail(s"Separator '${txt.charAt(current)}' is not '$sep' or '$end'")
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
    else TextReader.fail(s"Next token is not of type boolean: ${txt.substring(current)}")
  }

  def extractField(): JsonField =
    if(maxLength == current) {
      TextReader.fail(s"Tried to extract field that doesn't exist!")
    } else txt.charAt(current) match {
    case '"'                    => findNextString()
    case '{'                    => JsonObject(new TextObjectReader(self))
    case '['                    => JsonArray(new TextArrayIterator(self))
    case c if (isNumberChar(c)) => findNextNumber()
    case c if(c == 't' || c == 'f') => findNextBoolean()
    case 'n' if (txt.charAt(current + 1) == 'u' && txt.charAt(current + 2) == 'l' && txt.charAt(current + 3) == 'l') =>
       current += 4
       Null

    case _ => TextReader.fail(s"Failed to extract field from remaining json: ${txt.substring(current)}")
  }
}

final class TextObjectReader(cursor: JsonTextCursor) extends JsonObjectReader {

  // txt should not include the brackets {}, but everything just inside them
  val fields: List[(String, JsonField)] = {
    if(cursor.nextChar() != '{' ) TextReader.fail(s"Json is not an object: ${cursor.remainder}")
    cursor.trim()

    val builder = new collection.mutable.ListBuffer[(String, JsonField)]()

    @tailrec
    def looper(): Unit = {
      val JsonString(key) = cursor.findNextString()
      if (cursor.zoomPastSeparator(':', '}')) TextReader.fail(s"Invalid json. Remainder: '${cursor.remainder}'")
      val value = cursor.extractField()
      builder += ((key, value))
      if(!cursor.zoomPastSeparator(',', '}')) {
        looper()
      }
    }
    looper()
    if(cursor.nextChar() != '}' ) TextReader.fail(s"Json object missing closing bracket.: ${cursor.remainder}")
    builder.result
  }

  def getField(name: String) = {
    @tailrec def looper(lst: List[(String, JsonField)]): Option[JsonField] = lst match {
      case Nil => None
      case l if (l.head._1 == name) => Some(l.head._2)
      case l => looper(l.tail)
    }
    looper(fields)
  }

  lazy val getKeys: Seq[String] = fields.map(_._1)

  // Option forms
  def optObjectReader(key: String): Option[JsonObjectReader] = getField(key).map ( _ match {
    case JsonObject(reader) => reader
    case e => TextReader.fail(s"Field '$key' doesn't contain object: ${e.toString}")
  })

  def optArrayReader(key: String): Option[JsonArrayIterator] = getField(key).map ( _ match {
    case JsonArray(reader) => reader
    case e => TextReader.fail(s"Field '$key' doesn't contain array: ${e.toString}")
  })

  def optInt(key: String): Option[Int] = getField(key).map ( _ match {
    case n: JsonNumber => n.toInt
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optLong(key: String): Option[Long] = getField(key).map ( _ match {
    case n: JsonNumber => n.toLong
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optFloat(key: String): Option[Float] = getField(key).map ( _ match {
    case n: JsonNumber => n.toFloat
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optDouble(key: String): Option[Double] = getField(key).map ( _ match {
    case n: JsonNumber => n.toDouble
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigInt(key: String): Option[BigInt] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigInt
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBigDecimal(key: String): Option[BigDecimal] = getField(key).map ( _ match {
    case n: JsonNumber => n.toBigDecimal
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })

  def optBool(key: String): Option[Boolean] = getField(key).map ( _ match {
    case JsonBool(v) => v
    case e => TextReader.fail(s"Field '$key' doesn't contain Boolean: ${e.toString}")
  })

  def optString(key: String): Option[String] = getField(key).map ( _ match {
    case JsonString(str) => str
    case e => TextReader.fail(s"Field '$key' doesn't contain number: ${e.toString}")
  })
}

final class TextArrayIterator(cursor: JsonTextCursor) extends JsonArrayIterator {

  var fields: List[JsonField] = {
    if(cursor.nextChar() != '[' ) TextReader.fail(s"Json is not an array: ${cursor.remainder}")
    cursor.trim()
    val builder = new ListBuffer[JsonField]
    @tailrec
    def looper(): Unit = {
      builder += cursor.extractField()
      if(!cursor.zoomPastSeparator(',', ']')) looper()
    }

    looper()
    if(cursor.nextChar() != ']' ) TextReader.fail(s"Json object missing closing bracket!: ${cursor.remainder}")
    builder.result()
  }

  private def nextField: JsonField = {
    if(fields.isEmpty) TextReader.fail(s"TextArrayIterator is empty!")
    val value = fields.head
    fields = fields.tail
    value
  }

  def hasNext: Boolean = !fields.isEmpty

  // Option forms
  def getNextObjectReader: Option[JsonObjectReader] = nextField match {
    case JsonObject(obj) => Some(obj)
    case _ => None
  }

  def getNextArrayReader: Option[JsonArrayIterator] = nextField match {
    case JsonArray(arr) => Some(arr)
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
