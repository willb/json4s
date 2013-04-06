/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.json4s
package native

import org.json4s.ParserUtil.{Buffer, parseDouble, ParseException}
import org.json4s
import scala.io.Source

/** JSON parser.
 */
object JsonParser extends ParserMeta {
  import java.io._

  /**
   * Return parsed JSON.
   * @param in The input source of the json
   * @param parser The pull parser to use to produce the result
   * @param useBigDecimalForDouble use bigdecimal when a double is encountered
   * @tparam A The result type
   * @return the parsed json
   * @throws ParserUtil.ParseException is thrown if parsing fails
   */
  def parse[A](in: java.io.Reader, parser: (json4s.Parser) => A, useBigDecimalForDouble: Boolean, closeAutomatically: Boolean): A = {
    val dataSource = new Parser(new ParserUtil.Buffer(in, closeAutomatically), useBigDecimalForDouble)
    parser(dataSource)
  }

  private val EOF = (-1).asInstanceOf[Char]
  private sealed abstract class BlockMode
  private case object ARRAY extends BlockMode
  private case object OBJECT extends BlockMode

  class Parser(buf: Buffer, useBigDecimalForDouble: Boolean) extends org.json4s.Parser {

    private[this] var blocks = List.empty[BlockMode]
    private[this] var fieldNameMode = true

    def fail(msg: String) = throw new ParseException(s"$msg\nNear: ${buf.near}", null)

    private[this] def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

    private[this] def parseString: String =
      try {
        ParserUtil.unquote(buf)
      } catch {
        case p: ParseException => throw p
        case _: Throwable => fail("unexpected string end")
      }

    private[this] val valueBuilder = new StringBuilder

    private[this] def parseValue(first: Char) = {
      var wasInt = true
      var doubleVal = false
      val s = valueBuilder
      s.append(first)
      while (wasInt) {
        val c = buf.next
        if (c == '.' || c == 'e' || c == 'E') {
          doubleVal = true
          s.append(c)
        } else if (!(Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')) {
          wasInt = false
          buf.back
        } else s.append(c)
      }
      val value = s.toString
      s.clear()
      if (doubleVal) {
        if (useBigDecimalForDouble) { BigDecimalVal(BigDecimal(value)) } else { DoubleVal(value.toDouble) }
      } else IntVal(BigInt(value))
    }

    /** Parse next Token from stream.
     */
    def nextToken: Token = {
      while (true) {
        val c = buf.next
        if (c == EOF) {
          buf.automaticClose
          return End
        } else if (c == '{') {
          blocks ::= OBJECT
          fieldNameMode = true
          return OpenObj
        } else if (c == '}') {
          blocks = blocks.tail
          return CloseObj
        } else if (c == '"') {
          if (fieldNameMode && blocks.nonEmpty && blocks.head == OBJECT) return FieldStart(parseString)
          else {
            fieldNameMode = true
            return StringVal(parseString)
          }
        } else if (c == 't') {
          fieldNameMode = true
          if (buf.next == 'r' && buf.next == 'u' && buf.next == 'e') {
            return BoolVal(value = true)
          }
          fail("expected boolean")
        } else if (c == 'f') {
          fieldNameMode = true
           if (buf.next == 'a' && buf.next == 'l' && buf.next == 's' && buf.next == 'e') {
             return BoolVal(value = false)
           }
           fail("expected boolean")
         } else if (c == 'n') {
           fieldNameMode = true
           if (buf.next == 'u' && buf.next == 'l' && buf.next == 'l') {
             return NullVal
           }
           fail("expected null")
        } else if (c == ':') {
          if (blocks.nonEmpty && blocks.head == ARRAY) fail("Colon in an invalid position")
          fieldNameMode = false
        } else if (c == '[') {
          blocks ::= ARRAY
          return OpenArr
        } else if (c == ']') {
          fieldNameMode = true
          blocks = blocks.tail
          return CloseArr
        } else if (Character.isDigit(c) || c == '-' || c == '+') {
          fieldNameMode = true
          return parseValue(c)
        } else if (!isDelimiter(c)) fail(s"unknown token $c")
      }
      buf.automaticClose
      End
    }


  }

  
}
