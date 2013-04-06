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
  def parse[A](in: JsonInput, parser: (json4s.Parser) => A, useBigDecimalForDouble: Boolean, closeAutomatically: Boolean): A = {
    def newParser(r: java.io.Reader) =
      new Parser(new ParserUtil.Buffer(r, closeAutomatically), useBigDecimalForDouble)
    val dataSource = in match {
      case StringInput(s) => newParser(new StringReader(s))
      case ReaderInput(rdr) => newParser(rdr)
      case StreamInput(stream) => newParser(Source.fromInputStream(stream).bufferedReader())
      case FileInput(file) => newParser(Source.fromFile(file).bufferedReader())
    }
    parser(dataSource)
  }

  private val EOF = (-1).asInstanceOf[Char]

  class Parser(buf: Buffer, useBigDecimalForDouble: Boolean) extends org.json4s.Parser {
    import java.util.LinkedList

    private[this] val blocks = new LinkedList[BlockMode]()
    private[this] var fieldNameMode = true

    def fail(msg: String) = throw new ParseException(s"$msg\nNear: ${buf.near}", null)



    /** Parse next Token from stream.
     */
    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseString: String = 
        try {
          ParserUtil.unquote(buf)
        } catch {
          case p: ParseException => throw p
          case _: Throwable => fail("unexpected string end")
        }

      def parseValue(first: Char) = {
        var wasInt = true
        var doubleVal = false
        val s = new StringBuilder
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
        if (doubleVal) {
          if (useBigDecimalForDouble) { BigDecimalVal(BigDecimal(value)) } else { DoubleVal(value.toDouble) }
        }
        else IntVal(BigInt(value))
      }

      while (true) {
        val c = buf.next
        if (c == EOF) {
          buf.automaticClose
          return End
        } else if (c == '{') {
          blocks.addFirst(OBJECT)
          fieldNameMode = true
          return OpenObj
        } else if (c == '}') {
          blocks.poll
          return CloseObj
        } else if (c == '"') {
          if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseString)
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
          if (blocks.peek == ARRAY) fail("Colon in an invalid position")
          fieldNameMode = false
        } else if (c == '[') {
          blocks.addFirst(ARRAY)
          return OpenArr
        } else if (c == ']') {
          fieldNameMode = true
          blocks.poll
          return CloseArr
        } else if (Character.isDigit(c) || c == '-' || c == '+') {
          fieldNameMode = true
          return parseValue(c)
        } else if (!isDelimiter(c)) fail(s"unknown token $c")
      }
      buf.automaticClose
      End
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }

  
}
