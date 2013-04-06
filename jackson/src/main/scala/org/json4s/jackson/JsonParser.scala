package org.json4s
package jackson

import com.fasterxml.jackson.core.{JsonParser => JacksonParser, JsonFactory, JsonToken}
import scala.annotation.switch
import org.json4s.ParserUtil.ParseException
import java.io.Reader

class JsonParserMeta(factory: JsonFactory) extends ParserMeta {


  /**
   * Return parsed JSON.
   * @param in The input source of the json
   * @param parser The pull parser to use to produce the result
   * @param useBigDecimalForDouble use bigdecimal when a double is encountered
   * @tparam A The result type
   * @return the parsed json
   * @throws Parserutil.ParseException is thrown if parsing fails
   */
  def parse[A](in: java.io.Reader, parser: (Parser) => A, useBigDecimalForDouble: Boolean = false, closeAutomatically: Boolean = true): A = {
    val pp = factory.createParser(in)
    pp.configure(JacksonParser.Feature.AUTO_CLOSE_SOURCE, closeAutomatically)
    parser(new JsonParser(pp, useBigDecimalForDouble))
  }
}

object JsonParser extends JsonParserMeta(new JsonFactory())

class JsonParser(jp: JacksonParser, useBigDecimalForDouble: Boolean) extends Parser {

  import Parser._

  private[this] val TrueValue = BoolVal(value = true)
  private[this] val FalseValue = BoolVal(value = false)
  def fail(msg: String) = throw new ParseException(s"$msg\nNear: ${jp.getCurrentLocation.toString}", null)

  def nextToken: Token = {
    val tok = jp.nextToken()
    if (jp.isClosed || tok == null) End
    else if (tok == JsonToken.VALUE_TRUE) TrueValue
    else if (tok == JsonToken.VALUE_FALSE) FalseValue
    else if (tok == JsonToken.FIELD_NAME) FieldStart(jp.getCurrentName)
    else if (tok == JsonToken.VALUE_NULL) NullVal
    else if (tok == JsonToken.VALUE_NUMBER_FLOAT) {
      if (useBigDecimalForDouble) BigDecimalVal(jp.getDecimalValue) else DoubleVal(jp.getDoubleValue)
    } else if (tok == JsonToken.VALUE_NUMBER_INT) IntVal(jp.getBigIntegerValue)
    else if (tok == JsonToken.VALUE_STRING) StringVal(jp.getText)
    else if (tok == JsonToken.START_OBJECT) OpenObj
    else if (tok == JsonToken.START_ARRAY) OpenArr
    else if (tok == JsonToken.END_OBJECT) CloseObj
    else if (tok == JsonToken.END_ARRAY) CloseArr
    else fail("Unknown token encountered")
  }
}