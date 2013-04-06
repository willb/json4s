package org.json4s
package jackson

import com.fasterxml.jackson.databind.{ObjectMapper, DeserializationFeature}
import util.control.Exception.allCatch
import scala.util.Try

trait JsonMethods extends org.json4s.JsonMethods[JValue] {

  private[this] lazy val _defaultMapper = {
    val m = new ObjectMapper()
    m.registerModule(new Json4sScalaModule)
    m
  }
  def mapper = _defaultMapper

  def parse(in: JsonInput, useBigDecimalForDouble: Boolean = false): JValue = {
    val pm = new JsonParserMeta(mapper.getFactory)
    pm.parse(in, useBigDecimalForDouble = useBigDecimalForDouble)
  }

  def parseOpt(in: JsonInput, useBigDecimalForDouble: Boolean = false): Option[JValue] = {
    val pm = new JsonParserMeta(mapper.getFactory)
    pm.parseOpt(in, useBigDecimalForDouble = useBigDecimalForDouble)
  }

  def tryParse(in: JsonInput, useBigDecimalForDouble: Boolean = false): Try[JValue] = {
    val pm = new JsonParserMeta(mapper.getFactory)
    pm.tryParse(in, useBigDecimalForDouble = useBigDecimalForDouble)
  }

  def render(value: JValue): JValue = value

  def compact(d: JValue): String = mapper.writeValueAsString(d)

  def pretty(d: JValue): String = {
    val writer = mapper.writerWithDefaultPrettyPrinter()
    writer.writeValueAsString(d)
  }


  def asJValue[T](obj: T)(implicit writer: Writer[T]): JValue = writer.write(obj)
  def fromJValue[T](json: JValue)(implicit reader: Reader[T]): T = reader.read(json)
}

object JsonMethods extends JsonMethods