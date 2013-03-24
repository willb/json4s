package org.json4s
package playground

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 8:31 PM
 */
object AstReader {
  // Bootstrap methods
  def fromAst(jv: JValue): JsonReader = jv match {
    case j: JObject => new AstObjectReader(j)
    case a: JArray => new AstArrayReader(a)
    case JDouble(d) => JsonDouble(d)
    case JInt(i) => JsonBigInt(i)
    case JBool(b) => JsonBool(b)
    case JDecimal(d) => JsonBigDecimal(d)
    case JNothing =>  new AstObjectReader(JObject())
    case JNull =>  new AstObjectReader(JObject())
    case JString(s) => new JsonString(s)
  }
}

class AstObjectReader(obj: JObject) extends JsonObjectReader with JsonComplexReaderImpl[String] {

  def apply(key: String): JsonReader = {
    AstReader.fromAst(obj.obj.find{
      case (k, v) => k == key
    }.get._2)
  }
}

class AstArrayReader(arr: JArray) extends JsonArrayReader with JsonComplexReaderImpl[Int] {
  def apply(i: Int): JsonReader = AstReader.fromAst(arr.apply(i))

  def map[U](f: (JsonReader) => U) = arr.arr.map(v => f(AstReader.fromAst(v)))
}

