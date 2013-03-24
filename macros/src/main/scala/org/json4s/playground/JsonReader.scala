package org.json4s
package playground

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 12:39 PM
 */

class InvalidStructure(msg: String, value: JsonReader) extends java.lang.Exception(msg + value.getClass().toString)

sealed trait JsonReader

object JsonReader {
  def apply(in: Any): JsonReader = in match {
    case i: Int  => JsonInt(i)
    case l: Long => JsonLong(l)
    case b: BigInt => JsonBigInt(b)
    case i: Double => JsonDouble(i)
    case f: Float => JsonFloat(f)
    case b: BigDecimal => JsonBigDecimal(b)
    case b: Boolean => JsonBool(b)
    case s: String => JsonString(s)
    case e => throw new InvalidStructure(s"Value $e is not primative type", null)
  }

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

abstract class JsonComplexReader[KeyType] extends JsonReader {
  def getObjectReader(key: KeyType): JsonObjectReader
  def getArrayReader(key: KeyType): JsonArrayReader
  def getInt(key: KeyType): Int
  def getLong(key: KeyType): Long
  def getDouble(key: KeyType): Double
  def getBigInt(key: KeyType): BigInt
  def getBigDecimal(key: KeyType): BigDecimal
  def getBool(key: KeyType): Boolean
  def getString(key: KeyType): String
}

// inefficient implementation, but good for bootstrapping.
trait JsonComplexReaderImpl[KeyType] extends JsonComplexReader[KeyType] {
  def apply(i: KeyType): JsonReader

  // Some convenience methods that should be overridden for performance
  override def getObjectReader(key: KeyType) = apply(key) match {
    case r: JsonObjectReader => r
    case e => throw new InvalidStructure("Json not of type Object", e)
  }

  override def getArrayReader(key: KeyType) = apply(key) match {
    case r: JsonArrayReader => r
    case e => throw new InvalidStructure("Json not of type Array", e)
  }

  override def getInt(key: KeyType) = apply(key) match {
    case JsonInt(i) => i
    case JsonBigInt(i) => i.toInt
    case JsonLong(i) => i.asInstanceOf[Int]
    case e => throw new InvalidStructure("Json not of type Int", e)
  }

  def getLong(key: KeyType): Long = apply(key) match {
    case JsonLong(i) => i
    case JsonBigInt(i) => i.toLong
    case JsonInt(i) => i
    case e => throw new InvalidStructure("Json not of type Long", e)
  }

  def getDouble(key: KeyType): Double = apply(key) match {
    case JsonDouble(i) => i
    case JsonFloat(i) => i
    case JsonBigDecimal(i) => i.toDouble
    case e => throw new InvalidStructure("Json not of type Double", e)
  }

  def getBigInt(key: KeyType): BigInt = apply(key) match {
    case JsonBigInt(i) => i
    case JsonLong(i) => i
    case JsonInt(i) => i
    case e => throw new InvalidStructure("Json not of type BigInt", e)
  }

  def getBigDecimal(key: KeyType): BigDecimal = apply(key) match {
    case JsonDouble(i) => i
    case JsonFloat(i) => i
    case JsonBigDecimal(i) => i
    case e => throw new InvalidStructure("Json not of type BigDecimal", e)
  }

  def getBool(key: KeyType): Boolean = apply(key) match {
    case JsonBool(i) => i
    case JsonInt(i) => if (i == 0) false else true
    case e => throw new InvalidStructure("Json not of type Bool", e)
  }

  def getString(key: KeyType): String = apply(key) match {
    case p: JsonPrimative[_] => p.toString
    case e => throw new InvalidStructure("Json not of type string", e)
  }
}

abstract class JsonObjectReader extends JsonComplexReader[String]

abstract class JsonArrayReader extends JsonComplexReader[Int] {
  def map[T](f: JsonReader => T): List[T]
}

trait JsonPrimative[I] extends JsonReader {
  def value: I
  override def toString = value.toString
}

// It feels silly essentially remaking the ast because I cannot extend the current objects
case class JsonString(value: String) extends JsonPrimative[String] { override def toString = value }
case class JsonInt(value: Int) extends JsonPrimative[Int]
case class JsonLong(value: Long) extends JsonPrimative[Long]
case class JsonBigInt(value: BigInt) extends JsonPrimative[BigInt]
case class JsonDouble(value: Double) extends JsonPrimative[Double]
case class JsonFloat(value: Float) extends JsonPrimative[Float]
case class JsonBigDecimal(value: BigDecimal) extends JsonPrimative[BigDecimal]
case class JsonBool(value: Boolean) extends JsonPrimative[Boolean]

class AstObjectReader(obj: JObject) extends JsonObjectReader with JsonComplexReaderImpl[String] {

  def apply(key: String): JsonReader = {
    JsonReader.fromAst(obj.obj.find{
      case (k, v) => k == key
    }.get._2)
  }
}

class AstArrayReader(arr: JArray) extends JsonArrayReader with JsonComplexReaderImpl[Int] {
  def apply(i: Int): JsonReader = JsonReader.fromAst(arr.apply(i))

  def map[U](f: (JsonReader) => U) = arr.arr.map(v => f(JsonReader.fromAst(v)))
}


