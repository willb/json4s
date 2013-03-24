package org.json4s
package playground

import annotation.tailrec

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 8:31 PM
 */

final class AstObjectReader(lst: List[JField]) extends JsonObjectReader {
  private def getField(name: String) = {
    @tailrec def inner(lst: List[JField]): JValue = lst match {
      case Nil => null
      case h if h.head._1 == name => h.head._2
      case h => inner(h.tail)
    }
    inner(lst)
  }

  def getObjectReader(key: String): JsonObjectReader = getField(key) match {
    case JObject(fields) => new AstObjectReader(fields)
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Object", this)
  }

  def getArrayReader(key: String): JsonArrayIterator = getField(key) match {
    case JArray(arr) => new AstArrayIterator(arr)
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Array", this)
  }

  def getInt(key: String): Int = getField(key) match {
    case JInt(i) => i.toInt
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Int", this)
  }

  def getLong(key: String): Long = getField(key) match {
    case JInt(i) => i.toLong
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Long", this)
  }

  def getFloat(key: String): Float = getField(key) match {
    case JDouble(i) => i.asInstanceOf[Float]
    case JDecimal(i)=> i.toFloat
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Float", this)
  }

  def getDouble(key: String): Double = getField(key) match {
    case JDouble(i) => i
    case JDecimal(i)=> i.toDouble
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Double", this)
  }

  def getBigInt(key: String): BigInt = getField(key) match {
    case JInt(i) => i
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type BigInt", this)
  }

  def getBigDecimal(key: String): BigDecimal = getField(key) match {
    case JDecimal(i) => i
    case JDouble(i)  => BigDecimal(i)
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type BigDecimal", this)
  }

  def getBool(key: String): Boolean = getField(key) match {
    case JBool(i) => i
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type Boolean", this)
  }

  def getString(key: String): String = getField(key) match {
    case JString(i) => i
    case _ => throw new InvalidStructure(s"Object doesn't have field named '$key' with type String", this)
  }
}

final class AstArrayIterator(private var current: List[JValue]) extends JsonArrayIterator {

  private def next(): JValue = if(hasNext) {
    val old = current.head
    current = current.tail
    old
  } else null

  def hasNext: Boolean = current.isEmpty

  def nextObjectReader(): JsonObjectReader = next() match {
    case JObject(lst) => new AstObjectReader(lst)
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Object", this)
  }

  def nextArrayReader(): JsonArrayIterator = next() match {
    case JArray(lst) => new AstArrayIterator(lst)
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Array", this)
  }

  def nextInt(): Int = next() match {
    case JInt(i) => i.toInt
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Int", this)
  }

  def nextLong(): Long = next() match {
    case JInt(i) => i.toLong
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Long", this)
  }

  def nextDouble(): Double = next() match {
    case JDouble(i)  => i
    case JDecimal(i) => i.toDouble
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Double", this)
  }

  def nextFloat(): Float = next() match {
    case JDouble(i)  => i.asInstanceOf[Float]
    case JDecimal(i) => i.toFloat
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Float", this)
  }

  def nextBigInt(): BigInt = next() match {
    case JInt(i) => i
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type BigInt", this)
  }

  def nextBigDecimal(): BigDecimal = next() match {
    case JDecimal(i) => i
    case JDouble(i)  => BigDecimal(i)
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type BigDecimal", this)
  }

  def nextBool(): Boolean = next() match {
    case JBool(i) => i
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type Boolean", this)
  }

  def nextString(): String = next() match {
    case JString(i) => i
    case _ => throw new InvalidStructure(s"Array doesn't have next field of type String", this)
  }
}

