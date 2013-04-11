package org.json4s
package macro_readers

import annotation.tailrec

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 8:31 PM
 */

object AstReader {
  def apply(jv: JValue): JsonReader = jv match {
    case JObject(lst) => new AstObjectReader(lst)
    case JArray(lst) => new AstArrayIterator(lst)
    case e => throw new java.lang.IllegalStateException(s"JValue '$e' is not of type JObject or JArray")
  }
}

final class AstObjectReader(lst: List[JField]) extends JsonObjectReader {
  private def getField(name: String) = {
    @tailrec def inner(lst: List[JField]): JValue = lst match {
      case Nil => null
      case h if h.head._1 == name => h.head._2
      case h => inner(h.tail)
    }
    inner(lst)
  }

  lazy val getKeys = {
    val builder = Set.newBuilder[String]
    def buildKeys(fields: List[JField]) {
      if (!fields.isEmpty) {
        builder += fields.head._1
        buildKeys(fields.tail)
      }
    }
    buildKeys(lst)
    builder.result()
  }

  override def getObjectReader(key: String): JsonObjectReader = getField(key) match {
    case JObject(fields) => new AstObjectReader(fields)
    case _ => failStructure(s"Object doesn't have field named '$key' with type Object")
  }

  override def getArrayReader(key: String): JsonArrayIterator = getField(key) match {
    case JArray(arr) => new AstArrayIterator(arr)
    case _ => failStructure(s"Object doesn't have field named '$key' with type Array", this)
  }

  override def getInt(key: String): Int = getField(key) match {
    case JInt(i) => i.toInt
    case _ => failStructure(s"Object doesn't have field named '$key' with type Int", this)
  }

  override def getLong(key: String): Long = getField(key) match {
    case JInt(i) => i.toLong
    case _ => failStructure(s"Object doesn't have field named '$key' with type Long", this)
  }

  override def getFloat(key: String): Float = getField(key) match {
    case JDouble(i) => i.asInstanceOf[Float]
    case JDecimal(i)=> i.toFloat
    case _ => failStructure(s"Object doesn't have field named '$key' with type Float", this)
  }

  override def getDouble(key: String): Double = getField(key) match {
    case JDouble(i) => i
    case JDecimal(i)=> i.toDouble
    case _ => failStructure(s"Object doesn't have field named '$key' with type Double", this)
  }

  override def getBigInt(key: String): BigInt = getField(key) match {
    case JInt(i) => i
    case _ => failStructure(s"Object doesn't have field named '$key' with type BigInt", this)
  }

  override def getBigDecimal(key: String): BigDecimal = getField(key) match {
    case JDecimal(i) => i
    case JDouble(i)  => BigDecimal(i)
    case _ => failStructure(s"Object doesn't have field named '$key' with type BigDecimal", this)
  }

  override def getBool(key: String): Boolean = getField(key) match {
    case JBool(i) => i
    case _ => failStructure(s"Object doesn't have field named '$key' with type Boolean", this)
  }

  override def getString(key: String): String = getField(key) match {
    case JString(i) => i
    case _ => failStructure(s"Object doesn't have field named '$key' with type String", this)
  }

  // Option forms
  override def optObjectReader(key: String) = getField(key) match {
    case JObject(fields) => Some(new AstObjectReader(fields))
    case _ => None
  }

  override def optArrayReader(key: String) = getField(key) match {
    case JArray(arr) => Some(new AstArrayIterator(arr))
    case _ => None
  }

  override def optInt(key: String) = getField(key) match {
    case JInt(i) => Some(i.toInt)
    case _ => None
  }

  override def optLong(key: String) = getField(key) match {
    case JInt(i) => Some(i.toLong)
    case _ => None
  }

  override def optFloat(key: String) = getField(key) match {
    case JDouble(i) => Some(i.asInstanceOf[Float])
    case JDecimal(i)=> Some(i.toFloat)
    case _ => None
  }

  override def optDouble(key: String) = getField(key) match {
    case JDouble(i) => Some(i)
    case JDecimal(i)=> Some(i.toDouble)
    case _ => None
  }

  override def optBigInt(key: String) = getField(key) match {
    case JInt(i) => Some(i)
    case _ => None
  }

  override def optBigDecimal(key: String) = getField(key) match {
    case JDecimal(i) => Some(i)
    case JDouble(i)  => Some(BigDecimal(i))
    case _ => None
  }

  override def optBool(key: String) = getField(key) match {
    case JBool(i) => Some(i)
    case _ => None
  }

  override def optString(key: String) = getField(key) match {
    case JString(i) => Some(i)
    case _ => None
  }
}



final class AstArrayIterator(private var current: List[JValue]) extends JsonArrayIterator {

  override def toString() = "AstArrayIterator(" + current.toString + ")"

  private def next(): JValue = if(hasNext) {
    val old = current.head
    current = current.tail
    old
  } else null

  def hasNext: Boolean = !current.isEmpty

  def getNextObjectReader() = next() match {
    case JObject(lst) => Some(new AstObjectReader(lst))
    case _ => None
  }

  override def getNextArrayReader() = next() match {
    case JArray(lst) => Some(new AstArrayIterator(lst))
    case _ => None
  }

  override def getNextInt() = next() match {
    case JInt(i) => Some(i.toInt)
    case _ => None
  }

  override def getNextLong() = next() match {
    case JInt(i) => Some(i.toLong)
    case _ => None
  }

  override def getNextDouble() = next() match {
    case JDouble(i)  => Some(i)
    case JDecimal(i) => Some(i.toDouble)
    case _ => None
  }

  override def getNextFloat() = next() match {
    case JDouble(i)  => Some(i.asInstanceOf[Float])
    case JDecimal(i) => Some(i.toFloat)
    case _ => None
  }

  override def getNextBigInt() = next() match {
    case JInt(i) => Some(i)
    case _ => None
  }

  override def getNextBigDecimal() = next() match {
    case JDecimal(i) => Some(i)
    case JDouble(i)  => Some(BigDecimal(i))
    case _ => None
  }

  override def getNextBool() = next() match {
    case JBool(i) => Some(i)
    case _ => None
  }

  override def getNextString() = next() match {
    case JString(i) => Some(i)
    case _ => None
  }

  // Pure forms
  override def nextObjectReader(): JsonObjectReader = next() match {
    case JObject(lst) => new AstObjectReader(lst)
    case _ => failStructure(s"Array doesn't have next field of type Object", this)
  }

  override def nextArrayReader(): JsonArrayIterator = next() match {
    case JArray(lst) => new AstArrayIterator(lst)
    case e => failStructure(s"Array doesn't have next field of type Array: $e", this)
  }

  override def nextInt(): Int = next() match {
    case JInt(i) => i.toInt
    case _ => failStructure(s"Array doesn't have next field of type Int", this)
  }

  override def nextLong(): Long = next() match {
    case JInt(i) => i.toLong
    case _ => failStructure(s"Array doesn't have next field of type Long", this)
  }

  override def nextDouble(): Double = next() match {
    case JDouble(i)  => i
    case JDecimal(i) => i.toDouble
    case _ => failStructure(s"Array doesn't have next field of type Double", this)
  }

  override def nextFloat(): Float = next() match {
    case JDouble(i)  => i.asInstanceOf[Float]
    case JDecimal(i) => i.toFloat
    case _ => failStructure(s"Array doesn't have next field of type Float", this)
  }

  override def nextBigInt(): BigInt = next() match {
    case JInt(i) => i
    case _ => failStructure(s"Array doesn't have next field of type BigInt", this)
  }

  override def nextBigDecimal(): BigDecimal = next() match {
    case JDecimal(i) => i
    case JDouble(i)  => BigDecimal(i)
    case _ => failStructure(s"Array doesn't have next field of type BigDecimal", this)
  }

  override def nextBool(): Boolean = next() match {
    case JBool(i) => i
    case _ => failStructure(s"Array doesn't have next field of type Boolean", this)
  }

  override def nextString(): String = next() match {
    case JString(i) => i
    case _ => failStructure(s"Array doesn't have next field of type String", this)
  }
}

