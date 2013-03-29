package org.json4s
package playground

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 12:39 PM
 */

class InvalidStructure(msg: String, value: JsonReader) extends org.json4s.ParserUtil.ParseException(msg + value.getClass().toString, null)

sealed trait JsonReader {
  def fail(msg: String) = throw new java.lang.IllegalStateException(msg)
}

trait JsonObjectReader extends JsonReader {
  def getKeys: Seq[String]

  def getObjectReader(key: String): JsonObjectReader = optObjectReader(key).get
  def getArrayReader(key: String): JsonArrayIterator = optArrayReader(key).get
  def getInt(key: String): Int                       = optInt(key).get
  def getLong(key: String): Long                     = optLong(key).get
  def getFloat(key: String): Float                   = optFloat(key).get
  def getDouble(key: String): Double                 = optDouble(key).get
  def getBigInt(key: String): BigInt                 = optBigInt(key).get
  def getBigDecimal(key: String): BigDecimal         = optBigDecimal(key).get
  def getBool(key: String): Boolean                  = optBool(key).get
  def getString(key: String): String                 = optString(key).get

  // Option forms
  def optObjectReader(key: String): Option[JsonObjectReader]
  def optArrayReader(key: String): Option[JsonArrayIterator]
  def optInt(key: String): Option[Int]
  def optLong(key: String): Option[Long]
  def optFloat(key: String): Option[Float]
  def optDouble(key: String): Option[Double]
  def optBigInt(key: String): Option[BigInt]
  def optBigDecimal(key: String): Option[BigDecimal]
  def optBool(key: String): Option[Boolean]
  def optString(key: String): Option[String]
}

trait JsonArrayIterator extends JsonReader {
  def hasNext: Boolean
  def nextObjectReader: JsonObjectReader        = getNextObjectReader.get
  def nextArrayReader: JsonArrayIterator        = getNextArrayReader.get
  def nextInt: Int                              = getNextInt.get
  def nextLong: Long                            = getNextLong.get
  def nextFloat: Float                          = getNextFloat.get
  def nextDouble: Double                        = getNextDouble.get
  def nextBigInt: BigInt                        = getNextBigInt.get
  def nextBigDecimal: BigDecimal                = getNextBigDecimal.get
  def nextBool: Boolean                         = getNextBool.get
  def nextString: String                        = getNextString.get

  // Option forms
  def getNextObjectReader: Option[JsonObjectReader]
  def getNextArrayReader: Option[JsonArrayIterator]
  def getNextInt: Option[Int]
  def getNextLong: Option[Long]
  def getNextFloat: Option[Float]
  def getNextDouble: Option[Double]
  def getNextBigInt: Option[BigInt]
  def getNextBigDecimal: Option[BigDecimal]
  def getNextBool: Option[Boolean]
  def getNextString: Option[String]
}
