package org.json4s
package playground

/**
 * @author Bryce Anderson
 * Created on 3/23/13 at 12:39 PM
 */

class InvalidStructure(msg: String, value: JsonReader) extends java.lang.Exception(msg + value.getClass().toString)

sealed trait JsonReader

trait JsonObjectReader extends JsonReader {
  def getObjectReader(key: String): JsonObjectReader
  def getArrayReader(key: String): JsonArrayIterator
  def getInt(key: String): Int
  def getLong(key: String): Long
  def getFloat(key: String): Float
  def getDouble(key: String): Double
  def getBigInt(key: String): BigInt
  def getBigDecimal(key: String): BigDecimal
  def getBool(key: String): Boolean
  def getString(key: String): String
}

trait JsonArrayIterator extends JsonReader {
  def hasNext: Boolean
  def nextObjectReader: JsonObjectReader
  def nextArrayReader: JsonArrayIterator
  def nextInt: Int
  def nextLong: Long
  def nextFloat: Float
  def nextDouble: Double
  def nextBigInt: BigInt
  def nextBigDecimal: BigDecimal
  def nextBool: Boolean
  def nextString: String
}
