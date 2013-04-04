package org.json4s
package macroimpls.macrohelpers

import java.io.PrintWriter

/**
 * @author Bryce Anderson
 * Created on 4/1/13 at 8:35 AM
 */



class FastTextWriter(writer: java.io.Writer) extends JsonWriter[String] { self =>
  val isArray = 0
  val isArrayFirst = 1
  val isObject = 2
  val isObjectFirst = 3

  var state: Int = _

  def startArray() = { writer.write('['); state = isArrayFirst; self }

  def endArray() = { writer.write(']'); self }

  def startObject() = { writer.write('{'); state = isObjectFirst; self }

  def endObject() = { writer.write('}'); self }

  def string(value: String): JsonWriter[String] = {
    writer.write('"')
    JsonAST.quote(value, writer)

    writer.write('"')
    self
  }

  def byte(value: Byte) = { checkFirst(); writer.write(value); self }

  def int(value: Int) = { checkFirst(); writer.write(value.toString); self }

  def long(value: Long) = { checkFirst(); writer.write(value.toString); self }

  def bigInt(value: BigInt) = { checkFirst(); writer.write(value.toString); self }

  def boolean(value: Boolean) = { checkFirst(); writer.write(value.toString); self }

  def short(value: Short) = { checkFirst(); writer.write(value.toString); self }

  def float(value: Float) = { checkFirst(); writer.write(value.toString); self }

  def double(value: Double) = { checkFirst(); writer.write(value.toString); self }

  def bigDecimal(value: BigDecimal) = { checkFirst(); writer.write(value.toString); self }

  def checkFirst() {
    if(state == isArrayFirst) {
      state = isArray
    } else if (state == isArray || state == isObject) {
      writer.write(',')
    } else if (state == isObjectFirst) {
      state = isObject
    }
  }

  def startField(name: String) = {
    checkFirst()
    writer.write('"')
    writer.write(name)
    writer.write('"')
    writer.write(':')
    self
  }

  def result: String = writer.toString

  def addJValue(jv: _root_.org.json4s.JValue): JsonWriter[String] = ???
}