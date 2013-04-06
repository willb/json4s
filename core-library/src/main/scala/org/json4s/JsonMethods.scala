package org.json4s

import java.io.{Reader => JReader, File, InputStream}
import JsonAST._
import java.net.{URI, URL}

sealed trait JsonInput
case class StringInput(string: String) extends JsonInput
case class ReaderInput(reader: JReader) extends JsonInput
case class StreamInput(stream: InputStream) extends JsonInput
case class FileInput(file: File) extends JsonInput
//case class ByteArrayInput(bytes: Array[Byte], offset: Int = 0, length: Int = -1) extends JsonInput
//case class URLInput(url: URL) extends JsonInput
//case class URIInput(url: URI) extends JsonInput


trait JsonMethods[T] {

  def parse(in: JsonInput, useBigDecimalForDouble: Boolean = false): JValue
  def parseOpt(in: JsonInput, useBigDecimalForDouble: Boolean = false): Option[JValue]

  def render(value: JValue): T
  def compact(d: T): String
  def pretty(d: T): String
}
