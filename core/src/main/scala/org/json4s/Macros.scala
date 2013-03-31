package org.json4s

import io.{BufferRecycler, SegmentedStringWriter}
import language.experimental.macros
import macro_readers.JsonReader


trait Macros {
  
  def serialize[U](obj: U, writer: JsonWriter[_])(implicit defaultFormats: Formats) =
    macro macroimpls.Serializer.serializeImpl[U]

  def decompose[U](obj: U)(implicit defaultFormats: Formats): JValue =
    macro macroimpls.Serializer.decompose[U]

  def deserialize[U](reader: JsonReader)(implicit defaultFormats: Formats): U =
    macro macroimpls.Deserializer.deserialize_impl[U]

  def read[U](str: String)(implicit defaultFormats: Formats): U =
    macro macroimpls.Deserializer.read_impl[U]
}

object Macros extends Macros

trait MacroReader[U] extends Reader[U] {
  def extract(jvalue: JValue)(implicit defaultFormats: Formats): U =
    Macros.deserialize[U](macro_readers.AstReader(jvalue))

  def read(str: String)(implicit defaultFormats: Formats): U = Macros.read(str)
}

trait MacroWriter[-U] extends Writer[U] {
  def write(obj: U)(implicit defaultFormats: Formats): JValue = Macros.decompose[U](obj)
  def writeToStream(obj: U, writer: java.io.Writer)(implicit defaultFormats: Formats): java.io.Writer = {
    val w = JsonWriter.streaming(writer)
    Macros.serialize(obj, w)
    writer
  }
  def writeToStreamPretty(obj: U, writer: java.io.Writer)(implicit defaultFormats: Formats): java.io.Writer = {
    val w = JsonWriter.streamingPretty(writer)
    Macros.serialize(obj, w)
    writer
  }
  def writeString(obj: U)(implicit defaultFormats: Formats): String = {
    val sw = new SegmentedStringWriter(new BufferRecycler)
    try {
      Macros.serialize(obj, JsonWriter.streaming(sw))
      sw.getAndClear
    } finally {
      sw.close()
    }
  }
  def writeStringPretty(obj: U)(implicit defaultFormats: Formats): String = {
    val sw = new SegmentedStringWriter(new BufferRecycler)
    try {
      Macros.serialize(obj, JsonWriter.streamingPretty(sw))
      sw.getAndClear
    } finally {
      sw.close()
    }
  }
}

trait MacroJsonFormat[U] extends MacroReader[U] with MacroWriter[U] with JsonFormat[U]