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

  def decomposeWithBuilder[U, T](obj: U, builder: JsonWriter[T])(implicit formats: Formats) =
  macro macroimpls.Serializer.decomposeWithBuilder_impl[U,T]

  def extract[U](jvalue: JValue)(implicit defaultFormats: Formats): U =
  macro macroimpls.Deserializer.extract_impl[U]
}

object Macros extends Macros

trait MacroReader[U] extends Reader[U] {
  def extract(jvalue: JValue)(implicit defaultFormats: Formats): U =
    macro macroimpls.Deserializer.extract_impl[U]

  def read(str: String)(implicit defaultFormats: Formats): U =
  macro macroimpls.Deserializer.read_impl[U]
}

trait MacroWriter[-U] extends Writer[U] {
  def write(obj: U)(implicit defaultFormats: Formats): JValue =
    macro macroimpls.Serializer.decompose[U]
  def writeToStream[W <: java.io.Writer](obj: U, w: W)(implicit defaultFormats: Formats): W =
    macro macroimpls.Serializer.serializeToStreamWriter[U, W]

  def writeToStreamPretty[W <: java.io.Writer](obj: U, w: W)(implicit defaultFormats: Formats): W =
    macro macroimpls.Serializer.serializePrettyToStreamWriter[U, W]

  def writeString(obj: U)(implicit defaultFormats: Formats): String =
    macro macroimpls.Serializer.serializeToString[U]

  def writeStringPretty(obj: U)(implicit defaultFormats: Formats): String =
    macro macroimpls.Serializer.serializePrettyToString[U]
}

