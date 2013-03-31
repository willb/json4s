package org.json4s

import language.experimental.macros
import macro_readers.JsonReader


object Macros {
  
  def serialize[U](obj: U, writer: JsonWriter[_])(implicit defaultFormats: Formats) =
    macro macroimpls.Serializer.serializeImpl[U]

  def decompose[U](obj: U)(implicit defaultFormats: Formats) =
    macro macroimpls.Serializer.decompose[U]
  
  def deserialize[U](reader: JsonReader)(implicit defaultFormats: Formats) =
    macro macroimpls.Deserializer.deserialize_impl[U]

  def read[U](str: String)(implicit defaultFormats: Formats) =
    macro macroimpls.Deserializer.read_impl[U]
}