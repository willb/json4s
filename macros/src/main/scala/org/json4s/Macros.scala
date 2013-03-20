package org.json4s

import language.experimental.macros


object Macros {
  
  def serialize[U](obj: U, writer: JsonWriter[_])(implicit defaultFormats: Formats) =
      macro macroimpls.Serializer.serializeImpl[U]

  def decompose[U](obj: U)(implicit defaultFormats: Formats) =
      macro macroimpls.Serializer.decompose[U]
  
  def deserialize[U](params: JValue)(implicit defaultFormats: Formats) =
      macro macroimpls.Deserializer.deserialize_impl[U]
  def deserializeEither[U](params: JValue)(implicit defaultFormats: Formats): Either[ParserUtil.ParseException,U] =
      macro macroimpls.Deserializer.eitherDeserialize_impl[U]
}