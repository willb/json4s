package org.json4s

import language.experimental.macros


object Macros {
  type ParamsTpe = playground.ValueProvider[_]
  
  def serialize[U](obj:U, name:String, writer: macroimpls.Serializer.Writer)(implicit defaultFormats: Formats) = macro macroimpls.Serializer.impl[U]
  def serObject[U](obj: U)(implicit defaultFormats: Formats) = macro macroimpls.Serializer.implSerToObj[U]
  
  def deserialize[U](params: ParamsTpe,name:String)(implicit defaultFormats: Formats) = macro macroimpls.Deserializer.deserialize_impl[U]
  def marshalObject[U](params: ParamsTpe)(implicit defaultFormats: Formats) = macro macroimpls.Deserializer.marshalObject_impl[U]

}