package org

package object json4s {
  type JValue   = JsonAST.JValue
  val  JNothing = JsonAST.JNothing
  val  JNull    = JsonAST.JNull
  type JString  = JsonAST.JString
  val  JString  = JsonAST.JString
  type JNumber  = JsonAST.JNumber
  val  JNumber  = JsonAST.JNumber
  val  JDouble  = JsonAST.JDouble
  val  JDecimal = JsonAST.JDecimal
  val  JInt     = JsonAST.JInt
  type JBool    = JsonAST.JBool
  val  JBool    = JsonAST.JBool
  type JField   = JsonAST.JField
  val  JField   = JsonAST.JField
  type JObject  = JsonAST.JObject
  val  JObject  = JsonAST.JObject
  type JArray   = JsonAST.JArray
  val  JArray   = JsonAST.JArray

  val  TypeInfo = reflect.TypeInfo
  type TypeInfo = reflect.TypeInfo

  trait ParameterNameReader extends reflect.ParameterNameReader

  implicit def string2JsonInput(s: String): JsonInput = StringInput(s)
  implicit def reader2JsonInput(rdr: java.io.Reader): JsonInput = ReaderInput(rdr)
  implicit def stream2JsonInput(stream: java.io.InputStream): JsonInput = StreamInput(stream)
  implicit def file2JsonInput(file: java.io.File): JsonInput = FileInput(file)
  implicit def jvalue2extractable(jv: JValue) = new ExtractableJsonAstNode(jv)
  implicit def jvalue2monadic(jv: JValue) = new MonadicJValue(jv)
  implicit def jsonwritable[T: Writer](a: T) = new ToJsonWritable[T](a)


}
