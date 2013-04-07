package org.json4s

import _root_.play.api.libs.json._

package object playjson {

  object Converters {
    implicit class Play2Json4sAdapter(playJs: JsValue)(implicit formats: Formats) {
      def toJValue: JValue = playJs match {
        case JsNull => JNull
        case JsUndefined(_) => JNothing
        case JsBoolean(value) => JBool(value)
        case JsNumber(dec) =>
          if (dec.isValidLong) JInt(dec.toBigInt())
          else if (formats.wantsBigDecimal) JDecimal(dec)
          else JDouble(dec.doubleValue())
        case JsString(str) => JString(str)
        case JsArray(lst) => JArray((lst map (new Play2Json4sAdapter(_).toJValue)).toList)
        case JsObject(lst) => JObject((lst map (kv => kv._1 -> new Play2Json4sAdapter(kv._2).toJValue)).toList)
      }
    }

    implicit class Json4sPlayAdapter(json4sJs: JValue){
      def toJsValue: JsValue = json4sJs match {
        case JNothing => JsUndefined("missing json4s ast node")
        case JNull => JsNull
        case JInt(value) => JsNumber(BigDecimal(value))
        case JDouble(value) => JsNumber(BigDecimal(value))
        case JDecimal(value) => JsNumber(value)
        case JString(str) => JsString(str)
        case JArray(lst) => JsArray(lst map (new Json4sPlayAdapter(_).toJsValue))
        case JObject(lst) => JsObject(lst map (kv => kv._1 -> new Json4sPlayAdapter(kv._2).toJsValue))
      }
    }
  }

  object Conversions {
    implicit def play2json4sAst(playJs: JsValue)(implicit formats: Formats): JValue = playJs match {
      case JsNull => JNull
      case JsUndefined(_) => JNothing
      case JsBoolean(value) => JBool(value)
      case JsNumber(dec) =>
        if (dec.isValidLong) JInt(dec.toBigInt())
        else if (formats.wantsBigDecimal) JDecimal(dec)
        else JDouble(dec.doubleValue())
      case JsString(str) => JString(str)
      case JsArray(lst) => JArray((lst map play2json4sAst).toList)
      case JsObject(lst) => JObject((lst map (kv => kv._1 -> play2json4sAst(kv._2))).toList)
    }

    implicit def json4s2Play2(json4sJs: JValue): JsValue = json4sJs match {
      case JNothing => JsUndefined("missing json4s ast node")
      case JNull => JsNull
      case JInt(value) => JsNumber(BigDecimal(value))
      case JDouble(value) => JsNumber(BigDecimal(value))
      case JDecimal(value) => JsNumber(value)
      case JString(str) => JsString(str)
      case JArray(lst) => JsArray(lst map json4s2Play2)
      case JObject(lst) => JsObject(lst map (kv => kv._1 -> json4s2Play2(kv._2)))
    }
  }
}
