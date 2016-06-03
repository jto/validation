package jto.validation
package jsonast

import play.api.libs.json._

object Ast {
  def to(ast: JValue): JsValue =
    ast match {
      case JString(value) => JsString(value)
      case JBoolean(value) => JsBoolean(value)
      case JNull => JsNull
      case JArray(value) => JsArray(value.map(to))
      case JObject(value) => JsObject(value.mapValues(to))
      case JNumber(value) => JsNumber(BigDecimal(value))
    }

  def from(json: JsValue): JValue =
    json match {
      case JsString(value) => JString(value)
      case JsBoolean(value) => JBoolean(value)
      case JsNull => JNull
      case JsArray(value) => JArray(value.map(from))
      case JsObject(value) => JObject(value.mapValues(from))
      case JsNumber(value) => JNumber(value.toString)
    }
}
