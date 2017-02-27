package jto.validation
package jsonast

import play.api.libs.json._

object Ast {
  val to: Write[JValue, JsValue] = Write[JValue, JsValue] {
    case JNull           => JsNull
    case JObject (value) => JsObject(value.mapValues(to.writes))
    case JArray  (value) => JsArray(value.map(to.writes))
    case JBoolean(value) => JsBoolean(value)
    case JString (value) => JsString(value)
    case JNumber (value) => JsNumber(BigDecimal(value))
  }

  private def totalFrom(jsValue: JsValue): JValue = jsValue match {
    case JsNull           => JNull
    case JsObject (value) => JObject(value.mapValues(totalFrom).toMap)
    case JsArray  (value) => JArray(value.map(totalFrom).toVector)
    case JsBoolean(value) => JBoolean(value)
    case JsString (value) => JString(value)
    case JsNumber (value) => JNumber(value.toString)
  }

  val from: Rule[JsValue, JValue] = Rule(Path)(x => Valid(totalFrom(x)))
}
