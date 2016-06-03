package jto.validation
package jsonast

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Ast {
  // https://www.scala-js.org/doc/interoperability/types.html
  def to(ast: JValue): js.Dynamic =
    (ast match {
      case JString(value) => value
      case JBoolean(value) => value
      case JNull => null
      case JArray(value) => value.map(to).toJSArray
      case JObject(value) => value.mapValues(to).toJSDictionary
      case JNumber(value) =>
        val d = value.toDouble; if (d.isNaN || d.isInfinity) null else d
    }).asInstanceOf[js.Dynamic]

  def from(json: js.Dynamic): JValue =
    json match {
      case v if (v: Any).isInstanceOf[String] =>
        JString(v.asInstanceOf[String])
      case v if v.isInstanceOf[Boolean] => JBoolean(v.asInstanceOf[Boolean])
      case v if v == null => JNull
      case v if v.isInstanceOf[js.Array[_]] =>
        JArray(v.asInstanceOf[js.Array[js.Dynamic]].map(from))
      case v if js.typeOf(v) == "object" =>
        JObject(v.asInstanceOf[js.Dictionary[js.Dynamic]].mapValues(from))
      case v if js.typeOf(v) == "number" => JNumber(v.toString)
    }
}
