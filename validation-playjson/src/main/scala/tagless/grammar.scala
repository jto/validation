package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject, JsNull, JsString, JsNumber, JsBoolean}

import shapeless.tag.@@

trait JsonGrammar[K[_, _]] extends Grammar[JsValue, K] {
  implicit def jsNull: K[JsValue, JsNull.type] @@ Root
  implicit def jsObject: K[JsValue, JsObject] @@ Root
  implicit def jsString: K[JsValue, JsString] @@ Root
  implicit def jsNumber: K[JsValue, JsNumber] @@ Root
  implicit def jsBoolean: K[JsValue, JsBoolean] @@ Root
}
