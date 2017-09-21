package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject, JsNull, JsString, JsNumber, JsBoolean}

import shapeless.tag.@@

trait JsonGrammar[K[_, _]] extends Grammar[JsValue, K] {

  type I = JsValue

  implicit def seq[A](implicit k: K[_ >: Out <: I, A]): K[I, Seq[A]]
  implicit def list[A](implicit k: K[_ >: Out <: I, A]): K[I, List[A]]
  implicit def array[A: scala.reflect.ClassTag](implicit k: K[_ >: Out <: I, A]): K[I, Array[A]]
  implicit def traversable[A](implicit k: K[_ >: Out <: I, A]): K[I, Traversable[A]]


  implicit def jsNull: K[JsValue, JsNull.type] @@ Root
  implicit def jsObject: K[JsValue, JsObject] @@ Root
  implicit def jsString: K[JsValue, JsString] @@ Root
  implicit def jsNumber: K[JsValue, JsNumber] @@ Root
  implicit def jsBoolean: K[JsValue, JsBoolean] @@ Root
}
