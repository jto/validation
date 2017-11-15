package jto.validation
package v3.tagless
package jsonast

import shapeless.tag.@@
import jto.validation.jsonast._

trait JsonGrammar[K[_, _]] extends Grammar[JValue, K] {

  type I = JValue

  implicit def seq[A](implicit k: K[_ >: Out <: I, A]): K[I, Seq[A]]
  implicit def list[A](implicit k: K[_ >: Out <: I, A]): K[I, List[A]]
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: K[_ >: Out <: I, A]): K[I, Array[A]]
  implicit def traversable[A](
      implicit k: K[_ >: Out <: I, A]): K[I, Traversable[A]]

  implicit def jsNull: K[JValue, JNull.type] @@ Root
  implicit def jsObject: K[JValue, JObject] @@ Root
  implicit def jsString: K[JValue, JString] @@ Root
  implicit def jsNumber: K[JValue, JNumber] @@ Root
  implicit def jsBoolean: K[JValue, JBoolean] @@ Root
}
