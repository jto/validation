package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js

trait JsonGrammar[K[_, _]] extends Grammar[js.Dynamic, K] {

  type I = js.Dynamic

  implicit def seq[A](implicit k: K[_ >: Out <: I, A]): K[I, Seq[A]]
  implicit def list[A](implicit k: K[_ >: Out <: I, A]): K[I, List[A]]
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: K[_ >: Out <: I, A]): K[I, Array[A]]
  implicit def traversable[A](
      implicit k: K[_ >: Out <: I, A]): K[I, Traversable[A]]
}
