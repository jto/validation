package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject}
import cats.Semigroup

// import jto.validation.playjson.Rules
// import cats.syntax.cartesian._

import shapeless.tag.@@

object WritesGrammar extends JsonGrammar[types.flip[Write]#λ] with WriteConstraints {

  def at[A](p: Path)(k: => Write[A, JsValue]): Write[A, JsValue] =
    ???

  def opt[A](p: Path)(k: => Write[A, JsValue]): Write[Option[A], JsValue] =
    ???

  def knil = Write{ _ => JsObject(Nil) }

  implicit def int = ???
  implicit def string = ???
  implicit def bigDecimal = ???
  implicit def boolean = ???
  implicit def double = ???
  implicit def float = ???
  implicit def jBigDecimal = ???
  implicit def long = ???
  implicit def short = ???
  implicit def seq[A](implicit k: Write[A, JsValue]) = ???
  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, JsValue]) = ???
  implicit def map[A](implicit k: Write[A, JsValue]) = ???
  implicit def traversable[A](implicit k: Write[A, JsValue]) = ???

  implicit def jsNull = ???
  implicit def jsObject = ???
  implicit def jsString = ???
  implicit def jsNumber = ???
  implicit def jsBoolean = ???

  def toGoal[Repr, A] = ???

  implicit def composeTC = ???
  implicit def mergeTC[I]: Merge[Write[?, I]] = ???
  implicit def semigroupTC[I, O]: Semigroup[Write[O, I] @@ Root] = ???
}
