package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._
import cats.arrow.Compose
import cats.Semigroup

object RulesGrammar extends Grammar[JsValue, Rule]{
  def max[A](a: A): C[A] = ???
  def min[A](a: A): C[A] = ???
  def notEmpty(a: String): C[String] = ???

  def at[A](p: Path)(k: Rule[JsValue,A]): Rule[JsValue,A] = ???
  implicit def int: Rule[JsValue,Int] = ???
  def knil: Rule[JsValue,shapeless.HNil] = ???
  implicit def string: Rule[JsValue,String] = ???
  def toGoal[Repr, A]: Rule[JsValue,Repr] => Rule[JsValue,Goal[Repr,A]] = ???

  implicit def composeTC: Compose[Rule] = ???
  implicit def mergeTC[I]: Merge[Rule[I, ?]] = ???
  implicit def semigroupTC[A]: Semigroup[Rule[A,A]] = ???
}