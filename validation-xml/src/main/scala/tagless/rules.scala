package jto.validation
package v3.tagless
package xml

import cats.Semigroup

// import cats.syntax.cartesian._

import shapeless.tag.@@

import scala.xml._

trait RulesGrammar extends XmlGrammar[Rule] with RuleConstraints {
  self =>

  type Out = Node
  type P = RulesGrammar

  def mapPath(f: Path => Path): P = ???


  def at[A](p: Path)(k: => Rule[_ >: Out <: Node, A]): Rule[Node, A] = ???

  def opt[A](p: Path)(k: => Rule[_ >: Out <: Node, A]): Rule[Node, Option[A]] = ???

  // import shapeless.{::, HNil, HList}
  import shapeless.HNil

  def knil = Rule.pure(HNil)

  implicit def int = ???
  implicit def string = ???
  implicit def bigDecimal = ???
  implicit def boolean = ???
  implicit def double = ???
  implicit def float = ???
  implicit def jBigDecimal = ???
  implicit def long = ???
  implicit def short = ???
  implicit def seq[A](implicit k: Rule[_ >: Out <: Node, A]) = ???
  implicit def list[A](implicit k: Rule[_ >: Out <: Node, A]) = ???
  implicit def array[A: scala.reflect.ClassTag](implicit k: Rule[_ >: Out <: Node, A]) = ???
  implicit def map[A](implicit k: Rule[_ >: Out <: Node, A]) = ???
  implicit def traversable[A](implicit k: Rule[_ >: Out <: Node, A]) = ???

  def toGoal[Repr, A] = _.map { Goal.apply }

  implicit def composeTC = ???

  implicit def mergeTC: Merge[Rule[Out, ?]] = ???

  implicit def semigroupTC[I0, O]: Semigroup[Rule[I0, O] @@ Root] = ???

}

object RulesGrammar extends RulesGrammar
