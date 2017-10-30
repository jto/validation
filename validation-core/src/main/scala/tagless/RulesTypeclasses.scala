package jto.validation
package v3.tagless

import shapeless.{::, HList, HNil}
import shapeless.tag.@@
import cats.Semigroup
import cats.syntax.cartesian._

trait RulesTypeclasses[I] extends Typeclasses[I, Rule]{
  self: Primitives[I, Rule] =>

  def knil: Rule[Out, HNil] = Rule.zero[Out].map { _ => HNil }
  def kopt: Rule[Option[Out], HNil] = Rule.zero[Option[Out]].map { _ => HNil }

  def liftHList[B](fb: Rule[Out, B]): Rule[Out, B :: HNil] =
    fb.map(_ :: HNil)

  implicit def composeTC = Rule.ruleCompose

  implicit def mergeTC: Merge[Rule, Out] =
    new Merge[Rule, Out] {
      def merge[A, B <: HList](fa: Rule[Out, A], fb: Rule[Out, B]): Rule[Out, A :: B] =
        (fa |@| fb).map(_ :: _)
    }

  implicit def mergeTCOpt: Merge[Rule, Option[Out]] =
    new Merge[Rule, Option[Out]] {
      def merge[A, B <: HList](fa: Rule[Option[Out], A],fb: Rule[Option[Out], B]): Rule[Option[Out], A :: B] =
        Rule { mx =>
          import cats.syntax.cartesian._
          (fa.validate(mx) |@| fb.validate(mx)).map { (a, b) =>
            (Path, a :: b)
          }
        }
    }

  implicit def semigroupTC[I0, O]: Semigroup[Rule[I0, O] @@ Root] =
    Rule.ruleSemigroup
}