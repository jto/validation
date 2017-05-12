package jto.validation
package v3.tagless

import shapeless.{::, HList}
import shapeless.tag.@@
import cats.Semigroup
import cats.syntax.cartesian._

trait RulesTypeclasses[I] extends Typeclasses[I, Rule]{
  self: Primitives[I, Rule] =>

  implicit def composeTC = Rule.ruleCompose

  implicit def mergeTC: Merge[Rule[Out, ?]] =
    new Merge[Rule[Out, ?]] {
      def merge[A, B <: HList](fa: Rule[Out, A], fb: Rule[Out, B]): Rule[Out, A :: B] =
        (fa |@| fb).map(_ :: _)
    }

  implicit def semigroupTC[I0, O]: Semigroup[Rule[I0, O] @@ Root] =
    Rule.ruleSemigroup
}