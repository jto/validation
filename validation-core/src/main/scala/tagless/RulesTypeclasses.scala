package jto.validation
package v3.tagless

import shapeless.{::, HList, HNil}
import shapeless.tag.@@
import cats.Semigroup
import cats.syntax.apply._

trait RulesTypeclasses[I] extends Typeclasses[I, Rule]{
  self: Primitives[I, Rule] =>

  def asType[H, B](k: Rule[_ >: this.Out <: I, H])(
      implicit G: shapeless.Generic.Aux[B, H]): Rule[_ >: Out <: I, B] =
    k.map(x => G.from(x))

  def knil: Rule[Out, HNil] = Rule.zero[Out].map { _ => HNil }

  def liftHList[B](fb: Rule[Out, B]): Rule[Out, B :: HNil] =
    fb.map(_ :: HNil)

  implicit def composeTC = Rule.ruleArrow

  implicit def mergeTC: Merge[Rule, Out] =
    new Merge[Rule, Out] {
      def merge[A, B <: HList](fa: Rule[Out, A], fb: Rule[Out, B]): Rule[Out, A :: B] = {
        Rule.applicativeRule.map2(fa, fb)(_ :: _)
      }
    }

  implicit def mergeTCOpt: Merge[Rule, Option[Out]] =
    new Merge[Rule, Option[Out]] {
      def merge[A, B <: HList](fa: Rule[Option[Out], A],fb: Rule[Option[Out], B]): Rule[Option[Out], A :: B] =
        Rule { mx =>
          (fa.validate(mx), fb.validate(mx)).mapN { (a, b) =>
            (Path, a :: b)
          }
        }
    }

  implicit def semigroupTC[I0, O]: Semigroup[Rule[I0, O] @@ Root] =
    Rule.ruleSemigroup

  implicit def mkLazy =
    new v3.tagless.MkLazy[Rule] {
      def apply[A, B](k: => Rule[A, B]): Rule[A, B] =
        Rule { a => k.validateWithPath(a) }
    }
}