package jto.validation
package v3.tagless


import cats.{Semigroup, Monoid}
import cats.arrow.Compose
import shapeless.{::, HNil}
import shapeless.tag.@@
import types.flip

trait WritesTypeclasses[I] extends Typeclasses[I, flip[Write]#λ]{
  self: Primitives[I, flip[Write]#λ] =>

  def knil = Write[HNil, Out] { _ => iMonoid.empty }

  def liftHList[B](fb: Write[B, I]): Write[B :: HNil, I] =
    fb.contramap { _.head }

  protected def iMonoid: Monoid[Out]

  implicit def composeTC =
    new Compose[types.flip[Write]#λ] {
      def compose[A, B, C0](f: Write[C0, B], g: Write[B, A]): Write[C0, A] =
        f andThen g
    }

  import shapeless.{::, HList}

  implicit def mergeTC =
    new Merge[flip[Write]#λ, Out] {
      def merge[A, B <: HList](fa: Write[A, Out], fb: Write[B, Out]): Write[A :: B, Out] =
        Write { case a :: b =>
          val wa = fa.writes(a)
          val wb = fb.writes(b)
          iMonoid.combine(wa, wb)
        }
    }

  implicit def semigroupTC[I0, O]: Semigroup[Write[O, I0] @@ Root] =
    new Semigroup[Write[O, I0] @@ Root] {
      def combine(x: Write[O, I0] @@ Root, y: Write[O, I0] @@ Root): Write[O, I0] @@ Root = x
    }
}