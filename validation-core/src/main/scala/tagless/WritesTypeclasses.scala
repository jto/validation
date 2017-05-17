package jto.validation
package v3.tagless


import cats.{Semigroup, Monoid}
import cats.arrow.Compose
import shapeless.tag.@@
import types.flip

trait WritesTypeclasses[I] extends Typeclasses[I, flip[Write]#λ]{
  self: Primitives[I, flip[Write]#λ] =>

  protected def outMonoid: Monoid[Out]

  implicit def composeTC =
    new Compose[types.flip[Write]#λ] {
      def compose[A, B, C0](f: Write[C0, B], g: Write[B, A]): Write[C0, A] =
        f andThen g
    }

  import shapeless.{::, HList}

  implicit def mergeTC =
    new Merge[Write[?, Out]] {
      def merge[A, B <: HList](fa: Write[A, Out], fb: Write[B, Out]): Write[A :: B, Out] =
        Write { case a :: b =>
          val wa = fa.writes(a)
          val wb = fb.writes(b)
          outMonoid.combine(fa.writes(a), fb.writes(b))
        }
    }

  implicit def semigroupTC[I0, O]: Semigroup[Write[O, I0] @@ Root] =
    new Semigroup[Write[O, I0] @@ Root] {
      def combine(x: Write[O, I0] @@ Root, y: Write[O, I0] @@ Root): Write[O, I0] @@ Root = x
    }
}