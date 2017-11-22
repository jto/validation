package jto.validation
package v3.tagless


import cats.{Semigroup, Monoid}
import cats.arrow.Compose
import shapeless.{::, HNil}
import shapeless.tag.@@
import types.op

trait WritesTypeclasses[I] extends Typeclasses[I, op[Write]#λ]{
  self: Primitives[I, op[Write]#λ] =>

  def asType[H, B](k: Write[H, Out])(
      implicit G: shapeless.Generic.Aux[B, H]) =
    k.contramap(b => G.to(b))

  def knil = Write[HNil, Out] { _ => iMonoid.empty }

  def liftHList[B](fb: Write[B, I]): Write[B :: HNil, I] =
    fb.contramap { _.head }

  def iMonoid: Monoid[Out]

  implicit def composeTC =
    new Compose[types.op[Write]#λ] {
      def compose[A, B, C0](f: Write[C0, B], g: Write[B, A]): Write[C0, A] =
        f andThen g
    }

  import shapeless.{::, HList}

  implicit def mergeTC =
    new Merge[op[Write]#λ, Out] {
      def merge[A, B <: HList](fa: Write[A, Out], fb: Write[B, Out]): Write[A :: B, Out] =
        Write { case a :: b =>
          val wa = fa.writes(a)
          val wb = fb.writes(b)
          iMonoid.combine(wa, wb)
        }
    }

  implicit def mergeTCOpt: Merge[types.op[Write]#λ, Option[Out]] =
    new Merge[types.op[Write]#λ, Option[Out]] {
      def merge[A, B <: HList](fa: Write[A, Option[Out]], fb: Write[B, Option[Out]]): Write[A :: B, Option[Out]] =
        Write { case a :: b =>
          val wa = fa.writes(a)
          val wb = fb.writes(b)
          (wa, wb) match {
            case (None, None) => None
            case (None, b) => b
            case (a, None) => a
            case (Some(a), Some(b)) => Some(iMonoid.combine(a, b))
          }
        }
    }

  implicit def semigroupTC[I0, O]: Semigroup[Write[O, I0] @@ Root] =
    new Semigroup[Write[O, I0] @@ Root] {
      def combine(x: Write[O, I0] @@ Root, y: Write[O, I0] @@ Root): Write[O, I0] @@ Root = x
    }

  import v3.tagless.MkLazy
  implicit def mkLazy: MkLazy[types.op[Write]#λ] =
    new MkLazy[types.op[Write]#λ] {
      def apply[A, B](k: => Write[B, A]): Write[B, A] =
        Write { b => k.writes(b) }
    }
}