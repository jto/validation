package jto.validation
package v3

import shapeless.{ Path => _, _ }
import ops.hlist._

case class As[A](path: Path) {
  def ~[B](fb: As[B]): AsSyntax.AsSyntax2[A, B] =
    AsSyntax.AsSyntax2(this, fb)
}

// trait SequenceH[F[_], H <: HList] {
//   type Out <: HList
//   def sequence(h: H): F[Out]
// }
//
// object SequenceH {
//   type Aux[F[_], H <: HList, Out0 <: HList] = SequenceH[F, H] { type Out = Out0 }
//   def apply[F[_], H <: HList](implicit seqH: SequenceH[F, H]): Aux[F, H, seqH.Out] = seqH
// }
//
// object ApplicativeSequenceH {
//   import cats.Applicative
//   import SequenceH.Aux
//
//   implicit def hnilSeqenceH[F[_]: Applicative]: Aux[F, HNil, HNil] =
//     new SequenceH[F, HNil] {
//       type Out = HNil
//       def sequence(h: HNil) = Applicative[F].pure(HNil)
//     }
//
//   implicit def applySequenceH[F[_]: Applicative, H, T <: HList](
//     implicit seqH: SequenceH[F, T]
//   ): Aux[F, F[H] :: T, H :: seqH.Out] =
//     new SequenceH[F, F[H] :: T] {
//       type Out = H :: seqH.Out
//
//       def sequence(h: F[H] :: T): F[H :: seqH.Out] = {
//         val head :: tail = h
//         Applicative[F].map2(head, seqH.sequence(tail))(_ :: _)
//       }
//
//     }
// }

/**
* Contravariant Applicative Functor typeclass
* @see https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html
*/
trait Divisible[F[_]] extends cats.functor.Contravariant[F] {
  def divide[A, B, C](f: A => (B, C))(fb: F[B])(fc: F[C]): F[A]
  def conquer[A]: F[A]
}

object Divisible {
  def apply[F[_]](implicit d: Divisible[F]) = d

  implicit def writeDivisible[I: cats.Monoid] =
    new Divisible[Write[?, I]] {
      type F[A] = Write[A, I]
      def divide[A, B, C](f: A => (B, C))(fb: F[B])(fc: F[C]): F[A] =
        Write { a =>
          val (b, c) = f(a)
          cats.Monoid[I].combine(fb.writes(b), fc.writes(c))
        }
      def conquer[A]: F[A] =
        Write{_ => cats.Monoid[I].empty }
      def contramap[A, B](wa: F[A])(f: B => A): F[B] = wa.contramap(f)
    }
}

trait HSequence0[F[_]] {
  def empty: F[HNil]
  def sequence[A, H <: HList](fa: F[A], fh: F[H]): F[A :: H]
}

object HSequence0 {
  import cats.Applicative

  implicit def applicativeHSequence0[F[_]: Applicative]: HSequence0[F] =
    new HSequence0[F] {
      def empty = Applicative[F].pure(HNil)
      def sequence[A, H <: HList](fa: F[A], fh: F[H]): F[A :: H] =
        Applicative[F].map2(fa, fh)((a, h) => a :: h)
    }
}
