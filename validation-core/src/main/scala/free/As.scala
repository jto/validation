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

// trait HSequence[F[_]] {
//   def sequence(implicit lf: LeftFolder[Imps, F[HNil], seqH.type]): lf.Out
// }

trait Materialized[F[_]] {
  import cats.Apply

  type Imps <: HList
  val h: Imps

  object seqH extends Poly2 {
    implicit def caseApply[H <: HList, A](implicit p: Prepend[H, A :: HNil], ap: Apply[F]) =
      at[F[H], F[A]] { (fh, fa) =>
        ap.map2(fh, fa)(_ :+ _)
      }

    implicit def caseDivisibleHList[H, T <: HList, A, POut <: HList](implicit
      div: Divisible[F],
      pre: Prepend.Aux[H :: T, A :: HNil, POut],
      init: Init.Aux[POut, H :: T],
      last: Last.Aux[POut, A]
    ) =
      at[F[H :: T], F[A]] { (fh, fa) =>
        val f = (hl: pre.Out) => {
          val a = last(hl)
          val ht = init(hl)
          (ht, a)
        }
        div.divide(f)(fh)(fa)
      }

    implicit def caseDivisibleHNil[A](implicit div: Divisible[F]) =
      at[F[HNil], F[A]] { (fh, fa) =>
        val f = (hl: A :: HNil) => {
          val a :: HNil = hl
          (HNil: HNil, a)
        }
        div.divide(f)(fh)(fa)
      }
  }

  def sequence(implicit ap: cats.Applicative[F], lf: LeftFolder[Imps, F[HNil], seqH.type]) = {
    val z = ap.pure(HNil: HNil)
    h.foldLeft(z)(seqH)
  }

  def sequence(implicit div: Divisible[F], rf: LeftFolder[Imps, F[HNil], seqH.type]) = {
    val z = div.conquer[HNil]
    h.foldLeft(z)(seqH)
  }

  /*
  import jto.validation._, v3._, jsonast._, Rules._, Writes._
  import shapeless._
  import Divisible._

  val __ = jto.validation.Path

  val r1: Rule[JValue, Int] = (__ \ "foo").read[JValue, Int]
  val r2: Rule[JValue, String] = (__ \ "bar").read[JValue, String]
  val r3: Rule[JValue, Double] = (__ \ "baz").read[JValue, Double]
  val rs = r1 :: r2 :: r3 :: HNil
  val matA =
    new Materialized[({type R[A] = Rule[JValue, A]})#R] {
      type Imps =  Rule[JValue, Int] :: Rule[JValue, String] :: Rule[JValue, Double] :: HNil
      val h = rs
    }
  // matA.merge: res5: Rule[JValue, Int :: String :: Double :: HNil]

  val w1: Write[Int, JObject] = (__ \ "foo").write[Int, JObject]
  val w2: Write[String, JObject] = (__ \ "bar").write[String, JObject]
  val w3: Write[Double, JObject] = (__ \ "baz").write[Double, JObject]
  val ws = w1 :: w2 :: w3 :: HNil
  val matD =
    new Materialized[({type W[A] = Write[A, JObject]})#W] {
      type Imps =  Write[Int, JObject] :: Write[String, JObject] :: Write[Double, JObject] :: HNil
      val h = ws
    }
  // matD.merge2: Write[Int :: String :: Double :: HNil, JObject]
  */

}
