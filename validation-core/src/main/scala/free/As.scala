package jto.validation
package v3

import shapeless.{ HList, HNil, :: }
import shapeless.ops.hlist.Prepend

object TLP {
  type Flip[F[_, _]] = { type λ[B, A] = F[A, B] }
}

object Grammar {
  case class Min[T <: AnyVal](v: T)
  case class Max[T <: AnyVal](v: T)
  case object NotEmpty
  case object Optional
}

// import cats.~>
trait Interpreter[G, F[_, _], A] {
  def apply(g: G): F[A, A]
}

object RuleInterpreters {
  object R extends GenericRules with DateRules
  import Grammar._
  implicit def maxInt: Interpreter[Max[Int], Rule, Int] = ???
  implicit def minInt: Interpreter[Max[Int], Rule, Int] = ???
  implicit def notEmptyString: Interpreter[NotEmpty.type, Rule, Int] = ???
}

object As {
  def apply[A](path: Path) = new As[A, HNil](path, HNil)
}

case class As[A, H <: HList](path: Path, constraints: H) {
  def ~[B, HB <: HList](fb: As[B, HB]): AsSyntax.AsSyntax2[A, H, B, HB] =
    AsSyntax.AsSyntax2(this, fb)

  def materialize[F[_, _], I](implicit f: Path => F[I, A]) =
    f(path)

  def apply[H1 <: HList](h: H1)(implicit prepend: Prepend[H, H1]) =
    new As(path, constraints ::: h)
}

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

  implicit def divisibleHSequence0[F[_]: Divisible]: HSequence0[F] =
    new HSequence0[F] {
      def empty = Divisible[F].conquer[HNil]
      def sequence[A, H <: HList](fa: F[A], fh: F[H]): F[A :: H] =
        Divisible[F].divide[A :: H, A, H]({ case a :: h => (a, h)})(fa)(fh)
    }
}

/*
{
import jto.validation._, v3._, jsonast._, Rules._, Writes._, TLP._
val __ = jto.validation.Path
val a1 = As[Int](__ \ "bar")
val a2 = As[String](__ \ "baz")
val a3 = As[Double](__ \ "foo")
val as = a1 ~ a2 ~ a3
}
val rule = as.materialize[Rule, JValue]
val write = as.materialize[Flip[Write]#λ, JObject]
*/
