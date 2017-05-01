package jto.validation
package v3.tagless

import shapeless.{ ::, HNil, HList, Generic }
import shapeless.tag.@@

case class Goal[A, B](value: A) {
  def trivial(implicit ev: A =:= (B :: HNil)): B = value.head
}

trait Solver[I, O] {
  def solve(h: I): O
}

object Solver {

  def apply[I, O](implicit S: Solver[I, O]) = S

  implicit def hnilSolver =
    new Solver[HNil, HNil] {
      def solve(h: HNil): HNil = h
    }

  implicit def hlistSolver[H, T1 <: HList, T2 <: HList](implicit S: Solver[T1, T2]) =
    new Solver[H :: T1, H :: T2] {
      def solve(h: H :: T1): H :: T2 = h.head :: S.solve(h.tail)
    }

  implicit def goalSolver[I, O, T1 <: HList, T2 <: HList](implicit SG: Solver[I, O], ST: Solver[T1, T2]) =
    new Solver[Goal[I, O] :: T1, O :: T2]{
      def solve(h: Goal[I, O] :: T1): O :: T2 =
        SG.solve(h.head.value) :: ST.solve(h.tail)
    }

  implicit def genSolver[H <: HList, T, Repr <: HList](implicit
    G: Generic.Aux[T, Repr],
    S: Solver[H, Repr]
  ): Solver[H, T] =
    new Solver[H, T] {
      def solve(h: H): T = G.from(S.solve(h))
    }
}

trait Merge[F[_]] {
  def merge[A, B <: HList](fa: F[A], fb: F[B]): F[A :: B]
}

case class MergeOps[F[_], B <: HList](fb: F[B])(implicit M: Merge[F]) {
  def ~:[A](fa: F[A]) = M.merge(fa, fb)
}

package object syntax {
  implicit def toMergeOps[F[_, _], I, B <: HList](fb: F[I, B])(implicit M: Merge[F[I, ?]]) =
    MergeOps[F[I, ?], B](fb)

  def solve[H, T](g: Goal[H, T])(implicit S: Solver[H, T]): T = S.solve(g.value)

  def as[T] = new {
    def apply[I](i: I)(implicit S: Solver[I, T]): T = solve(Goal[I, T](i))
  }
}

trait Primitives[I, K[_, _]] {
  self: Constraints[K] with Typeclasses[K] =>

  // TODO: add Root tag on k
  def at[A](p: Path)(k: K[I, A]): K[I, A]
  def opt[A](p: Path)(k: K[I, A]): K[I, Option[A]]

  // TODO: add Root tag on k (breaks compose)
  def is[A](implicit K: K[I, A] @@ Root): K[I, A] = K

  def toGoal[Repr, A]: K[I, Repr] => K[I, Goal[Repr, A]]

  def goal[A] = {
    sealed trait Defered {
      def apply[Repr](k: K[I, Repr]): K[I, Goal[Repr, A]] = toGoal(k)
    }
    new Defered{}
  }

  def knil: K[I, HNil]

  implicit def int: K[I, Int] @@ Root
  implicit def string: K[I, String] @@ Root
  implicit def short: K[I, Short] @@ Root
  implicit def long: K[I, Long] @@ Root
  implicit def float: K[I, Float] @@ Root
  implicit def double: K[I, Double] @@ Root
  implicit def jBigDecimal: K[I, java.math.BigDecimal] @@ Root
  implicit def bigDecimal: K[I, BigDecimal] @@ Root
  implicit def boolean: K[I, Boolean] @@ Root
}

trait Constraints[K[_, _]] {
  type C[A] = K[A, A] @@ Root

  def required[A]: K[Option[A], A]
  def min[A](a: A)(implicit O: Ordering[A]): C[A]
  def max[A](a: A)(implicit O: Ordering[A]): C[A]
  def notEmpty: C[String]
  def minLength(l: Int): C[String]
}

trait Typeclasses[K[_, _]] {
  import cats.arrow.Compose
  implicit def composeTC: Compose[K]
  implicit def semigroupTC[A]: cats.Semigroup[K[A, A]]
  implicit def mergeTC[I]: Merge[K[I, ?]]
}

trait Grammar[I, K[_, _]]
  extends Primitives[I, K]
  with Constraints[K]
  with Typeclasses[K]
