package jto.validation
package v3.tagless

import shapeless.{ ::, HNil, HList }
import shapeless.tag.@@

case class Goal[A, B](value: A) {
  def trivial(implicit ev: A =:= (B :: HNil)): B = value.head
}

trait Merge[F[_]] {
  def merge[A, B <: HList](fa: F[A], fb: F[B]): F[A :: B]
}

case class MergeOps[F[_], B <: HList](fb: F[B])(implicit M: Merge[F]) {
  def ~:[A](fa: F[A]) = M.merge(fa, fb)
}

trait Primitives[I, K[_, _]] {
  self: Constraints[K] with Typeclasses[K] =>

  // TODO: add Root tag on k
  def at[A](p: Path)(k:  => K[I, A]): K[I, A]
  def opt[A](p: Path)(k: => K[I, A]): K[I, Option[A]]

  // TODO: add Root tag on k (breaks seq implicit resolution)
  def is[A](implicit K: K[I, A]): K[I, A] = K

  def toGoal[Repr, A]: K[I, Repr] => K[I, Goal[Repr, A]]

  sealed trait Defered[A] {
    def apply[Repr](k: K[I, Repr]): K[I, Goal[Repr, A]] = toGoal(k)
  }

  def goal[A] = new Defered[A]{}

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
  implicit def seq[A](implicit k: K[I, A]): K[I, Seq[A]]
  implicit def array[A: scala.reflect.ClassTag](implicit k: K[I, A]): K[I, Array[A]]
  implicit def map[A](implicit k: K[I, A]): K[I, Map[String, A]]
  implicit def traversable[A](implicit k: K[I, A]): K[I, Traversable[A]]
}

trait Constraints[K[_, _]] {
  type C[A] = K[A, A] @@ Root

  def required[A]: K[Option[A], A]
  def min[A](a: A)(implicit O: Ordering[A]): C[A]
  def max[A](a: A)(implicit O: Ordering[A]): C[A]
  def notEmpty: C[String]
  def minLength(l: Int): C[String]
  def email: C[String]
  def forall[I, O](k: K[I, O]): K[Seq[I], Seq[O]]
  def equalTo[A](a: A): C[A]
}

trait Typeclasses[K[_, _]] {
  import cats.arrow.Compose
  implicit def composeTC: Compose[K]
  implicit def semigroupTC[I, O]: cats.Semigroup[K[I, O] @@ Root]
  implicit def mergeTC[I]: Merge[K[I, ?]]
}

trait Grammar[I, K[_, _]]
  extends Primitives[I, K]
  with Constraints[K]
  with Typeclasses[K]
