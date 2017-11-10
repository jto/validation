package jto.validation
package v3.tagless

import shapeless.{ ::, HNil, HList }
import shapeless.tag.@@

object types {
  type flip[F[_, _]] = { type λ[B, A] = F[A, B] }
}

case class Goal[A, B](value: A) {
  def trivial(implicit ev: A =:= (B :: HNil)): B = value.head
}

trait Merge[K[_, _], Out] {
  def merge[A, B <: HList](fa: K[Out, A], fb: K[Out, B]): K[Out, A :: B]
}

case class MergeOps[K[_, _], Out, B <: HList](fb: K[Out, B])(implicit M: Merge[K, Out]) {
  def ~:[A](fa: K[Out, A]): K[Out, A :: B] = M.merge(fa, fb)
}

trait Primitives[I, K[_, _]] {
  self: Constraints[K] with Typeclasses[I, K] =>

  type Out <: I // XXX: This is dubious
  type P <: Primitives[I, K]

  @inline private def camelToUnderscores(name: String) =
    "[A-Z]".r.replaceAllIn(name, { m =>
      "_" + m.group(0).toLowerCase()
    })

  @inline protected def mapKeyPath(f: String => String) =
     mapPath { p =>
      val ns =
        p.path.map {
          case KeyPathNode(n) => KeyPathNode(f(n))
          case i => i
        }
      Path(ns)
    }

  def underScoreCase = mapKeyPath(camelToUnderscores)

  def mapPath(f: Path => Path): P

  // TODO: Introduce NonEmptyPath
  // def at[A](p: Path)(k: => K[Option[_ >: Out <: I], A]): K[Out, A]
  def at(p: Path): At[K, Out, I]
  def knil: K[Out, HNil]
  def kopt: K[Option[Out], HNil]

  def is[A](implicit K: K[_ >: Out <: I, A]): K[I, A]
  def req[A](implicit K: K[_ >: Out <: I, A]): K[Option[I], A]
  def opt[A](implicit K: K[_ >: Out <: I, A]): K[Option[I], Option[A]]

  def toGoal[Repr, A]: K[Out, Repr] => K[Out, Goal[Repr, A]]

  sealed trait Defered[A] {
    def apply[Repr](k: K[Out, Repr]): K[Out, Goal[Repr, A]] = toGoal(k)
  }

  def goal[A] = new Defered[A]{}

  implicit def int: K[I, Int] @@ Root
  implicit def string: K[I, String] @@ Root
  implicit def short: K[I, Short] @@ Root
  implicit def long: K[I, Long] @@ Root
  implicit def float: K[I, Float] @@ Root
  implicit def double: K[I, Double] @@ Root
  implicit def jBigDecimal: K[I, java.math.BigDecimal] @@ Root
  implicit def bigDecimal: K[I, BigDecimal] @@ Root
  implicit def boolean: K[I, Boolean] @@ Root

  // TODO: add non empty list
  implicit def seq[A](implicit k: K[_ >: Out <: I, A]): K[I, Seq[A]]
  implicit def list[A](implicit k: K[_ >: Out <: I, A]): K[I, List[A]]
  implicit def array[A: scala.reflect.ClassTag](implicit k: K[_ >: Out <: I, A]): K[I, Array[A]]
  implicit def map[A](implicit k: K[_ >: Out <: I, A]): K[I, Map[String, A]]
  implicit def traversable[A](implicit k: K[_ >: Out <: I, A]): K[I, Traversable[A]]

  import cats.arrow.Arrow

  def zip[A, B, C0, D](k1: K[Option[A], B], k2: K[Option[C0], D])(implicit A: Arrow[K]): K[Option[(A, C0)], (B, D)] = {
    val split = A.split(k1, k2)
    A.lmap(split){
      _.map{ case (a, b) =>
        (Option(a), Option(b))
      }.getOrElse((None, None))
    }
  }
}


trait LowPriorityTypeClasses[I, K[_, _]] {
  self: Typeclasses[I, K] with Primitives[I, K] =>
}

trait Typeclasses[I, K[_, _]] extends LowPriorityTypeClasses[I, K] {
  self: Primitives[I, K] =>

  import cats.arrow.Compose
  implicit def composeTC: Compose[K]
  implicit def semigroupTC[I0, O]: cats.Semigroup[K[I0, O] @@ Root]
  implicit def mergeTC: Merge[K, Out]
  implicit def mergeTCOpt: Merge[K, Option[Out]]
  implicit def toMergeOps[B <: HList, O: Merge[K, ?]](fb: K[O, B]): MergeOps[K, O, B] =
    MergeOps[K, O, B](fb)
}

trait Constraints[K[_, _]] {
  type C[A] = K[A, A] @@ Root

  def min[A](a: A)(implicit O: Ordering[A]): C[A]
  def max[A](a: A)(implicit O: Ordering[A]): C[A]
  def notEmpty: C[String]
  def minLength(l: Int): C[String]
  def maxLength(l: Int): C[String]
  def pattern(regex: scala.util.matching.Regex): C[String]
  def email: C[String]
  // TODO: make is work for any S <: Seq
  def forall[I, O](k: K[I, O]): K[Seq[I], Seq[O]]
  def equalTo[A](a: A): C[A]
}

trait Grammar[I, K[_, _]]
  extends Primitives[I, K]
  with Typeclasses[I, K]
  with Constraints[K]
