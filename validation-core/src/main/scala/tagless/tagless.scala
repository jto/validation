package jto.validation
package v3.tagless

import shapeless.{ ::, HNil, HList, Generic }

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


import syntax._

trait Primitives[I, K[_, _]] {
  def at[A](p: Path)(k: K[I, A]): K[I, A]
  def is[A](implicit K: K[I, A]) = K

  def toGoal[Repr, A]: K[I, Repr] => K[I, Goal[Repr, A]]

  sealed trait Defered[A] {
    def apply[Repr](k: K[I, Repr]): K[I, Goal[Repr, A]] = toGoal(k)
  }

  def goal[A] = new Defered[A]{}

  def knil: K[I, HNil]

  implicit def int: K[I, Int]
  implicit def string: K[I, String]
}

trait Constraints[K[_, _]] {
  type C[A] = K[A, A]

  def min[A](a: A): C[A]
  def max[A](a: A): C[A]
  def notEmpty(a: String): C[String]
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

object Demo {

  val __ = Path

  case class Foo(i: Int, s: String)
  case class Bar(s: String, foo: Foo)

  import cats.syntax.compose._
  import cats.syntax.semigroup._

  def foo[I, K[_, _]](implicit g: Grammar[I, K]): K[I, Goal[Int :: String :: HNil, Foo]] =
    g.goal[Foo] {
      import g._
      at(__ \ "i"){ is[Int] andThen (min(0) |+| max(3)) } ~:
      at(__ \ "s"){ is[String] } ~:
      knil
    }

  def bar[I, K[_, _]](implicit g: Grammar[I, K]): K[I, Goal[String :: Goal[Int :: String :: HNil, Foo] :: HNil, Bar]] =
    g.goal[Bar] {
      import g._
      at(__ \ "s"){ is[String] } ~:
      at(__ \ "foo"){ foo } ~:
      knil
    }

  // val supR = sup[JsValue, Rule].map(solve)
}
