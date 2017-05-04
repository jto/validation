package jto.validation
package v3.tagless

import shapeless.{ ::, HNil, HList, Generic }
import cats.Functor

// TODO: Add helpful implicit not found message
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

  implicit def functorSolver[I, O, F[_]: Functor, T <: HList](implicit S: Solver[I, O]) =
    new Solver[F[Goal[I, O]] :: T, F[O] :: T] {
      def solve(h: F[Goal[I, O]] :: T): F[O] :: T =
        Functor[F].map(h.head) {
          i => S.solve(i.value)
        } :: h.tail
    }

  implicit def seqSolver[I, O, T <: HList](implicit S: Solver[I, O]) =
    new Solver[Seq[Goal[I, O]] :: T, Seq[O] :: T] {
      def solve(h: Seq[Goal[I, O]] :: T): Seq[O] :: T =
        h.head.map {
          i => S.solve(i.value)
        } :: h.tail
    }

}