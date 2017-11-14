package jto.validation

import shapeless.{Generic, HList}

trait Tupler[H] {
  type In
  def to(t: In): H
  def from(t: H): In
}

object Tupler {
  type Aux[H, T] = Tupler[H]{ type In = T }

  implicit def hlistTupler[H <: HList, T](implicit
    T: shapeless.ops.hlist.Tupler.Aux[H, T],
    G: Generic.Aux[T, H]
  ): Aux[H, T] =
    new Tupler[H] {
      type In = T
      def to(t: T): H = G.to(t)
      def from(t: H): T = T(t)
    }
}