package jto.validation
package v3.tagless

import shapeless.HList

package object syntax {
  implicit def toMergeOps[F[_, _], I, B <: HList](fb: F[I, B])(implicit M: Merge[F[I, ?]]) =
    MergeOps[F[I, ?], B](fb)

  def solve[H, T](g: Goal[H, T])(implicit S: Solver[H, T]): T = S.solve(g.value)

  def as[T] = new {
    def apply[I](i: I)(implicit S: Solver[I, T]): T = solve(Goal[I, T](i))
  }
}