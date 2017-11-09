package jto.validation
package v3

package object tagless {
  def solve[H, T](g: Goal[H, T])(implicit S: Solver[H, T]): T = S.solve(g.value)

  def as[T] = new {
    def apply[I](i: I)(implicit S: Solver[I, T]): T =
      solve(Goal[I, T](i))
  }
}