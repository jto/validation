package jto.validation
package v3

package object tagless {
  def solve[H, T](g: Goal[H, T])(implicit S: Solver[H, T]): T = S.solve(g.value)

  def as[T] = new {
    def apply[I](i: I)(implicit S: Solver[I, T]): T =
      solve(Goal[I, T](i))
  }

  import cats.arrow.Arrow

  def zip[K[_, _], A, B, C, D](k1: K[Option[A], B], k2: K[Option[C], D])(implicit A: Arrow[K]): K[Option[(A, C)], (B, D)] = {
    val split = A.split(k1, k2)
    A.lmap(split){
      _.map{ case (a, b) =>
        (Option(a), Option(b))
      }.getOrElse((None, None))
    }
  }

}