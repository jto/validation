package jto.validation
package v3

package object tagless {
  def solve[H, T](g: Goal[H, T])(implicit S: Solver[H, T]): T = S.solve(g.value)

  def as[T] = new {
    def apply[I](i: I)(implicit S: Solver[I, T]): T =
      solve(Goal[I, T](i))
  }

  import cats.functor.Strong
  import cats.arrow.Compose
  import cats.syntax.strong._
  import cats.syntax.profunctor._
  import cats.syntax.compose._

  def zip[K[_, _], A, B, C, D](k1: K[Option[A], B], k2: K[Option[C], D])(implicit P: Strong[K], C: Compose[K]): K[Option[(A, C)], (B, D)] = {
    @inline def split[X, Y](o: Option[(X, Y)]): (Option[X], Option[Y]) =
      o.map{ case (a, b) =>
        (Option(a), Option(b))
      }.getOrElse((None, None))

    val rnode: K[Option[(A, C)], (B, Option[C])] =
      k1.first[Option[C]].lmap(split)

    val rattr: K[(B, Option[C]), (B, D)] = k2.second[B]
    (rnode andThen rattr)
  }
}