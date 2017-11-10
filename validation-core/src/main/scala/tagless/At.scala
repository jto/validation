package jto.validation
package v3.tagless

import cats.arrow.{FunctionK, Compose}
import cats.functor.Strong

trait At[K[_, _], O, T] {
  self =>
  def run: K[O, Option[T]]

  def prepare: K[Option[T], Option[T]]

  def apply[A](next: K[Option[T], A])(implicit C: Compose[K]): K[O, A] =
    C.andThen(C.andThen(run, prepare), next)

  def |->[P](
    other: At[K, T, P]
  )(implicit
    F: FunctionK[K[?, Option[P]], λ[α => K[Option[α], Option[P]]]],
    C: Compose[K]
  ): At[K, O, P] =
      new At[K, O, P] {
        def prepare = other.prepare
        def run = {
          val next = F(other.run)
          C.andThen(self.run, next)
        }
      }

  def |+>[P](
    other: At[K, T, P]
  )(implicit
    F: FunctionK[K[?, Option[P]], λ[α => K[Option[α], Option[P]]]],
    S: Strong[K],
    C: Compose[K]
  ): At[K, O, (T, P)] =
    new At[K, O, (T, P)] {

      import cats.syntax.profunctor._
      import cats.syntax.strong._
      import cats.syntax.cartesian._
      import cats.instances.option._

      @inline private def split[A, B](o: Option[(A, B)]): (Option[A], Option[B]) =
          o.map{ case (a, b) =>
            (Option(a), Option(b))
          }.getOrElse((None, None))

      @inline private def join[A, B](t: (Option[A], Option[B])): Option[(A, B)] =
        (t._1 |@| t._2).tupled

      def prepare: K[Option[(T, P)], Option[(T, P)]] = {
        val k1: K[Option[(T, P)], (Option[T], Option[P])] =
          self.prepare.first.lmap(x => split[T, P](x))
        val k2: K[(Option[T], Option[P]), Option[(T, P)]] =
          other.prepare.second.rmap(join[T, P] _)
        C.andThen(k1, k2)
      }

      def run: K[O, Option[(T, P)]] = {
        val k1 = self.run.rmap(s => (s, s))
        val k2 = F(other.run).second[Option[T]].rmap(join)
        C.andThen(k1, k2)
      }

    }
}

object At {

}