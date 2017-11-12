package jto.validation
package v3.tagless

import cats.arrow.Compose

trait At[K[_, _], O, T] {
  self =>
  def run: K[O, Option[T]]

  def is[A](next: => K[Option[T], A])(implicit C: Compose[K], lazily: MkLazy[K]): K[O, A] =
    lazily(C.andThen(run, next))
}
