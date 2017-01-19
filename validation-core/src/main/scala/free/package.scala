package jto.validation

import cats._

// This file will be empty when things are properly merged into validation-core.

package object free {
  /** To be moved in Write companion object. */
  implicit def invariantMonoidalWrite[O: Monoid]: InvariantMonoidal[Write[?, O]] =
    new InvariantMonoidal[Write[?, O]] {
      def product[A, B](fa: Write[A, O], fb: Write[B, O]): Write[(A, B), O] =
        Write { case (a, b) => Monoid[O].combine(fa.writes(a), fb.writes(b)) }

      def imap[A, B](fa: Write[A, O])(f: A => B)(g: B => A): Write[B, O] =
        fa.contramap(g)

      def pure[A](a: A): Write[A, O] =
        Write(_ => Monoid[O].empty)
    }

  /** To be moved in Rule companion object. */
  implicit def invariantMonoidalRule[I]: InvariantMonoidal[Rule[I, ?]] =
    new InvariantMonoidal[Rule[I, ?]] {
      def product[A, B](fa: Rule[I, A], fb: Rule[I, B]): Rule[I, (A, B)] =
        fb.ap(fa.map(a => b => (a, b)))

      def imap[A, B](fa: Rule[I, A])(f: A => B)(g: B => A): Rule[I, B] =
        fa.map(f)

      def pure[A](a: A): Rule[I, A] =
        Rule(_ => Valid(a))
    }

  // To be added directly to Path.
  implicit class PathAt(path: Path) {
    def as[A](implicit f: UFree[A]): UFree.Aux[A, f.Implicits] = f.rePath(path)
  }

  // Not sure what do to with this one...
  val __ = Path
}
