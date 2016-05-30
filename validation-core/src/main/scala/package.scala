package jto

import cats.Monoid

/**
  * Contains the validation API used by `Form`.
  *
  * For example, to define a custom constraint:
  * {{{
  *   val negative = Constraint[Int] {
  *     case i if i < 0 => Valid
  *     case _ => Invalid("Must be a negative number.")
  *   }
  * }}}
  */
package object validation {
  @annotation.implicitNotFound(
      "No implicit Mapping found from ${I} to ${O}. Try to define an implicit Mapping[${E}, ${I}, ${O}].")
  type Mapping[E, I, O] = I => Validated[Seq[E], O]
  type Constraint[T] = Mapping[ValidationError, T, T]
  type VA[O] = Validated[Seq[(Path, Seq[ValidationError])], O]

  type Validated[+E, +A] = cats.data.Validated[E, A]
  val Validated = cats.data.Validated
  type Valid[+A] = cats.data.Validated.Valid[A]
  val Valid = cats.data.Validated.Valid
  type Invalid[+E] = cats.data.Validated.Invalid[E]
  val Invalid = cats.data.Validated.Invalid

  implicit def validatedBackcompat[E, A](
      va: Validated[Seq[E], A]): VABackCompat[E, A] =
    new VABackCompat[E, A] {
      val v = va
    }

  implicit def seqAlgebra[A]: Monoid[Seq[A]] =
    new Monoid[Seq[A]] {
      def empty: Seq[A] = Seq.empty[A]
      def combine(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y
    }
}
