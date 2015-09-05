package jto

import cats.{Apply, Unapply}
import cats.syntax.{ApplyOps, ApplySyntax1}

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
  @annotation.implicitNotFound("No implicit Mapping found from ${I} to ${O}. Try to define an implicit Mapping[${E}, ${I}, ${O}].")
  type Mapping[E, I, O] = I => Validation[E, O]
  type Constraint[T] = Mapping[ValidationError, T, T]
  type VA[O] = Validation[(Path, Seq[ValidationError]), O]
  
  @deprecated("unlift can now be omitted", "2.0")
  def unlift[A, B](f: A => Option[B]): A => B = Function.unlift(f)
  
  implicit def toFunctionalBuilderOps[M[_], A](a: M[A])(implicit fcb: FunctionalCanBuild[M]): FunctionalBuilderOps[M, A] = new FunctionalBuilderOps[M, A](a)(fcb)
  
  implicit def applySyntaxU[FA](fa: FA)(implicit U: Unapply[Apply, FA]): ApplyOps[U.M, U.A] = {
    object As extends ApplySyntax1
    As.applySyntaxU(fa)
  }
}
