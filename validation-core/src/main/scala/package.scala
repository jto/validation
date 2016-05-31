package jto

import cats.{Monoid, Unapply, Functor}
import cats.functor.Invariant
import cats.syntax.{CartesianOps, CartesianSyntax1}
import cats.Cartesian
import scala.language.implicitConversions

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

  implicit def cartesianSyntaxU[FA](fa: FA)(
      implicit U: Unapply[Cartesian, FA]): CartesianOps[U.M, U.A] = {
    object As extends CartesianSyntax1
    As.cartesianSyntaxU(fa)
  }

  implicit def seqAlgebra[A]: Monoid[Seq[A]] =
    new Monoid[Seq[A]] {
      def empty: Seq[A] = Seq.empty[A]
      def combine(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y
    }

  // Typeclasses derivable given a Mixer2 instance
  // (all of these could be implicit but scalac is confused by `A with B`...)
  // ----------------------------------------------------------------------------------------
  def mixInvariants[F1[_], F2[_]](implicit I1: Invariant[F1], I2: Invariant[F2], M: Mixer2[F1, F2]) =
    new Invariant[Lambda[O => F1[O] with F2[O]]] {
      def imap[A, B](fa: F1[A] with F2[A])(f: A => B)(g: B => A): F1[B] with F2[B] =
        M.mix(I1.imap(fa)(f)(g), I2.imap(fa)(f)(g))
    }

  def mixFunctors[F1[_], F2[_]](implicit I1: Functor[F1], I2: Functor[F2], M: Mixer2[F1, F2]) =
    new Functor[Lambda[O => F1[O] with F2[O]]] {
      def map[A, B](fa: F1[A] with F2[A])(f: A => B): F1[B] with F2[B] =
        M.mix(I1.map(fa)(f), I2.map(fa)(f))
    }

  def mixSyntaxCombine[F1[_], F2[_]](implicit S1: SyntaxCombine[F1], S2: SyntaxCombine[F2], M: Mixer2[F1, F2]) =
    new SyntaxCombine[Lambda[I => F1[I] with F2[I]]] {
      def apply[A, B](ma: F1[A] with F2[A],
                      mb: F1[B] with F2[B]): F1[A ~ B] with F2[A ~ B] =
        M.mix(S1(ma, mb), S2(ma, mb))
    }

  def mixInvariantSyntaxObs[F1[_], F2[_], A](ff: F1[A] with F2[A])
      (implicit S: SyntaxCombine[Lambda[I => F1[I] with F2[I]]]) =
    new InvariantSyntaxObs[Lambda[I => F1[I] with F2[I]], A](ff)

  def mixFunctorSyntaxObs[F1[_], F2[_], A](ff: F1[A] with F2[A])
      (implicit S: SyntaxCombine[Lambda[I => F1[I] with F2[I]]]) =
    new FunctorSyntaxObs[Lambda[I => F1[I] with F2[I]], A](ff)

  // Guides scalac implicit resolution for mixing Rule[IR, ?] & Write[?, OW]
  // ----------------------------------------------------------------------------------------
  type Format[IR, OW, A] = Rule[IR, A] with Write[A, OW]

  implicit def formatMixSyntaxCombine[IR, OW: Monoid]
    : SyntaxCombine[Format[IR, OW, ?]] =
    mixSyntaxCombine[Rule[IR, ?], Write[?, OW]]

  implicit def formatMixInvariants[IR, OW]: Invariant[Format[IR, OW, ?]] =
    mixInvariants[Rule[IR, ?], Write[?, OW]]

  implicit def formatMixInvariantSyntaxObs[IR, OW: Monoid, A](
      f: Format[IR, OW, A]): InvariantSyntaxObs[Format[IR, OW, ?], A] =
    mixInvariantSyntaxObs[Rule[IR, ?], Write[?, OW], A](f)

  // Sugar (backward source compatible)
  // ----------------------------------------------------------------------------------------
  def Format[IR, OW, A](r: Rule[IR, A], w: Write[A, OW]): Format[IR, OW, A] =
    Mixer2.mixRuleWrite.mix(r, w)

  def Formatting[IR, OW] = new FormattingCurried[IR, OW] {}
  def From[IR] = new FromCurried[IR] {}
  def To[OW] = new ToCurried[OW] {}
}

class FormattingCurried[IR, OW] {
  import jto.validation._

  def apply[A](as: As2[Rule[IR, ?], Write[?, OW]] => Format[IR, OW, A])(
      implicit a1: At[Rule[IR, ?]],
      a2: At[Write[?, OW]],
      M: Mixer2[Rule[IR, ?], Write[?, OW]]): Format[IR, OW, A] =
    Build[Rule[IR, ?], Write[?, OW], A](as)
}

class FromCurried[IR] {
  import jto.validation._

  def apply[A](as: As1[Rule[IR, ?]] => Rule[IR, A])(
      implicit a1: At[Rule[IR, ?]]): Rule[IR, A] =
    Build[Rule[IR, ?], A](as)
}

class ToCurried[OW] {
  import jto.validation._

  def apply[A](as: As1[Write[?, OW]] => Write[A, OW])(
      implicit a1: At[Write[?, OW]]): Write[A, OW] =
    Build[Write[?, OW], A](as)
}
