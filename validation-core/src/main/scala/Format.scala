package jto.validation

import scala.annotation.implicitNotFound
import cats.Monoid
import cats.functor.Invariant

@implicitNotFound("No Format found for types ${IR},${IW}, ${O}. Try to implement an implicit Format[${IR}, ${IW}, ${O}].")
trait Format[IR, +IW, O] extends RuleLike[IR, O] with WriteLike[O, IW]

/**
 * Default formatters.
 */
object Format {
  def apply[IR, IW, O](r: RuleLike[IR, O], w: WriteLike[O, IW]): Format[IR, IW, O] =
    new Format[IR, IW, O] {
      def validate(i: IR) = r.validate(i)
      def writes(o: O): IW = w.writes(o)
    }

  implicit def invariantFormat[IR, IW]: Invariant[Format[IR, IW, ?]] =
    new Invariant[Format[IR, IW, ?]] {
      def imap[A, B](fa: Format[IR, IW, A])(f1: A => B)(f2: B => A): Format[IR, IW, B] =
        Format[IR, IW, B](Rule.toRule(fa).map(f1), Write.toWrite(fa).contramap(f2))
    }

  implicit def formatSyntaxCombine[IR, IW : Monoid](implicit rcb: SyntaxCombine[Rule[IR, ?]], wcb: SyntaxCombine[Write[?, IW]]): SyntaxCombine[Format[IR, IW, ?]] =
    new SyntaxCombine[Format[IR, IW, ?]] {
      def apply[A, B](fa: Format[IR, IW, A], fb: Format[IR, IW, B]): Format[IR, IW, A ~ B] =
        Format[IR, IW, A ~ B](rcb(Rule.toRule(fa), Rule.toRule(fb)), wcb(Write.toWrite(fa), Write.toWrite(fb)))
    }

  implicit def formatInvariantSyntaxObs[IR, IW : Monoid, O](f: Format[IR, IW, O])(implicit fcb: SyntaxCombine[Format[IR, IW, ?]]): InvariantSyntaxObs[Format[IR, IW, ?], O] =
    new InvariantSyntaxObs[Format[IR, IW, ?], O](f)(fcb)
}
