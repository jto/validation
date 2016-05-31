package jto.validation
package playjson

import play.api.libs.json.JsObject
import cats.Monoid
import cats.Functor
import cats.functor.Invariant
import scala.language.implicitConversions

object MixedDoc {
  // Boilerplate be build `Doc[A] with X` for `X` in
  // { `Rule[I, A]`, `Write[A, J]`, `Rule[I, A] with Write[A, J]` }
  // ----------------------------------------------------------------------------------------
  implicit def mixDocRule[I, J]: Mixer2[Doc, Rule[I, ?]] =
    new Mixer2[Doc, Rule[I, ?]] {
      def mix[A](m1: Doc[A], m2: Rule[I, A]): Doc[A] with Rule[I, A] =
        new Doc[A] with Rule[I, A] {
          def validate(data: I): VA[A] = m2.validate(data)
          def jsonSchema(title: String): JsObject = m1.jsonSchema(title)
        }
    }

  implicit def mixDocWrite[I, J]: Mixer2[Doc, Write[?, J]] =
    new Mixer2[Doc, Write[?, J]] {
      def mix[A](m1: Doc[A], m2: Write[A, J]): Doc[A] with Write[A, J] =
        new Doc[A] with Write[A, J] {
          def writes(i: A): J = m2.writes(i)
          def jsonSchema(title: String): JsObject = m1.jsonSchema(title)
        }
    }

  implicit def mixDocRuleWrite[I, J]: Mixer2[Doc, Format[I, J, ?]] =
    new Mixer2[Doc, Format[I, J, ?]] {
      def mix[A](m0: Doc[A], m1: Format[I, J, A]): Doc[A] with Format[I, J, A] =
        new Doc[A] with Rule[I, A] with Write[A, J] {
          def validate(data: I): VA[A] = m1.validate(data)
          def writes(i: A): J = m1.writes(i)
          def jsonSchema(title: String): JsObject = m0.jsonSchema(title)
        }
    }

  // Guides scalac implicit resolution for `Doc[?] with Rule[IR, ?]`
  // ----------------------------------------------------------------------------------------
  type DocRule[IR, A] = Doc[A] with Rule[IR, A]
  
  implicit def docRuleMixSyntaxCombine[IR]: SyntaxCombine[DocRule[IR, ?]] =
    mixSyntaxCombine[Doc, Rule[IR, ?]]

  implicit def docRuleMixFunctors[IR]: Functor[DocRule[IR, ?]] =
    mixFunctors[Doc, Rule[IR, ?]]

  implicit def docRuleMixFunctorSyntaxObs[IR, A](f: DocRule[IR, A])
      : FunctorSyntaxObs[DocRule[IR, ?], A] =
    mixFunctorSyntaxObs[Doc, Rule[IR, ?], A](f)

  // Guides scalac implicit resolution for `Doc[?] with Rule[IR, ?] with Write[?, OW]`
  // ----------------------------------------------------------------------------------------
  type DocFormat[IR, OW, A] = Doc[A] with Rule[IR, A] with Write[A, OW]

  implicit def docFormatMixSyntaxCombine[IR, OW: Monoid]: SyntaxCombine[DocFormat[IR, OW, ?]] =
    mixSyntaxCombine[Doc, Format[IR, OW, ?]]

  implicit def docFormatMixInvariants[IR, OW]: Invariant[DocFormat[IR, OW, ?]] =
    mixInvariants[Doc, Format[IR, OW, ?]]

  implicit def docFormatMixInvariantSyntaxObs[IR, OW: Monoid, A](f: DocFormat[IR, OW, A])
      : InvariantSyntaxObs[DocFormat[IR, OW, ?], A] =
    mixInvariantSyntaxObs[Doc, Format[IR, OW, ?], A](f)
}
