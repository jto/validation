package jto.validation
package v3.tagless

import play.api.libs.json.{JsValue, JsObject, JsArray}
import cats.Semigroup

import jto.validation.playjson.Rules
import cats.syntax.cartesian._

package object playjson {
  implicit object RulesGrammar extends Grammar[JsValue, Rule]{

    private def search(path: Path, json: JsValue): Option[JsValue] =
      path.path match {
        case KeyPathNode(k) :: t =>
          json match {
            case JsObject(js) =>
              js.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
            case _ => None
          }
        case IdxPathNode(i) :: t =>
          json match {
            case JsArray(js) => js.lift(i).flatMap(j => search(Path(t), j))
            case _ => None
          }
        case Nil =>
          Some(json)
      }

    def at[A](p: Path)(k: Rule[JsValue, A]): Rule[JsValue, A] =
      Rule(p) { js =>
        search(p, js) match {
          case None =>
            Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
          case Some(js) =>
            k.validate(js)
        }
      }

    def opt[A](p: Path)(k: Rule[JsValue,A]): Rule[JsValue, Option[A]] =
      Rule(p) { js =>
        search(p, js) match {
          case None =>
            Valid(None)
          case Some(js) =>
            k.validate(js).map(Option.apply)
        }
      }

    import shapeless.{::, HNil, HList}

    def knil =
      Rule.pure(HNil)

    def required[A]: Rule[Option[A], A] = Rules.required

    override implicit def int = Rules.intR
    override implicit def string = Rules.stringR
    override implicit def bigDecimal = Rules.bigDecimal
    override implicit def boolean = Rules.booleanR
    override implicit def double = Rules.doubleR
    override implicit def float = Rules.floatR
    override implicit def jBigDecimal = Rules.javaBigDecimal
    override implicit def long = Rules.longR
    override implicit def short = Rules.shortR

    override def max[A](a: A)(implicit O: Ordering[A]) = Rules.max(a)
    override def min[A](a: A)(implicit O: Ordering[A]) = Rules.min(a)
    override def notEmpty = Rules.notEmpty
    override def minLength(l: Int) = Rules.minLength(l)

    def toGoal[Repr, A] =
      _.map { Goal.apply }

    override implicit def composeTC =
      Rule.ruleCompose

    implicit def mergeTC[I]: Merge[Rule[I, ?]] =
      new Merge[Rule[I, ?]] {
        def merge[A, B <: HList](fa: Rule[I, A], fb: Rule[I, B]): Rule[I, A :: B] =
          (fa |@| fb).map(_ :: _)
      }

    implicit def semigroupTC[A]: Semigroup[Rule[A, A]] = ???
  }
}
