package jto.validation
package v3.tagless

import play.api.libs.json.{JsValue, JsObject, JsArray}
import cats.Semigroup

import jto.validation.playjson.Rules
import cats.syntax.cartesian._

package object playjson {

  implicit object RulesGrammar extends Grammar[JsValue, Rule]{

    @inline private def search(path: Path, json: JsValue): Option[JsValue] =
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

    def knil = Rule.pure(HNil)

    implicit def int = Rules.intR
    implicit def string = Rules.stringR
    implicit def bigDecimal = Rules.bigDecimal
    implicit def boolean = Rules.booleanR
    implicit def double = Rules.doubleR
    implicit def float = Rules.floatR
    implicit def jBigDecimal = Rules.javaBigDecimal
    implicit def long = Rules.longR
    implicit def short = Rules.shortR
    implicit def seq[A](implicit k: Rule[JsValue, A]) = Rules.pickSeq(k)
    implicit def array[A: scala.reflect.ClassTag](implicit k: Rule[JsValue, A]) = Rules.pickArray
    implicit def map[A](implicit k: Rule[JsValue, A]) = Rules.mapR
    implicit def traversable[A](implicit k: Rule[JsValue, A]) = Rules.pickTraversable

    def required[A]: Rule[Option[A], A] = Rules.required
    def max[A](a: A)(implicit O: Ordering[A]) = Rules.max(a)
    def min[A](a: A)(implicit O: Ordering[A]) = Rules.min(a)
    def notEmpty = Rules.notEmpty
    def minLength(l: Int) = Rules.minLength(l)
    def email = Rules.email
    def forall[I, O](k: Rule[I,O]) = Rules.seqR(k)
    def equalTo[T](t: T) = Rules.equalTo(t)

    def toGoal[Repr, A] = _.map { Goal.apply }

    implicit def composeTC = Rule.ruleCompose

    implicit def mergeTC[I]: Merge[Rule[I, ?]] =
      new Merge[Rule[I, ?]] {
        def merge[A, B <: HList](fa: Rule[I, A], fb: Rule[I, B]): Rule[I, A :: B] =
          (fa |@| fb).map(_ :: _)
      }

    implicit def semigroupTC[A]: Semigroup[Rule[A, A]] = ???
  }
}
