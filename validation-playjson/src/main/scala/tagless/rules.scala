package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject, JsArray}
import cats.Semigroup

import jto.validation.playjson.Rules
import cats.syntax.cartesian._

import shapeless.tag.@@

trait RulesGrammar extends JsonGrammar[Rule] with RuleConstraints {
  self =>

  type J = JsValue
  type P = RulesGrammar

  def mapPath(f: Path => Path): P =
    new RulesGrammar {
      override def at[A](p: Path)(k: => Rule[_ >: J <: JsValue, A]) =
        self.at(f(p))(k)

      override def opt[A](p: Path)(k: => Rule[_ >: J <: JsValue, A]) =
        self.opt(f(p))(k)
    }

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

  def at[A](p: Path)(k: => Rule[_ >: J <: JsValue, A]): Rule[JsValue, A] =
    Rule(p) { js =>
      search(p, js) match {
        case None =>
          Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(js) =>
          k.validate(js)
      }
    }

  def opt[A](p: Path)(k: => Rule[_ >: J <: JsValue, A]): Rule[JsValue, Option[A]] =
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
  implicit def seq[A](implicit k: Rule[_ >: JsValue <: JsValue, A]) = Rules.pickSeq(k)
  implicit def list[A](implicit k: Rule[_ >: JsValue <: JsValue, A]) = Rules.pickList(k)
  implicit def array[A: scala.reflect.ClassTag](implicit k: Rule[_ >: J <: JsValue, A]) = Rules.pickArray
  implicit def map[A](implicit k: Rule[_ >: J <: JsValue, A]) = Rules.mapR
  implicit def traversable[A](implicit k: Rule[_ >: J <: JsValue, A]) = Rules.pickTraversable

  implicit def jsNull = Rules.jsNullR
  implicit def jsObject = Rules.jsObjectR
  implicit def jsString = Rules.jsStringR
  implicit def jsNumber = Rules.jsNumberR
  implicit def jsBoolean = Rules.jsBooleanR

  def toGoal[Repr, A] = _.map { Goal.apply }

  implicit def composeTC = Rule.ruleCompose

  implicit def mergeTC: Merge[Rule[J, ?]] =
    new Merge[Rule[J, ?]] {
      def merge[A, B <: HList](fa: Rule[J, A], fb: Rule[J, B]): Rule[J, A :: B] =
        (fa |@| fb).map(_ :: _)
    }

  implicit def semigroupTC[I, O]: Semigroup[Rule[I, O] @@ Root] =
    Rule.ruleSemigroup

}

object RulesGrammar extends RulesGrammar
