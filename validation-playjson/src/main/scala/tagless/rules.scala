package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject, JsArray}
import jto.validation.playjson.Rules

trait RulesGrammar
    extends JsonGrammar[Rule]
    with RuleConstraints
    with RulesTypeclasses[JsValue] {

  self =>

  type Sup = JsValue
  type Out = JsValue
  type P = RulesGrammar

  def mapPath(f: Path => Path) =
    new RulesGrammar {
      override def at(p: Path) =
        self.at(f(p))
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
          case _           => None
        }
      case Nil =>
        Some(json)
    }

  def at(p: Path): At[Rule, JsValue, JsValue] =
    new At[Rule, JsValue, JsValue] {
      def run: Rule[JsValue, Option[JsValue]] =
        Rule { js =>
          Valid(p -> search(p, js))
        }
    }

  def is[A](implicit K: Rule[_ >: JsValue <: JsValue, A]): Rule[JsValue, A] = K

  def opt[A](implicit K: Rule[_ >: Out <: JsValue, A])
    : Rule[Option[JsValue], Option[A]] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x).map {
          case (p, v) =>
            (p, Option(v))
        }
      case None =>
        Valid(Path -> None)
    }

  def req[A](
      implicit K: Rule[_ >: Out <: JsValue, A]): Rule[Option[JsValue], A] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x)
      case None =>
        Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
    }

  implicit def int = Rules.intR
  implicit def string = Rules.stringR
  implicit def bigDecimal = Rules.bigDecimal
  implicit def boolean = Rules.booleanR
  implicit def double = Rules.doubleR
  implicit def float = Rules.floatR
  implicit def jBigDecimal = Rules.javaBigDecimal
  implicit def long = Rules.longR
  implicit def short = Rules.shortR

  implicit def seq[A](implicit k: Rule[_ >: Out <: JsValue, A]) =
    Rules.pickSeq(k)
  implicit def list[A](implicit k: Rule[_ >: Out <: JsValue, A]) =
    Rules.pickList(k)
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Rule[_ >: Out <: JsValue, A]) = Rules.pickArray
  implicit def map[A](implicit k: Rule[_ >: Out <: JsValue, A]) = Rules.mapR
  implicit def traversable[A](implicit k: Rule[_ >: Out <: JsValue, A]) =
    Rules.pickTraversable(k)

  implicit def jsNull = Rules.jsNullR
  implicit def jsObject = Rules.jsObjectR
  implicit def jsString = Rules.jsStringR
  implicit def jsNumber = Rules.jsNumberR
  implicit def jsBoolean = Rules.jsBooleanR
}

object RulesGrammar extends RulesGrammar
