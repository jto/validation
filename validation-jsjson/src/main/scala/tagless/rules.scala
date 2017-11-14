package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js
import jto.validation.jsjson.Rules

trait RulesGrammar
    extends JsonGrammar[Rule]
    with RuleConstraints
    with RulesTypeclasses[js.Dynamic] {

  self =>

  type Sup = js.Dynamic
  type Out = js.Dynamic
  type P = RulesGrammar

  def mapPath(f: Path => Path): P =
    new RulesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  @inline private def search(path: Path, json: js.Dynamic): Option[js.Dynamic] =
    path.path match {
      case KeyPathNode(k) :: t =>
        Rules.jsObjectR.validate(json).toOption.flatMap {
          obj: js.Dictionary[js.Dynamic] =>
            obj.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
        }

      case IdxPathNode(i) :: t =>
        Rules.jsArrayR.validate(json).toOption.flatMap {
          array: js.Array[js.Dynamic] =>
            array.lift(i).flatMap(j => search(Path(t), j))
        }

      case Nil => Some(json)
    }

  def at(p: Path): At[Rule, js.Dynamic, js.Dynamic] =
    new At[Rule, js.Dynamic, js.Dynamic] {
      def run: Rule[js.Dynamic, Option[js.Dynamic]] =
        Rule { js =>
          Valid(p -> search(p, js))
        }
    }

  def is[A](
      implicit K: Rule[_ >: js.Dynamic <: js.Dynamic, A]): Rule[js.Dynamic, A] =
    K

  def opt[A](implicit K: Rule[_ >: Out <: js.Dynamic, A])
    : Rule[Option[js.Dynamic], Option[A]] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x).map {
          case (p, v) =>
            (p, Option(v))
        }
      case None =>
        Valid(Path -> None)
    }

  def req[A](implicit K: Rule[_ >: Out <: js.Dynamic, A])
    : Rule[Option[js.Dynamic], A] =
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

  implicit def seq[A](implicit k: Rule[_ >: Out <: js.Dynamic, A]) =
    Rules.pickSeq(k)
  implicit def list[A](implicit k: Rule[_ >: Out <: js.Dynamic, A]) =
    Rules.pickList(k)
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Rule[_ >: Out <: js.Dynamic, A]) = Rules.pickArray
  implicit def map[A](implicit k: Rule[_ >: Out <: js.Dynamic, A]) = Rules.mapR
  implicit def traversable[A](implicit k: Rule[_ >: Out <: js.Dynamic, A]) =
    Rules.pickTraversable(k)

  def toGoal[Repr, A] = _.map { Goal.apply }
}

object RulesGrammar extends RulesGrammar
