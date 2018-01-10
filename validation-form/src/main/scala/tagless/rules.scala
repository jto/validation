package jto.validation
package v3.tagless
package forms

import jto.validation.forms._

sealed trait RulesGrammar
    extends FormGrammar[Rule]
    with RuleConstraints
    with RulesTypeclasses[PM.PM] {

  self =>

  type Out = PM.PM
  type P = RulesGrammar

  def mapPath(f: Path => Path): P =
    new RulesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def at(p: Path): At[Rule, PM.PM, PM.PM] =
    new At[Rule, PM.PM, PM.PM] {
      def run: Rule[PM.PM, Option[PM.PM]] =
        Rule { pm =>
          val r = PM.find(p)(pm)
          val m = r.headOption.map { _ => r }
          Valid(p -> m)
        }
    }

  def is[A](implicit K: Rule[_ >: PM.PM <: PM.PM, A]): Rule[PM.PM, A] = K

  def opt[A](implicit K: Rule[_ >: Out <: PM.PM, A])
    : Rule[Option[PM.PM], Option[A]] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x).map {
          case (p, v) =>
            (p, Option(v))
        }
      case None =>
        Valid(Path -> None)
    }

  def req[A](implicit K: Rule[_ >: Out <: PM.PM, A]): Rule[Option[PM.PM], A] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x)
      case None =>
        Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
    }

  implicit def int = Rules.parseString(Rules.intR)
  implicit def string = Rules.parseString(Rule.zero[String])
  implicit def bigDecimal = Rules.parseString(Rules.bigDecimal)
  implicit def boolean = Rules.parseString(Rules.booleanR)
  implicit def double = Rules.parseString(Rules.doubleR)
  implicit def float = Rules.parseString(Rules.floatR)
  implicit def jBigDecimal = Rules.parseString(Rules.javaBigDecimalR)
  implicit def long = Rules.parseString(Rules.longR)
  implicit def short = Rules.parseString(Rules.shortR)

  implicit def seq[A](implicit k: Rule[_ >: Out <: PM.PM, A]) =
    Rules.inT(Rules.seqR(k))(Path)

  implicit def list[A](implicit k: Rule[_ >: Out <: PM.PM, A]) =
    seq(k).map(_.toList)

  implicit def array[A: scala.reflect.ClassTag](implicit k: Rule[_ >: Out <: PM.PM, A]) = {
    val ct = implicitly[scala.reflect.ClassTag[A]]
    Rules.inArray(ct, Rules.arrayR(ct, k))(Path)
  }

  implicit def map[A](implicit k: Rule[_ >: Out <: PM.PM, A]): Rule[PM.PM, Map[String, A]] =
    Rule { pm =>
      import cats.instances.list._
      import cats.syntax.traverse._
      pm.toList.map {
        case (Path, _) =>
          // Ignore values at root Path. Is that a desirable behaviour ?
          // Should it trigger a validation error ?
          Valid(Nil)
        case (h \: r, s) =>
          val sub = Map(r -> s)
          k.repath(h ++ _)
            .validate(sub).map { v =>
              List(h.path.head.toString -> v)
            }
      }
      .sequence[VA, List[(String, A)]]
      .map { ll =>
        Path -> ll.flatten.toMap
      }
    }

  implicit def traversable[A](implicit k: Rule[_ >: Out <: PM.PM, A]) =
    Rules.inT(Rules.seqR(k))(Path).map(_.toTraversable)
}

object RulesGrammar extends RulesGrammar
