package jto.validation
package v3.tagless
package xml

import jto.validation.xml.Rules
import scala.xml._

trait RulesGrammar extends XmlGrammar[Rule] with RuleConstraints with RulesTypeclasses[Node] {
  self =>

  type Out = Node
  type P = RulesGrammar

  def mapPath(f: Path => Path): P =
    new RulesGrammar {
      override def at[A](p: Path)(k: => Rule[_ >: Out <: Node, A]) =
        self.at(f(p))(k)
      override def opt[A](p: Path)(k: => Rule[_ >: Out <: Node, A]) =
        self.opt(f(p))(k)
    }

  @inline private def search(path: Path, node: Node): Option[Node] = path.path match {
      case KeyPathNode(key) :: tail =>
        (node \ key).headOption.flatMap(childNode =>
              search(Path(tail), childNode))

      case IdxPathNode(idx) :: tail =>
        (node \ "_")
          .lift(idx)
          .flatMap(childNode => search(Path(tail), childNode))

      case Nil => Some(node)
    }

  def at[A](p: Path)(k: => Rule[_ >: Out <: Node, A]): Rule[Node, A] =
    Rule(p) { js =>
      search(p, js) match {
        case None =>
          Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(js) =>
          k.validate(js)
      }
    }

  def opt[A](p: Path)(k: => Rule[_ >: Out <: Node, A]): Rule[Node, Option[A]] =
    Rule(p) { js =>
      search(p, js) match {
        case None =>
          Valid(None)
        case Some(js) =>
          k.validate(js).map(Option.apply)
      }
    }

  import shapeless.HNil

  def knil = Rule.pure(HNil)

  implicit def int = Rules.nodeR(Rules.intR)
  implicit def string = Rules.nodeR(Rule.zero)
  implicit def bigDecimal = Rules.nodeR(Rules.bigDecimal)
  implicit def boolean = Rules.nodeR(Rules.booleanR)
  implicit def double = Rules.nodeR(Rules.doubleR)
  implicit def float = Rules.nodeR(Rules.floatR)
  implicit def jBigDecimal = Rules.nodeR(Rules.javaBigDecimalR)
  implicit def long = Rules.nodeR(Rules.longR)
  implicit def short = Rules.nodeR(Rules.shortR)
  implicit def seq[A](implicit k: Rule[_ >: Out <: Node, A]) = Rules.pickSeq(k)
  implicit def list[A](implicit k: Rule[_ >: Out <: Node, A]) = Rules.pickList(k)
  implicit def array[A: scala.reflect.ClassTag](implicit k: Rule[_ >: Out <: Node, A]) = Rules.pickList(k).map(_.toArray)
  implicit def map[A](implicit k: Rule[_ >: Out <: Node, A]) = ???
  implicit def traversable[A](implicit k: Rule[_ >: Out <: Node, A]) = Rules.pickTraversable(k)

  // TODO: attribute
  def toGoal[Repr, A] = _.map { Goal.apply }
}

object RulesGrammar extends RulesGrammar
