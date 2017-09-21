package jto.validation
package v3.tagless
package xml

import jto.validation.xml.Rules
import shapeless.tag, tag.@@
import scala.xml.{Attribute, Node}
// import cats.syntax.cartesian._

trait RulesGrammar
  extends XmlGrammar[XML.T, Rule]
  with RuleConstraints
  with RulesTypeclasses[XML.T] {
  self =>

  type N = XML.T
  type Out = N
  type Sub = N
  type P = RulesGrammar

  def mapPath(f: Path => Path): P =
    new RulesGrammar {
      override def at[A](p: Path)(k: => Rule[Option[_ >: Out <: N],A]): Rule[N, A] =
        self.at(f(p))(k)
    }

  @inline private def lookup(key: String, n: N): N =
    for {
      (_, nodes) <- n
      node <- nodes if node.label == key
    } yield {
      val attr = node.attributes.toList.collect {
        case a @ Attribute(_, _, _) => a
      }
      (attr, node.child.toList)
    }


  @inline private def search(path: Path, n: N): Option[N] =
    path.path match {
      case KeyPathNode(key) :: Nil =>
        val ns = lookup(key, n)
        ns.headOption.map { _ => ns }

      case KeyPathNode(key) :: tail =>
        val ns = lookup(key, n)
        ns.headOption.flatMap { _ =>
          search(Path(tail), ns)
        }

      case IdxPathNode(idx) :: tail =>
        // TODO: check this one
        ???
        // (node \ "_")
        //   .lift(idx)
        //   .flatMap(childNode => search(Path(tail), childNode))

      case Nil =>
        Some(n)
    }

  def at[A](p: Path)(k: => Rule[Option[_ >: Out <: N], A]): Rule[N, A] =
    Rule(p) { i =>
      val validated = k.validate(search(p, i))
      validated
    }

  def opt[A](implicit K: Rule[_ >: Out <: N, A]): Rule[Option[N], Option[A]] =
    Rule(Path) {
      case Some(x) =>
        K.validate(x).map(Option.apply)
      case None =>
        Valid(None)
    }

  def req[A](implicit K: Rule[_ >: Out <: N, A]): Rule[Option[N], A] =
    Rule(Path) {
      case Some(x) =>
        K.validate(x)
      case None =>
        Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
    }

  private def nodeR[O](implicit r: RuleLike[String, O]): Rule[N, O] @@ Root =
    tag[Root] {
      val err =
        Invalid(Seq(ValidationError(
                        "error.invalid",
                        "a non-leaf node can not be validated to a primitive type")))

      Rule.fromMapping[N, (List[Attribute], List[Node])] { xs =>
        Valid(xs.head) // TODO: is this safe ?
      } andThen
      Rule
        .fromMapping[(List[Attribute], List[Node]), String] { case (_, ns) =>
          val children = (ns \ "_")
          if (children.isEmpty) Valid(ns.text)
          else err
        }
      .andThen(r)
    }

  implicit def int = nodeR(Rules.intR)
  implicit def string = nodeR(Rule.zero)
  implicit def bigDecimal = nodeR(Rules.bigDecimal)
  implicit def boolean = nodeR(Rules.booleanR)
  implicit def double = nodeR(Rules.doubleR)
  implicit def float = nodeR(Rules.floatR)
  implicit def jBigDecimal = nodeR(Rules.javaBigDecimalR)
  implicit def long = nodeR(Rules.longR)
  implicit def short = nodeR(Rules.shortR)

  implicit def list[A](key: String)(implicit k: Rule[_ >: Out <: N, A]) =
    ???

  implicit def map[A](implicit k: Rule[_ >: Out <: N, A]) =
    ???

  def toGoal[Repr, A] = _.map { Goal.apply }

  def attr[A](key: String)(implicit r: Rule[N, A]): Rule[N, A] =
    Rule.fromMapping[N, N] { ns =>
      val vs =
        for {
          (attrs, _) <- ns
          filtered = attrs.filter(_.key == key).flatMap(_.value)
        } yield { (Nil, filtered) }
      Valid(vs)
    }.andThen(r.repath(_ \ s"@$key"))

}

object RulesGrammar extends RulesGrammar
