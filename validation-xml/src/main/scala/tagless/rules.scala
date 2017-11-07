package jto.validation
package v3.tagless
package xml

import jto.validation.xml.Rules
import shapeless.tag, tag.@@
import scala.xml.NodeSeq

trait RulesGrammar
  extends XmlGrammar[NodeSeq, Rule]
  with RuleConstraints
  with RulesTypeclasses[NodeSeq] {
  self =>

  type N = NodeSeq
  type Out = N
  type OutAttr = Out
  type Sub = N
  type P = RulesGrammar

  def mapPath(f: Path => Path): P =
    new RulesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  /**
  * Find the node with a given name
  */
  @inline private def lookup(key: String, nodes: N): N = {
    val ns =
      nodes.flatMap {
        case scala.xml.Group(cs) => cs
        case c => List(c)
      }
    for {
      node <- ns if node.label == key
    } yield node
  }


  @inline private def search(path: Path, n: N): Option[N] =
    path.path match {
      case KeyPathNode(key) :: Nil =>
        val ns = lookup(key, n)
        ns.headOption.map { _ => ns }

      case KeyPathNode(key) :: tail =>
        val ns = lookup(key, n).flatMap(_.child)
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

  def at(p: Path): At[Rule, Out, NodeSeq] =
    new At[Rule, Out, NodeSeq] {
      def prepare =
        Rule.zero[Option[NodeSeq]]
          .map( _.map(_.flatMap{ n =>
            if(n.child.isEmpty) {
              // XXX: represent the absence of text node
              // by a text node containing an empty String
              // so that this node does not simply disappear
              // while flatMaping
              scala.xml.Text("")
            } else {
              // Store child elements in a Group so that
              // we can build a proper list validation
              List(scala.xml.Group(n.child))
            }
          }))

      def run: Rule[Out, Option[NodeSeq]] =
        Rule.zero[Out].repath(_ => p).map{ search(p, _) }
    }

  def attr(key: String): At[Rule, OutAttr, N] =
    new At[Rule, OutAttr, N] {
      def prepare = Rule.zero[Option[NodeSeq]]
      def run: Rule[OutAttr, Option[N]] =
        Rule { out =>
         val ns = out.flatMap(_.attributes.filter(_.key == key).flatMap(_.value))
         Valid(Path(s"@$key") -> ns.headOption.map { _ => ns })
       }
    }

  def is[A](implicit K: Rule[_ >: Out <: N, A]): Rule[N, A] = K

  def opt[A](implicit K: Rule[_ >: Out <: N, A]): Rule[Option[N], Option[A]] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x).map { case (p, o) => (p, Option(o)) }
      case None =>
        Valid(Path -> None)
    }

  def req[A](implicit K: Rule[_ >: Out <: N, A]): Rule[Option[N], A] =
    Rule {
      case Some(x) =>
        K.validateWithPath(x)
      case None =>
        Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
    }

  private def nodeR[O](implicit r: RuleLike[String, O]): Rule[N, O] @@ Root =
    tag[Root] {
      Rule.fromMapping[NodeSeq, String] { case ns =>
        ns.headOption.map { n =>
          Valid(n.text)
        }.getOrElse {
          Valid("") // XXX: really hackish
        }
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

  implicit def list[A](implicit k: Rule[_ >: Out <: N, A]): Rule[NodeSeq, List[A]] =
    Rule[NodeSeq, List[A]] { ns =>
      import cats.instances.list._
      import cats.syntax.traverse._
      ns.theSeq.toList.zipWithIndex
        .map { case (n, i) =>
          k.repath((Path \ i) ++  _).validate(n)
        }.sequenceU.map(Path -> _)
    }

  implicit def array[A: scala.reflect.ClassTag](implicit k: Rule[_ >: Out <: N, A]): Rule[NodeSeq, Array[A]] =
    list(k).map(_.toArray)

  implicit def seq[A](implicit k: Rule[_ >: Out <: N, A]): Rule[NodeSeq, Seq[A]] =
    list[A](k).map(_.toSeq)

  implicit def traversable[A](implicit k: Rule[_ >: Out <: N, A]): Rule[NodeSeq, Traversable[A]] =
    list(k).map(_.toTraversable)

  implicit def map[A](implicit k: Rule[_ >: Out <: N, A]): Rule[NodeSeq, Map[String, A]] =
    Rule[NodeSeq, Map[String, A]] { in =>
      val ns =
        in.flatMap {
          case scala.xml.Group(cs) => cs
          case c => List(c)
        }
      import cats.instances.list._
      import cats.syntax.traverse._
      ns.theSeq.toList
        .map { n =>
          k.repath((Path \ n.label) ++  _)
           .validate(n.child)
           .map(n.label -> _)
        }.sequenceU.map(Path -> _.toMap)

    }

  def toGoal[Repr, A] = _.map { Goal.apply }
}

object RulesGrammar extends RulesGrammar
