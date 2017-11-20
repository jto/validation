package jto.validation
package v3.tagless
package xml

import jto.validation.xml.Rules
import shapeless.tag, tag.@@
import scala.xml.{NodeSeq, Null}

sealed trait RulesGrammar
    extends XmlGrammar[List[XML], Rule]
    with RuleConstraints
    with RulesTypeclasses[List[XML]] {
  self =>

  type N = List[XML]
  type Out = N
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
    for {
      (_, ns) <- nodes
      node <- ns if node.label == key
    } yield (node.attributes, NodeSeq.fromSeq(node.child))
  }
  @inline private def search(path: Path, n: N): Option[N] =
    path.path match {
      case KeyPathNode(key) :: Nil =>
        val ns = lookup(key, n)
        ns.headOption.map { _ =>
          ns
        }

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

  def at(p: Path): At[Rule, Out, N] =
    new At[Rule, Out, N] {
      def run =
        Rule.zero[Out].repath(_ => p).map { search(p, _) }
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
      Rule
        .fromMapping[N, String] {
          case ns =>
            ns.headOption
              .map {
                case (_, n) =>
                  Valid(n.text)
              }
              .getOrElse {
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

  implicit def list[A](implicit k: Rule[_ >: Out <: N, A]): Rule[N, List[A]] =
    Rule[N, List[A]] {
      case ns =>
        import cats.instances.list._
        import cats.syntax.traverse._
        ns.zipWithIndex
          .map {
            case (n, i) =>
              k.repath((Path \ i) ++ _).validate(List(n))
          }
          .sequence
          .map(Path -> _)
    }

  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Rule[_ >: Out <: N, A]): Rule[N, Array[A]] =
    list(k).map(_.toArray)

  implicit def seq[A](implicit k: Rule[_ >: Out <: N, A]): Rule[N, Seq[A]] =
    list[A](k).map(_.toSeq)

  implicit def traversable[A](
      implicit k: Rule[_ >: Out <: N, A]): Rule[N, Traversable[A]] =
    list(k).map(_.toTraversable)

  implicit def map[A](
      implicit k: Rule[_ >: Out <: N, A]): Rule[N, Map[String, A]] =
    Rule[N, Map[String, A]] { in =>
      import cats.instances.list._
      import cats.syntax.traverse._
      in.flatMap {
          case (_, ns) =>
            ns.theSeq.map { n =>
              k.repath((Path \ n.label) ++ _)
                .validate(List(n.attributes -> NodeSeq.fromSeq(n.child)))
                .map(n.label -> _)
            }
        }
        .sequence[VA, (String, A)]
        .map(Path -> _.toMap)
    }

  def attr[A](key: String): At[Rule, N, N] =
    new At[Rule, N, N] {
      def run: Rule[N, Option[N]] =
        Rule
          .zero[N]
          .repath(_ \ s"@$key")
          .map { ns =>
            val attrs =
              ns.flatMap {
                case (as, _) =>
                  val nodes = NodeSeq.fromSeq(Option(as(key)).toList.flatten)
                  nodes.headOption.map { _ =>
                    (Null, nodes)
                  }.toList
              }
            attrs.headOption.map { _ =>
              attrs
            }
          }
    }
}

object RulesGrammar extends RulesGrammar
