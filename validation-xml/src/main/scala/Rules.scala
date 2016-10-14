package jto.validation
package xml

import scala.xml._

trait Rules extends DefaultRules[Node] with ParsingRules {
  implicit def nodeR[O](implicit r: RuleLike[String, O]): Rule[Node, O] =
    Rule
      .fromMapping[Node, String] { node =>
        val children = (node \ "_")
        if (children.isEmpty) Valid(node.text)
        else
          Invalid(Seq(ValidationError(
                      "error.invalid",
                      "a non-leaf node can not be validated to String")))
      }
      .andThen(r)

  def attributeR[O](key: String)(
      implicit r: RuleLike[String, O]): Rule[Node, O] =
    Rule
      .fromMapping[Node, String] { node =>
        node.attribute(key).flatMap(_.headOption).map(_.text) match {
          case Some(value) => Valid(value)
          case None => Invalid(Seq(ValidationError("error.required")))
        }
      }
      .andThen(r)

  def optAttributeR[O](key: String)(
      implicit r: RuleLike[String, O]): Rule[Node, Option[O]] =
    Rule[Node, Option[O]] { node =>
      node.attribute(key).flatMap(_.headOption).map(_.text) match {
        case Some(str) => r.validate(str).map(Some(_))
        case None => Valid(None)
      }
    }

  implicit def pickInNode[II <: Node, O](p: Path)(
      implicit r: RuleLike[Node, O]): Rule[II, O] = {
    def search(path: Path, node: Node): Option[Node] = path.path match {
      case KeyPathNode(key) :: tail =>
        (node \ key).headOption.flatMap(childNode =>
              search(Path(tail), childNode))

      case IdxPathNode(idx) :: tail =>
        (node \ "_")
          .lift(idx)
          .flatMap(childNode => search(Path(tail), childNode))

      case Nil => Some(node)
    }

    Rule[II, Node] { node =>
      search(p, node) match {
        case None =>
          Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(resNode) => Valid(resNode)
      }
    }.andThen(r)
  }

  private def pickInS[T](implicit r: RuleLike[Seq[Node], T]): Rule[Node, T] =
    Rule
      .fromMapping[Node, Seq[Node]] { node =>
        val children = (node \ "_")
        Valid(children)
      }
      .andThen(r)

  implicit def pickSeq[O](implicit r: RuleLike[Node, O]): Rule[Node, Seq[O]] =
    pickInS(seqR[Node, O])
  implicit def pickSet[O](implicit r: RuleLike[Node, O]): Rule[Node, Set[O]] =
    pickInS(setR[Node, O])
  implicit def pickList[O](
      implicit r: RuleLike[Node, O]): Rule[Node, List[O]] =
    pickInS(listR[Node, O])
  implicit def pickTraversable[O](
      implicit r: RuleLike[Node, O]): Rule[Node, Traversable[O]] =
    pickInS(traversableR[Node, O])

  implicit def ooo[O](
      p: Path)(implicit pick: Path => RuleLike[Node, Node],
               coerce: RuleLike[Node, O]): Rule[Node, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](r: => RuleLike[J, O], noneValues: RuleLike[Node, Node]*)(
      implicit pick: Path => RuleLike[Node, Node],
      coerce: RuleLike[Node, J]): Path => Rule[Node, Option[O]] =
    super.opt[J, O](r, noneValues: _*)

  def pickChildWithAttribute[O](
      key: String, attrKey: String, attrValue: String)(
      implicit r: RuleLike[Node, O]): Rule[Node, O] =
    Rule
      .fromMapping[Node, Node] { node =>
        val maybeChild = (node \ "_").find(
            _.attribute(attrKey).filter(_.text == attrValue).isDefined)
        maybeChild match {
          case Some(child) => Valid(child)
          case None =>
            Invalid(Seq(ValidationError(
                        "error.required",
                        s"child with attribute $attrKey = $attrValue not found")))
        }
      }
      .andThen(r)
}

object Rules extends Rules
