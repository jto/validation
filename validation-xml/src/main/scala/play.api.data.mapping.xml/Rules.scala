package play.api.data.mapping.xml

import play.api.data.mapping._
import scala.xml._

object Rules extends DefaultRules[Node] with ParsingRules {
  import scala.language.{higherKinds, implicitConversions}

  implicit def nodeR: Rule[Node, String] = Rule.fromMapping { node =>
    val children = (node \ "_")
    if (children.isEmpty) Success(node.text)
    else Failure(Seq(ValidationError("error.invalid", "a non-leaf node can not be validated to String")))
  }

  implicit def nodeBooleanR = nodeR compose booleanR
  implicit def nodeIntR = nodeR compose intR
  implicit def nodeShortR = nodeR compose shortR
  implicit def nodeLongR = nodeR compose longR
  implicit def nodeFloatR = nodeR compose floatR
  implicit def nodeDoubleR = nodeR compose doubleR
  implicit def nodeBigDecimalR = nodeR compose bigDecimal

  def attributeR(key: String): Rule[Node, String] = Rule.fromMapping { node =>
    node.attribute(key).flatMap(_.headOption).map(_.text) match {
      case Some(value) => Success(value)
      case None => Failure(Seq(ValidationError("error.required")))
    }
  }

  def attributeBooleanR(key: String) = attributeR(key) compose booleanR
  def attributeIntR(key: String) = attributeR(key) compose intR
  def attributeShortR(key: String) = attributeR(key) compose shortR
  def attributeLongR(key: String) = attributeR(key) compose longR
  def attributeFloatR(key: String) = attributeR(key) compose floatR
  def attributeDoubleR(key: String) = attributeR(key) compose doubleR
  def attributeBigDecimalR(key: String) = attributeR(key) compose bigDecimal

  implicit def pickInNode[II <: Node, O](p: Path)(implicit r: RuleLike[Node, O]): Rule[II, O] = {

    def search(path: Path, node: Node): Option[Node] = path.path match {
      case KeyPathNode(key) :: tail =>
        (node \ key).headOption.flatMap(childNode => search(Path(tail), childNode))

      case IdxPathNode(idx) :: tail =>
        (node \ "_").lift(idx).flatMap(childNode => search(Path(tail), childNode))

      case Nil => Some(node)
    }

    Rule[II, Node] { node =>
      search(p, node) match {
        case None => Failure(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(resNode) => Success(resNode)
      }
    }.compose(r)
  }

  private def pickInS[T](implicit r: RuleLike[Seq[Node], T]): Rule[Node, T] = Rule.fromMapping[Node, Seq[Node]] { node =>
    val children = (node \ "_")
    Success(children)
  }.compose(r)

  implicit def pickSeq[O](implicit r: RuleLike[Node, O]): Rule[Node, Seq[O]] = pickInS(seqR[Node, O])
  implicit def pickSet[O](implicit r: RuleLike[Node, O]): Rule[Node, Set[O]] = pickInS(setR[Node, O])
  implicit def pickList[O](implicit r: RuleLike[Node, O]): Rule[Node, List[O]] = pickInS(listR[Node, O])
  implicit def pickTraversable[O](implicit r: RuleLike[Node, O]): Rule[Node, Traversable[O]] = pickInS(traversableR[Node, O])

  implicit def ooo[O](p: Path)(implicit pick: Path => RuleLike[Node, Node], coerce: RuleLike[Node, O]): Rule[Node, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](r: => RuleLike[J, O], noneValues: RuleLike[Node, Node]*)(implicit pick: Path => RuleLike[Node, Node], coerce: RuleLike[Node, J]): Path => Rule[Node, Option[O]] =
    super.opt[J, O](r, noneValues: _*)

  def pickChildWithAttribute[O](key: String, attrKey: String, attrValue: String)(implicit r: Rule[Node, O]): Rule[Node, O] =
    Rule.fromMapping[Node, Node] { node =>
      val maybeChild = (node \ "_").find(_.attribute(attrKey).filter(_.text == attrValue).isDefined)
      maybeChild match {
        case Some(child) => Success(child)
        case None => Failure(Seq(ValidationError("error.required", s"child with attribute $attrKey = $attrValue not found")))
      }
    }.compose(r)
}