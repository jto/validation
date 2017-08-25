package jto.validation
package v3.tagless
package xml

import scala.xml.{Group => SGroup, _}

sealed trait XML {

  def toFun: Elem => Elem =
    this match {
      case XML.Group(xs) =>
        xs.map(_.toFun).foldLeft[Elem => Elem](identity)(_ andThen _)
      case XML.Text(txt) =>
        el => el.copy(child = el.child ++ Text(txt))
      case XML.Attr(name, value) =>
        el => el.copy(attributes = el.attributes.append(new UnprefixedAttribute(name, value, Null)))
      case XML.At(location, value) => el =>
        val f = value.toFun
        val es =
          location.path.map {
            case KeyPathNode(key) =>
              Elem(null, key, Null, TopScope, false)
            case IdxPathNode(_) =>
              throw new RuntimeException("cannot write an attribute to a node with an index path")
          }

        val xml =
          es.lastOption.map { e =>
            val last = f(e)
            es.init.reverse.foldLeft(last){ (e, els) => els.copy(child = els.child ++ e) }
          }.getOrElse(SGroup(Nil))

        el.copy(child = el.child ++ xml)
    }
}

object XML {
  final case class Group[X <: XML](values: List[X]) extends XML {
    def build(implicit ev: X =:= XML.At): Node =
      values.map(_.build)
        .foldLeft(SGroup(Nil))((a, b) => SGroup(a ++ b))
  }

  final case class Text(value: String) extends XML
  final case class Attr(name: String, value: String) extends XML

  final case class At(location: Path, value: XML) extends XML {
    def build: Node =
      location.path match {
        case Nil =>
          ??? // should be impossible
        case KeyPathNode(key) :: Nil =>
          val root = Elem(null, key, Null, TopScope, false)
          value.toFun(root)
        case KeyPathNode(key) :: p =>
          val root = Elem(null, key, Null, TopScope, false)
          At(Path(p), value).toFun(root)
        case IdxPathNode(_) :: _ =>
          throw new RuntimeException("cannot write an attribute to a node with an index path")
      }
  }
}