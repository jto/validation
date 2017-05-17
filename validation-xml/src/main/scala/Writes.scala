package jto.validation
package xml

import shapeless.tag, tag.@@
import cats.Monoid
import scala.xml._

trait DefaultMonoids {
  // We define a monoid of the endofunctor xml.Elem => xml.Elem (alias XmlWriter)
  // Monoid[XmlWriter] thus has the propriety of a Monad in xml.Elem (being a monoid in the category of endofunctor)
  implicit def xmlMonoid = new Monoid[XmlWriter] {
    def combine(a1: XmlWriter, a2: XmlWriter): XmlWriter = a1 andThen a2
    def empty: XmlWriter = identity
  }
}

trait Writes
    extends DefaultWrites
    with NumericTypes2StringWrites
    with DefaultMonoids
    with GenericWrites[XmlWriter] {

  implicit def nodeW[I](implicit w: WriteLike[I, String]): Write[I, XmlWriter] @@ Root = Write {
    i => node =>
      node.copy(child = node.child :+ new Text(w.writes(i)))
  }

  def attributeW[I](name: String)(
      implicit w: WriteLike[I, String]): Write[I, XmlWriter] = Write {
    i => node =>
      node.copy(attributes = node.attributes.append(
              new UnprefixedAttribute(name, w.writes(i), Null)))
  }

  def optAttributeW[I](name: String)(
      implicit w: WriteLike[I, String]): Write[Option[I], XmlWriter] = Write {
    case Some(i) => attributeW(name)(w).writes(i)
    case None => xmlMonoid.empty
  }

  implicit def writeXml[I](path: Path)(
      implicit w: WriteLike[I, XmlWriter]): Write[I, XmlWriter] = Write { i =>
    val reversedPath = path.path.reverse
    reversedPath match {
      case Nil => w.writes(i)

      case KeyPathNode(key) :: tail =>
        val lastElem =
          w.writes(i).apply(new Elem(null, key, Null, TopScope, false))
        val newNode = tail.foldLeft(lastElem) {
          case (acc, IdxPathNode(_)) => acc
          case (acc, KeyPathNode(key)) =>
            new Elem(null, key, Null, TopScope, false, acc)
        }
        node =>
          node.copy(child = node.child :+ newNode)

        case IdxPathNode(_) :: _ =>
        throw new RuntimeException(
            "cannot write an attribute to a node with an index path")
    }
  }

  implicit def seqToNodeSeq[I](
      implicit w: WriteLike[I, XmlWriter]): Write[Seq[I], XmlWriter] = Write {
    is =>
      is.map(w.writes).foldLeft(xmlMonoid.empty)(xmlMonoid.combine)
  }

  def optionW[I, J](r: => WriteLike[I, J])(
      implicit w: Path => WriteLike[J, XmlWriter])
    : Path => Write[Option[I], XmlWriter] =
    super.optionW[I, J, XmlWriter](r, xmlMonoid.empty)

  implicit def optionW[I](implicit w: Path => WriteLike[I, XmlWriter])
    : Path => Write[Option[I], XmlWriter] =
    optionW(Write.zero[I])
}

object Writes extends Writes
