package jto.validation
package v3.tagless
package xml

import types.flip
import scala.xml._
import jto.validation.xml.{Writes => W}

trait WritesGrammar
  extends XmlGrammar[Node, flip[Write]#λ]
  with WriteConstraints
  with WritesTypeclasses[Node] {

  self =>

  type Out = Node
  type Sub = Elem
  type P = WritesGrammar

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Elem] =
        self.at(f(p))(k)
    }

  override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Elem] =
    Write { i =>
      val in = k.writes(i).getOrElse(Group(Nil))

      @annotation.tailrec
      def go(child: Elem, path: List[PathNode]): Elem =
        path match {
          case Nil =>
            child
          case KeyPathNode(key) :: xs =>
            go(Elem(null, key, Null, TopScope, false, child), xs)
          case IdxPathNode(_) :: _ =>
            throw new RuntimeException("cannot write an attribute to a node with an index path")
        }

      if(p.path.isEmpty)
        throw new RuntimeException("cannot write a node at no path")

      val KeyPathNode(root) :: tail = p.path.reverse
      go(Elem(null, root, Null, TopScope, false, in), tail)
    }

  def opt[A](implicit K: Write[A, _ >: Out <: Node]): Write[Option[A], Option[Node]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: Node]): Write[A, Option[Node]] =
    Write { a =>
      Option(K.writes(a))
    }

  def toGoal[Repr, A]: Write[Repr, Out] => Write[Goal[Repr, A], Out] =
    _.contramap{ _.value }

  private def txt[A](w: Write[A, String]) = Write[A, Node] { i => Text(w.writes(i)) }

  implicit def bigDecimal = txt(W.bigDecimalW)
  implicit def boolean = txt(W.booleanW)
  implicit def double = txt(W.doubleW)
  implicit def float = txt(W.floatW)
  implicit def int = txt(W.intW)
  implicit def jBigDecimal = txt(Write(_.toString))
  implicit def long = txt(W.longW)
  implicit def short = txt(W.shortW)
  implicit def string = txt(Write.zero)

  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: Node]): Write[Array[A], Node] = seq(k).contramap(_.toSeq)
  implicit def list[A](implicit k: Write[A, _ >: Out <: Node]): Write[List[A], Node] = seq(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: Node]): Write[Map[String, A], Node] = ???
  implicit def seq[A](implicit k: Write[A, _ >: Out <: Node]): Write[Seq[A], Node] = Write[Seq[A], Node] { as => Group(as.map(k.writes)) }
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: Node]): Write[Traversable[A], Node] = seq(k).contramap(_.toSeq)

  protected def iMonoid =
    new cats.Monoid[Out] {
      def combine(x: Out, y: Out): Out = Group(x ++ y)
      def empty = Group(Nil)
    }

  def withAttr[A, B](key: String, attrK: Write[B, Option[Node]])(K: Write[A, Elem]): Write[(A, B), Elem] =
    Write { case (a, b) =>
      val elem = K.writes(a)
      elem.copy(attributes = elem.attributes.append(new UnprefixedAttribute(key, attrK.writes(b), Null)))
    }
}

object WritesGrammar extends WritesGrammar
