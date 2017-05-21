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
  type P = WritesGrammar

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Out] =
        self.at(f(p))(k)
    }

  def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Out] =
    Write { i =>
      @annotation.tailrec
      def go(child: Node, path: List[PathNode]): Node =
        path match {
          case Nil =>
            child
          case KeyPathNode(key) :: xs =>
            go(Elem(null, key, Null, TopScope, false, child), xs)
          case IdxPathNode(_) :: _ =>
            throw new RuntimeException("cannot write an attribute to a node with an index path")
        }

      k.writes(i).map { node =>
        val reversedPath = p.path.reverse
        go(node, reversedPath)
      }.getOrElse(Group(Nil))
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
    new cats.Monoid[Node] {
      def combine(x: Node, y: Node): Node = Group(x ++ y)
      def empty = Group(Nil)
    }

  def withAttr[A, B](key: String, attrK: Write[B, Option[Node]])(K: Write[A, Option[Out]]): Write[(A, B), Option[Out]] =
    Write { case (a, b) =>
      for {
        // XXX: partial match + cast because scala.xml is terrible
        _elem @ Elem(_, _, _, _) <- K.writes(a)
        elem = _elem.asInstanceOf[Elem]
        elem2 = elem.copy(attributes = elem.attributes.append(new UnprefixedAttribute(key, attrK.writes(b), Null)))
      } yield elem2
    }
}

object WritesGrammar extends WritesGrammar
