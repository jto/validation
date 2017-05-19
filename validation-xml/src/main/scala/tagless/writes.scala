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

  type Out = Elem
  type P = WritesGrammar

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Out] =
        self.at(f(p))(k)
    }

  def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Out] =
    ???
    // Write { t =>
    //   val KeyPathNode(root) = p.path.head // XXX: will fail on empty path
    //   val rest = Path(p.path.tail)
    //   val js = k.writes(t)
    //   val w2 = W.optionW(Write.zero[Node])(W.writeXml _)(p)
    //   w2.writes(js)(Elem(null, root, Null, TopScope, true))
    // }

  def opt[A](implicit K: Write[A, _ >: Out <: Node]): Write[Option[A], Option[_ >: Out <: Node]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: Node]): Write[A, Option[_ >: Out <: Node]] =
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
  implicit def seq[A](implicit k: Write[A, _ >: Out <: Node]): Write[Seq[A], Node] = Write { as => Group(as.map(k.writes)) }
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: Node]): Write[Traversable[A], Node] = seq(k).contramap(_.toSeq)

  protected def outSemigroup =
    new cats.Semigroup[Out] {
      def combine(x: Out, y: Out): Out = ??? // x ++ y
    }

  def withAttr[A, B](key: String, attrK: Write[B, Option[_ >: Out <: Node]])(K: Write[A, Option[Out]]): Write[(A, B), Option[Out]] =
    Write { case (a, b) =>
      for {
        elem <- K.writes(a)
        elem2 = elem.copy(attributes = elem.attributes.append(new UnprefixedAttribute(key, attrK.writes(b), Null)))
      } yield elem2
    }
}

object WritesGrammar extends WritesGrammar
