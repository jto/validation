package jto.validation
package v3.tagless
package xml

import jto.validation.xml.{XmlWriter, Writes => W}
import types.flip
import scala.xml.Node

trait WritesGrammar
  extends XmlGrammar[XmlWriter, flip[Write]#λ]
  with WriteConstraints
  with WritesTypeclasses[XmlWriter] {

  self =>

  type Out = XmlWriter
  type P = WritesGrammar

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: XmlWriter]]): Write[A, Out] =
        self.at(f(p))(k)
    }

  def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: XmlWriter]]): Write[A, Out] =
    Write { t =>
      val js = k.writes(t)
      val w2 = W.optionW(Write.zero[XmlWriter])(W.writeXml _)(p)
      w2.writes(js)
    }

  def opt[A](implicit K: Write[A, _ >: Out <: XmlWriter]): Write[Option[A], Option[_ >: Out <: XmlWriter]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: XmlWriter]): Write[A, Option[_ >: Out <: XmlWriter]] =
    Write { a =>
      Option(K.writes(a))
    }

  def knil: Write[shapeless.HNil,Out] =
    Write { _ => W.xmlMonoid.empty }

  def toGoal[Repr, A]: Write[Repr,Out] => Write[Goal[Repr, A], Out] =
    _.contramap{ _.value }

  implicit def bigDecimal = W.nodeW(W.bigDecimalW)
  implicit def boolean = W.nodeW(W.booleanW)
  implicit def double = W.nodeW(W.doubleW)
  implicit def float = W.nodeW(W.floatW)
  implicit def int = W.nodeW(W.intW)
  implicit def jBigDecimal = ???
  implicit def long = W.nodeW(W.longW)
  implicit def short = W.nodeW(W.shortW)
  implicit def string = W.nodeW(Write.zero)

  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: XmlWriter]): Write[Array[A], XmlWriter] = seq(k).contramap(_.toSeq)
  implicit def list[A](implicit k: Write[A, _ >: Out <: XmlWriter]): Write[List[A], XmlWriter] = seq(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: XmlWriter]): Write[Map[String, A], XmlWriter] = ???
  implicit def seq[A](implicit k: Write[A, _ >: Out <: XmlWriter]): Write[Seq[A], XmlWriter] = W.seqToNodeSeq(k)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: XmlWriter]): Write[Traversable[A], XmlWriter] = seq(k).contramap(_.toSeq)

  protected def outMonoid = W.xmlMonoid

  def attr[A, B](key: String, K: Write[B, Option[_ >: Out <: Node]])(atK: Write[A, Option[Out]]): Write[(A, B), Option[Out]] = ???
}

object WritesGrammar extends WritesGrammar
