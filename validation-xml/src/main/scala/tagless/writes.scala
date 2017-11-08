package jto.validation
package v3.tagless
package xml

import types.flip
import jto.validation.xml.{Writes => W}

import shapeless.tag, tag.@@
import scala.xml.{NodeSeq, MetaData, Elem, Null, TopScope, Text, Attribute}
import cats.Monoid


trait WritesGrammar
  extends XmlGrammar[Either[MetaData, NodeSeq], flip[Write]#λ]
  with WriteConstraints
  with WritesTypeclasses[Either[MetaData, NodeSeq]] {

  // Emulate union types support using Either
  type _I = Either[MetaData, NodeSeq]
  type Out = Either[Nothing, NodeSeq]
  type OutAttr = Either[MetaData, Nothing]

  private def writeAt(p: Path)(ns: NodeSeq): NodeSeq =
    p.path match {
      case Nil => ns
      case KeyPathNode(key) :: tail =>
        writeAt(Path(tail))(new Elem(null, key, Null, TopScope, false, ns:_*))
      case IdxPathNode(_) :: _ =>
        throw new RuntimeException("cannot write an attribute to a node with an index path")
    }

  // TODO: Non empty Path only
  def at(p: Path): At[flip[Write]#λ, Out, _I] =
    new At[flip[Write]#λ, Out, _I] {
      def prepare: Write[Option[_I], Option[_I]] = Write.zero
      def run: Write[Option[_I], Out] =
        Write { x =>
          @inline def writeMeta(key: String)(m: MetaData): Elem =
            new Elem(null, key, m, TopScope, false)

          @inline def writeChild(key: String)(ns: NodeSeq): Elem =
            new Elem(null, key, Null, TopScope, false, ns:_*)

          @inline def go(in: _I): NodeSeq = {
            val rev = p.path.reverse
            val key =
              rev match {
                case Nil =>
                  throw new RuntimeException("cannot write an attribute to a node with an empty path")
                case KeyPathNode(key) :: _ =>
                  key
                case IdxPathNode(_) :: _ =>
                  throw new RuntimeException("cannot write an attribute to a node with an index path")
              }

            val node = in.fold(writeMeta(key) _, writeChild(key) _)
            writeAt(Path(rev.tail))(node)
          }

          Right(x.map(go).getOrElse(NodeSeq.Empty))
        }
    }

  def attr(key: String): At[flip[Write]#λ, OutAttr, _I] =
    new At[flip[Write]#λ, OutAttr, _I] {
      def prepare: Write[Option[_I], Option[_I]] = Write.zero
      def run: Write[Option[_I], OutAttr] =
        Write {
          case Some(Left(i)) => Left(i)
          case Some(Right(ns)) => Left(Attribute(key, ns, Null))
          case None => Left(Null)
        }
    }

  def is[A](implicit K: Write[A, _ >: Out <: _I]): Write[A,_I] = K
  def mapPath(f: jto.validation.Path => jto.validation.Path): P = ???

  def opt[A](implicit K: Write[A, _ >: Out <: _I]): Write[Option[A], Option[_I]] =
    Write {
      _.map(K.writes)
    }

  def req[A](implicit K: Write[A, _ >: Out <: _I]): Write[A, Option[_I]] =
    Write { a => Option(K.writes(a)) }

  private def txt[A](w: Write[A, String]): Write[A, _I] @@ Root =
    Write { a => Right(Text(w.writes(a))) }

  implicit def string: Write[String, _I] @@ Root = txt(Write.zero)
  implicit def bigDecimal: Write[BigDecimal, _I] @@ Root = txt(W.bigDecimalW)
  implicit def boolean: Write[Boolean, _I] @@ Root = txt(W.booleanW)
  implicit def double: Write[Double, _I] @@ Root = txt(W.doubleW)
  implicit def float: Write[Float, _I] @@ Root = txt(W.floatW)
  implicit def int: Write[Int, _I] @@ Root = txt(W.intW)
  implicit def jBigDecimal: Write[java.math.BigDecimal, _I] @@ Root = txt(Write(_.toString))
  implicit def long: Write[Long, _I] @@ Root = txt(W.longW)
  implicit def short: Write[Short, _I] @@ Root = txt(W.shortW)

  implicit def list[A](implicit k: Write[A, _ >: Out <: _I]): Write[List[A], _I] = ???
  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: _I]): Write[Array[A], _I] = ???
  implicit def seq[A](implicit k: Write[A, _ >: Out <: _I]): Write[Seq[A], _I] =
    list[A](k).contramap(_.toList)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: _I]): Write[Traversable[A], _I] = ???
  implicit def map[A](implicit k: Write[A, _ >: Out <: _I]): Write[Map[String,A], _I] = ???

  def toGoal[Repr, A]: Write[Repr,Out] => Write[Goal[Repr, A], Out] = ???

  def iMonoid: Monoid[Out] =
    new Monoid[Out] {
      def empty: Out = Right(NodeSeq.Empty)
      def combine(x: Out, y: Out): Out = Right(x.right.get ++ y.right.get)
    }

}

object WritesGrammar extends WritesGrammar
