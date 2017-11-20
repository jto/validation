package jto.validation
package v3.tagless
package xml

import types.flip
import jto.validation.xml.{Writes => W}

import shapeless.tag, tag.@@
import scala.xml.{Null, Text, Attribute, NodeSeq, MetaData, Elem, TopScope}
import cats.Monoid

sealed trait WritesGrammar
    extends XmlGrammar[List[XML], flip[Write]#λ]
    with WriteConstraints
    with WritesTypeclasses[List[XML]] {

  self =>

  type _I = List[XML]
  type Out = List[XML]
  type P = WritesGrammar

  def mapPath(f: Path => Path) =
    new WritesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  private def writeAt(p: Path)(ns: NodeSeq): NodeSeq =
    p.path match {
      case Nil => ns
      case KeyPathNode(key) :: tail =>
        writeAt(Path(tail))(new Elem(null, key, Null, TopScope, false, ns: _*))
      case IdxPathNode(_) :: _ =>
        throw new RuntimeException(
          "cannot write an attribute to a node with an index path")
    }

  // TODO: Non empty Path only
  def at(p: Path): At[flip[Write]#λ, Out, _I] =
    new At[flip[Write]#λ, Out, _I] {

      def run: Write[Option[_I], Out] =
        Write {
          _.map { i =>
            val rev = p.path.reverse
            val key =
              rev match {
                case Nil =>
                  throw new RuntimeException(
                    "cannot write an attribute to a node with an empty path")
                case KeyPathNode(key) :: _ =>
                  key
                case IdxPathNode(_) :: _ =>
                  throw new RuntimeException(
                    "cannot write an attribute to a node with an index path")
              }

            val child =
              i.map {
                case (as, ns) =>
                  new Elem(null, key, as, TopScope, false, ns: _*)
              }

            // if i is empty, we're in the case where the node is required but it's content
            // is optional AND is None. In that case, write the empty node.
            val cs =
              child.headOption
                .map { _ =>
                  child
                }
                .getOrElse(new Elem(null, key, Null, TopScope, false))

            List((Null, writeAt(Path(rev.tail))(NodeSeq.fromSeq(cs))))
          }.getOrElse(Nil)
        }
    }

  def is[A](implicit K: Write[A, _ >: Out <: _I]): Write[A, _I] = K

  def opt[A](
      implicit K: Write[A, _ >: Out <: _I]): Write[Option[A], Option[_I]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: _I]): Write[A, Option[_I]] =
    Write { a =>
      Option(K.writes(a))
    }

  private def txt[A](w: Write[A, String]): Write[A, _I] @@ Root =
    Write { a =>
      List(Null -> Text(w.writes(a)))
    }

  implicit def string: Write[String, _I] @@ Root = txt(Write.zero)
  implicit def bigDecimal: Write[BigDecimal, _I] @@ Root = txt(W.bigDecimalW)
  implicit def boolean: Write[Boolean, _I] @@ Root = txt(W.booleanW)
  implicit def double: Write[Double, _I] @@ Root = txt(W.doubleW)
  implicit def float: Write[Float, _I] @@ Root = txt(W.floatW)
  implicit def int: Write[Int, _I] @@ Root = txt(W.intW)
  implicit def jBigDecimal: Write[java.math.BigDecimal, _I] @@ Root =
    txt(Write(_.toString))
  implicit def long: Write[Long, _I] @@ Root = txt(W.longW)
  implicit def short: Write[Short, _I] @@ Root = txt(W.shortW)

  implicit def list[A](
      implicit k: Write[A, _ >: Out <: _I]): Write[List[A], _I] =
    Write { as =>
      as.flatMap(k.writes)
    }

  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Write[A, _ >: Out <: _I]): Write[Array[A], _I] =
    list[A](k).contramap(_.toList)

  implicit def seq[A](implicit k: Write[A, _ >: Out <: _I]): Write[Seq[A], _I] =
    list[A](k).contramap(_.toList)

  implicit def traversable[A](
      implicit k: Write[A, _ >: Out <: _I]): Write[Traversable[A], _I] =
    list[A](k).contramap(_.toList)

  implicit def map[A](
      implicit k: Write[A, _ >: Out <: _I]): Write[Map[String, A], _I] =
    Write[Map[String, A], _I] { m =>
      m.toList.foldLeft(iMonoid.empty) { (is, el) =>
        val (key, a) = el
        val ns = at(Path \ key).is(req(k)).writes(a)
        iMonoid.combine(is, ns)
      }
    }

  def attr[A](key: String): At[flip[Write]#λ, _I, _I] =
    new At[flip[Write]#λ, _I, _I] {
      def run: Write[Option[_I], _I] =
        Write { mi =>
          val is: _I = mi.getOrElse(Nil)
          is.map {
            case (as, ns) =>
              val meta: MetaData = as.append(Attribute(key, ns, Null))
              (meta, NodeSeq.Empty)
          }
        }
    }

  def iMonoid: Monoid[Out] =
    new Monoid[Out] {
      def empty: Out = Nil
      def combine(x: Out, y: Out): Out = {
        val xml =
          (x ++ y).foldLeft((Null, NodeSeq.Empty): XML) { (a, b) =>
            (a._1.append(b._1), a._2 ++ b._2)
          }
        List(xml)
      }
    }
}

object WritesGrammar extends WritesGrammar
