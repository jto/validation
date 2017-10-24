package jto.validation
package v3.tagless
package xml

import types.flip
// import jto.validation.xml.{Writes => W}

import shapeless.tag, tag.@@
import scala.xml.NodeSeq
import cats.Monoid

trait WritesGrammar extends Grammar[NodeSeq, flip[Write]#λ]
  with WriteConstraints
  with WritesTypeclasses[NodeSeq] {

self =>

type Out = NodeSeq

def at(p: Path): At[flip[Write]#λ, Out, NodeSeq] =
  new At[flip[Write]#λ, Out, NodeSeq] { a =>
    val path = p
    def run: Out => Option[NodeSeq] = ???
  }

implicit def bigDecimal: @@[Write[BigDecimal,NodeSeq], Root] = ???

implicit def boolean: @@[Write[Boolean,NodeSeq], Root] = ???

implicit def double: @@[Write[Double,NodeSeq], Root] = ???

implicit def float: @@[Write[Float,NodeSeq], Root] = ???

implicit def int: @@[Write[Int,NodeSeq], Root] = ???

def is[A](implicit K: Write[A, _ >: Out <: NodeSeq]): Write[A,NodeSeq] = ???

implicit def jBigDecimal: @@[Write[java.math.BigDecimal,NodeSeq], Root] = ???

implicit def long: @@[Write[Long,NodeSeq], Root] = ???

implicit def map[A](implicit k: Write[A, _ >: Out <: NodeSeq]): Write[Map[String,A],NodeSeq] = ???

def mapPath(f: Path => Path): P = ???

def opt[A](implicit K: Write[A, _ >: Out <: NodeSeq]): Write[Option[A],Option[NodeSeq]] = ???

def req[A](implicit K: Write[A, _ >: Out <: NodeSeq]): Write[A,Option[NodeSeq]] = ???

implicit def short: @@[Write[Short,NodeSeq], Root] = ???

implicit def string: @@[Write[String,NodeSeq], Root] = ???

def toGoal[Repr, A]: Write[Repr,Out] => Write[Goal[Repr,A], Out] = ???

implicit def applyAt: ApplyAt[flip[Write]#λ, Out, NodeSeq] =
  new ApplyAt[flip[Write]#λ, Out, NodeSeq]{
    def apply[A](at: At[flip[Write]#λ, Out, NodeSeq])(r: Write[A, Option[NodeSeq]]) =
      Write[A, Out] { a =>

        ???
      }
  }

def iMonoid: Monoid[Out] = ???

}

object WritesGrammar extends WritesGrammar

// trait WritesGrammar extends Grammar[XML, flip[Write]#λ]
//   with WriteConstraints
//   with WritesTypeclasses[XML] {

//   self =>

//   type Out = XML.Group
//   type P = WritesGrammar

//   import shapeless.tag.@@
//   def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: XML]]): Write[A, XML.Group] =
//     Write { a =>
//       val xml = k.writes(a).getOrElse(XML.Group(Nil))

//       // TODO: handle case when ps is empty
//       val ps = p.path.reverse.map {
//         case k @ KeyPathNode(_) =>
//           k
//         case _ =>
//           throw new RuntimeException("cannot write an attribute to a node with an index path")
//       }
//       val h = XML.At(ps.head, xml)
//       val at =
//         ps.tail.foldLeft(h) { (ats, kpn) =>
//           XML.At(kpn, ats)
//         }

//       XML.Group(at :: Nil)
//     }

//   def opt[A](implicit K: Write[A, _ >: Out <: XML]): Write[Option[A], Option[_ >: Out <: XML]] =
//     Write { _.map(K.writes) }

//   def req[A](implicit K: Write[A, _ >: Out <: XML]): Write[A, Option[_ >: Out <: XML]] =
//     Write { a => Option(K.writes(a)) }

//   def mapPath(f: Path => Path): P =
//     new WritesGrammar {
//       override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: XML]]): Write[A, XML.Group] =
//         self.at(f(p))(k)
//     }

//   private def txt[A](w: Write[A, String]): Write[A, XML.Text] @@ Root =
//     Write { i => XML.Text(w.writes(i)) }

//   implicit def bigDecimal = txt(W.bigDecimalW)
//   implicit def boolean = txt(W.booleanW)
//   implicit def double = txt(W.doubleW)
//   implicit def float = txt(W.floatW)
//   implicit def int = txt(W.intW)
//   implicit def jBigDecimal = txt(Write(_.toString))
//   implicit def long = txt(W.longW)
//   implicit def short = txt(W.shortW)
//   implicit def string = txt(Write.zero)

//   implicit def map[A](implicit k: Write[A, _ >: Out <: XML]): Write[Map[String, A], XML] = ???

//   implicit def seq[A](implicit k: Write[A, _ >: Out <: XML]): Write[Seq[A], XML] =
//     Write { as =>
//       XML.XList(as.map(k.writes).toList)
//     }
//   implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: XML]): Write[Array[A], XML] = seq(k).contramap(_.toSeq)
//   implicit def list[A](implicit k: Write[A, _ >: Out <: XML]): Write[List[A], XML] = seq(k).contramap(_.toSeq)
//   implicit def traversable[A](implicit k: Write[A, _ >: Out <: XML]): Write[Traversable[A], XML] = seq(k).contramap(_.toSeq)

//   def toGoal[Repr, A]: Write[Repr,Out] => Write[Goal[Repr, A], Out] =
//     _.contramap{ _.value }

//   def iMonoid: cats.Monoid[Out] =
//     new cats.Monoid[Out] {
//       def combine(x: Out, y: Out): Out =
//         XML.Group(x.values ++ y.values)
//       def empty =
//         XML.Group(Nil)
//     }

//   def attr[A](label: String)(implicit w: Write[A, XML.Text]): Write[A, XML] =
//     Write{ a =>
//       XML.Attr(label, w.writes(a).value)
//     }
// }

// object WritesGrammar extends WritesGrammar
