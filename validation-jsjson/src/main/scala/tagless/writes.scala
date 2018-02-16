package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js
import jto.validation.jsjson.Writes

import types.op

trait WritesGrammar
    extends JsonGrammar[op[Write]#λ]
    with WriteConstraints
    with WritesTypeclasses[js.Dynamic] {
  self =>

  type Out = js.Dynamic
  type Sup = Out
  type P = WritesGrammar

  def iMonoid = Writes.jsonMonoid

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def at(p: Path): At[op[Write]#λ, Out, js.Dynamic] =
    new At[op[Write]#λ, Out, js.Dynamic] {
      def run: Write[Option[js.Dynamic], Out] =
        Writes.optionW(Write.zero[js.Dynamic])(Writes.writeJson _)(p)
    }

  def is[A](
      implicit K: Write[A, _ >: Out <: js.Dynamic]): Write[A, js.Dynamic] = K

  def opt[A](implicit K: Write[A, _ >: Out <: js.Dynamic])
    : Write[Option[A], Option[js.Dynamic]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: js.Dynamic])
    : Write[A, Option[js.Dynamic]] =
    Write { a =>
      Option(K.writes(a))
    }

  implicit def int = Writes.intW
  implicit def string = Writes.stringW
  implicit def bigDecimal = Writes.bigDecimalW
  implicit def boolean = Writes.booleanW
  implicit def double = Writes.doubleW
  implicit def float = Writes.floatW
  implicit def jBigDecimal =
    Write[java.math.BigDecimal, js.Dynamic] { b =>
      bigDecimal.writes(BigDecimal(b))
    }
  implicit def long = Writes.longW
  implicit def short = Writes.shortW

  implicit def seq[A](implicit k: Write[A, _ >: Out <: js.Dynamic]) =
    Writes.seqToJsArray(k)
  implicit def list[A](implicit k: Write[A, _ >: Out <: js.Dynamic]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Write[A, _ >: Out <: js.Dynamic]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: js.Dynamic]) =
    Writes.mapW(k)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: js.Dynamic]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)

  implicit def jsNull = Write.zero
  implicit def jsObject = Write.zero
  implicit def jsString = Write.zero
  implicit def jsNumber = Write.zero
  implicit def jsBoolean = Write.zero
}

object WritesGrammar extends WritesGrammar
