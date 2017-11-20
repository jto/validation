package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject, JsNumber}
import jto.validation.playjson.Writes

import types.op

sealed trait WritesGrammar
    extends JsonGrammar[op[Write]#λ]
    with WriteConstraints
    with WritesTypeclasses[JsValue] {
  self =>

  type Out = JsObject
  type Sup = Out
  type P = WritesGrammar

  def iMonoid = Writes.jsonMonoid

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def at(p: Path): At[op[Write]#λ, Out, JsValue] =
    new At[op[Write]#λ, Out, JsValue] {
      def run: Write[Option[JsValue], Out] =
        Writes.optionW(Write.zero[JsValue])(Writes.writeJson _)(p)
    }

  def is[A](implicit K: Write[A, _ >: Out <: JsValue]): Write[A, JsValue] = K

  def opt[A](implicit K: Write[A, _ >: Out <: JsValue])
    : Write[Option[A], Option[JsValue]] =
    Write { _.map(K.writes) }

  def req[A](
      implicit K: Write[A, _ >: Out <: JsValue]): Write[A, Option[JsValue]] =
    Write { a =>
      Option(K.writes(a))
    }

  implicit def int = Writes.intW
  implicit def string = Writes.string
  implicit def bigDecimal = Writes.bigDecimalW
  implicit def boolean = Writes.booleanW
  implicit def double = Writes.doubleW
  implicit def float = Writes.floatW
  implicit def jBigDecimal = Write { (j: java.math.BigDecimal) =>
    JsNumber(j)
  }
  implicit def long = Writes.longW
  implicit def short = Writes.shortW

  implicit def seq[A](implicit k: Write[A, _ >: Out <: JsValue]) =
    Writes.seqToJsArray(k)
  implicit def list[A](implicit k: Write[A, _ >: Out <: JsValue]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Write[A, _ >: Out <: JsValue]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: JsValue]) =
    Writes.mapW(k)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: JsValue]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)

  implicit def jsNull = Write.zero
  implicit def jsObject = Write.zero
  implicit def jsString = Write.zero
  implicit def jsNumber = Write.zero
  implicit def jsBoolean = Write.zero
}

object WritesGrammar extends WritesGrammar
