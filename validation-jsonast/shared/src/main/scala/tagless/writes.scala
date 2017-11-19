package jto.validation
package v3.tagless
package jsonast

import jto.validation.jsonast._

import types.flip

sealed trait WritesGrammar
    extends JsonGrammar[Write.Co]
    with WriteConstraints
    with WritesTypeclasses[JValue] {
  self =>

  type Out = JObject
  type Sup = Out
  type P = WritesGrammar

  def iMonoid = Writes.jsonMonoid

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def at(p: Path): At[flip[Write]#λ, Out, JValue] =
    new At[flip[Write]#λ, Out, JValue] {
      def run: Write[Option[JValue], Out] =
        Writes.optionW(Write.zero[JValue])(Writes.writeJson _)(p)
    }

  def is[A](implicit K: Write[A, _ >: Out <: JValue]): Write[A, JValue] = K

  def opt[A](implicit K: Write[A, _ >: Out <: JValue])
    : Write[Option[A], Option[JValue]] =
    Write { _.map(K.writes) }

  def req[A](
      implicit K: Write[A, _ >: Out <: JValue]): Write[A, Option[JValue]] =
    Write { a =>
      Option(K.writes(a))
    }

  implicit def int = Writes.intW
  implicit def string = Writes.stringW
  implicit def bigDecimal = Writes.bigDecimalW
  implicit def boolean = Writes.booleanW
  implicit def double = Writes.doubleW
  implicit def float = Writes.floatW
  implicit def jBigDecimal = Write { (j: java.math.BigDecimal) =>
    JNumber(j.toString)
  }
  implicit def long = Writes.longW
  implicit def short = Writes.shortW

  implicit def seq[A](implicit k: Write[A, _ >: Out <: JValue]) =
    Writes.seqToJsArray(k)
  implicit def list[A](implicit k: Write[A, _ >: Out <: JValue]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def array[A: scala.reflect.ClassTag](
      implicit k: Write[A, _ >: Out <: JValue]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: JValue]) =
    Writes.mapW(k)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: JValue]) =
    Writes.seqToJsArray(k).contramap(_.toSeq)

  implicit def jsNull = Write.zero
  implicit def jsObject = Write.zero
  implicit def jsString = Write.zero
  implicit def jsNumber = Write.zero
  implicit def jsBoolean = Write.zero

  def toGoal[Repr, A]: Write[Repr, Out] => Write[Goal[Repr, A], Out] =
    _.contramap { _.value }
}

object WritesGrammar extends WritesGrammar
