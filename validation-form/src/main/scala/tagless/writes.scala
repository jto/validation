package jto.validation
package v3.tagless
package forms

import jto.validation.forms._

import shapeless.tag.@@
import types.flip

sealed trait WritesGrammar
    extends FormGrammar[flip[Write]#λ]
    with WriteConstraints
    with WritesTypeclasses[PM.PM] {
  self =>

  type Out = PM.PM
  type Sup = Out
  type P = WritesGrammar

  def iMonoid: cats.Monoid[Out] =
    new cats.Monoid[Out] {
      def empty: Out = Map.empty
      def combine(x: Out,y: Out): Out = x ++ y
    }

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def at(p: Path): At[flip[Write]#λ, Out, PM.PM] =
    new At[flip[Write]#λ, Out, PM.PM] {
      def run: Write[Option[PM.PM], Out] =
        Write[Option[PM.PM], Out] { opm =>
          val pm: PM.PM = opm.getOrElse[PM.PM](Map.empty)
          PM.repathPM(pm, p ++ _)
        }
    }

  def is[A](implicit K: Write[A, _ >: Out <: PM.PM]): Write[A, PM.PM] = K

  def opt[A](implicit K: Write[A, _ >: Out <: PM.PM])
    : Write[Option[A], Option[PM.PM]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: PM.PM]): Write[A, Option[PM.PM]] =
    Write { a =>
      Option(K.writes(a))
    }

  private def liftPM[A](r: Write[A, String]): Write[A, PM.PM] @@ Root =
    Write { a => Map(Path -> r.writes(a)) }

  implicit def int = liftPM(Writes.intW)
  implicit def string = liftPM(Write.zero[String])
  implicit def bigDecimal = liftPM(Writes.bigDecimalW)
  implicit def boolean = liftPM(Writes.booleanW)
  implicit def double = liftPM(Writes.doubleW)
  implicit def float = liftPM(Writes.floatW)
  implicit def jBigDecimal = liftPM(Write(_.toString))
  implicit def long = liftPM(Writes.longW)
  implicit def short = liftPM(Writes.shortW)

  implicit def seq[A](implicit k: Write[A, _ >: Out <: PM.PM]) =
    list(k).contramap(_.toList)

  implicit def list[A](implicit k: Write[A, _ >: Out <: PM.PM]) =
    Write { as =>
      as.zipWithIndex.map { case (a, i) =>
        PM.repathPM(k.writes(a), (Path \ i) ++ _)
      }.foldLeft[PM.PM](Map.empty)(_ ++ _)
    }

  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: PM.PM]) =
    list(k).contramap(_.toList)

  implicit def map[A](implicit k: Write[A, _ >: Out <: PM.PM]) =
    Write { m =>
      m.flatMap { case (s, a) =>
        val pm = k.writes(a)
        PM.repathPM(pm, (Path \ s) ++ _)
      }
    }

  implicit def traversable[A](implicit k: Write[A, _ >: Out <: PM.PM]) =
    list(k).contramap(_.toList)
}

object WritesGrammar extends WritesGrammar
