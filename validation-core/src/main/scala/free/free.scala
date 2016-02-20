package jto.validation
package free

import cats.{ Cartesian, Functor }
import cats.functor.{ Contravariant, Invariant }

trait Interpreter[I, G[_]] {
  type T
  type Out = G[T]
  def apply(is: I): Out
}

object Interpreter {
  type Aux[I0, G0[_], T0] = Interpreter[I0, G0]{ type T = T0 }
  type Of[G[_]] = { type T[α] = Interpreter[α, G] }

  def apply[I0, G0[_], T0](f: I0 => G0[T0]): Interpreter.Aux[I0, G0, T0] =
    new Interpreter[I0, G0]{
      type T = T0
      def apply(is: I0): G0[T0] = f(is)
    }
}

object Grammar {
  sealed trait Just
  case object Just
  case object NotEmpty
  case class Min[T](value: T)
  case class Max[T](value: T)
}

final case class At[F](path: Path, as: F)

trait Is {
  type T
  type V
  val v: V
  override def toString = s"Is($v)"
}

object Is {
  type Aux[T0, V0] = Is {
    type T = T0
    type V = V0
  }

  final class Deferred[T0] {
    def apply[V0](v0: V0): Aux[T0, V0] =
      new Is {
        type T = T0
        type V = V0
        val v = v0
      }
  }

  def apply[T] = new Deferred[T]
  def unapply[T, V](i: Aux[T, V]): Option[V] = Option(i.v)
}

object Interpreters {
  import Grammar._
  type R[T] = Rule[T, T]
  implicit def minI[T: Ordering]: Interpreter.Aux[Is.Aux[T, Min[T]], R, T] =
    Interpreter{ case Is(Min(v)) => GenericRules.min(v) }
  implicit def maxI[T: Ordering]: Interpreter.Aux[Is.Aux[T, Max[T]], R, T] =
    Interpreter{ case Is(Max(v)) => GenericRules.max(v) }
  implicit def notEmptyI: Interpreter.Aux[Is.Aux[String, NotEmpty.type], R, String] =
    Interpreter{ case _ => GenericRules.notEmpty }
}

object FreeVersion {
  import Grammar._
  import cats.free.FreeApplicative
  import FreeApplicative._

  val a1 = At(Path \ "foo", Is[String](NotEmpty))
  val a2 = At(Path \ "bar", Is[Int](Min(3)))
  val a3 = At(Path \ "int", Is[Int](Max(10)))

  import Interpreters._
  import cats.arrow.NaturalTransformation
  import cats.Id

  type R[T] = Rule[T, T]

  def interpret[G[_]] =
    new ApplyTC[Interpreter[?, G]] {
      def apply[A](tc: Interpreter[A, G], a: A): tc.Out = tc(a)
    }

  val toList =
    new NaturalTransformation[At, List] {
      def apply[A](fa: At[A]): List[A] = List(fa.as)
    }

  val free =
    (
      lift(a1) ~
      lift(a2) ~
      lift(a3)
    )

  val interpreted = free.withImplicits[Interpreter.Of[R]#T](interpret[R])

  import cats.std.list._
  val debug = interpreted.tupled.compile(toList).fold


}


