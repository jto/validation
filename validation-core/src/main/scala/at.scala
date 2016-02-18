package jto.validation

import cats.{ Cartesian, Functor }
import cats.functor.{ Contravariant, Invariant }

trait Interpreter[I, G[_]] {
  type T
  def apply(is: I): G[T]
}

object Interpreter {
  type Aux[I0, G0[_], T0] = Interpreter[I0, G0]{ type T = T0 }
}

trait InterpreterOf[G[_]] {
  type T[α] = Interpreter[α, G]
}

object Grammar {
  sealed trait Just
  case object Just
  case object NotEmpty
  case class Min[T](value: T)
  case class Max[T](value: T)
}

final case class At[F](path: Path, as: F)
object At {
  implicit def atFunctor: Functor[At] = ???
}

trait Is {
  type T
  type V
  val v: V
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
}

object Interpreters {
  import Grammar._
  type R[T] = Rule[T, T]
  implicit def minI: Interpreter.Aux[Is.Aux[Int, Min[Int]], R, Int] = ???
  implicit def notEmptyI: Interpreter.Aux[Is.Aux[String, NotEmpty.type], R, Int] = ???
}

object FreeVersion {
  import Grammar._

  implicit def freeToCartesianBuilder[A](fa: At[A]) =
    new CartesianBuilder[At] ~ fa

  val a1 = At(Path \ "foo", Is[String](NotEmpty))
  val a2 = At(Path \ "bar", Is[Int](Min(3)))
  val a3 = At(Path \ "int", Is[Int](Max(10)))

  import Interpreters._

  type R[T] = Rule[T, T]

  val free =
    (a1 ~ a2).withImplicits[InterpreterOf[R]#T]
}


