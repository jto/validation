package jto.validation
package free

import shapeless.{ ::, HList, HNil }
import shapeless.ops.hlist.{ Prepend, IsHCons }

sealed class Match[A, B] { type τs <: HList }

trait LowPriorityMatch {
  type Aux[A0, B0, τ0 <: HList] = Match[A0, B0] { type τs = τ0 }
  sealed trait τ
  sealed trait τ1[A]

  def apply[A, B](implicit m: Match[A, B]): Aux[A, B, m.τs] = m

  implicit def match2[F[_, _], A0, A1, B0, B1, τs0 <: HList, τs1 <: HList](implicit m0: Aux[A0, B0, τs0], m1: Aux[A1, B1, τs1], p: Prepend[τs0, τs1]): Aux[F[A0, A1], F[B0, B1], p.Out] =
    new Match[F[A0, A1], F[B0, B1]] { type τs = p.Out }
}

object Match extends LowPriorityMatch {

  implicit def matchEq[A0]: Aux[A0, A0, HNil] =
    new Match[A0, A0]{ type τs = HNil }

  implicit def match0[A0]: Aux[τ, A0, A0 :: HNil] =
    new Match[τ, A0] { type τs = A0 :: HNil }

  implicit def matchTC[F[_], TC[_[_]]]: Aux[TC[τ1], TC[F], F[τ] :: HNil] =
    new Match[TC[τ1], TC[F]] { type τs = F[τ] :: HNil }

  implicit def match1[F[_], A, B](implicit m0: Match[A, B]): Aux[F[A], F[B], m0.τs] =
      new Match[F[A], F[B]] { type τs = m0.τs }

  implicit def toMatchOps[A](a: A) =
    new MatchOps[A](a)
}

trait Match0[A, B] { type τ }
object Match0 {
  type Aux[A, B, τ0] = Match0[A, B] { type τ = τ0 }

  def apply[A, B](implicit m0: Match0[A, B]): Aux[A, B, m0.τ] = m0

  implicit def defaultMatch0[A, B, τ0](implicit m: Match.Aux[A, B, τ0 :: HNil]): Aux[A, B, τ0] =
    new Match0[A, B] { type τ = τ0 }
}

class MatchOps[A](a: A) {
  def unify[G[_]](implicit m: Match0[G[Match.τ], A]): G[m.τ] = a.asInstanceOf[G[m.τ]]
}

object test {
  import Match._
  import cats.Functor

  Match[Option[τ], Option[Int]]      // compiles
  Match[List[τ], List[Option[Int]]]  // compiles

  type EI[α] = Either[Int, α]
  Match[EI[τ], EI[Int]]   // compiles
  Match[EI[τ], Either[Int, String]]   // compiles

  type Foo[α] = List[Option[α]]
  Match[Foo[τ], Foo[Int]]   // compiles
  // Match[Foo[τ], List[Option[Int]]]   // compiles

  Match[List[Option[τ]], List[Option[Int]]] // compiles

  Match[List[Option[τ]], List[Option[List[Int]]]] // compiles
  Match[List[Option[List[τ]]], List[Option[List[Int]]]] // compile

  Match[Either[τ, Int], Either[String, Int]] // compile
  Match[Either[τ, τ], Either[String, Int]] // compile
  Match[Either[τ, τ], Either[List[String], Int]] // compile
  Match[Either[String, τ], Either[String, Int]] // compile
  Match[List[Either[String, τ]], List[Either[String, Int]]] // compile
  Match[Option[List[Either[String, τ]]], Option[List[Either[String, Int]]]] // compile

  Match[Option[Either[List[τ], τ]], Option[Either[List[String], Int]]]

  Match[Option[Either[List[τ], τ]], Option[Either[List[String], Int]]]

  Match[Functor[τ1], Functor[List]]

  List(1).unify[List]
  val e: Either[String, Int] = Right(4)
  e.unify[λ[α => Either[α, Int]]]
  Option(List(1)).unify[λ[α => Option[List[α]]]]
  List(e).unify[λ[α => List[Either[α, Int]]]]

  val es: Either[List[String], Int] = Right(4)
  es.unify[λ[α => Either[List[α], Int]]]
  es.unify[λ[α => Either[α, Int]]]

  // Match[Either[Int, τ], Either[String, Int]] // does not compile
  // Match[Option[τ], List[Option[List[Int]]]] // does not compile
  // Match[List[τ], Option[List[Int]]] // does not compile
  // Match[List[List[τ]], List[Option[List[Int]]]] // does not compile
  // Match[List[List[List[τ]]], List[Option[List[Int]]]] // does not compile
}

