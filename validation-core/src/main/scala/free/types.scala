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
    new syntax.MatchOps[A](a)
}

trait Match0[A, B] { type τ }
object Match0 {
  type Aux[A, B, τ0] = Match0[A, B] { type τ = τ0 }

  def apply[A, B](implicit m0: Match0[A, B]): Aux[A, B, m0.τ] = m0

  implicit def defaultMatch0[A, B, τ0](implicit m: Match.Aux[A, B, τ0 :: HNil]): Aux[A, B, τ0] =
    new Match0[A, B] { type τ = τ0 }
}
