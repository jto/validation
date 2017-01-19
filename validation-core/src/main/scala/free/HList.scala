package jto.validation
package free

/**
  * `HList`. Inspired from the shapeless implementation:
  * https://github.com/milessabin/shapeless/blob/shapeless-2.3.1/core/src/test/scala/shapeless/hlist.scala
  *
  * Copyright (c) 2011-14 Miles Sabin
  * Licensed under the Apache 2 license, http://www.apache.org/licenses/LICENSE-2.0.
  */
sealed trait HList

object HList {
  implicit class ConsHList[L <: HList](l: L) {
    def ::[H](h : H): H :: L = HCons(h, l)
  }
}

sealed trait ::[+H, +T <: HList] extends HList {
  def head: H
  def tail: T
}
final case class HCons[+H, +T <: HList](head: H, tail: T) extends ::[H, T]

sealed trait HNil extends HList
final case object HNil extends HNil

/**
  * `Lift` is a non-sealed equivalent to `shapeless.ops.hlist.LiftAll`, with the addition of the
  * `split` method to deconstruct a `Lift[F, H :: T]` into `(F[H], Lift[F, T])`.
  */
trait Lift[F[_], In <: HList] { self =>
  type Out <: HList
  def instances: Out

  def cons[A](fa: F[A]): Lift.Aux[F, A :: In, F[A] :: Out] =
    new Lift[F, A :: In] {
      type Out = F[A] :: self.Out
      def instances = fa :: self.instances
    }
}

object Lift {
  type Aux[F[_], In0 <: HList, Out0 <: HList] = Lift[F, In0] { type Out = Out0 }

  def apply[F[_], In <: HList](implicit l: Lift[F, In]): Lift[F, In] = l

  def empty[F[_]]: Lift.Aux[F, HNil, HNil] =
    new Lift[F, HNil] {
      type Out = HNil
      def instances = HNil
    }

  implicit class LiftSplit[F[_], H, T <: HList](self: Lift[F, H :: T]) {
    def split: (F[H], Lift[F, T]) = {
      val HCons(h, t) = self.instances
      (
        h.asInstanceOf[F[H]], // By definition of Lift
        new Lift[F, T] { type Out = t.type; def instances = t }
      )
    }
  }
}

/**
 * `UnliftPrepend` has the same functionalities than `shapeless.ops.hlists.Prepend`, with the addition
 * of the `unlift` methods which splits a `Lift[F, P ::: S]` in `(Lift[F, P], Lift[F, S])`.
 */
trait UnliftPrepend[P <: HList, S <: HList] {
  def prepend(prefix: P, suffix: S): Out

  def unlift[F[_]](la: Lift[F, Out]): (Lift[F, P], Lift[F, S])

  type Out <: HList
}

trait LowPriorityUnliftPrepend {
  type Aux[P <: HList, S <: HList, Out0 <: HList] = UnliftPrepend[P, S] { type Out = Out0 }

  implicit def hlistUnliftPrepend[PH, PT <: HList, S <: HList]
    (implicit pt: UnliftPrepend[PT, S]): UnliftPrepend.Aux[PH :: PT, S, PH :: pt.Out] =
      new UnliftPrepend[PH :: PT, S] {
        type Out = PH :: pt.Out

        def prepend(prefix: PH :: PT, suffix: S): Out =
          HCons(prefix.head, pt.prepend(prefix.tail, suffix))

        def unlift[F[_]](la: Lift[F, Out]): (Lift[F, PH :: PT], Lift[F, S]) = {
          val (fa, lt) = la.split
          val (pf, sf) = pt.unlift(lt)
          (pf.cons(fa), sf)
        }
      }
}

object UnliftPrepend extends LowPriorityUnliftPrepend {
  def apply[P <: HList, S <: HList](implicit p: UnliftPrepend[P, S]): UnliftPrepend[P, S] = p

  implicit def hnilUnliftPrepend1[S <: HList]: UnliftPrepend.Aux[HNil, S, S] =
    new UnliftPrepend[HNil, S] {
      type Out = S

      def prepend(prefix: HNil, suffix: S): S = suffix

      def unlift[F[_]](la: Lift[F, Out]): (Lift[F, HNil], Lift[F, S]) = (Lift.empty, la)
    }
}
