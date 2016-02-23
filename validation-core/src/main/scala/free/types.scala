package jto.validation
package free

trait Inner[FA] {
  type A
}

object Inner {
  type Aux[FA, A0] = Inner[FA] {
    type A = A0
  }

  def apply[FA](implicit is: Inner[FA]): Aux[FA, is.A] = is

  implicit def inner[F0[_], A0]: Aux[F0[A0], A0] =
    new Inner[F0[A0]] {
      type A = A0
    }
}

trait Outer[FA] {
  type F[_]
}

object Outer {
  type Aux[FA, F0[_]] = Outer[FA] {
    type F[X] = F0[X]
  }

  def apply[FA](implicit is: Outer[FA]): Aux[FA, is.F] = is

  implicit def outer[F0[_], A0]: Aux[F0[A0], F0] =
    new Outer[F0[A0]] {
      type F[X] = F0[X]
    }
}

trait Match[F[_], FA] {
  type In
}

trait LowPriorityMatch {
  type Aux[F[_], FA, In0] = Match[F, FA] { type In = In0 }

  implicit def match1[Out0[_], Out1[_], In0, In1, FA](implicit
    out0: Outer.Aux[FA, Out0],
    in0: Inner.Aux[FA, In0],
    out1: Outer.Aux[In0, Out1],
    in1: Inner.Aux[In0, In1],
    ev: FA =:= Out0[Out1[In1]]
  ): Aux[λ[α => Out0[Out1[α]]], FA, In1] =
    new Match[λ[α => Out0[Out1[α]]], FA]{
      type In = In1
    }
}

object Match extends LowPriorityMatch {
  implicit def directMatch[F[_], FA](implicit out: Outer.Aux[FA, F], in: Inner[FA]): Aux[F, FA, in.A] =
    new Match[F, FA] {
      type In = in.A
    }

  def apply[F[_], FA](implicit m: Match[F, FA]): Aux[F, FA, m.In] = m
}