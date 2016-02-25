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
    type F[α] = F0[α]
  }

  def apply[FA](implicit is: Outer[FA]): Aux[FA, is.F] = is

  implicit def outer[F0[_], A0]: Aux[F0[A0], F0] =
    new Outer[F0[A0]] {
      type F[α] = F0[α]
    }
}

trait Destruct[FA] {
  type F[_]
  type A
}

object Destruct {
  type Aux[FA0, F0[_], A0] =
    Destruct[FA0] {
      type F[α] = F0[α]
      type A = A0
    }

  implicit def defaultDestruct[FA](implicit out: Outer[FA], in: Inner[FA]): Aux[FA, out.F, in.A] =
    new Destruct[FA] {
      type F[α] = out.F[α]
      type A = in.A
    }

  def apply[FA](implicit d: Destruct[FA]): Aux[FA, d.F, d.A] = d
}

trait Match[F[_], FA] {
  type A
}

trait LowPriorityMatch {
  type Aux[F0[_], FA, A0] = Match[F0, FA] { type A = A0 }

  implicit def defaultMatch[F0[_], FA](implicit d: Destruct[FA] { type F[α] = F0[α] }): Aux[F0, FA, d.A] =
    new Match[F0, FA] {
      type A = d.A
    }
}

object Match extends LowPriorityMatch {

  implicit def recMatch[Out[_], F[_], FA, A0](implicit
    dfa: Destruct.Aux[FA, Out, A0],
    m: Match[F, A0]
  ): Aux[λ[α => Out[F[α]]], FA, m.A] = new Match[λ[α => Out[F[α]]], FA] { type A = m.A }

  def apply[F[_], FA](implicit m: Match[F, FA]): Aux[F, FA, m.A] = m
}

object test {
  Match[Option, Option[Int]]
  Match[List, List[Option[Int]]]

  type Foo[α] = List[Option[α]]
  Match[Foo, List[Option[Int]]]

  Match[λ[α => List[Option[α]]], List[Option[Int]]]
}

