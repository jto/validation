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

trait Match[Fτ, FA] {
  type A
}

trait LowPriorityMatch {
  type Aux[Fτ, FA, A0] = Match[Fτ, FA] { type A = A0 }

  sealed trait τ

  implicit def match0[F[_], A0]: Aux[F[τ], F[A0], A0] =
    new Match[F[τ], F[A0]] {
      type A = A0
    }
}

object Match extends LowPriorityMatch {
  implicit def match1[Fτ, FA, Out[_], Out1[_], Aτ, AFA]
    (implicit
      outτ: Outer.Aux[Fτ, Out],
      outfa: Outer.Aux[FA, Out1],
      eq: Out[τ] =:= Out1[τ],
      inτ: Inner.Aux[Fτ, Aτ],
      infa: Inner.Aux[FA, AFA],
      m: Match[Aτ, AFA]
    ): Aux[Fτ, FA, m.A] = new Match[Fτ, FA] { type A = m.A }

  def apply[Fτ, FA](implicit m: Match[Fτ, FA]): Aux[Fτ, FA, m.A] = m
}

object test {
  import Match.τ

  Match[Option[τ], Option[Int]]      // compiles
  Match[List[τ], List[Option[Int]]]  // compiles

  type Foo[α] = List[Option[α]]
  Match[Foo[τ], Foo[Int]]   // compiles
  // Match[Foo[τ], List[Option[Int]]]   // should compile ? does not.

  Match[List[Option[τ]], List[Option[Int]]] // compiles
  Match[List[Option[τ]], List[Option[List[Int]]]] // compiles
  Match[List[Option[List[τ]]], List[Option[List[Int]]]] // compile

  Match[Option[τ], List[Option[List[Int]]]] // should not compile
  // Match[List[τ], Option[List[Int]]] // should not compile
  // Match[List[List[τ]], List[Option[List[Int]]]] // should not compile
}

