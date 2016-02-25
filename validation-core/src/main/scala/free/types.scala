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

/*
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


trait Inner1[FA[_]] {
  type A[_]
}

object Inner1 {
  type Aux[FA[_], A0[_]] = Inner1[FA] {
    type A[α] = A0[α]
  }

  def apply[FA[_]](implicit is: Inner1[FA]): Aux[FA, is.A] = is

  implicit def inner1[F0[_], A0[_]]: Aux[λ[α => F0[A0[α]]], A0] =
    new Inner1[λ[α => F0[A0[α]]]] {
      type A[α] = A0[α]
    }
}

trait Outer1[FA[_]] {
  type F[_]
}

object Outer1 {
  type Aux[FA[_], F0[_]] = Outer1[FA] {
    type F[α] = F0[α]
  }

  def apply[FA[_]](implicit is: Outer1[FA]): Aux[FA, is.F] = is

  implicit def outer1[F0[_], A0[_]]: Aux[λ[α => F0[A0[α]]], F0] =
    new Outer1[λ[α => F0[A0[α]]]] {
      type F[α] = F0[α]
    }
}


trait Destruct1[FA[_]] {
  type F[_]
  type A[_]
}

object Destruct1 {

  type Aux[FA0[_], F0[_], A0[_]] =
    Destruct1[FA0] {
      type F[α] = F0[α]
      type A[α] = A0[α]
    }

  implicit def defaultDestruct1[F0[_], A0[_]](implicit out: Outer1[λ[α => F0[A0[α]]]], in: Inner1[λ[α => F0[A0[α]]]]): Aux[λ[α => F0[A0[α]]], out.F, in.A] =
    new Destruct1[λ[α => F0[A0[α]]]] {
      type F[α] = out.F[α]
      type A[α] = in.A[α]
    }

  def apply[FA[_]](implicit d: Destruct1[FA]): Aux[FA, d.F, d.A] = d
}
*/

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
  implicit def match1[Fτ, FA, Out[_], Aτ, AFA]
    (implicit
      outτ: Outer.Aux[Fτ, Out],
      outfa: Outer.Aux[FA, Out],
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

  // Match[Option[τ], List[Option[List[Int]]]] // should not compile
  // Match[List[τ], Option[List[Int]]] // should not compile
  // Match[List[List[τ]], List[Option[List[Int]]]] // should not compile
}

