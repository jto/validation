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

trait LowPriorityDestruct {
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
}

object Destruct extends LowPriorityDestruct {
  def apply[FA](implicit d: Destruct[FA]): Aux[FA, d.F, d.A] = d

  // implicit def recDestruct[FA, A](implicit out: Outer[FA], in: Inner.Aux[FA, A], d: Destruct[A]): Aux[FA, λ[α => out.F[d.F[α]]], d.A] =
  //   new Destruct[FA] {
  //     type F[α] = out.F[d.F[α]]
  //     type A = d.A
  //   }
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

trait LowPriorityDestruct1 {
  type Aux[FA0[_], F0[_], A0[_]] =
    Destruct1[FA0] {
      type F[X] = F0[X]
      type A[X] = A0[X]
    }

  implicit def defaultDestruct1[FA[_]](implicit out: Outer1[FA], in: Inner1[FA]): Aux[FA, out.F, in.A] =
    new Destruct1[FA] {
      type F[α] = out.F[α]
      type A[α] = in.A[α]
    }
}

object Destruct1 extends LowPriorityDestruct1 {
  def apply[FA[_]](implicit d: Destruct1[FA]): Aux[FA, d.F, d.A] = d

  // implicit def recDestruct[FA[_], A[_]](implicit out: Outer1[FA], in: Inner1.Aux[FA, A], d: Destruct1[A]): Aux[FA, λ[α => out.F[d.F[α]]], d.A] =
  //   new Destruct1[FA] {
  //     type F[α] = out.F[d.F[α]]
  //     type A[α] = d.A[α]
  //   }
}

// trait Match[F[_], FA] {
//   type A
// }

// trait LowPriorityMatch {
//   type Aux[F0[_], FA, A0] = Match[F0, FA] { type A = A0 }

//   implicit def defaultMatch[F[_], FA](implicit out: Outer.Aux[FA, F], in: Inner[FA]): Aux[F, FA, in.A] =
//     new Match[F, FA] {
//       type A = in.A
//     }
// }

// object Match {
//   implicit def recMatch[F[_], FA, Out[_], F1[_], A](implicit
//     df: Destruct1.Aux[F, Out, F1],
//     dfa: Destruct.Aux[FA, Out, A],
//     m: Match[F1, A]
//   ) = ???
// }

