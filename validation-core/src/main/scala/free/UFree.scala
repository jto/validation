package jto.validation
package free

import cats.InvariantMonoidal
import cats.syntax.all._
import jto.validation._

/**
  * Uncontrained Free to build validation like structure.
  *
  * Contrary to `Free` tranditional free structures, `UFree` are not tight to a particular ADT.
  *
  * {{{
  * // Example data type
  * case class Foo(b: Boolean, s: String, i: Int)
  *
  * // UFree expression, built with internal algebra.
  * val fa: UFree[Foo] =
  *   Imap(
  *     Zip(
  *       Zip(
  *         Leaf(Path \ "boolean_field")[Boolean],
  *         Leaf(Path \ "string_field")[String]
  *       ),
  *       Leaf(Path \ "int_field")[Int]
  *     )
  *   )(
  *     { case ((b, s), i)  => Foo(b, s, i) }
  *     { case Foo(b, s, i) => ((b, s), i)  }
  *   )
  *
  * // Summons and assembles Show[Boolean], Show[String] and Show[Int]
  * val a: Show[Foo] = fa.materialize[Show]
  * }}}
  *
  * `|@|`/`~` style syntax is available, implemented via the statically generated `UFreeSyntaxN`.
  * Note that `.materialize` reports precise errors about missing implicits.
  */
sealed trait UFree[A] {
  type Implicits <: HList

  private[validation] def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => Path=>F[t]], Implicits]): F[A]

  private[validation] def rePath(path: Path): UFree.Aux[A, Implicits]

  def ~[IB <: HList, IO <: HList, B]
    (fb: UFree.Aux[B, IB])
    (implicit p: UnliftPrepend.Aux[Implicits, IB, IO])
    : UFreeSyntax.UFreeSyntax2[IO, A, B] =
      UFreeSyntax.UFreeSyntax2(UFree.Zip(this: this.type, fb)(p))

  def imap[X](f: A => X, g: X => A): UFree.Aux[X, Implicits] =
    UFree.Imap(this: this.type)(f, g)

  def as[X](implicit t: CaseClassTupler.Aux[X, A]): UFree.Aux[X, Implicits] =
    imap(t.from, t.to)
}

trait LowPrioUFree {
  /** Calling `implicitly[UFree[A]]` generated a leaf, unless there is implicit `UFree` which higher priority. */
  implicit def freeAsLeaf[A]: UFree.Leaf[A] = UFree.Leaf[A](Path)
}

object UFree extends LiftInstances with LowPrioUFree {
  type Aux[A, I] = UFree[A] { type Implicits = I }

  /** Generates a `UFree` with case class fields as leafs labels using a macro. */
  def gen[A]: GenUFreeCurried[A] = new GenUFreeCurried[A]()

  private[validation] class GenUFreeCurried[A] {
    def apply[I](): Aux[A, I] = macro UFreeMacro.free[A, I]
  }

  private[validation] case class Leaf[A](path: Path) extends UFree[A] {
    type Implicits = A :: HNil

    private[validation] def rePath(p: Path): UFree.Aux[A, A :: HNil] =
      this.copy(path = p ++ path)

    private[validation] def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => Path=>F[t]], A :: HNil]): F[A] =
      lift.instances.asInstanceOf[(Path=>F[A]) :: HNil].head.apply(path)
  }

  private[validation] case class Zip[IA <: HList, IB <: HList, IO <: HList, A, B]
    (fa: UFree.Aux[A, IA], fb: UFree.Aux[B, IB])
    (implicit p: UnliftPrepend.Aux[IA, IB, IO])
    extends UFree[(A, B)] {
      type Implicits = IO

      private[validation] def rePath(path: Path): UFree.Aux[(A, B), IO] =
        Zip[IA, IB, IO, A, B](fa.rePath(path), fb.rePath(path))(p)

      private[validation] def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => Path=>F[t]], IO]): F[(A, B)] = {
        type ManualHighOrderUnification[t] = Path=>F[t]
        implicit val (la, lb) = p.unlift(lift: Lift[ManualHighOrderUnification, IO])
        fa.mat[F](la).product(fb.mat[F](lb))
      }
    }

  private[validation] case class Imap[IA <: HList, A, B]
    (fa: UFree.Aux[A, IA])
    (f: A => B, g: B => A)
    extends UFree[B] {
      type Implicits = IA

      private[validation] def rePath(path: Path): UFree.Aux[B, IA] =
        Imap(fa.rePath(path))(f, g)

      private[validation] def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => Path=>F[t]], IA]): F[B] =
        fa.mat[F](lift).imap(f)(g)
    }
}
