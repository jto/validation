package jto.validation

import cats.Monoid
import cats.functor.Contravariant
import shapeless.tag, tag.@@
import shapeless.{Generic, HList}
import shapeless.ops.hlist.{Tupler => STupler}

trait Tupler[H] {
  type In
  def to(t: In): H
  def from(t: H): In
}

object Tupler {
  type Aux[H, T] = Tupler[H]{ type In = T }

  implicit def hlistTupler[H <: HList, T](implicit
    T: STupler.Aux[H, T],
    G: Generic.Aux[T, H]
  ): Aux[H, T] =
    new Tupler[H] {
      type In = T
      def to(t: T): H = G.to(t)
      def from(t: H): T = G.from(t)
    }
}

trait WriteLike[I, +O] {

  /**
    * "Serialize" `i` to the output type
    */
  def writes(i: I): O
}

object WriteLike {
  implicit def zero[I]: WriteLike[I, I] = Write(identity[I] _)
}

trait Write[I, +O] extends WriteLike[I, O] {

  /**
    * returns a new Write that applies function `f` to the result of this write.
    * {{{
    *  val w = Writes.int.map("Number: " + _)
    *  w.writes(42) == "Number: 42"
    * }}}
    */
  def map[B](f: O => B): Write[I, B] =
    Write[I, B] {
      f.compose(x => this.writes(x))
    }

  /**
    * Returns a new Write that applies `this` Write, and then applies `w` to its result
    */
  def andThen[OO >: O, P](w: WriteLike[OO, P]): Write[I, P] =
    this.map(o => w.writes(o))

  def contramap[B](f: B => I): Write[B, O] =
    Write[B, O]((b: B) => writes(f(b)))

  def from[T](implicit G: Generic.Aux[T, I]): Write[T, O] =
    contramap(t => G.to(t))

  def tupled(implicit tupler: Tupler[I]): Write[tupler.In, O] =
    contramap(i => tupler.to(i))
}

object Write {
  def gen[I, O]: Write[I, O] = macro MappingMacros.write[I, O]

  def apply[I, O](w: I => O): Write[I, O] @@ Root =
    tag[Root](new Write[I, O] {
      def writes(i: I) = w(i)
    })

  sealed trait Deferred[O] {
    def apply[I](i: I)(implicit w: WriteLike[I, O]) = w.writes(i)
  }

  def apply[O] = new Deferred[O]{}

  def of[I, O](implicit w: Write[I, O]): Write[I, O] = w

  def toWrite[I, O](r: WriteLike[I, O]): Write[I, O] =
    new Write[I, O] {
      def writes(data: I): O = r.writes(data)
    }

  implicit def zero[I]: Write[I, I] @@ Root =
    tag[Root](toWrite(WriteLike.zero[I]))

  implicit def contravariantWrite[O]: Contravariant[Write[?, O]] =
    new Contravariant[Write[?, O]] {
      def contramap[A, B](wa: Write[A, O])(f: B => A): Write[B, O] =
        wa.contramap(f)
    }

  implicit def writeSyntaxCombine[O](
      implicit m: Monoid[O]): SyntaxCombine[Write[?, O]] =
    new SyntaxCombine[Write[?, O]] {
      def apply[A, B](wa: Write[A, O], wb: Write[B, O]): Write[A ~ B, O] =
        Write[A ~ B, O] {
          case a ~ b => m.combine(wa.writes(a), wb.writes(b))
        }
    }

  implicit def writeContravariantSyntaxObs[I, O: Monoid](
      w: Write[I, O])(implicit fcb: SyntaxCombine[Write[?, O]])
    : ContravariantSyntaxObs[Write[?, O], I] =
    new ContravariantSyntaxObs[Write[?, O], I](w)(fcb)

  import cats.arrow.FunctionK
  implicit def writeLiftFunctionK[Out]: FunctionK[Write[Option[Out], ?], λ[α => Write[Option[Out],Option[α]]]] =
    new FunctionK[Write[Option[Out], ?], λ[α => Write[Option[Out],Option[α]]]] {
      def apply[A](fa: Write[Option[Out], A]): Write[Option[Out], Option[A]] =
        Write { mo =>
          Option(fa.writes(mo))
        }
    }

  import cats.arrow.Arrow
  implicit def writeArrow =
    new Arrow[Write] {
      def lift[A, B](f: A => B): Write[A, B] =
        Write(f)

      def id[A]: Write[A, A] =
        Write.zero

      def compose[A, B, C](f: Write[B, C], g: Write[A, B]): Write[A, C] =
        g andThen f

      def first[A, B, C](fa: Write[A, B]): Write[(A, C), (B, C)] =
        Write { case (a, c) =>
          (fa.writes(a), c)
        }
    }

  import v3.tagless.types.flip
  implicit def invertedWriteArrow: Arrow[flip[Write]#λ] = ???
}
