package jto.validation

import cats.Monoid

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
}

object Write {
  def gen[I, O]: Write[I, O] = macro MappingMacros.write[I, O]

  def apply[I, O](w: I => O): Write[I, O] =
    new Write[I, O] {
      def writes(i: I) = w(i)
    }

  sealed trait Deferred[O] {
    def apply[I](i: I)(implicit w: WriteLike[I, O]) = w.writes(i)
  }

  def apply[O] = new Deferred[O]{}

  def of[I, O](implicit w: Write[I, O]): Write[I, O] = w

  def toWrite[I, O](r: WriteLike[I, O]): Write[I, O] =
    new Write[I, O] {
      def writes(data: I): O = r.writes(data)
    }

  implicit def zero[I]: Write[I, I] =
    toWrite(WriteLike.zero[I])

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

  implicit def writeDivisible[O: Monoid]: v3.Divisible[Write[?, O]] =
    new v3.Divisible[Write[?, O]] {
      type F[A] = Write[A, O]
      def divide[A, B, C](f: A => (B, C))(fb: F[B])(fc: F[C]): F[A] =
        Write { a =>
          val (b, c) = f(a)
          Monoid[O].combine(fb.writes(b), fc.writes(c))
        }
      def conquer[A]: F[A] =
        Write{_ => Monoid[O].empty }
      def contramap[A, B](wa: F[A])(f: B => A): F[B] =
        wa.contramap(f)
    }

  implicit def writeCompose =
    new cats.arrow.Compose[Write] {
      def compose[A, B, C](f: Write[B,C], g: Write[A,B]): Write[A,C] =
        g andThen f
    }

  implicit def writeHSequence0[O: Monoid] =
    v3.HSequence0.divisibleHSequence0[Write[?, O]]
}
