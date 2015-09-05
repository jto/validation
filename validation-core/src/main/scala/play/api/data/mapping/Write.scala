package jto.validation

import cats.Monoid
import cats.functor.Contravariant

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
  def compose[OO >: O, P](w: WriteLike[OO, P]): Write[I, P] =
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

  def toWrite[I, O](r: WriteLike[I, O]): Write[I, O] =
    new Write[I, O] {
      def writes(data: I): O = r.writes(data)
    }

  implicit def zero[I]: Write[I, I] =
    toWrite(WriteLike.zero[I])

  implicit def contravariantWrite[O]: Contravariant[Write[?, O]] =
    new Contravariant[Write[?, O]] {
      def contramap[A, B](wa: Write[A, O])(f: B => A): Write[B, O] =
        wa.contramap(f)
    }
  
  implicit def functionalCanBuildWrite[O](implicit m: Monoid[O]): FunctionalCanBuild[Write[?, O]] =
    new FunctionalCanBuild[Write[?, O]] {
      def apply[A, B](wa: Write[A, O], wb: Write[B, O]): Write[A ~ B, O] = Write[A ~ B, O] { 
        case a ~ b => m.combine(wa.writes(a), wb.writes(b))
      }
    }
    
  implicit def fboWrite[I, O : Monoid](w: Write[I, O]): FunctionalBuilderOps[Write[?, O], I] =
    toFunctionalBuilderOps[Write[?, O], I](w)
}
