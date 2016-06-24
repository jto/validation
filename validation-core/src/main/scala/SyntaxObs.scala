package jto.validation

case class ~[A, B](_1: A, _2: B)

trait SyntaxCombine[M[_]] {
  def apply[A, B](ma: M[A], mb: M[B]): M[A ~ B]
}

class InvariantSyntaxObs[M[_], A](ma: M[A])(implicit combine: SyntaxCombine[M]) {
  def ~[B](mb: M[B]): InvariantSyntax[M]#InvariantSyntax2[A, B] = {
    val b = new InvariantSyntax(combine)
    new b.InvariantSyntax2[A, B](ma, mb)
  }
}

class FunctorSyntaxObs[M[_], A](ma: M[A])(implicit combine: SyntaxCombine[M]) {
  def ~[B](mb: M[B]): FunctorSyntax[M]#FunctorSyntax2[A, B] = {
    val b = new FunctorSyntax(combine)
    new b.FunctorSyntax2[A, B](ma, mb)
  }
}

class ContravariantSyntaxObs[M[_], A](ma: M[A])(
    implicit combine: SyntaxCombine[M]) {
  def ~[B](mb: M[B]): ContravariantSyntax[M]#ContravariantSyntax2[A, B] = {
    val b = new ContravariantSyntax(combine)
    new b.ContravariantSyntax2[A, B](ma, mb)
  }
}
