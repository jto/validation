package jto.validation
package v3.tagless


sealed trait Auto[A] {
  def apply[T, K[_ ,_], Out](g: Grammar.Aux[T, K, Out], gs: Any*): K[Out, A] =
    macro MappingMacros.withGrammar[A, Grammar[T, K], K[Out, A]]
}

object Auto {
  def apply[A] = new Auto[A]{}
}
