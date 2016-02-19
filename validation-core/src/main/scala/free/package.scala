package jto.validation

package object free {
  import cats.free.FreeApplicative

  implicit def freeToCartesianBuilder[A](fa: FreeApplicative[At, A]) =
    new CartesianBuilder[λ[α => FreeApplicative[At, α]]] ~ fa
}