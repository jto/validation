package jto.validation

trait At[F[_, _], S, A] {
  def apply(p: Path): F[S, A]
}

object At {
  def apply[F[_, _], S, A](implicit at: At[F, S, A]) = at
}