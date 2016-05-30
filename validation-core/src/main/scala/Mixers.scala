package jto.validation

trait Mixer1[F1[_]] {
  def mix[A](m1: F1[A]): F1[A]
}

object Mixer1 {
  implicit def mixRule[I]: Mixer1[Rule[I, ?]] =
    new Mixer1[Rule[I, ?]] {
      def mix[A](m1: Rule[I, A]): Rule[I, A] = m1
    }

  implicit def mixWrite[I]: Mixer1[Write[?, I]] =
    new Mixer1[Write[?, I]] {
      def mix[A](m1: Write[A, I]): Write[A, I] = m1
    }
}

trait Mixer2[F1[_], F2[_]] {
  def mix[A](m1: F1[A], m2: F2[A]): F1[A] with F2[A]
}

object Mixer2 {
  implicit def mixRuleWrite[I, J]: Mixer2[Rule[I, ?], Write[?, J]] =
    new Mixer2[Rule[I, ?], Write[?, J]] {
      def mix[A](
          m1: Rule[I, A], m2: Write[A, J]): Rule[I, A] with Write[A, J] =
        new Rule[I, A] with Write[A, J] {
          def validate(data: I): VA[A] = m1.validate(data)
          def writes(i: A): J = m2.writes(i)
        }
    }
}

trait Mixer3[F1[_], F2[_], F3[_]] {
  def mix[A](m1: F1[A], m2: F2[A], m3: F3[A]): F1[A] with F2[A] with F3[A]
}
