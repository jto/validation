package jto.validation

trait Mixer[F1[_], F2[_]] {
  def mix[A](m1: F1[A], m2: F2[A]): F1[A] with F2[A]
}

object Mixer {
  implicit def mixRuleWrite[I, J]: Mixer[Rule[I, ?], Write[?, J]] =
    new Mixer[Rule[I, ?], Write[?, J]] {
      def mix[A](
          m1: Rule[I, A], m2: Write[A, J]): Rule[I, A] with Write[A, J] =
        new Rule[I, A] with Write[A, J] {
          def validate(data: I): VA[A] = m1.validate(data)
          def writes(i: A): J = m2.writes(i)
        }
    }
}
