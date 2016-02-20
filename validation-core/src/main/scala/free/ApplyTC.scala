package jto.validation.free

trait ApplyTC[TC[_] <: { type Out }] extends Serializable { self =>
  def apply[A](tc: TC[A], a: A): tc.Out
}