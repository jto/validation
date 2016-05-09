package jto.validation
package free
package syntax

class MatchOps[A](a: A) {
  def unify[G[_]](implicit m: Match0[G[Match.τ], A]): G[m.τ] =
    a.asInstanceOf[G[m.τ]]
}

object unify {
  implicit def toMatchOps[A](a: A) =
    new MatchOps[A](a)
}