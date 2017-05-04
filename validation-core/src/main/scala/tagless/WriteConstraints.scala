package jto.validation
package v3.tagless

trait WriteConstraints extends Constraints[types.flip[Write]#λ] {
  def max[A](a: A)(implicit O: Ordering[A]) = ???
  def min[A](a: A)(implicit O: Ordering[A]) = ???
  def notEmpty = ???
  def minLength(l: Int) = ???
  def email = ???
  def forall[I, O](k: Write[O, I]) = ???
  def equalTo[T](t: T) = ???
}