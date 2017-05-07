package jto.validation
package v3.tagless

trait WriteConstraints extends Constraints[types.flip[Write]#λ] {
  def max[A](a: A)(implicit O: Ordering[A]) = Write.zero
  def min[A](a: A)(implicit O: Ordering[A]) = Write.zero
  def notEmpty = Write.zero
  def minLength(l: Int) = Write.zero
  def email = Write.zero
  def forall[I, O](k: Write[O, I]) = Write { os => os.map(k.writes) }
  def equalTo[T](t: T) = Write.zero
}