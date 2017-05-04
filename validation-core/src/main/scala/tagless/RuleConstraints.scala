package jto.validation
package v3.tagless

trait RuleConstraints extends Constraints[Rule] {
  object R extends GenericRules
  def required[A]: Rule[Option[A], A] = R.required
  def max[A](a: A)(implicit O: Ordering[A]) = R.max(a)
  def min[A](a: A)(implicit O: Ordering[A]) = R.min(a)
  def notEmpty = R.notEmpty
  def minLength(l: Int) = R.minLength(l)
  def email = R.email
  def forall[I, O](k: Rule[I,O]) = R.seqR(k)
  def equalTo[T](t: T) = R.equalTo(t)
}