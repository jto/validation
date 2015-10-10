import jto.validation._
import org.specs2.mutable._
import org.specs2.matcher.MatchResult

/** Helper function to test a Format with Arbitrary inputs */
object TestRandomly extends Specification {
  def apply[O, OO <: O, T](format: Format[O, OO, T])(implicit arbitrary: Arbitrary[T]): MatchResult[Any] = {
    val t = arbitrary.value
    t shouldBe format.validate(format.writes(t)).toOption.get
    ()
  }

  def implicitly[O, OO <: O, T](implicit rule: RuleLike[O, T], write: WriteLike[T, OO], arbitrary: Arbitrary[T]) =
    apply(Format(Rule.toRule(rule), Write(write.writes)))
}
