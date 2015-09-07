import jto.validation._

import org.specs2.mutable._

object DefaultRulesSpec extends Specification {

  object R extends GenericRules
  import R._

  "DefaultRules" should {

    def failure(m: String, args: Any*) = Invalid(Seq(Path -> Seq(ValidationError(m, args:_*))))

    "validate non emptyness" in {
      notEmpty.validate("foo") mustEqual(Valid("foo"))
      notEmpty.validate("") mustEqual(failure("error.required"))
    }

    "validate min" in {
      min(4).validate(5) mustEqual(Valid(5))
      min(4).validate(4) mustEqual(Valid(4))
      min(4).validate(1) mustEqual(failure("error.min", 4))
      min(4).validate(-10) mustEqual(failure("error.min", 4))

      min("a").validate("b") mustEqual(Valid("b"))
    }

    "validate max" in {
      max(8).validate(5) mustEqual(Valid(5))
      max(5).validate(5) mustEqual(Valid(5))
      max(0).validate(1) mustEqual(failure("error.max", 0))
      max(-30).validate(-10) mustEqual(failure("error.max", -30))
    }

  }
}
