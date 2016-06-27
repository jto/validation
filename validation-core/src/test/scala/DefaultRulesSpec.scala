import jto.validation._

import org.scalatest._

class DefaultRulesSpec extends WordSpec with Matchers {

  object R extends GenericRules
  import R._

  "DefaultRules" should {

    def failure(m: String, args: Any*) =
      Invalid(Seq(Path -> Seq(ValidationError(m, args: _*))))

    "validate non emptyness" in {
      notEmpty.validate("foo") shouldBe (Valid("foo"))
      notEmpty.validate("") shouldBe (failure("error.required"))
    }

    "validate min" in {
      min(4).validate(5) shouldBe (Valid(5))
      min(4).validate(4) shouldBe (Valid(4))
      min(4).validate(1) shouldBe (failure("error.min", 4))
      min(4).validate(-10) shouldBe (failure("error.min", 4))

      min("a").validate("b") shouldBe (Valid("b"))
    }

    "validate max" in {
      max(8).validate(5) shouldBe (Valid(5))
      max(5).validate(5) shouldBe (Valid(5))
      max(0).validate(1) shouldBe (failure("error.max", 0))
      max(-30).validate(-10) shouldBe (failure("error.max", -30))
    }
  }
}
