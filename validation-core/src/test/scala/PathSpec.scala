import jto.validation._

import org.specs2.mutable._

object PathSpec extends Specification {
  "Path" should {
    "be compareable" in {
      (Path \ "foo" \ "bar") shouldBe (Path \ "foo" \ "bar")
      (Path \ "foo" \ "bar").hashCode shouldBe (Path \ "foo" \ "bar").hashCode
      (Path \ "foo" \ "bar") should not be (Path \ "foo")
      (Path \ "foo" \ "bar").hashCode should not be (Path \ "foo").hashCode
    }

    "compose" in {
      val c = (Path \ "foo" \ "bar") compose (Path \ "baz")
      val c2 = (Path \ "foo" \ "bar") ++ (Path \ "baz")
      c shouldBe (Path \ "foo" \ "bar" \ "baz")
      c2 shouldBe (Path \ "foo" \ "bar" \ "baz")
    }

    "have deconstructors" in {
      val path = Path \ "foo" \ "bar" \ "baz"

      val (h \: t) = path
      h shouldBe (Path \ "foo")
      t shouldBe (Path \ "bar" \ "baz")

      val (h1 \: h2 \: t2) = path
      h1 shouldBe (Path \ "foo")
      h2 shouldBe (Path \ "bar")
      t2 shouldBe (Path \ "baz")
    }
  }
}
