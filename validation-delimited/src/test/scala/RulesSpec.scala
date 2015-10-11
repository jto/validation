import jto.validation._
import jto.validation.delimited._
import jto.validation.delimited.Rules._
import org.scalatest._

class RulesSpec extends WordSpec with Matchers {
  "Rules" should {
    "demonstrate typical usage" in {
      case class Contact(name: String, email: String, birthday: Option[String])

      val contactReads = From[Delimited] { __ => (
        (__ \ 0).read[String] ~
        (__ \ 1).read(email) ~
        (__ \ 2).read(optionR[String](Rules.equalTo("N/A")))
      )(Contact)}

      val csv1 = "Ian Hummel,ian@example.com,1981-07-24"
      val csv2 = "Jane Doe,jane@example.com,N/A"

      contactReads.validate(csv1.split(",")) shouldBe Valid(Contact("Ian Hummel", "ian@example.com", Some("1981-07-24")))
      contactReads.validate(csv2.split(",")) shouldBe Valid(Contact("Jane Doe", "jane@example.com", None))
    }

    "read optional values" in {
      val str = Array("John Doe", "", "foo", "9393.12")
      (Path \ 0).read[Delimited, String].validate(str) shouldBe Valid("John Doe")
      (Path \ 1).read[Delimited, Option[String]].validate(str) shouldBe Valid(None)
      (Path \ 2).read[Delimited, Option[String]].validate(str) shouldBe Valid(Some("foo"))
      (Path \ 3).read[Delimited, Double].validate(str) shouldBe Valid(9393.12)
    }

    "read optional values using a different rule" in {
      val str = Array("John Doe", "\\N", "", "9393.12")

      (Path \ 0).read[Delimited, String].validate(str) shouldBe Valid("John Doe")
      (Path \ 1).read[Delimited, Option[String]](optionR(Rules.equalTo("\\N"))).validate(str) shouldBe Valid(None)
      (Path \ 2).read[Delimited, Option[String]](optionR(Rules.equalTo("\\N"))).validate(str) shouldBe Valid(Some(""))
      (Path \ 3).read[Delimited, Double].validate(str) shouldBe Valid(9393.12)
    }
  }
}
