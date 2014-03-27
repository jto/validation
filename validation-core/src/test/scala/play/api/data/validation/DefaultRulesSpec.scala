package play.api.data.mapping

import org.specs2.mutable._
import scala.util.control.Exception._
import play.api.libs.functional._
import play.api.libs.functional.syntax._

object DefaultRulesSpec extends Specification {

  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)

  val address = Address("Southover Street", "Brighton", "BN2 9UA")
  val person = Person("Joe Grey", 37, address)

  object R extends GenericRules
  import R._

  "DefaultRules" should {

    def failure(m: String, args: Any*) = Failure(Seq(Path -> Seq(ValidationError(m, args:_*))))

    "validate non emptyness" in {
      notEmpty.validate("foo") mustEqual(Success("foo"))
      notEmpty.validate("") mustEqual(failure("error.required"))
    }

    "validate min" in {
      min(4).validate(5) mustEqual(Success(5))
      min(4).validate(4) mustEqual(Success(4))
      min(4).validate(1) mustEqual(failure("error.min", 4))
      min(4).validate(-10) mustEqual(failure("error.min", 4))

      min("a").validate("b") mustEqual(Success("b"))
    }

    "validate max" in {
      max(8).validate(5) mustEqual(Success(5))
      max(5).validate(5) mustEqual(Success(5))
      max(0).validate(1) mustEqual(failure("error.max", 0))
      max(-30).validate(-10) mustEqual(failure("error.max", -30))
    }

    "validate a la carte" in {

      (Get[Person] \ 'age).read(min(0))
        .validate(person) mustEqual Success(person)

      (Get[Person] \ 'age).read(max(0))
        .validate(person) mustEqual Failure(List((Path \ "age", List(ValidationError("error.max", 0)))))

      (Get[Person] \ 'address \ 'city).read(notEmpty)
        .validate(person) mustEqual Success(person)

      (Get[Person] \ 'address \ 'city).read(maxLength(1))
        .validate(person) mustEqual Failure(List((Path \ "address" \ "city", List(ValidationError("error.maxLength", 1)))))

      Get[Person] { __ =>
        (__ \ 'age).read(min(0)) ~>
        (__ \ 'address \ 'city).read(notEmpty)
      }.validate(person) mustEqual Success(person)

      Get[Person] { __ =>
        (__ \ 'age).read(min(0)) ~>
        (__ \ 'address \ 'city).read(notEmpty)
      }.validate(Person("Joe Grey", -12, address.copy(city = ""))) mustEqual Failure(List((Path \ "age",List(ValidationError("error.min", 0))), (Path \ "address" \ "city", List(ValidationError("error.required")))))

      // (Get[Person] \ 'address \ 'plip).read(notEmpty) // does not compile

    }
  }
}
