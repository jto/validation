package play.api.data.mapping

import org.specs2.mutable._
import scala.util.control.Exception._
import play.api.libs.functional._
import play.api.libs.functional.syntax._

object ExperimentalSpec extends Specification {

  object R extends GenericRules
  import R._

  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)

  val address = Address("Southover Street", "Brighton", "BN2 9UA")
  val person = Person("Joe Grey", 37, address)

  "Experimental" should {
    import shapeless.test._

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

      illTyped("""(Get[Person] \ 'address \ 'plip).read(notEmpty)""")  // does not compile

      Get[Person] { __ =>
        (__ \ 'age).read(min(0)) ~>
        (__ \ 'address \ 'city).read(notEmpty)
      }.validate(Person("Joe Grey", -12, address.copy(city = ""))) mustEqual Failure(List((Path \ "age",List(ValidationError("error.min", 0))), (Path \ "address" \ "city", List(ValidationError("error.required")))))
    }
  }
}
