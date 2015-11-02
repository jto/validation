import scala.scalajs.js
import org.scalatest._

trait JsAnyEquality { this: Matchers =>
  implicit class ShouldBeEqualAfterStringify(val dynamic: js.Any) {
    def shouldBe(otherDynamic: js.Any): Assertion =
      js.JSON.stringify(dynamic) shouldBe js.JSON.stringify(otherDynamic)
  }
}
