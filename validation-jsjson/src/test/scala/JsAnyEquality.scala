package jto.validation
package jsjson
package test

import scala.scalajs.js
import org.scalatest._

trait JsAnyEquality {
  this: Matchers =>
  implicit class ShouldBeEqualAfterStringify(val dynamic: js.Any) {

    def shouldBe(otherDynamic: js.Any): Assertion =
      (dynamic, otherDynamic) match {
        case (d1 : js.Object, d2: js.Object) =>

          def props(d: js.Object): Map[String, js.Any] =
            d.asInstanceOf[js.Dictionary[js.Any]].toMap[String, js.Any]

          val props1 = props(d1)
          val props2 = props(d2)

          (props1.keySet should ===(props2.keySet))
          for (k <- props1.keySet) props1(k).shouldBe(props2(k))
          succeed

        case _ =>
          js.JSON.stringify(dynamic) shouldBe js.JSON.stringify(otherDynamic)
      }



  }
}
