package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._

class JsonRulesSpec extends RulesSpec[JsValue] {
  val grammar = RulesGrammar
  val testCases = JsonTestCases

  import grammar._

  "Specific JSON Rules" should {
    "support null" in {
        val jn = at(Path \ "n")(is[JsNull.type])

        jn.validate(Json.obj("n" -> JsNull)) shouldBe Valid(JsNull)

        jn.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "null"))))

        jn.validate(Json.obj("n" -> 4.5)) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "null"))))
      }

      "JsObject" in {
        at(Path \ "o")(is[JsObject])
          .validate(Json.obj("o" -> Json.obj("n" -> "foo"))) shouldBe
            Valid(JsObject(Seq("n" -> JsString("foo"))))

        def n = at(Path \ "n")(is[JsObject])

        n.validate(Json.obj("n" -> 42)) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Object"))))

        n.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Object"))))

        n.validate(Json.obj("n" -> Seq("foo"))) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Object"))))
      }

     "JsString" in {
        def n = at(Path \ "n")(is[JsString])
        n.validate(Json.obj("n" -> "foo")) shouldBe Valid(JsString("foo"))
        n.validate(Json.obj("n" -> 42)) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "String"))))
      }

      "JsNumber" in {
        def n = at(Path \ "n")(is[JsNumber])
        n.validate(Json.obj("n" -> 4)) shouldBe Valid(JsNumber(4))

        n.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Number"))))

        n.validate(Json.obj("n" -> 4.5)) shouldBe Valid(JsNumber(4.5))
      }

      "JsBoolean" in {
        def n = at(Path \ "n")(is[JsBoolean])
        n.validate(Json.obj("n" -> true)) shouldBe Valid(JsBoolean(true))
        n.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean"))))
      }
  }

}
