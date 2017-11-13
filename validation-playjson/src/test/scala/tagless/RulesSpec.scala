package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._

class JsonRulesSpec extends RulesSpec[JsValue] {
  val grammar = RulesGrammar
  val testCases = JsonTestCases

  type From = JsValue
  def transform = identity

  import grammar._

  "Specific JSON Rules" should {

    "check json types" when {
      "string" in {
        import testCases.string._
        def n = at(Path \ "n").is(req[String])
        def o = at(Path \ "o").is(req[String])

        n.validate(_42) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "String")))))

        n.validate(foos) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "String")))))

        o.validate(onFoo) shouldBe
          (Invalid(Seq(Path \ "o" -> Seq(
            ValidationError("error.invalid", "String")))))
      }

      "Traversable" in {
        import testCases.seq._

        at(Path \ "n").is(req[Traversable[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n").is(req[Traversable[String]])
          .validate(mixed) shouldBe
            (Invalid(Seq(Path \ "n" \ 1 -> Seq(
              ValidationError("error.invalid", "String")))))
      }

      "Seq" in {
        import testCases.seq._
        at(Path \ "n").is(req[Seq[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n").is(req[Seq[String]]).validate(mixed) shouldBe
          (Invalid(Seq(Path \ "n" \ 1 -> Seq(
            ValidationError("error.invalid", "String")))))
      }
    }

    "support null" in {
        val jn = at(Path \ "n").is(req[JsNull.type])

        jn.validate(Json.obj("n" -> JsNull)) shouldBe Valid(JsNull)

        jn.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "null"))))

        jn.validate(Json.obj("n" -> 4.5)) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "null"))))
      }

      "JsObject" in {
        at(Path \ "o").is(req[JsObject])
          .validate(Json.obj("o" -> Json.obj("n" -> "foo"))) shouldBe
            Valid(JsObject(Seq("n" -> JsString("foo"))))

        def n = at(Path \ "n").is(req[JsObject])

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
        def n = at(Path \ "n").is(req[JsString])
        n.validate(Json.obj("n" -> "foo")) shouldBe Valid(JsString("foo"))
        n.validate(Json.obj("n" -> 42)) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "String"))))
      }

      "JsNumber" in {
        def n = at(Path \ "n").is(req[JsNumber])
        n.validate(Json.obj("n" -> 4)) shouldBe Valid(JsNumber(4))

        n.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Number"))))

        n.validate(Json.obj("n" -> 4.5)) shouldBe Valid(JsNumber(4.5))
      }

      "JsBoolean" in {
        def n = at(Path \ "n").is(req[JsBoolean])
        n.validate(Json.obj("n" -> true)) shouldBe Valid(JsBoolean(true))
        n.validate(Json.obj("n" -> "foo")) shouldBe
          Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean"))))
      }
  }

}
