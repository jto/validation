package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._

object JsonTestCases extends TestCases[JsValue] {

  override val base = new base {
    val valid =
      Json.obj("firstname" -> "Julien",
               "lastname" -> "Tournay",
               "age" -> 27,
               "informations" ->
                  Json.obj("label" -> "Personal",
                    "email" -> "fakecontact@gmail.com",
                    "phones" -> Seq("01.23.45.67.89",
                                    "98.76.54.32.10")),
               "contacts" -> Seq(
                  Json.obj("label" -> "Personal",
                    "email" -> "fakecontact@gmail.com",
                     "phones" -> Seq("01.23.45.67.89",
                                     "98.76.54.32.10"))))

    val invalid =
      Json.obj("firstname" -> "Julien",
               "lastname" -> "Tournay",
               "age" -> 27,
               "informations" ->
                  Json.obj("label" -> "",
                    "email" -> "fakecontact@gmail.com",
                    "phones" -> Seq("01.23.45.67.89",
                                    "98.76.54.32.10")),
               "contacts" -> Seq(
                  Json.obj("label" -> "",
                    "email" -> "fakecontact@gmail.com",
                    "phones" -> Seq("01.23.45.67.89",
                                    "98.76.54.32.10"))))
  }

  val int = new int {
    val ok = Json.obj("n" -> 4)
    val foo = Json.obj("n" -> "foo")
    val float = Json.obj("n" -> 4.5)
    val noOK = Json.obj("n" -> Json.obj("o" -> 4))
    val noFoo = Json.obj("n" -> Json.obj("o" -> "foo"))
    val nopOK = Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4)))
    val nopFoo = Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo")))
  }

  val boolean = new boolean {
    val ok = Json.obj("n" -> true)
    val foo = int.foo
  }

  val string = new string {
    val foo = int.foo
    val foos = Json.obj("n" -> Seq("foo"))
    val _42 = Json.obj("n" -> 42)
    val onFoo = Json.obj("o" -> Json.obj("n" -> "foo"))
  }

  val option = new option {
    val nNull = Json.obj("n" -> JsNull)
    val fooBar = Json.obj("foo" -> "bar")
    val nBar = Json.obj("n" -> "bar")
  }

  val seq = new seq {
    val foos = Json.obj("n" -> Seq("foo"))
    val fooBars = Json.obj("foo" -> Seq("bar"))
    val foofoobars = Json.obj("foo" -> Json.obj("foo" -> Seq("bar")))
    val ns = Json.obj("n" -> Seq("foo", ""))
    val ints = Json.obj("n" -> Seq(1, 2, 3))
    val paf = Json.obj("n" -> "paf")
    val mixed = JsObject(Seq("n" -> JsArray(Seq(JsString("foo"), JsNumber(2)))))
  }

  val map = new map {
    val foobar = Json.obj("n" -> Json.obj("foo" -> "bar"))
    val ints = Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> 5))
    val mixed = Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))
  }

  val password = new password {
    val ok = Json.obj("login" -> "Alice",
                     "password" -> "s3cr3t",
                     "verify" -> "s3cr3t")

    val empty = Json.obj("login" -> "Alice",
                      "password" -> "s3cr3t",
                      "verify" -> "")

    val err = Json.obj("login" -> "Alice",
                      "password" -> "s3cr3t",
                      "verify" -> "bam")
  }

  val subclasses = new subclasses {
    val b = Json.obj("name" -> "B", "foo" -> 4)
    val c = Json.obj("name" -> "C", "bar" -> 6)
    val e = Json.obj("name" -> "E", "eee" -> 6)
  }

  val rec = new rec {
    val bobAndFriends =
      Json.obj("name" -> "bob",
               "friends" -> Seq(
                   Json.obj("name" -> "tom", "friends" -> Seq[JsObject]())))

    val bobAndFriend =
      Json.obj(
        "name" -> "bob",
        "friend" -> Json.obj("name" -> "tom"))
  }

}

class JsonRulesSpec extends RulesSpec[JsValue] {
  implicit val grammar = RulesGrammar
  val testCases = JsonTestCases

   //     "null" in {
  //       (Path \ "n")
  //         .read[JsValue, JsNull.type]
  //         .validate(Json.obj("n" -> JsNull)) shouldBe (Valid(JsNull))
  //       (Path \ "n")
  //         .read[JsValue, JsNull.type]
  //         .validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "null")))))
  //       (Path \ "n").read[JsValue, JsNull.type].validate(Json.obj("n" -> 4.5)) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "null")))))
  //     }

  //     "JsObject" in {
  //       (Path \ "o")
  //         .read[JsValue, JsObject]
  //         .validate(Json.obj("o" -> Json.obj("n" -> "foo"))) shouldBe
  //       (Valid(JsObject(Seq("n" -> JsString("foo")))))
  //       (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> 42)) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Object")))))
  //       (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Object")))))
  //       (Path \ "n")
  //         .read[JsValue, JsObject]
  //         .validate(Json.obj("n" -> Seq("foo"))) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Object")))))
  //     }

  //     "JsString" in {
  //       (Path \ "n").read[JsValue, JsString].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Valid(JsString("foo")))
  //       (Path \ "n").read[JsValue, JsString].validate(Json.obj("n" -> 42)) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "String")))))
  //     }

  //     "JsNumber" in {
  //       (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> 4)) shouldBe
  //       (Valid(JsNumber(4)))
  //       (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.number", "Number")))))
  //       (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> 4.5)) shouldBe
  //       (Valid(JsNumber(4.5)))
  //     }

  //     "JsBoolean" in {
  //       (Path \ "n").read[JsValue, JsBoolean].validate(Json.obj("n" -> true)) shouldBe
  //       (Valid(JsBoolean(true)))
  //       (Path \ "n").read[JsValue, JsBoolean].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Boolean")))))
  //     }
}
