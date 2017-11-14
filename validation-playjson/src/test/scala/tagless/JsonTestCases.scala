package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._

object JsonTestCases extends TestCases[JsValue] {

  override val base = new base {

    val id = Json.obj("id" -> "1")

    val info =
      Json.obj("label" -> "Personal",
               "email" -> "fakecontact@gmail.com",
               "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

    val infoNoLabel =
      Json.obj("email" -> "fakecontact@gmail.com",
               "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

    val noInfo = Json.obj()

    val jto = Json.obj(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "informations" -> Seq((Json.obj("label" -> "Personal") ++ info)))

    val valid =
      Json.obj("firstname" -> "Julien",
               "lastname" -> "Tournay",
               "age" -> 27,
               "informations" -> info,
               "contacts" -> Seq(info))

    val invalid =
      Json.obj(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" ->
          (Json.obj("label" -> "") ++ infoNoLabel),
        "contacts" -> Seq(Json.obj("label" -> "") ++ infoNoLabel)
      )

    val smthTrue = Json.obj("issmth" -> true)
    val smthFalse = Json.obj("issmth" -> false)
    val emptyObj = Json.obj()
  }

  val int = new int {
    val ok = Json.obj("n" -> 4)
    val foo = Json.obj("n" -> "foo")
    val float = Json.obj("n" -> 4.5)
    val bigdecimal = Json.obj("n" -> 4.5)
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
    val none = JsObject(Nil)
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
    val foobar = Json.obj("n" -> Json.obj("foo" -> List("bar")))
    val ints = Json.obj("n" -> Json.obj("foo" -> List(4), "bar" -> List(5)))
    val mixed =
      Json.obj("n" -> Json.obj("foo" -> List(4), "bar" -> List("frack")))
  }

  val password = new password {
    val ok =
      Json.obj("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "s3cr3t")

    val empty =
      Json.obj("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "")

    val err =
      Json.obj("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "bam")
  }

  val subclasses = new subclasses {
    val b = Json.obj("name" -> "B", "foo" -> 4)
    val c = Json.obj("name" -> "C", "bar" -> 6)
    val e = Json.obj("name" -> "E", "eee" -> 6)
  }

  val rec = new rec {
    val bobAndFriends =
      Json.obj("name" -> "bob", "friends" -> Seq(Json.obj("name" -> "tom")))

    val bobAndFriend =
      Json.obj("name" -> "bob", "friend" -> Json.obj("name" -> "tom"))
  }

}
