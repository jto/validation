package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._, Json.obj

object JsonTestCases extends TestCases[JsValue] {

  override val base = new base {

    val id = obj("id" -> "1")

    val info =
      obj("label" -> "Personal",
          "email" -> "fakecontact@gmail.com",
          "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

    val infoNoLabel =
      obj("email" -> "fakecontact@gmail.com",
          "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

    val noInfo = obj()

    val jto = obj("firstname" -> "Julien",
                  "lastname" -> "Tournay",
                  "informations" -> Seq((obj("label" -> "Personal") ++ info)))

    val valid =
      obj("firstname" -> "Julien",
          "lastname" -> "Tournay",
          "age" -> 27,
          "informations" -> info,
          "contacts" -> Seq(info))

    val invalid =
      obj(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" ->
          (obj("label" -> "") ++ infoNoLabel),
        "contacts" -> Seq(obj("label" -> "") ++ infoNoLabel)
      )

    val smthTrue = obj("issmth" -> true)
    val smthFalse = obj("issmth" -> false)
    val emptyObj = obj()
  }

  val int = new int {
    val ok = obj("n" -> 4)
    val foo = obj("n" -> "foo")
    val float = obj("n" -> 4.5)
    val bigdecimal = obj("n" -> 4.5)
    val noOK = obj("n" -> obj("o" -> 4))
    val noFoo = obj("n" -> obj("o" -> "foo"))
    val nopOK = obj("n" -> obj("o" -> obj("p" -> 4)))
    val nopFoo = obj("n" -> obj("o" -> obj("p" -> "foo")))
  }

  val boolean = new boolean {
    val ok = obj("n" -> true)
    val foo = int.foo
  }

  val string = new string {
    val foo = int.foo
    val foos = obj("n" -> Seq("foo"))
    val _42 = obj("n" -> 42)
    val onFoo = obj("o" -> obj("n" -> "foo"))
  }

  val option = new option {
    val nNull = obj("n" -> JsNull)
    val fooBar = obj("foo" -> "bar")
    val nBar = obj("n" -> "bar")
    val none = JsObject(Nil)
  }

  val seq = new seq {
    val foos = obj("n" -> Seq("foo"))
    val fooBars = obj("foo" -> Seq("bar"))
    val foofoobars = obj("foo" -> obj("foo" -> Seq("bar")))
    val ns = obj("n" -> Seq("foo", ""))
    val ints = obj("n" -> Seq(1, 2, 3))
    val paf = obj("n" -> "paf")
    val mixed = JsObject(Seq("n" -> JsArray(Seq(JsString("foo"), JsNumber(2)))))
  }

  val map = new map {
    val foobar = obj("n" -> obj("foo" -> List("bar")))
    val ints = obj("n" -> obj("foo" -> List(4), "bar" -> List(5)))
    val mixed =
      obj("n" -> obj("foo" -> List(4), "bar" -> List("frack")))
  }

  val password = new password {
    val ok =
      obj("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "s3cr3t")

    val empty =
      obj("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "")

    val err =
      obj("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "bam")
  }

  val subclasses = new subclasses {
    val b = obj("name" -> "B", "foo" -> 4)
    val c = obj("name" -> "C", "bar" -> 6)
    val e = obj("name" -> "E", "eee" -> 6)
  }

  val rec = new rec {
    val bobAndFriends =
      obj("name" -> "bob", "friends" -> Seq(obj("name" -> "tom")))

    val bobAndFriend =
      obj("name" -> "bob", "friend" -> obj("name" -> "tom"))
  }

}
