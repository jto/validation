package jto.validation
package v3.tagless
package jsonast

import jto.validation.jsonast._

object JsonTestCases extends TestCases[JValue] {

  def combine(a1: JObject, a2: JObject): JObject =
    jto.validation.jsonast.Writes.jsonMonoid.combine(a1, a2)

  def lit(t: (String, JValue)*): JObject = JObject(Map(t:_*))
  def arr(j: JValue*): JValue = JArray(j.toSeq)
  implicit def str(s: String): JValue = JString(s)
  implicit def numI(s: Int): JValue = JNumber(s)
  implicit def numD(s: Double): JValue = JNumber(s)
  implicit def bool(s: Boolean): JValue = JBoolean(s)


  override val base = new base {

    val id = lit("id" -> "1")

    val info =
      lit("label" -> "Personal",
          "email" -> "fakecontact@gmail.com",
          "phones" -> arr("01.23.45.67.89", "98.76.54.32.10"))

    val infoNoLabel =
      lit("email" -> "fakecontact@gmail.com",
          "phones" -> arr("01.23.45.67.89", "98.76.54.32.10"))

    val noInfo = lit()

    val jto = lit("firstname" -> "Julien",
                  "lastname" -> "Tournay",
                  "informations" -> arr(
                    combine(lit("label" -> "Personal"), info)
                  ))

    val valid =
      lit("firstname" -> "Julien",
          "lastname" -> "Tournay",
          "age" -> 27,
          "informations" -> info,
          "contacts" -> arr(info))

    val invalid =
      lit(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" ->
          combine(lit("label" -> ""), infoNoLabel),
        "contacts" -> arr(combine(lit("label" -> ""), infoNoLabel))
      )

    val smthTrue = lit("issmth" -> true)
    val smthFalse = lit("issmth" -> false)
    val emptyObj = lit()
  }

  val int = new int {
    val ok = lit("n" -> 4)
    val foo = lit("n" -> "foo")
    val float = lit("n" -> 4.5)
    val bigdecimal = lit("n" -> 4.5)
    val noOK = lit("n" -> lit("o" -> 4))
    val noFoo = lit("n" -> lit("o" -> "foo"))
    val nopOK = lit("n" -> lit("o" -> lit("p" -> 4)))
    val nopFoo = lit("n" -> lit("o" -> lit("p" -> "foo")))
  }

  val boolean = new boolean {
    val ok = lit("n" -> true)
    val foo = int.foo
  }

  val string = new string {
    val foo = int.foo
    val foos = lit("n" -> arr("foo"))
    val _42 = lit("n" -> 42)
    val onFoo = lit("o" -> lit("n" -> "foo"))
  }

  val option = new option {
    val nNull = lit("n" -> null)
    val fooBar = lit("foo" -> "bar")
    val nBar = lit("n" -> "bar")
    val none = lit()
  }

  val seq = new seq {
    val foos = lit("n" -> arr("foo"))
    val fooBars = lit("foo" -> arr("bar"))
    val foofoobars = lit("foo" -> lit("foo" -> arr("bar")))
    val ns = lit("n" -> arr("foo", ""))
    val ints = lit("n" -> arr(1, 2, 3))
    val paf = lit("n" -> "paf")
    private val as = arr("foo", 2)
    val mixed = lit("n" -> as)
  }

  val map = new map {
    val foobar = lit("n" -> lit("foo" -> arr("bar")))
    val ints = lit("n" -> lit("foo" -> arr(4), "bar" -> arr(5)))
    val mixed =
      lit("n" -> lit("foo" -> arr(4), "bar" -> arr("frack")))
  }

  val password = new password {
    val ok =
      lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "s3cr3t")

    val empty =
      lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "")

    val err =
      lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "bam")
  }

  val subclasses = new subclasses {
    val b = lit("name" -> "B", "foo" -> 4)
    val c = lit("name" -> "C", "bar" -> 6)
    val e = lit("name" -> "E", "eee" -> 6)
  }

  val rec = new rec {
    val bobAndFriends =
      lit("name" -> "bob", "friends" -> arr(lit("name" -> "tom")))

    val bobAndFriend =
      lit("name" -> "bob", "friend" -> lit("name" -> "tom"))
  }

}
