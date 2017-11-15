package jto.validation
package v3.tagless
package forms

import jto.validation.forms._


object FormTestCases extends TestCases[PM.PM] {

  def combine(a1: PM.PM, a2: PM.PM): PM.PM =
    a1 ++ a2

  def lit(ts: (Path, PM.PM)*): PM.PM =
    ts.map { case (p, pm) =>
      pm.map { case (path, s) =>
        (p ++ path, s)
      }
    }.foldLeft[PM.PM](Map.empty)(_ ++ _)

  def arr(ps: PM.PM*): PM.PM =
    ps.zipWithIndex.map { case (pm, i) =>
      val p = Path \ i
      lit((p, pm))
    }.foldLeft[PM.PM](Map.empty)(_ ++ _)

  implicit def stringToPath(s: String): Path = Path \ s
  implicit def stringToPM(s: String): PM.PM = Map(Path -> s)
  implicit def doubleToPM(s: Double): PM.PM = Map(Path -> s.toString)
  implicit def intToPM(s: Int): PM.PM = Map(Path -> s.toString)
  implicit def boolToPM(s: Boolean): PM.PM = Map(Path -> s.toString)

  implicit def fToPM[P, M](t: (P, M))(implicit toPath: P => Path, toPM: M => PM.PM): (Path, PM.PM) =
    (toPath(t._1), toPM(t._2))

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
    val bigdecimal = lit("n" -> "4.5")
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
    val nNull = lit()
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


// object FormTestCases extends TestCases[PM.PM] {

//   override val base = new base {

//     val R = Path

//     def lit(t: (String, String)*) =
//       Map(t.map { case (k, v) => (Path \ k) -> v }:_*)

//     def lit(t: (Path, String)*) =
//       Map(t:_*)

//     def lit(root: Path)(pms: PM.PM*): PM.PM =
//       ???

//     def arr(root: Path)(pms: PM.PM*): PM.PM =
//       ???

//     def arr(root: Path)(ss: String*): PM.PM =
//       ss.zipWithIndex.map { case (s, i) =>
//         (root \ i) -> s
//       }

//     val id = lit("id" -> "1")

//     val info =
//       lit(
//         R \ "label" -> "Personal",
//         R \ "email" -> "fakecontact@gmail.com"
//       ) ++ arr(R \ "phones")("01.23.45.67.89", "98.76.54.32.10")

//     val infoNoLabel =
//       lit(R \ "email" -> "fakecontact@gmail.com") ++
//       arr(R \ "phones")("01.23.45.67.89", "98.76.54.32.10")

//     val noInfo = lit()

//     val jto =
//       lit(
//         R \ "firstname" -> "Julien",
//         R \ "lastname" -> "Tournay"
//       ) ++ arr(R \ "informations") {
//         lit("label" -> "Personal") ++ info
//       }

//     val valid =
//       lit(
//         "firstname" -> "Julien",
//         "lastname" -> "Tournay",
//         "age" -> 27
//       ) ++
//       arr(R \ "informations")(info) ++
//       arr(R \ "contacts")(info) ++
//       lit(R \ "informations")(info) ++
//       arr(R \ "contacts")(info)

//     val invalid =
//       lit(
//         "firstname" -> "Julien",
//         "lastname" -> "Tournay",
//         "age" -> 27
//       ) ++
//       lit(R \ "informations")(lit("label" -> "") ++ infoNoLabel) ++
//       arr(R \ "contacts")(lit("label" -> "") ++ infoNoLabel)

//     val smthTrue = lit("issmth" -> true)
//     val smthFalse = lit("issmth" -> false)
//     val emptyObj = lit()
//   }

//   val int = new int {
//     val ok = lit("n" -> 4)
//     val foo = lit("n" -> "foo")
//     val float = lit("n" -> 4.5)
//     val bigdecimal = lit("n" -> 4.5)
//     val noOK = lit(R \ "n" \ "o" -> 4)
//     val noFoo = lit(R \ "n" \ "o" -> "foo")
//     val nopOK = lit(R \ "n" \ "o" \ "p" -> 4)
//     val nopFoo = lit(R \ "n" \ "o" \ "p" -> "foo")
//   }

//   val boolean = new boolean {
//     val ok = lit("n" -> true)
//     val foo = int.foo
//   }

//   val string = new string {
//     val foo = int.foo
//     val foos = arr("n")("foo")
//     val _42 = lit("n" -> 42)
//     val onFoo = lit(R \ "o" \ "n" -> "foo")
//   }

//   val option = new option {
//     val nNull = lit("n" -> null)
//     val fooBar = lit("foo" -> "bar")
//     val nBar = lit("n" -> "bar")
//     val none = lit()
//   }

//   val seq = new seq {
//     val foos = arr(R \ "n")("foo")
//     val fooBars = arr(R \ "foo")("bar")
//     val foofoobars = arr(R \ "foo" \ "foo")("bar")
//     val ns = arr(R \ "n")("foo", "")
//     val ints = arr(R \ "n")(1, 2, 3)
//     val paf = arr(R \ "n")("paf")
//     private val as = arr(R \ "foo")(2)
//     val mixed = lit("n" -> as)
//   }

//   val map = new map {
//     val foobar = arr(R \ "n" \ "foo")("bar")
//     val ints = lit("n" -> lit("foo" -> arr(4), "bar" -> arr(5)))
//     val mixed =
//       lit("n" -> lit("foo" -> arr(4), "bar" -> arr("frack")))
//   }

//   val password = new password {
//     val ok =
//       lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "s3cr3t")

//     val empty =
//       lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "")

//     val err =
//       lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "bam")
//   }

//   val subclasses = new subclasses {
//     val b = lit("name" -> "B", "foo" -> 4)
//     val c = lit("name" -> "C", "bar" -> 6)
//     val e = lit("name" -> "E", "eee" -> 6)
//   }

//   val rec = new rec {
//     val bobAndFriends =
//       lit("name" -> "bob", "friends" -> arr(lit("name" -> "tom")))

//     val bobAndFriend =
//       lit("name" -> "bob", "friend" -> lit("name" -> "tom"))
//   }

// }
