import jto.validation._
import jto.validation.jsjson.Rules._
import jto.validation.jsjson.Writes._
import org.scalatest._
import scala.scalajs.js

class FormatSpec extends WordSpec with Matchers with JsAnyEquality {

  case class User(id: Long, name: String)
  val luigi = User(1, "Luigi")

  "Format" should {
    
    "serialize and deserialize primitives" in {
      val f = Formatting[js.Dynamic, js.Dynamic] { __ =>
        (__ \ "id").format[Long]
      }

      val m = js.Dynamic.literal("id" -> 1L)

      f.writes(1L) shouldBe m 
      f.validate(m) shouldBe(Valid(1L))

      (Path \ "id").from[js.Dynamic](f).validate(js.Dynamic.literal()) shouldBe(Invalid(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize String" in {
      val f = Formatting[js.Dynamic, js.Dynamic] { __ =>
        (__ \ "id").format[String]
      }

      val m = js.Dynamic.literal("id" -> "CAFEBABE")

      f.writes("CAFEBABE") shouldBe m
      f.validate(m) shouldBe(Valid("CAFEBABE"))

      (Path \ "id").from[js.Dynamic](f).validate(js.Dynamic.literal()) shouldBe(Invalid(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize Seq[String]" in {
      val f = Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "ids").format[Seq[String]] }
      val m = js.Dynamic.literal("ids" -> js.Array("CAFEBABE", "FOOBAR"))

      f.validate(m) shouldBe(Valid(Seq("CAFEBABE", "FOOBAR")))
      f.writes(Seq("CAFEBABE", "FOOBAR")) shouldBe m
    }

    "serialize and deserialize User case class" in {
      implicit val userF = Formatting[js.Dynamic, js.Dynamic] { __ =>
        ((__ \ "id").format[Long] ~
         (__ \ "name").format[String])(User.apply, User.unapply)
      }

      val m = js.Dynamic.literal("id" -> 1L, "name" -> "Luigi")
      userF.validate(m) shouldBe(Valid(luigi))
    }

    "support primitives types" when {
      "Int" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Int] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Int] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Int] }.validate(js.Dynamic.literal("n" -> 4.5)) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n" \ "o").format[Int] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> 4))) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n" \ "o").format[Int] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> "foo"))) shouldBe(Invalid(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n" \ "o" \ "p").format[Int] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> js.Dynamic.literal("p" -> 4)))) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n" \ "o" \ "p").format[Int] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> js.Dynamic.literal("p" -> "foo")))) shouldBe(Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "foo").format[Int] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(error)
      }

      "Short" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Short] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Short] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Short] }.validate(js.Dynamic.literal("n" -> 4.5)) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Long] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Long] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Long] }.validate(js.Dynamic.literal("n" -> 4.5)) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Float] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Float] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Float] }.validate(js.Dynamic.literal("n" -> 4.5)) shouldBe(Valid(4.5F))
      }

      "Double" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Double] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(Valid(4))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Double] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Double] }.validate(js.Dynamic.literal("n" -> 4.5)) shouldBe(Valid(4.5))
      }

      "scala BigDecimal" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[BigDecimal] }.validate(js.Dynamic.literal("n" -> 4)) shouldBe(Valid(BigDecimal(4)))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[BigDecimal] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[BigDecimal] }.validate(js.Dynamic.literal("n" -> 4.5)) shouldBe(Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Boolean] }.validate(js.Dynamic.literal("n" -> true)) shouldBe(Valid(true))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Boolean] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[String] }.validate(js.Dynamic.literal("n" -> "foo")) shouldBe(Valid("foo"))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "o").format[String] }.validate(js.Dynamic.literal("o.n" -> "foo")) shouldBe(Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.required")))))
      }

      "Option" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Option[Boolean]] }.validate(js.Dynamic.literal("n" -> true)) shouldBe(Valid(Some(true)))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Option[Boolean]] }.validate(js.Dynamic.literal()) shouldBe(Valid(None))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Option[Boolean]] }.validate(js.Dynamic.literal("foo" -> "bar")) shouldBe(Valid(None))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Option[Boolean]] }.validate(js.Dynamic.literal("n" -> "bar")) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, Seq[V]]" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Map[String, Seq[String]]] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("foo" -> js.Array("bar")))) shouldBe(Valid(Map("foo" -> Seq("bar"))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Map[String, Seq[Int]]] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("foo" -> js.Array(4), "bar" -> js.Array(5)))) shouldBe(Valid(Map("foo" -> Seq(4), "bar" -> Seq(5))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "x").format[Map[String, Int]] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("foo" -> 4, "bar" -> "frack"))) shouldBe(Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Map[String, Seq[Int]]] }.validate(js.Dynamic.literal("n" -> js.Dynamic.literal("foo" -> js.Array(4), "bar" -> js.Array("frack")))) shouldBe(Invalid(Seq(Path \ "n" \ "bar" \ 0 -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Traversable[String]] }.validate(js.Dynamic.literal("n" -> js.Array("foo"))).toOption.get.toSeq shouldBe(Seq("foo"))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Traversable[Int]] }.validate(js.Dynamic.literal("n" -> js.Array(1, 2, 3))).toOption.get.toSeq shouldBe(Seq(1, 2, 3))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Traversable[Int]] }.validate(js.Dynamic.literal("n" -> js.Array("1", "paf"))) shouldBe(Invalid(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }

      "Array" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Array[String]] }.validate(js.Dynamic.literal("n" -> js.Array("foo"))).toOption.get.toSeq shouldBe(Seq("foo"))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Array[Int]] }.validate(js.Dynamic.literal("n" -> js.Array(1, 2, 3))).toOption.get.toSeq shouldBe(Seq(1, 2, 3))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Array[Int]] }.validate(js.Dynamic.literal("n" -> js.Array("1", "paf"))) shouldBe(Invalid(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }

      "Seq" in {
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Seq[String]] }.validate(js.Dynamic.literal("n" -> js.Array("foo"))).toOption.get shouldBe(Seq("foo"))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Seq[Int]] }.validate(js.Dynamic.literal("n" -> js.Array(1, 2, 3))).toOption.get shouldBe(Seq(1, 2, 3))
        Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "n").format[Seq[Int]] }.validate(js.Dynamic.literal("n" -> js.Array("1", "paf"))) shouldBe(Invalid(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }
    }

    "serialize and deserialize with validation" in {
      val f = Formatting[js.Dynamic, js.Dynamic] { __ =>
        ((__ \ "firstname").format(notEmpty) ~
         (__ \ "lastname").format(notEmpty)).tupled
      }

      val valid = js.Dynamic.literal(
        "firstname" -> "Julien",
        "lastname" -> "Tournay")

      val invalid = js.Dynamic.literal("lastname" -> "Tournay")

      val result = ("Julien", "Tournay")

      f.writes(result) shouldBe(valid)
      f.validate(valid) shouldBe(Valid(result))

      f.validate(invalid) shouldBe(Invalid(Seq((Path \ "firstname", Seq(ValidationError("error.required"))))))
    }

    "format seq" in {
      val valid = js.Dynamic.literal(
      "firstname" -> js.Array("Julien"),
      "foobar" -> js.Array(),
      "lastname" -> "Tournay",
      "age" -> 27,
      "information" -> js.Dynamic.literal(
        "label" -> "Personal",
        "email" -> "fakecontact@gmail.com",
        "phones" -> js.Array("01.23.45.67.89", "98.76.54.32.10")))

      def isNotEmpty[T <: Traversable[_]] = validateWith[T]("error.notEmpty"){ !_.isEmpty }

      Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "firstname").format[Seq[String]] }.validate(valid) shouldBe(Valid(Seq("Julien")))
      Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "foobar").format[Seq[String]] }.validate(valid) shouldBe(Valid(Seq()))
      Formatting[js.Dynamic, js.Dynamic] { __ => (__ \ "foobar").format(isNotEmpty[Seq[Int]]) }.validate(valid) shouldBe(Invalid(Seq(Path \ "foobar" -> Seq(ValidationError("error.notEmpty")))))
    }

    "format recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser(
        "bob",
        Seq(RecUser("tom")))

      val m = js.Dynamic.literal(
        "name" -> "bob",
        "friends" -> js.Array(js.Dynamic.literal("name" -> "tom", "friends" -> js.Array())))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = js.Dynamic.literal(
        "name" -> "bob",
        "friend" -> js.Dynamic.literal("name" -> "tom"))

      "using explicit notation" in {
        lazy val w: Format[js.Dynamic, js.Dynamic, RecUser] = Formatting[js.Dynamic, js.Dynamic]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friends").format(seqR(w), seqW(w)))(RecUser.apply, RecUser.unapply)
        }
        w.validate(m) shouldBe Valid(u)
        w.writes(u) shouldBe m

        lazy val w3: Format[js.Dynamic, js.Dynamic, User1] = Formatting[js.Dynamic, js.Dynamic]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friend").format(optionR(w3), optionW(w3)))(User1.apply, User1.unapply)
        }
        w3.validate(m1) shouldBe Valid(u1)
        w3.writes(u1) shouldBe m1
      }

      "using implicit notation" in {
        implicit lazy val w: Format[js.Dynamic, js.Dynamic, RecUser] = Formatting[js.Dynamic, js.Dynamic] { __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friends").format[Seq[RecUser]])(RecUser.apply, RecUser.unapply)
        }
        w.validate(m) shouldBe Valid(u)
        w.writes(u) shouldBe m

        implicit lazy val w3: Format[js.Dynamic, js.Dynamic, User1] = Formatting[js.Dynamic, js.Dynamic] { __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friend").format[Option[User1]])(User1.apply, User1.unapply)
        }
        w3.validate(m1) shouldBe Valid(u1)
        w3.writes(u1) shouldBe m1
      }
    }

    "work with Rule ans Write seamlessly" in {
      implicit val userF = Formatting[js.Dynamic, js.Dynamic] { __ =>
        ((__ \ "id").format[Long] ~
         (__ \ "name").format[String])(User.apply, User.unapply)
      }

      val  userJs = js.Dynamic.literal("id" -> 1L, "name" -> "Luigi")
      userF.validate(userJs) shouldBe(Valid(luigi))
      userF.writes(luigi) shouldBe(userJs)

      val fin = From[js.Dynamic] { __ =>
        (__ \ "user").read[User]
      }

      val m2 = js.Dynamic.literal("user" -> userJs)
      fin.validate(m2) shouldBe(Valid(luigi))

      val win = To[js.Dynamic] { __ =>
        (__ \ "user").write[User]
      }
      win.writes(luigi) shouldBe(m2)
    }
  }
}
