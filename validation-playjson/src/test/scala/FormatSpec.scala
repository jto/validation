import jto.validation._
import jto.validation.playjson._
import org.scalatest._
import play.api.libs.json.{JsValue, JsObject, Json, JsArray}
import scala.Function.unlift

class FormatSpec extends WordSpec with Matchers {
  case class User(id: Long, name: String)
  val luigi = User(1, "Luigi")

  "Format" should {
    "serialize and deserialize primitives" in {
      import Rules._
      import Writes._

      val f = Formatting[JsValue, JsObject] { __ =>
        (__ \ "id").format[Long]
      }

      val m = Json.obj("id" -> 1L)

      f.writes(1L) shouldBe (m)
      f.validate(m) shouldBe (Valid(1L))

      (Path \ "id").from[JsValue](f).validate(Json.obj()) shouldBe
      (Invalid(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize String" in {
      import Rules._
      import Writes._

      val f = Formatting[JsValue, JsObject] { __ =>
        (__ \ "id").format[String]
      }

      val m = Json.obj("id" -> "CAFEBABE")

      f.writes("CAFEBABE") shouldBe (m)
      f.validate(m) shouldBe (Valid("CAFEBABE"))

      (Path \ "id").from[JsValue](f).validate(Json.obj()) shouldBe
      (Invalid(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize Seq[String]" in {
      import Rules._
      import Writes._

      val f = Formatting[JsValue, JsObject] { __ =>
        (__ \ "ids").format[Seq[String]]
      }
      val m = Json.obj("ids" -> Seq("CAFEBABE", "FOOBAR"))

      f.validate(m) shouldBe (Valid(Seq("CAFEBABE", "FOOBAR")))
      f.writes(Seq("CAFEBABE", "FOOBAR")) shouldBe (m)
    }

    "serialize and deserialize User case class" in {
      import Rules._
      import Writes._

      implicit val userF = Formatting[JsValue, JsObject] { __ =>
        ((__ \ "id").format[Long] ~ (__ \ "name").format[String])(
            User.apply, unlift(User.unapply))
      }

      val m = Json.obj("id" -> 1L, "name" -> "Luigi")
      userF.validate(m) shouldBe (Valid(luigi))
    }

    "support primitives types" when {
      import Rules._
      import Writes._

      "Int" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Int]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Int]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Int")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Int]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Int")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n" \ "o").format[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> 4))) shouldBe (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n" \ "o").format[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> "foo"))) shouldBe
        (Invalid(Seq(Path \ "n" \ "o" -> Seq(
                        ValidationError("error.number", "Int")))))

        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n" \ "o" \ "p").format[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4)))) shouldBe
        (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n" \ "o" \ "p").format[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo")))) shouldBe
        (Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(
                        ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error =
          Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "foo").format[Int]
        }.validate(Json.obj("n" -> 4)) shouldBe (error)
      }

      "Short" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Short]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Short]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Short")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Short]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Short")))))
      }

      "Long" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Long]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Long]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Long")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Long]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Long")))))
      }

      "Float" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Float]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Float]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Float")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Float]
        }.validate(Json.obj("n" -> 4.5)) shouldBe (Valid(4.5F))
      }

      "Double" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Double]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Double]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Double")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Double]
        }.validate(Json.obj("n" -> 4.5)) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[jBigDecimal]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(new jBigDecimal("4")))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[jBigDecimal]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "BigDecimal")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[jBigDecimal]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Valid(new jBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[BigDecimal]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(BigDecimal(4)))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[BigDecimal]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "BigDecimal")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[BigDecimal]
        }.validate(Json.obj("n" -> 4.5)) shouldBe (Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Boolean]
        }.validate(Json.obj("n" -> true)) shouldBe (Valid(true))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Boolean]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[String]
        }.validate(Json.obj("n" -> "foo")) shouldBe (Valid("foo"))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "o").format[String]
        }.validate(Json.obj("o.n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.required")))))
      }

      "Option" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Option[Boolean]]
        }.validate(Json.obj("n" -> true)) shouldBe (Valid(Some(true)))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Option[Boolean]]
        }.validate(Json.obj()) shouldBe (Valid(None))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Option[Boolean]]
        }.validate(Json.obj("foo" -> "bar")) shouldBe (Valid(None))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Option[Boolean]]
        }.validate(Json.obj("n" -> "bar")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, Seq[V]]" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Map[String, Seq[String]]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> Seq("bar")))) shouldBe
        (Valid(Map("foo" -> Seq("bar"))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Map[String, Seq[Int]]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> Seq(4), "bar" -> Seq(5)))) shouldBe
        (Valid(Map("foo" -> Seq(4), "bar" -> Seq(5))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "x").format[Map[String, Int]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))) shouldBe
        (Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Map[String, Seq[Int]]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> Seq(4),
                                            "bar" -> Seq("frack")))) shouldBe
        (Invalid(Seq(Path \ "n" \ "bar" \ 0 -> Seq(
                        ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Traversable[String]]
        }.validate(Json.obj("n" -> Seq("foo"))).toOption.get.toSeq shouldBe
        (Seq("foo"))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Traversable[Int]]
        }.validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get.toSeq shouldBe
        (Seq(1, 2, 3))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Traversable[Int]]
        }.validate(Json.obj("n" -> Seq("1", "paf"))) shouldBe
        (Invalid(Seq(
                    Path \ "n" \ 0 -> Seq(
                        ValidationError("error.number", "Int")),
                    Path \ "n" \ 1 -> Seq(
                        ValidationError("error.number", "Int"))
                )))
      }

      "Array" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Array[String]]
        }.validate(Json.obj("n" -> Seq("foo"))).toOption.get.toSeq shouldBe
        (Seq("foo"))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Array[Int]]
        }.validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get.toSeq shouldBe
        (Seq(1, 2, 3))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Array[Int]]
        }.validate(Json.obj("n" -> Seq("1", "paf"))) shouldBe
        (Invalid(Seq(
                    Path \ "n" \ 0 -> Seq(
                        ValidationError("error.number", "Int")),
                    Path \ "n" \ 1 -> Seq(
                        ValidationError("error.number", "Int"))
                )))
      }

      "Seq" in {
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Seq[String]]
        }.validate(Json.obj("n" -> Seq("foo"))).toOption.get shouldBe
        (Seq("foo"))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Seq[Int]]
        }.validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get shouldBe
        (Seq(1, 2, 3))
        Formatting[JsValue, JsObject] { __ =>
          (__ \ "n").format[Seq[Int]]
        }.validate(Json.obj("n" -> Seq("1", "paf"))) shouldBe
        (Invalid(Seq(
                    Path \ "n" \ 0 -> Seq(
                        ValidationError("error.number", "Int")),
                    Path \ "n" \ 1 -> Seq(
                        ValidationError("error.number", "Int"))
                )))
      }
    }

    "serialize and deserialize with validation" in {
      import Rules._
      import Writes._

      val f = Formatting[JsValue, JsObject] { __ =>
        ((__ \ "firstname").format(notEmpty) ~ (__ \ "lastname").format(
                notEmpty)).tupled
      }

      val valid = Json.obj("firstname" -> "Julien", "lastname" -> "Tournay")

      val invalid = Json.obj("lastname" -> "Tournay")

      val result = ("Julien", "Tournay")

      f.writes(result) shouldBe (valid)
      f.validate(valid) shouldBe (Valid(result))

      f.validate(invalid) shouldBe
      (Invalid(Seq((Path \ "firstname",
                    Seq(ValidationError("error.required"))))))
    }

    "format seq" in {
      import Rules._
      import Writes._

      val valid =
        Json.obj("firstname" -> Seq("Julien"),
                 "foobar" -> JsArray(),
                 "lastname" -> "Tournay",
                 "age" -> 27,
                 "information" -> Json.obj("label" -> "Personal",
                                           "email" -> "fakecontact@gmail.com",
                                           "phones" -> Seq("01.23.45.67.89",
                                                           "98.76.54.32.10")))

      def isNotEmpty[T <: Traversable[_]] = validateWith[T]("error.notEmpty") {
        !_.isEmpty
      }

      Formatting[JsValue, JsObject] { __ =>
        (__ \ "firstname").format[Seq[String]]
      }.validate(valid) shouldBe (Valid(Seq("Julien")))
      Formatting[JsValue, JsObject] { __ =>
        (__ \ "foobar").format[Seq[String]]
      }.validate(valid) shouldBe (Valid(Seq()))
      Formatting[JsValue, JsObject] { __ =>
        (__ \ "foobar").format(isNotEmpty[Seq[Int]])
      }.validate(valid) shouldBe
      (Invalid(Seq(Path \ "foobar" -> Seq(ValidationError("error.notEmpty")))))
    }

    "format recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      val m = Json.obj(
          "name" -> "bob",
          "friends" -> Seq(Json.obj("name" -> "tom", "friends" -> Json.arr())))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = Json.obj("name" -> "bob", "friend" -> Json.obj("name" -> "tom"))

      "using explicit notation" in {
        import Rules._
        import Writes._

        lazy val w: Format[JsValue, JsObject, RecUser] =
          Formatting[JsValue, JsObject] { __ =>
            ((__ \ "name").format[String] ~ (__ \ "friends").format(
                    seqR(w), seqW(w)))(RecUser.apply, unlift(RecUser.unapply))
          }
        w.validate(m) shouldBe Valid(u)
        w.writes(u) shouldBe m

        lazy val w3: Format[JsValue, JsObject, User1] =
          Formatting[JsValue, JsObject] { __ =>
            ((__ \ "name").format[String] ~ (__ \ "friend").format(
                    optionR(w3),
                    optionW(w3)))(User1.apply, unlift(User1.unapply))
          }
        w3.validate(m1) shouldBe Valid(u1)
        w3.writes(u1) shouldBe m1
      }

      "using implicit notation" in {
        import Rules._
        import Writes._

        implicit lazy val w: Format[JsValue, JsObject, RecUser] =
          Formatting[JsValue, JsObject] { __ =>
            ((__ \ "name").format[String] ~ (__ \ "friends")
                  .format[Seq[RecUser]])(RecUser.apply,
                                         unlift(RecUser.unapply))
          }
        w.validate(m) shouldBe Valid(u)
        w.writes(u) shouldBe m

        implicit lazy val w3: Format[JsValue, JsObject, User1] =
          Formatting[JsValue, JsObject] { __ =>
            ((__ \ "name").format[String] ~ (__ \ "friend")
                  .format[Option[User1]])(User1.apply, unlift(User1.unapply))
          }
        w3.validate(m1) shouldBe Valid(u1)
        w3.writes(u1) shouldBe m1
      }
    }

    "work with Rule ans Write seamlessly" in {
      import Rules._
      import Writes._

      implicit val userF = Formatting[JsValue, JsObject] { __ =>
        ((__ \ "id").format[Long] ~ (__ \ "name").format[String])(
            User.apply, unlift(User.unapply))
      }

      val userJs = Json.obj("id" -> 1L, "name" -> "Luigi")
      userF.validate(userJs) shouldBe (Valid(luigi))
      userF.writes(luigi) shouldBe (userJs)

      val fin = From[JsObject] { __ =>
        (__ \ "user").read[User]
      }

      val m2 = Json.obj("user" -> userJs)
      fin.validate(m2) shouldBe (Valid(luigi))

      val win = To[JsValue] { __ =>
        (__ \ "user").write[User]
      }
      win.writes(luigi) shouldBe (m2)
    }

    "be covarianace in the write type" in {
      trait Animal
      trait Chat extends Animal
      case object MyChat extends Chat

      val f1: Format[Animal, Chat, Unit] =
        Format(Rule(_ => Valid(())), Write(_ => MyChat))
      val f2: Format[Animal, Animal, Unit] = f1

      // Note that we can achieve the above without covarianace on Format IW as follows:
      val f3: Format[Animal, Animal, Unit] =
        Format(Rule.toRule(f1), Write(f1.writes))

      f1.writes(()) shouldBe f2.writes(())
      f1.validate(MyChat) shouldBe f2.validate(MyChat)
    }
  }
}
