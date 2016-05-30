import jto.validation._
import jto.validation.playjson._
import org.scalatest._
import play.api.libs.json.{JsValue, Json, JsArray}

class FormatSpec extends WordSpec with Matchers {
  case class User(id: Long, name: String)
  val luigi = User(1, "Luigi")

  "Format" should {
    "serialize and deserialize primitives" in {
      import Rules._
      import Writes._

      val f = Formatting[JsValue, JsValue] { __ =>
        (__ \ "id").as[Long]
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

      val f = Formatting[JsValue, JsValue] { __ =>
        (__ \ "id").as[String]
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

      val f = Formatting[JsValue, JsValue] { __ =>
        (__ \ "ids").as[Seq[String]]
      }
      val m = Json.obj("ids" -> Seq("CAFEBABE", "FOOBAR"))

      f.validate(m) shouldBe (Valid(Seq("CAFEBABE", "FOOBAR")))
      f.writes(Seq("CAFEBABE", "FOOBAR")) shouldBe (m)
    }

    "serialize and deserialize User case class" in {
      import Rules._
      import Writes._

      implicit val userF: Format[JsValue, JsValue, User] =
        Formatting[JsValue, JsValue] { __ =>
          (
              (__ \ "id").as[Long] ~
              (__ \ "name").as[String]
          )(User.apply _, Function.unlift(User.unapply _))
        }

      val m = Json.obj("id" -> 1L, "name" -> "Luigi")
      userF.validate(m) shouldBe (Valid(luigi))
    }

    "support primitives types" when {
      import Rules._
      import Writes._

      "Int" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Int]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))

        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Int]
        }.validate(Json.obj("n" -> "foo"))
          .shouldBe(
              (Invalid(Seq(Path \ "n" -> Seq(
                          ValidationError("error.number", "Int")))))
          )

        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Int]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Int")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n" \ "o").as[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> 4))) shouldBe (Valid(4))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n" \ "o").as[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> "foo"))) shouldBe
        (Invalid(Seq(Path \ "n" \ "o" -> Seq(
                        ValidationError("error.number", "Int")))))

        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n" \ "o" \ "p").as[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4)))) shouldBe
        (Valid(4))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n" \ "o" \ "p").as[Int]
        }.validate(Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo")))) shouldBe
        (Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(
                        ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error =
          Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "foo").as[Int]
        }.validate(Json.obj("n" -> 4)) shouldBe (error)
      }

      "Short" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Short]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Short]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Short")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Short]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Short")))))
      }

      "Long" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Long]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Long]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Long")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Long]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Long")))))
      }

      "Float" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Float]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Float]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Float")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Float]
        }.validate(Json.obj("n" -> 4.5)) shouldBe (Valid(4.5F))
      }

      "Double" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Double]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(4))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Double]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Double")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Double]
        }.validate(Json.obj("n" -> 4.5)) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[jBigDecimal]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(new jBigDecimal("4")))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[jBigDecimal]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "BigDecimal")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[jBigDecimal]
        }.validate(Json.obj("n" -> 4.5)) shouldBe
        (Valid(new jBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[BigDecimal]
        }.validate(Json.obj("n" -> 4)) shouldBe (Valid(BigDecimal(4)))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[BigDecimal]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "BigDecimal")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[BigDecimal]
        }.validate(Json.obj("n" -> 4.5)) shouldBe (Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Boolean]
        }.validate(Json.obj("n" -> true)) shouldBe (Valid(true))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Boolean]
        }.validate(Json.obj("n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[String]
        }.validate(Json.obj("n" -> "foo")) shouldBe (Valid("foo"))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "o").as[String]
        }.validate(Json.obj("o.n" -> "foo")) shouldBe
        (Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.required")))))
      }

      "Option" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Option[Boolean]]
        }.validate(Json.obj("n" -> true)) shouldBe (Valid(Some(true)))

        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Option[Boolean]]
        }.validate(Json.obj()) shouldBe (Valid(None))

        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Option[Boolean]]
        }.validate(Json.obj("foo" -> "bar")) shouldBe (Valid(None))

        // Behavior changed and on this one :/, gotta revert to something like https://github.com/jto/validation/blob/8555087ea935e8ba3a077e78dc541474d5bf82e8/validation-core/src/main/scala/play/api/data/mapping/DefaultRules.scala#L355-L366)
        // Formatting[JsValue, JsValue] { __ =>
        //   (__ \ "n").as[Option[Boolean]]
        // }.validate(Json.obj("n" -> "bar")) shouldBe
        // (Invalid(Seq(Path \ "n" -> Seq(
        //                 ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, Seq[V]]" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Map[String, Seq[String]]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> Seq("bar")))) shouldBe
        (Valid(Map("foo" -> Seq("bar"))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Map[String, Seq[Int]]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> Seq(4), "bar" -> Seq(5)))) shouldBe
        (Valid(Map("foo" -> Seq(4), "bar" -> Seq(5))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "x").as[Map[String, Int]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))) shouldBe
        (Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Map[String, Seq[Int]]]
        }.validate(Json.obj("n" -> Json.obj("foo" -> Seq(4),
                                            "bar" -> Seq("frack")))) shouldBe
        (Invalid(Seq(Path \ "n" \ "bar" \ 0 -> Seq(
                        ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Traversable[String]]
        }.validate(Json.obj("n" -> Seq("foo"))).toOption.get.toSeq shouldBe
        (Seq("foo"))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Traversable[Int]]
        }.validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get.toSeq shouldBe
        (Seq(1, 2, 3))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Traversable[Int]]
        }.validate(Json.obj("n" -> Seq("1", "paf"))) shouldBe
        (Invalid(Seq(
                    Path \ "n" \ 0 -> Seq(
                        ValidationError("error.number", "Int")),
                    Path \ "n" \ 1 -> Seq(
                        ValidationError("error.number", "Int"))
                )))
      }

      "Array" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Array[String]]
        }.validate(Json.obj("n" -> Seq("foo"))).toOption.get.toSeq shouldBe
        (Seq("foo"))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Array[Int]]
        }.validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get.toSeq shouldBe
        (Seq(1, 2, 3))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Array[Int]]
        }.validate(Json.obj("n" -> Seq("1", "paf"))) shouldBe
        (Invalid(Seq(
                    Path \ "n" \ 0 -> Seq(
                        ValidationError("error.number", "Int")),
                    Path \ "n" \ 1 -> Seq(
                        ValidationError("error.number", "Int"))
                )))
      }

      "Seq" in {
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Seq[String]]
        }.validate(Json.obj("n" -> Seq("foo"))).toOption.get shouldBe
        (Seq("foo"))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Seq[Int]]
        }.validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get shouldBe
        (Seq(1, 2, 3))
        Formatting[JsValue, JsValue] { __ =>
          (__ \ "n").as[Seq[Int]]
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

      val f: Format[JsValue, JsValue, (String, String)] =
        Formatting[JsValue, JsValue] { __ =>
          (
              (__ \ "firstname").as[String](notEmpty, stringW) ~
              (__ \ "lastname").as[String](notEmpty, stringW)
          ).tupled
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

      def isNotEmpty[T <: Traversable[_]](implicit r: Rule[JsValue, T]) =
        validateWith[T]("error.notEmpty")(!_.isEmpty)

      Formatting[JsValue, JsValue] { __ =>
        (__ \ "firstname").as[Seq[String]]
      }.validate(valid) shouldBe (Valid(Seq("Julien")))
      Formatting[JsValue, JsValue] { __ =>
        (__ \ "foobar").as[Seq[String]]
      }.validate(valid) shouldBe (Valid(Seq()))
      // Formatting[JsValue, JsValue] { __ =>
      //   (__ \ "foobar").as(isNotEmpty[Seq[Int]])
      // }.validate(valid) shouldBe
      // (Invalid(Seq(Path \ "foobar" -> Seq(ValidationError("error.notEmpty")))))
    }

    // "format recursive" when {
    //   case class RecUser(name: String, friends: Seq[RecUser] = Nil)
    //   val u = RecUser("bob", Seq(RecUser("tom")))

    //   val m = Json.obj(
    //       "name" -> "bob",
    //       "friends" -> Seq(Json.obj("name" -> "tom", "friends" -> Json.arr())))

    //   case class User1(name: String, friend: Option[User1] = None)
    //   val u1 = User1("bob", Some(User1("tom")))
    //   val m1 = Json.obj("name" -> "bob", "friend" -> Json.obj("name" -> "tom"))

    //   "using explicit notation" in {
    //     import Rules._
    //     import Writes._

    //     lazy val w: Format[JsValue, JsValue, RecUser] =
    //       Formatting[JsValue, JsValue] { __ =>
    //         ((__ \ "name").as[String] ~ (__ \ "friends").as[Seq[RecUser]](
    //                 seqR(w), seqW(w))).unlifted(RecUser.apply, RecUser.unapply)
    //       }
    //     w.validate(m) shouldBe Valid(u)
    //     w.writes(u) shouldBe m

    //     lazy val w3: Format[JsValue, JsValue, User1] =
    //       Formatting[JsValue, JsValue] { __ =>
    //         ((__ \ "name").as[String] ~ (__ \ "friend").as(
    //                 optionR(w3),
    //                 optionW(w3))).unlifted(User1.apply, User1.unapply)
    //       }
    //     w3.validate(m1) shouldBe Valid(u1)
    //     w3.writes(u1) shouldBe m1
    //   }

    //   "using implicit notation" in {
    //     import Rules._
    //     import Writes._

    //     implicit lazy val w: Format[JsValue, JsValue, RecUser] =
    //       Formatting[JsValue, JsValue] { __ =>
    //         ((__ \ "name").as[String] ~ (__ \ "friends")
    //               .as[Seq[RecUser]])
    //           .unlifted(RecUser.apply, RecUser.unapply)
    //       }
    //     w.validate(m) shouldBe Valid(u)
    //     w.writes(u) shouldBe m

    //     implicit lazy val w3: Format[JsValue, JsValue, User1] =
    //       Formatting[JsValue, JsValue] { __ =>
    //         ((__ \ "name").as[String] ~ (__ \ "friend")
    //               .as[Option[User1]]).unlifted(User1.apply, User1.unapply)
    //       }
    //     w3.validate(m1) shouldBe Valid(u1)
    //     w3.writes(u1) shouldBe m1
    //   }
    //   }

    "work with Rule and Write seamlessly" in {
      import Rules._
      import Writes._

      implicit val userF = Formatting[JsValue, JsValue] { __ =>
        ((__ \ "id").as[Long] ~ (__ \ "name").as[String])
          .unlifted(User.apply, User.unapply)
      }

      val userJs = Json.obj("id" -> 1L, "name" -> "Luigi")
      userF.validate(userJs) shouldBe (Valid(luigi))
      userF.writes(luigi) shouldBe (userJs)

      val fin = From[JsValue] { __ =>
        (__ \ "user").as[User]
      }

      val m2 = Json.obj("user" -> userJs)
      fin.validate(m2) shouldBe (Valid(luigi))

      val win = To[JsValue] { __ =>
        (__ \ "user").as[User]
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
