import jto.validation._
import jto.validation.playjson.Writes._
import org.scalatest._
import play.api.libs.json.{JsValue, Json, JsString}

class WritesSpec extends WordSpec with Matchers {

  case class Contact(firstname: String,
                     lastname: String,
                     company: Option[String],
                     informations: Seq[ContactInformation])

  case class ContactInformation(
      label: String, email: Option[String], phones: Seq[String])

  val contact = Contact(
      "Julien",
      "Tournay",
      None,
      Seq(
          ContactInformation("Personal",
                             Some("fakecontact@gmail.com"),
                             Seq("01.23.45.67.89", "98.76.54.32.10"))))

  val contactJson = Json.obj(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "informations" -> Seq(
          Json.obj("label" -> "Personal",
                   "email" -> "fakecontact@gmail.com",
                   "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))))

  "Writes" should {

    "write string" in {
      val w = (Path \ "label").write[String, JsValue]
      w.writes("Hello World") shouldBe Json.obj("label" -> "Hello World")
    }

    "write option" in {
      val w = (Path \ "email").write[Option[String], JsValue]
      w.writes(Some("Hello World")) shouldBe Json.obj("email" -> "Hello World")
      w.writes(None) shouldBe Json.obj()

      (Path \ "n").write(optionW(intW)).writes(Some(5)) shouldBe Json.obj(
          "n" -> 5)
      (Path \ "n").write(optionW(intW)).writes(None) shouldBe Json.obj()
    }

    "write seq" in {
      val w = (Path \ "phones").write[Seq[String], JsValue]
      w.writes(Seq("01.23.45.67.89", "98.76.54.32.10")) shouldBe Json.obj(
          "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))
      w.writes(Nil) shouldBe Json.obj("phones" -> Seq[String]())
    }

    "support primitives types" when {
      "Int" in {
        (Path \ "n").write[Int, JsValue].writes(4) shouldBe
        (Json.obj("n" -> 4))
        (Path \ "n" \ "o").write[Int, JsValue].writes(4) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4)))
        (Path \ "n" \ "o" \ "p").write[Int, JsValue].writes(4) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4))))
      }

      "Short" in {
        (Path \ "n").write[Short, JsValue].writes(4) shouldBe
        (Json.obj("n" -> 4))
        (Path \ "n" \ "o").write[Short, JsValue].writes(4) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4)))
        (Path \ "n" \ "o" \ "p").write[Short, JsValue].writes(4) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4))))
      }

      "Long" in {
        (Path \ "n").write[Long, JsValue].writes(4) shouldBe
        (Json.obj("n" -> 4))
        (Path \ "n" \ "o").write[Long, JsValue].writes(4) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4)))
        (Path \ "n" \ "o" \ "p").write[Long, JsValue].writes(4) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4))))
      }

      "Float" in {
        (Path \ "n").write[Float, JsValue].writes(4.5f) shouldBe
        (Json.obj("n" -> 4.5))
        (Path \ "n" \ "o").write[Float, JsValue].writes(4.5f) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4.5)))
        (Path \ "n" \ "o" \ "p").write[Float, JsValue].writes(4.5f) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4.5))))
      }

      "Double" in {
        (Path \ "n").write[Double, JsValue].writes(4d) shouldBe
        (Json.obj("n" -> 4.0))
        (Path \ "n" \ "o").write[Double, JsValue].writes(4.5d) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4.5)))
        (Path \ "n" \ "o" \ "p").write[Double, JsValue].writes(4.5d) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4.5))))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        (Path \ "n").write[jBigDecimal, JsValue].writes(new jBigDecimal("4.0")) shouldBe (Json
              .obj("n" -> 4.0))
        (Path \ "n" \ "o")
          .write[jBigDecimal, JsValue]
          .writes(new jBigDecimal("4.5")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4.5)))
        (Path \ "n" \ "o" \ "p")
          .write[jBigDecimal, JsValue]
          .writes(new jBigDecimal("4.5")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4.5))))
      }

      "scala BigDecimal" in {
        (Path \ "n").write[BigDecimal, JsValue].writes(BigDecimal("4.0")) shouldBe
        (Json.obj("n" -> 4.0))
        (Path \ "n" \ "o").write[BigDecimal, JsValue].writes(BigDecimal("4.5")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> 4.5)))
        (Path \ "n" \ "o" \ "p")
          .write[BigDecimal, JsValue]
          .writes(BigDecimal("4.5")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4.5))))
      }

      "Boolean" in {
        (Path \ "n").write[Boolean, JsValue].writes(true) shouldBe
        (Json.obj("n" -> true))
        (Path \ "n" \ "o").write[Boolean, JsValue].writes(false) shouldBe
        (Json.obj("n" -> Json.obj("o" -> false)))
        (Path \ "n" \ "o" \ "p").write[Boolean, JsValue].writes(true) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> true))))
      }

      "String" in {
        (Path \ "n").write[String, JsValue].writes("foo") shouldBe
        (Json.obj("n" -> "foo"))
        (Path \ "n" \ "o").write[String, JsValue].writes("foo") shouldBe
        (Json.obj("n" -> Json.obj("o" -> "foo")))
        (Path \ "n" \ "o" \ "p").write[String, JsValue].writes("foo") shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo"))))
      }

      "Option" in {
        (Path \ "n").write[Option[String], JsValue].writes(Some("foo")) shouldBe
        (Json.obj("n" -> "foo"))
        (Path \ "n" \ "o").write[Option[String], JsValue].writes(Some("foo")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> "foo")))
        (Path \ "n" \ "o" \ "p")
          .write[Option[String], JsValue]
          .writes(Some("foo")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo"))))

        (Path \ "n").write[Option[String], JsValue].writes(None) shouldBe
        (Json.obj())
        (Path \ "n" \ "o").write[Option[String], JsValue].writes(None) shouldBe
        (Json.obj())
        (Path \ "n" \ "o" \ "p").write[Option[String], JsValue].writes(None) shouldBe
        (Json.obj())
      }

      "Map[String, Seq[V]]" in {
        (Path \ "n")
          .write[Map[String, Seq[String]], JsValue]
          .writes(Map("foo" -> Seq("bar"))) shouldBe
        (Json.obj("n" -> Json.obj("foo" -> Seq("bar"))))
        (Path \ "n")
          .write[Map[String, Seq[Int]], JsValue]
          .writes(Map("foo" -> Seq(4))) shouldBe
        (Json.obj("n" -> Json.obj("foo" -> Seq(4))))
        (Path \ "n" \ "o")
          .write[Map[String, Seq[Int]], JsValue]
          .writes(Map("foo" -> Seq(4))) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("foo" -> Seq(4)))))
        (Path \ "n" \ "o")
          .write[Map[String, Int], JsValue]
          .writes(Map("foo" -> 4)) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("foo" -> 4))))
        (Path \ "n" \ "o").write[Map[String, Int], JsValue].writes(Map.empty) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj())))
      }

      "Traversable" in {
        (Path \ "n")
          .write[Traversable[String], JsValue]
          .writes(Array("foo", "bar")) shouldBe
        (Json.obj("n" -> Seq("foo", "bar")))
        (Path \ "n" \ "o")
          .write[Traversable[String], JsValue]
          .writes(Array("foo", "bar")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Seq("foo", "bar"))))
        (Path \ "n" \ "o" \ "p")
          .write[Traversable[String], JsValue]
          .writes(Array("foo", "bar")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq("foo", "bar")))))

        (Path \ "n")
          .write[Traversable[String], JsValue]
          .writes(Array[String]()) shouldBe (Json.obj("n" -> Seq[String]()))
        (Path \ "n" \ "o")
          .write[Traversable[String], JsValue]
          .writes(Array[String]()) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Seq[String]())))
        (Path \ "n" \ "o" \ "p")
          .write[Traversable[String], JsValue]
          .writes(Array[String]()) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq[String]()))))
      }

      "Array" in {
        (Path \ "n").write[Array[String], JsValue].writes(Array("foo", "bar")) shouldBe
        (Json.obj("n" -> Seq("foo", "bar")))
        (Path \ "n" \ "o")
          .write[Array[String], JsValue]
          .writes(Array("foo", "bar")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Seq("foo", "bar"))))
        (Path \ "n" \ "o" \ "p")
          .write[Array[String], JsValue]
          .writes(Array("foo", "bar")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq("foo", "bar")))))

        (Path \ "n").write[Array[String], JsValue].writes(Array()) shouldBe
        (Json.obj("n" -> Seq[String]()))
        (Path \ "n" \ "o").write[Array[String], JsValue].writes(Array()) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Seq[String]())))
        (Path \ "n" \ "o" \ "p").write[Array[String], JsValue].writes(Array()) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq[String]()))))
      }

      "Seq" in {
        (Path \ "n").write[Seq[String], JsValue].writes(Seq("foo", "bar")) shouldBe
        (Json.obj("n" -> Seq("foo", "bar")))
        (Path \ "n" \ "o")
          .write[Seq[String], JsValue]
          .writes(Seq("foo", "bar")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Seq("foo", "bar"))))
        (Path \ "n" \ "o" \ "p")
          .write[Seq[String], JsValue]
          .writes(Seq("foo", "bar")) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq("foo", "bar")))))

        (Path \ "n").write[Seq[String], JsValue].writes(Nil) shouldBe
        (Json.obj("n" -> Seq[String]()))
        (Path \ "n" \ "o").write[Seq[String], JsValue].writes(Nil) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Seq[String]())))
        (Path \ "n" \ "o" \ "p").write[Seq[String], JsValue].writes(Nil) shouldBe
        (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq[String]()))))
      }
    }

    "compose" in {
      val w: Write[(Option[String], Seq[String]), JsValue] = To[JsValue] {
        __ =>
          ((__ \ "email").as[Option[String]] ~ (__ \ "phones").as[Seq[String]]).tupled
      }

      val v = Some("jto@foobar.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")

      w.writes(v) shouldBe Json.obj("email" -> "jto@foobar.com",
                                    "phones" -> Seq("01.23.45.67.89",
                                                    "98.76.54.32.10"))
      w.writes(Some("jto@foobar.com") -> Nil) shouldBe Json.obj(
          "email" -> "jto@foobar.com", "phones" -> Seq[String]())
      w.writes(None -> Nil) shouldBe Json.obj("phones" -> Seq[String]())
    }

    // "write Invalid" in {
    //   val f = Invalid[(Path, Seq[ValidationError]), String](Seq(Path \ "n" -> Seq(ValidationError("validation.type-mismatch", "Int"))))

    //   implicitly[Write[(Path, Seq[ValidationError]), JsValue]]
    //   implicitly[Write[Invalid[(Path, Seq[ValidationError]), String], JsValue]]

    //   val error =
    //     Json.obj("errors" ->
    //       Json.obj("/n" -> Json.arr(
    //         Json.obj(
    //           "msg" -> "validation.type-mismatch",
    //           "args" -> Seq("Int")))))

    //   (Path \ "errors").as[Invalid[(Path, Seq[ValidationError]), String], JsValue]
    //     .writes(f) shouldBe(error)
    // }

    "write Map" in {
      implicit val contactInformation = To[JsValue] { __ =>
        ((__ \ "label").as[String] ~ (__ \ "email").as[Option[String]] ~
            (__ \ "phones").as[Seq[String]])
          .unlifted(ContactInformation.unapply)
      }

      implicit val contactWrite = To[JsValue] { __ =>
        ((__ \ "firstname").as[String] ~ (__ \ "lastname").as[String] ~
            (__ \ "company").as[Option[String]] ~ (__ \ "informations")
              .as[Seq[ContactInformation]]).unlifted(Contact.unapply)
      }

      contactWrite.writes(contact) shouldBe contactJson
    }

    "write recursive" when {
      case class RecUser(name: String, friends: List[RecUser] = Nil)
      val u = RecUser("bob", List(RecUser("tom")))

      val m =
        Json.obj("name" -> "bob",
                 "friends" -> Seq(
                     Json.obj("name" -> "tom", "friends" -> Seq[JsValue]())))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = Json.obj("name" -> "bob", "friend" -> Json.obj("name" -> "tom"))

      // "using explicit notation" in {
      //   lazy val w: Write[RecUser, JsValue] = To[JsValue] { __ =>
      //     ((__ \ "name").as[String] ~ (__ \ "friends").as(seqW(w)))
      //       .unlifted(RecUser.unapply)
      //   }
      //   w.writes(u) shouldBe m

      //   lazy val w2: Write[RecUser, JsValue] =
      //     ((Path \ "name").write[String, JsValue] ~ (Path \ "friends").write(
      //             seqW(w2))).unlifted(RecUser.unapply)
      //   w2.writes(u) shouldBe m

      //   lazy val w3: Write[User1, JsValue] = To[JsValue] { __ =>
      //     ((__ \ "name").as[String] ~ (__ \ "friend").as(optionW(w3)))
      //       .unlifted(User1.unapply)
      //   }
      //   w3.writes(u1) shouldBe m1
      // }

      // "using implicit notation" in {
      //   implicit lazy val w: Write[RecUser, JsValue] = To[JsValue] { __ =>
      //     ((__ \ "name").as[String] ~ (__ \ "friends").as[Seq[RecUser]])
      //       .unlifted(RecUser.unapply)
      //   }
      //   w.writes(u) shouldBe m

      //   implicit lazy val w3: Write[User1, JsValue] = To[JsValue] { __ =>
      //     ((__ \ "name").as[String] ~ (__ \ "friend").as[Option[User1]])
      //       .unlifted(User1.unapply)
      //   }
      //   w3.writes(u1) shouldBe m1
      // }
    }

    "support write of value class" in {
      import TestValueClass._

      val w = To[JsValue] { __ =>
        (__ \ "id").as[Id]
      }

      w.writes(Id("1")) shouldBe Json.obj("id" -> "1")
    }
  }
}

object TestValueClass {
  case class Id(value: String) extends AnyVal
  object Id {
    implicit val writes: Write[Id, JsString] = Write(id => JsString(id.value))
  }
}
