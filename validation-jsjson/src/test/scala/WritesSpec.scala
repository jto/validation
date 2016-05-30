import jto.validation._
import jto.validation.jsjson.Writes._
import org.scalatest._
import scala.scalajs.js
import scala.Function.unlift

class WritesSpec extends WordSpec with Matchers with JsAnyEquality {

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

  val contactJson = js.Dynamic.literal(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "informations" -> js.Array(js.Dynamic.literal(
              "label" -> "Personal",
              "email" -> "fakecontact@gmail.com",
              "phones" -> js.Array("01.23.45.67.89", "98.76.54.32.10"))))

  "Writes" should {

    "write string" in {
      val w = (Path \ "label").write[String, js.Dynamic]
      w.writes("Hello World") shouldBe js.Dynamic.literal(
          "label" -> "Hello World")
    }

    "ignore values" in {
      (Path \ "n").write(ignored("foo")).writes("test") shouldBe js.Dynamic
        .literal("n" -> "foo")
      (Path \ "n").write(ignored(42)).writes(0) shouldBe js.Dynamic.literal(
          "n" -> 42)
    }

    "write option" in {
      val w = (Path \ "email").write[Option[String], js.Dynamic]
      w.writes(Some("Hello World")) shouldBe js.Dynamic.literal(
          "email" -> "Hello World")
      w.writes(None) shouldBe js.Dynamic.literal()

      (Path \ "n").write(optionW(intW)).writes(Some(5)) shouldBe js.Dynamic
        .literal("n" -> 5)
      (Path \ "n").write(optionW(intW)).writes(None) shouldBe js.Dynamic
        .literal()
    }

    "write seq" in {
      val w = (Path \ "phones").write[Seq[String], js.Dynamic]
      w.writes(Seq("01.23.45.67.89", "98.76.54.32.10")) shouldBe js.Dynamic
        .literal("phones" -> js.Array("01.23.45.67.89", "98.76.54.32.10"))
      w.writes(Nil) shouldBe js.Dynamic.literal("phones" -> js.Array())
    }

    "support primitives types" when {

      "Int" in {
        (Path \ "n").write[Int, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> 4))
        (Path \ "n" \ "o").write[Int, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> 4)))
        (Path \ "n" \ "o" \ "p").write[Int, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> 4))))
      }

      "Short" in {
        (Path \ "n").write[Short, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> 4))
        (Path \ "n" \ "o").write[Short, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> 4)))
        (Path \ "n" \ "o" \ "p").write[Short, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> 4))))
      }

      "Long" in {
        (Path \ "n").write[Long, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> 4))
        (Path \ "n" \ "o").write[Long, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> 4)))
        (Path \ "n" \ "o" \ "p").write[Long, js.Dynamic].writes(4) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> 4))))
      }

      "Float" in {
        (Path \ "n").write[Float, js.Dynamic].writes(4.5f) shouldBe
        (js.Dynamic.literal("n" -> 4.5))
        (Path \ "n" \ "o").write[Float, js.Dynamic].writes(4.5f) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> 4.5)))
        (Path \ "n" \ "o" \ "p").write[Float, js.Dynamic].writes(4.5f) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> 4.5))))
      }

      "Double" in {
        (Path \ "n").write[Double, js.Dynamic].writes(4d) shouldBe
        (js.Dynamic.literal("n" -> 4.0))
        (Path \ "n" \ "o").write[Double, js.Dynamic].writes(4.5d) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> 4.5)))
        (Path \ "n" \ "o" \ "p").write[Double, js.Dynamic].writes(4.5d) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> 4.5))))
      }

      "scala BigDecimal" in {
        (Path \ "n").write[BigDecimal, js.Dynamic].writes(BigDecimal("4.5")) shouldBe
        (js.Dynamic.literal("n" -> "4.5"))
        (Path \ "n" \ "o")
          .write[BigDecimal, js.Dynamic]
          .writes(BigDecimal("4.5")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> "4.5")))
        (Path \ "n" \ "o" \ "p")
          .write[BigDecimal, js.Dynamic]
          .writes(BigDecimal("4.5")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> "4.5"))))
      }

      "Boolean" in {
        (Path \ "n").write[Boolean, js.Dynamic].writes(true) shouldBe
        (js.Dynamic.literal("n" -> true))
        (Path \ "n" \ "o").write[Boolean, js.Dynamic].writes(false) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> false)))
        (Path \ "n" \ "o" \ "p").write[Boolean, js.Dynamic].writes(true) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> true))))
      }

      "String" in {
        (Path \ "n").write[String, js.Dynamic].writes("foo") shouldBe
        (js.Dynamic.literal("n" -> "foo"))
        (Path \ "n" \ "o").write[String, js.Dynamic].writes("foo") shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> "foo")))
        (Path \ "n" \ "o" \ "p").write[String, js.Dynamic].writes("foo") shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> "foo"))))
      }

      "Option" in {
        (Path \ "n").write[Option[String], js.Dynamic].writes(Some("foo")) shouldBe
        (js.Dynamic.literal("n" -> "foo"))
        (Path \ "n" \ "o")
          .write[Option[String], js.Dynamic]
          .writes(Some("foo")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> "foo")))
        (Path \ "n" \ "o" \ "p")
          .write[Option[String], js.Dynamic]
          .writes(Some("foo")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> "foo"))))

        (Path \ "n").write[Option[String], js.Dynamic].writes(None) shouldBe
        (js.Dynamic.literal())
        (Path \ "n" \ "o").write[Option[String], js.Dynamic].writes(None) shouldBe
        (js.Dynamic.literal())
        (Path \ "n" \ "o" \ "p").write[Option[String], js.Dynamic].writes(None) shouldBe
        (js.Dynamic.literal())
      }

      "Map[String, Seq[V]]" in {
        (Path \ "n")
          .write[Map[String, Seq[String]], js.Dynamic]
          .writes(Map("foo" -> Seq("bar"))) shouldBe
        (js.Dynamic.literal(
                "n" -> js.Dynamic.literal("foo" -> js.Array("bar"))))
        (Path \ "n")
          .write[Map[String, Seq[Int]], js.Dynamic]
          .writes(Map("foo" -> Seq(4))) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("foo" -> js.Array(4))))
        (Path \ "n" \ "o")
          .write[Map[String, Seq[Int]], js.Dynamic]
          .writes(Map("foo" -> Seq(4))) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("foo" -> js.Array(4)))))
        (Path \ "n" \ "o")
          .write[Map[String, Int], js.Dynamic]
          .writes(Map("foo" -> 4)) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("foo" -> 4))))
        (Path \ "n" \ "o")
          .write[Map[String, Int], js.Dynamic]
          .writes(Map.empty) shouldBe
        (js.Dynamic.literal(
                "n" -> js.Dynamic.literal("o" -> js.Dynamic.literal())))
      }

      "Traversable" in {
        (Path \ "n")
          .write[Traversable[String], js.Dynamic]
          .writes(Array("foo", "bar")) shouldBe
        (js.Dynamic.literal("n" -> js.Array("foo", "bar")))
        (Path \ "n" \ "o")
          .write[Traversable[String], js.Dynamic]
          .writes(Array("foo", "bar")) shouldBe
        (js.Dynamic.literal(
                "n" -> js.Dynamic.literal("o" -> js.Array("foo", "bar"))))
        (Path \ "n" \ "o" \ "p")
          .write[Traversable[String], js.Dynamic]
          .writes(Array("foo", "bar")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> js.Array("foo", "bar")))))

        (Path \ "n")
          .write[Traversable[String], js.Dynamic]
          .writes(Array[String]()) shouldBe
        (js.Dynamic.literal("n" -> js.Array()))
        (Path \ "n" \ "o")
          .write[Traversable[String], js.Dynamic]
          .writes(Array[String]()) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> js.Array())))
        (Path \ "n" \ "o" \ "p")
          .write[Traversable[String], js.Dynamic]
          .writes(Array[String]()) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> js.Array()))))
      }

      "Array" in {
        (Path \ "n")
          .write[Array[String], js.Dynamic]
          .writes(Array("foo", "bar")) shouldBe
        (js.Dynamic.literal("n" -> js.Array("foo", "bar")))
        (Path \ "n" \ "o")
          .write[Array[String], js.Dynamic]
          .writes(Array("foo", "bar")) shouldBe
        (js.Dynamic.literal(
                "n" -> js.Dynamic.literal("o" -> js.Array("foo", "bar"))))
        (Path \ "n" \ "o" \ "p")
          .write[Array[String], js.Dynamic]
          .writes(Array("foo", "bar")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> js.Array("foo", "bar")))))

        (Path \ "n").write[Array[String], js.Dynamic].writes(Array()) shouldBe
        (js.Dynamic.literal("n" -> js.Array()))
        (Path \ "n" \ "o").write[Array[String], js.Dynamic].writes(Array()) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> js.Array())))
        (Path \ "n" \ "o" \ "p")
          .write[Array[String], js.Dynamic]
          .writes(Array()) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> js.Array()))))
      }

      "Seq" in {
        (Path \ "n").write[Seq[String], js.Dynamic].writes(Seq("foo", "bar")) shouldBe
        (js.Dynamic.literal("n" -> js.Array("foo", "bar")))
        (Path \ "n" \ "o")
          .write[Seq[String], js.Dynamic]
          .writes(Seq("foo", "bar")) shouldBe
        (js.Dynamic.literal(
                "n" -> js.Dynamic.literal("o" -> js.Array("foo", "bar"))))
        (Path \ "n" \ "o" \ "p")
          .write[Seq[String], js.Dynamic]
          .writes(Seq("foo", "bar")) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> js.Array("foo", "bar")))))

        (Path \ "n").write[Seq[String], js.Dynamic].writes(Nil) shouldBe
        (js.Dynamic.literal("n" -> js.Array()))
        (Path \ "n" \ "o").write[Seq[String], js.Dynamic].writes(Nil) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal("o" -> js.Array())))
        (Path \ "n" \ "o" \ "p").write[Seq[String], js.Dynamic].writes(Nil) shouldBe
        (js.Dynamic.literal("n" -> js.Dynamic.literal(
                    "o" -> js.Dynamic.literal("p" -> js.Array()))))
      }
    }

    "compose" in {
      val w = To[js.Dynamic] { __ =>
        ((__ \ "email").write[Option[String]] ~ (__ \ "phones")
              .write[Seq[String]]).tupled
      }

      val v = Some("jto@foobar.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")

      w.writes(v) shouldBe js.Dynamic.literal(
          "email" -> "jto@foobar.com",
          "phones" -> js.Array("01.23.45.67.89", "98.76.54.32.10"))
      w.writes(Some("jto@foobar.com") -> Nil) shouldBe js.Dynamic.literal(
          "email" -> "jto@foobar.com", "phones" -> js.Array())
      w.writes(None -> Nil) shouldBe js.Dynamic.literal("phones" -> js.Array())
    }

    "write Map" in {
      implicit val contactInformation = To[js.Dynamic] { __ =>
        ((__ \ "label").write[String] ~ (__ \ "email").write[Option[String]] ~
            (__ \ "phones").write[Seq[String]])(unlift(ContactInformation.unapply))
      }

      implicit val contactWrite = To[js.Dynamic] { __ =>
        ((__ \ "firstname").write[String] ~ (__ \ "lastname").write[String] ~
            (__ \ "company").write[Option[String]] ~ (__ \ "informations")
              .write[Seq[ContactInformation]])(unlift(Contact.unapply))
      }

      contactWrite.writes(contact) shouldBe contactJson
    }

    "write recursive" when {
      case class RecUser(name: String, friends: List[RecUser] = Nil)
      val u = RecUser("bob", List(RecUser("tom")))

      val m = js.Dynamic.literal(
          "name" -> "bob",
          "friends" -> js.Array(
              js.Dynamic.literal("name" -> "tom", "friends" -> js.Array())))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = js.Dynamic.literal(
          "name" -> "bob", "friend" -> js.Dynamic.literal("name" -> "tom"))

      "using explicit notation" in {
        lazy val w: Write[RecUser, js.Dynamic] = To[js.Dynamic] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friends").write(seqW(w)))(unlift(RecUser.unapply))
        }
        w.writes(u) shouldBe m

        lazy val w2: Write[RecUser, js.Dynamic] =
          ((Path \ "name").write[String, js.Dynamic] ~ (Path \ "friends")
                .write(seqW(w2)))(unlift(RecUser.unapply))
        w2.writes(u) shouldBe m

        lazy val w3: Write[User1, js.Dynamic] = To[js.Dynamic] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friend").write(optionW(w3)))(unlift(User1.unapply))
        }
        w3.writes(u1) shouldBe m1
      }

      "using implicit notation" in {
        implicit lazy val w: Write[RecUser, js.Dynamic] = To[js.Dynamic] {
          __ =>
            ((__ \ "name").write[String] ~ (__ \ "friends")
                  .write[Seq[RecUser]])(unlift(RecUser.unapply))
        }
        w.writes(u) shouldBe m

        implicit lazy val w3: Write[User1, js.Dynamic] = To[js.Dynamic] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friend").write[Option[User1]])(unlift(User1.unapply))
        }
        w3.writes(u1) shouldBe m1
      }
    }
  }
}
