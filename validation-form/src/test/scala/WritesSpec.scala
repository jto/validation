import jto.validation._
import jto.validation.forms._
import org.scalatest._
import scala.Function.unlift

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

  val contactMap = Map("firstname" -> Seq("Julien"),
                       "lastname" -> Seq("Tournay"),
                       "informations[0].label" -> Seq("Personal"),
                       "informations[0].email" -> Seq("fakecontact@gmail.com"),
                       "informations[0].phones[0]" -> Seq("01.23.45.67.89"),
                       "informations[0].phones[1]" -> Seq("98.76.54.32.10"))

  import Writes._

  "Writes" should {

    "write string" in {
      val w = (Path \ "label").write[String, UrlFormEncoded]
      w.writes("Hello World") shouldBe Map("label" -> Seq("Hello World"))
    }

    "ignore values" in {
      (Path \ "n").write(ignored("foo")).writes("test") shouldBe Map(
          "n" -> Seq("foo"))
      (Path \ "n").write(ignored(42)).writes(0) shouldBe Map("n" -> Seq("42"))
    }

    "write option" in {
      val w = (Path \ "email").write[Option[String], UrlFormEncoded]
      w.writes(Some("Hello World")) shouldBe Map("email" -> Seq("Hello World"))
      w.writes(None) shouldBe Map.empty

      (Path \ "n").write(optionW(intW)).writes(Some(5)) shouldBe Map(
          "n" -> Seq("5"))
      (Path \ "n").write(optionW(intW)).writes(None) shouldBe Map.empty

      case class Foo(name: String)
      implicit val wf =
        (Path \ "name").write[String, UrlFormEncoded].contramap((_: Foo).name)
      val wmf = (Path \ "maybe").write[Option[Foo], UrlFormEncoded]
      wmf.writes(Some(Foo("bar"))) shouldBe Map("maybe.name" -> Seq("bar"))
      wmf.writes(None) shouldBe Map.empty
    }

    "write seq" in {
      val w = (Path \ "phones").write[Seq[String], UrlFormEncoded]
      w.writes(Seq("01.23.45.67.89", "98.76.54.32.10")) shouldBe Map(
          "phones[0]" -> Seq("01.23.45.67.89"),
          "phones[1]" -> Seq("98.76.54.32.10"))
      w.writes(Nil) shouldBe Map.empty
    }

    "write seq with duplicates" in {
      val w = (Path \ "phones").write[Seq[String], UrlFormEncoded]
      w.writes(Seq("01.23.45.67.89", "98.76.54.32.10", "98.76.54.32.10", "56.78.90.12.34")) shouldBe Map(
        "phones[0]" -> Seq("01.23.45.67.89"),
        "phones[1]" -> Seq("98.76.54.32.10"),
        "phones[2]" -> Seq("98.76.54.32.10"),
        "phones[3]" -> Seq("56.78.90.12.34")
      )
      w.writes(Nil) shouldBe Map.empty
    }

    "support primitives types" when {

      "Int" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Int]
        }.writes(4) shouldBe (Map("n" -> Seq("4")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Int]
        }.writes(4) shouldBe (Map("n.o" -> Seq("4")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Int]
        }.writes(4) shouldBe (Map("n.o.p" -> Seq("4")))
      }

      "Short" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Short]
        }.writes(4) shouldBe (Map("n" -> Seq("4")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Short]
        }.writes(4) shouldBe (Map("n.o" -> Seq("4")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Short]
        }.writes(4) shouldBe (Map("n.o.p" -> Seq("4")))
      }

      "Long" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Long]
        }.writes(4) shouldBe (Map("n" -> Seq("4")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Long]
        }.writes(4) shouldBe (Map("n.o" -> Seq("4")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Long]
        }.writes(4) shouldBe (Map("n.o.p" -> Seq("4")))
      }

      "Float" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Float]
        }.writes(4.5F) shouldBe (Map("n" -> Seq("4.5")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Float]
        }.writes(4.5F) shouldBe (Map("n.o" -> Seq("4.5")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Float]
        }.writes(4.5F) shouldBe (Map("n.o.p" -> Seq("4.5")))
      }

      "Double" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Double]
        }.writes(4.5D) shouldBe (Map("n" -> Seq("4.5")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Double]
        }.writes(4.5D) shouldBe (Map("n.o" -> Seq("4.5")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Double]
        }.writes(4.5D) shouldBe (Map("n.o.p" -> Seq("4.5")))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[jBigDecimal]
        }.writes(new jBigDecimal("4.0")) shouldBe (Map("n" -> Seq("4.0")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[jBigDecimal]
        }.writes(new jBigDecimal("4.5")) shouldBe (Map("n.o" -> Seq("4.5")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[jBigDecimal]
        }.writes(new jBigDecimal("4.5")) shouldBe (Map("n.o.p" -> Seq("4.5")))
      }

      "scala BigDecimal" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[BigDecimal]
        }.writes(BigDecimal("4.0")) shouldBe (Map("n" -> Seq("4.0")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[BigDecimal]
        }.writes(BigDecimal("4.5")) shouldBe (Map("n.o" -> Seq("4.5")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[BigDecimal]
        }.writes(BigDecimal("4.5")) shouldBe (Map("n.o.p" -> Seq("4.5")))
      }

      "Boolean" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Boolean]
        }.writes(true) shouldBe (Map("n" -> Seq("true")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Boolean]
        }.writes(false) shouldBe (Map("n.o" -> Seq("false")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Boolean]
        }.writes(true) shouldBe (Map("n.o.p" -> Seq("true")))
      }

      "String" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[String]
        }.writes("foo") shouldBe (Map("n" -> Seq("foo")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[String]
        }.writes("foo") shouldBe (Map("n.o" -> Seq("foo")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[String]
        }.writes("foo") shouldBe (Map("n.o.p" -> Seq("foo")))
      }

      "Option" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Option[String]]
        }.writes(Some("foo")) shouldBe (Map("n" -> Seq("foo")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Option[String]]
        }.writes(Some("foo")) shouldBe (Map("n.o" -> Seq("foo")))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Option[String]]
        }.writes(Some("foo")) shouldBe (Map("n.o.p" -> Seq("foo")))

        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Option[String]]
        }.writes(None) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Option[String]]
        }.writes(None) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Option[String]]
        }.writes(None) shouldBe (Map.empty)
      }

      "Map[String, Seq[V]]" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Map[String, Seq[String]]]
        }.writes(Map("foo" -> Seq("bar"))) shouldBe ((Map(
                "n.foo" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Map[String, Seq[Int]]]
        }.writes(Map("foo" -> Seq(4))) shouldBe ((Map("n.foo" -> Seq("4"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Map[String, Seq[Int]]]
        }.writes(Map("foo" -> Seq(4))) shouldBe ((Map("n.o.foo" -> Seq("4"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Map[String, Int]]
        }.writes(Map("foo" -> 4)) shouldBe ((Map("n.o.foo" -> Seq("4"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Map[String, Int]]
        }.writes(Map.empty) shouldBe (Map.empty)
      }

      "Traversable" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Traversable[String]]
        }.writes(Array("foo", "bar").toTraversable) shouldBe ((Map(
                "n[0]" -> Seq("foo"), "n[1]" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Traversable[String]]
        }.writes(Array("foo", "bar").toTraversable) shouldBe ((Map(
                "n.o[0]" -> Seq("foo"), "n.o[1]" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Traversable[String]]
        }.writes(Array("foo", "bar").toTraversable) shouldBe ((Map(
                "n.o.p[0]" -> Seq("foo"), "n.o.p[1]" -> Seq("bar"))))

        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Traversable[String]]
        }.writes(Array().toTraversable) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Traversable[String]]
        }.writes(Array().toTraversable) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Traversable[String]]
        }.writes(Array().toTraversable) shouldBe (Map.empty)
      }

      "Array" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Array[String]]
        }.writes(Array("foo", "bar")) shouldBe ((Map("n[0]" -> Seq("foo"),
                                                     "n[1]" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Array[String]]
        }.writes(Array("foo", "bar")) shouldBe ((Map("n.o[0]" -> Seq("foo"),
                                                     "n.o[1]" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Array[String]]
        }.writes(Array("foo", "bar")) shouldBe ((Map(
                "n.o.p[0]" -> Seq("foo"), "n.o.p[1]" -> Seq("bar"))))

        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Array[String]]
        }.writes(Array()) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Array[String]]
        }.writes(Array()) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Array[String]]
        }.writes(Array()) shouldBe (Map.empty)
      }

      "Seq" in {
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Seq[String]]
        }.writes(Seq("foo", "bar")) shouldBe ((Map("n[0]" -> Seq("foo"),
                                                   "n[1]" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Seq[String]]
        }.writes(Seq("foo", "bar")) shouldBe ((Map("n.o[0]" -> Seq("foo"),
                                                   "n.o[1]" -> Seq("bar"))))
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Seq[String]]
        }.writes(Seq("foo", "bar")) shouldBe ((Map("n.o.p[0]" -> Seq("foo"),
                                                   "n.o.p[1]" -> Seq("bar"))))

        To[UrlFormEncoded] { __ =>
          (__ \ "n").write[Seq[String]]
        }.writes(Nil) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o").write[Seq[String]]
        }.writes(Nil) shouldBe (Map.empty)
        To[UrlFormEncoded] { __ =>
          (__ \ "n" \ "o" \ "p").write[Seq[String]]
        }.writes(Nil) shouldBe (Map.empty)
      }
    }

    "compose" in {
      val w = To[UrlFormEncoded] { __ =>
        ((__ \ "email").write[Option[String]] ~
            (__ \ "phones").write[Seq[String]]).tupled
      }

      val v = Some("jto@foobar.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")

      w.writes(v) shouldBe Map("email" -> Seq("jto@foobar.com"),
                               "phones[0]" -> Seq("01.23.45.67.89"),
                               "phones[1]" -> Seq("98.76.54.32.10"))
      w.writes(Some("jto@foobar.com") -> Nil) shouldBe Map(
          "email" -> Seq("jto@foobar.com"))
      w.writes(None -> Nil) shouldBe Map.empty
    }

    "write Map" in {
      def contactWrite = {
        implicit val contactInformation = To[UrlFormEncoded] { __ =>
          ((__ \ "label").write[String] ~
              (__ \ "email").write[Option[String]] ~
              (__ \ "phones").write[Seq[String]])(
              unlift(ContactInformation.unapply))
        }

        To[UrlFormEncoded] { __ =>
          ((__ \ "firstname").write[String] ~
              (__ \ "lastname").write[String] ~
              (__ \ "company").write[Option[String]] ~
              (__ \ "informations").write[Seq[ContactInformation]])(
              unlift(Contact.unapply))
        }
      }

      contactWrite.writes(contact) shouldBe contactMap
    }

    "write recursive" when {
      case class RecUser(name: String, friends: List[RecUser] = Nil)
      val u = RecUser("bob", List(RecUser("tom")))

      val m = Map("name" -> Seq("bob"), "friends[0].name" -> Seq("tom"))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = Map("name" -> Seq("bob"), "friend.name" -> Seq("tom"))

      "using explicit notation" in {
        lazy val w: Write[RecUser, UrlFormEncoded] = To[UrlFormEncoded] { __ =>
          ((__ \ "name").write[String] ~
              (__ \ "friends").write(seqW(w)))(unlift(RecUser.unapply))
        }
        w.writes(u) shouldBe m

        lazy val w2: Write[RecUser, UrlFormEncoded] =
          ((Path \ "name").write[String, UrlFormEncoded] ~
              (Path \ "friends").write(seqW(w2)))(unlift(RecUser.unapply))
        w2.writes(u) shouldBe m

        lazy val w3: Write[User1, UrlFormEncoded] = To[UrlFormEncoded] { __ =>
          ((__ \ "name").write[String] ~
              (__ \ "friend").write(optionW(w3)))(unlift(User1.unapply))
        }
        w3.writes(u1) shouldBe m1
      }

      "using implicit notation" in {
        implicit lazy val w: Write[RecUser, UrlFormEncoded] =
          To[UrlFormEncoded] { __ =>
            ((__ \ "name").write[String] ~
                (__ \ "friends").write[Seq[RecUser]])(unlift(RecUser.unapply))
          }
        w.writes(u) shouldBe m

        implicit lazy val w3: Write[User1, UrlFormEncoded] =
          To[UrlFormEncoded] { __ =>
            ((__ \ "name").write[String] ~
                (__ \ "friend").write[Option[User1]])(unlift(User1.unapply))
          }
        w3.writes(u1) shouldBe m1
      }
    }

    "support write of value class" in {
      import TestValueClass._

      val w = To[UrlFormEncoded] { __ =>
        (__ \ "id").write[Id]
      }

      w.writes(Id("1")) shouldBe Map("id" -> Seq("1"))
    }
  }
}

object TestValueClass {
  case class Id(value: String) extends AnyVal
  object Id {
    implicit val writes: Write[Id, String] = Write(id => id.value)
  }
}
