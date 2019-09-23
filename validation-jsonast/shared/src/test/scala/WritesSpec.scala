package jto.validation
package jsonast
package test

import jto.validation._
import jto.validation.jsonast._
import jto.validation.jsonast.Writes._
import org.scalatest._
import scala.Function.unlift

final class WritesSpec extends WordSpec with Matchers {

  final case class Contact(firstname: String,
                     lastname: String,
                     company: Option[String],
                     informations: Seq[ContactInformation])

  final case class ContactInformation(
      label: String, email: Option[String], phones: Seq[String])

  val contact = Contact(
      "Julien",
      "Tournay",
      None,
      Seq(
          ContactInformation("Personal",
                             Some("fakecontact@gmail.com"),
                             Seq("01.23.45.67.89", "98.76.54.32.10"))))

  val contactJson = JObject(Map(
          "firstname" -> JString("Julien"),
          "lastname" -> JString("Tournay"),
          "informations" -> JArray(Seq(JObject(
                      Map(
                          "label" -> JString("Personal"),
                          "email" -> JString("fakecontact@gmail.com"),
                          "phones" -> JArray(Seq(
                                  JString("01.23.45.67.89"),
                                  JString("98.76.54.32.10")))))))))

  "Writes" should {

    "write string" in {
      val w = (Path \ "label").write[String, JObject]
      w.writes("Hello World") shouldBe JObject(
          Map("label" -> JString("Hello World")))
    }

    "ignore values" in {
      (Path \ "n").write(ignored("foo")).writes("test") shouldBe JObject(
          Map("n" -> JString("foo")))
      (Path \ "n").write(ignored(42)).writes(0) shouldBe JObject(
          Map("n" -> JNumber(42)))
    }

    "write option" in {
      val w = (Path \ "email").write[Option[String], JObject]
      w.writes(Some("Hello World")) shouldBe JObject(
          Map("email" -> JString("Hello World")))
      w.writes(None) shouldBe JObject(Map())

      (Path \ "n").write(optionW(intW)).writes(Some(5)) shouldBe JObject(
          Map("n" -> JNumber(5)))
      (Path \ "n").write(optionW(intW)).writes(None) shouldBe JObject(Map())
    }

    "write seq" in {
      val w = (Path \ "phones").write[Seq[String], JObject]
      w.writes(Seq("01.23.45.67.89", "98.76.54.32.10")) shouldBe JObject(
          Map("phones" -> JArray(Seq(JString("01.23.45.67.89"),
                                     JString("98.76.54.32.10")))))
      w.writes(Nil) shouldBe JObject(Map("phones" -> JArray()))
    }

    "support primitives types" when {

      "Int" in {
        (Path \ "n").write[Int, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JNumber(4))))
        (Path \ "n" \ "o").write[Int, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4))))))
        (Path \ "n" \ "o" \ "p").write[Int, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4))))))))
      }

      "Short" in {
        (Path \ "n").write[Short, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JNumber(4))))
        (Path \ "n" \ "o").write[Short, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4))))))
        (Path \ "n" \ "o" \ "p").write[Short, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4))))))))
      }

      "Long" in {
        (Path \ "n").write[Long, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JNumber(4))))
        (Path \ "n" \ "o").write[Long, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4))))))
        (Path \ "n" \ "o" \ "p").write[Long, JObject].writes(4) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4))))))))
      }

      "Float" in {
        (Path \ "n").write[Float, JObject].writes(4.5f) shouldBe (JObject(
                Map("n" -> JNumber(4.5))))
        (Path \ "n" \ "o").write[Float, JObject].writes(4.5f) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4.5))))))
        (Path \ "n" \ "o" \ "p").write[Float, JObject].writes(4.5f) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4.5))))))))
      }

      "Double" in {
        (Path \ "n").write[Double, JObject].writes(4.1d) shouldBe (JObject(
                Map("n" -> JNumber(4.1))))
        (Path \ "n" \ "o").write[Double, JObject].writes(4.5d) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4.5))))))
        (Path \ "n" \ "o" \ "p").write[Double, JObject].writes(4.5d) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4.5))))))))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        (Path \ "n").write[jBigDecimal, JObject].writes(new jBigDecimal("4.2")) shouldBe (JObject(
                Map("n" -> JNumber(4.2))))
        (Path \ "n" \ "o")
          .write[jBigDecimal, JObject]
          .writes(new jBigDecimal("4.5")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4.5))))))
        (Path \ "n" \ "o" \ "p")
          .write[jBigDecimal, JObject]
          .writes(new jBigDecimal("4.5")) shouldBe (JObject(Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4.5))))))))
      }

      "scala BigDecimal" in {
        (Path \ "n").write[BigDecimal, JObject].writes(BigDecimal("4")) shouldBe (JObject(
                Map("n" -> JNumber(4))))
        (Path \ "n" \ "o").write[BigDecimal, JObject].writes(BigDecimal("4.5")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JNumber(4.5))))))
        (Path \ "n" \ "o" \ "p")
          .write[BigDecimal, JObject]
          .writes(BigDecimal("4.5")) shouldBe (JObject(Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JNumber(4.5))))))))
      }

      "Boolean" in {
        (Path \ "n").write[Boolean, JObject].writes(true) shouldBe (JObject(
                Map("n" -> JBoolean(true))))
        (Path \ "n" \ "o").write[Boolean, JObject].writes(false) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JBoolean(false))))))
        (Path \ "n" \ "o" \ "p").write[Boolean, JObject].writes(true) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JBoolean(true))))))))
      }

      "String" in {
        (Path \ "n").write[String, JObject].writes("foo") shouldBe (JObject(
                Map("n" -> JString("foo"))))
        (Path \ "n" \ "o").write[String, JObject].writes("foo") shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JString("foo"))))))
        (Path \ "n" \ "o" \ "p").write[String, JObject].writes("foo") shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JString("foo"))))))))
      }

      "Option" in {
        (Path \ "n").write[Option[String], JObject].writes(Some("foo")) shouldBe (JObject(
                Map("n" -> JString("foo"))))
        (Path \ "n" \ "o").write[Option[String], JObject].writes(Some("foo")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JString("foo"))))))
        (Path \ "n" \ "o" \ "p")
          .write[Option[String], JObject]
          .writes(Some("foo")) shouldBe (JObject(Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JString("foo"))))))))

        (Path \ "n").write[Option[String], JObject].writes(None) shouldBe (JObject(
                Map()))
        (Path \ "n" \ "o").write[Option[String], JObject].writes(None) shouldBe (JObject(
                Map()))
        (Path \ "n" \ "o" \ "p").write[Option[String], JObject].writes(None) shouldBe (JObject(
                Map()))
      }

      "Map[String, Seq[V]]" in {
        (Path \ "n")
          .write[Map[String, Seq[String]], JObject]
          .writes(Map("foo" -> Seq("bar"))) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("foo" -> JArray(Seq(JString("bar"))))))))
        (Path \ "n")
          .write[Map[String, Seq[Int]], JObject]
          .writes(Map("foo" -> Seq(4))) shouldBe (JObject(
                Map("n" -> JObject(Map("foo" -> JArray(Seq(JNumber(4))))))))
        (Path \ "n" \ "o")
          .write[Map[String, Seq[Int]], JObject]
          .writes(Map("foo" -> Seq(4))) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JObject(
                                Map("foo" -> JArray(Seq(JNumber(4))))))))))
        (Path \ "n" \ "o")
          .write[Map[String, Int], JObject]
          .writes(Map("foo" -> 4)) shouldBe (JObject(Map("n" -> JObject(
                        Map("o" -> JObject(Map("foo" -> JNumber(4))))))))
        (Path \ "n" \ "o").write[Map[String, Int], JObject].writes(Map.empty) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JObject(Map()))))))
      }

      "Traversable" in {
        (Path \ "n")
          .write[Traversable[String], JObject]
          .writes(Array("foo", "bar")) shouldBe (JObject(
                Map("n" -> JArray(Seq(JString("foo"), JString("bar"))))))
        (Path \ "n" \ "o")
          .write[Traversable[String], JObject]
          .writes(Array("foo", "bar")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JArray(Seq(JString("foo"),
                                                         JString("bar"))))))))
        (Path \ "n" \ "o" \ "p")
          .write[Traversable[String], JObject]
          .writes(Array("foo", "bar")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JObject(
                                Map("p" -> JArray(Seq(JString("foo"),
                                                      JString("bar"))))))))))

        (Path \ "n")
          .write[Traversable[String], JObject]
          .writes(Array[String]()) shouldBe (JObject(Map("n" -> JArray())))
        (Path \ "n" \ "o")
          .write[Traversable[String], JObject]
          .writes(Array[String]()) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JArray())))))
        (Path \ "n" \ "o" \ "p")
          .write[Traversable[String], JObject]
          .writes(Array[String]()) shouldBe (JObject(Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JArray())))))))
      }

      "Array" in {
        (Path \ "n").write[Array[String], JObject].writes(Array("foo", "bar")) shouldBe (JObject(
                Map("n" -> JArray(Seq(JString("foo"), JString("bar"))))))
        (Path \ "n" \ "o")
          .write[Array[String], JObject]
          .writes(Array("foo", "bar")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JArray(Seq(JString("foo"),
                                                         JString("bar"))))))))
        (Path \ "n" \ "o" \ "p")
          .write[Array[String], JObject]
          .writes(Array("foo", "bar")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JObject(
                                Map("p" -> JArray(Seq(JString("foo"),
                                                      JString("bar"))))))))))

        (Path \ "n").write[Array[String], JObject].writes(Array()) shouldBe (JObject(
                Map("n" -> JArray())))
        (Path \ "n" \ "o").write[Array[String], JObject].writes(Array()) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JArray())))))
        (Path \ "n" \ "o" \ "p").write[Array[String], JObject].writes(Array()) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JArray())))))))
      }

      "Seq" in {
        (Path \ "n").write[Seq[String], JObject].writes(Seq("foo", "bar")) shouldBe (JObject(
                Map("n" -> JArray(Seq(JString("foo"), JString("bar"))))))
        (Path \ "n" \ "o")
          .write[Seq[String], JObject]
          .writes(Seq("foo", "bar")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JArray(Seq(JString("foo"),
                                                         JString("bar"))))))))
        (Path \ "n" \ "o" \ "p")
          .write[Seq[String], JObject]
          .writes(Seq("foo", "bar")) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JObject(
                                Map("p" -> JArray(Seq(JString("foo"),
                                                      JString("bar"))))))))))

        (Path \ "n").write[Seq[String], JObject].writes(Nil) shouldBe (JObject(
                Map("n" -> JArray())))
        (Path \ "n" \ "o").write[Seq[String], JObject].writes(Nil) shouldBe (JObject(
                Map("n" -> JObject(Map("o" -> JArray())))))
        (Path \ "n" \ "o" \ "p").write[Seq[String], JObject].writes(Nil) shouldBe (JObject(
                Map("n" -> JObject(
                        Map("o" -> JObject(Map("p" -> JArray())))))))
      }
    }

    "compose" in {
      val w: Write[(Option[String], Seq[String]), JObject] =
        To[JObject] { __ =>
          ((__ \ "email").write[Option[String]] ~
              (__ \ "phones").write[Seq[String]]).tupled
        }

      val v = Some("jto@foobar.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")

      w.writes(v) shouldBe JObject(
          Map("email" -> JString("jto@foobar.com"),
              "phones" -> JArray(Seq(JString("01.23.45.67.89"),
                                     JString("98.76.54.32.10")))))
      w.writes(Some("jto@foobar.com") -> Nil) shouldBe JObject(
          Map("email" -> JString("jto@foobar.com"), "phones" -> JArray()))
      w.writes(None -> Nil) shouldBe JObject(Map("phones" -> JArray()))
    }

    // "write Invalid" in {
    //   val f = Invalid[(Path, Seq[ValidationError]), String](Seq(Path \ "n" -> Seq(ValidationError("validation.type-mismatch", "Int"))))

    //   implicitly[Write[(Path, Seq[ValidationError]), JObject]]
    //   implicitly[Write[Invalid[(Path, Seq[ValidationError]), String], JObject]]

    //   val error =
    //     JObject("errors" ->
    //       JObject("/n" -> JArray
    //           JObject(
    //             "msg" -> JString("validation.type-mismatch"),
    //             "args" -> JArrayJString("Int"))))))))

    //   (Path \ "errors").write[Invalid[(Path, Seq[ValidationError]), String], JObject]
    //     .writes(f) shouldBe(error)
    // }

    "write Map" in {
      implicit val contactInformation = To[JObject] { __ =>
        ((__ \ "label").write[String] ~
            (__ \ "email").write[Option[String]] ~
            (__ \ "phones").write[Seq[String]])(
            unlift(ContactInformation.unapply))
      }

      implicit val contactWrite = To[JObject] { __ =>
        ((__ \ "firstname").write[String] ~
            (__ \ "lastname").write[String] ~
            (__ \ "company").write[Option[String]] ~
            (__ \ "informations").write[Seq[ContactInformation]])(
            unlift(Contact.unapply))
      }

      contactWrite.writes(contact) shouldBe contactJson
    }

    "write recursive" when {
      final case class RecUser(name: String, friends: List[RecUser] = Nil)
      val u = RecUser("bob", List(RecUser("tom")))

      val m = JObject(
          Map("name" -> JString("bob"),
              "friends" -> JArray(Seq(JObject(Map("name" -> JString("tom"),
                                                  "friends" -> JArray()))))))

      final case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(Map("name" -> JString("bob"),
                           "friend" -> JObject(Map("name" -> JString("tom")))))

      "using explicit notation" in {
        lazy val w: Write[RecUser, JObject] = To[JObject] { __ =>
          ((__ \ "name").write[String] ~
              (__ \ "friends").write(seqW(w)))(unlift(RecUser.unapply))
        }
        w.writes(u) shouldBe m

        lazy val w2: Write[RecUser, JObject] =
          ((Path \ "name").write[String, JObject] ~
              (Path \ "friends").write(seqW(w2)))(unlift(RecUser.unapply))
        w2.writes(u) shouldBe m

        lazy val w3: Write[User1, JObject] = To[JObject] { __ =>
          ((__ \ "name").write[String] ~
              (__ \ "friend").write(optionW(w3)))(unlift(User1.unapply))
        }
        w3.writes(u1) shouldBe m1
      }

      "using implicit notation" in {
        implicit lazy val w: Write[RecUser, JObject] = To[JObject] { __ =>
          ((__ \ "name").write[String] ~
              (__ \ "friends").write[Seq[RecUser]])(unlift(RecUser.unapply))
        }
        w.writes(u) shouldBe m

        implicit lazy val w3: Write[User1, JObject] = To[JObject] { __ =>
          ((__ \ "name").write[String] ~
              (__ \ "friend").write[Option[User1]])(unlift(User1.unapply))
        }
        w3.writes(u1) shouldBe m1
      }
    }

    "support write of value class" in {
      import TestValueClass._

      val w = To[JObject] { __ =>
        (__ \ "id").write[Id]
      }

      w.writes(Id("1")) shouldBe JObject(Map("id" -> JString("1")))
    }

    "write VA" in {
      val o = JObject(Map("a" -> JString("string")))

      To[VA[JObject], JObject](Valid(o)) shouldBe JObject(
          Map("isValid" -> JBoolean(true),
              "output" -> o,
              "errors" -> JNull))
    }
  }
}

object TestValueClass {
  final case class Id(value: String) extends AnyVal
  object Id {
    implicit val writes: Write[Id, JString] = Write(id => JString(id.value))
  }
}
