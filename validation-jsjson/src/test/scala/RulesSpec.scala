package jto.validation
package jsjson

import jto.validation.jsjson.Rules._
import org.scalatest._
import scala.scalajs.js

import js.Dynamic.{literal => lit}
import js.{Array => arr}

class RulesSpec extends WordSpec with Matchers {

  "Json Rules" should {

    val valid = lit(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "age" -> 27,
      "informations" -> lit("label" -> "Personal",
                            "email" -> "fakecontact@gmail.com",
                            "phones" -> arr("01.23.45.67.89", "98.76.54.32.10"))
    )

    val invalid = lit(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "age" -> 27,
      "informations" -> lit("label" -> "",
                            "email" -> "fakecontact@gmail.com",
                            "phones" -> arr("01.23.45.67.89", "98.76.54.32.10"))
    )

    "extract data" in {
      (Path \ "firstname").read[js.Dynamic, String].validate(valid) shouldBe
        (Valid("Julien"))
      val errPath = Path \ "foo"
      val error =
        Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[js.Dynamic, String].validate(invalid) shouldBe (error)
    }

    "support checked" in {
      val json = lit("issmth" -> true)
      val p = Path \ "issmth"
      p.from[js.Dynamic](checked).validate(json) shouldBe (Valid(true))
      p.from[js.Dynamic](checked).validate(lit()) shouldBe
        (Invalid(
          Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
      p.from[js.Dynamic](checked)
        .validate(lit("issmth" -> false)) shouldBe
        (Invalid(
          Seq(Path \ "issmth" -> Seq(ValidationError("error.equals", true)))))
    }

    "support all types of Json values" when {

      "null" in {
        (Path \ "n")
          .read[js.Dynamic, Null]
          .validate(lit("n" -> null)) shouldBe (Valid(null))
        (Path \ "n")
          .read[js.Dynamic, Null]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
        (Path \ "n")
          .read[js.Dynamic, Null]
          .validate(lit("n" -> 4.5)) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
      }

      "Int" in {
        (Path \ "n")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> 4)) shouldBe (Valid(4))
        (Path \ "n")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> 4.5)) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n" \ "o")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> lit("o" -> 4))) shouldBe
          (Valid(4))
        (Path \ "n" \ "o")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> lit("o" -> "foo"))) shouldBe
          (Invalid(
            Seq(
              Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o" \ "p")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> lit("o" -> lit("p" -> 4)))) shouldBe
          (Valid(4))
        (Path \ "n" \ "o" \ "p")
          .read[js.Dynamic, Int]
          .validate(lit("n" -> lit("o" -> lit("p" -> "foo")))) shouldBe
          (Invalid(
            Seq(Path \ "n" \ "o" \ "p" -> Seq(
              ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error =
          Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath
          .read[js.Dynamic, Int]
          .validate(lit("n" -> 4)) shouldBe
          (error)
      }

      "Short" in {
        (Path \ "n")
          .read[js.Dynamic, Short]
          .validate(lit("n" -> 4)) shouldBe (Valid(4))
        (Path \ "n")
          .read[js.Dynamic, Short]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        (Path \ "n")
          .read[js.Dynamic, Short]
          .validate(lit("n" -> 4.5)) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        (Path \ "n")
          .read[js.Dynamic, Long]
          .validate(lit("n" -> 4)) shouldBe (Valid(4))
        (Path \ "n")
          .read[js.Dynamic, Long]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        (Path \ "n")
          .read[js.Dynamic, Long]
          .validate(lit("n" -> 4.5)) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        (Path \ "n")
          .read[js.Dynamic, Float]
          .validate(lit("n" -> 4)) shouldBe (Valid(4))
        (Path \ "n")
          .read[js.Dynamic, Float]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        (Path \ "n")
          .read[js.Dynamic, Float]
          .validate(lit("n" -> 4.5)) shouldBe (Valid(4.5F))
      }

      "Double" in {
        (Path \ "n")
          .read[js.Dynamic, Double]
          .validate(lit("n" -> 4)) shouldBe (Valid(4))
        (Path \ "n")
          .read[js.Dynamic, Double]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        (Path \ "n")
          .read[js.Dynamic, Double]
          .validate(lit("n" -> 4.5)) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        (Path \ "n")
          .read[js.Dynamic, jBigDecimal]
          .validate(lit("n" -> 4)) shouldBe
          (Valid(new jBigDecimal("4")))
        (Path \ "n")
          .read[js.Dynamic, jBigDecimal]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(Seq(
            Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n")
          .read[js.Dynamic, jBigDecimal]
          .validate(lit("n" -> 4.5)) shouldBe
          (Valid(new jBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        (Path \ "n")
          .read[js.Dynamic, BigDecimal]
          .validate(lit("n" -> 4)) shouldBe
          (Valid(BigDecimal(4)))
        (Path \ "n")
          .read[js.Dynamic, BigDecimal]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(Seq(
            Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n")
          .read[js.Dynamic, BigDecimal]
          .validate(lit("n" -> 4.5)) shouldBe
          (Valid(BigDecimal(4.5)))
      }

      "String" in {
        (Path \ "n")
          .read[js.Dynamic, String]
          .validate(lit("n" -> "foo")) shouldBe (Valid("foo"))
        (Path \ "n")
          .read[js.Dynamic, String]
          .validate(lit("n" -> 42)) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "n")
          .read[js.Dynamic, String]
          .validate(lit("n" -> arr("foo"))) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "o")
          .read[js.Dynamic, String]
          .validate(lit("o" -> lit("n" -> "foo"))) shouldBe
          (Invalid(
            Seq(Path \ "o" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "js.Dictionary" in {
        (Path \ "o")
          .read[js.Dynamic, js.Dictionary[js.Dynamic]]
          .map(_.toSeq)
          .validate(lit("o" -> lit("n" -> "foo"))) shouldBe
          (Valid(Seq("n" -> "foo")))
        (Path \ "n")
          .read[js.Dynamic, js.Dictionary[js.Dynamic]]
          .validate(lit("n" -> 42)) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n")
          .read[js.Dynamic, js.Dictionary[js.Dynamic]]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n")
          .read[js.Dynamic, js.Dictionary[js.Dynamic]]
          .validate(lit("n" -> arr("foo"))) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
      }

      "Boolean" in {
        (Path \ "n")
          .read[js.Dynamic, Boolean]
          .validate(lit("n" -> true)) shouldBe (Valid(true))
        (Path \ "n")
          .read[js.Dynamic, Boolean]
          .validate(lit("n" -> "foo")) shouldBe
          (Invalid(
            Seq(
              Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Option" in {
        (Path \ "n")
          .read[js.Dynamic, Option[Boolean]]
          .validate(lit("n" -> true)) shouldBe
          (Valid(Some(true)))
        (Path \ "n")
          .read[js.Dynamic, Option[Boolean]]
          .validate(lit("n" -> null)) shouldBe (Valid(None))
        (Path \ "n")
          .read[js.Dynamic, Option[Boolean]]
          .validate(lit("foo" -> "bar")) shouldBe (Valid(None))
        (Path \ "n")
          .read[js.Dynamic, Option[Boolean]]
          .validate(lit("n" -> "bar")) shouldBe
          (Invalid(
            Seq(
              Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        (Path \ "n")
          .read[js.Dynamic, Map[String, String]]
          .validate(lit("n" -> lit("foo" -> "bar"))) shouldBe
          (Valid(Map("foo" -> "bar")))
        (Path \ "n")
          .read[js.Dynamic, Map[String, Int]]
          .validate(lit("n" -> lit("foo" -> 4, "bar" -> 5))) shouldBe
          (Valid(Map("foo" -> 4, "bar" -> 5)))
        (Path \ "x")
          .read[js.Dynamic, Map[String, Int]]
          .validate(lit("n" -> lit("foo" -> 4, "bar" -> "frack"))) shouldBe
          (Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        (Path \ "n")
          .read[js.Dynamic, Map[String, Int]]
          .validate(lit("n" -> lit("foo" -> 4, "bar" -> "frack"))) shouldBe
          (Invalid(Seq(
            Path \ "n" \ "bar" -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        (Path \ "n")
          .read[js.Dynamic, Traversable[String]]
          .validate(lit("n" -> arr("foo")))
          .toOption
          .get
          .toSeq shouldBe (Seq("foo"))
        (Path \ "n")
          .read[js.Dynamic, Traversable[Int]]
          .validate(lit("n" -> arr(1, 2, 3)))
          .toOption
          .get
          .toSeq shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[js.Dynamic, Traversable[String]]
          .validate(lit("n" -> "paf")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Array" in {
        (Path \ "n")
          .read[js.Dynamic, arr[String]]
          .validate(lit("n" -> arr("foo")))
          .toOption
          .get
          .toSeq shouldBe (Seq("foo"))
        (Path \ "n")
          .read[js.Dynamic, arr[Int]]
          .validate(lit("n" -> arr(1, 2, 3)))
          .toOption
          .get
          .toSeq shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[js.Dynamic, arr[String]]
          .validate(lit("n" -> "paf")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Seq" in {
        (Path \ "n")
          .read[js.Dynamic, Seq[String]]
          .validate(lit("n" -> arr("foo")))
          .toOption
          .get shouldBe (Seq("foo"))
        (Path \ "n")
          .read[js.Dynamic, Seq[Int]]
          .validate(lit("n" -> arr(1, 2, 3)))
          .toOption
          .get shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[js.Dynamic, Seq[String]]
          .validate(lit("n" -> "paf")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
        (Path \ "n")
          .read[js.Dynamic, Seq[String]]
          .validate(lit("n" -> arr("foo", 2))) shouldBe
          (Invalid(Seq(
            Path \ "n" \ 1 -> Seq(ValidationError("error.invalid", "String")))))
      }
    }

    "validate data" in {
      (Path \ "firstname").from[js.Dynamic](notEmpty).validate(valid) shouldBe
        (Valid("Julien"))

      val p = (Path \ "informations" \ "label")
      p.from[js.Dynamic](notEmpty).validate(valid) shouldBe (Valid("Personal"))
      p.from[js.Dynamic](notEmpty).validate(invalid) shouldBe
        (Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      (Path \ "firstname")
        .read[js.Dynamic, Option[String]]
        .validate(valid) shouldBe
        (Valid(Some("Julien")))
      (Path \ "foobar")
        .read[js.Dynamic, Option[String]]
        .validate(valid) shouldBe
        (Valid(None))
    }

    "validate deep" in {
      val p = (Path \ "informations" \ "label")

      From[js.Dynamic] { __ =>
        (__ \ "informations").read((__ \ "label").read(notEmpty))
      }.validate(valid) shouldBe (Valid("Personal"))

      From[js.Dynamic] { __ =>
        (__ \ "informations").read((__ \ "label").read(notEmpty))
      }.validate(invalid) shouldBe
        (Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[js.Dynamic] { __ =>
        (__ \ "first" \ "second").read[Option[String]]
      }.validate(null) shouldBe Valid(None)
    }

    "coerce type" in {
      (Path \ "age").read[js.Dynamic, Int].validate(valid) shouldBe (Valid(27))
      (Path \ "age").from[js.Dynamic](min(20)).validate(valid) shouldBe
        (Valid(27))
      (Path \ "age").from[js.Dynamic](max(50)).validate(valid) shouldBe
        (Valid(27))
      (Path \ "age").from[js.Dynamic](min(50)).validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
      (Path \ "age").from[js.Dynamic](max(0)).validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
      (Path \ "firstname").read[js.Dynamic, Int].validate(valid) shouldBe
        (Invalid(Seq(
          (Path \ "firstname") -> Seq(ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      import cats.syntax.semigroup._
      val composed = notEmpty |+| minLength(3)
      (Path \ "firstname").from[js.Dynamic](composed).validate(valid) shouldBe
        (Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(
        Seq(p -> Seq(ValidationError("error.required"),
                     ValidationError("error.minLength", 3))))
      p.from[js.Dynamic](composed).validate(invalid) shouldBe (err)
    }

    "compose validations" in {
      From[js.Dynamic] { __ =>
        ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty)).tupled
      }.validate(valid) shouldBe Valid("Julien" -> "Tournay")

      From[js.Dynamic] { __ =>
        ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
          (__ \ "informations" \ "label").read(notEmpty)).tupled
      }.validate(invalid) shouldBe Invalid(
        Seq((Path \ "informations" \ "label") -> Seq(
          ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      (Path \ "foo")
        .from[js.Dynamic](seqR(notEmpty))
        .validate(lit("foo" -> arr("bar")))
        .toOption
        .get shouldBe (Seq("bar"))

      From[js.Dynamic] { __ =>
        (__ \ "foo").read((__ \ "foo").read(seqR(notEmpty)))
      }.validate(lit("foo" -> lit("foo" -> arr("bar"))))
        .toOption
        .get shouldBe (Seq("bar"))

      (Path \ "n")
        .from[js.Dynamic](seqR(notEmpty))
        .validate(lit("n" -> arr("foo", ""))) shouldBe
        (Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      val v =
        lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "s3cr3t")

      val i1 = lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "")

      val i2 =
        lit("login" -> "Alice", "password" -> "s3cr3t", "verify" -> "bam")

      val passRule = From[js.Dynamic] { __ =>
        (
          (__ \ "password").read(notEmpty) ~
            (__ \ "verify").read(notEmpty)
        ).tupled.andThen(Rule.uncurry(Rules.equalTo[String]))
      }

      val rule = From[js.Dynamic] { __ =>
        ((__ \ "login").read(notEmpty) ~ passRule).tupled
      }

      rule.validate(v).shouldBe(Valid("Alice" -> "s3cr3t"))
      rule
        .validate(i1)
        .shouldBe(Invalid(
          Seq(Path \ "verify" -> Seq(ValidationError("error.required")))))
      rule
        .validate(i2)
        .shouldBe(Invalid(Seq(
          Path \ "verify" -> Seq(ValidationError("error.equals", "s3cr3t")))))
    }

    "validate subclasses (and parse the concrete class)" when {

      trait A
      case class B(foo: Int) extends A
      case class C(bar: Int) extends A

      val b = lit("name" -> "B", "foo" -> 4)
      val c = lit("name" -> "C", "bar" -> 6)
      val e = lit("name" -> "E", "eee" -> 6)

      val typeInvalid =
        Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "trying all possible Rules" in {

        val rb: Rule[js.Dynamic, A] = From[js.Dynamic] { __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo")
            .read[Int]
            .map(B.apply)
        }

        val rc: Rule[js.Dynamic, A] = From[js.Dynamic] { __ =>
          (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar")
            .read[Int]
            .map(C.apply)
        }

        val rule = rb orElse rc orElse Rule(_ => typeInvalid)

        rule.validate(b) shouldBe (Valid(B(4)))
        rule.validate(c) shouldBe (Valid(C(6)))
        rule.validate(e) shouldBe
          (Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

      "dicriminating on fields" in {

        val rule = From[js.Dynamic] { __ =>
          (__ \ "name").read[String].flatMap[A] {
            case "B" => (__ \ "foo").read[Int].map(B.apply)
            case "C" => (__ \ "bar").read[Int].map(C.apply)
            case _   => Rule(_ => typeInvalid)
          }
        }

        rule.validate(b) shouldBe (Valid(B(4)))
        rule.validate(c) shouldBe (Valid(C(6)))
        rule.validate(e) shouldBe
          (Invalid(
            Seq(
              Path \ "name" -> Seq(ValidationError("validation.unknownType")))))
      }
    }

    "perform complex validation" in {

      case class Contact(firstname: String,
                         lastname: String,
                         company: Option[String],
                         informations: Seq[ContactInformation])

      case class ContactInformation(label: String,
                                    email: Option[String],
                                    phones: Seq[String])

      val validJson = lit(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" -> arr(
          lit("label" -> "Personal",
              "email" -> "fakecontact@gmail.com",
              "phones" -> arr("01.23.45.67.89", "98.76.54.32.10")))
      )

      val invalidJson = lit(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" -> arr(
          lit("label" -> "",
              "email" -> "fakecontact@gmail.com",
              "phones" -> arr("01.23.45.67.89", "98.76.54.32.10")))
      )

      val infoValidated = From[js.Dynamic] { __ =>
        ((__ \ "label").read(notEmpty) ~ (__ \ "email").read(optionR(email)) ~
          (__ \ "phones").read(seqR(notEmpty)))(ContactInformation.apply)
      }

      val contactValidated = From[js.Dynamic] { __ =>
        ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
          (__ \ "company").read[Option[String]] ~ (__ \ "informations").read(
          seqR(infoValidated)))(Contact.apply)
      }

      val expected =
        Contact("Julien",
                "Tournay",
                None,
                Seq(
                  ContactInformation("Personal",
                                     Some("fakecontact@gmail.com"),
                                     List("01.23.45.67.89", "98.76.54.32.10"))))

      contactValidated.validate(validJson) shouldBe (Valid(expected))
      contactValidated.validate(invalidJson) shouldBe
        (Invalid(
          Seq((Path \ "informations" \ 0 \ "label") -> Seq(
            ValidationError("error.required")))))
    }

    "read recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      val m = lit("name" -> "bob",
                  "friends" -> arr(lit("name" -> "tom", "friends" -> arr())))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 =
        lit("name" -> "bob", "friend" -> lit("name" -> "tom"))

      "using explicit notation" in {
        lazy val w: Rule[js.Dynamic, RecUser] = From[js.Dynamic] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friends").read(seqR(w)))(
            RecUser.apply)
        }
        w.validate(m) shouldBe Valid(u)

        lazy val w2: Rule[js.Dynamic, RecUser] =
          ((Path \ "name").read[js.Dynamic, String] ~ (Path \ "friends")
            .from[js.Dynamic](seqR(w2)))(RecUser.apply)
        w2.validate(m) shouldBe Valid(u)

        lazy val w3: Rule[js.Dynamic, User1] = From[js.Dynamic] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friend").read(optionR(w3)))(
            User1.apply)
        }
        w3.validate(m1) shouldBe Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[js.Dynamic, RecUser] = From[js.Dynamic] {
          __ =>
            ((__ \ "name").read[String] ~ (__ \ "friends").read[Seq[RecUser]])(
              RecUser.apply)
        }
        w.validate(m) shouldBe Valid(u)

        implicit lazy val w3: Rule[js.Dynamic, User1] = From[js.Dynamic] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friend").read[Option[User1]])(
            User1.apply)
        }
        w3.validate(m1) shouldBe Valid(u1)
      }
    }

    "completely generic" in {
      type OptString[In] =
        Rule[String, String] => Path => Rule[In, Option[String]]

      def genR[In](opt: OptString[In])(implicit exs: Path => Rule[In, String]) =
        From[In] { __ =>
          ((__ \ "name").read(notEmpty) ~ (__ \ "color").read(opt(notEmpty))).tupled
        }

      val jsonR = {
        genR[js.Dynamic](optionR(_))
      }

      val json = lit("name" -> "bob", "color" -> "blue")
      val invalidJson = lit("color" -> "blue")

      jsonR.validate(json) shouldBe Valid(("bob", Some("blue")))
      jsonR.validate(invalidJson) shouldBe Invalid(
        Seq((Path \ "name", Seq(ValidationError("error.required")))))
    }
  }
}
