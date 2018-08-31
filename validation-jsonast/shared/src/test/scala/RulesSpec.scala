import jto.validation._
import jto.validation.jsonast._
import org.scalatest._

class RulesSpec extends WordSpec with Matchers {

  "Json Rules" should {

    import Rules._

    val valid = JObject(
        Map("firstname" -> JString("Julien"),
            "lastname" -> JString("Tournay"),
            "age" -> JNumber(27),
            "informations" -> JObject(
                Map("label" -> JString("Personal"),
                    "email" -> JString("fakecontact@gmail.com"),
                    "phones" -> JArray(Seq(JString("01.23.45.67.89"),
                                       JString("98.76.54.32.10")))))))

    val invalid = JObject(
        Map("firstname" -> JString("Julien"),
            "lastname" -> JString("Tournay"),
            "age" -> JNumber(27),
            "informations" -> JObject(
                Map("label" -> JString(""),
                    "email" -> JString("fakecontact@gmail.com"),
                    "phones" -> JArray(Seq(JString("01.23.45.67.89"),
                                       JString("98.76.54.32.10")))))))

    "extract data" in {
      (Path \ "firstname").read[JValue, String].validate(valid) shouldBe (Valid(
              "Julien"))
      val errPath = Path \ "foo"
      val error =
        Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[JValue, String].validate(invalid) shouldBe (error)
    }

    "support checked" in {
      val js = JObject(Map("issmth" -> JBoolean(true)))
      val p = Path \ "issmth"
      p.from[JValue](checked).validate(js) shouldBe (Valid(true))
      p.from[JValue](checked).validate(JObject(Map())) shouldBe (Invalid(
              Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
      p.from[JValue](checked)
        .validate(JObject(Map("issmth" -> JBoolean(false)))) shouldBe (Invalid(
              Seq(Path \ "issmth" -> Seq(
                      ValidationError("error.equals", true)))))
    }

    "support all types of Json values" when {

      "null" in {
        (Path \ "n")
          .read[JValue, JNull.type]
          .validate(JObject(Map("n" -> JNull))) shouldBe (Valid(JNull))
        (Path \ "n")
          .read[JValue, JNull.type]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "null")))))
        (Path \ "n")
          .read[JValue, JNull.type]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "null")))))
      }

      "Int" in {
        (Path \ "n")
          .read[JValue, Int]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(4))
        (Path \ "n")
          .read[JValue, Int]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Int")))))
        (Path \ "n")
          .read[JValue, Int]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Int")))))
        (Path \ "n" \ "o")
          .read[JValue, Int]
          .validate(JObject(Map("n" -> JObject(Map("o" -> JNumber(4)))))) shouldBe (Valid(
                4))
        (Path \ "n" \ "o")
          .read[JValue, Int]
          .validate(JObject(Map("n" -> JObject(Map("o" -> JString("foo")))))) shouldBe (Invalid(
                Seq(Path \ "n" \ "o" -> Seq(
                        ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o" \ "p")
          .read[JValue, Int]
          .validate(JObject(Map("n" -> JObject(Map("o" -> JObject(
                                  Map("p" -> JNumber(4)))))))) shouldBe (Valid(
                4))
        (Path \ "n" \ "o" \ "p")
          .read[JValue, Int]
          .validate(
              JObject(Map("n" -> JObject(Map("o" -> JObject(Map("p" -> JString(
                                          "foo")))))))) shouldBe (Invalid(
                Seq(Path \ "n" \ "o" \ "p" -> Seq(
                        ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error =
          Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[JValue, Int].validate(JObject(Map("n" -> JNumber(4)))) shouldBe (error)
      }

      "Short" in {
        (Path \ "n")
          .read[JValue, Short]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(4))
        (Path \ "n")
          .read[JValue, Short]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Short")))))
        (Path \ "n")
          .read[JValue, Short]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Short")))))
      }

      "Long" in {
        (Path \ "n")
          .read[JValue, Long]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(4))
        (Path \ "n")
          .read[JValue, Long]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Long")))))
        (Path \ "n")
          .read[JValue, Long]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Long")))))
      }

      "Float" in {
        (Path \ "n")
          .read[JValue, Float]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(4))
        (Path \ "n")
          .read[JValue, Float]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Float")))))
        (Path \ "n")
          .read[JValue, Float]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Valid(4.5F))
      }

      "Double" in {
        (Path \ "n")
          .read[JValue, Double]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(4))
        (Path \ "n")
          .read[JValue, Double]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Double")))))
        (Path \ "n")
          .read[JValue, Double]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        (Path \ "n")
          .read[JValue, jBigDecimal]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(
                new jBigDecimal("4")))
        (Path \ "n")
          .read[JValue, jBigDecimal]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "BigDecimal")))))
        (Path \ "n")
          .read[JValue, jBigDecimal]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Valid(
                new jBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        (Path \ "n")
          .read[JValue, BigDecimal]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(
                BigDecimal(4)))
        (Path \ "n")
          .read[JValue, BigDecimal]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "BigDecimal")))))
        (Path \ "n")
          .read[JValue, BigDecimal]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Valid(
                BigDecimal(4.5)))
      }

      "Boolean" in {
        (Path \ "n")
          .read[JValue, Boolean]
          .validate(JObject(Map("n" -> JBoolean(true)))) shouldBe (Valid(true))
        (Path \ "n")
          .read[JValue, Boolean]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        (Path \ "n")
          .read[JValue, String]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Valid(
                "foo"))
        (Path \ "n")
          .read[JValue, String]
          .validate(JObject(Map("n" -> JNumber(42)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "String")))))
        (Path \ "n")
          .read[JValue, String]
          .validate(JObject(Map("n" -> JArray(Seq(JString("foo")))))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "String")))))
        (Path \ "o")
          .read[JValue, String]
          .validate(JObject(Map("o" -> JObject(Map("n" -> JString("foo")))))) shouldBe (Invalid(
                Seq(Path \ "o" -> Seq(
                        ValidationError("error.invalid", "String")))))
      }

      "JObject" in {
        (Path \ "o")
          .read[JValue, JObject]
          .validate(JObject(Map("o" -> JObject(Map("n" -> JString("foo")))))) shouldBe (Valid(
                JObject(Map("n" -> JString("foo")))))
        (Path \ "n")
          .read[JValue, JObject]
          .validate(JObject(Map("n" -> JNumber(42)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Object")))))
        (Path \ "n")
          .read[JValue, JObject]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Object")))))
        (Path \ "n")
          .read[JValue, JObject]
          .validate(JObject(Map("n" -> JArray(Seq(JString("foo")))))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Object")))))
      }

      "JString" in {
        (Path \ "n")
          .read[JValue, JString]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Valid(
                JString("foo")))
        (Path \ "n")
          .read[JValue, JString]
          .validate(JObject(Map("n" -> JNumber(42)))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "String")))))
      }

      "JsNumber" in {
        (Path \ "n")
          .read[JValue, JNumber]
          .validate(JObject(Map("n" -> JNumber(4)))) shouldBe (Valid(
                JNumber(4)))
        (Path \ "n")
          .read[JValue, JNumber]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.number", "Number")))))
        (Path \ "n")
          .read[JValue, JNumber]
          .validate(JObject(Map("n" -> JNumber(4.5)))) shouldBe (Valid(
                JNumber(4.5)))
      }

      "JBoolean" in {
        (Path \ "n")
          .read[JValue, JBoolean]
          .validate(JObject(Map("n" -> JBoolean(true)))) shouldBe (Valid(
                JBoolean(true)))
        (Path \ "n")
          .read[JValue, JBoolean]
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Boolean")))))
      }

      "Option" in {
        (Path \ "n")
          .read[JValue, Option[Boolean]]
          .validate(JObject(Map("n" -> JBoolean(true)))) shouldBe (Valid(
                Some(true)))
        (Path \ "n")
          .read[JValue, Option[Boolean]]
          .validate(JObject(Map("n" -> JNull))) shouldBe (Valid(None))
        (Path \ "n")
          .read[JValue, Option[Boolean]]
          .validate(JObject(Map("foo" -> JString("bar")))) shouldBe (Valid(
                None))
        (Path \ "n")
          .read[JValue, Option[Boolean]]
          .validate(JObject(Map("n" -> JString("bar")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        (Path \ "n")
          .read[JValue, Map[String, String]]
          .validate(JObject(Map("n" -> JObject(Map("foo" -> JString("bar")))))) shouldBe (Valid(
                Map("foo" -> "bar")))
        (Path \ "n")
          .read[JValue, Map[String, Int]]
          .validate(JObject(Map("n" -> JObject(
                          Map("foo" -> JNumber(4),
                              "bar" -> JNumber(5)))))) shouldBe (Valid(
                Map("foo" -> 4, "bar" -> 5)))
        (Path \ "x")
          .read[JValue, Map[String, Int]]
          .validate(JObject(Map("n" -> JObject(
                          Map("foo" -> JNumber(4),
                              "bar" -> JString("frack")))))) shouldBe (Invalid(
                Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        (Path \ "n")
          .read[JValue, Map[String, Int]]
          .validate(JObject(Map("n" -> JObject(
                          Map("foo" -> JNumber(4),
                              "bar" -> JString("frack")))))) shouldBe (Invalid(
                Seq(Path \ "n" \ "bar" -> Seq(
                        ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        (Path \ "n")
          .read[JValue, Traversable[String]]
          .validate(JObject(Map("n" -> JArray(Seq(JString("foo"))))))
          .toOption
          .get
          .toSeq shouldBe (Seq("foo"))
        (Path \ "n")
          .read[JValue, Traversable[Int]]
          .validate(
              JObject(Map("n" -> JArray(Seq(JNumber(1), JNumber(2), JNumber(3))))))
          .toOption
          .get
          .toSeq shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[JValue, Traversable[String]]
          .validate(JObject(Map("n" -> JString("paf")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Array")))))
      }

      "Array" in {
        (Path \ "n")
          .read[JValue, Array[String]]
          .validate(JObject(Map("n" -> JArray(Seq(JString("foo"))))))
          .toOption
          .get
          .toSeq shouldBe (Seq("foo"))
        (Path \ "n")
          .read[JValue, Array[Int]]
          .validate(
              JObject(Map("n" -> JArray(Seq(JNumber(1), JNumber(2), JNumber(3))))))
          .toOption
          .get
          .toSeq shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[JValue, Array[String]]
          .validate(JObject(Map("n" -> JString("paf")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Array")))))
      }

      "Seq" in {
        (Path \ "n")
          .read[JValue, Seq[String]]
          .validate(JObject(Map("n" -> JArray(Seq(JString("foo"))))))
          .toOption
          .get shouldBe (Seq("foo"))
        (Path \ "n")
          .read[JValue, Seq[Int]]
          .validate(
              JObject(Map("n" -> JArray(Seq(JNumber(1), JNumber(2), JNumber(3))))))
          .toOption
          .get shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[JValue, Seq[String]]
          .validate(JObject(Map("n" -> JString("paf")))) shouldBe (Invalid(
                Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Array")))))
        (Path \ "n")
          .read[JValue, Seq[String]]
          .validate(JObject(Map("n" -> JArray(Seq(JString("foo"), JNumber(2)))))) shouldBe (Invalid(
                Seq(Path \ "n" \ 1 -> Seq(
                        ValidationError("error.invalid", "String")))))
      }
    }

    "validate data" in {
      (Path \ "firstname").from[JValue](notEmpty).validate(valid) shouldBe (Valid(
              "Julien"))

      val p = (Path \ "informations" \ "label")
      p.from[JValue](notEmpty).validate(valid) shouldBe (Valid("Personal"))
      p.from[JValue](notEmpty).validate(invalid) shouldBe (Invalid(
              Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      (Path \ "firstname").read[JValue, Option[String]].validate(valid) shouldBe (Valid(
              Some("Julien")))
      (Path \ "foobar").read[JValue, Option[String]].validate(valid) shouldBe (Valid(
              None))
    }

    "validate deep" in {
      val p = (Path \ "informations" \ "label")

      From[JValue] { __ =>
        (__ \ "informations").read((__ \ "label").read(notEmpty))
      }.validate(valid) shouldBe (Valid("Personal"))

      From[JValue] { __ =>
        (__ \ "informations").read((__ \ "label").read(notEmpty))
      }.validate(invalid) shouldBe (Invalid(
              Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[JValue] { __ =>
        (__ \ "first" \ "second").read[Option[String]]
      } validate (JNull) shouldBe Valid(None)
    }

    "coerce type" in {
      (Path \ "age").read[JValue, Int].validate(valid) shouldBe (Valid(27))
      (Path \ "age").from[JValue](min(20)).validate(valid) shouldBe (Valid(27))
      (Path \ "age").from[JValue](max(50)).validate(valid) shouldBe (Valid(27))
      (Path \ "age").from[JValue](min(50)).validate(valid) shouldBe (Invalid(
              Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
      (Path \ "age").from[JValue](max(0)).validate(valid) shouldBe (Invalid(
              Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
      (Path \ "firstname").read[JValue, Int].validate(valid) shouldBe (Invalid(
              Seq((Path \ "firstname") -> Seq(
                      ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      val composed = notEmpty |+| minLength(3)
      (Path \ "firstname").from[JValue](composed).validate(valid) shouldBe (Valid(
              "Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"),
                                     ValidationError("error.minLength", 3))))
      p.from[JValue](composed).validate(invalid) shouldBe (err)
    }

    "compose validations" in {
      From[JValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty)).tupled
      }.validate(valid) shouldBe Valid("Julien" -> "Tournay")

      From[JValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty) ~
            (__ \ "informations" \ "label").read(notEmpty)).tupled
      }.validate(invalid) shouldBe Invalid(
          Seq((Path \ "informations" \ "label") -> Seq(
                  ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      (Path \ "foo")
        .from[JValue](seqR(notEmpty))
        .validate(JObject(Map("foo" -> JArray(Seq(JString("bar"))))))
        .toOption
        .get shouldBe (Seq("bar"))

      From[JValue] { __ =>
        (__ \ "foo").read((__ \ "foo").read(seqR(notEmpty)))
      }.validate(JObject(
                Map("foo" -> JObject(Map("foo" -> JArray(Seq(JString("bar"))))))))
        .toOption
        .get shouldBe (Seq("bar"))

      (Path \ "n")
        .from[JValue](seqR(notEmpty))
        .validate(JObject(Map("n" -> JArray(Seq(JString("foo"), JString("")))))) shouldBe (Invalid(
              Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      val v = JObject(Map("login" -> JString("Alice"),
                          "password" -> JString("s3cr3t"),
                          "verify" -> JString("s3cr3t")))

      val i1 = JObject(Map("login" -> JString("Alice"),
                           "password" -> JString("s3cr3t"),
                           "verify" -> JString("")))

      val i2 = JObject(Map("login" -> JString("Alice"),
                           "password" -> JString("s3cr3t"),
                           "verify" -> JString("bam")))

      val passRule = From[JValue] { __ =>
        ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty)).tupled
          .andThen(
            Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
      }

      val rule = From[JValue] { __ =>
        ((__ \ "login").read(notEmpty) ~ passRule).tupled
      }

      rule.validate(v).shouldBe(Valid("Alice" -> "s3cr3t"))
      rule
        .validate(i1)
        .shouldBe(Invalid(Seq(Path \ "verify" -> Seq(
                        ValidationError("error.required")))))
      rule
        .validate(i2)
        .shouldBe(Invalid(Seq(Path \ "verify" -> Seq(
                        ValidationError("error.equals", "s3cr3t")))))
    }

    "validate subclasses (and parse the concrete class)" when {

      trait A
      case class B(foo: Int) extends A
      case class C(bar: Int) extends A

      val b = JObject(Map("name" -> JString("B"), "foo" -> JNumber(4)))
      val c = JObject(Map("name" -> JString("C"), "bar" -> JNumber(6)))
      val e = JObject(Map("name" -> JString("E"), "eee" -> JNumber(6)))

      val typeInvalid =
        Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        import cats.syntax.apply._

        val rb: Rule[JValue, A] = From[JValue] { __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo")
            .read[Int]
            .map(B.apply)
        }

        val rc: Rule[JValue, A] = From[JValue] { __ =>
          (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar")
            .read[Int]
            .map(C.apply)
        }

        val rule = rb orElse rc orElse Rule(_ => typeInvalid)

        rule.validate(b) shouldBe (Valid(B(4)))
        rule.validate(c) shouldBe (Valid(C(6)))
        rule.validate(e) shouldBe (Invalid(
                Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

      "by dicriminating on fields" in {

        val rule = From[JValue] { __ =>
          (__ \ "name").read[String].flatMap[A] {
            case "B" => (__ \ "foo").read[Int].map(B.apply)
            case "C" => (__ \ "bar").read[Int].map(C.apply)
            case _ => Rule(_ => typeInvalid)
          }
        }

        rule.validate(b) shouldBe (Valid(B(4)))
        rule.validate(c) shouldBe (Valid(C(6)))
        rule.validate(e) shouldBe (Invalid(
                Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }
    }

    "perform complex validation" in {

      case class Contact(firstname: String,
                         lastname: String,
                         company: Option[String],
                         informations: Seq[ContactInformation])

      case class ContactInformation(
          label: String, email: Option[String], phones: Seq[String])

      val validJson = JObject(
          Map("firstname" -> JString("Julien"),
              "lastname" -> JString("Tournay"),
              "age" -> JNumber(27),
              "informations" -> JArray(Seq(JObject(
                      Map("label" -> JString("Personal"),
                          "email" -> JString("fakecontact@gmail.com"),
                          "phones" -> JArray(Seq(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))))

      val invalidJson = JObject(
          Map("firstname" -> JString("Julien"),
              "lastname" -> JString("Tournay"),
              "age" -> JNumber(27),
              "informations" -> JArray(Seq(JObject(
                      Map("label" -> JString(""),
                          "email" -> JString("fakecontact@gmail.com"),
                          "phones" -> JArray(Seq(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))))

      val infoValidated = From[JValue] { __ =>
        ((__ \ "label").read(notEmpty) ~
            (__ \ "email").read(optionR(email)) ~
            (__ \ "phones").read(seqR(notEmpty)))(ContactInformation.apply)
      }

      val contactValidated = From[JValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty) ~
            (__ \ "company").read[Option[String]] ~
            (__ \ "informations").read(seqR(infoValidated)))(Contact.apply)
      }

      val expected = Contact(
          "Julien",
          "Tournay",
          None,
          Seq(ContactInformation("Personal",
                                 Some("fakecontact@gmail.com"),
                                 List("01.23.45.67.89", "98.76.54.32.10"))))

      contactValidated.validate(validJson) shouldBe (Valid(expected))
      contactValidated.validate(invalidJson) shouldBe (Invalid(
              Seq((Path \ "informations" \ 0 \ "label") -> Seq(
                      ValidationError("error.required")))))
    }

    "read recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      val m = JObject(
          Map("name" -> JString("bob"),
              "friends" -> JArray(Seq(JObject(Map("name" -> JString("tom"), "friends" -> JArray()))))))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(Map("name" -> JString("bob"),
                           "friend" -> JObject(Map("name" -> JString("tom")))))

      "using explicit notation" in {
        lazy val w: Rule[JValue, RecUser] = From[JValue] { __ =>
          ((__ \ "name").read[String] ~
              (__ \ "friends").read(seqR(w)))(RecUser.apply)
        }
        w.validate(m) shouldBe Valid(u)

        lazy val w2: Rule[JValue, RecUser] =
          ((Path \ "name").read[JValue, String] ~
              (Path \ "friends").from[JValue](seqR(w2)))(RecUser.apply)
        w2.validate(m) shouldBe Valid(u)

        lazy val w3: Rule[JValue, User1] = From[JValue] { __ =>
          ((__ \ "name").read[String] ~
              (__ \ "friend").read(optionR(w3)))(User1.apply)
        }
        w3.validate(m1) shouldBe Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[JValue, RecUser] = From[JValue] { __ =>
          ((__ \ "name").read[String] ~
              (__ \ "friends").read[Seq[RecUser]])(RecUser.apply)
        }
        w.validate(m) shouldBe Valid(u)

        implicit lazy val w3: Rule[JValue, User1] = From[JValue] { __ =>
          ((__ \ "name").read[String] ~
              (__ \ "friend").read[Option[User1]])(User1.apply)
        }
        w3.validate(m1) shouldBe Valid(u1)
      }
    }

    "completely generic" in {
      type OptString[In] =
        Rule[String, String] => Path => Rule[In, Option[String]]

      def genR[In](opt: OptString[In])(
          implicit exs: Path => Rule[In, String]) =
        From[In] { __ =>
          ((__ \ "name").read(notEmpty) ~
              (__ \ "color").read(opt(notEmpty))).tupled
        }

      val jsonR = {
        genR[JValue](optionR(_))
      }

      val json =
        JObject(Map("name" -> JString("bob"), "color" -> JString("blue")))
      val invalidJson = JObject(Map("color" -> JString("blue")))

      jsonR.validate(json) shouldBe Valid(("bob", Some("blue")))
      jsonR.validate(invalidJson) shouldBe Invalid(
          Seq((Path \ "name", Seq(ValidationError("error.required")))))
    }
  }
}
