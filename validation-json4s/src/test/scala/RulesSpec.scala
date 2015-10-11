import jto.validation._
import jto.validation.json4s._
import org.specs2.mutable._
import org.json4s.ast.safe._

object RulesSpec extends Specification {

  "Json Rules" should {
    
    import Rules._

    val valid = JObject(Map(
      "firstname" -> JString("Julien"),
      "lastname" -> JString("Tournay"),
      "age" -> JNumber(27),
      "informations" -> JObject(Map(
        "label" -> JString("Personal"),
        "email" -> JString("fakecontact@gmail.com"),
        "phones" -> JArray(JString("01.23.45.67.89"), JString("98.76.54.32.10"))))))

    val invalid = JObject(Map(
      "firstname" -> JString("Julien"),
      "lastname" -> JString("Tournay"),
      "age" -> JNumber(27),
      "informations" -> JObject(Map(
        "label" -> JString(""),
        "email" -> JString("fakecontact@gmail.com"),
        "phones" -> JArray(JString("01.23.45.67.89"), JString("98.76.54.32.10"))))))

    "extract data" in {
      (Path \ "firstname").read[JValue, String].validate(valid) mustEqual(Valid("Julien"))
      val errPath = Path \ "foo"
      val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[JValue, String].validate(invalid) mustEqual(error)
    }

    "support checked" in {
      val js = JObject(Map("issmth" -> JBoolean(true)))
      val p = Path \ "issmth"
      p.from[JValue](checked).validate(js) mustEqual(Valid(true))
      p.from[JValue](checked).validate(JObject(Map())) mustEqual(Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
      p.from[JValue](checked).validate(JObject(Map("issmth" -> JBoolean(false)))) mustEqual(Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.equals", true)))))
    }

    "support all types of Json values" in {

      "null" in {
        (Path \ "n").read[JValue, JNull.type].validate(JObject(Map("n" -> JNull))) mustEqual(Valid(JNull))
        (Path \ "n").read[JValue, JNull.type].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
        (Path \ "n").read[JValue, JNull.type].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
      }

      "Int" in {
        (Path \ "n").read[JValue, Int].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(4))
        (Path \ "n").read[JValue, Int].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n").read[JValue, Int].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n" \ "o").read[JValue, Int].validate(JObject(Map("n" -> JObject(Map("o" -> JNumber(4)))))) mustEqual(Valid(4))
        (Path \ "n" \ "o").read[JValue, Int].validate(JObject(Map("n" -> JObject(Map("o" -> JString("foo")))))) mustEqual(Invalid(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o" \ "p" ).read[JValue, Int].validate(JObject(Map("n" -> JObject(Map("o" -> JObject(Map("p" -> JNumber(4)))))))) mustEqual(Valid(4))
        (Path \ "n" \ "o" \ "p").read[JValue, Int].validate(JObject(Map("n" -> JObject(Map("o" -> JObject(Map("p" -> JString("foo")))))))) mustEqual(Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[JValue, Int].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(error)
      }

      "Short" in {
        (Path \ "n").read[JValue, Short].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(4))
        (Path \ "n").read[JValue, Short].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        (Path \ "n").read[JValue, Short].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        (Path \ "n").read[JValue, Long].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(4))
        (Path \ "n").read[JValue, Long].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        (Path \ "n").read[JValue, Long].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        (Path \ "n").read[JValue, Float].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(4))
        (Path \ "n").read[JValue, Float].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        (Path \ "n").read[JValue, Float].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Valid(4.8F))
      }

      "Double" in {
        (Path \ "n").read[JValue, Double].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(4))
        (Path \ "n").read[JValue, Double].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        (Path \ "n").read[JValue, Double].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Valid(4.8))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        (Path \ "n").read[JValue, jBigDecimal].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(new jBigDecimal("4")))
        (Path \ "n").read[JValue, jBigDecimal].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n").read[JValue, jBigDecimal].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Valid(new jBigDecimal("4.8")))
      }

      "scala BigDecimal" in {
        (Path \ "n").read[JValue, BigDecimal].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(BigDecimal(4)))
        (Path \ "n").read[JValue, BigDecimal].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n").read[JValue, BigDecimal].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Valid(BigDecimal(4.8)))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n").from[JValue](Rules.date).validate(JObject(Map("n" -> JString("1985-09-10")))) mustEqual(Valid(f.parse("1985-09-10")))
        (Path \ "n").from[JValue](Rules.date).validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date" in {
        skipped("Can't test on CI")
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n").from[JValue](Rules.isoDate).validate(JObject(Map("n" -> JString("1985-09-10T00:00:00+02:00")))) mustEqual(Valid(f.parse("1985-09-10")))
        (Path \ "n").from[JValue](Rules.isoDate).validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date.isoformat")))))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n").from[JValue](Rules.jodaDate).validate(JObject(Map("n" -> JString("1985-09-10")))) mustEqual(Valid(jd))
          (Path \ "n").from[JValue](Rules.jodaDate).validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          (Path \ "n").from[JValue](Rules.jodaTime).validate(JObject(Map("n" -> JNumber(dd.getTime)))) mustEqual(Valid(jd))
          (Path \ "n").from[JValue](Rules.jodaDate).validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n").from[JValue](Rules.jodaLocalDate).validate(JObject(Map("n" -> JString(ld.toString())))) mustEqual(Valid(ld))
          (Path \ "n").from[JValue](Rules.jodaLocalDate).validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n").from[JValue](Rules.sqlDate).validate(JObject(Map("n" -> JString("1985-09-10")))) mustEqual(Valid(ds))
      }

      "Boolean" in {
        (Path \ "n").read[JValue, Boolean].validate(JObject(Map("n" -> JBoolean(true)))) mustEqual(Valid(true))
        (Path \ "n").read[JValue, Boolean].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        (Path \ "n").read[JValue, String].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Valid("foo"))
        (Path \ "n").read[JValue, String].validate(JObject(Map("n" -> JNumber(42)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "n").read[JValue, String].validate(JObject(Map("n" -> JArray(JString("foo"))))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "o").read[JValue, String].validate(JObject(Map("o" -> JObject(Map("n" -> JString("foo")))))) mustEqual(Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "JObject" in {
        (Path \ "o").read[JValue, JObject].validate(JObject(Map("o" -> JObject(Map("n" -> JString("foo")))))) mustEqual(Valid(JObject(Map("n" -> JString("foo")))))
        (Path \ "n").read[JValue, JObject].validate(JObject(Map("n" -> JNumber(42)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n").read[JValue, JObject].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n").read[JValue, JObject].validate(JObject(Map("n" -> JArray(JString("foo"))))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
      }

      "JString" in {
        (Path \ "n").read[JValue, JString].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Valid(JString("foo")))
        (Path \ "n").read[JValue, JString].validate(JObject(Map("n" -> JNumber(42)))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "JsNumber" in {
        (Path \ "n").read[JValue, JNumber].validate(JObject(Map("n" -> JNumber(4)))) mustEqual(Valid(JNumber(4)))
        (Path \ "n").read[JValue, JNumber].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Number")))))
        (Path \ "n").read[JValue, JNumber].validate(JObject(Map("n" -> JNumber(4.8)))) mustEqual(Valid(JNumber(4.8)))
      }

      "JBoolean" in {
        (Path \ "n").read[JValue, JBoolean].validate(JObject(Map("n" -> JBoolean(true)))) mustEqual(Valid(JBoolean(true)))
        (Path \ "n").read[JValue, JBoolean].validate(JObject(Map("n" -> JString("foo")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Option" in {
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject(Map("n" -> JBoolean(true)))) mustEqual(Valid(Some(true)))
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject(Map("n" -> JNull))) mustEqual(Valid(None))
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject(Map("foo" -> JString("bar")))) mustEqual(Valid(None))
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject(Map("n" -> JString("bar")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        (Path \ "n").read[JValue, Map[String, String]].validate(JObject(Map("n" -> JObject(Map("foo" -> JString("bar")))))) mustEqual(Valid(Map("foo" -> "bar")))
        (Path \ "n").read[JValue, Map[String, Int]].validate(JObject(Map("n" -> JObject(Map("foo" -> JNumber(4), "bar" -> JNumber(5)))))) mustEqual(Valid(Map("foo" -> 4, "bar" -> 5)))
        (Path \ "x").read[JValue, Map[String, Int]].validate(JObject(Map("n" -> JObject(Map("foo" -> JNumber(4), "bar" -> JString("frack")))))) mustEqual(Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        (Path \ "n").read[JValue, Map[String, Int]].validate(JObject(Map("n" -> JObject(Map("foo" -> JNumber(4), "bar" -> JString("frack")))))) mustEqual(Invalid(Seq(Path \ "n" \ "bar" -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        (Path \ "n").read[JValue, Traversable[String]].validate(JObject(Map("n" -> JArray(JString("foo"))))).toOption.get.toSeq must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JValue, Traversable[Int]].validate(JObject(Map("n" -> JArray(JNumber(1), JNumber(2), JNumber(3))))).toOption.get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JValue, Traversable[String]].validate(JObject(Map("n" -> JString("paf")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Array" in {
        (Path \ "n").read[JValue, Array[String]].validate(JObject(Map("n" -> JArray(JString("foo"))))).toOption.get.toSeq must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JValue, Array[Int]].validate(JObject(Map("n" -> JArray(JNumber(1), JNumber(2), JNumber(3))))).toOption.get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JValue, Array[String]].validate(JObject(Map("n" -> JString("paf")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Seq" in {
        (Path \ "n").read[JValue, Seq[String]].validate(JObject(Map("n" -> JArray(JString("foo"))))).toOption.get must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JValue, Seq[Int]].validate(JObject(Map("n" -> JArray(JNumber(1), JNumber(2), JNumber(3))))).toOption.get must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JValue, Seq[String]].validate(JObject(Map("n" -> JString("paf")))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
        (Path \ "n").read[JValue, Seq[String]].validate(JObject(Map("n" -> JArray(JString("foo"), JNumber(2))))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.invalid", "String")))))
      }

    }

    "validate data" in {
      (Path \ "firstname").from[JValue](notEmpty).validate(valid) mustEqual(Valid("Julien"))

      val p = (Path \ "informations" \ "label")
      p.from[JValue](notEmpty).validate(valid) mustEqual(Valid("Personal"))
      p.from[JValue](notEmpty).validate(invalid) mustEqual(Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      (Path \ "firstname").read[JValue, Option[String]].validate(valid) mustEqual(Valid(Some("Julien")))
      (Path \ "foobar").read[JValue, Option[String]].validate(valid) mustEqual(Valid(None))
    }

    "validate deep" in {
      val p = (Path \ "informations" \ "label")

      From[JValue] { __ =>
        (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(valid) mustEqual(Valid("Personal"))

      From[JValue] { __ =>
        (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(invalid) mustEqual(Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[JValue]{ __ =>
        (__ \ "first" \ "second").read[Option[String]]
      }validate(JNull) mustEqual Valid(None)
    }

    "coerce type" in {
      (Path \ "age").read[JValue, Int].validate(valid) mustEqual(Valid(27))
      (Path \ "age").from[JValue](min(20)).validate(valid) mustEqual(Valid(27))
      (Path \ "age").from[JValue](max(50)).validate(valid) mustEqual(Valid(27))
      (Path \ "age").from[JValue](min(50)).validate(valid) mustEqual(Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
      (Path \ "age").from[JValue](max(0)).validate(valid) mustEqual(Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
      (Path \ "firstname").read[JValue, Int].validate(valid) mustEqual(Invalid(Seq((Path \ "firstname") -> Seq(ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      val composed = notEmpty |+| minLength(3)
      (Path \ "firstname").from[JValue](composed).validate(valid) mustEqual(Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"), ValidationError("error.minLength", 3))))
      p.from[JValue](composed).validate(invalid) mustEqual(err)
    }

    "compose validations" in {
      From[JValue]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty)).tupled
      }.validate(valid) mustEqual Valid("Julien" -> "Tournay")

      From[JValue]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "informations" \ "label").read(notEmpty)).tupled
      }.validate(invalid) mustEqual Invalid(Seq((Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      (Path \ "foo").from[JValue](seqR(notEmpty)).validate(JObject(Map("foo" -> JArray(JString("bar")))))
        .toOption.get must contain(exactly(Seq("bar"): _*))

      From[JValue]{ __ =>
        (__ \ "foo").read(
          (__ \ "foo").read(seqR(notEmpty)))
      }.validate(JObject(Map("foo" -> JObject(Map("foo" -> JArray(JString("bar")))))))
        .toOption.get must contain(exactly(Seq("bar"): _*))

      (Path \ "n").from[JValue](seqR(notEmpty))
        .validate(JObject(Map("n" -> JArray(JString("foo"), JString(""))))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      val v = JObject(Map(
        "login" -> JString("Alice"),
        "password" -> JString("s3cr3t"),
        "verify" -> JString("s3cr3t")))

      val i1 = JObject(Map(
        "login" -> JString("Alice"),
        "password" -> JString("s3cr3t"),
        "verify" -> JString("")))

      val i2 = JObject(Map(
        "login" -> JString("Alice"),
        "password" -> JString("s3cr3t"),
        "verify" -> JString("bam")))

      val passRule = From[JValue] { __ =>
        ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty))
          .tupled.compose(Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
      }

      val rule = From[JValue] { __ =>
        ((__ \ "login").read(notEmpty) ~ passRule).tupled
      }

      rule.validate(v).mustEqual(Valid("Alice" -> "s3cr3t"))
      rule.validate(i1).mustEqual(Invalid(Seq(Path \ "verify" -> Seq(ValidationError("error.required")))))
      rule.validate(i2).mustEqual(Invalid(Seq(Path \ "verify" -> Seq(ValidationError("error.equals", "s3cr3t")))))
    }

    "validate subclasses (and parse the concrete class)" in {

      trait A
      case class B(foo: Int) extends A
      case class C(bar: Int) extends A

      val b = JObject(Map("name" -> JString("B"), "foo" -> JNumber(4)))
      val c = JObject(Map("name" -> JString("C"), "bar" -> JNumber(6)))
      val e = JObject(Map("name" -> JString("E"), "eee" -> JNumber(6)))

      val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        val rb: Rule[JValue, A] = From[JValue]{ __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo").read[Int].map(B.apply)
        }

        val rc: Rule[JValue, A] = From[JValue]{ __ =>
          (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar").read[Int].map(C.apply)
        }

        val rule = rb orElse rc orElse Rule(_ => typeInvalid)

        rule.validate(b) mustEqual(Valid(B(4)))
        rule.validate(c) mustEqual(Valid(C(6)))
        rule.validate(e) mustEqual(Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

      "by dicriminating on fields" in {

        val rule = From[JValue] { __ =>
          (__ \ "name").read[String].flatMap[A] {
            case "B" => (__ \ "foo").read[Int].map(B.apply)
            case "C" => (__ \ "bar").read[Int].map(C.apply)
            case _ => Rule(_ => typeInvalid)
          }
        }

        rule.validate(b) mustEqual(Valid(B(4)))
        rule.validate(c) mustEqual(Valid(C(6)))
        rule.validate(e) mustEqual(Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

    }

    "perform complex validation" in {

      case class Contact(
        firstname: String,
        lastname: String,
        company: Option[String],
        informations: Seq[ContactInformation])

      case class ContactInformation(
        label: String,
        email: Option[String],
        phones: Seq[String])

      val validJson = JObject(Map(
        "firstname" -> JString("Julien"),
        "lastname" -> JString("Tournay"),
        "age" -> JNumber(27),
        "informations" -> JArray(JObject(Map(
          "label" -> JString("Personal"),
          "email" -> JString("fakecontact@gmail.com"),
          "phones" -> JArray(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))

      val invalidJson = JObject(Map(
        "firstname" -> JString("Julien"),
        "lastname" -> JString("Tournay"),
        "age" -> JNumber(27),
        "informations" -> JArray(JObject(Map(
          "label" -> JString(""),
          "email" -> JString("fakecontact@gmail.com"),
          "phones" -> JArray(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))

      val infoValidated = From[JValue] { __ =>
         ((__ \ "label").read(notEmpty) ~
          (__ \ "email").read(optionR(email)) ~
          (__ \ "phones").read(seqR(notEmpty))) (ContactInformation.apply)
      }

      val contactValidated = From[JValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "company").read[Option[String]] ~
         (__ \ "informations").read(seqR(infoValidated))) (Contact.apply)
      }

      val expected =
        Contact("Julien", "Tournay", None, Seq(
          ContactInformation("Personal", Some("fakecontact@gmail.com"), List("01.23.45.67.89", "98.76.54.32.10"))))

      contactValidated.validate(validJson) mustEqual(Valid(expected))
      contactValidated.validate(invalidJson) mustEqual(Invalid(Seq(
        (Path \ "informations" \ 0 \ "label") -> Seq(ValidationError("error.required")))))
    }

    "read recursive" in {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser(
        "bob",
        Seq(RecUser("tom")))

      val m = JObject(Map(
        "name" -> JString("bob"),
        "friends" -> JArray(JObject(Map("name" -> JString("tom"), "friends" -> JArray())))))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(Map(
        "name" -> JString("bob"),
        "friend" -> JObject(Map("name" -> JString("tom")))))

      "using explicit notation" in {
        lazy val w: Rule[JValue, RecUser] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read(seqR(w)))(RecUser.apply)
        }
        w.validate(m) mustEqual Valid(u)

        lazy val w2: Rule[JValue, RecUser] =
          ((Path \ "name").read[JValue, String] ~
           (Path \ "friends").from[JValue](seqR(w2)))(RecUser.apply)
        w2.validate(m) mustEqual Valid(u)

        lazy val w3: Rule[JValue, User1] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read(optionR(w3)))(User1.apply)
        }
        w3.validate(m1) mustEqual Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[JValue, RecUser] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read[Seq[RecUser]])(RecUser.apply)
        }
        w.validate(m) mustEqual Valid(u)

        implicit lazy val w3: Rule[JValue, User1] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read[Option[User1]])(User1.apply)
        }
        w3.validate(m1) mustEqual Valid(u1)
      }

    }


    "completely generic" in {
      type OptString[In] = Rule[String, String] => Path => Rule[In, Option[String]]

      def genR[In](opt: OptString[In])(implicit exs: Path => Rule[In, String]) =
        From[In] { __ =>
          ((__ \ "name").read(notEmpty) ~
           (__ \ "color").read(opt(notEmpty))).tupled
        }

      val jsonR = {
        genR[JValue](optionR(_))
      }

      val json = JObject(Map("name" -> JString("bob"), "color" -> JString("blue")))
      val invalidJson = JObject(Map("color" -> JString("blue")))

      jsonR.validate(json) mustEqual Valid(("bob", Some("blue")))
      jsonR.validate(invalidJson) mustEqual Invalid(Seq((Path \ "name", Seq(ValidationError("error.required")))))
    }

  }
}
