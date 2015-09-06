import jto.validation._
import jto.validation.json4s._
import org.specs2.mutable._
import org.json4s._

object RulesSpec extends Specification {

  "Json Rules" should {
    
    import Rules._

    val valid = JObject(
    "firstname" -> JString("Julien"),
    "lastname" -> JString("Tournay"),
    "age" -> JInt(27),
    "informations" -> JObject(
      "label" -> JString("Personal"),
      "email" -> JString("fakecontact@gmail.com"),
      "phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))

    val invalid = JObject(
    "firstname" -> JString("Julien"),
    "lastname" -> JString("Tournay"),
    "age" -> JInt(27),
    "informations" -> JObject(
      "label" -> JString(""),
      "email" -> JString("fakecontact@gmail.com"),
      "phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))

    "extract data" in {
      (Path \ "firstname").read[JValue, String].validate(valid) mustEqual(Success("Julien"))
      val errPath = Path \ "foo"
      val error = Failure(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[JValue, String].validate(invalid) mustEqual(error)
    }

    "support checked" in {
      val js = JObject("issmth" -> JBool(true))
      val p = Path \ "issmth"
      p.from[JValue](checked).validate(js) mustEqual(Success(true))
      p.from[JValue](checked).validate(JObject()) mustEqual(Failure(Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
      p.from[JValue](checked).validate(JObject("issmth" -> JBool(false))) mustEqual(Failure(Seq(Path \ "issmth" -> Seq(ValidationError("error.equals", true)))))
    }

    "support all types of Json values" in {

      "null" in {
        (Path \ "n").read[JValue, JNull.type].validate(JObject("n" -> JNull)) mustEqual(Success(JNull))
        (Path \ "n").read[JValue, JNull.type].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
        (Path \ "n").read[JValue, JNull.type].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
      }

      "Int" in {
        (Path \ "n").read[JValue, Int].validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        (Path \ "n").read[JValue, Int].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n").read[JValue, Int].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n" \ "o").read[JValue, Int].validate(JObject("n" -> JObject("o" -> JInt(4)))) mustEqual(Success(4))
        (Path \ "n" \ "o").read[JValue, Int].validate(JObject("n" -> JObject("o" -> JString("foo")))) mustEqual(Failure(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o" \ "p" ).read[JValue, Int].validate(JObject("n" -> JObject("o" -> JObject("p" -> JInt(4))))) mustEqual(Success(4))
        (Path \ "n" \ "o" \ "p").read[JValue, Int].validate(JObject("n" -> JObject("o" -> JObject("p" -> JString("foo"))))) mustEqual(Failure(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Failure(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[JValue, Int].validate(JObject("n" -> JInt(4))) mustEqual(error)
      }

      "Short" in {
        (Path \ "n").read[JValue, Short].validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        (Path \ "n").read[JValue, Short].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        (Path \ "n").read[JValue, Short].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        (Path \ "n").read[JValue, Long].validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        (Path \ "n").read[JValue, Long].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        (Path \ "n").read[JValue, Long].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        (Path \ "n").read[JValue, Float].validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        (Path \ "n").read[JValue, Float].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        (Path \ "n").read[JValue, Float].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(4.8F))
      }

      "Double" in {
        (Path \ "n").read[JValue, Double].validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        (Path \ "n").read[JValue, Double].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        (Path \ "n").read[JValue, Double].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(4.8))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        (Path \ "n").read[JValue, jBigDecimal].validate(JObject("n" -> JInt(4))) mustEqual(Success(new jBigDecimal("4")))
        (Path \ "n").read[JValue, jBigDecimal].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n").read[JValue, jBigDecimal].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(new jBigDecimal("4.8")))
      }

      "scala BigDecimal" in {
        (Path \ "n").read[JValue, BigDecimal].validate(JObject("n" -> JInt(4))) mustEqual(Success(BigDecimal(4)))
        (Path \ "n").read[JValue, BigDecimal].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n").read[JValue, BigDecimal].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(BigDecimal(4.8)))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n").from[JValue](Rules.date).validate(JObject("n" -> JString("1985-09-10"))) mustEqual(Success(f.parse("1985-09-10")))
        (Path \ "n").from[JValue](Rules.date).validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date" in {
        skipped("Can't test on CI")
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n").from[JValue](Rules.isoDate).validate(JObject("n" -> JString("1985-09-10T00:00:00+02:00"))) mustEqual(Success(f.parse("1985-09-10")))
        (Path \ "n").from[JValue](Rules.isoDate).validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date.isoformat")))))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n").from[JValue](Rules.jodaDate).validate(JObject("n" -> JString("1985-09-10"))) mustEqual(Success(jd))
          (Path \ "n").from[JValue](Rules.jodaDate).validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          (Path \ "n").from[JValue](Rules.jodaTime).validate(JObject("n" -> JInt(dd.getTime))) mustEqual(Success(jd))
          (Path \ "n").from[JValue](Rules.jodaDate).validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n").from[JValue](Rules.jodaLocalDate).validate(JObject("n" -> JString(ld.toString()))) mustEqual(Success(ld))
          (Path \ "n").from[JValue](Rules.jodaLocalDate).validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n").from[JValue](Rules.sqlDate).validate(JObject("n" -> JString("1985-09-10"))) mustEqual(Success(ds))
      }

      "Boolean" in {
        (Path \ "n").read[JValue, Boolean].validate(JObject("n" -> JBool(true))) mustEqual(Success(true))
        (Path \ "n").read[JValue, Boolean].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        (Path \ "n").read[JValue, String].validate(JObject("n" -> JString("foo"))) mustEqual(Success("foo"))
        (Path \ "n").read[JValue, String].validate(JObject("n" -> JInt(42))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "n").read[JValue, String].validate(JObject("n" -> JArray(List(JString("foo"))))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "o").read[JValue, String].validate(JObject("o" -> JObject("n" -> JString("foo")))) mustEqual(Failure(Seq(Path \ "o" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "JObject" in {
        (Path \ "o").read[JValue, JObject].validate(JObject("o" -> JObject("n" -> JString("foo")))) mustEqual(Success(JObject("n" -> JString("foo"))))
        (Path \ "n").read[JValue, JObject].validate(JObject("n" -> JInt(42))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n").read[JValue, JObject].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n").read[JValue, JObject].validate(JObject("n" -> JArray(List(JString("foo"))))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
      }

      "JString" in {
        (Path \ "n").read[JValue, JString].validate(JObject("n" -> JString("foo"))) mustEqual(Success(JString("foo")))
        (Path \ "n").read[JValue, JString].validate(JObject("n" -> JInt(42))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "JsNumber" in {
        (Path \ "n").read[JValue, JInt].validate(JObject("n" -> JInt(4))) mustEqual(Success(JInt(4)))
        (Path \ "n").read[JValue, JInt].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Number")))))
        (Path \ "n").read[JValue, JDecimal].validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(JDecimal(4.8)))
      }

      "JBool" in {
        (Path \ "n").read[JValue, JBool].validate(JObject("n" -> JBool(true))) mustEqual(Success(JBool(true)))
        (Path \ "n").read[JValue, JBool].validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Option" in {
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject("n" -> JBool(true))) mustEqual(Success(Some(true)))
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject("n" -> JNull)) mustEqual(Success(None))
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject("foo" -> JString("bar"))) mustEqual(Success(None))
        (Path \ "n").read[JValue, Option[Boolean]].validate(JObject("n" -> JString("bar"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        (Path \ "n").read[JValue, Map[String, String]].validate(JObject("n" -> JObject("foo" -> JString("bar")))) mustEqual(Success(Map("foo" -> "bar")))
        (Path \ "n").read[JValue, Map[String, Int]].validate(JObject("n" -> JObject("foo" -> JInt(4), "bar" -> JInt(5)))) mustEqual(Success(Map("foo" -> 4, "bar" -> 5)))
        (Path \ "x").read[JValue, Map[String, Int]].validate(JObject("n" -> JObject("foo" -> JInt(4), "bar" -> JString("frack")))) mustEqual(Failure(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        (Path \ "n").read[JValue, Map[String, Int]].validate(JObject("n" -> JObject("foo" -> JInt(4), "bar" -> JString("frack")))) mustEqual(Failure(Seq(Path \ "n" \ "bar" -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        (Path \ "n").read[JValue, Traversable[String]].validate(JObject("n" -> JArray(List(JString("foo"))))).get.toSeq must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JValue, Traversable[Int]].validate(JObject("n" -> JArray(List(JInt(1), JInt(2), JInt(3))))).get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JValue, Traversable[String]].validate(JObject("n" -> JString("paf"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Array" in {
        (Path \ "n").read[JValue, Array[String]].validate(JObject("n" -> JArray(List(JString("foo"))))).get.toSeq must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JValue, Array[Int]].validate(JObject("n" -> JArray(List(JInt(1), JInt(2), JInt(3))))).get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JValue, Array[String]].validate(JObject("n" -> JString("paf"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Seq" in {
        (Path \ "n").read[JValue, Seq[String]].validate(JObject("n" -> JArray(List(JString("foo"))))).get must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JValue, Seq[Int]].validate(JObject("n" -> JArray(List(JInt(1), JInt(2), JInt(3))))).get must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JValue, Seq[String]].validate(JObject("n" -> JString("paf"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
        (Path \ "n").read[JValue, Seq[String]].validate(JObject("n" -> JArray(List(JString("foo"), JInt(2))))) mustEqual(Failure(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.invalid", "String")))))
      }

    }

    "validate data" in {
      (Path \ "firstname").from[JValue](notEmpty).validate(valid) mustEqual(Success("Julien"))

      val p = (Path \ "informations" \ "label")
      p.from[JValue](notEmpty).validate(valid) mustEqual(Success("Personal"))
      p.from[JValue](notEmpty).validate(invalid) mustEqual(Failure(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      (Path \ "firstname").read[JValue, Option[String]].validate(valid) mustEqual(Success(Some("Julien")))
      (Path \ "foobar").read[JValue, Option[String]].validate(valid) mustEqual(Success(None))
    }

    "validate deep" in {
      val p = (Path \ "informations" \ "label")

      From[JValue] { __ =>
        (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(valid) mustEqual(Success("Personal"))

      From[JValue] { __ =>
        (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(invalid) mustEqual(Failure(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[JValue]{ __ =>
        (__ \ "first" \ "second").read[Option[String]]
      }validate(JNull) mustEqual Success(None)
    }

    "coerce type" in {
      (Path \ "age").read[JValue, Int].validate(valid) mustEqual(Success(27))
      (Path \ "age").from[JValue](min(20)).validate(valid) mustEqual(Success(27))
      (Path \ "age").from[JValue](max(50)).validate(valid) mustEqual(Success(27))
      (Path \ "age").from[JValue](min(50)).validate(valid) mustEqual(Failure(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
      (Path \ "age").from[JValue](max(0)).validate(valid) mustEqual(Failure(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
      (Path \ "firstname").read[JValue, Int].validate(valid) mustEqual(Failure(Seq((Path \ "firstname") -> Seq(ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      val composed = notEmpty |+| minLength(3)
      (Path \ "firstname").from[JValue](composed).validate(valid) mustEqual(Success("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Failure(Seq(p -> Seq(ValidationError("error.required"), ValidationError("error.minLength", 3))))
      p.from[JValue](composed).validate(invalid) mustEqual(err)
    }

    "compose validations" in {
      From[JValue]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty)).tupled
      }.validate(valid) mustEqual Success("Julien" -> "Tournay")

      From[JValue]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "informations" \ "label").read(notEmpty)).tupled
      }.validate(invalid) mustEqual Failure(Seq((Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      (Path \ "foo").from[JValue](seqR(notEmpty)).validate(JObject("foo" -> JArray(List(JString("bar")))))
        .get must contain(exactly(Seq("bar"): _*))

      From[JValue]{ __ =>
        (__ \ "foo").read(
          (__ \ "foo").read(seqR(notEmpty)))
      }.validate(JObject("foo" -> JObject("foo" -> JArray(List(JString("bar"))))))
        .get must contain(exactly(Seq("bar"): _*))

      (Path \ "n").from[JValue](seqR(notEmpty))
        .validate(JObject("n" -> JArray(List(JString("foo"), JString(""))))) mustEqual(Failure(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      val v = JObject(
        "login" -> JString("Alice"),
        "password" -> JString("s3cr3t"),
        "verify" -> JString("s3cr3t"))

      val i1 = JObject(
        "login" -> JString("Alice"),
        "password" -> JString("s3cr3t"),
        "verify" -> JString(""))

      val i2 = JObject(
        "login" -> JString("Alice"),
        "password" -> JString("s3cr3t"),
        "verify" -> JString("bam"))

      val passRule = From[JValue] { __ =>
        ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty))
          .tupled.compose(Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
      }

      val rule = From[JValue] { __ =>
        ((__ \ "login").read(notEmpty) ~ passRule).tupled
      }

      rule.validate(v).mustEqual(Success("Alice" -> "s3cr3t"))
      rule.validate(i1).mustEqual(Failure(Seq(Path \ "verify" -> Seq(ValidationError("error.required")))))
      rule.validate(i2).mustEqual(Failure(Seq(Path \ "verify" -> Seq(ValidationError("error.equals", "s3cr3t")))))
    }

    "validate subclasses (and parse the concrete class)" in {

      trait A
      case class B(foo: Int) extends A
      case class C(bar: Int) extends A

      val b = JObject("name" -> JString("B"), "foo" -> JInt(4))
      val c = JObject("name" -> JString("C"), "bar" -> JInt(6))
      val e = JObject("name" -> JString("E"), "eee" -> JInt(6))

      val typeFailure = Failure(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        val rb: Rule[JValue, A] = From[JValue]{ __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo").read[Int].map(B.apply _)
        }

        val rc: Rule[JValue, A] = From[JValue]{ __ =>
          (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar").read[Int].map(C.apply _)
        }

        val rule = rb orElse rc orElse Rule(_ => typeFailure)

        rule.validate(b) mustEqual(Success(B(4)))
        rule.validate(c) mustEqual(Success(C(6)))
        rule.validate(e) mustEqual(Failure(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

      "by dicriminating on fields" in {

        val rule = From[JValue] { __ =>
          (__ \ "name").read[String].flatMap[A] {
            case "B" => (__ \ "foo").read[Int].map(B.apply _)
            case "C" => (__ \ "bar").read[Int].map(C.apply _)
            case _ => Rule(_ => typeFailure)
          }
        }

        rule.validate(b) mustEqual(Success(B(4)))
        rule.validate(c) mustEqual(Success(C(6)))
        rule.validate(e) mustEqual(Failure(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
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

      val validJson = JObject(
        "firstname" -> JString("Julien"),
        "lastname" -> JString("Tournay"),
        "age" -> JInt(27),
        "informations" -> JArray(List(JObject(
          "label" -> JString("Personal"),
          "email" -> JString("fakecontact@gmail.com"),
          "phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))

      val invalidJson = JObject(
        "firstname" -> JString("Julien"),
        "lastname" -> JString("Tournay"),
        "age" -> JInt(27),
        "informations" -> JArray(List(JObject(
          "label" -> JString(""),
          "email" -> JString("fakecontact@gmail.com"),
          "phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))

      val infoValidation = From[JValue] { __ =>
         ((__ \ "label").read(notEmpty) ~
          (__ \ "email").read(optionR(email)) ~
          (__ \ "phones").read(seqR(notEmpty))) (ContactInformation.apply _)
      }

      val contactValidation = From[JValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "company").read[Option[String]] ~
         (__ \ "informations").read(seqR(infoValidation))) (Contact.apply _)
      }

      val expected =
        Contact("Julien", "Tournay", None, Seq(
          ContactInformation("Personal", Some("fakecontact@gmail.com"), List("01.23.45.67.89", "98.76.54.32.10"))))

      contactValidation.validate(validJson) mustEqual(Success(expected))
      contactValidation.validate(invalidJson) mustEqual(Failure(Seq(
        (Path \ "informations" \ 0 \ "label") -> Seq(ValidationError("error.required")))))
    }

    "read recursive" in {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser(
        "bob",
        Seq(RecUser("tom")))

      val m = JObject(
        "name" -> JString("bob"),
        "friends" -> JArray(List(JObject("name" -> JString("tom"), "friends" -> JArray(Nil)))))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(
        "name" -> JString("bob"),
        "friend" -> JObject("name" -> JString("tom")))

      "using explicit notation" in {
        lazy val w: Rule[JValue, RecUser] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read(seqR(w)))(RecUser.apply _)
        }
        w.validate(m) mustEqual Success(u)

        lazy val w2: Rule[JValue, RecUser] =
          ((Path \ "name").read[JValue, String] ~
           (Path \ "friends").from[JValue](seqR(w2)))(RecUser.apply _)
        w2.validate(m) mustEqual Success(u)

        lazy val w3: Rule[JValue, User1] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read(optionR(w3)))(User1.apply _)
        }
        w3.validate(m1) mustEqual Success(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[JValue, RecUser] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read[Seq[RecUser]])(RecUser.apply _)
        }
        w.validate(m) mustEqual Success(u)

        implicit lazy val w3: Rule[JValue, User1] = From[JValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read[Option[User1]])(User1.apply _)
        }
        w3.validate(m1) mustEqual Success(u1)
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

      val json = JObject("name" -> JString("bob"), "color" -> JString("blue"))
      val invalidJson = JObject("color" -> JString("blue"))

      jsonR.validate(json) mustEqual Success(("bob", Some("blue")))
      jsonR.validate(invalidJson) mustEqual Failure(Seq((Path \ "name", Seq(ValidationError("error.required")))))


      // val formR = {
      //   
      //   genR[UrlFormEncoded](optionR(_))
      // }
      // val form = Map("name" -> Seq("bob"), "color" -> Seq("blue"))
      // val invalidForm = Map("color" -> Seq("blue"))

      // formR.validate(form) mustEqual Success(("bob", Some("blue")))
      // formR.validate(invalidForm) mustEqual Failure(Seq((Path \ "name", Seq(ValidationError("error.required")))))
    }

  }
}
