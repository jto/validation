import jto.validation._
import jto.validation.json._
import org.specs2.mutable._
import play.api.libs.json.{JsValue, JsObject, Json, JsString, JsNumber, JsBoolean, JsArray, JsNull}

object RulesSpec extends Specification {

  "Json Rules" should {
    import Rules._

    val valid = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "Tournay",
    "age" -> 27,
    "informations" -> Json.obj(
      "label" -> "Personal",
      "email" -> "fakecontact@gmail.com",
      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10")))

    val invalid = Json.obj(
      "firstname" -> "Julien",
      "lastname" -> "Tournay",
      "age" -> 27,
      "informations" -> Json.obj(
        "label" -> "",
        "email" -> "fakecontact@gmail.com",
        "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10")))

    "extract data" in {
      (Path \ "firstname").read[JsValue, String].validate(valid) mustEqual(Valid("Julien"))
      val errPath = Path \ "foo"
      val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[JsValue, String].validate(invalid) mustEqual(error)
    }

    "support checked" in {
      val js = Json.obj("issmth" -> true)
      val p = Path \ "issmth"
      p.from[JsValue](checked).validate(js) mustEqual(Valid(true))
      p.from[JsValue](checked).validate(Json.obj()) mustEqual(Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
      p.from[JsValue](checked).validate(Json.obj("issmth" -> false)) mustEqual(Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.equals", true)))))
    }

    "support all types of Json values" in {

      "null" in {
        (Path \ "n").read[JsValue, JsNull.type].validate(Json.obj("n" -> JsNull)) mustEqual(Valid(JsNull))
        (Path \ "n").read[JsValue, JsNull.type].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
        (Path \ "n").read[JsValue, JsNull.type].validate(Json.obj("n" -> 4.8)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
      }

      "Int" in {
        (Path \ "n").read[JsValue, Int].validate(Json.obj("n" -> 4)) mustEqual(Valid(4))
        (Path \ "n").read[JsValue, Int].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n").read[JsValue, Int].validate(Json.obj("n" -> 4.8)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        (Path \ "n" \ "o").read[JsValue, Int].validate(Json.obj("n" -> Json.obj("o" -> 4))) mustEqual(Valid(4))
        (Path \ "n" \ "o").read[JsValue, Int].validate(Json.obj("n" -> Json.obj("o" -> "foo"))) mustEqual(Invalid(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o" \ "p" ).read[JsValue, Int].validate(Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4)))) mustEqual(Valid(4))
        (Path \ "n" \ "o" \ "p").read[JsValue, Int].validate(Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo")))) mustEqual(Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[JsValue, Int].validate(Json.obj("n" -> 4)) mustEqual(error)
      }

      "Short" in {
        (Path \ "n").read[JsValue, Short].validate(Json.obj("n" -> 4)) mustEqual(Valid(4))
        (Path \ "n").read[JsValue, Short].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        (Path \ "n").read[JsValue, Short].validate(Json.obj("n" -> 4.8)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        (Path \ "n").read[JsValue, Long].validate(Json.obj("n" -> 4)) mustEqual(Valid(4))
        (Path \ "n").read[JsValue, Long].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        (Path \ "n").read[JsValue, Long].validate(Json.obj("n" -> 4.8)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        (Path \ "n").read[JsValue, Float].validate(Json.obj("n" -> 4)) mustEqual(Valid(4))
        (Path \ "n").read[JsValue, Float].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        (Path \ "n").read[JsValue, Float].validate(Json.obj("n" -> 4.8)) mustEqual(Valid(4.8F))
      }

      "Double" in {
        (Path \ "n").read[JsValue, Double].validate(Json.obj("n" -> 4)) mustEqual(Valid(4))
        (Path \ "n").read[JsValue, Double].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        (Path \ "n").read[JsValue, Double].validate(Json.obj("n" -> 4.8)) mustEqual(Valid(4.8))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        (Path \ "n").read[JsValue, jBigDecimal].validate(Json.obj("n" -> 4)) mustEqual(Valid(new jBigDecimal("4")))
        (Path \ "n").read[JsValue, jBigDecimal].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n").read[JsValue, jBigDecimal].validate(Json.obj("n" -> 4.8)) mustEqual(Valid(new jBigDecimal("4.8")))
      }

      "scala BigDecimal" in {
        (Path \ "n").read[JsValue, BigDecimal].validate(Json.obj("n" -> 4)) mustEqual(Valid(BigDecimal(4)))
        (Path \ "n").read[JsValue, BigDecimal].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        (Path \ "n").read[JsValue, BigDecimal].validate(Json.obj("n" -> 4.8)) mustEqual(Valid(BigDecimal(4.8)))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n").from[JsValue](Rules.date).validate(Json.obj("n" -> "1985-09-10")) mustEqual(Valid(f.parse("1985-09-10")))
        (Path \ "n").from[JsValue](Rules.date).validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date" in {
        skipped("Can't test on CI")
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n").from[JsValue](Rules.isoDate).validate(Json.obj("n" -> "1985-09-10T00:00:00+02:00")) mustEqual(Valid(f.parse("1985-09-10")))
        (Path \ "n").from[JsValue](Rules.isoDate).validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date.isoformat")))))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n").from[JsValue](Rules.jodaDate).validate(Json.obj("n" -> "1985-09-10")) mustEqual(Valid(jd))
          (Path \ "n").from[JsValue](Rules.jodaDate).validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          (Path \ "n").from[JsValue](Rules.jodaTime).validate(Json.obj("n" -> dd.getTime)) mustEqual(Valid(jd))
          (Path \ "n").from[JsValue](Rules.jodaDate).validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n").from[JsValue](Rules.jodaLocalDate).validate(Json.obj("n" -> ld.toString())) mustEqual(Valid(ld))
          (Path \ "n").from[JsValue](Rules.jodaLocalDate).validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n").from[JsValue](Rules.sqlDate).validate(Json.obj("n" -> "1985-09-10")) mustEqual(Valid(ds))
      }

      "Boolean" in {
        (Path \ "n").read[JsValue, Boolean].validate(Json.obj("n" -> true)) mustEqual(Valid(true))
        (Path \ "n").read[JsValue, Boolean].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        (Path \ "n").read[JsValue, String].validate(Json.obj("n" -> "foo")) mustEqual(Valid("foo"))
        (Path \ "n").read[JsValue, String].validate(Json.obj("n" -> 42)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "n").read[JsValue, String].validate(Json.obj("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
        (Path \ "o").read[JsValue, String].validate(Json.obj("o" -> Json.obj("n" -> "foo"))) mustEqual(Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "JsObject" in {
        (Path \ "o").read[JsValue, JsObject].validate(Json.obj("o" -> Json.obj("n" -> "foo"))) mustEqual(Valid(JsObject(Seq("n" -> JsString("foo")))))
        (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> 42)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
        (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Object")))))
      }

      "JsString" in {
        (Path \ "n").read[JsValue, JsString].validate(Json.obj("n" -> "foo")) mustEqual(Valid(JsString("foo")))
        (Path \ "n").read[JsValue, JsString].validate(Json.obj("n" -> 42)) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "JsNumber" in {
        (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> 4)) mustEqual(Valid(JsNumber(4)))
        (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Number")))))
        (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> 4.8)) mustEqual(Valid(JsNumber(4.8)))
      }

      "JsBoolean" in {
        (Path \ "n").read[JsValue, JsBoolean].validate(Json.obj("n" -> true)) mustEqual(Valid(JsBoolean(true)))
        (Path \ "n").read[JsValue, JsBoolean].validate(Json.obj("n" -> "foo")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Option" in {
        (Path \ "n").read[JsValue, Option[Boolean]].validate(Json.obj("n" -> true)) mustEqual(Valid(Some(true)))
        (Path \ "n").read[JsValue, Option[Boolean]].validate(Json.obj("n" -> JsNull)) mustEqual(Valid(None))
        (Path \ "n").read[JsValue, Option[Boolean]].validate(Json.obj("foo" -> "bar")) mustEqual(Valid(None))
        (Path \ "n").read[JsValue, Option[Boolean]].validate(Json.obj("n" -> "bar")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        (Path \ "n").read[JsValue, Map[String, String]].validate(Json.obj("n" -> Json.obj("foo" -> "bar"))) mustEqual(Valid(Map("foo" -> "bar")))
        (Path \ "n").read[JsValue, Map[String, Int]].validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> 5))) mustEqual(Valid(Map("foo" -> 4, "bar" -> 5)))
        (Path \ "x").read[JsValue, Map[String, Int]].validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))) mustEqual(Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        (Path \ "n").read[JsValue, Map[String, Int]].validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))) mustEqual(Invalid(Seq(Path \ "n" \ "bar" -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        (Path \ "n").read[JsValue, Traversable[String]].validate(Json.obj("n" -> Seq("foo"))).toOption.get.toSeq must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JsValue, Traversable[Int]].validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JsValue, Traversable[String]].validate(Json.obj("n" -> "paf")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Array" in {
        (Path \ "n").read[JsValue, Array[String]].validate(Json.obj("n" -> Seq("foo"))).toOption.get.toSeq must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JsValue, Array[Int]].validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JsValue, Array[String]].validate(Json.obj("n" -> "paf")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Seq" in {
        (Path \ "n").read[JsValue, Seq[String]].validate(Json.obj("n" -> Seq("foo"))).toOption.get must contain(exactly(Seq("foo"): _*))
        (Path \ "n").read[JsValue, Seq[Int]].validate(Json.obj("n" -> Seq(1, 2, 3))).toOption.get must contain(exactly(Seq(1, 2, 3): _*))
        (Path \ "n").read[JsValue, Seq[String]].validate(Json.obj("n" -> "paf")) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
        (Path \ "n").read[JsValue, Seq[String]].validate(JsObject(Seq("n" -> JsArray(Seq(JsString("foo"), JsNumber(2)))))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.invalid", "String")))))
      }

    }

    "validate data" in {
      (Path \ "firstname").from[JsValue](notEmpty).validate(valid) mustEqual(Valid("Julien"))

      val p = (Path \ "informations" \ "label")
      p.from[JsValue](notEmpty).validate(valid) mustEqual(Valid("Personal"))
      p.from[JsValue](notEmpty).validate(invalid) mustEqual(Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      (Path \ "firstname").read[JsValue, Option[String]].validate(valid) mustEqual(Valid(Some("Julien")))
      (Path \ "foobar").read[JsValue, Option[String]].validate(valid) mustEqual(Valid(None))
    }

    "validate deep" in {
      val p = (Path \ "informations" \ "label")

      From[JsValue] { __ =>
        (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(valid) mustEqual(Valid("Personal"))

      From[JsValue] { __ =>
        (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(invalid) mustEqual(Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[JsValue]{ __ =>
        (__ \ "first" \ "second").read[Option[String]]
      }validate(JsNull) mustEqual Valid(None)
    }

    "coerce type" in {
      (Path \ "age").read[JsValue, Int].validate(valid) mustEqual(Valid(27))
      (Path \ "age").from[JsValue](min(20)).validate(valid) mustEqual(Valid(27))
      (Path \ "age").from[JsValue](max(50)).validate(valid) mustEqual(Valid(27))
      (Path \ "age").from[JsValue](min(50)).validate(valid) mustEqual(Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
      (Path \ "age").from[JsValue](max(0)).validate(valid) mustEqual(Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
      (Path \ "firstname").read[JsValue, Int].validate(valid) mustEqual(Invalid(Seq((Path \ "firstname") -> Seq(ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      val composed = notEmpty |+| minLength(3)
      (Path \ "firstname").from[JsValue](composed).validate(valid) mustEqual(Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"), ValidationError("error.minLength", 3))))
      p.from[JsValue](composed).validate(invalid) mustEqual(err)
    }

    "compose validations" in {
      From[JsValue]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty)).tupled
      }.validate(valid) mustEqual Valid("Julien" -> "Tournay")

      From[JsValue]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "informations" \ "label").read(notEmpty)).tupled
      }.validate(invalid) mustEqual Invalid(Seq((Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      (Path \ "foo").from[JsValue](seqR(notEmpty)).validate(Json.obj("foo" -> Seq("bar")))
        .toOption.get must contain(exactly(Seq("bar"): _*))

      From[JsValue]{ __ =>
        (__ \ "foo").read(
          (__ \ "foo").read(seqR(notEmpty)))
      }.validate(Json.obj("foo" -> Json.obj("foo" -> Seq("bar"))))
        .toOption.get must contain(exactly(Seq("bar"): _*))

      (Path \ "n").from[JsValue](seqR(notEmpty))
        .validate(Json.obj("n" -> Seq("foo", ""))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      val v = Json.obj(
        "login" -> "Alice",
        "password" -> "s3cr3t",
        "verify" -> "s3cr3t")

      val i1 = Json.obj(
        "login" -> "Alice",
        "password" -> "s3cr3t",
        "verify" -> "")

      val i2 = Json.obj(
        "login" -> "Alice",
        "password" -> "s3cr3t",
        "verify" -> "bam")

      val passRule = From[JsValue] { __ =>
        ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty))
          .tupled.compose(Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
      }

      val rule = From[JsValue] { __ =>
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

      val b = Json.obj("name" -> "B", "foo" -> 4)
      val c = Json.obj("name" -> "C", "bar" -> 6)
      val e = Json.obj("name" -> "E", "eee" -> 6)

      val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        val rb: Rule[JsValue, A] = From[JsValue]{ __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo").read[Int].map(B.apply)
        }

        val rc: Rule[JsValue, A] = From[JsValue]{ __ =>
          (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar").read[Int].map(C.apply)
        }

        val rule = rb orElse rc orElse Rule(_ => typeInvalid)

        rule.validate(b) mustEqual(Valid(B(4)))
        rule.validate(c) mustEqual(Valid(C(6)))
        rule.validate(e) mustEqual(Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

      "by dicriminating on fields" in {

        val rule = From[JsValue] { __ =>
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

      val validJson = Json.obj(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" -> Seq(Json.obj(
          "label" -> "Personal",
          "email" -> "fakecontact@gmail.com",
          "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))))

      val invalidJson = Json.obj(
        "firstname" -> "Julien",
        "lastname" -> "Tournay",
        "age" -> 27,
        "informations" -> Seq(Json.obj(
          "label" -> "",
          "email" -> "fakecontact@gmail.com",
          "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))))

      val infoValidated = From[JsValue] { __ =>
         ((__ \ "label").read(notEmpty) ~
          (__ \ "email").read(optionR(email)) ~
          (__ \ "phones").read(seqR(notEmpty)))(ContactInformation.apply)
      }

      val contactValidated = From[JsValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "company").read[Option[String]] ~
         (__ \ "informations").read(seqR(infoValidated)))(Contact.apply)
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

      val m = Json.obj(
        "name" -> "bob",
        "friends" -> Seq(Json.obj("name" -> "tom", "friends" -> Seq[JsObject]())))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = Json.obj(
        "name" -> "bob",
        "friend" -> Json.obj("name" -> "tom"))

      "using explicit notation" in {
        lazy val w: Rule[JsValue, RecUser] = From[JsValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read(seqR(w)))(RecUser.apply)
        }
        w.validate(m) mustEqual Valid(u)

        lazy val w2: Rule[JsValue, RecUser] =
          ((Path \ "name").read[JsValue, String] ~
           (Path \ "friends").from[JsValue](seqR(w2)))(RecUser.apply)
        w2.validate(m) mustEqual Valid(u)

        lazy val w3: Rule[JsValue, User1] = From[JsValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read(optionR(w3)))(User1.apply)
        }
        w3.validate(m1) mustEqual Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[JsValue, RecUser] = From[JsValue]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read[Seq[RecUser]])(RecUser.apply)
        }
        w.validate(m) mustEqual Valid(u)

        implicit lazy val w3: Rule[JsValue, User1] = From[JsValue]{ __ =>
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
        
        genR[JsValue](optionR(_))
      }

      val json = Json.obj("name" -> "bob", "color" -> "blue")
      val invalidJson = Json.obj("color" -> "blue")

      jsonR.validate(json) mustEqual Valid(("bob", Some("blue")))
      jsonR.validate(invalidJson) mustEqual Invalid(Seq((Path \ "name", Seq(ValidationError("error.required")))))
    }
  }
}
