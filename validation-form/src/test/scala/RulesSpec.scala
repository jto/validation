import jto.validation._
import jto.validation.forms._
import org.specs2.mutable._

object RulesSpec extends Specification {

  "Rules" should {

    import Rules._
    val valid: UrlFormEncoded = Map(
      "firstname" -> Seq("Julien", "ignored"),
      "lastname" -> Seq("Tournay"),
      "age" -> Seq("27"),
      "informations.label" -> Seq("Personal"),
      "informations.email" -> Seq("fakecontact@gmail.com"),
      "informations.phones[]" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

    val invalid = Map(
     "firstname" -> Seq("Julien"),
     "lastname" -> Seq("Tournay"),
     "age" -> Seq("27"),
     "informations.label" -> Seq(""),
     "informations.email" -> Seq("fakecontact@gmail.com"),
     "informations.phones[]" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

    "extract data" in {
      From[UrlFormEncoded] { __ =>
        (__ \ "firstname").read[String]
      }.validate(valid) mustEqual(Valid("Julien"))

      val error = Invalid(Seq((Path \ "foo") -> Seq(ValidationError("error.required"))))
      From[UrlFormEncoded] { __ =>
        (__ \ "foo").read[String]
      }.validate(invalid) mustEqual(error)
    }

    "support checked" in {
      val js = Map("issmth" -> Seq("true"))
      val p = Path \ "issmth"
      p.from[UrlFormEncoded](checked).validate(js) mustEqual(Valid(true))
      p.from[UrlFormEncoded](checked).validate(Map.empty) mustEqual(Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
      p.from[UrlFormEncoded](checked).validate(Map("issmth" -> Seq("false"))) mustEqual(Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.equals", true)))))
    }

    "ignore values" in {
      val r = From[UrlFormEncoded]{ __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "test").read(ignored[UrlFormEncoded, Int](42))).tupled
      }
      r.validate(valid) mustEqual(Valid("Julien" -> 42))
    }

    "support primitives types" in {

      "Int" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Int] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Int] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Int] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        From[UrlFormEncoded] { __ => (__ \ "n" \ "o").read[Int] }.validate(Map("n.o" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n" \ "o").read[Int] }.validate(Map("n.o" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        From[UrlFormEncoded] { __ => (__ \ "n" \ "o" \ "p").read[Int] }.validate(Map("n.o.p" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n" \ "o" \ "p").read[Int] }.validate(Map("n.o.p" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        From[UrlFormEncoded] { __ => (__ \ "foo").read[Int] }.validate(Map("n" -> Seq("4"))) mustEqual(error)
      }

      "Short" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Short] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Short] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Short] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Long] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Long] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Long] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Float] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Float] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Float] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Valid(4.8F))
      }

      "Double" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Double] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(4))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Double] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Double] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Valid(4.8))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => jBigDecimal}
        From[UrlFormEncoded] { __ => (__ \ "n").read[jBigDecimal] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(new jBigDecimal("4")))
        From[UrlFormEncoded] { __ => (__ \ "n").read[jBigDecimal] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[jBigDecimal] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Valid(new jBigDecimal("4.8")))
      }

      "scala BigDecimal" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[BigDecimal] }.validate(Map("n" -> Seq("4"))) mustEqual(Valid(BigDecimal(4)))
        From[UrlFormEncoded] { __ => (__ \ "n").read[BigDecimal] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[BigDecimal] }.validate(Map("n" -> Seq("4.8"))) mustEqual(Valid(BigDecimal(4.8)))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(date)
        }.validate(Map("n" -> Seq("1985-09-10"))) mustEqual(Valid(f.parse("1985-09-10")))

        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(date)
        }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date" in {
        skipped("Can't test on CI")
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(isoDate)
        }.validate(Map("n" -> Seq("1985-09-10T00:00:00+02:00"))) mustEqual(Valid(f.parse("1985-09-10")))

        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(isoDate)
        }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date.isoformat")))))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaDate)
          }.validate(Map("n" -> Seq("1985-09-10"))) mustEqual(Valid(jd))

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaDate)
          }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaTime)
          }.validate(Map("n" -> Seq(dd.getTime.toString))) mustEqual(Valid(jd))

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaDate)
          }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaLocalDate)
          }.validate(Map("n" -> Seq(ld.toString()))) mustEqual(Valid(ld))

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaLocalDate)
          }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())

        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(sqlDate)
        }.validate(Map("n" -> Seq("1985-09-10"))) mustEqual(Valid(ds))
      }

      "Boolean" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Boolean] }.validate(Map("n" -> Seq("true"))) mustEqual(Valid(true))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Boolean] }.validate(Map("n" -> Seq("TRUE"))) mustEqual(Valid(true))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Boolean] }.validate(Map("n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[String] }.validate(Map("n" -> Seq("foo"))) mustEqual(Valid("foo"))
        From[UrlFormEncoded] { __ => (__ \ "o").read[String] }.validate(Map("o.n" -> Seq("foo"))) mustEqual(Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.required")))))
      }

      "Option" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Option[Boolean]] }.validate(Map("n" -> Seq("true"))) mustEqual(Valid(Some(true)))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Option[Boolean]] }.validate(Map("n" -> Seq(""))) mustEqual(Valid(None))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Option[Boolean]] }.validate(Map("foo" -> Seq("bar"))) mustEqual(Valid(None))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Option[Boolean]] }.validate(Map("n" -> Seq("bar"))) mustEqual(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, Seq[V]]" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Map[String, Seq[String]]] }.validate(Map("n.foo" -> Seq("bar"))) mustEqual(Valid(Map("foo" -> Seq("bar"))))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Map[String, Seq[Int]]] }.validate(Map("n.foo" -> Seq("4"), "n.bar" -> Seq("5"))) mustEqual(Valid(Map("foo" -> Seq(4), "bar" -> Seq(5))))
        From[UrlFormEncoded] { __ => (__ \ "x").read[Map[String, Int]] }.validate(Map("n.foo" -> Seq("4"), "n.bar" -> Seq("frack"))) mustEqual(Valid(Map.empty))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Map[String, Seq[Int]]] }.validate(Map("n.foo" -> Seq("4"), "n.bar" -> Seq("frack"))) mustEqual(Invalid(Seq(Path \ "n" \ "bar" \ 0 -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Traversable[String]] }.validate(Map("n" -> Seq("foo"))).toOption.get.toSeq must contain(exactly(Seq("foo"): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Traversable[Int]] }.validate(Map("n[]" -> Seq("1", "2", "3"))).toOption.get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Traversable[Int]] }.validate(Map("n[]" -> Seq("1", "paf"))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int")))))
      }

      "Array" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Array[String]] }.validate(Map("n" -> Seq("foo"))).toOption.get.toSeq must contain(exactly(Seq("foo"): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Array[Int]] }.validate(Map("n[]" -> Seq("1", "2", "3"))).toOption.get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Array[Int]] }.validate(Map("n[]" -> Seq("1", "paf"))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int")))))
      }

      "Seq" in {
        From[UrlFormEncoded] { __ => (__ \ "n").read[Seq[String]] }.validate(Map("n" -> Seq("foo"))).toOption.get must contain(exactly(Seq("foo"): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Seq[Int]] }.validate(Map("n[]" -> Seq("1", "2", "3"))).toOption.get must contain(exactly(Seq(1, 2, 3): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Seq[Int]] }.validate(Map(
          "n[0]" -> Seq("1"),
          "n[1]" -> Seq("2"),
          "n[3]" -> Seq("3")
        )).toOption.get must contain(exactly(Seq(1, 2, 3): _*))
        From[UrlFormEncoded] { __ => (__ \ "n").read[Seq[Int]] }.validate(Map("n[]" -> Seq("1", "paf"))) mustEqual(Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int")))))
      }
    }

    "validate data" in {
      From[UrlFormEncoded] { __ => (__  \ "firstname").read(notEmpty) }.validate(valid) mustEqual(Valid("Julien"))

      val p = (Path \ "informations" \ "label")
      From[UrlFormEncoded] { __ => (__  \ "informations" \ "label").read(notEmpty) }.validate(valid) mustEqual(Valid("Personal"))
      From[UrlFormEncoded] { __ => (__  \ "informations" \ "label").read(notEmpty) }.validate(invalid) mustEqual(Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate seq" in {
      def isNotEmpty[T <: Traversable[_]] = validateWith[T]("error.notEmpty"){ !_.isEmpty }

      From[UrlFormEncoded] { __ => (__ \ "firstname").read[Seq[String]] }.validate(valid) mustEqual(Valid(Seq("Julien")))
      From[UrlFormEncoded] { __ => (__ \ "foobar").read[Seq[String]] }.validate(valid) mustEqual(Valid(Seq()))
      From[UrlFormEncoded] { __ => (__ \ "foobar").read(isNotEmpty[Seq[Int]]) }.validate(valid) mustEqual(Invalid(Seq(Path \ "foobar" -> Seq(ValidationError("error.notEmpty")))))
    }

    "validate optional" in {
      From[UrlFormEncoded] { __ => (__ \ "firstname").read[Option[String]] }.validate(valid) mustEqual(Valid(Some("Julien")))
      From[UrlFormEncoded] { __ => (__ \ "firstname").read[Option[Int]] }.validate(valid) mustEqual(Invalid(Seq(Path \ "firstname" -> Seq(ValidationError("error.number", "Int")))))
      From[UrlFormEncoded] { __ => (__ \ "foobar").read[Option[String]] }.validate(valid) mustEqual(Valid(None))
    }

    "validate deep" in {
      val p = (Path \ "informations" \ "label")

      From[UrlFormEncoded] { __ =>
         (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(valid) mustEqual(Valid("Personal"))

       From[UrlFormEncoded] { __ =>
         (__ \ "informations").read(
          (__ \ "label").read(notEmpty))
      }.validate(invalid) mustEqual(Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[UrlFormEncoded]{ __ =>
        (__ \ "first" \ "second").read[Option[String]]
      }validate(Map.empty) mustEqual Valid(None)
    }

    "coerce type" in {
      From[UrlFormEncoded] { __ => (__ \ "age").read[Int] }.validate(valid) mustEqual(Valid(27))
      From[UrlFormEncoded] { __ => (__ \ "firstname").read[Int] }.validate(valid) mustEqual(Invalid(Seq((Path \ "firstname") -> Seq(ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      // TODO: create MonoidOps
      val composed = notEmpty |+| minLength(3)
      From[UrlFormEncoded] { __ => (__ \ "firstname").read(composed) }.validate(valid) mustEqual(Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"), ValidationError("error.minLength", 3))))
      From[UrlFormEncoded] { __ => (__ \ "informations" \ "label").read(composed) }.validate(invalid) mustEqual(err)
    }

    "compose validations" in {
      From[UrlFormEncoded] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty)){ _ -> _ }
      }.validate(valid) mustEqual Valid("Julien" -> "Tournay")

      From[UrlFormEncoded] { __ =>
        ((__ \ "firstname").read(notEmpty) ~
         (__ \ "lastname").read(notEmpty) ~
         (__ \ "informations" \ "label").read(notEmpty)).tupled
      }.validate(invalid) mustEqual Invalid(Seq((Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))))
    }

    "validate dependent fields" in {
      val v = Map(
        "login" -> Seq("Alice"),
        "password" -> Seq("s3cr3t"),
        "verify" -> Seq("s3cr3t"))

      val i1 = Map(
        "login" -> Seq("Alice"),
        "password" -> Seq("s3cr3t"),
        "verify" -> Seq(""))

      val i2 = Map(
        "login" -> Seq("Alice"),
        "password" -> Seq("s3cr3t"),
        "verify" -> Seq("bam"))

      val passRule: Rule[UrlFormEncoded, String] = From[UrlFormEncoded] { __ =>
        ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty))
          .tupled.compose(Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
      }

      val rule = From[UrlFormEncoded] { __ =>
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

      val b = Map("name" -> Seq("B"), "foo" -> Seq("4"))
      val c = Map("name" -> Seq("C"), "bar" -> Seq("6"))
      val e = Map("name" -> Seq("E"), "eee" -> Seq("6"))

      val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        val rb: Rule[UrlFormEncoded, A] = From[UrlFormEncoded]{ __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo").read[Int].map(B.apply)
        }

        val rc: Rule[UrlFormEncoded, A] = From[UrlFormEncoded]{ __ =>
          (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar").read[Int].map(C.apply)
        }

        val rule = rb orElse rc orElse Rule(_ => typeInvalid)

        rule.validate(b) mustEqual(Valid(B(4)))
        rule.validate(c) mustEqual(Valid(C(6)))
        rule.validate(e) mustEqual(Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }

      "by dicriminating on fields" in {

        val rule = From[UrlFormEncoded] { __ =>
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

      val validM = Map(
        "firstname" -> Seq("Julien"),
        "lastname" -> Seq("Tournay"),
        "age" -> Seq("27"),
        "informations[0].label" -> Seq("Personal"),
        "informations[0].email" -> Seq("fakecontact@gmail.com"),
        "informations[0].phones[]" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

      val validNoMail1 = Map(
        "firstname" -> Seq("Julien"),
        "lastname" -> Seq("Tournay"),
        "age" -> Seq("27"),
        "informations[0].label" -> Seq("Personal"),
        "informations[0].email" -> Seq(),
        "informations[0].phones[]" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

      val validNoMail3 = Map(
        "firstname" -> Seq("Julien"),
        "lastname" -> Seq("Tournay"),
        "age" -> Seq("27"),
        "informations[0].label" -> Seq("Personal"),
        "informations[0].email" -> Seq(""),
        "informations[0].phones[]" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

      val validNoMail2 = Map(
        "firstname" -> Seq("Julien"),
        "lastname" -> Seq("Tournay"),
        "age" -> Seq("27"),
        "informations[0].label" -> Seq("Personal"),
        "informations[0].phones[]" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

      val validWithPhones = Map(
        "firstname" -> Seq("Julien"),
        "lastname" -> Seq("Tournay"),
        "age" -> Seq("27"),
        "informations[0].label" -> Seq("Personal"),
        "informations[0].email" -> Seq("fakecontact@gmail.com"),
        "informations[0].phones[0]" -> Seq("01.23.45.67.89"),
        "informations[0].phones[1]" -> Seq("98.76.54.32.10"))

      val invalidM = Map(
        "firstname" -> Seq("Julien"),
        "lastname" -> Seq("Tournay"),
        "age" -> Seq("27"),
        "informations[0].label" -> Seq(""),
        "informations[0].email" -> Seq("fakecontact@gmail.com"),
        "informations[0].phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

      val infoValidated = From[UrlFormEncoded]{ __ =>
       ((__ \ "label").read(notEmpty) ~
        (__ \ "email").read(optionR(email)) ~
        (__ \ "phones").read(seqR(notEmpty))) (ContactInformation.apply)
      }

      val contactValidated = From[UrlFormEncoded]{ __ =>
       ((__ \ "firstname").read(notEmpty) ~
        (__ \ "lastname").read(notEmpty) ~
        (__ \ "company").read[Option[String]] ~
        (__ \ "informations").read(seqR(infoValidated))) (Contact.apply)
      }

      val expected =
        Contact("Julien", "Tournay", None, Seq(
          ContactInformation("Personal", Some("fakecontact@gmail.com"), List("01.23.45.67.89", "98.76.54.32.10"))))

      val expectedNoMail =
        Contact("Julien", "Tournay", None, Seq(
          ContactInformation("Personal", None, List("01.23.45.67.89", "98.76.54.32.10"))))

      contactValidated.validate(validM) mustEqual(Valid(expected))
      contactValidated.validate(validWithPhones) mustEqual(Valid(expected))
      contactValidated.validate(validNoMail1) mustEqual(Valid(expectedNoMail))
      contactValidated.validate(validNoMail2) mustEqual(Valid(expectedNoMail))
      contactValidated.validate(validNoMail3) mustEqual(Valid(expectedNoMail))
      contactValidated.validate(invalidM) mustEqual(Invalid(Seq(
        (Path \ "informations" \ 0 \ "label") -> Seq(ValidationError("error.required")))))
    }

    "read recursive" in {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser(
        "bob",
        Seq(RecUser("tom")))

      val m = Map(
        "name" -> Seq("bob"),
        "friends[0].name" -> Seq("tom"),
        "friends[0].friends" -> Seq())

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = Map(
        "name" -> Seq("bob"),
        "friend.name" -> Seq("tom"))

      "using explicit notation" in {
        lazy val w: Rule[UrlFormEncoded, RecUser] = From[UrlFormEncoded]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read(seqR(w)))(RecUser.apply)
        }
        w.validate(m) mustEqual Valid(u)

        lazy val w2: Rule[UrlFormEncoded, RecUser] =
          ((Path \ "name").read[UrlFormEncoded, String] ~
           (Path \ "friends").from[UrlFormEncoded](seqR(w2)))(RecUser.apply)
        w2.validate(m) mustEqual Valid(u)

        lazy val w3: Rule[UrlFormEncoded, User1] = From[UrlFormEncoded]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read(optionR(w3)))(User1.apply)
        }
        w3.validate(m1) mustEqual Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[UrlFormEncoded, RecUser] = From[UrlFormEncoded]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friends").read[Seq[RecUser]])(RecUser.apply)
        }
        w.validate(m) mustEqual Valid(u)

        implicit lazy val w3: Rule[UrlFormEncoded, User1] = From[UrlFormEncoded]{ __ =>
          ((__ \ "name").read[String] ~
           (__ \ "friend").read[Option[User1]])(User1.apply)
        }
        w3.validate(m1) mustEqual Valid(u1)
      }

    }
  }
}
