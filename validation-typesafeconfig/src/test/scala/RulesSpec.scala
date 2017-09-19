import jto.validation._
import jto.validation.typesafeconfig._, Rules._
import org.scalatest._
import com.typesafe.config._

class RulesSpec extends WordSpec with Matchers {


  val valid = ConfigFactory.parseString("""
    firstname:  Julien
    lastname:  Tournay
    age:  27
    information: {
      label:  Personal
      email:  "fakecontact@gmail.com"
      phones:  [ "01.23.45.67.89", "98.76.54.32.10"]
    }
    informations: [{
      label:  Personal
      email:  "fakecontact@gmail.com"
      phones:  [ "01.23.45.67.89", "98.76.54.32.10"]
    }]
  """).root

  val invalid = ConfigFactory.parseString("""
    firstname:  Julien
    lastname:  Tournay
    age:  27
    information: {
      label:  ""
      email:  "fakecontact@gmail.com"
      phones:  [ "01.23.45.67.89", "98.76.54.32.10"]
    }
    informations: [{
      label: ""
      email:  "fakecontact@gmail.com"
      phones:  [ "01.23.45.67.89", "98.76.54.32.10"]
    }]
  """).root

  def conf(v: Any) = ConfigFactory.parseString(s"""n: $v""").root
  def nested1(v: Any) = ConfigFactory.parseString(s"""n: { o: $v }""").root
  def nested2(v: Any) = ConfigFactory.parseString(s"""n: { o: { p: $v } }""").root
  val nullConf = ConfigFactory.parseString("""n: null""").root

  "Config Rules" should {
    "extract data" in {
      (Path \ "firstname").read[ConfigValue, String].validate(valid) shouldBe
      (Valid("Julien"))
      val errPath = Path \ "foo"
      val error =
        Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[ConfigValue, String].validate(invalid) shouldBe (error)
    }

    "support checked" in {
      def conf(v: Boolean) =  ConfigFactory.parseString(s"""issmth: $v""").root

      val p = Path \ "issmth"
      p.from[ConfigValue](checked).validate(conf(true)) shouldBe (Valid(true))

      p.from[ConfigValue](checked).validate(invalid) shouldBe
      (Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))

      p.from[ConfigValue](checked).validate(conf(false)) shouldBe
      (Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.equals", true)))))
    }

    "support all types of Config values" when {

      "null" in {


        (Path \ "n")
          .read[ConfigValue, None.type]
          .validate(nullConf) shouldBe (Valid(None))

        (Path \ "n")
          .read[ConfigValue, None.type]
          .validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "null")))))
      }

      "Int" in {
        (Path \ "n").read[ConfigValue, Int].validate(conf(4)) shouldBe (Valid(4))

        (Path \ "n").read[ConfigValue, Int].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n").read[ConfigValue, Int].validate(conf(4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o")
          .read[ConfigValue, Int]
          .validate(nested1(4)) shouldBe (Valid(4))

        (Path \ "n" \ "o")
          .read[ConfigValue, Int]
          .validate(nested1("foo")) shouldBe
        (Invalid(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        (Path \ "n" \ "o" \ "p")
          .read[ConfigValue, Int]
          .validate(nested2(4)) shouldBe(Valid(4))

        (Path \ "n" \ "o" \ "p")
          .read[ConfigValue, Int]
          .validate(nested2("foo")) shouldBe
        (Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[ConfigValue, Int].validate(conf(4)) shouldBe
        (error)
      }

      "Short" in {
        (Path \ "n").read[ConfigValue, Short].validate(conf(4)) shouldBe (Valid(4))

        (Path \ "n").read[ConfigValue, Short].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))

        (Path \ "n").read[ConfigValue, Short].validate(conf(4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        (Path \ "n").read[ConfigValue, Long].validate(conf(4)) shouldBe (Valid(4))

        (Path \ "n").read[ConfigValue, Long].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))

        (Path \ "n").read[ConfigValue, Long].validate(conf(4.5)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        (Path \ "n").read[ConfigValue, Float].validate(conf(4)) shouldBe (Valid(4))

        (Path \ "n").read[ConfigValue, Float].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))

        (Path \ "n").read[ConfigValue, Float].validate(conf(4.5)) shouldBe (Valid(4.5F))
      }

      "Double" in {
        (Path \ "n").read[ConfigValue, Double].validate(conf(4)) shouldBe (Valid(4))
        (Path \ "n").read[ConfigValue, Double].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))

        (Path \ "n").read[ConfigValue, Double].validate(conf(4.5)) shouldBe (Valid(4.5))
      }

      "scala BigDecimal" in {
        (Path \ "n").read[ConfigValue, BigDecimal].validate(conf(4)) shouldBe (Valid(BigDecimal(4)))

        (Path \ "n").read[ConfigValue, BigDecimal].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))

        (Path \ "n").read[ConfigValue, BigDecimal].validate(conf(4.5)) shouldBe (Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        (Path \ "n").read[ConfigValue, Boolean].validate(conf(true)) shouldBe (Valid(true))
        (Path \ "n").read[ConfigValue, Boolean].validate(conf("foo")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        (Path \ "n").read[ConfigValue, String].validate(conf("foo")) shouldBe (Valid("foo"))
        (Path \ "n").read[ConfigValue, String].validate(conf("\"foo\"")) shouldBe (Valid("foo"))

        (Path \ "n").read[ConfigValue, String].validate(conf(42)) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))

        (Path \ "n").read[ConfigValue, String].validate(conf("[foo]")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "String")))))


        val invalid = ConfigFactory.parseString("""o: { n: foo }""").root
        (Path \ "o").read[ConfigValue, String].validate(invalid) shouldBe
        (Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.invalid", "String")))))
      }

      "Option" in {
        (Path \ "n")
          .read[ConfigValue, Option[Boolean]]
          .validate(conf(true)) shouldBe (Valid(Some(true)))

        (Path \ "n")
          .read[ConfigValue, Option[Boolean]]
          .validate(conf("null")) shouldBe (Valid(None))

        (Path \ "n")
          .read[ConfigValue, Option[Boolean]]
          .validate(invalid) shouldBe (Valid(None))

        (Path \ "n")
          .read[ConfigValue, Option[Boolean]]
          .validate(conf("bar")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        (Path \ "n")
          .read[ConfigValue, Map[String, String]]
          .validate(conf("{ foo: bar }")) shouldBe
        (Valid(Map("foo" -> "bar")))

        (Path \ "n")
          .read[ConfigValue, Map[String, Int]]
          .validate(conf("{ foo: 4, bar: 5 }")) shouldBe
        (Valid(Map("foo" -> 4, "bar" -> 5)))

        (Path \ "x")
          .read[ConfigValue, Map[String, Int]]
          .validate(conf("{ foo: 4, bar: frack }")) shouldBe
        (Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))

        (Path \ "n")
          .read[ConfigValue, Map[String, Int]]
          .validate(conf("{ foo: 4, bar: frack }")) shouldBe
        (Invalid(Seq(Path \ "n" \ "bar" -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        (Path \ "n")
          .read[ConfigValue, Traversable[String]]
          .validate(conf("[foo]"))
          .toOption
          .get
          .toSeq shouldBe (Seq("foo"))

        (Path \ "n")
          .read[ConfigValue, Traversable[Int]]
          .validate(conf("[1, 2, 3]"))
          .toOption
          .get
          .toSeq shouldBe (Seq(1, 2, 3))

        (Path \ "n")
          .read[ConfigValue, Traversable[String]]
          .validate(conf("paf")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(
                        ValidationError("error.invalid", "Array")))))
      }

      "Array" in {
        (Path \ "n")
          .read[ConfigValue, Array[String]]
          .validate(conf("[foo]"))
          .toOption
          .get
          .toSeq shouldBe (Seq("foo"))

        (Path \ "n")
          .read[ConfigValue, Array[Int]]
          .validate(conf("[1, 2, 3]"))
          .toOption
          .get
          .toSeq shouldBe (Seq(1, 2, 3))

        (Path \ "n")
          .read[ConfigValue, Array[String]]
          .validate(conf("paf")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))
      }

      "Seq" in {
        (Path \ "n")
          .read[ConfigValue, Seq[String]]
          .validate(conf("[foo]"))
          .toOption
          .get shouldBe (Seq("foo"))
        (Path \ "n")
          .read[ConfigValue, Seq[Int]]
          .validate(conf("[1, 2, 3]"))
          .toOption
          .get shouldBe (Seq(1, 2, 3))
        (Path \ "n")
          .read[ConfigValue, Seq[String]]
          .validate(conf("paf")) shouldBe
        (Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Array")))))

        (Path \ "n")
          .read[ConfigValue, Seq[String]]
          .validate(conf("[foo, 12]")) shouldBe
        (Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.invalid", "String")))))
      }
    }

    "validate data" in {
      (Path \ "firstname").from[ConfigValue](notEmpty).validate(valid) shouldBe
      (Valid("Julien"))

      val p = (Path \ "information" \ "label")
      p.from[ConfigValue](notEmpty).validate(valid) shouldBe (Valid("Personal"))
      p.from[ConfigValue](notEmpty).validate(invalid) shouldBe
      (Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      (Path \ "firstname").read[ConfigValue, Option[String]].validate(valid) shouldBe
      (Valid(Some("Julien")))
      (Path \ "foobar").read[ConfigValue, Option[String]].validate(valid) shouldBe
      (Valid(None))
    }

    "validate deep" in {
      val p = (Path \ "information" \ "label")

      From[ConfigValue] { __ =>
        (__ \ "information").read((__ \ "label").read(notEmpty))
      }.validate(valid) shouldBe (Valid("Personal"))

      From[ConfigValue] { __ =>
        (__ \ "information").read((__ \ "label").read(notEmpty))
      }.validate(invalid) shouldBe
      (Invalid(Seq(p -> Seq(ValidationError("error.required")))))
    }

    "validate deep optional" in {
      From[ConfigValue] { __ =>
        (__ \ "first" \ "second").read[Option[String]]
      } validate (valid) shouldBe Valid(None)
    }

    "coerce type" in {
      (Path \ "age").read[ConfigValue, Int].validate(valid) shouldBe (Valid(27))
      (Path \ "age").from[ConfigValue](min(20)).validate(valid) shouldBe
      (Valid(27))
      (Path \ "age").from[ConfigValue](max(50)).validate(valid) shouldBe
      (Valid(27))
      (Path \ "age").from[ConfigValue](min(50)).validate(valid) shouldBe
      (Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
      (Path \ "age").from[ConfigValue](max(0)).validate(valid) shouldBe
      (Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
      (Path \ "firstname").read[ConfigValue, Int].validate(valid) shouldBe
      (Invalid(Seq((Path \ "firstname") -> Seq(
                      ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      val composed = notEmpty |+| minLength(3)
      (Path \ "firstname").from[ConfigValue](composed).validate(valid) shouldBe
      (Valid("Julien"))

      val p = Path \ "information" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"),
                                     ValidationError("error.minLength", 3))))
      p.from[ConfigValue](composed).validate(invalid) shouldBe (err)
    }

    "compose validations" in {
      From[ConfigValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty)).tupled
      }.validate(valid) shouldBe Valid("Julien" -> "Tournay")

      From[ConfigValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
            (__ \ "information" \ "label").read(notEmpty)).tupled
      }.validate(invalid) shouldBe Invalid(
          Seq((Path \ "information" \ "label") -> Seq(
                  ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      (Path \ "n")
        .from[ConfigValue](seqR(notEmpty))
        .validate(conf("[bar]"))
        .toOption
        .get shouldBe (Seq("bar"))

      From[ConfigValue] { __ =>
        (__ \ "n").read((__ \ "foo").read(seqR(notEmpty)))
      }.validate(conf("{ foo: [bar] }")).toOption.get shouldBe (Seq("bar"))

      (Path \ "n")
        .from[ConfigValue](seqR(notEmpty))
        .validate(conf("""[foo, ""]""")) shouldBe
      (Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      val v = ConfigFactory.parseString("""
        login: Alice
        password: s3cr3t
        verify: s3cr3t
      """).root

      val i1 = ConfigFactory.parseString("""
        login: Alice
        password: s3cr3t
        verify: ""
      """).root

      val i2 = ConfigFactory.parseString("""
        login: Alice
        password: s3cr3t
        verify: bam
      """).root

      val passRule = From[ConfigValue] { __ =>
        ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty)).tupled
          .andThen(
            Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
      }

      val rule = From[ConfigValue] { __ =>
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

      val b = ConfigFactory.parseString("""
        name: B
        foo: 4
      """).root

      val c = ConfigFactory.parseString("""
        name: C
        bar: 6
      """).root

      val e = ConfigFactory.parseString("""
        name: E
        eee: 6
      """).root

      val typeInvalid =
        Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        import cats.syntax.cartesian._

        val rb: Rule[ConfigValue, A] = From[ConfigValue] { __ =>
          (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo")
            .read[Int]
            .map(B.apply)
        }

        val rc: Rule[ConfigValue, A] = From[ConfigValue] { __ =>
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

      "by dicriminating on fields" in {

        val rule = From[ConfigValue] { __ =>
          (__ \ "name").read[String].flatMap[A] {
            case "B" => (__ \ "foo").read[Int].map(B.apply)
            case "C" => (__ \ "bar").read[Int].map(C.apply)
            case _ => Rule(_ => typeInvalid)
          }
        }

        rule.validate(b) shouldBe (Valid(B(4)))
        rule.validate(c) shouldBe (Valid(C(6)))
        rule.validate(e) shouldBe
        (Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
      }
    }

    "perform complex validation" in {

      case class Contact(firstname: String,
                         lastname: String,
                         company: Option[String],
                         informations: Seq[ContactInformation])

      case class ContactInformation(
          label: String, email: Option[String], phones: Seq[String])

      val infoValidated = From[ConfigValue] { __ =>
        ((__ \ "label").read(notEmpty) ~ (__ \ "email").read(optionR(email)) ~
            (__ \ "phones").read(seqR(notEmpty)))(ContactInformation.apply)
      }

      val contactValidated = From[ConfigValue] { __ =>
        ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
            (__ \ "company").read[Option[String]] ~ (__ \ "informations").read(
                seqR(infoValidated)))(Contact.apply)
      }

      val expected = Contact(
          "Julien",
          "Tournay",
          None,
          Seq(ContactInformation("Personal",
                                 Some("fakecontact@gmail.com"),
                                 List("01.23.45.67.89", "98.76.54.32.10"))))

      contactValidated.validate(valid) shouldBe (Valid(expected))
      contactValidated.validate(invalid) shouldBe
      (Invalid(Seq((Path \ "informations" \ 0 \ "label") -> Seq(
                      ValidationError("error.required")))))
    }

    "read recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      val m = ConfigFactory.parseString("""
        name: bob
        friends: [{
          name: tom
          friends: []
        }]
      """).root

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = ConfigFactory.parseString("""
        name: bob
        friend: {
          name: tom
        }
      """).root

      "using explicit notation" in {
        lazy val w: Rule[ConfigValue, RecUser] = From[ConfigValue] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friends").read(seqR(w)))(
              RecUser.apply)
        }
        w.validate(m) shouldBe Valid(u)

        lazy val w2: Rule[ConfigValue, RecUser] =
          ((Path \ "name").read[ConfigValue, String] ~ (Path \ "friends")
                .from[ConfigValue](seqR(w2)))(RecUser.apply)
        w2.validate(m) shouldBe Valid(u)

        lazy val w3: Rule[ConfigValue, User1] = From[ConfigValue] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friend").read(optionR(w3)))(
              User1.apply)
        }
        w3.validate(m1) shouldBe Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[ConfigValue, RecUser] = From[ConfigValue] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friends").read[Seq[RecUser]])(
              RecUser.apply)
        }
        w.validate(m) shouldBe Valid(u)

        implicit lazy val w3: Rule[ConfigValue, User1] = From[ConfigValue] { __ =>
          ((__ \ "name").read[String] ~ (__ \ "friend").read[Option[User1]])(
              User1.apply)
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
          ((__ \ "name").read(notEmpty) ~ (__ \ "color").read(opt(notEmpty))).tupled
        }

      val confR = genR[ConfigValue](optionR(_))

      val conf = ConfigFactory.parseString("""
        name: bob
        color: blue
      """).root

      val invalidConf = ConfigFactory.parseString("""
        color: blue
      """).root

      confR.validate(conf) shouldBe Valid(("bob", Some("blue")))
      confR.validate(invalidConf) shouldBe Invalid(
        Seq((Path \ "name", Seq(ValidationError("error.required")))))
    }
  }
}
