package play.api.data.mapping.xml

import org.specs2.mutable._
import play.api.data.mapping._
import scala.xml._

object RulesSpec extends Specification {

  "Xml rules" should {
    import play.api.data.mapping.xml.Rules._

    val valid =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        <informations label="Personal">
          <email>fakecontact@gmail.com</email>
          <phones>
            <phone label="mobile">01.02</phone>
            <phone label="home">02.03</phone>
          </phones>
        </informations>
      </person>

    val invalid =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        <informations label="">
          <email>fakecontactgmail.com</email>
          <phones>
            <phone label="mobile">01.02</phone>
            <phone label="home">01.02</phone>
          </phones>
        </informations>
      </person>

    "extract data" in {
      (Path \ "firstname").read[Node, String].validate(valid) === Success("Julien")
      val errPath = Path \ "foo"
      val error = Failure(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[Node, String].validate(invalid) mustEqual(error)
    }

    "support attribute checked" in {
      val xml = <item checked="true">Item 1</item>
      attributeR[Boolean]("checked").validate(xml) === Success(true)
      attributeR[Boolean]("checked").validate(<empty></empty>) === Failure(Seq(Path -> Seq(ValidationError("error.required"))))
    }

    "support primitive types" in {

      "Int" in {
        Path.read[Node, Int].validate(<a>4</a>) === Success(4)
        attributeR[Int]("attr").validate(<a attr="4"></a>) === Success(4)
        Path.read[Node, Int].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Int"))))
        Path.read[Node, Int].validate(<a>4.8</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Int"))))
        (Path \ "b").read[Node, Int].validate(<a><b>4</b></a>) === Success(4)
        (Path \ "b").from[Node](attributeR[Int]("attr")).validate(<a><b attr="4"></b></a>) === Success(4)
        (Path \ "b").from[Node](attributeR[Int]("attr")).validate(<a><b attr="a"></b></a>) ===
          Failure(Seq(Path \ "b" -> Seq(ValidationError("error.number", "Int"))))

        val errPath = Path \ "foo"
        val error = Failure(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[Node, Int].validate(<a>4</a>) === error
      }

      "Short" in {
        Path.read[Node, Short].validate(<a>4</a>) === Success(4)
        attributeR[Short]("attr").validate(<a attr="4"></a>) === Success(4)
        Path.read[Node, Short].validate(<a>4.8</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Short"))))
        Path.read[Node, Short].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Short"))))
      }

      "Long" in {
        Path.read[Node, Long].validate(<a>4</a>) === Success(4)
        attributeR[Long]("attr").validate(<a attr="4"></a>) === Success(4)
        Path.read[Node, Long].validate(<a>4.8</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Long"))))
        Path.read[Node, Long].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Long"))))
      }

      "Float" in {
        Path.read[Node, Float].validate(<a>4</a>) === Success(4F)
        attributeR[Float]("attr").validate(<a attr="4"></a>) === Success(4F)
        Path.read[Node, Float].validate(<a>4.8</a>) === Success(4.8F)
        Path.read[Node, Float].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Float"))))
      }

      "Float" in {
        Path.read[Node, Double].validate(<a>4</a>) === Success(4D)
        attributeR[Double]("attr").validate(<a attr="4"></a>) === Success(4D)
        Path.read[Node, Double].validate(<a>4.8</a>) === Success(4.8D)
        Path.read[Node, Double].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Double"))))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        Path.read[Node, jBigDecimal].validate(<a>4</a>) === Success(new jBigDecimal("4"))
        attributeR[jBigDecimal]("attr").validate(<a attr="4"></a>) === Success(new jBigDecimal("4"))
        Path.read[Node, jBigDecimal].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "BigDecimal"))))
      }

      "scala BigDecimal" in {
        Path.read[Node, BigDecimal].validate(<a>4</a>) === Success(BigDecimal("4"))
        attributeR[BigDecimal]("attr").validate(<a attr="4"></a>) === Success(BigDecimal("4"))
        Path.read[Node, BigDecimal].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "BigDecimal"))))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        Path.from[Node](Rules.date).validate(<a>1985-09-10</a>) mustEqual(Success(f.parse("1985-09-10")))
        Path.from[Node](Rules.date).validate(<a>foo</a>) mustEqual(Failure(Seq(Path -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          Path.from[Node](Rules.jodaDate).validate(<a>1985-09-10</a>) mustEqual(Success(jd))
          Path.from[Node](Rules.jodaDate).validate(<a>foo</a>) mustEqual(Failure(Seq(Path -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          Path.from[Node](Rules.jodaTime).validate(<a>{dd.getTime}</a>) mustEqual(Success(jd))
          Path.from[Node](Rules.jodaDate).validate(<a>foo</a>) mustEqual(Failure(Seq(Path -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          Path.from[Node](Rules.jodaLocalDate).validate(<a>{ld.toString}</a>) mustEqual(Success(ld))
          Path.from[Node](Rules.jodaLocalDate).validate(<a>foo</a>) mustEqual(Failure(Seq(Path -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        Path.from[Node](Rules.sqlDate).validate(<a>1985-09-10</a>) mustEqual(Success(ds))
      }

      "Boolean" in {
        Path.read[Node, Boolean].validate(<a>true</a>) === Success(true)
        attributeR[Boolean]("attr").validate(<a attr="true"></a>) === Success(true)
        Path.read[Node, Boolean].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.invalid", "Boolean"))))
      }

      "String" in {
        Path.read[Node, String].validate(<a>foo</a>) === Success("foo")
        attributeR[String]("attr").validate(<a attr="foo"></a>) === Success("foo")
        Path.read[Node, Boolean].validate(<a><b>foo</b></a>) === Failure(Seq(Path -> Seq(ValidationError("error.invalid", "a non-leaf node can not be validated to String"))))
      }

      "Node" in {
        (Path \ "b").read[Node, Node].validate(<a><b>foo</b></a>) === Success(<b>foo</b>)
      }

      "Option" in {
        Path.read[Node, Option[Boolean]].validate(<a>true</a>) mustEqual(Success(Some(true)))
        (Path \ "b").read[Node, Option[Boolean]].validate(<a>hello</a>) mustEqual(Success(None))
        Path.read[Node, Option[Boolean]].validate(<a>foo</a>) mustEqual(Failure(Seq(Path -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Seq" in {
        Path.read[Node, Seq[String]].validate(<a><b>foo</b></a>).get must contain(exactly(Seq("foo"): _*))
        Path.read[Node, Seq[Int]].validate(<a><b>1</b><b>2</b></a>).get must contain(exactly(Seq(1, 2): _*))
        Path.read[Node, Seq[String]].validate(<a></a>).get  must contain(exactly(Seq.empty[String]: _*))
        Path.read[Node, Seq[String]].validate(<a>1</a>).get  must contain(exactly(Seq.empty[String]: _*))
        Path.read[Node, Seq[Int]].validate(<a><b>1</b><b>foo</b></a>) mustEqual(Failure(Seq(Path \ 1 -> Seq(ValidationError("error.number", "Int")))))
      }

      "validate data with composed contraints on attributes" in {
        (Path \ "firstname").from[Node](notEmpty).validate(valid) mustEqual(Success("Julien"))

        val p = (Path \ "informations")
        p.from[Node](attributeR[String]("label") compose notEmpty).validate(valid) mustEqual(Success("Personal"))
        p.from[Node](attributeR[String]("label") compose notEmpty).validate(invalid) mustEqual(Failure(Seq(p -> Seq(ValidationError("error.required")))))
      }

      "validate optional fields and attributes" in {
        val reads = From[Node] { __ =>
          (
            (__ \ "firstname").read[Option[String]] ~
            (__ \ "age").read[Option[Int]] ~
            (__ \ "foo").read[Option[String]] ~
            (__ \ "firstname").read(optAttributeR[String]("foo"))
            tupled
          )
        }
        reads.validate(valid) === Success((Some("Julien"), Some(27), None, None))

        val readsInvalid = From[Node] { __ =>
          (__ \ "foo").read(optAttributeR[String]("foo"))
        }
        readsInvalid.validate(valid) === Failure(Seq(Path \ "foo" -> Seq(ValidationError("error.required"))))
      }

      "validate deep" in {
        val p = (Path \ "informations")

        From[Node] { __ =>
          (__ \ "informations").read(
            (__ \ "email").read(email))
        }.validate(valid) mustEqual(Success("fakecontact@gmail.com"))

        From[Node] { __ =>
          (__ \ "informations").read(
            (__ \ "foo").read(email))
        }.validate(invalid) mustEqual(Failure(Seq((p \ "foo") -> Seq(ValidationError("error.required")))))
      }

      "coerce type" in {
        (Path \ "age").read[Node, Int].validate(valid) mustEqual(Success(27))
        (Path \ "age").from[Node](min(20)).validate(valid) mustEqual(Success(27))
        (Path \ "age").from[Node](max(50)).validate(valid) mustEqual(Success(27))
        (Path \ "age").from[Node](min(50)).validate(valid) mustEqual(Failure(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
        (Path \ "age").from[Node](max(0)).validate(valid) mustEqual(Failure(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
        (Path \ "firstname").read[Node, Int].validate(valid) mustEqual(Failure(Seq((Path \ "firstname") -> Seq(ValidationError("error.number", "Int")))))
      }

      "compose validations" in {
        From[Node]{ __ =>
          ((__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty)).tupled
        }.validate(valid) mustEqual Success("Julien" -> "Tournay")

        From[Node]{ __ =>
          ((__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty) ~
            (__ \ "informations" \ "label").read(notEmpty)).tupled
        }.validate(invalid) mustEqual Failure(Seq((Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))))
      }

      "lift validations to seq validations" in {
        Path.from[Node](seqR(notEmpty)).validate(<a><b>foo</b><b>bar</b></a>)
          .get must contain(exactly(Seq("foo", "bar"): _*))

        From[Node]{ __ =>
          (__ \ "b").read(seqR(notEmpty))
        }.validate(<a><b><c>foo</c><c>bar</c></b></a>)
          .get must contain(exactly(Seq("foo", "bar"): _*))

        Path.from[Node](seqR(notEmpty))
          .validate(<a><b>foo</b><b></b></a>) mustEqual(Failure(Seq(Path \ 1 -> Seq(ValidationError("error.required")))))
      }

      "validate dependent fields" in {
        val v =
          <form>
            <login>Alice</login>
            <password>s3cr3t</password>
            <verify>s3cr3t</verify>
          </form>

        val i1 =
          <form>
            <login>Alice</login>
            <password>s3cr3t</password>
            <verify></verify>
          </form>

        val i2 =
          <form>
            <login>Alice</login>
            <password>s3cr3t</password>
            <verify>bam</verify>
          </form>

        val passRule = From[Node] { __ =>
          ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty))
            .tupled.compose(Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
        }

        val rule = From[Node] { __ =>
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
          
        val b = <a><name>B</name><foo>4</foo></a>
        val c = <a><name>C</name><bar>6</bar></a>
        val e = <a><name>E</name><eee>6</eee></a>

        val typeFailure = Failure(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

        "by trying all possible Rules" in {
          val rb: Rule[Node, A] = From[Node]{ __ =>
            (__ \ "name").read(Rules.equalTo("B")) ~> (__ \ "foo").read[Int].fmap(B.apply _)
          }

          val rc: Rule[Node, A] = From[Node]{ __ =>
            (__ \ "name").read(Rules.equalTo("C")) ~> (__ \ "bar").read[Int].fmap(C.apply _)
          }

          val rule = rb orElse rc orElse Rule(_ => typeFailure)

          rule.validate(b) mustEqual(Success(B(4)))
          rule.validate(c) mustEqual(Success(C(6)))
          rule.validate(e) mustEqual(Failure(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
        }

        "by dicriminating on fields" in {

          val rule = From[Node] { __ =>
            (__ \ "name").read[String].flatMap[A] {
              case "B" => (__ \ "foo").read[Int].fmap(B.apply _)
              case "C" => (__ \ "bar").read[Int].fmap(C.apply _)
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
                            informations: ContactInformation)

        case class ContactInformation(
                                       label: String,
                                       email: Option[String],
                                       phones: Seq[String])

        val infoValidation = From[Node] { __ =>
          (
            (attributeR[String]("label") compose notEmpty) ~
            (__ \ "email").read(optionR(email)) ~
            (__ \ "phones").read(seqR(notEmpty))
          )(ContactInformation.apply _)
        }

        val contactValidation = From[Node] { __ =>
          ((__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty) ~
            (__ \ "company").read[Option[String]] ~
            (__ \ "informations").read(infoValidation)) (Contact.apply _)
        }

        val expected =
          Contact("Julien", "Tournay", None,
            ContactInformation("Personal", Some("fakecontact@gmail.com"), List("01.02", "02.03")))

        contactValidation.validate(valid) mustEqual(Success(expected))
        contactValidation.validate(invalid) mustEqual(Failure(Seq(
          (Path \ "informations") -> Seq(ValidationError("error.required")), Path \ "informations" \ "email" ->
          Seq(ValidationError("error.email")))))
      }

      "read recursive" in {
        case class RecUser(name: String, friends: Seq[RecUser] = Nil)
        val u = RecUser(
          "bob",
          Seq(RecUser("tom")))

        val m =
          <user>
            <name>bob</name>
            <friends>
              <user>
                <name>tom</name>
                <friends></friends>
              </user>
            </friends>
          </user>

        case class User1(name: String, friend: Option[User1] = None)
        val u1 = User1("bob", Some(User1("tom")))

        val m1 =
          <user>
            <name>bob</name>
            <friend>
              <name>tom</name>
            </friend>
          </user>

        "using explicit notation" in {
          lazy val w: Rule[Node, RecUser] = From[Node]{ __ =>
            ((__ \ "name").read[String] ~
             (__ \ "friends").read(seqR(w))
            )(RecUser.apply _)
          }
          w.validate(m) mustEqual Success(u)

          lazy val w2: Rule[Node, RecUser] =
            ((Path \ "name").read[Node, String] ~
              (Path \ "friends").from[Node](seqR(w2)))(RecUser.apply _)
          w2.validate(m) mustEqual Success(u)

          lazy val w3: Rule[Node, User1] = From[Node]{ __ =>
            ((__ \ "name").read[String] ~
              (__ \ "friend").read(optionR(w3)))(User1.apply _)
          }
          w3.validate(m1) mustEqual Success(u1)
        }

        "using implicit notation" in {
          implicit lazy val w: Rule[Node, RecUser] = From[Node]{ __ =>
            ((__ \ "name").read[String] ~
              (__ \ "friends").read[Seq[RecUser]])(RecUser.apply _)
          }
          w.validate(m) mustEqual Success(u)

          implicit lazy val w3: Rule[Node, User1] = From[Node]{ __ =>
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

        val jsonR = genR[Node](optionR(_))

        val validXml = <a><name>bob</name><color>blue</color></a>
        val invalidXml = <a><color>blue</color></a>

        jsonR.validate(validXml) mustEqual Success(("bob", Some("blue")))
        jsonR.validate(invalidXml) mustEqual Failure(Seq((Path \ "name", Seq(ValidationError("error.required")))))

      }

      "use attribute filtering" in {
        val entityXml =
          <entity>
            <prop name="name" value="Alexandre"></prop>
            <prop name="age" value="25"></prop>
            <prop name="job" value="software engineer" type="fulltime"></prop>
          </entity>

        val reads = From[Node] { __ =>
          import Rules._
          (
            pickChildWithAttribute("prop", attrKey = "name", attrValue = "job")(
              attributeR[String]("value") ~
              attributeR[String]("type")
              tupled
            ) ~
              pickChildWithAttribute("prop", attrKey = "name", attrValue = "age")(
                attributeR[Int]("value")
              )
            ) tupled
        }

        val invalidXml =
          <entity>
            <prop name="name" value="Alexandre"></prop>
            <prop name="job" value="software engineer" type="fulltime"></prop>
          </entity>

        reads.validate(invalidXml) === Failure(Seq((Path, Seq(ValidationError("error.required", "child with attribute name = age not found")))))
      }

    }

  }
}
