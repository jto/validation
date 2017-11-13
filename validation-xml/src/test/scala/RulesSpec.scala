import jto.validation._
import jto.validation.xml._
import jto.validation.xml.Rules._
import java.math.{BigDecimal => jBigDecimal}
import org.scalatest._
import scala.xml.Node

class RulesSpec extends WordSpec with Matchers {

  "Xml rules" should {

    val valid = <person>
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

    val invalid = <person>
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
      (Path \ "firstname").read[Node, String].validate(valid) shouldBe Valid(
          "Julien")
      val errPath = Path \ "foo"
      val error =
        Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[Node, String].validate(invalid) shouldBe (error)
    }

    "support attribute checked" in {
      val xml = <item checked="true">Item 1</item>
      attributeR[Boolean]("checked").validate(xml) shouldBe Valid(true)
      attributeR[Boolean]("checked").validate(<empty></empty>) shouldBe Invalid(
          Seq(Path -> Seq(ValidationError("error.required"))))
    }

    "read primitive types" when {

      "Int" in {
        Path.read[Node, Int].validate(<a>4</a>) shouldBe Valid(4)
        attributeR[Int]("attr").validate(<a attr="4"></a>) shouldBe Valid(4)
        Path.read[Node, Int].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Int"))))
        Path.read[Node, Int].validate(<a>4.5</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Int"))))
        (Path \ "b").read[Node, Int].validate(<a><b>4</b></a>) shouldBe Valid(
            4)
        (Path \ "b")
          .from[Node](attributeR[Int]("attr"))
          .validate(<a><b attr="4"></b></a>) shouldBe Valid(4)
        (Path \ "b")
          .from[Node](attributeR[Int]("attr"))
          .validate(<a><b attr="a"></b></a>) shouldBe Invalid(
            Seq(Path \ "b" -> Seq(ValidationError("error.number", "Int"))))

        val errPath = Path \ "foo"
        val error =
          Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        errPath.read[Node, Int].validate(<a>4</a>) shouldBe error
      }

      "Short" in {
        Path.read[Node, Short].validate(<a>4</a>) shouldBe Valid(4)
        attributeR[Short]("attr").validate(<a attr="4"></a>) shouldBe Valid(4)
        Path.read[Node, Short].validate(<a>4.5</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Short"))))
        Path.read[Node, Short].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Short"))))
      }

      "Long" in {
        Path.read[Node, Long].validate(<a>4</a>) shouldBe Valid(4)
        attributeR[Long]("attr").validate(<a attr="4"></a>) shouldBe Valid(4)
        Path.read[Node, Long].validate(<a>4.5</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Long"))))
        Path.read[Node, Long].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Long"))))
      }

      "Float" in {
        Path.read[Node, Float].validate(<a>4</a>) shouldBe Valid(4F)
        attributeR[Float]("attr").validate(<a attr="4"></a>) shouldBe Valid(4F)
        Path.read[Node, Float].validate(<a>4.5</a>) shouldBe Valid(4.5F)
        Path.read[Node, Float].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Float"))))
      }

      "Double" in {
        Path.read[Node, Double].validate(<a>4</a>) shouldBe Valid(4D)
        attributeR[Double]("attr").validate(<a attr="4"></a>) shouldBe Valid(
            4D)
        Path.read[Node, Double].validate(<a>4.5</a>) shouldBe Valid(4.5D)
        Path.read[Node, Double].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "Double"))))
      }

      "java BigDecimal" in {
        Path.read[Node, jBigDecimal].validate(<a>4</a>) shouldBe Valid(
            new jBigDecimal("4"))
        attributeR[jBigDecimal]("attr").validate(<a attr="4"></a>) shouldBe Valid(
            new jBigDecimal("4"))
        Path.read[Node, jBigDecimal].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "BigDecimal"))))
      }

      "scala BigDecimal" in {
        Path.read[Node, BigDecimal].validate(<a>4</a>) shouldBe Valid(
            BigDecimal("4"))
        attributeR[BigDecimal]("attr").validate(<a attr="4"></a>) shouldBe Valid(
            BigDecimal("4"))
        Path.read[Node, BigDecimal].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.number", "BigDecimal"))))
      }

      "Boolean" in {
        Path.read[Node, Boolean].validate(<a>true</a>) shouldBe Valid(true)
        attributeR[Boolean]("attr").validate(<a attr="true"></a>) shouldBe Valid(
            true)
        Path.read[Node, Boolean].validate(<a>no</a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError("error.invalid", "Boolean"))))
      }

      "String" in {
        Path.read[Node, String].validate(<a>foo</a>) shouldBe Valid("foo")
        attributeR[String]("attr").validate(<a attr="foo"></a>) shouldBe Valid(
            "foo")
        Path.read[Node, Boolean].validate(<a><b>foo</b></a>) shouldBe Invalid(
            Seq(Path -> Seq(ValidationError(
                        "error.invalid",
                        "a non-leaf node can not be validated to String"))))
      }

      "Node" in {
        (Path \ "b").read[Node, Node].validate(<a><b>foo</b></a>) shouldBe Valid(
            <b>foo</b>)
      }

      "Option" in {
        Path.read[Node, Option[Boolean]].validate(<a>true</a>) shouldBe
        (Valid(Some(true)))
        (Path \ "b").read[Node, Option[Boolean]].validate(<a>hello</a>) shouldBe
        (Valid(None))
        Path.read[Node, Option[Boolean]].validate(<a>foo</a>) shouldBe
        (Invalid(
                Seq(Path -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Seq" in {
        Path.read[Node, Seq[String]].validate(<a><b>foo</b></a>).toOption.get shouldBe
        (Seq("foo"))
        Path
          .read[Node, Seq[Int]]
          .validate(<a><b>1</b><b>2</b></a>)
          .toOption
          .get shouldBe (Seq(1, 2))
        Path.read[Node, Seq[String]].validate(<a></a>).toOption.get shouldBe
        (Seq.empty[String])
        Path.read[Node, Seq[String]].validate(<a>1</a>).toOption.get shouldBe
        (Seq.empty[String])
        Path.read[Node, Seq[Int]].validate(<a><b>1</b><b>foo</b></a>) shouldBe
        (Invalid(Seq(Path \ 1 -> Seq(ValidationError("error.number", "Int")))))
      }

      "validate data with composed contraints on attributes" in {
        (Path \ "firstname").from[Node](notEmpty).validate(valid) shouldBe
        (Valid("Julien"))

        val p = (Path \ "informations")
        p.from[Node](attributeR[String]("label") andThen notEmpty)
          .validate(valid) shouldBe (Valid("Personal"))
        p.from[Node](attributeR[String]("label") andThen notEmpty)
          .validate(invalid) shouldBe
        (Invalid(Seq(p -> Seq(ValidationError("error.required")))))
      }

      "validate optional fields and attributes" in {
        val reads = From[Node] { __ =>
          ((__ \ "firstname").read[Option[String]] ~ (__ \ "age")
            .read[Option[Int]] ~ (__ \ "foo").read[Option[String]] ~
          (__ \ "firstname").read(optAttributeR[String]("foo")) tupled)
        }
        reads.validate(valid) shouldBe Valid(
            (Some("Julien"), Some(27), None, None))

        val readsInvalid = From[Node] { __ =>
          (__ \ "foo").read(optAttributeR[String]("foo"))
        }
        readsInvalid.validate(valid) shouldBe Invalid(
            Seq(Path \ "foo" -> Seq(ValidationError("error.required"))))
      }

      "validate deep" in {
        val p = (Path \ "informations")

        From[Node] { __ =>
          (__ \ "informations").read((__ \ "email").read(email))
        }.validate(valid) shouldBe (Valid("fakecontact@gmail.com"))

        From[Node] { __ =>
          (__ \ "informations").read((__ \ "foo").read(email))
        }.validate(invalid) shouldBe
        (Invalid(Seq((p \ "foo") -> Seq(ValidationError("error.required")))))
      }

      "validate deep optional" in {
        From[Node] { __ =>
          (__ \ "first" \ "second").read[Option[String]]
        } validate (invalid) shouldBe Valid(None)
      }

      "coerce type" in {
        (Path \ "age").read[Node, Int].validate(valid) shouldBe (Valid(27))
        (Path \ "age").from[Node](min(20)).validate(valid) shouldBe (Valid(27))
        (Path \ "age").from[Node](max(50)).validate(valid) shouldBe (Valid(27))
        (Path \ "age").from[Node](min(50)).validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.min", 50)))))
        (Path \ "age").from[Node](max(0)).validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") -> Seq(ValidationError("error.max", 0)))))
        (Path \ "firstname").read[Node, Int].validate(valid) shouldBe
        (Invalid(Seq((Path \ "firstname") -> Seq(
                        ValidationError("error.number", "Int")))))
      }

      "compose validations" in {
        From[Node] { __ =>
          ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(
                  notEmpty)).tupled
        }.validate(valid) shouldBe Valid("Julien" -> "Tournay")

        From[Node] { __ =>
          ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
              (__ \ "informations" \ "label").read(notEmpty)).tupled
        }.validate(invalid) shouldBe Invalid(
            Seq((Path \ "informations" \ "label") -> Seq(
                    ValidationError("error.required"))))
      }

      "lift validations to seq validations" in {
        Path
          .from[Node](seqR(notEmpty))
          .validate(<a><b>foo</b><b>bar</b></a>)
          .toOption
          .get shouldBe (Seq("foo", "bar"))

        From[Node] { __ =>
          (__ \ "b").read(seqR(notEmpty))
        }.validate(<a><b><c>foo</c><c>bar</c></b></a>).toOption.get shouldBe
        (Seq("foo", "bar"))

        Path.from[Node](seqR(notEmpty)).validate(<a><b>foo</b><b></b></a>) shouldBe
        (Invalid(Seq(Path \ 1 -> Seq(ValidationError("error.required")))))
      }

      "validate dependent fields" in {
        val v = <form>
            <login>Alice</login>
            <password>s3cr3t</password>
            <verify>s3cr3t</verify>
          </form>

        val i1 = <form>
            <login>Alice</login>
            <password>s3cr3t</password>
            <verify></verify>
          </form>

        val i2 = <form>
            <login>Alice</login>
            <password>s3cr3t</password>
            <verify>bam</verify>
          </form>

        val passRule = From[Node] { __ =>
          ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty)).tupled
            .andThen(Rule
                .uncurry(Rules.equalTo[String]))
        }

        val rule = From[Node] { __ =>
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

        val b = <a><name>B</name><foo>4</foo></a>
        val c = <a><name>C</name><bar>6</bar></a>
        val e = <a><name>E</name><eee>6</eee></a>

        val typeInvalid =
          Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

        "by trying all possible Rules" in {
          import cats.syntax.cartesian._

          val rb: Rule[Node, A] = From[Node] { __ =>
            (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo")
              .read[Int]
              .map(B.apply)
          }

          val rc: Rule[Node, A] = From[Node] { __ =>
            (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar")
              .read[Int]
              .map(C.apply)
          }

          val rule = rb orElse rc orElse Rule(_ => typeInvalid)

          rule.validate(b) shouldBe (Valid(B(4)))
          rule.validate(c) shouldBe (Valid(C(6)))
          rule.validate(e) shouldBe
          (Invalid(
                  Seq(Path -> Seq(ValidationError("validation.unknownType")))))
        }

        "by dicriminating on fields" in {

          val rule = From[Node] { __ =>
            (__ \ "name").read[String].flatMap[A] {
              case "B" => (__ \ "foo").read[Int].map(B.apply)
              case "C" => (__ \ "bar").read[Int].map(C.apply)
              case _ => Rule(_ => typeInvalid)
            }
          }

          rule.validate(b) shouldBe (Valid(B(4)))
          rule.validate(c) shouldBe (Valid(C(6)))
          rule.validate(e) shouldBe
          (Invalid(
                  Seq(Path \ "name" -> Seq(ValidationError("validation.unknownType")))))
        }
      }

      "perform complex validation" in {

        case class Contact(firstname: String,
                           lastname: String,
                           company: Option[String],
                           informations: ContactInformation)

        case class ContactInformation(label: String,
                                      email: Option[String],
                                      phones: Seq[String])

        val infoValidated = From[Node] { __ =>
          (
            (attributeR[String]("label") andThen notEmpty) ~
            (__ \ "email").read(optionR(email)) ~
            (__ \ "phones").read(seqR(notEmpty))
          )(ContactInformation.apply)
        }

        val contactValidated = From[Node] { __ =>
          (
            (__ \ "firstname").read(notEmpty) ~
            (__ \ "lastname").read(notEmpty) ~
            (__ \ "company").read[Option[String]] ~
            (__ \ "informations").read(infoValidated)
          )(Contact.apply)
        }

        val expected =
          Contact("Julien",
                  "Tournay",
                  None,
                  ContactInformation("Personal",
                                     Some("fakecontact@gmail.com"),
                                     List("01.02", "02.03")))

        // contactValidated.validate(valid) shouldBe (Valid(expected))
        contactValidated.validate(invalid) shouldBe
        (Invalid(Seq((Path \ "informations") -> Seq(
                         ValidationError("error.required")),
                     Path \ "informations" \ "email" -> Seq(
                         ValidationError("error.email")))))
      }

      "read recursive" when {
        case class RecUser(name: String, friends: Seq[RecUser] = Nil)
        val u = RecUser("bob", Seq(RecUser("tom")))

        val m = <user>
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

        val m1 = <user>
            <name>bob</name>
            <friend>
              <name>tom</name>
            </friend>
          </user>

        "using explicit notation" in {
          lazy val w: Rule[Node, RecUser] = From[Node] { __ =>
            ((__ \ "name").read[String] ~ (__ \ "friends").read(seqR(w)))(
                RecUser.apply)
          }
          w.validate(m) shouldBe Valid(u)

          lazy val w2: Rule[Node, RecUser] =
            ((Path \ "name").read[Node, String] ~ (Path \ "friends")
                  .from[Node](seqR(w2)))(RecUser.apply)
          w2.validate(m) shouldBe Valid(u)

          lazy val w3: Rule[Node, User1] = From[Node] { __ =>
            ((__ \ "name").read[String] ~ (__ \ "friend").read(optionR(w3)))(
                User1.apply)
          }
          w3.validate(m1) shouldBe Valid(u1)
        }

        "using implicit notation" in {
          implicit lazy val w: Rule[Node, RecUser] = From[Node] { __ =>
            ((__ \ "name").read[String] ~ (__ \ "friends").read[Seq[RecUser]])(
                RecUser.apply)
          }
          w.validate(m) shouldBe Valid(u)

          implicit lazy val w3: Rule[Node, User1] = From[Node] { __ =>
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

        val jsonR = genR[Node](optionR(_))

        val validXml = <a><name>bob</name><color>blue</color></a>
        val invalidXml = <a><color>blue</color></a>

        jsonR.validate(validXml) shouldBe Valid(("bob", Some("blue")))
        jsonR.validate(invalidXml) shouldBe Invalid(
            Seq((Path \ "name", Seq(ValidationError("error.required")))))
      }

      "use attribute filtering" in {
        val reads = (
          pickChildsWithAttribute[String]("entity", attrKey = "type", attrValue = "type1")
                                         (attributeR[String]("name"))
        )

        val entityXml =
          <entities>
            <entity type="type1" name="Alexandre"></entity>
            <entity type="type2" name="Jean"></entity>
            <entity type="type1" name="Pierre"></entity>
            <prop type="type1" name="Jean"></prop>
          </entities>

        val emptyXml =
          <entities>
            <entity type="type2" name="Jean"></entity>
          </entities>

        val invalidXml =
          <entities>
            <entity type="type1"></entity>
          </entities>

        reads.validate(entityXml)  shouldBe Valid(Seq("Alexandre", "Pierre"))
        reads.validate(emptyXml)   shouldBe Valid(Seq.empty[String])
        reads.validate(invalidXml) shouldBe Invalid(
          Seq((Path \ 0, Seq(ValidationError("error.required"))))
        )

      }

      "use attribute filtering to read first" in {
        val entityXml = <entity>
            <entity name="age" value="24"></entity>
            <prop name="name" value="Alexandre"></prop>
            <prop name="age" value="25"></prop>
            <prop name="job" value="software engineer" type="fulltime"></prop>
          </entity>

        val reads = From[Node] { __ =>
          (pickChildWithAttribute("prop", attrKey = "name", attrValue = "job")(
                  attributeR[String]("value") ~ attributeR[String]("type") tupled
              ) ~ pickChildWithAttribute("prop",
                                         attrKey = "name",
                                         attrValue = "age")(
                  attributeR[Int]("value")
              )) tupled
        }

        reads.validate(entityXml) shouldBe Valid( ("software engineer" -> "fulltime", 25) )

        val invalidXml = <entity>
            <prop name="name" value="Alexandre"></prop>
            <prop name="job" value="software engineer" type="fulltime"></prop>
          </entity>

        reads.validate(invalidXml) shouldBe Invalid(
            Seq((Path,
                 Seq(ValidationError(
                         "error.required",
                         "child with attribute name = age not found")))))
      }
    }
  }
}
