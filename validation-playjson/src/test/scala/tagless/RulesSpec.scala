package jto.validation
package v3.tagless
package playjson

import jto.validation._
import org.scalatest._
import play.api.libs.json._
// import cats.syntax.compose._

trait TestCases[T] {
  trait base {
    def valid: T
    def invalid: T
  }

  trait int {
    def ok: T
    def foo: T
    def float: T
    def noOK: T
    def noFoo: T
    def nopOK: T
    def nopFoo: T
  }

  trait boolean {
    def ok: T
    def foo: T
  }

  trait string {
    def foo: T
    def foos: T
    def _42: T
    def onFoo: T
  }

  trait option {
    def nNull: T
    def fooBar: T
    def nBar: T
  }

  val base: base
  val int: int
  val boolean: boolean
  val string: string
  val option: option
}

object JsonTestCases extends TestCases[JsValue] {

  override val base = new base {
    val valid =
      Json.obj("firstname" -> "Julien",
               "lastname" -> "Tournay",
               "age" -> 27,
               "informations" ->
                  Json.obj("label" -> "Personal",
                    "email" -> "fakecontact@gmail.com",
                    "phones" -> Seq("01.23.45.67.89",
                                    "98.76.54.32.10")))

    val invalid =
      Json.obj("firstname" -> "Julien",
               "lastname" -> "Tournay",
               "age" -> 27,
               "informations" ->
                  Json.obj("label" -> "",
                    "email" -> "fakecontact@gmail.com",
                    "phones" -> Seq("01.23.45.67.89",
                                    "98.76.54.32.10")))
  }

  val int = new int {
    val ok = Json.obj("n" -> 4)
    val foo = Json.obj("n" -> "foo")
    val float = Json.obj("n" -> 4.5)
    val noOK = Json.obj("n" -> Json.obj("o" -> 4))
    val noFoo = Json.obj("n" -> Json.obj("o" -> "foo"))
    val nopOK = Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> 4)))
    val nopFoo = Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> "foo")))
  }

  val boolean = new boolean {
    val ok = Json.obj("n" -> true)
    val foo = int.foo
  }

  val string = new string {
    val foo = int.foo
    val foos = Json.obj("n" -> Seq("foo"))
    val _42 = Json.obj("n" -> 42)
    val onFoo = Json.obj("o" -> Json.obj("n" -> "foo"))
  }

  val option = new option {
    val nNull = Json.obj("n" -> JsNull)
    val fooBar = Json.obj("foo" -> "bar")
    val nBar = Json.obj("n" -> "bar")
  }

}

trait RulesSpec[T] extends WordSpec with Matchers {

  val testCases: TestCases[T]
  implicit val grammar: Grammar[T, Rule]

  import grammar._

  "Rules" should {

    "extract data" in {
      import testCases.base._

      def firstname =
        at(Path \ "firstname")(is[String])

      firstname.validate(valid) shouldBe (Valid("Julien"))

      val errPath = Path \ "foo"
      val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      def err = at(errPath)(is[String])
      err.validate(invalid) shouldBe (error)
    }
    // "support checked" in {
    //   val js = Json.obj("issmth" -> true)
    //   val p = Path \ "issmth"
    //   p.from[JsValue](checked).validate(js) shouldBe (Valid(true))
    //   p.from[JsValue](checked).validate(Json.obj()) shouldBe
    //   (Invalid(Seq(Path \ "issmth" -> Seq(ValidationError("error.required")))))
    //   p.from[JsValue](checked).validate(Json.obj("issmth" -> false)) shouldBe
    //   (Invalid(Seq(Path \ "issmth" -> Seq(
    //                   ValidationError("error.equals", true)))))
    // }

    "support all types of Json values" when {

  //     "null" in {
  //       (Path \ "n")
  //         .read[JsValue, JsNull.type]
  //         .validate(Json.obj("n" -> JsNull)) shouldBe (Valid(JsNull))
  //       (Path \ "n")
  //         .read[JsValue, JsNull.type]
  //         .validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "null")))))
  //       (Path \ "n").read[JsValue, JsNull.type].validate(Json.obj("n" -> 4.5)) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "null")))))
  //     }

      "Int" in {
        import testCases.int._

        def n = at(Path \ "n")(is[Int])

        n.validate(ok) shouldBe (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Int")))))
        n.validate(float) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Int")))))

        def no = at(Path \ "n" \ "o")(is[Int])

        no.validate(noOK) shouldBe (Valid(4))
        no.validate(noFoo) shouldBe
          (Invalid(Seq(Path \ "n" \ "o" -> Seq(
            ValidationError("error.number", "Int")))))

        def nop = at(Path \ "n" \ "o" \ "p")(is[Int])

        nop.validate(nopOK) shouldBe (Valid(4))
        nop.validate(nopFoo) shouldBe
          (Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(
            ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))

        def fooErr = at(errPath)(is[Int])
        fooErr.validate(ok) shouldBe (error)
      }

      "Short" in {
        import testCases.int._

        def n = at(Path \ "n")(is[Short])

        n.validate(ok) shouldBe(Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Short")))))
        n.validate(float) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Short")))))
      }

      "Long" in {
        import testCases.int._
        def n = at(Path \ "n")(is[Long])

        n.validate(ok) shouldBe (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Long")))))
        n.validate(float) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Long")))))
      }

      "Float" in {
        import testCases.int.{float => f, _}
        def n = at(Path \ "n")(is[Float])

        n.validate(ok) shouldBe
        (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Float")))))
        n.validate(f) shouldBe (Valid(4.5F))
      }

      "Double" in {
        import testCases.int._
        def n = at(Path \ "n")(is[Double])

        n.validate(ok) shouldBe (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Double")))))
        n.validate(float) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => JBigDecimal}
        import testCases.int._
        def n = at(Path \ "n")(is[JBigDecimal])

        n.validate(ok) shouldBe
        (Valid(new JBigDecimal("4")))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "BigDecimal")))))
        n.validate(float) shouldBe (Valid(new JBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        import testCases.int._
        def n = at(Path \ "n")(is[BigDecimal])

        n.validate(ok) shouldBe (Valid(BigDecimal(4)))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "BigDecimal")))))
        n.validate(float) shouldBe (Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        import testCases.boolean._
        def n = at(Path \ "n")(is[Boolean])

        n.validate(ok) shouldBe (Valid(true))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        import testCases.string._
        def n = at(Path \ "n")(is[String])
        def o = at(Path \ "o")(is[String])

        n.validate(foo) shouldBe (Valid("foo"))
        n.validate(_42) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "String")))))
        n.validate(foos) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "String")))))
        o.validate(onFoo) shouldBe
          (Invalid(Seq(Path \ "o" -> Seq(
            ValidationError("error.invalid", "String")))))
      }

  //     "JsObject" in {
  //       (Path \ "o")
  //         .read[JsValue, JsObject]
  //         .validate(Json.obj("o" -> Json.obj("n" -> "foo"))) shouldBe
  //       (Valid(JsObject(Seq("n" -> JsString("foo")))))
  //       (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> 42)) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Object")))))
  //       (Path \ "n").read[JsValue, JsObject].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Object")))))
  //       (Path \ "n")
  //         .read[JsValue, JsObject]
  //         .validate(Json.obj("n" -> Seq("foo"))) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Object")))))
  //     }

  //     "JsString" in {
  //       (Path \ "n").read[JsValue, JsString].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Valid(JsString("foo")))
  //       (Path \ "n").read[JsValue, JsString].validate(Json.obj("n" -> 42)) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "String")))))
  //     }

  //     "JsNumber" in {
  //       (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> 4)) shouldBe
  //       (Valid(JsNumber(4)))
  //       (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.number", "Number")))))
  //       (Path \ "n").read[JsValue, JsNumber].validate(Json.obj("n" -> 4.5)) shouldBe
  //       (Valid(JsNumber(4.5)))
  //     }

  //     "JsBoolean" in {
  //       (Path \ "n").read[JsValue, JsBoolean].validate(Json.obj("n" -> true)) shouldBe
  //       (Valid(JsBoolean(true)))
  //       (Path \ "n").read[JsValue, JsBoolean].validate(Json.obj("n" -> "foo")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Boolean")))))
  //     }

      "Option" in {
        import testCases.boolean._
        import testCases.option._
        def n = opt(Path \ "n")(is[Boolean])

        n.validate(ok) shouldBe (Valid(Some(true)))
        // n.validate(nNull) shouldBe (Valid(None)) // TODO
        n.validate(fooBar) shouldBe (Valid(None))
        n.validate(nBar) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

  //     "Map[String, V]" in {
  //       (Path \ "n")
  //         .read[JsValue, Map[String, String]]
  //         .validate(Json.obj("n" -> Json.obj("foo" -> "bar"))) shouldBe
  //       (Valid(Map("foo" -> "bar")))
  //       (Path \ "n")
  //         .read[JsValue, Map[String, Int]]
  //         .validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> 5))) shouldBe
  //       (Valid(Map("foo" -> 4, "bar" -> 5)))
  //       (Path \ "x")
  //         .read[JsValue, Map[String, Int]]
  //         .validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))) shouldBe
  //       (Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
  //       (Path \ "n")
  //         .read[JsValue, Map[String, Int]]
  //         .validate(Json.obj("n" -> Json.obj("foo" -> 4, "bar" -> "frack"))) shouldBe
  //       (Invalid(Seq(Path \ "n" \ "bar" -> Seq(
  //                       ValidationError("error.number", "Int")))))
  //     }

  //     "Traversable" in {
  //       (Path \ "n")
  //         .read[JsValue, Traversable[String]]
  //         .validate(Json.obj("n" -> Seq("foo")))
  //         .toOption
  //         .get
  //         .toSeq shouldBe (Seq("foo"))
  //       (Path \ "n")
  //         .read[JsValue, Traversable[Int]]
  //         .validate(Json.obj("n" -> Seq(1, 2, 3)))
  //         .toOption
  //         .get
  //         .toSeq shouldBe (Seq(1, 2, 3))
  //       (Path \ "n")
  //         .read[JsValue, Traversable[String]]
  //         .validate(Json.obj("n" -> "paf")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Array")))))
  //     }

  //     "Array" in {
  //       (Path \ "n")
  //         .read[JsValue, Array[String]]
  //         .validate(Json.obj("n" -> Seq("foo")))
  //         .toOption
  //         .get
  //         .toSeq shouldBe (Seq("foo"))
  //       (Path \ "n")
  //         .read[JsValue, Array[Int]]
  //         .validate(Json.obj("n" -> Seq(1, 2, 3)))
  //         .toOption
  //         .get
  //         .toSeq shouldBe (Seq(1, 2, 3))
  //       (Path \ "n")
  //         .read[JsValue, Array[String]]
  //         .validate(Json.obj("n" -> "paf")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Array")))))
  //     }

  //     "Seq" in {
  //       (Path \ "n")
  //         .read[JsValue, Seq[String]]
  //         .validate(Json.obj("n" -> Seq("foo")))
  //         .toOption
  //         .get shouldBe (Seq("foo"))
  //       (Path \ "n")
  //         .read[JsValue, Seq[Int]]
  //         .validate(Json.obj("n" -> Seq(1, 2, 3)))
  //         .toOption
  //         .get shouldBe (Seq(1, 2, 3))
  //       (Path \ "n")
  //         .read[JsValue, Seq[String]]
  //         .validate(Json.obj("n" -> "paf")) shouldBe
  //       (Invalid(Seq(Path \ "n" -> Seq(
  //                       ValidationError("error.invalid", "Array")))))
  //       (Path \ "n")
  //         .read[JsValue, Seq[String]]
  //         .validate(JsObject(Seq("n" -> JsArray(Seq(JsString("foo"),
  //                                                   JsNumber(2)))))) shouldBe
  //       (Invalid(Seq(Path \ "n" \ 1 -> Seq(
  //                       ValidationError("error.invalid", "String")))))
  //     }
    }
    "validate data" in {
      import testCases.base._
      def firstname = at(Path \ "firstname")(is[String] andThen notEmpty)

      firstname.validate(valid) shouldBe (Valid("Julien"))

      def label = at(Path \ "informations" \ "label")(is[String] andThen notEmpty)

      label.validate(valid) shouldBe (Valid("Personal"))
      label.validate(invalid) shouldBe
        (Invalid(Seq((Path \ "informations" \ "label") ->
          Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      import testCases.base._
      def firstname = opt(Path \ "firstname")(is[String] andThen notEmpty)
      firstname.validate(valid) shouldBe (Valid(Some("Julien")))

      def foobar = opt(Path \ "foobar")(is[String] andThen notEmpty)
      foobar.validate(valid) shouldBe (Valid(None))
    }

    "validate deep" in {
      import testCases.base._

      def label =
        at(Path \ "informations")(
          at(Path \ "label")(is[String] andThen notEmpty)
        )

      label.validate(valid) shouldBe (Valid("Personal"))

      val p = (Path \ "informations" \ "label")
      label.validate(invalid) shouldBe
        (Invalid(Seq(p ->
          Seq(ValidationError("error.required")))))

      // def second = opt(Path \ "first" \ "second")(is[String])
      // second validate (JsNull) shouldBe Valid(None) // TODO: extract
    }

    "coerce type" in {
      import testCases.base._
      def age = at(Path \ "age")(is[Int])
      age.validate(valid) shouldBe (Valid(27))

      def ageMin = at(Path \ "age")(is[Int] andThen min(20))
      ageMin.validate(valid) shouldBe (Valid(27))

      def ageMax = at(Path \ "age")(is[Int] andThen max(50))
      ageMax.validate(valid) shouldBe (Valid(27))

      def ageMin50 = at(Path \ "age")(is[Int] andThen min(50))
      ageMin50.validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") ->
          Seq(ValidationError("error.min", 50)))))

      def ageMax0 = at(Path \ "age")(is[Int] andThen max(0))
      ageMax0.validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") ->
            Seq(ValidationError("error.max", 0)))))

      def firstname = at(Path \ "firstname")(is[Int])
      firstname.validate(valid) shouldBe
        (Invalid(Seq((Path \ "firstname") -> Seq(
                      ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      import testCases.base._
      import cats.syntax.semigroup._

      val composed = notEmpty |+| minLength(3)
      def firstname = at(Path \ "firstname")(is[String] andThen composed)
      firstname.validate(valid) shouldBe (Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"),
                                     ValidationError("error.minLength", 3))))
      val label = at(p)(is[String] andThen composed)
      label.validate(invalid) shouldBe (err)
    }

/*
  //   "compose validations" in {
  //     From[JsValue] { __ =>
  //       ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty)).tupled
  //     }.validate(valid) shouldBe Valid("Julien" -> "Tournay")

  //     From[JsValue] { __ =>
  //       ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
  //           (__ \ "informations" \ "label").read(notEmpty)).tupled
  //     }.validate(invalid) shouldBe Invalid(
  //         Seq((Path \ "informations" \ "label") -> Seq(
  //                 ValidationError("error.required"))))
  //   }

  //   "lift validations to seq validations" in {
  //     (Path \ "foo")
  //       .from[JsValue](seqR(notEmpty))
  //       .validate(Json.obj("foo" -> Seq("bar")))
  //       .toOption
  //       .get shouldBe (Seq("bar"))

  //     From[JsValue] { __ =>
  //       (__ \ "foo").read((__ \ "foo").read(seqR(notEmpty)))
  //     }.validate(Json.obj("foo" -> Json.obj("foo" -> Seq("bar")))).toOption.get shouldBe
  //     (Seq("bar"))

  //     (Path \ "n")
  //       .from[JsValue](seqR(notEmpty))
  //       .validate(Json.obj("n" -> Seq("foo", ""))) shouldBe
  //     (Invalid(Seq(Path \ "n" \ 1 -> Seq(ValidationError("error.required")))))
  //   }

  //   "validate dependent fields" in {
  //     val v = Json.obj("login" -> "Alice",
  //                      "password" -> "s3cr3t",
  //                      "verify" -> "s3cr3t")

  //     val i1 = Json.obj("login" -> "Alice",
  //                       "password" -> "s3cr3t",
  //                       "verify" -> "")

  //     val i2 = Json.obj("login" -> "Alice",
  //                       "password" -> "s3cr3t",
  //                       "verify" -> "bam")

  //     val passRule = From[JsValue] { __ =>
  //       ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty)).tupled
  //         .andThen(
  //           Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
  //     }

  //     val rule = From[JsValue] { __ =>
  //       ((__ \ "login").read(notEmpty) ~ passRule).tupled
  //     }

  //     rule.validate(v).shouldBe(Valid("Alice" -> "s3cr3t"))
  //     rule
  //       .validate(i1)
  //       .shouldBe(Invalid(Seq(Path \ "verify" -> Seq(
  //                       ValidationError("error.required")))))
  //     rule
  //       .validate(i2)
  //       .shouldBe(Invalid(Seq(Path \ "verify" -> Seq(
  //                       ValidationError("error.equals", "s3cr3t")))))
  //   }

  //   "validate subclasses (and parse the concrete class)" when {

  //     trait A
  //     case class B(foo: Int) extends A
  //     case class C(bar: Int) extends A

  //     val b = Json.obj("name" -> "B", "foo" -> 4)
  //     val c = Json.obj("name" -> "C", "bar" -> 6)
  //     val e = Json.obj("name" -> "E", "eee" -> 6)

  //     val typeInvalid =
  //       Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

  //     "by trying all possible Rules" in {
  //       import cats.syntax.cartesian._

  //       val rb: Rule[JsValue, A] = From[JsValue] { __ =>
  //         (__ \ "name").read(Rules.equalTo("B")) *> (__ \ "foo")
  //           .read[Int]
  //           .map(B.apply)
  //       }

  //       val rc: Rule[JsValue, A] = From[JsValue] { __ =>
  //         (__ \ "name").read(Rules.equalTo("C")) *> (__ \ "bar")
  //           .read[Int]
  //           .map(C.apply)
  //       }

  //       val rule = rb orElse rc orElse Rule(Path)(_ => typeInvalid)

  //       rule.validate(b) shouldBe (Valid(B(4)))
  //       rule.validate(c) shouldBe (Valid(C(6)))
  //       rule.validate(e) shouldBe
  //       (Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
  //     }

  //     "by dicriminating on fields" in {

  //       val rule = From[JsValue] { __ =>
  //         (__ \ "name").read[String].flatMap[A] {
  //           case "B" => (__ \ "foo").read[Int].map(B.apply)
  //           case "C" => (__ \ "bar").read[Int].map(C.apply)
  //           case _ => Rule(Path)(_ => typeInvalid)
  //         }
  //       }

  //       rule.validate(b) shouldBe (Valid(B(4)))
  //       rule.validate(c) shouldBe (Valid(C(6)))
  //       rule.validate(e) shouldBe
  //       (Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType")))))
  //     }
  //   }

  //   "perform complex validation" in {

  //     case class Contact(firstname: String,
  //                        lastname: String,
  //                        company: Option[String],
  //                        informations: Seq[ContactInformation])

  //     case class ContactInformation(
  //         label: String, email: Option[String], phones: Seq[String])

  //     val validJson = Json.obj(
  //         "firstname" -> "Julien",
  //         "lastname" -> "Tournay",
  //         "age" -> 27,
  //         "informations" -> Seq(
  //             Json.obj("label" -> "Personal",
  //                      "email" -> "fakecontact@gmail.com",
  //                      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))))

  //     val invalidJson = Json.obj(
  //         "firstname" -> "Julien",
  //         "lastname" -> "Tournay",
  //         "age" -> 27,
  //         "informations" -> Seq(
  //             Json.obj("label" -> "",
  //                      "email" -> "fakecontact@gmail.com",
  //                      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))))

  //     val infoValidated = From[JsValue] { __ =>
  //       ((__ \ "label").read(notEmpty) ~ (__ \ "email").read(optionR(email)) ~
  //           (__ \ "phones").read(seqR(notEmpty)))(ContactInformation.apply)
  //     }

  //     val contactValidated = From[JsValue] { __ =>
  //       ((__ \ "firstname").read(notEmpty) ~ (__ \ "lastname").read(notEmpty) ~
  //           (__ \ "company").read[Option[String]] ~ (__ \ "informations").read(
  //               seqR(infoValidated)))(Contact.apply)
  //     }

  //     val expected = Contact(
  //         "Julien",
  //         "Tournay",
  //         None,
  //         Seq(ContactInformation("Personal",
  //                                Some("fakecontact@gmail.com"),
  //                                List("01.23.45.67.89", "98.76.54.32.10"))))

  //     contactValidated.validate(validJson) shouldBe (Valid(expected))
  //     contactValidated.validate(invalidJson) shouldBe
  //     (Invalid(Seq((Path \ "informations" \ 0 \ "label") -> Seq(
  //                     ValidationError("error.required")))))
  //   }

  //   "read recursive" when {
  //     case class RecUser(name: String, friends: Seq[RecUser] = Nil)
  //     val u = RecUser("bob", Seq(RecUser("tom")))

  //     val m =
  //       Json.obj("name" -> "bob",
  //                "friends" -> Seq(
  //                    Json.obj("name" -> "tom", "friends" -> Seq[JsObject]())))

  //     case class User1(name: String, friend: Option[User1] = None)
  //     val u1 = User1("bob", Some(User1("tom")))
  //     val m1 = Json.obj("name" -> "bob", "friend" -> Json.obj("name" -> "tom"))

  //     "using explicit notation" in {
  //       lazy val w: Rule[JsValue, RecUser] = From[JsValue] { __ =>
  //         ((__ \ "name").read[String] ~ (__ \ "friends").read(seqR(w)))(
  //             RecUser.apply)
  //       }
  //       w.validate(m) shouldBe Valid(u)

  //       lazy val w2: Rule[JsValue, RecUser] =
  //         ((Path \ "name").read[JsValue, String] ~ (Path \ "friends")
  //               .from[JsValue](seqR(w2)))(RecUser.apply)
  //       w2.validate(m) shouldBe Valid(u)

  //       lazy val w3: Rule[JsValue, User1] = From[JsValue] { __ =>
  //         ((__ \ "name").read[String] ~ (__ \ "friend").read(optionR(w3)))(
  //             User1.apply)
  //       }
  //       w3.validate(m1) shouldBe Valid(u1)
  //     }

  //     "using implicit notation" in {
  //       implicit lazy val w: Rule[JsValue, RecUser] = From[JsValue] { __ =>
  //         ((__ \ "name").read[String] ~ (__ \ "friends").read[Seq[RecUser]])(
  //             RecUser.apply)
  //       }
  //       w.validate(m) shouldBe Valid(u)

  //       implicit lazy val w3: Rule[JsValue, User1] = From[JsValue] { __ =>
  //         ((__ \ "name").read[String] ~ (__ \ "friend").read[Option[User1]])(
  //             User1.apply)
  //       }
  //       w3.validate(m1) shouldBe Valid(u1)
  //     }
  //   }

  //   "completely generic" in {
  //     type OptString[In] =
  //       Rule[String, String] => Path => Rule[In, Option[String]]

  //     def genR[In](opt: OptString[In])(
  //         implicit exs: Path => Rule[In, String]) =
  //       From[In] { __ =>
  //         ((__ \ "name").read(notEmpty) ~ (__ \ "color").read(opt(notEmpty))).tupled
  //       }

  //     val jsonR = {

  //       genR[JsValue](optionR(_))
  //     }

  //     val json = Json.obj("name" -> "bob", "color" -> "blue")
  //     val invalidJson = Json.obj("color" -> "blue")

  //     jsonR.validate(json) shouldBe Valid(("bob", Some("blue")))
  //     jsonR.validate(invalidJson) shouldBe Invalid(
  //         Seq((Path \ "name", Seq(ValidationError("error.required")))))
  //   }
  */
  }
}

class JsonRulesSpec extends RulesSpec[JsValue] {
  implicit val grammar = RulesGrammar
  val testCases = JsonTestCases
}
