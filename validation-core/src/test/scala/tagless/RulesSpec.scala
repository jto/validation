package jto.validation
package v3.tagless

import org.scalatest._

trait TestCases[T] {
  trait base {
    def valid: T
    def invalid: T
    def smthTrue: T
    def smthFalse: T
    def emptyObj: T
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

  trait seq {
    def foos: T
    def fooBars: T
    def foofoobars: T
    def ns: T
    def ints: T
    def paf: T
    def mixed: T
  }

  trait map {
    def foobar: T
    def ints: T
    def mixed: T
  }

  trait password {
    val ok: T
    val empty: T
    val err: T
  }

  trait subclasses {
    val b: T
    val c: T
    val e: T
  }

  trait rec {
    val bobAndFriends: T
    val bobAndFriend: T
  }

  val base: base
  val int: int
  val boolean: boolean
  val string: string
  val option: option
  val seq: seq
  val map: map
  val password: password
  val subclasses: subclasses
  val rec: rec
}

trait RulesSpec[T] extends WordSpec with Matchers {

  val testCases: TestCases[T]
  val grammar: Grammar[T, Rule]

  import grammar._
  import syntax._

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

    "support primitive types" when {

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

      "Option" in {
        import testCases.boolean._
        import testCases.option._
        def n = opt(Path \ "n")(is[Boolean])

        n.validate(ok) shouldBe (Valid(Some(true)))
        n.validate(fooBar) shouldBe (Valid(None))
        n.validate(nBar) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        import testCases.map._

        at(Path \ "n")(is[Map[String, String]])
          .validate(foobar) shouldBe Valid(Map("foo" -> "bar"))

        at(Path \ "n")(is[Map[String, Int]])
          .validate(ints) shouldBe Valid(Map("foo" -> 4, "bar" -> 5))

        at(Path \ "x")(is[Map[String, Int]])
          .validate(mixed) shouldBe
            Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required"))))

        at(Path \ "n")(is[Map[String, Int]])
          .validate(mixed) shouldBe
            Invalid(Seq(Path \ "n" \ "bar" -> Seq(
              ValidationError("error.number", "Int"))))
      }

      "Traversable" in {
        import testCases.seq._
        at(Path \ "n")(is[Traversable[String]])
          .validate(foos) shouldBe Valid(Seq("foo"))

        at(Path \ "n")(is[Traversable[Int]])
          .validate(ints) shouldBe Valid(Seq(1, 2, 3))

        at(Path \ "n")(is[Traversable[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n")(is[Traversable[String]])
          .validate(mixed) shouldBe
            (Invalid(Seq(Path \ "n" \ 1 -> Seq(
              ValidationError("error.invalid", "String")))))
      }

      "Array" in {
        import testCases.seq._
        at(Path \ "n")(is[Seq[String]])
          .validate(foos) shouldBe Valid(Seq("foo"))

        at(Path \ "n")(is[Seq[Int]])
          .validate(ints) shouldBe Valid(Seq(1, 2, 3))

        at(Path \ "n")(is[Seq[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n")(is[Seq[String]])
          .validate(mixed) shouldBe
            (Invalid(Seq(Path \ "n" \ 1 -> Seq(
              ValidationError("error.invalid", "String")))))
      }

      "Seq" in {
        import testCases.seq._
        at(Path \ "n")(is[Seq[String]])
          .validate(foos) shouldBe Valid(Seq("foo"))

        at(Path \ "n")(is[Seq[Int]])
          .validate(ints) shouldBe Valid(Seq(1, 2, 3))

        at(Path \ "n")(is[Seq[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n")(is[Seq[String]]).validate(mixed) shouldBe
          (Invalid(Seq(Path \ "n" \ 1 -> Seq(
            ValidationError("error.invalid", "String")))))
      }

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

    "compose validations" in {
      import testCases.base._

      val ne = is[String] andThen notEmpty

      def names =
        at(Path \ "firstname")(ne) ~:
        at(Path \ "lastname")(ne) ~:
        knil

      names.map(_.tupled).validate(valid) shouldBe Valid("Julien" -> "Tournay")

      def full =
        names ~:
        at(Path \ "informations" \ "label")(ne) ~:
        knil

      full.map(_.tupled).validate(invalid) shouldBe
        Invalid(Seq((Path \ "informations" \ "label") -> Seq(
          ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      import testCases.seq._

      def foo = at(Path \ "foo")(is[Seq[String]] andThen forall(notEmpty))
      foo.validate(fooBars) shouldBe Valid(Seq("bar"))

      def foofoo =
        at(Path \ "foo"){
          at(Path \ "foo")(is[Seq[String]] andThen forall(notEmpty))
        }
      foofoo.validate(foofoobars) shouldBe Valid(Seq("bar"))

      def n = at(Path \ "n")(is[Seq[String]] andThen forall(notEmpty))
      n.validate(ns) shouldBe
        (Invalid(Seq(Path \ "n" \ 1 ->
          Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      import testCases.password._
      object Rules extends GenericRules

      val passRule =
        (
          at(Path \ "password")(is[String] andThen notEmpty) ~:
          at(Path \ "verify")(is[String] andThen notEmpty) ~:
          knil
        ).map(_.tupled) andThen Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify"))

      val rule =
        (
          at(Path \ "login")(is[String] andThen notEmpty) ~:
          passRule ~:
          knil
        ).map(_.tupled)

      rule.validate(ok) shouldBe Valid("Alice" -> "s3cr3t")

      rule.validate(testCases.password.empty) shouldBe
        Invalid(Seq(Path \ "verify" ->
          Seq(ValidationError("error.required"))))

      rule.validate(err) shouldBe
        Invalid(Seq(Path \ "verify" ->
          Seq(ValidationError("error.equals", "s3cr3t"))))
    }

    "validate subclasses (and parse the concrete class)" when {
      import testCases.subclasses._

      trait A
      case class B(foo: Int) extends A
      case class C(bar: Int) extends A

      val typeInvalid =
        Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

      "by trying all possible Rules" in {
        import cats.syntax.cartesian._

        val rb: Rule[T, A] =
          (
            at(Path \ "name")(is[String] andThen equalTo("B")) *>
            at(Path \ "foo")(is[Int])
          ).map(B.apply)

        val rc: Rule[T, A] =
          (
            at(Path \ "name")(is[String] andThen equalTo("C")) *>
            at(Path \ "bar")(is[Int])
          ).map(C.apply)

        val rule = rb orElse rc orElse Rule(Path)(_ => typeInvalid)

        rule.validate(b) shouldBe Valid(B(4))
        rule.validate(c) shouldBe Valid(C(6))
        rule.validate(e) shouldBe
          Invalid(Seq(Path ->
            Seq(ValidationError("validation.unknownType"))))
      }

      "by dicriminating on fields" in {
        val rule =
          at(Path \ "name")(is[String]).flatMap[A] {
            case "B" => at(Path \ "foo")(is[Int]).map(B.apply)
            case "C" => at(Path \ "bar")(is[Int]).map(C.apply)
            case _ => Rule(Path)(_ => typeInvalid)
          }

        rule.validate(b) shouldBe Valid(B(4))
        rule.validate(c) shouldBe Valid(C(6))
        rule.validate(e) shouldBe
          Invalid(Seq(Path \ "name" ->
            Seq(ValidationError("validation.unknownType"))))
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

      def info =
        goal[ContactInformation] {
          at(Path \ "label")(is[String] andThen notEmpty) ~:
          opt(Path \ "email")(is[String] andThen email) ~:
          at(Path \ "phones")(is[Seq[String]] andThen forall(notEmpty)) ~:
          knil
        }

      def contact =
        goal[Contact]{
          at(Path \ "firstname")(is[String] andThen notEmpty) ~:
          at(Path \ "lastname")(is[String] andThen notEmpty) ~:
          opt(Path \ "company")(is[String]) ~:
          at(Path \ "contacts")(seq(info)) ~:
          knil
        }

      val expected = Contact(
          "Julien",
          "Tournay",
          None,
          Seq(
            ContactInformation("Personal",
             Some("fakecontact@gmail.com"),
             List("01.23.45.67.89", "98.76.54.32.10"))))

      val rule = contact.map(h => solve(h))

      import testCases.base._
      rule.validate(valid) shouldBe (Valid(expected))
      rule.validate(invalid) shouldBe
        (Invalid(Seq((Path \ "contacts" \ 0 \ "label") -> Seq(
          ValidationError("error.required")))))
    }

    "read recursive" when {
      import testCases.rec._

      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))

      "using explicit notation" in {
        lazy val w: Rule[T, RecUser] =
          (
            at(Path \ "name")(is[String]) ~:
            at(Path \ "friends")(seq(w)) ~:
            knil
          ).to[RecUser]

        w.validate(bobAndFriends) shouldBe Valid(u)

        lazy val w2: Rule[T, RecUser] =
          (
            at(Path \ "name")(is[String]) ~:
            at(Path \ "friends")(seq(w2)) ~:
            knil
          ).to[RecUser]

        w2.validate(bobAndFriends) shouldBe Valid(u)

        lazy val w3: Rule[T, User1] =
          (
            at(Path \ "name")(is[String]) ~:
            opt(Path \ "friend")(w3) ~:
            knil
          ).to[User1]

        w3.validate(bobAndFriend) shouldBe Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[T, RecUser] =
          (
            at(Path \ "name")(is[String]) ~:
            at(Path \ "friends")(is[Seq[RecUser]]) ~:
            knil
          ).to[RecUser]

        w.validate(bobAndFriends) shouldBe Valid(u)

        implicit lazy val w3: Rule[T, User1] =
          (
            at(Path \ "name")(is[String]) ~:
            opt(Path \ "friend")(is[User1]) ~:
            knil
          ).to[User1]

        w3.validate(bobAndFriend) shouldBe Valid(u1)
      }
    }

  }
}