package jto.validation
package v3.tagless

import org.scalatest._

trait RulesSpec[T] extends WordSpec with Matchers {

  val grammar: Grammar[T, Rule]
  val testCases: TestCases[grammar.Out]

  import grammar._

  "Rules" should {

    "extract data" in {
      import testCases.base._

      def firstname =
        at(Path \ "firstname")(req[String])

      firstname.validate(valid) shouldBe (Valid("Julien"))

      val errPath = Path \ "foo"
      val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      def err = at(errPath)(req[String])
      err.validate(invalid) shouldBe (error)
    }

    "support primitive types" when {

      "Int" in {
        import testCases.int._

        def n = at(Path \ "n")(req[Int])

        n.validate(ok) shouldBe (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Int")))))
        n.validate(float) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Int")))))

        def no = at(Path \ "n" \ "o")(req[Int])

        no.validate(noOK) shouldBe (Valid(4))
        no.validate(noFoo) shouldBe
          (Invalid(Seq(Path \ "n" \ "o" -> Seq(
            ValidationError("error.number", "Int")))))

        def nop = at(Path \ "n" \ "o" \ "p")(req[Int])

        nop.validate(nopOK) shouldBe (Valid(4))
        nop.validate(nopFoo) shouldBe
          (Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(
            ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))

        def fooErr = at(errPath)(req[Int])
        fooErr.validate(ok) shouldBe (error)
      }

      "Short" in {
        import testCases.int._

        def n = at(Path \ "n")(req[Short])

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
        def n = at(Path \ "n")(req[Long])

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
        def n = at(Path \ "n")(req[Float])

        n.validate(ok) shouldBe
        (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Float")))))
        n.validate(f) shouldBe (Valid(4.5F))
      }

      "Double" in {
        import testCases.int._
        def n = at(Path \ "n")(req[Double])

        n.validate(ok) shouldBe (Valid(4))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Double")))))
        n.validate(float) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => JBigDecimal}
        import testCases.int._
        def n = at(Path \ "n")(req[JBigDecimal])

        n.validate(ok) shouldBe
        (Valid(new JBigDecimal("4")))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "BigDecimal")))))
        n.validate(float) shouldBe (Valid(new JBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        import testCases.int._
        def n = at(Path \ "n")(req[BigDecimal])

        n.validate(ok) shouldBe (Valid(BigDecimal(4)))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "BigDecimal")))))
        n.validate(float) shouldBe (Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        import testCases.boolean._
        def n = at(Path \ "n")(req[Boolean])

        n.validate(ok) shouldBe (Valid(true))
        n.validate(foo) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        import testCases.string._
        def n = at(Path \ "n")(req[String])
        def o = at(Path \ "o")(req[String])
        n.validate(foo) shouldBe (Valid("foo"))
      }

      "Option" in {
        import testCases.boolean._
        import testCases.option._

        def n = at(Path \ "n")(opt[Boolean])

        n.validate(ok) shouldBe (Valid(Some(true)))
        n.validate(fooBar) shouldBe (Valid(None))
        n.validate(nBar) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        import testCases.map._

        at(Path \ "n")(req[Map[String, String]])
          .validate(foobar) shouldBe Valid(Map("foo" -> "bar"))

        at(Path \ "n")(req[Map[String, Int]])
          .validate(ints) shouldBe Valid(Map("foo" -> 4, "bar" -> 5))

        at(Path \ "x")(req[Map[String, Int]])
          .validate(mixed) shouldBe
            Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required"))))

        at(Path \ "n")(req[Map[String, Int]])
          .validate(mixed) shouldBe
            Invalid(Seq(Path \ "n" \ "bar" -> Seq(
              ValidationError("error.number", "Int"))))
      }

      "Traversable" in {
        import testCases.seq._
        at(Path \ "n")(req[Traversable[String]])
          .validate(foos) shouldBe Valid(Seq("foo"))

        at(Path \ "n")(req[Traversable[Int]])
          .validate(ints) shouldBe Valid(Seq(1, 2, 3))
      }

      "Array" in {
        import testCases.seq._
        at(Path \ "n")(req[Seq[String]])
          .validate(foos) shouldBe Valid(Seq("foo"))

        at(Path \ "n")(req[Seq[Int]])
          .validate(ints) shouldBe Valid(Seq(1, 2, 3))

        at(Path \ "n")(req[Seq[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n")(req[Seq[String]])
          .validate(mixed) shouldBe
            (Invalid(Seq(Path \ "n" \ 1 -> Seq(
              ValidationError("error.invalid", "String")))))
      }

      "Seq" in {
        import testCases.seq._
        at(Path \ "n")(req[Seq[String]])
          .validate(foos) shouldBe Valid(Seq("foo"))

        at(Path \ "n")(req[Seq[Int]])
          .validate(ints) shouldBe Valid(Seq(1, 2, 3))

        at(Path \ "n")(req[Seq[String]])
          .validate(paf) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.invalid", "Array")))))

        at(Path \ "n")(req[Seq[String]]).validate(mixed) shouldBe
          (Invalid(Seq(Path \ "n" \ 1 -> Seq(
            ValidationError("error.invalid", "String")))))
      }

    }
    "validate data" in {
      import testCases.base._
      def firstname = at(Path \ "firstname")(req[String] andThen notEmpty)

      firstname.validate(valid) shouldBe (Valid("Julien"))

      def label = at(Path \ "informations" \ "label")(req[String] andThen notEmpty)

      label.validate(valid) shouldBe (Valid("Personal"))
      label.validate(invalid) shouldBe
        (Invalid(Seq((Path \ "informations" \ "label") ->
          Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      import testCases.base._
      def firstname = at(Path \ "firstname")(opt(is[String] andThen notEmpty))
      firstname.validate(valid) shouldBe (Valid(Some("Julien")))

      def foobar = at(Path \ "foobar")(opt(is[String] andThen notEmpty))
      foobar.validate(valid) shouldBe (Valid(None))
    }

    "validate deep" in {
      import testCases.base._

      def label =
        at(Path \ "informations")(
          req(at(Path \ "label")(req(is[String] andThen notEmpty)))
        )

      label.validate(valid) shouldBe (Valid("Personal"))

      val p = (Path \ "informations" \ "label")
      label.validate(invalid) shouldBe
        (Invalid(Seq(p ->
          Seq(ValidationError("error.required")))))
    }

    "coerce type" in {
      import testCases.base._
      def age = at(Path \ "age")(req[Int])
      age.validate(valid) shouldBe (Valid(27))

      def ageMin = at(Path \ "age")(req[Int] andThen min(20))
      ageMin.validate(valid) shouldBe (Valid(27))

      def ageMax = at(Path \ "age")(req[Int] andThen max(50))
      ageMax.validate(valid) shouldBe (Valid(27))

      def ageMin50 = at(Path \ "age")(req[Int] andThen min(50))
      ageMin50.validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") ->
          Seq(ValidationError("error.min", 50)))))

      def ageMax0 = at(Path \ "age")(req[Int] andThen max(0))
      ageMax0.validate(valid) shouldBe
        (Invalid(Seq((Path \ "age") ->
            Seq(ValidationError("error.max", 0)))))

      def firstname = at(Path \ "firstname")(req[Int])
      firstname.validate(valid) shouldBe
        (Invalid(Seq((Path \ "firstname") -> Seq(
                      ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      import testCases.base._
      import cats.syntax.semigroup._

      val composed = notEmpty |+| minLength(3)
      def firstname = at(Path \ "firstname")(req[String] andThen composed)
      firstname.validate(valid) shouldBe (Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"),
                                     ValidationError("error.minLength", 3))))
      val label = at(p)(req[String] andThen composed)
      label.validate(invalid) shouldBe (err)
    }

    "compose validations" in {
      import testCases.base._

      val ne = req[String] andThen notEmpty

      def names =
        at(Path \ "firstname")(ne) ~:
        at(Path \ "lastname")(ne) ~:
        knil

      names.map(_.tupled).validate(valid) shouldBe Valid("Julien" -> "Tournay")

      def full =
        names ~:
        at(Path \ "informations" \ "label")(ne) ~:
        knil


      full.validate(emptyObj) shouldBe
        Invalid(Seq(
          (Path \ "firstname") -> Seq(ValidationError("error.required")),
          (Path \ "lastname") -> Seq(ValidationError("error.required")),
          (Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))
        ))

      full.map(_.tupled).validate(invalid) shouldBe
        Invalid(Seq((Path \ "informations" \ "label") -> Seq(
          ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      import testCases.seq._

      def foo = at(Path \ "foo")(req[Seq[String]] andThen forall(notEmpty))
      foo.validate(fooBars) shouldBe Valid(Seq("bar"))

      def foofoo =
        at(Path \ "foo"){
          req(at(Path \ "foo")(req(is[Seq[String]] andThen forall(notEmpty))))
        }
      foofoo.validate(foofoobars) shouldBe Valid(Seq("bar"))

      def n = at(Path \ "n")(req[Seq[String]] andThen forall(notEmpty))
      n.validate(ns) shouldBe
        (Invalid(Seq(Path \ "n" \ 1 ->
          Seq(ValidationError("error.required")))))
    }

    "validate dependent fields" in {
      import testCases.password._
      object Rules extends GenericRules

      val passRule =
        (
          at(Path \ "password")(req[String] andThen notEmpty) ~:
          at(Path \ "verify")(req[String] andThen notEmpty) ~:
          knil
        ).map(_.tupled) andThen Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify"))

      val rule =
        (
          at(Path \ "login")(req[String] andThen notEmpty) ~:
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

        val rb: Rule[grammar.Out, A] =
          (
            at(Path \ "name")(req[String] andThen equalTo("B")) *>
            at(Path \ "foo")(req[Int])
          ).map(B.apply)

        val rc: Rule[grammar.Out, A] =
          (
            at(Path \ "name")(req[String] andThen equalTo("C")) *>
            at(Path \ "bar")(req[Int])
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
          at(Path \ "name")(req[String]).flatMap[A] {
            case "B" => at(Path \ "foo")(req[Int]).map(B.apply)
            case "C" => at(Path \ "bar")(req[Int]).map(C.apply)
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
          at(Path \ "label")(req[String] andThen notEmpty) ~:
          at(Path \ "email")(opt(is[String] andThen email)) ~:
          at(Path \ "phones")(req[Seq[String]] andThen forall(notEmpty)) ~:
          knil
        }

      def contact =
        goal[Contact]{
          at(Path \ "firstname")(req[String] andThen notEmpty) ~:
          at(Path \ "lastname")(req[String] andThen notEmpty) ~:
          at(Path \ "company")(opt[String]) ~:
          at(Path \ "contacts")(req(seq(info))) ~:
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
        lazy val w: Rule[grammar.Out, RecUser] =
          (
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friends")(req(seq(w))) ~:
            knil
          ).to[RecUser]

        w.validate(bobAndFriends) shouldBe Valid(u)

        lazy val w2: Rule[grammar.Out, RecUser] =
          (
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friends")(req(seq(w2))) ~:
            knil
          ).to[RecUser]

        w2.validate(bobAndFriends) shouldBe Valid(u)

        lazy val w3: Rule[grammar.Out, User1] =
          (
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friend")(opt(w3)) ~:
            knil
          ).to[User1]

        w3.validate(bobAndFriend) shouldBe Valid(u1)
      }

      "using implicit notation" in {
        implicit lazy val w: Rule[grammar.Out, RecUser] =
          (
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friends")(req[Seq[RecUser]]) ~:
            knil
          ).to[RecUser]

        w.validate(bobAndFriends) shouldBe Valid(u)

        implicit lazy val w3: Rule[grammar.Out, User1] =
          (
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friend")(opt[User1]) ~:
            knil
          ).to[User1]

        w3.validate(bobAndFriend) shouldBe Valid(u1)
      }
    }

  }
}