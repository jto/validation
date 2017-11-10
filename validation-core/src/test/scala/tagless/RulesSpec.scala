package jto.validation
package v3.tagless

import org.scalatest._

trait RulesSpec[T] extends WordSpec with Matchers {

  type From

  val grammar: v3.tagless.Grammar[T, Rule]
  val testCases: TestCases[From]
  def transform: From => grammar.Out

  import grammar._

  "Rules" should {

    "extract data" in {
      import testCases.base._

      def firstname =
        at(Path \ "firstname")(req[String])

      firstname.validate(transform(valid)) shouldBe (Valid("Julien"))

      val errPath = Path \ "foo"
      val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
      def err = at(errPath)(req[String])
      err.validate(transform(invalid)) shouldBe (error)
    }

    "support primitive types" when {

      "Int" in {
        import testCases.int._

        def n = at(Path \ "n")(req[Int])

        n.validate(transform(ok)) shouldBe (Valid(4))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Int")))))
        n.validate(transform(float)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Int")))))

        def no = at(Path \ "n" \ "o")(req[Int])

        no.validate(transform(noOK)) shouldBe (Valid(4))
        no.validate(transform(noFoo)) shouldBe
          (Invalid(Seq(Path \ "n" \ "o" -> Seq(
            ValidationError("error.number", "Int")))))

        def nop = at(Path \ "n" \ "o" \ "p")(req[Int])

        nop.validate(transform(nopOK)) shouldBe (Valid(4))
        nop.validate(transform(nopFoo)) shouldBe
          (Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(
            ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))

        def fooErr = at(errPath)(req[Int])
        fooErr.validate(transform(ok)) shouldBe (error)
      }

      "Short" in {
        import testCases.int._

        def n = at(Path \ "n")(req[Short])

        n.validate(transform(ok)) shouldBe(Valid(4))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Short")))))
        n.validate(transform(float)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Short")))))
      }

      "Long" in {
        import testCases.int._
        def n = at(Path \ "n")(req[Long])

        n.validate(transform(ok)) shouldBe (Valid(4))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Long")))))
        n.validate(transform(float)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Long")))))
      }

      "Float" in {
        import testCases.int.{float => f, _}
        def n = at(Path \ "n")(req[Float])

        n.validate(transform(ok)) shouldBe
        (Valid(4))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Float")))))
        n.validate(transform(f)) shouldBe (Valid(4.5F))
      }

      "Double" in {
        import testCases.int._
        def n = at(Path \ "n")(req[Double])

        n.validate(transform(ok)) shouldBe (Valid(4))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "Double")))))
        n.validate(transform(float)) shouldBe (Valid(4.5))
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => JBigDecimal}
        import testCases.int._
        def n = at(Path \ "n")(req[JBigDecimal])

        n.validate(transform(ok)) shouldBe
        (Valid(new JBigDecimal("4")))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "BigDecimal")))))
        n.validate(transform(float)) shouldBe (Valid(new JBigDecimal("4.5")))
      }

      "scala BigDecimal" in {
        import testCases.int._
        def n = at(Path \ "n")(req[BigDecimal])

        n.validate(transform(ok)) shouldBe (Valid(BigDecimal(4)))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.number", "BigDecimal")))))
        n.validate(transform(float)) shouldBe (Valid(BigDecimal(4.5)))
      }

      "Boolean" in {
        import testCases.boolean._
        def n = at(Path \ "n")(req[Boolean])

        n.validate(transform(ok)) shouldBe (Valid(true))
        n.validate(transform(foo)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        import testCases.string._
        def n = at(Path \ "n")(req[String])
        def o = at(Path \ "o")(req[String])
        n.validate(transform(foo)) shouldBe (Valid("foo"))
      }

      "Option" in {
        import testCases.boolean._
        import testCases.option._

        def n = at(Path \ "n")(opt[Boolean])

        n.validate(transform(ok)) shouldBe (Valid(Some(true)))
        n.validate(transform(fooBar)) shouldBe (Valid(None))
        n.validate(transform(nBar)) shouldBe
          (Invalid(Seq(Path \ "n" -> Seq(
            ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, V]" in {
        import testCases.map._

        at(Path \ "n")(req[Map[String, String]])
          .validate(transform(foobar)) shouldBe Valid(Map("foo" -> "bar"))

        at(Path \ "n")(req[Map[String, Int]])
          .validate(transform(ints)) shouldBe Valid(Map("foo" -> 4, "bar" -> 5))

        at(Path \ "x")(req[Map[String, Int]])
          .validate(transform(mixed)) shouldBe
            Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required"))))

        at(Path \ "n")(req[Map[String, Int]])
          .validate(transform(mixed)) shouldBe
            Invalid(Seq(Path \ "n" \ "bar" -> Seq(
              ValidationError("error.number", "Int"))))
      }

      // TODO TODO TODO: PORT THOSES TESTS
      // "Traversable" in {
      //   import testCases.seq._
      //   at(Path \ "n")(req[Traversable[String]])
      //     .validate(foos) shouldBe Valid(Seq("foo"))

      //   at(Path \ "n")(req[Traversable[Int]])
      //     .validate(ints) shouldBe Valid(Seq(1, 2, 3))
      // }

      // "Array" in {
      //   import testCases.seq._
      //   at(Path \ "n")(req[Seq[String]])
      //     .validate(foos) shouldBe Valid(Seq("foo"))

      //   at(Path \ "n")(req[Seq[Int]])
      //     .validate(ints) shouldBe Valid(Seq(1, 2, 3))
      // }

      // "Seq" in {
      //   import testCases.seq._
      //   at(Path \ "n")(req[Seq[String]])
      //     .validate(foos) shouldBe Valid(Seq("foo"))

      //   at(Path \ "n")(req[Seq[Int]])
      //     .validate(ints) shouldBe Valid(Seq(1, 2, 3))
      // }
    }

    "validate data" in {
      import testCases.base._
      def firstname = at(Path \ "firstname")(req[String] andThen notEmpty)

      firstname.validate(transform(valid)) shouldBe (Valid("Julien"))

      def label = at(Path \ "informations" \ "label")(req[String] andThen notEmpty)

      label.validate(transform(valid)) shouldBe (Valid("Personal"))
      label.validate(transform(invalid)) shouldBe
        (Invalid(Seq((Path \ "informations" \ "label") ->
          Seq(ValidationError("error.required")))))
    }

    "validate optional" in {
      import testCases.base._
      def firstname = at(Path \ "firstname")(opt(is[String] andThen notEmpty))
      firstname.validate(transform(valid)) shouldBe (Valid(Some("Julien")))

      def foobar = at(Path \ "foobar")(opt(is[String] andThen notEmpty))
      foobar.validate(transform(valid)) shouldBe (Valid(None))
    }

    "validate deep" in {
      import testCases.base._

      val v = is[String] andThen notEmpty

      def label =
        at(Path \ "informations")(
          req(at(Path \ "label")(req(v)))
        )

      label.validate(transform(valid)) shouldBe (Valid("Personal"))

      val p = (Path \ "informations" \ "label")
      label.validate(transform(invalid)) shouldBe
        (Invalid(Seq(p ->
          Seq(ValidationError("error.required")))))

      def maybeLabel =
        at(Path \ "informations")(
          opt(at(Path \ "label")(req(v)))
        )

      maybeLabel.validate(transform(valid)) shouldBe (Valid(Option("Personal")))
      maybeLabel.validate(transform(invalid)) shouldBe (Valid(None))
    }

    "coerce type" in {
      import testCases.base._
      def age = at(Path \ "age")(req[Int])
      age.validate(transform(valid)) shouldBe (Valid(27))

      def ageMin = at(Path \ "age")(req[Int] andThen min(20))
      ageMin.validate(transform(valid)) shouldBe (Valid(27))

      def ageMax = at(Path \ "age")(req[Int] andThen max(50))
      ageMax.validate(transform(valid)) shouldBe (Valid(27))

      def ageMin50 = at(Path \ "age")(req[Int] andThen min(50))
      ageMin50.validate(transform(valid)) shouldBe
        (Invalid(Seq((Path \ "age") ->
          Seq(ValidationError("error.min", 50)))))

      def ageMax0 = at(Path \ "age")(req[Int] andThen max(0))
      ageMax0.validate(transform(valid)) shouldBe
        (Invalid(Seq((Path \ "age") ->
            Seq(ValidationError("error.max", 0)))))

      def firstname = at(Path \ "firstname")(req[Int])
      firstname.validate(transform(valid)) shouldBe
        (Invalid(Seq((Path \ "firstname") -> Seq(
                      ValidationError("error.number", "Int")))))
    }

    "compose constraints" in {
      import testCases.base._
      import cats.syntax.semigroup._

      val composed = notEmpty |+| minLength(3)
      def firstname = at(Path \ "firstname")(req[String] andThen composed)
      firstname.validate(transform(valid)) shouldBe (Valid("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Invalid(Seq(p -> Seq(ValidationError("error.required"),
                                     ValidationError("error.minLength", 3))))
      val label = at(p)(req[String] andThen composed)
      label.validate(transform(invalid)) shouldBe (err)
    }

    "compose validations" in {
      import testCases.base._

      val ne = req[String] andThen notEmpty

      def names =
        at(Path \ "firstname")(ne) ~:
        at(Path \ "lastname")(ne) ~:
        knil

      names.map(_.tupled).validate(transform(valid)) shouldBe Valid("Julien" -> "Tournay")

      def full =
        names ~:
        at(Path \ "informations" \ "label")(ne) ~:
        knil


      full.validate(transform(emptyObj)) shouldBe
        Invalid(Seq(
          (Path \ "firstname") -> Seq(ValidationError("error.required")),
          (Path \ "lastname") -> Seq(ValidationError("error.required")),
          (Path \ "informations" \ "label") -> Seq(ValidationError("error.required"))
        ))

      full.map(_.tupled).validate(transform(invalid)) shouldBe
        Invalid(Seq((Path \ "informations" \ "label") -> Seq(
          ValidationError("error.required"))))
    }

    "lift validations to seq validations" in {
      import testCases.seq._

      // def fooList = at(Path \ "foo")(req(list(string)))
      // fooList.validate(transform(fooBars)) shouldBe Valid(Seq("bar"))

      // def fooSeq = at(Path \ "foo")(req(seq(string)))
      // fooSeq.validate(transform(fooBars)) shouldBe Valid(Seq("bar"))

      // def foo = at(Path \ "foo")(req[Seq[String]] andThen forall(notEmpty))
      // foo.validate(transform(fooBars)) shouldBe Valid(Seq("bar"))

      // def foofoo =
      //   at(Path \ "foo"){
      //     req(at(Path \ "foo")(req(is[Seq[String]] andThen forall(notEmpty))))
      //   }
      // foofoo.validate(transform(foofoobars)) shouldBe Valid(Seq("bar"))

      def n = at(Path \ "n")(req[Seq[String]] andThen forall(notEmpty))
      n.validate(transform(ns)) shouldBe
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

      rule.validate(transform(ok)) shouldBe Valid("Alice" -> "s3cr3t")

      rule.validate(transform(testCases.password.empty)) shouldBe
        Invalid(Seq(Path \ "verify" ->
          Seq(ValidationError("error.required"))))

      rule.validate(transform(err)) shouldBe
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

        val rule = rb orElse rc orElse Rule(_ => typeInvalid)

        rule.validate(transform(b)) shouldBe Valid(B(4))
        rule.validate(transform(c)) shouldBe Valid(C(6))
        rule.validate(transform(e)) shouldBe
          Invalid(Seq(Path ->
            Seq(ValidationError("validation.unknownType"))))
      }

      "by dicriminating on fields" in {
        val rule =
          at(Path \ "name")(req[String]).flatMap[A] {
            case "B" => at(Path \ "foo")(req[Int]).map(B.apply)
            case "C" => at(Path \ "bar")(req[Int]).map(C.apply)
            case _ => Rule(_ => typeInvalid)
          }

        rule.validate(transform(b)) shouldBe Valid(B(4))
        rule.validate(transform(c)) shouldBe Valid(C(6))
        rule.validate(transform(e)) shouldBe
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
      rule.validate(transform(valid)) shouldBe (Valid(expected))
      rule.validate(transform(invalid)) shouldBe
        (Invalid(Seq((Path \ "contacts" \ 0 \ "label") -> Seq(
          ValidationError("error.required")))))
    }


    // "read recursive" when {
    //   import testCases.rec._

    //   case class RecUser(name: String, friends: Seq[RecUser] = Nil)
    //   val u = RecUser("bob", Seq(RecUser("tom")))

    //   case class User1(name: String, friend: Option[User1] = None)
    //   val u1 = User1("bob", Some(User1("tom")))

    //   "using explicit notation" in {
    //     lazy val w: Rule[grammar.Out, RecUser] =
    //       (
    //         at(Path \ "name")(req[String]) ~:
    //         at(Path \ "friends")(req(seq(w))) ~:
    //         knil
    //       ).to[RecUser]

    //     w.validate(transform(bobAndFriends)) shouldBe Valid(u)

    //     lazy val w2: Rule[grammar.Out, RecUser] =
    //       (
    //         at(Path \ "name")(req[String]) ~:
    //         at(Path \ "friends")(req(seq(w2))) ~:
    //         knil
    //       ).to[RecUser]

    //     w2.validate(transform(bobAndFriends)) shouldBe Valid(u)

    //     lazy val w3: Rule[grammar.Out, User1] =
    //       (
    //         at(Path \ "name")(req[String]) ~:
    //         at(Path \ "friend")(opt(w3)) ~:
    //         knil
    //       ).to[User1]

    //     w3.validate(transform(bobAndFriend)) shouldBe Valid(u1)
    //   }

    //   "using implicit notation" in {
    //     implicit lazy val w: Rule[grammar.Out, RecUser] =
    //       (
    //         at(Path \ "name")(req[String]) ~:
    //         at(Path \ "friends")(req[Seq[RecUser]]) ~:
    //         knil
    //       ).to[RecUser]

    //     w.validate(transform(bobAndFriends)) shouldBe Valid(u)

    //     implicit lazy val w3: Rule[grammar.Out, User1] =
    //       (
    //         at(Path \ "name")(req[String]) ~:
    //         at(Path \ "friend")(opt[User1]) ~:
    //         knil
    //       ).to[User1]

    //     w3.validate(transform(bobAndFriend)) shouldBe Valid(u1)
    //   }
    // }

  }
}