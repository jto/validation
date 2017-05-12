package jto.validation
package v3.tagless

import org.scalatest._

trait WritesSpec[T] extends WordSpec with Matchers {
  val testCases: TestCases[T]
  val grammar: Grammar[T, types.flip[Write]#λ]

  import grammar._

  case class Contact(firstname: String,
                     lastname: String,
                     company: Option[String],
                     informations: Seq[ContactInformation])

  case class ContactInformation(
      label: String, email: Option[String], phones: Seq[String])

  val contact = Contact(
      "Julien",
      "Tournay",
      None,
      Seq(
          ContactInformation("Personal",
                             Some("fakecontact@gmail.com"),
                             Seq("01.23.45.67.89", "98.76.54.32.10"))))

  "Writes" should {

    "support primitives types" when {
      "Int" in {
        import testCases.int._
        at(Path \ "n")(req[Int]).writes(4) shouldBe ok
        at(Path \ "n" \ "o")(req[Int]).writes(4) shouldBe noOK
        at(Path \ "n" \ "o" \ "p")(req[Int]).writes(4) shouldBe nopOK
      }

      "Short" in {
        import testCases.int._
        at(Path \ "n")(req[Short]).writes(4) shouldBe ok
        at(Path \ "n" \ "o")(req[Short]).writes(4) shouldBe noOK
        at(Path \ "n" \ "o" \ "p")(req[Short]).writes(4) shouldBe nopOK
      }

      "Long" in {
        import testCases.int._
        at(Path \ "n")(req[Long]).writes(4) shouldBe ok
        at(Path \ "n" \ "o")(req[Long]).writes(4) shouldBe noOK
        at(Path \ "n" \ "o" \ "p")(req[Long]).writes(4) shouldBe nopOK
      }

      "Float" in {
        import testCases.int.{float => f}
        at(Path \ "n")(req[Float]).writes(4.5f) shouldBe f
      }

      "Double" in {
        import testCases.int.{float => f}
        at(Path \ "n")(req[Double]).writes(4.5d) shouldBe f
      }

      "java BigDecimal" in {
        import java.math.{BigDecimal => JBigDecimal}
        import testCases.int.{float => f}
        at(Path \ "n")(req[JBigDecimal]).writes(new JBigDecimal("4.5")) shouldBe f
      }

      "scala BigDecimal" in {
        import testCases.int.{float => f}
        at(Path \ "n")(req[BigDecimal]).writes(BigDecimal("4.5")) shouldBe f
      }

      "Boolean" in {
        import testCases.boolean._
        at(Path \ "n")(req[Boolean]).writes(true) shouldBe ok
      }

      "String" in {
        import testCases.string._
        at(Path \ "n")(req[String]).writes("foo") shouldBe foo
        at(Path \ "o" \ "n")(req[String]).writes("foo") shouldBe onFoo
      }

      "Option" in {
        import testCases.option._
        at(Path \ "foo")(opt[String]).writes(Option("bar")) shouldBe fooBar
        at(Path \ "foo")(opt[String]).writes(None) shouldBe none
        at(Path \ "foo" \ "bar")(opt[String]).writes(None) shouldBe none
      }

    //   "Map[String, Seq[V]]" in {
    //     (Path \ "n")
    //       .write[Map[String, Seq[String]], JsObject]
    //       .writes(Map("foo" -> Seq("bar"))) shouldBe
    //     (Json.obj("n" -> Json.obj("foo" -> Seq("bar"))))
    //     (Path \ "n")
    //       .write[Map[String, Seq[Int]], JsObject]
    //       .writes(Map("foo" -> Seq(4))) shouldBe
    //     (Json.obj("n" -> Json.obj("foo" -> Seq(4))))
    //     (Path \ "n" \ "o")
    //       .write[Map[String, Seq[Int]], JsObject]
    //       .writes(Map("foo" -> Seq(4))) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj("foo" -> Seq(4)))))
    //     (Path \ "n" \ "o")
    //       .write[Map[String, Int], JsObject]
    //       .writes(Map("foo" -> 4)) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj("foo" -> 4))))
    //     (Path \ "n" \ "o").write[Map[String, Int], JsObject].writes(Map.empty) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj())))
    //   }

    //   "Traversable" in {
    //     (Path \ "n")
    //       .write[Traversable[String], JsObject]
    //       .writes(Array("foo", "bar")) shouldBe
    //     (Json.obj("n" -> Seq("foo", "bar")))
    //     (Path \ "n" \ "o")
    //       .write[Traversable[String], JsObject]
    //       .writes(Array("foo", "bar")) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Seq("foo", "bar"))))
    //     (Path \ "n" \ "o" \ "p")
    //       .write[Traversable[String], JsObject]
    //       .writes(Array("foo", "bar")) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq("foo", "bar")))))

    //     (Path \ "n")
    //       .write[Traversable[String], JsObject]
    //       .writes(Array[String]()) shouldBe (Json.obj("n" -> Seq[String]()))
    //     (Path \ "n" \ "o")
    //       .write[Traversable[String], JsObject]
    //       .writes(Array[String]()) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Seq[String]())))
    //     (Path \ "n" \ "o" \ "p")
    //       .write[Traversable[String], JsObject]
    //       .writes(Array[String]()) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq[String]()))))
    //   }

    //   "Array" in {
    //     (Path \ "n").write[Array[String], JsObject].writes(Array("foo", "bar")) shouldBe
    //     (Json.obj("n" -> Seq("foo", "bar")))
    //     (Path \ "n" \ "o")
    //       .write[Array[String], JsObject]
    //       .writes(Array("foo", "bar")) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Seq("foo", "bar"))))
    //     (Path \ "n" \ "o" \ "p")
    //       .write[Array[String], JsObject]
    //       .writes(Array("foo", "bar")) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq("foo", "bar")))))

    //     (Path \ "n").write[Array[String], JsObject].writes(Array()) shouldBe
    //     (Json.obj("n" -> Seq[String]()))
    //     (Path \ "n" \ "o").write[Array[String], JsObject].writes(Array()) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Seq[String]())))
    //     (Path \ "n" \ "o" \ "p").write[Array[String], JsObject].writes(Array()) shouldBe
    //     (Json.obj("n" -> Json.obj("o" -> Json.obj("p" -> Seq[String]()))))
    //   }

      "Seq" in {
        import testCases.seq._
        at(Path \ "n")(req[Seq[String]]).writes(Seq("foo")) shouldBe foos
        at(Path \ "foo" \ "foo")(req[Seq[String]]).writes(Seq("bar")) shouldBe foofoobars
        at(Path \ "n")(req[Seq[Int]]).writes(Seq(1, 2, 3)) shouldBe ints
      }
    }

    "compose" in {
      import testCases.base._
      val w =
        (
          at(Path \ "email")(opt[String]) ~:
          at(Path \ "phones")(req[Seq[String]]) ~:
          knil
        ).tupled

      val v = Some("fakecontact@gmail.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")
      w.writes(v) shouldBe testCases.base.info
      w.writes(None -> Nil) shouldBe noInfo
    }

    // "write Invalid" in {
    //   val f = Invalid[(Path, Seq[ValidationError]), String](Seq(Path \ "n" -> Seq(ValidationError("validation.type-mismatch", "Int"))))

    //   implicitly[Write[(Path, Seq[ValidationError]), JsObject]]
    //   implicitly[Write[Invalid[(Path, Seq[ValidationError]), String], JsObject]]

    //   val error =
    //     Json.obj("errors" ->
    //       Json.obj("/n" -> Json.arr(
    //         Json.obj(
    //           "msg" -> "validation.type-mismatch",
    //           "args" -> Seq("Int")))))

    //   (Path \ "errors").write[Invalid[(Path, Seq[ValidationError]), String], JsObject]
    //     .writes(f) shouldBe(error)
    // }

    "write Map" in {
      import testCases.base._

      implicit val contactInformation =
        {
          at(Path \ "label")(req[String]) ~:
          at(Path \ "email")(opt[String]) ~:
          at(Path \ "phones")(req[Seq[String]]) ~:
          knil
        }.from[ContactInformation]

      val contactWrite =
        {
          at(Path \ "firstname")(req[String]) ~:
          at(Path \ "lastname")(req[String]) ~:
          at(Path \ "company")(opt[String]) ~:
          at(Path \ "informations")(req[Seq[ContactInformation]]) ~:
          knil
        }.from[Contact]

      // TODO: use solver ?
      contactWrite.writes(contact) shouldBe jto
    }

    "write recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))

      "using explicit notation" in {
        import testCases.rec._
        lazy val w: Write[RecUser, Out] =
          {
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friends")(req(seq(w))) ~:
            knil
          }.from[RecUser]

        w.writes(u) shouldBe bobAndFriends

        lazy val w2: Write[User1, Out] =
          {
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friend")(opt(w2)) ~:
            knil
          }.from[User1]

        w2.writes(u1) shouldBe bobAndFriend
      }

      "using implicit notation" in {
        import testCases.rec._
        implicit lazy val w: Write[RecUser, Out] =
          {
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friends")(req[Seq[RecUser]]) ~:
            knil
          }.from[RecUser]

        w.writes(u) shouldBe bobAndFriends

        implicit lazy val w2: Write[User1, Out] =
          {
            at(Path \ "name")(req[String]) ~:
            at(Path \ "friend")(opt[User1]) ~:
            knil
          }.from[User1]

        w2.writes(u1) shouldBe bobAndFriend
      }
    }

    "support write of value class" in {
      import testCases.base._
      import TestValueClass._

      val w = at(Path \ "id")(Id.writes andThen req[String])
      w.writes(Id("1")) shouldBe id
    }
  }

}

object TestValueClass {
  case class Id(value: String) extends AnyVal
  object Id {
    implicit val writes: Write[Id, String] = Write(id => id.value)
  }
}