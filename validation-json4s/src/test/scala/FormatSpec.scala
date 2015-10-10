import jto.validation._
import jto.validation.json4s._
import org.scalatest._
import org.json4s.ast.safe._

class FormatSpec extends WordSpec with Matchers {
  case class User(id: Long, name: String)
  val luigi = User(1, "Luigi")

  "Format" should {





    "serialize and deserialize primitives" in {
      import Rules._
      import Writes._

      val f = Formatting[JValue, JObject] { __ =>
        (__ \ "id").format[Long]
      }

      val m = JObject(Map("id" -> JNumber(1L)))

      f.writes(1L) shouldBe(m)
      f.validate(m) shouldBe(Valid(1L))

      (Path \ "id").from[JValue](f).validate(JObject(Map())) shouldBe(Invalid(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize String" in {
      import Rules._
      import Writes._

      val f = Formatting[JValue, JObject] { __ =>
        (__ \ "id").format[String]
      }

      val m = JObject(Map("id" -> JString("CAFEBABE")))

      f.writes("CAFEBABE") shouldBe(m)
      f.validate(m) shouldBe(Valid("CAFEBABE"))

      (Path \ "id").from[JValue](f).validate(JObject(Map())) shouldBe(Invalid(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize Seq[String]" in {
      import Rules._
      import Writes._

      val f = Formatting[JValue, JObject] { __ => (__ \ "ids").format[Seq[String]] }
      val m = JObject(Map("ids" -> JArray(JString("CAFEBABE"), JString("FOOBAR"))))

      f.validate(m) shouldBe(Valid(Seq("CAFEBABE", "FOOBAR")))
      f.writes(Seq("CAFEBABE", "FOOBAR")) shouldBe(m)
    }

    "serialize and deserialize User case class" in {
      import Rules._
      import Writes._

      implicit val userF = Formatting[JValue, JObject] { __ =>
        ((__ \ "id").format[Long] ~
         (__ \ "name").format[String]).unlifted(User.apply, User.unapply)
      }

      val m = JObject(Map("id" -> JNumber(1L), "name" -> JString("Luigi")))
      userF.validate(m) shouldBe(Valid(luigi))
    }

    "support primitives types" when {
      import Rules._
      import Writes._

      "Int" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Int] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Int] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Int] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o").format[Int] }.validate(JObject(Map("n" -> JObject(Map("o" -> JNumber(4)))))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o").format[Int] }.validate(JObject(Map("n" -> JObject(Map("o" -> JString("foo")))))) shouldBe(Invalid(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o" \ "p").format[Int] }.validate(JObject(Map("n" -> JObject(Map("o" -> JObject(Map("p" -> JNumber(4)))))))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o" \ "p").format[Int] }.validate(JObject(Map("n" -> JObject(Map("o" -> JObject(Map("p" -> JString("foo")))))))) shouldBe(Invalid(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Invalid(Seq(errPath -> Seq(ValidationError("error.required"))))
        Formatting[JValue, JObject] { __ => (__ \ "foo").format[Int] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(error)
      }

      "Short" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Short] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Short] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Short] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Long] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Long] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Long] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Float] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Float] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Float] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Valid(4.8F))
      }

      "Double" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Double] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Double] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Double] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Valid(4.8))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        Formatting[JValue, JObject] { __ => (__ \ "n").format[jBigDecimal] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(new jBigDecimal("4")))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[jBigDecimal] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[jBigDecimal] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Valid(new jBigDecimal("4.8")))
      }

      "scala BigDecimal" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[BigDecimal] }.validate(JObject(Map("n" -> JNumber(4)))) shouldBe(Valid(BigDecimal(4)))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[BigDecimal] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[BigDecimal] }.validate(JObject(Map("n" -> JNumber(4.8)))) shouldBe(Valid(BigDecimal(4.8)))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.date, Writes.date)
        }.validate(JObject(Map("n" -> JString("1985-09-10")))) shouldBe(Valid(f.parse("1985-09-10")))

        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.date, Writes.date)
        }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date (Can't test on CI)" ignore {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.isoDate, Writes.isoDate)
        }.validate(JObject(Map("n" -> JString("1985-09-10T00:00:00+02:00")))) shouldBe(Valid(f.parse("1985-09-10")))

        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.isoDate, Writes.isoDate)
        }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date.isoformat")))))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaDate, Writes.jodaDate)
          }.validate(JObject(Map("n" -> JString("1985-09-10")))) shouldBe(Valid(jd))

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaDate, Writes.jodaDate)
          }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaTime, Writes.jodaTime)
          }.validate(JObject(Map("n" -> JNumber(dd.getTime)))) shouldBe(Valid(jd))

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaDate, Writes.jodaTime)
          }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaLocalDate, Writes.jodaLocalDate)
          }.validate(JObject(Map("n" -> JString(ld.toString())))) shouldBe(Valid(ld))

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaLocalDate, Writes.jodaLocalDate)
          }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())

        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.sqlDate, Writes.sqlDate)
        }.validate(JObject(Map("n" -> JString("1985-09-10")))) shouldBe(Valid(ds))
      }

      "Boolean" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Boolean] }.validate(JObject(Map("n" -> JBoolean(true)))) shouldBe(Valid(true))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Boolean] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[String] }.validate(JObject(Map("n" -> JString("foo")))) shouldBe(Valid("foo"))
        Formatting[JValue, JObject] { __ => (__ \ "o").format[String] }.validate(JObject(Map("o.n" -> JString("foo")))) shouldBe(Invalid(Seq(Path \ "o" -> Seq(ValidationError("error.required")))))
      }

      "Option" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject(Map("n" -> JBoolean(true)))) shouldBe(Valid(Some(true)))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject(Map())) shouldBe(Valid(None))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject(Map("foo" -> JString("bar")))) shouldBe(Valid(None))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject(Map("n" -> JString("bar")))) shouldBe(Invalid(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, Seq[V]]" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Map[String, Seq[String]]] }.validate(JObject(Map("n" -> JObject(Map("foo" -> JArray(JString("bar"))))))) shouldBe(Valid(Map("foo" -> Seq("bar"))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Map[String, Seq[Int]]] }.validate(JObject(Map("n" -> JObject(Map("foo" -> JArray(JNumber(4)), "bar" -> JArray(JNumber(5))))))) shouldBe(Valid(Map("foo" -> Seq(4), "bar" -> Seq(5))))
        Formatting[JValue, JObject] { __ => (__ \ "x").format[Map[String, Int]] }.validate(JObject(Map("n" -> JObject(Map("foo" -> JNumber(4), "bar" -> JString("frack")))))) shouldBe(Invalid(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Map[String, Seq[Int]]] }.validate(JObject(Map("n" -> JObject(Map("foo" -> JArray(JNumber(4)), "bar" -> JArray(JString("frack"))))))) shouldBe(Invalid(Seq(Path \ "n" \ "bar" \ 0 -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Traversable[String]] }.validate(JObject(Map("n" -> JArray(JString("foo"))))).toOption.get.toSeq shouldBe(Seq("foo"))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Traversable[Int]] }.validate(JObject(Map("n" -> JArray(JNumber(1), JNumber(2), JNumber(3))))).toOption.get.toSeq shouldBe(Seq(1, 2, 3))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Traversable[Int]] }.validate(JObject(Map("n" -> JArray(JString("1"), JString("paf"))))) shouldBe(Invalid(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }

      "Array" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Array[String]] }.validate(JObject(Map("n" -> JArray(JString("foo"))))).toOption.get.toSeq shouldBe(Seq("foo"))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Array[Int]] }.validate(JObject(Map("n" -> JArray(JNumber(1), JNumber(2), JNumber(3))))).toOption.get.toSeq shouldBe(Seq(1, 2, 3))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Array[Int]] }.validate(JObject(Map("n" -> JArray(JString("1"), JString("paf"))))) shouldBe(Invalid(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }

      "Seq" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Seq[String]] }.validate(JObject(Map("n" -> JArray(JString("foo"))))).toOption.get shouldBe(Seq("foo"))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Seq[Int]] }.validate(JObject(Map("n" -> JArray(JNumber(1), JNumber(2), JNumber(3))))).toOption.get shouldBe(Seq(1, 2, 3))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Seq[Int]] }.validate(JObject(Map("n" -> JArray(JString("1"), JString("paf"))))) shouldBe(Invalid(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }
    }

    "serialize and deserialize with validation" in {
      import Rules._
      import Writes._

      val f = Formatting[JValue, JObject] { __ =>
        ((__ \ "firstname").format(notEmpty) ~
         (__ \ "lastname").format(notEmpty)).tupled
      }

      val valid = JObject(Map(
        "firstname" -> JString("Julien"),
        "lastname" -> JString("Tournay")))

      val invalid = JObject(Map("lastname" -> JString("Tournay")))

      val result = ("Julien", "Tournay")

      f.writes(result) shouldBe(valid)
      f.validate(valid) shouldBe(Valid(result))

      f.validate(invalid) shouldBe(Invalid(Seq((Path \ "firstname", Seq(ValidationError("error.required"))))))
    }

    "format seq" in {
      import Rules._
      import Writes._

      val valid = JObject(Map(
      "firstname" -> JArray(JString("Julien")),
      "foobar" -> JArray(),
      "lastname" -> JString("Tournay"),
      "age" -> JNumber(27),
      "information" -> JObject(Map(
        "label" -> JString("Personal"),
        "email" -> JString("fakecontact@gmail.com"),
        "phones" -> JArray(JString("01.23.45.67.89"), JString("98.76.54.32.10"))))))

      def isNotEmpty[T <: Traversable[_]] = validateWith[T]("error.notEmpty"){ !_.isEmpty }

      Formatting[JValue, JObject] { __ => (__ \ "firstname").format[Seq[String]] }.validate(valid) shouldBe(Valid(Seq("Julien")))
      Formatting[JValue, JObject] { __ => (__ \ "foobar").format[Seq[String]] }.validate(valid) shouldBe(Valid(Seq()))
      Formatting[JValue, JObject] { __ => (__ \ "foobar").format(isNotEmpty[Seq[Int]]) }.validate(valid) shouldBe(Invalid(Seq(Path \ "foobar" -> Seq(ValidationError("error.notEmpty")))))
    }

    "format recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser(
        "bob",
        Seq(RecUser("tom")))

      val m = JObject(Map(
        "name" -> JString("bob"),
        "friends" -> JArray(JObject(Map("name" -> JString("tom"), "friends" -> JArray())))))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(Map(
        "name" -> JString("bob"),
        "friend" -> JObject(Map("name" -> JString("tom")))))

      "using explicit notation" in {
        import Rules._
        import Writes._

        lazy val w: Format[JValue, JObject, RecUser] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friends").format(seqR(w), seqW(w))).unlifted(RecUser.apply, RecUser.unapply)
        }
        w.validate(m) shouldBe Valid(u)
        w.writes(u) shouldBe m

        lazy val w3: Format[JValue, JObject, User1] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friend").format(optionR(w3), optionW(w3))).unlifted(User1.apply, User1.unapply)
        }
        w3.validate(m1) shouldBe Valid(u1)
        w3.writes(u1) shouldBe m1
      }

      "using implicit notation" in {
        import Rules._
        import Writes._

        implicit lazy val w: Format[JValue, JObject, RecUser] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friends").format[Seq[RecUser]]).unlifted(RecUser.apply, RecUser.unapply)
        }
        w.validate(m) shouldBe Valid(u)
        w.writes(u) shouldBe m

        implicit lazy val w3: Format[JValue, JObject, User1] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friend").format[Option[User1]]).unlifted(User1.apply, User1.unapply)
        }
        w3.validate(m1) shouldBe Valid(u1)
        w3.writes(u1) shouldBe m1
      }
    }

    "work with Rule ans Write seamlessly" in {
      import Rules._
      import Writes._

      implicit val userF = Formatting[JValue, JObject] { __ =>
        ((__ \ "id").format[Long] ~
         (__ \ "name").format[String]).unlifted(User.apply, User.unapply)
      }

      val  userJs = JObject(Map("id" -> JNumber(1L), "name" -> JString("Luigi")))
      userF.validate(userJs) shouldBe(Valid(luigi))
      userF.writes(luigi) shouldBe(userJs)

      val fin = From[JObject] { __ =>
        (__ \ "user").read[User]
      }

      val m2 = JObject(Map("user" -> userJs))
      fin.validate(m2) shouldBe(Valid(luigi))

      val win = To[JValue] { __ =>
        (__ \ "user").write[User]
      }
      win.writes(luigi) shouldBe(m2)
    }

  }

}
