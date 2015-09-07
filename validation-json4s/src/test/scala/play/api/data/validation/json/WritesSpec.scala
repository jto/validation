import jto.validation._
import jto.validation.json4s.Writes._
import org.specs2.mutable._
import org.json4s._

class WritesSpec extends Specification {

  case class Contact(
    firstname: String,
    lastname: String,
    company: Option[String],
    informations: Seq[ContactInformation])

  case class ContactInformation(
    label: String,
    email: Option[String],
    phones: Seq[String])

  val contact = Contact("Julien", "Tournay", None, Seq(
    ContactInformation("Personal", Some("fakecontact@gmail.com"), Seq("01.23.45.67.89", "98.76.54.32.10"))))

  val contactJson = JObject(
    "firstname" -> JString("Julien"),
    "lastname" -> JString("Tournay"),
    "informations" -> JArray(List(JObject(
      "label" -> JString("Personal"),
      "email" -> JString("fakecontact@gmail.com"),
      "phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))))

  "Writes" should {

    "write string" in {
      val w = (Path \ "label").write[String, JObject]
      w.writes("Hello World") mustEqual JObject("label" -> JString("Hello World"))
    }

    "ignore values" in {
      (Path \ "n").write(ignored("foo")).writes("test") mustEqual JObject("n" -> JString("foo"))
      (Path \ "n").write(ignored(42)).writes(0) mustEqual JObject("n" -> JInt(42))
    }

    "write option" in {
      val w = (Path \ "email").write[Option[String], JObject]
      w.writes(Some("Hello World")) mustEqual JObject("email" -> JString("Hello World"))
      w.writes(None) mustEqual JObject()

      (Path \ "n").write(optionW(intW)).writes(Some(5)) mustEqual JObject("n" -> JInt(5))
      (Path \ "n").write(optionW(intW)).writes(None) mustEqual JObject()
    }

    "write seq" in {
      val w = (Path \ "phones").write[Seq[String], JObject]
      w.writes(Seq("01.23.45.67.89", "98.76.54.32.10")) mustEqual JObject("phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10"))))
      w.writes(Nil) mustEqual JObject("phones" -> JArray(Nil))
    }

    "support primitives types" in {

      "Int" in {
        (Path \ "n").write[Int, JObject].writes(4) mustEqual(JObject("n" -> JInt(4)))
        (Path \ "n" \ "o").write[Int, JObject].writes(4) mustEqual(JObject("n" -> JObject("o"-> JInt(4))))
        (Path \ "n" \ "o" \ "p").write[Int, JObject].writes(4) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JInt(4)))))
      }

      "Short" in {
        (Path \ "n").write[Short, JObject].writes(4) mustEqual(JObject("n" -> JInt(4)))
        (Path \ "n" \ "o").write[Short, JObject].writes(4) mustEqual(JObject("n" -> JObject("o"-> JInt(4))))
        (Path \ "n" \ "o" \ "p").write[Short, JObject].writes(4) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JInt(4)))))
      }

      "Long" in {
        (Path \ "n").write[Long, JObject].writes(4) mustEqual(JObject("n" -> JInt(4)))
        (Path \ "n" \ "o").write[Long, JObject].writes(4) mustEqual(JObject("n" -> JObject("o"-> JInt(4))))
        (Path \ "n" \ "o" \ "p").write[Long, JObject].writes(4) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JInt(4)))))
      }

      "Float" in {
        (Path \ "n").write[Float, JObject].writes(4.8f) mustEqual(JObject("n" -> JDecimal(4.8)))
        (Path \ "n" \ "o").write[Float, JObject].writes(4.8f) mustEqual(JObject("n" -> JObject("o"-> JDecimal(4.8))))
        (Path \ "n" \ "o" \ "p").write[Float, JObject].writes(4.8f) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JDecimal(4.8)))))
      }

      "Double" in {
        (Path \ "n").write[Double, JObject].writes(4d) mustEqual(JObject("n" -> JDecimal(4.0)))
        (Path \ "n" \ "o").write[Double, JObject].writes(4.8d) mustEqual(JObject("n" -> JObject("o"-> JDecimal(4.8))))
        (Path \ "n" \ "o" \ "p").write[Double, JObject].writes(4.8d) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JDecimal(4.8)))))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        (Path \ "n").write[jBigDecimal, JObject].writes(new jBigDecimal("4.0")) mustEqual(JObject("n" -> JDecimal(4.0)))
        (Path \ "n" \ "o").write[jBigDecimal, JObject].writes(new jBigDecimal("4.8")) mustEqual(JObject("n" -> JObject("o"-> JDecimal(4.8))))
        (Path \ "n" \ "o" \ "p").write[jBigDecimal, JObject].writes(new jBigDecimal("4.8")) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JDecimal(4.8)))))
      }

      "scala BigDecimal" in {
        (Path \ "n").write[BigDecimal, JObject].writes(BigDecimal("4.0")) mustEqual(JObject("n" -> JDecimal(4.0)))
        (Path \ "n" \ "o").write[BigDecimal, JObject].writes(BigDecimal("4.8")) mustEqual(JObject("n" -> JObject("o"-> JDecimal(4.8))))
        (Path \ "n" \ "o" \ "p").write[BigDecimal, JObject].writes(BigDecimal("4.8")) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JDecimal(4.8)))))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        (Path \ "n").write(date).writes(d) mustEqual(JObject("n" -> JString("1985-09-10")))
      }

      "iso date" in {
        skipped("Can't test on CI")
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        (Path \ "n").write(isoDate).writes(d) mustEqual(JObject("n" -> JString("1985-09-10T00:00:00+02:00")))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n").write(jodaDate).writes(jd) mustEqual(JObject("n" -> JString("1985-09-10")))
        }

        "time" in {
          (Path \ "n").write(jodaTime).writes(jd) mustEqual(JObject("n" -> JInt(dd.getTime)))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n").write(jodaLocalDate).writes(ld) mustEqual(JObject("n" -> JString(ld.toString)))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n").write(sqlDate).writes(ds) mustEqual(JObject("n" -> JString("1985-09-10")))
      }

      "Boolean" in {
        (Path \ "n").write[Boolean, JObject].writes(true) mustEqual(JObject("n" -> JBool(true)))
        (Path \ "n" \ "o").write[Boolean, JObject].writes(false) mustEqual(JObject("n" -> JObject("o"-> JBool(false))))
        (Path \ "n" \ "o" \ "p").write[Boolean, JObject].writes(true) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JBool(true)))))
      }

      "String" in {
        (Path \ "n").write[String, JObject].writes("foo") mustEqual(JObject("n" -> JString("foo")))
        (Path \ "n" \ "o").write[String, JObject].writes("foo") mustEqual(JObject("n" -> JObject("o"-> JString("foo"))))
        (Path \ "n" \ "o" \ "p").write[String, JObject].writes("foo") mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JString("foo")))))
      }

      "Option" in {
        (Path \ "n").write[Option[String], JObject].writes(Some("foo")) mustEqual(JObject("n" -> JString("foo")))
        (Path \ "n" \ "o").write[Option[String], JObject].writes(Some("foo")) mustEqual(JObject("n" -> JObject("o"-> JString("foo"))))
        (Path \ "n" \ "o" \ "p").write[Option[String], JObject].writes(Some("foo")) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JString("foo")))))

        (Path \ "n").write[Option[String], JObject].writes(None) mustEqual(JObject())
        (Path \ "n" \ "o").write[Option[String], JObject].writes(None) mustEqual(JObject())
        (Path \ "n" \ "o" \ "p").write[Option[String], JObject].writes(None) mustEqual(JObject())
      }

      "Map[String, Seq[V]]" in {
        (Path \ "n").write[Map[String, Seq[String]], JObject].writes(Map("foo" -> Seq("bar"))) mustEqual(JObject("n" -> JObject("foo" -> JArray(List(JString("bar"))))))
        (Path \ "n").write[Map[String, Seq[Int]], JObject].writes(Map("foo" -> Seq(4))) mustEqual(JObject("n" -> JObject("foo" -> JArray(List(JInt(4))))))
        (Path \ "n" \ "o").write[Map[String, Seq[Int]], JObject].writes(Map("foo" -> Seq(4))) mustEqual(JObject("n" -> JObject("o" -> JObject("foo" -> JArray(List(JInt(4)))))))
        (Path \ "n" \ "o").write[Map[String, Int], JObject].writes(Map("foo" -> 4)) mustEqual(JObject("n" -> JObject("o" -> JObject("foo" -> JInt(4)))))
        (Path \ "n" \ "o").write[Map[String, Int], JObject].writes(Map.empty) mustEqual(JObject("n" -> JObject("o" -> JObject())))
      }

      "Traversable" in {
        (Path \ "n").write[Traversable[String], JObject].writes(Array("foo", "bar")) mustEqual(JObject("n" -> JArray(List(JString("foo"), JString("bar")))))
        (Path \ "n" \ "o").write[Traversable[String], JObject].writes(Array("foo", "bar")) mustEqual(JObject("n" -> JObject("o"-> JArray(List(JString("foo"), JString("bar"))))))
        (Path \ "n" \ "o" \ "p").write[Traversable[String], JObject].writes(Array("foo", "bar")) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JArray(List(JString("foo"), JString("bar")))))))

        (Path \ "n").write[Traversable[String], JObject].writes(Array[String]()) mustEqual(JObject("n" -> JArray(Nil)))
        (Path \ "n" \ "o").write[Traversable[String], JObject].writes(Array[String]()) mustEqual(JObject("n" -> JObject("o"-> JArray(Nil))))
        (Path \ "n" \ "o" \ "p").write[Traversable[String], JObject].writes(Array[String]()) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JArray(Nil)))))
      }

      "Array" in {
        (Path \ "n").write[Array[String], JObject].writes(Array("foo", "bar")) mustEqual(JObject("n" -> JArray(List(JString("foo"), JString("bar")))))
        (Path \ "n" \ "o").write[Array[String], JObject].writes(Array("foo", "bar")) mustEqual(JObject("n" -> JObject("o"-> JArray(List(JString("foo"), JString("bar"))))))
        (Path \ "n" \ "o" \ "p").write[Array[String], JObject].writes(Array("foo", "bar")) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JArray(List(JString("foo"), JString("bar")))))))

        (Path \ "n").write[Array[String], JObject].writes(Array()) mustEqual(JObject("n" -> JArray(Nil)))
        (Path \ "n" \ "o").write[Array[String], JObject].writes(Array()) mustEqual(JObject("n" -> JObject("o"-> JArray(Nil))))
        (Path \ "n" \ "o" \ "p").write[Array[String], JObject].writes(Array()) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JArray(Nil)))))
      }

      "Seq" in {
        (Path \ "n").write[Seq[String], JObject].writes(Seq("foo", "bar")) mustEqual(JObject("n" -> JArray(List(JString("foo"), JString("bar")))))
        (Path \ "n" \ "o").write[Seq[String], JObject].writes(Seq("foo", "bar")) mustEqual(JObject("n" -> JObject("o"-> JArray(List(JString("foo"), JString("bar"))))))
        (Path \ "n" \ "o" \ "p").write[Seq[String], JObject].writes(Seq("foo", "bar")) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JArray(List(JString("foo"), JString("bar")))))))

        (Path \ "n").write[Seq[String], JObject].writes(Nil) mustEqual(JObject("n" -> JArray(Nil)))
        (Path \ "n" \ "o").write[Seq[String], JObject].writes(Nil) mustEqual(JObject("n" -> JObject("o"-> JArray(Nil))))
        (Path \ "n" \ "o" \ "p").write[Seq[String], JObject].writes(Nil) mustEqual(JObject("n" -> JObject("o"-> JObject("p"-> JArray(Nil)))))
      }
    }

    "format data" in {
      val formatter = Write[Double, String]{ money =>
        import java.text.NumberFormat
        import java.util.Locale
        val f = NumberFormat.getCurrencyInstance(Locale.FRANCE)
        f.format(money)
      }
      val w = (Path \ "foo").write(formatter)
      w.writes(500d) mustEqual(JObject("foo" -> JString("500,00 €")))

      val w2 = To[JValue] { __ => (__ \ "foo").write(formatter) }
      w2.writes(500d) mustEqual(JObject("foo" -> JString("500,00 €")))
    }

    "compose" in {
      val w = To[JObject] { __ =>
        ((__ \ "email").write[Option[String]] ~
         (__ \ "phones").write[Seq[String]]).tupled
      }

      val v =  Some("jto@foobar.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")

      w.writes(v) mustEqual JObject("email" -> JString("jto@foobar.com"), "phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10"))))
      w.writes(Some("jto@foobar.com") -> Nil) mustEqual JObject("email" -> JString("jto@foobar.com"), "phones" -> JArray(Nil))
      w.writes(None -> Nil) mustEqual JObject("phones" -> JArray(Nil))
    }

    // "write Invalid" in {
    //   val f = Invalid[(Path, Seq[ValidationError]), String](Seq(Path \ "n" -> Seq(ValidationError("validation.type-mismatch", "Int"))))

    //   implicitly[Write[(Path, Seq[ValidationError]), JObject]]
    //   implicitly[Write[Invalid[(Path, Seq[ValidationError]), String], JObject]]

    //   val error =
    //     JObject("errors" ->
    //       JObject("/n" -> JArray(List(
    //           JObject(
    //             "msg" -> JString("validation.type-mismatch"),
    //             "args" -> JArray(List(JString("Int"))))))))

    //   (Path \ "errors").write[Invalid[(Path, Seq[ValidationError]), String], JObject]
    //     .writes(f) mustEqual(error)
    // }

    "write Map" in {
      implicit val contactInformation = To[JObject] { __ =>
        ((__ \ "label").write[String] ~
          (__ \ "email").write[Option[String]] ~
          (__ \ "phones").write[Seq[String]]) (ContactInformation.unapply _)
      }

      implicit val contactWrite = To[JObject] { __ =>
        ((__ \ "firstname").write[String] ~
         (__ \ "lastname").write[String] ~
         (__ \ "company").write[Option[String]] ~
         (__ \ "informations").write[Seq[ContactInformation]]) (Contact.unapply _)
      }

      contactWrite.writes(contact) mustEqual contactJson
    }

    "write recursive" in {
      case class RecUser(name: String, friends: List[RecUser] = Nil)
      val u = RecUser(
        "bob",
        List(RecUser("tom")))

      val m = JObject(
        "name" -> JString("bob"),
        "friends" -> JArray(List(JObject("name" -> JString("tom"), "friends" -> JArray(Nil)))))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(
        "name" -> JString("bob"),
        "friend" -> JObject("name" -> JString("tom")))

      "using explicit notation" in {
        lazy val w: Write[RecUser, JObject] = To[JObject]{ __ =>
          ((__ \ "name").write[String] ~
           (__ \ "friends").write(seqW(w)))(RecUser.unapply _)
        }
        w.writes(u) mustEqual m

        lazy val w2: Write[RecUser, JObject] =
          ((Path \ "name").write[String, JObject] ~
           (Path \ "friends").write(seqW(w2)))(RecUser.unapply _)
        w2.writes(u) mustEqual m

        lazy val w3: Write[User1, JObject] = To[JObject]{ __ =>
          ((__ \ "name").write[String] ~
           (__ \ "friend").write(optionW(w3)))(User1.unapply _)
        }
        w3.writes(u1) mustEqual m1
      }

      "using implicit notation" in {
        implicit lazy val w: Write[RecUser, JObject] = To[JObject]{ __ =>
          ((__ \ "name").write[String] ~
           (__ \ "friends").write[Seq[RecUser]])(RecUser.unapply _)
        }
        w.writes(u) mustEqual m

        implicit lazy val w3: Write[User1, JObject] = To[JObject]{ __ =>
          ((__ \ "name").write[String] ~
           (__ \ "friend").write[Option[User1]])(User1.unapply _)
        }
        w3.writes(u1) mustEqual m1
      }

    }

    "support write of value class" in {
      import TestValueClass._

      val w = To[JObject] { __ =>
        (__ \ "id").write[Id]
      }

      w.writes(Id("1")) mustEqual JObject("id" -> JString("1"))
    }

  }

}

object TestValueClass {
  case class Id(value: String) extends AnyVal
  object Id {
    implicit val writes: Write[Id, JString] = Write(id => JString(id.value))
  }
}
