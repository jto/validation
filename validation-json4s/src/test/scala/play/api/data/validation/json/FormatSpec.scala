/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api.libs.json4s

import org.specs2.mutable._
import play.api.libs.functional.syntax._
import org.json4s._

object FormatSpec extends Specification {
	case class User(id: Long, name: String)
	val luigi = User(1, "Luigi")

	"Format" should {

		import play.api.data.mapping._
		import play.api.data.mapping.json4s.Rules
		import play.api.data.mapping.json4s.Writes
    import Writes._

		"serialize and deserialize primitives" in {
			import Rules._
			import Writes._

			val f = Formatting[JValue, JObject] { __ =>
				(__ \ "id").format[Long]
			}

			val m = JObject("id" -> JInt(1L))

      f.writes(1L) mustEqual(m)
      f.validate(m) mustEqual(Success(1L))

      (Path \ "id").from[JValue](f).validate(JObject()) mustEqual(Failure(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }


    "serialize and deserialize String" in {
			import Rules._
			import Writes._

			val f = Formatting[JValue, JObject] { __ =>
				(__ \ "id").format[String]
			}

			val m = JObject("id" -> JString("CAFEBABE"))

      f.writes("CAFEBABE") mustEqual(m)
      f.validate(m) mustEqual(Success("CAFEBABE"))

      (Path \ "id").from[JValue](f).validate(JObject()) mustEqual(Failure(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize Seq[String]" in {
			import Rules._
			import Writes._

			val f = Formatting[JValue, JObject] { __ => (__ \ "ids").format[Seq[String]] }
			val m = JObject("ids" -> JArray(List(JString("CAFEBABE"), JString("FOOBAR"))))

			f.validate(m) mustEqual(Success(Seq("CAFEBABE", "FOOBAR")))
      f.writes(Seq("CAFEBABE", "FOOBAR")) mustEqual(m)
    }

    "serialize and deserialize User case class" in {
    	import Rules._
    	import Writes._

	    implicit val userF = Formatting[JValue, JObject] { __ =>
				((__ \ "id").format[Long] ~
			   (__ \ "name").format[String])(User.apply _, unlift(User.unapply _))
			}

			val m = JObject("id" -> JInt(1L), "name" -> JString("Luigi"))
			userF.validate(m) mustEqual(Success(luigi))
		}

		"support primitives types" in {
			import Rules._
    	import Writes._

      "Int" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Int] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Int] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Int] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Int")))))
        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o").format[Int] }.validate(JObject("n" -> JObject("o" -> JInt(4)))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o").format[Int] }.validate(JObject("n" -> JObject("o" -> JString("foo")))) mustEqual(Failure(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))

        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o" \ "p").format[Int] }.validate(JObject("n" -> JObject("o" -> JObject("p" -> JInt(4))))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n" \ "o" \ "p").format[Int] }.validate(JObject("n" -> JObject("o" -> JObject("p" -> JString("foo"))))) mustEqual(Failure(Seq(Path \ "n" \ "o" \ "p" -> Seq(ValidationError("error.number", "Int")))))

        val errPath = Path \ "foo"
        val error = Failure(Seq(errPath -> Seq(ValidationError("error.required"))))
        Formatting[JValue, JObject] { __ => (__ \ "foo").format[Int] }.validate(JObject("n" -> JInt(4))) mustEqual(error)
      }

      "Short" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Short] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Short] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Short] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Short")))))
      }

      "Long" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Long] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Long] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Long] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Long")))))
      }

      "Float" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Float] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Float] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Float")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Float] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(4.8F))
      }

      "Double" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Double] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(4))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Double] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "Double")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Double] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(4.8))
      }

      "java BigDecimal" in {
        import java.math.{ BigDecimal => jBigDecimal }
        Formatting[JValue, JObject] { __ => (__ \ "n").format[jBigDecimal] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(new jBigDecimal("4")))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[jBigDecimal] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[jBigDecimal] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(new jBigDecimal("4.8")))
      }

      "scala BigDecimal" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[BigDecimal] }.validate(JObject("n" -> JInt(4))) mustEqual(Success(BigDecimal(4)))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[BigDecimal] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.number", "BigDecimal")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[BigDecimal] }.validate(JObject("n" -> JDecimal(4.8))) mustEqual(Success(BigDecimal(4.8)))
      }

      "date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.date, Writes.date)
        }.validate(JObject("n" -> JString("1985-09-10"))) mustEqual(Success(f.parse("1985-09-10")))

        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.date, Writes.date)
        }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date" in {
        skipped("Can't test on CI")
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.isoDate, Writes.isoDate)
        }.validate(JObject("n" -> JString("1985-09-10T00:00:00+02:00"))) mustEqual(Success(f.parse("1985-09-10")))

        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.isoDate, Writes.isoDate)
        }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.date.isoformat")))))
      }

      "joda" in {
        import org.joda.time.DateTime
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaDate, Writes.jodaDate)
          }.validate(JObject("n" -> JString("1985-09-10"))) mustEqual(Success(jd))

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaDate, Writes.jodaDate)
          }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "time" in {
          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaTime, Writes.jodaTime)
          }.validate(JObject("n" -> JInt(dd.getTime))) mustEqual(Success(jd))

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaDate, Writes.jodaTime)
          }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaLocalDate, Writes.jodaLocalDate)
          }.validate(JObject("n" -> JString(ld.toString()))) mustEqual(Success(ld))

          Formatting[JValue, JObject] { __ =>
            (__ \ "n").format(Rules.jodaLocalDate, Writes.jodaLocalDate)
          }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        import java.util.Date
        val f = new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())

        Formatting[JValue, JObject] { __ =>
          (__ \ "n").format(Rules.sqlDate, Writes.sqlDate)
        }.validate(JObject("n" -> JString("1985-09-10"))) mustEqual(Success(ds))
      }

      "Boolean" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Boolean] }.validate(JObject("n" -> JBool(true))) mustEqual(Success(true))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Boolean] }.validate(JObject("n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "String" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[String] }.validate(JObject("n" -> JString("foo"))) mustEqual(Success("foo"))
        Formatting[JValue, JObject] { __ => (__ \ "o").format[String] }.validate(JObject("o.n" -> JString("foo"))) mustEqual(Failure(Seq(Path \ "o" -> Seq(ValidationError("error.required")))))
      }

      "Option" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject("n" -> JBool(true))) mustEqual(Success(Some(true)))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject()) mustEqual(Success(None))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject("foo" -> JString("bar"))) mustEqual(Success(None))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Option[Boolean]] }.validate(JObject("n" -> JString("bar"))) mustEqual(Failure(Seq(Path \ "n" -> Seq(ValidationError("error.invalid", "Boolean")))))
      }

      "Map[String, Seq[V]]" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Map[String, Seq[String]]] }.validate(JObject("n" -> JObject("foo" -> JArray(List(JString("bar")))))) mustEqual(Success(Map("foo" -> Seq("bar"))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Map[String, Seq[Int]]] }.validate(JObject("n" -> JObject("foo" -> JArray(List(JInt(4))), "bar" -> JArray(List(JInt(5)))))) mustEqual(Success(Map("foo" -> Seq(4), "bar" -> Seq(5))))
        Formatting[JValue, JObject] { __ => (__ \ "x").format[Map[String, Int]] }.validate(JObject("n" -> JObject("foo" -> JInt(4), "bar" -> JString("frack")))) mustEqual(Failure(Seq(Path \ "x" -> Seq(ValidationError("error.required")))))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Map[String, Seq[Int]]] }.validate(JObject("n" -> JObject("foo" -> JArray(List(JInt(4))), "bar" -> JArray(List(JString("frack")))))) mustEqual(Failure(Seq(Path \ "n" \ "bar" \ 0 -> Seq(ValidationError("error.number", "Int")))))
      }

      "Traversable" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Traversable[String]] }.validate(JObject("n" -> JArray(List(JString("foo"))))).get.toSeq must contain(exactly(Seq("foo"): _*))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Traversable[Int]] }.validate(JObject("n" -> JArray(List(JInt(1), JInt(2), JInt(3))))).get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Traversable[Int]] }.validate(JObject("n" -> JArray(List(JString("1"), JString("paf"))))) mustEqual(Failure(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }

      "Array" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Array[String]] }.validate(JObject("n" -> JArray(List(JString("foo"))))).get.toSeq must contain(exactly(Seq("foo"): _*))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Array[Int]] }.validate(JObject("n" -> JArray(List(JInt(1), JInt(2), JInt(3))))).get.toSeq must contain(exactly(Seq(1, 2, 3): _*))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Array[Int]] }.validate(JObject("n" -> JArray(List(JString("1"), JString("paf"))))) mustEqual(Failure(Seq(
          Path \ "n" \ 0 -> Seq(ValidationError("error.number", "Int")),
          Path \ "n" \ 1 -> Seq(ValidationError("error.number", "Int"))
        )))
      }

      "Seq" in {
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Seq[String]] }.validate(JObject("n" -> JArray(List(JString("foo"))))).get must contain(exactly(Seq("foo"): _*))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Seq[Int]] }.validate(JObject("n" -> JArray(List(JInt(1), JInt(2), JInt(3))))).get must contain(exactly(Seq(1, 2, 3): _*))
        Formatting[JValue, JObject] { __ => (__ \ "n").format[Seq[Int]] }.validate(JObject("n" -> JArray(List(JString("1"), JString("paf"))))) mustEqual(Failure(Seq(
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

			val valid = JObject(
				"firstname" -> JString("Julien"),
				"lastname" -> JString("Tournay"))

			val invalid = JObject("lastname" -> JString("Tournay"))

			val result = ("Julien", "Tournay")

      f.writes(result) mustEqual(valid)
      f.validate(valid) mustEqual(Success(result))

      f.validate(invalid) mustEqual(Failure(Seq((Path \ "firstname", Seq(ValidationError("error.required"))))))
    }

    "format seq" in {
    	import Rules._
    	import Writes._

    	val valid = JObject(
      "firstname" -> JArray(List(JString("Julien"))),
      "foobar" -> JArray(Nil),
      "lastname" -> JString("Tournay"),
      "age" -> JInt(27),
      "information" -> JObject(
      	"label" -> JString("Personal"),
      	"email" -> JString("fakecontact@gmail.com"),
      	"phones" -> JArray(List(JString("01.23.45.67.89"), JString("98.76.54.32.10")))))

      def isNotEmpty[T <: Traversable[_]] = validateWith[T]("error.notEmpty"){ !_.isEmpty }

      Formatting[JValue, JObject] { __ => (__ \ "firstname").format[Seq[String]] }.validate(valid) mustEqual(Success(Seq("Julien")))
      Formatting[JValue, JObject] { __ => (__ \ "foobar").format[Seq[String]] }.validate(valid) mustEqual(Success(Seq()))
      Formatting[JValue, JObject] { __ => (__ \ "foobar").format(isNotEmpty[Seq[Int]]) }.validate(valid) mustEqual(Failure(Seq(Path \ "foobar" -> Seq(ValidationError("error.notEmpty")))))
    }

    "format recursive" in {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser(
        "bob",
        Seq(RecUser("tom")))

      val m = JObject(
        "name" -> JString("bob"),
        "friends" -> JArray(List(JObject("name" -> JString("tom"), "friends" -> JArray(Nil)))))

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))
      val m1 = JObject(
        "name" -> JString("bob"),
        "friend" -> JObject("name" -> JString("tom")))

      "using explicit notation" in {
      	import Rules._
    		import Writes._

        lazy val w: Format[JValue, JObject, RecUser] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friends").format(seqR(w), seqW(w)))(RecUser.apply _, unlift(RecUser.unapply _))
        }
        w.validate(m) mustEqual Success(u)
        w.writes(u) mustEqual m

        lazy val w3: Format[JValue, JObject, User1] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friend").format(optionR(w3), optionW(w3)))(User1.apply _, unlift(User1.unapply _))
        }
        w3.validate(m1) mustEqual Success(u1)
        w3.writes(u1) mustEqual m1
      }

      "using implicit notation" in {
      	import Rules._
				import Writes._

        implicit lazy val w: Format[JValue, JObject, RecUser] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friends").format[Seq[RecUser]])(RecUser.apply _, unlift(RecUser.unapply _))
        }
        w.validate(m) mustEqual Success(u)
        w.writes(u) mustEqual m

        implicit lazy val w3: Format[JValue, JObject, User1] = Formatting[JValue, JObject]{ __ =>
          ((__ \ "name").format[String] ~
           (__ \ "friend").format[Option[User1]])(User1.apply _, unlift(User1.unapply _))
        }
        w3.validate(m1) mustEqual Success(u1)
        w3.writes(u1) mustEqual m1
      }
    }

		"work with Rule ans Write seamlessly" in {
			import Rules._
    	import Writes._

	    implicit val userF = Formatting[JValue, JObject] { __ =>
				((__ \ "id").format[Long] ~
			   (__ \ "name").format[String])(User.apply _, unlift(User.unapply _))
			}

			val  userJs = JObject("id" -> JInt(1L), "name" -> JString("Luigi"))
			userF.validate(userJs) mustEqual(Success(luigi))
			userF.writes(luigi) mustEqual(userJs)

			val fin = From[JObject] { __ =>
				(__ \ "user").read[User]
			}

			val m2 = JObject("user" -> userJs)
			fin.validate(m2) mustEqual(Success(luigi))

			val win = To[JValue] { __ =>
				(__ \ "user").write[User]
			}
			win.writes(luigi) mustEqual(m2)
		}

	}

}