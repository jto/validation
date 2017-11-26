import jto.validation._
import org.scalatest._

class DateSpec extends WordSpec with Matchers {
  "forms" should {
    import jto.validation.forms._
    import Rules._, Writes._

    "Read" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(dateR)
        }.validate(Map("n" -> Seq("1985-09-10"))) shouldBe
          (Valid(f.parse("1985-09-10")))

        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(dateR)
        }.validate(Map("n" -> Seq("foo"))) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date (Can't test on CI)" ignore {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(isoDateR)
        }.validate(Map("n" -> Seq("1985-09-10T00:00:00+02:00"))) shouldBe
          (Valid(f.parse("1985-09-10")))

        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(isoDateR)
        }.validate(Map("n" -> Seq("foo"))) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.date.isoformat")))))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaDateR)
          }.validate(Map("n" -> Seq("1985-09-10"))) shouldBe (Valid(jd))

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaDateR)
          }.validate(Map("n" -> Seq("foo"))) shouldBe
            (Invalid(
              Seq(
                Path \ "n" -> Seq(
                  ValidationError("error.expected.jodadate.format",
                                  "yyyy-MM-dd")))))
        }

        "time" in {
          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaTimeR)
          }.validate(Map("n" -> Seq(dd.getTime.toString))) shouldBe (Valid(jd))

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaDateR)
          }.validate(Map("n" -> Seq("foo"))) shouldBe
            (Invalid(
              Seq(
                Path \ "n" -> Seq(
                  ValidationError("error.expected.jodadate.format",
                                  "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaLocalDateR)
          }.validate(Map("n" -> Seq(ld.toString()))) shouldBe (Valid(ld))

          From[UrlFormEncoded] { __ =>
            (__ \ "n").read(jodaLocalDateR)
          }.validate(Map("n" -> Seq("foo"))) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())

        From[UrlFormEncoded] { __ =>
          (__ \ "n").read(sqlDateR)
        }.validate(Map("n" -> Seq("1985-09-10"))) shouldBe (Valid(ds))
      }
    }

    "Write" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write(dateW)
        }.writes(d) shouldBe (Map("n" -> Seq("1985-09-10")))
      }

      "iso date (Can't test on CI)" ignore {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write(isoDateW)
        }.writes(d) shouldBe (Map("n" -> Seq("1985-09-10T00:00:00+02:00")))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          To[UrlFormEncoded] { __ =>
            (__ \ "n").write(jodaDateW)
          }.writes(jd) shouldBe (Map("n" -> Seq("1985-09-10")))
        }

        "time" in {
          To[UrlFormEncoded] { __ =>
            (__ \ "n").write(jodaTimeW)
          }.writes(jd) shouldBe (Map("n" -> Seq(dd.getTime.toString)))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          To[UrlFormEncoded] { __ =>
            (__ \ "n").write(jodaLocalDateW)
          }.writes(ld) shouldBe (Map("n" -> Seq(ld.toString)))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        To[UrlFormEncoded] { __ =>
          (__ \ "n").write(sqlDateW)
        }.writes(ds) shouldBe (Map("n" -> Seq("1985-09-10")))
      }
    }
  }

  "json" should {
    import jto.validation.playjson._
    import play.api.libs.json.{JsValue, Json}
    import Rules._, Writes._

    "Read" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n")
          .from[JsValue](dateR)
          .validate(Json.obj("n" -> "1985-09-10")) shouldBe
          (Valid(f.parse("1985-09-10")))
        (Path \ "n")
          .from[JsValue](dateR)
          .validate(Json.obj("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date (Can't test on CI)" ignore {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n")
          .from[JsValue](isoDateR)
          .validate(Json.obj("n" -> "1985-09-10T00:00:00+02:00")) shouldBe
          (Valid(f.parse("1985-09-10")))
        (Path \ "n")
          .from[JsValue](isoDateR)
          .validate(Json.obj("n" -> "foo")) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.date.isoformat")))))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n")
            .from[JsValue](jodaDateR)
            .validate(Json.obj("n" -> "1985-09-10")) shouldBe (Valid(jd))
          (Path \ "n")
            .from[JsValue](jodaDateR)
            .validate(Json.obj("n" -> "foo")) shouldBe
            (Invalid(
              Seq(
                Path \ "n" -> Seq(
                  ValidationError("error.expected.jodadate.format",
                                  "yyyy-MM-dd")))))
        }

        "time" in {
          (Path \ "n")
            .from[JsValue](jodaTimeR)
            .validate(Json.obj("n" -> dd.getTime)) shouldBe (Valid(jd))
          (Path \ "n")
            .from[JsValue](jodaDateR)
            .validate(Json.obj("n" -> "foo")) shouldBe
            (Invalid(
              Seq(
                Path \ "n" -> Seq(
                  ValidationError("error.expected.jodadate.format",
                                  "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n")
            .from[JsValue](jodaLocalDateR)
            .validate(Json.obj("n" -> ld.toString())) shouldBe (Valid(ld))
          (Path \ "n")
            .from[JsValue](jodaLocalDateR)
            .validate(Json.obj("n" -> "foo")) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n")
          .from[JsValue](sqlDateR)
          .validate(Json.obj("n" -> "1985-09-10")) shouldBe (Valid(ds))
      }
    }

    "Write" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        (Path \ "n").write(dateW).writes(d) shouldBe
          (Json.obj("n" -> "1985-09-10"))
      }

      "iso date (Can't test on CI)" ignore {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        (Path \ "n").write(isoDateW).writes(d) shouldBe
          (Json.obj("n" -> "1985-09-10T00:00:00+02:00"))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n").write(jodaDateW).writes(jd) shouldBe
            (Json.obj("n" -> "1985-09-10"))
        }

        "time" in {
          (Path \ "n").write(jodaTimeW).writes(jd) shouldBe
            (Json.obj("n" -> dd.getTime))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n").write(jodaLocalDateW).writes(ld) shouldBe
            (Json.obj("n" -> ld.toString))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n").write(sqlDateW).writes(ds) shouldBe
          (Json.obj("n" -> "1985-09-10"))
      }
    }
  }

  "jsonAst" should {
    import jto.validation.jsonast._
    import Rules._, Writes._

    "Read" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n")
          .from[JValue](dateR)
          .validate(JObject(Map("n" -> JString("1985-09-10")))) shouldBe
          (Valid(f.parse("1985-09-10")))
        (Path \ "n")
          .from[JValue](dateR)
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "iso date (Can't test on CI)" ignore {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        (Path \ "n")
          .from[JValue](isoDateR)
          .validate(JObject(Map("n" -> JString("1985-09-10T00:00:00+02:00")))) shouldBe
          (Valid(f.parse("1985-09-10")))
        (Path \ "n")
          .from[JValue](isoDateR)
          .validate(JObject(Map("n" -> JString("foo")))) shouldBe
          (Invalid(
            Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.date.isoformat")))))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n")
            .from[JValue](jodaDateR)
            .validate(JObject(Map("n" -> JString("1985-09-10")))) shouldBe
            (Valid(jd))
          (Path \ "n")
            .from[JValue](jodaDateR)
            .validate(JObject(Map("n" -> JString("foo")))) shouldBe
            (Invalid(
              Seq(
                Path \ "n" -> Seq(
                  ValidationError("error.expected.jodadate.format",
                                  "yyyy-MM-dd")))))
        }

        "time" in {
          (Path \ "n")
            .from[JValue](jodaTimeR)
            .validate(JObject(Map("n" -> JNumber(dd.getTime)))) shouldBe
            (Valid(jd))
          (Path \ "n")
            .from[JValue](jodaDateR)
            .validate(JObject(Map("n" -> JString("foo")))) shouldBe
            (Invalid(
              Seq(
                Path \ "n" -> Seq(
                  ValidationError("error.expected.jodadate.format",
                                  "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n")
            .from[JValue](jodaLocalDateR)
            .validate(JObject(Map("n" -> JString(ld.toString())))) shouldBe
            (Valid(ld))
          (Path \ "n")
            .from[JValue](jodaLocalDateR)
            .validate(JObject(Map("n" -> JString("foo")))) shouldBe
            (Invalid(Seq(Path \ "n" -> Seq(
              ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n")
          .from[JValue](sqlDateR)
          .validate(JObject(Map("n" -> JString("1985-09-10")))) shouldBe
          (Valid(ds))
      }
    }

    "Write" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        (Path \ "n").write(dateW).writes(d) shouldBe
          (JObject(Map("n" -> JString("1985-09-10"))))
      }

      "iso date (Can't test on CI)" ignore {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        (Path \ "n").write(isoDateW).writes(d) shouldBe
          (JObject(Map("n" -> JString("1985-09-10T00:00:00+02:00"))))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          (Path \ "n").write(jodaDateW).writes(jd) shouldBe
            (JObject(Map("n" -> JString("1985-09-10"))))
        }

        "time" in {
          (Path \ "n").write(jodaTimeW).writes(jd) shouldBe
            (JObject(Map("n" -> JNumber(dd.getTime))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          (Path \ "n").write(jodaLocalDateW).writes(ld) shouldBe
            (JObject(Map("n" -> JString(ld.toString))))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        (Path \ "n").write(sqlDateW).writes(ds) shouldBe
          (JObject(Map("n" -> JString("1985-09-10"))))
      }
    }
  }

  "xml" should {
    import jto.validation.xml._
    import scala.xml.Node
    import Rules._, Writes._

    "Read" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        Path.from[Node](dateR).validate(<a>1985-09-10</a>) shouldBe
          (Valid(f.parse("1985-09-10")))
        Path.from[Node](dateR).validate(<a>foo</a>) shouldBe
          (Invalid(Seq(
            Path -> Seq(ValidationError("error.expected.date", "yyyy-MM-dd")))))
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          Path.from[Node](jodaDateR).validate(<a>1985-09-10</a>) shouldBe
            (Valid(jd))
          Path.from[Node](jodaDateR).validate(<a>foo</a>) shouldBe
            (Invalid(
              Seq(Path -> Seq(ValidationError("error.expected.jodadate.format",
                                              "yyyy-MM-dd")))))
        }

        "time" in {
          Path.from[Node](jodaTimeR).validate(<a>{dd.getTime}</a>) shouldBe
            (Valid(jd))
          Path.from[Node](jodaDateR).validate(<a>foo</a>) shouldBe
            (Invalid(
              Seq(Path -> Seq(ValidationError("error.expected.jodadate.format",
                                              "yyyy-MM-dd")))))
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          Path
            .from[Node](jodaLocalDateR)
            .validate(<a>{ld.toString}</a>) shouldBe
            (Valid(ld))
          Path.from[Node](jodaLocalDateR).validate(<a>foo</a>) shouldBe
            (Invalid(Seq(Path -> Seq(
              ValidationError("error.expected.jodadate.format", "")))))
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        Path.from[Node](sqlDateR).validate(<a>1985-09-10</a>) shouldBe
          (Valid(ds))
      }
    }

    "Write" when {
      "date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val d = f.parse("1985-09-10")
        Path.write(dateW).writes(d)(<a></a>) shouldBe (<a>1985-09-10</a>)
      }

      "joda" when {
        import org.joda.time.DateTime
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val jd = new DateTime(dd)

        "date" in {
          Path.write(jodaDateW).writes(jd)(<a></a>) shouldBe
            (<a>1985-09-10</a>)
        }

        "local date" in {
          import org.joda.time.LocalDate
          val ld = new LocalDate()
          Path.write(jodaLocalDateW).writes(ld)(<a></a>) shouldBe
            (<a>{ld.toString}</a>)
        }
      }

      "sql date" in {
        val f =
          new java.text.SimpleDateFormat("yyyy-MM-dd", java.util.Locale.FRANCE)
        val dd = f.parse("1985-09-10")
        val ds = new java.sql.Date(dd.getTime())
        Path.write(sqlDateW).writes(ds)(<a></a>) shouldBe (<a>1985-09-10</a>)
      }
    }
  }
}
