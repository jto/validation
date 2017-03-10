package jto.validation

trait DateWrites {
  def dateW(pattern: String): Write[java.util.Date, String] =
    Write[java.util.Date, String] { d =>
      new java.text.SimpleDateFormat(pattern).format(d)
    }

  implicit def dateW: Write[java.util.Date, String] =
    dateW("yyyy-MM-dd")

  def localDateW(pattern: String): Write[java.time.LocalDate, String] =
    Write[java.time.LocalDate, String] { d =>
      java.time.format.DateTimeFormatter.ofPattern(pattern).format(d)
    }

  implicit def localDateW: Write[java.time.LocalDate, String] =
    localDateW("yyyy-MM-dd")

  def zonedDateTimeW(pattern: String): Write[java.time.ZonedDateTime, String] =
    Write[java.time.ZonedDateTime, String] { d =>
      java.time.format.DateTimeFormatter.ofPattern(pattern).format(d)
    }

  implicit def zonedDateTimeW: Write[java.time.ZonedDateTime, String] =
    zonedDateTimeW("yyyy-MM-dd")

  implicit def timeW: Write[java.time.LocalDateTime, Long] =
    Write[java.time.LocalDateTime, Long](_.toInstant(java.time.ZoneOffset.UTC).toEpochMilli)

  def isoDateW: Write[java.util.Date, String] =
    Write[java.util.Date, String] { d =>
      import org.joda.time.format.ISODateTimeFormat
      val fmt = ISODateTimeFormat.dateTimeNoMillis()
      fmt.print(d.getTime)
    }

  def jodaDateW(pattern: String): Write[org.joda.time.DateTime, String] =
    Write[org.joda.time.DateTime, String] { d =>
      val fmt = org.joda.time.format.DateTimeFormat.forPattern(pattern)
      fmt.print(d)
    }

  implicit def jodaDateW: Write[org.joda.time.DateTime, String] =
    jodaDateW("yyyy-MM-dd")

  implicit def jodaTimeW: Write[org.joda.time.DateTime, Long] =
    Write[org.joda.time.DateTime, Long](_.getMillis)

  def jodaLocalDateW(pattern: String): Write[org.joda.time.LocalDate, String] =
    Write[org.joda.time.LocalDate, String] { d =>
      import org.joda.time.format.{DateTimeFormat, ISODateTimeFormat}
      val fmt =
        if (pattern == "") ISODateTimeFormat.date
        else DateTimeFormat.forPattern(pattern)
      fmt.print(d)
    }

  implicit def jodaLocalDateW: Write[org.joda.time.LocalDate, String] =
    jodaLocalDateW("")

  def sqlDateW(pattern: String): Write[java.sql.Date, String] =
    dateW(pattern).contramap(d => new java.util.Date(d.getTime))

  implicit def sqlDateW: Write[java.sql.Date, String] =
    sqlDateW("yyyy-MM-dd")
}

trait DefaultWrites extends DateWrites {
  protected def optionW[I, J, O](r: => WriteLike[I, J], empty: O)(
      implicit w: Path => WriteLike[J, O]) =
    (p: Path) =>
      Write[Option[I], O] { maybeI =>
        maybeI.map { i =>
          Write.toWrite(w(p)).contramap(r.writes).writes(i)
        }.getOrElse(empty)
    }

  implicit def seqW[I, O](implicit w: WriteLike[I, O]) =
    Write[Seq[I], Seq[O]] {
      _.map(w.writes)
    }

  implicit def headW[I, O](implicit w: WriteLike[I, O]): Write[I, Seq[O]] =
    Write.toWrite(w).map(Seq(_))

  def ignored[O](x: O) = Write[O, O](_ => x)
}

trait GenericWrites[O] {
  implicit def arrayW[I](implicit w: WriteLike[Seq[I], O]) =
    Write((_: Array[I]).toSeq) andThen w

  implicit def listW[I](implicit w: WriteLike[Seq[I], O]) =
    Write((_: List[I]).toSeq) andThen w

  implicit def traversableW[I](implicit w: WriteLike[Seq[I], O]) =
    Write((_: Traversable[I]).toSeq) andThen w

  implicit def setW[I](implicit w: WriteLike[Seq[I], O]) =
    Write((_: Set[I]).toSeq) andThen w
}

trait NumericTypes2StringWrites {
  implicit val intW: Write[Int, String] = Write(_.toString)
  implicit val shortW: Write[Short, String] = Write(_.toString)
  implicit val booleanW: Write[Boolean, String] = Write(_.toString)
  implicit val longW: Write[Long, String] = Write(_.toString)
  implicit val floatW: Write[Float, String] = Write(_.toString)
  implicit val doubleW: Write[Double, String] = Write(_.toString)
  implicit val bigDecimalW: Write[BigDecimal, String] = Write(_.toString)
}
