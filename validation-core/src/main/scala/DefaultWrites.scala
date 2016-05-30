package jto.validation

trait DateWrites[O] { self: DefaultWrites[O] =>
  def dateW(pattern: String): Write[java.util.Date, O] =
    Write[java.util.Date, String] { d =>
      new java.text.SimpleDateFormat(pattern).format(d)
    }.andThen(stringW)

  implicit def dateW: Write[java.util.Date, O] =
    dateW("yyyy-MM-dd")

  def isoDateW: Write[java.util.Date, O] =
    Write[java.util.Date, String] { d =>
      import org.joda.time.format.ISODateTimeFormat
      val fmt = ISODateTimeFormat.dateTimeNoMillis()
      fmt.print(d.getTime)
    }.andThen(stringW)

  def jodaDateW(pattern: String): Write[org.joda.time.DateTime, O] =
    Write[org.joda.time.DateTime, String] { d =>
      val fmt = org.joda.time.format.DateTimeFormat.forPattern(pattern)
      fmt.print(d)
    }.andThen(stringW)

  implicit def jodaDateW: Write[org.joda.time.DateTime, O] =
    jodaDateW("yyyy-MM-dd")

  def jodaLocalDateW(pattern: String): Write[org.joda.time.LocalDate, O] =
    Write[org.joda.time.LocalDate, String] { d =>
      import org.joda.time.format.{DateTimeFormat, ISODateTimeFormat}
      val fmt =
        if (pattern == "") ISODateTimeFormat.date
        else DateTimeFormat.forPattern(pattern)
      fmt.print(d)
    }.andThen(stringW)

  implicit def jodaLocalDateW: Write[org.joda.time.LocalDate, O] =
    jodaLocalDateW("")

  def sqlDateW(pattern: String): Write[java.sql.Date, O] =
    dateW(pattern).contramap(d => new java.util.Date(d.getTime))

  implicit def sqlDateW: Write[java.sql.Date, O] =
    sqlDateW("yyyy-MM-dd")
}

trait DefaultWrites[O] extends DateWrites[O] {
  def stringW: Write[String, O] // Abstract member

  implicit def seqW[I](implicit w: Write[I, O]): Write[Seq[I], Seq[O]] =
    Write[Seq[I], Seq[O]] {
      _.map(w.writes)
    }

  implicit def headW[I](implicit w: Write[I, O]): Write[I, Seq[O]] =
    Write.toWrite(w).map(Seq(_))
}

trait GenericWrites[O] {
  implicit def arrayW[I](implicit w: Write[Seq[I], O]) =
    Write((_: Array[I]).toSeq) andThen w

  implicit def listW[I](implicit w: Write[Seq[I], O]) =
    Write((_: List[I]).toSeq) andThen w

  implicit def traversableW[I](implicit w: Write[Seq[I], O]) =
    Write((_: Traversable[I]).toSeq) andThen w

  implicit def setW[I](implicit w: Write[Seq[I], O]) =
    Write((_: Set[I]).toSeq) andThen w
}
