package jto.validation

/**
  * This trait provides default Rule implementations,
  * from String to various date types and format
  */
trait DateRules {

  /**
    * Rule for the `java.util.Date` type.
    *
    * @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
    * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks
    */
  def dateR(format: String, corrector: String => String = identity) =
    Rule.fromMapping[String, java.util.Date] { s =>
      def parseDate(input: String): Option[java.util.Date] = {
        // REMEMBER THAT SIMPLEDATEFORMAT IS NOT THREADSAFE
        val df = new java.text.SimpleDateFormat(format)
        df.setLenient(false)
        try { Some(df.parse(input)) } catch {
          case _: java.text.ParseException => None
        }
      }

      parseDate(corrector(s)) match {
        case Some(d) => Valid(d)
        case None =>
          Invalid(Seq(ValidationError("error.expected.date", format)))
      }
    }

  /**
    * Default Rule for the `java.util.Date` type.
    * It uses the default date format: `yyyy-MM-dd`
    */
  implicit def dateR: Rule[String, java.util.Date] =
    dateR("yyyy-MM-dd")

  /**
    * Rule for the `org.joda.time.DateTime` type.
    *
    * @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
    * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks
    */
  def jodaDateR(pattern: String,
                corrector: String => String =
                  identity): Rule[String, org.joda.time.DateTime] =
    Rule.fromMapping[String, org.joda.time.DateTime] { s =>
      import scala.util.Try

      val df = org.joda.time.format.DateTimeFormat.forPattern(pattern)
      Try(df.parseDateTime(corrector(s)))
        .map(Valid.apply)
        .getOrElse(Invalid(Seq(ValidationError(
                        "error.expected.jodadate.format", pattern))))
    }

  /**
    * Default Rule for the `java.util.DateTime` type.
    */
  implicit def jodaTimeR: Rule[Long, org.joda.time.DateTime] =
    Rule.fromMapping[Long, org.joda.time.DateTime] { d =>
      Valid(new org.joda.time.DateTime(d.toLong))
    }

  /**
    * the default implicit JodaDate reads
    * It uses the default date format: `yyyy-MM-dd`
    */
  implicit def jodaDateR: Rule[String, org.joda.time.DateTime] =
    jodaDateR("yyyy-MM-dd")

  implicit def jodaLocalDateR(
      pattern: String,
      corrector: String => String =
        identity): Rule[String, org.joda.time.LocalDate] =
    Rule.fromMapping[String, org.joda.time.LocalDate] { s =>
      import scala.util.Try
      import org.joda.time.format.{DateTimeFormat, ISODateTimeFormat}

      val df =
        if (pattern == "") ISODateTimeFormat.localDateParser
        else DateTimeFormat.forPattern(pattern)
      Try(org.joda.time.LocalDate.parse(corrector(s), df))
        .map(Valid.apply)
        .getOrElse(Invalid(Seq(ValidationError(
                        "error.expected.jodadate.format", pattern))))
    }

  /**
    * The default implicit Rule for `org.joda.time.LocalDate`
    */
  implicit def jodaLocalDateR: Rule[String, org.joda.time.LocalDate] =
    jodaLocalDateR("")

  /**
    * ISO 8601 Reads
    */
  def isoDateR: Rule[String, java.util.Date] =
    Rule.fromMapping[String, java.util.Date] { s =>
      import scala.util.Try
      import org.joda.time.format.ISODateTimeFormat
      val parser = ISODateTimeFormat.dateOptionalTimeParser()
      Try(parser.parseDateTime(s).toDate())
        .map(Valid.apply)
        .getOrElse(Invalid(
                Seq(ValidationError("error.expected.date.isoformat"))))
    }

  /**
    * Rule for the `java.sql.Date` type.
    *
    * @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
    * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks
    */
  def sqlDateR(
      pattern: String,
      corrector: String => String = identity): Rule[String, java.sql.Date] =
    dateR(pattern, corrector).map((d: java.util.Date) =>
          new java.sql.Date(d.getTime))

  /**
    * The default implicit Rule for `java.sql.Date`
    */
  implicit def sqlDateR: Rule[String, java.sql.Date] =
    sqlDateR("yyyy-MM-dd")
}

/**
  * GenericRules provides basic constraints, utility methods on Rules, and completely generic Rules.
  * Extends this trait if your implementing a new set of Rules.
  */
trait GenericRules[II] {
  def stringR: Rule[II, String] // Abstract member

  /**
    * Create a new constraint, verifying that the provided predicate is satisfied.
    * {{{
    *   def notEmpty = validateWith[String]("validation.nonemptytext"){ !_.isEmpty }
    * }}}
    * @param msg The error message to return if predicate `pred` is not satisfied
    * @param args Arguments for the `ValidationError`
    * @param pred A predicate to satify
    * @return A new Rule validating data of type `I` against a predicate `p`
    */
  def validateWith[A](msg: String, args: Any*)(
      pred: A => Boolean)(implicit r: Rule[II, A]): Rule[II, A] =
    r.andThen(Rule.fromMapping[A, A] { v =>
      if (!pred(v)) Invalid(Seq(ValidationError(msg, args: _*))) else Valid(v)
    })

  /**
    * lift a `Rule[I, O]` to a Rule of `Rule[Seq[I], Array[O]]`
    * {{{
    *   (Path \ "foo").read(array(notEmpty)) // create a Rules validating that an Array contains non-empty Strings
    * }}}
    * @param r A Rule[I, O] to lift
    * @return A new Rule
    */
  implicit def arrayR[I, O: scala.reflect.ClassTag](
      implicit r: Rule[I, O]): Rule[Seq[I], Array[O]] =
    seqR[I, O](r).map(_.toArray)

  /**
    * lift a `Rule[I, O]` to a Rule of `Rule[Seq[I], Traversable[O]]`
    * {{{
    *   (Path \ "foo").read(traversable(notEmpty)) // create a Rules validating that an Traversable contains non-empty Strings
    * }}}
    * @param r A Rule[I, O] to lift
    * @return A new Rule
    */
  implicit def traversableR[I, O](
      implicit r: Rule[I, O]): Rule[Seq[I], Traversable[O]] =
    seqR[I, O](r).map(_.toTraversable)

  /**
    * lift a `Rule[I, O]` to a Rule of `Rule[Seq[I], Set[O]]`
    * {{{
    *   (Path \ "foo").read(set(notEmpty)) // create a Rules validating that a Set contains non-empty Strings
    * }}}
    * @param r A Rule[I, O] to lift
    * @return A new Rule
    */
  implicit def setR[I, O](implicit r: Rule[I, O]): Rule[Seq[I], Set[O]] =
    seqR[I, O](r).map(_.toSet)

  /**
    * lift a `Rule[I, O]` to a Rule of `Rule[Seq[I], Seq[O]]`
    * {{{
    *   (Path \ "foo").read(seq(notEmpty)) // create a Rules validating that an Seq contains non-empty Strings
    * }}}
    * @param r A Rule[I, O] to lift
    * @return A new Rule
    */
  implicit def seqR[I, O](implicit r: Rule[I, O]): Rule[Seq[I], Seq[O]] =
    Rule { is =>
      val withI = is.zipWithIndex.map {
        case (v, i) =>
          Rule.toRule(r).repath((Path \ i) ++ _).validate(v)
      }
      import cats.std.list._
      import cats.syntax.traverse._
      withI.toList.sequenceU
    }

  /**
    * lift a `Rule[I, O]` to a Rule of `Rule[Seq[I], List[O]]`
    * {{{
    *   (Path \ "foo").read(list(notEmpty)) // create a Rules validating that an List contains non-empty Strings
    * }}}
    * @param r A Rule[I, O] to lift
    * @return A new Rule
    */
  implicit def listR[I, O](implicit r: Rule[I, O]): Rule[Seq[I], List[O]] =
    seqR[I, O](r).map(_.toList)

  /**
    * Create a Rule validation that a Seq[I] is not empty, and attempt to convert it's first element as a `O`
    * {{{
    *   (Path \ "foo").read(headAs(int))
    * }}}
    */
  implicit def headAs[I, O](implicit c: Rule[I, O]) =
    Rule
      .fromMapping[Seq[I], I] {
        _.headOption
          .map(Valid[I](_))
          .getOrElse(Invalid[Seq[ValidationError]](
                  Seq(ValidationError("error.required"))))
      }
      .andThen(c)

  def not[I, O](r: Rule[I, O]) = Rule[I, I] { d =>
    r.validate(d) match {
      case Valid(_) => Invalid(Nil)
      case Invalid(_) => Valid(d)
    }
  }

  /**
    * Create a "constant" Rule which is always a success returning value `o`
    * (Path \ "x").read(ignored(42))
    */
  def ignored[I, O](o: O) = (_: Path) => Rule[I, O](_ => Valid(o))

  /**
    * Create a Rule of equality
    * {{{
    *   (Path \ "foo").read(equalTo("bar"))
    * }}}
    */
  def equalTo[A](t: A)(implicit r: Rule[II, A]): Rule[II, A] =
    validateWith[A]("error.equals", t)(_.equals(t))

  /**
    * a Rule validating that a String is not empty.
    * @note This Rule does '''NOT''' trim the String beforehand
    * {{{
    *   (Path \ "foo").read(notEmpty)
    * }}}
    */
  val notEmpty: Rule[II, String] =
    validateWith[String]("error.required")(!_.isEmpty)(stringR)

  /**
    * {{{
    *   (Path \ "foo").read(min(0)) // validate that there's a positive int at (Path \ "foo")
    * }}}
    */
  def min[T](m: T)(implicit o: Ordering[T], r: Rule[II, T]): Rule[II, T] =
    validateWith[T]("error.min", m)(x => o.gteq(x, m))

  /**
    * {{{
    *   (Path \ "foo").read(max(0)) // validate that there's a negative int at (Path \ "foo")
    * }}}
    */
  def max[T](m: T)(implicit o: Ordering[T], r: Rule[II, T]): Rule[II, T] =
    validateWith[T]("error.max", m)(x => o.lteq(x, m))

  /**
    * {{{
    *   (Path \ "foo").read(minLength(5)) // The length of this String must be >= 5
    * }}}
    */
  def minLength(l: Int): Rule[II, String] =
    validateWith[String]("error.minLength", l)(_.size >= l)(stringR)

  /**
    * {{{
    *   (Path \ "foo").read(maxLength(5)) // The length of this String must be <= 5
    * }}}
    */
  def maxLength(l: Int): Rule[II, String] =
    validateWith[String]("error.maxLength", l)(_.size <= l)(stringR)

  /**
    * Validate that a String matches the provided regex
    * {{{
    *   (Path \ "foo").read(pattern("[a-z]".r)) // This String contains only letters
    * }}}
    */
  def pattern(regex: scala.util.matching.Regex): Rule[II, String] =
    validateWith("error.pattern", regex)(
        regex.unapplySeq(_: String).isDefined)(stringR)

  /**
    * Validate that a String is a valid email
    * {{{
    *   (Path \ "email").read(email) // This String is an email
    * }}}
    */
  def email: Rule[II, String] =
    Rule.fromMapping[II, String](
        pattern(
            """\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r)
          .validate(_)
          .bimap(_ => Seq(ValidationError("error.email")), identity)
    )

  /**
    * A Rule that always succeed
    */
  def noConstraint[F]: Constraint[F] = Valid(_)

  /**
    * A Rule for HTML checkboxes
    */
  def checked(implicit b: Rule[II, Boolean]): Rule[II, Boolean] =
    equalTo(true)(b)
}

/**
  * DefaultRules provides basic rules implementations for inputs of type `I`
  * Extends this trait if your implementing a new set of Rules for `I`.
  */
trait DefaultRules[I] extends GenericRules[I] with DateRules {
  protected def opt[J, O](r: Rule[J, O], noneValues: Rule[I, I]*)(
      implicit pick: Path => Rule[I, I], coerce: Rule[I, J]) =
    (path: Path) =>
      Rule[I, Option[O]] { (d: I) =>
        val isNone =
          not(noneValues.foldLeft(Rule.zero[I])(_ andThen not(_))).map(_ =>
                None)
        val v = (pick(path).validate(d).map(Some.apply) orElse Valid(None))
        Validated.fromEither(
            v.toEither.right.flatMap {
              case None => Right(None)
              case Some(i) =>
                isNone
                  .orElse(Rule
                        .toRule(coerce)
                        .andThen(r)
                        .map[Option[O]](Some.apply))
                  .validate(i)
                  .toEither
            }
        )
    }

  def mapR[K, O](
      r: Rule[K, O], p: Rule[I, Seq[(String, K)]]): Rule[I, Map[String, O]] = {
    Rule
      .toRule(p)
      .andThen(Path)(
          Rule { fs =>
        val validations = fs.map { f =>
          Rule
            .toRule(r)
            .repath((Path \ f._1) ++ _)
            .validate(f._2)
            .map(f._1 -> _)
        }
        import cats.std.list._
        import cats.syntax.traverse._
        validations.toList.sequenceU.map(_.toMap)
      })
  }
}
