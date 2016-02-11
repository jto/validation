package jto.validation

/**
  * Backcompat with 1.x. All the methods are deprecated
  */

object Success {
  @deprecated("Success is deprecated, cats.data.Validated.Valid", "2.0")
  def apply[A](a: A) = cats.data.Validated.valid(a)
}

object Failure {
  @deprecated("Failure is deprecated, cats.data.Validated.Invalid", "2.0")
  def apply[A](ves: Seq[ValidationError]) = cats.data.Validated.invalid(ves)
}

trait VABackCompat[E, A] {
  import cats.data.Validated.{ valid, invalid }

  val v: Validated[Seq[E], A]

  @deprecated("isSuccess is deprecated, use isValid", "2.0")
  def isSuccess = v.isValid

  @deprecated("isFailure is deprecated, use isInvalid", "2.0")
  def isFailure = v.isInvalid

  @deprecated("viaEither is deprecated", "2.0")
  def viaEither[EE, AA](f: Either[Seq[E], A] => Either[Seq[EE], AA]): Validated[Seq[EE], AA] =
    f(v.toEither).fold(invalid, valid)

  @deprecated("filterNot is deprecated", "2.0")
  def filterNot[EE >: E](error: EE)(p: A => Boolean): Validated[Seq[EE], A] =
    viaEither { _.right.flatMap { a => if (p(a)) Left(Seq(error)) else Right(a) } }

  @deprecated("filterNot is deprecated", "2.0")
  def filterNot(p: A => Boolean): Validated[Seq[E], A] =
    viaEither { _.right.flatMap { a => if (p(a)) Left(Nil) else Right(a) } }

  @deprecated("filter is deprecated", "2.0")
  def filter(p: A => Boolean): Validated[Seq[E], A] =
    viaEither { _.right.flatMap { a => if (p(a)) Right(a) else Left(Nil) } }

  @deprecated("filter is deprecated", "2.0")
  def filter[EE >: E](otherwise: EE)(p: A => Boolean): Validated[Seq[EE], A] =
    viaEither { _.right.flatMap { a => if (p(a)) Right(a) else Left(Seq(otherwise)) } }

  @deprecated("collect is deprecated", "2.0")
  def collect[EE >: E, B](otherwise: EE)(p: PartialFunction[A, B]): Validated[Seq[EE], B] = viaEither {
    _.right.flatMap {
      case t if p.isDefinedAt(t) => Right(p(t))
      case _ => Left(Seq(otherwise))
    }
  }

  @deprecated("withFilter is deprecated", "2.0")
  def withFilter(p: A => Boolean) = new WithFilter(p)

  final class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Validated[Seq[E], B] = v match {
      case Valid(a) =>
        if (p(a)) Valid(f(a))
        else Invalid(Nil)
      case Invalid(errs) => Invalid(errs)
    }
    def flatMap[EE >: E, B](f: A => Validated[Seq[EE], B]): Validated[Seq[EE], B] = v match {
      case Valid(a) =>
        if (p(a)) f(a)
        else Invalid(Nil)
      case Invalid(errs) => Invalid(errs)
    }
    def foreach(f: A => Unit): Unit = v match {
      case Valid(a) if p(a) => f(a)
      case _ => ()
    }
    def withFilter(q: A => Boolean) = new WithFilter(a => p(a) && q(a))
  }

  @deprecated("get is deprecated, use toOption.get", "2.0")
  def get = v.toOption.get

  @deprecated("asOpt is deprecated, use toOption", "2.0")
  def asOpt = v.toOption

  @deprecated("asEither is deprecated, use toEither", "2.0")
  def asEither = v.toEither

  @deprecated("recover is deprecated, use toXor.recover(errManager).toValidated", "2.0")
  def recover[AA >: A](errManager: PartialFunction[Invalid[Seq[E]], AA]): Validated[Seq[E], AA] =
    v.toXor.recover{ case err => errManager(Invalid(err)) }.toValidated

  @deprecated("recoverTotal is deprecated, use fold", "2.0")
  def recoverTotal[AA >: A](errManager: Invalid[Seq[E]] => AA): AA =
    recover{ case err => errManager(err) }.toOption.get

  @deprecated("fail.map is deprecated, use leftMap", "2.0")
  def fail = FailProjection(v)

  @deprecated("success.map is deprecated, use map", "2.0")
  def success = v
}

final case class FailProjection[+E, +A](v: Validated[Seq[E], A]) {
  def map[F](f: Seq[E] => Seq[F]): Validated[Seq[F], A] = v match {
    case Valid(v) => Valid(v)
    case Invalid(e) => Invalid(f(e))
  }
}

object Validation {
  @deprecated("sequence is deprecated, use .toList.sequenceU", "2.0")
  def sequence[E, A](vs: Seq[Validated[Seq[E], A]]): Validated[Seq[E], Seq[A]] = {
    import cats.std.list._; import cats.syntax.traverse._;
    vs.toList.sequenceU
  }

}