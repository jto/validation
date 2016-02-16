import jto.validation._

import org.specs2.mutable._

object ValidatedSpec extends Specification {

  "Validated" should {

    val success: Validated[Seq[String], Int] = Valid[Int](5)
    val failure: Validated[Seq[String], Int] = Invalid[Seq[String]]("err" :: Nil)

    "be a Functor" in {
      // identity
      success.map(identity) must equalTo(success)
      failure.map(identity) must equalTo(failure)
      // composition
      val p = (_: Int) + 2
      val q = (_: Int) * 3
      success.map(p compose q) must equalTo(success.map(q).map(p))
      failure.map(p compose q) must equalTo(failure.map(q).map(p))

      success.map(_ + 2) must equalTo(Valid[Int](7))
      failure.map(_ + 2) must equalTo(failure)
    }

    "be foldable" in {
      success.fold(
        err => "err",
        identity
      ) must equalTo(5)

      failure.fold(
        err => "err",
        identity
      ) must equalTo("err")
    }

    "have an Applicative" in {
      val app = implicitly[cats.Applicative[Validated[Seq[String], ?]]]

      val u: Validated[Seq[String], Int => Int] = Valid[Int => Int](_ + 2)
      val v: Validated[Seq[String], Int => Int] = Valid[Int => Int](_ * 3)
      val w: Validated[Seq[String], Int] = Valid[Int](5)

      app.ap(app.pure((_: Int) + 2))(app.pure(5)) must equalTo(app.pure(7))

      // identity
      app.ap(app.pure[Int => Int](identity _))(success) must equalTo(success)
      app.ap(app.pure[Int => Int](identity _))(failure) must equalTo(failure)

      // composition
      val p = app.pure((f: Int => Int) => f compose (_: Int => Int))
      app.ap(app.ap(app.ap(p)(u))(v))(w) must equalTo(
        app.ap(u)(app.ap(v)(w)))

      // homomorphism
      val f = (_: Int) + 2
      val x = 5
      app.ap(app.pure(f))(app.pure(x)) must equalTo(app.pure(f(x)))

      // interchange
      app.ap(u)(app.pure(x)) must equalTo(
        app.ap(app.pure((f: Int => Int) => f(x)))(u))
    }

    /*
    "implement filter" in {
      success.filter((_: Int) == 5) must equalTo(success)
      failure.filter((_: Int) == 5) must equalTo(failure)
    }
    */

    "have recovery methods" in {
      success.getOrElse(42) must equalTo(5)
      failure.getOrElse(42) must equalTo(42)

      success.orElse(Valid(42)) must equalTo(success)
      failure.getOrElse(Valid(42)) must equalTo(Valid(42))
    }

    "be easily convertible to scala standars API types" in {
      success.toOption must equalTo(Some(5))
      failure.toOption must equalTo(None)

      success.toEither must equalTo(Right(5))
      failure.toEither must equalTo(Left("err" :: Nil))
    }

    "sequence" in {
      val f1: Validated[List[String], String] = Invalid(List("err1"))
      val f2: Validated[List[String], String] = Invalid(List("err2"))
      val s1: Validated[List[String], String] = Valid("1")
      val s2: Validated[List[String], String] = Valid("2")

      import cats.std.list._
      import cats.syntax.traverse._
      List(s1, s2).sequenceU must equalTo(Valid(List("1", "2")))
      List(f1, f2).sequenceU must equalTo(Invalid(List("err1", "err2")))
    }

  }
}
