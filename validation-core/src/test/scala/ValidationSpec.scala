import jto.validation._

import org.scalatest._

class ValidatedSpec extends WordSpec with Matchers {

  "Validated" should {

    val success: Validated[Seq[String], Int] = Valid[Int](5)
    val failure: Validated[Seq[String], Int] =
      Invalid[Seq[String]]("err" :: Nil)

    "be a Functor" in {
      // identity
      success.map(identity) shouldBe (success)
      failure.map(identity) shouldBe (failure)
      // composition
      val p = (_: Int) + 2
      val q = (_: Int) * 3
      success.map(p compose q) shouldBe (success.map(q).map(p))
      failure.map(p compose q) shouldBe (failure.map(q).map(p))

      success.map(_ + 2) shouldBe (Valid[Int](7))
      failure.map(_ + 2) shouldBe (failure)
    }

    "be foldable" in {
      success.fold(
          err => "err",
          identity
      ) shouldBe (5)

      failure.fold(
          err => "err",
          identity
      ) shouldBe ("err")
    }

    "have an Applicative" in {
      val app = implicitly[cats.Applicative[Validated[Seq[String], ?]]]

      val u: Validated[Seq[String], Int => Int] = Valid[Int => Int](_ + 2)
      val v: Validated[Seq[String], Int => Int] = Valid[Int => Int](_ * 3)
      val w: Validated[Seq[String], Int] = Valid[Int](5)

      app.ap(app.pure((_: Int) + 2))(app.pure(5)) shouldBe (app.pure(7))

      // identity
      app.ap(app.pure[Int => Int](identity _))(success) shouldBe (success)
      app.ap(app.pure[Int => Int](identity _))(failure) shouldBe (failure)

      // composition
      val p = app.pure((f: Int => Int) => f compose (_: Int => Int))
      app.ap(app.ap(app.ap(p)(u))(v))(w) shouldBe (app.ap(u)(app.ap(v)(w)))

      // homomorphism
      val f = (_: Int) + 2
      val x = 5

      app.ap(app.pure(f))(app.pure(x)) shouldBe (app.pure(f(x)))

      // interchange
      app.ap(u)(app.pure(x)) shouldBe (app.ap(
              app.pure((f: Int => Int) => f(x)))(u))
    }

    /*
    "implement filter" in {
      success.filter((_: Int) == 5) shouldBe(success)
      failure.filter((_: Int) == 5) shouldBe(failure)
    }
     */

    "have recovery methods" in {
      success.getOrElse(42) shouldBe (5)
      failure.getOrElse(42) shouldBe (42)

      success.orElse(Valid(42)) shouldBe (success)
      failure.getOrElse(Valid(42)) shouldBe (Valid(42))
    }

    "be easily convertible to scala standars API types" in {
      success.toOption shouldBe (Some(5))
      failure.toOption shouldBe (None)

      success.toEither shouldBe (Right(5))
      failure.toEither shouldBe (Left("err" :: Nil))
    }

    "sequence" in {
      val f1: Validated[List[String], String] = Invalid(List("err1"))
      val f2: Validated[List[String], String] = Invalid(List("err2"))
      val s1: Validated[List[String], String] = Valid("1")
      val s2: Validated[List[String], String] = Valid("2")

      import cats.instances.list._
      import cats.syntax.traverse._
      type VS[X] = Validated[List[String], X]
      List(s1, s2).sequence[VS, String] shouldBe (Valid(List("1", "2")))
      List(f1, f2).sequence[VS, String] shouldBe (Invalid(List("err1", "err2")))
    }
  }
}
