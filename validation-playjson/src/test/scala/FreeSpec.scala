import jto.validation._
import org.scalatest._
import play.api.libs.json.{JsValue, JsObject, Json, JsString, JsNumber, JsBoolean, JsArray, JsNull}

import shapeless.test._

class FreeSpec extends WordSpec with Matchers {

  import shapeless.Poly1
  import jto.validation.free._

  trait Inter[C[_]] extends Poly1 {
    implicit def inter[T, V](implicit int: Interpreter[Is.Aux[T, V], C]) = at[Is.Aux[T, V]] { is =>
      int(is)
    }
  }

  trait FromI[I] extends Poly1 {
    implicit def ruleFrom[O](implicit r0: Rule[I, O]) = at[Rule[O, O]] { r1 =>
      r0 compose r1
    }
  }

  trait PickIn[I] extends Poly1 {
    implicit def pick[O](implicit p: Path => Rule[I, I]) = at[Rule[I, O]] { r =>
      (path: Path) => p(path) compose r
    }
  }

  import Grammar._
  import cats.free.FreeApplicative
  import FreeApplicative._

  val a1 = At(Path \ "foo", Is[String](NotEmpty))
  val a2 = At(Path \ "bar", Is[Int](Min(3)))
  val a3 = At(Path \ "int", Is[Int](Max(10)))

  val free =
    (
      lift(a1) ~
      lift(a2) ~
      lift(a3)
    )

  type R[T] = Rule[T, T]

  "Free" should {

    "types" in {
      import Match._
      import cats.Functor

      Match[Option[τ], Option[Int]]
      Match[List[τ], List[Option[Int]]]

      type EI[α] = Either[Int, α]
      Match[EI[τ], EI[Int]]
      Match[EI[τ], Either[Int, String]]

      type Foo[α] = List[Option[α]]
      Match[Foo[τ], Foo[Int]]
      // Match[Foo[τ], List[Option[Int]]]   // should compile...

      Match[List[Option[τ]], List[Option[Int]]]

      Match[List[Option[τ]], List[Option[List[Int]]]]
      Match[List[Option[List[τ]]], List[Option[List[Int]]]]

      Match[Either[τ, Int], Either[String, Int]]
      Match[Either[τ, τ], Either[String, Int]]
      Match[Either[τ, τ], Either[List[String], Int]]
      Match[Either[String, τ], Either[String, Int]]
      Match[List[Either[String, τ]], List[Either[String, Int]]]
      Match[Option[List[Either[String, τ]]], Option[List[Either[String, Int]]]]

      Match[Option[Either[List[τ], τ]], Option[Either[List[String], Int]]]

      Match[Option[Either[List[τ], τ]], Option[Either[List[String], Int]]]

      Match[Functor[τ1], Functor[List]]

      Match[FA[At, τ], FA[At, Int]]

      List(1).unify[List]
      val e: Either[String, Int] = Right(4)
      e.unify[λ[α => Either[α, Int]]]
      Option(List(1)).unify[λ[α => Option[List[α]]]]
      List(e).unify[λ[α => List[Either[α, Int]]]]

      val es: Either[List[String], Int] = Right(4)
      es.unify[λ[α => Either[List[α], Int]]]
      es.unify[λ[α => Either[α, Int]]]

      illTyped("Match[Either[Int, τ], Either[String, Int]]")
      illTyped("Match[Option[τ], List[Option[List[Int]]]]")
      illTyped("Match[List[τ], Option[List[Int]]]")
      illTyped("Match[List[List[τ]], List[Option[List[Int]]]]")
      illTyped("Match[List[List[List[τ]]], List[Option[List[Int]]]]")
    }

    "compile" in {
      import play.api.libs.json._
      import jto.validation.Path
      import Interpreters.{ R => _, _ }
      import jto.validation.playjson.Rules._
      import cats.free.FreeApplicative._
      import cats.Id

      object interpretRule extends Inter[R]
      object fromJsValue extends FromI[JsValue]
      object pickInJsValue extends PickIn[JsValue]

      object applyAt extends Poly1 {
        implicit def app[I, O] = at[FA[Id, At[Path => Rule[I, O]]]] { fa =>
          fa.map { at => at.as(at.path) }
        }
      }

      val interpreted =
        free.mapEach(interpretRule)
          .mapEach(fromJsValue)
          .mapEach(pickInJsValue)
          // .as[FA[Id, ?]]
          // .mapEach(applyAt)
    }
  }

}
