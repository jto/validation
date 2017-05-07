package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._
import org.scalatest._
import cats.syntax.compose._

class CrossCompile extends WordSpec with Matchers {

  implicit val rg = RulesGrammar
  implicit val wg = WritesGrammar

  import types._

  "grammar" should {

    "compile to symetric rule and write" in {
      def info[K[_, _]](implicit g: Grammar[JsValue, K]) = {
        import g._
        at(Path \ "label")(is[String] andThen notEmpty) ~:
        opt(Path \ "email")(is[String] andThen email) ~:
        at(Path \ "phones")(is[Seq[String]] andThen forall(notEmpty)) ~:
        knil
      }

      case class Info(label: String, email: Option[String], phones: Seq[String])
      val ex = Info("label", Option("fakecontact@gmail.com"), Seq("phone1", "phone2"))

      val write = info[flip[Write]#λ].from[Info]
      val rule = info[Rule].to[Info]
      val sym = (rule.validate _) compose (write.writes _)
      sym(ex) shouldBe Valid(ex)
    }

    "compose" in {
      case class Id(value: String)
      implicit val idW: Write[Id, String] = Write(id => id.value)
      implicit val idR: Rule[String, Id] = Rule.zero[String].map(Id.apply)

      def id[K[_, _]](implicit g: Grammar[JsValue, K], valid: K[String, Id]) = {
        import g._
        at(Path \ "id")(is[String] andThen valid)
      }

      val ex = Id("value")

      val write = id[flip[Write]#λ]
      val rule = id[Rule]
      val sym = (rule.validate _) compose (write.writes _)
      sym(ex) shouldBe Valid(ex)

    }

    "merge Ks" in {
      import cats.syntax.semigroup._
      val p = Path \ "percent"
      def percent[K[_, _]](implicit g: Grammar[JsValue, K]) = {
        import g._
        at(p)(is[Int] andThen min(0) |+| max(100))
      }

      val write = percent[flip[Write]#λ]
      val rule = percent[Rule]
      val sym = (rule.validate _) compose (write.writes _)

      sym(10) shouldBe Valid(10)
      sym(-10) shouldBe Invalid(Seq((p) -> Seq(ValidationError("error.min", 0))))
      sym(200) shouldBe Invalid(Seq((p) -> Seq(ValidationError("error.max", 100))))
    }
  }

}