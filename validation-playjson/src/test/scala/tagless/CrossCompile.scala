package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._
import org.scalatest._
import cats.syntax.compose._

class CrossCompile extends WordSpec with Matchers {

  "grammar" should {

    "compile to symetric rule and write" in {
      def info[K[_, _]](g: Grammar[JsValue, K]) = {
        import g._
        at(Path \ "label")(is[String] andThen notEmpty) ~:
        opt(Path \ "email")(is[String] andThen email) ~:
        at(Path \ "phones")(is[Seq[String]] andThen forall(notEmpty)) ~:
        knil
      }

      case class Info(label: String, email: Option[String], phones: Seq[String])
      val ex = Info("label", Option("fakecontact@gmail.com"), Seq("phone1", "phone2"))

      val write = info[types.flip[Write]#λ](WritesGrammar).from[Info]
      val rule = info(RulesGrammar).to[Info]
      val sym = (rule.validate _) compose (write.writes _)
      sym(ex) shouldBe Valid(ex)
    }

    "compose" in {
      case class Id(value: String)
      implicit val idW: Write[Id, String] = Write(id => id.value)
      implicit val idR: Rule[String, Id] = Rule.zero[String].map(Id.apply)

      def id[K[_, _]](g: Grammar[JsValue, K])(implicit valid: K[String, Id]) = {
        import g._
        at(Path \ "id")(is[String] andThen valid)
      }

      val ex = Id("value")

      val write = id[types.flip[Write]#λ](WritesGrammar)
      val rule = id(RulesGrammar)
      val sym = (rule.validate _) compose (write.writes _)
      sym(ex) shouldBe Valid(ex)

    }
  }

}