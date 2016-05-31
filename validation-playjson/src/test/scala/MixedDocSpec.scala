import jto.validation._
import jto.validation.playjson._
import org.scalatest._
import play.api.libs.json.{Json, JsValue}
import cats.Functor

case class TypeOne(param1: Int, param2: String)

class MixedDocSpec extends WordSpec with Matchers {
  "Doc" should {
    "Be builddable along with a Rule" in {
      import Rules._
      import MixedDoc._
      
      val docRuleTypeOne: Doc[TypeOne] with Rule[JsValue, TypeOne] =
        Build[Doc, Rule[JsValue, ?], TypeOne] { __ =>
          (
            (__ \ "param1").as[Int] ~
            (__ \ "param2").as[String]
          )(TypeOne.apply)
        }

      assert(
        docRuleTypeOne.jsonSchema == Json.obj(
          "title" -> "TypeOne",
          "type" -> "object",
          "properties" -> Json.obj(
            "param1" -> Json.obj(
              "type" -> "number",
              "format" -> "number"),
            "param2" -> Json.obj(
              "type" -> "string"))))
      
      assert(docRuleTypeOne.validate(Json.obj("param1" -> 1, "param2" -> "S")) == Valid(TypeOne(1, "S")))
    }
    
    "Be builddable generically" in {
      import Rules._
      
      def buildTypeOne[F[_]]
        (implicit 
          f1: F[Int],
          f2: F[String],
          f4: SyntaxCombine[F],
          f5: Functor[F],
          f6: At[F]
        ): F[TypeOne] =
          Build[F, TypeOne] { __ =>
            (
              new FunctorSyntaxObs(
                (__ \ "param1").as[Int]) ~
                (__ \ "param2").as[String]
            )(TypeOne.apply)
          }
      
      val docTypeOne = buildTypeOne[Doc]
      
      assert(
        docTypeOne.jsonSchema == Json.obj(
          "title" -> "TypeOne",
          "type" -> "object",
          "properties" -> Json.obj(
            "param1" -> Json.obj(
              "type" -> "number",
              "format" -> "number"),
            "param2" -> Json.obj(
              "type" -> "string"))))
      
      val ruleTypeOne = buildTypeOne[Rule[JsValue, ?]]
      
      assert(ruleTypeOne.validate(Json.obj("param1" -> 1, "param2" -> "S")) == Valid(TypeOne(1, "S")))
    }
  }
}
