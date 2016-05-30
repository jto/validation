import jto.validation._
import jto.validation.playjson._
import org.scalatest._
import play.api.libs.json.Json
import cats.syntax.all._

/** Examples from https://goo.gl/Zgbgty */
case class TypeTwo(param1: Int, param2: Long)
case class TypeThreeParamOne(param0: String)
case class TypeThree(param1: TypeThreeParamOne, param2: Double)
case class TypeFour(param1param0: String, param2: Double)

class DocSpec extends WordSpec with Matchers {
  "Format" should {
    "print TypeTwo" in {
      val docTypeTwo: Doc[TypeTwo] = Build[Doc, TypeTwo] { __ =>
        (
            (__ \ "param1").as[Int] ~
            (__ \ "param2").as[Long]
        )(TypeTwo.apply)
      }

      assert(
          docTypeTwo.jsonSchema == Json.obj(
              "title" -> "TypeTwo",
              "type" -> "object",
              "properties" -> Json.obj(
                  "param1" -> Json.obj("type" -> "number",
                                       "format" -> "number"),
                  "param2" -> Json.obj("type" -> "number",
                                       "format" -> "number"))))
    }

    "print TypeThree" in {
      implicit val docTypeThreeParamOne: Doc[TypeThreeParamOne] =
        Build[Doc, TypeThreeParamOne] { __ =>
          (__ \ "param0").as[String].map(TypeThreeParamOne.apply)
        }

      val docTypeThree: Doc[TypeThree] = Build[Doc, TypeThree] { __ =>
        (
            (__ \ "param1").as[TypeThreeParamOne] ~
            (__ \ "param2").as[Double]
        )(TypeThree.apply)
      }

      assert(
          docTypeThree.jsonSchema == Json.obj(
              "title" -> "TypeThree",
              "type" -> "object",
              "properties" -> Json.obj(
                  "param1" -> Json.obj(
                      "title" -> "TypeThreeParamOne",
                      "type" -> "object",
                      "properties" -> Json.obj(
                          "param0" -> Json.obj("type" -> "string"))),
                  "param2" -> Json.obj("type" -> "number",
                                       "format" -> "number"))))
    }

    "print TypeFour" in {
      val docTypeFour: Doc[TypeFour] = Build[Doc, TypeFour] { __ =>
        (
            (__ \ "param1" \ "param0").as[String] ~
            (__ \ "param2").as[Double]
        )(TypeFour.apply)
      }

      assert(
          docTypeFour.jsonSchema == Json.obj(
              "title" -> "TypeFour",
              "type" -> "object",
              "properties" -> Json.obj(
                  "param1" -> Json.obj(
                      "type" -> "object",
                      "properties" -> Json.obj(
                          "param0" -> Json.obj("type" -> "string"))),
                  "param2" -> Json.obj("type" -> "number",
                                       "format" -> "number"))))
    }
  }
}
