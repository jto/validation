package model

import jto.validation._
import jto.validation.jsonast._
import scala.Function.unlift

case class User(
  name: String,
  age: Int,
  email: Option[String],
  isAlive: Boolean
)

object User {
  import Rules._, Writes._
  implicit val format: Format[JValue, JObject, User] =
    Formatting[JValue, JObject] { __ =>
      (
        (__ \ "name").format(notEmpty) ~
        (__ \ "age").format(min(0) |+| max(130)) ~
        (__ \ "email").format(optionR(email), optionW(stringW)) ~
        (__ \ "isAlive").format[Boolean]
      )(User.apply, unlift(User.unapply))
    }
}
