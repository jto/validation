package client

import jto.validation.{To, Format, Valid, Invalid, VA, Formatting}
import jto.validation.jsjson.{Rules, Writes}
import scala.scalajs.js
import js.annotation.JSExport
import model.User
import scala.Function.{unlift, const}

@JSExport
object Validate {
  @JSExport
  def user(json: js.Dynamic): js.Dynamic = {
    import Rules._, Writes._

    implicit val format =
      Formatting[js.Dynamic, js.Dynamic] { __ =>
        (
          (__ \ "name").format(notEmpty) ~
          (__ \ "age").format(min(0) |+| max(130)) ~
          (__ \ "email").format(optionR(email), optionW(stringW)) ~
          (__ \ "isAlive").format[Boolean]
        )(User.apply, unlift(User.unapply))
      }

    To[VA[User], js.Dynamic](format.validate(json))
  }
}
