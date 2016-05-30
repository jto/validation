package client

import jto.validation.{ Format, Valid, Invalid, VA, Formatting }
import jto.validation.jsjson.{ Rules, Writes }
import scala.scalajs.js
import js.annotation.JSExport
import model.User
import scala.Function.const

@JSExport
object Validate {
  @JSExport
  def user(json: js.Dynamic): js.Dictionary[Any] = {
    import Rules._, Writes._

    val format =
      Formatting[js.Dynamic, js.Dynamic] { __ =>
        (
          (__ \ "name").format(notEmpty) ~
          (__ \ "age").format(min(0) |+| max(130)) ~
          (__ \ "email").format(optionR(email), optionW(stringW)) ~
          (__ \ "isAlive").format[Boolean]
        ).unlifted(User.apply, User.unapply _)
      }

    val validated: VA[User] = format.validate(json)

    js.Dictionary(
      "isSuccess" -> validated.fold(const(false), const(true)),
      "output"    -> validated.fold(const(null), format.writes),
      "errors"    -> validated.fold(e => failureW.writes(Invalid(e)), const(null))
    )
  }
}
