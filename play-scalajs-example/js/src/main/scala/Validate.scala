package client

import jto.validation.{Rule, Valid, Invalid, VA}
import jto.validation.jsjson.{Rules, Writes}
import scala.scalajs.js
import js.annotation.JSExport
import model.User

@JSExport
object Validate {
  @JSExport
  def user(json: js.Dynamic): js.Dynamic = {
    import Rules._, Writes._

    val rule: Rule[js.Dynamic, User] = Rule.gen
    val validated: VA[User] = rule.validate(json)
    validated match {
      case e @ Invalid(_) => failureW.writes(e)
      case Valid(_) => json
    }
  }
}
