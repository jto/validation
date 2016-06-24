package client

import jto.validation._
import jto.validation.jsonast.Ast
import jto.validation.jsjson._
import scala.scalajs.js
import js.annotation.JSExport
import model.User
import scala.Function.{unlift, const}

@JSExport
object Validate {
  @JSExport
  def user(json: js.Dynamic): js.Dynamic = {
    import Writes._

    implicit val format: Format[js.Dynamic, js.Dynamic, User] = Format(
      Ast.from andThen User.format,
      Write.toWrite(User.format) andThen Ast.to
    )

    To[VA[User], js.Dynamic](format.validate(json))
  }
}
