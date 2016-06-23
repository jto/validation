package controllers

import jto.validation._
import jto.validation.jsonast._
import play.api.Environment
import play.api.libs.json._
import play.api.mvc._

import model.User

class Application()(implicit environment: Environment) extends Controller {
  def index = Action {
    val write: Write[User, JsValue] = Write.toWrite(User.format) andThen Ast.to
    val user: User = User("supercat", 20, Some("e@mail.com"), true)
    val json: String = Json.prettyPrint(write.writes(user))
    Ok(views.html.index(json))
  }
}
