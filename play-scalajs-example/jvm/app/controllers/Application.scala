package controllers

import jto.validation.playjson.Writes
import jto.validation.Write
import play.api.Environment
import play.api.libs.json._
import play.api.mvc._

import model.User

class Application()(implicit environment: Environment) extends Controller {

  def index = Action {
    import Writes._
    val write: Write[User, JsObject] = Write.gen[User, JsObject]
    val user: User = User("supercat", 20, Some("e@mail.com"), true)
    val json: String = Json.prettyPrint(write.writes(user))
    Ok(views.html.index(json))
  }

}
