package client

import scala.scalajs.js

object Main extends js.JSApp {
  def main(): Unit = {
    println("Hello console!")
    throw new Exception("Check out my stack trace")
  }
}
