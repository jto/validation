package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js

class JsonRulesSpec extends RulesSpec[js.Dynamic] {
  val grammar = RulesGrammar
  val testCases = JsonTestCases

  type From = js.Dynamic
  def transform = identity
}
