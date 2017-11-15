package jto.validation
package v3.tagless
package jsonast

import jto.validation.jsonast._

class JsonRulesSpec extends jto.validation.v3.tagless.RulesSpec[JValue] {
  val grammar = RulesGrammar
  val testCases = JsonTestCases

  type From = JValue
  def transform = identity _
}
