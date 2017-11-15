package jto.validation
package v3.tagless
package forms

import jto.validation.forms._

class FormRulesSpec extends jto.validation.v3.tagless.RulesSpec[PM.PM] {
  val grammar = RulesGrammar
  val testCases = FormTestCases

  type From = PM.PM
  def transform = identity _
}
