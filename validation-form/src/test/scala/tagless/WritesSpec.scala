package jto.validation
package v3.tagless
package forms

import jto.validation.forms._

class FormWritesSpec extends jto.validation.v3.tagless.WritesSpec[PM.PM] {
  type To = PM.PM
  def transform = identity _
  val grammar = WritesGrammar
  val testCases = FormTestCases
}
