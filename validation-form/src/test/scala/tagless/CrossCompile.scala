package jto.validation
package v3.tagless
package forms

import jto.validation.forms._

class CrossCompile extends v3.tagless.CrossCompile[PM.PM] {
  val rg = RulesGrammar
  val wg = WritesGrammar
  def upcast = implicitly
}
