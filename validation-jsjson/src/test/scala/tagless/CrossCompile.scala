package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js

class CrossCompile extends v3.tagless.CrossCompile[js.Dynamic] {
  val rg = RulesGrammar
  val wg = WritesGrammar
  def upcast = implicitly
}
