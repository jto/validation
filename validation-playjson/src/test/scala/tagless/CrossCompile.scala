package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._

class CrossCompile extends v3.tagless.CrossCompile[JsValue] {
  val rg = RulesGrammar
  val wg = WritesGrammar
  def upcast = implicitly
}
