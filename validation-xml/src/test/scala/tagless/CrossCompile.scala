package jto.validation
package v3.tagless
package xml

class CrossCompile extends v3.tagless.CrossCompile[List[XML]] {
  val rg = RulesGrammar
  val wg = WritesGrammar
  def upcast = implicitly
}
