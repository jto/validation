package jto.validation
package v3.tagless
package xml


trait XmlGrammar[I, K[_, _]] extends Grammar[I, K] {
  type OutAttr <: I
  def attr(key: String): At[K, OutAttr, I]
}
