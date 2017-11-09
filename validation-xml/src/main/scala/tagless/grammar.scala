package jto.validation
package v3.tagless
package xml


trait XmlGrammar[I, K[_, _]] extends Grammar[I, K] {
  def attr(key: String): At[K, I, I]
}
