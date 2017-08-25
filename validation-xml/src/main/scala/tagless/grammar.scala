package jto.validation
package v3.tagless
package xml


trait XmlGrammar[I, K[_, _]] extends Grammar[I, K] {

  type Sub <: Out

  def attr[A](label: String)(implicit w: K[Out, A]): K[Out, A]
}
