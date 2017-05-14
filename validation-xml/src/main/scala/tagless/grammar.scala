package jto.validation
package v3.tagless
package xml

import scala.xml.Node

trait XmlGrammar[K[_, _]] extends Grammar[Node, K] {
  def attr[A](key: String)(K: K[Option[_ >: Out <: Node], A]): K[Option[_ >: Out <: Node], A]
}
