package jto.validation
package v3.tagless
package xml


trait XmlGrammar[I, K[_, _]] extends Grammar[I, K] {

  final class AttributeOps[A](K: K[Option[Out], A]) {
    def attr[B](key: String, attrK: K[Option[I], B]): K[Option[Out], (A, B)] =
      withAttr(key, attrK)(K)
  }

  implicit def toAttributeOps[A](K: K[Option[Out], A]) = new AttributeOps(K)

  def withAttr[A, B](key: String, attrK: K[Option[I], B])(K: K[Option[Out], A]): K[Option[Out], (A, B)]
}
