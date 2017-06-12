package jto.validation
package v3.tagless
package xml


trait XmlGrammar[I, K[_, _]] extends Grammar[I, K] {

  type Sub <: Out

  // final class AttributeOps[A](K: K[Sub, A]) {
  //   def attr[B](key: String, attrK: K[Option[I], B]): K[Sub, (A, B)] =
  //     withAttr(key, attrK)(K)
  // }

  // implicit def toAttributeOps[A](K: K[Sub, A]) = new AttributeOps(K)

  // def withAttr[A, B](key: String, attrK: K[Option[I], B])(K: K[Sub, A]): K[Sub, (A, B)]
}
