package jto.validation
package v3.tagless
package xml


trait XmlGrammar[I, K[_, _]] extends Grammar[I, K] {

  type Sub <: Out

  // implicit def seq[A](implicit k: K[_ >: Out <: I, A]): K[I, Seq[A]]
  implicit def list[A](implicit k: K[_ >: Out <: I, A]): K[I, List[A]]
  // implicit def array[A: scala.reflect.ClassTag](implicit k: K[_ >: Out <: I, A]): K[I, Array[A]]
  // implicit def traversable[A](implicit k: K[_ >: Out <: I, A]): K[I, Traversable[A]]

  // def attr[A](label: String)(implicit w: K[Out, A]): K[Out, A]
}
