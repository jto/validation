package jto.validation
package mson

import cats.InvariantMonoidal

sealed trait Markdown {
  override def toString(): String = {
    def r(required: Boolean): String = if (required) ", required" else ""

    def show0(md: Markdown, ident: Int): List[(String, Int)] =
      md match {
        case MdLeaf(tpe, required) =>
          List(s"($tpe${r(required)})" -> ident)

        case MdArray(value, required) =>
          (s"(array${r(required)})" -> ident) :: show0(value, ident + 1)

        case MdObject(value, required) =>
          val o = (s"(object${r(required)})" -> ident)
          o :: value.toList.flatMap { case (k, v) =>
            val (h, i) :: t = show0(v, ident + 1)
            (s"$k $h" -> i) :: t
          }
      }

    show0(this, 0).map { case (s, i) => "  " * i + "- " + s }.mkString("\n")
  }
}
case class MdLeaf(tpe: String, required: Boolean = true)                    extends Markdown
case class MdArray(value: Markdown, required: Boolean = true)               extends Markdown
case class MdObject(value: Map[String, Markdown], required: Boolean = true) extends Markdown

object Markdown {
  def deepMerge(one: Markdown, another: Markdown): Markdown = {
    def merge(existingObject: MdObject, otherObject: MdObject): MdObject = {
      val result = existingObject.value ++ otherObject.value.map {
        case (otherKey, otherValue) =>
          val maybeExistingValue = existingObject.value.get(otherKey)
          val newValue = (maybeExistingValue, otherValue) match {
            case (Some(e: MdObject), o: MdObject) => merge(e, o)
            case _ => otherValue
          }
          otherKey -> newValue
      }
      MdObject(result)
    }
    (one, another) match {
      case (o: MdObject, a: MdObject) => merge(o, a)
      case _ => ???
    }
  }
}

trait Mson[A] {
  def api: Markdown
}

object Mson {
  def apply[A](a: Markdown): Mson[A] = new Mson[A] { def api = a }

  implicit val msonInvariantMonoidal: InvariantMonoidal[Mson] = new InvariantMonoidal[Mson] {
    def product[A, B](fa: Mson[A], fb: Mson[B]): Mson[(A, B)] = Mson[(A, B)](Markdown.deepMerge(fa.api, fb.api))
    def imap[A, B](fa: Mson[A])(f: A => B)(g: B => A): Mson[B] = Mson[B](fa.api)
    def pure[A](a: A): Mson[A] = Mson[A](null)
  }

  implicit def msonInt:    Mson[Int]    = Mson[Int]   (MdLeaf("number", true))
  implicit def msonString: Mson[String] = Mson[String](MdLeaf("string", true))
  implicit def msonDouble: Mson[Double] = Mson[Double](MdLeaf("number", true))

  implicit def msonOption[A](implicit m: Mson[A]): Mson[Option[A]] = Mson[Option[A]](
    m.api match {
      case MdLeaf(tpe, _)        => MdLeaf(tpe, false)
      case MdArray(elements, _)  => MdArray(elements, false)
      case MdObject(elements, _) => MdObject(elements, false)
    }
  )

  implicit def msonArray[A](implicit m: Mson[A]): Mson[List[A]] =
    Mson[List[A]](MdArray(m.api, true))

  implicit def pickInMson[A](implicit m: Mson[A]): Path => Mson[A] = path => Mson[A] {
    if (path.path.collectFirst { case x: IdxPathNode => x }.isDefined)
      throw new RuntimeException(s"path $path is not a path of MSON")
    else {
      val keys: List[String] = path.path.collect { case KeyPathNode(x) => x }
      keys.foldLeft(m.api) { case (api, key) => MdObject(Map(key -> api)) }
    }
  }
}
