package jto.validation
package playjson

import play.api.libs.json.{JsObject, Json}
import scala.reflect.ClassTag
import scala.language.implicitConversions
import cats.Functor

trait Doc[T] {
  def jsonSchema(title: String): JsObject
  def jsonSchema(implicit ct: ClassTag[T]): JsObject = jsonSchema(ct.toString)
}

object Doc extends DocCoreImplicits with DocDefaultImplicits

trait DocCoreImplicits {
  implicit val atDoc: At[Doc] = new At[Doc] {
    def at[A](path: Path, f: Doc[A])(implicit ct: ClassTag[A]): Doc[A] = {
      def at0(p: List[PathNode]): JsObject =
        p match {
          case KeyPathNode(x) :: Nil =>
            Json.obj(x -> f.jsonSchema)

          case KeyPathNode(x) :: xs =>
            Json.obj(
                x -> Json.obj(
                    "type" -> "object",
                    "properties" -> at0(xs)
                ))

          case _ =>
            throw new RuntimeException(s"path $path is not a path of JsValue")
        }

      new Doc[A] {
        def jsonSchema(title: String): JsObject =
          Json.obj(
              "title" -> title,
              "type" -> "object",
              "properties" -> at0(path.path)
          )
      }
    }
  }

  implicit val docSyntaxCombine: SyntaxCombine[Doc] = new SyntaxCombine[Doc] {
    def apply[A, B](ma: Doc[A], mb: Doc[B]): Doc[A ~ B] =
      new Doc[A ~ B] {
        def jsonSchema(title: String): JsObject =
          ma.jsonSchema(title) deepMerge mb.jsonSchema(title)
      }
  }

  implicit val docSyntaxFunctor: Functor[Doc] = new Functor[Doc] {
    def map[A, B](fa: Doc[A])(f: A => B): Doc[B] =
      new Doc[B] {
        def jsonSchema(title: String): JsObject = fa.jsonSchema(title)
      }
  }

  implicit def docFunctorSyntaxCombineObs[A](d: Doc[A]) =
    new FunctorSyntaxObs[Doc, A](d)

  implicit def docMix[I]: Mixer1[Doc] =
    new Mixer1[Doc] {
      def mix[A](m1: Doc[A]): Doc[A] = m1
    }
}

trait DocDefaultImplicits {
  implicit val intD: Doc[Int] = new Doc[Int] {
    def jsonSchema(title: String): JsObject =
      Json.obj("type" -> "number", "format" -> "number")
  }

  implicit val longD: Doc[Long] = new Doc[Long] {
    def jsonSchema(title: String): JsObject = intD.jsonSchema(title)
  }

  implicit val doubleD: Doc[Double] = new Doc[Double] {
    def jsonSchema(title: String): JsObject = intD.jsonSchema(title)
  }

  implicit val stringD: Doc[String] = new Doc[String] {
    def jsonSchema(title: String): JsObject =
      Json.obj("type" -> "string")
  }
}
