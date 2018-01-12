package jto.validation
package v3.tagless
package openapi

import scala.reflect.ClassTag
import shapeless.{HNil, Generic}
import shapeless.tag, tag.@@
import cats.Semigroup
import cats.arrow.Compose

import io.swagger.oas.models.media.{Schema => ASchema, _}

import types._
sealed trait Schema[A] {
  import io.swagger.util.{Yaml, Json}
  import com.fasterxml.jackson.databind.node.ObjectNode

  def underlying: ASchema[A]

  // TODO: use a better type than String
  def yaml: String = {
    val o = Yaml.mapper.convertValue(underlying, classOf[ObjectNode])
    Yaml.pretty(o)
  }

  def json: String = {
    val o = Json.mapper.convertValue(underlying, classOf[ObjectNode])
    Json.pretty(o)
  }
}

object Schema {
  private[openapi] def apply[A](p: ASchema[A]) =
    new Schema[A] {
      val underlying = p
    }

  private[openapi] def root[A](p: ASchema[A]) = tag[Root](apply(p))
}

sealed trait SchemaGrammar extends Grammar[Nothing, snd[Schema]#λ] {
  self =>

  type Sup = Nothing
  type Out = Sup
  type P = SchemaGrammar

  // Members declared in jto.validation.v3.tagless.Primitives
  def mapPath(f: Path => Path) =
    new SchemaGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  protected def asType[H, B](k: Schema[H])(
      implicit G: Generic.Aux[B, H]): Schema[B] = ???

  def is[A](implicit K: Schema[A]): Schema[A] = K

  def at(p: Path): At[snd[Schema]#λ, Out, Nothing] = ???
  def knil: Schema[HNil] = ???

  def opt[A](implicit K: Schema[A]): Schema[Option[A]] = ???
  def req[A](implicit K: Schema[A]): Schema[A] = ???

  implicit def string: Schema[String] @@ Root = Schema.root(new StringSchema {})
  implicit def long: Schema[Long] @@ Root = ???
  implicit def short: Schema[Short] @@ Root = ???
  implicit def boolean: Schema[Boolean] @@ Root = ???
  implicit def double: Schema[Double] @@ Root = ???
  implicit def float: Schema[Float] @@ Root = ???
  implicit def int: Schema[Int] @@ Root = ???
  implicit def bigDecimal: Schema[BigDecimal] @@ Root = ???
  implicit def jBigDecimal: Schema[java.math.BigDecimal] @@ Root =
    Schema.root(new NumberSchema {})

  implicit def list[A](implicit k: Schema[A]): C[List[A]] = ???
  implicit def map[A](implicit k: Schema[A]): C[Map[String, A]] =
    ???
  implicit def seq[A](implicit k: Schema[A]): C[Seq[A]] = ???
  implicit def array[A: ClassTag](implicit k: Schema[A]): C[Array[A]] = ???
  implicit def traversable[A](implicit k: Schema[A]): C[Traversable[A]] =
    ???

  // Members declared in jto.validation.v3.tagless.Constraints
  def email: C[String] = Schema.root[String](new EmailSchema {})
  def equalTo[A](a: A): C[A] = ???
  def forall[I, O](k: Schema[O]): C[Seq[O]] = ???
  def max[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def maxLength(l: Int): C[String] = ???
  def min[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def minLength(l: Int): C[String] = ???
  def notEmpty: C[String] = ???
  def pattern(regex: scala.util.matching.Regex): C[String] = ???

  // Members declared in jto.validation.v3.tagless.Typeclasses
  implicit def composeTC: Compose[snd[Schema]#λ] =
    new Compose[snd[Schema]#λ] {
      def compose[A, B, D](f: Schema[D], g: Schema[B]): Schema[D] = ???
    }

  implicit def mergeTC: Merge[snd[Schema]#λ, Out] = ???
  implicit def mkLazy: MkLazy[snd[Schema]#λ] =
    new MkLazy[snd[Schema]#λ] {
      def apply[A, B](k: => Schema[B]): Schema[B] = k
    }

  implicit def semigroupTC[I0, O]: Semigroup[Schema[O] @@ Root] =
    new Semigroup[Schema[O] @@ Root] {
      def combine(x: Schema[O] @@ Root,
                  y: Schema[O] @@ Root): Schema[O] @@ Root = {
        import scala.collection.JavaConverters._
        val all = List[ASchema[_]](x.underlying, y.underlying).asJava
        val s = new ComposedSchema().allOf(all).asInstanceOf[ASchema[O]]
        Schema.root(s)

      }
    }
}

// object SchemaGrammar extends SchemaGrammar
