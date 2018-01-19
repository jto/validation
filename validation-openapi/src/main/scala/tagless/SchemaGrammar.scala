package jto.validation
package v3.tagless
package openapi

import scala.reflect.ClassTag
import shapeless.{HList, HNil, Generic}
import shapeless.tag, tag.@@
import cats.Semigroup
import cats.arrow.Compose

import io.swagger.oas.models.media.{Schema => ASchema, _}

import types._

/*
import jto.validation._, v3.tagless._, openapi._
import cats.syntax.compose._

def info[T, K[_, _]](implicit g: Grammar[T, K]) = {
    import g._
    at(Path \ "label").is(req[String]) ~:
    at(Path \ "email").is(opt(is[String] andThen email)) ~:
    knil
}

info(SchemaGrammar).json
 */

/**
  * Grammar support for OpenAPI v3
  */
sealed trait SchemaGrammar extends Grammar[Nothing, Schema] {
  self =>

  type Sup = Nothing
  type Out = Nothing
  type P = SchemaGrammar

  // Members declared in jto.validation.v3.tagless.Constraints
  def email: C[String] =
    Schema.root(Property.Raw[Property.String.type]("email"))
  def equalTo[A](a: A): C[A] = ???
  def forall[I, O](k: Schema[I, O]): Schema[Seq[I], Seq[O]] = ???
  def max[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def maxLength(l: Int): C[String] = ???
  def min[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def minLength(l: Int): C[String] = ???
  def notEmpty: C[String] = ???
  def pattern(regex: scala.util.matching.Regex): C[String] = ???

  protected def asType[H, B](k: Schema[Out, H])(
      implicit G: Generic.Aux[B, H]): Schema[Out, B] =
    Schema(k.root, k.properties)

  def at(p: Path): At[Schema, Out, Nothing] =
    new At[Schema, Out, Nothing] {
      def run: Schema[Out, Option[Nothing]] =
        Schema.at(p)
    }

  def is[A](implicit K: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, A] = K
  def knil: Schema[Out, HNil] = Schema.empty

  def mapPath(f: Path => Path): SchemaGrammar.P =
    new SchemaGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def opt[A](implicit K: Schema[_ >: Out <: Nothing, A])
    : Schema[Option[Nothing], Option[A]] = Schema(Path, K.properties)

  def req[A](implicit K: Schema[_ >: Out <: Nothing, A])
    : Schema[Option[Nothing], A] = {
    val reqProp = (Path, Property.Required)
    Schema(Path, K.properties :+ reqProp)
  }

  implicit def string: Schema[Nothing, String] @@ Root =
    Schema.root(Property.String)
  implicit def short: Schema[Nothing, Short] @@ Root =
    Schema.root(Property.Integer)
  implicit def int: Schema[Nothing, Int] @@ Root =
    Schema.root(Property.Integer)
  implicit def double: Schema[Nothing, Double] @@ Root =
    Schema.root(Property.Number, Property.Double)
  implicit def float: Schema[Nothing, Float] @@ Root =
    Schema.root(Property.Number, Property.Float)
  implicit def long: Schema[Nothing, Long] @@ Root =
    Schema.root(Property.Number, Property.Int64)
  implicit def bigDecimal: Schema[Nothing, BigDecimal] @@ Root =
    Schema.root(Property.Number)
  implicit def jBigDecimal: Schema[Nothing, java.math.BigDecimal] @@ Root =
    Schema.root(Property.Number)
  implicit def boolean: Schema[Nothing, Boolean] @@ Root =
    Schema.root(Property.Boolean)

  implicit def array[A: ClassTag](
      implicit k: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, Array[A]] =
    ???

  implicit def list[A](
      implicit k: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, List[A]] =
    ???

  implicit def map[A](implicit k: Schema[_ >: Out <: Nothing, A])
    : Schema[Nothing, Map[String, A]] = ???

  implicit def seq[A](
      implicit k: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, Seq[A]] = ???

  implicit def traversable[A](implicit k: Schema[_ >: Out <: Nothing, A])
    : Schema[Nothing, Traversable[A]] = ???

  // Members declared in jto.validation.v3.tagless.Typeclasses
  implicit def composeTC: Compose[Schema] =
    new Compose[Schema] {
      def compose[A, B, C0](f: Schema[B, C0], g: Schema[A, B]): Schema[A, C0] =
        Schema[A, C0](g.root ++ f.root, g.properties ++ f.properties)
    }

  import shapeless.::
  implicit def mergeTC: Merge[Schema, Out] =
    new Merge[Schema, Out] {
      def merge[A, B <: HList](fa: Schema[Out, A],
                               fb: Schema[Out, B]): Schema[Out, A :: B] = {
        def rootify[X](s: Schema[Out, X]) =
          s.properties.map { case (p, props) => (s.root ++ p, props) }
        Schema[Out, A :: B](Path, rootify(fa) ++ rootify(fb))
      }

    }

  implicit def mkLazy: MkLazy[Schema] =
    new MkLazy[Schema] {
      def apply[A, B](k: => Schema[A, B]): Schema[A, B] = k
    }

  implicit def semigroupTC[I0, O]: Semigroup[Schema[I0, O] @@ Root] =
    new Semigroup[Schema[I0, O] @@ Root] {
      def combine(x: Schema[I0, O] @@ Root,
                  y: Schema[I0, O] @@ Root): Schema[I0, O] @@ Root =
        tag[Root](Schema(Path, x.properties ++ y.properties))
    }
}

object SchemaGrammar extends SchemaGrammar
