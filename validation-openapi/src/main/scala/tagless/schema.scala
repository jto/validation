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

/*
sealed trait Schema[X, A] {
  self =>
  import io.swagger.util.{Yaml, Json}
  import com.fasterxml.jackson.databind.node.ObjectNode

  def path: Path
  type U
  def underlying: ASchema[U]

  // TODO: use a better type than String
  def yaml: String = {
    // TODO: support path
    val o = Yaml.mapper.convertValue(underlying, classOf[ObjectNode])
    Yaml.pretty(o)
  }

  def json: String = {
    val o = Json.mapper.convertValue(underlying, classOf[ObjectNode])
    Json.pretty(o)
  }

  def repath(f: Path => Path): Schema[X, A] =
    new Schema[X, A] {
      val path = f(self.path)
      type U = self.U
      val underlying = self.underlying
    }
}
 */

sealed trait Schema[X, A] {
  type U
  def yaml: String = ???
  def json: String = ???
}

sealed trait PrimitiveSchema[X, A] extends Schema[X, A] {
  def underlying: ASchema[U]
}

object PrimitiveSchema {
  def apply[X, A, U0](s: ASchema[U0]) =
    new PrimitiveSchema[X, A] {
      type U = U0
      def underlying = s
    }

  def unapply[X, A](s: PrimitiveSchema[X, A]): Option[ASchema[s.U]] =
    Option(s.underlying)
}

sealed trait PositionalSchema[X, A] extends Schema[X, A] {
  self =>
  def at: PathNode
  def schema: Schema[X, A] { type U = self.U }
}

object PositionalSchema {
  def apply[X, A](p: PathNode, s: Schema[X, A]) =
    new PositionalSchema[X, A] {
      type U = s.U
      def at = p
      def schema = s
    }

  def unapply[X, A](s: PositionalSchema[X, A])
    : Option[(PathNode, Schema[X, A] { type U = s.U })] =
    Option((s.at, s.schema))
}

object Schema {
  private[openapi] def empty[X, A] =
    new PrimitiveSchema[X, A] {
      type U = Nothing
      def underlying = new ASchema[Nothing] {}
    }

  private[openapi] def apply[A](p: ASchema[A]) =
    new PrimitiveSchema[Nothing, A] {
      type U = A
      def underlying = p
    }

  private[openapi] def root[A, B](p: ASchema[B]) =
    tag[Root](new PrimitiveSchema[A, B] {
      type U = B
      def underlying = p
    })
}

/**
  * Grammar support for OpenAPI v3
  */
sealed trait SchemaGrammar extends Grammar[Nothing, Schema] {
  self =>

  type Sup = Nothing
  type Out = Nothing
  type P = SchemaGrammar

  def mapPath(f: Path => Path) =
    new SchemaGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  protected def asType[H, B](k: Schema[Nothing, H])(
      implicit G: Generic.Aux[B, H]): Schema[Nothing, B] = ???

  def is[A](implicit K: Schema[_ <: Nothing, A]): Schema[Nothing, A] = K

  def at(p: Path): At[Schema, Out, Nothing] =
    new At[Schema, Out, Nothing] {
      def run: Schema[Out, Option[Nothing]] =
        ???
    }

  def knil: Schema[Nothing, HNil] = ???

  @inline private def nullable[F[_], A](v: Boolean)(
      K: Schema[Nothing, A]): Schema[Option[Nothing], F[A]] = {
    @inline def go(s: Schema[Nothing, A]): Schema[Option[Nothing], F[A]] =
      s match {
        case PositionalSchema(at, schema) =>
          PositionalSchema(at, go(schema))
        case PrimitiveSchema(as) => {
          // FIXME: Mutation based = BAD
          val u = as
          u.setNullable(v)
          PrimitiveSchema(u)
        }
      }

    go(K)
  }

  def opt[A](implicit K: Schema[_ <: Nothing, A]) =
    nullable[Option, A](true)(K)

  def req[A](implicit K: Schema[_ <: Nothing, A]): Schema[Option[Nothing], A] =
    nullable[cats.Id, A](false)(K)

  implicit def string: Schema[Nothing, String] @@ Root =
    Schema.root(new StringSchema {})
  implicit def long: Schema[Nothing, Long] @@ Root = ???
  implicit def short: Schema[Nothing, Short] @@ Root = ???
  implicit def boolean: Schema[Nothing, Boolean] @@ Root = ???
  implicit def double: Schema[Nothing, Double] @@ Root = ???
  implicit def float: Schema[Nothing, Float] @@ Root = ???
  implicit def int: Schema[Nothing, Int] @@ Root = ???
  implicit def bigDecimal: Schema[Nothing, BigDecimal] @@ Root = ???
  implicit def jBigDecimal: Schema[Nothing, java.math.BigDecimal] @@ Root =
    Schema.root(new NumberSchema {})

  implicit def list[A](
      implicit k: Schema[_ <: Nothing, A]): Schema[Nothing, List[A]] = ???
  implicit def map[A](
      implicit k: Schema[_ <: Nothing, A]): Schema[Nothing, Map[String, A]] =
    ???
  implicit def array[A: ClassTag](
      implicit k: Schema[_ <: Nothing, A]): Schema[Nothing, Array[A]] = ???
  implicit def seq[A](
      implicit k: Schema[_ <: Nothing, A]): Schema[Nothing, Seq[A]] = ???
  implicit def traversable[A](
      implicit k: Schema[_ <: Nothing, A]): Schema[Nothing, Traversable[A]] =
    ???

  def email: C[String] = Schema.root[String, String](new EmailSchema {})
  def equalTo[A](a: A): C[A] = ???
  def forall[I, O](k: Schema[I, O]): Schema[Seq[I], Seq[O]] = ???
  def max[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def maxLength(l: Int): C[String] = ???
  def min[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def minLength(l: Int): C[String] = ???
  def notEmpty: C[String] = ???
  def pattern(regex: scala.util.matching.Regex): C[String] = ???

  implicit def composeTC: Compose[Schema] =
    ???
  // new Compose[Schema] {
  //   def compose[A, B, C0](f: Schema[B, C0], g: Schema[A, B]): Schema[A, C0] =
  //     new Schema[A, C0] {
  //       type U = f.U
  //       val path = g.path ++ f.path
  //       val underlying = f.underlying
  //     }
  // }

  implicit def mergeTC: Merge[Schema, Out] = ???
  implicit def mkLazy: MkLazy[Schema] =
    new MkLazy[Schema] {
      def apply[A, B](k: => Schema[A, B]): Schema[A, B] = k
    }

  implicit def semigroupTC[I0, O]: Semigroup[Schema[I0, O] @@ Root] =
    ???
  // new Semigroup[Schema[I0, O] @@ Root] {
  //   def combine(x: Schema[I0, O] @@ Root,
  //               y: Schema[I0, O] @@ Root): Schema[I0, O] @@ Root = {
  //     import scala.collection.JavaConverters._
  //     val all = List[ASchema[_]](x.underlying, y.underlying).asJava
  //     val s = new ComposedSchema().allOf(all).asInstanceOf[ASchema[O]]
  //     Schema.root[I0, O](s)
  //   }
  // }
}

object SchemaGrammar extends SchemaGrammar
