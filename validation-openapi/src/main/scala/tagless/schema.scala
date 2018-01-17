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

/**
  * @see https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#schemaObject
  */
sealed trait Property

object Property {
  sealed abstract class Type(val `type`: String) extends Property
  object Type {
    def unapply(t: Type): Option[String] = Option(t.`type`)
  }
  case object Integer extends Type("integer")
  case object Number extends Type("number")
  case object String extends Type("string")
  case object Boolean extends Type("boolean")

  final case object Required extends Property

  sealed abstract class Format[T <: Type](val format: String) extends Property
  object Format {
    def unapply[T <: Type](f: Format[T]): Option[String] = Option(f.format)
  }
  case object Int32 extends Format[Integer.type]("int32")
  case object Int64 extends Format[Integer.type]("int64")
  case object Float extends Format[Number.type]("float")
  case object Double extends Format[Number.type]("double")
  case object Byte extends Format[String.type]("byte")
  case object Binary extends Format[String.type]("binary")
  case object Date extends Format[String.type]("date")
  case object `Date-time` extends Format[String.type]("date-time")
  case object Password extends Format[String.type]("password")
  case class Raw[T <: Type](f: String) extends Format[T](f)

  case object NoProp extends Property
}

private[openapi] sealed trait PropTree
private[openapi] final case class Props(`type`: Property.Type,
                                        ps: List[Property])
    extends PropTree
private[openapi] final case class Obj(ps: List[PropTree.Named]) extends PropTree

object PropTree {
  private[openapi] final case class Named(name: String,
                                          properties: List[Property],
                                          children: PropTree)
}

sealed trait UntypedSchema {
  def root: Path
  def properties: List[(Path, Property)]
}

final case class Schema[X, A] private[openapi] (
    root: Path,
    properties: List[(Path, Property)]
) extends UntypedSchema {

  import io.swagger.util.{Yaml, Json}
  import com.fasterxml.jackson.databind.node.ObjectNode

  /**
    * Turn the schema into a flat list of localized properties
    */
  private def addRoot(root: Path)(ps: List[(Path, Property)]) =
    for {
      (path, p) <- ps
    } yield (root ++ path, p)

  /**
    * Turn the flat structure in a tree where nodes are grouped by name
    */
  private def toTree(ps: List[(Path, Property)]): PropTree = {
    ps.partition { _._1 == Path } match {
      case (r0, Nil) =>
        val root = r0.map(_._2)
        // XXX: type lookup is a bit unsafe...
        val (types, prop) = root.partition {
          case Property.Type(_) => true
          case _                => false
        }
        val ts = types.collect { case t @ Property.Type(_) => t }
        Props(ts.head, root)
      case (root, objs) =>
        val ts =
          objs
            .groupBy { _._1.path.head }
            .toList
            .collect {
              // TODO: IdxPathNode ???
              case (KeyPathNode(name), ps0) =>
                val props = root.map(_._2)
                val subtree =
                  ps0.map { case (Path(_ :: t), p) => (Path(t), p) }
                PropTree.Named(name, props, toTree(subtree))
            }
        Obj(ts)
    }
  }

  /**
    * Turn the internal tree into a Java Schema.
    */
  private def updateSchema(s: ASchema[_])(ps: List[Property]): ASchema[_] = {
    ps.foreach {
      case Property.Type(_)   => ()
      case Property.Format(f) => s.setFormat(f)
      case Property.Required  => () // TODO: add required on parent ?
      case Property.NoProp    => ()
    }
    s
  }

  /**
    * Turn the internal tree into a Java Schema.
    */
  private def toSchema(tree: PropTree): ASchema[_] = tree match {
    case Props(Property.Integer, ps) => updateSchema(new IntegerSchema)(ps)
    case Props(Property.Number, ps)  => updateSchema(new NumberSchema)(ps)
    case Props(Property.String, ps)  => updateSchema(new StringSchema)(ps)
    case Props(Property.Boolean, ps) => updateSchema(new BooleanSchema)(ps)

    case Obj(ps) =>
      val obj = new ObjectSchema()
      ps.foreach {
        case PropTree.Named(name, properties, children) =>
          obj.addProperties(name, toSchema(children))
          updateSchema(obj)(properties)
      }
      obj
  }

  private def aSchema: ASchema[_] = {
    val props = addRoot(root)(properties)
    val tree = toTree(props)
    toSchema(tree)
  }

  // TODO: use a better type than String
  def yaml: String = {
    val o = Yaml.mapper.convertValue(aSchema, classOf[ObjectNode])
    Yaml.pretty(o)
  }

  def json: String = {
    val o = Json.mapper.convertValue(aSchema, classOf[ObjectNode])
    Json.pretty(o)
  }

}

object Schema {
  def empty[X, A] = Schema[Nothing, A](Path, Nil)

  def at[X, A](p: Path, prop: Property = Property.NoProp) =
    Schema[Nothing, A](p, List(Path -> prop))

  def root[X, A](p: Property*) =
    tag[Root](Schema[X, A](Path, p.map(Path -> _).toList))
}

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
