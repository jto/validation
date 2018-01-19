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
private[openapi] final case class Obj(properties: List[Property],
                                      children: List[PropTree.Named])
    extends PropTree

object PropTree {
  private[openapi] final case class Named(name: String, children: PropTree)
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

  // TODO: use a better type than String
  def yaml: String = {
    val aSchema = SchemaOps.aSchema(this)
    val o = Yaml.mapper.convertValue(aSchema, classOf[ObjectNode])
    Yaml.pretty(o)
  }

  def json: String = {
    val aSchema = SchemaOps.aSchema(this)
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

private[openapi] object SchemaOps {

  /**
    * Turn the schema into a flat list of localized properties
    */
  def addRoot(root: Path)(ps: List[(Path, Property)]) =
    for {
      (path, p) <- ps
    } yield (root ++ path, p)

  /**
    * Turn the flat structure in a tree where nodes are grouped by name
    */
  def toTree(ps: List[(Path, Property)]): PropTree = {
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
        val props = root.map(_._2)
        val ts =
          objs
            .groupBy { _._1.path.head }
            .toList
            .collect {
              // TODO: IdxPathNode ???
              case (KeyPathNode(name), ps0) =>
                val subtree =
                  ps0.map { case (Path(_ :: t), p) => (Path(t), p) }
                PropTree.Named(name, toTree(subtree))
            }
        Obj(props, ts)
    }
  }

  /**
    * Turn the internal tree into a Java Schema.
    */
  def updateSchema(s: ASchema[_])(ps: List[Property]): ASchema[_] = {
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
  def toSchema(tree: PropTree): ASchema[_] = tree match {
    case Props(Property.Integer, ps) =>
      updateSchema(new IntegerSchema)(ps)
    case Props(Property.Number, ps) =>
      updateSchema(new NumberSchema)(ps)
    case Props(Property.String, ps) =>
      updateSchema(new StringSchema)(ps)
    case Props(Property.Boolean, ps) =>
      updateSchema(new BooleanSchema)(ps)

    case Obj(props, children) =>
      import scala.collection.JavaConverters._
      val obj = new ObjectSchema()
      updateSchema(obj)(props)

      val requireds =
        children.collect {
          case PropTree.Named(name, Props(_, subProps))
              if subProps.contains(Property.Required) =>
            name
          case PropTree.Named(name, Obj(subProps, _))
              if subProps.contains(Property.Required) =>
            name
        }

      obj.setRequired(requireds.asJava)

      children.foreach {
        case PropTree.Named(name, children) =>
          obj.addProperties(name, toSchema(children))
      }
      obj
  }

  def aSchema(sc: UntypedSchema): ASchema[_] = {
    val props = addRoot(sc.root)(sc.properties)
    val tree = toTree(props)
    toSchema(tree)
  }
}
