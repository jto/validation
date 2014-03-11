/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api.libs.json

import scala.collection._
import scala.collection.mutable.ListBuffer

import scala.annotation.tailrec
import play.api.data.validation.ValidationError

/**
 * Generic json value
 */
sealed trait JsValue {

  /**
   * Return the property corresponding to the fieldName, supposing we have a JsObject.
   *
   * @param fieldName the name of the property to lookup
   * @return the resulting JsValue. If the current node is not a JsObject or doesn't have the property, a JsUndefined will be returned.
   */
  def \(fieldName: String): JsValue = JsUndefined("'" + fieldName + "'" + " is undefined on object: " + this)

  /**
   * Return the element at a given index, supposing we have a JsArray.
   *
   * @param idx the index to lookup
   * @return the resulting JsValue. If the current node is not a JsArray or the index is out of bounds, a JsUndefined will be returned.
   */
  def apply(idx: Int): JsValue = JsUndefined(this.toString + " is not an array")

  /**
   * Lookup for fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  def \\(fieldName: String): Seq[JsValue] = Nil

  // override def toString = Json.stringify(this) // TODO

  /**
   * Prune the Json AST according to the provided JsPath
   */
  //def prune(path: JsPath): JsValue = path.prune(this)

}

/**
 * Represent a Json null value.
 * with Scala 2.10-M7, this code generates WARNING : https://issues.scala-lang.org/browse/SI-6513
 */
case object JsNull extends JsValue

/**
 * Represent a missing Json value.
 */
class JsUndefined(err: => String) extends JsValue {
  def error = err
  override def toString = "JsUndefined(" + err + ")"
}

object JsUndefined {
  def apply(err: => String) = new JsUndefined(err)
  def unapply(o: Object): Boolean = o.isInstanceOf[JsUndefined]
}

/**
 * Represent a Json boolean value.
 */
case class JsBoolean(value: Boolean) extends JsValue

/**
 * Represent a Json number value.
 */
case class JsNumber(value: BigDecimal) extends JsValue

/**
 * Represent a Json string value.
 */
case class JsString(value: String) extends JsValue

/**
 * Represent a Json array value.
 */
case class JsArray(value: Seq[JsValue] = List()) extends JsValue {

  /**
   * Access a value of this array.
   *
   * @param index Element index.
   */
  override def apply(index: Int): JsValue = {
    value.lift(index).getOrElse(JsUndefined("Array index out of bounds in " + this))
  }

  /**
   * Lookup for fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  override def \\(fieldName: String): Seq[JsValue] = value.flatMap(_ \\ fieldName)

  /**
   * Concatenates this array with the elements of an other array.
   */
  def ++(other: JsArray): JsArray =
    JsArray(value ++ other.value)

  /**
   * Append an element to this array.
   */
  def :+(el: JsValue): JsArray = JsArray(value :+ el)
  def append(el: JsValue): JsArray = this.:+(el)

  /**
   * Prepend an element to this array.
   */
  def +:(el: JsValue): JsArray = JsArray(el +: value)
  def prepend(el: JsValue): JsArray = this.+:(el)

}

/**
 * Represent a Json object value.
 */
case class JsObject(fields: Seq[(String, JsValue)]) extends JsValue {

  lazy val value: Map[String, JsValue] = fields.toMap

  /**
   * Return the property corresponding to the fieldName, supposing we have a JsObject.
   *
   * @param fieldName the name of the property to lookup
   * @return the resulting JsValue. If the current node is not a JsObject or doesn't have the property, a JsUndefined will be returned.
   */
  override def \(fieldName: String): JsValue = value.get(fieldName).getOrElse(super.\(fieldName))

  /**
   * Lookup for fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  override def \\(fieldName: String): Seq[JsValue] = {
    value.foldLeft(Seq[JsValue]())((o, pair) => pair match {
      case (key, value) if key == fieldName => o ++ (value +: (value \\ fieldName))
      case (_, value) => o ++ (value \\ fieldName)
    })
  }

  /**
   * Return all keys
   */
  def keys: Set[String] = fields.map(_._1).toSet

  /**
   * Return all values
   */
  def values: Set[JsValue] = fields.map(_._2).toSet

  def fieldSet: Set[(String, JsValue)] = fields.toSet

  /**
   * Merge this object with an other one. Values from other override value of the current object.
   */
  def ++(other: JsObject): JsObject =
    JsObject(fields.filterNot(field => other.keys(field._1)) ++ other.fields)

  /**
   * removes one field from JsObject
   */
  def -(otherField: String): JsObject =
    JsObject(fields.filterNot(_._1 == otherField))

  /**
   * adds one field from JsObject
   */
  def +(otherField: (String, JsValue)): JsObject =
    JsObject(fields :+ otherField)

  /**
   * merges everything in depth and doesn't stop at first level as ++
   * TODO : improve because coding is nasty there
   */
  def deepMerge(other: JsObject): JsObject = {
    def step(fields: List[(String, JsValue)], others: List[(String, JsValue)]): Seq[(String, JsValue)] = {
      others match {
        case List() => fields
        case List(sv) =>
          var found = false
          val newFields = fields match {
            case List() => List(sv)
            case _ => fields.foldLeft(List[(String, JsValue)]()) { (acc, field) =>
              field match {
                case (key, obj: JsObject) if (key == sv._1) =>
                  found = true
                  acc :+ key -> {
                    sv._2 match {
                      case o @ JsObject(_) => obj.deepMerge(o)
                      case js => js
                    }
                  }
                case (key, value) if (key == sv._1) =>
                  found = true
                  acc :+ key -> sv._2
                case (key, value) => acc :+ key -> value
              }
            }
          }

          if (!found) fields :+ sv
          else newFields

        case head :: tail =>
          var found = false
          val headFields = fields match {
            case List() => List(head)
            case _ => fields.foldLeft(List[(String, JsValue)]()) { (acc, field) =>
              field match {
                case (key, obj: JsObject) if (key == head._1) =>
                  found = true
                  acc :+ key -> {
                    head._2 match {
                      case o @ JsObject(_) => obj.deepMerge(o)
                      case js => js
                    }
                  }
                case (key, value) if (key == head._1) =>
                  found = true
                  acc :+ key -> head._2
                case (key, value) => acc :+ key -> value
              }
            }
          }

          if (!found) step(fields :+ head, tail)
          else step(headFields, tail)

      }
    }

    JsObject(step(fields.toList, other.fields.toList))
  }

  override def equals(other: Any): Boolean =
    other match {

      case that: JsObject =>
        (that canEqual this) &&
          fieldSet == that.fieldSet

      case _ => false
    }

  def canEqual(other: Any): Boolean = other.isInstanceOf[JsObject]

  override def hashCode: Int = fieldSet.hashCode()

}