/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api.libs.json

/**
 * Helper functions to handle JsValues.
 */
object Json {

  /**
   * Convert a JsValue to its string representation.
   *
   * {{{
   * scala> Json.stringify(Json.obj(
   *   "field1" -> Json.obj(
   *     "field11" -> "value11",
   *     "field12" -> Json.arr("alpha", 123L)
   *   )
   * ))
   * res0: String = {"field1":{"field11":"value11","field12":["alpha",123]}}
   *
   * scala> Json.stringify(res0)
   * res1: String = {"field1":{"field11":"value11","field12":["alpha",123]}}
   * }}}
   *
   * @param json the JsValue to convert
   * @return a String with the json representation
   */
  // def stringify(json: JsValue): String = JacksonJson.generateFromJsValue(json)

  /**
   * Convert a JsValue to its pretty string representation using default Jackson
   * pretty printer (line feeds after each fields and 2-spaces indentation).
   *
   * {{{
   * scala> Json.stringify(Json.obj(
   *   "field1" -> Json.obj(
   *     "field11" -> "value11",
   *     "field12" -> Json.arr("alpha", 123L)
   *   )
   * ))
   * res0: String = {"field1":{"field11":"value11","field12":["alpha",123]}}
   *
   * scala> Json.prettyPrint(res0)
   * res1: String =
   * {
   *   "field1" : {
   *     "field11" : "value11",
   *     "field12" : [ "alpha", 123 ]
   *   }
   * }
   * }}}
   *
   * @param json the JsValue to convert
   * @return a String with the json representation
   */
  // def prettyPrint(json: JsValue): String = JacksonJson.prettyPrint(json)

  import play.api.data.mapping._
  sealed trait JsValueWrapper extends NotNull
  private case class JsValueWrapperImpl(field: JsValue) extends JsValueWrapper
  import scala.language.implicitConversions
  implicit def toJsFieldJsValueWrapper[T](field: T)(implicit w: Write[T, JsValue]): JsValueWrapper = JsValueWrapperImpl(w.writes(field))

  def obj(fields: (String, JsValueWrapper)*): JsObject = JsObject(fields.map(f => (f._1, f._2.asInstanceOf[JsValueWrapperImpl].field)))
  def arr(fields: JsValueWrapper*): JsArray = JsArray(fields.map(_.asInstanceOf[JsValueWrapperImpl].field))

}
