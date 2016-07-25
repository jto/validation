package jto.validation
package jsonast

sealed trait JValue
case object JNull extends JValue
case class JObject (value: Map[String, JValue] = Map.empty) extends JValue
case class JArray  (value: Seq[JValue] = Seq.empty)         extends JValue
case class JBoolean(value: Boolean)                         extends JValue
case class JString (value: String)                          extends JValue
case class JNumber (value: String)                          extends JValue {
  require(JNumber.regex.matcher(value).matches)
}
case class JUndefined(value: String)                        extends JValue

object JNumber {
  val regex = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r.pattern
  def apply(i: Int): JNumber = JNumber(i.toString)
  def apply(l: Long): JNumber = JNumber(l.toString)
  def apply(d: Double): JNumber = JNumber(d.toString)
}
