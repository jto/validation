package jto.validation
package jsonast

sealed trait JValue
final case object JNull extends JValue
final case class JObject (value: Map[String, JValue] = Map.empty) extends JValue
final case class JArray  (value: Seq[JValue] = Seq.empty)         extends JValue
final case class JBoolean(value: Boolean)                         extends JValue
final case class JString (value: String)                          extends JValue
final case class JNumber (value: String)                          extends JValue {
  require(JNumber.regex.matcher(value).matches)
}

object JNumber {
  val regex = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r.pattern
  def apply(i: Int): JNumber = JNumber(i.toString)
  def apply(l: Long): JNumber = JNumber(l.toString)
  def apply(d: Double): JNumber = JNumber(d.toString)
}
