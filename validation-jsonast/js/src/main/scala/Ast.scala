package jto.validation
package jsonast

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Ast {
  val to: Write[JValue, js.Dynamic] = Write[JValue, js.Any] {
    case JNull           => null
    case JObject(value)  => value.mapValues(to.writes).toJSDictionary
    case JArray(value)   => value.map(to.writes).toJSArray
    case JBoolean(value) => value
    case JString(value)  => value
    case JNumber(value) =>
      val d = value.toDouble
      if (d.isNaN || d.isInfinity) null else d
  }.map(_.asInstanceOf[js.Dynamic])

  private val undefined = scala.scalajs.js.undefined
  private case class FunctionInJsonException(path: Path) extends Exception

  private def unsafeAny2JValue(input: Any, path: Path): JValue = input match {
    case null        => JNull
    case s: String   => JString(s)
    case b: Boolean  => JBoolean(b)
    case d: Double   => JNumber(d.toString)
    case `undefined` => JNull

    case a: js.Array[js.Dynamic @unchecked] =>
      JArray(a.map(v => unsafeAny2JValue(v, path \ 0)))

    case o: js.Object =>
      JObject(
        o.asInstanceOf[js.Dictionary[js.Dynamic]]
          .map { case (k, v) => k -> unsafeAny2JValue(v, path \ k) }
          .toMap)

    case _ =>
      // This is a trade off between the various option to handle js.Function in json objects.
      // We could also go one step further and return all the paths which contain functions,
      // but this would imply sequence over Validated, which would throw away the perfs in
      // the general case.
      //
      // This is what other are doing:
      // - The native JSON.stringity is completely silent.
      // - Circe parses then as nulls https://goo.gl/iQ0ANV.
      throw new FunctionInJsonException(path)
  }

  val from: Rule[js.Dynamic, JValue] = Rule { j =>
    try {
      Valid(Path -> unsafeAny2JValue(j, Path))
    } catch {
      case FunctionInJsonException(path) =>
        Invalid(
          Seq(path -> Seq(ValidationError("Json cannot contain functions."))))
    }
  }
}
