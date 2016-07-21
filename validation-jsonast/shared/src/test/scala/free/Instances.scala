package jto.validation.free

import cats._
import jto.validation.jsonast._

/** jsonast specific instances, once things are properly merged into validation-core. */
object Instances {
  implicit val jsValueMonoid: Monoid[JValue] =
    new Monoid[JValue] {
      def empty: JValue = JObject()

      def combine(a: JValue, b: JValue): JValue =
        jto.validation.jsonast.Writes.jsonMonoid.combine(a.asInstanceOf[JObject], b.asInstanceOf[JObject])
    }
}
