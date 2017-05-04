package jto.validation
package v3.tagless

import org.scalatest._

trait WritesSpec[T] extends WordSpec with Matchers {
  val testCases: TestCases[T]
  val grammar: Grammar[T, types.flip[Write]#λ]
}