package jto.validation
package mson

import org.scalatest._
import jto.validation.free._

case class Example1(
  id: Int,
  name: String,
  price: Double,
  tags: Option[List[String]]
)

case class Example2(
  address: Option[List[String]]
)

case class Item(
  name: Option[String],
  description: Option[String]
)

case class Example3(
  items: Option[List[Item]]
)

class MsonSpec extends WordSpec with Matchers {
  "Mson" should {
    "conform to example1" in {
      val freeExample1 = UFree.gen[Example1]()
      val api = freeExample1.materialize[Mson].api

      api shouldBe MdObject(Map(
        "id"    -> MdLeaf("number"),
        "name"  -> MdLeaf("string"),
        "price" -> MdLeaf("number"),
        "tags"  -> MdArray(MdLeaf("string"), false)
      ))

      api.toString shouldBe """
        |- (object, required)
        |  - id (number, required)
        |  - name (string, required)
        |  - price (number, required)
        |  - tags (array)
        |    - (string, required)
        """.trim.stripMargin
    }

    "conform to example2" in {
      val freeExample2 = UFree.gen[Example2]()
      val api = freeExample2.materialize[Mson].api

      api shouldBe MdObject(Map(
        "address" -> MdArray(MdLeaf("string"), false)
      ))

      api.toString shouldBe """
        |- (object, required)
        |  - address (array)
        |    - (string, required)
        """.trim.stripMargin
    }

    "conform to example3" in {
      val freeItem = UFree.gen[Item]()
      implicit val itemApi: Mson[Item] = freeItem.materialize[Mson]

      val freeExample3 = UFree.gen[Example3]()
      val api = freeExample3.materialize[Mson].api

      api shouldBe MdObject(Map(
        "items" -> MdArray(MdObject(Map(
          "name"        -> MdLeaf("string", false),
          "description" -> MdLeaf("string", false)
        )), false)
      ))

      api.toString shouldBe """
        |- (object, required)
        |  - items (array)
        |    - (object, required)
        |      - name (string)
        |      - description (string)
        """.trim.stripMargin
    }
  }
}
