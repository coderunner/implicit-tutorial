package edu

import org.scalatest.FunSuite

/**
 * Test class for JsonObject
 */
class TestJsonObject extends FunSuite {

  val jsonObjectMap = Map("name" -> "joe", "age" -> 34)
  val expectedJson = "{\n" +
    "name : \"joe\",\n" +
    "age : " + 34 + "\n" +
    "}\n"

  test("should render json") {
    val jsonObject = new JsonObject(jsonObjectMap)

    assert(expectedJson === jsonObject.render())
  }

  test("unsupported type should throw exception") {
    intercept[RuntimeException] {
      new JsonObject(Map("name" -> "joe", "height" -> 2.67)).render()
    }
  }

  //  test("nice if could render from a map directly") {
  //    assert(expectedJson === jsonObjectMap.render())
  //  }

}
