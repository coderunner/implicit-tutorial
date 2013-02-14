package edu

import org.scalatest.FunSuite
import edu.JsonObject._

/**
 * Test class for JsonObject
 */
class TestJsonObject extends FunSuite {

  val jsonObjectMap = Map("name" :-> "joe", "age" :-> 34)
  val expectedJson = "{\n" +
    "name : \"joe\",\n" +
    "age : " + 34 + "\n" +
    "}\n"

  test("should render json") {
    val jsonObject = new JsonObject(jsonObjectMap)

    assert(expectedJson === renderJson(jsonObject))
  }

//  test("should not compile") {
//    intercept[RuntimeException] {
//      renderJson(new JsonObject(Map("name" :-> "joe", "height" :-> 2.67)))
//    }
//  }

  test("nice if could render from a map directly") {
    assert(expectedJson === renderJson(jsonObjectMap))
  }

}
