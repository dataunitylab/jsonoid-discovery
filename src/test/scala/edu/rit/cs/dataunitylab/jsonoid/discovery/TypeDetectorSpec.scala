package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._

class TypeDetectorSpec extends UnitSpec {
  behavior of "TypeDetectorSchema"

  it should "detect an array type" in {
    TypeDetector.detectType(Map("maxContains" -> 3)) shouldBe Some("array")
  }

  it should "detect a number type" in {
    TypeDetector.detectType(Map("maximum" -> 3)) shouldBe Some("number")
  }

  it should "detect an object type" in {
    TypeDetector.detectType(Map("minProperties" -> 3)) shouldBe Some("object")
  }

  it should "detect a string type" in {
    TypeDetector.detectType(Map("format" -> "url")) shouldBe Some("string")
  }
}
