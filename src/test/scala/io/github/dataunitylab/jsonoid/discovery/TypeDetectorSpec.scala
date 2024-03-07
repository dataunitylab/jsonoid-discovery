package io.github.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._

class TypeDetectorSpec extends UnitSpec {
  behavior of "TypeDetectorSchema"

  it should "detect an array type" in {
    TypeDetector.detectType(Map("maxContains" -> 3)).value shouldBe "array"
  }

  it should "detect a number type" in {
    TypeDetector.detectType(Map("maximum" -> 3)).value shouldBe "number"
  }

  it should "detect an object type" in {
    TypeDetector.detectType(Map("minProperties" -> 3)).value shouldBe "object"
  }

  it should "detect a string type" in {
    TypeDetector.detectType(Map("format" -> "uri")).value shouldBe "string"
  }
}
