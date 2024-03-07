package io.github.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._

class NullSchemaSpec extends UnitSpec {
  behavior of "NullSchema"

  val nullSchema: NullSchema = NullSchema()

  it should "show nulls as a valid type" in {
    nullSchema.isValidType(JNull) shouldBe (true)
  }

  it should "show non-nulls as an invalid type" in {
    nullSchema.isValidType(JString("foo")) shouldBe (false)
  }

  it should "not be compatible with other schemas" in {
    nullSchema.isSubsetOf(StringSchema("foo")) shouldBe false
  }

  it should "should be compatible with another null schema" in {
    nullSchema.isSubsetOf(nullSchema) shouldBe true
  }

  it should "should have only Info-level anomalies with non-null values" in {
    nullSchema.maxAnomalyLevel(JString("foo")).value shouldBe AnomalyLevel.Info
  }

  it should "should not show null values as anomalous" in {
    nullSchema.collectAnomalies(JNull) shouldBe empty
  }
}
