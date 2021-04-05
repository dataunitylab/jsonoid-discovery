package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class ProductSchemaSpec extends UnitSpec {
  behavior of "ProductSchema"

  private val schema1 = BooleanSchema()
  private val schema2 = IntegerSchema()
  private val productSchema = ProductSchema(schema1).properties.merge(schema2)

  it should "track required properties" in {
    productSchema should contain (ProductSchemaTypesProperty(Map(schema1.getClass
      -> schema1, schema2.getClass -> schema2)))
  }
}
