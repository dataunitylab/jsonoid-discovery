package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

object SchemaGen {
  val p = JsonoidParams().withPropertySet(PropertySets.AllProperties)

  val genBoolSchema = for {
    value <- arbitrary[Boolean]
  } yield BooleanSchema(value)(p)

  val genIntSchema = for {
    value <- arbitrary[BigInt]
  } yield IntegerSchema(value)(p)

  val genNullSchema = Gen.const(NullSchema())

  val genNumberSchema = for {
    value <- arbitrary[BigDecimal]
  } yield NumberSchema(value)(p)

  val genStringSchema = for {
    value <- arbitrary[String]
  } yield StringSchema(value)(p)

  // Note: genNumberSchema is excluded for now since we can't
  //       serialize and deserialize and get the same values
  val genPrimitiveSchema =
    Gen.oneOf(genBoolSchema, genIntSchema, genNullSchema, genStringSchema)

  val genListSchema = for {
    itemSchema <- genPrimitiveSchema
  } yield ArraySchema.array(itemSchema)(p)

  val genTupleSchema = for {
    itemSchemas <- Gen.listOf(genPrimitiveSchema)
  } yield ArraySchema.tuple(itemSchemas)(p)

  val genArraySchema = Gen.oneOf(genListSchema, genTupleSchema)

  val genObjectSchema = for {
    objectSchemas <- Gen.mapOf(Gen.zip(arbitrary[String], genPrimitiveSchema))
  } yield ObjectSchema(objectSchemas)(p)

  val genAnySchema: Gen[JsonSchema[_]] = Gen.oneOf(
    genObjectSchema,
    genTupleSchema,
    genListSchema,
    genPrimitiveSchema
  )
}
