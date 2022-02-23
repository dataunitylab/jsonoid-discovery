package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

object MergeAllOfTransformer {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    schema.transformProperties(
      { case p @ ProductSchema(properties) =>
        val schemaTypesProp = p.properties.get[ProductSchemaTypesProperty]
        schemaTypesProp.productType match {
          case AllOf =>
            var schema = schemaTypesProp.baseSchema
            schemaTypesProp.schemaTypes.foreach { s =>
              schema = schema.merge(s, Intersect)
            }
            schema
          case _ => p
        }
      },
      true
    )
  }
}
