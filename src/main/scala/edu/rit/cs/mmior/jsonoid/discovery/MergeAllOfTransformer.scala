package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

object MergeAllOfTransformer {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    schema.transformProperties(
      { case ps @ ProductSchema(properties) =>
        val schemaTypesProp = ps.properties.get[ProductSchemaTypesProperty]
        schemaTypesProp.productType match {
          case AllOf =>
            var schema = schemaTypesProp.baseSchema
            schemaTypesProp.schemaTypes.foreach { s =>
              schema = schema.merge(s, Intersect)
            }
            schema
          case _ => ps
        }
      },
      true
    )
  }
}
