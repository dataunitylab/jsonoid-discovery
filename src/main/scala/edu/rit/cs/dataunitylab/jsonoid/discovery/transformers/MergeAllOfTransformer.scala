package io.github.dataunitylab.jsonoid.discovery
package transformers

import schemas._

/** Combine multiple instances of `allOf` into a single schema.
  */
object MergeAllOfTransformer {
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    schema.transformProperties(
      { case ps @ ProductSchema(properties) =>
        val schemaTypesProp = ps.properties.get[ProductSchemaTypesProperty]
        schemaTypesProp.productType match {
          case AllOf =>
            schemaTypesProp.schemaTypes.fold(schemaTypesProp.baseSchema)(
              _.merge(_, Intersect)
            )
          case _ => ps
        }
      },
      true
    )
  }
}
