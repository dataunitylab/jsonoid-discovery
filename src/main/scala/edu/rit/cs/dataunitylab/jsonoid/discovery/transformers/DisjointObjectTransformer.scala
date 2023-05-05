package edu.rit.cs.dataunitylab.jsonoid.discovery
package transformers

import Helpers._
import schemas._

/** Transforms objects schemas with disjoint keys into products
  */
object DisjointObjectTransformer
    extends SchemaWalker[(Seq[Set[String]], ObjectSchema)] {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    val objects = walk(
      schema,
      { case (_, o: ObjectSchema) =>
        (o.properties.get[DependenciesProperty].disjointSets, o)
      }
    ).filter { case (_, (s, _)) => s.size > 1 }

    var finalSchema = schema
    objects.foreach { case (path, (keySets, objSchema)) =>
      val pointer = pathToInexactPointer(path)

      // Build a new product schema with subsets of the original schema
      val disjointSchemas = keySets.map(objSchema.onlyKeys(_))
      val newSchema = ProductSchema.product(disjointSchemas.toList)(p)

      finalSchema = finalSchema.replaceWithSchema(pointer, newSchema)
    }

    finalSchema
  }
}
