package edu.rit.cs.dataunitylab.jsonoid.discovery

import Helpers._
import schemas._

/** Use entropy calculations to detect objects with dynamic keys
  *
  *  This implements the approach from "Reducing Ambiguity in Json Schema
  *  Discovery" by Spoth et al.
  *
  *  https://dl.acm.org/doi/abs/10.1145/3448016.3452801
  */
object DynamicObjectTransformer extends SchemaWalker[ObjectSchema] {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    val objects = walk(schema, { case (_, o: ObjectSchema) => o }).filter {
      case (_, o: ObjectSchema) =>
        val objectTypes = o.properties.get[ObjectTypesProperty].objectTypes

        // Check if all values are of the same basic type
        val schemaTypes = objectTypes.view.map(_._2.schemaType)
        val sameType = schemaTypes.forall(_ === schemaTypes.head)

        // Calculate key entropy
        val presenceProp = o.properties.get[FieldPresenceProperty]
        val entropy = presenceProp.fieldPresence.map { case (_, v: BigInt) =>
          val prob =
            (BigDecimal(v) / BigDecimal(presenceProp.totalCount)).toDouble
          -prob * scala.math.log(prob)
        }.sum

        // Keep all objects with the same type and key entropy > 1
        sameType && entropy > 1.0
    }

    var finalSchema = schema
    objects.foreach { case (path, objSchema) =>
      // Construct thew new final type
      val pointer = pathToInexactPointer(path)
      val objectTypes =
        objSchema.properties.get[ObjectTypesProperty].objectTypes
      val valueType = objectTypes.values.fold(ZeroSchema())(_.merge(_))

      // Generate the new schema
      val props = SchemaProperties.empty[Map[String, JsonSchema[_]]]
      props.add(DynamicObjectTypeProperty(valueType))
      val newSchema = DynamicObjectSchema(props)(p)

      if (pointer.isEmpty) {
        finalSchema = newSchema
      } else {
        finalSchema = finalSchema.replaceWithSchema(pointer, newSchema)
      }
    }

    finalSchema
  }
}
