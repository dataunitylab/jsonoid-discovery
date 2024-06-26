package io.github.dataunitylab.jsonoid.discovery

import scala.reflect.ClassTag

import Helpers._
import schemas._

final case class Incompatibility[T](path: String, property: ClassTag[T])

object IncompatibilityCollector {

  /** Give a more detailed incompatibility by manually checking the type of
    * these two schemas.
    *
    * @param path
    *   the path of the potential incompatibility
    * @param s1
    *   the first schema
    * @param s2
    *   the second schema
    * @param tag
    *   the type of incompatibility to report
    *
    * @return
    *   an sequence of a single incompatibility if one exists
    */
  def typeIncompat(
      path: String,
      s1: JsonSchema[_],
      s2: JsonSchema[_],
      tag: ClassTag[_]
  ): Seq[Incompatibility[_]] = if (
    s1.schemaType === s2.schemaType ||
    s1.schemaType === "any" ||
    s2.schemaType === "any"
  ) {
    Seq()
  } else {
    Seq(Incompatibility(path, tag))
  }

  /** Check for incompatibilities at a given path.
    *
    * @param base
    *   the base schema
    * @param other
    *   the other schema to check for incompatibilities
    * @param path
    *   the path of the potential incompatibility
    *
    * @return
    *   a sequence of incompatibilities at the given path
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.Recursion",
      "org.wartremover.warts.TraversableOps"
    )
  )
  private def findIncompatibilitiesAtPath(
      base: JsonSchema[_],
      other: JsonSchema[_],
      skipIfSubset: Boolean,
      path: String
  ): Seq[Incompatibility[_]] = {
    (base, other) match {
      case (_, _) if (base.isSubsetOf(other) && skipIfSubset) =>
        Seq()

      case (o1: ObjectSchema, o2: ObjectSchema) =>
        // Show incompatibilities for each key in the object
        val t1 = o1.properties.get[ObjectTypesProperty].objectTypes
        val t2 = o2.properties.get[ObjectTypesProperty].objectTypes
        val typeIncompats =
          t1.keySet.intersect(t2.keySet).toSeq.flatMap { key =>
            val objPath = s"$path.$key"
            typeIncompat(
              objPath,
              t1(key),
              t2(key),
              ClassTag(classOf[ObjectTypesProperty])
            ) ++ findIncompatibilitiesAtPath(
              t1(key),
              t2(key),
              skipIfSubset,
              objPath
            )
          }

        // We dealt with the recursive case above, so no recursion here
        typeIncompats ++ o1
          .findIncompatibilities(o2, false)
          .map(Incompatibility(path, _))

      case (ps1: ProductSchema, ps2: ProductSchema) =>
        val t1 = ps1.properties.get[ProductSchemaTypesProperty].schemas
        val t2 = ps2.properties.get[ProductSchemaTypesProperty].schemas

        // Show incompatibilities from the closest compatible schemas
        t2.flatMap { s2 =>
          val compatTypes = t1.filter(s1 =>
            s1.schemaType === "any" || s1.schemaType === s2.schemaType
          )
          if (compatTypes.nonEmpty) {
            compatTypes
              .map(s1 => s1.properties.findIncompatibilities(s2.properties))
              .minBy(_.length)
          } else {
            Seq(ClassTag(classOf[ProductSchemaTypesProperty]))
          }
        }.map(Incompatibility(path, _))

      case (ps: ProductSchema, _) =>
        // Collect incompatibilities from the most compatible schema
        val types = ps.properties
          .get[ProductSchemaTypesProperty]
          .schemas
          .filter(s =>
            s.schemaType === "any" || s.schemaType === other.schemaType
          )
        if (types.isEmpty) {
          Seq(
            Incompatibility(path, ClassTag(classOf[ProductSchemaTypesProperty]))
          )
        } else {
          types.zipWithIndex
            .map { case (schema, index) =>
              findIncompatibilitiesAtPath(schema, other, skipIfSubset, path)
            }
            .minBy(_.length)
        }

      case (a1: ArraySchema, a2: ArraySchema) =>
        val t1 = a1.properties.get[ItemTypeProperty].itemType
        val t2 = a2.properties.get[ItemTypeProperty].itemType

        // We deal with the recursive case below, so skip it here
        val arrayIncompats =
          a1.findIncompatibilities(a2, false).map(Incompatibility(path, _))

        (t1, t2) match {
          case (Left(s1), Left(s2)) =>
            // Check compatibility of array items
            typeIncompat(
              path,
              s1,
              s2,
              ClassTag(classOf[ItemTypeProperty])
            ) ++ arrayIncompats ++ findIncompatibilitiesAtPath(
              s1,
              s2,
              skipIfSubset,
              path
            )

          case (Left(schema), Right(schemas)) =>
            // Combine the tuple schemas and check against the array item
            val oneSchema = schemas.fold(ZeroSchema())(_.merge(_, Union))
            arrayIncompats ++ findIncompatibilitiesAtPath(
              schema,
              oneSchema,
              skipIfSubset,
              path
            )

          case (Right(schemas1), Right(schemas2))
              if (schemas1.length == schemas2.length) =>
            // Compare items in tuple schemas pairwise
            arrayIncompats ++ schemas1.zip(schemas2).zipWithIndex.flatMap {
              case ((s1, s2), index) =>
                typeIncompat(
                  path,
                  s1,
                  s2,
                  ClassTag(classOf[ItemTypeProperty])
                ) ++ s1.properties
                  .findIncompatibilities(s2.properties)
                  .map(Incompatibility(path, _))
            }

          // We can't provide more detail on incompatibility here
          case (_, _) =>
            base.properties
              .findIncompatibilities(other.properties)
              .map(Incompatibility(path, _))
        }

      case (_, _) =>
        base.properties
          .findIncompatibilities(other.properties)
          .map(Incompatibility(path, _))
    }
  }

  /** Check for incompatibilities across an entire schema..
    *
    * @param base
    *   the base schema
    * @param other
    *   the other schema to check for incompatibilities
    *
    * @return
    *   a sequence of incompatibilities at the given path
    */
  def findIncompatibilities(
      base: JsonSchema[_],
      other: JsonSchema[_],
      skipIfSubset: Boolean = true
  ): Seq[Incompatibility[_]] = {
    val incompats = findIncompatibilitiesAtPath(base, other, skipIfSubset, "$")

    // Incompatiblities should be found if the schema is not
    // a subset, otherwise no incompatibilities should be found
    assert(base.isSubsetOf(other) === incompats.isEmpty)

    incompats
  }
}
