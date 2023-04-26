package edu.rit.cs.dataunitylab.jsonoid.discovery

import schemas._

/** A helper trait for any class that needs to walk a schema and build a map
  * of values collected from the properties.
  */
trait SchemaWalker[T] {

  /** Helper for [[extractValues]] that extracts a sequence of values from a
    *  single schema without recursion.
    */
  private def extractSingle(
      schema: JsonSchema[_],
      extractor: PartialFunction[(String, JsonSchema[_]), T],
      path: String
  ): Seq[(String, T)] = {
    if (extractor.isDefinedAt((path, schema))) {
      Seq((path, extractor(path, schema)))
    } else {
      Seq.empty[(String, T)]
    }
  }

  /** Helper for [[walk]] that recursively extracts values from a schema.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  private def extractValues(
      schema: JsonSchema[_],
      extractor: PartialFunction[(String, JsonSchema[_]), T],
      prefix: String = "$",
      follow: Function1[JsonSchema[_], Boolean]
  ): Seq[(String, T)] = {
    if (follow(schema)) {
      (schema.definitions.values
        .flatMap(extractValues(_, extractor, prefix, follow))
        .toSeq) ++
        (schema match {
          case d: DynamicObjectSchema =>
            val value = d.properties.get[DynamicObjectTypeProperty].valueType
            extractSingle(d, extractor, prefix) ++ extractValues(
              value,
              extractor,
              prefix + ".*",
              follow
            )

          case o: ObjectSchema =>
            val props = o.properties.get[ObjectTypesProperty].objectTypes
            val extractedProps =
              extractSingle(o, extractor, prefix) ++ props.keySet.toSeq.flatMap(
                key =>
                  extractValues(
                    props(key),
                    extractor,
                    prefix + "." + key,
                    follow
                  )
              )

            // XXX patternProperties are represented using the regex string
            val patternProps = o.properties
              .getOrNone[PatternTypesProperty]
              .map(_.patternTypes)
              .getOrElse(Map.empty)
            val extractedPatternProps = patternProps.keySet.toSeq
              .flatMap(key =>
                extractValues(
                  patternProps(key),
                  extractor,
                  prefix + "." + key.toString,
                  follow
                )
              )

            extractedProps.toSeq ++ extractedPatternProps.toSeq
          case a: ArraySchema =>
            extractSingle(a, extractor, prefix) ++ (a.properties
              .get[ItemTypeProperty]
              .itemType match {
              case Left(singleSchema) =>
                extractValues(singleSchema, extractor, prefix + "[*]", follow)
              case Right(multipleSchemas) =>
                multipleSchemas.zipWithIndex.flatMap { case (schema, index) =>
                  extractValues(
                    schema,
                    extractor,
                    s"$prefix[${index.toString}]",
                    follow
                  )
                }
            })
          case p: ProductSchema =>
            val types = p.properties.get[ProductSchemaTypesProperty].schemas
            extractSingle(p, extractor, prefix) ++ types.zipWithIndex.flatMap {
              case (schema, index) =>
                extractValues(
                  schema,
                  extractor,
                  s"$prefix[${index.toString}]",
                  follow
                )
            }
          case r: ReferenceSchema =>
            extractSingle(r, extractor, prefix) ++
              (r.properties.getOrNone[ReferenceObjectProperty] match {
                case Some(objProp) =>
                  extractValues(
                    objProp.schema,
                    extractor,
                    prefix,
                    follow
                  )
                case None => Seq.empty
              })
          case x =>
            extractSingle(x, extractor, prefix)
        })
    } else {
      Seq.empty
    }
  }

  /** Recursively extracts values from a schema to build a map from paths to the
    * extracted values.
    *
    * @param schema the schema to extract values from
    * @param extractor a function that extracts values from a single schema
    * @param follow a function that returns true if the traversal should continue
    *
    * @return a map from paths to the extracted values
    */
  def walk(
      schema: JsonSchema[_],
      extractor: PartialFunction[(String, JsonSchema[_]), T],
      follow: Function1[JsonSchema[_], Boolean] = (s) =>
        !s.isInstanceOf[ReferenceSchema]
  ): Map[String, T] = {
    extractValues(schema, extractor, "$", follow).toMap
  }
}
