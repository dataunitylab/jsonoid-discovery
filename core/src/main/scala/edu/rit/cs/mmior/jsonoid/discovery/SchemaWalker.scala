package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

trait SchemaWalker[T] {
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
                  extractValues(schema, extractor, s"$prefix[$index]", follow)
                }
            })
          case p: ProductSchema =>
            val types = p.properties.get[ProductSchemaTypesProperty].schemas
            extractSingle(p, extractor, prefix) ++ types.zipWithIndex.flatMap {
              case (schema, index) =>
                extractValues(schema, extractor, s"$prefix[$index]", follow)
            }
          case r: ReferenceSchema =>
            extractSingle(r, extractor, prefix) ++
              (if (r.properties.has[ReferenceObjectProperty]) {
                 val obj = r.properties.get[ReferenceObjectProperty].schema
                 extractValues(
                   schema,
                   extractor,
                   prefix,
                   follow
                 )
               } else {
                 Seq.empty
               })
          case x =>
            extractSingle(x, extractor, prefix)
        })
    } else {
      Seq.empty
    }
  }

  def walk(
      schema: JsonSchema[_],
      extractor: PartialFunction[(String, JsonSchema[_]), T],
      follow: Function1[JsonSchema[_], Boolean] = (s) =>
        !s.isInstanceOf[ReferenceSchema]
  ): Map[String, T] = {
    extractValues(schema, extractor, "$", follow).toMap
  }
}
