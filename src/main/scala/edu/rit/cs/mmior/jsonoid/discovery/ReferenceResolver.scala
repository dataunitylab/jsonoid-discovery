package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

object ReferenceResolver extends SchemaWalker[Unit] {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    walk(schema, resolveReferences(schema))
    schema
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  private def resolvePath(
      path: String,
      rootSchema: ObjectSchema
  ): JsonSchema[_] = {
    val strippedPath = if (path.startsWith("#")) {
      path.substring(1)
    } else {
      path
    }

    val defs = rootSchema.properties
      .getOrNone[DefinitionsProperty]
      .map(_.definitions)
      .getOrElse(Map.empty)
    strippedPath.split("/") match {
      case Array("", "$defs", defn)       => defs(defn)
      case Array("", "definitions", defn) => defs(defn)
      case Array("", "$defs", _) | Array("", "definitions", _) =>
        throw new UnsupportedOperationException(
          "can't resolve nested definition"
        )
      case _ =>
        rootSchema.findByPointer(strippedPath).get
    }
  }

  private def resolveReferences(
      rootSchema: JsonSchema[_]
  ): PartialFunction[(String, JsonSchema[_]), Unit] = {
    case (_, r: ReferenceSchema)
        if !r.properties.has[ReferenceObjectProperty] =>
      r.properties.add(
        ReferenceObjectProperty(
          resolvePath(
            r.properties.get[ReferencePathProperty].path,
            rootSchema.asInstanceOf[ObjectSchema]
          )
        )
      )
  }
}
