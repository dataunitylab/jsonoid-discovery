package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

object ReferenceResolver extends SchemaWalker[Unit] {
  @SuppressWarnings(
    Array("org.wartremover.warts.While", "org.wartremover.warts.Var")
  )
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    var refs = Map.empty[String, Unit]
    var visited: Set[JsonSchema[_]] = Set.empty
    do {
      refs = walk(
        schema,
        resolveReferences(schema),
        (s) => {
          if (s.isInstanceOf[ReferenceSchema]) {
            val found = visited.contains(s)
            visited += s

            !found
          } else {
            true
          }
        }
      )
    } while (!refs.isEmpty)
    schema
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  private def resolvePath(
      path: String,
      rootSchema: JsonSchema[_]
  ): JsonSchema[_] = {
    val strippedPath = if (path.startsWith("#")) {
      path.substring(1)
    } else {
      path
    }

    val defs = rootSchema.definitions
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
            rootSchema
          )
        )
      )
  }
}
