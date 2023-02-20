package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

/** Resolve references in a schema by annotating all references with the schema
  * that is being referenced. Specifically any instances of
  * [[schemas.ReferencePathProperty]] in [[schemas.ReferenceSchema]] will have
  * an instances of [[schemas.ReferenceObjectProperty]] added.
  */
object ReferenceResolver extends SchemaWalker[Unit] {

  /** Resolve references in a schema.
    *
    * @param schema the schema to resolve references in
    *
    * @return the schema with annotated references
    */
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

  /** Helper for [[resolveReferences]] which finds the schema for a referenced
    * path.
    *
    * @param path the path of the referenced schema
    * @param rootSchema the root schema where definitions are located
    *
    * @return the schema being referenced
    */
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

  /** Used in [[transformSchema]] to resolve references.
    *
    * @param rootSchema the root schema where definitions are located
    */
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
