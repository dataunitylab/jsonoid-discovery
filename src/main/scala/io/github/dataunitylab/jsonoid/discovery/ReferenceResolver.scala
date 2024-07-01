package io.github.dataunitylab.jsonoid.discovery

import schemas._
import utils.JsonPointer

/** Resolve references in a schema by annotating all references with the schema
  * that is being referenced. Specifically any instances of
  * [[schemas.ReferencePointerProperty]] in [[schemas.ReferenceSchema]] will
  * have an instance of [[schemas.ReferenceObjectProperty]] added.
  */
object ReferenceResolver extends SchemaWalker[Unit] {

  /** Resolve references in a schema.
    *
    * @param schema
    *   the schema to resolve references in
    *
    * @return
    *   the schema with annotated references
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
    * pointer.
    *
    * @param pointer
    *   the pointer of the referenced schema
    * @param rootSchema
    *   the root schema where definitions are located
    *
    * @return
    *   the schema being referenced
    */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  private def resolvePointer(
      pointer: String,
      rootSchema: JsonSchema[_]
  ): JsonSchema[_] = {
    val strippedPointer = JsonPointer.fromString(
      if (pointer.startsWith("#")) pointer.substring(1) else pointer
    )

    val defs = rootSchema.definitions
    strippedPointer.parts match {
      case List("$defs", defn)       => defs(defn)
      case List("definitions", defn) => defs(defn)
      case List("$defs", _) | List("definitions", _) =>
        throw new UnsupportedOperationException(
          "can't resolve nested definition"
        )
      case _ =>
        rootSchema.findByPointer(strippedPointer).get
    }
  }

  /** Used in [[transformSchema]] to resolve references.
    *
    * @param rootSchema
    *   the root schema where definitions are located
    */
  private def resolveReferences(
      rootSchema: JsonSchema[_]
  ): PartialFunction[(String, JsonSchema[_]), Unit] = {
    case (_, r: ReferenceSchema)
        if !r.properties.has[ReferenceObjectProperty] =>
      r.properties.add(
        ReferenceObjectProperty(
          resolvePointer(
            r.properties.get[ReferencePointerProperty].pointer,
            rootSchema
          )
        )
      )
  }
}
