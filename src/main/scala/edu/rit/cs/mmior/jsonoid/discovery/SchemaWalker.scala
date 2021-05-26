package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

trait SchemaWalker[T] {
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  private def extractValues(
      schema: JsonSchema[_],
      extractor: PartialFunction[JsonSchema[_], T],
      prefix: String = "$"
  ): Seq[(String, T)] = {
    schema match {
      case o: ObjectSchema =>
        val props = o.properties.get[ObjectTypesProperty].objectTypes
        props.keySet.toSeq.flatMap(key =>
          extractValues(props(key), extractor, prefix + "." + key)
        )
      case a: ArraySchema =>
        val arrayType = a.properties.get[ItemTypeProperty].itemType match {
          case Left(singleSchema) => singleSchema
          case Right(multipleSchemas) =>
            multipleSchemas.fold(ZeroSchema())(_.merge(_))
        }
        extractValues(arrayType, extractor, prefix + "[*]")
      case x =>
        if (extractor.isDefinedAt(x)) {
          Seq((prefix, extractor(x)))
        } else {
          Seq.empty[(String, T)]
        }
    }
  }

  def walk(
      schema: JsonSchema[_],
      extractor: PartialFunction[JsonSchema[_], T]
  ): Map[String, T] = {
    extractValues(schema, extractor, "$").toMap
  }
}
