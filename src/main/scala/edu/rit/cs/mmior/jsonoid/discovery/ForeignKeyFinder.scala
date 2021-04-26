package edu.rit.cs.mmior.jsonoid.discovery

import com.sangupta.bloomfilter.impl.InMemoryBloomFilter

import schemas._

final case class ForeignKey(localPath: String, foreignPath: String)

object ForeignKeyFinder {
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  private def collectFiltersWithPrefix(
      prefix: String,
      schema: JsonSchema[_]
  ): Seq[(String, InMemoryBloomFilter[_])] = {
    schema match {
      case o: ObjectSchema =>
        val props = o.properties.get[ObjectTypesProperty].objectTypes
        props.keySet.toSeq.flatMap(key =>
          collectFiltersWithPrefix(prefix + "." + key, props(key))
        )
      case a: ArraySchema =>
        val arrayType = a.properties.get[ItemTypeProperty].itemType match {
          case Left(singleSchema) => singleSchema
          case Right(multipleSchemas) =>
            multipleSchemas.fold(ZeroSchema())(_.merge(_))
        }
        collectFiltersWithPrefix(
          prefix + "[*]",
          arrayType
        )
      case i: IntegerSchema =>
        Seq((prefix, i.properties.get[IntBloomFilterProperty].bloomFilter))
      case n: NumberSchema =>
        Seq((prefix, n.properties.get[NumBloomFilterProperty].bloomFilter))
      case s: StringSchema =>
        Seq((prefix, s.properties.get[StringBloomFilterProperty].bloomFilter))
      case _ => Seq.empty[(String, InMemoryBloomFilter[_])]
    }
  }

  def collectFiltersByPath(
      schema: JsonSchema[_]
  ): Map[String, InMemoryBloomFilter[_]] = {
    collectFiltersWithPrefix("$", schema).toMap
  }

  def findForeignKeys(schema: JsonSchema[_]): List[ForeignKey] = {
    val filters = collectFiltersByPath(schema)

    filters.keySet.toSeq
      .combinations(2)
      .flatMap { case Seq(pathA, pathB) =>
        val filterA = filters(pathA)
        val filterB = filters(pathB)

        @SuppressWarnings(Array("org.wartremover.warts.Var"))
        var keys = Seq.empty[ForeignKey]

        if (filterA.maybeSubsetOf(filterB)) {
          keys = keys ++ Seq(ForeignKey(pathA, pathB))
        }
        if (filterB.maybeSubsetOf(filterA)) {
          keys = keys ++ Seq(ForeignKey(pathB, pathA))
        }

        keys
      }
      .toList
  }
}
