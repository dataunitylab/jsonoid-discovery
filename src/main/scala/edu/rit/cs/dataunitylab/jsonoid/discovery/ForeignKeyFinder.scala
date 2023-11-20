package io.github.dataunitylab.jsonoid.discovery

import schemas._
import utils.BloomFilter

/** Represents a discovered foreign key.
  *
  * @constructor Create a new foreign key with two paths.
  * @param localPath the path of the referencing property
  * @param foreignPath the path of the referenced property
  */
final case class ForeignKey(localPath: String, foreignPath: String)

/** Find possible foreign keys by comparing values discovered from the schema.
  */
object ForeignKeyFinder extends SchemaWalker[BloomFilter[_]] {

  /** Collect Bloom filters of all types in the schemas.
    *
    * @param schemas the schemas to search for filters
    * @return a map from paths to Bloom filters
    */
  private def collectFiltersByPath(
      schema: JsonSchema[_]
  ): Map[String, BloomFilter[_]] = {
    val extractor: PartialFunction[(String, JsonSchema[_]), BloomFilter[_]] = {
      case (_, i: IntegerSchema) if i.properties.has[IntBloomFilterProperty] =>
        i.properties.get[IntBloomFilterProperty].bloomFilter
      case (_, n: NumberSchema) if n.properties.has[NumBloomFilterProperty] =>
        n.properties.get[NumBloomFilterProperty].bloomFilter
      case (_, s: StringSchema)
          if s.properties.has[StringBloomFilterProperty] =>
        s.properties.get[StringBloomFilterProperty].bloomFilter
    }
    walk(schema, extractor).toMap
  }

  /** Returns a list of possible foreign keys.
    *
    * @param schema the schema to search for foreign keys
    */
  def findForeignKeys(schema: JsonSchema[_]): List[ForeignKey] = {
    val filters = collectFiltersByPath(schema)

    // Check all possible pairs of keys (in any order)
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
