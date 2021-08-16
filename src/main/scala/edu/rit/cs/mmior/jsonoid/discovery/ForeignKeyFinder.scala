package edu.rit.cs.mmior.jsonoid.discovery

import com.sangupta.bloomfilter.impl.RoaringBloomFilter

import schemas._

final case class ForeignKey(localPath: String, foreignPath: String)

object ForeignKeyFinder extends SchemaWalker[RoaringBloomFilter[_]] {
  def collectFiltersByPath(
      schema: JsonSchema[_]
  ): Map[String, RoaringBloomFilter[_]] = {
    val extractor: PartialFunction[JsonSchema[_], RoaringBloomFilter[_]] = {
      case i: IntegerSchema if
      i.properties.has[IntBloomFilterProperty] =>
        i.properties.get[IntBloomFilterProperty].bloomFilter
      case n: NumberSchema if
      n.properties.has[NumBloomFilterProperty] =>
        n.properties.get[NumBloomFilterProperty].bloomFilter
      case s: StringSchema if
      s.properties.has[StringBloomFilterProperty] =>
        s.properties.get[StringBloomFilterProperty].bloomFilter
    }
    walk(schema, extractor).toMap
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
