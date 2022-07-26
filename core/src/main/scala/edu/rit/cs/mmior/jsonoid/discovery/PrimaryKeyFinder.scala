package edu.rit.cs.mmior.jsonoid.discovery

import schemas._
import utils.HyperLogLog

final case class PrimaryKey(path: String)

object PrimaryKeyFinder extends SchemaWalker[HyperLogLog[_]] {
  private val getHLL
      : PartialFunction[(String, JsonSchema[_]), HyperLogLog[_]] = {
    // Get the HyperLogLog object for the specific type
    case (_, i: IntegerSchema) if i.properties.has[IntHyperLogLogProperty] =>
      i.properties.get[IntHyperLogLogProperty].hll
    case (_, n: NumberSchema) if n.properties.has[NumHyperLogLogProperty] =>
      n.properties.get[NumHyperLogLogProperty].hll
    case (_, s: StringSchema) if s.properties.has[StringHyperLogLogProperty] =>
      s.properties.get[StringHyperLogLogProperty].hll
  }

  def findPrimaryKeys(schema: JsonSchema[_]): List[PrimaryKey] = {
    schema match {
      case o: ObjectSchema =>
        // Get the total count of all times the object has occurred
        val totalCount = o.properties.get[FieldPresenceProperty].totalCount

        // Build a map from field names to their HLL objects
        val fieldHLLs = walk(schema, getHLL)

        // Check if the estimated cardinality falls within the
        // bounds that make the field a possible primary key
        val primaryKeys = fieldHLLs.flatMap { case (path, hll) =>
          val count = hll.count()
          val margin = count * 0.05
          if ((count + margin) >= totalCount.toLong) {
            List(PrimaryKey(path))
          } else {
            List()
          }
        }
        primaryKeys.toList
      case _ => List()
    }
  }
}
