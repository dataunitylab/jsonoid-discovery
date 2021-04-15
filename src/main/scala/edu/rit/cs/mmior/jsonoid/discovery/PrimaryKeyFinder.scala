package edu.rit.cs.mmior.jsonoid.discovery

import schemas._
import utils.HyperLogLog

final case class PrimaryKey(path: String)

object PrimaryKeyFinder {
  private def getHLL(schema: JsonSchema[_]): Option[HyperLogLog] = {
    // Get the HyperLogLog object for the specific type
    schema match {
      case i: IntegerSchema =>
        Some(i.properties.get[IntHyperLogLogProperty].hll)
      case n: NumberSchema =>
        Some(n.properties.get[NumHyperLogLogProperty].hll)
      case s: StringSchema =>
        Some(s.properties.get[StringHyperLogLogProperty].hll)
      case _ => None
    }
  }

  def findPrimaryKeys(schema: JsonSchema[_]): List[PrimaryKey] = {
    schema match {
      case o: ObjectSchema =>
        // Get the total count of all times the object has occurred
        val totalCount = o.properties.get[FieldPresenceProperty].totalCount

        // Build a map from field names to their HLL objects
        val objectTypes = o.properties.get[ObjectTypesProperty].objectTypes
        val fieldHLLs = objectTypes.mapValues(getHLL).collect {
          case (k, Some(v)) => k -> v
        }

        // Check if the estimated cardinality falls within the
        // bounds that make the field a possible primary key
        val primaryKeys = fieldHLLs.flatMap { case (path, hll) =>
          val count = hll.count()
          val margin = count * hll.getStandardError()
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
