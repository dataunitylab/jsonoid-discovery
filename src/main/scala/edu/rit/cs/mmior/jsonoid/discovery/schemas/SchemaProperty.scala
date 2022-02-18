package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s._

trait SchemaProperty[T, S <: SchemaProperty[T, _]] {
  def mergeable: Boolean = true

  def intersectMerge(prop: S)(implicit er: EquivalenceRelation): S =
    unionMerge(prop)(er)

  // XXX: This should really take and return the same
  //      concrete type, but it does not currently
  def unionMerge(prop: S)(implicit er: EquivalenceRelation): S

  def mergeOnlySameType(
      prop: SchemaProperty[T, _],
      mergeType: MergeType
  )(implicit er: EquivalenceRelation): S = {
    // XXX This only works if the S is the same as the wildcard type
    //     but this is needed since we can't cast to an unknown type
    mergeType match {
      case Union     => unionMerge(prop.asInstanceOf[S])
      case Intersect => intersectMerge(prop.asInstanceOf[S])
    }
  }

  def mergeValue(value: T)(implicit er: EquivalenceRelation): S

  // This must be implemented for any property which contains schema objects
  // Currently this is only three properties:
  // * ObjectTypesProperty in ObjectSchema
  // * PatternTypesProperty in ObjectSchema
  // * ItemTypeProperty in ArraySchema
  // * ProductSchemaTypesProperty in ProductSchema
  def transform(transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]): S =
    this.asInstanceOf[S]

  def toJson: JObject

  def isAnomalous(value: JValue, path: String = "$"): Boolean =
    !collectAnomalies(value, path).isEmpty

  def collectAnomalies(value: JValue, path: String = "$"): Seq[Anomaly] =
    Seq.empty
}
