package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

import org.json4s._

trait SchemaProperty[T] {
  type S <: SchemaProperty[T]

  /** Build a new instance of the property with default parameters
    *
    * @param p the JSONoid parameters to use during construction
    */
  def newDefault()(implicit p: JsonoidParams): SchemaProperty[T]

  /** Whether it is possible to merge this property
    *
    * @return true if the property is mergeable, false otherwise
    */
  def mergeable: Boolean = true

  /** True if this property is for informational purposes only
    * (not for validation), and false otherwise
    */
  def isInformational: Boolean = false

  /** Merge with another property using the intersect strategy
    *
    * For properties used in validation, the property should
    * validate against the intersection of the values used to
    * discover each property.
    *
    * @param prop the property to merge with
    * @param p the JSONoid paramters
    * @return the merged property
    */
  def intersectMerge(prop: S)(implicit p: JsonoidParams): S =
    unionMerge(prop)(p)

  /** Merge with another property using the union strategy.
    *
    * For properties used in validation, the property should
    * validate against the union of the values used to
    * discover each property.
    *
    * @param prop the property to merge with
    * @param p the JSONoid paramters
    * @return the merged property
    */
  def unionMerge(prop: S)(implicit p: JsonoidParams): S

  def mergeOnlySameType(
      prop: SchemaProperty[T],
      mergeType: MergeType
  )(implicit p: JsonoidParams): S = {
    // XXX This only works if the S is the same as the wildcard type
    //     but this is needed since we can't cast to an unknown type
    mergeType match {
      case Union     => unionMerge(prop.asInstanceOf[S])
      case Intersect => intersectMerge(prop.asInstanceOf[S])
    }
  }

  /** Update the property by merging in a single related value
    *
    * @param value the value to merge
    * @param p the JSONoid parameters to use during merging
    * @return an updated property with the merged value
    */
  def mergeValue(value: T)(implicit p: JsonoidParams): S

  /** Recursively transform properties nested under this property.
    *
    * This must be implemented for any property which contain schema objects
    * Currently this is only these properties:
    *  * DynamicObjectTypeProperty in DynamicObjectSchema
    *  * ObjectTypesProperty in ObjectSchema
    *  * PatternTypesProperty in ObjectSchema
    *  * ItemTypeProperty in ArraySchema
    *  * ProductSchemaTypesProperty in ProductSchema
    *
    * @param transformer a function to be applied to recursively tranform properties
    * @param path the path to the current property
    * @return the recursively transformed property
    */
  def transform(
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): S = this.asInstanceOf[S]

  /** Produce a JSON representation of this property
    *
    * @param p the JSONoid parameters to use during converion
    */
  def toJson()(implicit p: JsonoidParams): JObject

  /** Whether a value should be considered anomalous according to the current property
    *
    * @param value the value to check
    * @param path the path of this property
    * @param tag a captured `ClassTag` used to collect anomalies
    */
  def isAnomalous[S <: JValue](value: S, path: String = "$")(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Boolean = {
    !collectAnomalies(value, path)(p, tag).isEmpty
  }

  /** Find all possible anomalies for a value according to the current property
    *
    * @param value the value to check for anomalies
    * @param path the path of this property
    * @param tag a captured `ClassTag` to use in the generated anomaly
    * @return a list of collected anomalies
    */
  def collectAnomalies[S <: JValue](value: S, path: String = "$")(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] =
    Seq.empty

  /** Whether this property is compatible with another property
    *
    * For validation properties, this means that any value that is valid
    * according to this property can *potentially* be valid according
    * to the other property.
    *
    * @param other
    * @param recursive
    * @param p
    */
  def isSubsetOf(other: S, recursive: Boolean = true)(implicit
      p: JsonoidParams
  ): Boolean = isInformational

  /** Expand a property to cover another property
    *
    * @param other the other property to expand to
    * @return this property expanded to cover the other property
    */
  def expandTo(other: Option[S]): S = this.asInstanceOf[S]
}
