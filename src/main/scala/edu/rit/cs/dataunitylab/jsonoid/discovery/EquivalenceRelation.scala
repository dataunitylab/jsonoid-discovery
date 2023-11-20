package io.github.dataunitylab.jsonoid.discovery

import Helpers._
import schemas._

/** Predefined sets of equivalence relations.
  */
object EquivalenceRelations {

  /** An equivalence relation which considers objects equal if they have the same
    * set of keys.
    */
  implicit object LabelEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      (kind1, kind2) match {
        case (obj1: ObjectSchema, obj2: ObjectSchema) => {
          val types1 = obj1.properties.get[ObjectTypesProperty].objectTypes
          val types2 = obj2.properties.get[ObjectTypesProperty].objectTypes

          types1.keySet === types2.keySet
        }
        case _ => kind1.schemaType === kind1.schemaType
      }
    }
  }

  /** An equivalence relation which considers objects equal if they have any
    * overlapping set of keys.
    */
  implicit object IntersectingLabelEquivalenceRelation
      extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      (kind1, kind2) match {
        case (obj1: ObjectSchema, obj2: ObjectSchema) => {
          val types1 = obj1.properties.get[ObjectTypesProperty].objectTypes
          val types2 = obj2.properties.get[ObjectTypesProperty].objectTypes

          !(types1.keySet & types2.keySet).isEmpty
        }
        case _ => kind1.schemaType === kind1.schemaType
      }
    }
  }

  /** An equivalence relation which considers objects equal if all keys which are
    * in common between the objects have the same type.
    */
  implicit object TypeMatchEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      (kind1, kind2) match {
        case (obj1: ObjectSchema, obj2: ObjectSchema) => {
          val types1 = obj1.properties.get[ObjectTypesProperty].objectTypes
          val types2 = obj2.properties.get[ObjectTypesProperty].objectTypes

          val intersectingKeys = types1.keySet & types2.keySet
          intersectingKeys.forall { key =>
            types1(key).schemaType === types2(key).schemaType
          }
        }
        case _ => kind1.schemaType === kind1.schemaType
      }
    }
  }

  /** An equivalence relation which considers schemas equal if they have the same
    * type.
    */
  implicit object KindEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      kind1.schemaType === kind1.schemaType
    }
  }

  /** An equivalence relation which *always* considers schemas equal.
    */
  implicit object AlwaysEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = true
  }

  /** An equivalence relation which *never* considers schemas equal.
    */
  implicit object NonEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = false
  }
}

/** Represents an equivalence relation between two schemas that determines
  * whether two schemas should be considered equivalent in the final discovered
  * schema.
  *
  * For more details see [Parametric inference for massive JSON
  * datasets](https://link.springer.com/article/10.1007/s00778-018-0532-7) by
  * Baazizi et al
  */
abstract class EquivalenceRelation extends Serializable {

  /** Returns true if the two schemas should be considered equivalent.
    *
    * @param kind1 the first schema to compare
    * @param kind2 the second schem to compare
    */
  def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean
}
