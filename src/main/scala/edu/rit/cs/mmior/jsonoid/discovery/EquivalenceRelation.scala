package edu.rit.cs.mmior.jsonoid.discovery

import Helpers._
import schemas._

object EquivalenceRelations {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit object LabelEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      (kind1, kind2) match {
        case (obj1: ObjectSchema, obj2: ObjectSchema) => {
          val types1 = obj1.properties.get[ObjectTypesProperty].objectTypes
          val types2 = obj2.properties.get[ObjectTypesProperty].objectTypes

          types1.keySet == types2.keySet
        }
        case _ => kind1.schemaType === kind1.schemaType
      }
    }
  }

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

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit object TypeMatchEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      (kind1, kind2) match {
        case (obj1: ObjectSchema, obj2: ObjectSchema) => {
          val types1 = obj1.properties.get[ObjectTypesProperty].objectTypes
          val types2 = obj2.properties.get[ObjectTypesProperty].objectTypes

          val intersectingKeys = types1.keySet & types2.keySet
          intersectingKeys.forall { key =>
            types1(key).schemaType == types2(key).schemaType
          }
        }
        case _ => kind1.schemaType === kind1.schemaType
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit object KindEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = {
      kind1.schemaType === kind1.schemaType
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit object AlwaysEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = true
  }

  implicit object NonEquivalenceRelation extends EquivalenceRelation {
    def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean = false
  }
}

abstract class EquivalenceRelation extends Serializable {
  def fuse(kind1: JsonSchema[_], kind2: JsonSchema[_]): Boolean
}
