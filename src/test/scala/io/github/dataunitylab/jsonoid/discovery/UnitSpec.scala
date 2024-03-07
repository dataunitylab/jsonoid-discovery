package io.github.dataunitylab.jsonoid.discovery

import org.apache.log4j.{Level, Logger}
import org.scalactic.Equality
import org.scalatest.{BeforeAndAfter, Checkpoints, OptionValues}
import org.scalatest.enablers.Containing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import schemas._
import utils.JsonPointer

abstract class UnitSpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfter
    with OptionValues
    with Checkpoints {
  before {
    Logger.getLogger("org.apache.spark").setLevel(Level.OFF)
  }

  def withParams(
      additionalProperties: Boolean = false,
      propSet: PropertySet = PropertySets.AllProperties,
      er: EquivalenceRelation = EquivalenceRelations.KindEquivalenceRelation
  )(testCode: JsonoidParams => Any): Unit = {
    implicit val params =
      JsonoidParams()
        .withAdditionalProperties(additionalProperties)
        .withER(er)
        .withPropertySet(propSet)
    testCode(params)
  }
}

object UnitSpec {
  implicit val p: JsonoidParams = JsonoidParams()
  implicit val pointerToString: JsonPointer => String = _.toString
  implicit val pointerFromString: String => JsonPointer = JsonPointer.fromString

  implicit def containingNatureOfSchemaProperties[SchemaProperties[
      _
  ] <: Iterable[SchemaProperty[_]]](implicit
      equality: Equality[SchemaProperty[_]]
  ): Containing[SchemaProperties[_]] = new Containing[SchemaProperties[_]] {
    def contains(props: SchemaProperties[_], elem: Any): Boolean = {
      props.exists(equality.areEqual(_, elem))
    }

    def containsNoneOf(
        props: SchemaProperties[_],
        elems: scala.collection.Seq[Any]
    ): Boolean = {
      !containsOneOf(props, elems)
    }

    def containsOneOf(
        props: SchemaProperties[_],
        elems: scala.collection.Seq[Any]
    ): Boolean = {
      !elems.exists(e => props.exists(equality.areEqual(_, e)))
    }
  }
}
