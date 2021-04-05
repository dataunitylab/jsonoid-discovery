package edu.rit.cs.mmior.jsonoid.discovery

import scala.language.implicitConversions

import org.scalactic.Equality
import org.scalatest.enablers.Containing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import schemas._
import Helpers._

abstract class UnitSpec extends AnyFlatSpec with Matchers

object UnitSpec {
  implicit def containingNatureOfSchemaProperties[SchemaProperties[_] <: Iterable[SchemaProperty[_]]](implicit equality: Equality[SchemaProperty[_]]): Containing[SchemaProperties[_]] = new
  Containing[SchemaProperties[_]] {
    def contains(props: SchemaProperties[_], elem: Any): Boolean = {
      props.exists(equality.areEqual(_, elem))
    }

    def containsNoneOf(props: SchemaProperties[_], elems: scala.collection.Seq[Any]): Boolean = {
      !containsOneOf(props, elems)
    }

    def containsOneOf(props: SchemaProperties[_], elems: scala.collection.Seq[Any]): Boolean = {
      !elems.exists(e => props.exists(equality.areEqual(_, e)))
    }
  }
}
