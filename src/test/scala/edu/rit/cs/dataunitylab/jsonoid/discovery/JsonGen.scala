package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

object JsonGen {
  val genString = for {
    value <- arbitrary[String]
  } yield JString(value)

  val genDouble = for {
    value <- arbitrary[Double]
  } yield JDouble(value)

  val genDecimal = for {
    value <- arbitrary[BigDecimal]
  } yield JDecimal(value)

  val genInt = for {
    value <- arbitrary[BigInt]
  } yield JInt(value)

  val genLong = for {
    value <- arbitrary[Long]
  } yield JLong(value)

  val genBool = for {
    value <- arbitrary[Boolean]
  } yield JBool(value)

  val genPrimitive =
    Gen.oneOf(genString, genDouble, genDecimal, genInt, genLong, genBool)

  val genObject = Gen.sized { size =>
    for {
      keys <- Gen.listOfN(size, arbitrary[String])
      values <- Gen.listOfN(size, genValue)
    } yield JObject(keys zip values)
  }

  val genArray = for {
    items <- Gen.listOf(genValue)
  } yield JArray(items)

  val genValue: Gen[JValue] =
    Gen.oneOf(Gen.lzy(genObject), Gen.lzy(genArray), genPrimitive)
}
