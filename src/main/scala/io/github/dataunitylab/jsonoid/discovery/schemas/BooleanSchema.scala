package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect._

import org.json4s.JsonDSL._
import org.json4s._

object BooleanSchema {
  def apply(
      value: Boolean
  )(implicit p: JsonoidParams): BooleanSchema = {
    BooleanSchema(
      p.propSet.booleanProperties.mergeValue(value)(p)
    )
  }

  lazy val MinProperties: SchemaProperties[Boolean] = {
    val props = SchemaProperties.empty[Boolean]
    props.add(BooleanConstantProperty())

    props
  }

  lazy val SimpleProperties: SchemaProperties[Boolean] = {
    val props = SchemaProperties.empty[Boolean]
    props.add(BooleanConstantProperty())

    props
  }

  lazy val AllProperties: SchemaProperties[Boolean] = {
    val props = SchemaProperties.empty[Boolean]
    props.add(BooleanConstantProperty())
    props.add(BooleanPercentProperty())

    props
  }
}

/** Represents Boolean values in JSON Schema.
  */
final case class BooleanSchema(
    override val properties: SchemaProperties[Boolean] =
      BooleanSchema.AllProperties
) extends JsonSchema[Boolean] {
  override val schemaType = "boolean"

  override val validTypes: Set[Class[_]] = Set(classOf[JBool])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ BooleanSchema(otherProperties) =>
      BooleanSchema(properties.merge(otherProperties, mergeType))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[Boolean]): BooleanSchema = {
    val newSchema = BooleanSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  override def toJson()(implicit p: JsonoidParams): JObject = {
    properties.getOrNone[BooleanConstantProperty] match {
      case Some(BooleanConstantProperty(Some(true), _)) =>
        ("const" -> JBool(true))
      case Some(BooleanConstantProperty(_, Some(true))) =>
        ("const" -> JBool(false))
      case _ => super.toJson()(p)
    }
  }

  override def collectAnomalies[S <: JValue](
      value: S,
      path: String
  )(implicit p: JsonoidParams, t: ClassTag[S]): Seq[Anomaly] = {
    value match {
      case JBool(_) => Seq.empty
      case _        => Seq(Anomaly(path, "expected boolean type", AnomalyLevel.Fatal))
    }
  }
}

/** Tracks the percentage of true values for a boolean.
  *
  * @constructor Create a new Boolean percent property
  * @param totalTrue the number of true values observed
  * @param totalFalse the number of false values observed
  */
final case class BooleanPercentProperty(
    totalTrue: BigInt = 0,
    totalFalse: BigInt = 0
) extends SchemaProperty[Boolean] {
  override type S = BooleanPercentProperty

  override def newDefault()(implicit p: JsonoidParams): BooleanPercentProperty =
    BooleanPercentProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = if (
    totalTrue > 0 || totalFalse > 0
  ) {
    ("pctTrue" -> totalTrue.toDouble / (totalTrue + totalFalse).toDouble)
  } else {
    Nil
  }

  override def unionMerge(
      otherProp: BooleanPercentProperty
  )(implicit p: JsonoidParams): BooleanPercentProperty = {
    BooleanPercentProperty(
      totalTrue + otherProp.totalTrue,
      totalFalse + otherProp.totalFalse
    )
  }

  override def mergeValue(
      value: Boolean
  )(implicit p: JsonoidParams): BooleanPercentProperty = {
    BooleanPercentProperty(
      totalTrue + (if (value) 1 else 0),
      totalFalse + (if (!value) 1 else 0)
    )
  }
}

/** Tracks whether all values are either true or false
  *
  * @constructor Create a new Boolean constant property
  * @param allTrue whether all values are true
  * @param allFalse whether all values are false
  */
final case class BooleanConstantProperty(
    allTrue: Option[Boolean] = None,
    allFalse: Option[Boolean] = None
) extends SchemaProperty[Boolean] {
  override type S = BooleanConstantProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): BooleanConstantProperty =
    BooleanConstantProperty()

  override val isInformational = false

  override def toJson()(implicit p: JsonoidParams): JObject = Nil

  override def unionMerge(
      otherProp: BooleanConstantProperty
  )(implicit p: JsonoidParams): BooleanConstantProperty = {
    BooleanConstantProperty(
      (allTrue, otherProp.allTrue) match {
        case (None, None) => None
        case _ =>
          Some(allTrue.getOrElse(false) && otherProp.allTrue.getOrElse(false))
      },
      (allFalse, otherProp.allFalse) match {
        case (None, None) => None
        case _ =>
          Some(allFalse.getOrElse(false) && otherProp.allFalse.getOrElse(false))
      }
    )
  }

  override def mergeValue(
      value: Boolean
  )(implicit p: JsonoidParams): BooleanConstantProperty = {
    BooleanConstantProperty(
      Some(allTrue.getOrElse(true) && value),
      Some(allFalse.getOrElse(true) && !value)
    )
  }

  override def isSubsetOf(
      other: BooleanConstantProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    val trueCompat = (allTrue, other.allTrue) match {
      case (Some(a), Some(b)) => a >= b
      case _                  => true
    }
    val falseCompat = (allFalse, other.allFalse) match {
      case (Some(a), Some(b)) => a >= b
      case _                  => true
    }
    trueCompat && falseCompat
  }

  override def expandTo(
      other: Option[BooleanConstantProperty]
  ): BooleanConstantProperty = {
    BooleanConstantProperty(Some(false), Some(false))
  }
}
