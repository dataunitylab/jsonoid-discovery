package edu.rit.cs.dataunitylab.jsonoid.discovery

import Helpers._
import schemas._
import utils.HyperLogLog

/** Represents a discovered primary key.
  *
  * @constructor Create a new primary key with two paths.
  * @param path the path of the primary key
  */
final case class PrimaryKey(path: String)

object PrimaryKeyFeatures {

  /** A penalty applied to long primary keys. */
  val LengthPenalty: Int = 50
}

/** A set of features used to score primary key candidates.
  *
  * @constructor Create a new set of primary key features.
  * @param maxCount the maximum cardinality of values at this path
  * @param hasPrefixOrSuffix whether the path has a prefix or suffix common to primary keys
  * @param depth the depth of the primary key in the JSON document
  * @param idType the type of the primary key
  * @param maxLength the maximum length of all prinary key values
  */
final case class PrimaryKeyFeatures(
    maxCount: Double,
    hasPrefixOrSuffix: Boolean,
    depth: Int,
    idType: Boolean,
    maxLength: Int
) {

  /** A score for ranking candidate primary keys. */
  def score: Double = {
    // These features are taken primarily from "An embedding driven approach to
    // automatically detect identifiers and references in document stores" by
    // Manel Souibgui, Faten Atiguia, Sadok Ben Yahia, and Samira Si-Said Cherfi
    // https://www.sciencedirect.com/science/article/pii/S0169023X22000209
    //
    // We currently only consider single attribute primary keys so we ignore
    // the cardinality feature. We also consider UUIDs to have length 0.
    (if (hasPrefixOrSuffix) 1.0 else 0.0) +
      1.0 / (depth + 1) +
      (if (idType) 1.0 else 0.0) +
      1.0 / 1.0.max(maxLength - PrimaryKeyFeatures.LengthPenalty)
  }
}

object PrimaryKeyFinder extends SchemaWalker[PrimaryKeyFeatures] {

  /** A list of common prefixes and suffixes for primary keys. */
  val prefixOrSuffix: List[String] =
    List("id", "key", "nr", "no", "pk", "num", "code")

  /** Helper for [[getFeatures]] to construct a [[PrimaryKeyFeatures]] object
    * from information extracted from the schema.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def extractFeatures(
      hll: HyperLogLog,
      path: String,
      idType: Boolean,
      maxLength: Int
  ): PrimaryKeyFeatures = {
    val lastKeyPart = path.split('.').last

    PrimaryKeyFeatures(
      (hll.count * (1 + hll.getStandardError)),
      prefixOrSuffix.exists(lastKeyPart.startsWith(_)) || prefixOrSuffix.exists(
        lastKeyPart.endsWith(_)
      ),
      path.count(_ == '.') - 1,
      idType,
      maxLength
    )
  }

  /** Used in [[findPrimaryKeys]] to extract featuers useful for ranking.
    */
  private val getFeatures
      : PartialFunction[(String, JsonSchema[_]), PrimaryKeyFeatures] = {
    // Get the features for the specific type
    case (path, i: IntegerSchema) if i.properties.has[IntHyperLogLogProperty] =>
      val maxLength = if (i.properties.has[MaxIntValueProperty]) {
        i.properties
          .get[MaxIntValueProperty]
          .maxIntValue
          .getOrElse(0)
          .toString
          .length
      } else {
        0
      }
      extractFeatures(
        i.properties.get[IntHyperLogLogProperty].hll,
        path,
        true,
        maxLength
      )

    case (path, n: NumberSchema) if n.properties.has[NumHyperLogLogProperty] =>
      val maxLength = if (n.properties.has[MaxNumValueProperty]) {
        n.properties
          .get[MaxNumValueProperty]
          .maxNumValue
          .getOrElse(0.0)
          .toString
          .length
      } else {
        0
      }

      extractFeatures(
        n.properties.get[NumHyperLogLogProperty].hll,
        path,
        false,
        maxLength
      )

    case (path, s: StringSchema)
        if s.properties.has[StringHyperLogLogProperty] =>
      // If we have UUID format, consider the length as zero since UUIDs are
      // long, but fairly likely to be used as primary key values
      val format =
        s.properties.getOrNone[FormatProperty].map(_.maxFormat().getOrElse(""))
      val isUUID = format.map(_ === "uuid").getOrElse(false)

      val maxLength = if (s.properties.has[MaxLengthProperty] && !isUUID) {
        s.properties.get[MaxLengthProperty].maxLength.getOrElse(0)
      } else {
        0
      }
      extractFeatures(
        s.properties.get[StringHyperLogLogProperty].hll,
        path,
        true,
        maxLength
      )
  }

  /** Find a list of possible primary keys in a schema.
    *
    * @param schema the schema to search for primary keys
    *
    * @return a list of possible primary keys
    */
  def findPrimaryKeys(schema: JsonSchema[_]): List[PrimaryKey] = {
    schema match {
      case o: ObjectSchema =>
        // Get the total count of all times the object has occurred
        val totalCount = o.properties.get[FieldPresenceProperty].totalCount

        // Collect all scores for possible primary keys
        val fieldsByScore = walk(schema, getFeatures).toList
          .filter(
            _._2.maxCount >= totalCount.toDouble
          )
          .map(x => (x._1, x._2.score))
          .sortBy(-_._2)

        val primaryKeys = if (fieldsByScore.size > 1) {
          // Rank features by score and find the cliff
          // (largest difference between scores)
          val cliff = fieldsByScore
            .sliding(2)
            .map(x => x(1)._2 - x(0)._2)
            .zipWithIndex
            .minBy(_._1)
            ._2

          fieldsByScore.slice(0, cliff + 1).map(x => PrimaryKey(x._1))
        } else {
          fieldsByScore.map(x => PrimaryKey(x._1))
        }

        primaryKeys
      case _ => List()
    }
  }
}
