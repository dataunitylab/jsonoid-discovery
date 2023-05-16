package edu.rit.cs.dataunitylab.jsonoid.discovery
package transformers

import scala.jdk.CollectionConverters._

import com.github.dataunitylab.fuzzysets.FuzzySet
import org.christopherfrantz.dbscan.{DBSCANClusterer, DistanceMetric}

import Helpers._
import schemas._

/** Used to calculate similarity fuzzy sets located at two paths.
  *
  * @constructor Create a new similarity fuzzy set calculator.
  * @param fuzzySets a map of fuzzy sets to paths
  */
final case class SimilarityMetric(val fuzzySets: Map[String, FuzzySet[String]])
    extends DistanceMetric[String] {

  /** Calculate the distance between the fuzzy sets at two paths.
    *
    * @param path1 the first path
    * @param path2 the second path
    */
  def calculateDistance(path1: String, path2: String): Double = {
    // Look up the set based on path, calculate similarity, and invert it
    val dist = 1.0 - fuzzySets(path1).similarity(fuzzySets(path2))

    // Distance must be non-negative
    assert(dist >= 0)

    dist
  }
}

/** Use fuzzy set similarity to transform a schema by creating definitions that
  * capture repeated structures within the schema.
  */
object DefinitionTransformer extends SchemaWalker[FuzzySet[String]] {

  /** Convert a JSON Path value into a JSON Pointer.
    *
    * @param path the JSON Path value
    * @return the JSON Pointer
    */
  def pathToPointer(path: String): String = {
    // Path must not be empty
    assert(path.nonEmpty)

    path.substring(1).replace(".", "/").replaceAll("""\[(.+?)\]""", "/$1")
  }

  /** Transform the schema to replace repeated structures.
    *
    * @param schema the schema to transform
    * @param addObject whether to capture the object within the created reference
    * @return the transformed schema
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Var",
      "org.wartremover.warts.While"
    )
  )
  def transformSchema(
      schema: JsonSchema[_],
      addObject: Boolean = true
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    // Discover clusters within the schema
    val clusters = findClusters(schema).map(_.map(pathToPointer _))

    // Track a new schema with replaced definitions
    var replaced = Set.empty[String]
    var definitions = Set.empty[String]
    var definitionSchema = schema
    clusters.toList
      // Process clusters with the shortest paths first
      // so we avoid replacing nested definitions later
      .sortBy(cluster => -cluster.map(_.size).sum * 1.0 / cluster.size)
      .zipWithIndex
      .foreach { case (cluster, index) =>
        // Check that we're not replacing something already replaced
        if (!cluster.exists(c => replaced.exists(c.startsWith(_)))) {
          // Merge all schemas in the cluster to create a new definition
          val clusterSchema = cluster
            .map(schema.findByPointer(_).get)
            .fold(ZeroSchema())(_.merge(_))

          // Name the definition by finding a common suffix of the clusters
          // (reverse the values and find a prefix)
          // We also only take the part of the name after the last slash
          val lastParts =
            cluster.flatMap(_.split("/").reverse.dropWhile { x =>
              x === "*" || (x forall Character.isDigit)
            }.headOption)
          var definition = if (lastParts.size > 1) {
            val afterUnderscore = lastParts.map(_.split("_").last)
            if (
              afterUnderscore.size === 1 && afterUnderscore.headOption.get.nonEmpty
            ) {
              afterUnderscore.headOption.get
            } else {
              s"defn${index.toString}"
            }
          } else {
            lastParts.headOption.getOrElse(s"defn${index.toString}")
          }

          // The definition name must not be empty
          assert(definition.nonEmpty)

          // Handle possible name collisions
          while (definitions.contains(definition)) {
            definition += "_"
          }
          definitions += definition

          // Add the definition to the schema
          definitionSchema.addDefinition(clusterSchema, definition)

          // Replace all instances of the clustered value with the definition
          cluster.foreach { pointer =>
            definitionSchema = definitionSchema.replaceWithReference(
              pointer,
              s"#/$$defs/${definition}",
              if (addObject) { Some(clusterSchema) }
              else { None }
            )
          }
        }

        // Track the paths that have been replaced
        replaced ++= cluster
      }

    definitionSchema
  }

  /** Find clusters of similar nested keyswithin the schema.
    *
    * @param schema the schema to cluster
    * @return a set of set of paths which are clusters
    */
  def findClusters(schema: JsonSchema[_]): Set[Set[String]] = {
    // Build fuzzy sets representing key occurrence in all objects
    val fuzzySets = walk(schema, buildFuzzySet)
    if (fuzzySets.size <= 1) {
      Set.empty[Set[String]]
    } else {
      val clusterer = new DBSCANClusterer(
        fuzzySets.keySet.asJava,
        2,
        0.1f,
        SimilarityMetric(fuzzySets)
      )
      clusterer.performClustering.asScala.map(_.asScala.toSet).toSet
    }
  }

  /** Build a fuzzy set representing key occurrence at a path.
    */
  private val buildFuzzySet
      : PartialFunction[(String, JsonSchema[_]), FuzzySet[String]] = {
    case (_, o: ObjectSchema) if o.properties.has[FieldPresenceProperty] =>
      val fieldPresence = o.properties.get[FieldPresenceProperty]
      FuzzySet(fieldPresence.fieldPresence.keySet.map { key =>
        key -> fieldPresence
          .fieldPresence(key)
          .floatValue / fieldPresence.totalCount.floatValue
      }.toMap)
  }
}
