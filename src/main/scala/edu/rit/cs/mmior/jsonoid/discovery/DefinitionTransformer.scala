package edu.rit.cs.mmior.jsonoid.discovery

import scala.collection.JavaConverters._

import com.github.dataunitylab.fuzzysets.FuzzySet
import org.christopherfrantz.dbscan.{DBSCANClusterer, DistanceMetric}

import Helpers._
import schemas._

final case class SimilarityMetric(val fuzzySets: Map[String, FuzzySet[String]])
    extends DistanceMetric[String] {
  def calculateDistance(path1: String, path2: String): Double = {
    // Look up the set based on path, calculate similarity, and invert it
    1.0 - fuzzySets(path1).similarity(fuzzySets(path2))
  }
}

object DefinitionTransformer extends SchemaWalker[FuzzySet[String]] {
  def pathToPointer(path: String): String = {
    path.substring(1).replace(".", "/").replaceAll("""\[(.+?)\]""", "/$1")
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Var",
      "org.wartremover.warts.While"
    )
  )
  def transformSchema(
      schema: JsonSchema[_]
  )(implicit er: EquivalenceRelation): JsonSchema[_] = {
    // Discover clusters within the schema
    val clusters = findClusters(schema).map(_.map(pathToPointer _))

    // Track a new schema with replaced definitions
    var replaced = Set.empty[String]
    var definitions = Set.empty[String]
    var definitionSchema: JsonSchema[_] = schema
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
            cluster.map(_.split("/").reverse.dropWhile { x =>
              x === "*" || (x forall Character.isDigit)
            }.head)
          var definition = if (lastParts.size > 1) {
            val afterUnderscore = lastParts.map(_.split("_").last)
            if (
              afterUnderscore.size === 1 && afterUnderscore.headOption.get.length > 0
            ) {
              afterUnderscore.headOption.get
            } else {
              s"defn${index}"
            }
          } else {
            lastParts.headOption.get
          }
          //
          // Handle possible name collisions
          while (definitions.contains(definition)) {
            definition += "_"
          }
          definitions += definition

          // Add the definition to the schema
          definitionSchema = definitionSchema
            .asInstanceOf[ObjectSchema]
            .withDefinition(clusterSchema, definition)

          // Replace all instances of the clustered value with the definition
          cluster.foreach { pointer =>
            definitionSchema = definitionSchema.replaceWithReference(
              pointer,
              s"#/$$defs/${definition}"
            )
          }
        }

        // Track the paths that have been replaced
        replaced ++= cluster
      }

    definitionSchema
  }

  def findClusters(schema: JsonSchema[_]): Set[Set[String]] = {
    // Build fuzzy sets representing key occurrence in all objects
    val fuzzySets = walk(schema, buildFuzzySet)
    if (fuzzySets.isEmpty) {
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
