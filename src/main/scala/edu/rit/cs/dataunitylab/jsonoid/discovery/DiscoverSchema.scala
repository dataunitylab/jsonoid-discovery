package edu.rit.cs.dataunitylab.jsonoid.discovery

import java.io.File
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.language.existentials

import scopt.OptionParser
import org.json4s._
import org.json4s.jackson.JsonMethods._

import Helpers._
import schemas._
import transformers._

/** Internal configuration object for the command line interface.
  */
private final case class Config(
    input: Option[File] = None,
    writeOutput: Option[File] = None,
    writeValues: Option[File] = None,
    propertySet: PropertySet = PropertySets.AllProperties,
    onlyProperties: Option[Seq[String]] = None,
    equivalenceRelation: EquivalenceRelation =
      EquivalenceRelations.KindEquivalenceRelation,
    extendedFormats: Boolean = false,
    addDefinitions: Boolean = false,
    maxExamples: Option[Int] = None,
    additionalProperties: Boolean = false,
    formatThreshold: Option[Float] = None,
    splitPercentage: Option[Double] = None,
    obliviousExpansion: Boolean = false,
    resetFormatLength: Boolean = false,
    randomSeed: Option[Long] = None,
    neverExpand: Boolean = false,
    debug: Boolean = false,
    detectDynamic: Boolean = false,
    detectDisjoint: Boolean = false,
    numericStrings: Boolean = false
)

object DiscoverSchema {

  /** Perform split schema discovery by randomly splitting the input and
    * generating two different schemas.
    *
    * @param jsons an iterator of JSON objects to perform schema discovery on
    * @param propSet the property set to use for schema discovery
    * @param splitPercentage the percentage of the input to use for training
    *
    * @return a tuple of the discovered schemas
    */
  def splitDiscover(
      jsons: Iterator[JValue],
      splitFraction: Double = 0.9f
  )(implicit p: JsonoidParams): (JsonSchema[_], JsonSchema[_]) = {
    val initialSchemas: (JsonSchema[_], JsonSchema[_]) =
      (ZeroSchema(), ZeroSchema())
    jsons.foldLeft(initialSchemas) { (schemas, json) =>
      {
        // Discover the schema for this single value
        val newSchema = discoverFromValue(json)(p)

        // Merge the value into the appropriate schema
        if (util.Random.nextDouble() > splitFraction) {
          (schemas._1.merge(newSchema), schemas._2)
        } else {
          (schemas._1, schemas._2.merge(newSchema))
        }
      }
    }
  }

  /** Perform schema discovery on a set of JSON objects.
    *
    * @param jsons an iterator of JSON objects to perform schema discovery on
    * @param propSet the property set to use for schema discovery
    * @return tthe discovered schema
    */
  def discover(
      jsons: Iterator[JValue]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    jsons.map(discoverFromValue(_)(p)).fold(ZeroSchema())(_.merge(_))
  }

  /** Discover a schema from a single JSON object.
    *
    * @param value the JSON object to discover the schema for
    * @param propSet the property set to use for schema discovery
    */
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def discoverFromValue(
      value: JValue
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    value match {
      case JArray(items) =>
        ArraySchema(items.map(discoverFromValue(_)(p)))(p)
      case JBool(bool)     => BooleanSchema(bool)(p)
      case JDecimal(dec)   => NumberSchema(dec)(p)
      case JDouble(dbl)    => NumberSchema(dbl)(p)
      case JInt(int)       => IntegerSchema(int)(p)
      case JLong(long)     => IntegerSchema(long)(p)
      case JNothing        => NullSchema()
      case JNull           => NullSchema()
      case JObject(fields) => discoverObjectFields(fields)(p)
      case JSet(items) =>
        ArraySchema(items.map(discoverFromValue(_)(p)).toList)(p)
      case JString(str) => StringSchema(str)(p)
    }
  }

  /** Discover a schema from a set of fields in a JSON object.
    *
    * @param fields the fields to discover the schema for
    * @param propSet the property set to use for schema discovery
    */
  private def discoverObjectFields(
      fields: Seq[JField]
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    ObjectSchema(
      fields
        .map { case (k, v) => (k, discoverFromValue(v)) }
        .asInstanceOf[Seq[(String, JsonSchema[_])]]
        .toMap
    )(p)
  }

  /** Produce an iterator of JSON objects from a source.
    */
  def jsonFromSource(source: Source): Iterator[JValue] = {
    source.getLines().map(parse(_))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def transformSchema(
      schema: JsonSchema[_],
      otherSchema: Option[JsonSchema[_]] = None,
      addDefinitions: Boolean = false,
      detectDynamic: Boolean = false,
      detectDisjoint: Boolean = false
  )(implicit p: JsonoidParams): JsonSchema[_] = {
    var transformedSchema = schema
    if (detectDynamic) {
      transformedSchema = DynamicObjectTransformer
        .transformSchema(transformedSchema)(p)
    }
    if (detectDisjoint) {
      transformedSchema = DisjointObjectTransformer
        .transformSchema(transformedSchema)(p)
    }
    if (addDefinitions) {
      transformedSchema = DefinitionTransformer
        .transformSchema(transformedSchema)(p)
    }
    transformedSchema = EnumTransformer
      .transformSchema(transformedSchema, otherSchema)(p)

    // Reset max/min length from strings if a format is defined
    if (p.resetFormatLength) {
      transformedSchema = schema.transformProperties(
        { case s: StringSchema =>
          val format =
            s.properties.getOrNone[FormatProperty].map(_.maxFormat()(p))
          if (format.isDefined) {
            StringSchema(
              s.properties
                .replacePropertyWithDefault[MaxLengthProperty]()
                .replacePropertyWithDefault[MinLengthProperty]()
            )
          } else {
            s
          }
        },
        true
      )
    }

    transformedSchema
  }

  // $COVERAGE-OFF$ No automated testing of CLI
  implicit val propertySetRead: scopt.Read[PropertySet] =
    scopt.Read.reads(typeName =>
      typeName match {
        case "All"    => PropertySets.AllProperties
        case "Min"    => PropertySets.MinProperties
        case "Simple" => PropertySets.SimpleProperties
      }
    )

  implicit val equivalenceRelationRead: scopt.Read[EquivalenceRelation] =
    scopt.Read.reads(erName =>
      erName match {
        case "Kind"  => EquivalenceRelations.KindEquivalenceRelation
        case "Label" => EquivalenceRelations.LabelEquivalenceRelation
        case "IntersectingLabel" =>
          EquivalenceRelations.IntersectingLabelEquivalenceRelation
        case "TypeMatch" =>
          EquivalenceRelations.TypeMatchEquivalenceRelation
      }
    )

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def outputSchema(
      schema: JsonSchema[_],
      maybeFile: Option[File] = None
  )(implicit p: JsonoidParams): Unit = {
    val schemaStr = pretty(render(schema.toJsonSchema()(p)))
    maybeFile match {
      case Some(file) =>
        Files.write(
          file.toPath(),
          schemaStr.getBytes(StandardCharsets.UTF_8)
        )
      case None => println(schemaStr)
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.Var"
    )
  )
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("jsonoid-discover") {
      head("jsonoid-discover", BuildInfo.version)

      help("help")

      arg[File]("<input>")
        .optional()
        .action((x, c) => c.copy(input = Some(x)))
        .text("a JSON file to perform discovery on, one object per line")

      opt[File]('w', "write-output")
        .action((x, c) => c.copy(writeOutput = Some(x)))
        .valueName("<file>")
        .text("file to write the generated schema to, defaults to stdout")

      opt[File]('v', "values")
        .action((x, c) => c.copy(writeValues = Some(x)))
        .valueName("<file>")
        .text("a file where a table of collected values should be written")

      opt[PropertySet]('p', "prop")
        .action((x, c) => c.copy(propertySet = x))
        .text("the set of properties to calculate [All, Min, Simple]")

      opt[Seq[String]]('o', "only-properties")
        .optional()
        .action((x, c) => c.copy(onlyProperties = Some(x)))
        .text("limit discovered properties")

      opt[EquivalenceRelation]('e', "equivalence-relation")
        .action((x, c) => c.copy(equivalenceRelation = x))
        .text(
          "the equivalence relation to use when merging" +
            " [Kind, Label, IntersectingLabel, TypeMatch]"
        )

      opt[Unit]("extended-formats")
        .action((x, c) => c.copy(extendedFormats = true))
        .text("whether to include extended formats")

      opt[Unit]('d', "add-definitions")
        .action((x, c) => c.copy(addDefinitions = true))
        .text("extract similar objects to create definitions")

      opt[Unit]('y', "detect-dynamic")
        .action((x, c) => c.copy(detectDynamic = true))
        .text("detect objects with dynamic keys")

      opt[Unit]('j', "detect-disjoint")
        .action((x, c) => c.copy(detectDisjoint = true))
        .text("detect objects with disjoint keys")

      opt[Unit]("numeric-strings")
        .action((x, c) => c.copy(numericStrings = true))
        .text("detect numbers represented as strings")

      opt[Int]("max-examples")
        .action((x, c) => c.copy(maxExamples = Some(x)))
        .text("maximum number of examples to extract")

      opt[Unit]('a', "additional-properties")
        .action((x, c) => c.copy(additionalProperties = true))
        .text("set additionalProperties to true in the generated schema")

      opt[Double]("format-threshold")
        .action((x, c) => c.copy(formatThreshold = Some(x.toFloat)))
        .text("set the fraction of values that must match a given format")

      opt[Double]('s', "split-percentage")
        .action((x, c) => c.copy(splitPercentage = Some(x)))
        .text("use split discovery with a specified percentage of documents")

      opt[Unit]("reset-format-length")
        .action((x, c) => c.copy(resetFormatLength = true))
        .text("reset the max/min length of strings with a format")

      opt[Unit]("oblivious-expansion")
        .action((x, c) => c.copy(obliviousExpansion = true))
        .text("expand the generated schema without using split discovery")

      opt[Long]("random-seed")
        .action((x, c) => c.copy(randomSeed = Some(x)))
        .text("random seed to use for sampling")

      opt[Unit]("never-expand")
        .action((x, c) => c.copy(neverExpand = true))
        .text("never expand the generated schema")

      opt[Unit]("debug")
        .action((x, c) => c.copy(debug = true))
        .text("enable debugging features")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val tmpDir = if (config.debug) {
          val dir = Files.createTempDirectory("jsonoid")
          System.err.println(s"tmpDir: ${dir.toString}")

          dir
        } else {
          Paths.get(System.getProperty("java.io.tmpdir"))
        }

        if (config.randomSeed.isDefined) {
          scala.util.Random.setSeed(config.randomSeed.get)
        }

        val source = config.input match {
          case Some(file) => Source.fromFile(file)
          case None       => Source.stdin
        }

        val propSet = config.onlyProperties match {
          case Some(propNames) => config.propertySet.onlyNamed(propNames)
          case None            => config.propertySet
        }

        // Enable numeric string detection
        if (config.numericStrings) {
          propSet.stringProperties.add(StringNumericProperty())
        }

        val jsons = jsonFromSource(source)
        var p = JsonoidParams()
          .withER(config.equivalenceRelation)
          .withExtendedFormats(config.extendedFormats)
          .withAdditionalProperties(config.additionalProperties)
          .withPropertySet(propSet)
          .withResetFormatLength(config.resetFormatLength)
        if (config.maxExamples.isDefined) {
          p = p.withMaxExamples(config.maxExamples.get)
        }
        if (config.formatThreshold.isDefined) {
          p = p.withFormatThreshold(config.formatThreshold.get)
        }

        val (schema: ObjectSchema, testSchema: Option[ObjectSchema]) =
          config.splitPercentage match {
            case Some(pct) =>
              val schemas = splitDiscover(jsons, pct)(p)
              val trainSchema = schemas._1.asInstanceOf[ObjectSchema]
              val testSchema = schemas._2.asInstanceOf[ObjectSchema]

              val expandSchema = if (config.obliviousExpansion) {
                None
              } else {
                Some(testSchema)
              }
              val finalSchema = if (config.neverExpand) {
                trainSchema
              } else {
                trainSchema.expandTo(expandSchema)
              }

              // If debugging is enabled, save the training and test schemas
              if (config.debug) {
                outputSchema(
                  trainSchema,
                  Some(tmpDir.resolve("train.json").toFile)
                )
                outputSchema(
                  testSchema,
                  Some(tmpDir.resolve("test.json").toFile)
                )
                outputSchema(
                  finalSchema,
                  Some(tmpDir.resolve("final.json").toFile)
                )
              }

              if (!finalSchema.isCompatibleWith(testSchema)) {
                val incompats = IncompatibilityCollector.findIncompatibilities(
                  finalSchema,
                  testSchema
                )
                incompats.foreach(System.err.println(_))
                throw new IllegalStateException(
                  "Split discovery failed to find a compatible schema"
                )
              }

              (finalSchema, Some(testSchema))
            case None =>
              if (config.obliviousExpansion && !config.neverExpand) {
                val unexpandedSchema = discover(jsons)(p)
                // If debugging is enabled, save the schema before expansion
                if (config.debug) {
                  outputSchema(
                    unexpandedSchema,
                    Some(tmpDir.resolve("train.json").toFile)
                  )
                }
                (unexpandedSchema.expandTo(None), None)
              } else {
                (discover(jsons)(p), None)
              }
          }

        // Check if transformations are valid
        if (
          config.detectDynamic && config.propertySet =/= PropertySets.AllProperties
        ) {
          throw new IllegalArgumentException(
            "All properties required to detect dynamic objects"
          )
        }
        if (
          config.detectDisjoint && config.propertySet =/= PropertySets.AllProperties
        ) {
          throw new IllegalArgumentException(
            "All properties required to detect dynamic objects"
          )
        }
        if (
          config.addDefinitions && config.propertySet =/= PropertySets.AllProperties
        ) {
          throw new IllegalArgumentException(
            "All properties required to compute definitions"
          )
        }

        var transformedSchema: JsonSchema[_] =
          transformSchema(
            schema,
            testSchema,
            config.addDefinitions,
            config.detectDynamic
          )(p)

        // If debugging is enabled, save the schema before expansion
        if (config.debug) {
          outputSchema(
            transformedSchema,
            Some(tmpDir.resolve("transformed.json").toFile)
          )
        }

        if (config.writeValues.isDefined) {
          val outputStream = new FileOutputStream(config.writeValues.get)
          ValueTableGenerator.writeValueTable(
            transformedSchema,
            outputStream
          )
        }

        outputSchema(transformedSchema, config.writeOutput)
      case None => System.exit(1)
    }
  }
  // $COVERAGE-ON$
}
