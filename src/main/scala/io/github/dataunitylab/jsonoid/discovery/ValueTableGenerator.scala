package io.github.dataunitylab.jsonoid.discovery

import java.io.OutputStream

import com.github.tototoshi.csv.CSVWriter

import schemas._

/** Allow writing all values observed at a particular path to a CSV file.
  */
object ValueTableGenerator extends SchemaWalker[List[String]] {

  /** Write a table of observed values to the given output stream.
    *
    * @param schema
    *   the schema whose values should be written
    * @param output
    *   the output stream to write to
    */
  def writeValueTable(schema: JsonSchema[_], output: OutputStream): Unit = {
    val exampleExtractor
        : PartialFunction[(String, JsonSchema[_]), List[String]] = {
      case (_, i: IntegerSchema) =>
        i.properties.get[IntExamplesProperty].examples.examples.map(_.toString)
      case (_, n: NumberSchema) =>
        n.properties.get[NumExamplesProperty].examples.examples.map(_.toString)
      case (_, s: StringSchema) =>
        s.properties.get[StringExamplesProperty].examples.examples
    }
    val values = walk(schema, exampleExtractor)

    val csvWriter = CSVWriter.open(output)
    val keys = values.keys.toList.sorted

    // Write the header
    csvWriter.writeRow(keys)

    // Write values for each row
    val allValues = keys.map(k => values(k).sorted)
    val maxLen = allValues.map(_.length).foldLeft(0)(_ max _)
    csvWriter.writeAll(allValues.map(_.padTo(maxLen, "")).transpose)
    csvWriter.close()
  }
}
