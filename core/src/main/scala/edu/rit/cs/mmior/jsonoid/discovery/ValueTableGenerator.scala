package edu.rit.cs.mmior.jsonoid.discovery

import java.io.OutputStream

import com.github.tototoshi.csv.CSVWriter

import schemas._

object ValueTableGenerator extends SchemaWalker[List[String]] {
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
