package edu.rit.cs.dataunitylab.jsonoid.discovery

import scala.io.Source

import com.code_intelligence.jazzer.api.FuzzedDataProvider

@SuppressWarnings(
  Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Return"
  )
)
object DiscoverSchemaFuzzer {
  def fuzzerTestOneInput(data: FuzzedDataProvider): Unit = {
    val input = data.consumeRemainingAsString()
    try {
      val jsons = DiscoverSchema.jsonFromSource(Source.fromString(input))
      DiscoverSchema.discover(jsons)
    } catch {
      // XXX These failures should be dealt with
      case e: com.fasterxml.jackson.core.JsonParseException => return
      case e: com.fasterxml.jackson.databind.exc.MismatchedInputException =>
        return
      case e: java.lang.NumberFormatException => return
    }
  }
}
