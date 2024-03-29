package io.github.dataunitylab.jsonoid.discovery

import scala.io.Source

import com.code_intelligence.jazzer.api.FuzzedDataProvider

object DiscoverSchemaFuzzer {
  def fuzzerTestOneInput(data: FuzzedDataProvider): Unit = {
    val input = data.consumeRemainingAsString()
    val jsons = DiscoverSchema.jsonFromSource(Source.fromString(input))
    DiscoverSchema.discover(jsons)
  }
}
