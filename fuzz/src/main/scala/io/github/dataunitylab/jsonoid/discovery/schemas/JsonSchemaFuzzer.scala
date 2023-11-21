package io.github.dataunitylab.jsonoid.discovery
package schemas

import com.code_intelligence.jazzer.api.FuzzedDataProvider
import org.json4s.jackson.JsonMethods._

object JsonSchemaFuzzer {
  def fuzzerTestOneInput(data: FuzzedDataProvider): Unit = {
    val input = data.consumeRemainingAsString()
    try {
      val json = parse(input)
      JsonSchema.fromJson(json)
    } catch {
      case e: com.fasterxml.jackson.core.JsonParseException =>
      case e: com.fasterxml.jackson.databind.exc.MismatchedInputException =>
      case e: UnsupportedOperationException =>
    }
  }
}

