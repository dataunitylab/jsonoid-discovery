package io.github.dataunitylab.jsonoid

import java.io.File
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import scopt.OptionParser

import discovery.Helpers._
import discovery.schemas.JsonSchema

/** Internal configuration object for the command line interface.
  */
private final case class Config(
    schema: Option[File] = None,
    json: Option[File] = None
)

object BowtieValidator {
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.OptionPartial",
      "org.wartremover.warts.While"
    )
  )
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("jsonoid-bowtie-validate") {
      head("jsonoid-bowtie-validate", discovery.BuildInfo.version)

      help("help")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        implicit val formats: Formats = DefaultFormats

        // Validate the start command
        val startCmd = parse(StdIn.readLine())
        assert(startCmd \ "cmd" === JString("start"))
        assert((startCmd \ "version").extract[Int] === 1)

        // Print a ready message
        println(
          compact(
            render(
              ("version" -> 1) ~
                ("ready" -> true) ~
                ("implementation" -> (("language" -> "scala") ~
                  ("name" -> "jsonoid") ~
                  ("homepage" -> "https://github.com/dataunitylab/jsonoid-discovery") ~
                  ("issues" -> "https://github.com/dataunitylab/jsonoid-discovery/issues") ~
                  ("dialects" -> List(
                    "https://json-schema.org/draft/2020-12/schema"
                  ))))
            )
          )
        )

        // Loop over all commands
        breakable {
          while (true) {
            val cmd = parse(StdIn.readLine())

            (cmd \ "cmd").extract[String] match {
              case "stop" => break()

              // We don't really support implicit dialects but since we only
              // support a single dialect and we will only try to validate
              // using that dialect, it's effectively the same thing
              case "dialect" => println(compact(render(("ok" -> true))))

              case "run" => {
                val seqId = (cmd \ "seq")
                val maybeSchema: Try[Option[JsonSchema[_]]] =
                  try {
                    Success(Some(JsonSchema.fromJson(cmd \ "case" \ "schema")))
                  } catch {
                    // We will treat this as something to skip
                    case e: UnsupportedOperationException => Success(None)

                    // This will be reported as an actual error
                    case e: Throwable => Failure(e)
                  }

                println(
                  compact(
                    render(
                      ("seq" -> seqId) ~ ("results" ->
                        (cmd \ "case" \ "tests")
                          .extract[List[JObject]]
                          .map { test =>
                            val jsonResult: JObject = maybeSchema match {
                              case Success(Some(schema)) =>
                                // Get the validation result
                                val result =
                                  Try(!schema.isAnomalous(test \ "instance"))

                                result match {
                                  case Success(valid) => ("valid" -> valid)

                                  // Pass on the error from validation
                                  case Failure(e) =>
                                    ("errored" -> true) ~ ("context" -> ("message" -> e.getMessage) ~ ("traceback" -> e.getStackTrace
                                      .mkString("\n")))
                                }

                              // Skip this test since it raised UnsupportedOperationException earlier
                              case Success(None) => ("skipped" -> true)

                              // Pass on the error from schema parsing
                              case Failure(e) =>
                                ("errored" -> true) ~ ("context" -> ("message" -> e.getMessage) ~ ("traceback" -> e.getStackTrace
                                  .mkString("\n")))
                            }

                            jsonResult
                          })
                    )
                  )
                )
              }
            }
          }
        }
      case None => System.exit(1)
    }
  }
}
