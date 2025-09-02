# AGENTS.md

JSONoid is a JSON Schema discovery tool written in Scala 2.
The goal of JSONoid is to produce a schema in JSON Schema format from a collection of JSON files.
There are two main ways to run the project.
The first is using `DiscoverSchema` from the command line and providing [JSON Lines](https://jsonlines.org/) formatted data on standard input.
The second is to use [Apache Spark](https://spark.apache.org/) for distributed schema discovery.

## Coding standards

- All code is written using Scala 3
- [WartRemover](https://www.wartremover.org/) is used for linting with the list of linters used listed in `build.sbt`
  - Where possible, avoiding writing any code that requires a linter to be disabled
  - For example, `src/main/scala/io/github/dataunitylab/jsonoid/discovery/Helpers.scala` defines `=/=` that can be used instead of `!=` in some cases
- [Scalafmt](https://scalameta.org/scalafmt/) is used to format code
  - Code can be formatted with `sbt scalafmt` or for test code, `Test/scalafmt`
  - After writing any new code, run Scalafmt to ensure that code is formatted correctly
- [Scalafix](https://scalacenter.github.io/scalafix/) is also used as an additional linter
  - Code can be checked with `sbt scalafix` or for test code, `Test/scalafix`
  - After writing any new code, run Scalafix to ensure that all linter rules are followed
- Document all code using Scaladoc whenever possible
- New tests should be written for any new classes or added functionality and tests should all pass before committing

## Project structure

- All source code is under `src/main/scala/io/github/dataunitylab/jsonoid`
  - `discover/schemas` contains classes that represent different JSON Schema types, e.g. `{"type": "number"}` is `NumberSchema`
  - Each schema class contains a number of *monoids* that are used to discover a schema for that type of value
  - `discover/transformers` contains classes used after discovery for schema post-processing for tasks such as detecting possible definitions in the generated schema
  - All code that is specific to distributed schema discovery with Apache Spark is under `discover/spark`
- All tests are under `src/test/scala/io/github/dataunitylab/jsonoid`
  - Tests are run with `sbt test`
  - Individual tests can be run with `sbt testOnly *PATTERN_TO_TEST*`
  - The corresponding test code for a class will be at the same path as the class with `Spec` added to the end of the class name
  - All tests make use of the spec format in [ScalaTest](https://www.scalatest.org/)
- All versions of runtime dependencies are defined in `project/Dependencies.scala`
- Avoid adding new dependencies where possible, but if a new dependency is required, it should first be added to `project/Dependencies.scala` and then added to `build.sbt`
- Any build plugins are defined in `project/plugins.sbt`
- The project uses [GitHub Actions](https://github.com/features/actions) extensively for continuous integration

## Monoids

Monoids are a core concept in JSONoid that are used to discover different properties for the final schema.
Generally a single monoid is responsible for a single property in the final schema.
For example, `MinNumValueProperty` is the monoid responsible for the `minimum` property of a number schema.
There are two core operations defined on monoids: initialization using the `newDefault` method and merging.
Merging uses `mergeValue` to merge in a single value or `intersectMerge` to merge two monoids of the same type.
Each monoid maintains some internal state necessary to produce the final value of the property, which can be accessed in JSON format with the `toJson` method that is used when constructing the final schema.

## Relevant repositories
- [Apache Spark](https://github.com/apache/spark)
- [JSONoid](https://github.com/dataunitylab/jsonoid-dicovery)
- [Scala](https://github.com/scala/scala)
- [Scalafix](https://github.com/scalacenter/scalafix)
- [Scalafmt](https://github.com/scalameta/scalafmt)
- [ScalaTest](https://github.com/scalatest/scalatest)
- [WartRemover](https://github.com/wartremover/wartremover)
