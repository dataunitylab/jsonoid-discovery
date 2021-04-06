import Dependencies._

ThisBuild / scalaVersion      := "2.12.13"
ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / organization      := "edu.rit.cs"
ThisBuild / organizationName  := "Rochester Institute of Technology"

lazy val root = (project in file("."))
  .settings(
    name := "JSONoid Discovery",
    libraryDependencies ++= Seq(
        // Scala
        json4s,
        json4sScalaz,
        jsonSchemaValidator,

        spark % "provided",
        sparkSql % "provided",

        scalaTest % Test,
    ),
    dependencyOverrides ++= Seq(
      jacksonDatabind,
    )
  )

wartremoverErrors ++= Seq(
  Wart.ArrayEquals,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.Equals,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.MutableDataStructures,
  Wart.NonUnitStatements,
  Wart.Null,
  Wart.Option2Iterable,
  Wart.OptionPartial,
  Wart.PublicInference,
  Wart.Recursion,
  Wart.Return,
  Wart.StringPlusAny,
  Wart.TraversableOps,
  Wart.TryPartial,
  Wart.Var,
  Wart.While,
)

enablePlugins(GhpagesPlugin)
enablePlugins(SiteScaladocPlugin)

git.remoteRepo := "git@github.com:michaelmior/jsonoid-discovery.git"

fork in Test := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
