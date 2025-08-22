import Dependencies._
import com.typesafe.sbt.packager.docker._
import xerial.sbt.Sonatype._

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organization := "io.github.dataunitylab"
ThisBuild / organizationName := "Rochester Institute of Technology"

inThisBuild(
  List(
    organization := "io.github.dataunitylab",
    homepage := Some(url("https://github.com/dataunitylab/jsonoid-discovery")),
    licenses := List("MIT" -> url("http://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        "michaelmior",
        "Michael Mior",
        "mmior@mail.rit.edu ",
        url("https://michael.mior.ca")
      )
    ),
    sonatypeProjectHosting := Some(
      GitHubHosting(
        "dataunitylab",
        "jsonoid-discovery",
        "mmior@mail.rit.edu"
      )
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

scalafixOnCompile := true
ThisBuild / scalafixDependencies += "net.pixiv" %% "scalafix-pixiv-rule" % "4.5.3"

mainClass := Some("io.github.dataunitylab.jsonoid.discovery.DiscoverSchema")

Global / onChangedBuildSource := ReloadOnSourceChanges

val nonConsoleCompilerOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Ywarn-unused:imports",
  "-deprecation",
  "-release:8"
) ++ {
  if (sys.env.get("ENABLE_ASSERTIONS").isEmpty)
    Seq("-Xdisable-assertions")
  else
    Nil
}

val generateSchemas = taskKey[Unit]("Generate example schemas")

generateSchemas := {
  import sbt.Attributed.data

  import java.nio.file.{FileSystems, Files}

  val r = (Compile / runner).value
  val cp = (Compile / fullClasspath).value

  // Generate the output directory to store generated schemas
  val schemaPath = FileSystems.getDefault().getPath("target", "jsonoid-schemas")
  Files.createDirectories(schemaPath)

  val inputs = List(
    "earthquakes.json",
    "gdp.json",
    "mr-robot.json",
    "nobel.json",
    "rickandmorty.json",
    "test.json",
    "jsonlines-example.json"
  )

  for (input <- inputs) {
    r.run(
      "io.github.dataunitylab.jsonoid.discovery.DiscoverSchema",
      data(cp),
      Seq(
        "src/test/resources/" + input,
        "-p",
        "Simple",
        "-w",
        schemaPath.resolve(input).toString
      ),
      (streams.value: @sbtUnchecked).log
    )
  }
}

lazy val root = (project in file("."))
  .settings(
    name := "JSONoid Discovery",
    libraryDependencies ++= Seq(
      bloomFilter,
      ddSketch,
      json4s,
      json4sScalaz,
      dbscan,
      fuzzySets,
      hyperLogLog,
      jsonSchemaValidator,
      openLocationCode,
      protobuf,
      scopt,
      scalaCsv,
      validator,
      spark % "provided",
      sparkSql % "provided",
      scalactic % Test,
      scalaTest % Test,
      scalaTestPlus % Test,
      scalaCheck % Test
    ),
    dependencyOverrides ++= Seq(
      jacksonDatabind,
      protobuf,
      snappyJava
    ),
    javacOptions ++= Seq("-source", "11", "-target", "11"),
    scalacOptions ++= nonConsoleCompilerOptions,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "io.github.dataunitylab.jsonoid.discovery"
  )

lazy val fuzz = (project in file("fuzz"))
  .settings(
    libraryDependencies ++= Seq(
      jazzer,
      json4s
    )
  )
  .dependsOn(root)

Compile / compile / wartremoverErrors ++= Seq(
  Wart.ArrayEquals,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.Equals,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.FinalVal,
  Wart.JavaConversions,
  Wart.JavaSerializable,
  Wart.LeakingSealed,
  Wart.MutableDataStructures,
  Wart.NonUnitStatements,
  Wart.Null,
  Wart.Option2Iterable,
  Wart.OptionPartial,
  Wart.Product,
  Wart.PublicInference,
  Wart.Recursion,
  Wart.Return,
  Wart.Serializable,
  Wart.StringPlusAny,
  Wart.ToString,
  Wart.TripleQuestionMark,
  Wart.TryPartial,
  Wart.Var,
  Wart.While
)

Compile / console / scalacOptions := (console / scalacOptions).value.filterNot(
  opt => nonConsoleCompilerOptions.contains(opt)
)

enablePlugins(BuildInfoPlugin)
enablePlugins(DockerPlugin)
enablePlugins(GitHubPagesPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(SiteScaladocPlugin)

dockerEntrypoint := Seq("/opt/docker/bin/discover-schema")
dockerBaseImage := "azul/zulu-openjdk:11-jre"

gitHubPagesOrgName := "dataunitylab"
gitHubPagesRepoName := "jsonoid-discovery"
gitHubPagesSiteDir := baseDirectory.value / "target/site"

git.remoteRepo := "git@github.com:dataunitylab/jsonoid-discovery.git"
git.useGitDescribe := true

Test / fork := true

ThisBuild / assemblyShadeRules := Seq(
  ShadeRule.rename("org.json4s.**" -> "shadejson4s.@1").inAll,
  ShadeRule.rename("com.google.protobuf.**" -> "shadeprotobuf.@1").inAll
)

assembly / assemblyMergeStrategy := {
  case "module-info.class"                     => MergeStrategy.discard
  case "META-INF/versions/9/module-info.class" => MergeStrategy.discard
  case x                                       =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
fuzz / assembly / assemblyMergeStrategy := (assembly / assemblyMergeStrategy).value
assembly / assemblyJarName := s"jsonoid-discovery-${version.value}.jar"
fuzz / assembly / assemblyJarName := "fuzz.jar"

import sbtassembly.AssemblyPlugin.defaultUniversalScript
assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false))

run / connectInput := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

apiMappings ++= {
  def mappingsFor(
      organization: String,
      names: List[String],
      location: String,
      revision: (String) => String = identity
  ): Seq[(File, URL)] =
    for {
      entry: Attributed[File] <- (Compile / fullClasspath).value
      module: ModuleID <- entry.get(moduleID.key)
      if module.organization == organization
      if names.exists(module.name.startsWith)
    } yield entry.data -> url(location.format(revision(module.revision)))

  val mappings: Seq[(File, URL)] =
    mappingsFor(
      "org.scala-lang",
      List("scala-library"),
      "http://scala-lang.org/api/%s/"
    )

  mappings.toMap
}

ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"
