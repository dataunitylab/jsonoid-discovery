import Dependencies._

ThisBuild / scalaVersion      := "2.11.12"
ThisBuild / versionScheme     := Some("early-semver")
ThisBuild / organization      := "edu.rit.cs"
ThisBuild / organizationName  := "Rochester Institute of Technology"
ThisBuild / githubOwner       := "michaelmior"
ThisBuild / githubRepository  := "jsonoid-discovery"

mainClass := Some("edu.rit.cs.mmior.jsonoid.discovery.DiscoverSchema")

Global / onChangedBuildSource := ReloadOnSourceChanges

val nonConsoleCompilerOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Ywarn-unused-import",
  "-deprecation"
)

lazy val root = (project in file("."))
  .settings(
    name := "JSONoid Discovery",
    resolvers += Resolver.githubPackages("michaelmior"),
    libraryDependencies ++= Seq(
        bloomFilter,
        json4s,
        json4sScalaz,
        hyperLogLog,
        jsonSchemaValidator,
        scopt,
        scalaCsv,

        spark % "provided",
        sparkSql % "provided",

        scalaTest % Test,
    ),
    dependencyOverrides ++= Seq(
      jacksonDatabind,
    ),
    scalacOptions ++= nonConsoleCompilerOptions,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "edu.rit.cs.mmior.jsonoid"
  )

wartremoverErrors ++= Seq(
  Wart.ArrayEquals,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.Equals,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.MutableDataStructures,
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

Compile / compile / wartremoverErrors += Wart.NonUnitStatements

Compile / console / scalacOptions := (console / scalacOptions)
  .value.filterNot(opt =>
    opt.contains("wartremover") ||
    nonConsoleCompilerOptions.contains(opt)
)

enablePlugins(BuildInfoPlugin)
enablePlugins(GhpagesPlugin)
enablePlugins(GitVersioning)
enablePlugins(SiteScaladocPlugin)

git.remoteRepo := "git@github.com:michaelmior/jsonoid-discovery.git"
git.useGitDescribe := true

Test / fork := true

val metaschemaFile = settingKey[File]("Metaschema file object")
metaschemaFile := (Compile / resourceManaged).value / "site" / "metaschema.json"

val metaschemaUrl = settingKey[String]("Metaschema URL")
metaschemaUrl := {if (sys.env.getOrElse("METASCHEMA_LOCAL", "0").toInt == 1)
  "file://" + metaschemaFile.value.getPath.toString
else
  "file://" + metaschemaFile.value.getPath.toString
}

Compile / resourceGenerators += Def.task {
  val metaschema = IO.read(new java.io.File("metaschema-template.json"))
  IO.write(metaschemaFile.value, metaschema.replaceAll("METASCHEMA_URL", metaschemaUrl.value))
  Seq(metaschemaFile.value)
}.taskValue

Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "edu" / "rit" / "cs" / "mmior" / "jsonoid" / "discovery" / "Metaschema.scala"
  IO.write(file, "package edu.rit.cs.mmior.jsonoid.discovery\nobject Metaschema { val url = \"" + metaschemaUrl.value + "\" }")
  Seq(file)
}.taskValue

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

run / connectInput := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
