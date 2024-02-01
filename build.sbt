Test / parallelExecution := false

val chiselVersion = "5.1.0"

lazy val fixedpointSettings = Seq(
  name := "fixedpoint",
  version := "0.1",
  scalaVersion := "2.13.10",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-language:reflectiveCalls",
    "-Ymacro-annotations",
    "language:higherKinds",
    "-Ydelambdafy:inline"
  ),
  libraryDependencies ++= Seq(
    "org.chipsalliance" %% "chisel" % chiselVersion,
    "org.scalatest" %% "scalatest" % "3.2.15" % "test",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test"
  ),
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.2").cross(CrossVersion.full)),
  addCompilerPlugin(("org.chipsalliance" %% "chisel-plugin" % chiselVersion).cross(CrossVersion.full))
)

lazy val chiseltest = (project in file("chiseltest"))

lazy val fixedpoint = (project in file("."))
  .dependsOn(chiseltest)
  .settings(fixedpointSettings)
