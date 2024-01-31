scalaVersion := "2.13.10"
scalacOptions += "-language:higherKinds"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

scalacOptions += "-Ydelambdafy:inline"
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-language:reflectiveCalls",
  "-Ymacro-annotations"
)
val chiselVersion = "5.1.0"
addCompilerPlugin("org.chipsalliance" %% "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % chiselVersion,
  "edu.berkeley.cs" %% "chiseltest" % "5.0.2",
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test",
)

Test / parallelExecution := false
