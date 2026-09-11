// ---------------------------------------------------------------------------
// Scala port of the OpenGamma Strata `strata-collect` / `strata-basics`
// modules. Exactly two sbt projects exist in this build:
//
//   strata-collect  -> strata-collect/  (the ported subset of Java collect)
//   strata-basics   -> the ROOT project, with its source roots relocated
//                      into strata-basics/ so the layout stays symmetrical
//
// sbt always materialises a project rooted at the build root, so making
// `strata-basics` that project is the only way to end up with exactly the
// two project ids. The Maven tree under modules/** is untouched and is never
// referenced by this build.
// ---------------------------------------------------------------------------

ThisBuild / scalaVersion := "2.13.18"
ThisBuild / organization := "com.opengamma.strata"
ThisBuild / version := "2.12.74-SNAPSHOT"

lazy val catsVersion = "2.13.0"
lazy val catsEffectVersion = "3.7.1"
lazy val circeVersion = "0.14.16"
lazy val scalaTestVersion = "3.2.20"
lazy val scalaCheckVersion = "1.20.0"
lazy val scalaTestPlusVersion = "3.2.20.0"
lazy val catsEffectTestingVersion = "1.8.0"
lazy val disciplineScalaTestVersion = "2.3.0"

lazy val commonScalacOptions = Seq(
  "-release",
  "21",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint:_",
  "-Wunused:_",
  "-Wvalue-discard",
  "-Wnumeric-widen",
  "-Werror"
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-effect" % catsEffectVersion,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test,
    "org.scalatestplus" %% "scalacheck-1-20" % scalaTestPlusVersion % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % catsEffectTestingVersion % Test
  ),
  // Scala-only source roots: no src/main/java can ever be compiled.
  Compile / unmanagedSourceDirectories := Seq((Compile / scalaSource).value),
  Test / unmanagedSourceDirectories := Seq((Test / scalaSource).value),
  Test / fork := true,
  Test / javaOptions ++= Seq(
    s"-Dparity.report.dir=${(ThisBuild / baseDirectory).value}/target/parity-report"
  ),
  Test / testOptions += Tests.Argument(
    TestFrameworks.ScalaTest,
    "-u",
    s"${(ThisBuild / baseDirectory).value}/target/test-reports"
  )
)

lazy val `strata-collect` = Project("strata-collect", file("strata-collect"))
  .settings(name := "strata-collect")
  .settings(commonSettings)

lazy val `strata-basics` = Project("strata-basics", file("."))
  .aggregate(`strata-collect`)
  .dependsOn(`strata-collect` % "compile->compile;test->test")
  .settings(name := "strata-basics")
  .settings(commonSettings)
  .settings(
    // The root project keeps its sources under strata-basics/ rather than at
    // the build root, so the repository root stays free of a src/ tree.
    Compile / scalaSource := baseDirectory.value / "strata-basics" / "src" / "main" / "scala",
    Test / scalaSource := baseDirectory.value / "strata-basics" / "src" / "test" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "strata-basics" / "src" / "main" / "resources",
    Test / resourceDirectory := baseDirectory.value / "strata-basics" / "src" / "test" / "resources",
    target := baseDirectory.value / "strata-basics" / "target",
    Compile / mainClass := Some("com.opengamma.strata.basics.demo.BasicsDemoApp"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.typelevel" %% "discipline-scalatest" % disciplineScalaTestVersion % Test
    )
  )
