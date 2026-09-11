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

// Settings shared by both projects. The compiler option list is defined here,
// once, and applied unscoped so that it governs Compile, Test and the REPL
// alike. `-release 21` pins the bytecode level to JVM 21 (class-file major
// version 65) while also checking the sources against the JDK 21 API, and the
// final option promotes every remaining warning to an error, so the build is
// warning-clean by construction. No warning is filtered or silenced anywhere
// in this build, and no option below is ever scoped away.
lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
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
  ),
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
  // Scala-only source roots. Each project is restricted to its own
  // src/{main,test}/scala, which makes it impossible for this build to compile
  // a Java source - including any src/main/java below the build root, which
  // the root project would otherwise inherit.
  Compile / unmanagedSourceDirectories := Seq((Compile / scalaSource).value),
  Test / unmanagedSourceDirectories := Seq((Test / scalaSource).value),
  // Tests run in a forked JVM, so both output locations below are absolute and
  // anchored at the build root rather than at a project base directory: the two
  // projects have different base directories, and the parity harness plus the
  // gate script expect all reports under <build root>/target.
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

// `aggregate` makes a single `sbt test` at the build root run both projects'
// suites; the "compile->compile;test->test" edge gives strata-basics both the
// main classes and the test-scope helpers (testkit, generators) of
// strata-collect, replacing the Maven main-jar plus test-jar dependency pair.
lazy val `strata-basics` = Project("strata-basics", file("."))
  .aggregate(`strata-collect`)
  .dependsOn(`strata-collect` % "compile->compile;test->test")
  .settings(name := "strata-basics")
  .settings(commonSettings)
  .settings(
    // The root project keeps its sources, resources and build output under
    // strata-basics/ rather than at the build root, so the layout is
    // symmetrical with strata-collect and the pre-existing src/ tree at the
    // build root - which belongs to the Maven build - is never read.
    Compile / scalaSource := baseDirectory.value / "strata-basics" / "src" / "main" / "scala",
    Test / scalaSource := baseDirectory.value / "strata-basics" / "src" / "test" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "strata-basics" / "src" / "main" / "resources",
    Test / resourceDirectory := baseDirectory.value / "strata-basics" / "src" / "test" / "resources",
    target := baseDirectory.value / "strata-basics" / "target",
    // `sbt "strata-basics/run"` launches the demo without prompting. The demo
    // is an IOApp, so it runs in its own JVM: that gives it the main thread and
    // therefore the ordinary cats-effect shutdown and resource-cleanup path.
    Compile / mainClass := Some("com.opengamma.strata.basics.demo.BasicsDemoApp"),
    Compile / run / fork := true,
    // Law-checking libraries: only this project hosts the typeclass law suite,
    // so they stay off the strata-collect classpath.
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.typelevel" %% "discipline-scalatest" % disciplineScalaTestVersion % Test
    )
  )
