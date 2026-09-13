ThisBuild / scalaVersion := "2.13.18"
ThisBuild / organization := "com.opengamma.strata"
ThisBuild / version := "2.12.74-SNAPSHOT"

// ---------------------------------------------------------------------------
// The two directories the acceptance gates collect their artifacts from.
//
// Both are anchored at the build root and handed to the test JVMs as absolute paths.
// That is not a stylistic choice: tests are forked, the two projects have different base
// directories, and the gate script reads the parity reports and the JUnit XML from one
// place each. A relative path would resolve against whichever working directory a forked
// JVM happened to have, and the consumers therefore refuse one - see
// `ParityHarness.reportDirectoryFrom` and the equivalent in `DoubleArrayParitySpec`.
//
// Both directories also sit outside every project's own build output, because the root
// project keeps its output under strata-basics/. They are consequently registered with
// `cleanFiles` on the root project below, so that `sbt clean` empties them: a report or a
// test-report file left behind by an earlier run is indistinguishable from one this run
// produced, and a gate that aggregates the directory would count it.
// ---------------------------------------------------------------------------
def parityReportDirectory(buildRoot: File): File = buildRoot / "target" / "parity-report"

def testReportDirectory(buildRoot: File): File = buildRoot / "target" / "test-reports"

lazy val catsVersion = "2.13.0"
lazy val catsEffectVersion = "3.7.1"
lazy val circeVersion = "0.14.16"
lazy val scalaTestVersion = "3.2.20"
lazy val scalaCheckVersion = "1.20.0"
lazy val scalaTestPlusVersion = "3.2.20.0"
lazy val catsEffectTestingVersion = "1.8.0"
lazy val disciplineScalaTestVersion = "2.3.0"

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
  // Restricting the directories is necessary but not sufficient: sbt's default
  // include filter for unmanaged sources is `"*.java" | "*.scala"`, so a Java
  // source placed inside one of the Scala directories above would still be
  // compiled and its class file emitted. Filtering on the extension as well
  // makes "Scala-only" a property of file types and not merely of locations, so
  // no Java source anywhere can enter either module. Resources are unaffected:
  // they are selected by `unmanagedResources / includeFilter`, which is left at
  // its default, so test fixtures and manifests are still copied as before.
  Compile / unmanagedSources / includeFilter := "*.scala",
  Test / unmanagedSources / includeFilter := "*.scala",
  Test / fork := true,
  Test / javaOptions ++= Seq(
    s"-Dparity.report.dir=${parityReportDirectory((ThisBuild / baseDirectory).value).getAbsolutePath}"
  ),
  // The one test-report setting: ScalaTest's `-u` reporter writes one JUnit XML
  // file per suite, which is the machine-readable artifact whose `tests`
  // attributes the test-count gate sums. The path is absolute and anchored at
  // the build root because tests are forked and the gate reads a single
  // directory for both projects. The reporter creates that directory itself, so
  // this setting performs no filesystem work while it is evaluated.
  Test / testOptions += Tests.Argument(
    TestFrameworks.ScalaTest,
    "-u",
    testReportDirectory((ThisBuild / baseDirectory).value).getAbsolutePath
  )
)

lazy val `strata-collect` = Project("strata-collect", file("strata-collect"))
  .settings(name := "strata-collect")
  .settings(commonSettings)

// sbt materialises a project rooted at the build root whether or not one is
// declared, so `strata-basics` has to BE that project: it takes `file(".")` as
// its base and relocates its source roots, below.
//
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
    // The two gate artifact directories are outside both projects' `target`, so nothing
    // would otherwise remove them. Registering them here - on the aggregating root
    // project only, so an aggregated `clean` deletes each of them exactly once rather
    // than racing a second delete of the same tree - makes `sbt clean` the step that
    // guarantees a run's reports are that run's. Note that this deletes the two report
    // directories and nothing else under the build root's target, which also holds sbt's
    // own live logging and streams directories.
    cleanFiles ++= Seq(
      parityReportDirectory((ThisBuild / baseDirectory).value),
      testReportDirectory((ThisBuild / baseDirectory).value)
    ),
    Compile / mainClass := Some("com.opengamma.strata.basics.demo.BasicsDemoApp"),
    // Fork the demo so cats-effect owns the process lifecycle: the IOApp gets
    // its own main thread, shutdown hook and cancellation path.
    Compile / run / fork := true,
    // Law-checking libraries: only this project hosts the typeclass law suite,
    // so they stay off the strata-collect classpath.
    //
    // The invariant: exactly one ScalaTest/ScalaCheck adapter on this test
    // classpath, the `scalacheck-1-20` one above that matches the resolved
    // ScalaCheck. discipline-scalatest declares `scalacheck-1-18` instead - a
    // different artifact id, so nothing evicts it, and the two publish the same
    // class names with a byte-different `CheckerAsserting`. The exclusion is safe
    // because discipline-scalatest reaches the adapter only through `Checkers`,
    // identical in both; Gate 2's duplicate-class audit keeps it that way.
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      ("org.typelevel" %% "discipline-scalatest" % disciplineScalaTestVersion % Test)
        .exclude("org.scalatestplus", "scalacheck-1-18_2.13")
    )
  )
