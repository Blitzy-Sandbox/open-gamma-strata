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

import sbt.internal.{AppenderSupplier, LogManager}
import sbt.internal.util.{Appender, ConsoleAppender, ConsoleOut}

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

// ---------------------------------------------------------------------------
// Durable capture of a test task's log.
//
// A ScalaCheck property failure is reported through an exception that carries the
// falsifying values, and those values are this port's own types, which are
// deliberately not java.io.Serializable. When tests are forked, ScalaTest cannot
// send such an exception over the socket that links the forked JVM to sbt, so it
// substitutes an empty NotSerializableWrapperException before transmitting the
// event. Every reporter configured through `testOptions` - the JUnit XML writer
// and the file reporter alike - is built on the sbt side of that socket and
// therefore sees only the substitute: the falsifying values, the failure location
// and the `Init Seed` that reproduces the run are absent from those files.
//
// The one place the original text survives is the log channel: ScalaTest's
// sbt-log reporter runs inside the forked JVM and logs the full failure block,
// which is why the console shows it. Attaching an appender to the test tasks'
// loggers copies exactly that text into a file, so a property failure stays
// reproducible from the persisted reports rather than from a console scrollback
// nobody kept.
//
// The appender is built inside the supplier, which sbt calls when it creates the
// logger for a test task - that is, after any `clean` in the same session - so
// the file is (re)created per run and holds that run's log.
// ---------------------------------------------------------------------------
def testLogAppender(logFile: File): Appender = {
  IO.createDirectory(logFile.getParentFile)
  val stream = new java.io.PrintStream(new java.io.FileOutputStream(logFile, false), true)
  ConsoleAppender(logFile.getName, ConsoleOut.printStreamOut(stream), false)
}

def capturingLogManager(logFile: File): LogManager =
  LogManager.withLoggers(extra = new AppenderSupplier {
    override def apply(key: Def.ScopedKey[_]): Seq[Appender] = Seq(testLogAppender(logFile))
  })

// The captured log of one project's tests, beside that project's JUnit XML. The
// name carries the project id, so the two projects of an aggregated run keep
// their logs apart, and it is not of the form the test-count gate globs
// (`TEST-*.xml`), so it cannot be mistaken for a suite report.
lazy val testLogFile = Def.setting(
  testReportDirectory((ThisBuild / baseDirectory).value) / s"test-log-${name.value}.txt"
)

// `logManager` is read by sbt when it builds a task's logger rather than by
// another setting or task, so sbt's unused-key lint cannot see the three
// definitions below being consumed and reports them as unused. They are
// consumed - the captured log files prove it - so the key is excluded from that
// one check, and from that check only: nothing about compiler warnings, which
// remain errors, is affected.
Global / excludeLintKeys += logManager

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
  // Tests run in a forked JVM, so both output locations below are absolute and
  // anchored at the build root rather than at a project base directory: the two
  // projects have different base directories, and the parity harness plus the
  // gate script expect all reports under <build root>/target.
  Test / fork := true,
  Test / javaOptions ++= Seq(
    s"-Dparity.report.dir=${parityReportDirectory((ThisBuild / baseDirectory).value).getAbsolutePath}"
  ),
  // Exactly one writer of JUnit XML. The default value of this setting is sbt's
  // own JUnitXmlTestsListener, which writes a second copy of every suite's XML
  // into each project's target/test-reports - so the same suite exists twice on
  // disk and a recursive read of the tree counts every test twice. The ScalaTest
  // `-u` reporter configured below is the single writer, into the one
  // build-root-anchored directory. Clearing this setting drops only that
  // listener: the task-scoped value of `testListeners` keeps sbt's console
  // TestLogger and the TestStatusReporter that `testQuick` relies on.
  Test / testListeners := Nil,
  // The log capture described above, on each task that runs tests. It is scoped
  // per task rather than globally, so it touches the logging of nothing else in
  // the build.
  Test / test / logManager := capturingLogManager(testLogFile.value),
  Test / testOnly / logManager := capturingLogManager(testLogFile.value),
  Test / testQuick / logManager := capturingLogManager(testLogFile.value),
  Test / testOptions ++= {
    val reports = testReportDirectory((ThisBuild / baseDirectory).value)
    // The `-u` reporter creates its directory itself, but the file reporter
    // below opens its file without creating parent directories, and `clean`
    // deletes this directory (it is registered with `cleanFiles`). Creating it
    // here - in the sbt JVM, while the test task's options are computed, before
    // the forked test JVM starts - makes each reporter independent of the
    // other's side effects and of the order the arguments are parsed in.
    IO.createDirectory(reports)
    Seq(
      // One JUnit XML file per suite: the machine-readable artifact whose
      // `tests` attributes the test-count gate sums.
      Tests.Argument(
        TestFrameworks.ScalaTest,
        "-u",
        reports.getAbsolutePath
      ),
      // ScalaTest's own run log as a file: every suite and test name with its
      // verdict, each ordinary failure's message and source location, and the
      // run summary. It makes a run readable without parsing the per-suite XML
      // and without a terminal scrollback, and it is published with the XML because
      // CI stores the whole target/test-reports directory. The file name carries
      // the project id, so the two projects of an aggregated run never write to
      // the same file, and the `W` drops the terminal colour codes, which a file
      // has no use for and which would otherwise sit between every line and the
      // text a reader or a grep is looking for.
      //
      // This reporter is built on the sbt side of the socket described above, so
      // a ScalaCheck property failure reaches it already substituted: its
      // counterexample is in the captured log next to it, not here.
      Tests.Argument(
        TestFrameworks.ScalaTest,
        "-fW",
        (reports / s"scalatest-${name.value}.txt").getAbsolutePath
      )
    )
  }
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
