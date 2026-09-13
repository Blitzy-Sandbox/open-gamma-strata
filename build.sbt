import scala.util.control.NonFatal

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
// One writer into that directory, and a report set that is either complete or
// loud about not being.
//
// Two independent writers can produce the per-suite JUnit XML, and they fail in
// opposite ways.
//
//   * ScalaTest's `-u` reporter, which the technical specification mandates,
//     runs in the SBT JVM - not in the fork - and is fed by ScalaTest's
//     slave-to-master socket. That socket carries each event as a serialized
//     Java object, and no type in this port takes part in Java serialization:
//     a table- or property-check failure whose clue holds a domain value
//     therefore throws inside `ObjectOutputStream`, corrupts the stream, and
//     every write after it fails with a broken pipe. The reporter stops there,
//     so the report set ends at the first such failure and the framework
//     summary printed afterwards describes only the events that arrived - it
//     can read "All tests passed" for a module that failed.
//
//   * sbt's own `JUnitXmlTestsListener`, which is fed by sbt's fork protocol -
//     names, counts and rendered messages, never serialized domain objects -
//     and therefore records every suite, failing ones included. Left at its
//     default it writes into each project's own `target/test-reports`, which is
//     a second and a third copy of the very artifact the test-count gate sums,
//     so a tree-wide aggregation counts every suite three times.
//
// Neither writer can be pointed at the configured directory alongside the
// other: they choose the same file name for a suite, so both writing there
// would race on one path. The listener is therefore retargeted to a staging
// directory that no consumer reads, and `reportAuditingTestResultLogger` below
// promotes a staged report into the configured directory for exactly the suites
// whose ScalaTest report is missing, left over from an earlier run, or short of
// the cases and failures sbt itself recorded. The staging directory is emptied
// when a test task starts and deleted when it ends, so after any run the
// configured directory is the only place below the build root holding
// `TEST-*.xml`.
// ---------------------------------------------------------------------------
def junitStagingDirectory(projectTarget: File): File = projectTarget / "junit-xml-staging"

// The epoch millisecond at which a project's test task began, keyed by project
// id. The two projects write into one report directory and their test tasks can
// run at the same time, so the audit of one must not read the other's start
// time; and a report file older than the task being audited is a file an
// earlier run left behind rather than evidence of this one.
lazy val testTaskStartedAt: java.util.concurrent.ConcurrentHashMap[String, java.lang.Long] =
  new java.util.concurrent.ConcurrentHashMap[String, java.lang.Long]()

// How much older than the recorded start a report file may be and still count
// as this run's. It absorbs clock and filesystem-timestamp granularity and
// nothing else: a file an earlier run left behind is minutes old, not two
// seconds.
lazy val reportFreshnessSlackMillis: Long = 2000L

/**
 * Makes the JUnit report directory and this project's staging directory usable
 * before a single test runs, and records when the test task started.
 *
 * ScalaTest's reporter discovers an unusable report path one file at a time,
 * fails each write on its own, and leaves the run reporting success with no
 * machine-readable evidence at all. Deciding it here, once, converts that into
 * a task failure before any test has run.
 *
 * @param project  the project id, under which this task's start time is recorded
 * @param reportDirectory  the one directory the JUnit XML is written to
 * @param staging  this project's staging directory for sbt's own listener
 */
def prepareTestReports(project: String, reportDirectory: File, staging: File): Unit = {
  if (reportDirectory.exists && !reportDirectory.isDirectory) {
    throw new sbt.internal.util.MessageOnlyException(
      s"the JUnit report directory $reportDirectory is not a directory, so not one test report " +
        "can be written there. The run stops here rather than passing with no machine-readable " +
        "evidence of what it ran.")
  }
  IO.createDirectory(reportDirectory)
  val probe = reportDirectory / s".report-directory-probe-$project"
  try {
    IO.write(probe, "probe")
  } catch {
    case NonFatal(cause) =>
      throw new sbt.internal.util.MessageOnlyException(
        s"the JUnit report directory $reportDirectory cannot be written to ($cause), so not one " +
          "test report can be written there. The run stops here rather than passing with no " +
          "machine-readable evidence of what it ran.")
  } finally {
    IO.delete(probe)
  }
  IO.delete(staging)
  IO.createDirectory(staging)
  testTaskStartedAt.put(project, java.lang.Long.valueOf(System.currentTimeMillis()))
}

/**
 * The `tests`, `failures` and `errors` a JUnit report declares.
 *
 * Only the opening `testsuite` tag is read, which both writers put at the head
 * of the file, so a suite whose rendered failure text runs to megabytes costs
 * the same as an empty one.
 *
 * @param report  the report file
 * @return the three counts, or None when there is no readable report with a
 *   `testsuite` element and a numeric `tests` attribute
 */
def junitSuiteCounts(report: File): Option[(Int, Int, Int)] =
  if (!report.isFile) {
    None
  } else {
    val head =
      try {
        val source = scala.io.Source.fromFile(report, "UTF-8")
        try source.take(8192).mkString
        finally source.close()
      } catch {
        case NonFatal(_) => ""
      }
    def attribute(name: String, tag: String): Option[Int] =
      s"""\\b$name="([0-9]+)"""".r.findFirstMatchIn(tag).map(matched => matched.group(1).toInt)
    """(?s)<testsuite\b[^>]*>""".r
      .findFirstIn(head)
      .flatMap(tag =>
        attribute("tests", tag).map(tests =>
          (tests, attribute("failures", tag).getOrElse(0), attribute("errors", tag).getOrElse(0))))
  }

/**
 * Completes this run's JUnit XML from sbt's own test events and says what is
 * still missing.
 *
 * For every suite sbt saw, the configured report is compared with what sbt
 * itself recorded for that suite. A report that is absent, older than this
 * task, covering fewer cases, or recording fewer failures than sbt counted is
 * replaced by the staged report sbt's own listener wrote - the two writers
 * agree on the suite name, the case count and every case's name, and differ
 * only in that the staged one still holds the failures the socket lost. What
 * cannot be completed is returned, one line per suite, for the caller to fail
 * the task with.
 *
 * @param log  the task logger; every promotion is reported through it
 * @param output  sbt's own result for the run
 * @param project  the project id whose start time gates freshness
 * @param reportDirectory  the one directory the JUnit XML is written to
 * @param staging  the staging directory holding sbt's own reports
 * @param emptyRunIsFailure  true when executing no suite at all is itself a
 *   failure, which is the case for a whole-project `test` and not for a
 *   filtered `testOnly` that may legitimately match nothing in a project
 * @param taskName  the task being audited, for the messages
 * @return the problems that remain, empty when the report set is complete
 */
def auditTestReports(
    log: Logger,
    output: Tests.Output,
    project: String,
    reportDirectory: File,
    staging: File,
    emptyRunIsFailure: Boolean,
    taskName: String): Seq[String] = {
  val recordedStart = Option(testTaskStartedAt.get(project)).map(started => started.longValue)
  // With no recorded start - a path that never ran the setup action - freshness
  // is not asserted. Existence and the counts still are: a guard that cannot
  // date a file is better than one that fails every run it cannot date.
  val freshnessFloor = recordedStart.map(started => started - reportFreshnessSlackMillis)
  val suites = output.events.toSeq.sortBy { case (suite, _) => suite }
  if (suites.isEmpty) {
    if (emptyRunIsFailure) {
      Seq(
        s"$taskName executed no test suite at all, so it proves nothing: a run that executes " +
          "nothing is indistinguishable from a run in which everything passed, and the report " +
          s"directory $reportDirectory still holds whatever an earlier run left there. Check that " +
          "the test sources are where this project expects them.")
    } else {
      // A filtered task that matches nothing in this project is not a failure -
      // the selector may match in the other project - but it is not evidence
      // either, and any report in the directory belongs to an earlier run. It
      // is said out loud so that a run which executed nothing cannot be read as
      // a run in which everything passed.
      log.warn(
        s"$taskName executed no test suite in this project, so it is evidence about none: any " +
          s"report in $reportDirectory was written by an earlier run.")
      Nil
    }
  } else {
    val problems = Seq.newBuilder[String]
    val promotions = Seq.newBuilder[String]
    suites.foreach {
      case (suite, result) =>
        val cases = result.passedCount + result.failureCount + result.errorCount +
          result.skippedCount + result.ignoredCount + result.canceledCount + result.pendingCount
        val failures = result.failureCount + result.errorCount
        val report = reportDirectory / s"TEST-$suite.xml"
        val staged = staging / s"TEST-$suite.xml"
        def shortcoming: Option[String] =
          junitSuiteCounts(report) match {
            case None =>
              Some("no report was written for it")
            case Some(_) if freshnessFloor.exists(floor => report.lastModified < floor) =>
              Some("its report is one an earlier run left behind")
            case Some((declared, _, _)) if declared < cases =>
              Some(s"its report covers $declared of the $cases case(s) that ran")
            case Some((_, reportedFailures, reportedErrors))
                if reportedFailures + reportedErrors < failures =>
              Some(
                s"its report records ${reportedFailures + reportedErrors} of the $failures " +
                  "failure(s) it had")
            case Some(_) =>
              None
          }
        shortcoming.foreach { reason =>
          if (staged.isFile) {
            IO.copyFile(staged, report)
            shortcoming match {
              case Some(remaining) =>
                problems += s"$suite: $remaining, and sbt's own report for it could not replace that"
              case None =>
                promotions += s"$suite ($reason)"
            }
          } else {
            problems += s"$suite: $reason, and sbt's own test events left none to complete it from"
          }
        }
    }
    val promoted = promotions.result()
    if (promoted.nonEmpty) {
      log.warn(
        s"$taskName: ${promoted.size} of the ${suites.size} suite(s) that ran were reported " +
          "incompletely by ScalaTest's own reporter, so any framework summary above understates " +
          s"this run. Their reports in $reportDirectory were completed from sbt's own test events:")
      promoted.take(12).foreach(promotion => log.warn(s"  $promotion"))
      if (promoted.size > 12) {
        log.warn(s"  and ${promoted.size - 12} more")
      }
    }
    problems.result()
  }
}

/**
 * The result logger for one test task: sbt's own reporting, then the audit.
 *
 * The default logger runs first, so the console still carries the standard
 * summary and the list of failed tests. Its verdict is held rather than thrown
 * until the audit has run, so a run that both failed tests and lost reports
 * reports both rather than only the first.
 *
 * @param project  the project id
 * @param reportDirectory  the one directory the JUnit XML is written to
 * @param staging  this project's staging directory
 * @param emptyRunIsFailure  true for a whole-project `test`
 * @return the logger to install for that task
 */
def reportAuditingTestResultLogger(
    project: String,
    reportDirectory: File,
    staging: File,
    emptyRunIsFailure: Boolean): TestResultLogger =
  TestResultLogger { (log, output, taskName) =>
    val standardOutcome = scala.util.Try(TestResultLogger.Default.run(log, output, taskName))
    val problems =
      auditTestReports(
        log,
        output,
        project,
        reportDirectory,
        staging,
        emptyRunIsFailure,
        taskName)
    IO.delete(staging)
    if (problems.isEmpty) {
      standardOutcome.get
    } else {
      // A run that also failed on its own says so here, because the audit's
      // throw replaces the default logger's and the reason for that one must
      // not be lost with it.
      val alsoFailed = standardOutcome.failed.toOption
        .map(cause => s" The run also failed on its own: ${cause.getMessage}.")
        .getOrElse("")
      if (output.events.isEmpty) {
        throw new sbt.internal.util.MessageOnlyException(problems.mkString(" ") + alsoFailed)
      } else {
        problems.foreach(problem => log.error(problem))
        throw new sbt.internal.util.MessageOnlyException(
          s"$taskName ran ${output.events.size} suite(s) and ${problems.size} of them have no " +
            s"usable report in $reportDirectory, so the machine-readable record of this run is " +
            s"incomplete and nothing may be concluded from it.$alsoFailed")
      }
    }
  }

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
    // circe-parser is Test-scope, which for this one coordinate is a deliberate
    // divergence from AAP §0.5.1's "both, Compile", recorded as row 17 of
    // SCALA_MIGRATION.md section (g). The codecs are written against circe-core and
    // derived by circe-generic, and neither module's main sources read JSON - the demo
    // encodes only - while 43 test sources import `io.circe.parser`. Compile scope
    // therefore put circe-parser, and through it circe-jawn and jawn-parser, on both
    // modules' Compile classpaths, where no source of this build resolves anything from
    // them and where a consumer of the two published libraries would inherit all three.
    // Declaring it Test keeps every test that parses a fixture or a round-trip
    // compiling, and leaves both Compile classpaths at circe-core and circe-generic.
    "io.circe" %% "circe-parser" % circeVersion % Test,
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
  // Two absolute paths handed to the forked test JVMs. The first is where the parity specs
  // write their reports. The second is the build root itself, which the two audit specs that
  // derive their subject matter from the module *sources* - `FailableSurfaceSpec` and
  // `ApiSurfaceSpec` - resolve their paths against. Neither may use the working directory for
  // that: tests are forked, the two projects have different base directories, and a run started
  // by hand or by an IDE has a third. Both specs fall back to locating the root from their own
  // compiled location, so this property makes the answer deterministic rather than supplying
  // the only answer.
  Test / javaOptions ++= Seq(
    s"-Dparity.report.dir=${parityReportDirectory((ThisBuild / baseDirectory).value).getAbsolutePath}",
    // The build root, for the specs that derive a fact from the module sources
    // themselves (the failable-surface and public-surface audits). They read
    // source roots, and a source root resolved against the process working
    // directory is resolved against a different directory in each of the two
    // forks - and against something else again under any other runner - so the
    // anchor is handed in as an absolute path exactly as the parity report
    // directory above is. The name is the one those specs read; see
    // `ApiSurfaceSpec.BuildRootProperty` and `FailableSurfaceSpec.BuildRootProperty`.
    s"-Dstrata.build.root=${(ThisBuild / baseDirectory).value.getAbsolutePath}"
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
  ),
  // sbt's own JUnit XML listener, retargeted from this project's
  // target/test-reports - a second copy of the artifact the gate counts - to a
  // staging directory the audit promotes from. Replacing the list rather than
  // filtering it is what makes the configured directory the only place a report
  // is written; the console logger and the test-status reporter are added to
  // this list per task by sbt itself and are unaffected.
  // The File overload is the one that takes the directory it writes into; the
  // String overload appends "test-reports" to what it is given, which is how
  // the default lands in <project>/target/test-reports. The second argument is
  // that default's own: false names each report TEST-<suite>.xml, the name
  // ScalaTest's reporter and the test-count gate both use.
  Test / testListeners := Seq(
    new JUnitXmlTestsListener(
      junitStagingDirectory(target.value),
      false,
      streams.value.log
    )
  ),
  // Runs in the sbt JVM immediately before the tests of any test task, forked
  // or not.
  Test / testOptions += Tests.Setup(() =>
    prepareTestReports(
      name.value,
      testReportDirectory((ThisBuild / baseDirectory).value),
      junitStagingDirectory(target.value)
    )
  ),
  // `testResultLogger` is defined per task, so each task that runs tests gets
  // the audit. Only the whole-project `test` treats "no suite ran at all" as a
  // failure: a filtered `testOnly` or `testQuick` can legitimately match
  // nothing in one of the two projects while matching in the other.
  Test / test / testResultLogger := reportAuditingTestResultLogger(
    name.value,
    testReportDirectory((ThisBuild / baseDirectory).value),
    junitStagingDirectory(target.value),
    emptyRunIsFailure = true
  ),
  Test / testOnly / testResultLogger := reportAuditingTestResultLogger(
    name.value,
    testReportDirectory((ThisBuild / baseDirectory).value),
    junitStagingDirectory(target.value),
    emptyRunIsFailure = false
  ),
  Test / testQuick / testResultLogger := reportAuditingTestResultLogger(
    name.value,
    testReportDirectory((ThisBuild / baseDirectory).value),
    junitStagingDirectory(target.value),
    emptyRunIsFailure = false
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
