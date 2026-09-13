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
// Two writers can produce the per-suite JUnit XML, and only one of them can
// report this port's failures at all.
//
//   * ScalaTest's own `-u` reporter, which the technical specification names,
//     cannot: with forked tests it runs in the SBT JVM rather than in the fork
//     and is fed by ScalaTest's slave-to-master socket, which carries each
//     event as a JAVA-SERIALIZED object. No type in this port takes part in
//     Java serialization, so a table- or property-check failure whose clue
//     holds a domain value throws inside `ObjectOutputStream` and takes the
//     reporter, the run's verdict and the whole test task with it - see
//     `withoutRemoteReporting` below for that failure in full. Configuring it
//     also silences ScalaTest's sbt-log reporting, because ScalaTest logs
//     through sbt only while no reporter of its own is configured, so the
//     console would lose its per-test lines and its failure detail as well.
//     It is therefore NOT configured, which is this build's one deliberate
//     divergence from AAP section 0.3.1's wording; what that wording is FOR -
//     one directory of `TEST-<suite>.xml` whose `tests` attributes the
//     test-count gate sums - is delivered below, by the other writer.
//
//   * sbt's own `JUnitXmlTestsListener`, which can: it is fed by sbt's fork
//     protocol - names, counts and rendered messages, never serialized domain
//     objects - so it records every suite and every failing case whatever the
//     failure holds. It demonstrably wrote a complete report for the very
//     failure that killed ScalaTest's socket. It is therefore the single
//     authoritative writer, retargeted from its default (each project's own
//     `target/test-reports`, which would be a second and a third copy of the
//     artifact the gate sums) to the one configured directory. Its per-suite
//     file name, its `tests`/`failures`/`errors` attributes and its
//     `<testcase>` `classname` and `name` are the same as ScalaTest's
//     reporter's, which is what keeps the test-count gate and the Java-to-Scala
//     traceability join reading exactly what they read before.
//
// A single writer means a report is missing only if writing it failed, so
// `reportAuditingTestResultLogger` below does not complete one report set from
// another: it checks, against sbt's own account of the run, that every suite
// that ran has a report of this run covering at least the cases and failures
// sbt counted, and fails the task naming the suites for which that is untrue.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// The test framework, with ScalaTest's slave-to-master socket taken out of it.
//
// ScalaTest's sbt integration decides between two modes by one thing only: the
// `remoteArgs` it is handed. `Framework.runner` reads
//
//     if (remoteArgs.isEmpty) parse the real reporter arguments
//     else                    replace them all with `-K <host> <port>`
//
// and sbt asks the runner it creates in ITS OWN jvm for `remoteArgs()`, which -
// unconditionally, whatever reporters are configured - opens a `ServerSocket`,
// starts a thread accepting on it, and returns its host and port for the fork
// to connect back to (`Framework$ScalaTestRunner.remoteArgs`). Forked tests
// then run in "slave" mode: the fork's only reporter is a `SocketReporter` that
// writes every ScalaTest `Event` to that socket as a JAVA-SERIALIZED object,
// and the sbt-side runner rebuilds the run from what it reads.
//
// That is fatal here, and it is fatal by design on both sides. A `TestFailed`
// event carries the throwable that failed the test, and a table- or
// property-check failure carries the row that falsified it - a domain value.
// No type in this port takes part in Java serialization: writing one throws
// `IllegalArgumentException` from its own `writeObject`, which is a deliberate,
// audited property of the port and not a defect to be relaxed. So the write
// throws mid-object, the socket's object stream is left corrupt, ScalaTest
// prints "Reporter completed abruptly", the sbt side prints "Unable to read
// from client" and returns its accept loop to `ServerSocket.accept`, and
// `ScalaTestRunner.done()` - which ends with an unbounded `Thread.join()` on
// that accept thread - never returns. The test task hangs forever: no verdict,
// no failure list, and no result logger, so nothing below this point in the
// file ever runs and the report set is whatever was written before the hang.
//
// Handing the fork an EMPTY `remoteArgs` removes that whole path. The socket is
// never opened, because only `remoteArgs()` opens it and this runner never
// asks the delegate for its own; and the fork, seeing no remote arguments,
// runs ScalaTest in ordinary master mode. No reporter of ScalaTest's own is
// configured by this build - the block above says why the `-u` reporter is
// not - so the fork reports the only two ways left, both of them
// sbt's: through the `EventHandler`, which is what sbt's own result line and
// the JUnit XML listener are built from, and through the `Logger`s sbt gave
// it, which is where the per-test console lines and a failure's rendered
// detail come from. Both cross the fork boundary as sbt's own
// `ForkEvent`/`ForkError`, carrying names, statuses and RENDERED messages
// rather than domain objects, so no test outcome can be lost to serialization
// again, whatever a failing test holds.
//
// Nothing else about the framework is changed: detection, task creation and
// argument handling are the delegate's. `done()` is delegated for its cleanup
// and its result discarded, because in this arrangement the sbt-side runner
// observes no events and its summary would therefore describe an empty run,
// which printed beside a real one states something untrue about it. What the
// console carries instead is sbt's own "Passed/Failed: Total n, Failed n,
// Errors n, Passed n" line, counted from the events sbt received, and the
// audit line `auditTestReports` logs.
//
// The regression row of scripts/verify-gates.sh holds this in place: it runs a
// suite that fails from inside a table whose rows hold a domain value, under a
// timeout, and requires a prompt non-zero exit and a complete report for it.
// Remove this wrapper and that row hangs until its timeout and fails.
// ---------------------------------------------------------------------------
def withoutRemoteReporting(delegate: sbt.testing.Framework): sbt.testing.Framework =
  new sbt.testing.Framework {
    def name(): String = delegate.name()

    def fingerprints(): Array[sbt.testing.Fingerprint] = delegate.fingerprints()

    def runner(
        args: Array[String],
        remoteArgs: Array[String],
        testClassLoader: ClassLoader): sbt.testing.Runner = {
      val underlying = delegate.runner(args, remoteArgs, testClassLoader)
      new sbt.testing.Runner {
        def tasks(taskDefs: Array[sbt.testing.TaskDef]): Array[sbt.testing.Task] =
          underlying.tasks(taskDefs)

        def args(): Array[String] = underlying.args()

        // Never delegated. Asking the delegate for its remote arguments is
        // precisely what opens the socket, so the one way not to have one is
        // not to ask - and an empty answer is what tells the fork to report
        // through sbt's own protocol instead.
        def remoteArgs(): Array[String] = Array.empty[String]

        def done(): String = {
          // The delegate's own end-of-run work still happens; only its
          // summary string is dropped, and deliberately: this runner saw no
          // events, so that string describes an empty run.
          val _ = underlying.done()
          ""
        }
      }
    }
  }

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
 * Makes the JUnit report directory usable before a single test runs, and
 * records when the test task started.
 *
 * A report writer discovers an unusable report path one file at a time, fails
 * each write on its own, and leaves the run reporting success with no
 * machine-readable evidence at all. Deciding it here, once, converts that into
 * a task failure before any test has run.
 *
 * @param project  the project id, under which this task's start time is recorded
 * @param reportDirectory  the one directory the JUnit XML is written to
 */
def prepareTestReports(project: String, reportDirectory: File): Unit = {
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
 * Audits this run's JUnit XML against sbt's own account of the run, and says
 * what is missing from it.
 *
 * For every suite sbt saw, the configured report is compared with what sbt
 * itself recorded for that suite: a report that is absent, older than this
 * task, covering fewer cases, or recording fewer failures than sbt counted is
 * a report that cannot be read as evidence of this run, and is returned - one
 * line per suite, naming what is wrong with it - for the caller to fail the
 * task with. The comparison is what makes a lost or half-written report a
 * failure rather than a run that looks like it passed: an incomplete set of
 * reports all saying `failures="0"` is indistinguishable from a green run.
 *
 * @param log  the task logger; the run's own counts are reported through it
 * @param output  sbt's own result for the run
 * @param project  the project id whose start time gates freshness
 * @param reportDirectory  the one directory the JUnit XML is written to
 * @param emptyRunIsFailure  true when executing no suite at all is itself a
 *   failure, which is the case for a whole-project `test` and not for a
 *   filtered `testOnly` that may legitimately match nothing in a project
 * @param taskName  the task being audited, for the messages
 * @return the problems found, empty when the report set is complete
 */
def auditTestReports(
    log: Logger,
    output: Tests.Output,
    project: String,
    reportDirectory: File,
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
    val counted = suites.map {
      case (suite, result) =>
        val cases = result.passedCount + result.failureCount + result.errorCount +
          result.skippedCount + result.ignoredCount + result.canceledCount + result.pendingCount
        val failures = result.failureCount + result.errorCount
        val report = reportDirectory / s"TEST-$suite.xml"
        junitSuiteCounts(report) match {
          case None =>
            problems += s"$suite: no report was written for it"
          case Some(_) if freshnessFloor.exists(floor => report.lastModified < floor) =>
            problems += s"$suite: its report is one an earlier run left behind"
          case Some((declared, _, _)) if declared < cases =>
            problems += s"$suite: its report covers $declared of the $cases case(s) that ran"
          case Some((_, reportedFailures, reportedErrors))
              if reportedFailures + reportedErrors < failures =>
            problems +=
              s"$suite: its report records ${reportedFailures + reportedErrors} of the $failures " +
                "failure(s) it had"
          case Some(_) =>
            ()
        }
        (cases, failures)
    }
    // This task's own account of the run, from sbt's test events rather than
    // from any report file: the number of suites and cases whose reports were
    // just checked, and the failures among them. It is the line a reader and
    // the acceptance gate take the run's size from, and it is stated even when
    // every report is in order, because "the reports are complete" means
    // nothing without saying what they are complete with respect to.
    val totalCases = counted.map { case (cases, _) => cases }.sum
    val totalFailures = counted.map { case (_, failures) => failures }.sum
    log.info(
      s"$taskName: ${suites.size} suite(s), $totalCases test case(s) and $totalFailures " +
        s"failure(s) recorded by sbt; reports in $reportDirectory audited against that.")
    problems.result()
  }
}

/**
 * The result logger for one test task: sbt's own reporting, then the audit.
 *
 * The default logger runs first, so the console still carries sbt's own
 * "Passed/Failed: Total n, Failed n, Errors n, Passed n" line and the list of
 * failed tests. Its verdict is held rather than thrown until the audit has run,
 * so a run that both failed tests and lost reports reports both rather than
 * only the first - and so the audit runs on every outcome, a failing run
 * included, rather than only on the way out of a green one.
 *
 * @param project  the project id
 * @param reportDirectory  the one directory the JUnit XML is written to
 * @param emptyRunIsFailure  true for a whole-project `test`
 * @return the logger to install for that task
 */
def reportAuditingTestResultLogger(
    project: String,
    reportDirectory: File,
    emptyRunIsFailure: Boolean): TestResultLogger =
  TestResultLogger { (log, output, taskName) =>
    val standardOutcome = scala.util.Try(TestResultLogger.Default.run(log, output, taskName))
    val problems =
      auditTestReports(
        log,
        output,
        project,
        reportDirectory,
        emptyRunIsFailure,
        taskName)
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
  // Forked tests report through sbt's own fork protocol rather than through
  // ScalaTest's slave-to-master socket; see `withoutRemoteReporting` above for
  // why that socket cannot carry this port's test failures. It is applied to
  // every framework this build loads, because the decision belongs to the
  // build - the fork's reporting path - and not to one framework's arguments.
  Test / loadedTestFrameworks ~= (frameworks =>
    frameworks.map { case (id, framework) => id -> withoutRemoteReporting(framework) }),
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
  // The one test-report setting: sbt's own JUnit XML listener, writing one
  // `TEST-<suite>.xml` per suite into the single configured directory. That is
  // the machine-readable artifact whose `tests` attributes the test-count gate
  // sums, and this listener is the only writer of it - see the commentary above
  // `prepareTestReports` for why ScalaTest's own `-u` reporter cannot be, and
  // what that costs in relation to AAP section 0.3.1's wording.
  //
  // The path is absolute and anchored at the build root because tests are
  // forked and the gate reads a single directory for both projects; the
  // listener's default would be each project's own target/test-reports, a
  // second and a third copy of the artifact being counted. Replacing the list
  // rather than adding to it is what removes that default; the console logger
  // and the test-status reporter are added to this list per task by sbt itself
  // and are unaffected.
  //
  // The File overload is the one that takes the directory it writes into; the
  // String overload appends "test-reports" to what it is given, which is how
  // the default lands in <project>/target/test-reports. The second argument is
  // that default's own: false names each report TEST-<suite>.xml, the name the
  // test-count gate and the Java-to-Scala traceability join both read.
  Test / testListeners := Seq(
    new JUnitXmlTestsListener(
      testReportDirectory((ThisBuild / baseDirectory).value),
      false,
      streams.value.log
    )
  ),
  // Runs in the sbt JVM immediately before the tests of any test task, forked
  // or not.
  Test / testOptions += Tests.Setup(() =>
    prepareTestReports(
      name.value,
      testReportDirectory((ThisBuild / baseDirectory).value)
    )
  ),
  // `testResultLogger` is defined per task, so each task that runs tests gets
  // the audit. Only the whole-project `test` treats "no suite ran at all" as a
  // failure: a filtered `testOnly` or `testQuick` can legitimately match
  // nothing in one of the two projects while matching in the other.
  Test / test / testResultLogger := reportAuditingTestResultLogger(
    name.value,
    testReportDirectory((ThisBuild / baseDirectory).value),
    emptyRunIsFailure = true
  ),
  Test / testOnly / testResultLogger := reportAuditingTestResultLogger(
    name.value,
    testReportDirectory((ThisBuild / baseDirectory).value),
    emptyRunIsFailure = false
  ),
  Test / testQuick / testResultLogger := reportAuditingTestResultLogger(
    name.value,
    testReportDirectory((ThisBuild / baseDirectory).value),
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
