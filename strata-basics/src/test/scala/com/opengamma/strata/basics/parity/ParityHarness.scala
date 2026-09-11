/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.all._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.decode
import io.circe.syntax._

import com.opengamma.strata.collect.io.Resources
import com.opengamma.strata.collect.result.Failure

/**
 * One row of a parity fixture, identified well enough for a discrepancy to be traced back to it.
 *
 * Every row model in this package extends this trait, and the identity it reports is the name a
 * failure is filed under in the published report. Where the captured document gives a row its own
 * `id` key - `daycount-baseline.json`, `holiday-baseline.json`, `fx-baseline.json` and
 * `currency-math-baseline.json` all do - the row model reads that key straight into this member.
 * Where it does not, because the `source` field already names the case closely enough for the
 * capture's purposes - `schedule-baseline.json` is the one fixture of that shape - the row model
 * derives an identity instead, conventionally from `source` together with the inputs that
 * distinguish the row from its neighbours. Section 6 of `tools/parity-capture/README.md` is the
 * authority on which fixture is which, and it puts the rule plainly: a row carries an `id` exactly
 * where `source` cannot identify it alone.
 *
 * Identities are consequently not required to be unique, and the harness never assumes they are;
 * a unique one simply makes a report actionable without reading the fixture beside it. What the
 * identity must be is stable across runs and free of anything that varies between them, because
 * it is written to a file that is compared between runs.
 */
trait ParityRow {

  /** The name this row is reported under. */
  def id: String
}

/**
 * One discrepancy: the row it was found in, and what differed.
 *
 * The message is produced by one of the comparators of [[ParityHarness]] and already names the
 * field, the value the port produced, the value captured from Java and both differences, so a
 * report is diagnosable on its own without re-running the suite that wrote it.
 *
 * @param id  the identity of the row the discrepancy was found in
 * @param message  what differed, at full precision
 */
final case class ParityFailure(id: String, message: String)

/**
 * The published outcome of measuring one fixture.
 *
 * These five fields, in this order, are the report document that `scripts/verify-gates.sh` reads,
 * so nothing may be added to them: no timestamp, no echo of the tolerance, no host name and
 * emphatically no duration. Gate 3 copies `rows` and `passed` into `target/gate-report.md`
 * verbatim and requires `failed` to be zero.
 *
 * ===How the counts relate===
 *
 * `rows` counts fixture rows. `passed` counts the rows that produced no message at all. `failed`
 * counts '''discrepancies''', not rows, because one row can differ in several fields at once and
 * each of them is worth reporting. `passed + failed` therefore equals `rows` only when no row
 * differs in more than one way, and `failed == 0` is equivalent to `passed == rows`, which is the
 * condition the gate actually asserts.
 *
 * @param fixture  the fixture stem, which is also the name of the report file
 * @param rows  the number of fixture rows evaluated
 * @param passed  the number of rows that matched the Java baseline in every respect
 * @param failed  the number of discrepancies found, over all rows
 * @param failures  every discrepancy, in fixture order
 */
final case class ParityReport(
    fixture: String,
    rows: Int,
    passed: Int,
    failed: Int,
    failures: Vector[ParityFailure])

/**
 * The shared measurement apparatus of the five parity specs of this module.
 *
 * ===What is being measured===
 *
 * The port must reproduce the numbers of the Java implementation it replaces. The values it is
 * held to were not re-derived here by hand: they were captured from the untouched '''Java'''
 * modules of the Maven tree by `tools/parity-capture/capture-baseline.jsh`, which cross-checked
 * every one of them against the constants of the Java tests before writing it out, and they are
 * committed as the five baseline documents in `strata-basics/src/test/resources/parity`. Those
 * documents are read-only artefacts. This harness never edits one, never "corrects" an
 * expectation, never loosens a bound and never skips a row: if a row disagrees with the port then
 * the port is wrong, and reporting the disagreement is the whole job. Their schema of record is
 * section 6 of `tools/parity-capture/README.md`.
 *
 * The gate that consumes the measurement (AAP section 0.10.1, Gate 3 for the user's Rule 2) runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/{daycount,schedule,fx,currency-math,holiday}.json` for this
 * module - `double-array.json` is the sibling obligation of `strata-collect` and is written by
 * that module's own spec. This file holds no test and its name deliberately does not end in
 * `ParitySpec`, so the selector above never picks it up.
 *
 * ===The tolerance rule===
 *
 * A numeric comparison passes only when '''both''' bounds hold:
 *
 * {{{
 * |actual - expected| <= 1e-9
 * |actual - expected| <= 1e-9 * max(|actual|, |expected|, 1e-300)
 * }}}
 *
 * The conjunction is the rule; neither bound alone is. Applied on its own, the relative bound would
 * admit an absolute error of a thousand between values of the order of 1e12, and the absolute bound
 * would admit an error a million times the values themselves between values of the order of 1e-15.
 * The floor of `1e-300` keeps the relative bound meaningful as the values approach zero, and keeps
 * the scale from being zero when both values are. Dates, date lists, strings, integers, booleans,
 * names and the `Decimal`-shaped amounts of `Money` and `BigMoney` are compared '''exactly'''
 * through [[assertExact]], never with a tolerance.
 *
 * ===The report is written before anything is asserted===
 *
 * [[runFixture]] evaluates the whole fixture, writes the report, and only then returns; the
 * decision to fail belongs to [[failIfAny]], which a spec calls afterwards. A run that ends in a
 * discrepancy therefore still leaves its counts on disk, which is precisely the case in which they
 * are worth having. A row whose check raises is recorded as a discrepancy of that row rather than
 * being allowed to abandon the run for the same reason.
 *
 * ===The report directory is required===
 *
 * The directory comes from the `parity.report.dir` system property, which `build.sbt` supplies to
 * the forked test JVM. There is no fallback: a default would let a misconfigured run write its
 * reports where the gate does not look and so report a pass that nothing measured. A missing
 * property fails loudly instead.
 *
 * ===Effects, and why they are all here===
 *
 * Reading a fixture, writing a report and lifting a failure into a thrown error are the only
 * effectful acts of the measurement, and the user's Rule 7 confines `cats.effect` in this module's
 * test tree to this package. They are consequently all defined here and nowhere else. The `IO`
 * that a spec sees is only ever the one this object hands it.
 *
 * ===Decoding a fixture===
 *
 * [[load]] decodes a whole document into row models. The captured documents write a non-finite
 * double as one of the three tagged strings `"NaN"`, `"Infinity"` and `"-Infinity"` and every
 * finite one as a full-precision JSON number, which is the single policy of the port. A spec must
 * therefore bring that policy into implicit scope before it derives its row decoders:
 *
 * {{{
 * import com.opengamma.strata.collect.json.Codecs.implicits._
 * }}}
 *
 * An imported implicit outranks the one circe publishes for `Double` in its own companion, so
 * every `Double` and `Option[Double]` of the document reads a tagged value with no ambiguity. The
 * policy matters in practice rather than in theory: `fx-baseline.json` carries a `JPY/CAD` rate of
 * `0.0` whose reciprocal is `Infinity`.
 */
object ParityHarness {

  //-------------------------------------------------------------------------
  // Contract constants. The two tolerances are the user's Rule 2; the property name is an
  // agreement with `build.sbt`; the fixture stems are an agreement with `scripts/verify-gates.sh`.
  //-------------------------------------------------------------------------

  /** The absolute bound of the parity rule. */
  val AbsoluteTolerance: Double = 1e-9

  /** The relative bound of the parity rule. */
  val RelativeTolerance: Double = 1e-9

  /** The floor of the relative bound's scale, which keeps that bound meaningful near zero. */
  private val RelativeFloor: Double = 1e-300

  /** The system property through which the build supplies the report directory. */
  private val ReportDirectoryProperty: String = "parity.report.dir"

  /** The number of discrepancies quoted in a failure message; the report always holds them all. */
  private val FailureMessageLimit: Int = 20

  //-------------------------------------------------------------------------
  // JSON encoding of the report. Derived at compile time, so nothing on this path reflects at
  // run time, and derived in declaration order, which is what fixes the five keys of the
  // document and their order.
  //-------------------------------------------------------------------------

  implicit val parityFailureEncoder: Encoder[ParityFailure] = deriveEncoder[ParityFailure]

  implicit val parityReportEncoder: Encoder[ParityReport] = deriveEncoder[ParityReport]

  //-------------------------------------------------------------------------
  // The two lifts from the error channel into a failed effect.
  //
  // A spec builds its inputs through the same validating factories as any other caller, and an
  // input the port refuses to build is not a parity result of any kind: it is a defect in the
  // port or in the fixture, and it ends the row rather than being counted as a discrepancy. These
  // are the only place in this module's test tree where a `Failure` becomes a thrown error, and
  // they are visible across this package alone so that no spec outside it can acquire the habit.
  //
  // They are never used to observe an expected failure. An `error` row of a fixture is an
  // expectation in its own right and is checked with `assertLeft` or `attemptArgCheck`.
  //-------------------------------------------------------------------------

  /**
   * Lifts a single-cause failure into a failed effect.
   *
   * @param result  the outcome of building something the row needs
   * @return the value; the effect fails with an [[java.lang.IllegalStateException]] carrying the
   *         failure's message when there is none
   */
  private[parity] def raise[A](result: Either[Failure, A]): IO[A] =
    IO.fromEither(result.leftMap(failure => new IllegalStateException(failure.message)))

  /**
   * Lifts an accumulated failure into a failed effect, joining every cause into one message.
   *
   * @param result  the outcome of building something the row needs
   * @return the value; the effect fails with an [[java.lang.IllegalStateException]] carrying every
   *         failure's message, joined by `"; "`, when there is none
   */
  private[parity] def raiseNec[A](result: Either[NonEmptyChain[Failure], A]): IO[A] =
    IO.fromEither(
      result.leftMap(failures =>
        new IllegalStateException(failures.toChain.toList.map(_.message).mkString("; "))))

  //-------------------------------------------------------------------------
  // Loading a fixture.
  //-------------------------------------------------------------------------

  /**
   * Reads and decodes a captured baseline.
   *
   * Every fixture is a top-level JSON array of uniform row objects, so the document is decoded
   * into row models in one step. A document that does not match the row model fails the effect
   * unchanged, carrying the cursor path circe reports: that a fixture and a spec no longer agree
   * on what is being measured is a different fact from the port disagreeing with Java, and
   * absorbing the first into the second would make the gate's verdict worthless.
   *
   * @param resource  the classpath name of the fixture, relative to the test resource root and
   *                  without a leading separator, for example `parity/daycount-baseline.json`
   * @return the decoded rows in fixture order; the effect fails when the resource is absent or
   *         does not decode
   */
  def load[A: Decoder](resource: String): IO[Vector[A]] =
    Resources.readClasspathText(resource).flatMap(text => IO.fromEither(decode[Vector[A]](text)))

  //-------------------------------------------------------------------------
  // The comparators.
  //
  // Each answers with the discrepancies it found: an empty list means the values are at parity.
  // Reporting rather than throwing is deliberate and is what makes a report complete. A row is
  // usually several comparisons at once - a year fraction, a day count and a list of dates - and a
  // caller concatenates their results, so every field that differs is named instead of only the
  // first one, and the remaining rows of the fixture still get measured.
  //-------------------------------------------------------------------------

  /**
   * Compares a number the port produced against the number captured from Java.
   *
   * The order of the three decisions below matters.
   *
   * Bit-identical values are at parity first, before any arithmetic. That is what makes a
   * not-a-number match a not-a-number and an infinity match the same infinity, and it is load
   * bearing rather than decorative: `|Infinity - Infinity|` is not a number, and every comparison
   * against a value that is not a number is false, so a fixture's `"Infinity"` expectation would
   * otherwise be reported as a discrepancy. `fx-baseline.json` carries exactly that case.
   *
   * A pair that is not bit-identical and involves a value outside the finite range differs, and is
   * reported without computing a difference. Two different values outside the finite range, and a
   * value outside it against a finite one, are never within any tolerance.
   *
   * A finite pair is at parity when the absolute difference is within `1e-9` '''and''' also within
   * `1e-9` of the scale of the larger operand, floored at `1e-300`. Both bounds are applied; see
   * the tolerance rule documented on this object for why neither alone would do.
   *
   * Note that positive and negative zero are not bit-identical, so they reach the third decision,
   * where a difference of zero is within both bounds and they are at parity. That is the intended
   * reading: signed zero is a representation, not a quantity, and the fixtures carry `-0.0`.
   *
   * @param label  the name of the field being compared, which is what makes a report readable
   * @param actual  the value the port produced
   * @param expected  the value captured from the Java implementation
   * @return the discrepancy, or nothing when the two are at parity
   */
  def assertParity(label: String, actual: Double, expected: Double): List[String] =
    if (java.lang.Double.compare(actual, expected) == 0) {
      Nil
    } else if (!java.lang.Double.isFinite(actual) || !java.lang.Double.isFinite(expected)) {
      List(
        s"$label: actual $actual, expected $expected; a value outside the finite range " +
          "is at parity only with an identical value")
    } else {
      val absolute = math.abs(actual - expected)
      val scale = math.max(math.max(math.abs(actual), math.abs(expected)), RelativeFloor)
      val relative = absolute / scale
      val withinAbsolute = absolute <= AbsoluteTolerance
      val withinRelative = absolute <= RelativeTolerance * scale
      if (withinAbsolute && withinRelative) {
        Nil
      } else {
        List(
          s"$label: actual $actual, expected $expected, absolute difference $absolute " +
            s"(bound $AbsoluteTolerance, met: $withinAbsolute), relative difference $relative " +
            s"(bound $RelativeTolerance, met: $withinRelative)")
      }
    }

  /**
   * Compares a number that either side may be without.
   *
   * Several fixtures carry an expectation that is present for some rows and JSON nothing for
   * others - a stub that a schedule does not have, a rate a matrix cannot produce - so absence is
   * itself an expectation and is compared as one. Two absences are at parity; two present values
   * are compared by [[assertParity]]; one of each differs.
   *
   * @param label  the name of the field being compared
   * @param actual  the value the port produced, if it produced one
   * @param expected  the value captured from the Java implementation, if it recorded one
   * @return the discrepancy, or nothing when the two are at parity
   */
  def assertParityOpt(label: String, actual: Option[Double], expected: Option[Double]): List[String] =
    (actual, expected) match {
      case (None, None) => Nil
      case (Some(actualValue), Some(expectedValue)) => assertParity(label, actualValue, expectedValue)
      case (Some(actualValue), None) =>
        List(s"$label: actual $actualValue, expected no value")
      case (None, Some(expectedValue)) =>
        List(s"$label: actual no value, expected $expectedValue")
    }

  /**
   * Compares a sequence of numbers element by element.
   *
   * A length difference is reported once and stops there, because a shifted sequence would
   * otherwise report a discrepancy at every index and bury the one fact worth knowing. Equal
   * lengths are compared index by index, each under its own indexed label, so a report names the
   * position that differs.
   *
   * @param label  the name of the field being compared
   * @param actual  the values the port produced
   * @param expected  the values captured from the Java implementation
   * @return the discrepancies, or nothing when every element is at parity
   */
  def assertParitySeq(label: String, actual: Seq[Double], expected: Seq[Double]): List[String] =
    if (actual.size != expected.size) {
      List(
        s"$label: actual ${actual.size} elements, expected ${expected.size} elements " +
          s"(actual ${actual.mkString("[", ", ", "]")}, expected ${expected.mkString("[", ", ", "]")})")
    } else {
      actual.iterator
        .zip(expected.iterator)
        .zipWithIndex
        .flatMap { case ((actualValue, expectedValue), index) =>
          assertParity(s"$label[$index]", actualValue, expectedValue)
        }
        .toList
    }

  /**
   * Compares two values for equality.
   *
   * This is the only comparator permitted for a date, a list of dates, a string, an integer, a
   * boolean, a convention or index name, and the `Decimal`-shaped amount of a `Money` or a
   * `BigMoney`. AAP section 0.6.1 requires date, list and string expectations to compare exactly,
   * and the money amounts are captured as their decimal string form precisely so that they are
   * compared exactly rather than through a tolerance that would hide a rounding defect: rounding
   * to a currency's minor units is a discrete decision, and a value that is out by one unit in the
   * last place is wrong rather than close.
   *
   * @param label  the name of the field being compared
   * @param actual  the value the port produced
   * @param expected  the value captured from the Java implementation
   * @return the discrepancy, or nothing when the two are equal
   */
  def assertExact[A](label: String, actual: A, expected: A): List[String] =
    if (actual == expected) Nil else List(s"$label: actual $actual, expected $expected")

  /**
   * Checks that an operation the fixture records as an error did fail.
   *
   * Every fixture carries `error` rows, and an error is an expectation in its own right rather
   * than a gap in the data: an operation the Java implementation refused must be refused by the
   * port too. Where the port reports that refusal in the error channel - which is where it reports
   * every data-dependent failure - this is how a spec checks it, with no `getOrElse`, no `.value`
   * and no partial function anywhere near an `Either`.
   *
   * The failure value itself is not compared against the captured message. The port's messages are
   * its own, built from its own sealed failure model, and pinning them to the wording of the
   * implementation being replaced would make the fixture a test of prose.
   *
   * @param label  the name of the operation being checked
   * @param actual  the outcome the port produced
   * @return the discrepancy, or nothing when the operation failed as recorded
   */
  def assertLeft(label: String, actual: Either[_, _]): List[String] =
    actual match {
      case Left(_) => Nil
      case Right(value) => List(s"$label: expected a failure, but the operation returned $value")
    }

  /**
   * Checks that an operation succeeded, and compares what it produced.
   *
   * The check runs only on success, so a spec never has to reach into an `Either` to get at the
   * value it wants to compare. A failure is reported once, naming the failure value, and the
   * checks that would have been applied to a value are skipped rather than reported as further
   * discrepancies of their own.
   *
   * @param label  the name of the operation being checked
   * @param actual  the outcome the port produced
   * @param check  the comparisons to apply to the value
   * @return the discrepancies, or nothing when the operation succeeded and everything matched
   */
  def assertRight[A](label: String, actual: Either[_, A])(check: A => List[String]): List[String] =
    actual match {
      case Right(value) => check(value)
      case Left(failure) => List(s"$label: expected a value, but the operation failed with $failure")
    }

  /**
   * Checks that a precondition the fixture records as an error was enforced.
   *
   * A handful of captured errors are not data-dependent failures but violations of a caller
   * contract, which the port enforces the way the implementation being replaced did: by refusing
   * to return. Two families reach the fixtures - a day count asked for a year fraction or a day
   * count over dates supplied out of order, and a holiday calendar operation on a year outside
   * 0000 to 9999 - and both are documented as such in AAP section 0.3.3.
   *
   * The throw is observed inside an effect and turned back into an ordinary value, which is what
   * keeps a precondition row in the report alongside every other kind of row instead of ending the
   * run. It is the only sanctioned way for a spec here to observe one: a bare `try`/`catch` or an
   * `intercept` would abandon the measurement of the rest of the row.
   *
   * @param label  the name of the operation being checked
   * @param thunk  the call that must refuse its arguments
   * @return the discrepancy, or nothing when the call refused them as recorded
   */
  def attemptArgCheck(label: String)(thunk: => Any): IO[List[String]] =
    IO.delay[Any](thunk).attempt.map {
      case Left(_: IllegalArgumentException) => Nil
      case Left(other) =>
        List(
          s"$label: expected the precondition to be refused with an IllegalArgumentException, " +
            s"but the call failed with ${describe(other)}")
      case Right(value) =>
        List(
          s"$label: expected the precondition to be refused with an IllegalArgumentException, " +
            s"but the call returned $value")
    }

  //-------------------------------------------------------------------------
  // The driver.
  //-------------------------------------------------------------------------

  /**
   * Measures a whole fixture and publishes the result.
   *
   * The rows are evaluated one after another, in the order the document lists them, so that a
   * report is reproducible byte for byte from one run to the next: nothing here is concurrent,
   * nothing is scheduled and nothing depends on how the runner happens to interleave work.
   *
   * A row contributes either nothing, when it matched in every respect, or one discrepancy per
   * message its check produced. A check that raises instead of answering contributes exactly one
   * discrepancy naming the error, and the run continues: an exception escaping to the top would
   * take the counts of every row after it with it, and those counts are the reason the report
   * exists. That is also why the report is written here, before this method returns and therefore
   * before [[failIfAny]] can end the test.
   *
   * The counting convention is the one documented on [[ParityReport]]: `passed` counts rows,
   * `failed` counts discrepancies, and their sum exceeds `rows` when a row differs in more than
   * one field.
   *
   * The report is written to `<parity.report.dir>/<fixture>.json`. The five stems this module uses
   * are `daycount`, `schedule`, `fx`, `currency-math` and `holiday` - each the name of the
   * baseline document with `-baseline.json` removed - and `scripts/verify-gates.sh` looks for
   * exactly those names.
   *
   * @param fixture  the fixture stem, which names both the measurement and the report file
   * @param resource  the classpath name of the captured baseline
   * @param check  the comparisons to apply to one row, answering with everything that differed
   * @return the published report; the effect fails only when the fixture cannot be read or
   *         decoded, or when the report cannot be written
   */
  def runFixture[R <: ParityRow: Decoder](fixture: String, resource: String)(
      check: R => IO[List[String]]): IO[ParityReport] =
    for {
      rows <- load[R](resource)
      outcomes <- rows.traverse(row => check(row).attempt.map(outcome => discrepancies(row, outcome)))
      failures = outcomes.flatten
      report = ParityReport(fixture, rows.size, outcomes.count(_.isEmpty), failures.size, failures)
      _ <- writeReport(report)
    } yield report

  /**
   * Fails the calling spec when a report holds any discrepancy.
   *
   * The message quotes the counts and the first twenty discrepancies, which keeps
   * the console output of a systematically broken port readable while the complete detail stays in
   * the report document on disk.
   *
   * @param report  the published report
   * @return nothing, when the fixture was reproduced exactly; otherwise the effect fails with an
   *         [[java.lang.AssertionError]] describing what differed
   */
  def failIfAny(report: ParityReport): IO[Unit] =
    if (report.failed == 0) IO.unit else IO.raiseError(new AssertionError(summarise(report)))

  //-------------------------------------------------------------------------
  // Internals: the outcome of one row, the report directory, and the two renderings.
  //-------------------------------------------------------------------------

  /** Turns the outcome of one row's checks into the discrepancies it contributes. */
  private def discrepancies(row: ParityRow, outcome: Either[Throwable, List[String]]): Vector[ParityFailure] =
    outcome match {
      case Right(messages) => messages.iterator.map(message => ParityFailure(row.id, message)).toVector
      case Left(error) => Vector(ParityFailure(row.id, s"unexpected error: $error"))
    }

  /**
   * The directory the report is written to, which the build must have named.
   *
   * There is deliberately no default. Gate 3 reads the six reports from one absolute path, so a
   * fallback would let a misconfigured run write them somewhere the gate never looks, and a gate
   * that finds no report is indistinguishable from a measurement that was never made. A loud
   * failure is the correct outcome, and the property is read inside the effect so that no spec
   * touches ambient state while it is being constructed.
   */
  private def reportDir: IO[Path] =
    IO.delay(sys.props.get(ReportDirectoryProperty).map(_.trim).filter(_.nonEmpty)).flatMap {
      case Some(configured) => IO.pure(Paths.get(configured))
      case None =>
        IO.raiseError(
          new IllegalStateException(
            s"the system property '$ReportDirectoryProperty' is not set, so there is nowhere to " +
              "write the parity report; build.sbt supplies it to the forked test JVM through " +
              "'Test / javaOptions' under 'Test / fork := true', and this harness has no default " +
              "on purpose, because a report the gate cannot find is indistinguishable from a " +
              "measurement that was never made"))
    }

  /**
   * Writes one report, creating the directory if it is not there yet.
   *
   * The document is the JSON of [[ParityReport]] and carries exactly its five fields, in
   * declaration order, followed by a single newline so that the file is a well-formed text file.
   *
   * @param report  the report to publish
   * @return the path written, so that a caller can name it
   */
  private def writeReport(report: ParityReport): IO[Path] =
    for {
      directory <- reportDir
      stem <- reportStem(report.fixture)
      written <- IO.blocking {
        val created = Files.createDirectories(directory)
        Files.write(
          created.resolve(s"$stem.json"),
          (report.asJson.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
      }
    } yield written

  /**
   * Checks that a fixture stem names one file inside the report directory.
   *
   * A stem becomes part of a path, so a blank one, or one carrying a separator, would put the
   * report somewhere other than where the gate collects it. Both are caller mistakes rather than
   * data-dependent failures, which is why this refuses them outright.
   */
  private def reportStem(fixture: String): IO[String] = {
    val stem = fixture.trim
    if (stem.isEmpty || stem.exists(character => character == '/' || character == '\\')) {
      IO.raiseError(
        new IllegalArgumentException(
          s"a fixture stem names one report file inside the report directory, so it must be a " +
            s"plain name; '$fixture' is not one"))
    } else {
      IO.pure(stem)
    }
  }

  /** Renders a report as the message of the assertion that fails the spec. */
  private def summarise(report: ParityReport): String = {
    val quoted =
      report.failures.take(FailureMessageLimit).map(failure => s"  ${failure.id}: ${failure.message}")
    val omitted = report.failures.size - quoted.size
    val remainder = if (omitted > 0) Vector(s"  ... and $omitted more") else Vector.empty[String]
    val heading =
      s"parity fixture '${report.fixture}' did not reproduce the Java baseline: ${report.failed} " +
        s"discrepancies over ${report.rows} rows, of which ${report.passed} matched in every " +
        s"respect; the full report is '${report.fixture}.json' in the directory named by the " +
        s"'$ReportDirectoryProperty' system property"
    (heading +: (quoted ++ remainder)).mkString("\n")
  }

  /** Renders a thrown error as its type and message, for a report that has to stand alone. */
  private def describe(error: Throwable): String =
    s"${error.getClass.getName}: ${Option(error.getMessage).getOrElse("no message")}"
}
