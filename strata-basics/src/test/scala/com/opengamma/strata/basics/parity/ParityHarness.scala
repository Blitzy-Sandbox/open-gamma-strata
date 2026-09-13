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
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.parse
import io.circe.syntax._

import com.opengamma.strata.collect.io.Resources
import com.opengamma.strata.collect.result.Failure

/**
 * One row of a parity fixture, identified well enough for a discrepancy to be traced back to it.
 *
 * The identity is the name a failure is filed under in the published report: the row's own `id`
 * key where the document carries one, otherwise one derived from the fields that distinguish the
 * row from its neighbours. Identities need not be unique, but they must be stable across runs,
 * because they are written to a file that is compared between runs.
 */
trait ParityRow {

  /** The name this row is reported under. */
  def id: String
}

/** One discrepancy: the row it was found in, and what differed, at full precision. */
final case class ParityFailure(id: String, message: String)

/**
 * The published outcome of measuring one fixture.
 *
 * These five fields, in this order, are the report document `scripts/verify-gates.sh` reads, and
 * nothing is added to them. `rows` counts the fixture rows evaluated, `passed` those that matched
 * in '''every''' respect, `failed` the '''discrepancies''' over all rows rather than the rows that
 * differ - one row can differ in several fields, each worth reporting - and `failures` carries
 * them. So `failed == 0` is equivalent to `passed == rows`, while `passed + failed` equals `rows`
 * only when no row differs twice. `strata-collect` publishes the same five keys under the same
 * convention from `DoubleArrayParitySpec`, which cannot import this file because the build's
 * dependency edge runs from `strata-basics` to `strata-collect`.
 */
final case class ParityReport(
    fixture: String,
    rows: Int,
    passed: Int,
    failed: Int,
    failures: Vector[ParityFailure])

/**
 * The keys a documented object shape of a fixture may carry.
 *
 * A derived decoder reads the fields its model declares and ignores every other key, which for a
 * measurement is the wrong default: an object that gained, lost or renamed a key would decode in
 * silence and the expectation it carries would go unmeasured while the report still read
 * `failed == 0`, so the check happens before the object is decoded, where the keys are still
 * visible. A schema is one or more '''variants''' - a name and the exact keys an object of that
 * variant carries - plus the keys optional in every variant, and a key set satisfies it when some
 * variant's keys are all present and nothing beyond that variant's and the optional ones is. The
 * matching variant's name is returned, so a hand-written decoder dispatches on the same decision
 * that validated the object.
 */
final case class KeySchema(
    shape: String,
    variants: Vector[(String, Set[String])],
    optional: Set[String]) {

  require(variants.nonEmpty, s"a key schema names at least one variant; '$shape' names none")

  /** The same schema, with these keys permitted in addition to every variant's own. */
  def withOptional(keys: Set[String]): KeySchema = copy(optional = optional ++ keys)

  /** The name of the first documented variant the given key set satisfies, if any. */
  def matching(keys: Set[String]): Option[String] =
    variants.collectFirst {
      case (name, expected) if expected.subsetOf(keys) && keys.subsetOf(expected ++ optional) => name
    }

  /** Every key any variant of this schema knows, including the optional ones. */
  def known: Set[String] = variants.foldLeft(optional)((all, variant) => all ++ variant._2)

  /** The schema as one line of a refusal message. */
  def describe: String = {
    val rendered = variants.map { case (name, expected) => s"$name{${render(expected)}}" }
    val extra = if (optional.isEmpty) "" else s", with {${render(optional)}} optional in each"
    s"${rendered.mkString(" | ")}$extra"
  }

  /** Renders a key set in sorted order, so a message is the same from one run to the next. */
  private def render(keys: Set[String]): String = keys.toVector.sorted.mkString(", ")
}

object KeySchema {

  /** A shape whose objects all carry exactly these keys. */
  def uniform(shape: String, keys: Set[String]): KeySchema =
    KeySchema(shape, Vector(shape -> keys), Set.empty)

  /** A shape with several documented key sets, tried in the order given. */
  def variants(shape: String, variants: (String, Set[String])*): KeySchema =
    KeySchema(shape, variants.toVector, Set.empty)
}

/**
 * The shared measurement apparatus of the five parity specs of this module.
 *
 * The five baseline documents committed under `strata-basics/src/test/resources/parity` are what
 * this module is measured against, and they are read-only here: no fixture is edited, no
 * expectation corrected or loosened and no row skipped.
 *
 * ===The tolerance rule===
 *
 * A numeric comparison passes only when '''both''' bounds hold: `|actual - expected| <= 1e-9` and
 * `|actual - expected| <= 1e-9 * max(|actual|, |expected|, 1e-300)`. Neither alone is the rule:
 * the relative bound by itself admits an absolute error of a thousand at values of the order of
 * 1e12, and the absolute bound by itself an error a million times the values themselves at 1e-15.
 * The `1e-300` floor keeps the relative bound meaningful, and its scale non-zero, as the values
 * approach zero. Everything that is not a measured number is compared '''exactly''', through
 * [[assertExact]].
 *
 * ===The report is written before anything is asserted===
 *
 * [[runFixture]] evaluates the whole fixture, writes the report and returns; the verdict belongs
 * to [[failIfAny]], which a spec calls afterwards, so a run that ends in a discrepancy still
 * leaves its counts on disk. A row whose check raises becomes one discrepancy of that row instead
 * of ending the run.
 *
 * The report goes to `<parity.report.dir>/<fixture>.json`; this module's stems are `daycount`,
 * `schedule`, `fx`, `currency-math` and `holiday`, and `scripts/verify-gates.sh` reads exactly
 * those names, so a stem - like the `ParitySpec` suffix by which the specs are selected - is part
 * of the build contract. The directory is required and absolute, supplied by `build.sbt` to the
 * forked test JVM through the `parity.report.dir` system property; [[reportDirectoryFrom]] is that
 * whole decision, and says why it has no fallback.
 *
 * ===A non-finite double is tagged===
 *
 * A fixture writes a non-finite double as one of the tagged strings `"NaN"`, `"Infinity"` and
 * `"-Infinity"`, and every finite one as a full-precision JSON number, so a consumer must
 * `import com.opengamma.strata.collect.json.Codecs.implicits._` before deriving its row decoders;
 * an imported implicit outranks the one circe publishes for `Double`, so every `Double` and
 * `Option[Double]` reads a tagged value with no ambiguity. In practice `fx-baseline.json` carries
 * a `JPY/CAD` rate of `0.0` whose reciprocal is `Infinity`.
 */
object ParityHarness {

  //-------------------------------------------------------------------------
  // Contract constants: the two tolerance bounds, the report-directory property name agreed with
  // `build.sbt`, and the ceilings a fixture document is decoded under.
  //-------------------------------------------------------------------------

  /** The absolute bound of the parity rule. */
  val AbsoluteTolerance: Double = 1e-9

  /** The relative bound of the parity rule. */
  val RelativeTolerance: Double = 1e-9

  /** The floor of the relative bound's scale, which keeps that bound meaningful near zero. */
  private val RelativeFloor: Double = 1e-300

  /**
   * The system property through which the build supplies the report directory, visible across this
   * package so that `DayCountParitySpec`, which exercises this harness, reads the same name.
   */
  private[parity] val ReportDirectoryProperty: String = "parity.report.dir"

  /** The number of discrepancies quoted in a failure message; the report always holds them all. */
  private val FailureMessageLimit: Int = 20

  /**
   * The largest fixture document this harness decodes, in characters, checked before the text is
   * parsed. The largest baseline committed today is roughly 12.7 MiB, so a regenerated fixture
   * keeps headroom while a resource of arbitrary size cannot be materialised.
   */
  private[parity] val MaxFixtureCharacters: Int = 32 * 1024 * 1024

  /**
   * The largest number of rows a fixture may hold, checked on the parsed array before any row
   * model is built. The largest baseline committed today holds 18,356 rows, an order of magnitude
   * below this ceiling.
   */
  private[parity] val MaxFixtureRows: Int = 200000

  //-------------------------------------------------------------------------
  // JSON encoding of the report, derived at compile time in declaration order, which is what fixes
  // the five keys of the document and their order.
  //-------------------------------------------------------------------------

  implicit val parityFailureEncoder: Encoder[ParityFailure] = deriveEncoder[ParityFailure]

  implicit val parityReportEncoder: Encoder[ParityReport] = deriveEncoder[ParityReport]

  //-------------------------------------------------------------------------
  // The two lifts from the error channel into a failed effect, for building an input a row must
  // have: an input that cannot be built is a defect rather than a parity result, so it ends the
  // row. They are never used to observe an expected failure - an `error` row is an expectation in
  // its own right, checked with `assertLeft` or `attemptArgCheck` - and they are the only place in
  // this test tree where a `Failure` becomes a thrown error. `private[parity]`, because the five
  // specs of this package need them and nothing outside should.
  //-------------------------------------------------------------------------

  /** Lifts a single-cause failure into a failed effect carrying the failure's message. */
  private[parity] def raise[A](result: Either[Failure, A]): IO[A] =
    IO.fromEither(result.leftMap(failure => new IllegalStateException(failure.message)))

  /** Lifts an accumulated failure into a failed effect, joining every cause into one message. */
  private[parity] def raiseNec[A](result: Either[NonEmptyChain[Failure], A]): IO[A] =
    IO.fromEither(
      result.leftMap(failures =>
        new IllegalStateException(failures.toChain.toList.map(_.message).mkString("; "))))

  //-------------------------------------------------------------------------
  // The strict schema layer: every object of a fixture - a row, and every object nested in one -
  // is checked against its documented `KeySchema` before it is decoded. These members are public
  // because not every fixture consumer is in this package: `ReferenceDataManifestSpec` decodes the
  // committed manifest through the same two wrappers.
  //-------------------------------------------------------------------------

  /**
   * Checks one JSON object against its documented key set. The refusal names either what the
   * document carries instead of an object, or the keys no variant knows, what each variant wanted
   * and did not get, and the schema itself.
   */
  def strictKeys(schema: KeySchema, cursor: HCursor): Decoder.Result[String] =
    cursor.keys match {
      case None =>
        Left(
          DecodingFailure(
            s"expected a JSON object for ${schema.shape}, whose documented shape is " +
              s"${schema.describe}; the document carries ${cursor.value.name} here",
            cursor.history))
      case Some(keys) =>
        val present = keys.toSet
        schema.matching(present) match {
          case Some(variant) => Right(variant)
          case None =>
            val unknown = (present -- schema.known).toVector.sorted
            val missing = schema.variants
              .map { case (name, expected) => s"$name is missing {${(expected -- present).toVector.sorted.mkString(", ")}}" }
              .mkString("; ")
            Left(
              DecodingFailure(
                s"${schema.shape} carries the keys {${present.toVector.sorted.mkString(", ")}}, " +
                  s"which satisfy no documented variant of ${schema.describe}: " +
                  s"unknown keys {${unknown.mkString(", ")}}, and $missing. A captured object " +
                  "whose keys are not the documented ones is a fixture that has stopped agreeing " +
                  "with this spec: decoding it anyway would leave whatever the new key carries " +
                  "unmeasured while the report still read zero failures",
                cursor.history))
        }
    }

  /** Wraps a decoder so that the object is checked against its documented key set first. */
  def strictObject[A](schema: KeySchema)(decoder: Decoder[A]): Decoder[A] =
    Decoder.instance(cursor => strictKeys(schema, cursor).flatMap(_ => decoder(cursor)))

  /**
   * Wraps a family of decoders, choosing between them by the variant the object satisfies. The
   * variant is decided once, by the check that validated the keys, so a consumer cannot dispatch
   * on one reading of the object while having validated another.
   */
  def strictVariant[A](schema: KeySchema)(decoder: String => Decoder[A]): Decoder[A] =
    Decoder.instance(cursor => strictKeys(schema, cursor).flatMap(variant => decoder(variant)(cursor)))

  //-------------------------------------------------------------------------
  // Loading a fixture.
  //-------------------------------------------------------------------------

  /**
   * Reads and decodes a baseline document, a top-level JSON array of uniform row objects.
   *
   * Four things are settled here rather than left to the caller, each because the alternative is a
   * measurement that looks like a pass: the decode is bounded at [[MaxFixtureCharacters]] before
   * the text is parsed, and it is bounded in time and cancelable while it waits, so a fixture whose
   * source stopped yielding fails with a diagnostic rather than pinning the fiber that reads it or
   * the handle it opened; the row count is bounded at [[MaxFixtureRows]], on the parsed array
   * before a row model is built; a document that is not an array is refused as such rather than as
   * a decode failure of a row; and an empty fixture is refused, because an empty array decodes into
   * no rows and would publish `rows = 0`, `passed = 0`, `failed = 0`, which [[failIfAny]] accepts.
   * This form declares '''no''' row schema: it exists for a row model that reads a subset of the
   * document - `DayCountParitySpec` uses one, a two-field probe run against every committed
   * baseline - and a measuring spec uses [[loadStrict]] instead.
   */
  def load[A: Decoder](resource: String): IO[Vector[A]] =
    Resources
      .readClasspathText(resource)
      .flatMap(text => decodeRowsIn[A](resource, text, MaxFixtureCharacters, MaxFixtureRows, None))

  /**
   * Reads and decodes a baseline whose row keys are declared, which is what a measuring spec uses.
   * Every row is checked against `schema` before it is decoded, and a refusal names the offending
   * row by its index. The schema of an object nested in a row is declared where that object's
   * decoder is built, with [[strictObject]] or [[strictVariant]], the only place its keys are seen.
   */
  def loadStrict[A: Decoder](resource: String, schema: KeySchema): IO[Vector[A]] =
    Resources
      .readClasspathText(resource)
      .flatMap(text =>
        decodeRowsIn[A](resource, text, MaxFixtureCharacters, MaxFixtureRows, Some(schema)))

  /**
   * Decodes one fixture document under explicit bounds, with no row schema declared. The bounds are
   * parameters rather than this object's constants so that the decision can be exercised at a size
   * a test can afford, on exactly the code the fixtures go through.
   */
  private[parity] def decodeRows[A: Decoder](
      resource: String,
      text: String,
      maxCharacters: Int,
      maxRows: Int): IO[Vector[A]] =
    decodeRowsIn[A](resource, text, maxCharacters, maxRows, None)

  /**
   * Decodes one fixture document under explicit bounds, against an optional row schema. Each bound
   * is applied before the work it bounds, and the schema by wrapping the row decoder, so a row's
   * keys are checked immediately before that row is decoded and the refusal carries the cursor
   * path, which names the row's index.
   */
  private[parity] def decodeRowsIn[A: Decoder](
      resource: String,
      text: String,
      maxCharacters: Int,
      maxRows: Int,
      schema: Option[KeySchema]): IO[Vector[A]] =
    for {
      _ <- refuse(
        text.length > maxCharacters,
        s"the parity fixture '$resource' is ${text.length} characters, which is beyond the " +
          s"$maxCharacters this harness decodes; the limit exists so that the size of a fixture " +
          "cannot determine the memory of the test process")
      json <- IO.fromEither(parse(text))
      rows <- IO.fromOption(json.asArray)(
        new IllegalStateException(
          s"the parity fixture '$resource' is not a top-level JSON array of rows, which is the " +
            "shape every captured baseline has"))
      _ <- refuse(
        rows.isEmpty,
        s"the parity fixture '$resource' holds no rows; an empty fixture would publish a report " +
          "of zero rows, zero passed and zero failed, which the failure gate accepts, so every " +
          "comparison the baseline exists to make would be retired while Gate 3 still reported " +
          "a pass")
      _ <- refuse(
        rows.size > maxRows,
        s"the parity fixture '$resource' holds ${rows.size} rows, which is beyond the $maxRows " +
          "this harness measures")
      rowDecoder = schema.fold(Decoder[A])(declared => strictObject[A](declared)(Decoder[A]))
      decoded <- IO.fromEither(Decoder.decodeVector(rowDecoder).decodeJson(json))
    } yield decoded

  /** Fails the effect with the given explanation when the condition holds. */
  private def refuse(condition: Boolean, message: String): IO[Unit] =
    if (condition) IO.raiseError(new IllegalStateException(message)) else IO.unit

  //-------------------------------------------------------------------------
  // The comparators. Each answers with the discrepancies it found, so an empty list means parity.
  // A row is usually several comparisons at once, and a caller concatenates their results, so
  // every field that differs is named instead of only the first.
  //-------------------------------------------------------------------------

  /**
   * Compares a number against the number the fixture records, under a `label` naming the field.
   *
   * The order of the three decisions matters. `java.lang.Double.compare(actual, expected) == 0`
   * comes first: it is not raw-bit identity, because payloads are canonicalised, so any
   * not-a-number compares equal to any other and `-0.0` is ordered below `+0.0`. It has to come
   * first because `|Infinity - Infinity|` is not a number and every comparison against a
   * not-a-number is false, so an `"Infinity"` expectation would otherwise be reported as a
   * discrepancy; `fx-baseline.json` carries exactly that case. Second, a pair not called equal
   * there and involving a value outside the finite range differs, and is reported without
   * computing a difference. Third, a finite pair is at parity when the absolute difference is
   * within `1e-9` '''and''' within `1e-9` of the scale of the larger operand, floored at `1e-300`.
   *
   * Positive and negative zero are ordered apart by the first decision, so they reach the third,
   * where a difference of zero is within both bounds and they are at parity: signed zero is a
   * representation, not a quantity, and the fixtures carry `-0.0`.
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
   * Compares a number that either side may be without. Absence is itself an expectation - a stub a
   * schedule does not have, a rate a matrix cannot produce - so two absences are at parity, two
   * present values are compared by [[assertParity]], and one of each differs.
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
   * Compares a sequence of numbers element by element, each under its own indexed label, so a
   * report names the position that differs. A length difference is reported once and stops there,
   * because a shifted sequence would otherwise report a discrepancy at every index.
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
   * Compares two values for equality: the only comparator permitted for a date, a list of dates, a
   * string, an integer, a boolean, a convention or index name, and the `Decimal`-shaped amount of a
   * `Money` or a `BigMoney`. Those amounts are recorded in decimal string form so that they are
   * compared exactly rather than through a tolerance that would hide a rounding defect: rounding to
   * a currency's minor units is discrete, so a value out by one unit in the last place is wrong.
   */
  def assertExact[A](label: String, actual: A, expected: A): List[String] =
    if (actual == expected) Nil else List(s"$label: actual $actual, expected $expected")

  /**
   * Checks that an operation the fixture records as an error did fail, which is how a spec checks a
   * refusal reported in the error channel: no `getOrElse`, no `.value` and no partial function near
   * an `Either`. The failure value is not compared against any recorded wording, which would make
   * the fixture a test of prose.
   */
  def assertLeft(label: String, actual: Either[_, _]): List[String] =
    actual match {
      case Left(_) => Nil
      case Right(value) => List(s"$label: expected a failure, but the operation returned $value")
    }

  /**
   * Checks that an operation succeeded, and compares what it produced. The check runs only on
   * success, so a spec never has to reach into an `Either`, and a failure is reported once, naming
   * the failure value, instead of as further discrepancies of the checks that were skipped.
   */
  def assertRight[A](label: String, actual: Either[_, A])(check: A => List[String]): List[String] =
    actual match {
      case Right(value) => check(value)
      case Left(failure) => List(s"$label: expected a value, but the operation failed with $failure")
    }

  /**
   * Checks that a precondition the fixture records as an error was enforced: a day count over dates
   * out of order, or without the schedule information it needs, and a holiday calendar operation on
   * a year outside 0000 to 9999 are caller-contract violations rather than data-dependent failures,
   * enforced by refusing to return. The throw is observed inside an effect and turned back into an
   * ordinary value, which keeps the row in the report instead of ending the run; this is the only
   * sanctioned way for a spec here to observe one, and only the type of the refusal is asserted,
   * never its text.
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
   * Measures a whole fixture and publishes the result, loading the rows through [[loadStrict]], so
   * the row schema is not optional here. Rows are evaluated in document order, so a report is
   * reproducible from one run to the next, and the counts are checked against
   * [[contractViolations]] before they are written.
   */
  def runFixture[R <: ParityRow: Decoder](fixture: String, resource: String, schema: KeySchema)(
      check: R => IO[List[String]]): IO[ParityReport] =
    runFixtureWith[R, Unit](fixture, resource, schema)(_ => IO.unit)((_, row) => check(row))

  /**
   * Measures a whole fixture whose rows are checked against something built from the document as a
   * whole. `setup` runs '''inside''' the measurement, under `attempt`: building it in the spec
   * instead would mean an unbuildable input raised before any report was written, leaving a
   * failing run with no counts at all. When it fails, the failure becomes one attributed
   * discrepancy in a report of `passed = 0`, written like any other, and the verdict is left to
   * [[failIfAny]].
   */
  def runFixtureWith[R <: ParityRow: Decoder, S](
      fixture: String,
      resource: String,
      schema: KeySchema)(setup: Vector[R] => IO[S])(check: (S, R) => IO[List[String]]): IO[ParityReport] =
    reportDir.flatMap(directory =>
      runFixtureWithIn[R, S](directory, fixture, resource, Some(schema))(setup)(check))

  /**
   * Measures a whole fixture into a named directory: [[runFixture]] with the one piece of ambient
   * configuration supplied as an argument. This form exists so that `DayCountParitySpec` can
   * observe a published report, including that of a failing run, without writing into the
   * directory the reports are collected from.
   */
  private[parity] def runFixtureIn[R <: ParityRow: Decoder](
      directory: Path,
      fixture: String,
      resource: String)(check: R => IO[List[String]]): IO[ParityReport] =
    runFixtureWithIn[R, Unit](directory, fixture, resource, None)(_ => IO.unit)((_, row) => check(row))

  /**
   * The one implementation of the measurement, of which every other driver form is a special case.
   * The order of the steps is the contract: the rows are decoded, the shared context is built under
   * `attempt` so that its failure is report content rather than an escape, every row is measured in
   * document order, and the report is written before this method returns.
   */
  private[parity] def runFixtureWithIn[R <: ParityRow: Decoder, S](
      directory: Path,
      fixture: String,
      resource: String,
      schema: Option[KeySchema])(setup: Vector[R] => IO[S])(
      check: (S, R) => IO[List[String]]): IO[ParityReport] =
    for {
      rows <- schema.fold(load[R](resource))(declared => loadStrict[R](resource, declared))
      prepared <- setup(rows).attempt
      report <- prepared match {
        case Right(context) =>
          rows
            .traverse(row =>
              check(context, row).attempt.map(outcome => discrepancies(row, outcome)))
            .map(outcomes =>
              ParityReport(
                fixture,
                rows.size,
                outcomes.count(_.isEmpty),
                outcomes.map(_.size).sum,
                outcomes.flatten))
        case Left(error) => IO.pure(setupFailureReport(fixture, rows.size, error))
      }
      _ <- publish(directory, report)
    } yield report

  /**
   * The report of a run whose shared context could not be built: no row was measured, so `passed`
   * is zero and the one discrepancy is attributed to the setup, under the fixture stem with
   * `:setup`, an identity no row carries.
   */
  private def setupFailureReport(fixture: String, rows: Int, error: Throwable): ParityReport = {
    val failure =
      ParityFailure(
        s"$fixture:setup",
        s"the shared context this fixture is measured against could not be built, so none of " +
          s"its $rows rows was measured: ${describe(error)}")
    ParityReport(fixture, rows, 0, 1, Vector(failure))
  }

  /**
   * Writes a report, once its counts have been checked against the shared convention. Counts that
   * contradict it are a defect in this harness, so the effect fails naming the violation rather
   * than publishing a number that means something other than what it says.
   */
  private def publish(directory: Path, report: ParityReport): IO[Unit] =
    contractViolations(report) match {
      case Nil => writeReportTo(directory, report).void
      case violations =>
        IO.raiseError(
          new IllegalStateException(
            s"the parity report for '${report.fixture}' does not satisfy the counting convention " +
              s"both modules publish under: ${violations.mkString("; ")}"))
    }

  /**
   * The four ways a report can contradict the counting convention documented on [[ParityReport]],
   * as a list rather than an assertion so that a violation names itself:
   *
   *  - '''the counts are in range''' - `passed` lies between zero and `rows`, and no count of
   *    the three is negative;
   *  - '''a clean report measured something, and all of it''' - `failed == 0` requires at least
   *    one row and `passed` to be all of them, so an abandoned run cannot publish it;
   *  - '''a discrepancy is described''' - a positive `failed` lists at least one and never more
   *    than it counts;
   *  - '''a discrepancy belongs to a row that did not pass''' - `passed` is short of `rows`.
   */
  private[parity] def contractViolations(report: ParityReport): List[String] = {
    val inRange =
      if (report.rows >= 0 && report.passed >= 0 && report.passed <= report.rows &&
        report.failed >= 0) {
        Nil
      } else {
        List(
          s"the counts are out of range: ${report.passed} passed and ${report.failed} failed " +
            s"over ${report.rows} rows")
      }
    val cleanRunMeasuredEverything =
      if (report.failed != 0 || (report.rows >= 1 && report.passed == report.rows)) Nil
      else
        List(
          s"the report claims no discrepancy while ${report.passed} of ${report.rows} rows " +
            "passed; a clean report has at least one row and every one of them passing, because " +
            "'failed == 0' is the condition the gate reads as parity")
    val discrepanciesDescribed =
      if (report.failed == 0 || (report.failures.nonEmpty && report.failures.size <= report.failed)) {
        Nil
      } else {
        List(
          s"the report counts ${report.failed} discrepancies and lists ${report.failures.size}; " +
            "a report lists at least one of the discrepancies it counts and never more than it " +
            "counts")
      }
    val attributedToAFailingRow =
      if (report.failed == 0 || report.rows == 0 || report.passed < report.rows) Nil
      else
        List(
          s"the report counts ${report.failed} discrepancies while all ${report.rows} rows " +
            "passed; a discrepancy belongs to a row that did not pass")
    inRange ::: cleanRunMeasuredEverything ::: discrepanciesDescribed ::: attributedToAFailingRow
  }

  /**
   * Fails the calling spec when a report holds any discrepancy. The message quotes the counts and
   * the first twenty discrepancies, which keeps the console output of a systematically broken run
   * readable while the complete detail stays in the report document on disk.
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
   * The directory the report is written to, read inside the effect so that no spec touches ambient
   * state while it is being constructed; the decision it feeds is [[reportDirectoryFrom]].
   */
  private def reportDir: IO[Path] =
    IO.delay(sys.props.get(ReportDirectoryProperty))
      .flatMap(configured =>
        IO.fromEither(reportDirectoryFrom(configured).leftMap(new IllegalStateException(_))))

  /**
   * Decides where reports go, from the configured text alone, and refuses two values with no
   * fallback. Nothing configured: a default would let a misconfigured run write where nothing
   * collects the reports, and a report that is not there is indistinguishable from a measurement
   * never made. A relative path: tests are forked, so it would resolve against the working
   * directory of whichever forked JVM ran the spec rather than the one directory the reports are
   * collected from.
   */
  private[parity] def reportDirectoryFrom(configured: Option[String]): Either[String, Path] =
    configured.map(_.trim).filter(_.nonEmpty) match {
      case None =>
        Left(
          s"the system property '$ReportDirectoryProperty' is not set, so there is nowhere to " +
            "write the parity report; build.sbt supplies it to the forked test JVM through " +
            "'Test / javaOptions' under 'Test / fork := true', and this harness has no default " +
            "on purpose, because a report the gate cannot find is indistinguishable from a " +
            "measurement that was never made")
      case Some(value) =>
        Either
          .catchNonFatal(Paths.get(value))
          .leftMap(error =>
            s"the system property '$ReportDirectoryProperty' is '$value', which is not a usable " +
              s"path: ${error.getMessage}")
          .flatMap { path =>
            if (path.isAbsolute) {
              Right(path.normalize)
            } else {
              Left(
                s"the system property '$ReportDirectoryProperty' is '$value', which is relative; " +
                  "it must be absolute, because the test JVM is forked and a relative path would " +
                  "resolve against its working directory rather than the one directory the gate " +
                  "reads the reports from")
            }
          }
    }

  /**
   * Writes one report into a named directory, creating the directory if it is not there yet. The
   * document is the JSON of [[ParityReport]] and carries exactly its five fields, in declaration
   * order, followed by a single newline so that the file is a well-formed text file.
   */
  private[parity] def writeReportTo(directory: Path, report: ParityReport): IO[Path] =
    for {
      stem <- reportStem(report.fixture)
      written <- IO.blocking {
        val created = Files.createDirectories(directory)
        Files.write(
          created.resolve(s"$stem.json"),
          (report.asJson.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
      }
    } yield written

  /**
   * Checks that a fixture stem names one file inside the report directory: a stem becomes part of a
   * path, so a blank one, or one carrying a separator, would put the report somewhere other than
   * where the reports are collected. Both are caller mistakes, refused outright.
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
