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
 * ===The counting convention is one convention, across both modules===
 *
 * All '''six''' parity reports mean the same thing by the same key, and that is a contract rather
 * than a coincidence. Gate 3 reads `daycount`, `schedule`, `fx`, `currency-math` and `holiday`
 * from this module and `double-array` from `strata-collect`, prints their `rows` and `passed`
 * side by side in one table, and a reader comparing two lines of that table has to be comparing
 * the same quantity. The convention is:
 *
 *  - `rows` is the number of fixture rows evaluated.
 *  - `passed` is the number of those rows that matched the Java baseline in '''every''' respect.
 *  - `failed` is the number of '''discrepancies''' found over all rows, not the number of rows
 *    that differ: one row can differ in several fields at once and each of them is worth
 *    reporting.
 *  - `failures` carries those discrepancies. This module publishes all of them; a module that
 *    caps the list says so in the list itself and still reports the true total in `failed`.
 *
 * `passed + failed` therefore equals `rows` only when no row differs in more than one way, while
 * `failed == 0` is always equivalent to `passed == rows` - which is the condition the gate
 * asserts. `strata-collect` cannot import this file, because the build's dependency edge runs
 * from `strata-basics` to `strata-collect` and never the other way, so its `DoubleArrayParitySpec`
 * restates this convention and asserts it of the report it publishes; the invariants that hold it
 * to the letter of it are [[ParityHarness.contractViolations]], which both modules check.
 *
 * The number of individual '''checks''' a run executed is deliberately not a field here. It is
 * not comparable between fixtures - a day-count row is one number, a double-array row is
 * ninety-odd - and the five keys are fixed by the gate. Where the count of executed checks is the
 * evidence that a fixture was measured rather than skipped, the spec that owns the fixture
 * asserts it directly against the shapes it decoded.
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
 * The keys a documented object shape of a captured fixture may carry.
 *
 * ===Why a fixture consumer needs this===
 *
 * A derived JSON decoder reads the fields its model declares and ignores every other key of the
 * object it is given. That is the right default for a wire format that evolves, and precisely the
 * wrong one for a measurement: a key the capture starts emitting - a new expectation, a new
 * operand, a renamed field - would be dropped in silence, the rows would decode perfectly, the
 * report would show `failed == 0`, and Gate 3 would publish a pass over a fixture that was no
 * longer being measured in full. Declaring the key set closes that gap in the only place it can
 * be closed: before the object is decoded, where the keys are still visible.
 *
 * ===What a schema is===
 *
 * One or more '''variants''', each a name and the exact set of keys an object of that variant
 * carries, plus the keys that are optional in every variant. A key set satisfies the schema when
 * some variant's keys are all present and nothing outside that variant's keys and the optional
 * ones is. The matching variant's name is returned, so a hand-written decoder can dispatch on the
 * same decision that validated the object instead of making it a second time.
 *
 * The captured baselines are uniform documents - every row of a fixture carries the same keys,
 * present and possibly null, rather than omitting the ones it has nothing to say about - so most
 * schemas here are a single variant with no optional keys, which is exact equality of key sets.
 * Variants exist for the documents that genuinely have more than one row shape, and `optional`
 * for the nested objects that genuinely omit a key.
 *
 * @param shape  what is being decoded, for the message of a refusal
 * @param variants  the documented key sets, tried in order, of which there is at least one
 * @param optional  keys permitted in addition to every variant's own
 */
final case class KeySchema(
    shape: String,
    variants: Vector[(String, Set[String])],
    optional: Set[String]) {

  require(variants.nonEmpty, s"a key schema names at least one variant; '$shape' names none")

  /** The same schema, with these keys permitted in addition to every variant's own. */
  def withOptional(keys: Set[String]): KeySchema = copy(optional = optional ++ keys)

  /**
   * The name of the first documented variant the given key set satisfies.
   *
   * @param keys  the keys the object actually carries
   * @return the variant's name, or nothing when no variant is satisfied
   */
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
 * ===The report directory is required, and absolute===
 *
 * The directory comes from the `parity.report.dir` system property, which `build.sbt` supplies to
 * the forked test JVM as an absolute path anchored at the build root. There is no fallback: a
 * default would let a misconfigured run write its reports where the gate does not look and so
 * report a pass that nothing measured. A missing property fails loudly instead, and so does a
 * relative one - tests are forked, so a relative path resolves against whatever working directory
 * the forked JVM was given, which is not the one directory Gate 3 collects. The whole decision is
 * [[reportDirectoryFrom]], which is a function of the configured text alone and is tested as one.
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
 *
 * ===Decoding is strict about keys===
 *
 * A derived decoder reads the fields its model declares and ignores every other key of the object
 * it is given, which for a measurement is the wrong default: a key the capture starts emitting
 * would be dropped in silence, every row would decode, the report would read `failed == 0`, and
 * Gate 3 would publish a pass over a fixture no longer measured in full. So every object of a
 * captured document is checked against a declared [[KeySchema]] before it is decoded. A consumer
 * declares the '''row''' schema where it loads - [[loadStrict]], or the [[runFixture]] form that
 * takes a schema - and wraps the decoder of every object '''nested''' in a row with
 * [[strictObject]] or [[strictVariant]], which is the only place a nested object's keys are
 * visible. `ReferenceDataManifestSpec`, which reads the captured manifest rather than a row array,
 * uses the same two wrappers.
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

  /**
   * The system property through which the build supplies the report directory.
   *
   * Visible across this package so that the harness's own contract tests name the same property
   * the harness reads, rather than a copy of its name that could drift from it.
   */
  private[parity] val ReportDirectoryProperty: String = "parity.report.dir"

  /** The number of discrepancies quoted in a failure message; the report always holds them all. */
  private val FailureMessageLimit: Int = 20

  /**
   * The largest fixture document this harness decodes, in characters.
   *
   * The ceiling bounds what a fixture can cost this process, and it is checked before the document
   * is parsed rather than after. The largest baseline committed today is `daycount-baseline.json`
   * at roughly 12.7 MiB, so the ceiling leaves it a factor of two and a half of headroom: a
   * regenerated fixture is not going to be refused, while a resource of arbitrary size still
   * cannot be materialised. [[com.opengamma.strata.collect.io.Resources.MaxBytes]] is the bound on
   * the read that precedes this one; this is the bound on the decode.
   */
  private[parity] val MaxFixtureCharacters: Int = 32 * 1024 * 1024

  /**
   * The largest number of rows a fixture may hold.
   *
   * Checked on the parsed array before any row model is built, so an oversized document costs the
   * JSON tree and not a vector of domain values as well. The largest baseline committed today
   * holds 18,356 rows, so the ceiling is an order of magnitude above the data it guards.
   */
  private[parity] val MaxFixtureRows: Int = 200000

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
  // The strict schema layer.
  //
  // Every object of a captured document - a row, and every object nested inside one - is checked
  // against its documented key set before it is decoded. A derived decoder reads the fields its
  // model declares and ignores the rest, so without this layer a key the capture started emitting
  // would be dropped in silence and the expectation it carried would go unmeasured while the
  // report still read `failed == 0`. The check is a function of the keys alone, so it costs one
  // set comparison per object and is applied in the one place where the keys are still visible.
  //
  // These members are public because the fixture consumers of this module are not all in this
  // package: `ReferenceDataManifestSpec` sits beside it in `com.opengamma.strata.basics` and
  // decodes the captured manifest through the same helper, which is the point - one strictness
  // rule, one message, one place to change it. `strata-collect` cannot reach them at all, because
  // the build's dependency edge runs from `strata-basics` to `strata-collect`; its own parity spec
  // restates the rule, as it already restates the tolerance rule and the report writer.
  //-------------------------------------------------------------------------

  /**
   * Checks one JSON object against its documented key set.
   *
   * Two refusals are possible and they are different facts. A value that is not an object at all
   * is reported as such, naming what the document has instead: a nested object that has become a
   * string is a fixture that no longer agrees with the spec, not a field that failed to decode. A
   * key set that matches no documented variant is reported with the keys the object carries that
   * no variant knows, the keys each variant wanted and did not get, and the schema itself, so the
   * message says what to do about it without opening the document beside it.
   *
   * @param schema  the documented shape
   * @param cursor  the object being decoded
   * @return the name of the variant the object satisfies, or the refusal
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

  /**
   * Wraps a decoder so that the object is checked against its documented key set first.
   *
   * This is the form every derived decoder of a fixture consumer takes:
   *
   * {{{
   * implicit val rowDecoder: Decoder[Row] =
   *   ParityHarness.strictObject(RowSchema)(deriveDecoder[Row])
   * }}}
   *
   * @param schema  the documented shape
   * @param decoder  the decoder to apply once the keys are known to be the documented ones
   * @return the strict decoder
   */
  def strictObject[A](schema: KeySchema)(decoder: Decoder[A]): Decoder[A] =
    Decoder.instance(cursor => strictKeys(schema, cursor).flatMap(_ => decoder(cursor)))

  /**
   * Wraps a family of decoders, choosing between them by the variant the object satisfies.
   *
   * The variant is decided once, by the check that validated the keys, and handed to the chooser;
   * a consumer therefore cannot dispatch on one reading of the object while having validated
   * another, which is the failure mode a separate `if (keys contains …)` would reintroduce.
   *
   * @param schema  the documented shape, whose variants the chooser must cover
   * @param decoder  the decoder to use for a given variant name
   * @return the strict decoder
   */
  def strictVariant[A](schema: KeySchema)(decoder: String => Decoder[A]): Decoder[A] =
    Decoder.instance(cursor => strictKeys(schema, cursor).flatMap(variant => decoder(variant)(cursor)))

  //-------------------------------------------------------------------------
  // Loading a fixture.
  //-------------------------------------------------------------------------

  /**
   * Reads and decodes a captured baseline.
   *
   * Every fixture is a top-level JSON array of uniform row objects. A document that does not match
   * the row model fails the effect, carrying the cursor path circe reports: that a fixture and a
   * spec no longer agree on what is being measured is a different fact from the port disagreeing
   * with Java, and absorbing the first into the second would make the gate's verdict worthless.
   *
   * Four things about the document are settled here rather than left to the caller, each because
   * the alternative is a measurement that looks like a pass:
   *
   *  - '''The read is bounded.''' `Resources` refuses a resource beyond its own documented byte
   *    ceiling, and this decode refuses a document beyond [[MaxFixtureCharacters]].
   *  - '''The row count is bounded''', at [[MaxFixtureRows]], and is checked on the parsed array
   *    before a single row model is built.
   *  - '''The document is an array.''' A fixture that has become an object, or a number, is
   *    rejected as such rather than as a decode failure of a row.
   *  - '''An empty fixture is refused.''' This is the one that matters most. An empty array
   *    decodes perfectly well into no rows, and [[runFixture]] would then publish a report of
   *    `rows = 0`, `passed = 0`, `failed = 0`, which [[failIfAny]] accepts - so replacing any
   *    baseline with `[]` would retire every comparison in it while Gate 3 still reported green.
   *    A fixture with no rows measures nothing, and measuring nothing is not a pass.
   *
   * The two ceilings bound the document as a whole, and therefore everything inside it: no string
   * and no depth of nesting can exceed the length of the text that carries them. What they do not
   * decide is whether a row is '''shaped''' as its measurement needs - that two operand lists are
   * the same length, that a matrix is rectangular, that a row carries the expectations the spec
   * evaluates. Those are invariants of the row model rather than of the document, so each spec
   * asserts its own before it evaluates anything, and a row that fails one is reported as a
   * fixture that no longer agrees with the spec rather than as a discrepancy of the port.
   *
   * This form declares '''no''' row schema, so the rows are decoded by the model's own decoder
   * alone. It exists for a row model that deliberately reads a subset of the document - the
   * harness's own contract tests use one, a two-field probe that is run against every committed
   * baseline - and a measuring spec must not use it: [[loadStrict]] is the form that states the
   * keys the document is expected to carry, and every fixture consumer of this package and of
   * `com.opengamma.strata.basics` goes through that one.
   *
   * @param resource  the classpath name of the fixture, relative to the test resource root and
   *                  without a leading separator, for example `parity/daycount-baseline.json`
   * @return the decoded rows in fixture order, of which there is at least one; the effect fails
   *         when the resource is absent, oversized, empty, not an array, too long or does not
   *         decode
   */
  def load[A: Decoder](resource: String): IO[Vector[A]] =
    Resources
      .readClasspathText(resource)
      .flatMap(text => decodeRowsIn[A](resource, text, MaxFixtureCharacters, MaxFixtureRows, None))

  /**
   * Reads and decodes a captured baseline whose row keys are declared.
   *
   * This is [[load]] with the document's own shape stated, and it is what a measuring spec uses.
   * Every row is checked against `schema` '''before''' it is decoded, so a row that has gained a
   * key, lost one, or changed shape is refused with the keys named rather than decoded into a
   * model that has no field for it - which is how a newly captured expectation would otherwise go
   * unmeasured while the report still read zero failures. The check names the offending row by
   * its index in the document.
   *
   * A consumer declares the schema of the '''row''' here and the schema of every object
   * '''nested''' in a row with [[strictObject]] or [[strictVariant]] where that object's decoder
   * is built, because only the decoder of a nested object ever sees its keys.
   *
   * @param resource  the classpath name of the fixture
   * @param schema  the documented key sets of one row of this fixture
   * @return the decoded rows in fixture order, of which there is at least one
   */
  def loadStrict[A: Decoder](resource: String, schema: KeySchema): IO[Vector[A]] =
    Resources
      .readClasspathText(resource)
      .flatMap(text =>
        decodeRowsIn[A](resource, text, MaxFixtureCharacters, MaxFixtureRows, Some(schema)))

  /**
   * Decodes one fixture document under explicit bounds, with no row schema declared.
   *
   * The order of the four steps is the point of the method: each bound is applied before the work
   * it bounds. The length of the text is known without parsing it, the length of the array is
   * known without decoding its elements, and only a document that has passed both is turned into
   * row models. The bounds are parameters rather than constants read from scope so that this
   * decision can be tested at a size a test can afford, on exactly the code the fixtures go
   * through; [[load]] and [[loadStrict]] supply the real ceilings.
   *
   * @param resource  the name of the fixture, for the failure messages
   * @param text  the document
   * @param maxCharacters  the longest document accepted
   * @param maxRows  the largest number of rows accepted
   * @return the decoded rows in document order, of which there is at least one
   */
  private[parity] def decodeRows[A: Decoder](
      resource: String,
      text: String,
      maxCharacters: Int,
      maxRows: Int): IO[Vector[A]] =
    decodeRowsIn[A](resource, text, maxCharacters, maxRows, None)

  /**
   * Decodes one fixture document under explicit bounds, against an optional row schema.
   *
   * The schema, where one is declared, is applied by wrapping the row decoder rather than by a
   * separate pass over the document: the keys of a row are then checked immediately before that
   * row is decoded, and a refusal carries the cursor path, which names the row's index. That is
   * the same strictness layer a nested object gets, applied at the outermost level.
   *
   * @param resource  the name of the fixture, for the failure messages
   * @param text  the document
   * @param maxCharacters  the longest document accepted
   * @param maxRows  the largest number of rows accepted
   * @param schema  the documented key sets of one row, where the caller declares them
   * @return the decoded rows in document order, of which there is at least one
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
   * The counting convention is the one documented on [[ParityReport]] and shared with the
   * `double-array` report of `strata-collect`: `passed` counts rows that matched in every
   * respect, `failed` counts discrepancies, and their sum exceeds `rows` when a row differs in
   * more than one field. The counts are checked against [[contractViolations]] before they are
   * written.
   *
   * The rows are loaded through [[loadStrict]], so the schema is not optional here: a row that
   * has gained a key, lost one or changed shape is refused by name instead of being decoded into
   * a model that has no field for it, which is how a newly captured expectation would otherwise
   * go unmeasured while this report still read zero failures.
   *
   * The report is written to `<parity.report.dir>/<fixture>.json`. The five stems this module uses
   * are `daycount`, `schedule`, `fx`, `currency-math` and `holiday` - each the name of the
   * baseline document with `-baseline.json` removed - and `scripts/verify-gates.sh` looks for
   * exactly those names.
   *
   * @param fixture  the fixture stem, which names both the measurement and the report file
   * @param resource  the classpath name of the captured baseline
   * @param schema  the documented key sets of one row of that baseline
   * @param check  the comparisons to apply to one row, answering with everything that differed
   * @return the published report; the effect fails only when the report directory is not
   *         configured as an absolute path, when the fixture cannot be read, is empty, does not
   *         carry the declared keys or does not decode, or when the report cannot be written
   */
  def runFixture[R <: ParityRow: Decoder](fixture: String, resource: String, schema: KeySchema)(
      check: R => IO[List[String]]): IO[ParityReport] =
    runFixtureWith[R, Unit](fixture, resource, schema)(_ => IO.unit)((_, row) => check(row))

  /**
   * Measures a whole fixture that needs something built from the whole document first.
   *
   * A few fixtures cannot be measured row by row from the row alone: one captured operation names
   * no rate of its own and is replayed against the rates the document as a whole registers, so
   * something has to be assembled from every row before the first row can be checked. Building it
   * in the spec, before the driver runs, is what this method exists to prevent: a port regression
   * that made one captured input unbuildable would then raise before any report was written, and
   * a failing parity run would leave Gate 3 with no counts at all - exactly the case the counts
   * exist for.
   *
   * So `setup` runs '''inside''' the measurement, under [[cats.effect.IO.attempt]]. When it
   * succeeds, every row is checked against what it produced. When it fails, the failure becomes
   * one attributed discrepancy in a report of `passed = 0` - no row was measured, which is what
   * the counting convention then says - the report is written like any other, and the verdict is
   * left to [[failIfAny]], which fails the spec with the artefact already on disk.
   *
   * @param fixture  the fixture stem, which names both the measurement and the report file
   * @param resource  the classpath name of the captured baseline
   * @param schema  the documented key sets of one row of that baseline
   * @param setup  what the row checks need, built from the whole document
   * @param check  the comparisons to apply to one row, given that
   * @return the published report
   */
  def runFixtureWith[R <: ParityRow: Decoder, S](
      fixture: String,
      resource: String,
      schema: KeySchema)(setup: Vector[R] => IO[S])(check: (S, R) => IO[List[String]]): IO[ParityReport] =
    reportDir.flatMap(directory =>
      runFixtureWithIn[R, S](directory, fixture, resource, Some(schema))(setup)(check))

  /**
   * Measures a whole fixture and publishes the result into a named directory.
   *
   * This is [[runFixture]] with the one piece of ambient configuration - where the report goes -
   * supplied as an argument instead of read from a system property. Every spec uses [[runFixture]]
   * and therefore the configured directory; this form exists so that the harness's own contract
   * tests can observe a published report, including the report of a failing run, without writing
   * into the directory the gate collects.
   *
   * @param directory  the directory the report is written to, which is created if absent
   * @param fixture  the fixture stem, which names both the measurement and the report file
   * @param resource  the classpath name of the captured baseline
   * @param check  the comparisons to apply to one row, answering with everything that differed
   * @return the published report
   */
  private[parity] def runFixtureIn[R <: ParityRow: Decoder](
      directory: Path,
      fixture: String,
      resource: String)(check: R => IO[List[String]]): IO[ParityReport] =
    runFixtureWithIn[R, Unit](directory, fixture, resource, None)(_ => IO.unit)((_, row) => check(row))

  /**
   * The one implementation of the measurement, of which every other driver form is a special
   * case.
   *
   * The order of the four steps is the contract: the rows are decoded, the shared context is
   * built under `attempt` so that its failure is report content rather than an escape, every row
   * is measured in document order, and the report is written before this method returns and
   * therefore before any verdict can be reached. The counts it publishes are checked against
   * [[contractViolations]] on the way out, which is what keeps this module and `strata-collect`
   * publishing the same five keys with the same meaning.
   *
   * @param directory  the directory the report is written to, which is created if absent
   * @param fixture  the fixture stem, which names both the measurement and the report file
   * @param resource  the classpath name of the captured baseline
   * @param schema  the documented key sets of one row, where the caller declares them
   * @param setup  what the row checks need, built from the whole document
   * @param check  the comparisons to apply to one row, given that
   * @return the published report
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
   * The report of a run whose shared context could not be built.
   *
   * No row was measured, so `passed` is zero and the single discrepancy is attributed to the
   * setup rather than to a row: attributing it to a row would name a row that is not at fault,
   * and leaving it unattributed would put an entry in the report that says nothing about where to
   * look. The identity is the fixture stem with `:setup`, which no captured row carries.
   *
   * @param fixture  the fixture stem
   * @param rows  the number of rows the document held, all of them unmeasured
   * @param error  what building the context failed with
   * @return the report to publish
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
   * Writes a report, once its counts have been checked against the shared convention.
   *
   * The check comes first, and its failure is not a parity result: counts that contradict the
   * convention are a defect in this harness, and publishing them would put a number into
   * `target/gate-report.md` that means something other than what the column says. There is
   * nothing worth publishing in that case, so the effect fails naming the violation instead.
   *
   * @param directory  the directory to write into
   * @param report  the report to publish
   * @return nothing; the effect fails when the counts are inconsistent or the write fails
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
   * The ways in which a report can contradict the counting convention.
   *
   * The convention is documented on [[ParityReport]] and is shared with the `double-array` report
   * that `strata-collect` publishes, which restates these same invariants and asserts them of its
   * own report. They are stated here as a list rather than as an assertion so that both a harness
   * guard and a spec can use them, and so that a violation names itself. There are four:
   *
   *  - '''The counts are in range.''' `passed` counts rows, so it is between zero and `rows`, and
   *    neither it nor `failed` nor `rows` is negative.
   *  - '''A clean report measured something, and measured all of it.''' `failed == 0` says every
   *    comparison matched, which is only meaningful over at least one row and requires `passed`
   *    to be all of them. This is the invariant that stops an empty or abandoned run from being
   *    read as a pass - the condition Gate 3 asserts is `failed == 0`, so a run that measured
   *    nothing must not be able to publish it.
   *  - '''A discrepancy is described.''' When `failed` is positive the report lists at least one
   *    discrepancy and never more than it counts, so a module that caps its list still reports
   *    the true total.
   *  - '''A discrepancy belongs to a row that did not pass.''' When something differed and rows
   *    were measured, `passed` is short of `rows`.
   *
   * The last three are what admit the two shapes a report takes when the measurement could not be
   * completed, both of which are worth publishing and neither of which is a pass: a run whose
   * shared context could not be built (`rows` rows, none passed, one discrepancy naming the
   * setup) and, in `strata-collect`, a run whose fixture could not even be read (`rows = 0`, none
   * passed, one discrepancy naming the refusal).
   *
   * @param report  the report to check
   * @return every violation found, or nothing when the report is consistent
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
   * The property is read inside the effect so that no spec touches ambient state while it is being
   * constructed, and the decision it feeds is [[reportDirectoryFrom]].
   */
  private def reportDir: IO[Path] =
    IO.delay(sys.props.get(ReportDirectoryProperty))
      .flatMap(configured =>
        IO.fromEither(reportDirectoryFrom(configured).leftMap(new IllegalStateException(_))))

  /**
   * Decides where reports go, from the configured text alone.
   *
   * Two values are refused, and neither refusal has a fallback.
   *
   * '''Nothing configured.''' Gate 3 reads the six reports from one path. A default directory would
   * let a misconfigured run write them somewhere the gate never looks, and a gate that finds no
   * report is indistinguishable from a measurement that was never made.
   *
   * '''A relative path.''' Tests are forked, so a relative path resolves against the working
   * directory of whichever forked JVM ran the spec - which is not the build root the gate collects
   * from, and is not even the same for the two projects. A relative value is therefore a
   * misconfiguration that would scatter the reports rather than a shorter way of naming the right
   * directory, and `build.sbt` supplies an absolute path precisely so that it never arises.
   *
   * @param configured  the value of the system property, if it is set
   * @return the directory, or the explanation of why the configured value cannot be used
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
   * Writes one report into a named directory, creating the directory if it is not there yet.
   *
   * The document is the JSON of [[ParityReport]] and carries exactly its five fields, in
   * declaration order, followed by a single newline so that the file is a well-formed text file.
   *
   * @param directory  the directory to write into
   * @param report  the report to publish
   * @return the path written, so that a caller can name it
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
