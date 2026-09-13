/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxMatrix
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * Parity of the FX surface against the committed baseline, which holds the reference values and
 * is read-only here.
 *
 * The measurement covers the matrix a list of rates builds, the rate answered for a pair, the
 * conversion of a single amount and of a multi-currency amount, the cross rate of two `FxRate`
 * values, and the merge of two matrices. Every row rebuilds a matrix from its insertion-ordered
 * rate list and replays the recorded operations against it.
 *
 * ===What is compared, and with which comparator===
 *
 * Every '''number''' - a rate, a converted amount, a cross rate, an element of a matrix - goes
 * through [[ParityHarness.assertParity]] and is at parity only within 1e-9 of the reference value
 * '''both''' absolutely and relatively. Every '''identity''' - a currency code, a pair name, and
 * the currency '''order''' of a matrix - goes through [[ParityHarness.assertExact]] and must be
 * equal. No tolerance is ever applied to an identity, and no identity is compared with one.
 *
 * The relative bound earns its place on the values here: crossing `EUR/USD` 1.1428571428571428
 * with `USD/GBP` 0.8 gives 0.9142857142857143, while the same quantity computed by a different
 * association - 0.8 / 0.875 - gives 0.9142857142857144, a last-place difference.
 *
 * ===Non-finite values are part of the measurement===
 *
 * Two scenarios define `JPY/CAD` as `0.0`, so the reciprocal `CAD/JPY` is `+∞`, which this
 * library's single non-finite policy writes as the tagged string `"Infinity"`. That is why
 * `Codecs.implicits._` is in scope in the companion '''before''' the decoders are derived:
 * without it a `Double` field would be read by circe's own decoder and the row would fail to
 * decode instead of being measured. Two equal non-finite values are at parity without arithmetic,
 * because `assertParity` compares with `java.lang.Double.compare` first, which reports equality
 * for two identical non-finite values; `|∞ − ∞|` is `NaN` and would otherwise defeat every bound.
 * Nothing special-cases them, and the second test asserts at least one such expectation is
 * present, so losing those rows cannot leave a green measurement behind.
 *
 * ===An error is an expectation===
 *
 * A rate query for a currency the matrix does not hold, a cross of two rates with no unique
 * common currency and a merge with a matrix sharing no currency are failures that depend on the
 * data of their arguments, so each is asserted through the error channel, uniformly through
 * [[ParityHarness.assertLeft]] and [[ParityHarness.assertRight]] - no `intercept`, no
 * `try`/`catch`, and no `Either` opened with `.get`, `.value` or a partial function. The
 * '''reason''' is asserted as `Failure.CurrencyConversion`, because every constructor reachable
 * on the paths this fixture exercises builds one: `FxMatrix.noRateFound`,
 * `FxMatrix.noCommonCurrency`, `FxMatrix.unplaceableRates` and the no-unique-common-currency
 * branch of `FxRate.crossRate`. The recorded message is never asserted; it is quoted in the
 * diagnostic.
 *
 * ===Row schema===
 *
 * Every row carries the same nine keys, and an expectation is nested '''inside''' the input that
 * produced it rather than held in a parallel array, so an expectation cannot be misaligned with
 * its input:
 *
 * {{{
 * {"id":"single-pair-matrix","source":"…",
 *  "matrix":[{"pair":"GBP/USD","rate":1.6}],
 *  "matrixState":{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]},
 *  "queries":[{"base":"GBP","counter":"USD","fxRate":1.6},
 *             {"base":"USD","counter":"EUR","error":"…"}],
 *  "conversions":[{"currency":"GBP","amount":100.0,"target":"USD","converted":{…}}],
 *  "multi":[{"amounts":[{"currency":"GBP","amount":1600.0}],"target":"USD","multiConverted":{…}}],
 *  "crosses":[{"rate1":{"pair":"EUR/USD","rate":1.1428571428571428},
 *              "rate2":{"pair":"USD/GBP","rate":0.8},
 *              "crossRate":{"pair":"EUR/GBP","rate":0.9142857142857143}}],
 *  "merges":[{"other":[{"pair":"EUR/CHF","rate":1.2}],"merged":{"currencies":[…],"rates":[[…],…]}}]}
 * }}}
 *
 * An entry carries '''exactly one''' of its expectation and an `error`, and the discriminator is
 * per entry, so one list may legitimately mix the two - `identity-and-single-rate-conversion`
 * answers three identity queries and refuses five genuine ones. "Exactly one" is read strictly
 * rather than preferentially: an entry declaring both keys and an entry declaring neither are
 * both refused rather than resolved in favour of one, because resolving in favour of the `error`
 * would measure a recorded value as a refusal and report a pass for a comparison that never
 * happened. A key present with a JSON `null` declares nothing. A documented variant adds a tenth
 * top-level `error` key for a definition the builder itself rejects, with `matrixState` null and
 * the five lists empty; no committed row exercises it, and it is read and measured all the same.
 *
 * Those key sets are '''enforced''': every shape declares its keys as a [[KeySchema]] beside the
 * model that reads it - the row's through [[ParityHarness.loadStrict]], every nested object's
 * through [[ParityHarness.strictObject]] - so an object whose keys are not the documented ones is
 * refused with the keys named. A derived decoder would otherwise ignore a key it had gained,
 * leaving a newly captured expectation unmeasured while this report still read `failed == 0`.
 *
 * ===Why the matrix is rebuilt in bulk===
 *
 * A row's `matrix` is the defining list of rates '''in the order they were added''', and that
 * order is part of the value: it decides the matrix's currency order and, for a pair whose
 * currencies are both already present, which rate is the reference - updating a rate is not
 * symmetric, and a switch of direction matters. The list is therefore never sorted, reordered,
 * deduplicated or filtered, and a rate of `0.0` is placed where the builder places it. It is
 * rebuilt through [[com.opengamma.strata.basics.currency.FxMatrix.ofRates]], which folds left to
 * right; the bulk form is '''required''' rather than convenient, because a rate for two
 * currencies neither of which is held yet - which this fixture carries - is refused by the single
 * `withRate` by design, while `withRates` holds such a rate back and retries it when a later rate
 * brings its connection in. A definition that does not rebuild is a defect in the fixture or in
 * this module rather than an expectation, so it is lifted with [[ParityHarness.raise]] and fails
 * the row loudly.
 *
 * Each row also carries `matrixState` as `{currencies, rates}`, so the rebuild itself is
 * measured: the currency vector exactly, which asserts the insertion order rather than assuming
 * it, and every rate element at 1e-9.
 */
class FxParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import FxParitySpec._

  test("the FX matrix, rate, conversion, cross-rate and merge surfaces reproduce the Java baseline exactly") {
    ParityHarness
      .runFixture[FxRow](FixtureName, FixtureResource, RowSchema)(checkRow)
      .flatMap(report => ParityHarness.failIfAny(report))
      .as(succeed)
  }

  test("the fixture carries the population the FX baseline is required to measure") {
    ParityHarness.loadStrict[FxRow](FixtureResource, RowSchema).map { rows =>
      val counted = Counts.combineAll(rows.map(_.counts))
      withClue(s"fixture rows: ${rows.size}; measured entries: $counted: ") {
        // The floors are the population this fixture is required to carry, restated where it is
        // consumed: a gate reading `failed == 0` cannot tell a complete measurement from a
        // thinned one. They are floors, so extending the coverage stays possible.
        rows.size should be >= MinimumRows
        rows.map(_.id).distinct should have size rows.size.toLong
        RequiredRowIds.filterNot(rows.map(_.id).contains) shouldBe empty
        // Every scenario names the population it came from, which makes a failure attributable
        // to a scenario.
        rows.filter(_.source.trim.isEmpty).map(_.id) shouldBe empty
        counted.matrixEntries should be >= MinimumMatrixEntries
        counted.queries should be >= MinimumQueries
        counted.conversions should be >= MinimumConversions
        counted.multi should be >= MinimumMultiConversions
        counted.crosses should be >= MinimumCrosses
        counted.merges should be >= MinimumMerges
        // An error is an expectation, so the refusals are a population in their own right: a
        // fixture that kept every value and dropped every error would measure only the happy path.
        counted.refusals should be >= MinimumRefusals
        // The tagged non-finite policy is load bearing here, so the value that motivates it is
        // asserted to be present: losing the zero-rate rows would leave a green measurement of a
        // smaller surface behind.
        withClue("no expectation carries a non-finite value, so the zero-rate rows are missing: ") {
          rows.exists(_.hasNonFiniteExpectation) shouldBe true
        }
        // A row carries either the state its definition built or the error that prevented it,
        // never both and never neither, and a rejected definition carries no operations.
        rows.filter(row => row.error.isDefined == row.matrixState.isDefined).map(_.id) shouldBe empty
        rows.filter(row => row.error.isDefined && row.entryCounts.sum > 0).map(_.id) shouldBe empty
        // Each surface has its own refusals, so a fixture that kept the same number of errors
        // but moved them all into one list would not pass.
        counted.refusedQueries should be >= MinimumRefusedQueries
        counted.refusedCrosses should be >= MinimumRefusedCrosses
        counted.refusedMerges should be >= MinimumRefusedMerges
        // The recorded currency order of a definition is the order its currencies first appear
        // in it, including the definitions whose held-back rate is placed only once a later rate
        // connects it.
        rows
          .filter(row =>
            row.matrixState.exists(state => state.currencies != row.firstAppearanceOrder))
          .map(_.id) shouldBe empty
      }
    }
  }

  test("an entry's outcome is read from exactly one declared key, and every other shape is refused") {
    // Crafted entries rather than the fixture: every committed entry states exactly one outcome,
    // so the document cannot demonstrate that the other shapes are refused, and an entry carrying
    // both an `error` and its recorded value would otherwise be measured as a refusal with the
    // value never compared. The declared key set refuses both keys at once and neither by
    // omission; inside a declared key set a key present with JSON `null` states nothing.
    IO {
      val queryPrefix = """{"base":"GBP","counter":"USD","""
      val conversionPrefix = """{"currency":"GBP","amount":100.0,"target":"USD","""
      val convertedAmount = """{"currency":"USD","amount":160.0}"""
      val javaMessage = "IllegalArgumentException: No FX rate found for USD/EUR"

      parser.decode[Query](s"""$queryPrefix"fxRate":1.6}""") shouldBe
        Right(Query("GBP", "USD", Expected.Value(1.6d)))
      parser.decode[Query](s"""$queryPrefix"$ErrorField":"$javaMessage"}""") shouldBe
        Right(Query("GBP", "USD", Expected.Failed(javaMessage)))
      parser.decode[Conversion](s"""$conversionPrefix"converted":$convertedAmount}""") shouldBe
        Right(Conversion("GBP", 100.0d, "USD", Expected.Value(AmountValue("USD", 160.0d))))

      // A conversion carrying an `error` instead is refused by its key set rather than read as a
      // refusal: the shape is not a documented one, so it is reported rather than measured.
      withClue("a conversion carrying an error: ") {
        parser
          .decode[Conversion](s"""$conversionPrefix"$ErrorField":"$javaMessage"}""")
          .left
          .map(_.getMessage) match {
          case Left(message) =>
            message should include("satisfy no documented variant")
            message should include(s"unknown keys {$ErrorField}")
            message should include("missing {converted}")
          case Right(entry) => fail(s"a conversion carrying an error decoded as $entry")
        }
      }

      val bothDeclared =
        Vector[(String, String, Either[io.circe.Error, Any])](
          (
            "queries",
            "fxRate",
            parser.decode[Query](s"""$queryPrefix"fxRate":1.6,"$ErrorField":"$javaMessage"}""")),
          (
            "conversions",
            "converted",
            parser.decode[Conversion](
              s"""$conversionPrefix"converted":$convertedAmount,"$ErrorField":"$javaMessage"}""")),
          (
            "multi",
            "multiConverted",
            parser.decode[MultiConversion](
              s"""{"amounts":[{"currency":"GBP","amount":1600.0}],"target":"USD",""" +
                s""""multiConverted":{"currency":"USD","amount":2560.0},""" +
                s""""$ErrorField":"$javaMessage"}""")),
          (
            "crosses",
            "crossRate",
            parser.decode[Cross](
              s"""{"rate1":{"pair":"EUR/USD","rate":1.25},"rate2":{"pair":"USD/GBP","rate":0.8},""" +
                s""""crossRate":{"pair":"EUR/GBP","rate":1.0},"$ErrorField":"$javaMessage"}""")),
          (
            "merges",
            "merged",
            parser.decode[Merge](
              s"""{"other":[{"pair":"EUR/CHF","rate":1.2}],""" +
                s""""merged":{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]},""" +
                s""""$ErrorField":"$javaMessage"}""")))
      bothDeclared.foreach { case (group, key, outcome) =>
        withClue(s"a $group entry declaring both '$ErrorField' and '$key': ") {
          outcome.left.map(_.getMessage) match {
            case Left(message) =>
              message should include("satisfy no documented variant")
              message should include(ErrorField)
              message should include(key)
            case Right(entry) => fail(s"an entry declaring both outcomes decoded as $entry")
          }
        }
      }

      // Neither key declared: by omission the key set decides it, naming the outcome key the
      // shape wanted and did not get; by `null` the entry decoder decides it, because a key
      // present and null declares nothing. Both are refusals, never a default to either side.
      val neitherByOmission =
        Vector[(String, String, Either[io.circe.Error, Any])](
          ("queries", "fxRate", parser.decode[Query]("""{"base":"GBP","counter":"USD"}""")),
          (
            "conversions",
            "converted",
            parser.decode[Conversion]("""{"currency":"GBP","amount":100.0,"target":"USD"}""")))
      neitherByOmission.foreach { case (group, key, outcome) =>
        withClue(s"a $group entry omitting both '$ErrorField' and '$key': ") {
          outcome.left.map(_.getMessage) match {
            case Left(message) =>
              message should include("satisfy no documented variant")
              message should include(s"missing {$key}")
            case Right(entry) => fail(s"an entry declaring no outcome decoded as $entry")
          }
        }
      }
      withClue(s"a query whose '$ErrorField' is present and null: ") {
        parser.decode[Query](s"""$queryPrefix"$ErrorField":null}""").left.map(_.getMessage) match {
          case Left(message) =>
            message should include(s"exactly one of '$ErrorField' and 'fxRate'")
            message should include("declares neither")
            message should include("present and null")
          case Right(entry) => fail(s"a query stating no outcome decoded as $entry")
        }
      }

      withClue("an outcome stated with the wrong JSON type: ") {
        parser.decode[Query](s"""$queryPrefix"fxRate":true}""").isLeft shouldBe true
        parser.decode[Query](s"""$queryPrefix"$ErrorField":404}""").isLeft shouldBe true
        parser
          .decode[Conversion](s"""$conversionPrefix"converted":"USD 160.0"}""")
          .isLeft shouldBe true
      }
      succeed
    }
  }

  /*
   * The three tests below measure the decoding rather than the implementation: a derived decoder
   * reads the fields its model declares and ignores every other key, so they hold each shape's
   * declared keys to their word, a key gained, lost or renamed being refused by name.
   */
  test("every documented FX object shape is read under its declared keys and no others") {
    IO {
      val discrepancies = Shapes.toList.flatMap(shape => checkStrictness(shape))
      withClue(s"${discrepancies.size} shape(s) not strictly decoded: ${discrepancies.mkString("; ")}: ") {
        discrepancies shouldBe empty
      }
    }
  }

  test("an FX entry carrying both outcome keys, or neither, is refused by its key set") {
    IO {
      val discrepancies =
        OutcomeShapes.toList.flatMap { case (shape, valueKey) => checkOutcomeKeys(shape, valueKey) }
      withClue(s"${discrepancies.size} outcome key set(s) admitted: ${discrepancies.mkString("; ")}: ") {
        discrepancies shouldBe empty
      }
    }
  }

  test("the FX row reads the documented tenth key and nothing beyond the nine") {
    IO {
      withClue(s"the nine-key row, carrying {${keysOf(RowSample).mkString(", ")}}, was refused: ") {
        refusalOf(strictRowDecoder.decodeJson(RowSample)) shouldBe None
      }
      val rejectedDefinition = strictRowDecoder.decodeJson(RefusedRowSample)
      withClue(s"the tenth-key row was refused: ${refusalOf(rejectedDefinition).getOrElse("")}: ") {
        rejectedDefinition.map(row => (row.error.isDefined, row.matrixState, row.entryCounts.sum)) shouldBe
          Right((true, None, 0))
      }
      // Nothing beyond those two shapes: an undocumented key is refused on either of them, by
      // name, which is what keeps `optional` from being a hole in the row's key set.
      val refusals =
        List(RowSample, RefusedRowSample).flatMap(row =>
          refusalOf(strictRowDecoder.decodeJson(withKey(row, AddedKey, Json.True))))
      withClue(s"a row carrying '$AddedKey' was accepted: ") {
        refusals.size shouldBe 2
      }
      withClue(s"a refusal does not name '$AddedKey': ${refusals.mkString("; ")}: ") {
        refusals.filterNot(message => message.contains(AddedKey)) shouldBe empty
      }
    }
  }
}

/**
 * The row model of `parity/fx-baseline.json`, and the comparisons applied to one row. No field is
 * interpreted on the way in and no default invented for an absent one, and the rate matrices are
 * read as plain nested vectors rather than through this library's own matrix codec.
 */
private[parity] object FxParitySpec {

  // This import must precede the derivations below: it is what reads the tagged `"Infinity"` of
  // the zero-rate rows as `Double.PositiveInfinity` instead of failing the row.
  import Codecs.implicits.doubleCodec

  //-------------------------------------------------------------------------
  // Contract constants, and the population the committed baseline holds as floors: 14 scenarios,
  // 60 defining rates, 304 queries (7 refused), 42 conversions, 6 multi-currency conversions,
  // 13 crosses (5 refused), 4 merges (2 refused), and 14 refusals in all.
  //-------------------------------------------------------------------------

  val FixtureName: String = "fx"

  val FixtureResource: String = "parity/fx-baseline.json"

  val ErrorField: String = "error"

  /** The reason every refusal of this surface carries. */
  val RefusalReason: FailureReason = FailureReason.CURRENCY_CONVERSION

  val MinimumRows: Int = 14

  val MinimumMatrixEntries: Int = 60

  val MinimumQueries: Int = 304

  val MinimumConversions: Int = 42

  val MinimumMultiConversions: Int = 6

  val MinimumCrosses: Int = 13

  val MinimumMerges: Int = 4

  val MinimumRefusals: Int = 14

  val MinimumRefusedQueries: Int = 7

  val MinimumRefusedCrosses: Int = 5

  val MinimumRefusedMerges: Int = 2

  /** Every scenario the baseline is required to carry; losing one narrows the measured surface. */
  val RequiredRowIds: Vector[String] =
    Vector(
      "cross-rate-triangulating-matrix",
      "convert-multi-currency-amount",
      "zero-rate-connected-matrix",
      "shifted-zero-rate-connected-matrix",
      "resize-nine-currency-matrix",
      "empty-matrix",
      "single-pair-matrix",
      "identity-and-single-rate-conversion",
      "merge-cases",
      "random-scenario-0",
      "random-scenario-1",
      "random-scenario-2",
      "random-scenario-3",
      "fx-rate-cross-rates")

  //-------------------------------------------------------------------------
  // The row model, each shape's declared key schema beside the model that reads it.
  //-------------------------------------------------------------------------

  /**
   * One outcome: a value, or the refusal standing in its place - not an `Option`, because a
   * refusal is an expectation in its own right and an absence would lose its recorded message.
   */
  sealed trait Expected[+A] {

    def asOption: Option[A]
  }

  object Expected {

    final case class Value[+A](value: A) extends Expected[A] {
      override def asOption: Option[A] = Some(value)
    }

    final case class Failed(message: String) extends Expected[Nothing] {
      override def asOption: Option[Nothing] = None
    }
  }

  /** A rate for a currency pair, as `{"pair": "EUR/USD", "rate": 1.4}`. */
  final case class RatePoint(pair: String, rate: Double)

  val RatePointSchema: KeySchema = KeySchema.uniform("rate point", Set("pair", "rate"))

  /** An amount in a currency, as `{"currency": "USD", "amount": 2560.0}`. */
  final case class AmountValue(currency: String, amount: Double)

  val AmountValueSchema: KeySchema = KeySchema.uniform("amount", Set("currency", "amount"))

  /** The state of a built matrix: its currencies in matrix order, and its rates row by row. */
  final case class MatrixState(currencies: Vector[String], rates: Vector[Vector[Double]]) {

    def rowCount: Int = rates.size

    /** The number of columns the recorded matrix holds, zero for a matrix with no rows. */
    def columnCount: Int = rates.headOption.fold(0)(_.size)

    def allRates: Vector[Double] = rates.flatten
  }

  val MatrixStateSchema: KeySchema = KeySchema.uniform("matrix state", Set("currencies", "rates"))

  /** A rate query and its outcome, as `{"base": "GBP", "counter": "USD", "fxRate": 1.6}`. */
  final case class Query(base: String, counter: String, fxRate: Expected[Double])

  /** The keys of a [[Query]]: two variants of one schema, so the outcome is exclusive by keys. */
  val QuerySchema: KeySchema =
    KeySchema.variants(
      "query",
      "answered" -> Set("base", "counter", "fxRate"),
      "refused" -> Set("base", "counter", ErrorField))

  /** A single-amount conversion and its outcome. */
  final case class Conversion(
      currency: String,
      amount: Double,
      target: String,
      converted: Expected[AmountValue])

  /** The keys of a [[Conversion]]: only the answered set; no refused conversion is documented. */
  val ConversionSchema: KeySchema =
    KeySchema.uniform("conversion", Set("currency", "amount", "target", "converted"))

  /** A multi-currency conversion and its outcome. */
  final case class MultiConversion(
      amounts: Vector[AmountValue],
      target: String,
      multiConverted: Expected[AmountValue])

  val MultiConversionSchema: KeySchema =
    KeySchema.uniform("multi-currency conversion", Set("amounts", "target", "multiConverted"))

  /** A cross of two rates and its outcome. */
  final case class Cross(rate1: RatePoint, rate2: RatePoint, crossRate: Expected[RatePoint])

  val CrossSchema: KeySchema =
    KeySchema.variants(
      "cross",
      "answered" -> Set("rate1", "rate2", "crossRate"),
      "refused" -> Set("rate1", "rate2", ErrorField))

  /** A merge with another matrix and its outcome; merging with the empty matrix is refused. */
  final case class Merge(other: Vector[RatePoint], merged: Expected[MatrixState])

  val MergeSchema: KeySchema =
    KeySchema.variants(
      "merge",
      "answered" -> Set("other", "merged"),
      "refused" -> Set("other", ErrorField))

  /**
   * One scenario: the defining rates in order, the state they built and the operations replayed
   * against it; `matrixState` is absent and `error` present only for a rejected definition.
   */
  final case class FxRow(
      id: String,
      source: String,
      matrix: Vector[RatePoint],
      matrixState: Option[MatrixState],
      queries: Vector[Query],
      conversions: Vector[Conversion],
      multi: Vector[MultiConversion],
      crosses: Vector[Cross],
      merges: Vector[Merge],
      error: Option[String])
      extends ParityRow {

    def entryCounts: Vector[Int] =
      Vector(queries.size, conversions.size, multi.size, crosses.size, merges.size)

    def counts: Counts =
      Counts(
        matrixEntries = matrix.size,
        queries = queries.size,
        conversions = conversions.size,
        multi = multi.size,
        crosses = crosses.size,
        merges = merges.size,
        refusedQueries = queries.count(_.fxRate.asOption.isEmpty),
        refusedConversions = conversions.count(_.converted.asOption.isEmpty),
        refusedMulti = multi.count(_.multiConverted.asOption.isEmpty),
        refusedCrosses = crosses.count(_.crossRate.asOption.isEmpty),
        refusedMerges = merges.count(_.merged.asOption.isEmpty))

    def expectedDoubles: Vector[Double] =
      matrixState.toVector.flatMap(_.allRates) ++
        queries.flatMap(_.fxRate.asOption) ++
        conversions.flatMap(_.converted.asOption.map(_.amount)) ++
        multi.flatMap(_.multiConverted.asOption.map(_.amount)) ++
        crosses.flatMap(_.crossRate.asOption.map(_.rate)) ++
        merges.flatMap(_.merged.asOption.toVector.flatMap(_.allRates))

    def hasNonFiniteExpectation: Boolean =
      expectedDoubles.exists(value => !java.lang.Double.isFinite(value))

    /**
     * The currencies of this definition in the order they first appear, read from the recorded
     * names rather than resolved, which would weaken the comparison they feed.
     */
    def firstAppearanceOrder: Vector[String] =
      matrix.foldLeft(Vector.empty[String]) { (seen, entry) =>
        entry.pair
          .split('/')
          .toVector
          .foldLeft(seen)((known, code) => if (known.contains(code)) known else known :+ code)
      }
  }

  /**
   * The keys of an [[FxRow]]: the documented nine, and the documented tenth. A list a scenario
   * does not exercise is carried empty rather than omitted, and `error` is `optional` rather than
   * a second variant, so both documented shapes are read and everything else refused; the
   * exclusivity it implies is not a property of the key set and is measured by `checkShape`.
   */
  val RowSchema: KeySchema =
    KeySchema
      .uniform(
        "fx row",
        Set(
          "id",
          "source",
          "matrix",
          "matrixState",
          "queries",
          "conversions",
          "multi",
          "crosses",
          "merges"))
      .withOptional(Set(ErrorField))

  /** What the fixture holds, as counted by the population test. */
  final case class Counts(
      matrixEntries: Int,
      queries: Int,
      conversions: Int,
      multi: Int,
      crosses: Int,
      merges: Int,
      refusedQueries: Int,
      refusedConversions: Int,
      refusedMulti: Int,
      refusedCrosses: Int,
      refusedMerges: Int) {

    def entries: Int = queries + conversions + multi + crosses + merges

    def refusals: Int =
      refusedQueries + refusedConversions + refusedMulti + refusedCrosses + refusedMerges

    def combine(other: Counts): Counts =
      Counts(
        matrixEntries = matrixEntries + other.matrixEntries,
        queries = queries + other.queries,
        conversions = conversions + other.conversions,
        multi = multi + other.multi,
        crosses = crosses + other.crosses,
        merges = merges + other.merges,
        refusedQueries = refusedQueries + other.refusedQueries,
        refusedConversions = refusedConversions + other.refusedConversions,
        refusedMulti = refusedMulti + other.refusedMulti,
        refusedCrosses = refusedCrosses + other.refusedCrosses,
        refusedMerges = refusedMerges + other.refusedMerges)

    override def toString: String =
      s"matrix=$matrixEntries, queries=$queries, conversions=$conversions, multi=$multi, " +
        s"crosses=$crosses, merges=$merges, refusals=$refusals"
  }

  object Counts {

    val empty: Counts = Counts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def combineAll(values: Vector[Counts]): Counts =
      values.foldLeft(empty)((total, next) => total.combine(next))
  }

  //-------------------------------------------------------------------------
  // Decoders. Every product is derived; the five entry decoders are hand-written because an
  // entry's expectation is exactly one of two sibling keys rather than a field of its own.
  //-------------------------------------------------------------------------

  /**
   * Reads an entry's outcome from the one key that states it, and refuses every other shape.
   * Both keys are inspected before either is read, so an entry carrying both is refused rather
   * than measured as a refusal with its recorded value discarded; a key declares an outcome only
   * when present and not JSON `null`, see [[declared]]; and the declared key is decoded from its
   * own cursor, so the wrong JSON type fails to decode rather than reading as the sibling.
   */
  private def expectedDecoder[A: Decoder](field: String): Decoder[Expected[A]] =
    Decoder.instance { cursor =>
      val error = cursor.downField(ErrorField)
      val value = cursor.downField(field)
      (declared(error), declared(value)) match {
        case (true, false) => error.as[String].map(message => Expected.Failed(message))
        case (false, true) => value.as[A].map(captured => Expected.Value(captured))
        case (true, true) => Left(DecodingFailure(unstatedOutcome(field, "both"), cursor.history))
        case (false, false) =>
          Left(DecodingFailure(unstatedOutcome(field, "neither"), cursor.history))
      }
    }

  /**
   * Whether the key a cursor points at declares an outcome: it does when present '''and''' not
   * JSON `null`, so that the sibling fixtures' habit of writing `"error": null` on a successful
   * entry declares nothing rather than a second outcome.
   */
  private def declared(cursor: ACursor): Boolean =
    cursor.succeeded && !cursor.focus.exists(json => json.isNull)

  /** The message for an entry with no single outcome: it names both keys and the `null` rule. */
  private def unstatedOutcome(field: String, declaredKeys: String): String =
    s"an entry states its outcome in exactly one of '$ErrorField' and '$field', but this entry " +
      s"declares $declaredKeys (a key that is absent, or present and null, declares nothing)"

  implicit val ratePointDecoder: Decoder[RatePoint] =
    ParityHarness.strictObject(RatePointSchema)(deriveDecoder[RatePoint])

  implicit val amountValueDecoder: Decoder[AmountValue] =
    ParityHarness.strictObject(AmountValueSchema)(deriveDecoder[AmountValue])

  implicit val matrixStateDecoder: Decoder[MatrixState] =
    ParityHarness.strictObject(MatrixStateSchema)(deriveDecoder[MatrixState])

  implicit val queryDecoder: Decoder[Query] =
    ParityHarness.strictObject(QuerySchema)(Decoder.instance { cursor =>
      for {
        base <- cursor.get[String]("base")
        counter <- cursor.get[String]("counter")
        fxRate <- expectedDecoder[Double]("fxRate").apply(cursor)
      } yield Query(base, counter, fxRate)
    })

  implicit val conversionDecoder: Decoder[Conversion] =
    ParityHarness.strictObject(ConversionSchema)(Decoder.instance { cursor =>
      for {
        currency <- cursor.get[String]("currency")
        amount <- cursor.get[Double]("amount")
        target <- cursor.get[String]("target")
        converted <- expectedDecoder[AmountValue]("converted").apply(cursor)
      } yield Conversion(currency, amount, target, converted)
    })

  implicit val multiConversionDecoder: Decoder[MultiConversion] =
    ParityHarness.strictObject(MultiConversionSchema)(Decoder.instance { cursor =>
      for {
        amounts <- cursor.get[Vector[AmountValue]]("amounts")
        target <- cursor.get[String]("target")
        converted <- expectedDecoder[AmountValue]("multiConverted").apply(cursor)
      } yield MultiConversion(amounts, target, converted)
    })

  implicit val crossDecoder: Decoder[Cross] =
    ParityHarness.strictObject(CrossSchema)(Decoder.instance { cursor =>
      for {
        rate1 <- cursor.get[RatePoint]("rate1")
        rate2 <- cursor.get[RatePoint]("rate2")
        crossRate <- expectedDecoder[RatePoint]("crossRate").apply(cursor)
      } yield Cross(rate1, rate2, crossRate)
    })

  implicit val mergeDecoder: Decoder[Merge] =
    ParityHarness.strictObject(MergeSchema)(Decoder.instance { cursor =>
      for {
        other <- cursor.get[Vector[RatePoint]]("other")
        merged <- expectedDecoder[MatrixState]("merged").apply(cursor)
      } yield Merge(other, merged)
    })

  /** The row's own fields. Not implicit: only the strict decoder below may be summoned. */
  private val fxRowFields: Decoder[FxRow] = deriveDecoder[FxRow]

  /**
   * The row decoder with [[RowSchema]] applied, as the loader applies it, so the loading path and
   * the strictness tests are both strict about keys by construction.
   */
  implicit val strictRowDecoder: Decoder[FxRow] = ParityHarness.strictObject(RowSchema)(fxRowFields)

  //-------------------------------------------------------------------------
  // The documented shapes, as the strictness tests read them: each pairs its schema with an
  // object that satisfies it and the decoder that reads it. The samples are hand-built, because
  // what is tested is this file's statement of what the document is allowed to say.
  //-------------------------------------------------------------------------

  /** A key no documented shape knows, standing for one a later baseline starts carrying. */
  val AddedKey: String = "capturedLater"

  /** One documented object shape: its name, its declared keys, a satisfying object, its decoder. */
  final case class Shape(
      name: String,
      schema: KeySchema,
      sample: Json,
      read: Json => Decoder.Result[Any])

  object Shape {

    def of[A](name: String, schema: KeySchema, sample: Json)(implicit decoder: Decoder[A]): Shape =
      Shape(name, schema, sample, json => decoder.decodeJson(json))
  }

  private def ratePointOf(pair: String, rate: Double): Json =
    Json.obj("pair" -> Json.fromString(pair), "rate" -> Json.fromDoubleOrNull(rate))

  private def amountOf(currency: String, value: Double): Json =
    Json.obj("currency" -> Json.fromString(currency), "amount" -> Json.fromDoubleOrNull(value))

  private val MatrixStateSample: Json =
    Json.obj(
      "currencies" -> Json.arr(Json.fromString("GBP"), Json.fromString("USD")),
      "rates" -> Json.arr(
        Json.arr(Json.fromDoubleOrNull(1.0), Json.fromDoubleOrNull(1.6)),
        Json.arr(Json.fromDoubleOrNull(0.625), Json.fromDoubleOrNull(1.0))))

  private val AnsweredQuerySample: Json =
    Json.obj(
      "base" -> Json.fromString("GBP"),
      "counter" -> Json.fromString("USD"),
      "fxRate" -> Json.fromDoubleOrNull(1.6))

  private val RefusedQuerySample: Json =
    Json.obj(
      "base" -> Json.fromString("USD"),
      "counter" -> Json.fromString("EUR"),
      ErrorField -> Json.fromString("IllegalArgumentException: No FX rate found for USD/EUR"))

  private val ConversionSample: Json =
    Json.obj(
      "currency" -> Json.fromString("GBP"),
      "amount" -> Json.fromDoubleOrNull(100.0),
      "target" -> Json.fromString("USD"),
      "converted" -> amountOf("USD", 160.0))

  private val MultiConversionSample: Json =
    Json.obj(
      "amounts" -> Json.arr(amountOf("GBP", 1600.0)),
      "target" -> Json.fromString("USD"),
      "multiConverted" -> amountOf("USD", 2560.0))

  private val AnsweredCrossSample: Json =
    Json.obj(
      "rate1" -> ratePointOf("EUR/USD", 1.1428571428571428),
      "rate2" -> ratePointOf("USD/GBP", 0.8),
      "crossRate" -> ratePointOf("EUR/GBP", 0.9142857142857143))

  private val RefusedCrossSample: Json =
    Json.obj(
      "rate1" -> ratePointOf("EUR/USD", 1.1428571428571428),
      "rate2" -> ratePointOf("EUR/USD", 1.1428571428571428),
      ErrorField -> Json.fromString(
        "IllegalArgumentException: Currency pairs must have a single currency in common"))

  private val AnsweredMergeSample: Json =
    Json.obj("other" -> Json.arr(ratePointOf("USD/CHF", 1.2)), "merged" -> MatrixStateSample)

  private val RefusedMergeSample: Json =
    Json.obj(
      "other" -> Json.arr(ratePointOf("EUR/CHF", 1.2)),
      ErrorField -> Json.fromString(
        "IllegalArgumentException: FxMatrix must contain a common currency to be merged"))

  val RowSample: Json =
    Json.obj(
      "id" -> Json.fromString("schema-sample"),
      "source" -> Json.fromString("FxParitySpec.RowSample"),
      "matrix" -> Json.arr(ratePointOf("GBP/USD", 1.6)),
      "matrixState" -> MatrixStateSample,
      "queries" -> Json.arr(AnsweredQuerySample, RefusedQuerySample),
      "conversions" -> Json.arr(ConversionSample),
      "multi" -> Json.arr(MultiConversionSample),
      "crosses" -> Json.arr(AnsweredCrossSample, RefusedCrossSample),
      "merges" -> Json.arr(AnsweredMergeSample, RefusedMergeSample))

  /**
   * The row of the documented tenth key: a rejected definition, `matrixState` null, the five
   * lists empty, and the top-level `error` saying why. It is held apart from [[Shapes]] because
   * removing this shape's `optional` tenth key leaves the other documented shape, which is read.
   */
  val RefusedRowSample: Json =
    Json.obj(
      "id" -> Json.fromString("schema-sample-rejected-definition"),
      "source" -> Json.fromString("FxParitySpec.RefusedRowSample"),
      "matrix" -> Json.arr(ratePointOf("GBP/USD", 1.6), ratePointOf("EUR/CHF", 1.2)),
      "matrixState" -> Json.Null,
      "queries" -> Json.arr(),
      "conversions" -> Json.arr(),
      "multi" -> Json.arr(),
      "crosses" -> Json.arr(),
      "merges" -> Json.arr(),
      ErrorField -> Json.fromString(
        "IllegalStateException: Unable to create FX Matrix from the input rates"))

  /**
   * Every documented shape whose sample carries only required keys, each read through the decoder
   * the measurement itself uses for it rather than through a second copy.
   */
  val Shapes: Vector[Shape] =
    Vector(
      Shape.of[FxRow]("row", RowSchema, RowSample)(strictRowDecoder),
      Shape.of[RatePoint]("rate point", RatePointSchema, ratePointOf("GBP/USD", 1.6)),
      Shape.of[AmountValue]("amount", AmountValueSchema, amountOf("USD", 160.0)),
      Shape.of[MatrixState]("matrix state", MatrixStateSchema, MatrixStateSample),
      Shape.of[Query]("answered query", QuerySchema, AnsweredQuerySample),
      Shape.of[Query]("refused query", QuerySchema, RefusedQuerySample),
      Shape.of[Conversion]("conversion", ConversionSchema, ConversionSample),
      Shape.of[MultiConversion](
        "multi-currency conversion",
        MultiConversionSchema,
        MultiConversionSample),
      Shape.of[Cross]("answered cross", CrossSchema, AnsweredCrossSample),
      Shape.of[Cross]("refused cross", CrossSchema, RefusedCrossSample),
      Shape.of[Merge]("answered merge", MergeSchema, AnsweredMergeSample),
      Shape.of[Merge]("refused merge", MergeSchema, RefusedMergeSample))

  /** The three entry shapes whose outcome is one of two sibling keys, with each value key. */
  val OutcomeShapes: Vector[(Shape, String)] =
    Vector(
      Shape.of[Query]("query", QuerySchema, AnsweredQuerySample) -> "fxRate",
      Shape.of[Cross]("cross", CrossSchema, AnsweredCrossSample) -> "crossRate",
      Shape.of[Merge]("merge", MergeSchema, AnsweredMergeSample) -> "merged")

  /**
   * Checks that a shape is read under its documented keys and under no others: the sample must be
   * accepted, while a key no variant knows, each of its own keys removed and each renamed must
   * not be, and every refusal must '''name''' the key concerned.
   */
  def checkStrictness(shape: Shape): List[String] = {
    val accepted = refusalOf(shape.read(shape.sample)) match {
      case Some(message) =>
        List(
          s"${shape.name}: the documented sample, carrying " +
            s"{${keysOf(shape.sample).mkString(", ")}}, was refused, so the declared schema " +
            s"${shape.schema.describe} does not describe it: $message")
      case None => Nil
    }
    val added =
      requireRefusal(
        shape,
        s"with '$AddedKey' added",
        withKey(shape.sample, AddedKey, Json.True),
        AddedKey)
    val mutated = keysOf(shape.sample).toList.flatMap { key =>
      requireRefusal(shape, s"without '$key'", withoutKey(shape.sample, key), key) :::
        requireRefusal(
          shape,
          s"with '$key' renamed to '${renamedOf(key)}'",
          renamedKey(shape.sample, key, renamedOf(key)),
          renamedOf(key))
    }
    accepted ::: added ::: mutated
  }

  /**
   * Checks that an entry whose outcome is one of two keys carries exactly one: both at once would
   * read a recorded value as a refusal, and neither would leave the entry with no expectation.
   */
  def checkOutcomeKeys(shape: Shape, valueKey: String): List[String] = {
    val both = withKey(shape.sample, ErrorField, Json.fromString("IllegalArgumentException: both"))
    val neither = withoutKey(shape.sample, valueKey)
    requireRefusal(shape, s"carrying both '$valueKey' and '$ErrorField'", both, ErrorField) :::
      requireRefusal(shape, s"carrying neither '$valueKey' nor '$ErrorField'", neither, valueKey)
  }

  /** Checks that one mutated object is refused, and that the refusal names the key concerned. */
  private def requireRefusal(
      shape: Shape,
      label: String,
      json: Json,
      key: String): List[String] =
    refusalOf(shape.read(json)) match {
      case None =>
        List(
          s"${shape.name} $label: was accepted, so ${shape.schema.describe} is not being " +
            s"enforced and whatever '$key' carries would go unmeasured")
      case Some(message) if !message.contains(key) =>
        List(
          s"${shape.name} $label: was refused, but the refusal does not name '$key': $message")
      case Some(_) => Nil
    }

  def keysOf(json: Json): Vector[String] =
    json.asObject.fold(Vector.empty[String])(fields => fields.keys.toVector)

  def withKey(json: Json, key: String, value: Json): Json = json.mapObject(_.add(key, value))

  def withoutKey(json: Json, key: String): Json = json.mapObject(_.remove(key))

  def renamedKey(json: Json, key: String, renamed: String): Json =
    json.mapObject(fields => fields.remove(key).add(renamed, fields(key).getOrElse(Json.Null)))

  /** The name a renamed key is given, which no documented variant knows. */
  private def renamedOf(key: String): String = s"${key}Renamed"

  def refusalOf(result: Decoder.Result[Any]): Option[String] =
    result.swap.toOption.map(failure => failure.message)

  //-------------------------------------------------------------------------
  // Measuring one row.
  //-------------------------------------------------------------------------

  /**
   * Measures one scenario, answering with everything that differed from the baseline. Each step
   * is only attempted when the one before it holds: a row whose shape this spec cannot measure is
   * reported as such, a row whose definition was rejected is measured by asserting that refusal,
   * and any other row is rebuilt and every operation replayed. Nothing short-circuits after that.
   */
  def checkRow(row: FxRow): IO[List[String]] = {
    val shape = checkShape(row)
    if (shape.nonEmpty) {
      IO.pure(shape)
    } else {
      val rebuilt = rebuild(row.matrix)
      row.error match {
        case Some(message) => IO.pure(refused("matrix", message, rebuilt))
        case None => ParityHarness.raise(rebuilt).map(matrix => measure(matrix, row))
      }
    }
  }

  /**
   * Checks that a row is shaped as this spec measures it. These are invariants of the row model
   * rather than of the document, so a failure is reported as a fixture that no longer agrees with
   * this spec, and as a message rather than a raise, so every such row is reported.
   */
  private def checkShape(row: FxRow): List[String] = {
    // A row states either the matrix its definition built or the error that prevented it, never
    // both and never neither; the second shape is the documented tenth-key variant.
    val variant =
      if (row.error.isDefined == row.matrixState.isDefined) {
        List(
          "shape: a row carries either its built matrix state or the error that prevented it, " +
            s"but this row carries state=${row.matrixState.isDefined} and " +
            s"error=${row.error.isDefined}")
      } else {
        Nil
      }
    // A definition that was rejected cannot have been queried, so a row that claims both is a row
    // whose expectations cannot all have been recorded from the same state.
    val emptied =
      if (row.error.isDefined && row.entryCounts.sum > 0) {
        List(
          "shape: a row whose definition was rejected carries no operations, but this row " +
            s"carries ${row.entryCounts.sum}")
      } else {
        Nil
      }
    val states =
      row.matrixState.toList.flatMap(state => checkStateShape("matrixState", state)) :::
        row.merges.toList.zipWithIndex.flatMap { case (merge, index) =>
          merge.merged.asOption.toList.flatMap(state =>
            checkStateShape(s"merges[$index].merged", state))
        }
    variant ::: emptied ::: states
  }

  /** Checks a recorded state is square with one currency per row, so elements read by index. */
  private def checkStateShape(label: String, state: MatrixState): List[String] = {
    val square =
      if (state.rates.forall(row => row.size == state.rowCount)) {
        Nil
      } else {
        List(
          s"$label: rates is not square: ${state.rowCount} rows of sizes " +
            s"${state.rates.map(_.size).distinct.sorted.mkString(", ")}")
      }
    val named =
      if (state.currencies.size == state.rowCount) {
        Nil
      } else {
        List(
          s"$label: ${state.currencies.size} currencies but ${state.rowCount} rate rows")
      }
    square ::: named
  }

  /** Replays every operation of a row against the matrix its definition built. */
  private def measure(matrix: FxMatrix, row: FxRow): List[String] =
    row.matrixState.toList.flatMap(state => compareState("matrixState", matrix, state)) :::
      row.queries.toList.zipWithIndex.flatMap { case (entry, index) =>
        checkQuery(matrix, entry, index)
      } ::: row.conversions.toList.zipWithIndex.flatMap { case (entry, index) =>
        checkConversion(matrix, entry, index)
      } ::: row.multi.toList.zipWithIndex.flatMap { case (entry, index) =>
        checkMulti(matrix, entry, index)
      } ::: row.crosses.toList.zipWithIndex.flatMap { case (entry, index) =>
        checkCross(entry, index)
      } ::: row.merges.toList.zipWithIndex.flatMap { case (entry, index) =>
        checkMerge(matrix, entry, index)
      }

  //-------------------------------------------------------------------------
  // Rebuilding a matrix from its defining rates.
  //-------------------------------------------------------------------------

  /** Rebuilds the matrix a rate list defines, in fixture order, through bulk `FxMatrix.ofRates`. */
  private def rebuild(entries: Vector[RatePoint]): Either[Failure, FxMatrix] =
    entries
      .traverse(entry => CurrencyPair.parse(entry.pair).map(pair => (pair, entry.rate)))
      .flatMap(rates => FxMatrix.ofRates(rates))

  /**
   * Rebuilds one recorded `FxRate`, collapsing the validated constructor's accumulated failures
   * into the single failure this file's comparators take; the diagnostic keeps every message.
   */
  private def rateOf(point: RatePoint): Either[Failure, FxRate] =
    CurrencyPair
      .parse(point.pair)
      .flatMap(pair => FxRate.of(pair, point.rate).leftMap(failures => Failure.collapse(failures)))

  //-------------------------------------------------------------------------
  // The six groups.
  //-------------------------------------------------------------------------

  /** Compares a matrix against the recorded state, dimensions before elements. */
  private def compareState(label: String, matrix: FxMatrix, expected: MatrixState): List[String] = {
    val order =
      ParityHarness.assertExact(
        s"$label.currencies",
        matrix.currencies.map(_.code),
        expected.currencies)
    val dimensions =
      ParityHarness.assertExact(
        s"$label.rates.dimensions",
        (matrix.rates.rowCount, matrix.rates.columnCount),
        (expected.rowCount, expected.columnCount))
    if (dimensions.nonEmpty) {
      order ::: dimensions
    } else {
      val elements =
        for {
          row <- (0 until expected.rowCount).toList
          column <- (0 until expected.columnCount).toList
          message <- ParityHarness.assertParity(
            s"$label.rates[$row][$column] (${pairName(expected, row, column)})",
            matrix.rates.get(row, column),
            expected.rates(row)(column))
        } yield message
      order ::: elements
    }
  }

  /** Replays one rate query, its position in the list carried in the label. */
  private def checkQuery(matrix: FxMatrix, entry: Query, index: Int): List[String] = {
    val label = s"queries[$index] ${entry.base}/${entry.counter}"
    (Currency.of(entry.base), Currency.of(entry.counter)).tupled match {
      case Left(failure) => List(unbuildable(label, failure))
      case Right((base, counter)) =>
        compareRate(s"$label.fxRate", matrix.fxRate(base, counter), entry.fxRate)
    }
  }

  /**
   * Replays one single-amount conversion through both routes this module offers: `FxMatrix.convert`
   * and `CurrencyAmount.convertedTo(currency, provider)` are both measured against the one
   * recorded value, pinning the two entry points to each other as well as to the baseline.
   */
  private def checkConversion(matrix: FxMatrix, entry: Conversion, index: Int): List[String] = {
    val label = s"conversions[$index] ${entry.amount} ${entry.currency} -> ${entry.target}"
    val built =
      for {
        source <- Currency.of(entry.currency)
        target <- Currency.of(entry.target)
        amount <- CurrencyAmount.of(source, entry.amount)
      } yield (amount, target)
    built match {
      case Left(failure) => List(unbuildable(label, failure))
      case Right((amount, target)) =>
        compareAmount(s"$label.convert", matrix.convert(amount, target), entry.converted) :::
          compareAmount(
            s"$label.convertedTo",
            amount.convertedTo(target, matrix),
            entry.converted)
    }
  }

  /**
   * Replays one multi-currency conversion through both routes this module offers. The amounts are
   * assembled with `MultiCurrencyAmount.of`, which '''rejects''' duplicate currencies, because
   * every recorded entry names each currency once; the merging `total` would accept a duplicate.
   */
  private def checkMulti(matrix: FxMatrix, entry: MultiConversion, index: Int): List[String] = {
    val label =
      s"multi[$index] ${entry.amounts.map(amount => amount.currency).mkString("+")} -> ${entry.target}"
    val built =
      for {
        amounts <- entry.amounts.toList.traverse(amount =>
          Currency.of(amount.currency).flatMap(currency => CurrencyAmount.of(currency, amount.amount)))
        total <- MultiCurrencyAmount.of(amounts)
        target <- Currency.of(entry.target)
      } yield (total, target)
    built match {
      case Left(failure) => List(unbuildable(label, failure))
      case Right((total, target)) =>
        compareAmount(s"$label.convert", matrix.convert(total, target), entry.multiConverted) :::
          compareAmount(
            s"$label.convertedTo",
            total.convertedTo(target, matrix),
            entry.multiConverted)
    }
  }

  /**
   * Replays one cross rate. A cross is a property of the two rates alone and does not consult the
   * row's matrix, which is why the scenario carrying thirteen of them has an empty definition.
   */
  private def checkCross(entry: Cross, index: Int): List[String] = {
    val label = s"crosses[$index] ${entry.rate1.pair} x ${entry.rate2.pair}"
    (rateOf(entry.rate1), rateOf(entry.rate2)).tupled match {
      case Left(failure) => List(unbuildable(label, failure))
      case Right((first, second)) =>
        comparePoint(s"$label.crossRate", first.crossRate(second), entry.crossRate)
    }
  }

  /**
   * Replays one merge, the other matrix rebuilt the same insertion-ordered way so its currency
   * order is the order the merge walks it in; merging with an empty matrix is refused.
   */
  private def checkMerge(matrix: FxMatrix, entry: Merge, index: Int): List[String] = {
    val label = s"merges[$index] with ${entry.other.map(point => point.pair).mkString("+")}"
    rebuild(entry.other) match {
      case Left(failure) => List(unbuildable(s"$label.other", failure))
      case Right(other) =>
        val actual = matrix.merge(other)
        entry.merged match {
          case Expected.Failed(message) => refused(label, message, actual)
          case Expected.Value(state) =>
            ParityHarness.assertRight(label, actual)(merged =>
              compareState(s"$label.merged", merged, state))
        }
    }
  }

  //-------------------------------------------------------------------------
  // Comparators: one per kind of outcome, each handling the value and the refusal.
  //-------------------------------------------------------------------------

  /** Compares a numeric outcome against its expectation. */
  private def compareRate(
      label: String,
      actual: Either[Failure, Double],
      expected: Expected[Double]): List[String] =
    expected match {
      case Expected.Failed(message) => refused(label, message, actual)
      case Expected.Value(value) =>
        ParityHarness.assertRight(label, actual)(rate =>
          ParityHarness.assertParity(label, rate, value))
    }

  /** Compares an amount outcome against its expectation, currency exactly and amount at 1e-9. */
  private def compareAmount(
      label: String,
      actual: Either[Failure, CurrencyAmount],
      expected: Expected[AmountValue]): List[String] =
    expected match {
      case Expected.Failed(message) => refused(label, message, actual)
      case Expected.Value(value) =>
        ParityHarness.assertRight(label, actual) { amount =>
          ParityHarness.assertExact(s"$label.currency", amount.currency.code, value.currency) :::
            ParityHarness.assertParity(s"$label.amount", amount.amount, value.amount)
        }
    }

  /** Compares a rate outcome against its expectation, pair exactly and rate at 1e-9. */
  private def comparePoint(
      label: String,
      actual: Either[Failure, FxRate],
      expected: Expected[RatePoint]): List[String] =
    expected match {
      case Expected.Failed(message) => refused(label, message, actual)
      case Expected.Value(value) =>
        ParityHarness.assertRight(label, actual) { rate =>
          ParityHarness.assertExact(s"$label.pair", rate.pair.toString, value.pair) :::
            ParityHarness.assertParity(s"$label.rate", rate.rate, value.rate)
        }
    }

  /**
   * Checks that an operation the baseline records as refused is refused, in the error channel and
   * for the stated reason; its recorded message is quoted in the label and never compared.
   */
  private def refused(
      label: String,
      javaMessage: String,
      actual: Either[Failure, Any]): List[String] = {
    val described = s"$label (java refused with '$javaMessage')"
    ParityHarness.assertLeft(described, actual) ::: reasonOf(described, actual)
  }

  /** Compares the reason where the operation did refuse; `assertLeft` has reported the rest. */
  private def reasonOf(label: String, actual: Either[Failure, Any]): List[String] =
    actual match {
      case Left(failure) =>
        ParityHarness.assertExact(s"$label reason", failure.reason, RefusalReason)
      case Right(_) => Nil
    }

  /**
   * The message for an input the fixture states and this module cannot build: a disagreement
   * about what a currency code, a pair or a rate '''is''', which is never conflated with a
   * refusal the row expects.
   */
  private def unbuildable(label: String, failure: Failure): String =
    s"$label: the fixture states an input the port cannot build: ${failure.message}"

  /** Names the pair a matrix element holds the rate for, for a readable diagnostic. */
  private def pairName(state: MatrixState, row: Int, column: Int): String =
    (state.currencies.lift(row), state.currencies.lift(column)) match {
      case (Some(base), Some(counter)) => s"$base/$counter"
      case _ => s"row $row, column $column"
    }
}
