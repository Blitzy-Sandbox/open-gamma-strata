/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

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
 * Parity of the FX surface against the Java baseline.
 *
 * This is the measurement that pins the FX layer of the port: the matrix a list of rates builds,
 * the rate it answers for a pair, the conversion of a single amount and of a multi-currency
 * amount, the cross rate of two `FxRate` values, and the merge of two matrices. Every row rebuilds
 * a captured matrix from its insertion-ordered rate list and then replays the operations the
 * capture recorded against it.
 *
 * The gate that consumes it (AAP section 0.10.1, Gate 3, for the user's Rule 2) runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/fx.json`, requiring `failed == 0`. The `ParitySpec` suffix of
 * this class, its package, the fixture stem `fx` and the five keys of the report document are
 * therefore all part of that contract and none of them may drift.
 *
 * ===What is compared, and with which comparator===
 *
 * Every '''number''' - a rate, a converted amount, a cross rate, an element of a matrix - goes
 * through [[ParityHarness.assertParity]] and is at parity only when it is within 1e-9 of the
 * captured value '''both''' absolutely and relatively. Every '''identity''' - a currency code, a
 * pair name, and the currency '''order''' of a matrix - goes through
 * [[ParityHarness.assertExact]] and must be equal. No tolerance is ever applied to an identity,
 * and no identity is ever compared with a tolerance.
 *
 * That split is not cosmetic here. The README of the capture records that
 * `eurUsd.crossRate(usdGbp)` is `0.9142857142857143` and not the `0.9142857142857144` that
 * recomputing `(8/7) * (4/5)` by a different association produces
 * (`tools/parity-capture/README.md`, section 6). A last-digit difference of that kind is what the
 * relative bound is for, and it is also the whole argument for capturing a baseline instead of
 * writing one by hand.
 *
 * ===Non-finite values are part of the measurement===
 *
 * `FxMatrixTest` builds a connected matrix containing `JPY/CAD = 0.0`
 * (`modules/basics/src/test/java/com/opengamma/strata/basics/currency/FxMatrixTest.java:332`, and
 * again for the shifted variant at `:357`), so the reciprocal `CAD/JPY` is `+∞`. The fixture
 * carries that as the tagged string `"Infinity"`, which is the port's single non-finite policy,
 * and it is why `Codecs.implicits._` is imported into the companion '''before''' the decoders are
 * derived: without it a `Double` field would be read by circe's own decoder and the row would fail
 * to decode rather than be measured. Two equal non-finite values are at parity without arithmetic
 * because `assertParity` compares bit patterns first - `|∞ − ∞|` is `NaN` and would otherwise
 * defeat every bound - so nothing here special-cases them, and the zero-rate rows are measured
 * exactly like the rest. The second test below asserts that at least one such expectation is
 * actually present, so quietly losing those rows cannot leave a green measurement behind.
 *
 * ===An error is an expectation===
 *
 * The Java implementation refused six kinds of request that reach this fixture, and the port must
 * refuse them too - in its error channel, because each is a failure that depends on the data of
 * its arguments (AAP section 0.3.3). A rate query for a currency the matrix does not hold, a cross
 * of two rates with no unique common currency, and a merge with a matrix sharing no currency all
 * threw `IllegalArgumentException` in Java
 * (`FxMatrixTest.java:56-59,100-105,129-130,283-290`; `FxRateTest.java:207-231`), while the
 * builder's own refusal threw `IllegalStateException`; the port answers all of them with a `Left`,
 * so this spec asserts the '''channel''' uniformly and does not distinguish the two Java
 * exception types.
 *
 * It does assert the '''reason''', because for this surface it is unambiguous: all four
 * constructors that can produce a failure on any path this fixture exercises build a
 * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] -
 * `FxMatrix.noRateFound`, `FxMatrix.noCommonCurrency`, `FxMatrix.unplaceableRates`
 * (`FxMatrix.scala:1011,1025,1040`) and the no-unique-common-currency branch of
 * `FxRate.crossRate` (`FxRate.scala:219`). Asserting it turns "something failed" into "the
 * documented thing failed", which is what makes the `Left` worth having.
 *
 * What is '''not''' asserted is the captured message. AAP section 0.8.3 records the
 * exception-to-`Failure` mapping as a deliberate divergence, so pinning the port's wording to the
 * wording of the implementation it replaces would make this a test of prose. The Java message is
 * quoted in the diagnostic instead, where it explains what the row expected.
 *
 * Neither `intercept` nor `try`/`catch` appears in this file, and no `Either` is ever opened with
 * `.get`, `.value` or a partial function: an outcome is compared through the harness's
 * `assertLeft` and `assertRight`, which report once and carry on. That is not a stylistic
 * preference - abandoning a row on its first difference would discard the remaining measurements
 * of that row, and those are exactly what a report is for.
 *
 * ===Row schema===
 *
 * Section 6 of `tools/parity-capture/README.md` is the schema of record - strict JSON admits no
 * comments - and the model in the companion follows it field for field. Every row carries the same
 * nine keys, and an expectation is nested '''inside''' the input that produced it rather than held
 * in a parallel array, so an expectation can never be misaligned with its input:
 *
 * {{{
 * {"id":"single-pair-matrix","source":"FxMatrixTest.singlePairMatrix",
 *  "matrix":[{"pair":"GBP/USD","rate":1.6}],
 *  "matrixState":{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]},
 *  "queries":[{"base":"GBP","counter":"USD","fxRate":1.6},
 *             {"base":"USD","counter":"EUR","error":"IllegalArgumentException: No FX rate …"}],
 *  "conversions":[{"currency":"GBP","amount":100.0,"target":"USD",
 *                  "converted":{"currency":"USD","amount":160.0}}],
 *  "multi":[{"amounts":[{"currency":"GBP","amount":1600.0}],"target":"USD",
 *            "multiConverted":{"currency":"USD","amount":2560.0}}],
 *  "crosses":[{"rate1":{"pair":"EUR/USD","rate":1.1428571428571428},
 *              "rate2":{"pair":"USD/GBP","rate":0.8},
 *              "crossRate":{"pair":"EUR/GBP","rate":0.9142857142857143}}],
 *  "merges":[{"other":[{"pair":"EUR/CHF","rate":1.2}],
 *             "merged":{"currencies":["GBP","USD","EUR","CHF"],"rates":[[…],…]}}]}
 * }}}
 *
 * An entry carries either its expectation or an `error`, and the discriminator is per entry, so
 * one list may legitimately mix the two - `identity-and-single-rate-conversion` answers three
 * identity queries and refuses five genuine ones. The README also documents a variant that adds a
 * tenth top-level `error` key, sets `matrixState` to `null` and leaves the five lists empty, for a
 * definition the builder itself rejects. No committed row exercises it, and it is read and
 * measured all the same rather than left to break on the first one anybody captures.
 *
 * ===Why the matrix is rebuilt in bulk===
 *
 * A row's `matrix` is the defining list of rates '''in the order they were added''', and that
 * order is part of the value: it decides the matrix's currency order, and for a pair whose
 * currencies are both already present it decides which rate is the reference, the asymmetry
 * `FxMatrixTest.updatingRateIsNotSymmetric` (`:179`) and `rateCanBeUpdatedWithDirectionSwitched`
 * (`:243`) pin. The list is therefore never sorted, reordered, deduplicated or filtered here, and
 * a rate of `0.0` is placed exactly as the Java builder placed it
 * (`modules/basics/src/main/java/com/opengamma/strata/basics/currency/FxMatrixBuilder.java:85-140`).
 *
 * It is rebuilt through [[com.opengamma.strata.basics.currency.FxMatrix.ofRates]], which is
 * `FxMatrix.empty.withRates(…)` and folds the entries left to right in exactly that order. The
 * bulk form is '''required''' rather than merely convenient: two of the captured definitions offer
 * a rate for two currencies neither of which is held yet - `CHF/AUD` and `JPY/CAD`, marked
 * "Neither currency seen before" in the Java test (`FxMatrixTest.java:330,332`) - which the single
 * `withRate` refuses by design, directing such callers to `withRates`
 * (`FxMatrix.scala:255-259`). `withRates` holds the rate back and retries it when a later rate
 * brings its connection in, which is what the Java collector `entriesToFxMatrix()` the capture ran
 * did. Rebuilding one rate at a time would report a defect in the fixture that is not there.
 *
 * A definition that does not rebuild is a defect in the fixture or in the port rather than an
 * expectation, unless the row says otherwise with the tenth key, so it is lifted with
 * [[ParityHarness.raise]] and fails the row loudly instead of being absorbed into a comparison
 * that then passes.
 *
 * ===The matrix's own state is measured too===
 *
 * Each row carries `matrixState`, the built matrix's `{currencies, rates}` as Java held it. It is
 * compared in full - the currency vector exactly, so the insertion order is asserted rather than
 * assumed, and every element of the rate matrix at 1e-9. That is the strongest available check on
 * the rebuild, and it is captured baseline: leaving it unread would discard it while measuring the
 * queries that happen to be asked of it.
 *
 * ===No timing===
 *
 * Nothing here asserts a duration, and the report this publishes carries none.
 */
class FxParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import FxParitySpec._

  /*
   * The measurement is declared first, deliberately: the report it publishes is the artifact
   * Gate 3 collects, so it is written on every run of this suite rather than only on the runs
   * where some other case happens to pass first.
   */
  test("the FX matrix, rate, conversion, cross-rate and merge surfaces reproduce the Java baseline exactly") {
    ParityHarness
      .runFixture[FxRow](FixtureName, FixtureResource)(checkRow)
      .flatMap(report => ParityHarness.failIfAny(report))
      .as(succeed)
  }

  test("the fixture carries the population the FX baseline is required to measure") {
    ParityHarness.load[FxRow](FixtureResource).map { rows =>
      val counted = Counts.combineAll(rows.map(_.counts))
      withClue(s"fixture rows: ${rows.size}; measured entries: $counted: ") {
        // The floors are the contract of the capture, restated on the consuming side so that a
        // reduced fixture fails here instead of reporting a green measurement of less. They are
        // floors rather than equalities so that extending the coverage stays possible.
        rows.size should be >= MinimumRows
        rows.map(_.id).distinct should have size rows.size.toLong
        RequiredRowIds.filterNot(rows.map(_.id).contains) shouldBe empty
        // Every scenario states its provenance, which is what makes a failure attributable back
        // to the Java test or generated population it came from.
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
        // The tagged non-finite policy is load bearing for this fixture rather than theoretical,
        // so the value that motivates it is asserted to be present. Losing the zero-rate rows
        // would otherwise leave a green measurement of a smaller surface behind.
        withClue("no expectation carries a non-finite value, so the zero-rate rows are missing: ") {
          rows.exists(_.hasNonFiniteExpectation) shouldBe true
        }
        // Both row shapes the README describes are read, and a row is one or the other: a row
        // rejected by the builder carries no state and no operations, and any other row carries
        // its state. A half-populated row is a fixture this spec cannot measure as intended.
        rows.filter(row => row.error.isDefined == row.matrixState.isDefined).map(_.id) shouldBe empty
        rows.filter(row => row.error.isDefined && row.entryCounts.sum > 0).map(_.id) shouldBe empty
        // The refusals are also required per group, so a fixture that kept the fourteen errors
        // but moved them all into one list would not pass: each surface has its own documented
        // refusals - five genuine queries of an absent currency plus two empty-matrix and
        // single-pair queries, the five documented cross-rate failures, and the disjoint and
        // empty merges.
        counted.refusedQueries should be >= MinimumRefusedQueries
        counted.refusedCrosses should be >= MinimumRefusedCrosses
        counted.refusedMerges should be >= MinimumRefusedMerges
        // The captured currency order of every definition is the order its currencies first
        // appear in it - including the two definitions that offer a rate before the rate that
        // connects it, where the order is the order the held-back rate was finally placed in.
        // The measurement compares the port's order against the captured one; this states what
        // that captured order is, so the insertion-order contract is legible here and not only
        // in the baseline document.
        rows
          .filter(row =>
            row.matrixState.exists(state => state.currencies != row.firstAppearanceOrder))
          .map(_.id) shouldBe empty
      }
    }
  }
}

/**
 * The row model of `parity/fx-baseline.json`, and the comparisons applied to one row.
 *
 * The model follows section 6 of `tools/parity-capture/README.md` field for field. It is a
 * faithful reading of the captured document and nothing more: no field is interpreted on the way
 * in, no default is invented for an absent one, and the rate matrices are read as plain nested
 * vectors of doubles rather than through the port's own matrix codec - a parity measurement that
 * decoded its baseline with the code under measurement would be testing that code against itself.
 */
private[parity] object FxParitySpec {

  // Every `Double` in this fixture - a rate, an amount, an element of a matrix - is read through
  // the port's single non-finite policy, which renders `NaN`, `+∞` and `−∞` as tagged strings.
  // This import must precede the derivations below: it is what makes the captured `"Infinity"` of
  // the zero-rate rows decode to `Double.PositiveInfinity` instead of failing the row.
  import Codecs.implicits.doubleCodec

  //-------------------------------------------------------------------------
  // Contract constants. The first two are agreements with something outside this file - the
  // resource name with the capture script that writes it, the fixture stem with the gate that
  // reads `<parity.report.dir>/fx.json` - so neither may drift.
  //-------------------------------------------------------------------------

  /** The fixture stem, which is also the `fixture` field of the report and its file name. */
  val FixtureName: String = "fx"

  /** The classpath name of the captured baseline, relative to the test resource root. */
  val FixtureResource: String = "parity/fx-baseline.json"

  /** The key an entry carries in place of its expectation when the Java implementation refused. */
  val ErrorField: String = "error"

  /**
   * The reason every refusal of this surface carries.
   *
   * Each of the four constructors reachable from the operations this fixture exercises builds a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]]: `FxMatrix.noRateFound`
   * and `FxMatrix.noCommonCurrency` for a rate that is not in the matrix and a merge with no
   * currency in common, `FxMatrix.unplaceableRates` for a definition that can never be placed, and
   * the no-unique-common-currency branch of `FxRate.crossRate`. The reason is therefore asserted
   * rather than left unread, which is what distinguishes "the documented refusal" from "something
   * went wrong".
   */
  val RefusalReason: FailureReason = FailureReason.CURRENCY_CONVERSION

  /** The number of scenarios the committed baseline holds. */
  val MinimumRows: Int = 14

  /** The defining rate entries, over every scenario. */
  val MinimumMatrixEntries: Int = 60

  /** The rate queries, over every scenario - every ordered pair of the four random scenarios. */
  val MinimumQueries: Int = 304

  /** The single-amount conversions, over every scenario. */
  val MinimumConversions: Int = 42

  /** The multi-currency conversions: one and three amounts, into each of three currencies. */
  val MinimumMultiConversions: Int = 6

  /** The cross rates: the eight agreeing permutations and the five documented failures. */
  val MinimumCrosses: Int = 13

  /** The merges: disjoint, duplicate currencies, additional currencies and empty. */
  val MinimumMerges: Int = 4

  /** The refusals the baseline records, which the README states as fourteen. */
  val MinimumRefusals: Int = 14

  /** The refused rate queries: the empty matrix, the single pair, and five genuine `AUD` queries. */
  val MinimumRefusedQueries: Int = 7

  /** The refused crosses: two identity pairs, two same-currency pairs, one with nothing in common. */
  val MinimumRefusedCrosses: Int = 5

  /** The refused merges: the disjoint matrix and the empty one. */
  val MinimumRefusedMerges: Int = 2

  /**
   * Every scenario the baseline is required to carry.
   *
   * Each is a population the capture documents, and losing one would narrow the measured surface
   * without changing any count enough to notice: the triangulating matrix, the multi-currency
   * conversions, the two zero-rate matrices whose reciprocal is infinite, the nine-currency matrix
   * that forces the builder to resize past its minimum of eight, the two degenerate matrices, the
   * identity-and-single-rate conversions, the four merge cases, the four seeded random scenarios
   * and the `FxRate` cross rates.
   */
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
  // The row model.
  //-------------------------------------------------------------------------

  /**
   * One outcome the capture recorded: a value, or the refusal that stands in its place.
   *
   * The discriminator is per '''entry''' rather than per row, because one list legitimately mixes
   * the two - `identity-and-single-rate-conversion` answers three identity queries and refuses
   * five genuine ones. It is deliberately not an `Option`: a refusal is an expectation in its own
   * right, and collapsing it to an absence would both lose the captured message that explains the
   * row and make a dropped expectation indistinguishable from a refused one.
   *
   * @tparam A  the type of the value the operation produces when it succeeds
   */
  sealed trait Expected[+A] {

    /** The value, where the operation was expected to produce one. */
    def asOption: Option[A]
  }

  object Expected {

    /**
     * An operation the Java implementation answered.
     *
     * @param value  what it answered
     */
    final case class Value[+A](value: A) extends Expected[A] {
      override def asOption: Option[A] = Some(value)
    }

    /**
     * An operation the Java implementation refused.
     *
     * @param message  the message it refused with, `SimpleClassName: message`, which is carried
     *                 into the diagnostic and never asserted against the port's own wording
     */
    final case class Failed(message: String) extends Expected[Nothing] {
      override def asOption: Option[Nothing] = None
    }
  }

  /**
   * A rate for a currency pair, as `{"pair": "EUR/USD", "rate": 1.4}`.
   *
   * The same shape carries three different things in this fixture, so one model serves all three:
   * an entry of a matrix definition, an entry of the matrix a merge is given, and the cross rate
   * a pair of rates is expected to produce.
   *
   * @param pair  the pair, in the `BASE/COUNTER` form `CurrencyPair.toString` produces
   * @param rate  the rate of one unit of the base currency in the counter currency
   */
  final case class RatePoint(pair: String, rate: Double)

  /**
   * An amount in a currency, as `{"currency": "USD", "amount": 2560.0}`.
   *
   * @param currency  the currency code
   * @param amount  the amount
   */
  final case class AmountValue(currency: String, amount: Double)

  /**
   * The state of a built matrix: its currencies in matrix order, and its rates row by row.
   *
   * The currency at each position is the currency of that row and column, so the vector's order
   * is the matrix's insertion order and comparing it exactly is what asserts that order.
   *
   * @param currencies  the currency codes, in matrix order
   * @param rates  the rates, as a vector of rows
   */
  final case class MatrixState(currencies: Vector[String], rates: Vector[Vector[Double]]) {

    /** The number of rows the captured matrix holds. */
    def rowCount: Int = rates.size

    /** The number of columns the captured matrix holds, zero for a matrix with no rows. */
    def columnCount: Int = rates.headOption.fold(0)(_.size)

    /** Every rate of the captured matrix, for the non-finite population check. */
    def allRates: Vector[Double] = rates.flatten
  }

  /**
   * A rate query and its outcome, as `{"base": "GBP", "counter": "USD", "fxRate": 1.6}`.
   *
   * @param base  the base currency code
   * @param counter  the counter currency code
   * @param fxRate  the rate the matrix answered, or the refusal
   */
  final case class Query(base: String, counter: String, fxRate: Expected[Double])

  /**
   * A single-amount conversion and its outcome.
   *
   * @param currency  the currency code of the amount to convert
   * @param amount  the amount to convert
   * @param target  the currency code to convert into
   * @param converted  the amount that resulted, or the refusal
   */
  final case class Conversion(
      currency: String,
      amount: Double,
      target: String,
      converted: Expected[AmountValue])

  /**
   * A multi-currency conversion and its outcome.
   *
   * @param amounts  the amounts to convert, one per currency
   * @param target  the currency code to convert into
   * @param multiConverted  the total that resulted, or the refusal
   */
  final case class MultiConversion(
      amounts: Vector[AmountValue],
      target: String,
      multiConverted: Expected[AmountValue])

  /**
   * A cross of two rates and its outcome.
   *
   * @param rate1  the first rate
   * @param rate2  the second rate
   * @param crossRate  the rate the cross produced, or the refusal
   */
  final case class Cross(rate1: RatePoint, rate2: RatePoint, crossRate: Expected[RatePoint])

  /**
   * A merge with another matrix and its outcome.
   *
   * @param other  the defining rates of the matrix being merged in, which may be empty - merging
   *               with an empty matrix is refused, deliberately, because a merge works through a
   *               currency common to both and an empty matrix has none
   * @param merged  the state of the matrix that resulted, or the refusal
   */
  final case class Merge(other: Vector[RatePoint], merged: Expected[MatrixState])

  /**
   * One scenario: a matrix definition, the state it built, and the operations replayed against it.
   *
   * @param id  the scenario's stable handle, which is the name the report prints
   * @param source  the Java test method or generated population the row came from
   * @param matrix  the defining rates, in the order they were added
   * @param matrixState  the state the definition built, absent only for a definition the builder
   *                     itself rejected
   * @param queries  the rate queries
   * @param conversions  the single-amount conversions
   * @param multi  the multi-currency conversions
   * @param crosses  the cross rates
   * @param merges  the merges
   * @param error  the refusal of the definition itself, present only for a definition the builder
   *               rejected, in which case the five lists above are empty
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

    /** The number of entries this row carries in each of its five operation lists. */
    def entryCounts: Vector[Int] =
      Vector(queries.size, conversions.size, multi.size, crosses.size, merges.size)

    /** What this row contributes to the measured population. */
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

    /** Every number this row expects, which is where the non-finite population is looked for. */
    def expectedDoubles: Vector[Double] =
      matrixState.toVector.flatMap(_.allRates) ++
        queries.flatMap(_.fxRate.asOption) ++
        conversions.flatMap(_.converted.asOption.map(_.amount)) ++
        multi.flatMap(_.multiConverted.asOption.map(_.amount)) ++
        crosses.flatMap(_.crossRate.asOption.map(_.rate)) ++
        merges.flatMap(_.merged.asOption.toVector.flatMap(_.allRates))

    /** Whether this row expects a value JSON cannot express, which the tagged policy carries. */
    def hasNonFiniteExpectation: Boolean =
      expectedDoubles.exists(value => !java.lang.Double.isFinite(value))

    /**
     * The currencies of this definition, in the order they first appear in it.
     *
     * This reads the captured `BASE/COUNTER` names directly rather than resolving them, because
     * it describes the '''document''' rather than the port: it is what the captured currency order
     * is compared against, so deriving it through the code under measurement would weaken the
     * comparison.
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
   * What the fixture holds, as counted by the population test.
   *
   * @param matrixEntries  defining rate entries
   * @param queries  rate queries
   * @param conversions  single-amount conversions
   * @param multi  multi-currency conversions
   * @param crosses  cross rates
   * @param merges  merges
   * @param refusedQueries  rate queries recorded as refused
   * @param refusedConversions  conversions recorded as refused
   * @param refusedMulti  multi-currency conversions recorded as refused
   * @param refusedCrosses  crosses recorded as refused
   * @param refusedMerges  merges recorded as refused
   */
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

    /** The number of operations replayed, over every list. */
    def entries: Int = queries + conversions + multi + crosses + merges

    /** The number of operations recorded as refused, over every list. */
    def refusals: Int =
      refusedQueries + refusedConversions + refusedMulti + refusedCrosses + refusedMerges

    /** Adds another row's contribution to this one. */
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

    /** The contribution of no rows at all. */
    val empty: Counts = Counts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    /**
     * Totals the contributions of every row.
     *
     * @param values  the per-row contributions
     * @return their total
     */
    def combineAll(values: Vector[Counts]): Counts =
      values.foldLeft(empty)((total, next) => total.combine(next))
  }

  //-------------------------------------------------------------------------
  // Decoders. Every product is derived; the four entry decoders are written by hand because an
  // entry's expectation is one of two sibling keys rather than a field of its own.
  //-------------------------------------------------------------------------

  /**
   * Reads an entry's outcome from whichever of the two keys it carries.
   *
   * The `error` key is tried first and decides the answer when it holds a string, which is what
   * makes the refusal the expectation rather than a missing value. A key that is absent - or
   * present and `null`, the form the other fixtures of this module use - leaves the named
   * expectation to be read, and an entry carrying neither fails to decode, which the harness
   * reports as a fixture that no longer agrees with this spec.
   *
   * @param field  the name of the key carrying the value
   * @tparam A  the type of the value
   * @return the decoder for that entry's outcome
   */
  private def expectedDecoder[A: Decoder](field: String): Decoder[Expected[A]] =
    Decoder.instance { cursor =>
      cursor.get[Option[String]](ErrorField).flatMap {
        case Some(message) => Right(Expected.Failed(message))
        case None => cursor.get[A](field).map(value => Expected.Value(value))
      }
    }

  implicit val ratePointDecoder: Decoder[RatePoint] = deriveDecoder[RatePoint]

  implicit val amountValueDecoder: Decoder[AmountValue] = deriveDecoder[AmountValue]

  implicit val matrixStateDecoder: Decoder[MatrixState] = deriveDecoder[MatrixState]

  implicit val queryDecoder: Decoder[Query] =
    Decoder.instance { cursor =>
      for {
        base <- cursor.get[String]("base")
        counter <- cursor.get[String]("counter")
        fxRate <- expectedDecoder[Double]("fxRate").apply(cursor)
      } yield Query(base, counter, fxRate)
    }

  implicit val conversionDecoder: Decoder[Conversion] =
    Decoder.instance { cursor =>
      for {
        currency <- cursor.get[String]("currency")
        amount <- cursor.get[Double]("amount")
        target <- cursor.get[String]("target")
        converted <- expectedDecoder[AmountValue]("converted").apply(cursor)
      } yield Conversion(currency, amount, target, converted)
    }

  implicit val multiConversionDecoder: Decoder[MultiConversion] =
    Decoder.instance { cursor =>
      for {
        amounts <- cursor.get[Vector[AmountValue]]("amounts")
        target <- cursor.get[String]("target")
        converted <- expectedDecoder[AmountValue]("multiConverted").apply(cursor)
      } yield MultiConversion(amounts, target, converted)
    }

  implicit val crossDecoder: Decoder[Cross] =
    Decoder.instance { cursor =>
      for {
        rate1 <- cursor.get[RatePoint]("rate1")
        rate2 <- cursor.get[RatePoint]("rate2")
        crossRate <- expectedDecoder[RatePoint]("crossRate").apply(cursor)
      } yield Cross(rate1, rate2, crossRate)
    }

  implicit val mergeDecoder: Decoder[Merge] =
    Decoder.instance { cursor =>
      for {
        other <- cursor.get[Vector[RatePoint]]("other")
        merged <- expectedDecoder[MatrixState]("merged").apply(cursor)
      } yield Merge(other, merged)
    }

  implicit val fxRowDecoder: Decoder[FxRow] = deriveDecoder[FxRow]

  //-------------------------------------------------------------------------
  // Measuring one row.
  //-------------------------------------------------------------------------

  /**
   * Measures one scenario, answering with everything that differed from the Java baseline.
   *
   * The three steps are ordered so that each is only attempted when the one before it holds. A row
   * whose shape this spec cannot measure is reported as such and nothing is computed from it; a row
   * whose definition the builder was recorded as rejecting is measured by asserting that refusal;
   * any other row is rebuilt - loudly, through [[ParityHarness.raise]], because a definition that
   * fails to rebuild is a defect rather than an expectation - and then every operation it carries
   * is replayed against the result.
   *
   * Nothing short-circuits after that point: all six groups are measured and their messages
   * accumulated, because the remaining measurements of a row are exactly what a report is for.
   *
   * @param row  the scenario
   * @return every discrepancy found, labelled by group and index
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
   * Checks that a row is shaped as this spec measures it.
   *
   * These are invariants of the row model rather than of the document, so the harness does not
   * check them and a row that fails one is reported as a fixture that no longer agrees with this
   * spec rather than as a discrepancy of the port. Each is answered as a message rather than
   * raised, so a spec run over a changed fixture reports every row that changed instead of
   * stopping at the first.
   *
   * @param row  the scenario
   * @return what is wrong with the row's shape, or nothing
   */
  private def checkShape(row: FxRow): List[String] = {
    // A row states either the matrix its definition built or the error that prevented it, never
    // both and never neither. The second shape is the tenth-key variant of the README; no
    // committed row uses it, and it is measured rather than rejected if one ever does.
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
    // whose expectations cannot all have been captured from the same state.
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

  /**
   * Checks that a captured matrix state is square and names one currency per row.
   *
   * A rate matrix of `n` currencies is `n` by `n`, so these two are the shape every comparison of
   * one depends on, and checking them here is what lets [[compareState]] read elements by index.
   *
   * @param label  the name of the state being checked
   * @param state  the captured state
   * @return what is wrong with its shape, or nothing
   */
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

  /**
   * Replays every operation of a row against the matrix its definition built.
   *
   * @param matrix  the rebuilt matrix
   * @param row  the scenario
   * @return every discrepancy, in the order the groups are declared in the document
   */
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

  /**
   * Rebuilds the matrix a list of captured rates defines.
   *
   * The entries are placed in the order the fixture holds them and are never sorted, reordered,
   * deduplicated or filtered: that order decides the currency order of the result and, for a pair
   * whose currencies are both already held, which of the two is the reference of the update - the
   * asymmetry `FxMatrixTest.updatingRateIsNotSymmetric` (`:179`) pins. A rate of `0.0` is placed
   * exactly as the Java builder placed it, which is what puts an infinite reciprocal into the two
   * zero-rate scenarios.
   *
   * [[com.opengamma.strata.basics.currency.FxMatrix.ofRates]] is the bulk form, and it is required
   * rather than merely convenient: two captured definitions offer a rate whose two currencies are
   * both still absent, which the single-rate `withRate` refuses by design while `withRates` holds
   * it back and retries it once a later rate connects it - the behaviour of the Java collector the
   * capture ran. It is still a strict left-to-right fold over these entries.
   *
   * @param entries  the defining rates, in the order they were added
   * @return the matrix they define, or the failure describing why they define none
   */
  private def rebuild(entries: Vector[RatePoint]): Either[Failure, FxMatrix] =
    entries
      .traverse(entry => CurrencyPair.parse(entry.pair).map(pair => (pair, entry.rate)))
      .flatMap(rates => FxMatrix.ofRates(rates))

  /**
   * Rebuilds one captured `FxRate`.
   *
   * The accumulated failures of the validated constructor are collapsed into the single failure
   * this file's comparators take, which loses nothing: the diagnostic keeps every message.
   *
   * @param point  the captured pair and rate
   * @return the rate, or the failure describing why it is not one
   */
  private def rateOf(point: RatePoint): Either[Failure, FxRate] =
    CurrencyPair
      .parse(point.pair)
      .flatMap(pair => FxRate.of(pair, point.rate).leftMap(failures => Failure.collapse(failures)))

  //-------------------------------------------------------------------------
  // The six groups.
  //-------------------------------------------------------------------------

  /**
   * Compares a matrix against the state Java built from the same definition.
   *
   * The currency vector is compared '''exactly''', which is what asserts the insertion order the
   * port is required to preserve, and every element of the rate matrix is compared at 1e-9. The
   * dimensions are compared before any element is read, so a matrix of the wrong size is reported
   * rather than indexed.
   *
   * @param label  the name of the state being compared
   * @param matrix  the matrix the port built
   * @param expected  the state Java built
   * @return every discrepancy
   */
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

  /**
   * Replays one rate query.
   *
   * @param matrix  the rebuilt matrix
   * @param entry  the query and its outcome
   * @param index  the query's position in its list, which the label carries
   * @return every discrepancy
   */
  private def checkQuery(matrix: FxMatrix, entry: Query, index: Int): List[String] = {
    val label = s"queries[$index] ${entry.base}/${entry.counter}"
    (Currency.of(entry.base), Currency.of(entry.counter)).tupled match {
      case Left(failure) => List(unbuildable(label, failure))
      case Right((base, counter)) =>
        compareRate(s"$label.fxRate", matrix.fxRate(base, counter), entry.fxRate)
    }
  }

  /**
   * Replays one single-amount conversion, through both routes the port offers.
   *
   * `FxMatrix.convert` is the method the capture recorded, and
   * `CurrencyAmount.convertedTo(currency, provider)` is the `FxConvertible` route to the same
   * number. Both are measured against the one captured value, so the two documented entry points
   * are pinned to each other as well as to Java, at the cost of one extra comparison.
   *
   * @param matrix  the rebuilt matrix
   * @param entry  the conversion and its outcome
   * @param index  the conversion's position in its list, which the label carries
   * @return every discrepancy
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
   * Replays one multi-currency conversion, through both routes the port offers.
   *
   * The amounts are assembled with `MultiCurrencyAmount.of`, the factory that '''rejects'''
   * duplicate currencies, because every captured entry names each currency once - one amount in
   * `GBP`, and three in `EUR`, `GBP` and `USD`. Using the merging `total` instead would silently
   * accept a duplicated currency that the capture never recorded, and reporting such an entry as
   * an input this spec cannot build is the honest outcome.
   *
   * @param matrix  the rebuilt matrix
   * @param entry  the conversion and its outcome
   * @param index  the conversion's position in its list, which the label carries
   * @return every discrepancy
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
   * Replays one cross rate.
   *
   * A cross is a property of the two rates alone and does not consult the row's matrix, which is
   * why the scenario that carries thirteen of them has an empty definition.
   *
   * @param entry  the two rates and the outcome of crossing them
   * @param index  the cross's position in its list, which the label carries
   * @return every discrepancy
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
   * Replays one merge.
   *
   * The other matrix is rebuilt the same insertion-ordered way, so its own currency order is the
   * order the merge walks it in. An empty other matrix rebuilds to the empty matrix, and merging
   * with it is refused - the fixture records that, deliberately, because a merge works through a
   * currency common to both matrices and an empty one has none.
   *
   * @param matrix  the rebuilt matrix being merged into
   * @param entry  the other matrix's definition and the outcome of the merge
   * @param index  the merge's position in its list, which the label carries
   * @return every discrepancy
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

  /**
   * Compares a numeric outcome against its expectation.
   *
   * @param label  the name of the operation
   * @param actual  what the port answered
   * @param expected  what Java answered
   * @return every discrepancy
   */
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

  /**
   * Compares an amount outcome against its expectation, currency exactly and amount at 1e-9.
   *
   * @param label  the name of the operation
   * @param actual  what the port answered
   * @param expected  what Java answered
   * @return every discrepancy
   */
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

  /**
   * Compares a rate outcome against its expectation, pair exactly and rate at 1e-9.
   *
   * @param label  the name of the operation
   * @param actual  what the port answered
   * @param expected  what Java answered
   * @return every discrepancy
   */
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
   * Checks that an operation Java refused is refused, in the error channel, for the stated reason.
   *
   * The captured message is quoted in the label and never compared: the port reports through its
   * own sealed failure model, and AAP section 0.8.3 records that mapping as a deliberate
   * divergence, so asserting the wording would make this a test of prose. The '''reason''' is
   * compared, because for this surface it is unambiguous - see [[RefusalReason]].
   *
   * @param label  the name of the operation
   * @param javaMessage  the message the Java implementation refused with
   * @param actual  what the port answered
   * @return every discrepancy
   */
  private def refused(
      label: String,
      javaMessage: String,
      actual: Either[Failure, Any]): List[String] = {
    val described = s"$label (java refused with '$javaMessage')"
    ParityHarness.assertLeft(described, actual) ::: reasonOf(described, actual)
  }

  /**
   * Compares the reason of a refusal, where the operation did refuse.
   *
   * A refusal that did not happen at all has already been reported by `assertLeft`, so this adds
   * nothing for it rather than reporting the same thing twice.
   *
   * @param label  the name of the operation
   * @param actual  what the port answered
   * @return the discrepancy, or nothing
   */
  private def reasonOf(label: String, actual: Either[Failure, Any]): List[String] =
    actual match {
      case Left(failure) =>
        ParityHarness.assertExact(s"$label reason", failure.reason, RefusalReason)
      case Right(_) => Nil
    }

  /**
   * The message for an input the fixture states and the port cannot build.
   *
   * This is a disagreement between the fixture and the port about what a currency code, a pair or
   * a rate '''is''', which is not what any expectation of the row is about, so it is worded as
   * such and never conflated with a refusal the row expects.
   *
   * @param label  the name of the operation whose input could not be built
   * @param failure  why it could not be built
   * @return the message
   */
  private def unbuildable(label: String, failure: Failure): String =
    s"$label: the fixture states an input the port cannot build: ${failure.message}"

  /**
   * Names the pair a matrix element holds the rate for, for a readable diagnostic.
   *
   * @param state  the captured state, whose currency vector names the rows and columns
   * @param row  the element's row
   * @param column  the element's column
   * @return the pair name, or the indices where the state names no currency for them
   */
  private def pairName(state: MatrixState, row: Int, column: Int): String =
    (state.currencies.lift(row), state.currencies.lift(column)) match {
      case (Some(base), Some(counter)) => s"$base/$counter"
      case _ => s"row $row, column $column"
    }
}
