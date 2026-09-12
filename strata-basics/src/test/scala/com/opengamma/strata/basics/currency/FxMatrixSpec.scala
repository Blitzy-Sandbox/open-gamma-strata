/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.util.matching.Regex

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor4
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import com.opengamma.strata.basics.Arbitraries.genEdgeFxMatrix
import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.CHF
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.JPY
import com.opengamma.strata.basics.currency.Currency.NZD
import com.opengamma.strata.basics.currency.Currency.SEK
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.basics.testkit.CurrencyAmountMatchers._
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FxMatrix]], ported from the Java `FxMatrixTest`.
 *
 * All thirty-two methods of the original are here, one test each, under the name the Java method
 * had - including the misspelling of `ratedCanBeUpdatedAndAddedViaBuilder`, which is kept so that
 * a Java test method and a test of this suite stay in one-to-one correspondence in the migration
 * manifest. The values, the fixtures and the exact-versus-tolerant split of every assertion are
 * the original's.
 *
 * The suite holds those thirty-two ported methods, the case of this port's own described next,
 * and the eight further cases of the closing section, each of which carries a descriptive name
 * because no Java method corresponds to it. The first of them covers the structurally
 * valid '''edge''' states of a matrix - an off-diagonal pair that are not reciprocals of each
 * other, a `NaN`, either infinity and a negative zero - which [[FxMatrix.fromMatrix]] admits
 * because it checks the shape and the diagonal and nothing else, and which every fixture of the
 * original and of the tests below misses: they are reciprocal and finite but for the one zero
 * rate. Its scaladoc, at the test itself, says what each of those states costs if it breaks.
 *
 * ===The builder and the two collectors have no target===
 *
 * This is the suite with the widest gap between the original API and the ported one. The original
 * built every fixture through a mutable builder - `FxMatrix.builder().addRate(…).build()`, and
 * `matrix.toBuilder().addRate(…).build()` to extend an existing matrix - and collected streams
 * into a matrix through the two `Collector` factories `entriesToFxMatrix()` and
 * `pairsToFxMatrix()`. None of the four is ported: the builder's mutating calls are replaced by
 * methods of [[FxMatrix]] that answer with a new matrix, and a collection of rates is placed by
 * the factories of its companion. Eleven of the thirty-two ported tests are written against that
 * machinery, and each of them keeps its name, states the substitution in its own scaladoc, and
 * asserts the same facts about the same matrix:
 *
 *   - a chain of `addRate` calls followed by `build()` becomes [[placedOneByOne]], a fold of
 *     [[FxMatrix.withRate]] over the same rates in the same order, threading the outcome;
 *   - `addRates(map)` and the two collectors become [[FxMatrix.ofRates]] over the same entries in
 *     the same order;
 *   - `toBuilder().addRate(r).build()` becomes `matrix.withRate(r)` on the matrix itself.
 *
 * The two forms are not interchangeable in one respect, and the pair of tests
 * `cannotAddEntryWithNoCommonCurrencyAndBuild` and
 * `canAddEntryWithNoCommonCurrencyIfSuppliedBySubsequentEntries` is what pins it. A rate for two
 * currencies a matrix holds neither of cannot be placed when it is offered, and the original held
 * such a rate in the builder until a later rate connected it, reporting from `build()` only those
 * it could never place. That tolerance belongs to the collection factories here, where a later
 * rate can still arrive; a single rate offered through [[FxMatrix.withRate]] has no later offer
 * to wait for and is a failure at that point. Both forms are asserted for the disjoint case, so
 * the difference is stated rather than implied.
 *
 * The matrix of the original also over-allocated its backing array to a power of two and resized
 * it when a ninth currency arrived, which `addMultipleRatesSingle` existed to exercise. The ported
 * matrix allocates exactly what it holds, so the resize has no counterpart; that test therefore
 * asserts only the rates, which are what the original asserted too.
 *
 * ===Three properties of a matrix that are pinned only here===
 *
 * '''The order of the currencies is part of the value.''' It is the order [[FxMatrix.currencies]]
 * holds them in, the order [[FxMatrix.toString]] writes them in, the order the JSON form carries,
 * and it is read by equality. So `addMultipleRates` feeds an ordered sequence rather than an
 * unordered map - the original used a `LinkedHashMap` for exactly that reason - and `equalsGood`
 * asserts that two matrices holding the same rates for the same currencies in a different order
 * are '''not''' equal.
 *
 * '''Updating a rate is deliberately not symmetric.''' When a matrix already holds both
 * currencies of an offered rate, the first is the reference and the second is restated against it,
 * so `withRate(USD, GBP, 1 / 1.6)` does not produce the matrix `withRate(GBP, USD, 1.6)` produces
 * whenever a third currency is present. `updatingRateIsNotSymmetric` carries the original's
 * expectations operand for operand, and they are behaviour rather than an accident - see that
 * test.
 *
 * '''A rate of zero is legal and its reciprocal is infinite.''' The two stream tests of the
 * original both include a `JPY/CAD` rate of `0.0`, and that rate is the point of them: the matrix
 * must still build and still answer every other pair. Those tests keep the zero rate here, assert
 * the infinite reciprocal explicitly so that the behaviour is pinned rather than incidental, and
 * `testSerializeDeserialize` round-trips such a matrix through JSON, which is the case the
 * tagged-double representation of this port exists for.
 *
 * ===Failures are values===
 *
 * The original asserted `IllegalArgumentException` four times and `IllegalStateException` twice.
 * Nothing in the ported type throws for any of the six: each is a `Left` asserted through the
 * matchers of [[com.opengamma.strata.collect.testkit.ResultMatchers]], with the reason compared
 * as a member of the closed family of reasons and the message pinned with `Regex.quote` so that
 * the comparison is the literal message rather than a pattern that happens to match it. All six
 * are [[com.opengamma.strata.collect.result.FailureReason.CURRENCY_CONVERSION]] - the absent rate,
 * the disjoint merge and the rate that could never be placed alike - and the wording of each is
 * the wording the original threw with.
 *
 * ===What is asserted elsewhere===
 *
 * The typeclass law suites, the compile-time sweep over the construction surface of every
 * validated type, the sweep that exercises every failable method of the module with a failing
 * input and the property-based round trip of every codec each live in their own spec at the root
 * of the test tree; the fixture-driven numerical parity of the matrix against the captured Java
 * baseline, including its zero-rate matrix and its disjoint merges, belongs to
 * `parity.FxParitySpec`.
 *
 * ===Eight tests state properties the Java suite left unstated===
 *
 * The thirty-two ported tests assert the in-line expectations of the Java test and nothing beyond
 * them. Eight further tests follow them, in their own section below, under names of their own: the
 * Java suite has no method they correspond to and the migration manifest maps none to them. Each
 * pins a property of the ported implementation that the Java suite exercised only incidentally -
 * that a collection of rates connecting to nothing is refused with a bounded listing of them,
 * how the rates held back are stored and when they are retried, that a multi-currency conversion
 * stops at the first rate it cannot find and totals in currency order, that the two collection
 * factories agree and that the one taking rate values traverses its argument once, and that the
 * ordered set of currencies is held rather than rebuilt. They are stated here because they are
 * properties of this type's observable behaviour; see the section comment above them.
 *
 * @see [[FxMatrix]] for the type under test, whose scaladoc documents the placement of a rate
 * @see [[FxRate]] for the single rate, which [[FxMatrix.of]] also accepts a collection of
 */
final class FxMatrixSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  /**
   * The tolerance the original compared a derived rate against, and its value.
   *
   * The original held this as an offset object of the assertion library it used; there is no such
   * object here, and a tolerance is an ordinary number passed to the matcher or to the tolerance
   * operator of the test framework. The value is the original's `TOLERANCE`, unchanged, and it is
   * applied at exactly the rates the original applied it to - a rate read straight out of the
   * matrix is asserted exactly, a rate the matrix derived from two others within this.
   */
  private val TOL: Double = 1e-6

  /**
   * A value of a type unrelated to a matrix, for the equality assertion that needs one.
   *
   * Held at the type `Any` and named as the original named it, so that the assertion reads as a
   * comparison against a foreign value rather than as a comparison the compiler could reject.
   */
  private val ANOTHER_TYPE: Any = ""

  /**
   * The tolerance column of a table of expected rates that asserts a rate exactly.
   *
   * Named rather than written as a bare zero so that a row of such a table says which of the two
   * comparisons of the original it carries, which is the distinction those tables exist to keep.
   */
  private val Exactly: Double = 0d

  //-------------------------------------------------------------------------
  test("emptyMatrixCanHandleTrivialRate") {
    val matrix = FxMatrix.empty
    matrix.getCurrencies shouldBe empty
    matrix.currencies shouldBe Vector.empty
    // a currency against itself converts at one whether or not the matrix mentions it, which is
    // answered before the matrix is consulted and is what lets the empty matrix answer at all
    matrix.fxRate(USD, USD) shouldBe Right(1d)
    matrix.toString shouldBe "FxMatrix[ : ]"
  }

  test("emptyMatrixCannotDoConversion") {
    // the original reached the empty matrix through `builder().build()`; the builder has no target
    // and `FxMatrix.empty` is that same value, being the matrix every fold over rates starts from
    val matrix = FxMatrix.empty
    matrix.getCurrencies shouldBe empty
    val rate: FailureOr[Double] = matrix.fxRate(USD, EUR)
    rate should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    rate should haveFailureMessageMatching(
      Regex.quote("No FX rate found for USD/EUR, matrix only contains rates for []"))
    // the conversion of an amount carries the failure of the lookup it performs
    matrix.convert(amountOf(USD, 100d), EUR) should beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  test("singleRateMatrixByOfCurrencyPairFactory") {
    val matrix = FxMatrix.of(CurrencyPair.of(GBP, USD), 1.6d)
    matrix.getCurrencies shouldBe Set(GBP, USD)
    matrix.currencies shouldBe Vector(GBP, USD)
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 0.625d, Exactly),
        (GBP, GBP, 1d, Exactly),
        (USD, USD, 1d, Exactly)))
    matrix.toString shouldBe "FxMatrix[GBP, USD : [1.0, 1.6],[0.625, 1.0]]"
  }

  test("singleRateMatrixByOfCurrenciesFactory") {
    val matrix = FxMatrix.of(GBP, USD, 1.6d)
    matrix.getCurrencies shouldBe Set(GBP, USD)
    matrix.currencies shouldBe Vector(GBP, USD)
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 0.625d, Exactly),
        (GBP, GBP, 1d, Exactly),
        (USD, USD, 1d, Exactly)))
    // the two factories place a rate identically, the pair form being defined in terms of this one
    matrix shouldBe FxMatrix.of(CurrencyPair.of(GBP, USD), 1.6d)
  }

  /**
   * The builder of the original has no target: this places the same single rate into the empty
   * matrix through [[FxMatrix.withRate]], which is what the builder's `addRate` became, and
   * asserts the facts the original asserted about the result.
   */
  test("singleRateMatrixByBuilder") {
    val matrix = matrixOf(rateFor(GBP, USD, 1.6d))
    matrix.getCurrencies shouldBe Set(GBP, USD)
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 0.625d, Exactly)))
    // placing the only rate of a matrix reaches the same value as the single-rate factory
    matrix shouldBe FxMatrix.of(GBP, USD, 1.6d)
  }

  /**
   * The original added the rate to a builder through its currency-pair overload; the ported
   * currency-pair overload of [[FxMatrix.withRate]] is that same operation on a matrix.
   */
  test("canAddRateUsingCurrencyPair") {
    val matrix = unwrap(FxMatrix.empty.withRate(CurrencyPair.of(GBP, USD), 1.6d))
    matrix.getCurrencies shouldBe Set(GBP, USD)
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 0.625d, Exactly)))
  }

  /**
   * The builder of the original has no target; the matrix is placed rate by rate instead. The
   * absent rate is a `Left` where the original threw.
   */
  test("singleRateMatrixCannotDoConversionForUnknownCurrency") {
    val matrix = matrixOf(rateFor(GBP, USD, 1.6d))
    matrix.getCurrencies shouldBe Set(GBP, USD)
    val rate: FailureOr[Double] = matrix.fxRate(USD, EUR)
    rate should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    rate should haveFailureMessageMatching(
      Regex.quote("No FX rate found for USD/EUR, matrix only contains rates for [GBP, USD]"))
  }

  /**
   * The builder of the original has no target; the three rates are placed one at a time, in the
   * order the original added them, which is what fixes the positions of the four currencies and
   * therefore the derived rates asserted here.
   */
  test("matrixCalculatesCrossRates") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d))

    matrix.getCurrencies shouldBe Set(GBP, USD, EUR, CHF)
    matrix.currencies shouldBe Vector(GBP, USD, EUR, CHF)

    // a rate placed directly is exact; a rate the matrix derived from two others is asserted
    // within the tolerance, exactly as the original split them
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 1d / 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        (USD, EUR, 1d / 1.4d, Exactly),
        (EUR, GBP, 1.4d / 1.6d, TOL),
        (GBP, EUR, 1.6d / 1.4d, TOL),
        (EUR, CHF, 1.2d, Exactly)))
  }

  //-------------------------------------------------------------------------
  /**
   * The original offered a disconnected rate to a builder, which held it, and then asserted that
   * `build()` rejected it. There is no build step here, so the rejection is asserted in both of
   * the places it can now arise, which is what makes the two forms of placement distinct rather
   * than interchangeable:
   *
   *   - [[FxMatrix.ofRates]] holds the rate back while further rates may still connect it and
   *     reports it once none did, which is the state `build()` reported, with its wording;
   *   - [[FxMatrix.withRate]] has no later offer to wait for, so the very same rate offered on its
   *     own is a failure at the point it is offered.
   */
  test("cannotAddEntryWithNoCommonCurrencyAndBuild") {
    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(
      Vector(
        rateFor(GBP, USD, 1.6d),
        rateFor(CHF, AUD, 1.6d)))
    collected should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    collected should haveFailureMessageMatching(
      Regex.quote("Received rates with no currencies in common with other: {CHF/AUD=1.6}"))

    val offered: FailureOr[FxMatrix] = placedOneByOne(
      rateFor(GBP, USD, 1.6d),
      rateFor(CHF, AUD, 1.6d))
    offered should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    offered should haveFailureMessageMatching(
      Regex.quote("Received rates with no currencies in common with other: {CHF/AUD=1.6}"))
  }

  /**
   * The complement of the test above, and the reason the collection factory holds a rate back at
   * all: the `CHF/AUD` rate cannot be placed when it is offered, nor can the `EUR/CHF` rate that
   * follows it, and the final `EUR/USD` rate connects both to the matrix at once. The original
   * built this and asserted nothing beyond the absence of an exception; the succeeding outcome is
   * asserted here, together with the order the currencies end up in, which is part of the value
   * and is what the order of the retry fixes.
   */
  test("canAddEntryWithNoCommonCurrencyIfSuppliedBySubsequentEntries") {
    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(
      Vector(
        rateFor(GBP, USD, 1.6d),
        rateFor(CHF, AUD, 1.6d), // cannot be placed - nothing ties it to USD or GBP
        rateFor(EUR, CHF, 1.2d), // again cannot be placed
        rateFor(EUR, USD, 1.4d))) // now everything can be tied together
    collected should beSuccess

    val matrix = unwrap(collected)
    matrix.getCurrencies shouldBe Set(GBP, USD, EUR, CHF, AUD)
    // EUR arrives with the rate that connects it, then the rates held back are placed in the
    // order they were offered in - EUR/CHF brings in CHF, which lets CHF/AUD bring in AUD
    matrix.currencies shouldBe Vector(GBP, USD, EUR, CHF, AUD)
  }

  /**
   * The builder of the original has no target: a second rate for a pair the matrix already holds
   * both currencies of is an update, whether it is offered to a builder or to a matrix, so this
   * places both rates one at a time and asserts the updated matrix.
   */
  test("rateCanBeUpdatedInBuilder") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.5d),
      rateFor(GBP, USD, 1.6d))
    matrix.getCurrencies shouldBe Set(GBP, USD)
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 0.625d, Exactly)))
  }

  /**
   * The misspelling of this test's name is the original's and is kept deliberately, so that the
   * migration manifest can join this test to the Java method it came from.
   *
   * The original extended an existing matrix by reaching for `toBuilder()`, adding two rates and
   * building again. `toBuilder` has no target: [[FxMatrix.withRate]] answers with a new matrix, so
   * extending one is calling it, and the first matrix is untouched by the second - which is
   * asserted here and could not be asserted of a builder.
   */
  test("ratedCanBeUpdatedAndAddedViaBuilder") {
    val matrix1 = matrixOf(rateFor(GBP, USD, 1.5d))

    matrix1.getCurrencies shouldBe Set(GBP, USD)
    matrix1.fxRate(GBP, USD) shouldBe Right(1.5d)

    val matrix2 = unwrap(
      matrix1
        .withRate(GBP, USD, 1.6d) // an update, both currencies being held
        .flatMap(updated => updated.withRate(EUR, USD, 1.4d))) // a new currency

    matrix2.getCurrencies shouldBe Set(GBP, USD, EUR)
    assertRates(
      matrix2,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly)))

    // the matrix the rates were placed into is unchanged, every method answering with a new value
    matrix1.getCurrencies shouldBe Set(GBP, USD)
    matrix1.fxRate(GBP, USD) shouldBe Right(1.5d)
  }

  /**
   * The update of a rate is asymmetric '''by design''', and this test is the whole of the record
   * of that. It is not a defect to be smoothed away by a later reader.
   *
   * When a matrix holds both currencies of an offered rate, the first is the reference and the
   * second is restated against it: every rate involving the restated currency is recomputed from
   * the new rate and the reference currency's existing rates, and every rate of the reference
   * currency against a third currency is left alone. So `withRate(GBP, USD, 1.6)` and
   * `withRate(USD, GBP, 1 / 1.6)` agree about the `GBP/USD` rate itself and disagree about what
   * that rate implies for `EUR`:
   *
   *   - restating `USD` against `GBP` moves `EUR/USD` and leaves `EUR/GBP` where it was;
   *   - restating `GBP` against `USD` leaves `EUR/USD` where it was and moves `EUR/GBP`.
   *
   * The expected numbers below are the original's, which it in turn recorded from the
   * implementation that preceded it, written as the same arithmetic rather than as decimal
   * literals so that what is being asserted is the orientation of the update and not a rounding.
   * A symmetric rule would answer differently for every matrix of three or more currencies, which
   * is why the two matrices are asserted to differ before their rates are read at all.
   */
  test("updatingRateIsNotSymmetric") {
    val matrix1 = matrixOf(
      rateFor(GBP, USD, 1.5d),
      rateFor(EUR, USD, 1.4d))

    val matrix2 = unwrap(matrix1.withRate(GBP, USD, 1.6d))

    // switching the currency order for the update gives a different matrix, and has a different
    // effect on the rates
    val matrix3 = unwrap(matrix1.withRate(USD, GBP, 1d / 1.6d))

    matrix2 should not equal matrix3

    matrix1.getCurrencies should have size 3
    assertRates(
      matrix1,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.5d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        (EUR, GBP, 1.4d / 1.5d, TOL)))

    assertRates(
      matrix2,
      Table(
        ("base", "counter", "expected", "tolerance"),
        // the rate that was updated
        (GBP, USD, 1.6d, Exactly),
        // the update restated USD against GBP, so EUR/USD is affected - it becomes 1.49333 -
        (EUR, USD, 1.4d * (1.6d / 1.5d), TOL),
        // - but EUR/GBP is not, staying at 0.9333
        (EUR, GBP, 1.4d / 1.5d, TOL)))

    assertRates(
      matrix3,
      Table(
        ("base", "counter", "expected", "tolerance"),
        // the rate that was updated
        (GBP, USD, 1.6d, Exactly),
        // this update restated GBP against USD, so there is no effect on EUR/USD
        (EUR, USD, 1.4d, Exactly),
        // - but there is an effect on EUR/GBP, which becomes 0.875
        (EUR, GBP, (1.4d / 1.5d) * (1.5d / 1.6d), TOL)))
  }

  /**
   * The builder of the original has no target; the update is offered to the matrix itself.
   *
   * A matrix of two currencies is the one case in which the direction of an update does not
   * matter, because the asymmetry documented on `updatingRateIsNotSymmetric` acts on the rates of
   * a '''third''' currency and there is none here. The switched update therefore reaches the very
   * same matrix, which is asserted alongside the rate the original asserted.
   */
  test("rateCanBeUpdatedWithDirectionSwitched") {
    val matrix1 = matrixOf(rateFor(GBP, USD, 1.6d))

    matrix1.getCurrencies should have size 2
    matrix1.fxRate(GBP, USD) shouldBe Right(1.6d)

    val matrix2 = unwrap(matrix1.withRate(USD, GBP, 0.625d))

    matrix2.getCurrencies should have size 2
    matrix2.fxRate(GBP, USD) shouldBe Right(1.6d)
    matrix2 shouldBe matrix1
  }

  //-------------------------------------------------------------------------
  /**
   * The original passed an insertion-ordered map to the builder's `addRates`, having chosen that
   * map for its order. [[FxMatrix.ofRates]] takes an iterable of pair and rate and places them in
   * the order it yields them, so the fixture here is an ordered sequence: a map literal of this
   * language does not guarantee an order, and the order is part of the resulting value.
   *
   * Both collection factories are asserted to agree, which is the one place that is stated. The
   * rates of this fixture are all greater than zero, so they can be expressed as [[FxRate]] values
   * and [[FxMatrix.of]] accepts them; the two stream tests below cannot use that factory, their
   * fixture holding a rate of zero.
   */
  test("addSimpleMultipleRates") {
    // an ordered sequence, so that a rate offered before the rate that connects it is offered
    // first - which is what the original's choice of map was for
    val rates: Vector[(CurrencyPair, Double)] = Vector(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))

    val matrix = unwrap(FxMatrix.ofRates(rates))

    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 1d / 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        (USD, EUR, 1d / 1.4d, Exactly),
        (EUR, GBP, 1.4d / 1.6d, TOL),
        (GBP, EUR, 1.6d / 1.4d, TOL)))

    // the same rates stated as rate values reach the same matrix, the two collection factories
    // being one factory with two ways of stating a rate
    val fromRateValues = FxMatrix.of(
      Vector(
        fxRateOf(GBP, USD, 1.6d),
        fxRateOf(EUR, USD, 1.4d)))
    fromRateValues shouldBe Right(matrix)
  }

  /**
   * The builder of the original held the disconnected rate and rejected it from `build()`; here
   * the collection factory holds it while further rates may connect it and reports it once the
   * collection is exhausted, naming the rate it could never place.
   */
  test("addMultipleRatesContainingEntryWithNoCommonCurrency") {
    val rates: Vector[(CurrencyPair, Double)] = Vector(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(JPY, CAD, 0.01d)) // neither currency is linked to one of the others

    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(rates)
    collected should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    collected should haveFailureMessageMatching(
      Regex.quote("Received rates with no currencies in common with other: {JPY/CAD=0.01}"))
  }

  /**
   * The bulk placement of the original, with its full fixture. The order of the sequence is what
   * the whole test turns on - three of the seven rates cannot be placed when they are offered -
   * so it is an ordered sequence and the order the currencies end up in is asserted.
   */
  test("addMultipleRates") {
    val rates: Vector[(CurrencyPair, Double)] = Vector(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(CHF, AUD, 1.2d), // neither currency seen before
      rateFor(SEK, AUD, 0.16d), // AUD seen before but not placed yet
      rateFor(JPY, CAD, 0.01d), // neither currency seen before
      rateFor(EUR, CHF, 1.2d),
      rateFor(JPY, USD, 0.0084d))

    val matrix = unwrap(FxMatrix.ofRates(rates))

    // EUR/CHF places CHF, whose arrival places CHF/AUD, whose arrival places SEK/AUD; JPY/USD
    // then places JPY, whose arrival places JPY/CAD
    matrix.currencies shouldBe Vector(GBP, USD, EUR, CHF, AUD, SEK, JPY, CAD)

    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 1d / 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        (USD, EUR, 1d / 1.4d, Exactly),
        (EUR, GBP, 1.4d / 1.6d, TOL),
        (GBP, EUR, 1.6d / 1.4d, TOL),
        (EUR, CHF, 1.2d, Exactly)))
  }

  /**
   * The original collected a stream of map entries into a matrix through the `entriesToFxMatrix`
   * collector. That collector is not ported - a collection of rates is placed by
   * [[FxMatrix.ofRates]], and a stream by draining it into one - so the same entries in the same
   * order are placed by that factory here.
   *
   * The `JPY/CAD` rate of '''zero''' in this fixture is the point of the test and is kept exactly
   * as the original had it. No factory of the ported type judges a rate, so the zero is accepted,
   * the reciprocal recorded for it is infinite, and the matrix must still answer every other pair
   * - which is what the two rates the original asserted show. The infinite reciprocal is asserted
   * explicitly below so that it is pinned behaviour rather than something the test happens not to
   * look at; it is also why this fixture cannot be stated as [[FxRate]] values, which reject a
   * rate of zero.
   */
  test("streamEntriesToMatrix") {
    val matrix = unwrap(FxMatrix.ofRates(streamRates))

    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        // the zero rate, and the infinity that is its reciprocal
        (JPY, CAD, 0d, Exactly),
        (CAD, JPY, Double.PositiveInfinity, Exactly),
        // every rate of the zero-rated currency follows from it, in both directions
        (GBP, CAD, 0d, Exactly),
        (CAD, GBP, Double.PositiveInfinity, Exactly)))

    // and the diagonal of the matrix is still a unit diagonal, so the value remains one this type
    // would build from its own currencies and rates
    matrix.currencies.indices.foreach(index => matrix.rates.get(index, index) shouldBe 1d)
  }

  /**
   * The original mapped each entry of the stream through a pair of key and shifted rate before
   * collecting it with the `pairsToFxMatrix` collector. Neither the collector nor the pair type is
   * ported: an ordinary two-element tuple is the pair, and [[FxMatrix.ofRates]] takes exactly a
   * collection of those, so the map is a `map` over the fixture and the collect is that factory.
   *
   * The fixture is the one `streamEntriesToMatrix` uses, zero rate included, shifted by the
   * original's factor; the zero shifts to zero and its reciprocal is infinite still.
   */
  test("streamPairsToMatrix") {
    val shifted: Vector[(CurrencyPair, Double)] =
      streamRates.map { case (pair, rate) => (pair, rate * 1.01d) } // apply some shift

    val matrix = unwrap(FxMatrix.ofRates(shifted))

    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.616d, Exactly),
        (EUR, USD, 1.414d, Exactly),
        (JPY, CAD, 0d, Exactly),
        (CAD, JPY, Double.PositiveInfinity, Exactly)))
  }

  /**
   * The original added nine rates one at a time, having noted that a ninth currency forced its
   * backing array to be resized and that the resize should cause no issue. The ported matrix
   * allocates exactly the currencies it holds, so there is no resize and nothing about one to
   * observe; what remains observable is the rates, which is what the original asserted, so this
   * places the same nine rates through the same one-at-a-time placement and asserts them.
   */
  test("addMultipleRatesSingle") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d),
      rateFor(EUR, CHF, 1.2d), // the same rate again, so an update to it
      rateFor(CHF, AUD, 1.2d),
      rateFor(SEK, AUD, 0.16d),
      rateFor(JPY, USD, 0.0084d),
      rateFor(JPY, CAD, 0.01d),
      rateFor(USD, NZD, 1.3d))

    matrix.currencies shouldBe Vector(GBP, USD, EUR, CHF, AUD, SEK, JPY, CAD, NZD)

    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 1d / 1.6d, Exactly),
        (EUR, USD, 1.4d, TOL),
        (USD, EUR, 1d / 1.4d, TOL),
        (EUR, GBP, 1.4d / 1.6d, TOL),
        (GBP, EUR, 1.6d / 1.4d, TOL),
        (EUR, CHF, 1.2d, Exactly)))
  }

  //-------------------------------------------------------------------------
  /**
   * The builder of the original has no target; the two rates are placed one at a time.
   *
   * The conversion of an amount reports a failure as a value, so the amounts asserted here are
   * projected out of a succeeding outcome and then asserted through the matchers of
   * [[com.opengamma.strata.basics.testkit.CurrencyAmountMatchers]], which is the pairing the
   * chained assertion of the original reads as.
   */
  test("convertCurrencyAmount") {
    val matrix = matrixOf(
      rateFor(GBP, EUR, 1.4d),
      rateFor(GBP, USD, 1.6d))

    val amount = amountOf(GBP, 1600d)

    // an amount already in the requested currency is returned unchanged, the matrix not being
    // consulted for it
    matrix.convert(amount, GBP) shouldBe Right(amount)

    unwrap(matrix.convert(amount, USD)) should (haveCurrency(USD) and haveAmount(2560d))
    unwrap(matrix.convert(amount, EUR)) should (haveCurrency(EUR) and haveAmount(2240d))
  }

  /**
   * The builder of the original has no target; the two rates are placed one at a time. The
   * original built the empty multi-currency value through its no-argument factory, which is
   * [[MultiCurrencyAmount.empty]] here.
   *
   * A value holding nothing totals to zero of the requested currency, and needs no rate to do so,
   * which is why this succeeds for all three currencies.
   */
  test("convertMultipleCurrencyAmountWithNoEntries") {
    val matrix = matrixOf(
      rateFor(GBP, EUR, 1.4d),
      rateFor(GBP, USD, 1.6d))

    val amount = MultiCurrencyAmount.empty

    unwrap(matrix.convert(amount, GBP)) should (haveCurrency(GBP) and haveAmount(0d))
    unwrap(matrix.convert(amount, USD)) should (haveCurrency(USD) and haveAmount(0d))
    unwrap(matrix.convert(amount, EUR)) should (haveCurrency(EUR) and haveAmount(0d))
  }

  /**
   * The builder of the original has no target; the two rates are placed one at a time. The
   * conversion of a multi-currency value collapses it to a single amount, as it did in the
   * original, so a value of one amount converts exactly as that amount does.
   */
  test("convertMultipleCurrencyAmountWithSingleEntry") {
    val matrix = matrixOf(
      rateFor(GBP, EUR, 1.4d),
      rateFor(GBP, USD, 1.6d))

    val amount = multiOf(amountOf(GBP, 1600d))

    unwrap(matrix.convert(amount, GBP)) should (haveCurrency(GBP) and haveAmount(1600d))
    unwrap(matrix.convert(amount, USD)) should (haveCurrency(USD) and haveAmount(2560d))
    unwrap(matrix.convert(amount, EUR)) should (haveCurrency(EUR) and haveAmount(2240d))
  }

  /**
   * The builder of the original has no target; the two rates are placed one at a time.
   *
   * The terms are totalled in the alphabetical order of their currency codes, which is the order
   * the multi-currency value holds them in and the order the sorted set of the original yielded
   * them in. Floating-point addition is order-sensitive, so that agreement is what lets these
   * three expectations be the original's expressions unchanged - two of them asserted exactly, and
   * the conversion to sterling within the tolerance, exactly as the original split them.
   */
  test("convertMultipleCurrencyAmountWithMultipleEntries") {
    val matrix = matrixOf(
      rateFor(GBP, EUR, 1.4d),
      rateFor(GBP, USD, 1.6d))

    val amount = multiOf(
      amountOf(GBP, 1600d),
      amountOf(EUR, 1200d),
      amountOf(USD, 1500d))

    unwrap(matrix.convert(amount, GBP)) should
      (haveCurrency(GBP) and haveAmount(1600d + (1200d / 1.4d) + (1500d / 1.6d), TOL))

    unwrap(matrix.convert(amount, USD)) should
      (haveCurrency(USD) and haveAmount((1600d * 1.6d) + ((1200d / 1.4d) * 1.6d) + 1500d))

    unwrap(matrix.convert(amount, EUR)) should
      (haveCurrency(EUR) and haveAmount((1600d * 1.4d) + 1200d + ((1500d / 1.6d) * 1.4d)))
  }

  //-------------------------------------------------------------------------
  /**
   * The builder of the original has no target; both matrices are placed rate by rate. Two matrices
   * with no currency in common cannot be merged, and that is a `Left` where the original threw,
   * naming both sets of currencies in the order each matrix holds them.
   */
  test("cannotMergeDisjointMatrices") {
    val matrix1 = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))

    val matrix2 = matrixOf(
      rateFor(CHF, AUD, 1.2d),
      rateFor(SEK, AUD, 0.16d))

    val merged: FailureOr[FxMatrix] = matrix1.merge(matrix2)
    merged should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    merged should haveFailureMessageMatching(
      Regex.quote("There are no currencies in common between [GBP, USD, EUR] and [CHF, AUD, SEK]"))
  }

  /**
   * The builder of the original has no target; both matrices are placed rate by rate.
   *
   * The other matrix holds the same currencies as this one and different rates for them. A merge
   * adds only the currencies this matrix does not hold, so there is nothing to add and the result
   * is this matrix unchanged - the rates of the other matrix are merged '''into''' this one rather
   * than combined with it, and none of them displaces a rate already here.
   */
  test("mergeIgnoresDuplicateCurrencies") {
    val matrix1 = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d))

    val matrix2 = matrixOf(
      rateFor(GBP, USD, 1.7d),
      rateFor(EUR, USD, 1.5d),
      rateFor(EUR, CHF, 1.3d))

    val result = unwrap(matrix1.merge(matrix2))
    result shouldBe matrix1
  }

  /**
   * The builder of the original has no target; both matrices are placed rate by rate.
   *
   * Every currency the other matrix adds is placed at the rate that matrix records between it and
   * the '''first''' currency of this matrix the other also holds - `EUR` here, `GBP` and `USD` not
   * being in the other matrix - so the rates of the result are coherent with the rates already in
   * this matrix. The two rates read straight from the other matrix are asserted exactly and the
   * three the merge derived within the tolerance, exactly as the original split them.
   */
  test("mergeAddsInAdditionalCurrencies") {
    val matrix1 = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))

    val matrix2 = matrixOf(
      rateFor(EUR, CHF, 1.2d),
      rateFor(CHF, AUD, 1.2d))

    val result = unwrap(matrix1.merge(matrix2))
    result.getCurrencies shouldBe Set(USD, GBP, EUR, CHF, AUD)
    // the added currencies take the positions after the currencies this matrix already held, in
    // the order the other matrix holds them
    result.currencies shouldBe Vector(GBP, USD, EUR, CHF, AUD)

    assertRates(
      result,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (GBP, EUR, 1.6d / 1.4d, TOL),
        (EUR, CHF, 1.2d, Exactly),
        (CHF, AUD, 1.2d, Exactly),
        (GBP, CHF, (1.6d / 1.4d) * 1.2d, TOL),
        (GBP, AUD, (1.6d / 1.4d) * 1.2d * 1.2d, TOL)))
  }

  //-------------------------------------------------------------------------
  /**
   * The equality rows of the original, with the builder replaced by placement, and one row the
   * original did not have.
   *
   * That row is the one that states what the order of the currencies is: two matrices holding the
   * same rate for the same two currencies, offered in opposite directions, hold their currencies
   * in opposite orders and are '''not''' equal. The order is part of the value - it is what
   * `toString` writes and what the JSON form carries - so this is the behaviour rather than an
   * artefact, and it is the behaviour of the implementation being ported, whose equality read an
   * ordered map of currency to position.
   */
  test("equalsGood") {
    val m1 = matrixOf(rateFor(GBP, USD, 1.4d))
    val m2 = matrixOf(rateFor(GBP, USD, 1.39d))
    val m3 = matrixOf(rateFor(GBP, USD, 1.39d))
    val m4 = matrixOf(rateFor(GBP, EUR, 1.2d))

    m1.equals(m1) shouldBe true
    m2.equals(m2) shouldBe true
    m3.equals(m3) shouldBe true
    m4.equals(m4) shouldBe true

    m1.equals(m2) shouldBe false
    m1.equals(m4) shouldBe false

    m2.equals(m3) shouldBe true

    // the same rate offered in the opposite direction holds the same two currencies in the
    // opposite order, which is a different matrix
    val forwards = FxMatrix.of(GBP, USD, 1.6d)
    val backwards = FxMatrix.of(USD, GBP, 1d / 1.6d)
    forwards.getCurrencies shouldBe backwards.getCurrencies
    forwards.currencies shouldBe Vector(GBP, USD)
    backwards.currencies shouldBe Vector(USD, GBP)
    forwards.equals(backwards) shouldBe false
  }

  test("equalsBad") {
    val test = matrixOf(rateFor(USD, GBP, 1.4d))

    // a value of an unrelated type is not equal to a matrix, in either direction. The original
    // also asserted equality against the absent-reference literal; that case is subsumed by the
    // types of this port, where a reference to a matrix cannot be absent and there is no such
    // literal to compare against, so it is recorded here rather than written.
    test.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(test) shouldBe false
  }

  /**
   * Equal matrices hash alike, which is the whole of the contract the hash bears.
   *
   * The original also asserted that two matrices holding different rates hash '''differently'''.
   * That is not carried, and deliberately: a hash guarantees only that equal values agree, and two
   * distinct values may legally collide, so an assertion that they do not would make a correct
   * hash fail here. What the original was reaching for - that the rates are part of the identity
   * of a matrix, and not only its currencies - is asserted by `equalsGood`, which is where it
   * belongs.
   */
  test("hashCodeCoverage") {
    val m2 = matrixOf(rateFor(GBP, USD, 1.39d))
    val m3 = matrixOf(rateFor(GBP, USD, 1.39d))

    m2 shouldBe m3
    m2.hashCode shouldBe m3.hashCode
    Hash[FxMatrix].hash(m2) shouldBe Hash[FxMatrix].hash(m3)

    // a matrix reached by a different route hashes as the matrix it equals
    val placed = matrixOf(rateFor(GBP, USD, 1.39d))
    val direct = FxMatrix.of(GBP, USD, 1.39d)
    placed shouldBe direct
    placed.hashCode shouldBe direct.hashCode
  }

  //-------------------------------------------------------------------------
  /**
   * The reflective bean sweep of the original has no counterpart: there is no meta-bean to walk
   * and no property to read by name. What it was standing in for - that the value behaves as a
   * value - is asserted directly over the two matrices it swept, through the instances the
   * companion publishes, and over the text form the rest of the library reads through `Show`.
   */
  test("coverage") {
    val emptyMatrix = FxMatrix.empty
    val test = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d))
    val same = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d))

    // equality is stated through `eqv`, which is the form the contract bears: the instance
    // guarantees that equal matrices hash alike and says nothing whatever about unequal ones, so
    // the hashes of `test` and `emptyMatrix` are deliberately left uncompared
    Hash[FxMatrix].eqv(test, same) shouldBe true
    Hash[FxMatrix].eqv(test, emptyMatrix) shouldBe false
    Hash[FxMatrix].hash(test) shouldBe Hash[FxMatrix].hash(same)

    // the rendering is the text form of the type, so the two agree exactly for every matrix
    Show[FxMatrix].show(emptyMatrix) shouldBe "FxMatrix[ : ]"
    Show[FxMatrix].show(test) shouldBe test.toString
    // the currencies in matrix order, then the rows of the matrix, which is the form the
    // implementation being ported wrote
    Show[FxMatrix].show(FxMatrix.of(GBP, USD, 1.6d)) shouldBe
      "FxMatrix[GBP, USD : [1.0, 1.6],[0.625, 1.0]]"
    test.toString should startWith("FxMatrix[GBP, USD, EUR, CHF : ")
  }

  /**
   * The original cycled each matrix through the compact XML form of the bean library it used.
   * Neither that library nor any wire compatibility with it is ported, so the round trip here is
   * through the JSON codec of this port, over the same three matrices the original cycled plus one
   * the original had no reason to hold.
   *
   * That fourth matrix carries a rate of '''zero''', whose recorded reciprocal is infinite. It is
   * the case the tagged representation of a double exists for: neither infinity has a JSON number
   * to be written as, so both are written as strings, and a round trip that lost them would lose
   * the matrix. The document of that matrix is therefore pinned literally.
   *
   * Decoding routes the currencies and rates through [[FxMatrix.fromMatrix]], the same factory a
   * caller's own currencies and rates go through, so a document that does not describe a matrix is
   * a decoding failure rather than a value that bypassed the checks.
   */
  test("testSerializeDeserialize") {
    val test1 = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d))
    val test2 = matrixOf(
      rateFor(GBP, USD, 1.7d),
      rateFor(EUR, USD, 1.5d),
      rateFor(EUR, CHF, 1.3d))

    cycled(FxMatrix.empty) shouldBe FxMatrix.empty
    cycled(test1) shouldBe test1
    cycled(test2) shouldBe test2

    // the shape is an object of two fields, the currencies in matrix order and the rates as an
    // array of rows, with the fields in declaration order and nothing else present
    val encoded: Json = FxMatrix.of(GBP, USD, 1.6d).asJson
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("currencies", "rates"))
    encoded.noSpaces shouldBe """{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]}"""
    encoded.as[FxMatrix] shouldBe Right(FxMatrix.of(GBP, USD, 1.6d))

    FxMatrix.empty.asJson.noSpaces shouldBe """{"currencies":[],"rates":[]}"""

    // the zero rate and the infinity recorded as its reciprocal, written as a number and as a
    // tagged string respectively, and recovered as they were
    val zeroRated = FxMatrix.of(JPY, CAD, 0d)
    zeroRated.asJson.noSpaces shouldBe
      """{"currencies":["JPY","CAD"],"rates":[[1.0,0.0],["Infinity",1.0]]}"""
    cycled(zeroRated) shouldBe zeroRated
    unwrap(cycled(zeroRated).fxRate(CAD, JPY)) shouldBe Double.PositiveInfinity

    // currencies and rates stated directly reach the matrix the single-rate factory builds, which
    // is the route the decoder takes
    FxMatrix.fromMatrix(
      Vector(GBP, USD),
      DoubleMatrix.of(2, 2, 1d, 1.6d, 0.625d, 1d)) shouldBe Right(FxMatrix.of(GBP, USD, 1.6d))

    // a document whose diagonal is not one, and one whose currencies repeat, describe no matrix
    Json
      .obj(
        "currencies" -> Json.arr(Json.fromString("GBP"), Json.fromString("USD")),
        "rates" -> Json.arr(
          Json.arr(Json.fromDoubleOrNull(1d), Json.fromDoubleOrNull(1.6d)),
          Json.arr(Json.fromDoubleOrNull(0.625d), Json.fromDoubleOrNull(2d))))
      .as[FxMatrix]
      .isLeft shouldBe true
    Json
      .obj(
        "currencies" -> Json.arr(Json.fromString("GBP"), Json.fromString("GBP")),
        "rates" -> Json.arr(
          Json.arr(Json.fromDoubleOrNull(1d), Json.fromDoubleOrNull(1d)),
          Json.arr(Json.fromDoubleOrNull(1d), Json.fromDoubleOrNull(1d))))
      .as[FxMatrix]
      .isLeft shouldBe true
  }

  /**
   * Asserts the structurally valid edge states of a matrix, over the shared edge generator.
   *
   * [[FxMatrix.fromMatrix]] - the factory a decoded document arrives at - checks three things and
   * no more: the currencies are distinct, the rates are a square matrix of their number, and the
   * diagonal is one. It deliberately requires neither reciprocity nor triangulation, because the
   * builder being ported accepted whatever rates it was given, so a matrix may hold an
   * off-diagonal pair that are not reciprocals of each other, a `NaN`, either infinity and a
   * negative zero. Those states are legal, they are what the bit-pattern equality and the tagged
   * document form of this type exist for, and every fixture of the original and of the tests above
   * is reciprocal and finite apart from the one zero rate - so until now they were asserted
   * nowhere.
   *
   * The generator is the module's shared edge generator for this type, which builds every matrix
   * through `fromMatrix` for exactly that reason - placing a rate computes the reciprocal for the
   * opposite position, which is the state a non-reciprocal matrix does not have - and it is the
   * same generator the typeclass law suite runs its `FxMatrix` rule sets over, so a matrix this
   * test admits is one those laws are checked over too. The property-based sweep over every codec
   * of the module lives in the module's JSON round-trip spec, which is another unit's file, so the
   * document half of this coverage is asserted here beside the equality half it belongs with.
   *
   * The matrix stated by hand after the property is the one the generator pins: a non-reciprocal
   * pair - 2.0 one way and 5.0 the other, where reciprocity would require 0.5 - a negative zero, a
   * `NaN` and both infinities, all in one value, so that each is present however the draws fall.
   */
  test("a matrix holding non-reciprocal and non-finite rates is a value the codec carries") {
    forAll(genEdgeFxMatrix, minSuccessful(200)) { (matrix: FxMatrix) =>
      // the document form carries every entry, the three JSON has no number for as tagged strings
      cycled(matrix) shouldBe matrix
      // and the value read back is equal to the value written and hashes with it, which for a
      // matrix holding a `NaN` holds only because the equality compares bit patterns
      Hash[FxMatrix].eqv(cycled(matrix), matrix) shouldBe true
      Hash[FxMatrix].hash(cycled(matrix)) shouldBe Hash[FxMatrix].hash(matrix)
      Show[FxMatrix].show(matrix) shouldBe matrix.toString
      // every generated matrix is one `fromMatrix` accepts, which is what makes the states above
      // legal states rather than values that bypassed a check
      FxMatrix.fromMatrix(matrix.currencies, matrix.rates) shouldBe Right(matrix)
    }

    val edges: FxMatrix = unwrapNec(
      FxMatrix.fromMatrix(
        Vector(GBP, USD, EUR),
        DoubleMatrix.of(
          3,
          3,
          1d,
          2d,
          Double.NaN,
          5d,
          1d,
          Double.PositiveInfinity,
          -0.0d,
          Double.NegativeInfinity,
          1d)))

    // the pair that is not reciprocal, kept as it was stated rather than recomputed
    unwrap(edges.fxRate(GBP, USD)) shouldBe 2d
    unwrap(edges.fxRate(USD, GBP)) shouldBe 5d
    // the three entries no JSON number can carry, and the signed zero the equality tells apart
    unwrap(edges.fxRate(GBP, EUR)).isNaN shouldBe true
    unwrap(edges.fxRate(USD, EUR)) shouldBe Double.PositiveInfinity
    unwrap(edges.fxRate(EUR, USD)) shouldBe Double.NegativeInfinity
    java.lang.Double.doubleToLongBits(unwrap(edges.fxRate(EUR, GBP))) shouldBe
      java.lang.Double.doubleToLongBits(-0.0d)

    // the whole value survives its document form, and the document is pinned as written text
    cycled(edges) shouldBe edges
    edges.asJson.noSpaces shouldBe
      """{"currencies":["GBP","USD","EUR"],""" +
        """"rates":[[1.0,2.0,"NaN"],[5.0,1.0,"Infinity"],[-0.0,"-Infinity",1.0]]}"""
    // a matrix holding a `NaN` equals itself, where the primitive comparison of that entry would
    // not, and the negative zero is what keeps it apart from the same matrix holding a positive one
    edges.equals(
      unwrapNec(FxMatrix.fromMatrix(edges.currencies, edges.rates))) shouldBe true
    val positiveZero: FxMatrix = unwrapNec(
      FxMatrix.fromMatrix(
        Vector(GBP, USD, EUR),
        DoubleMatrix.of(
          3,
          3,
          1d,
          2d,
          Double.NaN,
          5d,
          1d,
          Double.PositiveInfinity,
          0.0d,
          Double.NegativeInfinity,
          1d)))
    edges.equals(positiveZero) shouldBe false
    Hash[FxMatrix].eqv(edges, positiveZero) shouldBe false

    // and the one structural state that is not legal, so that the checks the factory does make
    // are stated beside the ones it does not
    FxMatrix
      .fromMatrix(Vector(GBP, USD), DoubleMatrix.of(2, 2, 1d, 2d, 0.5d, Double.NaN))
      .isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // The tests from here to the fixtures below are not ported from the Java suite and have no
  // method of it to correspond to. They pin behaviour of this type that the Java suite left to
  // follow from its implementation: the bounded refusal of a collection that connects to
  // nothing, the storage and retry of the rates held back by a collection factory, the route a
  // multi-currency conversion takes, the agreement of the two collection
  // factories and the single traversal of the one that takes rate values, and the ordered set of
  // currencies being a held value. Each states in its own scaladoc what it pins and why that is
  // observable rather than internal.

  /**
   * A collection of rates that connect to nothing is refused, and the refusal names a bounded
   * number of them.
   *
   * Twenty-one of the twenty-two rates offered here share no currency with the matrix or with each
   * other, so every one of them is held back until the collection is exhausted and then reported.
   * Two things are pinned. The '''listing is bounded''': eight rates are named and the rest are
   * counted, so the message a caller reads does not grow with the collection they offered. And the
   * '''work is bounded''': holding a rate back is a keyed write that does not examine the rates
   * already held back, and a pass over those rates happens only when an offer brought a currency
   * into the matrix, so twenty-one disconnected offers cost time proportional to their number
   * rather than to its square. The mechanism rather than the elapsed time is what the two tests
   * after this one assert, because a timing assertion would pin the machine and not the code.
   */
  test("manyDisconnectedRatesAreRefusedWithABoundedListing") {
    val rates: Vector[(CurrencyPair, Double)] = rateFor(GBP, USD, 1.6d) +: disconnectedRates

    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(rates)
    collected should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    collected should haveFailureMessageMatching(
      Regex.quote(
        "Received rates with no currencies in common with other: {AED/ARS=1.1, BGN/BHD=1.1, " +
          "BRL/CLP=1.1, CNH/CNY=1.1, COP/CZK=1.1, DKK/EGP=1.1, HKD/HRK=1.1, HUF/IDR=1.1, " +
          "and 13 more}"))
  }

  /**
   * A rate offered again for a pair that is still held back replaces the rate held for it and
   * keeps that pair's position among the rates held back.
   *
   * This is what pins the keyed, insertion-ordered store the rates held back are kept in: the
   * `CHF/AUD` rate is offered twice and the `JPY/CAD` rate between them once, and the refusal
   * names `CHF/AUD` first - its original position - carrying the '''second''' of its two rates.
   * A store that appended the repeat would name the pair twice and in the wrong order, and one
   * that replaced it by removal and re-insertion would name it last.
   */
  test("rateOfferedAgainWhileHeldBackReplacesItInPlace") {
    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(
      Vector(
        rateFor(GBP, USD, 1.6d),
        rateFor(CHF, AUD, 1.2d), // held back, and the first of the rates held back
        rateFor(JPY, CAD, 0.01d), // held back, after it
        rateFor(CHF, AUD, 1.3d))) // the same pair again: the rate is replaced, the position kept

    collected should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    collected should haveFailureMessageMatching(
      Regex.quote(
        "Received rates with no currencies in common with other: {CHF/AUD=1.3, JPY/CAD=0.01}"))
  }

  /**
   * A chain of rates that connects only when the last rate arrives places its currencies in the
   * order the retry of the rates held back reaches them.
   *
   * Three rates are held back in turn and the fourth offered rate connects one of them; each pass
   * over the rates held back then places exactly the one rate the matrix has come to reach, so the
   * currencies arrive one pass at a time and in the order the passes reach them - `NZD` from the
   * rate that connected, then `EUR`, `CHF` and `AUD`. The order is part of the value, so this is
   * an assertion about the result and not about how it was computed; it is also what shows that
   * retrying only when a currency arrived reaches the state retrying after every offer reached.
   */
  test("chainOfHeldBackRatesConnectingAtTheEndKeepsTheCurrencyOrder") {
    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(
      Vector(
        rateFor(GBP, USD, 1.6d),
        rateFor(CHF, AUD, 1.2d), // held back
        rateFor(EUR, CHF, 1.2d), // held back
        rateFor(NZD, EUR, 1.1d), // held back
        rateFor(USD, NZD, 1.4d))) // places NZD, whose arrival unwinds the chain one pass at a time

    collected should beSuccess
    unwrap(collected).currencies shouldBe Vector(GBP, USD, NZD, EUR, CHF, AUD)
  }

  /**
   * A multi-currency conversion stops at the first amount it can find no rate for.
   *
   * The amounts are held in the alphabetical order of their currency codes, so `GBP` is converted,
   * `JPY` is the first currency this matrix holds no rate for and `NZD` is never reached. The
   * failure is the failure of the `JPY` lookup, naming that pair: a conversion that carried the
   * last failure instead, or accumulated both, would name `NZD` or mention it as well.
   */
  test("convertMultiCurrencyAmountStopsAtTheFirstMissingRate") {
    val matrix = matrixOf(rateFor(GBP, USD, 1.6d))

    val amount = multiOf(
      amountOf(GBP, 100d),
      amountOf(JPY, 200d),
      amountOf(NZD, 300d))

    val converted: FailureOr[CurrencyAmount] = matrix.convert(amount, USD)
    converted should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    converted should haveFailureMessageMatching(
      Regex.quote("No FX rate found for JPY/USD, matrix only contains rates for [GBP, USD]"))
  }

  /**
   * A multi-currency conversion totals the converted amounts in the alphabetical order of their
   * currency codes, and the total is that sum exactly.
   *
   * Floating-point addition is neither exact nor associative, so the order the terms are added in
   * is part of the answer and the captured baseline of this port records the numbers it produces.
   * The expectation here is therefore built as the conversion builds it - from zero, each amount
   * multiplied by the rate this matrix answers for its currency, added in currency order - and
   * compared exactly rather than within a tolerance, which is what makes a rearrangement of the
   * summation a failing test rather than a passing one.
   */
  test("convertMultiCurrencyAmountTotalsInCurrencyOrder") {
    val matrix = matrixOf(
      rateFor(GBP, EUR, 1.4d),
      rateFor(GBP, USD, 1.6d))

    val amount = multiOf(
      amountOf(GBP, 1600d),
      amountOf(EUR, 1200d),
      amountOf(USD, 1500d))

    // EUR, then GBP, then USD - the order the amounts are held in, which is the order they total
    val expected: Double =
      0d +
        (1200d * unwrap(matrix.fxRate(EUR, USD))) +
        (1600d * unwrap(matrix.fxRate(GBP, USD))) +
        (1500d * unwrap(matrix.fxRate(USD, USD)))

    unwrap(matrix.convert(amount, USD)) should (haveCurrency(USD) and haveAmount(expected))
  }

  /**
   * The two collection factories are one factory: rates stated as values place identically to the
   * same pairs and rates stated directly, including when a rate must be held back.
   *
   * The fixture is deliberately out of order - `CHF/AUD` and `EUR/CHF` cannot be placed when they
   * are offered and the final `EUR/USD` rate connects them - so the agreement asserted covers the
   * tolerance of that order and not merely the straightforward case. The order the currencies end
   * up in is asserted too, being part of the value the two factories must agree on.
   */
  test("ofRateValuesAgreesWithOfPairsAndRatesIncludingHeldBackRates") {
    val rateValues: Vector[FxRate] = Vector(
      fxRateOf(GBP, USD, 1.6d),
      fxRateOf(CHF, AUD, 1.6d),
      fxRateOf(EUR, CHF, 1.2d),
      fxRateOf(EUR, USD, 1.4d))

    val fromRateValues: FailureOr[FxMatrix] = FxMatrix.of(rateValues)
    val fromPairsAndRates: FailureOr[FxMatrix] =
      FxMatrix.ofRates(rateValues.map(rate => (rate.pair, rate.rate)))

    fromRateValues should beSuccess
    fromRateValues shouldBe fromPairsAndRates
    unwrap(fromRateValues).currencies shouldBe Vector(GBP, USD, EUR, CHF, AUD)
  }

  /**
   * The factory taking rate values traverses the collection it is given exactly once.
   *
   * The rates are offered through a collection that yields its iterator once and fails the test if
   * it is asked for a second one, so a factory that materialised the rates before placing them -
   * or read the collection twice for any other reason - fails here rather than merely allocating.
   * The matrix built from it is the matrix the same rates in a strict collection build, so the
   * single traversal places every rate.
   */
  test("ofRateValuesTraversesTheCollectionOnce") {
    val rateValues: Vector[FxRate] = Vector(
      fxRateOf(GBP, USD, 1.6d),
      fxRateOf(EUR, USD, 1.4d),
      fxRateOf(EUR, CHF, 1.2d))

    FxMatrix.of(new SingleUseRates(rateValues)) shouldBe FxMatrix.of(rateValues)
  }

  /**
   * The ordered set of currencies is a held value: two calls answer the same instance, and it
   * iterates in matrix order.
   *
   * A matrix is immutable, so the set derived from its currencies cannot change and rebuilding it
   * per call would allocate for every caller that reads it. The identity assertion is what pins
   * that it is held rather than rebuilt - equality alone would pass either way - and the order
   * assertion is what pins that holding it did not cost the property the set is documented to
   * have.
   */
  test("getCurrenciesIsHeldRatherThanRebuilt") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))

    val currencies: Set[Currency] = matrix.getCurrencies
    currencies should be theSameInstanceAs matrix.getCurrencies
    currencies.iterator.toVector shouldBe matrix.currencies
    currencies shouldBe Set(GBP, USD, EUR)
  }

  //-------------------------------------------------------------------------
  /**
   * The seven rates the two stream tests of the original collected, in the order it stated them.
   *
   * The order is part of the fixture rather than a presentational choice: three of the seven
   * cannot be placed when they are offered, so the order decides which later rate connects each of
   * them and therefore the position every currency ends up in. The original stated them in an
   * insertion-ordered map for that reason, and an ordered sequence is what states it here.
   *
   * The `JPY/CAD` rate is zero, deliberately, and both tests depend on it - see
   * `streamEntriesToMatrix`.
   */
  private val streamRates: Vector[(CurrencyPair, Double)] = Vector(
    rateFor(GBP, USD, 1.6d),
    rateFor(EUR, USD, 1.4d),
    rateFor(CHF, AUD, 1.2d), // neither currency seen before
    rateFor(SEK, AUD, 0.1d), // AUD seen before but not placed yet
    rateFor(JPY, CAD, 0d), // neither currency seen before, and the rate is zero
    rateFor(EUR, CHF, 1.2d),
    rateFor(JPY, USD, 0.008d))

  /**
   * Twenty-one rates, no two of which share a currency and none of which shares one with the
   * `GBP/USD` rate the test offering them places first.
   *
   * Forty-two distinct currencies are needed to state twenty-one mutually disconnected pairs, so
   * the codes are written through the companion rather than imported one by one, which would treble
   * the import block of this suite for a single fixture. Every rate is the same number, because
   * what the fixture is for is the number of rates that can never be placed and not their values -
   * and one number makes the bounded listing the refusal produces readable as an expectation.
   *
   * The order is the order they are offered in and therefore the order the refusal lists them in,
   * so the first eight of them are the eight the message names.
   */
  private val disconnectedRates: Vector[(CurrencyPair, Double)] = Vector(
    rateFor(Currency.AED, Currency.ARS, 1.1d),
    rateFor(Currency.BGN, Currency.BHD, 1.1d),
    rateFor(Currency.BRL, Currency.CLP, 1.1d),
    rateFor(Currency.CNH, Currency.CNY, 1.1d),
    rateFor(Currency.COP, Currency.CZK, 1.1d),
    rateFor(Currency.DKK, Currency.EGP, 1.1d),
    rateFor(Currency.HKD, Currency.HRK, 1.1d),
    rateFor(Currency.HUF, Currency.IDR, 1.1d),
    rateFor(Currency.ILS, Currency.INR, 1.1d),
    rateFor(Currency.ISK, Currency.KRW, 1.1d),
    rateFor(Currency.KZT, Currency.MAD, 1.1d),
    rateFor(Currency.MXN, Currency.MYR, 1.1d),
    rateFor(Currency.NOK, Currency.OMR, 1.1d),
    rateFor(Currency.PEN, Currency.PHP, 1.1d),
    rateFor(Currency.PKR, Currency.PLN, 1.1d),
    rateFor(Currency.QAR, Currency.RON, 1.1d),
    rateFor(Currency.RUB, Currency.SAR, 1.1d),
    rateFor(Currency.SGD, Currency.XAG, 1.1d),
    rateFor(Currency.THB, Currency.TRY, 1.1d),
    rateFor(Currency.TWD, Currency.UAH, 1.1d),
    rateFor(Currency.VND, Currency.ZAR, 1.1d))

  //-------------------------------------------------------------------------
  /**
   * A collection of rates that can be traversed exactly once.
   *
   * The collection factory taking rate values accepts an `Iterable`, and an `Iterable` is not
   * required to be re-traversable: one backed by an iterator yields its elements to the first
   * traversal and has nothing left for a second. This states that collection, and states it
   * strictly - a second request for an iterator fails the test rather than answering an empty one -
   * so a factory that reads its argument twice is reported as the defect it is instead of silently
   * building a matrix from a prefix of the rates.
   *
   * The single iterator is handed out through a one-element iterator of its own, which is what lets
   * the class record that it has been handed out without holding a mutable field.
   *
   * @param rates  the rates to yield, in order
   */
  private final class SingleUseRates(rates: Vector[FxRate]) extends Iterable[FxRate] {

    /** The single traversal this collection admits, held until it is asked for. */
    private val traversals: Iterator[Iterator[FxRate]] = Iterator.single(rates.iterator)

    /**
     * Answers the one iterator this collection has, or fails the test.
     *
     * @return the iterator over the rates, on the first call only
     */
    override def iterator: Iterator[FxRate] =
      if (traversals.hasNext) traversals.next()
      else fail("Expected the rates to be traversed once but a second traversal was requested")
  }

  //-------------------------------------------------------------------------
  /**
   * States one rate to place, as the pair and rate the collection factories take.
   *
   * This is the shape of one argument list of the builder's `addRate`, so a fixture written with
   * it reads as the original's chain of calls did, and it is the element type
   * [[FxMatrix.ofRates]] and [[FxMatrix.withRates]] consume.
   *
   * @param base  the first currency of the pair, the reference currency of an update
   * @param counter  the second currency of the pair, the currency restated by an update
   * @param rate  the rate of the first currency in terms of the second
   * @return the pair and rate
   */
  private def rateFor(base: Currency, counter: Currency, rate: Double): (CurrencyPair, Double) =
    CurrencyPair.of(base, counter) -> rate

  /**
   * Places the specified rates into the empty matrix one at a time, threading the outcome.
   *
   * This is what a chain of the builder's `addRate` calls followed by `build()` becomes: each rate
   * is offered to the matrix the previous rate produced, and a rate that cannot be placed ends the
   * fold with its failure. It differs from [[FxMatrix.ofRates]] in exactly one way, which
   * `cannotAddEntryWithNoCommonCurrencyAndBuild` asserts: a rate offered here has no later rate to
   * wait for, so a rate with no currency in common fails at the point it is offered rather than at
   * the end of the collection.
   *
   * @param rates  the rates to place, in the order to place them
   * @return the matrix holding those rates, or the failure of the first rate that could not be
   *   placed
   */
  private def placedOneByOne(rates: (CurrencyPair, Double)*): FailureOr[FxMatrix] =
    rates.foldLeft[FailureOr[FxMatrix]](Right(FxMatrix.empty)) {
      case (outcome, (pair, rate)) => outcome.flatMap(matrix => matrix.withRate(pair, rate))
    }

  /**
   * Places the specified rates one at a time, failing the test if any of them could not be placed.
   *
   * This is the fixture form of [[placedOneByOne]], for the tests whose matrix is expected to be
   * built rather than refused.
   *
   * @param rates  the rates to place, in the order to place them
   * @return the matrix holding those rates
   */
  private def matrixOf(rates: (CurrencyPair, Double)*): FxMatrix =
    unwrap(placedOneByOne(rates: _*))

  /**
   * Asserts every expected rate of a matrix, one row of a table each.
   *
   * The rows are asserted through a single check so that the assertions of a multi-rate test are
   * one statement rather than a run of them, and so that a failing row reports which pair failed.
   * The tolerance column carries the distinction the original drew between its two comparisons: a
   * row whose tolerance is [[Exactly]] asserts the rate as it stands, and any other row asserts it
   * within that tolerance.
   *
   * @param matrix  the matrix to query
   * @param rows  the base currency, counter currency, expected rate and tolerance of each
   *   expectation
   * @return the assertion over every row
   */
  private def assertRates(
      matrix: FxMatrix,
      rows: TableFor4[Currency, Currency, Double, Double]): Assertion =
    forAll(rows) { (base: Currency, counter: Currency, expected: Double, tolerance: Double) =>
      withClue(s"fxRate($base, $counter): ") {
        assertRate(unwrap(matrix.fxRate(base, counter)), expected, tolerance)
      }
    }

  /**
   * Asserts one rate, exactly or within a tolerance.
   *
   * A tolerance of [[Exactly]] compares the numbers as they stand, which is what the original did
   * for a rate read straight out of the matrix and is the only comparison that can express an
   * expectation of an infinite rate - no finite tolerance spans a difference that is not finite.
   * Any other tolerance is the tolerant comparison of the test framework, whose bound is
   * inclusive.
   *
   * @param actual  the rate the matrix answered
   * @param expected  the rate expected
   * @param tolerance  the largest difference that still passes, or [[Exactly]] for no tolerance
   * @return the assertion
   */
  private def assertRate(actual: Double, expected: Double, tolerance: Double): Assertion =
    if (tolerance == Exactly) actual shouldBe expected else actual shouldBe (expected +- tolerance)

  /**
   * Round-trips a matrix through its JSON codec, failing the test if the document it encodes to
   * does not decode.
   *
   * This replaces the helper of the original, which wrote a matrix as compact XML through the bean
   * library it used and read it back. That library is not ported and no compatibility with its
   * documents is claimed, so the round trip is through the codec of this port; what is asserted of
   * the result is what the original asserted, that the value read back equals the value written.
   *
   * @param matrix  the matrix to encode and decode
   * @return the matrix decoded from the document the given matrix encodes to
   */
  private def cycled(matrix: FxMatrix): FxMatrix =
    matrix.asJson
      .as[FxMatrix]
      .fold(
        failure => fail(s"Expected the encoded matrix to decode but it failed with: $failure"),
        decoded => decoded)

  /**
   * Reads the value out of an outcome that is expected to have produced one, failing the test with
   * the failure's message when it did not.
   *
   * Every failable operation of this module reports a failure as a value, so the fixtures of this
   * suite and the results it asserts on are projected out of an outcome through this one helper.
   * A test that means to assert a failure asserts the outcome itself with the matchers instead.
   *
   * @param outcome  the outcome expected to carry a value
   * @tparam A  the type of the value
   * @return the value it carries
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the operation failed with: ${failure.message}"),
      value => value)

  /**
   * Reads the value out of an accumulating outcome that is expected to have produced one.
   *
   * Only the rate values of `addSimpleMultipleRates` need this: [[FxRate.of]] accumulates, so its
   * outcome carries a chain of failures rather than one, and every message of the chain is
   * reported when a fixture unexpectedly fails to build.
   *
   * @param outcome  the outcome expected to carry a value
   * @tparam A  the type of the value
   * @return the value it carries
   */
  private def unwrapNec[A](outcome: ResultNec[A]): A =
    outcome.fold(
      failures =>
        fail(
          "Expected a value but the operation failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      value => value)

  /**
   * Builds a rate value, failing the test if the arguments describe none.
   *
   * @param base  the base currency
   * @param counter  the counter currency
   * @param rate  the rate, expected to be one a rate value admits
   * @return the rate value
   */
  private def fxRateOf(base: Currency, counter: Currency, rate: Double): FxRate =
    unwrapNec(FxRate.of(base, counter, rate))

  /**
   * Builds an amount, failing the test if the currency and number describe none.
   *
   * @param currency  the currency of the amount
   * @param amount  the number of that currency, expected to be one an amount may hold
   * @return the amount
   */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))

  /**
   * Builds a multi-currency value from amounts of distinct currencies, failing the test if they
   * describe none.
   *
   * @param amounts  the amounts, each of a different currency
   * @return the value holding those amounts
   */
  private def multiOf(amounts: CurrencyAmount*): MultiCurrencyAmount =
    unwrap(MultiCurrencyAmount.of(amounts: _*))
}
