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
 * Test [[FxMatrix]].
 *
 * The order the currencies were placed in is part of the value: [[FxMatrix.currencies]] keeps it,
 * and equality, `toString` and the JSON document carry it.
 *
 * [[FxMatrix.withRate]] for a pair whose currencies are both held treats the first as the
 * reference and restates the second against it, so `withRate(GBP, USD, r)` and
 * `withRate(USD, GBP, 1 / r)` agree about `GBP/USD` and disagree about what it implies for a
 * third currency.
 *
 * A rate of `0.0` is accepted and its reciprocal is `Infinity`, which is why the JSON codec tags
 * non-finite rates and why equality compares doubles canonically. A rate sharing no currency with
 * the matrix is deferred and retried as later rates connect, and a collection that ends with it
 * still unconnected is refused with a bounded listing.
 *
 * Where construction can fail - [[FxMatrix.fromMatrix]], [[FxMatrix.withRate]],
 * [[FxMatrix.ofRates]] and [[FxMatrix.merge]] - the failure is a `Left` value rather than a thrown
 * exception, and the type is a `sealed abstract case class` with a private constructor, publishing
 * no `apply` or `copy`, so every fixture here is built through the factories and unwrapped.
 */
final class FxMatrixSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  /** The tolerance a rate the matrix derived from two others is asserted within. */
  private val TOL: Double = 1e-6

  /** A value of a type unrelated to a matrix, held at `Any` for the equality that needs one. */
  private val ANOTHER_TYPE: Any = ""

  /** The tolerance column of a table of expected rates that asserts a rate exactly. */
  private val Exactly: Double = 0d

  //-------------------------------------------------------------------------
  test("emptyMatrixCanHandleTrivialRate") {
    val matrix = FxMatrix.empty
    matrix.getCurrencies shouldBe empty
    matrix.currencies shouldBe Vector.empty
    // a currency against itself converts at one before the matrix is consulted, so the empty
    // matrix answers it
    matrix.fxRate(USD, USD) shouldBe Right(1d)
    matrix.toString shouldBe "FxMatrix[ : ]"
  }

  test("emptyMatrixCannotDoConversion") {
    val matrix = FxMatrix.empty
    matrix.getCurrencies shouldBe empty
    val rate: FailureOr[Double] = matrix.fxRate(USD, EUR)
    rate should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    rate should haveFailureMessageMatching(
      Regex.quote("No FX rate found for USD/EUR, matrix only contains rates for []"))
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
    matrix shouldBe FxMatrix.of(CurrencyPair.of(GBP, USD), 1.6d)
  }

  test("singleRateMatrixByBuilder") {
    val matrix = matrixOf(rateFor(GBP, USD, 1.6d))
    matrix.getCurrencies shouldBe Set(GBP, USD)
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, GBP, 0.625d, Exactly)))
    matrix shouldBe FxMatrix.of(GBP, USD, 1.6d)
  }

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

  test("singleRateMatrixCannotDoConversionForUnknownCurrency") {
    val matrix = matrixOf(rateFor(GBP, USD, 1.6d))
    matrix.getCurrencies shouldBe Set(GBP, USD)
    val rate: FailureOr[Double] = matrix.fxRate(USD, EUR)
    rate should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    rate should haveFailureMessageMatching(
      Regex.quote("No FX rate found for USD/EUR, matrix only contains rates for [GBP, USD]"))
  }

  /**
   * The order the three rates are placed in fixes the positions of the four currencies and so the
   * derived rates asserted here.
   */
  test("matrixCalculatesCrossRates") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d),
      rateFor(EUR, CHF, 1.2d))

    matrix.getCurrencies shouldBe Set(GBP, USD, EUR, CHF)
    matrix.currencies shouldBe Vector(GBP, USD, EUR, CHF)

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
   * The two placements refuse a disconnected rate at different moments: [[FxMatrix.ofRates]] holds
   * it back while further rates may still connect it and reports it once none did, while a rate
   * offered on its own through [[FxMatrix.withRate]] has no later offer to wait for and fails
   * where it is offered. Both are asserted, so the difference is stated rather than implied.
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
   * The complement of the test above, and why a rate is held back at all: `CHF/AUD` and the
   * `EUR/CHF` rate that follows it cannot be placed when they are offered, and the final `EUR/USD`
   * rate connects both at once. The order the retry places them in is the currency order asserted.
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
   * [[FxMatrix.withRate]] answers with a new matrix, so extending a matrix leaves the matrix the
   * rates were placed into unchanged, which is asserted at the end.
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

    matrix1.getCurrencies shouldBe Set(GBP, USD)
    matrix1.fxRate(GBP, USD) shouldBe Right(1.5d)
  }

  /**
   * Updating a rate is asymmetric. When a matrix holds both currencies of an offered rate, the
   * first is the reference and the second is restated against it: every rate involving the
   * restated currency is recomputed from the new rate and the reference currency's existing rates,
   * and every rate of the reference currency against a third currency is left alone. So
   * `withRate(GBP, USD, 1.6)` and `withRate(USD, GBP, 1 / 1.6)` agree about `GBP/USD` and disagree
   * about what it implies for `EUR`:
   *
   *   - restating `USD` against `GBP` moves `EUR/USD` and leaves `EUR/GBP` where it was;
   *   - restating `GBP` against `USD` leaves `EUR/USD` where it was and moves `EUR/GBP`.
   *
   * The expectations are written as the same arithmetic rather than as decimal literals, so what
   * is asserted is the orientation of the update and not a rounding.
   */
  test("updatingRateIsNotSymmetric") {
    val matrix1 = matrixOf(
      rateFor(GBP, USD, 1.5d),
      rateFor(EUR, USD, 1.4d))

    val matrix2 = unwrap(matrix1.withRate(GBP, USD, 1.6d))

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
        (GBP, USD, 1.6d, Exactly),
        (EUR, USD, 1.4d * (1.6d / 1.5d), TOL),
        (EUR, GBP, 1.4d / 1.5d, TOL)))

    assertRates(
      matrix3,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        (EUR, GBP, (1.4d / 1.5d) * (1.5d / 1.6d), TOL)))
  }

  /**
   * A matrix of two currencies is the one case in which the direction of an update does not
   * matter: the asymmetry acts on the rates of a '''third''' currency and there is none here, so
   * the switched update reaches the very same matrix.
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
  test("addSimpleMultipleRates") {
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

    // the same rates stated as rate values reach the same matrix; that form needs rates above
    // zero, which the zero-rated fixtures below cannot use
    val fromRateValues = FxMatrix.of(
      Vector(
        fxRateOf(GBP, USD, 1.6d),
        fxRateOf(EUR, USD, 1.4d)))
    fromRateValues shouldBe Right(matrix)
  }

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
   * Three of the seven rates cannot be placed when they are offered, so the order of the sequence
   * decides the order the currencies end up in, which is asserted.
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
   * The `JPY/CAD` rate of '''zero''' is the point of this fixture: no factory judges a rate, so
   * the zero is accepted, the reciprocal recorded for it is `Infinity`, every other pair is still
   * answered, and every rate of the zero-rated currency follows from it in both directions.
   */
  test("streamEntriesToMatrix") {
    val matrix = unwrap(FxMatrix.ofRates(streamRates))

    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (EUR, USD, 1.4d, Exactly),
        (JPY, CAD, 0d, Exactly),
        (CAD, JPY, Double.PositiveInfinity, Exactly),
        (GBP, CAD, 0d, Exactly),
        (CAD, GBP, Double.PositiveInfinity, Exactly)))

    // the diagonal is still a unit diagonal, so a zero rate leaves the value one `fromMatrix`
    // would accept
    matrix.currencies.indices.foreach(index => matrix.rates.get(index, index) shouldBe 1d)
  }

  /** The zero rate of the shared fixture shifts to zero, and its reciprocal is infinite still. */
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
  test("convertCurrencyAmount") {
    val matrix = matrixOf(
      rateFor(GBP, EUR, 1.4d),
      rateFor(GBP, USD, 1.6d))

    val amount = amountOf(GBP, 1600d)

    // an amount already in the requested currency is returned unchanged, without consulting the
    // matrix
    matrix.convert(amount, GBP) shouldBe Right(amount)

    unwrap(matrix.convert(amount, USD)) should (haveCurrency(USD) and haveAmount(2560d))
    unwrap(matrix.convert(amount, EUR)) should (haveCurrency(EUR) and haveAmount(2240d))
  }

  /**
   * A value holding nothing totals to zero of the requested currency and needs no rate to do so,
   * so every conversion here succeeds.
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
   * The terms are totalled in the alphabetical order of their currency codes, the order the
   * multi-currency value holds them in; floating-point addition is order-sensitive, so that order
   * is part of each expected total.
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
   * A merge adds only the currencies this matrix does not hold, so another matrix of the same
   * currencies with different rates adds nothing and displaces no rate: the result is this matrix.
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
    // and it is that matrix rather than a copy of it, since a merge that adds nothing has no rate
    // to place and nothing to copy
    result should be theSameInstanceAs matrix1
  }

  /**
   * Every currency the other matrix adds is placed at the rate that matrix records between it and
   * the '''first''' currency of this matrix the other also holds - `EUR` here - so the rates of
   * the result stay coherent with the rates already held.
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
   * Equality reads the currency order as well as the rates: two matrices holding the same rate for
   * the same two currencies, offered in opposite directions, hold their currencies in opposite
   * orders and are '''not''' equal.
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

    val forwards = FxMatrix.of(GBP, USD, 1.6d)
    val backwards = FxMatrix.of(USD, GBP, 1d / 1.6d)
    forwards.getCurrencies shouldBe backwards.getCurrencies
    forwards.currencies shouldBe Vector(GBP, USD)
    backwards.currencies shouldBe Vector(USD, GBP)
    forwards.equals(backwards) shouldBe false
  }

  test("equalsBad") {
    val test = matrixOf(rateFor(USD, GBP, 1.4d))

    test.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(test) shouldBe false
  }

  /**
   * Equal matrices hash alike, which is the whole of the contract: distinct matrices may legally
   * collide, so their hashes are deliberately not compared here.
   */
  test("hashCodeCoverage") {
    val m2 = matrixOf(rateFor(GBP, USD, 1.39d))
    val m3 = matrixOf(rateFor(GBP, USD, 1.39d))

    m2 shouldBe m3
    m2.hashCode shouldBe m3.hashCode
    Hash[FxMatrix].hash(m2) shouldBe Hash[FxMatrix].hash(m3)

    val placed = matrixOf(rateFor(GBP, USD, 1.39d))
    val direct = FxMatrix.of(GBP, USD, 1.39d)
    placed shouldBe direct
    placed.hashCode shouldBe direct.hashCode
  }

  //-------------------------------------------------------------------------
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

    // the instance guarantees only that equal matrices hash alike, so the hashes of `test` and
    // `emptyMatrix` are deliberately left uncompared
    Hash[FxMatrix].eqv(test, same) shouldBe true
    Hash[FxMatrix].eqv(test, emptyMatrix) shouldBe false
    Hash[FxMatrix].hash(test) shouldBe Hash[FxMatrix].hash(same)

    Show[FxMatrix].show(emptyMatrix) shouldBe "FxMatrix[ : ]"
    Show[FxMatrix].show(test) shouldBe test.toString
    // the currencies in matrix order, then the rows of the matrix
    Show[FxMatrix].show(FxMatrix.of(GBP, USD, 1.6d)) shouldBe
      "FxMatrix[GBP, USD : [1.0, 1.6],[0.625, 1.0]]"
    test.toString should startWith("FxMatrix[GBP, USD, EUR, CHF : ")
  }

  /**
   * A matrix with a rate of '''zero''' records an infinite reciprocal, and no JSON number can
   * carry an infinity, so the codec tags a non-finite rate as a string; that document is pinned
   * literally here because a round trip that lost the tag would lose the matrix.
   *
   * Decoding routes the currencies and rates through [[FxMatrix.fromMatrix]], the same factory a
   * caller's own currencies and rates go through, so a document that describes no matrix is a
   * decoding failure rather than a value that bypassed the checks.
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

    // an object of two fields: the currencies in matrix order and the rates as an array of rows
    val encoded: Json = FxMatrix.of(GBP, USD, 1.6d).asJson
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("currencies", "rates"))
    encoded.noSpaces shouldBe """{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]}"""
    encoded.as[FxMatrix] shouldBe Right(FxMatrix.of(GBP, USD, 1.6d))

    FxMatrix.empty.asJson.noSpaces shouldBe """{"currencies":[],"rates":[]}"""

    // the zero rate written as a number and its infinite reciprocal as a tagged string
    val zeroRated = FxMatrix.of(JPY, CAD, 0d)
    zeroRated.asJson.noSpaces shouldBe
      """{"currencies":["JPY","CAD"],"rates":[[1.0,0.0],["Infinity",1.0]]}"""
    cycled(zeroRated) shouldBe zeroRated
    unwrap(cycled(zeroRated).fxRate(CAD, JPY)) shouldBe Double.PositiveInfinity

    // currencies and rates stated directly reach the matrix the single-rate factory builds
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
   * [[FxMatrix.fromMatrix]] checks three things and no more: the currencies are distinct, the
   * rates are a square matrix of their number, and the diagonal is one. Reciprocity and
   * triangulation are deliberately not required, so a matrix may hold an off-diagonal pair that
   * are not reciprocals of each other, a `NaN`, either infinity and a negative zero.
   *
   * Equality compares such rates through `java.lang.Double.doubleToLongBits`, which canonicalises
   * every `NaN` payload to one value: a matrix holding a `NaN` therefore equals itself, and `-0.0`
   * stays distinct from `0.0`. The generator builds every matrix through `fromMatrix`, because
   * placing a rate computes the reciprocal for the opposite position and so cannot reach a
   * non-reciprocal state.
   *
   * The matrix stated by hand after the property holds each of those states at once - a
   * non-reciprocal pair of 2.0 and 5.0 where reciprocity would require 0.5, a negative zero, a
   * `NaN` and both infinities - so that each is present however the draws fall.
   */
  test("a matrix holding non-reciprocal and non-finite rates is a value the codec carries") {
    forAll(genEdgeFxMatrix, minSuccessful(200)) { (matrix: FxMatrix) =>
      // the document form carries every entry, the three JSON has no number for as tagged strings
      cycled(matrix) shouldBe matrix
      // a matrix holding a `NaN` still equals and hashes with the value read back, the comparison
      // being canonical rather than raw-bit
      Hash[FxMatrix].eqv(cycled(matrix), matrix) shouldBe true
      Hash[FxMatrix].hash(cycled(matrix)) shouldBe Hash[FxMatrix].hash(matrix)
      Show[FxMatrix].show(matrix) shouldBe matrix.toString
      // every generated matrix is one `fromMatrix` accepts, so these are legal states rather than
      // values that bypassed a check
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
    // the three entries no JSON number can carry, and the negative zero equality tells apart
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
    // a matrix holding a `NaN` equals itself, where the primitive `==` on that entry would not,
    // and the negative zero keeps it apart from the same matrix holding a positive one
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

    // and the one structural state that is not legal - a diagonal entry that is not one
    FxMatrix
      .fromMatrix(Vector(GBP, USD), DoubleMatrix.of(2, 2, 1d, 2d, 0.5d, Double.NaN))
      .isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * A rate sharing no currency with the matrix is held back and retried as later rates connect; a
   * collection that ends with such rates still unconnected is refused, and the refusal names at
   * most eight of them and counts the rest, so the message a caller reads does not grow with the
   * collection they offered.
   *
   * Twenty-one of the twenty-two rates offered here share no currency with the matrix or with each
   * other, so every one of them is held back until the collection is exhausted and then reported.
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
   * keeps that pair's position among the rates held back, so the refusal names `CHF/AUD` first -
   * its original position - carrying the '''second''' of its two rates.
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
   * order the retry reaches them: `NZD` from the rate that connected, then `EUR`, `CHF` and `AUD`,
   * one pass over the rates held back at a time.
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
   * A multi-currency conversion stops at the first amount it can find no rate for. The amounts are
   * held in the alphabetical order of their currency codes, so `GBP` converts, `JPY` is the first
   * currency without a rate and `NZD` is never reached: the failure names the `JPY` pair only.
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
   * currency codes, and the total is that sum exactly. Floating-point addition is neither exact
   * nor associative, so the expectation is built as the conversion builds it - from zero, each
   * amount times the rate answered for its currency, in currency order - and compared exactly, so
   * that a rearrangement of the summation fails here.
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
   * Rates stated as values place identically to the same pairs and rates stated directly. The
   * fixture is deliberately out of order - `CHF/AUD` and `EUR/CHF` cannot be placed when they are
   * offered and the final `EUR/USD` rate connects them - so the agreement covers deferral, and it
   * covers the currency order, which is part of the value the two factories must agree on.
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
   * The factory taking rate values traverses the collection it is given exactly once: the rates
   * arrive through a collection that fails the test on a second request for an iterator, and the
   * matrix it builds is the matrix the same rates in a re-traversable collection build.
   */
  test("ofRateValuesTraversesTheCollectionOnce") {
    val rateValues: Vector[FxRate] = Vector(
      fxRateOf(GBP, USD, 1.6d),
      fxRateOf(EUR, USD, 1.4d),
      fxRateOf(EUR, CHF, 1.2d))

    FxMatrix.of(new SingleUseRates(rateValues)) shouldBe FxMatrix.of(rateValues)
  }

  //-------------------------------------------------------------------------
  /**
   * A collection of rates is placed into one accumulator of rows that is sealed into a matrix
   * once, rather than into a matrix per rate, so the two routes to the same rates have to reach
   * the same value: the collection placed in one call, and the same rates placed one at a time
   * through [[FxMatrix.withRate]]. Equality of matrices reads the currencies in order and the
   * rates by bit pattern, so the comparison below is of every rate exactly and not within a
   * tolerance.
   *
   * The fixture chains thirteen currencies, so the rows the placement writes into are grown three
   * times over the course of the fold - after the second, fourth and eighth currency - and the
   * rates placed before each of those points have to survive it.
   */
  test("aCollectionOfRatesPlacesExactlyAsTheSameRatesPlacedOneAtATime") {
    val collected = unwrap(FxMatrix.ofRates(chainedRates))
    val oneByOne = unwrap(placedOneByOne(chainedRates: _*))

    collected.currencies shouldBe chainedCurrencies
    oneByOne.currencies shouldBe chainedCurrencies
    collected shouldBe oneByOne
    collected.rates shouldBe oneByOne.rates

    // every rate of the chain brings a currency in, and the rate a currency arrives at is the
    // rate given multiplied by the reference currency's rate against itself, which is one - so
    // each placed rate is held exactly, and the opposite direction is exactly its reciprocal
    chainedRates.foreach {
      case (pair, rate) =>
        withClue(s"fxRate($pair): ") {
          collected.fxRate(pair.base, pair.counter) shouldBe Right(rate)
          collected.fxRate(pair.counter, pair.base) shouldBe Right(1d / rate)
        }
    }

    // and the value is one the structural checks accept, which is what a row or column left
    // unwritten by the growth of the rows would break
    unwrapNec(FxMatrix.fromMatrix(collected.currencies, collected.rates)) shouldBe collected
  }

  /**
   * The rows a placement writes into are grown by doubling their number rather than by one per
   * currency, so a matrix of more currencies than the initial room holds is built through several
   * growths. This fixture is the case where every rate of the result is known exactly: ten
   * currencies chained at a rate of one, whose every derived rate is one as well, because
   * multiplying by one and dividing one by one are both exact. Every element of the ten by ten
   * matrix is therefore asserted, which is what a cell left behind by a growth - or a cell of the
   * room reserved beyond the currencies leaking into the result - fails.
   */
  test("aMatrixGrownPastItsInitialRoomHoldsEveryRateItWasGiven") {
    val unitRates: Vector[(CurrencyPair, Double)] =
      chainedCurrencies.take(10).sliding(2).map(pair => rateFor(pair(0), pair(1), 1d)).toVector

    val matrix = unwrap(FxMatrix.ofRates(unitRates))

    matrix.currencies shouldBe chainedCurrencies.take(10)
    matrix.rates.rowCount shouldBe 10
    matrix.rates.columnCount shouldBe 10
    matrix.currencies.indices.foreach { row =>
      matrix.currencies.indices.foreach { column =>
        withClue(s"rates.get($row, $column): ") {
          matrix.rates.get(row, column) shouldBe 1d
        }
      }
    }

    matrix shouldBe unwrap(placedOneByOne(unitRates: _*))

    // the five-currency prefix crosses the first growth and the ten-currency matrix the second,
    // so the shorter matrix is asserted as well rather than only implied by the longer one
    val five = unwrap(FxMatrix.ofRates(unitRates.take(4)))
    five.currencies shouldBe chainedCurrencies.take(5)
    five.currencies.indices.foreach(row =>
      five.currencies.indices.foreach(column => five.rates.get(row, column) shouldBe 1d))
  }

  /**
   * The rates of the chain offered in an order that holds five of them back: the first link, then
   * every second link from the third - none of which shares a currency with anything placed when
   * it is offered - and then the links that connect them. Each of those connecting offers brings a
   * currency in, and the pass over the rates held back then places the one rate that has become
   * placeable, so the rates are placed in the order they connect and not in the order they were
   * offered.
   *
   * That makes the sequence of placements the same sequence the connecting order performs, so the
   * two orders must reach the very same matrix - the same currencies in the same positions and
   * every rate equal bit for bit. This is the case a placement that shared rows between the value
   * a pass read and the value it wrote would break.
   */
  test("ratesHeldBackAndRetriedReachTheSameMatrixAsTheConnectingOrder") {
    val connecting: Vector[(CurrencyPair, Double)] = chainedRates.zipWithIndex.collect {
      case (entry, index) if index >= 2 && index % 2 == 0 => entry
    }
    val connected: Vector[(CurrencyPair, Double)] = chainedRates.zipWithIndex.collect {
      case (entry, index) if index % 2 == 1 => entry
    }
    val offered: Vector[(CurrencyPair, Double)] = chainedRates.head +: (connecting ++ connected)

    val collected = unwrap(FxMatrix.ofRates(offered))

    collected.currencies shouldBe chainedCurrencies
    collected shouldBe unwrap(FxMatrix.ofRates(chainedRates))
    chainedRates.foreach {
      case (pair, rate) =>
        withClue(s"fxRate($pair): ") {
          collected.fxRate(pair.base, pair.counter) shouldBe Right(rate)
        }
    }

    // the same rates offered the other way round hold nothing back and place their currencies in
    // the order the offers reach them: the last link is the initial pair, in the order its own
    // currencies are given, and every rate after it adds its base currency
    val reversed = unwrap(FxMatrix.ofRates(chainedRates.reverse))
    reversed.currencies shouldBe
      Vector(chainedCurrencies(11), chainedCurrencies(12)) ++ chainedCurrencies.reverse.drop(2)
    reversed shouldBe unwrap(placedOneByOne(chainedRates.reverse: _*))

    // and a rate offered with its counter currency already placed is held as one divided by the
    // reciprocal that was placed, which is the arithmetic asserted rather than a rounding of it
    chainedRates.reverse.tail.foreach {
      case (pair, rate) =>
        withClue(s"fxRate($pair): ") {
          reversed.fxRate(pair.base, pair.counter) shouldBe Right(1d / (1d / rate))
          reversed.fxRate(pair.counter, pair.base) shouldBe Right(1d / rate)
        }
    }
  }

  /**
   * A rate that becomes placeable part way through a pass over the rates held back waits for the
   * next pass rather than being placed as soon as it can be, and the currency order is where that
   * shows. Two of the rates held back here - `EUR/CHF` and `EUR/SEK` - become placeable together
   * when `EUR` arrives, and placing the first of them brings `CHF` in, which is what `CHF/AUD` was
   * waiting for. Because the pass places what it found placeable when it began, `SEK` takes its
   * position before `AUD`; a placement that examined the rates held back again after each rate it
   * placed would have brought `AUD` in first.
   */
  test("aRateThatBecomesPlaceablePartWayThroughAPassWaitsForTheNextPass") {
    val collected: FailureOr[FxMatrix] = FxMatrix.ofRates(
      Vector(
        rateFor(GBP, USD, 1.6d),
        rateFor(EUR, CHF, 1.2d), // held back
        rateFor(EUR, SEK, 9.5d), // held back
        rateFor(CHF, AUD, 1.4d), // held back, and placeable only once EUR/CHF has been placed
        rateFor(USD, EUR, 0.9d))) // places EUR, so the two EUR rates become placeable together

    collected should beSuccess
    val matrix = unwrap(collected)
    matrix.currencies shouldBe Vector(GBP, USD, EUR, CHF, SEK, AUD)

    // the rates of that placement, each exact: every one of them brought its currency in
    assertRates(
      matrix,
      Table(
        ("base", "counter", "expected", "tolerance"),
        (GBP, USD, 1.6d, Exactly),
        (USD, EUR, 0.9d, Exactly),
        (EUR, CHF, 1.2d, Exactly),
        (EUR, SEK, 9.5d, Exactly),
        (CHF, AUD, 1.4d, Exactly)))
  }

  /**
   * A rate offered for one currency against itself, where the matrix already holds that currency,
   * is an update whose reference currency and restated currency are the same one. It restates
   * that currency against its own rates as they stood: the rates read are the rates in the column
   * of the reference currency, which is exactly the column the update overwrites, so a matrix
   * that wrote the restated rates as it computed them would restate the currency partly against
   * its own new rates. The expectation is therefore written as the rule applied to a snapshot of
   * the rates taken before the update, element by element.
   *
   * The snapshot also proves the update does not reach the matrix it was placed into: the rates of
   * that matrix are asserted against the same snapshot afterwards.
   */
  test("updatingTheRateOfACurrencyAgainstItselfRestatesItFromTheRatesAsTheyStood") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.5d),
      rateFor(EUR, USD, 1.4d))
    matrix.currencies shouldBe Vector(GBP, USD, EUR)

    val size = matrix.currencies.size
    val restated = 1 // the position of USD, the currency named on both sides of the rate
    val rate = 1.7d
    val before: Vector[Vector[Double]] =
      Vector.tabulate(size, size)((row, column) => matrix.rates.get(row, column))

    val updated = unwrap(matrix.withRate(USD, USD, rate))

    updated.currencies shouldBe matrix.currencies
    (0 until size).foreach { row =>
      (0 until size).foreach { column =>
        withClue(s"rates.get($row, $column): ") {
          val expected =
            if (row == restated && column == restated) before(row)(column)
            else if (row == restated) 1d / (rate * before(column)(restated))
            else if (column == restated) rate * before(row)(restated)
            else before(row)(column)
          updated.rates.get(row, column) shouldBe expected
        }
      }
    }

    // the matrix the update was placed into holds the rates it held before it
    (0 until size).foreach(row =>
      (0 until size).foreach(column => matrix.rates.get(row, column) shouldBe before(row)(column)))
    matrix shouldBe matrixOf(rateFor(GBP, USD, 1.5d), rateFor(EUR, USD, 1.4d))

    // and the rate of the currency against itself is still one, so the result is a value the
    // structural checks accept
    updated.rates.get(restated, restated) shouldBe 1d
    unwrapNec(FxMatrix.fromMatrix(updated.currencies, updated.rates)) shouldBe updated
  }

  /**
   * The lookup of a rate is two lookups of a position and one read of the rates, and the positions
   * are numbers rather than options - so the four ways a query can turn out are asserted here:
   * both currencies held, a currency against itself whether or not the matrix holds it, and each
   * of the two positions absent. A refusal names the pair asked for and the currencies the matrix
   * holds, whichever of the two currencies is the missing one.
   */
  test("aRateIsAnsweredForThePairsTheMatrixHoldsAndRefusedNamingThePairForTheRest") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))
    val held = Regex.quote("matrix only contains rates for [GBP, USD, EUR]")

    matrix.fxRate(GBP, USD) shouldBe Right(1.6d)
    matrix.fxRate(USD, GBP) shouldBe Right(1d / 1.6d)

    // a currency against itself is one before the matrix is consulted, held or not
    matrix.fxRate(GBP, GBP) shouldBe Right(1d)
    matrix.fxRate(NZD, NZD) shouldBe Right(1d)

    val baseAbsent: FailureOr[Double] = matrix.fxRate(NZD, USD)
    baseAbsent should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    baseAbsent should haveFailureMessageMatching(
      Regex.quote("No FX rate found for NZD/USD, ") + held)

    val counterAbsent: FailureOr[Double] = matrix.fxRate(GBP, NZD)
    counterAbsent should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    counterAbsent should haveFailureMessageMatching(
      Regex.quote("No FX rate found for GBP/NZD, ") + held)

    val bothAbsent: FailureOr[Double] = matrix.fxRate(NZD, CAD)
    bothAbsent should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    bothAbsent should haveFailureMessageMatching(
      Regex.quote("No FX rate found for NZD/CAD, ") + held)

    // and the conversion of an amount carries the same refusal, since it is that lookup
    matrix.convert(amountOf(NZD, 100d), USD) should beFailureWith(
      FailureReason.CURRENCY_CONVERSION)
  }

  /**
   * The listing of the currencies a refusal names is rendered once per matrix rather than once
   * per refusal, because a caller that only tests whether a rate was found would otherwise pay
   * for a rendering proportional to the matrix on every query that misses.
   *
   * What is asserted here is that holding the rendering changed nothing a caller can observe:
   * every refusal of a matrix carries the same message it always did, whichever query produced
   * it and however many came before it, and the rendering takes no part in the value - two
   * matrices that are equal stay equal, and the JSON and the text form are untouched - because
   * it is derived from the currencies rather than held as one of them.
   */
  test("theCurrencyListingOfARefusalIsRenderedOncePerMatrixAndIsNotPartOfTheValue") {
    val matrix = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))
    val expected: String =
      Regex.quote("No FX rate found for NZD/USD, matrix only contains rates for [GBP, USD, EUR]")

    // the first refusal renders the listing; every refusal after it reads the same rendering, and
    // the message is identical whichever query missed and however often
    matrix.fxRate(NZD, USD) should haveFailureMessageMatching(expected)
    matrix.fxRate(NZD, USD) should haveFailureMessageMatching(expected)
    matrix.fxRate(NZD, USD) should haveFailureMessageMatching(expected)
    matrix.fxRate(NZD, USD) shouldBe matrix.fxRate(NZD, USD)

    // a conversion reaches the same listing through the same query
    matrix.convert(amountOf(NZD, 100d), USD) should haveFailureMessageMatching(expected)

    // an equal matrix that has never refused anything answers the identical message, so the
    // rendering is a function of the currencies and not of what the matrix has been asked
    val untouched = matrixOf(
      rateFor(GBP, USD, 1.6d),
      rateFor(EUR, USD, 1.4d))
    untouched shouldBe matrix
    untouched.fxRate(NZD, USD) should haveFailureMessageMatching(expected)

    // and the rendering is no part of the value: equality, hashing, JSON and the text form of a
    // matrix that has refused a query are those of one that has not
    Hash[FxMatrix].hash(matrix) shouldBe Hash[FxMatrix].hash(untouched)
    matrix.asJson shouldBe untouched.asJson
    Show[FxMatrix].show(matrix) shouldBe Show[FxMatrix].show(untouched)
    matrix.toString shouldBe untouched.toString
  }

  /**
   * The ordered set of currencies is a held value: two calls answer the same instance, which is
   * why the assertion is on identity rather than equality, and it iterates in matrix order.
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
   * The seven rates the two stream tests share, in an order that matters: three of them cannot be
   * placed when they are offered, so the order decides which later rate connects each of them and
   * therefore the position every currency ends up in. The `JPY/CAD` rate is zero deliberately.
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
   * `GBP/USD` rate the test offering them places first. Every rate is the same number, the fixture
   * being about how many rates can never be placed rather than their values, and the order is the
   * order they are offered in and so the order the refusal lists them in: the first eight are the
   * eight the message names.
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

  /**
   * Thirteen currencies, in the order a chain of rates over them places them.
   *
   * Thirteen is chosen so that a placement over these rates grows the rows it writes into three
   * times - the room reserved holds two currencies, and it doubles as the third, the fifth and the
   * ninth currency arrive - and so that the result is wider than the room the last of those
   * growths reserved. The first five are the currencies the older fixtures use, so the two sets
   * read alike.
   */
  private val chainedCurrencies: Vector[Currency] = Vector(
    GBP,
    USD,
    EUR,
    CHF,
    AUD,
    SEK,
    JPY,
    CAD,
    NZD,
    Currency.NOK,
    Currency.DKK,
    Currency.PLN,
    Currency.CZK)

  /**
   * Twelve rates chaining the currencies above, each connecting one new currency to the currency
   * before it, and no two of them equal.
   *
   * A chain is the fixture that pins the placement of a new currency: every rate brings a
   * currency in, so the rate offered is the rate the matrix ends up holding for that pair, and
   * offering the same rates in reverse makes every one of them but the first a rate held back and
   * retried. The rates are distinct so that a rate written into the wrong row or column cannot
   * agree with the rate that belongs there.
   */
  private val chainedRates: Vector[(CurrencyPair, Double)] =
    chainedCurrencies
      .sliding(2)
      .zipWithIndex
      .map { case (pair, index) => rateFor(pair(0), pair(1), 1.1d + 0.37d * index) }
      .toVector

  //-------------------------------------------------------------------------
  /**
   * A collection of rates that can be traversed exactly once, a second request for an iterator
   * failing the test rather than answering an empty one. The single iterator is handed out through
   * a one-element iterator of its own, which records that it was handed out without a mutable
   * field.
   */
  private final class SingleUseRates(rates: Vector[FxRate]) extends Iterable[FxRate] {

    /** The single traversal this collection admits, held until it is asked for. */
    private val traversals: Iterator[Iterator[FxRate]] = Iterator.single(rates.iterator)

    /** Answers the one iterator this collection has, or fails the test. */
    override def iterator: Iterator[FxRate] =
      if (traversals.hasNext) traversals.next()
      else fail("Expected the rates to be traversed once but a second traversal was requested")
  }

  //-------------------------------------------------------------------------
  /**
   * States one rate to place, as the pair and rate [[FxMatrix.ofRates]] and
   * [[FxMatrix.withRates]] consume. The base currency is the reference currency of an update and
   * the counter currency the one restated against it.
   */
  private def rateFor(base: Currency, counter: Currency, rate: Double): (CurrencyPair, Double) =
    CurrencyPair.of(base, counter) -> rate

  /**
   * Places the specified rates into the empty matrix one at a time, threading the outcome, and
   * answers the matrix or the failure of the first rate that could not be placed.
   *
   * A rate offered here has no later rate to wait for, so a rate with no currency in common fails
   * where it is offered rather than at the end of a collection as [[FxMatrix.ofRates]] does.
   */
  private def placedOneByOne(rates: (CurrencyPair, Double)*): FailureOr[FxMatrix] =
    rates.foldLeft[FailureOr[FxMatrix]](Right(FxMatrix.empty)) {
      case (outcome, (pair, rate)) => outcome.flatMap(matrix => matrix.withRate(pair, rate))
    }

  /**
   * Places the specified rates one at a time, failing the test if any of them could not be placed:
   * the fixture form of [[placedOneByOne]].
   */
  private def matrixOf(rates: (CurrencyPair, Double)*): FxMatrix =
    unwrap(placedOneByOne(rates: _*))

  /**
   * Asserts every expected rate of a matrix, one row of a table each, the clue naming the pair a
   * failing row queried. A row whose tolerance is [[Exactly]] asserts the rate as it stands, and
   * any other row asserts it within that tolerance.
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
   * Asserts one rate, exactly or within a tolerance whose bound is inclusive.
   *
   * A tolerance of [[Exactly]] compares the numbers as they stand, and is the only comparison that
   * can express an infinite rate - no finite tolerance spans a difference that is not finite.
   */
  private def assertRate(actual: Double, expected: Double, tolerance: Double): Assertion =
    if (tolerance == Exactly) actual shouldBe expected else actual shouldBe (expected +- tolerance)

  /**
   * Round-trips a matrix through its JSON codec, failing the test if the document it encodes to
   * does not decode.
   */
  private def cycled(matrix: FxMatrix): FxMatrix =
    matrix.asJson
      .as[FxMatrix]
      .fold(
        failure => fail(s"Expected the encoded matrix to decode but it failed with: $failure"),
        decoded => decoded)

  /**
   * Reads the value out of an outcome that is expected to have produced one, failing the test with
   * the failure's message when it did not. A test that means to assert a failure asserts the
   * outcome itself with the matchers instead.
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the operation failed with: ${failure.message}"),
      value => value)

  /**
   * Reads the value out of an accumulating outcome that is expected to have produced one, quoting
   * every message of the chain when it did not. [[FxRate.of]] and [[FxMatrix.fromMatrix]]
   * accumulate, so their outcomes carry a chain of failures rather than one.
   */
  private def unwrapNec[A](outcome: ResultNec[A]): A =
    outcome.fold(
      failures =>
        fail(
          "Expected a value but the operation failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      value => value)

  /** Builds a rate value, failing the test if the arguments describe none. */
  private def fxRateOf(base: Currency, counter: Currency, rate: Double): FxRate =
    unwrapNec(FxRate.of(base, counter, rate))

  /** Builds an amount, failing the test if the currency and number describe none. */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))

  /**
   * Builds a multi-currency value from amounts of distinct currencies, failing the test if they
   * describe none.
   */
  private def multiOf(amounts: CurrencyAmount*): MultiCurrencyAmount =
    unwrap(MultiCurrencyAmount.of(amounts: _*))
}
