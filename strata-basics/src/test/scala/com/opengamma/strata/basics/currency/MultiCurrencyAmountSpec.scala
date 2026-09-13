/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.SortedMap
import scala.util.matching.Regex

import cats.Hash
import cats.Monoid
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.CHF
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.JPY
import com.opengamma.strata.basics.currency.Currency.NZD
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[MultiCurrencyAmount]]. The contract asserted here:
 *
 *   - [[MultiCurrencyAmount.of]] refuses a collection naming one currency twice, reporting it as a
 *     `Left`, while [[MultiCurrencyAmount.total]] merges the repeats by summing them;
 *   - the amounts are held sorted by currency code, which makes aggregation independent of the
 *     order they arrived in and the JSON form byte-stable;
 *   - reading a currency the value does not hold is a `Left`; `getAmountOrZero` answers zero;
 *   - a conversion through an [[FxRateProvider]] is a `Left` when a rate is missing;
 *   - the concrete `Monoid` behaviour - identity, one aggregation, order independence.
 *
 * Every factory here reports an invalid argument as a value, so each fixture is built through
 * [[multiOf]] or [[amountOf]], which fold the outcome and fail the test naming the cause.
 */
final class MultiCurrencyAmountSpec extends AnyFunSuite with Matchers {

  private val CCY1: Currency = AUD

  private val CCY2: Currency = CAD

  private val CCY3: Currency = CHF

  private val AMT1: Double = 101d

  private val AMT2: Double = 103d

  private val AMT3: Double = 107d

  private val CA1: CurrencyAmount = amountOf(CCY1, AMT1)

  private val CA2: CurrencyAmount = amountOf(CCY2, AMT2)

  private val CA3: CurrencyAmount = amountOf(CCY3, AMT3)

  private val MTA: MultiCurrencyAmount = multiOf(CA1, CA2)

  /** A currency none of these fixtures holds an amount in. */
  private val NON_EXISTING: Currency = JPY

  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  test("test_empty") {
    assertMCA(MultiCurrencyAmount.empty)
    MultiCurrencyAmount.empty.size shouldBe 0
    MultiCurrencyAmount.empty.toString shouldBe "[]"

    Monoid[MultiCurrencyAmount].empty shouldBe MultiCurrencyAmount.empty
    Monoid[MultiCurrencyAmount].combine(MultiCurrencyAmount.empty, MTA) shouldBe MTA
    Monoid[MultiCurrencyAmount].combine(MTA, MultiCurrencyAmount.empty) shouldBe MTA
  }

  //-------------------------------------------------------------------------
  test("test_of_CurrencyDouble") {
    assertMCA(unwrap(MultiCurrencyAmount.of(CCY1, AMT1)), CA1)

    MultiCurrencyAmount.of(CCY1, Double.NaN) should beFailureWith(FailureReason.INVALID)
    MultiCurrencyAmount.of(CCY1, Double.NaN) should haveFailureMessageMatching(
      Regex.quote("Argument 'amount' must not be NaN"))
  }

  test("test_of_CurrencyDouble_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of("AUD", AMT1)""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[Currency], AMT1)""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(AMT1)""")
    assertCompiles("""MultiCurrencyAmount.of(CCY1, AMT1)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_VarArgs_empty") {
    assertMCA(multiOf())
    MultiCurrencyAmount.of() shouldBe Right(MultiCurrencyAmount.empty)
  }

  test("test_of_VarArgs") {
    assertMCA(multiOf(CA1, CA3), CA1, CA3)
  }

  test("test_of_VarArgs_duplicate") {
    val duplicated: FailureOr[MultiCurrencyAmount] =
      MultiCurrencyAmount.of(CA1, amountOf(CCY1, AMT2))
    duplicated should beFailureWith(FailureReason.INVALID)
    duplicated should haveFailureMessageMatching(Regex.quote("Currency is duplicated: AUD"))
  }

  test("test_of_VarArgs_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of(CA1, CCY2)""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[CurrencyAmount], CA2)""")
    assertCompiles("""MultiCurrencyAmount.of(CA1, CA2)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_Iterable") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, CA3)
    assertMCA(unwrap(MultiCurrencyAmount.of(iterable)), CA1, CA3)
  }

  test("test_of_Iterable_duplicate") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, amountOf(CCY1, AMT2))
    val duplicated: FailureOr[MultiCurrencyAmount] = MultiCurrencyAmount.of(iterable)
    duplicated should beFailureWith(FailureReason.INVALID)
    duplicated should haveFailureMessageMatching(Regex.quote("Currency is duplicated: AUD"))
  }

  test("test_of_Iterable_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[Iterable[CurrencyAmount]])""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(List("AUD 101"))""")
    assertCompiles("""MultiCurrencyAmount.of(List(CA1, CA2))""")
  }

  test("test_of_Iterable_containsNull") {
    assertDoesNotCompile(
      """MultiCurrencyAmount.of(List(CA1, Option.empty[CurrencyAmount], CA2))""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(List(Option(CA1), Option(CA2)))""")
    assertCompiles("""MultiCurrencyAmount.of(List(Option(CA1), Option(CA2)).flatten)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_Map") {
    val map: Map[Currency, Double] = Map(CCY1 -> AMT1, CCY3 -> AMT3)
    assertMCA(unwrap(MultiCurrencyAmount.of(map)), CA1, CA3)

    // a map cannot name a currency twice, so the only failure left for this factory is a number
    // no amount may hold
    val notNumbers: Map[Currency, Double] = Map(CCY1 -> Double.NaN)
    MultiCurrencyAmount.of(notNumbers) should beFailureWith(FailureReason.INVALID)
  }

  test("test_of_Map_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[Map[Currency, Double]])""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(Map("AUD" -> AMT1))""")
    assertCompiles("""MultiCurrencyAmount.of(Map(CCY1 -> AMT1))""")
  }

  //-------------------------------------------------------------------------
  test("test_total_Iterable") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, CA3)
    assertMCA(MultiCurrencyAmount.total(iterable), CA1, CA3)
  }

  test("test_total_Iterable_duplicate") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, amountOf(CCY1, AMT2), CA2)
    assertMCA(MultiCurrencyAmount.total(iterable), amountOf(CCY1, AMT1 + AMT2), CA2)
    MultiCurrencyAmount.total(iterable).getAmountOrZero(CCY1).amount shouldBe 204d

    MultiCurrencyAmount.of(iterable) should beFailureWith(FailureReason.INVALID)
  }

  test("test_total_Iterable_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.total(Option.empty[Iterable[CurrencyAmount]])""")
    assertDoesNotCompile("""MultiCurrencyAmount.total()""")
    assertCompiles("""MultiCurrencyAmount.total(List(CA1, CA2))""")
  }

  test("test_total_Iterable_containsNull") {
    assertDoesNotCompile(
      """MultiCurrencyAmount.total(List(CA1, Option.empty[CurrencyAmount], CA2))""")
    assertDoesNotCompile("""MultiCurrencyAmount.total(List(Option(CA1), Option(CA2)))""")
    assertCompiles("""MultiCurrencyAmount.total(List(Option(CA1), Option(CA2)).flatten)""")
  }

  //-------------------------------------------------------------------------
  test("test_collector") {
    val amount: List[CurrencyAmount] =
      List(amountOf(CCY1, 100d), amountOf(CCY1, 150d), amountOf(CCY2, 100d))
    val expected: MultiCurrencyAmount = multiOf(amountOf(CCY1, 250d), amountOf(CCY2, 100d))

    MultiCurrencyAmount.total(amount) shouldBe expected
    Monoid[MultiCurrencyAmount].combineAll(amount.map(single => multiOf(single))) shouldBe expected
    assertMCA(MultiCurrencyAmount.total(amount), amountOf(CCY1, 250d), amountOf(CCY2, 100d))
  }

  /** The amounts of this input are whole numbers, which double addition reorders exactly. */
  test("test_collector_parallel") {
    val amount: List[CurrencyAmount] =
      List(amountOf(CCY1, 100d), amountOf(CCY1, 150d), amountOf(CCY2, 100d))
    val reordered: List[CurrencyAmount] = List(amount(2), amount(0), amount(1))
    val reversed: List[CurrencyAmount] = amount.reverse
    val expected: MultiCurrencyAmount = multiOf(amountOf(CCY1, 250d), amountOf(CCY2, 100d))

    MultiCurrencyAmount.total(reordered) shouldBe expected
    MultiCurrencyAmount.total(reversed) shouldBe expected
    Monoid[MultiCurrencyAmount].combineAll(
      reordered.map(single => multiOf(single))) shouldBe expected
    Monoid[MultiCurrencyAmount].combineAll(
      reversed.map(single => multiOf(single))) shouldBe expected

    // and two partial totals combined equal one total of everything, so the aggregate does not
    // depend on how the input was grouped either
    Monoid[MultiCurrencyAmount].combine(
      MultiCurrencyAmount.total(amount.take(1)),
      MultiCurrencyAmount.total(amount.drop(1))) shouldBe expected
  }

  test("test_collector_null") {
    assertDoesNotCompile(
      """MultiCurrencyAmount.total(List(amountOf(CCY1, 100d), Option.empty[CurrencyAmount]))""")
    assertDoesNotCompile("""Monoid[MultiCurrencyAmount].combineAll(List(CA1, CA2))""")
    assertCompiles(
      """Monoid[MultiCurrencyAmount].combineAll(List(MTA, MultiCurrencyAmount.empty))""")
  }

  //-------------------------------------------------------------------------
  test("test_beanBuilder") {
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)
    assertMCA(test, CA1, CA2, CA3)

    // every factory builds the same value, in any input order
    unwrap(MultiCurrencyAmount.of(List(CA3, CA1, CA2))) shouldBe test
    unwrap(
      MultiCurrencyAmount.of(Map(CCY3 -> AMT3, CCY1 -> AMT1, CCY2 -> AMT2))) shouldBe test
    MultiCurrencyAmount.total(List(CA2, CA3, CA1)) shouldBe test
  }

  test("test_beanBuilder_invalid") {
    val invalid: FailureOr[MultiCurrencyAmount] =
      MultiCurrencyAmount.of(List(CA1, CA2, amountOf(CA1.currency, AMT3)))
    invalid should beFailureWith(FailureReason.INVALID)
    invalid should haveFailureMessageMatching(Regex.quote("Currency is duplicated: AUD"))
  }

  //-------------------------------------------------------------------------
  test("test_contains_null") {
    assertDoesNotCompile("""MTA.contains("AUD")""")
    assertDoesNotCompile("""MTA.contains(Option.empty[Currency])""")
    assertDoesNotCompile("""MTA.contains()""")
    assertCompiles("""MTA.contains(CCY1)""")
  }

  //-------------------------------------------------------------------------
  test("test_plus_CurrencyDouble_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(AUD, 3d)
    assertMCA(test, cb, amountOf(AUD, 120d))
  }

  test("test_plus_CurrencyDouble_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(NZD, 3d)
    assertMCA(test, ca, cb, amountOf(NZD, 3d))
  }

  test("test_plus_CurrencyDouble_null") {
    assertDoesNotCompile("""MTA.plus("AUD", 1d)""")
    assertDoesNotCompile("""MTA.plus(Option.empty[Currency], 1d)""")
    assertCompiles("""MTA.plus(CCY1, 1d)""")
  }

  //-------------------------------------------------------------------------
  test("test_plus_CurrencyAmount_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(cc)
    assertMCA(test, cb, amountOf(AUD, 120d))
  }

  test("test_plus_CurrencyAmount_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(cc)
    assertMCA(test, ca, cb, cc)
  }

  test("test_plus_CurrencyAmount_null") {
    assertDoesNotCompile("""MTA.plus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""MTA.plus(CCY1)""")
    assertCompiles("""MTA.plus(CA1)""")
  }

  //-------------------------------------------------------------------------
  test("test_plus_MultiCurrencyAmount_mergeAndAdd") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val cd: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf(cc, cd)
    val test: MultiCurrencyAmount = mc1.plus(mc2)
    assertMCA(test, cb, cd, amountOf(AUD, 120d))
  }

  test("test_plus_MultiCurrencyAmount_empty") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf()
    val test: MultiCurrencyAmount = mc1.plus(mc2)
    assertMCA(test, ca, cb)
  }

  test("test_plus_MultiCurrencyAmount_null") {
    assertDoesNotCompile("""MTA.plus(Option.empty[MultiCurrencyAmount])""")
    assertDoesNotCompile("""MTA.plus()""")
    assertCompiles("""MTA.plus(MultiCurrencyAmount.empty)""")
  }

  //-------------------------------------------------------------------------
  test("test_minus_CurrencyDouble_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(AUD, 3d)
    assertMCA(test, cb, amountOf(AUD, 114d))
  }

  test("test_minus_CurrencyDouble_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(NZD, 3d)
    // a currency the value does not hold is inserted negated
    assertMCA(test, ca, cb, amountOf(NZD, -3d))
  }

  test("test_minus_CurrencyDouble_null") {
    assertDoesNotCompile("""MTA.minus("AUD", 1d)""")
    assertDoesNotCompile("""MTA.minus(Option.empty[Currency], 1d)""")
    assertCompiles("""MTA.minus(CCY1, 1d)""")
  }

  //-------------------------------------------------------------------------
  test("test_minus_CurrencyAmount_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(cc)
    assertMCA(test, cb, amountOf(AUD, 114d))
  }

  test("test_minus_CurrencyAmount_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(cc)
    assertMCA(test, ca, cb, cc.negated)
  }

  test("test_minus_CurrencyAmount_null") {
    assertDoesNotCompile("""MTA.minus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""MTA.minus(CCY1)""")
    assertCompiles("""MTA.minus(CA1)""")
  }

  //-------------------------------------------------------------------------
  test("test_minus_MultiCurrencyAmount_mergeAndAdd") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val cd: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf(cc, cd)
    val test: MultiCurrencyAmount = mc1.minus(mc2)
    assertMCA(test, cb, cd.negated, amountOf(AUD, 114d))
  }

  test("test_minus_MultiCurrencyAmount_empty") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf()
    val test: MultiCurrencyAmount = mc1.minus(mc2)
    assertMCA(test, ca, cb)
  }

  test("test_minus_MultiCurrencyAmount_null") {
    assertDoesNotCompile("""MTA.minus(Option.empty[MultiCurrencyAmount])""")
    assertDoesNotCompile("""MTA.minus()""")
    assertCompiles("""MTA.minus(MultiCurrencyAmount.empty)""")
  }

  //-------------------------------------------------------------------------
  test("test_multipliedBy") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.multipliedBy(2.5d)
    assertMCA(test, CA1.multipliedBy(2.5d), CA2.multipliedBy(2.5d))
  }

  //-------------------------------------------------------------------------
  /**
   * An amount of zero negates to a positive zero, so a value holding only zeros negates to itself.
   * The sign of a raw `Double` read back out of the value is invisible to `==`, which is why it is
   * compared through `doubleToLongBits` here.
   */
  test("test_negated") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.negated
    assertMCA(test, CA1.negated, CA2.negated)

    val zeros: MultiCurrencyAmount = multiOf(CurrencyAmount.zero(USD), CurrencyAmount.zero(EUR))
    zeros.negated shouldBe zeros
    multiOf(amountOf(USD, -0d), amountOf(EUR, -0d)).negated shouldBe zeros

    zeros.negated.toMap.valuesIterator.foreach { amount =>
      withClue(s"$amount: ") {
        java.lang.Double.doubleToLongBits(amount) shouldBe java.lang.Double.doubleToLongBits(0d)
      }
    }
    java.lang.Double.doubleToLongBits(
      multiOf(amountOf(USD, -0d)).negated.getAmountOrZero(USD).amount) shouldBe
      java.lang.Double.doubleToLongBits(0d)
  }

  //-------------------------------------------------------------------------
  test("test_mapAmounts") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.mapAmounts(amount => amount * 2.5d + 1d)
    assertMCA(
      test,
      CA1.mapAmount(amount => amount * 2.5d + 1d),
      CA2.mapAmount(amount => amount * 2.5d + 1d))
  }

  test("test_mapAmounts_null") {
    assertDoesNotCompile("""MTA.mapAmounts()""")
    assertDoesNotCompile("""MTA.mapAmounts((amount: Double) => amount.toString)""")
    assertCompiles("""MTA.mapAmounts((amount: Double) => amount * 2d)""")
  }

  //-------------------------------------------------------------------------
  /**
   * The whole difference from `mapAmounts`: two amounts mapped onto one currency are added rather
   * than reported as a duplicate.
   */
  test("test_mapCurrencyAmounts") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.mapCurrencyAmounts(_ => amountOf(CCY3, 1d))
    assertMCA(test, amountOf(CCY3, 2d))
  }

  test("test_mapCurrencyAmounts_null") {
    assertDoesNotCompile("""MTA.mapCurrencyAmounts()""")
    assertDoesNotCompile("""MTA.mapCurrencyAmounts((amount: CurrencyAmount) => amount.amount)""")
    assertCompiles("""MTA.mapCurrencyAmounts((amount: CurrencyAmount) => amount.negated)""")
  }

  //-------------------------------------------------------------------------
  test("test_stream") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount =
      MultiCurrencyAmount.total(base.iterator.map(ca => ca.mapAmount(amount => amount * 3d)).toList)
    assertMCA(
      test,
      CA1.mapAmount(amount => amount * 3d),
      CA2.mapAmount(amount => amount * 3d))

    base.iterator.toList shouldBe List(CA1, CA2)
    multiOf(CA2, CA1).iterator.toList shouldBe List(CA1, CA2)
    multiOf(CA3, CA2, CA1).iterator.toList shouldBe List(CA1, CA2, CA3)
    multiOf(CA3, CA2, CA1).getAmounts.toList shouldBe List(CA1, CA2, CA3)
    MultiCurrencyAmount.empty.iterator.toList shouldBe List.empty[CurrencyAmount]
  }

  //-------------------------------------------------------------------------
  /** The map is the `SortedMap[Currency, Double]` the value holds, handed back as it is. */
  test("test_toMap") {
    val test: SortedMap[Currency, Double] = multiOf(CA1, CA2).toMap
    test.size shouldBe 2
    test.contains(CA1.currency) shouldBe true
    test.contains(CA2.currency) shouldBe true
    test.get(CA1.currency) shouldBe Some(CA1.amount)
    test.get(CA2.currency) shouldBe Some(CA2.amount)
    test.keysIterator.toList shouldBe List(CCY1, CCY2)

    multiOf(CA3, CA2, CA1).toMap.keysIterator.toList shouldBe List(CCY1, CCY2, CCY3)
    multiOf(CA3, CA2, CA1).toMap.valuesIterator.toList shouldBe List(AMT1, AMT2, AMT3)
    MultiCurrencyAmount.empty.toMap.isEmpty shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * A value holding exactly one amount already in the requested currency needs no rate, so it
   * converts even under `noConversion()`, which refuses every lookup including the identity.
   */
  test("test_convertedTo_rateProvider_noConversionSize1") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((base, counter) =>
      Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter")))
    val test: MultiCurrencyAmount = multiOf(CA2)
    test.convertedTo(CCY2, provider) should haveValue(CA2)
    test.convertedTo(CCY2, FxRateProvider.noConversion()) should haveValue(CA2)
  }

  test("test_convertedTo_rateProvider_conversionSize1") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((base, counter) =>
      if (base == CCY1 && counter == CCY2) {
        Right(2.5d)
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount = multiOf(CA1)
    test.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    test.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, 252.5d))

    test.convertedTo(CCY3, provider) should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    test.convertedTo(CCY3, provider) should haveFailureMessageMatching(
      Regex.quote("No rate is available for AUD/CHF"))
  }

  /**
   * A value holding more than one amount asks the provider for a rate for every amount it holds,
   * the one already in the requested currency included, so the provider here carries the identity
   * case and one that refuses the identity fails the conversion. The total is summed from zero in
   * currency-code order.
   */
  test("test_convertedTo_rateProvider_conversionSize2") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((base, counter) =>
      if (base == counter) {
        Right(1d)
      } else if (base == CCY1 && counter == CCY2) {
        Right(2.5d)
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount = multiOf(CA1, CA2)
    test.convertedTo(CCY2, provider) should
      haveValue(unwrap(CA2.plus(amountOf(CCY2, AMT1 * 2.5d))))
    test.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, 355.5d))

    test.convertedTo(CCY2, FxRateProvider.noConversion()) should
      beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // a value holding nothing converts to zero of the target, with no rate needed
    MultiCurrencyAmount.empty.convertedTo(CCY2, FxRateProvider.noConversion()) should
      haveValue(CurrencyAmount.zero(CCY2))
  }

  //-----------------------------------------------------------------------
  /**
   * The JSON form is one `amounts` array in currency order, so a value built in another order
   * encodes to the same bytes; a document naming one currency twice is refused, because decoding
   * routes through [[MultiCurrencyAmount.of]] rather than letting the last amount win.
   */
  test("test_serialization") {
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe
      """{"amounts":[{"currency":"AUD","amount":101.0},{"currency":"CAD","amount":103.0},""" +
        """{"currency":"CHF","amount":107.0}]}"""
    encoded.as[MultiCurrencyAmount] shouldBe Right(test)

    multiOf(CA3, CA1, CA2).asJson.noSpaces shouldBe encoded.noSpaces
    Json
      .obj("amounts" -> Json.arr(CA3.asJson, CA2.asJson, CA1.asJson))
      .as[MultiCurrencyAmount] shouldBe Right(test)

    MultiCurrencyAmount.empty.asJson.noSpaces shouldBe """{"amounts":[]}"""
    Json.obj("amounts" -> Json.arr(CA1.asJson, CA1.asJson)).as[MultiCurrencyAmount].isLeft shouldBe
      true
  }

  /**
   * Inequality is stated through `eqv` rather than through the hashes: the instance guarantees
   * that equal values hash equally and says nothing about unequal ones, so asserting that two
   * distinct values hash differently would make a correct hash fail here.
   */
  test("coverage") {
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)
    val same: MultiCurrencyAmount = multiOf(CA3, CA2, CA1)
    val other: MultiCurrencyAmount = multiOf(CA1, CA2)

    Hash[MultiCurrencyAmount].eqv(test, test) shouldBe true
    Hash[MultiCurrencyAmount].eqv(test, same) shouldBe true
    Hash[MultiCurrencyAmount].eqv(test, other) shouldBe false
    Hash[MultiCurrencyAmount].eqv(other, MultiCurrencyAmount.empty) shouldBe false
    Hash[MultiCurrencyAmount].hash(test) shouldBe Hash[MultiCurrencyAmount].hash(same)
    test.equals(ANOTHER_TYPE) shouldBe false

    Show[MultiCurrencyAmount].show(test) shouldBe test.toString
    Show[MultiCurrencyAmount].show(test) shouldBe "[AUD 101, CAD 103, CHF 107]"
    Show[MultiCurrencyAmount].show(MultiCurrencyAmount.empty) shouldBe "[]"
    Show[MultiCurrencyAmount].show(
      multiOf(amountOf(GBP, 100d), amountOf(USD, 200d))) shouldBe "[GBP 100, USD 200]"
  }

  //-------------------------------------------------------------------------
  /**
   * Every route that merges - [[MultiCurrencyAmount.total]], the three
   * [[MultiCurrencyAmount.plus]] members, [[MultiCurrencyAmount.mapCurrencyAmounts]] and
   * `Monoid[MultiCurrencyAmount].combineAll` - adds what arrives to what has accumulated, in
   * arrival order. The three numbers are chosen so the two associations of the same sum differ in
   * their last bit - `0.6000000000000001` left to right against `0.6` right to left - so a route
   * that reordered the additions, or seeded the sum with a zero instead of the first amount, fails
   * here.
   */
  test("the aggregate adds a repeated currency left to right, to the last bit") {
    val first: CurrencyAmount = amountOf(CCY1, 0.1d)
    val second: CurrencyAmount = amountOf(CCY1, 0.2d)
    val third: CurrencyAmount = amountOf(CCY1, 0.3d)

    val leftToRight: Double = 0.6000000000000001d
    (0.1d + 0.2d) + 0.3d shouldBe leftToRight
    0.1d + (0.2d + 0.3d) shouldBe 0.6d
    leftToRight should not be 0.6d

    bitsOf(MultiCurrencyAmount.total(List(first, second, third)), CCY1) shouldBe bits(leftToRight)
    bitsOf(multiOf(first).plus(CCY1, 0.2d).plus(CCY1, 0.3d), CCY1) shouldBe bits(leftToRight)
    bitsOf(multiOf(first).plus(second).plus(third), CCY1) shouldBe bits(leftToRight)

    bitsOf(multiOf(first).plus(multiOf(second)).plus(multiOf(third)), CCY1) shouldBe
      bits(leftToRight)
    bitsOf(
      Monoid[MultiCurrencyAmount].combineAll(
        List(multiOf(first), multiOf(second), multiOf(third))),
      CCY1) shouldBe bits(leftToRight)

    // the mapping that moves three currencies onto one merges in currency-code order
    val spread: MultiCurrencyAmount =
      multiOf(amountOf(CCY1, 0.1d), amountOf(CCY2, 0.2d), amountOf(CCY3, 0.3d))
    bitsOf(spread.mapCurrencyAmounts(amount => amountOf(USD, amount.amount)), USD) shouldBe
      bits(leftToRight)
  }

  /**
   * `combineAll` aggregates a collection of whole values in one pass while `combine` adds two at a
   * time, and both are published, so the two must not be able to disagree. The values overlap in
   * two currencies and hold numbers whose sums are inexact in binary, and the comparison is made
   * on `doubleToLongBits` entry by entry, so a differing number of intermediate roundings shows up
   * rather than being absorbed as an approximate match.
   */
  test("combineAll agrees with folding combine over the same values, entry by entry") {
    val additive: Monoid[MultiCurrencyAmount] = Monoid[MultiCurrencyAmount]
    val values: List[MultiCurrencyAmount] = List(
      multiOf(amountOf(CCY1, 0.1d), amountOf(CCY2, 1.1d)),
      multiOf(amountOf(CCY1, 0.2d), amountOf(CCY3, 2.2d)),
      multiOf(amountOf(CCY2, 0.3d), amountOf(CCY3, 3.3d)),
      multiOf(amountOf(CCY1, 0.4d)))

    val aggregated: MultiCurrencyAmount = additive.combineAll(values)
    val folded: MultiCurrencyAmount = values.foldLeft(additive.empty)(additive.combine)

    aggregated.toMap.keysIterator.toList shouldBe folded.toMap.keysIterator.toList
    folded.toMap.foreach { case (currency, amount) =>
      withClue(s"$currency: ") {
        bitsOf(aggregated, currency) shouldBe bits(amount)
      }
    }
    aggregated shouldBe folded
  }

  /**
   * The aggregation adds numbers rather than amounts, so the numeric invariant of an amount has to
   * be reached on that path too: two infinities of opposite sign sum to a value that is not a
   * number, which every merging route rejects with the wording [[CurrencyAmount]] reports, while a
   * negative zero arriving from outside is normalised away.
   */
  test("the aggregate keeps the numeric invariant of an amount") {
    val positive: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    val negative: CurrencyAmount = amountOf(CCY1, Double.NegativeInfinity)
    val notNumber: String = "Argument 'amount' must not be NaN"

    the[IllegalArgumentException] thrownBy MultiCurrencyAmount.total(
      List(positive, negative)) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive).plus(
      CCY1,
      Double.NegativeInfinity) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive).plus(
      negative) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive).plus(
      multiOf(negative)) should have message notNumber
    the[IllegalArgumentException] thrownBy Monoid[MultiCurrencyAmount].combineAll(
      List(multiOf(positive), multiOf(negative))) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive, amountOf(CCY2, 1d)).mapAmounts(
      amount => amount - amount) should have message notNumber

    // an infinity on its own is an amount, so the invariant fires on the sum and not before
    bitsOf(MultiCurrencyAmount.total(List(positive, positive)), CCY1) shouldBe
      bits(Double.PositiveInfinity)

    bitsOf(unwrap(MultiCurrencyAmount.of(CCY1, -0d)), CCY1) shouldBe bits(0d)
    bitsOf(multiOf(CA1).mapAmounts(_ => -0d), CCY1) shouldBe bits(0d)
    bitsOf(unwrap(MultiCurrencyAmount.of(Map(CCY1 -> -0d))), CCY1) shouldBe bits(0d)
  }

  //-------------------------------------------------------------------------
  /**
   * A single unavailable rate decides the whole conversion, so the traversal stops there: the
   * provider refuses the second currency in code order and counts two lookups for three amounts,
   * and the failure carried out is the one reported for that currency rather than for a later one.
   */
  test("convertedTo asks for no rate after the first one the provider cannot supply") {
    val provider: CountingRateProvider = new CountingRateProvider((base, counter) =>
      if (base == CCY1 && counter == USD) {
        Right(2.5d)
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)

    val converted: FailureOr[CurrencyAmount] = test.convertedTo(USD, provider)
    converted should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    converted should haveFailureMessageMatching(Regex.quote("No rate is available for CAD/USD"))
    provider.callCount shouldBe 2
  }

  /**
   * The converted amounts are added from zero in currency-code order, and the rates are chosen so
   * that the sum in that order, `0.6000000000000001`, differs in its last bit from the sum in the
   * reverse order, `0.6`. The count pins the other half: one lookup per amount the value holds.
   */
  test("convertedTo totals the converted amounts in currency-code order, to the last bit") {
    val rates: Map[Currency, Double] = Map(CCY1 -> 0.1d, CCY2 -> 0.2d, CCY3 -> 0.3d)
    val provider: CountingRateProvider = new CountingRateProvider((base, counter) =>
      if (counter == USD) {
        rates.get(base).toRight(Failure.CurrencyConversion(s"No rate is available for $base/USD"))
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount =
      multiOf(amountOf(CCY1, 1d), amountOf(CCY2, 1d), amountOf(CCY3, 1d))

    ((0d + 0.1d) + 0.2d) + 0.3d shouldBe 0.6000000000000001d
    ((0d + 0.3d) + 0.2d) + 0.1d shouldBe 0.6d

    val converted: CurrencyAmount = unwrap(test.convertedTo(USD, provider))
    converted.currency shouldBe USD
    bits(converted.amount) shouldBe bits(0.6000000000000001d)
    provider.callCount shouldBe 3
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that a value holds exactly the expected amounts - its size, its amounts, its
   * currencies - and that [[NON_EXISTING]] is absent from it, reported by `getAmount` and read as
   * zero by `getAmountOrZero`.
   */
  private def assertMCA(actual: MultiCurrencyAmount, expected: CurrencyAmount*): Assertion = {
    actual.size shouldBe expected.size
    actual.getAmounts.size shouldBe expected.size
    actual.getAmounts.toList should contain theSameElementsAs expected
    actual.iterator.toList should contain theSameElementsAs expected

    expected.foreach { expectedAmount =>
      withClue(s"$expectedAmount: ") {
        actual.contains(expectedAmount.currency) shouldBe true
        actual.getAmount(expectedAmount.currency) should haveValue(expectedAmount)
        actual.getAmountOrZero(expectedAmount.currency) shouldBe expectedAmount
      }
    }
    actual.getCurrencies shouldBe expected.map(amount => amount.currency).toSet

    actual.contains(NON_EXISTING) shouldBe false
    actual.getAmount(NON_EXISTING) should beFailureWith(FailureReason.INVALID)
    actual.getAmount(NON_EXISTING) should haveFailureMessageMatching(
      Regex.quote(s"Unknown currency $NON_EXISTING"))
    actual.getAmountOrZero(NON_EXISTING) shouldBe CurrencyAmount.zero(NON_EXISTING)
  }

  /**
   * Reads the value out of an outcome, folding rather than opening it with a partial accessor, so
   * that a fixture which fails to build is reported as a test failure naming the reason.
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the factory failed with: ${failure.message}"),
      value => value)

  /**
   * Builds a value from amounts of distinct currencies, failing the test if they describe none; a
   * test that means to aggregate a repeated currency calls [[MultiCurrencyAmount.total]] instead.
   */
  private def multiOf(amounts: CurrencyAmount*): MultiCurrencyAmount =
    unwrap(MultiCurrencyAmount.of(amounts: _*))

  /** Builds an amount, failing the test if the currency and number describe none. */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))

  /**
   * Returns `doubleToLongBits` of the amount a value holds of a currency, failing the test naming
   * the currency if it holds none rather than reading an absent amount as zero. The pins compare
   * bits because `shouldBe` on two sums that differ in their last bit reports them as
   * "0.6 was not equal to 0.6".
   */
  private def bitsOf(value: MultiCurrencyAmount, currency: Currency): Long =
    bits(
      value.toMap.getOrElse(
        currency,
        fail(s"Expected an amount of $currency but the value held $value")))

  private def bits(amount: Double): Long = java.lang.Double.doubleToLongBits(amount)

  /**
   * A rate provider that answers with a function and counts how often it was asked, which is what
   * the conversion pins observe: both how far the traversal ran and how often each amount was
   * looked up are statements about the number of lookups rather than about the result. The counter
   * is an `AtomicInteger`, and each test builds a fresh provider, so no count depends on the
   * order the tests ran in.
   */
  private final class CountingRateProvider(rates: (Currency, Currency) => Either[Failure, Double])
      extends FxRateProvider {

    private val calls: AtomicInteger = new AtomicInteger(0)

    override def fxRate(
        baseCurrency: Currency,
        counterCurrency: Currency): Either[Failure, Double] = {
      // Bound to a wildcard because `-Wvalue-discard` rejects discarding the new count; the tests
      // read it through callCount.
      val _ = calls.incrementAndGet()
      rates(baseCurrency, counterCurrency)
    }

    def callCount: Int = calls.get()
  }
}
