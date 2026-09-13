/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

import scala.util.matching.Regex

import cats.Eq
import cats.Hash
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
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.Outcome
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[MultiCurrencyAmountArray]].
 *
 * The value holds one [[DoubleArray]] per currency plus a size of its own rather than a collection
 * of amounts, and exactly two things make that layout observable:
 *
 *   - '''zero padding''': a currency the run holds is held for every index, so an index naming no
 *     value for it reads `0.0` rather than a gap (`test_of`, `test_get`).
 *   - '''the stored size''': a run of amounts naming no currency at all has an empty map of
 *     values, so only a stored field can answer its size (`test_empty_amounts`,
 *     `test_of_function_empty_amounts`, `serializeSize`).
 *
 * A run holds its currencies in code order, which is the order of the rendering, the JSON, the
 * rates a conversion asks for and the reasons a failed aggregation reports. Failures are `Left`
 * values, so fixtures unwrap once through [[unwrap]]; an index outside a run raises from the array
 * it reads rather than reporting.
 */
final class MultiCurrencyAmountArraySpec extends AnyFunSuite with Matchers {

  /** Three amounts, each naming the same three currencies, in index order. */
  private val VALUES_ARRAY: MultiCurrencyAmountArray =
    MultiCurrencyAmountArray.of(
      List(
        multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d)),
        multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
        multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))))

  /**
   * The runs of single-currency amounts the aggregation tests total: two name USD and two GBP, so
   * the aggregation has both a currency to carry through and a currency to add up. Every value is
   * a whole number, so the sums are exact and the total can be asserted for equality.
   */
  private val CollectorArrays: List[CurrencyAmountArray] =
    List(
      CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
      CurrencyAmountArray.of(USD, DoubleArray.of(5d, 6d, 7d)),
      CurrencyAmountArray.of(EUR, DoubleArray.of(2d, 4d, 6d)),
      CurrencyAmountArray.of(GBP, DoubleArray.of(11d, 12d, 13d)),
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 2d, 3d)))

  /** The aggregate of [[CollectorArrays]]. */
  private val CollectorExpected: MultiCurrencyAmountArray =
    arrayOf(
      USD -> DoubleArray.of(15d, 26d, 37d),
      EUR -> DoubleArray.of(2d, 4d, 6d),
      GBP -> DoubleArray.of(12d, 14d, 16d))

  /**
   * The tolerance of the conversion into a currency the run already holds: that expectation is a
   * sum of quotients while the conversion multiplies by the reciprocal rate.
   */
  private val Tolerance: Double = 1e-6d

  /** The wording reported when two runs that have to be combined differ in size. */
  private val SizeMismatchMessage: String = "Sizes must be equal, this size is 2, other size is 3"

  /** The prefix reported for arrays that disagree about their length. */
  private val ArrayLengthPrefix: String = "Arrays must have the same size"

  /** The whole message raised when the function form is handed a size no run can have. */
  private val NegativeSizeMessage: String =
    "Argument 'size' must not be negative but has value -1"

  /** A value of an unrelated type, held at `Any` so the equality comparison compiles. */
  private val ForeignValue: Any = ""

  //-------------------------------------------------------------------------
  /**
   * Asserts the factories that read amounts, and with them the zero padding of the type.
   *
   * Ragged input produces a full-length array for every currency any amount names, `0.0` where an
   * amount does not name it. A currency no amount named is not held at all and is reported rather
   * than answered with zeroes, which is what keeps it distinguishable from one held and zero
   * everywhere.
   */
  test("test_of") {
    VALUES_ARRAY.getValues(GBP) should haveValue(DoubleArray.of(20d, 21d, 22d))
    VALUES_ARRAY.getValues(USD) should haveValue(DoubleArray.of(30d, 32d, 33d))
    VALUES_ARRAY.getValues(EUR) should haveValue(DoubleArray.of(40d, 43d, 44d))
    VALUES_ARRAY.size shouldBe 3
    VALUES_ARRAY.getCurrencies shouldBe Set(GBP, USD, EUR)

    val raggedArray: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        List(
          multiOf(amountOf(EUR, 4d)),
          multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
          multiOf(amountOf(EUR, 44d))))

    raggedArray.size shouldBe 3
    raggedArray.getCurrencies shouldBe Set(GBP, USD, EUR)
    raggedArray.getValues(GBP) should haveValue(DoubleArray.of(0d, 21d, 0d))
    raggedArray.getValues(USD) should haveValue(DoubleArray.of(0d, 32d, 0d))
    raggedArray.getValues(EUR) should haveValue(DoubleArray.of(4d, 43d, 44d))

    raggedArray.get(0) shouldBe multiOf(amountOf(EUR, 4d), amountOf(GBP, 0d), amountOf(USD, 0d))

    val unknown: FailureOr[DoubleArray] = raggedArray.getValues(AUD)
    unknown should beFailureWith(FailureReason.INVALID)
    unknown should haveFailureMessageMatching(Regex.quote("No values available for AUD"))

    MultiCurrencyAmountArray.of(
      multiOf(amountOf(EUR, 4d)),
      multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
      multiOf(amountOf(EUR, 44d))) shouldBe raggedArray
  }

  /** A run of two empty amounts holds no values, so only its stored size says how long it is. */
  test("test_empty_amounts") {
    val array: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(MultiCurrencyAmount.empty, MultiCurrencyAmount.empty)
    array.size shouldBe 2
    array.get(0) shouldBe MultiCurrencyAmount.empty
    array.get(1) shouldBe MultiCurrencyAmount.empty
    array.values shouldBe Map.empty[Currency, DoubleArray]
    array.getCurrencies shouldBe Set.empty[Currency]
  }

  /**
   * Asserts the function form and the one evaluation per index it documents.
   *
   * Values are held per currency, so a factory that built each currency's array by calling the
   * function again would evaluate it once per currency and index - nine times here rather than
   * three. Each expectation is the amount put in with the currencies it does not name at zero.
   */
  test("test_of_function") {
    val mca1: MultiCurrencyAmount = multiOf(amountOf(GBP, 10d), amountOf(USD, 20d))
    val mca2: MultiCurrencyAmount = multiOf(amountOf(GBP, 10d), amountOf(EUR, 30d))
    val mca3: MultiCurrencyAmount = multiOf(amountOf(USD, 40d))
    val amounts: List[MultiCurrencyAmount] = List(mca1, mca2, mca3)

    val calls: AtomicInteger = new AtomicInteger(0)
    val counted: Int => MultiCurrencyAmount = index => {
      val _ = calls.incrementAndGet()
      amounts(index)
    }
    val test: MultiCurrencyAmountArray = MultiCurrencyAmountArray.of(3, counted)

    test.size shouldBe 3
    test.getCurrencies shouldBe Set(GBP, USD, EUR)
    test.get(0) shouldBe mca1.plus(EUR, 0d)
    test.get(1) shouldBe mca2.plus(USD, 0d)
    test.get(2) shouldBe mca3.plus(GBP, 0d).plus(EUR, 0d)
    calls.get() shouldBe 3
  }

  /**
   * Asserts the size the function form refuses, and the boundary size it admits.
   *
   * A negative size is a caller contract rather than data, so it is raised with its whole message
   * rather than reported. The function is asserted never to have been evaluated: the amounts of a
   * negative size are the empty sequence rather than a raise, so a factory that materialised them
   * first would answer with the run of size zero instead of refusing. Size zero is the admitted
   * boundary of the same check, and evaluates the function for no index at all.
   */
  test("of(size, valueFunction) refuses a negative size and admits zero") {
    val calls: AtomicInteger = new AtomicInteger(0)
    val counted: Int => MultiCurrencyAmount = index => {
      val _ = calls.incrementAndGet()
      multiOf(amountOf(GBP, index.toDouble))
    }

    val refused: IllegalArgumentException =
      intercept[IllegalArgumentException](MultiCurrencyAmountArray.of(-1, counted))
    refused.getMessage shouldBe NegativeSizeMessage
    // the size is checked before any amount is asked for
    calls.get() shouldBe 0

    val none: MultiCurrencyAmountArray = MultiCurrencyAmountArray.of(0, counted)
    none.size shouldBe 0
    none.values shouldBe Map.empty[Currency, DoubleArray]
    none.getCurrencies shouldBe Set.empty[Currency]
    calls.get() shouldBe 0
  }

  /**
   * Asserts that a round trip preserves the size, which is why the size is a field.
   *
   * A run of amounts naming no currency encodes with an empty object of values, so only the `size`
   * field says how long it is: a codec that omitted or recomputed it would read the document back
   * as a run of size zero. The document shape is asserted as well as the value read back, so a
   * change of shape fails here rather than being absorbed by a self round trip; the currencies
   * appear in code order.
   */
  test("serializeSize") {
    val encoded: Json = VALUES_ARRAY.asJson
    encoded.noSpaces shouldBe
      """{"size":3,"values":{"EUR":[40.0,43.0,44.0],"GBP":[20.0,21.0,22.0],"USD":[30.0,32.0,33.0]}}"""
    val deserialized: MultiCurrencyAmountArray = decoded(encoded)
    deserialized.size shouldBe 3
    deserialized shouldBe VALUES_ARRAY

    val empty: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(MultiCurrencyAmount.empty, MultiCurrencyAmount.empty)
    val emptyEncoded: Json = empty.asJson
    emptyEncoded.noSpaces shouldBe """{"size":2,"values":{}}"""
    val deserializedEmpty: MultiCurrencyAmountArray = decoded(emptyEncoded)
    deserializedEmpty.size shouldBe 2
    deserializedEmpty shouldBe empty
    deserializedEmpty.values shouldBe Map.empty[Currency, DoubleArray]
  }

  /**
   * Asserts the stored size through the factory that is told it: three empty amounts describe a run
   * of size three holding no values, and only this factory can produce such a run of any size.
   */
  test("test_of_function_empty_amounts") {
    val test: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(3, (_: Int) => MultiCurrencyAmount.empty)
    test.size shouldBe 3
    test.values shouldBe Map.empty[Currency, DoubleArray]
    test.get(0) shouldBe MultiCurrencyAmount.empty
    test.get(2) shouldBe MultiCurrencyAmount.empty
    decoded(test.asJson).size shouldBe 3
  }

  /**
   * Asserts the factory that reads the representation directly, and what it rejects.
   *
   * Every array has to hold one value per index of the run, so this is the one factory that reports
   * a `Left`. The whole message is asserted and not only its prefix, because it is deterministic:
   * the length is settled by the array of the first currency in code order, so `EUR` with three
   * values is the reference and `GBP` with two is what disagrees. The empty map describes the run
   * of size zero rather than being rejected.
   */
  test("test_of_map") {
    val array: MultiCurrencyAmountArray =
      arrayOf(GBP -> DoubleArray.of(20d, 21d, 22d), EUR -> DoubleArray.of(40d, 43d, 44d))

    val expected: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        List(
          multiOf(amountOf(GBP, 20d), amountOf(EUR, 40d)),
          multiOf(amountOf(GBP, 21d), amountOf(EUR, 43d)),
          multiOf(amountOf(GBP, 22d), amountOf(EUR, 44d))))

    array.size shouldBe 3
    array shouldBe expected

    val unequal: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(GBP -> DoubleArray.of(20d, 21d), EUR -> DoubleArray.of(40d, 43d, 44d)))
    unequal should beFailureWith(FailureReason.INVALID)
    unequal should haveFailureMessageMatching(Regex.quote(ArrayLengthPrefix) + ".*")
    unequal should haveFailureMessageMatching(
      Regex.quote("Arrays must have the same size but found sizes 3 and 2"))

    val empty: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.of(Map.empty[Currency, DoubleArray]))
    empty.size shouldBe 0
    empty.values shouldBe Map.empty[Currency, DoubleArray]
  }

  /**
   * Asserts the values of the run, as a whole and one currency at a time.
   *
   * The map comparison is by content, so the key order is asserted separately: a run holds its
   * currencies in code order. A currency the run does not hold is a reported failure rather than
   * an array of zeroes.
   */
  test("test_getValues") {
    val expected: Map[Currency, DoubleArray] = Map(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d))
    VALUES_ARRAY.values shouldBe expected
    VALUES_ARRAY.values.keys.toList shouldBe List(EUR, GBP, USD)

    VALUES_ARRAY.getValues(EUR) should haveValue(DoubleArray.of(40d, 43d, 44d))
    val unknown: FailureOr[DoubleArray] = VALUES_ARRAY.getValues(CHF)
    unknown should beFailureWith(FailureReason.INVALID)
    unknown should haveFailureMessageMatching(Regex.quote("No values available for CHF"))
  }

  /**
   * Asserts that an index reassembles the amount held at it, padded currencies included, and that
   * an index outside the run raises rather than answering with an amount.
   *
   * An index of a ragged run names more currencies than the amount it was built from, the extra
   * ones at zero. An index outside the run is a caller contract rather than a property of the data,
   * so it stays the index throw of the backing array instead of widening into the reported-failure
   * channel: the fixture holds three values per currency, so `3` is one past the end and `-1` one
   * before the start. What is pinned is the documented `IndexOutOfBoundsException`, whose message
   * is the runtime's own wording rather than this library's and so is not asserted.
   */
  test("test_get") {
    val expected: MultiCurrencyAmount =
      multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))
    VALUES_ARRAY.get(2) shouldBe expected
    VALUES_ARRAY.get(0) shouldBe
      multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d))

    val ragged: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(multiOf(amountOf(EUR, 4d)), multiOf(amountOf(GBP, 21d)))
    ragged.get(0) shouldBe multiOf(amountOf(EUR, 4d), amountOf(GBP, 0d))
    ragged.get(1) shouldBe multiOf(amountOf(EUR, 0d), amountOf(GBP, 21d))

    intercept[IndexOutOfBoundsException](VALUES_ARRAY.get(3))
    intercept[IndexOutOfBoundsException](VALUES_ARRAY.get(-1))
  }

  /** Asserts the amounts of the run in index order, through the iterator and through `toList`. */
  test("test_stream") {
    val expected: List[MultiCurrencyAmount] = List(
      multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d)),
      multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
      multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d)))

    VALUES_ARRAY.iterator.toList shouldBe expected
    VALUES_ARRAY.toList shouldBe expected
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the conversion, which collapses the currencies into one array of values.
   *
   * The expectation is the sum of the products the conversion computes rather than the decimals
   * they round to, which would introduce a rounding the conversion never performs. The conversion
   * accumulates the currencies in code order, which for these values gives the same double as the
   * order written here, so this holds for equality while the test below needs a tolerance.
   *
   * A rate the provider cannot supply fails the whole conversion rather than converting the
   * currencies it can, and carries the currency-conversion reason rather than the invalid one.
   */
  test("test_convertedTo") {
    val provider: FxRateProvider =
      rateProvider(Map((GBP, CAD) -> 2d, (USD, CAD) -> 1.3d, (EUR, CAD) -> 1.4d))
    val expected: DoubleArray = DoubleArray.of(
      20d * 2d + 30d * 1.3d + 40d * 1.4d,
      21d * 2d + 32d * 1.3d + 43d * 1.4d,
      22d * 2d + 33d * 1.3d + 44d * 1.4d)

    val converted: CurrencyAmountArray = unwrap(VALUES_ARRAY.convertedTo(CAD, provider))
    converted.currency shouldBe CAD
    converted.size shouldBe 3
    converted.values shouldBe expected
    VALUES_ARRAY.convertedTo(CAD, provider) should haveValue(CurrencyAmountArray.of(CAD, expected))

    val missingRate: FailureOr[CurrencyAmountArray] =
      VALUES_ARRAY.convertedTo(CAD, rateProvider(Map((GBP, CAD) -> 2d, (USD, CAD) -> 1.3d)))
    missingRate should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    missingRate should haveFailureMessageMatching(Regex.quote("No FX rate found for EUR/CAD"))
  }

  /**
   * Asserts the conversion into a currency the run already holds: its own GBP values are carried
   * through at the rate of one the provider answers for a currency and itself.
   *
   * The expectation is a sum of quotients while the conversion multiplies by the reciprocal, and a
   * quotient by 1.5 and a product by its reciprocal agree only to within rounding, so the
   * comparison carries [[Tolerance]] rather than asserting which of the two is computed.
   */
  test("test_convertedTo_existingCurrency") {
    val provider: FxRateProvider = rateProvider(Map((USD, GBP) -> 1d / 1.5d, (EUR, GBP) -> 0.7d))
    val converted: CurrencyAmountArray = unwrap(VALUES_ARRAY.convertedTo(GBP, provider))
    val expected: DoubleArray = DoubleArray.of(
      20d + 30d / 1.5d + 40d * 0.7d,
      21d + 32d / 1.5d + 43d * 0.7d,
      22d + 33d / 1.5d + 44d * 0.7d)

    converted.currency shouldBe GBP
    converted.size shouldBe 3
    converted.values.equalWithTolerance(expected, Tolerance) shouldBe true
  }


  //-------------------------------------------------------------------------
  /**
   * Asserts the equality and hashing of a value whose map holds arrays.
   *
   * The arrays are compared element by element rather than by identity, so separately built runs
   * holding equal values are equal and hash alike; the same values assembled in another order are
   * equal too, the same currencies at a different size are not, and an unrelated value is not.
   */
  test("test_equalsHashCode") {
    val array: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        List(
          multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d)),
          multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
          multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))))

    array shouldBe VALUES_ARRAY
    array.hashCode shouldBe VALUES_ARRAY.hashCode
    Eq[MultiCurrencyAmountArray].eqv(array, VALUES_ARRAY) shouldBe true
    Hash[MultiCurrencyAmountArray].hash(array) shouldBe
      Hash[MultiCurrencyAmountArray].hash(VALUES_ARRAY)

    // the currencies given in yet another order: a run holds them sorted, so neither equality nor
    // hashing can see the order they arrived in
    val fromMap: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      GBP -> DoubleArray.of(20d, 21d, 22d))
    fromMap shouldBe VALUES_ARRAY
    fromMap.hashCode shouldBe VALUES_ARRAY.hashCode

    val shorter: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d),
      USD -> DoubleArray.of(30d, 32d),
      EUR -> DoubleArray.of(40d, 43d))
    Eq[MultiCurrencyAmountArray].eqv(VALUES_ARRAY, shorter) shouldBe false
    VALUES_ARRAY.equals(ForeignValue) shouldBe false
  }

  //-------------------------------------------------------------------------
  /** Asserts the equality, hashing and rendering instances over two deliberately different runs. */
  test("coverage") {
    val test2: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        multiOf(amountOf(GBP, 21d), amountOf(USD, 31d), amountOf(EUR, 41d)),
        multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d)))

    Eq[MultiCurrencyAmountArray].eqv(VALUES_ARRAY, VALUES_ARRAY) shouldBe true
    Eq[MultiCurrencyAmountArray].eqv(test2, test2) shouldBe true
    Eq[MultiCurrencyAmountArray].eqv(VALUES_ARRAY, test2) shouldBe false

    Hash[MultiCurrencyAmountArray].hash(VALUES_ARRAY) shouldBe VALUES_ARRAY.hashCode
    Hash[MultiCurrencyAmountArray].hash(test2) shouldBe test2.hashCode

    VALUES_ARRAY.size shouldBe 3
    VALUES_ARRAY.values.keys.toList shouldBe List(EUR, GBP, USD)
    test2.size shouldBe 2
    test2.values.keys.toList shouldBe List(EUR, GBP, USD)

    Show[MultiCurrencyAmountArray].show(VALUES_ARRAY) shouldBe VALUES_ARRAY.toString
    Show[MultiCurrencyAmountArray].show(test2) shouldBe test2.toString
    VALUES_ARRAY.toString shouldBe
      "MultiCurrencyAmountArray{size=3, values={EUR=[40.0, 43.0, 44.0], " +
      "GBP=[20.0, 21.0, 22.0], USD=[30.0, 32.0, 33.0]}}"
    test2.toString shouldBe
      "MultiCurrencyAmountArray{size=2, values={EUR=[41.0, 44.0], GBP=[21.0, 22.0], " +
      "USD=[31.0, 33.0]}}"
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the element-wise addition of two runs over the union of their currencies.
   *
   * A currency only one run holds is carried through as it stands, which is the padding read
   * through addition - the other run holds zero for it at every index. Adding the two the other
   * way round gives the same result: the union is symmetric and these whole-number sums are exact.
   */
  test("test_plusArray") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(180d, 186d, 188d),
      CHF -> DoubleArray.of(300d, 308d, 312d))

    array1.plus(array2) should haveValue(expected)
    array2.plus(array1) should haveValue(expected)
  }

  /**
   * Asserts the addition of one amount to every index, which has no size to disagree about: a
   * currency only the amount names becomes one of the result, holding that value at every index.
   */
  test("test_plusAmount") {
    val array: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val amount: MultiCurrencyAmount =
      unwrap(MultiCurrencyAmount.of(Map(GBP -> 21d, EUR -> 143d, CHF -> 254d)))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(21d, 21d, 21d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(183d, 186d, 187d),
      CHF -> DoubleArray.of(304d, 308d, 310d))

    array.plus(amount) should haveValue(expected)
    array.plus(MultiCurrencyAmount.empty) should haveValue(array)
  }

  /**
   * Asserts that adding runs of different sizes is a `Left` rather than a throw: which run is
   * longer is a property of the values a caller holds. The wording names the size of the run the
   * operation was called on first, so both directions are asserted.
   */
  test("test_plusDifferentSize") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d),
      EUR -> DoubleArray.of(40d, 43d),
      CHF -> DoubleArray.of(50d, 54d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))

    val shorterPlusLonger: FailureOr[MultiCurrencyAmountArray] = array1.plus(array2)
    shorterPlusLonger should beFailureWith(FailureReason.INVALID)
    shorterPlusLonger should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))

    val longerPlusShorter: FailureOr[MultiCurrencyAmountArray] = array2.plus(array1)
    longerPlusShorter should beFailureWith(FailureReason.INVALID)
    longerPlusShorter should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
  }

  /**
   * Asserts the element-wise subtraction of two runs, which behaves as addition but for one thing
   * the fixtures show: a currency only the other run holds is carried through '''negated''',
   * because this run holds zero for it at every index.
   */
  test("test_minusArray") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(-20d, -21d, -22d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(-100d, -100d, -100d),
      CHF -> DoubleArray.of(-200d, -200d, -200d))

    array1.minus(array2) should haveValue(expected)
  }

  /**
   * Asserts the subtraction of one amount from every index, over the same three-way choice: a
   * currency only the amount names holds the negation of that value at every index.
   */
  test("test_minusAmount") {
    val array: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val amount: MultiCurrencyAmount =
      unwrap(MultiCurrencyAmount.of(Map(GBP -> 21d, EUR -> 143d, CHF -> 254d)))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(-21d, -21d, -21d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(-103d, -100d, -99d),
      CHF -> DoubleArray.of(-204d, -200d, -198d))

    array.minus(amount) should haveValue(expected)
    array.minus(MultiCurrencyAmount.empty) should haveValue(array)
  }

  /**
   * Asserts that subtracting runs of different sizes is a `Left`. Addition and subtraction share
   * the check and its wording, so both directions are asserted here as well.
   */
  test("test_minusDifferentSize") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d),
      EUR -> DoubleArray.of(40d, 43d),
      CHF -> DoubleArray.of(50d, 54d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))

    val shorterMinusLonger: FailureOr[MultiCurrencyAmountArray] = array1.minus(array2)
    shorterMinusLonger should beFailureWith(FailureReason.INVALID)
    shorterMinusLonger should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))

    val longerMinusShorter: FailureOr[MultiCurrencyAmountArray] = array2.minus(array1)
    longerMinusShorter should beFailureWith(FailureReason.INVALID)
    longerMinusShorter should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the aggregation of single-currency runs: each contributes its values under its own
   * currency, and a currency appearing more than once has its runs added element by element. The
   * reversed input is asserted with it, since these whole-number sums are exact.
   */
  test("collector") {
    MultiCurrencyAmountArray.total(CollectorArrays) should haveValue(CollectorExpected)
    MultiCurrencyAmountArray.total(CollectorArrays.reverse) should haveValue(CollectorExpected)
  }

  /**
   * Asserts the aggregating factory beyond the shared input: no runs at all, which describes the
   * run of size zero; a single run, carried through under its currency; the same run twice, added
   * to itself; and the currencies of the result in code order whatever order the input arrived in.
   */
  test("total") {
    MultiCurrencyAmountArray.total(CollectorArrays) should haveValue(CollectorExpected)

    val none: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.total(List.empty[CurrencyAmountArray]))
    none.size shouldBe 0
    none.values shouldBe Map.empty[Currency, DoubleArray]

    val gbpRun: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(11d, 12d, 13d))
    val single: MultiCurrencyAmountArray = unwrap(MultiCurrencyAmountArray.total(List(gbpRun)))
    single.size shouldBe 3
    single.getCurrencies shouldBe Set(GBP)
    single.getValues(GBP) should haveValue(DoubleArray.of(11d, 12d, 13d))

    val doubled: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.total(List(gbpRun, gbpRun)))
    doubled.getValues(GBP) should haveValue(DoubleArray.of(22d, 24d, 26d))

    unwrap(MultiCurrencyAmountArray.total(CollectorArrays)).values.keys.toList shouldBe
      List(EUR, GBP, USD)
  }

  /**
   * Asserts that aggregating runs of different lengths is reported rather than thrown, by both
   * routes that report it: runs of different currencies disagree only in the run that holds them
   * together, where the first currency in code order settles the reference length - GBP with two
   * values settles it and USD with three disagrees - while runs of the same currency are added
   * element by element first, so the single-currency addition reports it with its own wording.
   */
  test("collectorDifferentArrayLengths") {
    val differentCurrencies: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 2d))))
    differentCurrencies should beFailureWith(FailureReason.INVALID)
    differentCurrencies should haveFailureMessageMatching(Regex.quote(ArrayLengthPrefix) + ".*")
    differentCurrencies should haveFailureMessageMatching(
      Regex.quote("Arrays must have the same size but found sizes 2 and 3"))

    val sameCurrency: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
          CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d))))
    sameCurrency should beFailureWith(FailureReason.INVALID)
    sameCurrency should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that an index is reassembled from the stored values alone, in currency-code order.
   *
   * The entries are read as the map the amount holds, so no currency is merged away, renamed or
   * reordered on the way out of the arrays. Two amounts naming one currency each give every index
   * a currency its own amount did not name, at zero; a run holding no currency reads no array at
   * all, so every index answers with the empty amount, inside the run or not.
   */
  test("get reassembles every currency of the run, in currency-code order") {
    VALUES_ARRAY.get(0) shouldBe multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d))
    VALUES_ARRAY.get(1) shouldBe multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d))
    VALUES_ARRAY.get(2) shouldBe multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))
    VALUES_ARRAY.get(0).toMap.keys.toList shouldBe List(EUR, GBP, USD)
    VALUES_ARRAY.get(0).toMap shouldBe Map(EUR -> 40d, GBP -> 20d, USD -> 30d)

    val disjoint: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(multiOf(amountOf(GBP, 1d)), multiOf(amountOf(USD, 2d)))
    disjoint.get(0) shouldBe multiOf(amountOf(GBP, 1d), amountOf(USD, 0d))
    disjoint.get(1) shouldBe multiOf(amountOf(GBP, 0d), amountOf(USD, 2d))
    disjoint.get(0).toMap shouldBe Map(GBP -> 1d, USD -> 0d)

    val noCurrencies: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(2, (_: Int) => MultiCurrencyAmount.empty)
    noCurrencies.get(0) shouldBe MultiCurrencyAmount.empty
    noCurrencies.get(1) shouldBe MultiCurrencyAmount.empty
    noCurrencies.get(7) shouldBe MultiCurrencyAmount.empty
    noCurrencies.get(-1) shouldBe MultiCurrencyAmount.empty
  }

  /**
   * Asserts the documented invariant reconstruction raises, and the one it can no longer raise.
   *
   * The invariant it raises is a caller-contract invariant, kept as such by this port: an index
   * outside the run is the index exception of the underlying array, raised as the array is read.
   * It is asserted here because it is a property of the route an index takes out of the arrays.
   *
   * The invariant it can no longer raise is the amount invariant. Reconstruction still applies it
   * - the numbers go through the checked-map constructor of [[MultiCurrencyAmount]], which is the
   * same computation - but the element invariant of this type has already established that every
   * value of every array passes it, so there is no run from which reading an index can be
   * refused. The run that would once have produced that refusal cannot be built at all, which the
   * test below asserts at the construction boundary where the refusal now happens; here the
   * statement is the reverse one, that every route into a run leaves every index readable.
   */
  test("get raises its documented index invariant and can no longer raise the amount invariant") {
    an[IndexOutOfBoundsException] should be thrownBy VALUES_ARRAY.get(3)
    an[IndexOutOfBoundsException] should be thrownBy VALUES_ARRAY.get(-1)

    // a run holding the one value an amount refuses cannot be built, so `get` has no such run to
    // be presented with: the factory that reads the representation directly - the only route by
    // which such a value could ever have entered a run, the amount-reading factories being unable
    // to carry one - reports it instead
    MultiCurrencyAmountArray.of(
      Map(GBP -> DoubleArray.of(1d, Double.NaN), USD -> DoubleArray.of(2d, 3d))) should
      beFailureWith(FailureReason.INVALID)

    // and every index of a run that does exist is readable, the infinities included, which are
    // values an amount holds
    val infinite: MultiCurrencyAmountArray =
      arrayOf(
        GBP -> DoubleArray.of(1d, Double.PositiveInfinity),
        USD -> DoubleArray.of(2d, Double.NegativeInfinity))
    infinite.get(0) shouldBe multiOf(amountOf(GBP, 1d), amountOf(USD, 2d))
    infinite.get(1) shouldBe
      multiOf(
        amountOf(GBP, Double.PositiveInfinity),
        amountOf(USD, Double.NegativeInfinity))
    noException should be thrownBy infinite.toList
  }

  /**
   * Asserts the element invariant of this type at the construction boundary that establishes it.
   *
   * The elements of a run are amounts kept as numbers, so a value that is not a number is not an
   * element of a run - and this is the test that states it for each of the two channels the type
   * has:
   *
   *   - the factory that reads the representation directly reports it, accumulating one reason
   *     per offending currency in currency order and beside the reasons the same input gives for
   *     its lengths, which is the shape every checked factory of this port has;
   *   - the two arithmetic members that are total in signature raise it, naming the currency and
   *     the index, exactly as the arithmetic of [[CurrencyAmount]] raises its own invariant for
   *     the sum of two opposite infinities.
   *
   * The wording is asserted literally, currency and index included, because that location is the
   * whole value of the message: a run of three currencies and a hundred thousand indices says
   * nothing about which of its numbers is wrong.
   */
  test("the element invariant refuses a value that is not a number") {
    val reported: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(
          USD -> DoubleArray.of(1d, Double.NaN),
          GBP -> DoubleArray.of(Double.NaN, 2d),
          EUR -> DoubleArray.of(3d, 4d)))
    reported should beFailureWith(FailureReason.INVALID)
    // one reason per offending currency, in currency order, each naming the first offending
    // index of that currency
    failureMessages(reported) shouldBe List(
      "Argument 'values' for GBP must not be NaN at index 0",
      "Argument 'values' for USD must not be NaN at index 1")

    // and it accumulates beside the length reasons rather than replacing them
    val both: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(GBP -> DoubleArray.of(1d, Double.NaN), USD -> DoubleArray.of(2d)))
    failureMessages(both) shouldBe List(
      "Arrays must have the same size but found sizes 2 and 1",
      "Argument 'values' for GBP must not be NaN at index 1")

    // the aggregating factory reports it too, since adding two runs of one currency can produce
    // such a value
    val totalled: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.PositiveInfinity)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NegativeInfinity))))
    failureMessages(totalled) shouldBe List(
      "Argument 'values' for GBP must not be NaN at index 1")

    // the two total arithmetic members raise it where they produce it
    val infinite: MultiCurrencyAmountArray =
      arrayOf(GBP -> DoubleArray.of(1d, 2d), USD -> DoubleArray.of(3d, Double.PositiveInfinity))
    the[IllegalArgumentException] thrownBy infinite.multipliedBy(0d) should have message
      "Argument 'values' for USD must not be NaN at index 1"
    the[IllegalArgumentException] thrownBy infinite.mapAmounts(_ => Double.NaN) should have message
      "Argument 'values' for GBP must not be NaN at index 0"
    // an ordinary scaling of the same run is unaffected: the infinities are values it holds
    infinite.multipliedBy(2d).getValues(USD) should
      haveValue(DoubleArray.of(6d, Double.PositiveInfinity))
  }

  /**
   * Asserts that the members with a failure channel report a refused element in it.
   *
   * The four members that combine values and the conversion each already answer with a failure,
   * so a sum, difference or converted value that is no amount is reported there rather than
   * raised out of it - which is what lets a caller holding two runs of opposed infinities read
   * the reason instead of catching it. The existing checks keep their precedence: a size mismatch
   * is still reported ahead of any addition, since there is no addition to perform.
   */
  test("the element invariant is reported where a route already reports") {
    val positive: MultiCurrencyAmountArray =
      arrayOf(GBP -> DoubleArray.of(1d, Double.PositiveInfinity))
    val negative: MultiCurrencyAmountArray =
      arrayOf(GBP -> DoubleArray.of(1d, Double.NegativeInfinity))

    val sum: FailureOr[MultiCurrencyAmountArray] = positive.plus(negative)
    sum should beFailureWith(FailureReason.INVALID)
    sum should haveFailureMessageMatching(
      Regex.quote("Argument 'values' for GBP must not be NaN at index 1"))

    val difference: FailureOr[MultiCurrencyAmountArray] = positive.minus(positive)
    difference should haveFailureMessageMatching(
      Regex.quote("Argument 'values' for GBP must not be NaN at index 1"))

    // the amount forms, which shift every index by one multi-currency amount
    positive.plus(multiOf(amountOf(GBP, Double.NegativeInfinity))) should
      haveFailureMessageMatching(
        Regex.quote("Argument 'values' for GBP must not be NaN at index 1"))
    positive.minus(multiOf(amountOf(GBP, Double.PositiveInfinity))) should
      haveFailureMessageMatching(
        Regex.quote("Argument 'values' for GBP must not be NaN at index 1"))

    // the size check still comes first, so nothing is added and no element reason is reached
    val shorter: FailureOr[MultiCurrencyAmountArray] =
      positive.plus(arrayOf(GBP -> DoubleArray.of(1d)))
    shorter should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 2, other size is 1"))

    // a conversion collapses the currencies into a run of single-currency amounts, so opposed
    // infinities in two currencies, and a rate that is not a number, are reported in the channel
    // the conversion already has
    val opposed: MultiCurrencyAmountArray =
      arrayOf(
        GBP -> DoubleArray.of(Double.PositiveInfinity),
        USD -> DoubleArray.of(Double.NegativeInfinity))
    val converted: FailureOr[CurrencyAmountArray] =
      opposed.convertedTo(CAD, rateProvider(Map((GBP, CAD) -> 1d, (USD, CAD) -> 1d)))
    converted should beFailureWith(FailureReason.INVALID)
    converted should haveFailureMessageMatching(
      Regex.quote("Argument 'values' must not be NaN at index 0"))

    val atNotANumberRate: FailureOr[CurrencyAmountArray] =
      arrayOf(GBP -> DoubleArray.of(1d, 2d))
        .convertedTo(CAD, rateProvider(Map((GBP, CAD) -> Double.NaN)))
    atNotANumberRate should haveFailureMessageMatching(
      Regex.quote("Argument 'values' must not be NaN at index 0"))
  }

  /**
   * Asserts that the collection, varargs and function forms share one transposition: each produces
   * the same two full-length arrays, with the padded zero where the amount named no value.
   */
  test("the three total factories transpose amounts into one array per currency") {
    val disjointAmounts: List[MultiCurrencyAmount] =
      List(multiOf(amountOf(GBP, 1d)), multiOf(amountOf(USD, 2d)))

    assertDisjointRun(MultiCurrencyAmountArray.of(disjointAmounts))
    assertDisjointRun(MultiCurrencyAmountArray.of(disjointAmounts: _*))
    assertDisjointRun(MultiCurrencyAmountArray.of(2, index => disjointAmounts(index)))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that a conversion asks for each currency's rate exactly once, in code order.
   *
   * The rate is applied to a whole array, so the documented contract is one lookup per currency of
   * the run and not one per currency and index, which would be nine here. The order is asserted
   * with the count because it is the order the sum is accumulated in, which the test below needs.
   */
  test("convertedTo asks for each currency's rate exactly once, in currency-code order") {
    val asked: AtomicReference[Vector[Currency]] = new AtomicReference(Vector.empty[Currency])
    val provider: FxRateProvider =
      recordingRateProvider(Map((EUR, CAD) -> 1.4d, (GBP, CAD) -> 2d, (USD, CAD) -> 1.3d), asked)

    val converted: CurrencyAmountArray = unwrap(VALUES_ARRAY.convertedTo(CAD, provider))

    converted.size shouldBe 3
    converted.values shouldBe DoubleArray.of(
      0d + 20d * 2d + 30d * 1.3d + 40d * 1.4d,
      0d + 21d * 2d + 32d * 1.3d + 43d * 1.4d,
      0d + 22d * 2d + 33d * 1.3d + 44d * 1.4d)
    asked.get().size shouldBe 3
    asked.get() shouldBe Vector(EUR, GBP, USD)
  }

  /**
   * Asserts that the first rate the provider cannot supply fails the whole conversion, with no rate
   * asked for after it: the provider answers the first currency in code order and refuses the
   * second, and is asked exactly twice although the run holds three currencies. The failure
   * returned is the one reported for that second currency, which tells a caller which rate to
   * supply.
   */
  test("convertedTo stops at the first rate the provider cannot supply") {
    val asked: AtomicReference[Vector[Currency]] = new AtomicReference(Vector.empty[Currency])
    val provider: FxRateProvider =
      recordingRateProvider(Map((EUR, CAD) -> 1.4d, (USD, CAD) -> 1.3d), asked)

    val refused: FailureOr[CurrencyAmountArray] = VALUES_ARRAY.convertedTo(CAD, provider)

    refused should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    refused should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/CAD"))
    asked.get().size shouldBe 2
    asked.get() shouldBe Vector(EUR, GBP)
  }

  /**
   * Asserts the converted values exactly, against the sum computed in the test.
   *
   * The expectation is built in the order and with the operands the conversion documents - from
   * zero, the currencies by code, each term the value multiplied by the rate - and the values are
   * chosen so that accumulating the same three terms in reverse gives a different number, which
   * the test asserts alongside.
   */
  test("convertedTo accumulates the currencies in currency-code order, to the last bit") {
    val run: MultiCurrencyAmountArray = arrayOf(
      EUR -> DoubleArray.of(1d, 3d),
      GBP -> DoubleArray.of(1d, 5d),
      USD -> DoubleArray.of(1d, 7d))
    val rates: Map[(Currency, Currency), Double] =
      Map((EUR, CAD) -> 0.1d, (GBP, CAD) -> 0.2d, (USD, CAD) -> 0.3d)

    val expectedFirst: Double = 0d + 1d * 0.1d + 1d * 0.2d + 1d * 0.3d
    val expectedSecond: Double = 0d + 3d * 0.1d + 5d * 0.2d + 7d * 0.3d
    val reversedFirst: Double = 0d + 1d * 0.3d + 1d * 0.2d + 1d * 0.1d

    val converted: CurrencyAmountArray = unwrap(run.convertedTo(CAD, rateProvider(rates)))
    converted.values shouldBe DoubleArray.of(expectedFirst, expectedSecond)
    converted.values.get(0) shouldBe expectedFirst
    reversedFirst should not be expectedFirst
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that the runs of one currency are added in the order the input presents them.
   *
   * The values are chosen so that the order is observable: adding them the other way round gives a
   * different number, which the test asserts as well. A currency offered a single run contributes
   * the values it already holds, unchanged.
   */
  test("total adds the runs of one currency left to right, to the last bit") {
    val first: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(0.1d, 1d))
    val second: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(0.2d, 2d))
    val third: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(0.3d, 3d))

    val expectedFirst: Double = (0.1d + 0.2d) + 0.3d
    val reversedFirst: Double = (0.3d + 0.2d) + 0.1d

    val totalled: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.total(List(first, second, third)))
    totalled.size shouldBe 2
    totalled.getCurrencies shouldBe Set(GBP)
    totalled.getValues(GBP) should haveValue(DoubleArray.of(expectedFirst, (1d + 2d) + 3d))
    reversedFirst should not be expectedFirst

    val single: MultiCurrencyAmountArray = unwrap(MultiCurrencyAmountArray.total(List(second)))
    single.getValues(GBP) should haveValue(second.values)
  }

  /**
   * Asserts that each currency's runs are added among themselves and the result holds the
   * currencies in code order, from an input naming two of them more than once and interleaved.
   */
  test("total keeps the per-currency arrays in currency-code order") {
    val totalled: MultiCurrencyAmountArray = unwrap(
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(10d, 20d)),
          CurrencyAmountArray.of(USD, DoubleArray.of(3d, 4d)),
          CurrencyAmountArray.of(EUR, DoubleArray.of(100d, 200d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(30d, 40d)))))

    totalled.size shouldBe 2
    totalled.values.keys.toList shouldBe List(EUR, GBP, USD)
    totalled.getValues(EUR) should haveValue(DoubleArray.of(100d, 200d))
    totalled.getValues(GBP) should haveValue(DoubleArray.of(40d, 60d))
    totalled.getValues(USD) should haveValue(DoubleArray.of(4d, 6d))
  }

  /**
   * Asserts that the aggregation reads no further than the outcome needs.
   *
   * Two runs of one currency whose lengths differ decide the whole call, so the input is an
   * `Iterable` whose iterator records what it hands over and fails the test if it is asked for
   * anything past that pair. The same input read as an ordinary list gives the same single reason,
   * so the short circuit is about how much is read and not about what is reported.
   *
   * The disagreement across currencies is decided in the run that holds them together instead, and
   * every currency that disagrees with the first in code order is reported, in code order.
   */
  test("total reads no further than the outcome needs") {
    val handed: AtomicReference[Vector[Int]] = new AtomicReference(Vector.empty[Int])
    val decisive: List[CurrencyAmountArray] = List(
      CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
      CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d)))

    val shortCircuited: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(readOnceThenFailing(decisive, handed))
    shortCircuited should beFailureWith(FailureReason.INVALID)
    shortCircuited should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
    // the sizes of the two runs that decided the answer, and nothing handed over after them
    handed.get() shouldBe Vector(3, 2)

    failureMessages(MultiCurrencyAmountArray.total(decisive)) shouldBe
      List("Sizes must be equal, this size is 3, other size is 2")

    val acrossCurrencies: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(EUR, DoubleArray.of(1d, 2d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(1d)),
          CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d, 3d))))
    acrossCurrencies should beFailureWith(FailureReason.INVALID)
    // EUR settles the length, and GBP and USD are reported in that order
    failureMessages(acrossCurrencies) shouldBe List(
      "Arrays must have the same size but found sizes 2 and 1",
      "Arrays must have the same size but found sizes 2 and 3")
  }

  //-------------------------------------------------------------------------
  /** Builds a run from values per currency - one value per index - in any order of the pairs. */
  private def arrayOf(values: (Currency, DoubleArray)*): MultiCurrencyAmountArray =
    unwrap(MultiCurrencyAmountArray.of(values.toMap))

  /** Asserts the run the three total factories build from `[GBP 1]` then `[USD 2]`. */
  private def assertDisjointRun(run: MultiCurrencyAmountArray): Assertion = {
    run.size shouldBe 2
    run.values.keys.toList shouldBe List(GBP, USD)
    run.getValues(GBP) should haveValue(DoubleArray.of(1d, 0d))
    run.getValues(USD) should haveValue(DoubleArray.of(0d, 2d))
  }

  /** Reads a run back out of a document, failing the test with the codec's reason if it cannot. */
  private def decoded(json: Json): MultiCurrencyAmountArray =
    json
      .as[MultiCurrencyAmountArray]
      .fold(
        failure => fail(s"Expected a run but decoding failed with: ${failure.message}"),
        value => value)

  /**
   * Builds a provider answering from a table of rates. A currency and itself answers with one,
   * which is what makes a conversion into a currency the run already holds work; a pair the table
   * does not name is reported with the currency-conversion reason a missing rate has.
   */
  private def rateProvider(rates: Map[(Currency, Currency), Double]): FxRateProvider =
    FxRateProvider.fromFunction((baseCurrency, counterCurrency) =>
      if (baseCurrency == counterCurrency) {
        Right(1d)
      } else {
        rates
          .get((baseCurrency, counterCurrency))
          .toRight(
            Failure.CurrencyConversion(s"No FX rate found for $baseCurrency/$counterCurrency"))
      })

  /**
   * [[rateProvider]] recording the base currency of each lookup in `asked` as the lookup happens,
   * so the length of what it holds is the number of rates the conversion asked for and its order
   * is the order it asked in.
   */
  private def recordingRateProvider(
      rates: Map[(Currency, Currency), Double],
      asked: AtomicReference[Vector[Currency]]): FxRateProvider =
    FxRateProvider.fromFunction { (baseCurrency, counterCurrency) =>
      val _ = asked.updateAndGet(seen => seen :+ baseCurrency)
      rates
        .get((baseCurrency, counterCurrency))
        .toRight(Failure.CurrencyConversion(s"No FX rate found for $baseCurrency/$counterCurrency"))
    }

  /**
   * Wraps runs in an `Iterable` recording the size of each run it hands over in `handed`, and
   * failing the test where it happens if asked for anything more. The iterator is endless rather
   * than merely exhausted, so a factory that kept reading would not quietly run out of input.
   */
  private def readOnceThenFailing(
      elements: List[CurrencyAmountArray],
      handed: AtomicReference[Vector[Int]]): Iterable[CurrencyAmountArray] =
    new Iterable[CurrencyAmountArray] {
      override def iterator: Iterator[CurrencyAmountArray] =
        elements.iterator.map { element =>
          val _ = handed.updateAndGet(seen => seen :+ element.size)
          element
        } ++ Iterator.continually[CurrencyAmountArray](
          fail("the aggregation read an input after the outcome had already been decided"))
    }

  /**
   * Reads the messages of the failures an outcome carries, in order: the message matchers answer
   * whether some failure matches, which cannot distinguish one reason from several.
   */
  private def failureMessages[R, A](outcome: R)(implicit shape: Outcome.Aux[R, A]): List[String] =
    shape.failures(outcome).map(failure => failure.message)

  /**
   * Reads the value out of an outcome expected to have produced one, covering both the
   * single-failure and the accumulating shape through the type class the matchers resolve by. An
   * outcome carrying failures is reported as a failed test naming every one of them, so a fixture
   * that cannot be built never appears as an error raised from an unrelated line.
   */
  private def unwrap[R, A](outcome: R)(implicit shape: Outcome.Aux[R, A]): A = {
    val failures: List[String] = shape.failures(outcome).map(failure => failure.message)
    shape.value(outcome) match {
      case Some(value) if failures.isEmpty => value
      case _ =>
        fail(s"Expected a value but the factory failed with: ${failures.mkString(", ")}")
    }
  }

  /** Builds a multi-currency amount from amounts naming distinct currencies. */
  private def multiOf(amounts: CurrencyAmount*): MultiCurrencyAmount =
    unwrap(MultiCurrencyAmount.of(amounts: _*))

  /** Builds an amount, failing the test if the currency and value describe none. */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))
}
