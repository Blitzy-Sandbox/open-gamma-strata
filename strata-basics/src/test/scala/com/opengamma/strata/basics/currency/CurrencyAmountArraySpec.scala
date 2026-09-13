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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.Outcome
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[CurrencyAmountArray]].
 *
 * The amounts a caller reads are kept as one primitive array of values and a single currency, so
 * each amount is built as it is read rather than stored: `iterator`, `toList` and `get` are three
 * routes to the same amount and agree element by element, and an amount built from a value goes
 * through the signed-zero normalisation of [[CurrencyAmount]], so a `-0.0` in the array is read
 * back as `+0.0`. The factory that builds from a function reads it once per index and in index
 * order, which is why some of the tests below count evaluations and record the indices rather
 * than reading values only: a value-only assertion cannot tell one pass from two. The direct
 * factory admits an array of no values, so the empty run is covered here.
 *
 * The two factories that reject a mixed run of amounts report it in different words -
 * `of(size, valueFunction)` with `Currencies differ: GBP and USD`, `of(amounts)` with the "only
 * one" wording of `strata-collect`, `Multiple values found where only one was expected: GBP and
 * USD`. Each is asserted as the production code words it, because that is the text a log line
 * carries, and is pinned through `Regex.quote` so the comparison is literal rather than a pattern
 * that happens to match.
 *
 * The element invariant of the type - every value of an array is a value [[CurrencyAmount]]
 * holds, so a value that is not a number is not an element of a run at all - is asserted in both
 * of the channels that state it: the raise of the routes that are total in signature, and the
 * failure of the routes that already report one.
 *
 * @see [[CurrencyAmountArray]] for the type under test
 * @see [[CurrencyAmount]] for a single amount
 * @see [[FxRate]] for the rate the conversions are performed with
 */
final class CurrencyAmountArraySpec extends AnyFunSuite with Matchers {

  private val Values: DoubleArray = DoubleArray.of(1d, 2d, 3d)

  private val Rate: Double = 1.61d

  private val SizeMismatchMessage: String = "Sizes must be equal, this size is 3, other size is 2"

  private val CurrencyMismatchMessage: String =
    "Currencies must be equal, this currency is GBP, other currency is USD"

  /** Typed `Any` so the foreign-value equality assertion compiles as a comparison. */
  private val ForeignValue: Any = ""

  //-------------------------------------------------------------------------
  test("test_of_CurrencyDoubleArray") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    test.currency shouldBe GBP
    test.values shouldBe Values
    test.size shouldBe 3
    test.get(0) shouldBe amountOf(GBP, 1d)
    test.get(1) shouldBe amountOf(GBP, 2d)
    test.get(2) shouldBe amountOf(GBP, 3d)
    test.iterator.toList shouldBe gbpAmounts
    test.toList shouldBe gbpAmounts
  }

  /**
   * Asserts where the element invariant is established, and that it is established once.
   *
   * The invariant is a property of the numbers rather than of the route, so it is established by
   * whichever route takes numbers in from outside - each examines them once and then raises or
   * reports what it found - and the single construction point they all reach examines nothing
   * further. Restating it in the constructor would examine every element of every run a second
   * time, and a third time on the routes that have to examine them anyway to say which element
   * they refused; that pass is as long as the pass that does the arithmetic, so on a run of a
   * hundred thousand scenarios it is the difference between one traversal and two.
   *
   * A behavioural test cannot count traversals, so what is read is the compiled form, as the
   * boxing test below reads it: the class file of the type refers to no scan of its values at all,
   * while the companion - where every examining route lives - refers to one. The two together
   * place the examination and pin that it is not also performed once per construction.
   *
   * What the type promises is unchanged by that, and the rest of this test is the promise: one
   * wording, reported and raised by the same input, and no run holding a value an amount does not,
   * including on the routes that examine nothing because they read amounts.
   */
  test("the element invariant is established where the numbers arrive, not once per construction") {
    compiledFormOf(classOf[CurrencyAmountArray]) should not include "indexOf"
    compiledFormOf(
      Class.forName("com.opengamma.strata.basics.currency.CurrencyAmountArray$")) should
      include("indexOf")

    // one examination means one wording: the same refused element raises and reports the same
    // sentence, rather than two channels describing it in two ways
    val raised: IllegalArgumentException = intercept[IllegalArgumentException](
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NaN)))
    val positive: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.PositiveInfinity))
    val negative: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NegativeInfinity))
    positive.plus(negative) should haveFailureMessageMatching(Regex.quote(raised.getMessage))

    // the two factories that read amounts examine nothing, and what keeps them inside the
    // invariant is the invariant of the amounts they read - asserted here as the premise it is,
    // in both of that type's channels
    the[IllegalArgumentException] thrownBy CurrencyAmount.create(GBP, Double.NaN) should
      have message "Argument 'amount' must not be NaN"
    CurrencyAmount.of(GBP, Double.NaN) should beFailureWith(FailureReason.INVALID)

    // so a run transposed from amounts holds only values the run admits, the infinities included,
    // and holds exactly the values the amounts held
    val extremes: List[CurrencyAmount] = List(
      amountOf(GBP, Double.PositiveInfinity),
      amountOf(GBP, Double.NegativeInfinity),
      amountOf(GBP, 0d))
    unwrap(CurrencyAmountArray.of(extremes)).values shouldBe
      DoubleArray.of(Double.PositiveInfinity, Double.NegativeInfinity, 0d)
    unwrap(CurrencyAmountArray.of(3, index => extremes(index))).values shouldBe
      DoubleArray.of(Double.PositiveInfinity, Double.NegativeInfinity, 0d)
  }

  /**
   * Asserts the empty run, the other end of what the direct factory admits.
   *
   * An array of no values is a `DoubleArray` and nothing in the type rejects one, so the empty
   * run is a value: every member whose answer could depend on there being an element is asserted
   * here. The collection-based factory is the one that needs an amount, because it reads the
   * currency from the amounts it is given.
   */
  test("an empty run of amounts is a value the total factory builds and the codec carries") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.EMPTY)
    test.currency shouldBe GBP
    test.size shouldBe 0
    test.values shouldBe DoubleArray.EMPTY
    test.values.isEmpty shouldBe true
    test.iterator.toList shouldBe List.empty[CurrencyAmount]
    test.toList shouldBe List.empty[CurrencyAmount]

    test shouldBe CurrencyAmountArray.of(GBP, DoubleArray.EMPTY)
    test.hashCode shouldBe CurrencyAmountArray.of(GBP, DoubleArray.EMPTY).hashCode
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(GBP, DoubleArray.EMPTY)) shouldBe true
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(USD, DoubleArray.EMPTY)) shouldBe false
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(GBP, Values)) shouldBe false

    val fxRate: FxRate = unwrap(FxRate.of(GBP, USD, Rate))
    test.convertedTo(GBP, fxRate) should haveValue(test)
    test.convertedTo(USD, fxRate) should haveValue(CurrencyAmountArray.of(USD, DoubleArray.EMPTY))

    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe """{"currency":"GBP","values":[]}"""
    encoded.as[CurrencyAmountArray] shouldBe Right(test)
  }

  test("test_of_List") {
    val values: List[CurrencyAmount] = gbpAmounts
    val test: CurrencyAmountArray = unwrap(CurrencyAmountArray.of(values))
    test.currency shouldBe GBP
    test.values shouldBe DoubleArray.of(1d, 2d, 3d)
    test.size shouldBe 3
    test.get(0) shouldBe amountOf(GBP, 1d)
    test.get(1) shouldBe amountOf(GBP, 2d)
    test.get(2) shouldBe amountOf(GBP, 3d)
    test.iterator.toList shouldBe values
    test.toList shouldBe values
  }

  /**
   * Asserts that a collection naming two currencies is reported rather than thrown.
   *
   * The wording is that of the "only one" check the factory reads the currencies through. The
   * empty collection is asserted alongside because it is the other way this factory fails.
   */
  test("test_of_CurrencyList_mixedCurrency") {
    val mixed: List[CurrencyAmount] =
      List(amountOf(GBP, 1d), amountOf(USD, 2d), amountOf(GBP, 3d))
    val outcome: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(mixed)
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should haveFailureMessageMatching(
      Regex.quote("Multiple values found where only one was expected: GBP and USD"))

    val empty: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List.empty[CurrencyAmount])
    empty should beFailureWith(FailureReason.INVALID)
    empty should haveFailureMessageMatching(
      Regex.quote("Argument iterable 'amounts' must not be empty"))
  }

  /** Asserts the function form, including the one-evaluation-per-index guarantee it documents. */
  test("test_of_function") {
    val values: List[CurrencyAmount] = gbpAmounts
    val calls: AtomicInteger = new AtomicInteger(0)
    val counted: Int => CurrencyAmount = index => {
      val _ = calls.incrementAndGet()
      values(index)
    }
    val test: CurrencyAmountArray = unwrap(CurrencyAmountArray.of(3, counted))
    test.currency shouldBe GBP
    test.values shouldBe DoubleArray.of(1d, 2d, 3d)
    test.size shouldBe 3
    test.get(0) shouldBe amountOf(GBP, 1d)
    test.get(1) shouldBe amountOf(GBP, 2d)
    test.get(2) shouldBe amountOf(GBP, 3d)
    test.iterator.toList shouldBe values
    test.toList shouldBe values
    calls.get() shouldBe 3
  }

  /**
   * Asserts that a function producing two currencies is reported rather than thrown.
   *
   * The factory evaluates every index and reports each currency that disagrees, so the outcome is
   * read for the wording of a failure it holds rather than for a single failure.
   */
  test("test_of_function_mixedCurrency") {
    val mixed: List[CurrencyAmount] =
      List(amountOf(GBP, 1d), amountOf(USD, 2d), amountOf(GBP, 3d))
    val outcome: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(3, index => mixed(index))
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should haveFailureMessageMatching(Regex.quote("Currencies differ: GBP and USD"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the whole failable surface of `plus`, the order the two checks run in included.
   *
   * `plus` examines the size before the currency, so an array differing in both is reported as
   * differing in size and the currency check is never reached - an order that cannot be inferred
   * from either mismatch on its own.
   */
  test("test_plus") {
    val base: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)

    base.plus(CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 1d, 1d))) should
      haveValue(CurrencyAmountArray.of(GBP, DoubleArray.of(2d, 3d, 4d)))

    val shorter: FailureOr[CurrencyAmountArray] =
      base.plus(CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 1d)))
    shorter should beFailureWith(FailureReason.INVALID)
    shorter should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))

    val otherCurrency: FailureOr[CurrencyAmountArray] =
      base.plus(CurrencyAmountArray.of(USD, DoubleArray.of(1d, 1d, 1d)))
    otherCurrency should beFailureWith(FailureReason.INVALID)
    otherCurrency should haveFailureMessageMatching(Regex.quote(CurrencyMismatchMessage))

    val both: FailureOr[CurrencyAmountArray] =
      base.plus(CurrencyAmountArray.of(USD, DoubleArray.of(1d, 1d)))
    both should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))
    both shouldNot haveFailureMessageMatching(Regex.quote(CurrencyMismatchMessage))

    base.plus(amountOf(GBP, 0.5d)) should
      haveValue(CurrencyAmountArray.of(GBP, DoubleArray.of(1.5d, 2.5d, 3.5d)))
    val amountCurrency: FailureOr[CurrencyAmountArray] = base.plus(amountOf(USD, 0.5d))
    amountCurrency should beFailureWith(FailureReason.INVALID)
    amountCurrency should haveFailureMessageMatching(Regex.quote(CurrencyMismatchMessage))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the conversion, with the expectation written as products rather than as decimals.
   *
   * Typing `1.61, 3.22, 4.83` would introduce a rounding the conversion never performs and would
   * hide a real difference behind it.
   */
  test("test_convertedTo") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    val fxRate: FxRate = unwrap(FxRate.of(GBP, USD, Rate))
    val expectedValues: DoubleArray = DoubleArray.of(1d * Rate, 2d * Rate, 3d * Rate)
    test.convertedTo(USD, fxRate) should haveValue(CurrencyAmountArray.of(USD, expectedValues))
  }

  test("test_convertedTo_noConversionNecessary") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    val fxRate: FxRate = unwrap(FxRate.of(GBP, USD, Rate))
    test.convertedTo(GBP, fxRate) should haveValue(test)
  }

  /**
   * Asserts that a conversion the rate on offer cannot perform is reported rather than thrown.
   *
   * The reason is the conversion reason rather than the general invalid-argument one, so a caller
   * can tell a conversion it could fix by supplying a rate from one it could not.
   */
  test("test_convertedTo_missingFxRate") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    val fxRate: FxRate = unwrap(FxRate.of(EUR, USD, Rate))
    val outcome: FailureOr[CurrencyAmountArray] = test.convertedTo(USD, fxRate)
    outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    outcome should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/USD"))
  }

  //-------------------------------------------------------------------------
  test("test_minus_currencyAmount") {
    val array: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    array.minus(amountOf(GBP, 0.5d)) should
      haveValue(CurrencyAmountArray.of(GBP, DoubleArray.of(0.5d, 1.5d, 2.5d)))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the equality, hashing and rendering instances over two arrays and a foreign value.
   *
   * Equality is by currency and then by the values element by element, so two separately built
   * arrays holding equal values are equal and hash alike while the currencies keep the GBP and
   * USD arrays apart.
   */
  test("coverage") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    val test2: CurrencyAmountArray = CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d, 3d))
    val same: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 2d, 3d))

    Eq[CurrencyAmountArray].eqv(test, test) shouldBe true
    Eq[CurrencyAmountArray].eqv(test, same) shouldBe true
    Eq[CurrencyAmountArray].eqv(test, test2) shouldBe false
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 2d))) shouldBe
      false

    Hash[CurrencyAmountArray].hash(test) shouldBe Hash[CurrencyAmountArray].hash(same)
    test.hashCode shouldBe same.hashCode
    test.equals(ForeignValue) shouldBe false

    Show[CurrencyAmountArray].show(test) shouldBe test.toString
    test.toString shouldBe "CurrencyAmountArray{currency=GBP, values=[1.0, 2.0, 3.0]}"
  }

  /**
   * Asserts one concrete round trip through the codec.
   *
   * An array is an object of its currency code and its values, the keys appear in the order the
   * type declares its fields, and the document read back is the array that was written. An
   * infinite element is written as a tagged string, which is how a value JSON cannot express as a
   * number survives the round trip.
   */
  test("test_serialization") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe """{"currency":"GBP","values":[1.0,2.0,3.0]}"""
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("currency", "values"))
    encoded.as[CurrencyAmountArray] shouldBe Right(test)

    val infinite: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.PositiveInfinity))
    infinite.asJson.noSpaces shouldBe """{"currency":"GBP","values":[1.0,"Infinity"]}"""
    infinite.asJson.as[CurrencyAmountArray] shouldBe Right(infinite)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that the function form reads each index exactly once, in index order.
   *
   * `test_of_function` counts the evaluations; this records the indices themselves, so index zero
   * is seen first and no index twice - neither the amounts nor their values may be gathered by a
   * second pass over the function.
   */
  test("of_function reads each index exactly once, in index order") {
    val length: Int = 200
    val seen: AtomicReference[Vector[Int]] = new AtomicReference(Vector.empty[Int])
    val recorded: Int => CurrencyAmount = index => {
      val _ = seen.updateAndGet(indices => indices :+ index)
      amountOf(GBP, index.toDouble)
    }
    val test: CurrencyAmountArray = unwrap(CurrencyAmountArray.of(length, recorded))
    seen.get() shouldBe Vector.range(0, length)
    test.size shouldBe length
    test.values shouldBe DoubleArray.tabulate(length)(index => index.toDouble)
  }

  /**
   * Asserts that `iterator`, `toList` and `get` agree element by element with the values.
   *
   * The three are one behaviour reading the values of the array by index, so they agree with
   * each other and with the array itself, in index order, over the whole element domain of this
   * type - every finite value, both infinities, which are values [[CurrencyAmount]] holds, and a
   * signed zero, which it normalises. There is no fourth case: a value that is not a number is
   * not an element of a run at all, which the element invariant asserted below establishes
   * before an array exists, so none of the three can be presented with one.
   *
   * That is the point worth stating here: reading an element of a run is total. It raises
   * nothing, it reports nothing, and it cannot be the place where a value smuggled in through
   * some other route is discovered.
   */
  test("iterator, toList and get agree element by element with the values") {
    val values: DoubleArray =
      DoubleArray.of(1d, -2.5d, 0d, Double.PositiveInfinity, Double.NegativeInfinity)
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, values)
    val byIndex: List[CurrencyAmount] = List.range(0, test.size).map(index => test.get(index))
    byIndex.map(amount => amount.currency) shouldBe List.fill(test.size)(GBP)
    byIndex.map(amount => amount.amount) shouldBe values.toList
    test.iterator.toList shouldBe byIndex
    test.toList shouldBe byIndex

    // the three routes read every element of the domain without raising, the infinities
    // included, and obtaining the traversal reads nothing at all
    noException should be thrownBy test.iterator
    noException should be thrownBy test.iterator.toList
    noException should be thrownBy test.toList
    noException should be thrownBy List.range(0, test.size).map(index => test.get(index))
  }

  /**
   * Asserts that the iterator builds an amount only when it is read.
   *
   * Nothing is materialised when the traversal is obtained: the amounts are produced by the
   * reads that ask for them. Two reads of one index are the observable consequence - they answer
   * with two amounts that are equal and are not the same object, which a traversal holding
   * amounts it had built up front could not do - and a consumer that stops early takes exactly
   * the prefix it asked for.
   *
   * The assertion no longer runs through an element that is not a number, as it once did: such
   * an element is not a value of this type, so it cannot be used to observe when an amount is
   * built. What replaced it observes the same thing without depending on a refusal.
   */
  test("the iterator builds an amount only when it is read") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)
    val first: CurrencyAmount = test.iterator.next()
    val again: CurrencyAmount = test.iterator.next()
    first shouldBe again
    // two separate objects, so neither traversal handed out an amount it had built beforehand
    (first eq again) shouldBe false
    test.iterator.take(1).toList shouldBe List(amountOf(GBP, 1d))
    test.iterator.take(2).toList shouldBe List(amountOf(GBP, 1d), amountOf(GBP, 2d))
  }

  /**
   * Asserts the element invariant on the routes that are total in signature.
   *
   * A value that is not a number is a value [[CurrencyAmount]] does not hold, so it is not a
   * value an element of a run holds either, and this is where that is stated for the three
   * routes whose signature has nowhere to report it: the direct factory, which takes the numbers
   * a caller holds, and the two arithmetic members that produce numbers of their own. Each
   * raises the documented invariant naming the argument and the index, exactly as the arithmetic
   * of [[CurrencyAmount]] raises its own for the sum of two opposite infinities.
   *
   * The index matters as much as the refusal: a run of a hundred thousand values that is merely
   * "not a number somewhere" tells a caller nothing about which value to look at.
   */
  test("the element invariant refuses a value that is not a number where a route is total") {
    the[IllegalArgumentException] thrownBy
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NaN)) should have message
      "Argument 'values' must not be NaN at index 1"
    the[IllegalArgumentException] thrownBy
      CurrencyAmountArray.of(GBP, DoubleArray.of(Double.NaN)) should have message
      "Argument 'values' must not be NaN at index 0"

    // an operation that produces such a value is refused where it is produced: zero times an
    // infinite element, and a mapping that answers with one directly
    val infinite: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.PositiveInfinity))
    the[IllegalArgumentException] thrownBy infinite.multipliedBy(0d) should have message
      "Argument 'values' must not be NaN at index 1"
    the[IllegalArgumentException] thrownBy
      CurrencyAmountArray.of(GBP, Values).mapAmounts(_ => Double.NaN) should have message
      "Argument 'values' must not be NaN at index 0"

    // and the infinities themselves are elements the type holds, so the invariant is about the
    // one value it refuses rather than about non-finite values in general
    infinite.values.get(1).isPosInfinity shouldBe true
    infinite.multipliedBy(2d).values shouldBe DoubleArray.of(2d, Double.PositiveInfinity)
  }

  /**
   * Asserts the element invariant on the routes that already report failures.
   *
   * Where a member has a failure channel of its own, a refused element is reported in it rather
   * than raised out of it - which is what makes a sum of opposed infinities and a conversion at
   * a rate that is not a number answers a caller reads. The wording is the one the raising
   * routes use, so the two channels cannot describe the same refusal differently.
   *
   * The existing checks of each member keep their precedence: `plus` reports the size and then
   * the currency of the other array, and only an addition that actually happened can report a
   * sum. The rate of the conversion is one an [[FxRate]] holds - it is neither negative nor
   * zero, which is all that type asks - so this is reachable without inventing a provider.
   */
  test("the element invariant is reported where a route already reports") {
    val positive: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.PositiveInfinity))
    val negative: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NegativeInfinity))

    val sum: FailureOr[CurrencyAmountArray] = positive.plus(negative)
    sum should beFailureWith(FailureReason.INVALID)
    sum should haveFailureMessageMatching(
      Regex.quote("Argument 'values' must not be NaN at index 1"))

    val difference: FailureOr[CurrencyAmountArray] = positive.minus(positive)
    difference should beFailureWith(FailureReason.INVALID)
    difference should haveFailureMessageMatching(
      Regex.quote("Argument 'values' must not be NaN at index 1"))

    // the single-amount forms report the same way, and the index named is the first offending
    // element rather than the first element
    positive.plus(amountOf(GBP, Double.NegativeInfinity)) should
      haveFailureMessageMatching(Regex.quote("Argument 'values' must not be NaN at index 1"))
    CurrencyAmountArray
      .of(GBP, DoubleArray.of(Double.PositiveInfinity, 1d))
      .minus(amountOf(GBP, Double.PositiveInfinity)) should
      haveFailureMessageMatching(Regex.quote("Argument 'values' must not be NaN at index 0"))

    // a rate that is not a number is a rate the type holds, and the conversion reports the
    // values it would have produced rather than raising from underneath the failure channel
    val notANumberRate: FxRate = unwrap(FxRate.of(GBP, USD, Double.NaN))
    val converted: FailureOr[CurrencyAmountArray] =
      CurrencyAmountArray.of(GBP, Values).convertedTo(USD, notANumberRate)
    converted should beFailureWith(FailureReason.INVALID)
    converted should haveFailureMessageMatching(
      Regex.quote("Argument 'values' must not be NaN at index 0"))

    // the size and currency checks still come first, so an array that differs in size reports
    // that and never reaches the addition
    val shorter: FailureOr[CurrencyAmountArray] =
      positive.plus(CurrencyAmountArray.of(GBP, DoubleArray.of(1d)))
    shorter should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 2, other size is 1"))
  }

  /**
   * Asserts that an amount built from a negative zero value is normalised.
   *
   * The total factory neither examines nor changes the numbers it is handed, so the array keeps
   * the negative zero, while every amount read back out of it is built through the construction
   * path of [[CurrencyAmount]] and carries that type's normalisation of the sign of zero. The
   * difference is asserted with `java.lang.Double.compare`, which separates `-0.0` from `+0.0`
   * where `==` treats them as equal.
   */
  test("an amount built from a negative zero value is normalised") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(-0d, 1d))
    // the array keeps the value it was given, which only a sign-aware comparison sees
    java.lang.Double.compare(test.values.get(0), -0d) shouldBe 0
    java.lang.Double.compare(test.values.get(0), 0d) should not be 0
    java.lang.Double.compare(test.get(0).amount, 0d) shouldBe 0
    java.lang.Double.compare(test.toList.head.amount, 0d) shouldBe 0
    java.lang.Double.compare(test.iterator.toList.head.amount, 0d) shouldBe 0
    // equality of an amount compares its number the same way, so the normalised amounts are equal
    // to one the checking factory builds from a positive zero
    test.get(0) shouldBe amountOf(GBP, 0d)
    test.iterator.toList shouldBe List(amountOf(GBP, 0d), amountOf(GBP, 1d))
  }

  /**
   * Asserts on the compiled form that traversing the amounts boxes neither index nor value.
   *
   * A behavioural test cannot distinguish a boxed traversal from an unboxed one, so what is read
   * is the declared methods of the type and of the view it traverses, and the bytes of the view's
   * class file as they were emitted for this run.
   */
  test("traversing the amounts crosses no boxing adapter") {
    val traversal: Class[_] =
      Class.forName("com.opengamma.strata.basics.currency.CurrencyAmountArray$Amounts")

    // no mapping function over the indices survives on the type
    val mappers: Array[String] =
      classOf[CurrencyAmountArray].getDeclaredMethods.map(_.getName).filter(_.contains("iterator"))
    mappers.filter(_.startsWith("$anonfun")) shouldBe empty

    // the element accessor takes the index as a primitive and answers with an amount
    val accessors: Array[java.lang.reflect.Method] = traversal.getDeclaredMethods
      .filter(method => method.getName == "apply" && method.getParameterTypes.toList == List(classOf[Int]))
    accessors.map(_.getReturnType).toList should contain(classOf[CurrencyAmount])

    // and nothing in the compiled view adapts a primitive
    val compiled: String = compiledFormOf(traversal)
    compiled should not include "BoxesRunTime"
    compiled should not include "java/lang/Integer.valueOf"
    compiled should not include "java/lang/Double.valueOf"
  }

  //-------------------------------------------------------------------------
  /** The three GBP amounts of the fixture, in index order. */
  private def gbpAmounts: List[CurrencyAmount] =
    List(amountOf(GBP, 1d), amountOf(GBP, 2d), amountOf(GBP, 3d))

  /**
   * Reads the compiled form of a class as text, for an assertion about what was emitted.
   *
   * The bytes are read as `ISO-8859-1` because that maps every byte to exactly one character: the
   * names a class refers to are plain ASCII in its constant pool, so searching the text for one
   * finds the reference and nothing is lost in the decoding.
   *
   * @param target  the class whose compiled form is wanted
   * @return the bytes of its class file, one byte per character
   */
  private def compiledFormOf(target: Class[_]): String = {
    val resource: String = target.getName.replace('.', '/') + ".class"
    val stream: java.io.InputStream = Option(getClass.getClassLoader.getResourceAsStream(resource))
      .getOrElse(fail(s"the compiled form of ${target.getName} is not on the class path"))
    try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.ISO_8859_1)
    finally stream.close()
  }

  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * Both shapes the factories here return are covered - a single failure and an accumulated chain
   * - and an outcome carrying failures is reported as a failed test naming every one of them, so
   * a fixture that cannot be built never appears as an error raised from an unrelated line.
   *
   * @param outcome  the outcome expected to carry a value
   * @param shape  the view of that outcome as failures and a value
   * @tparam R  the type of the outcome
   * @tparam A  the type of the value the outcome carries
   * @return the value it carries
   */
  private def unwrap[R, A](outcome: R)(implicit shape: Outcome.Aux[R, A]): A = {
    val failures: List[String] = shape.failures(outcome).map(failure => failure.message)
    shape.value(outcome) match {
      case Some(value) if failures.isEmpty => value
      case _ =>
        fail(s"Expected a value but the factory failed with: ${failures.mkString(", ")}")
    }
  }

  /**
   * Builds an amount, failing the test if the currency and value describe none.
   *
   * @param currency  the currency the amount is in
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the amount
   */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))
}
