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
 * Test [[CurrencyAmountArray]], ported from the Java `CurrencyAmountArrayTest`.
 *
 * The original holds twelve test methods, and this suite holds those twelve ported methods -
 * each under the name the original gave it, so that a Java test method and a test of this suite
 * stay in one-to-one correspondence in the migration manifest - plus one case of this port's own.
 * That thirteenth test covers the '''empty''' run: the direct factory of this type is total and
 * admits an array of no values, the Java test built runs of three elements only, and no other
 * suite of the module reached the boundary, so a regression that rejected an empty run or
 * mis-sized it would have failed nothing. It carries a descriptive name rather than a Java one
 * because there is no Java method it corresponds to.
 *
 * Three of the twelve keep their name and change what they assert, and each says so at the test
 * itself:
 *
 *   - `test_plus` had a body that never called `plus`: it was a verbatim duplicate of
 *     `test_of_function_mixedCurrency`, so the member it names was covered nowhere. The name is
 *     kept and a body covering the contract of `plus` is written in its place.
 *   - `coverage` drove the reflective bean sweep of the library being ported, which no type of
 *     this port has. It asserts the equality, hashing and rendering instances that replaced the
 *     sweep, over the same two values the original built.
 *   - `test_serialization` round-tripped through Java serialization, which no type of this port
 *     supports. It asserts one concrete round trip through the codec that replaced it.
 *
 * Four further tests follow those twelve, under names of their own so that the correspondence
 * stays one-to-one. They pin the boundary between the amounts a caller holds and the single
 * primitive array their values are kept in, which the ported twelve exercise only for a three
 * element run of finite values: the order and the number of times a factory reads its input, the
 * agreement of `iterator`, `toList` and `get`, the point at which each amount is built, and the
 * normalisation an amount built from a value goes through.
 *
 * ===Failure is a value, so the fixtures are unwrapped===
 *
 * Four of the assertions of the original were `assertThatIllegalArgumentException`, and each is a
 * `Left` here: a collection or a function that does not describe one run of amounts is reported
 * by the factory, and a conversion the rate on offer cannot perform is reported by the
 * conversion. The reason is compared as a value of the closed family of reasons and the wording
 * is pinned through `Regex.quote`, so what is asserted is the literal message rather than a
 * pattern that happens to match it.
 *
 * Two wordings are worth reading twice, because the two factories that reject a mixed collection
 * of amounts do not report it identically:
 *
 *   - `of(size, valueFunction)` carries the wording of the implementation being ported,
 *     `Currencies differ: GBP and USD`.
 *   - `of(amounts)` reads the currencies of the collection through the "only one" check of
 *     `strata-collect` and carries that check's wording,
 *     `Multiple values found where only one was expected: GBP and USD`. The implementation being
 *     ported reached the same conclusion through the same shared helper - a stream reduction that
 *     threw from inside `Guavate.ensureOnlyOne` - so the route is the one it took and only the
 *     text differs. It is asserted as the production code words it, which is what a log line
 *     carries.
 *
 * The fixtures themselves are built through [[unwrap]], the single unwrapping helper of the
 * suite, so a fixture that fails to build is reported as a failed test naming the reason rather
 * than raising from somewhere else.
 *
 * ===What is asserted elsewhere===
 *
 * The compile-time proofs that this type has no public `apply`, no `copy` and no reachable
 * escape hatch into the backing array belong to `ApiSurfaceSpec`; the sweep over every validated
 * factory of the module to `SmartConstructorSpec`; the sweep over every failable method to
 * `FailableSurfaceSpec`; the typeclass laws to `TypeclassLawsSpec`; the property-based round trip
 * of every codec to `json.JsonRoundTripSpec`; the numerical parity of currency arithmetic against
 * the Java baseline to `parity.CurrencyMathParitySpec`, and of the underlying array itself to
 * `strata-collect`'s `parity.DoubleArrayParitySpec`. This suite asserts the cases of the Java
 * test it is ported from.
 *
 * @see [[CurrencyAmountArray]] for the type under test
 * @see [[CurrencyAmount]] for a single amount
 * @see [[FxRate]] for the rate the conversions are performed with
 */
final class CurrencyAmountArraySpec extends AnyFunSuite with Matchers {

  /** The values of the fixture array, as the original wrote them. */
  private val Values: DoubleArray = DoubleArray.of(1d, 2d, 3d)

  /** The rate of the conversion tests, as the original wrote it. */
  private val Rate: Double = 1.61d

  /** The wording reported when two arrays that have to be combined differ in size. */
  private val SizeMismatchMessage: String = "Sizes must be equal, this size is 3, other size is 2"

  /** The wording reported when an array is combined with amounts of another currency. */
  private val CurrencyMismatchMessage: String =
    "Currencies must be equal, this currency is GBP, other currency is USD"

  /**
   * A value of a type unrelated to an array, for the equality assertion that needs one.
   *
   * Held at the type `Any` so that the comparison reads as one against a foreign value rather
   * than as one the compiler could reject outright.
   */
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
    // the stream of the original is an iterator here, and the eager form of it is `toList`
    test.iterator.toList shouldBe gbpAmounts
    test.toList shouldBe gbpAmounts
  }

  /**
   * Asserts the empty run, which is the boundary of the total factory and was covered nowhere.
   *
   * The three-element fixture above is the only run the Java test built through this factory, and
   * a run of '''no''' amounts is the other end of what the factory admits: the direct factory is
   * total, an array of no values is a `DoubleArray`, and nothing in the type rejects one. It is
   * the collection-based factory that needs an amount - it reads the currency from the amounts it
   * is given, so an empty collection names no currency and is reported, which
   * `test_of_CurrencyList_mixedCurrency` asserts. The two must not be confused, and this test is
   * where the difference is stated.
   *
   * Every member whose answer could plausibly depend on there being an element is asserted: the
   * currency is carried, the size is zero, the values are the empty array, both iteration forms
   * produce nothing, the same-currency conversion answers with the instance itself without
   * consulting the rate, a conversion that needs a rate applies it to no elements, and the codec
   * round trip carries the empty run back as the value it started as. A regression that rejected
   * an empty array, that assigned it the wrong size, that lost its currency or that broke its
   * document form would fail here.
   */
  test("an empty run of amounts is a value the total factory builds and the codec carries") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.EMPTY)
    test.currency shouldBe GBP
    test.size shouldBe 0
    test.values shouldBe DoubleArray.EMPTY
    test.values.isEmpty shouldBe true
    test.iterator.toList shouldBe List.empty[CurrencyAmount]
    test.toList shouldBe List.empty[CurrencyAmount]

    // the run is a value of the type, so it is equal to another built the same way and to no run
    // of another currency or length
    test shouldBe CurrencyAmountArray.of(GBP, DoubleArray.EMPTY)
    test.hashCode shouldBe CurrencyAmountArray.of(GBP, DoubleArray.EMPTY).hashCode
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(GBP, DoubleArray.EMPTY)) shouldBe true
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(USD, DoubleArray.EMPTY)) shouldBe false
    Eq[CurrencyAmountArray].eqv(test, CurrencyAmountArray.of(GBP, Values)) shouldBe false

    // the conversion into its own currency answers with the instance and consults no rate, and a
    // conversion that does need one applies it to no elements
    val fxRate: FxRate = unwrap(FxRate.of(GBP, USD, Rate))
    test.convertedTo(GBP, fxRate) should haveValue(test)
    test.convertedTo(USD, fxRate) should haveValue(CurrencyAmountArray.of(USD, DoubleArray.EMPTY))

    // and the document form of an empty run is the currency and an empty array of values, which
    // reads back as the run it was written from
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
   * The wording is that of the "only one" check the factory reads the currencies through, as the
   * note on this class explains, and the empty collection is asserted alongside: it is the other
   * way this factory can fail and the implementation being ported described it nowhere - it read
   * the result of an empty stream reduction and raised a no-such-element error.
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

  /**
   * Asserts the function form, including the one-evaluation-per-index guarantee it documents.
   *
   * The count is kept in an atomic integer rather than in a mutable local, both because neither
   * the domain nor the test code of this port holds one and because the counter is written from
   * inside a function the factory calls and read after it returns.
   */
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
    // the factory documents one evaluation per index, in index order, and nothing more
    calls.get() shouldBe 3
  }

  /**
   * Asserts that a function producing two currencies is reported rather than thrown.
   *
   * This route carries the wording of the implementation being ported. That implementation
   * stopped at the first amount whose currency disagreed; this one evaluates every index and
   * reports each currency that disagrees, which is why the outcome is read for the wording of a
   * failure it holds rather than for a single failure.
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
   * Asserts the contract of `plus`, which the Java method of this name asserted nowhere.
   *
   * The body of the original was a verbatim duplicate of `test_of_function_mixedCurrency` - a
   * copy-paste that left `plus` uncovered in the very test class named after it. The name is kept
   * because the migration manifest joins a Java test method to a test of this suite by name, and
   * the body is written for the member the name promises, so this port strictly increases what is
   * covered rather than reproducing the omission.
   *
   * What is covered is the whole failable surface of the member: the element-wise sum, a size
   * mismatch, a currency mismatch at equal size, the precedence between those two, and the form
   * that adds a single amount to every element. The precedence assertion is the one that could
   * not be inferred from the others: an array differing in both size and currency is reported as
   * differing in size, because the size is examined first, and that ordering is the behaviour of
   * the implementation being ported rather than an accident of how it was written.
   */
  test("test_plus") {
    val base: CurrencyAmountArray = CurrencyAmountArray.of(GBP, Values)

    // the element-wise sum, delegated to the values themselves
    base.plus(CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 1d, 1d))) should
      haveValue(CurrencyAmountArray.of(GBP, DoubleArray.of(2d, 3d, 4d)))

    // a size mismatch: there is no amount to add at an index only one array holds
    val shorter: FailureOr[CurrencyAmountArray] =
      base.plus(CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 1d)))
    shorter should beFailureWith(FailureReason.INVALID)
    shorter should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))

    // a currency mismatch at equal size: the sum of amounts of two currencies has no currency
    val otherCurrency: FailureOr[CurrencyAmountArray] =
      base.plus(CurrencyAmountArray.of(USD, DoubleArray.of(1d, 1d, 1d)))
    otherCurrency should beFailureWith(FailureReason.INVALID)
    otherCurrency should haveFailureMessageMatching(Regex.quote(CurrencyMismatchMessage))

    // both wrong: the size is checked first, so that is what is reported and the currency check
    // is never reached
    val both: FailureOr[CurrencyAmountArray] =
      base.plus(CurrencyAmountArray.of(USD, DoubleArray.of(1d, 1d)))
    both should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))
    both shouldNot haveFailureMessageMatching(Regex.quote(CurrencyMismatchMessage))

    // the single-amount form shifts every element, and has no size to disagree about
    base.plus(amountOf(GBP, 0.5d)) should
      haveValue(CurrencyAmountArray.of(GBP, DoubleArray.of(1.5d, 2.5d, 3.5d)))
    val amountCurrency: FailureOr[CurrencyAmountArray] = base.plus(amountOf(USD, 0.5d))
    amountCurrency should beFailureWith(FailureReason.INVALID)
    amountCurrency should haveFailureMessageMatching(Regex.quote(CurrencyMismatchMessage))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the conversion, with the expectation written as the products the original wrote.
   *
   * The expected values are `1 * 1.61`, `2 * 1.61` and `3 * 1.61` rather than the decimals those
   * products round to, exactly as the original wrote them: typing `1.61, 3.22, 4.83` would
   * introduce a rounding the conversion never performs and would hide a real difference behind
   * it.
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
   * The reason is the one this port gives a rate that is unavailable rather than the general
   * invalid-argument reason, so a caller can tell a conversion it could fix by supplying a rate
   * from one it could not.
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
   * Asserts the instances that replaced the reflective bean sweep of the original.
   *
   * The original drove `coverImmutableBean` over a GBP array and `coverBeanEquals` over that
   * array and a USD one holding the same values. Neither has a target here - nothing of this port
   * inspects a class while the program runs - so the two values are built as the original built
   * them and the equality, hashing and rendering instances are asserted over them directly.
   *
   * Equality is by currency and then by the values element by element, compared on their bit
   * patterns, so two separately built arrays holding equal values are equal and hash alike while
   * the two currencies keep the GBP and USD arrays apart.
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
   * Asserts the codec that replaced the Java serialization round trip of the original.
   *
   * One concrete example, which is what the original asserted: an array is an object of its
   * currency code and its values, the keys appear in the order the type declares its fields, and
   * the document read back is the array that was written. The sweep over every codec of the module
   * belongs to `json.JsonRoundTripSpec`.
   *
   * The second half is the one element this type admits that JSON cannot express as a number. It
   * goes through the single policy this port has for a double, which writes it as a tagged string,
   * so every array the type admits survives the round trip - and an array is compared on bit
   * patterns, so an infinite element decoded back is equal to the one encoded.
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
  // The four tests below are additions to the ported set rather than ports of Java test
  // methods, so each carries a name of its own and none of the names above changes: the
  // migration manifest joins a Java test method to a test of this suite by name. They pin the
  // boundary between the amounts a caller holds and the single primitive array this type keeps
  // their values in - the order and the number of times a factory reads its input, the
  // agreement of the three ways of reading amounts back, when each amount is built, and the
  // normalisation every amount built from a value goes through. The tests above exercise that
  // boundary only for a three element run of finite values.

  /**
   * Asserts that the function form reads each index exactly once, in index order.
   *
   * `test_of_function` counts the evaluations; this records the indices themselves, which is the
   * other half of the promise the factory documents - a function that reads a sequence of inputs
   * sees index zero first and every index exactly once, so neither the amounts nor their values
   * may be gathered by a second pass over the function. The indices are recorded in an atomic
   * reference over an immutable vector, which is how the suites of this port record a sequence
   * of calls: the writes happen inside a function the factory calls and the sequence is read
   * after it returns, and neither a mutable field nor a mutable collection is involved.
   *
   * The run is two hundred elements rather than three so that the order is pinned well beyond
   * the length at which a hand-written list of expected calls stays readable.
   */
  test("of_function reads each index exactly once, in index order") {
    val length: Int = 200
    // the indices the function has been passed, newest last
    val seen: AtomicReference[Vector[Int]] = new AtomicReference(Vector.empty[Int])
    val recorded: Int => CurrencyAmount = index => {
      // bound to a wildcard because the new sequence is of no interest here; the assertions
      // below read it back from the reference
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
   * each other and with the array itself, in index order, for every value this type admits -
   * the two infinities included, which are values [[CurrencyAmount]] holds.
   *
   * A value that is not a number is the one value [[CurrencyAmount]] does not hold, and it is
   * refused as the amount for that element is built rather than earlier: obtaining the iterator
   * reads no element and raises nothing, while reading the element raises whichever of the three
   * routes reads it.
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

    val withNotANumber: CurrencyAmountArray =
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NaN))
    // obtaining the iterator reads no element, so nothing is raised by the call itself
    noException should be thrownBy withNotANumber.iterator
    an[IllegalArgumentException] should be thrownBy withNotANumber.get(1)
    an[IllegalArgumentException] should be thrownBy withNotANumber.iterator.toList
    an[IllegalArgumentException] should be thrownBy withNotANumber.toList
  }

  /**
   * Asserts that the iterator builds an amount only when it is read.
   *
   * The element that is not a number sits at index one, so an iterator that built every amount
   * when it was obtained, or read ahead of the caller, would raise the invariant of
   * [[CurrencyAmount]] before the first amount could be taken. Taking the first amount and
   * stopping therefore both succeeds and proves that a caller which stops early never pays for
   * the rest of the run - which is what the member documents and what makes it the counterpart
   * of the stream of the implementation being ported rather than of its list.
   */
  test("the iterator builds an amount only when it is read") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(1d, Double.NaN))
    test.iterator.take(1).toList shouldBe List(amountOf(GBP, 1d))
  }

  /**
   * Asserts that an amount built from a negative zero value is normalised.
   *
   * The array holds the value it was given, negative zero and all, because it neither examines
   * nor changes the numbers handed to the total factory. Every amount read back out of it,
   * however, is built through the construction path of [[CurrencyAmount]] and therefore carries
   * that type's normalisation of the sign of zero - so the array keeps a negative zero while
   * every route that reads it answers with a positive one.
   *
   * That difference is the assertion worth having here: it is observable only because both are
   * compared on their bit patterns, and it is what shows that reading an element still performs
   * the whole invariant of [[CurrencyAmount]] rather than pairing a number with a currency.
   */
  test("an amount built from a negative zero value is normalised") {
    val test: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(-0d, 1d))
    // the array keeps the value it was given: compared on bit patterns, -0.0 is not 0.0
    java.lang.Double.compare(test.values.get(0), -0d) shouldBe 0
    java.lang.Double.compare(test.values.get(0), 0d) should not be 0
    // every route that builds an amount from that value normalises the sign
    java.lang.Double.compare(test.get(0).amount, 0d) shouldBe 0
    java.lang.Double.compare(test.toList.head.amount, 0d) shouldBe 0
    java.lang.Double.compare(test.iterator.toList.head.amount, 0d) shouldBe 0
    // and the amounts are consequently equal to the amount the checking factory builds from a
    // positive zero, equality of an amount being on the bit pattern of its number too
    test.get(0) shouldBe amountOf(GBP, 0d)
    test.iterator.toList shouldBe List(amountOf(GBP, 0d), amountOf(GBP, 1d))
  }

  /**
   * Asserts that traversing the amounts crosses no boxing adapter, element by element.
   *
   * The representation of this type exists to hold a run of amounts as one currency and one
   * primitive array rather than as an object per element, and a traversal that boxed something
   * per element would give back a share of what that buys - the numbers on their way out, or the
   * indices on their way in. The property is therefore about the compiled form rather than about
   * an answer, and it is asserted here on the compiled form directly, which is the only place it
   * is visible: a behavioural test cannot distinguish a boxed traversal from an unboxed one.
   *
   * Three things are read, and together they cover both operands:
   *
   *   - the traversal has '''no mapping function''' left on the type at all. A traversal written
   *     as a mapped range of indices compiles to a synthetic `$anonfun$iterator$…` method taking
   *     its index as an object, and it is that method which unboxes per element; no method of
   *     that name exists;
   *   - the element accessor of the view the traversal reads takes its index as a
   *     '''primitive''' `int` and answers with a [[CurrencyAmount]], so an index reaches the
   *     array without being boxed and an amount leaves without being wrapped;
   *   - the compiled view names '''no primitive adapter''' of the runtime at all, which is read
   *     from its class file rather than inferred: no `BoxesRunTime` and no `valueOf` of a boxed
   *     primitive appears anywhere in it.
   *
   * The class file is read from the class path as a resource and searched as bytes, so the
   * assertion is on what the compiler actually emitted for this run of the suite.
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
   * The class file is taken from the class path the suite is running against, so what is searched
   * is the form produced by the compilation under test rather than a rebuild of it. The bytes are
   * read as `ISO-8859-1` because that maps every byte to exactly one character: the names of the
   * methods a class refers to are plain ASCII in its constant pool, so searching the text for one
   * finds the reference and nothing else can be lost in the decoding.
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
   * This is the single unwrapping helper of the suite and it covers every shape a factory of this
   * port returns - the single-failure outcome of [[CurrencyAmount.of]] and the accumulating
   * outcome of [[CurrencyAmountArray.of]] and [[FxRate.of]] - through the same type class the
   * matchers of this port are resolved by. An outcome carrying failures is reported as a failed
   * test naming every one of them, so a fixture that cannot be built never appears as an error
   * raised from an unrelated line.
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
