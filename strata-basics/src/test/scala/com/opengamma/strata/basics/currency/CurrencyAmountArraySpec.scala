/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger

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
 * The original holds twelve test methods and so does this suite, each under the name the
 * original gave it, so that a Java test method and a test of this suite stay in one-to-one
 * correspondence in the migration manifest. Three of the twelve keep their name and change what
 * they assert, and each says so at the test itself:
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
  /** The three GBP amounts of the fixture, in index order. */
  private def gbpAmounts: List[CurrencyAmount] =
    List(amountOf(GBP, 1d), amountOf(GBP, 2d), amountOf(GBP, 3d))

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
