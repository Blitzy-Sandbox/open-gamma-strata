/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger

import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[CurrencyAmount]], ported from the Java `CurrencyAmountTest`.
 *
 * Every one of the thirty-three methods of the original is kept under the name the original gave
 * it, with the two data-driven methods each staying '''one''' test that runs the whole table of
 * the provider it was driven by, so that a Java test method and a test of this suite remain in
 * one-to-one correspondence in the migration manifest. One of the thirty-three keeps its name and
 * changes its subject: the original's `test_serialization` round-tripped through Java
 * serialization, which no type of this port supports, so the manifest records that method as
 * consolidated into `json.JsonRoundTripSpec` and the `test_serialization` of this suite is the
 * per-type representation of the codec that replaced it, exactly as `CurrencySpec` is written.
 * Two further tests are added at the end for behaviour of the ported type the Java class covered
 * nowhere: the typeclass instances, and the numeric edges only an infinite operand can reach.
 *
 * ===Failure is a value, so the fixtures are unwrapped===
 *
 * [[CurrencyAmount.of]] reports a value this type does not admit rather than raising it, so every
 * amount of this suite is built through [[amountOf]], which unwraps the outcome once and reports
 * a fixture that could not be built as a failed test naming the cause. The original could write
 * `CurrencyAmount.of(CCY1, AMT1)` as a constant because its factory threw.
 *
 * Three groups of assertions change shape with that, and each is noted again at the test it
 * affects:
 *
 *   - '''an amount that is not a number''' was an `IllegalArgumentException` from the factory and
 *     is a `Left` carrying `FailureReason.INVALID` here, with the message the argument check of
 *     this library reports. The reason is compared as a value of the closed family of reasons and
 *     the message is pinned through `Regex.quote`, so the comparison is the literal message and
 *     not a pattern that happens to match it.
 *   - '''two amounts in different currencies''' were an `IllegalArgumentException` from `plus` and
 *     `minus` and are a `Left` carrying `INVALID` with the two wordings the original threw with,
 *     which are asserted exactly because those two are what a log line and a user-facing message
 *     carry.
 *   - '''text that names no amount''' was an `IllegalArgumentException` from `parse` and is a
 *     `Left` carrying `FailureReason.PARSING`, in the two wordings the original reported.
 *
 * ===Where a throw is still asserted, and why===
 *
 * Two places, and they are deliberate rather than left over.
 *
 * The arithmetic of this type keeps the total signature the implementation being ported had, and
 * a result that is not a number - reachable only by combining infinities - raises the documented
 * invariant through the argument check instead of widening every arithmetic method into a failure
 * channel. `test_numericEdges` asserts that, with the `IllegalArgumentException` the type
 * documents.
 *
 * `test_parse_String_bad` asserts a `NullPointerException` for the null row of the original's
 * provider. A null argument is outside the contract of every public entry point of this port: the
 * `ArgChecker.notNull` family was dropped, an absence is modelled by `Option`, and the port
 * declares no Java-callable façade that would have to defend against one. The Java method threw
 * on that row as well, so this is not a parity regression - it is the same rejection with a
 * different exception type - and a sibling unit records it in `SCALA_MIGRATION.md`.
 *
 * ===No absent argument can be passed===
 *
 * The five `*_nullCurrency` and `*_CurrencyAmount_null` methods of the original passed Java's
 * absent reference and asserted a run-time rejection. Each keeps its name here and asserts the
 * fact that replaced the check: the argument is of a type that cannot be omitted and that an
 * `Option` does not satisfy, so the call the original made does not compile, and the valid call
 * is asserted to compile alongside so that the proof cannot be passing for an unrelated reason.
 * A null literal still conforms to a reference type in this language, so these proofs are written
 * the way `FxRateSpec` writes them - a wrong type and a missing argument - and never as a null
 * literal, which would compile.
 *
 * ===What is asserted elsewhere===
 *
 * The property-based round trip of every codec of the module belongs to `json.JsonRoundTripSpec`,
 * the typeclass laws to `TypeclassLawsSpec`, the compile-time sweep over the construction surface
 * of every validated type to `SmartConstructorSpec`, and the fixture-driven numerical parity of
 * currency arithmetic to `parity.CurrencyMathParitySpec`. This suite asserts the cases of the
 * Java test it is ported from, which overlap those sweeps by design.
 *
 * @see [[CurrencyAmount]] for the type under test
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 * @see [[FxConvertible]] for the conversion contract this type implements
 */
final class CurrencyAmountSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The currency of the fixture amount, as the original named and valued it. */
  private val CCY1: Currency = AUD

  /** A second currency, for the cases where two amounts must disagree about theirs. */
  private val CCY2: Currency = CAD

  /** The amount of the fixture, written with the `d` suffix this build requires of a `Double`. */
  private val AMT1: Double = 100d

  /** A second amount, the operand of the arithmetic tests. */
  private val AMT2: Double = 200d

  /** The fixture amount, `AUD 100`. */
  private val CCY_AMOUNT: CurrencyAmount = amountOf(CCY1, AMT1)

  /** The fixture amount negated, `AUD -100`. */
  private val CCY_AMOUNT_NEGATIVE: CurrencyAmount = amountOf(CCY1, -AMT1)

  /**
   * A value of a type unrelated to an amount, for the equality assertion that needs one.
   *
   * Held at the type `Any` and named as the original named it, so that the assertion reads as a
   * comparison against a foreign value rather than as a comparison the compiler could reject.
   */
  private val ANOTHER_TYPE: Any = ""

  /** The message reported for a value that is not a number, by both routes into the type. */
  private val NotANumberMessage: String = "Argument 'amount' must not be NaN"

  //-------------------------------------------------------------------------
  /**
   * The rows of the original's `data_parseGood` provider, unchanged.
   *
   * The fifth row is the one worth reading twice: `USD -0` names a negative zero, which this type
   * normalises to a positive zero, so the expected amount is built through the same factory and
   * normalised the same way. The row is kept because the original had it and because it is the
   * only row that exercises the normalisation on the parse path.
   */
  private val data_parseGood: TableFor3[String, Currency, Double] = Table(
    ("input", "currency", "amount"),
    ("AUD 100.001", AUD, 100.001d),
    ("AUD 321.123", AUD, 321.123d),
    ("AUD 123", AUD, 123d),
    ("GBP 0", GBP, 0d),
    ("USD -0", USD, -0d),
    ("EUR -0.01", EUR, -0.01d))

  /**
   * The rows of the original's `data_parseBad` provider, each with the message it reports.
   *
   * The original asserted one blanket exception type for the whole table. Here every row carries
   * its expected wording, because the four rows are two failure modes and a single reason would
   * not tell them apart: text whose shape does not admit an amount at all - too short, or without
   * the separator at the fourth character - is reported as an invalid format, while text of the
   * right shape whose amount part names no number is reported as an unparsable amount. Asserting
   * the table with one reason alone would let a port that had collapsed the two wordings, or that
   * had stopped checking the shape before reading, pass unnoticed.
   *
   * The provider's fifth row, the null one, is asserted separately in the test: it is the one row
   * whose outcome is a throw, and it needs no wording.
   */
  private val data_parseBad: TableFor2[String, String] = Table(
    ("input", "message"),
    ("AUD", "Unable to parse amount, invalid format: AUD"),
    ("123", "Unable to parse amount, invalid format: 123"),
    ("AUD aa", "Unable to parse amount: AUD aa"),
    ("AUD -.+-", "Unable to parse amount: AUD -.+-"))

  //-------------------------------------------------------------------------
  test("test_fixture") {
    CCY_AMOUNT.currency shouldBe CCY1
    CCY_AMOUNT.amount shouldBe AMT1
  }

  //-------------------------------------------------------------------------
  test("test_zero_Currency") {
    val test: CurrencyAmount = CurrencyAmount.zero(USD)
    test.currency shouldBe USD
    test.amount shouldBe 0d
    test.isZero shouldBe true
    test.isPositive shouldBe false
    test.isNegative shouldBe false
  }

  /**
   * Asserts at compile time the absent-currency rejection the original asserted at run time.
   *
   * A zero amount needs a currency, and the argument is a `Currency` rather than anything that
   * could stand in for one: the text of a code is not accepted in its place, an `Option` is not
   * either, and the argument cannot be omitted. See the note on this class for why none of these
   * proofs is written as a null literal.
   */
  test("test_zero_Currency_nullCurrency") {
    assertDoesNotCompile("""CurrencyAmount.zero("USD")""")
    assertDoesNotCompile("""CurrencyAmount.zero(Option.empty[Currency])""")
    assertDoesNotCompile("""CurrencyAmount.zero()""")
    assertCompiles("""CurrencyAmount.zero(Currency.USD)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_Currency") {
    val test: CurrencyAmount = amountOf(USD, AMT1)
    test.currency shouldBe USD
    test.amount shouldBe AMT1
    test.isZero shouldBe false
    test.isPositive shouldBe true
    test.isNegative shouldBe false
  }

  test("test_of_Currency_negative") {
    val test: CurrencyAmount = amountOf(USD, -1d)
    test.currency shouldBe USD
    test.amount shouldBe -1d
    test.isZero shouldBe false
    test.isPositive shouldBe false
    test.isNegative shouldBe true
  }

  /**
   * Asserts that a negative zero is normalised away, by its bit pattern.
   *
   * The comparison is the original's, and it has to be by bit pattern: `-0.0 == 0.0` holds for
   * every pair of zeros, so an equality assertion would pass against an amount that had kept the
   * sign. What this test states is that no amount of this type ever holds a negative zero, which
   * is what makes two amounts that are both zero always equal.
   */
  test("test_of_Currency_negativeZero") {
    val test: CurrencyAmount = amountOf(USD, -0d)
    test.currency shouldBe USD
    java.lang.Double.doubleToLongBits(test.amount) shouldBe java.lang.Double.doubleToLongBits(0d)
    test.isZero shouldBe true
    test.isPositive shouldBe false
    test.isNegative shouldBe false
  }

  /**
   * Asserts that a value which is not a number is reported rather than thrown.
   *
   * The original asserted an `IllegalArgumentException` from the factory. Whether an amount is a
   * number is a property of the value a caller holds rather than of the calling code, so it is
   * reported as a failure the caller decides what to do about, with the message the argument
   * check of this library produces for that check.
   */
  test("test_of_Currency_NaN") {
    val outcome: FailureOr[CurrencyAmount] = CurrencyAmount.of(USD, Double.NaN)
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should haveFailureMessageMatching(Regex.quote(NotANumberMessage))

    // the two infinities are admitted, which is the asymmetry the implementation being ported
    // chose: a calculation that overflows still describes a direction
    CurrencyAmount.of(USD, Double.PositiveInfinity) should beSuccess
    CurrencyAmount.of(USD, Double.NegativeInfinity) should beSuccess
  }

  /**
   * Asserts at compile time the absent-currency rejection of the two-argument factory.
   *
   * As with `test_zero_Currency_nullCurrency`: the currency is a required argument of a type an
   * `Option` does not satisfy, and the amount cannot be omitted either. The reversed argument
   * order is included because it is the one mistake that a factory overloaded on its first
   * argument could plausibly have accepted.
   */
  test("test_of_Currency_nullCurrency") {
    assertDoesNotCompile("""CurrencyAmount.of(Option.empty[Currency], AMT1)""")
    assertDoesNotCompile("""CurrencyAmount.of(AMT1, Currency.USD)""")
    assertDoesNotCompile("""CurrencyAmount.of(Currency.USD)""")
    assertCompiles("""CurrencyAmount.of(Currency.USD, AMT1)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_String") {
    val test: CurrencyAmount = amountOf("USD", AMT1)
    test.currency shouldBe USD
    test.amount shouldBe AMT1

    // the code is resolved through the closed currency family of this port, so a code it does
    // not hold is reported rather than invented - which is the one behavioural divergence of
    // `Currency` and the reason this factory can fail for its first argument at all
    CurrencyAmount.of("AAA", AMT1) should beFailureWith(FailureReason.PARSING)
  }

  /**
   * Asserts at compile time the absent-code rejection of the text factory.
   *
   * The factory is overloaded on its first argument, which is why the original had to cast its
   * absent reference to a `String` to reach this one at all. Neither overload accepts an
   * `Option` in place of the code, and the amount still cannot be omitted; text that names no
   * currency is a reported failure rather than a rejected argument, which is asserted in
   * `test_of_String` above.
   */
  test("test_of_String_nullCurrency") {
    assertDoesNotCompile("""CurrencyAmount.of(Option.empty[String], AMT1)""")
    assertDoesNotCompile("""CurrencyAmount.of("USD")""")
    assertCompiles("""CurrencyAmount.of("USD", AMT1)""")
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_roundTrip") {
    CurrencyAmount.parse(CCY_AMOUNT.toString) should haveValue(CCY_AMOUNT)
  }

  test("test_parse_String_good") {
    forAll(data_parseGood) { (input: String, currency: Currency, amount: Double) =>
      CurrencyAmount.parse(input) should haveValue(amountOf(currency, amount))
    }
  }

  /**
   * Asserts the rows of the original's bad-text provider, and the null row it also held.
   *
   * The four rows of text are reported as parsing failures in the two wordings recorded on
   * [[data_parseBad]]. The fifth row is the null one, and it is the single place in this suite
   * that asserts a throw over an argument: a null argument is outside the contract of every
   * public entry point of this port - the `notNull` family of checks was dropped, an absence is
   * modelled by `Option`, and no Java-callable façade is declared that would have to defend
   * against one - so the read of the text fails where it first touches it. The original threw on
   * this row too, which is why the difference is the exception type and not the behaviour; a
   * sibling unit records the exact form in `SCALA_MIGRATION.md`.
   */
  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, message: String) =>
      val outcome: FailureOr[CurrencyAmount] = CurrencyAmount.parse(input)
      outcome should beFailureWith(FailureReason.PARSING)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    intercept[NullPointerException](CurrencyAmount.parse(null))
  }

  /**
   * Asserts that rejected text is quoted back bounded and on one line, in both wordings.
   *
   * No counterpart in the Java test class, and none was possible: the original interpolated the
   * text it was handed into the exception it threw, as it stood, so the size of the message was
   * the size of the input and a line break in the input was a line break in the message. This
   * port reports the rejection as a value, and the text it names is rendered rather than
   * reproduced. Both wordings are asserted, because both quote the text.
   */
  test("parsing rejects text of any size without echoing it unbounded or across lines") {
    // Ten thousand characters with no separator at the fourth position: the invalid-format
    // wording, which is the wording reached before anything is read from the text.
    val payload = "H" * 10000
    val bounded: FailureOr[CurrencyAmount] = CurrencyAmount.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    // The echo is the rendering the message is built from, so the message is the fixed wording
    // plus at most `MaxDescribedInput + 3` characters of the text, whatever its size - where it
    // was once the whole ten thousand.
    val message = bounded.left.toOption.map(failure => failure.message).getOrElse("")
    message.length should be <=
      "Unable to parse amount, invalid format: ".length + Failure.MaxDescribedInput + 3
    message shouldBe
      s"Unable to parse amount, invalid format: ${"H" * Failure.MaxDescribedInput}..."

    // The same payload behind a well-formed prefix reaches the other wording, which is bounded
    // by the same rendering.
    val longAmount: FailureOr[CurrencyAmount] = CurrencyAmount.parse(s"AUD $payload")
    longAmount should beFailureWith(FailureReason.PARSING)
    longAmount.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount: AUD ${"H" * (Failure.MaxDescribedInput - 4)}...")

    // Text holding a line break cannot put one in the message, so a line-oriented consumer of
    // the message cannot be made to record a line the library did not report. The line break is
    // placed to reach each wording in turn: at the separator position for the format failure,
    // and inside the amount part for the unparsable-amount failure.
    val injectedFormat = CurrencyAmount.parse("AUD\n1.5")
    injectedFormat should beFailureWith(FailureReason.PARSING)
    val injectedFormatMessage =
      injectedFormat.left.toOption.map(failure => failure.message).getOrElse("")
    injectedFormatMessage should not include "\n"
    injectedFormatMessage should not include "\r"
    injectedFormatMessage shouldBe "Unable to parse amount, invalid format: AUD\\n1.5"

    val injectedAmount = CurrencyAmount.parse("AUD 1.5\nINJECTED")
    injectedAmount should beFailureWith(FailureReason.PARSING)
    val injectedAmountMessage =
      injectedAmount.left.toOption.map(failure => failure.message).getOrElse("")
    injectedAmountMessage should not include "\n"
    injectedAmountMessage should not include "\r"
    injectedAmountMessage shouldBe "Unable to parse amount: AUD 1.5\\nINJECTED"

    // And the messages for ordinary rejected text are unchanged, character for character, which
    // is what makes the bound invisible to every caller but the adversarial one.
    CurrencyAmount.parse("AUD").left.toOption.map(failure => failure.message) shouldBe
      Some("Unable to parse amount, invalid format: AUD")
    CurrencyAmount.parse("AUD aa").left.toOption.map(failure => failure.message) shouldBe
      Some("Unable to parse amount: AUD aa")
  }

  //-------------------------------------------------------------------------
  test("test_plus_CurrencyAmount") {
    val ccyAmount: CurrencyAmount = amountOf(CCY1, AMT2)
    CCY_AMOUNT.plus(ccyAmount) should haveValue(amountOf(CCY1, AMT1 + AMT2))
  }

  /**
   * Asserts at compile time the absent-operand rejection of the addition.
   *
   * The operand is either an amount or a plain value - the two overloads - and neither accepts an
   * `Option` or nothing at all. As elsewhere in this suite, the proofs are a wrong type and a
   * missing argument rather than a null literal.
   */
  test("test_plus_CurrencyAmount_null") {
    assertDoesNotCompile("""CCY_AMOUNT.plus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""CCY_AMOUNT.plus()""")
    assertCompiles("""CCY_AMOUNT.plus(CCY_AMOUNT)""")
    assertCompiles("""CCY_AMOUNT.plus(AMT2)""")
  }

  test("test_plus_CurrencyAmount_wrongCurrency") {
    val outcome: FailureOr[CurrencyAmount] = CCY_AMOUNT.plus(amountOf(CCY2, AMT2))
    // an amount is a number *of* a currency, so there is no meaningful sum of amounts of
    // different ones; the mismatch depends on the values a caller holds, so it is reported
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should haveFailureMessageMatching(
      Regex.quote("Unable to add amounts in different currencies"))
  }

  test("test_plus_double") {
    CCY_AMOUNT.plus(AMT2) shouldBe amountOf(CCY1, AMT1 + AMT2)
  }

  //-------------------------------------------------------------------------
  test("test_minus_CurrencyAmount") {
    val ccyAmount: CurrencyAmount = amountOf(CCY1, AMT2)
    CCY_AMOUNT.minus(ccyAmount) should haveValue(amountOf(CCY1, AMT1 - AMT2))
  }

  /** As `test_plus_CurrencyAmount_null`, for the subtraction. */
  test("test_minus_CurrencyAmount_null") {
    assertDoesNotCompile("""CCY_AMOUNT.minus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""CCY_AMOUNT.minus()""")
    assertCompiles("""CCY_AMOUNT.minus(CCY_AMOUNT)""")
    assertCompiles("""CCY_AMOUNT.minus(AMT2)""")
  }

  test("test_minus_CurrencyAmount_wrongCurrency") {
    val outcome: FailureOr[CurrencyAmount] = CCY_AMOUNT.minus(amountOf(CCY2, AMT2))
    outcome should beFailureWith(FailureReason.INVALID)
    // the wording differs from the addition's by one word, and both are pinned, because a port
    // that reported one of them for both operations would be indistinguishable otherwise
    outcome should haveFailureMessageMatching(
      Regex.quote("Unable to subtract amounts in different currencies"))
  }

  test("test_minus_double") {
    CCY_AMOUNT.minus(AMT2) shouldBe amountOf(CCY1, AMT1 - AMT2)
  }

  //-------------------------------------------------------------------------
  test("test_multipliedBy") {
    CCY_AMOUNT.multipliedBy(3.5d) shouldBe amountOf(CCY1, AMT1 * 3.5d)
  }

  test("test_mapAmount") {
    // the operation is an ordinary function rather than the primitive-specialised interface of
    // the implementation being ported, which is the same thing expressed in this language
    CCY_AMOUNT.mapAmount(value => value * 2d + 1d) shouldBe amountOf(CCY1, AMT1 * 2d + 1d)
    CCY_AMOUNT.mapAmount(value => value).currency shouldBe CCY1
  }

  //-------------------------------------------------------------------------
  test("test_negated") {
    CCY_AMOUNT.negated shouldBe CCY_AMOUNT_NEGATIVE
    CCY_AMOUNT_NEGATIVE.negated shouldBe CCY_AMOUNT
    CurrencyAmount.zero(USD) shouldBe CurrencyAmount.zero(USD).negated
    amountOf(USD, -0d).negated shouldBe CurrencyAmount.zero(USD)
  }

  test("test_negative") {
    CCY_AMOUNT.negative shouldBe CCY_AMOUNT_NEGATIVE
    CCY_AMOUNT_NEGATIVE.negative shouldBe CCY_AMOUNT_NEGATIVE
  }

  test("test_positive") {
    CCY_AMOUNT.positive shouldBe CCY_AMOUNT
    CCY_AMOUNT_NEGATIVE.positive shouldBe CCY_AMOUNT
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts conversion at a rate the caller supplies, including the rate that must be one.
   *
   * The original asserted an `IllegalArgumentException` for the last case. Supplying a rate other
   * than one for a conversion into the currency the amount already has is a property of the
   * arguments, so it is reported; what makes the case worth keeping is that it stops a caller
   * scaling an amount by passing a rate for a conversion that does not happen.
   */
  test("test_convertedTo_explicitRate") {
    CCY_AMOUNT.convertedTo(CCY2, 2.5d) should haveValue(amountOf(CCY2, AMT1 * 2.5d))

    // a conversion into the currency the amount already has needs no arithmetic, so a rate of
    // one returns this amount - the same instance, which is what "unchanged" means here
    CCY_AMOUNT.convertedTo(CCY1, 1d) should haveValue(CCY_AMOUNT)
    CCY_AMOUNT.convertedTo(CCY1, 1d).map(result => result eq CCY_AMOUNT) should haveValue(true)

    val refused: FailureOr[CurrencyAmount] = CCY_AMOUNT.convertedTo(CCY1, 1.5d)
    refused should beFailureWith(FailureReason.INVALID)
    refused should haveFailureMessageMatching(
      Regex.quote("FX rate must be 1 when no conversion required"))
  }

  /**
   * Asserts conversion at a rate taken from a provider, and the [[FxConvertible]] contract.
   *
   * The original supplied its provider as a lambda returning a constant rate, which is
   * `FxRateProvider.fromFunction` here. Two things are asserted beyond the original's two
   * assertions, and both are properties of the implementation rather than of the test:
   *
   *   - an amount already in the requested currency is returned '''without the provider being
   *     consulted''', so such a conversion succeeds even under a provider that holds no rates at
   *     all. A counting function is what makes that observable: an implementation that asked for
   *     a rate and then discarded it would satisfy an assertion about the value alone.
   *   - this method is the [[FxConvertible]] implementation of the type, so it is exercised once
   *     through a reference of that type. `CurrencyAmount` is the only implementor of that trait
   *     in the module today - the multi-currency and exact-decimal types are not delivered yet -
   *     so this is the only place the contract is exercised at all.
   */
  test("test_convertedTo_rateProvider") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    CCY_AMOUNT.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    CCY_AMOUNT.convertedTo(CCY1, provider) should haveValue(CCY_AMOUNT)

    // the counter records every rate the provider is asked for. An `AtomicInteger` keeps it
    // without a mutable field, as the lazy provider's own spec does, and the returned count is
    // bound to a wildcard because only the total below is of interest.
    val consulted: AtomicInteger = new AtomicInteger(0)
    val counting: FxRateProvider = FxRateProvider.fromFunction { (_, _) =>
      val _ = consulted.incrementAndGet()
      Right(2.5d)
    }

    CCY_AMOUNT.convertedTo(CCY1, counting) should haveValue(CCY_AMOUNT)
    consulted.get shouldBe 0
    CCY_AMOUNT.convertedTo(CCY2, counting) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    consulted.get shouldBe 1

    // the same two conversions through the contract, from a reference of the trait's type
    val convertible: FxConvertible[CurrencyAmount] = CCY_AMOUNT
    convertible.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    convertible.convertedTo(CCY1, provider) should haveValue(CCY_AMOUNT)

    // the short circuit stated once more against a provider that supplies no rate for any pair,
    // the identity pair included: the conversion into the amount's own currency still succeeds,
    // because that provider is never asked. The cross-currency conversion is the same provider's
    // failure, which is how the failure channel of this method is the provider's own.
    val empty: FxRateProvider = FxRateProvider.noConversion()
    CCY_AMOUNT.convertedTo(CCY1, empty) should haveValue(CCY_AMOUNT)
    CCY_AMOUNT.convertedTo(CCY2, empty) should beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val other: CurrencyAmount = amountOf(CCY1, AMT1)

    // `equals` is called as a method, as the original called it, so that the whole matrix is
    // stated - including the reflexive case, which a `==` between two identical expressions
    // would be flagged for in this build
    CCY_AMOUNT.equals(CCY_AMOUNT) shouldBe true
    CCY_AMOUNT.equals(other) shouldBe true
    other.equals(CCY_AMOUNT) shouldBe true
    CCY_AMOUNT.hashCode shouldBe other.hashCode

    // an amount built afresh from the same arguments is one value with the fixture, not a second
    // name for it, which is what the original's second comparison stated
    CCY_AMOUNT shouldBe amountOf(CCY1, AMT1)
    CCY_AMOUNT.hashCode shouldBe amountOf(CCY1, AMT1).hashCode

    // the currency and the amount are both part of the identity of a value: the two amounts
    // below differ from the fixture in one of them each
    CCY_AMOUNT.equals(amountOf(CCY2, AMT1)) shouldBe false
    CCY_AMOUNT.equals(amountOf(CCY1, AMT2)) shouldBe false
  }

  test("test_equals_bad") {
    // the original also asserted that an amount does not equal Java's absent reference. That
    // case is subsumed by the types of this port, where a reference to an amount cannot be
    // absent, so it is recorded here rather than written; the foreign-type case is the part of
    // the method that remains expressible, and it is asserted in both directions
    CCY_AMOUNT.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(CCY_AMOUNT) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    // an amount that is a whole number is written without a fractional part, which is what the
    // implementation being ported wrote and what documents and expectations carry
    amountOf(AUD, 100d).toString shouldBe "AUD 100"
    amountOf(AUD, 100.123d).toString shouldBe "AUD 100.123"
  }

  //-----------------------------------------------------------------------
  /**
   * Asserts the JSON codec that replaced the original's Java-serialization round trip.
   *
   * The manifest records the original's `test_serialization` as consolidated into
   * `json.JsonRoundTripSpec`, where the property-based sweep over every codec of the module
   * lives. This test is the per-type representation of the same subject, as `CurrencySpec`'s is:
   * the document shape is pinned by example, and the decoder is shown to route its fields through
   * the same factory a caller's arguments go through, so a document naming an amount this type
   * would not have built is a decoding failure rather than a value that bypassed the check.
   */
  test("test_serialization") {
    val encoded: Json = CCY_AMOUNT.asJson
    encoded.noSpaces shouldBe """{"currency":"AUD","amount":100.0}"""
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("currency", "amount"))
    encoded.as[CurrencyAmount] shouldBe Right(CCY_AMOUNT)

    // the amount goes through the single policy this port has for a double, which writes the
    // values JSON cannot express as tagged strings, so every value an amount may hold survives
    // the round trip
    val infinite: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    infinite.asJson.noSpaces shouldBe """{"currency":"AUD","amount":"Infinity"}"""
    infinite.asJson.as[CurrencyAmount] shouldBe Right(infinite)

    // a document naming a value this type does not admit, a currency the family does not hold,
    // or missing a field altogether is rejected
    Json
      .obj("currency" -> Json.fromString("AUD"), "amount" -> Json.fromString("NaN"))
      .as[CurrencyAmount]
      .isLeft shouldBe true
    Json
      .obj("currency" -> Json.fromString("AAA"), "amount" -> Json.fromDoubleOrNull(1d))
      .as[CurrencyAmount]
      .isLeft shouldBe true
    Json.obj("currency" -> Json.fromString("AUD")).as[CurrencyAmount].isLeft shouldBe true
  }

  /**
   * Asserts the text contract the reflective string conversion of the original gave.
   *
   * The Java method asserted the round trip of the string-conversion library the type was
   * annotated for. That library is gone with the port, and the guarantee its annotations gave is
   * what this test asserts directly: the text form, the rendering instance and `toString` all
   * agree, and `parse` reads that text back as the same amount. Those are the identities the
   * migration treats as load-bearing, because a document, a log line or a test expectation
   * written before the port depends on them.
   */
  test("test_jodaConvert") {
    val rendered: String = CCY_AMOUNT.toString
    rendered shouldBe "AUD 100"
    Show[CurrencyAmount].show(CCY_AMOUNT) shouldBe rendered
    CurrencyAmount.parse(rendered) should haveValue(CCY_AMOUNT)

    // the same three identities over the values whose rendering is not a plain whole number: a
    // negative amount, a fractional one, and the two values outside the real numbers, which the
    // platform writes as `Infinity` and `-Infinity` and which `parse` reads back
    val others: List[CurrencyAmount] = List(
      CCY_AMOUNT_NEGATIVE,
      amountOf(GBP, 100.123d),
      amountOf(EUR, -0.01d),
      CurrencyAmount.zero(USD),
      amountOf(USD, Double.PositiveInfinity),
      amountOf(USD, Double.NegativeInfinity))
    others.foreach { amount =>
      withClue(s"$amount: ") {
        Show[CurrencyAmount].show(amount) shouldBe amount.toString
        CurrencyAmount.parse(amount.toString) should haveValue(amount)
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the ordering, hashing and equality instance, which no Java test class covers.
   *
   * The Java type was `Comparable` and its test asserted equality and hashing alone; the port
   * publishes one instance carrying all three, so this is where the ordering is pinned. It orders
   * by currency alphabetically and then by amount, which is how the implementation being ported
   * compared amounts, and it agrees with equality exactly: `compare` returns zero precisely when
   * `eqv` holds.
   *
   * The values outside the real numbers are the interesting half. A value that is not a number is
   * unrepresentable by construction - `CurrencyAmount.of` rejects it, which
   * `test_of_Currency_NaN` asserts - so the IEEE cases reachable here are the two infinities and
   * the zero whose sign construction normalises away, and both are asserted through the instance
   * rather than assumed.
   */
  test("test_typeclass_order") {
    val audLow: CurrencyAmount = amountOf(AUD, AMT1)
    val audHigh: CurrencyAmount = amountOf(AUD, AMT2)
    val cadLow: CurrencyAmount = amountOf(CAD, AMT1)

    // the currency is the first key, so an AUD amount orders before a CAD one whatever the two
    // amounts are, and the amount is the second
    (Order[CurrencyAmount].compare(audLow, audHigh) < 0) shouldBe true
    (Order[CurrencyAmount].compare(audHigh, audLow) > 0) shouldBe true
    (Order[CurrencyAmount].compare(audHigh, cadLow) < 0) shouldBe true
    (Order[CurrencyAmount].compare(cadLow, audHigh) > 0) shouldBe true
    List(cadLow, audHigh, audLow).sorted(Order[CurrencyAmount].toOrdering) shouldBe
      List(audLow, audHigh, cadLow)

    // `compare` returns zero exactly when `eqv` holds, and equal amounts hash alike
    Order[CurrencyAmount].compare(audLow, amountOf(AUD, AMT1)) shouldBe 0
    Order[CurrencyAmount].eqv(audLow, amountOf(AUD, AMT1)) shouldBe true
    Order[CurrencyAmount].eqv(audLow, audHigh) shouldBe false
    Order[CurrencyAmount].eqv(audLow, cadLow) shouldBe false
    Hash[CurrencyAmount].hash(audLow) shouldBe Hash[CurrencyAmount].hash(amountOf(AUD, AMT1))

    // the infinities order where their signs put them, and each is equal to itself
    val plusInfinity: CurrencyAmount = amountOf(AUD, Double.PositiveInfinity)
    val minusInfinity: CurrencyAmount = amountOf(AUD, Double.NegativeInfinity)
    (Order[CurrencyAmount].compare(minusInfinity, plusInfinity) < 0) shouldBe true
    (Order[CurrencyAmount].compare(audLow, plusInfinity) < 0) shouldBe true
    (Order[CurrencyAmount].compare(audLow, minusInfinity) > 0) shouldBe true
    Order[CurrencyAmount].compare(plusInfinity, amountOf(AUD, Double.PositiveInfinity)) shouldBe 0
    Order[CurrencyAmount].eqv(plusInfinity, amountOf(AUD, Double.PositiveInfinity)) shouldBe true
    Hash[CurrencyAmount].hash(plusInfinity) shouldBe
      Hash[CurrencyAmount].hash(amountOf(AUD, Double.PositiveInfinity))

    // a negative zero is normalised at construction, so the two zeros are one value here - which
    // is the one place this instance differs from a bit-pattern comparison of unnormalised
    // doubles, where they would not be
    val negativeZero: CurrencyAmount = amountOf(AUD, -0d)
    val positiveZero: CurrencyAmount = amountOf(AUD, 0d)
    Order[CurrencyAmount].compare(negativeZero, positiveZero) shouldBe 0
    Order[CurrencyAmount].eqv(negativeZero, positiveZero) shouldBe true
    Hash[CurrencyAmount].hash(negativeZero) shouldBe Hash[CurrencyAmount].hash(positiveZero)
  }

  /**
   * Asserts the numeric edges of the arithmetic, which no Java test class covers.
   *
   * The arithmetic of this type is total in signature, as the implementation being ported was,
   * and the invariant that no amount is a value which is not a number is raised through the
   * argument check rather than reported as a failure. That split is a recorded divergence of the
   * port - the data-dependent failures of the type are values, and the numeric edge only an
   * infinite operand can reach is an invariant - and this test is what pins it: each operation
   * below produces a value that is not a number from operands the type admits, and the last group
   * is the overflow that is '''not''' an edge, because an infinite result is a value an amount may
   * hold.
   */
  test("test_numericEdges") {
    val plusInfinity: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    val minusInfinity: CurrencyAmount = amountOf(CCY1, Double.NegativeInfinity)

    // infinities of opposite sign added, and of the same sign subtracted, are the two sums that
    // are not numbers. Both are reached through the amount-taking overloads, whose currency
    // check passes first, so the exception comes from the arithmetic and not from the currency.
    intercept[IllegalArgumentException](plusInfinity.plus(minusInfinity)).getMessage shouldBe
      NotANumberMessage
    intercept[IllegalArgumentException](plusInfinity.minus(plusInfinity)).getMessage shouldBe
      NotANumberMessage
    intercept[IllegalArgumentException](
      plusInfinity.plus(Double.NegativeInfinity)).getMessage shouldBe NotANumberMessage

    // an infinite amount multiplied by zero, and an operation that produces a value which is not
    // a number from an infinite operand
    intercept[IllegalArgumentException](plusInfinity.multipliedBy(0d)).getMessage shouldBe
      NotANumberMessage
    intercept[IllegalArgumentException](
      plusInfinity.mapAmount(value => value - value)).getMessage shouldBe NotANumberMessage

    // the currency check comes first, so a mismatch is still reported as a value even when the
    // sum of the two amounts would not have been a number
    plusInfinity.plus(amountOf(CCY2, Double.NegativeInfinity)) should
      beFailureWith(FailureReason.INVALID)

    // and the overflow that is allowed: the product of two finite amounts may be infinite, which
    // is a value this type admits, so nothing is raised and the result is that infinity
    amountOf(CCY1, Double.MaxValue).multipliedBy(2d) shouldBe plusInfinity
    amountOf(CCY1, Double.MaxValue).plus(Double.MaxValue) shouldBe plusInfinity
    amountOf(CCY1, -Double.MaxValue).multipliedBy(2d) shouldBe minusInfinity
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the amount out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite, and it exists because
   * [[CurrencyAmount.of]] is the public route into the type and reports what was wrong with its
   * arguments as a value. The outcome is matched rather than unwrapped by a partial accessor, so
   * a fixture that fails to build is reported as a test failure naming the reason instead of
   * raising an error from somewhere else in the suite.
   *
   * @param outcome  the outcome expected to carry an amount
   * @return the amount it carries
   */
  private def unwrap(outcome: FailureOr[CurrencyAmount]): CurrencyAmount =
    outcome.fold(
      failure => fail(s"Expected an amount but the factory failed with: ${failure.message}"),
      amount => amount)

  /**
   * Builds an amount, failing the test if the currency and value describe none.
   *
   * @param currency  the currency the amount is in
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the amount
   */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))

  /**
   * Builds an amount from a currency code, failing the test if the code and value describe none.
   *
   * The text factory has two ways to fail where the currency-taking one has one, so it gets its
   * own helper rather than being folded into [[amountOf]]: a code outside the closed currency
   * family of this port is reported, and `test_of_String` asserts that as a property of the
   * subject.
   *
   * @param currencyCode  the three letter ISO-4217 currency code, upper case
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the amount
   */
  private def amountOf(currencyCode: String, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currencyCode, amount))
}
