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
 * one-to-one correspondence in the migration manifest. This suite therefore holds those
 * thirty-three tests and exactly two more, so that a reader comparing it against the Java class
 * finds every method and no method that was never there, and every Java method the manifest names
 * joins to a test of this suite. The first addition is `test_toMoney_toBigMoney`, which asserts
 * the two conversions to the exact-decimal types. The original published both and tested neither
 * from here - `MoneyTest` and `BigMoneyTest` asserted each of them as the return leg of their own
 * round trips - and in this port each answers with an outcome rather than raising, so the case
 * that is genuinely this type's own is stated where the conversions live. The second is the
 * concrete comparison the ordering instance performs, which the Java class never tested and which
 * is therefore named as a sentence rather than after a method that does not exist. Neither maps to
 * a Java method and the manifest records a row for neither. One of the thirty-three keeps its name
 * and changes its subject: the original's
 * `test_serialization` round-tripped through Java serialization, which no type of this port
 * supports, so the manifest records that method as consolidated into `json.JsonRoundTripSpec` and
 * the `test_serialization` of this suite is the per-type representation of the codec that
 * replaced it, exactly as `CurrencySpec` is written.
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
 * ===No throw is asserted here===
 *
 * The arithmetic of this type keeps the total signature the implementation being ported had, and
 * a result that is not a number - reachable only by combining infinities of opposing sign -
 * raises the documented invariant through the argument check instead of widening every arithmetic
 * method into a failure channel. That numeric edge is asserted once for the whole module, in the
 * root `SmartConstructorSpec`, so no test of this suite intercepts an exception. What this suite
 * states about the same operations is the other half of the divergence: they are '''total''' for
 * every operand pair the type admits and reaches a result with, the overflow of a finite
 * calculation to an infinity included, which `test_plus_double` and `test_multipliedBy` assert.
 *
 * ===No absent argument can be passed===
 *
 * The five `*_nullCurrency` and `*_CurrencyAmount_null` methods of the original passed the absent
 * reference of the language it was written in and asserted a run-time rejection. Each keeps its
 * name here and asserts the fact that replaced the check: the argument is of a type that cannot
 * be omitted and that an `Option` does not satisfy, so the call the original made does not
 * compile, and the valid call is asserted to compile alongside so that the proof cannot be
 * passing for an unrelated reason. There is no run-time guard to assert instead - the
 * `ArgChecker.notNull` family has no target in this port, an absence is modelled by `Option`, and
 * no Java-callable façade is declared that would have to defend against one. An absent reference
 * still conforms to a reference type in this language, so these proofs are written the way
 * `FxRateSpec` writes them - a wrong type and a missing argument - and never as that literal,
 * which would compile and assert nothing.
 *
 * ===What is asserted elsewhere===
 *
 * The property-based round trip of every codec of the module belongs to `json.JsonRoundTripSpec`,
 * the '''laws''' of the ordering, hashing and rendering instances to `TypeclassLawsSpec`, the
 * sweep over the construction surface of every validated type - with the documented numeric edge
 * of this type's arithmetic among them - to `SmartConstructorSpec`, the compile-time proof that
 * the type publishes no `apply` and no `copy` to `ApiSurfaceSpec`, and the fixture-driven
 * numerical parity of currency arithmetic against the Java baseline to
 * `parity.CurrencyMathParitySpec`.
 *
 * What the law suite owns is the laws of the ordering and nothing further: every lawful total
 * order satisfies them and only one such order is this type's, so the comparison the instance
 * actually performs is asserted here, in the second of this suite's two additions. This suite
 * therefore asserts the cases of the Java test it is ported from plus its two additions - the
 * conversions to the exact-decimal types and that comparison - and nothing else those sweeps
 * own, which is what keeps it at thirty-five tests and keeps a failure here attributable to one
 * ported method, to the conversions or to the ordering.
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
   * its expected wording, because the rows are two failure modes and a single reason would not
   * tell them apart: text whose shape does not admit an amount at all - too short, or without the
   * separator at the fourth character - is reported as an invalid format, while text of the right
   * shape whose amount part names no admissible number is reported as an unparsable amount.
   * Asserting the table with one reason alone would let a port that had collapsed the two
   * wordings, or that had stopped checking the shape before reading, pass unnoticed.
   *
   * The provider's fifth row held the absent reference of the language the original was written
   * in. It is substituted here by `GBP NaN` rather than dropped, so that the table keeps five
   * rows and the substitute earns its place: an absent argument cannot be passed to this method at
   * all - the parameter is a `String` and this port declares no façade that would have to defend
   * against one - whereas `GBP NaN` is text of exactly the right shape that names a number
   * '''this type does not admit''', which is the one rejection path of `parse` that no other row
   * reaches. It therefore replaces an assertion that can no longer be written with one that
   * covers the code the original row never did.
   */
  private val data_parseBad: TableFor2[String, String] = Table(
    ("input", "message"),
    ("AUD", "Unable to parse amount, invalid format: AUD"),
    ("123", "Unable to parse amount, invalid format: 123"),
    ("AUD aa", "Unable to parse amount: AUD aa"),
    ("AUD -.+-", "Unable to parse amount: AUD -.+-"),
    ("GBP NaN", "Unable to parse amount: GBP NaN"))

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
   * proofs is written as the absent-reference literal of the language.
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

  /**
   * Asserts the rows of the original's good-text provider, and the signed zero among them.
   *
   * Each row is compared against the amount its currency and value build, and the comparison is
   * by bit pattern - it is the equality of the type - so the `USD -0` row already states that the
   * parse path normalises. The bit pattern of that row's amount is nonetheless read out and
   * asserted directly afterwards, because the row is the only one whose expectation would be
   * satisfied by an unnormalised value under the equality any other type in the module would have
   * had, and a reader of this test should not have to derive that from two files.
   */
  test("test_parse_String_good") {
    forAll(data_parseGood) { (input: String, currency: Currency, amount: Double) =>
      CurrencyAmount.parse(input) should haveValue(amountOf(currency, amount))
    }

    // the negative-zero row, read out by bit pattern: `-0.0 == 0.0` holds for every pair of
    // zeros, so only this comparison distinguishes a normalised amount from one that kept the
    // sign the text carried
    val negativeZero: CurrencyAmount = unwrap(CurrencyAmount.parse("USD -0"))
    java.lang.Double.doubleToLongBits(negativeZero.amount) shouldBe
      java.lang.Double.doubleToLongBits(0d)
  }

  /**
   * Asserts the rows of the original's bad-text provider, and how rejected text is quoted back.
   *
   * The five rows of [[data_parseBad]] are reported as parsing failures in the two wordings
   * recorded there, each pinned through `Regex.quote` so the assertion is the literal message
   * rather than a pattern that happens to match it. The fifth row is the substitute for the one
   * the original could write and this port cannot, and the reasoning for it is on
   * [[data_parseBad]].
   *
   * The second half of this test is the one thing the original could not assert. The original
   * interpolated the text it was handed into the exception it threw, as it stood, and this port
   * names it the same way in the failure it returns - so the wording is that one, and a caller is
   * handed back exactly what was refused. What the port adds is the boundary at which such a
   * failure is written out: its rendering bounds every part and escapes anything that could forge
   * a line of a log holding it. Both wordings quote the text, so both are asserted.
   */
  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, message: String) =>
      val outcome: FailureOr[CurrencyAmount] = CurrencyAmount.parse(input)
      outcome should beFailureWith(FailureReason.PARSING)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    // Ten thousand characters with no separator at the fourth position reach the invalid-format
    // wording, which is the wording reached before anything is read from the text, and the
    // failure names the whole of what it refused.
    val payload: String = "H" * 10000
    val bounded: FailureOr[CurrencyAmount] = CurrencyAmount.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    bounded.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount, invalid format: $payload")
    // The rendering of that failure is where the size stops: the ten thousand characters reach a
    // log as a few hundred, marked to say that there was more.
    val rendered = Show[Failure].show(bounded.left.toOption.getOrElse(fail("expected a failure")))
    rendered.length should be < 1000
    rendered should startWith("PARSING: Unable to parse amount, invalid format: HHH")
    rendered should endWith("...")

    // The same payload behind a well-formed prefix reaches the other wording, named in full and
    // bounded by the same rendering.
    val prefixed: FailureOr[CurrencyAmount] = CurrencyAmount.parse(s"AUD $payload")
    prefixed.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount: AUD $payload")
    Show[Failure]
      .show(prefixed.left.toOption.getOrElse(fail("expected a failure")))
      .length should be < 1000

    // Text holding a line break is named as it stands and rendered on one line, so a
    // line-oriented consumer of the rendering cannot be made to record a line the library did not
    // report. The break is placed to reach each wording in turn: at the separator position for
    // the format failure, and inside the amount part for the unparsable-amount failure.
    CurrencyAmount.parse("AUD\n1.5") should haveFailureMessageMatching(
      Regex.quote("Unable to parse amount, invalid format: AUD\n1.5"))
    CurrencyAmount.parse("AUD 1.5\nINJECTED") should haveFailureMessageMatching(
      Regex.quote("Unable to parse amount: AUD 1.5\nINJECTED"))
    val injected: FailureOr[CurrencyAmount] = CurrencyAmount.parse("AUD 1.5\nINJECTED")
    Show[Failure].show(injected.left.toOption.getOrElse(fail("expected a failure"))) shouldBe
      "PARSING: Unable to parse amount: AUD 1.5\\nINJECTED"
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
   * missing argument rather than the absent-reference literal of the language.
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

    // the currency check comes first, so a mismatch is reported as a value even for two operands
    // whose sum the arithmetic could not have produced at all
    amountOf(CCY1, Double.PositiveInfinity).plus(amountOf(CCY2, Double.NegativeInfinity)) should
      beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the total addition of a plain value, including the operands that reach an infinity.
   *
   * The value is taken to be in the currency of the amount, so no currency can disagree and the
   * addition is total, as it was in the implementation being ported. That totality is asserted
   * over the operands where it is least obvious: a finite sum that overflows the range of a
   * `Double`, and an operand that is already infinite. Both produce an infinity, which is a value
   * this type admits, so both return an amount rather than reporting or raising anything. The one
   * sum that is not a number - two infinities of opposing sign - is the documented invariant of
   * the type and is asserted for the whole module in the root `SmartConstructorSpec`.
   */
  test("test_plus_double") {
    CCY_AMOUNT.plus(AMT2) shouldBe amountOf(CCY1, AMT1 + AMT2)
    amountOf(CCY1, Double.MaxValue).plus(Double.MaxValue) shouldBe
      amountOf(CCY1, Double.PositiveInfinity)
    amountOf(CCY1, Double.PositiveInfinity).plus(AMT2) shouldBe
      amountOf(CCY1, Double.PositiveInfinity)
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
  /**
   * Asserts the total multiplication, including the product that overflows to an infinity.
   *
   * The multiplication is written as the amount times the value, the order the implementation
   * being ported used, so a product that rounds differently under the other order rounds the same
   * way here. As with the addition, the product of two finite operands may be infinite, and an
   * infinity is a value this type admits, so the operation stays total there.
   */
  test("test_multipliedBy") {
    CCY_AMOUNT.multipliedBy(3.5d) shouldBe amountOf(CCY1, AMT1 * 3.5d)
    amountOf(CCY1, Double.MaxValue).multipliedBy(2d) shouldBe
      amountOf(CCY1, Double.PositiveInfinity)
    amountOf(CCY1, -Double.MaxValue).multipliedBy(2d) shouldBe
      amountOf(CCY1, Double.NegativeInfinity)
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
   * Asserts the two conversions to the exact-decimal types, in the direction this type owns.
   *
   * The original published `toMoney` and `toBigMoney` here and asserted them from the other side,
   * as the return leg of `MoneyTest.testToCurrencyAmount` and `BigMoneyTest.testToCurrencyAmount`.
   * Both round trips are still asserted there, in the original's form; what this test adds is the
   * three things that belong to this end of the conversion and to no Java test:
   *
   *   - each conversion equals the factory of the type converted to, which is how the original
   *     defined it - `Money.of(currency, amount)` and `BigMoney.of(currency, amount)`;
   *   - the two differ in what they round to, and the difference is observable rather than
   *     notional: the Australian dollar quotes two digits, so `AUD 100.125` becomes `AUD 100.13`
   *     as money and stays `AUD 100.125` at scale twelve;
   *   - an '''infinite''' amount, which this type admits and no decimal holds, is reported by both
   *     rather than raised. That is the port's divergence: the original raised from exactly the
   *     same decimal conversion, so the case existed there too and had no value to carry it.
   */
  test("test_toMoney_toBigMoney") {
    CCY_AMOUNT.toMoney shouldBe Money.of(CCY1, AMT1)
    CCY_AMOUNT.toBigMoney shouldBe BigMoney.of(CCY1, AMT1)

    // money rounds to the minor units the currency quotes, while the wider type keeps the digit
    // that rounding would have dropped
    amountOf(CCY1, 100.125d).toMoney.map(money => money.amount.toString) shouldBe Right("100.13")
    amountOf(CCY1, 100.125d).toBigMoney.map(money => money.amount.toString) shouldBe Right("100.125")

    // an amount already within the currency's minor units survives both trips unchanged, which is
    // the identity the two money suites assert as their return leg
    CCY_AMOUNT.toMoney.map(money => money.toCurrencyAmount) shouldBe Right(CCY_AMOUNT)
    CCY_AMOUNT.toBigMoney.map(money => money.toCurrencyAmount) shouldBe Right(CCY_AMOUNT)

    // and the one value this type admits that no decimal holds is reported by both conversions
    amountOf(CCY1, Double.PositiveInfinity).toMoney should beFailureWith(FailureReason.INVALID)
    amountOf(CCY1, Double.PositiveInfinity).toBigMoney should beFailureWith(FailureReason.INVALID)
    amountOf(CCY1, Double.NegativeInfinity).toMoney should beFailureWith(FailureReason.INVALID)
    amountOf(CCY1, Double.NegativeInfinity).toBigMoney should beFailureWith(FailureReason.INVALID)
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
  /**
   * Asserts the equality and hashing matrix of the original, through both of its entries.
   *
   * The matrix is stated twice over: once through `equals` and `hashCode`, as the original stated
   * it, and once through the `Hash` instance the port publishes, because that instance is what
   * every generic caller of this type - a set, a map key, a law suite, a codec property - actually
   * consults. The two must agree, and a port that defined the instance over anything but the
   * methods would be caught here rather than in a distant law failure.
   *
   * Two cases go beyond the original's matrix and both are properties of the bit-pattern equality
   * this type uses. An infinite amount equals another built from the same infinity, which a
   * comparison written with `==` on the doubles would also have given; and the two zeros are one
   * value here, because construction normalises the sign away, which is the single place this
   * equality differs from a bit comparison of unnormalised doubles. The laws of the instance -
   * reflexivity, symmetry, transitivity and the hash agreeing with equality in general - belong to
   * the root `TypeclassLawsSpec`, and so do the laws of the ordering the same instance carries
   * with it, whose concrete comparison is asserted by the ordering test below; this test asserts
   * the values the ported method named plus the two the type's own normalisation makes
   * interesting.
   */
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

    // the same matrix through the instance the port publishes, which is what a generic caller
    // consults and which must agree with the methods above
    Hash[CurrencyAmount].eqv(CCY_AMOUNT, other) shouldBe true
    Hash[CurrencyAmount].hash(CCY_AMOUNT) shouldBe Hash[CurrencyAmount].hash(other)
    Hash[CurrencyAmount].eqv(CCY_AMOUNT, amountOf(CCY2, AMT1)) shouldBe false
    Hash[CurrencyAmount].eqv(CCY_AMOUNT, amountOf(CCY1, AMT2)) shouldBe false

    // an infinity equals the same infinity, and hashes alike
    val plusInfinity: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    Hash[CurrencyAmount].eqv(plusInfinity, amountOf(CCY1, Double.PositiveInfinity)) shouldBe true
    Hash[CurrencyAmount].eqv(plusInfinity, amountOf(CCY1, Double.NegativeInfinity)) shouldBe false
    Hash[CurrencyAmount].hash(plusInfinity) shouldBe
      Hash[CurrencyAmount].hash(amountOf(CCY1, Double.PositiveInfinity))

    // and the two zeros are one value, because construction normalised the sign away - the case
    // that makes `test_of_Currency_negativeZero` load-bearing rather than decorative
    amountOf(CCY1, -0d) shouldBe amountOf(CCY1, 0d)
    Hash[CurrencyAmount].eqv(amountOf(CCY1, -0d), amountOf(CCY1, 0d)) shouldBe true
    Hash[CurrencyAmount].hash(amountOf(CCY1, -0d)) shouldBe
      Hash[CurrencyAmount].hash(amountOf(CCY1, 0d))
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
  /**
   * Asserts the comparison the ordering instance performs, not merely that it is lawful.
   *
   * This test has no counterpart in the Java class, which never tested `compareTo`, so it is a
   * test of this port and carries no manifest row. It exists because the law suite of
   * `TypeclassLawsSpec` proves only that the instance is a total order consistent with itself and
   * agreeing with equality - properties an ordering that read the amount before the currency, or
   * that reversed either of the two comparisons, would satisfy exactly as well. Without the
   * assertions below, any of those orderings would keep this module green while sorting a report
   * of amounts into an order the implementation being ported never produced.
   *
   * What is asserted is therefore the comparison itself, in the two parts the port is held to: the
   * currency first, alphabetically by code, and the amount second by `java.lang.Double.compare`
   * [[CurrencyAmount.order]]. Every pair is stated in both directions, because a sign is only
   * pinned by its opposite; the equal case is stated as `compare == 0` and tied to the equality
   * this suite asserts elsewhere, because the two are required to agree for every value; and the
   * three values outside the ordinary ones that the second comparison makes interesting - a
   * negative zero and the two infinities - are stated last.
   */
  test("the ordering compares the currency before the amount and agrees with equality") {
    val order: Order[CurrencyAmount] = implicitly[Order[CurrencyAmount]]

    // the currency decides the result whatever the amounts are, so a large amount of the earlier
    // code is less than a small amount of the later one - the assertion an ordering that read the
    // amount first would fail in both directions
    val audLarge: CurrencyAmount = amountOf(CCY1, AMT1)
    val cadSmall: CurrencyAmount = amountOf(CCY2, 1d)
    (order.compare(audLarge, cadSmall) < 0) shouldBe true
    (order.compare(cadSmall, audLarge) > 0) shouldBe true

    // the same comparison with the amounts equal, so the sign above is the currency's and not an
    // accident of the two amounts chosen for it
    (order.compare(amountOf(CCY1, AMT1), amountOf(CCY2, AMT1)) < 0) shouldBe true
    (order.compare(amountOf(CCY2, AMT1), amountOf(CCY1, AMT1)) > 0) shouldBe true

    // within one currency the amount decides, ascending: a lower amount, the same amount and a
    // higher one, each in both directions, so a reversed amount comparison fails here
    (order.compare(CCY_AMOUNT_NEGATIVE, CCY_AMOUNT) < 0) shouldBe true
    (order.compare(CCY_AMOUNT, CCY_AMOUNT_NEGATIVE) > 0) shouldBe true
    (order.compare(CCY_AMOUNT, amountOf(CCY1, AMT2)) < 0) shouldBe true
    (order.compare(amountOf(CCY1, AMT2), CCY_AMOUNT) > 0) shouldBe true
    order.compare(CCY_AMOUNT, amountOf(CCY1, AMT1)) shouldBe 0

    // the two parts stated together as the order a caller sees: sorting a mixed list groups the
    // amounts by currency first and orders each group by amount, which is the arrangement the
    // implementation being ported produced
    val mixed: List[CurrencyAmount] =
      List(cadSmall, amountOf(CCY1, AMT2), CCY_AMOUNT_NEGATIVE, audLarge)
    mixed.sorted(order.toOrdering) shouldBe
      List(CCY_AMOUNT_NEGATIVE, audLarge, amountOf(CCY1, AMT2), cadSmall)

    // a zero comparison is equality and a non-zero comparison is inequality, over every pair of
    // those four values: this is what ties the ordering to the equality `test_equals_hashCode`
    // asserts, and it holds in both directions of the iteration because the sweep is complete
    mixed.foreach { left =>
      mixed.foreach { right =>
        withClue(s"$left vs $right: ") {
          (order.compare(left, right) == 0) shouldBe (left == right)
        }
      }
    }

    // `java.lang.Double.compare(-0.0, 0.0)` is negative, so an ordering over unnormalised amounts
    // would sort an amount built from a negative zero below one built from a positive zero.
    // Construction normalises the sign away - the normalisation `test_of_Currency_negativeZero`
    // asserts - so the two are one value here and the comparison is zero, which is also what
    // agreement with equality requires of them
    order.compare(amountOf(CCY1, -0d), amountOf(CCY1, 0d)) shouldBe 0

    // the two infinities are amounts this type admits, and each sorts outside every finite amount
    // of the same currency - which `java.lang.Double.compare` gives and a subtraction of the two
    // amounts would not
    val negativeInfinity: CurrencyAmount = amountOf(CCY1, Double.NegativeInfinity)
    val positiveInfinity: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    (order.compare(negativeInfinity, CCY_AMOUNT_NEGATIVE) < 0) shouldBe true
    (order.compare(CCY_AMOUNT_NEGATIVE, negativeInfinity) > 0) shouldBe true
    (order.compare(positiveInfinity, amountOf(CCY1, AMT2)) > 0) shouldBe true
    (order.compare(amountOf(CCY1, AMT2), positiveInfinity) < 0) shouldBe true

    // and the currency still decides first against an infinite amount, so the second comparison
    // is reached only for two amounts in one currency
    (order.compare(positiveInfinity, amountOf(CCY2, Double.NegativeInfinity)) < 0) shouldBe true
    (order.compare(amountOf(CCY2, Double.NegativeInfinity), positiveInfinity) > 0) shouldBe true
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
