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
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[CurrencyAmount]].
 *
 * [[CurrencyAmount.of]] reports a value this type does not admit rather than raising it, so an
 * amount built through it goes through [[amountOf]], which unwraps the outcome once and fails the
 * test naming the cause when it carries none. [[CurrencyAmount.zero]] is total, so amounts built
 * through it stand as they are, and a parse outcome is unwrapped the same way where a test reads
 * a value out of it.
 */
final class CurrencyAmountSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val CCY1: Currency = AUD

  private val CCY2: Currency = CAD

  private val AMT1: Double = 100d

  private val AMT2: Double = 200d

  private val CCY_AMOUNT: CurrencyAmount = amountOf(CCY1, AMT1)

  private val CCY_AMOUNT_NEGATIVE: CurrencyAmount = amountOf(CCY1, -AMT1)

  /** A value unrelated to an amount, held at `Any` so the foreign-value comparison compiles. */
  private val ANOTHER_TYPE: Any = ""

  /** The message `of` reports for an amount that is not a number; pinned because a log shows it. */
  private val NotANumberMessage: String = "Argument 'amount' must not be NaN"

  /**
   * The longest the amount part of the text form may be, as [[CurrencyAmount]] states it.
   *
   * Restated here because it is not visible outside that companion and because both sides of it
   * are asserted. The value is the 256 characters [[Money]] and [[BigMoney]] read the same text
   * form within: one bound holds across the four types of this package that read a number out of
   * text, where this type once carried a thousand characters calibrated on the longest exact
   * decimal spelling of a double.
   */
  private val MaxAmountTextLength: Int = 256

  /**
   * The longest text a rejection quotes back in full, which is the longest text this type accepts.
   *
   * A three letter currency code, the separator after it and an amount at the ceiling above.
   * Text within it is named character for character, and text beyond it is named through the
   * bounded renderer, so the cost of a rejection is capped by this type rather than chosen by
   * its caller.
   */
  private val MaxQuotedTextLength: Int = 3 + 1 + MaxAmountTextLength

  /**
   * The greatest number of characters of rejected text a message can carry.
   *
   * The bounded renderer of a failure writes at most five hundred and twelve characters of a part
   * and then the three of an ellipsis, so this is the cap a message reaches however long the
   * rejected text was.
   */
  private val MaxRenderedMessagePart: Int = 512 + 3

  //-------------------------------------------------------------------------
  /**
   * Text that parses, with the currency and amount each row names.
   *
   * The `USD -0` row is the only one that exercises the sign normalisation on the parse path; its
   * expected amount is built through the same factory, so it is normalised the same way.
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
   * Text that does not parse, each row with the message it reports.
   *
   * Every row carries its own wording because the rows are two distinct failure modes: text whose
   * shape admits no amount - too short, or without the separator at the fourth character - is an
   * invalid format, while text of the right shape whose amount part names no admissible number is
   * an unparsable amount. One blanket reason would not tell the two apart.
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
   * Asserts that construction normalises a negative zero away.
   *
   * The comparison goes through `doubleToLongBits`, which tells `-0.0` from `+0.0`, because
   * `-0.0 == 0.0` is true in IEEE arithmetic: `==` would pass whether or not `of` normalises.
   * Since no amount holds a negative zero, two amounts that are both zero are always equal.
   */
  test("test_of_Currency_negativeZero") {
    val test: CurrencyAmount = amountOf(USD, -0d)
    test.currency shouldBe USD
    java.lang.Double.doubleToLongBits(test.amount) shouldBe java.lang.Double.doubleToLongBits(0d)
    test.isZero shouldBe true
    test.isPositive shouldBe false
    test.isNegative shouldBe false
  }

  test("test_of_Currency_NaN") {
    val outcome: FailureOr[CurrencyAmount] = CurrencyAmount.of(USD, Double.NaN)
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should haveFailureMessageMatching(Regex.quote(NotANumberMessage))

    // `of` rejects a value that is not a number but admits the two infinities: a calculation
    // that overflows still describes a direction
    CurrencyAmount.of(USD, Double.PositiveInfinity) should beSuccess
    CurrencyAmount.of(USD, Double.NegativeInfinity) should beSuccess
  }

  /**
   * Asserts that the single check of `of` reports the value the accumulating validators of the
   * collect module report for the same rejection.
   *
   * `of` tests the one thing that can be wrong with its arguments directly and answers with a
   * failure it holds as a constant, rather than building a `Validated`, converting it, chaining
   * it and collapsing a chain of one. The equality asserted here is what makes that an
   * implementation detail: the whole failure - its reason, its message and its attributes - is
   * compared against what `Validate.notNaN` and `Failure.collapse` produce, so the assertion is
   * not a copy of the wording but the wording's source. A change to the argument-check message of
   * either module is therefore a failing test here rather than a divergence between the value
   * this factory reports and the one the invariant of the type raises.
   */
  test("the single check of of reports what the accumulating validators report") {
    val reported: Failure = CurrencyAmount
      .of(USD, Double.NaN)
      .left
      .toOption
      .getOrElse(fail("expected a failure"))
    val accumulated: Failure = Validate
      .notNaN(Double.NaN, "amount")
      .toEither
      .left
      .toOption
      .map(Failure.collapse)
      .getOrElse(fail("expected the validator to reject a value that is not a number"))

    reported shouldBe accumulated
    reported.reason shouldBe accumulated.reason
    reported.message shouldBe accumulated.message
    reported.attributes shouldBe accumulated.attributes
    reported.attributes shouldBe empty
    Show[Failure].show(reported) shouldBe Show[Failure].show(accumulated)
    reported.asJson.noSpaces shouldBe accumulated.asJson.noSpaces

    // the same wording reaches a caller through the invariant of the type, which is the route a
    // value that is not a number takes when it is produced by the arithmetic rather than supplied
    the[IllegalArgumentException] thrownBy CCY_AMOUNT.multipliedBy(
      Double.NaN) should have message NotANumberMessage
  }

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

    // the code is resolved through the closed currency family, so a code it does not hold is
    // reported rather than invented - which is why this factory can fail for its first argument
    CurrencyAmount.of("AAA", AMT1) should beFailureWith(FailureReason.PARSING)
  }

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

    // the negative-zero row read out through `doubleToLongBits`, which tells `-0.0` from `+0.0`:
    // `-0.0 == 0.0` is true in IEEE arithmetic, so no `==` comparison could distinguish a
    // normalised amount from one that kept the sign the text carried
    val negativeZero: CurrencyAmount = unwrap(CurrencyAmount.parse("USD -0"))
    java.lang.Double.doubleToLongBits(negativeZero.amount) shouldBe
      java.lang.Double.doubleToLongBits(0d)
  }

  /**
   * Asserts the rejected rows of [[data_parseBad]], and how rejected text is quoted back.
   *
   * Each message is pinned through `Regex.quote`, so the assertion is the literal message a log
   * or a user sees rather than a pattern that happens to match it.
   *
   * The property the quotation has is that text within the length this type can accept is named
   * in full and text beyond it is named bounded, so a rejection cannot be made to cost more than
   * the ceiling however long the input is. Both sides of that boundary are asserted below, and
   * the rendering - which bounds and escapes every part it writes - is asserted on top of it,
   * because the two bounds are independent: one caps what the failure carries, the other caps
   * what a reader of lines is shown.
   */
  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, message: String) =>
      val outcome: FailureOr[CurrencyAmount] = CurrencyAmount.parse(input)
      outcome should beFailureWith(FailureReason.PARSING)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    // text with no separator at the fourth position is refused on its shape, before anything is
    // read from it; ten thousand characters is far beyond any text that could have named an
    // amount, so the failure names what it refused bounded rather than in full
    val payload: String = "H" * 10000
    val bounded: FailureOr[CurrencyAmount] = CurrencyAmount.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    val boundedMessage: String =
      bounded.left.toOption.map(failure => failure.message).getOrElse(fail("expected a failure"))
    boundedMessage should startWith("Unable to parse amount, invalid format: HHH")
    boundedMessage should endWith("...")
    boundedMessage.length should be < 1000
    // the rendering bounds what a log is shown on top of that, and still marks that there was
    // more
    val rendered = Show[Failure].show(bounded.left.toOption.getOrElse(fail("expected a failure")))
    rendered.length should be < 1000
    rendered should startWith("PARSING: Unable to parse amount, invalid format: HHH")
    rendered should endWith("...")

    // the other wording is bounded in exactly the same way, and for the same reason
    val prefixed: FailureOr[CurrencyAmount] = CurrencyAmount.parse(s"AUD $payload")
    val prefixedMessage: String =
      prefixed.left.toOption.map(failure => failure.message).getOrElse(fail("expected a failure"))
    prefixedMessage should startWith("Unable to parse amount: AUD HHH")
    prefixedMessage should endWith("...")
    prefixedMessage.length should be < 1000
    Show[Failure]
      .show(prefixed.left.toOption.getOrElse(fail("expected a failure")))
      .length should be < 1000

    // a rejection whose text could have named an amount is quoted in full, whatever it holds: a
    // hundred characters of it, the whole of which appears in the message
    val realistic: String = "ZZZ " + ("9" * 100)
    CurrencyAmount.parse(realistic).left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount: $realistic")

    // and the boundary between the two is the longest text this type accepts - a three letter
    // code, the separator and a numeral at the ceiling, which is 260 characters. Either side of
    // it is asserted through a raw line break, because that is what tells the two quotations
    // apart at a length where they are otherwise the same characters: within the bound the text
    // is handed back untouched, so the break survives into `message` and is escaped only when
    // the failure is written out, while beyond the bound the text goes through the renderer,
    // which escapes it there and then
    val atQuotationBound: String = "ZZZ\n" + ("8" * (MaxAmountTextLength - 1)) + "x"
    atQuotationBound.length shouldBe MaxQuotedTextLength
    CurrencyAmount.parse(atQuotationBound).left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount, invalid format: $atQuotationBound")
    val pastQuotationBound: String = "ZZZ\n" + ("8" * MaxAmountTextLength) + "x"
    pastQuotationBound.length shouldBe MaxQuotedTextLength + 1
    val pastMessage: String = CurrencyAmount
      .parse(pastQuotationBound)
      .left
      .toOption
      .map(failure => failure.message)
      .getOrElse(fail("expected a failure"))
    pastMessage should not be s"Unable to parse amount, invalid format: $pastQuotationBound"
    pastMessage should not include "\n"
    pastMessage should include("\\n")

    // whatever a sender chooses, the message a rejection carries is capped: a megabyte of text
    // is named in a few hundred characters, so the cost of a rejection is bounded by this type
    // and not by the length of its input (CWE-400/CWE-770)
    val megabyte: String = "H" * (1024 * 1024)
    val cappedMessage: String = CurrencyAmount
      .parse(megabyte)
      .left
      .toOption
      .map(failure => failure.message)
      .getOrElse(fail("expected a failure"))
    cappedMessage.length should be < 1000
    cappedMessage.length shouldBe
      "Unable to parse amount, invalid format: ".length + MaxRenderedMessagePart

    // text holding a line break is named as it stands but rendered on one line, so a
    // line-oriented consumer cannot be made to record a line the library did not report. The
    // break is placed to reach each wording in turn: at the separator position, then inside the
    // amount part. Both texts are within the quotation bound, so the raw break survives into
    // `message` and is escaped only by the renderer - which is the division of labour the
    // bounding does not change
    CurrencyAmount.parse("AUD\n1.5") should haveFailureMessageMatching(
      Regex.quote("Unable to parse amount, invalid format: AUD\n1.5"))
    CurrencyAmount.parse("AUD 1.5\nINJECTED") should haveFailureMessageMatching(
      Regex.quote("Unable to parse amount: AUD 1.5\nINJECTED"))
    val injected: FailureOr[CurrencyAmount] = CurrencyAmount.parse("AUD 1.5\nINJECTED")
    Show[Failure].show(injected.left.toOption.getOrElse(fail("expected a failure"))) shouldBe
      "PARSING: Unable to parse amount: AUD 1.5\\nINJECTED"
  }

  /**
   * Asserts the ceiling on the amount part from both sides of it.
   *
   * The ceiling is a deliberate narrowing rather than a bug fix: it was calibrated on the longest
   * exact decimal spelling of a double, at 767 significant digits, and is now the 256 characters
   * [[Money]] and [[BigMoney]] read the same text form within, so one bound holds across the four
   * types of this package that read a number out of text. What it refuses is text whose extra
   * digits cannot change the value it names, and it reports through the wording a text that names
   * no amount has always reported rather than through a wording of its own.
   */
  test("the amount part is bounded, and reports through the wording of an unreadable amount") {
    // a numeral of exactly the ceiling is read exactly as it always was, zeroes and all
    val atCeiling: String = "0." + ("0" * (MaxAmountTextLength - 2))
    atCeiling.length shouldBe MaxAmountTextLength
    unwrap(CurrencyAmount.parse(s"AUD $atCeiling")).amount shouldBe 0d

    // a significant numeral at the ceiling reads as the value it names, so the bound is a bound
    // on the text and not on the precision
    val significantAtCeiling: String = "1." + ("0" * (MaxAmountTextLength - 3)) + "5"
    significantAtCeiling.length shouldBe MaxAmountTextLength
    unwrap(CurrencyAmount.parse(s"AUD $significantAtCeiling")).amount shouldBe 1d

    // one character more names no amount, through that same wording and with no numeric reading.
    // The whole text is one character past the quotation bound, so it is quoted through the
    // renderer - which returns text of this length holding nothing escapable exactly as it
    // stands, so what a caller reads here is still the text they wrote
    val pastCeiling: String = "0." + ("0" * (MaxAmountTextLength - 1))
    pastCeiling.length shouldBe MaxAmountTextLength + 1
    val refused: FailureOr[CurrencyAmount] = CurrencyAmount.parse(s"AUD $pastCeiling")
    refused should beFailureWith(FailureReason.PARSING)
    refused.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount: AUD $pastCeiling")

    // the bound is on the amount part alone: the currency code and the separator ahead of it are
    // not counted, so a text of the ceiling plus four characters is the longest one accepted
    unwrap(CurrencyAmount.parse(s"AUD $atCeiling")).currency shouldBe AUD
  }

  //-------------------------------------------------------------------------
  test("test_plus_CurrencyAmount") {
    val ccyAmount: CurrencyAmount = amountOf(CCY1, AMT2)
    CCY_AMOUNT.plus(ccyAmount) should haveValue(amountOf(CCY1, AMT1 + AMT2))
  }

  test("test_plus_CurrencyAmount_null") {
    assertDoesNotCompile("""CCY_AMOUNT.plus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""CCY_AMOUNT.plus()""")
    assertCompiles("""CCY_AMOUNT.plus(CCY_AMOUNT)""")
    assertCompiles("""CCY_AMOUNT.plus(AMT2)""")
  }

  test("test_plus_CurrencyAmount_wrongCurrency") {
    val outcome: FailureOr[CurrencyAmount] = CCY_AMOUNT.plus(amountOf(CCY2, AMT2))
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should haveFailureMessageMatching(
      Regex.quote("Unable to add amounts in different currencies"))

    // the currency check comes first, so a mismatch is reported even for two operands whose sum
    // the arithmetic could not have produced at all
    amountOf(CCY1, Double.PositiveInfinity).plus(amountOf(CCY2, Double.NegativeInfinity)) should
      beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the total addition of a plain value, including the operands that reach an infinity.
   *
   * The value is taken to be in the currency of the amount, so no currency can disagree and the
   * addition is total: a finite sum that overflows, and an operand that is already infinite, both
   * answer with an amount rather than a failure, because an infinity is a value this type admits.
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

  test("test_minus_CurrencyAmount_null") {
    assertDoesNotCompile("""CCY_AMOUNT.minus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""CCY_AMOUNT.minus()""")
    assertCompiles("""CCY_AMOUNT.minus(CCY_AMOUNT)""")
    assertCompiles("""CCY_AMOUNT.minus(AMT2)""")
  }

  test("test_minus_CurrencyAmount_wrongCurrency") {
    val outcome: FailureOr[CurrencyAmount] = CCY_AMOUNT.minus(amountOf(CCY2, AMT2))
    outcome should beFailureWith(FailureReason.INVALID)
    // the wording differs from the addition's by one word, and both are pinned: an
    // implementation reporting one of them for both operations would be indistinguishable
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
   * The multiplication is the amount times the value, in that order, so a product that would
   * round differently under the other order is pinned here. As with the addition, the product of
   * two finite operands may be infinite, and the operation stays total there.
   */
  test("test_multipliedBy") {
    CCY_AMOUNT.multipliedBy(3.5d) shouldBe amountOf(CCY1, AMT1 * 3.5d)
    amountOf(CCY1, Double.MaxValue).multipliedBy(2d) shouldBe
      amountOf(CCY1, Double.PositiveInfinity)
    amountOf(CCY1, -Double.MaxValue).multipliedBy(2d) shouldBe
      amountOf(CCY1, Double.NegativeInfinity)
  }

  test("test_mapAmount") {
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
   * Asserts the two conversions to the exact-decimal types.
   *
   * The two differ in what they round to: the Australian dollar quotes two digits, so
   * `AUD 100.125` becomes `AUD 100.13` as money and stays `AUD 100.125` at scale twelve. An
   * infinite amount, which this type admits and no decimal holds, is reported by both.
   */
  test("test_toMoney_toBigMoney") {
    CCY_AMOUNT.toMoney shouldBe Money.of(CCY1, AMT1)
    CCY_AMOUNT.toBigMoney shouldBe BigMoney.of(CCY1, AMT1)

    // money rounds to the minor units the currency quotes, while the wider type keeps the digit
    // that rounding would have dropped
    amountOf(CCY1, 100.125d).toMoney.map(money => money.amount.toString) shouldBe Right("100.13")
    amountOf(CCY1, 100.125d).toBigMoney.map(money => money.amount.toString) shouldBe Right("100.125")

    // an amount already within the currency's minor units survives both round trips unchanged,
    // which an amount with a finer digit would not
    CCY_AMOUNT.toMoney.map(money => money.toCurrencyAmount) shouldBe Right(CCY_AMOUNT)
    CCY_AMOUNT.toBigMoney.map(money => money.toCurrencyAmount) shouldBe Right(CCY_AMOUNT)

    amountOf(CCY1, Double.PositiveInfinity).toMoney should beFailureWith(FailureReason.INVALID)
    amountOf(CCY1, Double.PositiveInfinity).toBigMoney should beFailureWith(FailureReason.INVALID)
    amountOf(CCY1, Double.NegativeInfinity).toMoney should beFailureWith(FailureReason.INVALID)
    amountOf(CCY1, Double.NegativeInfinity).toBigMoney should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts conversion at a rate the caller supplies, including the rate that must be one.
   *
   * A rate other than one for a conversion into the currency the amount already has is refused,
   * which is what stops a caller scaling an amount by passing a rate for a conversion that does
   * not happen.
   */
  test("test_convertedTo_explicitRate") {
    CCY_AMOUNT.convertedTo(CCY2, 2.5d) should haveValue(amountOf(CCY2, AMT1 * 2.5d))

    // a conversion into the currency the amount already has needs no arithmetic, so a rate of
    // one short-circuits to this very instance, which is what the reference comparison states
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
   * A counting provider is what makes "no rate was consulted" observable: an implementation that
   * asked for a rate and then discarded it would satisfy an assertion about the converted value
   * alone. `convertedTo` is this type's [[FxConvertible]] implementation, so it is also exercised
   * once through a reference of that trait.
   */
  test("test_convertedTo_rateProvider") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    CCY_AMOUNT.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    CCY_AMOUNT.convertedTo(CCY1, provider) should haveValue(CCY_AMOUNT)

    // the counter records every rate the provider is asked for; the incremented value is bound
    // to a wildcard because only the totals asserted below are of interest
    val consulted: AtomicInteger = new AtomicInteger(0)
    val counting: FxRateProvider = FxRateProvider.fromFunction { (_, _) =>
      val _ = consulted.incrementAndGet()
      Right(2.5d)
    }

    CCY_AMOUNT.convertedTo(CCY1, counting) should haveValue(CCY_AMOUNT)
    consulted.get shouldBe 0
    CCY_AMOUNT.convertedTo(CCY2, counting) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    consulted.get shouldBe 1

    val convertible: FxConvertible[CurrencyAmount] = CCY_AMOUNT
    convertible.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    convertible.convertedTo(CCY1, provider) should haveValue(CCY_AMOUNT)

    // a provider that supplies no rate for any pair, the identity pair included, still answers
    // the same-currency conversion, because it is never asked; the cross-currency conversion is
    // that provider's own failure
    val empty: FxRateProvider = FxRateProvider.noConversion()
    CCY_AMOUNT.convertedTo(CCY1, empty) should haveValue(CCY_AMOUNT)
    CCY_AMOUNT.convertedTo(CCY2, empty) should beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the equality and hashing matrix, through the methods and through the `Hash` instance.
   *
   * Both are stated because a set, a map key or a codec property consults the instance while
   * `==` calls the method, and the two are required to agree.
   *
   * Equality compares the amounts with `java.lang.Double.compare`, which distinguishes `-0.0`
   * from `+0.0` and treats a value that is not a number as equal to itself; construction rules
   * both out, which is why the two zeros below are one value and why every pair of zero amounts
   * is equal.
   */
  test("test_equals_hashCode") {
    val other: CurrencyAmount = amountOf(CCY1, AMT1)

    // the matrix is stated through `equals` itself, the reflexive case included, because that is
    // the method the instance asserted further down has to agree with
    CCY_AMOUNT.equals(CCY_AMOUNT) shouldBe true
    CCY_AMOUNT.equals(other) shouldBe true
    other.equals(CCY_AMOUNT) shouldBe true
    CCY_AMOUNT.hashCode shouldBe other.hashCode

    CCY_AMOUNT shouldBe amountOf(CCY1, AMT1)
    CCY_AMOUNT.hashCode shouldBe amountOf(CCY1, AMT1).hashCode

    CCY_AMOUNT.equals(amountOf(CCY2, AMT1)) shouldBe false
    CCY_AMOUNT.equals(amountOf(CCY1, AMT2)) shouldBe false

    // the same matrix through the instance
    Hash[CurrencyAmount].eqv(CCY_AMOUNT, other) shouldBe true
    Hash[CurrencyAmount].hash(CCY_AMOUNT) shouldBe Hash[CurrencyAmount].hash(other)
    Hash[CurrencyAmount].eqv(CCY_AMOUNT, amountOf(CCY2, AMT1)) shouldBe false
    Hash[CurrencyAmount].eqv(CCY_AMOUNT, amountOf(CCY1, AMT2)) shouldBe false

    val plusInfinity: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    Hash[CurrencyAmount].eqv(plusInfinity, amountOf(CCY1, Double.PositiveInfinity)) shouldBe true
    Hash[CurrencyAmount].eqv(plusInfinity, amountOf(CCY1, Double.NegativeInfinity)) shouldBe false
    Hash[CurrencyAmount].hash(plusInfinity) shouldBe
      Hash[CurrencyAmount].hash(amountOf(CCY1, Double.PositiveInfinity))

    // the two zeros are one value, because construction normalises the sign away
    amountOf(CCY1, -0d) shouldBe amountOf(CCY1, 0d)
    Hash[CurrencyAmount].eqv(amountOf(CCY1, -0d), amountOf(CCY1, 0d)) shouldBe true
    Hash[CurrencyAmount].hash(amountOf(CCY1, -0d)) shouldBe
      Hash[CurrencyAmount].hash(amountOf(CCY1, 0d))
  }

  test("test_equals_bad") {
    CCY_AMOUNT.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(CCY_AMOUNT) shouldBe false
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the comparison [[CurrencyAmount.order]] performs, not merely that it is lawful.
   *
   * Lawfulness alone does not pin this order: an ordering that read the amount before the
   * currency, or that reversed either comparison, would be just as lawful while sorting a report
   * of amounts differently. What is asserted is therefore the comparison itself - the currency
   * first by code, the amount second by `java.lang.Double.compare` - with every pair stated in
   * both directions, because a sign is only pinned by its opposite.
   */
  test("the ordering compares the currency before the amount and agrees with equality") {
    val order: Order[CurrencyAmount] = implicitly[Order[CurrencyAmount]]

    // the currency decides whatever the amounts are: a large amount of the earlier code is less
    // than a small amount of the later one
    val audLarge: CurrencyAmount = amountOf(CCY1, AMT1)
    val cadSmall: CurrencyAmount = amountOf(CCY2, 1d)
    (order.compare(audLarge, cadSmall) < 0) shouldBe true
    (order.compare(cadSmall, audLarge) > 0) shouldBe true

    // the same comparison with the amounts equal, so the sign above is the currency's and not an
    // accident of the amounts chosen for it
    (order.compare(amountOf(CCY1, AMT1), amountOf(CCY2, AMT1)) < 0) shouldBe true
    (order.compare(amountOf(CCY2, AMT1), amountOf(CCY1, AMT1)) > 0) shouldBe true

    // within one currency the amount decides, ascending
    (order.compare(CCY_AMOUNT_NEGATIVE, CCY_AMOUNT) < 0) shouldBe true
    (order.compare(CCY_AMOUNT, CCY_AMOUNT_NEGATIVE) > 0) shouldBe true
    (order.compare(CCY_AMOUNT, amountOf(CCY1, AMT2)) < 0) shouldBe true
    (order.compare(amountOf(CCY1, AMT2), CCY_AMOUNT) > 0) shouldBe true
    order.compare(CCY_AMOUNT, amountOf(CCY1, AMT1)) shouldBe 0

    val mixed: List[CurrencyAmount] =
      List(cadSmall, amountOf(CCY1, AMT2), CCY_AMOUNT_NEGATIVE, audLarge)
    mixed.sorted(order.toOrdering) shouldBe
      List(CCY_AMOUNT_NEGATIVE, audLarge, amountOf(CCY1, AMT2), cadSmall)

    // `compare` returns zero exactly where equality holds, over every ordered pair of those four
    // values: the ordering and the equality are required to agree
    mixed.foreach { left =>
      mixed.foreach { right =>
        withClue(s"$left vs $right: ") {
          (order.compare(left, right) == 0) shouldBe (left == right)
        }
      }
    }

    // `java.lang.Double.compare(-0.0, 0.0)` is negative, so an ordering over unnormalised
    // amounts would sort a negative zero below a positive one. Construction normalises the sign
    // away, so the two are one value and the comparison is zero, as agreement with equality
    // requires
    order.compare(amountOf(CCY1, -0d), amountOf(CCY1, 0d)) shouldBe 0

    // each infinity sorts outside every finite amount of the same currency, which
    // `java.lang.Double.compare` gives and a subtraction of the two amounts would not
    val negativeInfinity: CurrencyAmount = amountOf(CCY1, Double.NegativeInfinity)
    val positiveInfinity: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    (order.compare(negativeInfinity, CCY_AMOUNT_NEGATIVE) < 0) shouldBe true
    (order.compare(CCY_AMOUNT_NEGATIVE, negativeInfinity) > 0) shouldBe true
    (order.compare(positiveInfinity, amountOf(CCY1, AMT2)) > 0) shouldBe true
    (order.compare(amountOf(CCY1, AMT2), positiveInfinity) < 0) shouldBe true

    (order.compare(positiveInfinity, amountOf(CCY2, Double.NegativeInfinity)) < 0) shouldBe true
    (order.compare(amountOf(CCY2, Double.NegativeInfinity), positiveInfinity) > 0) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    // an integral amount is written without a decimal point, which documents, log lines and
    // expectations carry
    amountOf(AUD, 100d).toString shouldBe "AUD 100"
    amountOf(AUD, 100.123d).toString shouldBe "AUD 100.123"
  }

  //-----------------------------------------------------------------------
  /**
   * Asserts the JSON document shape by example, and that decoding enforces the type's invariant.
   *
   * The decoder routes its fields through the same factory a caller's arguments go through, so a
   * document naming an amount this type would not build is a decoding failure rather than a value
   * that bypassed the check.
   */
  test("test_serialization") {
    val encoded: Json = CCY_AMOUNT.asJson
    encoded.noSpaces shouldBe """{"currency":"AUD","amount":100.0}"""
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("currency", "amount"))
    encoded.as[CurrencyAmount] shouldBe Right(CCY_AMOUNT)

    // the amount goes through the one double policy of this library, which writes the values JSON
    // cannot express as tagged strings, so every value an amount may hold survives the round trip
    val infinite: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    infinite.asJson.noSpaces shouldBe """{"currency":"AUD","amount":"Infinity"}"""
    infinite.asJson.as[CurrencyAmount] shouldBe Right(infinite)

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
   * Asserts the text contract of the type: one form, produced and read back by every route.
   *
   * `toString`, the `Show` instance and `parse` are required to agree, because a document, a log
   * line or a stored expectation is written in that one form and read back from it.
   */
  test("test_jodaConvert") {
    val rendered: String = CCY_AMOUNT.toString
    rendered shouldBe "AUD 100"
    Show[CurrencyAmount].show(CCY_AMOUNT) shouldBe rendered
    CurrencyAmount.parse(rendered) should haveValue(CCY_AMOUNT)

    // the same agreement over values that do not render as a plain integral amount, the
    // infinities included: those render as `Infinity` and `-Infinity`, and `parse` reads both
    // back
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
   * The outcome is folded rather than read through a partial accessor, so a fixture that fails to
   * build is reported as a test failure naming the reason.
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
   * The text factory can also fail on a code the closed currency family does not hold, which is
   * why it has its own helper.
   *
   * @param currencyCode  the three letter ISO-4217 currency code, upper case
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the amount
   */
  private def amountOf(currencyCode: String, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currencyCode, amount))
}
