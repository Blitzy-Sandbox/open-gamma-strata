/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.util.matching.Regex

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4

import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FxRate]], ported from the Java `FxRateTest`.
 *
 * The original holds twenty-four test methods - twenty-two plain and two driven by a data
 * provider - and so does this suite, under the names it gave them, so that a Java test method
 * and a test of this suite stay in one-to-one correspondence in the migration manifest. The two
 * data-driven methods stay '''one''' test each, with the rows of the original provider held in a
 * table inside them, so that each contributes a single test case rather than one per row.
 *
 * ===Three failure modes, kept apart===
 *
 * This is the densest failure surface of the package: the original asserted
 * `IllegalArgumentException` twenty-three times, and those twenty-three sites are not one
 * behaviour but three, which this suite is careful never to conflate.
 *
 *   - '''a rate that is not greater than zero''' and '''two identical currencies with a rate other
 *     than one''' are the two constraints of the type. Both are checked by `FxRate.of`, both are
 *     reported as [[com.opengamma.strata.collect.result.FailureReason.INVALID]], and they are
 *     told apart here by their message - the wording of the second is the wording the original
 *     validator threw with, and it is pinned exactly. `of` accumulates, so an input that breaks
 *     both is asserted to report both, in order, which is something the original could not do.
 *   - '''text that names no rate''' is [[com.opengamma.strata.collect.result.FailureReason.PARSING]],
 *     and within it there are two wordings: text the expression of [[FxRate.parse]] does not
 *     match at all is `Invalid rate: …`, while text that matches but names a rate the type
 *     rejects is `Unable to parse rate: …`. The provider of the original mixes both in one table -
 *     its last three rows are constraint failures reached through parsing, not format failures -
 *     so every row here carries its own expected wording. Asserting the table with one blanket
 *     reason would let a port that had collapsed the two wordings, or that had stopped checking
 *     the constraints on the parse path, pass unnoticed.
 *   - '''a pair this rate cannot convert''' is
 *     [[com.opengamma.strata.collect.result.FailureReason.CURRENCY_CONVERSION]], for the lookup,
 *     the two conversions and the cross-rate derivation alike, each with the message the original
 *     threw with.
 *
 * Every one of those is asserted as a `Left` value through the matchers of
 * [[com.opengamma.strata.collect.testkit.ResultMatchers]], with the reason compared as a member
 * of the closed family of reasons rather than as text, and the message pinned through
 * `Regex.quote` so that the comparison is the literal message and not a pattern that happens to
 * match it. Nothing here catches an exception, because nothing in the ported type throws one.
 *
 * ===Where the ported API differs from the original===
 *
 * Four differences shape the assertions below, and each is asserted in the form the ported type
 * actually has rather than in the form the original had:
 *
 *   - `getPair()` is the accessor `pair`, and the rate, which the original kept private, is the
 *     public accessor `rate`.
 *   - `FxRate.of` returns `ResultNec[FxRate]`, an accumulating outcome, so the fixtures of this
 *     suite are built through it and unwrapped once by [[unwrap]].
 *   - [[FxRate.parse]] returns `FailureOr[FxRate]`, a '''single''' failure rather than a chain:
 *     the two wordings of the parse path are alternatives, so there is never more than one of
 *     them to report. That is the production signature and it is what is asserted here.
 *   - the type is not comparable and its companion publishes `Hash` and `Show` and no `Order`,
 *     so `coverage` asserts those two instances and does not look for a third.
 *
 * One test is routed differently from the body of the original for the same reason. The original
 * named a test `test_fxRate_forPair` and then called the two-currency lookup in it, exactly as
 * `test_fxRate_forBase` did; here the name is kept and the test is routed through the
 * `fxRate(CurrencyPair)` overload the name describes, over the rows the original used, so the two
 * tests cover the two overloads instead of covering the same one twice.
 *
 * ===Equality is bit for bit===
 *
 * [[FxRate.equals]] compares the rate by its bit pattern, which is what the generated bean it
 * replaces did. None of the rows of the original reaches a rate outside the real numbers or a
 * negative zero - `FxRate.of` rejects the zeros and every row here is finite - so the assertions
 * of `test_equals_hashCode` compare values directly, and the case that pins the rate as part of
 * the identity of a value is the pair of rates that differ in nothing else, which the original
 * also had.
 *
 * ===What is asserted elsewhere===
 *
 * The typeclass law suites, the compile-time sweep over the construction surface of every
 * validated type, the numeric-edge preconditions, the sweep that exercises every failable method
 * of the module with a failing input, and the property-based round trip of every codec each live
 * in their own spec at the root of the test tree; the fixture-driven numerical parity of FX
 * conversion and cross rates belongs to `parity.FxParitySpec`. This suite asserts the cases of
 * the Java test it is ported from, which overlap those sweeps by design.
 *
 * @see [[FxRate]] for the type under test
 * @see [[CurrencyPair]] for the pair, whose `cross` decides which cross rates exist
 */
final class FxRateSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * A value of a type unrelated to a rate, for the equality assertion that needs one.
   *
   * Held at the type `Any` and named as the original named it, so that the assertion reads as a
   * comparison against a foreign value rather than as a comparison the compiler could reject.
   */
  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  /**
   * The rates the cross-rate test crosses, named and valued as the original named and valued
   * them.
   *
   * The rates are written as quotients of small integers - `5d / 4d` rather than `1.25d` - because
   * the expected cross rate is the product of two of them and has to be the same `Double` bit for
   * bit, whichever way round the two inputs are written. A decimal literal for the product would
   * be an assertion about rounding rather than about the cross-rate orientation this test exists
   * to pin.
   */
  private val gbpUsd: FxRate = rateOf(GBP, USD, 5d / 4d)

  /** The `USD/GBP` rate, the inverse direction of [[gbpUsd]]. */
  private val usdGbp: FxRate = rateOf(USD, GBP, 4d / 5d)

  /** The `EUR/USD` rate crossed through `USD` below. */
  private val eurUsd: FxRate = rateOf(EUR, USD, 8d / 7d)

  /** The `USD/EUR` rate, the inverse direction of [[eurUsd]]. */
  private val usdEur: FxRate = rateOf(USD, EUR, 7d / 8d)

  /**
   * The cross rate every successful orientation has to produce.
   *
   * Its pair is the market convention pair of `EUR` and `GBP` - which is `EUR/GBP` - because
   * [[CurrencyPair.cross]] answers in convention order however its two inputs are written, and
   * its rate is the product in the order [[FxRate.crossRate]] forms it.
   */
  private val eurGbp: FxRate = rateOf(EUR, GBP, (8d / 7d) * (4d / 5d))

  /** The identity rate for `GBP`, which no cross can go through. */
  private val gbpGbp: FxRate = rateOf(GBP, GBP, 1d)

  /** The identity rate for `USD`, which no cross can go through. */
  private val usdUsd: FxRate = rateOf(USD, USD, 1d)

  /** A rate sharing no currency with [[gbpUsd]], for the row that has no common currency. */
  private val eurCad: FxRate = rateOf(EUR, CAD, 12d / 5d)

  //-------------------------------------------------------------------------
  /**
   * Arguments of the two-currency factory that describe no rate, with the message each reports.
   *
   * The first and the third row are the two rows of the original: a negative rate, and two
   * identical currencies with a rate other than one. The second row is the boundary of the first
   * constraint, zero, which the original asserted for the parse path but not for this factory and
   * which is the value a careless port of `notNegative` would let through.
   *
   * The message is part of every row because the two constraints share a reason. Without it a
   * port that reported the positivity failure for an identity pair - or the other way round -
   * would satisfy these rows, and the wording of the second is the wording of the validator being
   * ported, which reaches logs and documents.
   */
  private val data_ofInvalidCurrencies: TableFor4[Currency, Currency, Double, String] = Table(
    ("base", "counter", "rate", "message"),
    (GBP, USD, -1.5d, "Argument 'rate' must not be negative or zero but has value -1.5"),
    (GBP, USD, 0d, "Argument 'rate' must not be negative or zero but has value 0.0"),
    (GBP, GBP, 2d, "Conversion rate between identical currencies must be one"))

  /**
   * Arguments of the currency-pair factory that describe no rate, with the message each reports.
   *
   * The same three cases as above, reached through the other factory, which is defined as this
   * one applied to the pair of its two currencies and therefore has to reject exactly the same
   * rates with exactly the same wording.
   */
  private val data_ofInvalidPair: TableFor3[CurrencyPair, Double, String] = Table(
    ("pair", "rate", "message"),
    (
      CurrencyPair.of(GBP, USD),
      -1.5d,
      "Argument 'rate' must not be negative or zero but has value -1.5"),
    (
      CurrencyPair.of(GBP, USD),
      0d,
      "Argument 'rate' must not be negative or zero but has value 0.0"),
    (CurrencyPair.of(USD, USD), 2d, "Conversion rate between identical currencies must be one"))

  //-------------------------------------------------------------------------
  /**
   * Text that names a rate, with the rate it names, transcribed from the Java provider.
   *
   * All seven rows of the original are here, and three of them are doing work that is easy to
   * mistake for repetition. `USD/EUR 3.00000000` pins that a fraction of zeros names the whole
   * number `3` and not something that merely prints like it; `USD/EUR 2` pins that a rate needs
   * no fractional part at all; and `cAd/GbP 1.25` pins that the text is folded before it is
   * matched, so parsing is insensitive to the case of what a caller wrote.
   */
  private val data_parseGood: TableFor4[String, Currency, Currency, Double] = Table(
    ("input", "base", "counter", "rate"),
    ("USD/EUR 205.123", USD, EUR, 205.123d),
    ("USD/EUR 3.00000000", USD, EUR, 3d),
    ("USD/EUR 2", USD, EUR, 2d),
    ("USD/EUR 0.1", USD, EUR, 0.1d),
    ("EUR/USD 0.001", EUR, USD, 0.001d),
    ("EUR/EUR 1", EUR, EUR, 1d),
    ("cAd/GbP 1.25", CAD, GBP, 1.25d))

  /**
   * Text that names no rate, with the message each reports, transcribed from the Java provider.
   *
   * Nine of the ten rows of the original are here unchanged, and the table is deliberately not
   * uniform: the first five and the ninth are text the expression of [[FxRate.parse]] does not
   * match, reported as `Invalid rate: …`, while the sixth, seventh and eighth '''do''' match and
   * are rejected afterwards by the two constraints of the type, reported as
   * `Unable to parse rate: …`. Those three are a negative rate, a zero rate and two identical
   * currencies with a rate other than one - the whole constraint surface of the type, reached
   * through parsing - which is why each row carries its own wording rather than the table being
   * asserted with one.
   *
   * The tenth row of the original was Java's absent-reference literal, which cannot be written
   * against this API - the parameter is a `String` a caller supplies, and this port has no such
   * literal to supply - so it is replaced by a further input that is rejected for the same reason
   * the first five are: a rate written in exponent notation, which the rate group of the
   * expression admits no letter into, so the whole text fails to match. The message quotes the
   * text as it was supplied rather than as it was folded to upper case, which that row also pins.
   */
  private val data_parseBad: TableFor2[String, String] = Table(
    ("input", "message"),
    ("AUD 1.25", "Invalid rate: AUD 1.25"),
    ("AUD/GB 1.25", "Invalid rate: AUD/GB 1.25"),
    ("AUD GBP 1.25", "Invalid rate: AUD GBP 1.25"),
    ("AUD:GBP 1.25", "Invalid rate: AUD:GBP 1.25"),
    ("123/456", "Invalid rate: 123/456"),
    ("EUR/GBP -1.25", "Unable to parse rate: EUR/GBP -1.25"),
    ("EUR/GBP 0", "Unable to parse rate: EUR/GBP 0"),
    ("EUR/EUR 1.25", "Unable to parse rate: EUR/EUR 1.25"),
    ("", "Invalid rate: "),
    ("EUR/USD 1e3", "Invalid rate: EUR/USD 1e3"))

  //-------------------------------------------------------------------------
  /**
   * Currency pairs `GBP/USD 1.25` answers for, with the rate it answers.
   *
   * The second row is the one that matters: the inverted pair is answered with the reciprocal
   * rather than refused, and the expected value is written as the division the implementation
   * performs so that the row asserts the reciprocal itself and not a decimal that rounds to it.
   */
  private val data_fxRateFound: TableFor3[Currency, Currency, Double] = Table(
    ("base", "counter", "rate"),
    (GBP, USD, 1.25d),
    (USD, GBP, 1d / 1.25d))

  /**
   * Currency pairs `GBP/USD 1.25` cannot answer for, with the message each reports.
   *
   * A rate holds one pair, so a question about a pair it does not hold has no answer. The row of
   * the original is kept and the message names the pair that was asked about, which is what makes
   * the failure useful to a caller holding several rates.
   */
  private val data_fxRateMissing: TableFor3[Currency, Currency, String] = Table(
    ("base", "counter", "message"),
    (GBP, AUD, "No FX rate found for GBP/AUD"))

  /**
   * Pairs the pair-form lookup answers for, with the rate it answers, from the Java rows.
   *
   * Five rows, of which three are the identity case: a currency and itself converts at one
   * whether or not this rate mentions that currency at all, which is why `AUD/AUD` is answered by
   * a `GBP/USD` rate. That is the first case the implementation tests and the easiest to lose.
   */
  private val data_fxRatePairFound: TableFor2[CurrencyPair, Double] = Table(
    ("pair", "rate"),
    (CurrencyPair.of(GBP, USD), 1.25d),
    (CurrencyPair.of(USD, GBP), 1d / 1.25d),
    (CurrencyPair.of(GBP, GBP), 1d),
    (CurrencyPair.of(USD, USD), 1d),
    (CurrencyPair.of(AUD, AUD), 1d))

  /**
   * Pairs the pair-form lookup cannot answer for, with the message each reports, from the Java
   * rows.
   *
   * All five rows of the original are kept: a pair holding one of the two currencies of the rate
   * either way round, and a pair holding neither. Sharing a currency with the rate is not enough,
   * which is the point of the four rows that do.
   */
  private val data_fxRatePairMissing: TableFor2[CurrencyPair, String] = Table(
    ("pair", "message"),
    (CurrencyPair.of(AUD, GBP), "No FX rate found for AUD/GBP"),
    (CurrencyPair.of(GBP, AUD), "No FX rate found for GBP/AUD"),
    (CurrencyPair.of(AUD, USD), "No FX rate found for AUD/USD"),
    (CurrencyPair.of(USD, AUD), "No FX rate found for USD/AUD"),
    (CurrencyPair.of(EUR, AUD), "No FX rate found for EUR/AUD"))

  //-------------------------------------------------------------------------
  /**
   * Pairs of rates that cross, with the rate the cross has to produce, from the Java rows.
   *
   * All eight rows of the original are here and they are the whole point of the test: the same
   * `EUR/GBP` rate has to come out of `EUR/USD` crossed with `USD/GBP`, of either of those two
   * written the other way round, and of the two supplied in the other order - eight combinations
   * of two orientations each, plus the order of the two arguments. The expected value is
   * [[eurGbp]], whose rate is the product of the two input rates in the order the implementation
   * forms it, so a port that had reassociated the multiplication or inverted the wrong operand
   * would fail here rather than agree to within rounding.
   */
  private val data_crossRate: TableFor3[FxRate, FxRate, FxRate] = Table(
    ("first", "second", "expected"),
    (eurUsd, usdGbp, eurGbp),
    (eurUsd, gbpUsd, eurGbp),
    (usdEur, usdGbp, eurGbp),
    (usdEur, gbpUsd, eurGbp),
    (gbpUsd, usdEur, eurGbp),
    (gbpUsd, eurUsd, eurGbp),
    (usdGbp, usdEur, eurGbp),
    (usdGbp, eurUsd, eurGbp))

  /**
   * Pairs of rates that do not cross, with the message each reports, from the Java rows.
   *
   * All five rows of the original are kept, and they are three distinct reasons no cross exists:
   * one of the rates is an identity, so its two currencies are one and there is no third; the two
   * rates name the same two currencies, either in the same order or inverted, so again there is
   * no third; and the two rates share no currency at all. The message names both pairs, which is
   * the wording the original threw with.
   */
  private val data_crossRateInvalid: TableFor3[FxRate, FxRate, String] = Table(
    ("first", "second", "message"),
    (gbpGbp, gbpUsd, "Unable to cross when no unique common currency: GBP/GBP and GBP/USD"),
    (usdUsd, gbpUsd, "Unable to cross when no unique common currency: USD/USD and GBP/USD"),
    (gbpUsd, gbpUsd, "Unable to cross when no unique common currency: GBP/USD and GBP/USD"),
    (gbpUsd, usdGbp, "Unable to cross when no unique common currency: GBP/USD and USD/GBP"),
    (gbpUsd, eurCad, "Unable to cross when no unique common currency: GBP/USD and EUR/CAD"))

  //-------------------------------------------------------------------------
  test("test_of_CurrencyCurrencyDouble") {
    val test = rateOf(GBP, USD, 1.5d)
    test.pair shouldBe CurrencyPair.of(GBP, USD)
    // the rate is a public accessor of this port, where the bean being ported kept it private
    // and exposed the value only through the lookup and the text form; both of those are
    // asserted here as well, so the three views of the value are pinned together
    test.rate shouldBe 1.5d
    test.fxRate(GBP, USD) should haveValue(1.5d)
    test.toString shouldBe "GBP/USD 1.5"
  }

  test("test_of_CurrencyCurrencyDouble_reverseStandardOrder") {
    // a pair written the other way round from the market convention is kept exactly as it was
    // supplied - `of` does not reorient its arguments, and `toConventional` is the member that
    // does, asserted separately below
    val test = rateOf(USD, GBP, 0.8d)
    test.pair shouldBe CurrencyPair.of(USD, GBP)
    test.rate shouldBe 0.8d
    test.fxRate(USD, GBP) should haveValue(0.8d)
    test.toString shouldBe "USD/GBP 0.8"
  }

  test("test_of_CurrencyCurrencyDouble_same") {
    // two identical currencies are legal at a rate of exactly one, which is the identity rate;
    // the text form writes a whole-number rate without a fractional part, so this is "USD/USD 1"
    val test = rateOf(USD, USD, 1d)
    test.pair shouldBe CurrencyPair.of(USD, USD)
    test.pair.isIdentity shouldBe true
    test.rate shouldBe 1d
    test.fxRate(USD, USD) should haveValue(1d)
    test.toString shouldBe "USD/USD 1"
  }

  test("test_of_CurrencyCurrencyDouble_invalid") {
    forAll(data_ofInvalidCurrencies) {
      (base: Currency, counter: Currency, rate: Double, message: String) =>
        val outcome: ResultNec[FxRate] = FxRate.of(base, counter, rate)
        // the two constraints share a reason, so the reason alone would not tell them apart;
        // the message is what distinguishes a rate that is not positive from an identity pair
        // carrying a rate other than one
        outcome should beFailureWith(FailureReason.INVALID)
        outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    // the two constraints are checked in one expression and accumulate, so arguments that break
    // both are told about both, in the order the checks are written. The original checked them
    // in two places and reported whichever it reached first, so this is behaviour the port adds
    // and the reason `of` returns a chain rather than a single failure.
    val both: List[Failure] = failuresOf(FxRate.of(GBP, GBP, -1d))
    both.map(failure => failure.reason) shouldBe
      List(FailureReason.INVALID, FailureReason.INVALID)
    both.map(failure => failure.message) shouldBe List(
      "Argument 'rate' must not be negative or zero but has value -1.0",
      "Conversion rate between identical currencies must be one")
  }

  test("test_of_CurrencyCurrencyDouble_null") {
    // The Java test passed the absent-reference literal for each currency in turn and asserted
    // an IllegalArgumentException. That case cannot be written against this API and needs no
    // run-time guard: both currencies are required `Currency` values, and the `notNull` family
    // of checks was dropped in the port because an absent value is modelled by `Option` rather
    // than by an absent reference. What replaced the run-time check is therefore asserted at
    // compile time - a currency supplied as the text of its code, and an argument omitted
    // altogether, are both rejected before the program runs - and the valid call is asserted to
    // compile so that the proofs cannot be passing for some unrelated reason.
    assertDoesNotCompile("""FxRate.of("GBP", USD, 1.5d)""")
    assertDoesNotCompile("""FxRate.of(GBP, "USD", 1.5d)""")
    assertDoesNotCompile("""FxRate.of(GBP, USD)""")
    assertCompiles("""FxRate.of(GBP, USD, 1.5d)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_CurrencyPairDouble") {
    val test = rateOf(CurrencyPair.of(GBP, USD), 1.5d)
    test.pair shouldBe CurrencyPair.of(GBP, USD)
    test.rate shouldBe 1.5d
    test.fxRate(GBP, USD) should haveValue(1.5d)
    test.toString shouldBe "GBP/USD 1.5"
  }

  test("test_of_CurrencyPairDouble_reverseStandardOrder") {
    val test = rateOf(CurrencyPair.of(USD, GBP), 0.8d)
    test.pair shouldBe CurrencyPair.of(USD, GBP)
    test.rate shouldBe 0.8d
    test.fxRate(USD, GBP) should haveValue(0.8d)
    test.toString shouldBe "USD/GBP 0.8"
  }

  test("test_of_CurrencyPairDouble_same") {
    val test = rateOf(CurrencyPair.of(USD, USD), 1d)
    test.pair shouldBe CurrencyPair.of(USD, USD)
    test.pair.isIdentity shouldBe true
    test.rate shouldBe 1d
    test.fxRate(USD, USD) should haveValue(1d)
    test.toString shouldBe "USD/USD 1"
  }

  test("test_of_CurrencyPairDouble_invalid") {
    forAll(data_ofInvalidPair) { (pair: CurrencyPair, rate: Double, message: String) =>
      val outcome: ResultNec[FxRate] = FxRate.of(pair, rate)
      outcome should beFailureWith(FailureReason.INVALID)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    // this factory is the one the two-currency form delegates to, so it accumulates identically
    val both: List[Failure] = failuresOf(FxRate.of(CurrencyPair.of(GBP, GBP), -1d))
    both.map(failure => failure.message) shouldBe List(
      "Argument 'rate' must not be negative or zero but has value -1.0",
      "Conversion rate between identical currencies must be one")
  }

  test("test_of_CurrencyPairDouble_null") {
    // as above: the pair is a required argument of a type that cannot be absent, so the case the
    // original asserted at run time is asserted at compile time here. The first proof is the one
    // that matters - a rate alone does not name a pair - and the second shows that the text form
    // of a pair is not accepted in place of a pair.
    assertDoesNotCompile("""FxRate.of(1.5d)""")
    assertDoesNotCompile("""FxRate.of("GBP/USD", 1.5d)""")
    assertDoesNotCompile("""FxRate.of(CurrencyPair.of(GBP, USD))""")
    assertCompiles("""FxRate.of(CurrencyPair.of(GBP, USD), 1.5d)""")
  }

  //-------------------------------------------------------------------------
  test("test_toConventional") {
    // a rate whose pair is written against the market convention is reoriented and its rate
    // inverted, so `USD/GBP 0.8` is the same rate as `GBP/USD 1.25`. Which of the two directions
    // is conventional is decided by `CurrencyPair.isConventional`, whose own spec pins the
    // decision procedure; this test asserts only that the rate follows it.
    rateOf(USD, GBP, 0.8d).toConventional shouldBe rateOf(GBP, USD, 1.25d)
    rateOf(USD, GBP, 0.8d).toConventional.pair shouldBe CurrencyPair.of(GBP, USD)
    rateOf(USD, GBP, 0.8d).toConventional.rate shouldBe 1d / 0.8d

    // a rate already in the conventional direction is returned unchanged, rather than inverted
    // twice, so the member is idempotent
    rateOf(GBP, USD, 1.25d).toConventional shouldBe rateOf(GBP, USD, 1.25d)
    rateOf(GBP, USD, 1.25d).toConventional.toConventional shouldBe rateOf(GBP, USD, 1.25d)

    // an identity pair is conventional, so the identity rate is returned as it is. Inverting it
    // would give the same value, which is why this case is invisible in the result and asserted
    // here for the decision rather than for the arithmetic.
    rateOf(GBP, GBP, 1d).toConventional shouldBe rateOf(GBP, GBP, 1d)
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_good") {
    forAll(data_parseGood) { (input: String, base: Currency, counter: Currency, rate: Double) =>
      FxRate.parse(input) should haveValue(rateOf(base, counter, rate))
    }
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, message: String) =>
      val outcome: FailureOr[FxRate] = FxRate.parse(input)
      // text that names no rate is reported as a parsing failure rather than by throwing
      outcome should beFailureWith(FailureReason.PARSING)
      // and the wording says which kind of rejection it was: text the expression does not match
      // at all, or text that matches and then breaks a constraint of the type. The three rows of
      // the second kind are the constraint surface reached through parsing, and pinning the
      // wording per row is what keeps them from collapsing into the format failures.
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  //-------------------------------------------------------------------------
  test("test_inverse") {
    val test = rateOf(GBP, USD, 1.25d)
    test.inverse shouldBe rateOf(USD, GBP, 0.8d)
    test.inverse.pair shouldBe CurrencyPair.of(USD, GBP)
    // the reciprocal is the division the implementation performs, asserted as that division
    // rather than as a decimal literal that rounds to the same value
    test.inverse.rate shouldBe 1d / 1.25d
  }

  test("test_inverse_same") {
    // the identity rate is its own inverse: its pair inverts to itself and the reciprocal of one
    // is one
    val test = rateOf(GBP, GBP, 1d)
    test.inverse shouldBe rateOf(GBP, GBP, 1d)
    test.inverse.rate shouldBe 1d
  }

  //-------------------------------------------------------------------------
  test("test_fxRate_forBase") {
    // the two-currency lookup, which is the single abstract member of `FxRateProvider` and what
    // makes a rate usable wherever a provider is expected
    val test = rateOf(GBP, USD, 1.25d)
    forAll(data_fxRateFound) { (base: Currency, counter: Currency, rate: Double) =>
      test.fxRate(base, counter) should haveValue(rate)
    }
    forAll(data_fxRateMissing) { (base: Currency, counter: Currency, message: String) =>
      val outcome: FailureOr[Double] = test.fxRate(base, counter)
      // the absence of a rate is the expected outcome of asking about a pair this rate does not
      // hold, reported in the type rather than by throwing, and the reason is compared as a
      // member of the closed family of reasons
      outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  test("test_fxRate_forPair") {
    // the ordered-pair overload, which is defined as the lookup above applied to the two
    // currencies of the pair. The original named this test for the pair form and then called the
    // two-currency form in it; the rows are the rows of the original and they are routed through
    // the overload the name describes, so the two tests cover the two overloads.
    val test = rateOf(GBP, USD, 1.25d)
    forAll(data_fxRatePairFound) { (pair: CurrencyPair, rate: Double) =>
      test.fxRate(pair) should haveValue(rate)
    }
    forAll(data_fxRatePairMissing) { (pair: CurrencyPair, message: String) =>
      val outcome: FailureOr[Double] = test.fxRate(pair)
      outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  //-------------------------------------------------------------------------
  test("test_convert_double") {
    val test = rateOf(GBP, USD, 1.25d)
    // the conversion is the amount multiplied by the rate of the lookup, so it succeeds exactly
    // where the lookup does and carries its failure where it does not
    test.convert(100d, GBP, USD) should haveValue(125d)
    test.convert(100d, USD, GBP) should haveValue(100d / 1.25d)

    val missing: FailureOr[Double] = test.convert(100d, GBP, AUD)
    missing should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    missing should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/AUD"))
  }

  test("test_convert_Decimal") {
    val test = rateOf(GBP, USD, 1.25d)
    val hundred = decimalOf(Decimal.of(100L))
    // the exact-decimal conversion takes the same route as the type being ported: the rate is
    // looked up as a `Double` and the amount is multiplied by it through the decimal's own
    // multiplication, so the result is an exact decimal rather than a rounded binary value
    test.convert(hundred, GBP, USD) should haveValue(decimalOf(Decimal.of(125L)))
    test.convert(hundred, USD, GBP) should haveValue(decimalOf(Decimal.of(100d / 1.25d)))

    val missing: FailureOr[Decimal] = test.convert(hundred, GBP, AUD)
    missing should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    missing should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/AUD"))
  }

  //-------------------------------------------------------------------------
  test("test_crossRate") {
    forAll(data_crossRate) { (first: FxRate, second: FxRate, expected: FxRate) =>
      first.crossRate(second) should haveValue(expected)
    }

    // the pair of the cross is in market convention order whichever way round the two inputs
    // are written, which the equality above already requires; it is stated once directly so that
    // the orientation is visible in the spec and not only implied by the expected value
    eurUsd.crossRate(usdGbp).map(cross => cross.pair) shouldBe
      Right(CurrencyPair.of(EUR, GBP))
    gbpUsd.crossRate(usdEur).map(cross => cross.pair) shouldBe
      Right(CurrencyPair.of(EUR, GBP))

    forAll(data_crossRateInvalid) { (first: FxRate, second: FxRate, message: String) =>
      val outcome: FailureOr[FxRate] = first.crossRate(second)
      // two rates that name no third currency between them have no cross, which is reported as a
      // conversion failure naming both pairs rather than by throwing
      outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = rateOf(AUD, GBP, 1.25d)
    val a2 = rateOf(AUD, GBP, 1.25d)
    val b = rateOf(USD, GBP, 1.25d)
    val c = rateOf(USD, GBP, 1.35d)

    // `equals` is called as a method, as the original called it, so that the whole matrix is
    // stated - including the reflexive cases, which a `==` between two identical expressions
    // would be flagged for in this build. `b` and `c` differ in nothing but their rate, which is
    // what pins the rate as part of the identity of a value.
    a1.equals(a1) shouldBe true
    a1.equals(a2) shouldBe true
    a1.equals(b) shouldBe false
    a1.equals(c) shouldBe false

    b.equals(a1) shouldBe false
    b.equals(a2) shouldBe false
    b.equals(b) shouldBe true
    b.equals(c) shouldBe false

    c.equals(a1) shouldBe false
    c.equals(a2) shouldBe false
    c.equals(b) shouldBe false
    c.equals(c) shouldBe true

    a1.hashCode shouldBe a2.hashCode

    // the same statements through the instance that carries equality and hashing for this port,
    // which is where the rest of the library reads them from. The companion publishes one
    // equality-bearing instance - a `Hash`, which extends `Eq` - so there is no second notion of
    // equality that could disagree with these, and no `Order`, because rates of different pairs
    // do not order.
    Hash[FxRate].eqv(a1, a2) shouldBe true
    Hash[FxRate].eqv(a1, b) shouldBe false
    Hash[FxRate].eqv(b, c) shouldBe false
    Hash[FxRate].hash(a1) shouldBe Hash[FxRate].hash(a2)
  }

  test("test_equals_bad") {
    val test = rateOf(AUD, GBP, 1.25d)

    // a value of an unrelated type is not equal to a rate, in either direction. The original also
    // asserted equality against the absent-reference literal; that case is subsumed by the types
    // of this port, where a reference to a rate cannot be absent and there is no such literal to
    // compare against, so it is recorded here rather than written.
    test.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(test) shouldBe false
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not supported by this port; the JSON codec is the serialized form of
    // a rate, and it is an object of two fields - the pair as its text form and the rate as a
    // number. The two values the original round-tripped are round-tripped through it, one
    // explicit example each, with the encoded bytes pinned: the property-based sweep over every
    // codec of the module belongs to the module's JSON round-trip spec.
    val test = rateOf(GBP, USD, 1.25d)
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe """{"pair":"GBP/USD","rate":1.25}"""
    encoded.as[FxRate] shouldBe Right(test)

    val identityRate = rateOf(GBP, GBP, 1d)
    val encodedIdentity: Json = identityRate.asJson
    encodedIdentity.noSpaces shouldBe """{"pair":"GBP/GBP","rate":1.0}"""
    encodedIdentity.as[FxRate] shouldBe Right(identityRate)

    // decoding routes the two fields through the same factory a caller's arguments go through, so
    // a document naming a rate this type would not have built is a decoding failure rather than a
    // value that bypassed the constraints
    Json
      .obj("pair" -> Json.fromString("GBP/USD"), "rate" -> Json.fromDoubleOrNull(0d))
      .as[FxRate]
      .isLeft shouldBe true
    Json
      .obj("pair" -> Json.fromString("GBP/GBP"), "rate" -> Json.fromDoubleOrNull(1.25d))
      .as[FxRate]
      .isLeft shouldBe true
  }

  //-----------------------------------------------------------------------
  test("coverage") {
    // The reflective bean sweep of the original has no counterpart: there is no meta-bean to walk
    // and no property to read by name. What it was standing in for - that the value behaves as a
    // value - is asserted directly over two distinct rates through the instances the companion
    // publishes, and over the text form the rest of the library reads through `Show`.
    val test = rateOf(GBP, USD, 1.25d)
    val same = rateOf(GBP, USD, 1.25d)
    val other = rateOf(GBP, USD, 1.35d)

    Hash[FxRate].eqv(test, same) shouldBe true
    Hash[FxRate].eqv(test, other) shouldBe false
    Hash[FxRate].hash(test) shouldBe Hash[FxRate].hash(same)
    Hash[FxRate].hash(test) should not be Hash[FxRate].hash(other)

    // the rendering instance renders what the text form renders, so a rate reaching a message
    // through either route reads the same
    Show[FxRate].show(test) shouldBe test.toString
    Show[FxRate].show(test) shouldBe "GBP/USD 1.25"
    Show[FxRate].show(other) shouldBe "GBP/USD 1.35"

    // a whole-number rate is written without a fractional part, which is what the original wrote
    // and what documents and expectations carry
    val integral = rateOf(EUR, USD, 5d)
    integral.toString shouldBe "EUR/USD 5"
    Show[FxRate].show(integral) shouldBe "EUR/USD 5"

    // and the text form is the form the parsing factory reads back
    FxRate.parse(Show[FxRate].show(test)) should haveValue(test)
    FxRate.parse(Show[FxRate].show(integral)) should haveValue(integral)
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the rate out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite, and it exists because `FxRate.of` is the
   * only way to build a rate and reports what was wrong with its arguments as a value. The
   * outcome is matched rather than unwrapped by a partial accessor, so a fixture that fails to
   * build is reported as a test failure naming every reason it failed instead of raising an error
   * from somewhere else in the suite.
   *
   * @param outcome  the outcome expected to carry a rate
   * @return the rate it carries
   */
  private def unwrap(outcome: ResultNec[FxRate]): FxRate =
    outcome.fold(
      failures =>
        fail(
          "Expected a rate but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      rate => rate)

  /**
   * Builds a rate from two currencies and a rate, failing the test if the arguments describe
   * none.
   *
   * @param base  the base currency
   * @param counter  the counter currency
   * @param rate  the rate, expected to be one the type admits
   * @return the rate
   */
  private def rateOf(base: Currency, counter: Currency, rate: Double): FxRate =
    unwrap(FxRate.of(base, counter, rate))

  /**
   * Builds a rate from a currency pair and a rate, failing the test if the arguments describe
   * none.
   *
   * @param pair  the currency pair
   * @param rate  the rate, expected to be one the type admits
   * @return the rate
   */
  private def rateOf(pair: CurrencyPair, rate: Double): FxRate = unwrap(FxRate.of(pair, rate))

  /**
   * Reads the failures out of an outcome that is expected to have produced none of a rate.
   *
   * Only the accumulation assertion needs this: the matchers hold when '''some''' failure of an
   * outcome satisfies what was asked, which is the right reading for a single-failure outcome but
   * says nothing about how many failures a chain holds. Asserting that two broken constraints are
   * both reported, in order, needs the chain itself.
   *
   * @param outcome  the outcome expected to carry failures
   * @return the failures it carries, in the order it holds them
   */
  private def failuresOf(outcome: ResultNec[FxRate]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      rate => fail(s"Expected a failure but the factory built the rate $rate"))

  /**
   * Reads the decimal out of an outcome that is expected to have produced one.
   *
   * The decimal factories of the collection library report an unusable value the same way the
   * rate factory does, so the amounts of the decimal conversion test are unwrapped here. This is
   * a second helper rather than an overload of [[unwrap]] because the two error channels - a
   * single failure and a chain of them - are the same type once the compiler has erased them.
   *
   * @param outcome  the outcome expected to carry a decimal
   * @return the decimal it carries
   */
  private def decimalOf(outcome: FailureOr[Decimal]): Decimal =
    outcome.fold(
      failure => fail(s"Expected a decimal but it failed with: ${failure.message}"),
      value => value)
}
