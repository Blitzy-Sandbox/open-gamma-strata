/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.math.BigDecimal

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[Currency]], ported from the Java `CurrencyTest`.
 *
 * Every one of the twenty-five methods of the original is kept, under the name the original
 * gave it - including the misspelling in `test_triangulatonCurrency`, which is left exactly as
 * it was found so that the method-level traceability of the migration stays one-to-one. The two
 * methods the original drove from a data provider each become a single test that runs its whole
 * table, so a Java method and a test of this suite remain in one-to-one correspondence rather
 * than one-to-many.
 *
 * ===What the shape of the port changes===
 *
 * Four differences from the original are structural rather than a matter of taste, and each is
 * noted again at the test it affects:
 *
 *  - '''An unknown code is rejected rather than invented.''' The original minted a currency for
 *    any three upper-case letters, guessing zero minor unit digits and USD triangulation, so
 *    `Currency.of("AAA")` returned a usable currency whose data was a guess. This family is
 *    closed over the currencies whose reference data the module holds, and a code outside it is
 *    reported as a `Failure` whose reason is `PARSING`. This is the one behavioural divergence
 *    of the type, recorded as such in `SCALA_MIGRATION.md`, and it is what
 *    `test_of_String_unknownCurrencyCreated` and `test_parse_String_unknownCurrencyCreated`
 *    assert - each keeps the name of the method whose behaviour it replaces, because that is
 *    the behaviour a reader of either language will come here looking for.
 *  - '''Failure is a value.''' Both factories report a failure instead of raising one, so every
 *    assertion that was `assertThatIllegalArgumentException` is an assertion that the outcome
 *    is a `Left` carrying the `PARSING` reason, compared as a value of the closed family of
 *    reasons rather than as text.
 *  - '''No absent argument can be passed.''' The two places the original passed Java's absent
 *    reference - the last row of each bad-input provider, and `test_compareTo_null` - have no
 *    counterpart, because the port drops the `notNull` family of the argument checker along with
 *    the exceptions it raised. Each keeps its name and asserts the nearest thing that can be
 *    written, which for the providers is one more piece of malformed text and for the comparison
 *    is a proof that the compiler will not let a currency be omitted or supplied at the wrong
 *    type.
 *  - '''Reflection and platform serialization are gone.''' `coverage` swept the private
 *    constructor of the loader that read the currency configuration, `test_consistency`
 *    reflected over the constant fields of `Currency`, and `test_serialization` round-tripped a
 *    currency through the serialization mechanism of the platform. The loader has no
 *    counterpart - the configuration is compiled data now - so each of the three asserts the
 *    guarantee it stood for over the compiled data and the JSON codec instead, with no
 *    reflection of any kind.
 *
 * ===What this suite pins about the data, and what it leaves to others===
 *
 * The contents of each row of the reference data - the minor unit digits and the triangulation
 * currency the row carries - are asserted row for row against the data captured from the
 * implementation being ported, by `ReferenceDataManifestSpec`, and the closedness of every
 * named family is asserted by `NamedEnumClosedSpec`; neither is repeated here. The inventory of
 * the family is pinned here, because the behaviour this suite asserts is stated over it:
 * `coverage` compares the seventy-four codes, the nineteen historic ones, the fifty-five active
 * ones and the nine market convention priority entries against codes transcribed into this file
 * as literals and in order, so a table substituted wholesale, a row moved between the active
 * and the historic part or a reordered priority list fails rather than passing a count, and
 * `test_constants` reads each of the fifty-five published constants back against the code
 * written here for it, so a constant bound to the wrong currency fails as well. Everything else
 * this suite asserts is the behaviour of the type: how a code resolves, what a currency knows
 * about itself, how it rounds, orders, compares, renders and serializes.
 *
 * Likewise the typeclass law suites belong to `TypeclassLawsSpec`, the construction-surface
 * proofs to `ApiSurfaceSpec` and the property-based sweep of every codec to
 * `json.JsonRoundTripSpec`; the single compile-time proof in `test_compareTo_null` and the
 * single representation example in `test_serialization` are the ports of those two Java
 * methods, not a second copy of those sweeps.
 */
final class CurrencySpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The seven currencies the original named in four of its methods, with their codes.
   *
   * The original repeated the same seven currencies in `test_constants`, `test_getAvailable`,
   * `test_minorUnits` and `test_triangulatonCurrency`. They are gathered here once so that the
   * set under test cannot drift between those tests, and each of them keeps its own test.
   */
  private val dataConstants: TableFor2[String, Currency] = Table(
    ("code", "currency"),
    ("USD", Currency.USD),
    ("EUR", Currency.EUR),
    ("JPY", Currency.JPY),
    ("GBP", Currency.GBP),
    ("CHF", Currency.CHF),
    ("AUD", Currency.AUD),
    ("CAD", Currency.CAD))

  /**
   * Every one of the fifty-five published constants, with the code each is required to carry.
   *
   * The pairing is the thing under test, which is why the code of every row is written out here
   * as a literal and is never read off the constant beside it. A constant bound to the wrong row
   * of the reference data - a `val NZD` reading the row of `NOK` - leaves the size of the
   * family, the set of currencies on offer and the round trip of every code through the
   * factories exactly as they were, so nothing but an expectation written independently of the
   * declaration can catch it. The original read the same fact reflectively off the constant
   * fields of the type; this table is the port of that sweep, without reflection of any kind.
   *
   * The rows are in the declaration order of [[Currency]] - the eight currencies it groups as
   * the most commonly traded, then the other currencies alphabetically, then the unapplicable
   * currency and the four metals - so that the table can be read against those declarations
   * side by side. The order is immaterial to what is asserted.
   */
  private val publicConstants: TableFor2[String, Currency] = Table(
    ("code", "currency"),
    ("USD", Currency.USD),
    ("EUR", Currency.EUR),
    ("JPY", Currency.JPY),
    ("GBP", Currency.GBP),
    ("CHF", Currency.CHF),
    ("AUD", Currency.AUD),
    ("CAD", Currency.CAD),
    ("NZD", Currency.NZD),
    ("AED", Currency.AED),
    ("ARS", Currency.ARS),
    ("BGN", Currency.BGN),
    ("BHD", Currency.BHD),
    ("BRL", Currency.BRL),
    ("CLP", Currency.CLP),
    ("CNH", Currency.CNH),
    ("CNY", Currency.CNY),
    ("COP", Currency.COP),
    ("CZK", Currency.CZK),
    ("DKK", Currency.DKK),
    ("EGP", Currency.EGP),
    ("HKD", Currency.HKD),
    ("HRK", Currency.HRK),
    ("HUF", Currency.HUF),
    ("IDR", Currency.IDR),
    ("ILS", Currency.ILS),
    ("INR", Currency.INR),
    ("ISK", Currency.ISK),
    ("KRW", Currency.KRW),
    ("KZT", Currency.KZT),
    ("MAD", Currency.MAD),
    ("MXN", Currency.MXN),
    ("MYR", Currency.MYR),
    ("NOK", Currency.NOK),
    ("OMR", Currency.OMR),
    ("PEN", Currency.PEN),
    ("PHP", Currency.PHP),
    ("PKR", Currency.PKR),
    ("PLN", Currency.PLN),
    ("QAR", Currency.QAR),
    ("RON", Currency.RON),
    ("RUB", Currency.RUB),
    ("SAR", Currency.SAR),
    ("SEK", Currency.SEK),
    ("SGD", Currency.SGD),
    ("THB", Currency.THB),
    ("TRY", Currency.TRY),
    ("TWD", Currency.TWD),
    ("UAH", Currency.UAH),
    ("VND", Currency.VND),
    ("ZAR", Currency.ZAR),
    ("XXX", Currency.XXX),
    ("XAG", Currency.XAG),
    ("XAU", Currency.XAU),
    ("XPD", Currency.XPD),
    ("XPT", Currency.XPT))

  /**
   * The number of minor unit digits of each of the seven currencies.
   *
   * The zero of the yen is the row that matters: it is the only one of the seven whose minor
   * unit differs, and it is what the rounding tests below exercise from the other side.
   */
  private val dataMinorUnits: TableFor2[String, Int] = Table(
    ("code", "minorUnitDigits"),
    ("USD", 2),
    ("EUR", 2),
    ("JPY", 0),
    ("GBP", 2),
    ("CHF", 2),
    ("AUD", 2),
    ("CAD", 2))

  /**
   * The triangulation currency of each of the seven currencies.
   *
   * All seven triangulate through the dollar, the dollar included, which is the answer the
   * reference data of each of them carries.
   */
  private val dataTriangulation: TableFor2[String, Currency] = Table(
    ("code", "triangulationCurrency"),
    ("USD", Currency.USD),
    ("EUR", Currency.USD),
    ("JPY", Currency.USD),
    ("GBP", Currency.USD),
    ("CHF", Currency.USD),
    ("AUD", Currency.USD),
    ("CAD", Currency.USD))

  /**
   * Text that names no currency through the exact factory, transcribed from the Java provider.
   *
   * The first six rows are the six the original listed, verbatim and in its order: empty text,
   * two letters, a lower-case code, four letters, three digits and a code with a leading space.
   * Each sits deliberately on one side of a boundary - too short, too long, wrong case, wrong
   * character class, padded - so a single altered character would turn a boundary case into a
   * repeat of another row.
   *
   * The seventh row is this port's replacement for the absent-reference row of the provider: no
   * such value can be passed to this factory, so the nearest input that can actually be written
   * is one more piece of malformed text, a code whose third character is a digit.
   */
  private val dataOfBad: TableFor1[String] = Table(
    "input",
    "",
    "AB",
    "gbp",
    "ABCD",
    "123",
    " GBP",
    "GB1")

  /**
   * Text that names no currency through the case-tolerant factory, transcribed from the Java
   * provider.
   *
   * The same rows as [[dataOfBad]] without the lower-case code, which this factory accepts by
   * design and which `test_parse_String_lowerCase` asserts it accepts; the absent-reference row
   * of the original is replaced on the same reasoning as there.
   */
  private val dataParseBad: TableFor1[String] = Table(
    "input",
    "",
    "AB",
    "ABCD",
    "123",
    " GBP",
    "GB1")

  /**
   * Amounts and their expected roundings, transcribed from the three rounding methods of the
   * original.
   *
   * The five rows are the five the original asserted, and the three dollar rows are the ones
   * that pin the rule rather than illustrate it: `63.34500001` and `63.34499999` straddle the
   * tie by one part in a hundred million, and they round in opposite directions. The two yen
   * rows exercise the same rule at a minor unit of zero digits.
   *
   * Every expectation is written as a `Double` so that this table serves both the `Double`
   * method and the [[Decimal]] method; the arbitrary-precision method needs its expectations as
   * text and has [[dataRoundMinorUnitsText]] for that reason.
   */
  private val dataRoundMinorUnits: TableFor3[Currency, Double, Double] = Table(
    ("currency", "amount", "expected"),
    (Currency.USD, 63.347d, 63.35d),
    (Currency.USD, 63.34500001d, 63.35d),
    (Currency.USD, 63.34499999d, 63.34d),
    (Currency.JPY, 63.347d, 63.0d),
    (Currency.JPY, 63.5347d, 64.0d))

  /**
   * The same five rounding rows with their expectations as text, for the arbitrary-precision
   * method.
   *
   * The scale of the result is part of what that method promises - a dollar amount rounds to
   * two decimal places and keeps them, a yen amount to none - and an expectation built from
   * text is the only way to state a scale exactly, which is why the original built its
   * expectations from text too. `63` and `64` therefore carry no decimal places, while `63.35`
   * and `63.34` carry two.
   */
  private val dataRoundMinorUnitsText: TableFor3[Currency, Double, String] = Table(
    ("currency", "amount", "expected"),
    (Currency.USD, 63.347d, "63.35"),
    (Currency.USD, 63.34500001d, "63.35"),
    (Currency.USD, 63.34499999d, "63.34"),
    (Currency.JPY, 63.347d, "63"),
    (Currency.JPY, 63.5347d, "64"))

  /**
   * The seventy-four currency codes of the reference data, in the order it declares them.
   *
   * Transcribed from the seventy-four sections of
   * `modules/basics/src/main/resources/META-INF/com/opengamma/strata/config/base/Currency.ini`,
   * the read-only configuration that the compiled table of [[CurrencyData]] replaces, in the
   * order that file lists them: the active currencies alphabetically, then the unapplicable
   * currency and the four metals, then the currencies replaced by the euro.
   *
   * Written out as a literal because that is the only way the comparison in `coverage` is a
   * comparison against the configuration rather than a comparison of the compiled table with
   * itself. Every other view of the codes - `CurrencyData.codes`, `nonHistoricCodes`,
   * `historicCodes`, `Currency.values` and `Currency.getAvailableCurrencies` - is derived from
   * that one table, so all of them would agree with it however it had been mistranscribed.
   */
  private val expectedCodes: Vector[String] = Vector(
    "AED", "ARS", "AUD", "BGN", "BHD", "BRL", "CAD", "CHF", "CLP", "CNH",
    "CNY", "COP", "CZK", "DKK", "EGP", "EUR", "GBP", "HKD", "HRK", "HUF",
    "IDR", "ILS", "INR", "ISK", "JPY", "KRW", "KZT", "MAD", "MXN", "MYR",
    "NOK", "NZD", "OMR", "PEN", "PHP", "PKR", "PLN", "QAR", "RON", "RUB",
    "SAR", "SEK", "SGD", "THB", "TRY", "TWD", "UAH", "USD", "VND", "ZAR",
    "XXX", "XAG", "XAU", "XPD", "XPT", "ATS", "BEF", "CYP", "DEM", "EEK",
    "ESP", "FIM", "FRF", "GRD", "IEP", "ITL", "LTL", "LUF", "LVL", "MTL",
    "NLG", "PTE", "SIT", "SKK")

  /**
   * The nineteen codes the configuration marks historic, in the order it declares them.
   *
   * These are the sections of `Currency.ini` carrying `historic = true`, which are the
   * currencies replaced by the euro. The split is a datum in its own right - the code list above
   * does not carry it - so it is transcribed separately, and it is the datum that decides which
   * currencies `Currency.getAvailableCurrencies` offers and which merely remain resolvable.
   */
  private val expectedHistoricCodes: Vector[String] = Vector(
    "ATS", "BEF", "CYP", "DEM", "EEK", "ESP", "FIM", "FRF", "GRD", "IEP",
    "ITL", "LTL", "LUF", "LVL", "MTL", "NLG", "PTE", "SIT", "SKK")

  /**
   * The fifty-five codes the configuration leaves in active use, in the order it declares them.
   *
   * The complement of [[expectedHistoricCodes]] within [[expectedCodes]], transcribed rather
   * than computed from the other two so that the three literals check one another, and the
   * codes that are required to have one published constant apiece.
   */
  private val expectedActiveCodes: Vector[String] = Vector(
    "AED", "ARS", "AUD", "BGN", "BHD", "BRL", "CAD", "CHF", "CLP", "CNH",
    "CNY", "COP", "CZK", "DKK", "EGP", "EUR", "GBP", "HKD", "HRK", "HUF",
    "IDR", "ILS", "INR", "ISK", "JPY", "KRW", "KZT", "MAD", "MXN", "MYR",
    "NOK", "NZD", "OMR", "PEN", "PHP", "PKR", "PLN", "QAR", "RON", "RUB",
    "SAR", "SEK", "SGD", "THB", "TRY", "TWD", "UAH", "USD", "VND", "ZAR",
    "XXX", "XAG", "XAU", "XPD", "XPT")

  /**
   * The nine currencies of the market convention priority ordering, highest priority first.
   *
   * Transcribed from the `ordering` entry of the `[marketConventionPriority]` section of
   * `CurrencyData.ini`, which sits beside the `Currency.ini` named above under
   * `modules/basics/src/main/resources/META-INF/com/opengamma/strata/config/base`. The ordering
   * itself is the data - it decides the base currency of a pair that is not configured - so it
   * is asserted in order rather than as a set.
   */
  private val expectedMarketConventionPriority: Vector[String] =
    Vector("XAU", "EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "JPY")

  /**
   * Resolves a currency code, failing the test when the family holds no currency for it.
   *
   * The original wrote `Currency test = Currency.of("SEK")` and went on to assert over `test`.
   * The factory here answers with a value rather than with an exception, so this helper is what
   * keeps those tests reading the same way: it unwraps the successful side once, and reports
   * the failure of an unexpected `Left` as a failed assertion naming the code and the message
   * rather than as a pattern match the reader has to step over.
   *
   * Every code passed to it in this suite is a code the family holds; the tests that assert a
   * code is rejected use the factory directly, so that the failure is the thing being asserted.
   *
   * @param code  the three letter code of a currency the family is expected to hold
   * @return the currency with that code
   */
  private def currencyOf(code: String): Currency =
    Currency.of(code).fold(
      failure => fail(s"Expected a currency for '$code' but was Failure: ${failure.message}"),
      currency => currency)

  /**
   * Parses text into a currency, failing the test when it names none.
   *
   * The case-tolerant counterpart of [[currencyOf]], kept separate so that a test asserting the
   * leniency of parsing cannot accidentally go through the exact factory, and vice versa.
   *
   * @param text  text expected to name a currency of the family, in any case
   * @return the currency the text names
   */
  private def currencyParsed(text: String): Currency =
    Currency.parse(text).fold(
      failure => fail(s"Expected a currency for '$text' but was Failure: ${failure.message}"),
      currency => currency)

  //-------------------------------------------------------------------------
  test("test_constants") {
    forAll(dataConstants) { (code: String, currency: Currency) =>
      withClue(s"$code: ") {
        Currency.of(code) should haveValue(currency)
        // a constant and the result of resolving its code are the same object, because the
        // constants select from the single set of instances the companion builds rather than
        // constructing their own
        (currencyOf(code) eq currency) shouldBe true
      }
    }

    // The seven rows above are the seven the original named. The constants themselves are
    // fifty-five, and each of them is asserted here against the code written for it in
    // `publicConstants`, which is what makes a constant reading the wrong row of the reference
    // data a failure: the code and the name a constant reports have to be the code expected of
    // it, and resolving that code has to yield that very instance through both factories.
    forAll(publicConstants) { (code: String, currency: Currency) =>
      withClue(s"$code: ") {
        currency.code shouldBe code
        currency.name shouldBe code
        (currencyOf(code) eq currency) shouldBe true
        Currency.valueOf(code) shouldBe Some(currency)
      }
    }

    val constantCodes: Set[String] =
      publicConstants.iterator.map { case (code, _) => code }.toSet
    val constantCurrencies: Set[Currency] =
      publicConstants.iterator.map { case (_, currency) => currency }.toSet
    // the table has to hold all fifty-five constants and no constant twice, or the sweep above
    // would pass while saying nothing about a constant it happens not to mention
    publicConstants.size shouldBe 55
    constantCodes.size shouldBe 55
    constantCurrencies.size shouldBe 55
    // and the fifty-five have to be exactly the currencies on offer: this is the completeness
    // direction, failing both for an active currency that no constant names and for a constant
    // that names a currency the reference data marks historic
    constantCurrencies shouldBe Currency.getAvailableCurrencies
  }

  //-------------------------------------------------------------------------
  test("test_getAvailable") {
    val available: Set[Currency] = Currency.getAvailableCurrencies
    forAll(dataConstants) { (code: String, currency: Currency) =>
      withClue(s"$code: ") {
        available should contain(currency)
      }
    }
    // The original exposed the currencies it had configured and left the ones replaced by the
    // euro out of that set, which is the contract this size states from the Scala side: the
    // fifty-five currencies in active use, and none of the nineteen historic ones. That the
    // active rows are exactly the rows with constants is asserted by `test_constants` above,
    // over the whole of the fifty-five, and again for the non-metal subset of them by
    // `test_consistency` below.
    available.size shouldBe 55
    available.size shouldBe CurrencyData.nonHistoricCodes.size
  }

  //-------------------------------------------------------------------------
  test("test_of_String") {
    val test: Currency = currencyOf("SEK")
    test.code shouldBe "SEK"
    test.name shouldBe "SEK"
    test shouldBe Currency.SEK
    // the original asserted the identity of two separate resolutions of the same code; it holds
    // here because a currency exists only as one of the instances of the companion
    (test eq currencyOf("SEK")) shouldBe true
  }

  test("test_of_String_historicCurrency") {
    val test: Currency = currencyOf("BEF")
    test.code shouldBe "BEF"
    test.minorUnitDigits shouldBe 2
    test.triangulationCurrency shouldBe Currency.EUR
    (test eq currencyOf("BEF")) shouldBe true
    // a currency replaced by the euro resolves with its real minor units and its real
    // triangulation currency, as it did in the original, but it is not one of the currencies
    // offered to choose from
    Currency.getAvailableCurrencies should not contain (test)
  }

  /**
   * Asserts the one behavioural divergence of this type.
   *
   * The original minted a currency for any three upper-case letters that named none of the
   * currencies it had configured, giving it zero minor unit digits and USD triangulation, and
   * cached it; the name of this method is a record of that. Here the family is closed over the
   * currencies whose reference data the module holds, so a code outside it is reported as a
   * `Failure` whose reason is `PARSING` - the divergence recorded in `SCALA_MIGRATION.md`.
   */
  test("test_of_String_unknownCurrencyCreated") {
    val outcome: FailureOr[Currency] = Currency.of("AAA")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(".*AAA.*")
    // nothing was created and nothing was cached, so the absent value is absent again
    Currency.valueOf("AAA") shouldBe None
    Currency.of("AAA") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_String_lowerCase") {
    // this factory matches the code exactly, as the original did; the case-tolerant route is
    // `parse`, which `test_parse_String_lowerCase` asserts accepts this very input
    Currency.of("gbp") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_String_bad") {
    forAll(dataOfBad) { (input: String) =>
      withClue(s"'$input': ") {
        Currency.of(input) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_parse_String") {
    val test: Currency = currencyParsed("GBP")
    test.code shouldBe "GBP"
    test shouldBe Currency.GBP
    (test eq Currency.GBP) shouldBe true
  }

  /**
   * Asserts the same divergence as `test_of_String_unknownCurrencyCreated`, reached through the
   * case-tolerant factory.
   *
   * The original folded the text to upper case and then minted `ZYX`; here the fold happens and
   * the lookup then fails, so the failure names the folded text rather than the text as given.
   */
  test("test_parse_String_unknownCurrencyCreated") {
    val outcome: FailureOr[Currency] = Currency.parse("zyx")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(".*ZYX.*")
    Currency.of("ZYX") should beFailureWith(FailureReason.PARSING)
  }

  test("test_parse_String_lowerCase") {
    val test: Currency = currencyParsed("gbp")
    test.code shouldBe "GBP"
    (test eq Currency.GBP) shouldBe true
    // the fold is over the whole of the text, so mixed case resolves as well; this is the only
    // leniency this family offers, since it declares no alternate spelling and no lenient
    // pattern
    (currencyParsed("GbP") eq Currency.GBP) shouldBe true
  }

  test("test_parse_String_bad") {
    forAll(dataParseBad) { (input: String) =>
      withClue(s"'$input': ") {
        Currency.parse(input) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_minorUnits") {
    forAll(dataMinorUnits) { (code: String, minorUnitDigits: Int) =>
      withClue(s"$code: ") {
        currencyOf(code).minorUnitDigits shouldBe minorUnitDigits
      }
    }
  }

  test("test_triangulatonCurrency") {
    // the name of this method carries the misspelling of the original deliberately, so that the
    // Java method and this test stay in one-to-one correspondence in the migration manifest
    forAll(dataTriangulation) { (code: String, triangulationCurrency: Currency) =>
      withClue(s"$code: ") {
        currencyOf(code).triangulationCurrency shouldBe triangulationCurrency
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_roundMinorUnits_double") {
    forAll(dataRoundMinorUnits) { (currency: Currency, amount: Double, expected: Double) =>
      withClue(s"${currency.code} $amount: ") {
        currency.roundMinorUnits(amount) shouldBe expected
      }
    }
  }

  test("test_roundMinorUnits_BigDecimal") {
    forAll(dataRoundMinorUnitsText) { (currency: Currency, amount: Double, expected: String) =>
      withClue(s"${currency.code} $amount: ") {
        // the amount is built from a `Double`, exactly as the original built it, so the value
        // rounded is the full binary expansion of that double and not the short decimal its
        // text names; the expectation is built from text, because only text states a scale
        val rounded: BigDecimal = currency.roundMinorUnits(new BigDecimal(amount))
        val expectedAmount: BigDecimal = new BigDecimal(expected)
        rounded shouldBe expectedAmount
        // equality of an arbitrary-precision decimal is sensitive to its scale, so the
        // assertion above pins the number of decimal places as well as the value; stating the
        // scale on its own line makes that part of the contract explicit rather than implied
        rounded.scale shouldBe expectedAmount.scale
        rounded.scale shouldBe currency.minorUnitDigits
      }
    }
  }

  test("test_roundMinorUnits_Decimal") {
    forAll(dataRoundMinorUnits) { (currency: Currency, amount: Double, expected: Double) =>
      withClue(s"${currency.code} $amount: ") {
        val rounded: FailureOr[Decimal] =
          Decimal.of(amount).map(decimal => currency.roundMinorUnits(decimal))
        val expectedAmount: FailureOr[Decimal] = Decimal.of(expected)
        // both sides are asserted to hold a value before they are compared, so that a pair of
        // failures cannot pass as a pair of equal outcomes
        rounded should beSuccess
        expectedAmount should beSuccess
        rounded shouldBe expectedAmount
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_compareTo") {
    val a: Currency = Currency.EUR
    val b: Currency = Currency.GBP
    val c: Currency = Currency.JPY
    // the comparison of the original is the ordering typeclass here, which is the same value as
    // the hashing and the equality of the type, so these assertions are about the one instance
    // the companion publishes
    Order[Currency].compare(a, a) shouldBe 0
    Order[Currency].compare(b, b) shouldBe 0
    Order[Currency].compare(c, c) shouldBe 0

    (Order[Currency].compare(a, b) < 0) shouldBe true
    (Order[Currency].compare(b, a) > 0) shouldBe true

    (Order[Currency].compare(a, c) < 0) shouldBe true
    (Order[Currency].compare(c, a) > 0) shouldBe true

    (Order[Currency].compare(b, c) < 0) shouldBe true
    (Order[Currency].compare(c, b) > 0) shouldBe true

    // the ordering is alphabetical by code, which is the ordering the original documented and
    // produced, so sorting the three currencies recovers the order asserted above
    List(c, a, b).sorted(Order[Currency].toOrdering) shouldBe List(a, b, c)
  }

  /**
   * Records that the guard the original asserted here cannot be written in the port.
   *
   * The Java method asserted that comparing a currency with Java's absent reference raised a
   * `NullPointerException`. Comparison here is the ordering typeclass, whose operation takes two
   * currencies and admits no such value, and the port drops the `notNull` family of the argument
   * checker along with the exceptions it raised. The method therefore keeps its name and asserts
   * the fact that replaced the guard: the compiler refuses a comparison whose second argument is
   * missing or is not a currency, so there is no call for a run-time guard to reject.
   */
  test("test_compareTo_null") {
    assertDoesNotCompile("""Order[Currency].compare(Currency.EUR, "GBP")""")
    assertDoesNotCompile("""Order[Currency].compare(Currency.EUR)""")
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1: Currency = Currency.GBP
    val a2: Currency = currencyOf("GBP")
    val b: Currency = Currency.EUR
    (a1 == a1) shouldBe true
    (a1 == b) shouldBe false
    (a1 == a2) shouldBe true

    (a2 == a1) shouldBe true
    (a2 == a2) shouldBe true
    (a2 == b) shouldBe false

    (b == a1) shouldBe false
    (b == a2) shouldBe false
    (b == b) shouldBe true

    a1.hashCode shouldBe a2.hashCode

    // the equality of the type and the equality-bearing instance of the companion are one
    // notion, so the same matrix has to hold through the typeclass
    Eq[Currency].eqv(a1, a2) shouldBe true
    Eq[Currency].eqv(a1, b) shouldBe false
    Eq[Currency].eqv(b, a2) shouldBe false
    Hash[Currency].hash(a1) shouldBe Hash[Currency].hash(a2)
  }

  test("test_equals_bad") {
    val a: Any = Currency.GBP
    val foreign: Any = new Object
    // The original also asserted that a currency does not equal Java's absent reference. That
    // case is subsumed by the types of the port, where a currency cannot hold one and the
    // comparison cannot be written; the two foreign-type cases below are the part of the method
    // that remains expressible, and they are asserted through a reference of type `Any` so that
    // the comparison is the universal one the original performed.
    (a == "String") shouldBe false
    (a == foreign) shouldBe false
    (a == Currency.EUR) shouldBe false
    (a == Currency.GBP) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    val test: Currency = Currency.GBP
    test.toString shouldBe "GBP"
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the properties the reflective sweep of the original stood for.
   *
   * The Java method swept the private constructor of the loader that read the currency
   * configuration from the class path, to record that the class was not meant to be
   * instantiated. That loader has no counterpart here - the configuration is the compiled data
   * of [[CurrencyData]], so there is nothing to load, and the data is an object, so there is no
   * constructor to reach - and the sweep itself worked by reflection, which this port does not
   * use anywhere. What the method stood for is therefore asserted directly: the typeclass
   * instances of the type agree with one another over two distinct currencies, and the compiled
   * data holds exactly the inventory the loader used to read - the seventy-four codes in the
   * order the configuration declares them, the nineteen historic ones, the fifty-five active
   * ones and the nine market convention priority entries, each compared against the codes
   * transcribed from that configuration above rather than against a count.
   */
  test("coverage") {
    val gbp: Currency = Currency.GBP
    val eur: Currency = Currency.EUR
    Eq[Currency].eqv(gbp, gbp) shouldBe true
    Eq[Currency].eqv(gbp, eur) shouldBe false
    Hash[Currency].hash(gbp) shouldBe Hash[Currency].hash(currencyOf("GBP"))
    // the hashing instance is the equality instance of this type, so the inequality of two
    // currencies is asserted through it as an equality; that their hashes differ is deliberately
    // not asserted, because hashing promises only that equal values hash equally and two unequal
    // currencies are free to collide without breaking anything
    Hash[Currency].eqv(gbp, eur) shouldBe false
    Show[Currency].show(gbp) shouldBe "GBP"
    Show[Currency].show(eur) shouldBe "EUR"
    (Order[Currency].compare(gbp, eur) > 0) shouldBe true

    // the cardinalities of the data the loader was replaced by: the whole table, the active
    // subset the constants name, the historic remainder and the market convention ordering
    CurrencyData.rows.size shouldBe 74
    CurrencyData.nonHistoricCodes.size shouldBe 55
    CurrencyData.historicCodes.size shouldBe 19
    CurrencyData.marketConventionPriority.size shouldBe 9
    Currency.values.size shouldBe CurrencyData.rows.size

    // the three transcriptions are checked against one another before any of them is used as an
    // expectation, so that a code dropped from one of them cannot pass as agreement with the
    // table: the active codes followed by the historic ones are the whole inventory, which is
    // how the configuration lists them
    (expectedActiveCodes ++ expectedHistoricCodes) shouldBe expectedCodes

    // and the inventory itself, code for code and in order, against those transcriptions rather
    // than against another view of the same compiled table. A table substituted wholesale, a row
    // moved between the active and the historic part, a pair of codes exchanged or a reordered
    // priority list is a failure here, and none of them is visible to the sizes above.
    CurrencyData.codes shouldBe expectedCodes
    CurrencyData.historicCodes shouldBe expectedHistoricCodes
    CurrencyData.nonHistoricCodes shouldBe expectedActiveCodes
    CurrencyData.marketConventionPriority shouldBe expectedMarketConventionPriority
    Currency.values.toList.map(currency => currency.code) shouldBe expectedCodes.toList
  }

  /**
   * Asserts the serialized form of a currency, which is its JSON form.
   *
   * The Java method round-tripped a currency through the serialization mechanism of the
   * platform, which this port does not support. Its replacement is the JSON codec, and what
   * this test pins is the shape that codec is required to have: a currency is written as the
   * bare string of its code and never as an object, so a document written by the implementation
   * being ported reads back here as the same currency. One explicit example is asserted here
   * deliberately - the property-based round trip over every codec-bearing type of the module
   * belongs to `json.JsonRoundTripSpec`, which is where the migration manifest records this
   * Java method as consolidated, and this test is the per-type representation rather than a
   * second copy of that sweep.
   */
  test("test_serialization") {
    val encoded: Json = Encoder[Currency].apply(Currency.GBP)
    encoded shouldBe Json.fromString("GBP")
    encoded.asString shouldBe Some("GBP")

    val decoded: Either[DecodingFailure, Currency] = Decoder[Currency].decodeJson(encoded)
    decoded shouldBe Right(Currency.GBP)

    // a string naming no currency of the family is rejected as a decoding failure rather than
    // decoded into a currency the factory could not have produced
    val rejected: Either[DecodingFailure, Currency] =
      Decoder[Currency].decodeJson(Json.fromString("AAA"))
    rejected.isLeft shouldBe true
  }

  /**
   * Asserts the text contract the reflective string conversion of the original gave.
   *
   * The Java method asserted the round trip of the string-conversion library the type was
   * annotated for. That library is gone with the port, and the guarantee its two annotations
   * gave is what this test asserts directly: a currency renders as its three letter code -
   * through its name, through its rendering instance and through `toString`, all three agreeing
   * - and that code reads back as the same currency. These are the identities the migration
   * treats as load-bearing, because they are what a document, a log line or a test expectation
   * written before the port depends on.
   */
  test("test_jodaConvert") {
    forAll(dataConstants) { (code: String, currency: Currency) =>
      withClue(s"$code: ") {
        currency.name shouldBe code
        currency.toString shouldBe code
        Show[Currency].show(currency) shouldBe code
        Currency.parse(currency.name) should haveValue(currency)
        (currencyParsed(currency.name) eq currency) shouldBe true
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that the currency table, the currency pair table and the currencies on offer
   * describe the same set of currencies.
   *
   * The Java method built three sets and asserted them equal: the currencies of the
   * configuration it loaded, the currencies named by the configured currency pairs, and the
   * currencies held in the public constant fields of the type, which it read reflectively. Each
   * set was filtered of the codes beginning with `X`, because those are the metals and the
   * unapplicable currency, which are configured and are traded as pairs but are not currencies
   * of a country.
   *
   * The port builds the same three sets without reflection and without a loader: the first from
   * the compiled currency table, the second from the compiled currency pair table, and the third
   * from the currencies on offer, which is what the constant fields amounted to. The filter is
   * the one the original applied, character for character.
   */
  test("test_consistency") {
    // every active row of the table resolves, so the set below is the whole active subset and
    // not a subset of it that a missing row would silently shrink
    val nonHistoric: Set[Currency] = CurrencyData.rows.iterator
      .filterNot(row => row.historic)
      .flatMap(row => Currency.valueOf(row.code).toList)
      .toSet
    nonHistoric.size shouldBe CurrencyData.nonHistoricCodes.size

    val dataCurrencies: Set[Currency] =
      nonHistoric.filter(currency => currency.code.charAt(0) != 'X')

    val pairCurrencies: Set[Currency] = CurrencyPairData.rows.iterator
      .flatMap { case (base, counter, _) => List(base, counter) }
      .filter(currency => currency.code.charAt(0) != 'X')
      .toSet

    val availableCurrencies: Set[Currency] =
      Currency.getAvailableCurrencies.filter(currency => currency.code.charAt(0) != 'X')

    dataCurrencies shouldBe pairCurrencies
    dataCurrencies shouldBe availableCurrencies

    // the three sets are compared as sets, so their order is immaterial; the size is asserted so
    // that the two equalities above cannot be satisfied by three empty sets
    dataCurrencies.size shouldBe 50
  }
}
