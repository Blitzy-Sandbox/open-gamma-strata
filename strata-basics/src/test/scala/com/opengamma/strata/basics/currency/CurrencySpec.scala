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
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[Currency]].
 *
 * The family is closed over the currencies whose reference data the module holds: a code outside
 * it is a `Failure` whose reason is `PARSING`, not a currency resolved with invented data.
 *
 * The inventories the data is compared against - the seventy-four codes in declared order, the
 * nineteen historic ones, the fifty-five active ones and the nine market convention priority
 * entries - are transcribed below as ordered literals rather than derived from the compiled
 * data, because every other view of the codes derives from that same data and would agree with
 * it however it had been mistranscribed.
 */
final class CurrencySpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

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
   * The pairing is what is under test, so the code of every row is a literal and is never read
   * off the constant beside it: a `val NZD` bound to the row of `NOK` leaves the size of the
   * family, the currencies on offer and the round trip of every code through the factories
   * exactly as they were, and only an expectation written independently of the declaration
   * catches it.
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

  private val dataMinorUnits: TableFor2[String, Int] = Table(
    ("code", "minorUnitDigits"),
    ("USD", 2),
    ("EUR", 2),
    ("JPY", 0),
    ("GBP", 2),
    ("CHF", 2),
    ("AUD", 2),
    ("CAD", 2))

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
   * Text that names no currency through the exact factory.
   *
   * Each row sits on one side of a different boundary - too short, too long, wrong case, wrong
   * character class, padded - so a single altered character would turn a boundary case into a
   * repeat of another row.
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

  private val dataParseBad: TableFor1[String] = Table(
    "input",
    "",
    "AB",
    "ABCD",
    "123",
    " GBP",
    "GB1")

  /**
   * Amounts and their expected roundings, shared by the `Double` and the [[Decimal]] method.
   *
   * `63.34500001` and `63.34499999` straddle the tie by one part in a hundred million and round
   * in opposite directions; the yen rows exercise the same rule at a minor unit of zero digits.
   */
  private val dataRoundMinorUnits: TableFor3[Currency, Double, Double] = Table(
    ("currency", "amount", "expected"),
    (Currency.USD, 63.347d, 63.35d),
    (Currency.USD, 63.34500001d, 63.35d),
    (Currency.USD, 63.34499999d, 63.34d),
    (Currency.JPY, 63.347d, 63.0d),
    (Currency.JPY, 63.5347d, 64.0d))

  /**
   * The same rounding rows with their expectations as text, for the arbitrary-precision method.
   *
   * The scale of the result is part of what that method promises - a dollar amount rounds to two
   * decimal places and keeps them, a yen amount to none - and only an expectation built from
   * text states a scale exactly: `63` and `64` carry no decimal places, `63.35` and `63.34` two.
   */
  private val dataRoundMinorUnitsText: TableFor3[Currency, Double, String] = Table(
    ("currency", "amount", "expected"),
    (Currency.USD, 63.347d, "63.35"),
    (Currency.USD, 63.34500001d, "63.35"),
    (Currency.USD, 63.34499999d, "63.34"),
    (Currency.JPY, 63.347d, "63"),
    (Currency.JPY, 63.5347d, "64"))

  /**
   * The seventy-four currency codes of the reference data, in the order [[CurrencyData]]
   * declares them: the active currencies alphabetically, then the unapplicable currency and the
   * four metals, then the currencies replaced by the euro.
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
   * The nineteen codes marked historic, in declared order: the currencies replaced by the euro.
   *
   * The split is a datum in its own right - the code list above does not carry it - and it is
   * what decides which currencies `getAvailableCurrencies` offers and which merely resolve.
   */
  private val expectedHistoricCodes: Vector[String] = Vector(
    "ATS", "BEF", "CYP", "DEM", "EEK", "ESP", "FIM", "FRF", "GRD", "IEP",
    "ITL", "LTL", "LUF", "LVL", "MTL", "NLG", "PTE", "SIT", "SKK")

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
   * The ordering itself is the datum - it decides the base currency of a pair that carries no
   * configured convention - so it is asserted in order rather than as a set.
   */
  private val expectedMarketConventionPriority: Vector[String] =
    Vector("XAU", "EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "JPY")

  /**
   * Resolves a currency code, failing the test when the family holds no currency for it.
   *
   * Every code passed here is one the family holds; the tests that assert a code is rejected use
   * the factory directly, so that the failure is the thing being asserted.
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

  /**
   * Reads the message of an outcome that is expected to have failed.
   *
   * A successful outcome is reported as a failed assertion naming the currency it found, so a
   * test asserting a message cannot pass by accident on an outcome that had none.
   *
   * @param outcome  the outcome expected to carry a failure
   * @return the message of that failure
   */
  private def messageOf(outcome: FailureOr[Currency]): String =
    outcome.fold(
      failure => failure.message,
      currency => fail(s"Expected a failure but found the currency '$currency'"))

  //-------------------------------------------------------------------------
  test("test_constants") {
    forAll(dataConstants) { (code: String, currency: Currency) =>
      withClue(s"$code: ") {
        Currency.of(code) should haveValue(currency)
        // a constant and the result of resolving its code are the same object: the constants
        // select from the single set of instances the companion builds
        (currencyOf(code) eq currency) shouldBe true
      }
    }

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
    // the currencies on offer are the fifty-five in active use; the nineteen historic ones stay
    // resolvable but are not offered
    available.size shouldBe 55
    available.size shouldBe CurrencyData.nonHistoricCodes.size
  }

  //-------------------------------------------------------------------------
  test("test_of_String") {
    val test: Currency = currencyOf("SEK")
    test.code shouldBe "SEK"
    test.name shouldBe "SEK"
    test shouldBe Currency.SEK
    (test eq currencyOf("SEK")) shouldBe true
  }

  test("test_of_String_historicCurrency") {
    val test: Currency = currencyOf("BEF")
    test.code shouldBe "BEF"
    test.minorUnitDigits shouldBe 2
    test.triangulationCurrency shouldBe Currency.EUR
    (test eq currencyOf("BEF")) shouldBe true
    // BEF is one of the currencies replaced by the euro: it resolves with its own minor units
    // and triangulation currency, but it is not one of the currencies offered to choose from
    Currency.getAvailableCurrencies should not contain (test)
  }

  test("test_of_String_unknownCurrencyCreated") {
    val outcome: FailureOr[Currency] = Currency.of("AAA")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(".*AAA.*")
    // a rejected code mints nothing and caches nothing, so it is still unknown on a second ask
    Currency.valueOf("AAA") shouldBe None
    Currency.of("AAA") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_String_lowerCase") {
    // `of` matches the code exactly; `parse` is the case-tolerant route
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
   * `parse` folds the text to upper case before the lookup, so an unknown code fails naming the
   * folded code `ZYX` rather than the text as given.
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
    // the fold is over the whole of the text, so mixed case resolves as well; case is the only
    // leniency this family offers, as it declares no alternate spelling and no lenient pattern
    (currencyParsed("GbP") eq Currency.GBP) shouldBe true
  }

  test("test_parse_String_bad") {
    forAll(dataParseBad) { (input: String) =>
      withClue(s"'$input': ") {
        Currency.parse(input) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  /**
   * A rejected code is named in full in the failure, while the rendering of that failure is
   * bounded and escaped, so no code can make a rendering unbounded or make it carry a line
   * break.
   */
  test("both factories name a rejected code in full, and their failures render bounded and on one line") {
    val payload = "H" * 10000
    val bounded: FailureOr[Currency] = Currency.of(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    messageOf(bounded) shouldBe s"Currency name not found: $payload"
    // `parse` folds the text and then resolves it exactly as `of` does, and the fold of this
    // payload is the payload, so both factories name it identically
    messageOf(Currency.parse(payload)) shouldBe messageOf(bounded)
    val rendered = Show[Failure].show(bounded.left.toOption.getOrElse(fail("expected a failure")))
    rendered.length should be < 1000
    rendered should startWith("PARSING: Currency name not found: HHH")
    rendered should endWith("...")

    // a code holding a line break is named as it stands but rendered on one line, so a
    // line-oriented consumer cannot be made to record a line the library did not report
    val injected = Currency.of("EUR\nUSD")
    messageOf(injected) shouldBe "Currency name not found: EUR\nUSD"
    val injectedRendering =
      Show[Failure].show(injected.left.toOption.getOrElse(fail("expected a failure")))
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe "PARSING: Currency name not found: EUR\\nUSD"

    messageOf(Currency.of("AAA")) shouldBe "Currency name not found: AAA"
    messageOf(Currency.parse("zyx")) shouldBe "Currency name not found: ZYX"
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
        // the amount comes from a `Double`, so what is rounded is the full binary expansion of
        // that double and not the short decimal its text names; the expectation comes from text
        // because only text states a scale, and equality here is sensitive to scale
        val rounded: BigDecimal = currency.roundMinorUnits(new BigDecimal(amount))
        val expectedAmount: BigDecimal = new BigDecimal(expected)
        rounded shouldBe expectedAmount
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
        // both sides hold a value before they are compared, so that a pair of failures cannot
        // pass as a pair of equal outcomes
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
    Order[Currency].compare(a, a) shouldBe 0
    Order[Currency].compare(b, b) shouldBe 0
    Order[Currency].compare(c, c) shouldBe 0

    (Order[Currency].compare(a, b) < 0) shouldBe true
    (Order[Currency].compare(b, a) > 0) shouldBe true

    (Order[Currency].compare(a, c) < 0) shouldBe true
    (Order[Currency].compare(c, a) > 0) shouldBe true

    (Order[Currency].compare(b, c) < 0) shouldBe true
    (Order[Currency].compare(c, b) > 0) shouldBe true

    // the ordering is alphabetical by code, so sorting recovers the order asserted above
    List(c, a, b).sorted(Order[Currency].toOrdering) shouldBe List(a, b, c)
  }

  /**
   * Comparison is the ordering typeclass, whose operation takes two currencies and admits no
   * absent value, so the guard is the compiler's: a comparison against something that is not a
   * currency, or with the second argument missing, does not compile.
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

    // `Eq` and `Hash` are the universal equality and hash of the type, one notion rather than
    // two, so the same matrix holds through the typeclasses
    Eq[Currency].eqv(a1, a2) shouldBe true
    Eq[Currency].eqv(a1, b) shouldBe false
    Eq[Currency].eqv(b, a2) shouldBe false
    Hash[Currency].hash(a1) shouldBe Hash[Currency].hash(a2)
  }

  test("test_equals_bad") {
    val a: Any = Currency.GBP
    val foreign: Any = new Object
    // the reference is typed `Any` so that what is exercised is the universal equality; written
    // against a `Currency` reference these lines would be rejected as comparing unrelated types
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
  test("coverage") {
    val gbp: Currency = Currency.GBP
    val eur: Currency = Currency.EUR
    Eq[Currency].eqv(gbp, gbp) shouldBe true
    Eq[Currency].eqv(gbp, eur) shouldBe false
    Hash[Currency].hash(gbp) shouldBe Hash[Currency].hash(currencyOf("GBP"))
    // the inequality of two currencies is asserted through the hashing instance as an equality;
    // that their hashes differ is not asserted, because hashing promises only that equal values
    // hash equally and two unequal currencies are free to collide
    Hash[Currency].eqv(gbp, eur) shouldBe false
    Show[Currency].show(gbp) shouldBe "GBP"
    Show[Currency].show(eur) shouldBe "EUR"
    (Order[Currency].compare(gbp, eur) > 0) shouldBe true

    CurrencyData.rows.size shouldBe 74
    CurrencyData.nonHistoricCodes.size shouldBe 55
    CurrencyData.historicCodes.size shouldBe 19
    CurrencyData.marketConventionPriority.size shouldBe 9
    Currency.values.size shouldBe CurrencyData.rows.size

    // the three literals are checked against one another before any of them is used as an
    // expectation, so that a code dropped from one cannot pass as agreement with the data
    (expectedActiveCodes ++ expectedHistoricCodes) shouldBe expectedCodes

    // the inventory itself, code for code and in order: a table substituted wholesale, a row
    // moved between the active and the historic part, a pair of codes exchanged or a reordered
    // priority list fails here, and none of that is visible to the sizes above
    CurrencyData.codes shouldBe expectedCodes
    CurrencyData.historicCodes shouldBe expectedHistoricCodes
    CurrencyData.nonHistoricCodes shouldBe expectedActiveCodes
    CurrencyData.marketConventionPriority shouldBe expectedMarketConventionPriority
    Currency.values.toList.map(currency => currency.code) shouldBe expectedCodes.toList
  }

  /**
   * The serialized form of a currency is the bare JSON string of its code, never an object, so
   * any document naming currencies by code reads back here as the same currencies.
   */
  test("test_serialization") {
    val encoded: Json = Encoder[Currency].apply(Currency.GBP)
    encoded shouldBe Json.fromString("GBP")
    encoded.asString shouldBe Some("GBP")

    val decoded: Either[DecodingFailure, Currency] = Decoder[Currency].decodeJson(encoded)
    decoded shouldBe Right(Currency.GBP)

    // the family stays closed through the decoder: a string naming no currency of it is a
    // decoding failure, not a currency the factory could not have produced
    val rejected: Either[DecodingFailure, Currency] =
      Decoder[Currency].decodeJson(Json.fromString("AAA"))
    rejected.isLeft shouldBe true
  }

  /**
   * The three letter code is the textual identity of a currency: `name`, the rendering instance
   * and `toString` all produce it, and parsing it back yields that same currency.
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
   * The currency table, the currency pair table and the currencies on offer describe the same
   * set of currencies, once the codes beginning with `X` are filtered out: those are the metals
   * and the unapplicable currency, which are held as data and traded as pairs but belong to no
   * country.
   */
  test("test_consistency") {
    // every active row resolves, so this is the whole active subset and not a subset of it that
    // an unresolvable row would silently shrink
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

    // the size is asserted so that the two equalities above cannot be satisfied by empty sets
    dataCurrencies.size shouldBe 50
  }
}
