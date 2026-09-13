/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.Inspectors
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.BHD
import com.opengamma.strata.basics.currency.Currency.BRL
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.CHF
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.JPY
import com.opengamma.strata.basics.currency.Currency.NOK
import com.opengamma.strata.basics.currency.Currency.NZD
import com.opengamma.strata.basics.currency.Currency.SEK
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.basics.currency.Currency.XAU
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[CurrencyPair]].
 *
 * The conventional direction of a pair is decided by the configured pairs first, then by the
 * market convention priority list, then by lexicographic order of the two codes; `isConventional`
 * and `toConventional` both rest on that procedure, and `test_isConventional_Consistency` asserts
 * the guarantee it needs - exactly one direction of any two currencies is conventional - over
 * every unordered pair rather than a sample. `test_cross_CurrencyPair` pins the order in which
 * the four cross cases are tried: two pairs can share both a base and a counter, so more than
 * one case can apply, and the case that wins decides the base of the cross and therefore which
 * of two rates a caller inverts.
 */
final class CurrencyPairSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * A value of an unrelated type, typed as `Any` because `==` between a pair and a string is a
   * warning and this build treats warnings as errors; `equals` on an `Any` states the same
   * rejection without asking the compiler to approve the comparison.
   */
  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  private val gbpGbp: CurrencyPair = CurrencyPair.of(GBP, GBP)
  private val gbpUsd: CurrencyPair = CurrencyPair.of(GBP, USD)
  private val usdGbp: CurrencyPair = CurrencyPair.of(USD, GBP)
  private val eurGbp: CurrencyPair = CurrencyPair.of(EUR, GBP)
  private val eurUsd: CurrencyPair = CurrencyPair.of(EUR, USD)
  private val usdEur: CurrencyPair = CurrencyPair.of(USD, EUR)

  //-------------------------------------------------------------------------
  /** The `cAd/GbP` row pins that parsing folds its input rather than requiring upper case. */
  private val data_parseGood: TableFor3[String, Currency, Currency] = Table(
    ("input", "base", "counter"),
    ("USD/EUR", USD, EUR),
    ("EUR/USD", EUR, USD),
    ("EUR/EUR", EUR, EUR),
    ("cAd/GbP", CAD, GBP))

  /** Rejected text; the whole input has to match, so `AUD/GBP/EUR` is rejected as well. */
  private val data_parseBad: TableFor1[String] = Table(
    "input",
    "AUD",
    "AUD/GB",
    "AUD GBP",
    "AUD:GBP",
    "123/456",
    "",
    "AUD/GBP/EUR")

  private val data_isInverse: TableFor2[CurrencyPair, Boolean] = Table(
    ("other", "expected"),
    (gbpUsd, false),
    (CurrencyPair.of(GBP, USD), false),
    (CurrencyPair.of(USD, GBP), true),
    (CurrencyPair.of(GBP, EUR), false),
    (CurrencyPair.of(EUR, GBP), false),
    (CurrencyPair.of(USD, EUR), false),
    (CurrencyPair.of(EUR, USD), false))

  /**
   * The eight crossing rows all produce `EUR/GBP` and never `GBP/EUR`, because a cross is
   * returned in market convention order, and between them they cover every arrangement of the
   * shared currency across the two pairs.
   */
  private val data_cross: TableFor3[CurrencyPair, CurrencyPair, Option[CurrencyPair]] = Table(
    ("pair", "other", "expected"),
    (gbpUsd, gbpUsd, Option.empty[CurrencyPair]),
    (gbpUsd, usdGbp, Option.empty[CurrencyPair]),
    (gbpGbp, gbpUsd, Option.empty[CurrencyPair]),
    (gbpUsd, gbpGbp, Option.empty[CurrencyPair]),
    (gbpUsd, usdEur, Some(eurGbp)),
    (gbpUsd, eurUsd, Some(eurGbp)),
    (usdGbp, usdEur, Some(eurGbp)),
    (usdGbp, eurUsd, Some(eurGbp)),
    (usdEur, gbpUsd, Some(eurGbp)),
    (usdEur, usdGbp, Some(eurGbp)),
    (eurUsd, gbpUsd, Some(eurGbp)),
    (eurUsd, usdGbp, Some(eurGbp)))

  /**
   * The reason column names the step of the decision procedure that governs each row. `XAU/EUR`
   * is the one row that pins the head of the priority ordering, and `GBP/GBP` the one that pins
   * the final comparison being non-strict.
   */
  private val data_isConventional: TableFor3[CurrencyPair, Boolean, String] = Table(
    ("pair", "expected", "reason"),
    (gbpUsd, true, "configured pair"),
    (usdGbp, false, "inverse of the configured GBP/USD"),
    (CurrencyPair.of(GBP, BRL), true, "unconfigured; GBP is in the ordering, BRL is not"),
    (CurrencyPair.of(BRL, GBP), false, "unconfigured; GBP outranks BRL, so BRL cannot be the base"),
    (CurrencyPair.of(BHD, BRL), true, "unconfigured, neither listed; BHD < BRL"),
    (CurrencyPair.of(BRL, BHD), false, "unconfigured, neither listed; BRL > BHD"),
    (gbpGbp, true, "identity pair; the final comparison is not strict"),
    (eurUsd, true, "configured pair"),
    (usdEur, false, "inverse of the configured EUR/USD"),
    (CurrencyPair.of(XAU, EUR), true, "unconfigured; XAU heads the ordering"),
    (CurrencyPair.of(EUR, XAU), false, "unconfigured; XAU outranks EUR, so EUR cannot be the base"),
    (CurrencyPair.of(NZD, CAD), true, "configured pair, and NZD also outranks CAD in the ordering"),
    (CurrencyPair.of(CAD, NZD), false, "inverse of the configured NZD/CAD"),
    (CurrencyPair.of(CHF, JPY), true, "configured pair, and CHF also outranks JPY in the ordering"),
    (CurrencyPair.of(JPY, CHF), false, "inverse of the configured CHF/JPY"),
    (CurrencyPair.of(NOK, SEK), true, "configured pair; neither currency is in the ordering"),
    (CurrencyPair.of(SEK, NOK), false, "inverse of the configured NOK/SEK; SEK > NOK too"))

  private val data_toConventional: TableFor2[CurrencyPair, CurrencyPair] = Table(
    ("pair", "expected"),
    (gbpUsd, gbpUsd),
    (usdGbp, gbpUsd),
    (CurrencyPair.of(GBP, BRL), CurrencyPair.of(GBP, BRL)),
    (CurrencyPair.of(BRL, GBP), CurrencyPair.of(GBP, BRL)),
    (CurrencyPair.of(BHD, BRL), CurrencyPair.of(BHD, BRL)),
    (CurrencyPair.of(BRL, BHD), CurrencyPair.of(BHD, BRL)))

  /**
   * An unconfigured pair falls back to the sum of the minor unit digits of its two currencies,
   * which the last two rows make visible: `BHD` has three where the others have two, so the
   * answer is five rather than four.
   */
  private val data_rateDigits: TableFor2[CurrencyPair, Int] = Table(
    ("pair", "expected"),
    (gbpUsd, 4),
    (usdGbp, 4),
    (CurrencyPair.of(BRL, GBP), 4),
    (CurrencyPair.of(GBP, BRL), 4),
    (CurrencyPair.of(BRL, BHD), 5),
    (CurrencyPair.of(BHD, BRL), 5))

  //-------------------------------------------------------------------------
  test("test_getAvailable") {
    val available = CurrencyPair.getAvailablePairs
    available.contains(CurrencyPair.of(EUR, USD)) shouldBe true
    available.contains(CurrencyPair.of(EUR, GBP)) shouldBe true
    available.contains(CurrencyPair.of(GBP, USD)) shouldBe true

    available.size shouldBe 92

    // the set holds one direction of each configured pair and never both, which is what makes
    // the second step of `isConventional` decidable
    available.filter(pair => available.contains(pair.inverse)) shouldBe empty
  }

  //-------------------------------------------------------------------------
  test("test_of_CurrencyCurrency") {
    val test = CurrencyPair.of(GBP, USD)
    test.base shouldBe GBP
    test.counter shouldBe USD
    test.isIdentity shouldBe false
    test.toSet shouldBe Set(GBP, USD)
    test.toString shouldBe "GBP/USD"
  }

  test("test_of_CurrencyCurrency_reverseStandardOrder") {
    val test = CurrencyPair.of(USD, GBP)
    test.base shouldBe USD
    test.counter shouldBe GBP
    test.isIdentity shouldBe false
    test.toSet shouldBe Set(GBP, USD)
    test.toString shouldBe "USD/GBP"

    // `toSet` returns an insertion-ordered set, and its order - the conventional base first,
    // whichever way round the pair is written - is part of the contract
    test.toSet.toList shouldBe List(GBP, USD)
    CurrencyPair.of(GBP, USD).toSet.toList shouldBe List(GBP, USD)
  }

  test("test_of_CurrencyCurrency_same") {
    // a pair of one currency with itself is legal, which is why the factory is total and
    // returns a pair rather than an outcome
    val test = CurrencyPair.of(USD, USD)
    test.base shouldBe USD
    test.counter shouldBe USD
    test.isIdentity shouldBe true
    test.toString shouldBe "USD/USD"
  }

  test("test_of_CurrencyCurrency_null") {
    assertDoesNotCompile("""CurrencyPair.of(USD)""")
    assertDoesNotCompile("""CurrencyPair.of("GBP", USD)""")
    assertDoesNotCompile("""CurrencyPair.of(GBP, "USD")""")
    assertCompiles("""CurrencyPair.of(GBP, USD)""")
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_good") {
    forAll(data_parseGood) { (input: String, base: Currency, counter: Currency) =>
      CurrencyPair.parse(input) should haveValue(CurrencyPair.of(base, counter))
    }
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String) =>
      CurrencyPair.parse(input) should beFailureWith(FailureReason.PARSING)
    }
  }

  /**
   * Rejected input text is named in full in the failure message, and quoted bounded and on one
   * line when that failure is rendered, so oversized or multi-line input cannot be echoed into a
   * log unbounded or be made to forge a line of it.
   */
  test("parsing names rejected text in full, and the failure renders bounded and on one line") {
    val payload = "H" * 10000
    val bounded = CurrencyPair.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    failure.message shouldBe s"Invalid currency pair: $payload"
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: Invalid currency pair: HHH")
    rendered should endWith("...")

    val injected = CurrencyPair.parse("EUR\nUSD")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedFailure = injected.left.toOption.getOrElse(fail("expected a failure"))
    injectedFailure.message shouldBe "Invalid currency pair: EUR\nUSD"
    val injectedRendering = Show[Failure].show(injectedFailure)
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe "PARSING: Invalid currency pair: EUR\\nUSD"

    // the bound applies to the rendering only: the message for ordinary rejected text is
    // unchanged, character for character
    CurrencyPair.parse("AUD:GBP").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid currency pair: AUD:GBP")
  }

  //-------------------------------------------------------------------------
  test("test_inverse") {
    val test = CurrencyPair.of(GBP, USD)
    test.inverse shouldBe CurrencyPair.of(USD, GBP)
  }

  test("test_inverse_same") {
    val test = CurrencyPair.of(GBP, GBP)
    test.inverse shouldBe CurrencyPair.of(GBP, GBP)
  }

  //-------------------------------------------------------------------------
  test("test_contains_Currency") {
    val test = CurrencyPair.of(GBP, USD)
    test.contains(GBP) shouldBe true
    test.contains(USD) shouldBe true
    test.contains(EUR) shouldBe false
  }

  test("test_contains_Currency_same") {
    val test = CurrencyPair.of(GBP, GBP)
    test.contains(GBP) shouldBe true
    test.contains(USD) shouldBe false
    test.contains(EUR) shouldBe false
  }

  test("test_contains_Currency_null") {
    assertDoesNotCompile("""CurrencyPair.of(GBP, USD).contains("GBP")""")
    assertCompiles("""CurrencyPair.of(GBP, USD).contains(GBP)""")
  }

  //-------------------------------------------------------------------------
  test("test_other_Currency") {
    val test = CurrencyPair.of(GBP, USD)
    test.other(GBP) should haveValue(USD)
    test.other(USD) should haveValue(GBP)

    // a currency the pair does not hold arrives from the same document or market data as the
    // pair, so `other` reports it as data rather than as a coding error, and the wording is
    // pinned because a user may already be reading it in a log
    test.other(EUR) should beFailureWith(FailureReason.INVALID)
    test.other(EUR) should haveFailureMessageMatching(
      "Unable to find other currency, EUR is not present in GBP/USD")
  }

  test("test_other_Currency_same") {
    val test = CurrencyPair.of(GBP, GBP)
    test.other(GBP) should haveValue(GBP)
    test.other(EUR) should beFailureWith(FailureReason.INVALID)
  }

  test("test_other_Currency_null") {
    assertDoesNotCompile("""CurrencyPair.of(GBP, USD).other("EUR")""")
    assertCompiles("""CurrencyPair.of(GBP, USD).other(EUR)""")
  }

  //-------------------------------------------------------------------------
  test("test_isInverse_CurrencyPair") {
    val test = CurrencyPair.of(GBP, USD)
    forAll(data_isInverse) { (other: CurrencyPair, expected: Boolean) =>
      test.isInverse(other) shouldBe expected
    }
  }

  test("test_isInverse_CurrencyPair_null") {
    assertDoesNotCompile("""CurrencyPair.of(GBP, USD).isInverse(USD)""")
    assertCompiles("""CurrencyPair.of(GBP, USD).isInverse(CurrencyPair.of(USD, GBP))""")
  }

  //-------------------------------------------------------------------------
  test("test_cross_CurrencyPair") {
    forAll(data_cross) {
      (pair: CurrencyPair, other: CurrencyPair, expected: Option[CurrencyPair]) =>
        pair.cross(other) shouldBe expected
    }
  }

  test("test_cross_CurrencyPair_null") {
    assertDoesNotCompile("""CurrencyPair.of(GBP, USD).cross(USD)""")
    assertCompiles("""CurrencyPair.of(GBP, USD).cross(CurrencyPair.of(USD, EUR))""")
  }

  //-----------------------------------------------------------------------
  test("test_isConventional") {
    forAll(data_isConventional) { (pair: CurrencyPair, expected: Boolean, reason: String) =>
      withClue(s"$pair, $reason: ") {
        pair.isConventional shouldBe expected
      }
    }
  }

  test("test_isConventional_Consistency") {
    // Exactly one of the two directions of any two currencies is conventional, which is the
    // guarantee `toConventional` rests on; it is asserted for every unordered pair of the
    // available currencies rather than sampled.
    val allCurrencies: Vector[Currency] = Currency.getAvailableCurrencies.toVector
    allCurrencies.size shouldBe 55

    Inspectors.forAll(allCurrencies.indices.toVector) { i =>
      Inspectors.forAll(((i + 1) until allCurrencies.size).toVector) { j =>
        val pair = CurrencyPair.of(allCurrencies(i), allCurrencies(j))
        val inversePair = CurrencyPair.of(allCurrencies(j), allCurrencies(i))
        withClue(s"$pair against $inversePair: ") {
          pair.isConventional shouldBe !inversePair.isConventional
        }
      }
    }
  }

  test("test_toConventional") {
    forAll(data_toConventional) { (pair: CurrencyPair, expected: CurrencyPair) =>
      pair.toConventional shouldBe expected
      pair.toConventional.toConventional shouldBe expected
    }
  }

  test("test_rateDigits") {
    forAll(data_rateDigits) { (pair: CurrencyPair, expected: Int) =>
      pair.getRateDigits shouldBe expected
      pair.inverse.getRateDigits shouldBe expected
    }
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = CurrencyPair.of(AUD, GBP)
    val a2 = CurrencyPair.of(AUD, GBP)
    val b = CurrencyPair.of(USD, GBP)
    val c = CurrencyPair.of(USD, EUR)

    // the matrix is stated through `equals` itself, because that is the method the `Hash`
    // instance below must agree with
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

    // the companion publishes one equality-bearing instance, an `Order` that is also a `Hash`,
    // so there is no second notion of equality that could disagree with the above
    Order[CurrencyPair].eqv(a1, a2) shouldBe true
    Order[CurrencyPair].eqv(a1, b) shouldBe false
    Hash[CurrencyPair].hash(a1) shouldBe Hash[CurrencyPair].hash(a2)

    // and that ordering is base first, then counter, each by its code, and agrees with the
    // equality: it compares zero exactly when the two pairs are equal
    Order[CurrencyPair].compare(a1, a2) shouldBe 0
    Order[CurrencyPair].compare(a1, b) should be < 0
    Order[CurrencyPair].compare(b, a1) should be > 0
    Order[CurrencyPair].compare(b, c) should be > 0
  }

  test("test_equals_bad") {
    val test = CurrencyPair.of(AUD, GBP)

    test.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(test) shouldBe false
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // the codec writes the text form as a bare string rather than an object of two codes
    CurrencyPair.of(GBP, USD).asJson shouldBe Json.fromString("GBP/USD")
    Json.fromString("GBP/USD").as[CurrencyPair] shouldBe Right(CurrencyPair.of(GBP, USD))

    CurrencyPair.of(GBP, GBP).asJson shouldBe Json.fromString("GBP/GBP")
    Json.fromString("GBP/GBP").as[CurrencyPair] shouldBe Right(CurrencyPair.of(GBP, GBP))

    // decoding goes through the same parsing the text form uses
    Json.fromString("GBP-USD").as[CurrencyPair].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // the rendered text is the identity of a pair - the text in documents, messages and
    // expectations - and the parsing factory reads it back
    Show[CurrencyPair].show(CurrencyPair.of(GBP, USD)) shouldBe "GBP/USD"
    Show[CurrencyPair].show(CurrencyPair.of(GBP, GBP)) shouldBe "GBP/GBP"

    CurrencyPair.parse(Show[CurrencyPair].show(CurrencyPair.of(GBP, USD))) should
      haveValue(CurrencyPair.of(GBP, USD))
    CurrencyPair.parse(Show[CurrencyPair].show(CurrencyPair.of(GBP, GBP))) should
      haveValue(CurrencyPair.of(GBP, GBP))

    // the rendered form and `toString` are the same text, so a pair reaching a message through
    // either route reads the same
    Show[CurrencyPair].show(CurrencyPair.of(GBP, USD)) shouldBe CurrencyPair.of(GBP, USD).toString
  }
}
