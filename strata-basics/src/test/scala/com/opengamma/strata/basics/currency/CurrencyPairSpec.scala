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
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[CurrencyPair]], ported from the Java `CurrencyPairTest`.
 *
 * Each of the twenty-seven annotated methods of the original is present below under its own
 * name, and the two that were driven by a data provider are one test each with their table
 * inside, so the method-level traceability of the migration is exact: one test here for one
 * test method there.
 *
 * ===What this suite owns===
 *
 * Two of its tests are load-bearing for the rest of the module rather than routine.
 *
 *   - `test_isConventional` and `test_isConventional_Consistency` are the only place the
 *     market convention priority ''ordering'' is observable as behaviour. Nothing else reads
 *     the ordering directly, so a transcription error in it - a currency moved, dropped or
 *     duplicated - surfaces here or nowhere.
 *   - `test_cross_CurrencyPair` pins the ''order'' in which the four cross cases are tried.
 *     Two pairs can share both a base and a counter, so more than one of the four cases can
 *     apply at once, and which currency ends up the base of the cross decides which of two
 *     rates a caller inverts. Cross-rate assertions elsewhere in the module rest on the
 *     twelve rows below.
 *
 * ===How the shape of the port changes the assertions===
 *
 * Three differences from the original are structural, and each is explained again at the test
 * it affects:
 *
 *   - `parse` and `other` report a failure rather than throwing. The original asserted an
 *     IllegalArgumentException at ten sites; seven of them passed an absent reference and are
 *     covered by the point below, and the other three - the row of rejected text, and the two
 *     calls asking for the other currency of a pair that does not hold it - are assertions
 *     here that the outcome is a failure carrying the expected reason. No test here asserts a
 *     thrown exception, because no method of this type throws.
 *   - No reference in this API can be absent, so the five tests that passed Java's
 *     absent-reference literal keep their names and assert what replaced that contract:
 *     the compiler rejects the call. An argument is either supplied at its declared type or
 *     the code does not compile, so the guard the original tested at run time is tested here
 *     at compile time by `assertDoesNotCompile`, paired with `assertCompiles` on the valid
 *     call so that a proof cannot pass by naming something that would not compile either way.
 *   - Java serialization and annotation-driven string conversion have no counterpart. Their
 *     two tests assert what those mechanisms stood for: that a pair survives a round trip
 *     through its JSON form, and that its text form is the identity a reader and a writer
 *     agree on.
 *
 * @see [[CurrencyPairData]] for the configured pairs the convention tests read
 * @see [[CurrencyData]] for the market convention priority ordering
 */
final class CurrencyPairSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * A value of an unrelated type, for the equality contract of `test_equals_bad`.
   *
   * This is the `ANOTHER_TYPE` constant of the Java test, and it is typed as `Any` for the
   * reason the Java test needed no such care: comparing a pair with a string through `==` is
   * a warning in this build and a warning is an error here. Typing the value as `Any` and
   * calling `equals` asserts what the original asserted - that the equality of a pair rejects
   * a value of another type - without asking the compiler to approve the comparison.
   */
  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  /** The identity pair `GBP/GBP`, as in the Java `test_cross_CurrencyPair`. */
  private val gbpGbp: CurrencyPair = CurrencyPair.of(GBP, GBP)

  /** The conventional pair `GBP/USD`. */
  private val gbpUsd: CurrencyPair = CurrencyPair.of(GBP, USD)

  /** The inverse of [[gbpUsd]]. */
  private val usdGbp: CurrencyPair = CurrencyPair.of(USD, GBP)

  /** The conventional pair `EUR/GBP`, which is the cross every crossing row produces. */
  private val eurGbp: CurrencyPair = CurrencyPair.of(EUR, GBP)

  /** The conventional pair `EUR/USD`. */
  private val eurUsd: CurrencyPair = CurrencyPair.of(EUR, USD)

  /** The inverse of [[eurUsd]]. */
  private val usdEur: CurrencyPair = CurrencyPair.of(USD, EUR)

  //-------------------------------------------------------------------------
  /**
   * Text that names a pair, with the two currencies it names, transcribed from the Java
   * provider.
   *
   * The four rows are the whole of the parsing contract the original stated: a pair either
   * way round, an identity pair, and - the row that is easy to mistake for a typing error -
   * `cAd/GbP`, whose mixed case pins that parsing folds its input rather than requiring the
   * upper case form.
   */
  private val data_parseGood: TableFor3[String, Currency, Currency] = Table(
    ("input", "base", "counter"),
    ("USD/EUR", USD, EUR),
    ("EUR/USD", EUR, USD),
    ("EUR/EUR", EUR, EUR),
    ("cAd/GbP", CAD, GBP))

  /**
   * Text that names no pair, transcribed from the Java provider.
   *
   * Six of the seven rows of the original are here unchanged: a single code, a counter code
   * of two letters, a space and a colon in place of the separator, three digit "codes", and
   * text holding nothing at all. The seventh row of the original was Java's absent-reference
   * literal, which cannot be written against this API - the parameter is a `String` a caller
   * supplies, and this port has no such literal to supply - so it is replaced by a further
   * input that is rejected for the same reason the others are: text that holds more than one
   * separator, which the whole text has to match and does not.
   */
  private val data_parseBad: TableFor1[String] = Table(
    "input",
    "AUD",
    "AUD/GB",
    "AUD GBP",
    "AUD:GBP",
    "123/456",
    "",
    "AUD/GBP/EUR")

  /**
   * Pairs compared against `GBP/USD` for inversion, transcribed from the Java test.
   *
   * All seven rows of the original are kept, and the point of the table is the six that
   * answer false: a pair is the inverse of `GBP/USD` only when it holds those same two
   * currencies the other way round, so neither an equal pair nor a pair sharing one currency
   * qualifies.
   */
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
   * Pairs to cross and the cross they produce, transcribed from the Java test.
   *
   * The twelve rows are the contract of `cross` in full, and they are grouped as the original
   * grouped them. The four that produce nothing are the four reasons there can be no cross:
   * a pair crossed with itself, a pair crossed with its own inverse, and an identity pair on
   * either side. The eight that produce one all produce `EUR/GBP` - not `GBP/EUR` - because
   * a cross is returned in market convention order, and they cover every arrangement of the
   * shared currency: shared as counter and base, as counter and counter, as base and base,
   * and as base and counter.
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
   * Pairs and whether they follow the market convention, with the step that decides each.
   *
   * The first seven rows are those of the Java test, carrying its two comments as the reason
   * column. The remaining ten are the cases the migration plan adds, and they were each
   * worked out from the decision procedure against the reference data rather than assumed:
   *
   *   - `EUR/USD`, `NZD/CAD`, `CHF/JPY` and `NOK/SEK` are configured pairs, so the first step
   *     decides them and their inverses. `NZD/CAD` and `CHF/JPY` are worth stating because
   *     the priority ordering would answer the same way and the configuration is what
   *     actually answers; `SEK/NOK` is worth stating because the ordering would ''not'' reach
   *     it at all - neither Scandinavian currency is listed - yet it is still decided by
   *     configuration, through the second step, since `NOK/SEK` is a configured pair. The
   *     lexicographic fall back would answer false for `SEK/NOK` as well, so the row asserts
   *     the same value either way; the reason column records which step the data makes
   *     governing.
   *   - `XAU/EUR` is not configured in either direction, so the ordering decides it, and it
   *     is the pair that pins the ''head'' of that ordering: gold is listed before the euro,
   *     which is the only place in this suite where the first entry of the ordering is
   *     observable.
   *   - `GBP/GBP` is the identity pair, decided by the final comparison being non-strict.
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

  /**
   * Pairs and the conventional pair for their two currencies, transcribed from the Java test.
   *
   * The six rows are three pairs of rows, each asserting that a pair and its inverse agree on
   * the conventional direction, and the three cases are the three steps that can decide it:
   * configuration, the priority ordering, and the lexicographic fall back.
   */
  private val data_toConventional: TableFor2[CurrencyPair, CurrencyPair] = Table(
    ("pair", "expected"),
    (gbpUsd, gbpUsd),
    (usdGbp, gbpUsd),
    (CurrencyPair.of(GBP, BRL), CurrencyPair.of(GBP, BRL)),
    (CurrencyPair.of(BRL, GBP), CurrencyPair.of(GBP, BRL)),
    (CurrencyPair.of(BHD, BRL), CurrencyPair.of(BHD, BRL)),
    (CurrencyPair.of(BRL, BHD), CurrencyPair.of(BHD, BRL)))

  /**
   * Pairs and the number of digits of a market quote for them, transcribed from the Java test.
   *
   * The first four rows read the reference data, directly for `GBP/USD` and through the
   * inverse for `USD/GBP`, and then reach the fall back for the two directions of `GBP/BRL`,
   * which is unconfigured and whose two currencies have two minor unit digits each. The last
   * two rows are the ones that make the fall back visible as a ''sum'': the Bahraini dinar
   * has three minor unit digits where every other currency of this table has two, so the
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

    // the set is closed and compiled rather than loaded, so its size is a fact this suite can
    // state: the reference data describes ninety-two conventional pairs. What each of them
    // holds is asserted by the module's reference data manifest spec, not here.
    available.size shouldBe 92

    // the set holds one direction of each configured pair and never both. That asymmetry is
    // what makes the second step of `isConventional` decidable, so it is asserted where the
    // set itself is under test rather than left implied by the rows of the data.
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

    // the original asserted the contents of the set and said nothing about its order. This
    // port documents an order - the conventional base first, whichever way round the pair is
    // written - and returns an insertion-ordered set to carry it, so the order is asserted
    // here, on the pair that is written the other way round and therefore the only one where
    // the two could differ.
    test.toSet.toList shouldBe List(GBP, USD)
    CurrencyPair.of(GBP, USD).toSet.toList shouldBe List(GBP, USD)
  }

  test("test_of_CurrencyCurrency_same") {
    // a pair of one currency with itself is legal: there is nothing about two currencies to
    // reject, which is why the factory is total and returns a pair rather than an outcome
    val test = CurrencyPair.of(USD, USD)
    test.base shouldBe USD
    test.counter shouldBe USD
    test.isIdentity shouldBe true
    test.toString shouldBe "USD/USD"
  }

  test("test_of_CurrencyCurrency_null") {
    // The Java test passed the absent-reference literal for each argument in turn and
    // asserted an IllegalArgumentException. That case cannot be written against this API and
    // needs no run-time guard: `of` takes two required `Currency` values, the `notNull`
    // family of checks was dropped in the port because an absent value is modelled by
    // `Option` rather than by an absent reference, and neither argument can be omitted or
    // supplied as anything else. What replaced the run-time check is therefore asserted at
    // compile time - an argument left out, and an argument supplied as the text of a currency
    // code, are both rejected before the program runs - and the valid call is asserted to
    // compile so that the two proofs cannot be passing for some unrelated reason.
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
      // text that names no pair is reported as a parsing failure rather than by throwing,
      // and the reason is asserted by value so that a failure of some other kind - a
      // currency code that names no currency, say - would not satisfy this row
      CurrencyPair.parse(input) should beFailureWith(FailureReason.PARSING)
    }
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
    // an identity pair holds one currency, so it contains that one and nothing else
    val test = CurrencyPair.of(GBP, GBP)
    test.contains(GBP) shouldBe true
    test.contains(USD) shouldBe false
    test.contains(EUR) shouldBe false
  }

  test("test_contains_Currency_null") {
    // As in `test_of_CurrencyCurrency_null`: an absent reference is not expressible here, and
    // the check that guarded it at run time is now the type of the parameter. The nearest
    // thing a caller could supply instead - the text of a currency code - does not compile,
    // and the currency itself does.
    assertDoesNotCompile("""CurrencyPair.of(GBP, USD).contains("GBP")""")
    assertCompiles("""CurrencyPair.of(GBP, USD).contains(GBP)""")
  }

  //-------------------------------------------------------------------------
  test("test_other_Currency") {
    val test = CurrencyPair.of(GBP, USD)
    test.other(GBP) should haveValue(USD)
    test.other(USD) should haveValue(GBP)

    // A currency the pair does not hold is data rather than a coding error - it arrives from
    // the same document or market data as the pair - so it is reported as a failure instead
    // of throwing. The reason is asserted by value, and the message is asserted to be the
    // wording of the original, which a user may already be reading in a log.
    test.other(EUR) should beFailureWith(FailureReason.INVALID)
    test.other(EUR) should haveFailureMessageMatching(
      "Unable to find other currency, EUR is not present in GBP/USD")
  }

  test("test_other_Currency_same") {
    // for an identity pair the other currency is that same currency, which is what asking
    // for "the other one" means when both are the same
    val test = CurrencyPair.of(GBP, GBP)
    test.other(GBP) should haveValue(GBP)
    test.other(EUR) should beFailureWith(FailureReason.INVALID)
  }

  test("test_other_Currency_null") {
    // not expressible, as above, and scoped to this method
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
    // not expressible, as above: the parameter is a pair, and a currency is not one
    assertDoesNotCompile("""CurrencyPair.of(GBP, USD).isInverse(USD)""")
    assertCompiles("""CurrencyPair.of(GBP, USD).isInverse(CurrencyPair.of(USD, GBP))""")
  }

  //-------------------------------------------------------------------------
  test("test_cross_CurrencyPair") {
    // every row matters here, and the eight that produce a cross matter twice over: they pin
    // the order in which the four cases are tried, which decides the base of the cross when
    // the two pairs share both a base and a counter, and a caller computing a cross rate
    // reads that base to decide which of its two rates to invert
    forAll(data_cross) {
      (pair: CurrencyPair, other: CurrencyPair, expected: Option[CurrencyPair]) =>
        pair.cross(other) shouldBe expected
    }
  }

  test("test_cross_CurrencyPair_null") {
    // not expressible, as above
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
    // Every pair of two currencies has exactly one conventional direction. That is the
    // guarantee `toConventional` rests on, and through it every caller that normalises a pair
    // before looking a rate up, so it is asserted exhaustively rather than by sampling: over
    // the fifty-five currencies in active use there are 1,485 unordered pairs, and each is
    // checked in both directions.
    //
    // The loop of the original is kept as a loop, over an indexed sequence taken from the
    // set, so that each unordered pair is visited exactly once. Both levels are inspections
    // rather than bare iterations, which is what makes every assertion a value the suite
    // examines rather than one it evaluates and drops, and the clue names the two pairs that
    // disagreed so a failure identifies the currencies rather than an index.
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

      // applying it again changes nothing, which is what makes it usable as a normalisation
      pair.toConventional.toConventional shouldBe expected
    }
  }

  test("test_rateDigits") {
    forAll(data_rateDigits) { (pair: CurrencyPair, expected: Int) =>
      pair.getRateDigits shouldBe expected

      // a quote carries the same precision either way round, so a pair and its inverse agree
      pair.inverse.getRateDigits shouldBe expected
    }
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = CurrencyPair.of(AUD, GBP)
    val a2 = CurrencyPair.of(AUD, GBP)
    val b = CurrencyPair.of(USD, GBP)
    val c = CurrencyPair.of(USD, EUR)

    // `equals` is called as a method, as the original called it, so that the whole matrix is
    // stated - including the reflexive cases, which a `==` between two identical expressions
    // would be flagged for in this build
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

    // the same statements through the instance that carries equality and hashing for this
    // port, which is where the rest of the library reads them from. The companion publishes
    // one equality-bearing instance, an `Order` that is also a `Hash`, so there is no second
    // notion of equality that could disagree with these.
    Order[CurrencyPair].eqv(a1, a2) shouldBe true
    Order[CurrencyPair].eqv(a1, b) shouldBe false
    Hash[CurrencyPair].hash(a1) shouldBe Hash[CurrencyPair].hash(a2)

    // and the ordering agrees with that equality - it compares zero exactly when the two
    // pairs are equal - which is the law the instance has to satisfy. The original did not
    // order pairs at all; the ordering is base first, then counter, each by its code.
    Order[CurrencyPair].compare(a1, a2) shouldBe 0
    Order[CurrencyPair].compare(a1, b) should be < 0
    Order[CurrencyPair].compare(b, a1) should be > 0
    Order[CurrencyPair].compare(b, c) should be > 0
  }

  test("test_equals_bad") {
    val test = CurrencyPair.of(AUD, GBP)

    // a value of an unrelated type is not equal to a pair, in either direction. The original
    // also asserted equality against the absent-reference literal; that case is subsumed by
    // the types of this port, where a reference to a pair cannot be absent and there is no
    // such literal to compare against, so it is recorded here rather than written.
    test.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(test) shouldBe false
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not supported by this port; the JSON codec is the serialized form
    // of a pair, and it writes the text form as a bare string rather than an object of two
    // codes. The two values the original round-tripped are round-tripped through it, one
    // explicit example each - the property-based sweep over every codec of the module belongs
    // to the module's JSON round-trip spec.
    CurrencyPair.of(GBP, USD).asJson shouldBe Json.fromString("GBP/USD")
    Json.fromString("GBP/USD").as[CurrencyPair] shouldBe Right(CurrencyPair.of(GBP, USD))

    CurrencyPair.of(GBP, GBP).asJson shouldBe Json.fromString("GBP/GBP")
    Json.fromString("GBP/GBP").as[CurrencyPair] shouldBe Right(CurrencyPair.of(GBP, GBP))

    // decoding goes through the same parsing the text form uses, so text that names no pair
    // is a decoding failure rather than a silently accepted value
    Json.fromString("GBP-USD").as[CurrencyPair].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // Annotation-driven string conversion has no counterpart either. Its two halves were a
    // rendering method and a parsing factory, and the name string it produced is the identity
    // of a pair - the text in documents, messages and expectations - so the round trip the
    // original asserted through that mechanism is asserted through the rendering instance and
    // the parsing factory, on the same two values, and the text is exactly the text the
    // original produced.
    Show[CurrencyPair].show(CurrencyPair.of(GBP, USD)) shouldBe "GBP/USD"
    Show[CurrencyPair].show(CurrencyPair.of(GBP, GBP)) shouldBe "GBP/GBP"

    CurrencyPair.parse(Show[CurrencyPair].show(CurrencyPair.of(GBP, USD))) should
      haveValue(CurrencyPair.of(GBP, USD))
    CurrencyPair.parse(Show[CurrencyPair].show(CurrencyPair.of(GBP, GBP))) should
      haveValue(CurrencyPair.of(GBP, GBP))

    // the rendering instance and `toString` are the same text, so a pair reaching a message
    // through either route reads the same
    Show[CurrencyPair].show(CurrencyPair.of(GBP, USD)) shouldBe CurrencyPair.of(GBP, USD).toString
  }

  //-------------------------------------------------------------------------
  // Mapping from the Java test class, for the record: all twenty-seven annotated methods are
  // present above under their Java names, none dropped and none consolidated, and the two
  // that were driven by a data provider are one test each holding their whole table. The
  // ten exception assertions of the original are accounted for as follows: the seven that
  // passed an absent reference become compile-time proofs, spread over the five tests whose
  // names end in `_null`, and the remaining three become failure assertions.
}
