/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[Payment]], ported from the Java `PaymentTest`.
 *
 * The original held eleven test methods and this suite holds eleven, each under the name the
 * original gave it, because the migration is traced method by method and a Java test method and a
 * test of this suite are joined on the pair of suite class and test name. Three of the eleven
 * asserted something that has no direct counterpart in this port, and what replaced each is stated
 * here once rather than argued again in every test.
 *
 * ===The builder is replaced by `copy`===
 *
 * `test_builder` built a payment through the generated builder of the Joda-Beans bean. This port
 * carries no builder: [[Payment]] is a total type - a pair of two values that are already valid,
 * with no invariant of its own - so it is an ordinary `case class` whose `copy` is public, and
 * `copy` is what takes the builder's place across the whole migration. The test therefore keeps
 * its name, for the traceability join, and exercises `copy` in the builder's stead. A builder was
 * a run-time contract, where a field left unset was discovered when `build` was called; `copy` is
 * a compile-time one, and it is the stronger of the two.
 *
 * ===The reflective coverage sweep is replaced by named assertions===
 *
 * `coverage` called the two reflective bean-coverage helpers of the Java test helper: one walked
 * every property of an immutable bean through its meta-bean, the other compared two beans property
 * by property. There is no meta-bean here and nothing in this port reads a class while the program
 * runs, so neither helper has a target - the retained test helper of the ported collect module has
 * five members and neither of these is among them. Their substance does have a
 * target: they stood for the claims that two instances built independently from equal parts are
 * equal, that instances differing in any field are not, and that an instance renders itself
 * faithfully. Those claims are asserted directly below, on the type's own members and on its two
 * typeclass instances, which says more than the sweep did because each case names the outcome it
 * expects instead of merely visiting a field.
 *
 * ===Java serialization is replaced by the JSON codec===
 *
 * `test_serialization` asserted a Java-serialization round trip. Java serialization is not part of
 * this port at all; the codec derived when [[Payment]] is compiled is its single serialized form,
 * so the round trip asserted below is `decode(encode(x)) == x` over one concrete payment, together
 * with the exact shape of the document. One example is deliberate: the sweep over every
 * codec-bearing type of the module, driven by generators, belongs to the module's JSON round-trip
 * spec, and repeating it here would duplicate it rather than add to it.
 *
 * ===What the port changed in the calls themselves===
 *
 * Three call shapes differ from the Java test, each following the production type:
 *
 *  - the three-argument `Payment.of` takes a raw number, so it has to build a [[CurrencyAmount]]
 *    and inherits that type's refusal of a value which is not a number. It returns an outcome
 *    rather than a payment, and this suite unwraps it through [[unwrap]] rather than assuming the
 *    value is there. Every other factory takes an amount that has already been checked and is
 *    total.
 *  - `adjustDate` takes a `LocalDate => LocalDate` and not a temporal adjuster of the platform, so
 *    the `ofDateAdjuster` wrapper the Java test put round its lambda is dropped and the function is
 *    passed directly.
 *  - `convertedTo` returns an outcome, because a conversion between two different currencies needs
 *    a rate the provider may not hold, and the provider is built with
 *    [[FxRateProvider.fromFunction]]: the provider's rate lookup itself returns an outcome, so the
 *    bare two-argument lambda of the Java test no longer conforms to it.
 *
 * ===What this suite does not do===
 *
 * It asserts no typeclass law - the law suites of the module cover the instances published here -
 * runs no generated sweep, loads no fixture and performs no effect. It also leaves
 * [[AdjustablePayment]] entirely alone, including the resolution of its date against reference
 * data: that type has a spec of its own.
 */
final class PaymentSpec extends AnyFunSuite with Matchers {

  /** A thousand pounds received, the positive fixture of the Java test. */
  private val GBP_P1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, 1000d))

  /** A thousand pounds paid, the negative fixture of the Java test. */
  private val GBP_M1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, -1000d))

  /**
   * Sixteen hundred euro, the fixture the Java test converted into: it is a thousand pounds at the
   * flat rate of 1.6 that the rate provider of `test_convertedTo_rateProvider` supplies, and the
   * product is exactly this value rather than merely close to it, which is why that test compares
   * amounts for equality and needs no tolerance.
   */
  private val EUR_P1600: CurrencyAmount = unwrap(CurrencyAmount.of(EUR, 1600d))

  /** The earlier of the two dates of the Java test, the one the adjustment tests move. */
  private val DATE_2015_06_29: LocalDate = date(2015, 6, 29)

  /** The later of the two dates of the Java test, the payment date of most of its fixtures. */
  private val DATE_2015_06_30: LocalDate = date(2015, 6, 30)

  /**
   * The JSON form of `Payment.of(GBP_P1000, DATE_2015_06_30)`.
   *
   * An object holding the two fields under the names the Java bean declared and in the order the
   * type declares them: the amount as the object [[CurrencyAmount]] publishes, and the date as the
   * ISO-8601 string of the platform.
   */
  private val ExpectedJson: String =
    """{"value":{"currency":"GBP","amount":1000.0},"date":"2015-06-30"}"""

  //-------------------------------------------------------------------------
  test("test_of_3args") {
    val test = unwrap(Payment.of(GBP, 1000d, DATE_2015_06_30))

    // The four facts the Java test asserted. Two of the four accessors are read under the names
    // the bean gave them, which the production type keeps; the other two are the fields of the
    // case class, where the bean had a getter apiece.
    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    // This is the one factory of the type that can fail, and it fails for one reason: the amount
    // arrives as a raw number and has to become a CurrencyAmount, which refuses a value that is
    // not a number. The failure is reported as a value, so the refusal is asserted here as the
    // outcome of a call rather than as a raised error - which is the whole point of the change,
    // and is why this test unwraps the successful case above instead of taking the payment for
    // granted. What the failure says, and the accumulation of failures generally, belong to the
    // specs of CurrencyAmount and to the module's smart-constructor spec; all that matters here is
    // that the door exists and that this factory is the one that has it.
    Payment.of(GBP, Double.NaN, DATE_2015_06_30).isLeft shouldBe true
  }

  test("test_of_2args") {
    val test = Payment.of(GBP_P1000, DATE_2015_06_30)

    // The same four facts, reached through the factory that takes an amount already checked. No
    // unwrapping: this overload returns a payment, because a payment adds no invariant to the two
    // values it holds and the amount brought its own validity with it.
    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    // The factory and the case-class constructor are two spellings of one thing, which is what
    // makes this a total type.
    test shouldBe Payment(GBP_P1000, DATE_2015_06_30)
  }

  test("test_ofPay") {
    val test = Payment.ofPay(GBP_P1000, DATE_2015_06_30)

    // The amount handed in was positive and the payment holds it negative: `ofPay` states a
    // direction rather than preserving the sign it was given. The date is untouched.
    test.value shouldBe GBP_M1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe -1000d
    test.date shouldBe DATE_2015_06_30

    // The normalisation is a normalisation and not a negation, which is observable and is the
    // reason `ofPay` is safe to apply to an amount of unknown sign: an amount that is already
    // negative passes through unchanged, where an unconditional negation would turn it back into
    // money received. Asserting only the case the Java test asserted would pass for either
    // implementation, so the second case is asserted too.
    Payment.ofPay(GBP_M1000, DATE_2015_06_30).getAmount shouldBe -1000d
    Payment.ofPay(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  test("test_ofReceive") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // The mirror image: the amount is stated positive, and one that is already positive is left
    // exactly as it came in.
    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    // and the case that distinguishes the normalisation from a negation, as above.
    Payment.ofReceive(GBP_M1000, DATE_2015_06_30).getAmount shouldBe 1000d
    Payment.ofReceive(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  /**
   * The builder test of the Java original, asserting `copy` in the builder's place.
   *
   * The Java method built its payment field by field through the generated builder. Per the
   * migration plan (AAP 0.1.2), the generated Joda-Beans builders have no target in this port and
   * the `copy` of the case class replaces them, so this test keeps its name - the traceability
   * join is on the name - and asserts the replacement: a payment is derived from an unrelated one
   * by naming both fields, and the four facts the Java builder test asserted are asserted of the
   * result.
   *
   * The starting payment is deliberately equal to the target in neither field, so that the
   * assertions cannot be satisfied by a `copy` that quietly ignores one of its arguments.
   */
  test("test_builder") {
    val test = Payment.of(EUR_P1600, DATE_2015_06_29).copy(value = GBP_P1000, date = DATE_2015_06_30)

    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    // A copy that renames nothing is the payment it came from, which is the identity a caller of
    // the bean reached by turning it back into a builder and building it again; and one field may
    // be replaced without disturbing the other, which is what the builder was used for at the
    // ported call sites.
    test.copy() shouldBe test
    test.copy(date = DATE_2015_06_29).value shouldBe GBP_P1000
    test.copy(date = DATE_2015_06_29).date shouldBe DATE_2015_06_29
    test.copy(value = EUR_P1600).value shouldBe EUR_P1600
    test.copy(value = EUR_P1600).date shouldBe DATE_2015_06_30
  }

  //-------------------------------------------------------------------------
  test("test_adjustDate") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_29)
    val expected = Payment.of(GBP_P1000, DATE_2015_06_29.plusDays(1))

    // The Java test wrapped its lambda in the `ofDateAdjuster` factory of the platform; the
    // production method takes the function itself, so the wrapper is gone and the function is
    // passed as it stands. The date moves and the amount does not.
    test.adjustDate(d => d.plusDays(1)) shouldBe expected
    test.adjustDate(_.plusDays(1)).value shouldBe GBP_P1000
    test.adjustDate(_.plusDays(1)).date shouldBe DATE_2015_06_29.plusDays(1)
  }

  /**
   * The no-change adjustment, asserted by reference identity.
   *
   * The Java method asserted `isSameAs`, not equality, and the distinction is the whole content of
   * the test: `adjustDate` returns '''this''' payment when the function hands back the date it was
   * given, and an equality assertion would pass whether that short circuit survived the port or
   * was replaced by a fresh copy of equal value. Identity is therefore asserted here, twice and
   * deliberately - through the matcher, which reports the two instances when it fails, and through
   * the reference comparison itself, which is the fact being claimed.
   *
   * The adjuster moves the date and moves it back, exactly as the Java test's did, so the short
   * circuit is reached through a function that is not the identity function: what the method
   * compares is the date it got back against the date it started from, not the function it was
   * handed.
   */
  test("test_adjustDate_noChange") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_29)
    val adjusted = test.adjustDate(d => d.plusDays(1).minusDays(1))

    adjusted should be theSameInstanceAs test
    assert(adjusted eq test, "adjustDate must return this payment when the adjusted date is unchanged")
  }

  //-------------------------------------------------------------------------
  test("test_negated") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // Negation reverses the direction of a known payment and leaves the date alone. It is
    // unconditional, which is what distinguishes it from the sign normalisation of `ofPay` and
    // `ofReceive`: applying it twice returns to the payment it started from.
    test.negated shouldBe Payment.of(GBP_M1000, DATE_2015_06_30)
    test.negated.date shouldBe DATE_2015_06_30
    test.negated.negated shouldBe test
  }

  //-------------------------------------------------------------------------
  test("test_convertedTo_rateProvider") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // The Java test's provider was the bare lambda `(ccy1, ccy2) -> 1.6d`. The rate lookup of the
    // ported provider returns an outcome, since a provider that holds no rate for a pair says so
    // rather than raising, so the flat rate is lifted into that outcome and the function is turned
    // into a provider explicitly.
    val provider = FxRateProvider.fromFunction((_, _) => Right(1.6d))

    // A thousand pounds at 1.6 is sixteen hundred euro, on the same date: a conversion changes the
    // currency of a payment and never when it is made.
    test.convertedTo(EUR, provider) should haveValue(Payment.ofReceive(EUR_P1600, DATE_2015_06_30))

    // A conversion into the currency the payment already has returns the payment, and the provider
    // is not consulted for a rate it would never need. The Java test asserted this by equality;
    // equality is asserted here through the same matcher, and the identity of the result is
    // asserted underneath it, because returning `this` is the documented behaviour of the
    // production method and an equal copy would satisfy the matcher alone.
    test.convertedTo(GBP, provider) should haveValue(test)
    assert(
      unwrap(test.convertedTo(GBP, provider)) eq test,
      "a conversion into the currency the payment already has must return this payment")

    // The same-currency route needs no rate at all, which is what makes it safe under a provider
    // that supplies none: the conversion succeeds where any other would be declined.
    test.convertedTo(GBP, FxRateProvider.noConversion()) should haveValue(test)
    test.convertedTo(EUR, FxRateProvider.noConversion()).isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * The coverage test of the Java original, asserting the equality, hashing and rendering of the
   * type directly rather than through a reflective sweep.
   *
   * The two instances are the two the Java method used - a thousand pounds on the later date, and
   * sixteen hundred euro on the earlier one - so the pair it compared is the pair compared here,
   * differing in both of their fields.
   *
   * Two properties of the type are worth naming while they are asserted. Its equality delegates
   * the amount to [[CurrencyAmount]], which compares a double by its bit pattern rather than by
   * the numeric comparison of the platform, so an infinite payment is equal to an equal infinite
   * payment - asserted below, because it is the delegation that is being claimed and not merely
   * the equality of two finite payments. And the companion publishes a `Hash` and a `Show` and no
   * `Order`: the bean being ported is not `Comparable`, so an ordering of payments would be this
   * port's invention, and a caller that needs one sorts by the field it means.
   */
  test("coverage") {
    val test = Payment.of(GBP_P1000, DATE_2015_06_30)
    val test2 = Payment.of(EUR_P1600, DATE_2015_06_29)

    // An instance built independently of the fixtures, from equal parts, is equal to the first -
    // which is what makes the equality structural rather than by reference - and hashes equally.
    // `Hash` is the type's single equality-bearing instance, so it is asserted to agree with the
    // platform equality and with the platform hash.
    val same = Payment(unwrap(CurrencyAmount.of(GBP, 1000d)), date(2015, 6, 30))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[Payment].eqv(same, test) shouldBe true
    Hash[Payment].hash(same) shouldBe test.hashCode

    // The pair of the Java coverage call is unequal, under the platform equality and under the
    // typeclass alike, and so is each instance that differs from the first in one field only -
    // which is what proves both fields participate rather than just the one that happens to be
    // read first.
    test2 should not be test
    Hash[Payment].eqv(test2, test) shouldBe false
    Payment.of(GBP_P1000, DATE_2015_06_29) should not be test
    Payment.of(EUR_P1600, DATE_2015_06_30) should not be test
    Hash[Payment].eqv(Payment.of(GBP_P1000, DATE_2015_06_29), test) shouldBe false
    Hash[Payment].eqv(Payment.of(EUR_P1600, DATE_2015_06_30), test) shouldBe false

    // Equality on the amount is the bit-pattern equality of the type that holds it, reached here
    // through a value the numeric comparison of the platform also calls equal to itself but which
    // no arithmetic produced twice: two infinite payments built separately are one value. The
    // amount refuses a value that is not a number, so an infinity is the boundary case this type
    // can actually hold, and the fact underneath the assertion is asserted with it.
    val infinite = Payment.of(unwrap(CurrencyAmount.of(GBP, Double.PositiveInfinity)), DATE_2015_06_30)
    infinite shouldBe Payment.of(unwrap(CurrencyAmount.of(GBP, 1d / 0d)), DATE_2015_06_30)
    Hash[Payment].eqv(infinite, test) shouldBe false
    java.lang.Double.compare(Double.PositiveInfinity, 1d / 0d) shouldBe 0

    // `Show` renders what `toString` renders, and what `toString` renders is the form the Java
    // bean produced - the two fields named in declaration order between braces, with the amount
    // rendered by its own type, which prints a whole number without a decimal point. The literal
    // is pinned so that a change to the rendering cannot pass unnoticed.
    Show[Payment].show(test) shouldBe test.toString
    test.toString shouldBe "Payment{value=GBP 1000, date=2015-06-30}"
    test2.toString shouldBe "Payment{value=EUR 1600, date=2015-06-29}"
  }

  test("test_serialization") {
    val test = Payment.of(GBP_P1000, DATE_2015_06_30)

    // The document, compared as a parsed document rather than as printed text: what is claimed is
    // the fields present, their names and their values, not the whitespace or the order a printer
    // happens to choose.
    test.asJson shouldBe json(ExpectedJson)

    // and the round trip, in both directions. An encoding that is right and a decoding that is
    // wrong would still round-trip if only the round trip were asserted, and the other way about.
    decode[Payment](test.asJson.noSpaces) shouldBe Right(test)

    // The shape is pinned member by member as well, because the whole-document comparison above
    // would also be satisfied by a literal that had drifted with the codec: the two field names
    // are the ones the Java bean declared, in declaration order; the amount nests the object form
    // of its own type; and the date is the ISO-8601 string of the platform.
    test.asJson.asObject.map(obj => obj.keys.toList) shouldBe Some(List("value", "date"))
    test.asJson.hcursor.get[String]("date") shouldBe Right("2015-06-30")
    test.asJson.hcursor.downField("value").get[String]("currency") shouldBe Right("GBP")
    test.asJson.hcursor.downField("value").get[Double]("amount") shouldBe Right(1000d)
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite. It exists because two of the calls this
   * suite makes report what was wrong with their arguments as a value rather than by raising - the
   * three-argument `Payment.of`, and `CurrencyAmount.of` behind the fixtures - and because the
   * Java test asserted their results directly. The outcome is folded rather than opened by a
   * partial accessor, so a fixture or a call that unexpectedly fails is reported as a test failure
   * naming the reason instead of raising an error from somewhere else in the suite.
   *
   * @tparam A  the type of the value the outcome is expected to carry
   * @param outcome  the outcome expected to carry a value
   * @return the value it carries
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the call failed with: ${failure.message}"),
      value => value)

  /**
   * Parses one of the expected JSON forms of this suite into the JSON model.
   *
   * A literal in this file that is not itself valid JSON is a defect in the suite rather than a
   * failure of the subject, so it is reported as one.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))
}
