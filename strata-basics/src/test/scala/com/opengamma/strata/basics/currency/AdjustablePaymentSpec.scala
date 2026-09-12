/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.time.DayOfWeek
import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions.FOLLOWING
import com.opengamma.strata.basics.date.HolidayCalendarIds.GBLO
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[AdjustablePayment]], ported from the Java `AdjustablePaymentTest`.
 *
 * The original held twelve test methods and this suite holds twelve, each under the name the
 * original gave it, because the migration is traced method by method and a Java test method and a
 * test of this suite are joined on the pair of suite class and test name. Three of the twelve
 * asserted something whose form changed in the port, and what replaced each is stated here once
 * rather than argued again in every test.
 *
 * ===Eight of the twelve are one claim, asserted eight times===
 *
 * The first eight methods walk a grid: three intents - take the sign as given, force it negative,
 * force it positive - each in a fixed-date and an adjustable-date form, plus the two overloads
 * that take a currency and a raw number. Every one of them asserts the same four facts of the
 * result, and what distinguishes the pairs is only whether the date given was wrapped into an
 * [[com.opengamma.strata.basics.date.AdjustableDate]] carrying no adjustment or passed through as
 * it stood. The four assertions are therefore made once, in [[assertPayment]], and each of the
 * eight tests states its call and the four values it expects; the eight names survive for the
 * traceability join. The pairs are what the grid exists to prove, so neither half of any pair is
 * dropped.
 *
 * ===Resolution reports a missing calendar rather than raising===
 *
 * `test_resolve` is the one test here that reaches reference data, and the production method it
 * exercises returns a value where the bean being ported returned a payment and raised a
 * `ReferenceDataNotFoundException` for reference data that could not supply the calendar. That
 * outcome is now a `Left(`[[com.opengamma.strata.collect.result.Failure.MissingData]]`)`, per the
 * migration plan's mandate that a failure depending on the data of the arguments becomes an
 * explicit value. The Java assertion is therefore made through the outcome matchers, and the
 * failing case the change introduces is asserted alongside it in the same test rather than under
 * a name the Java suite never had. The reader form of the same resolution, which the port adds,
 * is asserted there too - it is the only place in this package where reference data is threaded
 * through a `Kleisli`.
 *
 * ===The reflective coverage sweep is replaced by named assertions===
 *
 * `coverage` called the two reflective bean-coverage helpers of the Java test helper: one walked
 * every property of an immutable bean through its meta-bean, the other compared two beans property
 * by property. There is no meta-bean here and nothing in this port reads a class while the program
 * runs, so neither helper has a target - the retained test helper of the ported collect module has
 * five members and neither of these is among them. Their substance does have a target: they stood
 * for the claims that two instances built independently from equal parts are equal, that instances
 * differing in any field are not, and that an instance renders itself faithfully. Those claims are
 * asserted directly below, on the type's own members and on its two typeclass instances, over the
 * same pair of instances the Java method covered.
 *
 * ===Java serialization is replaced by the JSON codec===
 *
 * `test_serialization` asserted a Java-serialization round trip. Java serialization is not part of
 * this port at all; the codec derived when [[AdjustablePayment]] is compiled is its single
 * serialized form, so the round trip asserted below is `decode(encode(x)) == x`, together with the
 * exact shape of the document. Two examples are used and only two: the fixed-date payment the Java
 * method serialized, and the adjustable-date one, which is the only case in which the nested
 * business day adjustment - and through it the holiday calendar identifier - has to survive the
 * trip. The sweep over every codec-bearing type of the module, driven by generators, belongs to
 * the module's JSON round-trip spec, and repeating it here would duplicate it rather than add to
 * it.
 *
 * ===What this suite does not do===
 *
 * It asserts no typeclass law - the law suites of the module cover the instances published here -
 * runs no generated sweep, loads no fixture and performs no effect. It leaves
 * [[com.opengamma.strata.basics.date.AdjustableDate]],
 * [[com.opengamma.strata.basics.date.BusinessDayAdjustment]],
 * [[com.opengamma.strata.basics.date.HolidayCalendarId]] and
 * [[com.opengamma.strata.basics.ReferenceData]] untested in their own right, each having a spec of
 * its own, and uses them only as the fixtures that make an adjustable payment adjustable.
 */
final class AdjustablePaymentSpec extends AnyFunSuite with Matchers {

  /**
   * The reference data the resolution test runs against.
   *
   * The standard set of the library, which holds every built-in holiday calendar including the
   * London calendar the fixture below names. It is a value in this port where the Java original
   * called a method for it; nothing else about its use changes.
   */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** A thousand pounds received, the positive fixture of the Java test. */
  private val GBP_P1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, 1000d))

  /** A thousand pounds paid, the negative fixture of the Java test. */
  private val GBP_M1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, -1000d))

  /**
   * Sixteen hundred euro, the second instance of the Java coverage method.
   *
   * It differs from the first fixture in both currency and amount, which is what makes the pair
   * the Java sweep compared a pair that cannot be equal for either field's reason alone.
   */
  private val EUR_P1600: CurrencyAmount = unwrap(CurrencyAmount.of(EUR, 1600d))

  /**
   * The Monday the adjustable fixture resolves to.
   *
   * It is the expected outcome of `test_resolve` and never an input: nothing in this suite builds
   * a payment on this date.
   */
  private val DATE_2015_06_29: LocalDate = date(2015, 6, 29)

  /**
   * A Sunday, to be moved by the London calendar under the following convention.
   *
   * This is the fixture that makes resolution observable: the unadjusted date is not a business
   * day anywhere, so a resolution that silently failed to adjust would be caught.
   */
  private val DATE_2015_06_28_ADJ: AdjustableDate =
    AdjustableDate.of(date(2015, 6, 28), BusinessDayAdjustment.of(FOLLOWING, GBLO))

  /** The Tuesday most of the Java fixtures are paid on, as a plain date. */
  private val DATE_2015_06_30: LocalDate = date(2015, 6, 30)

  /**
   * The same Tuesday as an adjustable date carrying no adjustment.
   *
   * This is what every fixed-date factory is expected to wrap its date into, so it is the expected
   * value of the `date` field in each of the four fixed-date tests rather than a separate input.
   */
  private val DATE_2015_06_30_FIX: AdjustableDate = AdjustableDate.of(date(2015, 6, 30))

  /**
   * The JSON form of `AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)`.
   *
   * An object holding the two fields under the names the Java bean declared and in the order the
   * type declares them: the amount as the object [[CurrencyAmount]] publishes, and the date as the
   * object [[com.opengamma.strata.basics.date.AdjustableDate]] publishes, whose absent adjustment
   * is written out in full as the no-adjustment convention over the no-holidays calendar rather
   * than omitted.
   */
  private val ExpectedFixedJson: String =
    """{"value":{"currency":"GBP","amount":1000.0},""" +
      """"date":{"unadjusted":"2015-06-30",""" +
      """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}}"""

  /**
   * The JSON form of `AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)`.
   *
   * The same shape as above with a real adjustment in the nested date: the convention and the
   * calendar identifier, each as the bare string of its own name.
   */
  private val ExpectedAdjustableJson: String =
    """{"value":{"currency":"GBP","amount":1000.0},""" +
      """"date":{"unadjusted":"2015-06-28",""" +
      """"adjustment":{"convention":"Following","calendar":"GBLO"}}}"""

  //-------------------------------------------------------------------------
  test("test_of_3argsFixed") {
    // The amount arrives as a raw number, so this overload has to build a CurrencyAmount and
    // inherits that type's refusal of a value which is not a number: it returns an outcome, which
    // is unwrapped rather than assumed to hold a payment.
    val test = unwrap(AdjustablePayment.of(GBP, 1000d, DATE_2015_06_30))

    // The plain date is wrapped into an adjustable date carrying no adjustment, which is the fact
    // that distinguishes this overload from the adjustable one below.
    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_30_FIX)

    // The one door in this type through which construction can fail, asserted as the value it is
    // rather than as a raised error. What the failure says belongs to the spec of CurrencyAmount
    // and to the module's smart-constructor spec; all that matters here is that this overload has
    // the door and reports through it.
    AdjustablePayment.of(GBP, Double.NaN, DATE_2015_06_30).isLeft shouldBe true
  }

  test("test_of_3argsAdjustable") {
    val test = unwrap(AdjustablePayment.of(GBP, 1000d, DATE_2015_06_28_ADJ))

    // The adjustable date is carried through exactly as given: a factory is not the place a date
    // is adjusted, and the Sunday survives until it is resolved.
    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_28_ADJ)

    // The second of the two failing overloads, with the same single door.
    AdjustablePayment.of(GBP, Double.NaN, DATE_2015_06_28_ADJ).isLeft shouldBe true
  }

  test("test_of_2argsFixed") {
    // No unwrapping: the amount was checked by the type that holds it and an adjustable payment
    // adds no invariant of its own, so this factory returns a payment.
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_30_FIX)

    // The fixed-date factory is its adjustable counterpart applied to the wrapping, which is what
    // keeps the two halves of the pair from drifting apart.
    test shouldBe AdjustablePayment.of(GBP_P1000, DATE_2015_06_30_FIX)
  }

  test("test_of_2argsAdjustable") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_28_ADJ)

    // The factory and the case-class constructor are two spellings of one thing, which is what
    // makes this a total type.
    test shouldBe AdjustablePayment(GBP_P1000, DATE_2015_06_28_ADJ)
  }

  //-------------------------------------------------------------------------
  test("test_ofPayFixed") {
    val test = AdjustablePayment.ofPay(GBP_P1000, DATE_2015_06_30)

    // The amount handed in was positive and the payment holds it negative: `ofPay` states a
    // direction rather than preserving the sign it was given. The date is wrapped, untouched.
    assertPayment(test, GBP_M1000, GBP, -1000d, DATE_2015_06_30_FIX)

    // The normalisation is a normalisation and not a negation, which is observable and is the
    // reason `ofPay` is safe to apply to an amount of unknown sign: an amount that is already
    // negative passes through unchanged, where an unconditional negation would turn it back into
    // money received. Asserting only the case the Java test asserted would pass for either
    // implementation, so the second case is asserted too.
    AdjustablePayment.ofPay(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  test("test_ofPayAdjustable") {
    val test = AdjustablePayment.ofPay(GBP_P1000, DATE_2015_06_28_ADJ)

    assertPayment(test, GBP_M1000, GBP, -1000d, DATE_2015_06_28_ADJ)

    AdjustablePayment.ofPay(GBP_M1000, DATE_2015_06_28_ADJ) shouldBe test
  }

  test("test_ofReceiveFixed") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // The mirror image: the amount is stated positive, and one that is already positive is left
    // exactly as it came in.
    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_30_FIX)

    // and the case that distinguishes the normalisation from a negation, as above.
    AdjustablePayment.ofReceive(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  test("test_ofReceiveAdjustable") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_28_ADJ)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_28_ADJ)

    AdjustablePayment.ofReceive(GBP_M1000, DATE_2015_06_28_ADJ) shouldBe test
  }

  //-------------------------------------------------------------------------
  /**
   * Resolution against reference data, the one test of this suite that reaches it.
   *
   * The Java method asserted a single equality: a payment received on the adjustable Sunday
   * resolves to a payment on the Monday. That equality is asserted here through the outcome
   * matcher, because the production method returns
   * `Either[`[[com.opengamma.strata.collect.result.Failure]]`, `[[Payment]]`]` where the bean
   * being ported returned a bare payment, and two further claims are asserted with it:
   *
   *  - the reader form of the same resolution. The migration plan has every type that resolves
   *    against reference data offer both the direct method and a `toReader` that is the same
   *    resolution as a value awaiting its data, so the two are asserted to agree. This is the only
   *    place in this package where that composition surface is exercised.
   *  - the failing case the port introduces. Reference data that cannot supply the calendar the
   *    date's adjustment names raised a `ReferenceDataNotFoundException` in the library being
   *    ported; here it is a
   *    `Left(`[[com.opengamma.strata.collect.result.Failure.MissingData]]`)`, which is the
   *    migration plan's rule that a failure depending on the data of the arguments is reported as
   *    a value. The minimal set is the reference data used for it: it holds the four weekend and
   *    no-holiday calendars and nothing else, so the London calendar is genuinely absent from it
   *    while the no-holidays calendar an unadjusted date needs is still there.
   */
  test("test_resolve") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_28_ADJ)
    val expected = Payment.of(GBP_P1000, DATE_2015_06_29)

    // Why the date moves, and why these two dates and not any others: 28 June 2015 was a Sunday,
    // and the following convention over the London calendar rolls a non-business day forwards to
    // the next business day, which was Monday 29 June. The two days are asserted so that neither
    // date reads as an arbitrary fixture, and so that a change to the calendar data underneath
    // this test cannot quietly turn it into a test of nothing.
    DATE_2015_06_28_ADJ.unadjusted.getDayOfWeek shouldBe DayOfWeek.SUNDAY
    DATE_2015_06_29.getDayOfWeek shouldBe DayOfWeek.MONDAY

    // The Java assertion. Resolution settles when the payment is made and never how much it is, so
    // the amount is carried across untouched while the date moves.
    test.resolve(REF_DATA) should haveValue(expected)
    test.resolve(REF_DATA).map(payment => payment.value) shouldBe Right(GBP_P1000)
    test.resolve(REF_DATA).map(payment => payment.date) shouldBe Right(DATE_2015_06_29)

    // The reader form, which the port adds: the same resolution held as a function from reference
    // data, run against the same data, giving the same payment.
    test.toReader.run(REF_DATA) should haveValue(expected)
    test.toReader.run(REF_DATA) shouldBe test.resolve(REF_DATA)

    // A payment whose date carries no adjustment resolves to that very date - it is not exempt
    // from the lookup, since the absent adjustment names the no-holidays calendar, but the
    // convention then moves nothing.
    AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_30).resolve(REF_DATA) should
      haveValue(Payment.of(GBP_P1000, DATE_2015_06_30))

    // The failing case: reference data without the London calendar cannot resolve this date, and
    // says so as a value carrying the reason for a datum that is absent.
    test.resolve(ReferenceData.minimal) should beFailureWith(FailureReason.MISSING_DATA)
    test.toReader.run(ReferenceData.minimal) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_negated") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // Negation reverses the direction of a known payment and leaves the date alone. It is
    // unconditional, which is what distinguishes it from the sign normalisation of `ofPay` and
    // `ofReceive`: applying it twice returns to the payment it started from.
    test.negated shouldBe AdjustablePayment.of(GBP_M1000, DATE_2015_06_30)
    test.negated.date shouldBe DATE_2015_06_30_FIX
    test.negated.negated shouldBe test
  }

  //-------------------------------------------------------------------------
  /**
   * The coverage test of the Java original, asserting the equality, hashing and rendering of the
   * type directly rather than through a reflective sweep.
   *
   * The two instances are the two the Java method used - a thousand pounds on the fixed Tuesday,
   * and sixteen hundred euro on the adjustable Sunday - so the pair it compared is the pair
   * compared here, differing in both of their fields.
   *
   * Two properties of the type are worth naming while they are asserted. Its equality holds no
   * comparison of its own: it delegates the amount to [[CurrencyAmount]], which compares a double
   * by its bit pattern rather than by the numeric comparison of the platform, and the date to
   * [[com.opengamma.strata.basics.date.AdjustableDate]], which compares the agreement and not the
   * day the agreement works out to - both delegations are asserted below, because it is the
   * delegation that is being claimed and not merely the equality of two ordinary payments. And the
   * companion publishes a `Hash` and a `Show` and no `Order`: the bean being ported is not
   * `Comparable`, so an ordering of payments would be this port's invention, and a caller that
   * needs one sorts by the field it means.
   */
  test("coverage") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)
    val test2 = AdjustablePayment.of(EUR_P1600, DATE_2015_06_28_ADJ)

    // An instance built independently of the fixtures, from equal parts, is equal to the first -
    // which is what makes the equality structural rather than by reference - and hashes equally.
    // `Hash` is the type's single equality-bearing instance, so it is asserted to agree with the
    // platform equality and with the platform hash.
    val same =
      AdjustablePayment(unwrap(CurrencyAmount.of(GBP, 1000d)), AdjustableDate.of(date(2015, 6, 30)))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[AdjustablePayment].eqv(same, test) shouldBe true
    Hash[AdjustablePayment].hash(same) shouldBe test.hashCode

    // The pair of the Java coverage call is unequal, under the platform equality and under the
    // typeclass alike, and so is each instance that differs from the first in one field only -
    // which is what proves both fields participate rather than just the one that happens to be
    // read first.
    test2 should not be test
    Hash[AdjustablePayment].eqv(test2, test) shouldBe false
    AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ) should not be test
    AdjustablePayment.of(EUR_P1600, DATE_2015_06_30) should not be test
    Hash[AdjustablePayment].eqv(AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ), test) shouldBe false
    Hash[AdjustablePayment].eqv(AdjustablePayment.of(EUR_P1600, DATE_2015_06_30), test) shouldBe false

    // Equality on the amount is the bit-pattern equality of the type that holds it, reached here
    // through a value the numeric comparison of the platform also calls equal to itself but which
    // no arithmetic produced twice: two infinite payments built separately are one value. The
    // amount refuses a value that is not a number, so an infinity is the boundary case this type
    // can actually hold.
    val infinite =
      AdjustablePayment.of(unwrap(CurrencyAmount.of(GBP, Double.PositiveInfinity)), DATE_2015_06_30)
    infinite shouldBe AdjustablePayment.of(unwrap(CurrencyAmount.of(GBP, 1d / 0d)), DATE_2015_06_30)
    Hash[AdjustablePayment].eqv(infinite, test) shouldBe false

    // Equality on the date is the equality of the agreement: two payments on the same unadjusted
    // Sunday are unequal where one of them would move and the other would not, even though the
    // amounts match.
    AdjustablePayment.of(GBP_P1000, AdjustableDate.of(date(2015, 6, 28))) should not be
      AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)

    // `Show` renders what `toString` renders, and what `toString` renders is the form the Java
    // bean produced - the two fields named in declaration order between braces, with the amount
    // rendered by its own type, which prints a whole number without a decimal point, and the date
    // by its own, which names its adjustment where it has one. The literals are pinned so that a
    // change to the rendering cannot pass unnoticed.
    Show[AdjustablePayment].show(test) shouldBe test.toString
    test.toString shouldBe "AdjustablePayment{value=GBP 1000, date=2015-06-30}"
    test2.toString shouldBe
      "AdjustablePayment{value=EUR 1600, date=2015-06-28 adjusted by Following using calendar GBLO}"
  }

  test("test_serialization") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)

    // The document, compared as a parsed document rather than as printed text: what is claimed is
    // the fields present, their names and their values, not the whitespace or the order a printer
    // happens to choose.
    test.asJson shouldBe json(ExpectedFixedJson)

    // and the round trip. An encoding that is right and a decoding that is wrong would still round
    // trip if only the round trip were asserted, and the other way about, so both are asserted.
    decode[AdjustablePayment](test.asJson.noSpaces) shouldBe Right(test)

    // The shape is pinned member by member as well, because the whole-document comparison above
    // would also be satisfied by a literal that had drifted with the codec: the two field names are
    // the ones the Java bean declared, in declaration order; the amount nests the object form of
    // its own type; and the date nests the object form of its own.
    test.asJson.asObject.map(obj => obj.keys.toList) shouldBe Some(List("value", "date"))
    test.asJson.hcursor.downField("value").get[String]("currency") shouldBe Right("GBP")
    test.asJson.hcursor.downField("value").get[Double]("amount") shouldBe Right(1000d)
    test.asJson.hcursor.downField("date").get[String]("unadjusted") shouldBe Right("2015-06-30")

    // The adjustable variant, which is the only one that puts a business day adjustment - and
    // through it a holiday calendar identifier - into the document. The fixed variant above would
    // not exercise either, since its adjustment is the no-adjustment constant.
    val adjustable = AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)
    adjustable.asJson shouldBe json(ExpectedAdjustableJson)
    decode[AdjustablePayment](adjustable.asJson.noSpaces) shouldBe Right(adjustable)

    // and the identifier in particular survived, rather than merely something that compares equal
    // by an accident of the enclosing document.
    decode[AdjustablePayment](adjustable.asJson.noSpaces)
      .map(payment => payment.date.adjustment.calendar) shouldBe Right(GBLO)

    // Neither field of this type is optional, so neither route writes an absent one. That is
    // already settled by the two whole-document comparisons above, each of which pins the complete
    // set of members: a document carrying a member the literal does not name would fail them. It
    // is stated here because the encoder is wrapped in the port's single policy for products all
    // the same, and this is what that policy amounts to for a product with no optional field.
    test.asJson.asObject.map(obj => obj.size) shouldBe Some(2)
    adjustable.asJson.asObject.map(obj => obj.size) shouldBe Some(2)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the four facts every one of the eight factory tests asserts of its result.
   *
   * The Java suite repeated these four assertions in each of its first eight methods, and the
   * repetition was the whole body of each. They are made once here, so that each test above states
   * only what is particular to it - the factory it calls and the four values that call is expected
   * to produce - and so that a change to what a factory is expected to expose is made in one
   * place. Each assertion is labelled, so a failure names the fact that failed rather than leaving
   * a bare pair of payments to be compared by eye.
   *
   * The `date` fact is the one that distinguishes a fixed-date factory from its adjustable-date
   * counterpart: the fixed forms are expected to have wrapped the date they were given into an
   * adjustable date carrying no adjustment, and the adjustable forms to have carried theirs
   * through untouched.
   *
   * @param actual  the payment the factory under test produced
   * @param value  the amount the payment is expected to hold, after any sign normalisation
   * @param currency  the currency the payment is expected to report
   * @param amount  the signed amount the payment is expected to report
   * @param paymentDate  the adjustable date the payment is expected to hold
   * @return the assertion of the last of the four facts, the preceding three having already been
   *   asserted
   */
  private def assertPayment(
      actual: AdjustablePayment,
      value: CurrencyAmount,
      currency: Currency,
      amount: Double,
      paymentDate: AdjustableDate): Assertion = {
    withClue("value: ")(actual.value shouldBe value)
    withClue("getCurrency: ")(actual.getCurrency shouldBe currency)
    withClue("getAmount: ")(actual.getAmount shouldBe amount)
    withClue("date: ")(actual.date shouldBe paymentDate)
  }

  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite. It exists because the fixtures of this
   * suite and two of the calls it makes report what was wrong with their arguments as a value
   * rather than by raising - `CurrencyAmount.of` behind the fixtures, and the two three-argument
   * `AdjustablePayment.of` overloads - and because the Java test asserted their results directly.
   * The outcome is folded rather than opened by a partial accessor, so a fixture or a call that
   * unexpectedly fails is reported as a test failure naming the reason instead of raising an error
   * from somewhere else in the suite.
   *
   * The resolution outcomes are deliberately not unwrapped through this helper: they are asserted
   * as outcomes, through the matchers, because whether they succeed is the subject of the test.
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
