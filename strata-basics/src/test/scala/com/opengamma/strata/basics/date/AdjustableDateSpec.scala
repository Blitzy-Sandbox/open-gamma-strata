/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[AdjustableDate]], ported from the Java `AdjustableDateTest`.
 *
 * All eight methods of the Java class are kept, each under the name the Java method had -
 * `test_of_1arg`, `test_of_2args_withAdjustment`, `test_of_2args_withNoAdjustment`,
 * `test_of_null`, `test_adjusted`, `equals`, `coverage` and `test_serialization` - so that a
 * Java test method and a test of this suite stay in one-to-one correspondence and the
 * method-level traceability the migration manifest records resolves on the pair of suite class
 * and test name. `equals` is deliberately spelled without a prefix: that is the name of the
 * Java method. Nothing is added under a name of its own; everything this port asserts beyond
 * the Java assertions belongs to whichever of the eight methods already owned that ground.
 *
 * ===The provider table===
 *
 * The Java `test_adjusted` was parameterised from `data_adjusted`: six rows of input date and
 * expected date over the `Sat/Sun` calendar, in which the Saturday and the Sunday both roll
 * forward to the Monday and the three weekdays are returned unaltered. The six rows are
 * transcribed here verbatim and in the Java order, and they are driven through this suite's
 * single `test_adjusted`, as a table rather than as six separate tests, so that the one Java
 * method maps to one test of this suite.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - [[AdjustableDate.adjusted]] returns `Either[Failure, LocalDate]` where the Java method
 *     returned a bare date and threw `ReferenceDataNotFoundException` for a calendar the
 *     reference data does not hold. Every ported assertion is therefore that the outcome is a
 *     success carrying the Java value, and the failure path - which the Java class never
 *     exercised, having nothing but an exception to exercise it with - is asserted in
 *     `test_adjusted` by the reason the failure carries rather than by its message.
 *   - Java asserted one entry point, `adjusted(refData)`. This port has a second,
 *     [[AdjustableDate.toReader]], which is the same adjustment expressed as a value awaiting
 *     reference data, so both are driven through every row of the provider and the reader is
 *     additionally composed with another reader: supplying the reference data now, later, or in
 *     composition with a second lookup must not change what is computed. That is what makes the
 *     explicitly threaded reference data of AAP section 0.3.3 observable from a test.
 *   - `test_of_null` asserted `IllegalArgumentException` for a null in each argument position.
 *     `ArgCheck` has no `notNull` family in this port (AAP section 0.4.1) because Scala's types
 *     make those cases unrepresentable rather than checked, so each null site becomes a
 *     compile-time proof that the argument cannot be omitted or filled with a value of another
 *     kind, each paired with the call that does compile.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep of a Joda bean's properties,
 *     equality, hashing and rendering. There is no bean and no reflection in this port, so that
 *     sweep has no target and what it stood for is asserted directly over the same value the
 *     Java method swept.
 *   - `test_serialization` asserted Joda-Beans binary and JSON round trips. Joda wire
 *     compatibility is out of scope for this port, so the test is a circe round trip through the
 *     derived codec, which additionally pins the concrete document: the two Java property names,
 *     in declaration order. The property-based round trip over every codec-bearing type of the
 *     module lives in `json.JsonRoundTripSpec`; what a property cannot state is the document
 *     itself, which is what is stated here.
 *
 * ===The rendering of a date needing no adjustment===
 *
 * Three of the eight Java methods turn on one user-visible rule: an adjustable date carrying
 * [[BusinessDayAdjustment.NONE]] renders as its date alone, and every other adjustable date
 * renders as its date, the words ` adjusted by ` and its adjustment. Both forms are asserted
 * character for character here, and the short form is asserted to follow the value of the
 * adjustment rather than the factory that built it.
 *
 * @see [[BusinessDayAdjustmentSpec]] for the adjustment this date delegates to
 * @see [[HolidayCalendarIdSpec]] for the calendar resolution that adjustment delegates to
 */
class AdjustableDateSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The no-adjustment constant, named as the Java class named it. */
  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  /** The adjustment of the Java class: move forwards off a Saturday or a Sunday. */
  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  // the dates of the Java class, named as it named them, so that a row of the table below can be
  // read against the Java source without translating a date
  private val THU_2014_07_10: LocalDate = LocalDate.of(2014, 7, 10)
  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)
  private val TUE_2014_07_15: LocalDate = LocalDate.of(2014, 7, 15)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * The six rows of the Java `data_adjusted` provider.
   *
   * Each row is a date to adjust and the date the library being ported produces for it under
   * the `Following` convention against the `Sat/Sun` calendar. The Thursday, the Friday and the
   * Tuesday are business days and are returned unaltered; the Saturday and the Sunday are not,
   * and both roll forward to the Monday, which is itself one of the rows and so is pinned as a
   * date needing no adjustment as well as being the answer for the two that do.
   */
  private val dataAdjusted: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expected"),
    (THU_2014_07_10, THU_2014_07_10),
    (FRI_2014_07_11, FRI_2014_07_11),
    (SAT_2014_07_12, MON_2014_07_14),
    (SUN_2014_07_13, MON_2014_07_14),
    (MON_2014_07_14, MON_2014_07_14),
    (TUE_2014_07_15, TUE_2014_07_15))

  //-------------------------------------------------------------------------
  test("test_of_1arg") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_NONE

    // The rendering of a date needing no adjustment is the date alone, which is the Java form
    // character for character, and `Show` renders what `toString` renders.
    test.toString shouldBe "2014-07-11"
    Show[AdjustableDate].show(test) shouldBe "2014-07-11"

    // The one-argument factory is the constructor paired with the no-adjustment constant, and
    // adjusting such a date returns the date it was given.
    test shouldBe AdjustableDate(FRI_2014_07_11, BusinessDayAdjustment.NONE)
    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)
    test.toReader.run(REF_DATA) should haveValue(FRI_2014_07_11)

    // The no-holidays calendar the constant names is part of the minimal reference data as well
    // as the standard set, so a date needing no adjustment does not need the full set to adjust.
    test.adjusted(ReferenceData.minimal) should haveValue(FRI_2014_07_11)

    // A weekend date carrying the constant is also returned unaltered: what decides the answer
    // is the adjustment, not whether the date happens to be a business day anywhere.
    AdjustableDate.of(SAT_2014_07_12).adjusted(REF_DATA) should haveValue(SAT_2014_07_12)
    AdjustableDate.of(SAT_2014_07_12).toString shouldBe "2014-07-12"
  }

  test("test_of_2args_withAdjustment") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // The full rendering: the date, the words ` adjusted by `, and the adjustment's own
    // rendering. This is the Java expected string character for character.
    test.toString shouldBe "2014-07-11 adjusted by Following using calendar Sat/Sun"
    Show[AdjustableDate].show(test) shouldBe "2014-07-11 adjusted by Following using calendar Sat/Sun"

    // The date given is a Friday, so the adjustment has nothing to do and the adjusted date is
    // the unadjusted one - which is the assertion the Java method made.
    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)
    test.toReader.run(REF_DATA) should haveValue(FRI_2014_07_11)

    // The unadjusted date is kept as it was given even where it is a weekend, and the long form
    // is what such a date renders as; the adjusted date is reached only by adjusting.
    val weekend: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    weekend.unadjusted shouldBe SAT_2014_07_12
    weekend.toString shouldBe "2014-07-12 adjusted by Following using calendar Sat/Sun"
    weekend.adjusted(REF_DATA) should haveValue(MON_2014_07_14)

    // `of` is the factory of the library being ported, and construction being total the
    // case-class constructor is the same value - which is what lets a ported call site read
    // unchanged while new code may use either.
    test shouldBe AdjustableDate(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
  }

  test("test_of_2args_withNoAdjustment") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_NONE)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_NONE

    // Naming the no-adjustment constant explicitly is the same value as leaving it out, so the
    // rendering is the short form and is identical to the one-argument case.
    test.toString shouldBe "2014-07-11"
    test shouldBe AdjustableDate.of(FRI_2014_07_11)
    test.toString shouldBe AdjustableDate.of(FRI_2014_07_11).toString
    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)

    // The short form follows the value of the adjustment and not the constant it was taken
    // from: an adjustment built from its two parts is equal to `NONE` and renders the same way.
    val rebuilt: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.NO_HOLIDAYS)
    rebuilt shouldBe BDA_NONE
    AdjustableDate.of(FRI_2014_07_11, rebuilt).toString shouldBe "2014-07-11"

    // And an adjustment that adjusts nothing but names some other calendar is not `NONE`, so it
    // renders in the long form - exactly as the adjustment itself does, which is where that
    // deliberate lack of normalisation is documented.
    val noAdjustSatSun: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.SAT_SUN)
    noAdjustSatSun should not be BDA_NONE
    AdjustableDate.of(FRI_2014_07_11, noAdjustSatSun).toString shouldBe
      "2014-07-11 adjusted by NoAdjust using calendar Sat/Sun"
    AdjustableDate.of(SAT_2014_07_12, noAdjustSatSun).adjusted(REF_DATA) should
      haveValue(SAT_2014_07_12)
  }

  test("test_of_null") {
    // The Java method asserted `IllegalArgumentException` for a null in each argument position:
    // `of(null)`, `of(null, adjustment)`, `of(date, null)` and `of(null, null)`. Scala's types
    // make each of those cases unrepresentable rather than checked - `ArgCheck` has no `notNull`
    // family in this port (AAP section 0.4.1), because construction is total and a caller
    // cannot reach either factory without supplying every argument with a value of its declared
    // type. What remains to prove is therefore the compile-time requirement itself, and it is
    // proved once for each null site of the Java method: nothing here passes a null.
    assertDoesNotCompile("AdjustableDate.of()") // of(null): no date at all
    assertDoesNotCompile("AdjustableDate.of(BDA_FOLLOW_SAT_SUN)") // of(null, adjustment): the date
    assertDoesNotCompile("AdjustableDate.of(FRI_2014_07_11, HolidayCalendarIds.SAT_SUN)") // of(date, null)
    assertDoesNotCompile("AdjustableDate(FRI_2014_07_11)") // the adjustment cannot be left out
    assertDoesNotCompile("AdjustableDate()") // of(null, null): neither argument supplied

    // Each of the five proofs above is paired with a call that differs from it only in the
    // arguments supplied, so that none of them can be passing because of a typo or a name that
    // is not in scope rather than because of the requirement it is meant to prove.
    assertCompiles("AdjustableDate.of(FRI_2014_07_11)")
    assertCompiles("AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)")
    assertCompiles("AdjustableDate(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)")

    // And the values those calls produce, so that the test states what the requirement buys:
    // two total factories, neither of which can fail.
    AdjustableDate.of(FRI_2014_07_11).unadjusted shouldBe FRI_2014_07_11
    AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN).adjustment shouldBe BDA_FOLLOW_SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("test_adjusted") {
    // The Java parameterised method: every row of `data_adjusted`, against the adjustment the
    // Java class held. Java asserted the direct path; this port asserts the reader as well, and
    // the two must agree row by row, because the reader is the same adjustment expressed as a
    // value awaiting its reference data rather than a second implementation of it.
    forAll(dataAdjusted) { (input: LocalDate, expected: LocalDate) =>
      val test: AdjustableDate = AdjustableDate.of(input, BDA_FOLLOW_SAT_SUN)

      withClue(s"$test: ") {
        val outcome: Either[Failure, LocalDate] = test.adjusted(REF_DATA)
        outcome should haveValue(expected)
        test.toReader.run(REF_DATA) should haveValue(expected)
        test.toReader.run(REF_DATA) shouldBe outcome

        // The unadjusted date is untouched by adjusting, which is the point of holding both: the
        // date as agreed and the date as it will settle are recoverable from one value.
        test.unadjusted shouldBe input
      }
    }

    // Two readers compose, which is the reason the reader form exists: several adjustments are
    // assembled while no reference data is available, and the data is supplied once, to the
    // composition, rather than to each of them. Both rows here are rows of the provider above -
    // the Saturday and the Sunday, which both roll forward to the Monday.
    val saturday: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    val sunday: AdjustableDate = AdjustableDate.of(SUN_2014_07_13, BDA_FOLLOW_SAT_SUN)
    val both: RefDataReader[(LocalDate, LocalDate)] = (saturday.toReader, sunday.toReader).tupled
    both.run(REF_DATA) should haveValue((MON_2014_07_14, MON_2014_07_14))

    // A composition need not produce the dates themselves: it produces whatever is computed
    // from them, the reference data having been supplied once for the whole expression.
    val sameDay: RefDataReader[Boolean] =
      (saturday.toReader, sunday.toReader).mapN((first, second) => first == second)
    sameDay.run(REF_DATA) should haveValue(true)

    // A single reader maps as well, so one adjusted date feeds a later calculation without the
    // reference data being named again.
    saturday.toReader.map(adjusted => adjusted.getDayOfWeek).run(REF_DATA) should
      haveValue(MON_2014_07_14.getDayOfWeek)

    // Adjusting against real holiday data, which the Java provider never reached because it
    // used a weekend-only calendar: the London summer bank holiday of 2014 falls on Monday the
    // 25th, so a date adjusted forward off the Saturday before it reaches the Tuesday.
    val london: AdjustableDate =
      AdjustableDate.of(
        LocalDate.of(2014, 8, 23),
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO))
    london.adjusted(REF_DATA) should haveValue(LocalDate.of(2014, 8, 26))
    london.toReader.run(REF_DATA) should haveValue(LocalDate.of(2014, 8, 26))

    // The failure path, which the Java class never exercised because the Java method threw
    // `ReferenceDataNotFoundException`. A calendar the reference data cannot supply is reported
    // as a missing-data failure - asserted by the reason the failure carries, by value - through
    // both entry points, and nothing is raised.
    val unknown: AdjustableDate =
      AdjustableDate.of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))
    unknown.adjusted(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjusted(REF_DATA)

    // A composition fails as a whole where any one of its parts cannot be resolved, so a caller
    // supplying the data once learns of the missing calendar once.
    (saturday.toReader, unknown.toReader).tupled.run(REF_DATA) should
      beFailureWith(FailureReason.MISSING_DATA)

    // Empty reference data holds nothing at all - not even the no-holidays calendar - so even a
    // date needing no adjustment fails against it. That is the behaviour of the library being
    // ported rather than a defect of this port, and it is reported rather than thrown.
    saturday.adjusted(ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
    AdjustableDate.of(FRI_2014_07_11).adjusted(ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    // The four values of the Java method, built as it built them: one value twice over, one
    // differing in its date, and one differing in its adjustment.
    val a1: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val a2: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val b: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    val c: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_NONE)

    // the three Java assertions
    a1.equals(a2) shouldBe true
    a1.equals(b) shouldBe false
    a1.equals(c) shouldBe false

    // Equality is by field and hashing agrees with it, so a value built twice is one value for
    // every purpose a map or a set puts it to.
    a1 shouldBe a2
    a1.hashCode shouldBe a2.hashCode
    a1 should not be b
    a1 should not be c

    // The `Hash` instance is the type's only equality-bearing instance, and `Eq` is obtained
    // from it by subtyping, so what it decides must be what `equals` decides.
    Hash[AdjustableDate].eqv(a1, a2) shouldBe true
    Hash[AdjustableDate].eqv(a1, b) shouldBe false
    Hash[AdjustableDate].eqv(a1, c) shouldBe false
    Hash[AdjustableDate].hash(a1) shouldBe Hash[AdjustableDate].hash(a2)

    // A value of another type is not equal to an adjustable date. The comparison is written
    // through `equals`, which takes `Any`, because `==` between unrelated types is a compile
    // error under the warning settings of this build.
    a1.equals("2014-07-11 adjusted by Following using calendar Sat/Sun") shouldBe false
    a1.equals(FRI_2014_07_11) shouldBe false
    a1.equals(BDA_FOLLOW_SAT_SUN) shouldBe false

    // What is held is the agreement, so two dates that adjust to the same day under different
    // adjustments are still two values - which is the reason `c`, whose adjustment differs and
    // whose adjusted date does not, is unequal to `a1`.
    val modified: AdjustableDate =
      AdjustableDate.of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN))
    a1.adjusted(REF_DATA) shouldBe modified.adjusted(REF_DATA)
    c.adjusted(REF_DATA) shouldBe a1.adjusted(REF_DATA)
    a1 should not be modified
    Hash[AdjustableDate].eqv(a1, modified) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(AdjustableDate.of(FRI_2014_07_11,
    // BDA_FOLLOW_SAT_SUN))`: a reflective sweep over a Joda bean's properties, equality, hashing
    // and rendering. There is no bean and no reflection in this port, so that sweep has no
    // target and the properties it stood for are asserted directly, over exactly the value the
    // Java method named.
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val same: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val otherDate: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    val otherAdjustment: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_NONE)

    // The two properties read back as they were given.
    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // Equality, hashing and their agreement, which the sweep read through the bean.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[AdjustableDate].eqv(test, same) shouldBe true
    Hash[AdjustableDate].hash(test) shouldBe Hash[AdjustableDate].hash(same)
    Hash[AdjustableDate].eqv(test, otherDate) shouldBe false
    Hash[AdjustableDate].eqv(test, otherAdjustment) shouldBe false
    test.equals(FRI_2014_07_11) shouldBe false

    // `copy` changes one field and leaves the other, which is the property sweep's write half.
    // It is public here because this type is total (AAP section 0.3.3 kind `[T]`): both fields
    // are required, neither can be rejected, and there is no invariant for a validating factory
    // to protect - unlike the plural `AdjustableDates`, whose non-empty list is such an
    // invariant and which therefore has no `copy` at all.
    test.copy(unadjusted = SAT_2014_07_12) shouldBe otherDate
    test.copy(adjustment = BDA_NONE) shouldBe otherAdjustment
    test.copy(unadjusted = SAT_2014_07_12).adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.copy(adjustment = BDA_NONE).unadjusted shouldBe FRI_2014_07_11

    // The three ways of naming one value - the factory, the constructor and `copy` - agree, and
    // a copy that reaches the no-adjustment constant reaches the short rendering with it.
    AdjustableDate(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN) shouldBe test
    AdjustableDate.of(FRI_2014_07_11).copy(adjustment = BDA_FOLLOW_SAT_SUN) shouldBe test
    test.copy(adjustment = BDA_NONE).toString shouldBe "2014-07-11"

    // Rendering, which the sweep read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting an adjustable date into a message must not differ, in
    // either of the two forms.
    test.toString shouldBe "2014-07-11 adjusted by Following using calendar Sat/Sun"
    Show[AdjustableDate].show(test) shouldBe test.toString
    Show[AdjustableDate].show(otherAdjustment) shouldBe otherAdjustment.toString
    Show[AdjustableDate].show(otherAdjustment) shouldBe "2014-07-11"

    // The behaviour the sweep could not reach at all: adjusting, through both entry points.
    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)
    otherDate.adjusted(REF_DATA) should haveValue(MON_2014_07_14)
    otherDate.toReader.run(REF_DATA) should haveValue(MON_2014_07_14)
  }

  test("test_serialization") {
    // The Java method was `assertSerialization`, a Joda-Beans binary and JSON round trip. Joda
    // wire compatibility is out of scope for this port, so the round trip is the derived circe
    // codec, and the document it produces is pinned here: an object of the two fields under the
    // names the Java bean declared, in declaration order, the date as an ISO-8601 string and the
    // adjustment as the object its own codec writes.
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("unadjusted", "adjustment"))
    encoded.noSpaces shouldBe
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    // Neither field is optional, and the encoder is wrapped in this port's drop-nulls policy in
    // any case, so no null can appear in the document of any adjustable date.
    encoded.noSpaces should not include "null"
    AdjustableDate.of(FRI_2014_07_11).asJson.noSpaces should not include "null"

    // The rendering rule of `toString` is not the document: a date carrying the no-adjustment
    // constant still serializes both of its fields, so that what is decoded is the value that
    // was encoded rather than the abbreviation it renders as.
    AdjustableDate.of(FRI_2014_07_11).asJson.noSpaces shouldBe
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

    // The round trip itself, for the value the Java method serialized, for a date carrying the
    // no-adjustment constant, and for a weekend date whose adjustment does something.
    decode[AdjustableDate](encoded.noSpaces) shouldBe Right(test)
    decode[AdjustableDate](
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""") shouldBe
      Right(test)
    decode[AdjustableDate](AdjustableDate.of(FRI_2014_07_11).asJson.noSpaces) shouldBe
      Right(AdjustableDate.of(FRI_2014_07_11))
    val weekend: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    decode[AdjustableDate](weekend.asJson.noSpaces) shouldBe Right(weekend)
    decode[AdjustableDate](weekend.asJson.noSpaces).map(value => value.adjusted(REF_DATA)) shouldBe
      Right(weekend.adjusted(REF_DATA))

    // Equal values encode to identical bytes, neither field having a representation that
    // depends on how it was built.
    AdjustableDate
      .of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarId.of("Sat/Sun")))
      .asJson
      .noSpaces shouldBe encoded.noSpaces

    // Both fields are required, so a document missing either is rejected rather than defaulted,
    // and an adjustable date is an object rather than the string it renders as.
    decode[AdjustableDate]("""{"unadjusted":"2014-07-11"}""").isLeft shouldBe true
    decode[AdjustableDate](
      """{"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""").isLeft shouldBe true
    decode[AdjustableDate]("{}").isLeft shouldBe true
    Json.fromString("2014-07-11").as[AdjustableDate].isLeft shouldBe true

    // Text that is not a date, and an adjustment naming no convention of the closed family, are
    // both rejected by the decoder rather than carried into a value.
    val notADate: String =
      """{"unadjusted":"not-a-date","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""
    val notAConvention: String =
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"Rubbish","calendar":"Sat/Sun"}}"""
    decode[AdjustableDate](notADate).isLeft shouldBe true
    decode[AdjustableDate](notAConvention).isLeft shouldBe true

    // A calendar this library knows nothing about is a fact about the reference data rather than
    // about the document, so it decodes and fails only when the date is adjusted.
    val unknown: Either[io.circe.Error, AdjustableDate] =
      decode[AdjustableDate](
        """{"unadjusted":"2014-07-11","adjustment":{"convention":"Following","calendar":"XXXX"}}""")
    unknown shouldBe Right(
      AdjustableDate.of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)))
    unknown.map(value => value.adjusted(REF_DATA).isLeft) shouldBe Right(true)
  }
}
