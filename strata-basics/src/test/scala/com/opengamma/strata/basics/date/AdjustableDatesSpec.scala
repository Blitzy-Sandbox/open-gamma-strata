/*
 * Copyright (C) 2021 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.apply._

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[AdjustableDates]], ported from the Java `AdjustableDatesTest`.
 *
 * The three methods of the Java class are kept, each under the name the Java method had -
 * `test_of_noAdjustment`, `test_of_withAdjustment` and `coverage` - so that a Java test method
 * and a test of this suite stay in one-to-one correspondence and the method-level traceability
 * the migration manifest records resolves on the pair of suite class and test name. Nothing is
 * added under a name of its own: everything this port asserts beyond the Java assertions belongs
 * to whichever of the three methods already owned that ground.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - [[AdjustableDates.unadjusted]] is a `cats.data.NonEmptyList` rather than a list, because
 *     there is always at least one date and AAP section 0.3.3 carries such an invariant in the
 *     type rather than restating it as a check. Every assertion over the dates is therefore
 *     against a `NonEmptyList`, and a list is never compared with one.
 *   - All five factories return `ResultNec[AdjustableDates]` where the Java factories returned a
 *     value and threw for a run of dates that was out of order, repeated a date, or held no date
 *     at all. Every construction in this suite therefore threads the error channel: the outcome
 *     is asserted to be a success and the value is read out of it by folding, never by a partial
 *     accessor, so a fixture that fails to build is reported as a test failure naming the reasons
 *     rather than raising an error from somewhere else in the suite.
 *   - The Java class had one entry point for adjustment, `adjusted(refData)`. This port has a
 *     second, [[AdjustableDates.toReader]], the same adjustment expressed as a value awaiting its
 *     reference data, so both are asserted and the reader is additionally composed with another
 *     reader: supplying the reference data now, later, or in composition must not change what is
 *     computed. That is what makes the explicitly threaded reference data of AAP section 0.3.3
 *     observable from a test.
 *   - `coverage` called `coverImmutableBean`, `coverBeanEquals` and `assertSerialization` -
 *     reflective sweeps of a Joda bean's properties, equality, hashing and rendering, and a
 *     Joda-Beans wire round trip. There is no bean, no reflection and no Joda compatibility in
 *     this port, so those sweeps have no Scala target and what they stood for is asserted
 *     directly over the same values the Java method swept.
 *
 * ===What `coverage` carries beyond the sweep===
 *
 * The Java roster is three methods and this port keeps it at three, so the three guarantees of
 * the ported type that no Java method reaches are asserted in `coverage`, which is where the
 * sweep they replace used to be:
 *
 *   - the rejection of a run of dates that is not strictly increasing, in both of its forms - a
 *     date out of order and a date that repeats - reported as a `Failure.Invalid` and asserted by
 *     the reason the failure carries, by value;
 *   - the de-duplication [[AdjustableDates.adjusted]] performs, which is the behaviour of the
 *     library being ported and is easy to lose: two distinct unadjusted dates a weekend apart
 *     adjust onto the same Monday, and the result holds that Monday once;
 *   - the JSON round trip through the codec pair, with the document pinned and an invalid payload
 *     rejected by the validating decoder rather than carried into a value.
 *
 * The property-based round trip over every codec-bearing type of the module lives in
 * `json.JsonRoundTripSpec`, the per-type matrix of invalid inputs in `SmartConstructorSpec`, one
 * case per failable method in `FailableSurfaceSpec`, and the compile-level proofs that this type
 * has no public `apply` or `copy` in `ApiSurfaceSpec`; what those cannot state is the concrete
 * document and the concrete dates, which is what is stated here.
 *
 * @see [[AdjustableDateSpec]] for the single-date form of the same pairing
 * @see [[BusinessDayAdjustmentSpec]] for the adjustment these dates delegate to
 */
class AdjustableDatesSpec extends AnyFunSuite with Matchers {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The no-adjustment constant, named as the Java class named it. */
  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  /** The adjustment of the Java class: move forwards off a Saturday or a Sunday. */
  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  // the dates of the Java class, named as it named them, so that an assertion here can be read
  // against the Java source without translating a date; the Saturday is this port's addition,
  // needed because a weekend pair is what makes the de-duplication of `adjusted` observable
  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * The wording the factories report for a run of dates that is not strictly increasing.
   *
   * This is the message of the validator of the bean being ported, character for character, and
   * it covers both ways the run can be wrong, a single test of strict ordering rejecting a date
   * that is out of order and a date that repeats alike.
   */
  private val OrderMessage: String = "Dates must be in order and without duplicates"

  /** The wording the list factories report for a collection holding no date. */
  private val EmptyMessage: String = "Argument iterable 'unadjusted' must not be empty"

  //-------------------------------------------------------------------------
  test("test_of_noAdjustment") {
    // The Java method, assertion for assertion: the varargs factory with no adjustment, its two
    // properties read back, the expansion into single adjustable dates, the rendering, the
    // adjusted dates, and the equality of the varargs and list factories.
    val outcome: ResultNec[AdjustableDates] = AdjustableDates.of(FRI_2014_07_11, SUN_2014_07_13)
    outcome should beSuccess
    val test: AdjustableDates = unwrap(outcome)

    test.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)
    test.adjustment shouldBe BDA_NONE

    // The expansion: one adjustable date per unadjusted date, in the same order, each carrying
    // the adjustment held here - which is the no-adjustment constant, so each is the value
    // `AdjustableDate.of` produces from its date alone.
    test.toAdjustableDateList shouldBe
      NonEmptyList.of(AdjustableDate.of(FRI_2014_07_11), AdjustableDate.of(SUN_2014_07_13))
    test.toAdjustableDateList.map(element => element.adjustment) shouldBe
      NonEmptyList.of(BDA_NONE, BDA_NONE)
    test.toAdjustableDateList.map(element => element.unadjusted) shouldBe test.unadjusted

    // The rendering, character for character: dates carrying the no-adjustment constant render
    // as the bracketed list alone, because naming an adjustment that adjusts nothing would say
    // something untrue about it.
    test.toString shouldBe "[2014-07-11, 2014-07-13]"

    // The adjusted dates. No adjustment applies, so both dates are returned as they stand even
    // though the second of them is a Sunday, and they are returned whatever reference data the
    // adjustment is run against.
    test.adjusted(REF_DATA) should haveValue(NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))
    test.adjusted(ReferenceData.minimal) should
      haveValue(NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))

    // The Java equality assertion: the list factory produces the same value as the varargs
    // factory. Both the values and the outcomes carrying them are equal, the outcome being the
    // shape a caller actually holds.
    val fromList: ResultNec[AdjustableDates] =
      AdjustableDates.of(List(FRI_2014_07_11, SUN_2014_07_13))
    fromList should beSuccess
    unwrap(fromList) shouldBe test
    fromList shouldBe outcome

    // The fifth factory of this port, which takes the shape the type holds and is the one the
    // other four funnel into, reaches the same value from the same dates.
    val fromNonEmpty: ResultNec[AdjustableDates] =
      AdjustableDates.of(BDA_NONE, NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))
    fromNonEmpty should beSuccess
    unwrap(fromNonEmpty) shouldBe test

    // Naming the no-adjustment constant explicitly is the same as not naming an adjustment at
    // all, in both the varargs and the list form, which is what lets the rendering rule turn on
    // the value of the adjustment rather than on the factory that built the value.
    unwrap(AdjustableDates.of(BDA_NONE, FRI_2014_07_11, SUN_2014_07_13)) shouldBe test
    unwrap(AdjustableDates.of(BDA_NONE, List(FRI_2014_07_11, SUN_2014_07_13))) shouldBe test

    // A single date is a run of one: the varargs factory cannot be given an empty run, since the
    // first date is a parameter of its own, so it reaches the check for order and nothing else.
    val single: AdjustableDates = unwrap(AdjustableDates.of(FRI_2014_07_11))
    single.unadjusted shouldBe NonEmptyList.one(FRI_2014_07_11)
    single.adjustment shouldBe BDA_NONE
    single.toAdjustableDateList shouldBe NonEmptyList.one(AdjustableDate.of(FRI_2014_07_11))
    single.toString shouldBe "[2014-07-11]"
    single.adjusted(REF_DATA) should haveValue(NonEmptyList.one(FRI_2014_07_11))
  }

  //-------------------------------------------------------------------------
  test("test_of_withAdjustment") {
    // The Java method, assertion for assertion, with the adjustment the Java class held: the
    // varargs factory, the two properties, the expansion with the adjustment propagated into
    // every element, the rendering, the adjusted dates, and the equality of the two factories.
    val outcome: ResultNec[AdjustableDates] =
      AdjustableDates.of(BDA_FOLLOW_SAT_SUN, FRI_2014_07_11, SUN_2014_07_13)
    outcome should beSuccess
    val test: AdjustableDates = unwrap(outcome)

    test.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // The adjustment is a property of the set, so it reaches every element of the expansion
    // rather than only the first of them.
    test.toAdjustableDateList shouldBe
      NonEmptyList.of(
        AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN),
        AdjustableDate.of(SUN_2014_07_13, BDA_FOLLOW_SAT_SUN))
    test.toAdjustableDateList.map(element => element.adjustment) shouldBe
      NonEmptyList.of(BDA_FOLLOW_SAT_SUN, BDA_FOLLOW_SAT_SUN)

    // The rendering, character for character: the bracketed list, the words ` adjusted by ` and
    // the adjustment, which renders as its convention and the name of its calendar.
    test.toString shouldBe "[2014-07-11, 2014-07-13] adjusted by Following using calendar Sat/Sun"

    // The adjusted dates: the Friday is a business day of the Sat/Sun calendar and is returned
    // unaltered, and the Sunday rolls forward to the Monday under the following convention.
    val adjusted: Either[Failure, NonEmptyList[LocalDate]] = test.adjusted(REF_DATA)
    adjusted should haveValue(NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14))

    // The unadjusted dates are untouched by adjusting, which is the point of holding both: the
    // dates as agreed and the dates as they will settle are recoverable from one value.
    test.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)

    // Adjusting the members of the expansion one at a time reaches the same dates, and keeps the
    // correspondence between an unadjusted date and its adjusted date that `adjusted` does not
    // preserve. It costs one calendar resolution per date, which is why `adjusted` exists.
    val perDate: NonEmptyList[Either[Failure, LocalDate]] =
      test.toAdjustableDateList.map(element => element.adjusted(REF_DATA))
    val expectedPerDate: NonEmptyList[Either[Failure, LocalDate]] =
      NonEmptyList.of(Right(FRI_2014_07_11), Right(MON_2014_07_14))
    perDate shouldBe expectedPerDate

    // The Java equality assertion: the list factory produces the same value as the varargs
    // factory, and so does the non-empty-list factory the other four funnel into.
    val fromList: ResultNec[AdjustableDates] =
      AdjustableDates.of(BDA_FOLLOW_SAT_SUN, List(FRI_2014_07_11, SUN_2014_07_13))
    fromList should beSuccess
    unwrap(fromList) shouldBe test
    fromList shouldBe outcome
    unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))) shouldBe
      test

    // The reader: the same adjustment expressed as a value awaiting its reference data. It is
    // not a second implementation of adjustment - it delegates - so it must agree with the
    // direct path date for date, and does.
    val reader: RefDataReader[NonEmptyList[LocalDate]] = test.toReader
    reader.run(REF_DATA) should haveValue(NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14))
    reader.run(REF_DATA) shouldBe adjusted

    // Two readers compose, which is the reason the reader form exists: several adjustments are
    // assembled while no reference data is available, and the data is supplied once, to the
    // composition, rather than to each of them.
    val saturday: AdjustableDates = unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, SAT_2014_07_12))
    val both: RefDataReader[(NonEmptyList[LocalDate], NonEmptyList[LocalDate])] =
      (test.toReader, saturday.toReader).tupled
    both.run(REF_DATA) should
      haveValue((NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14), NonEmptyList.one(MON_2014_07_14)))

    // A composition need not produce the dates themselves: it produces whatever is computed from
    // them, the reference data having been supplied once for the whole expression.
    val sameFinalDate: RefDataReader[Boolean] =
      (test.toReader, saturday.toReader).mapN((first, second) => first.last == second.last)
    sameFinalDate.run(REF_DATA) should haveValue(true)

    // A single reader maps as well, so the adjusted dates feed a later calculation without the
    // reference data being named again.
    reader.map(dates => dates.size).run(REF_DATA) should haveValue(2)

    // Adjusting against real holiday data, which the Java method never reached because it used a
    // weekend-only calendar: the London summer bank holiday of 2014 falls on Monday the 25th, so
    // the Saturday before it rolls forward to the Tuesday while the Friday before stands.
    val london: AdjustableDates =
      unwrap(
        AdjustableDates.of(
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO),
          LocalDate.of(2014, 8, 22),
          LocalDate.of(2014, 8, 23)))
    london.adjusted(REF_DATA) should
      haveValue(NonEmptyList.of(LocalDate.of(2014, 8, 22), LocalDate.of(2014, 8, 26)))
    london.toReader.run(REF_DATA) should
      haveValue(NonEmptyList.of(LocalDate.of(2014, 8, 22), LocalDate.of(2014, 8, 26)))

    // The failure path, which the Java class never exercised because the Java method threw. A
    // calendar the reference data cannot supply is reported as a missing-data failure - asserted
    // by the reason the failure carries, by value - through both entry points, and nothing is
    // raised; the failure of one part of a composition is the failure of the whole, so a caller
    // supplying the data once learns of the missing calendar once.
    val unknown: AdjustableDates =
      unwrap(
        AdjustableDates.of(
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR),
          FRI_2014_07_11,
          SUN_2014_07_13))
    unknown.adjusted(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjusted(REF_DATA)
    (test.toReader, unknown.toReader).tupled.run(REF_DATA) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(AdjustableDates.of(FRI_2014_07_11))`,
    // `coverBeanEquals(test, test2)` and `assertSerialization(test2)`: reflective sweeps over a
    // Joda bean's properties, equality, hashing and rendering, and a Joda-Beans wire round trip.
    // There is no bean, no reflection and no Joda wire compatibility in this port, so none of
    // the three has a Scala target, and what they stood for is asserted directly over exactly
    // the values the Java method named.
    val test: AdjustableDates = unwrap(AdjustableDates.of(FRI_2014_07_11))
    val test2: AdjustableDates =
      unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, FRI_2014_07_11, SUN_2014_07_13))
    val same: AdjustableDates = unwrap(AdjustableDates.of(FRI_2014_07_11))

    // The two properties read back as they were given, on both of the swept values.
    test.unadjusted shouldBe NonEmptyList.one(FRI_2014_07_11)
    test.adjustment shouldBe BDA_NONE
    test2.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)
    test2.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // Equality, hashing and their agreement, which the sweeps read through the bean. `Hash` is
    // the type's single equality-bearing instance, so `Eq` is obtained from it by subtyping and
    // the two cannot disagree.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[AdjustableDates].eqv(test, same) shouldBe true
    Hash[AdjustableDates].hash(test) shouldBe Hash[AdjustableDates].hash(same)
    Hash[AdjustableDates].eqv(test, test2) shouldBe false
    test.equals(FRI_2014_07_11) shouldBe false

    // Both fields take part: two values differing only in their dates are unequal, and so are
    // two differing only in their adjustment - even though, these dates being business days of
    // neither calendar in question, the second pair adjusts to different dates only because one
    // of them adjusts at all.
    val otherDates: AdjustableDates = unwrap(AdjustableDates.of(SUN_2014_07_13))
    val sameDatesNoAdjustment: AdjustableDates =
      unwrap(AdjustableDates.of(FRI_2014_07_11, SUN_2014_07_13))
    Hash[AdjustableDates].eqv(test, otherDates) shouldBe false
    Hash[AdjustableDates].eqv(sameDatesNoAdjustment, test2) shouldBe false
    sameDatesNoAdjustment.unadjusted shouldBe test2.unadjusted

    // Rendering, which the sweeps read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting a run of adjustable dates into a message must not differ,
    // in either of the two forms.
    Show[AdjustableDates].show(test) shouldBe test.toString
    Show[AdjustableDates].show(test2) shouldBe test2.toString
    Show[AdjustableDates].show(test) shouldBe "[2014-07-11]"
    Show[AdjustableDates].show(test2) shouldBe
      "[2014-07-11, 2014-07-13] adjusted by Following using calendar Sat/Sun"

    //-----------------------------------------------------------------------
    // The invariant: the dates have to be strictly increasing. A date out of order and a date
    // that repeats are the two ways one run can be wrong, and a single test of strict ordering
    // rejects both, so both are reported under one reason - asserted by value rather than by the
    // text of the message.
    val outOfOrder: ResultNec[AdjustableDates] =
      AdjustableDates.of(SUN_2014_07_13, FRI_2014_07_11)
    val duplicated: ResultNec[AdjustableDates] =
      AdjustableDates.of(FRI_2014_07_11, FRI_2014_07_11)
    outOfOrder should beFailureWith(FailureReason.INVALID)
    duplicated should beFailureWith(FailureReason.INVALID)

    // The accumulating shape is respected while the chain holds one failure: the two conditions
    // this type checks are mutually exclusive - a collection holding no date holds no pair of
    // dates to be out of order - so a rejected run reports exactly one reason, and that reason is
    // the whole failure, attributes included.
    failuresOf(outOfOrder) shouldBe List(Failure.Invalid(OrderMessage))
    failuresOf(duplicated) shouldBe List(Failure.Invalid(OrderMessage))

    // Every factory reports it, the rejection being a property of the dates rather than of the
    // route taken into the type.
    AdjustableDates.of(List(SUN_2014_07_13, FRI_2014_07_11)) should
      beFailureWith(FailureReason.INVALID)
    AdjustableDates.of(BDA_FOLLOW_SAT_SUN, SUN_2014_07_13, FRI_2014_07_11) should
      beFailureWith(FailureReason.INVALID)
    AdjustableDates.of(BDA_FOLLOW_SAT_SUN, List(FRI_2014_07_11, FRI_2014_07_11)) should
      beFailureWith(FailureReason.INVALID)
    AdjustableDates.of(BDA_FOLLOW_SAT_SUN, NonEmptyList.of(SUN_2014_07_13, FRI_2014_07_11)) should
      beFailureWith(FailureReason.INVALID)

    // A collection holding no date is the other rejected input, and only the two list factories
    // can express it: the varargs factories take their first date as a parameter of its own.
    val empty: ResultNec[AdjustableDates] = AdjustableDates.of(List.empty[LocalDate])
    empty should beFailureWith(FailureReason.INVALID)
    failuresOf(empty) shouldBe List(Failure.Invalid(EmptyMessage))
    failuresOf(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, List.empty[LocalDate])) shouldBe
      List(Failure.Invalid(EmptyMessage))

    // A run of one is vacuously increasing, and a run whose dates merely touch at the ends of
    // consecutive days is increasing too, so neither is rejected.
    AdjustableDates.of(List(FRI_2014_07_11)) should beSuccess
    AdjustableDates.of(FRI_2014_07_11, SAT_2014_07_12, SUN_2014_07_13) should beSuccess

    //-----------------------------------------------------------------------
    // The de-duplication of `adjusted`, which the library being ported performed with a distinct
    // stream: the unadjusted dates are distinct by the invariant above, but two of them a weekend
    // apart adjust onto the same Monday, and the result holds that Monday once. The list returned
    // is therefore shorter than the unadjusted run, and is the set of business days these dates
    // fall on rather than one date per unadjusted date.
    val weekend: AdjustableDates =
      unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, SAT_2014_07_12, SUN_2014_07_13))
    weekend.unadjusted.size shouldBe 2
    weekend.adjusted(REF_DATA) should haveValue(NonEmptyList.one(MON_2014_07_14))
    weekend.toReader.run(REF_DATA) should haveValue(NonEmptyList.one(MON_2014_07_14))

    // Only the dates that collide collapse: a longer run keeps every business day it reaches,
    // once each, in the order of first occurrence.
    val run: AdjustableDates =
      unwrap(
        AdjustableDates.of(BDA_FOLLOW_SAT_SUN, FRI_2014_07_11, SAT_2014_07_12, SUN_2014_07_13))
    run.unadjusted.size shouldBe 3
    run.adjusted(REF_DATA) should haveValue(NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14))

    // The correspondence the de-duplication discards survives in the expansion, which is the
    // form to use where it has to: both weekend dates are still there, each with its own adjusted
    // date, and both of those are the Monday.
    val weekendPerDate: NonEmptyList[Either[Failure, LocalDate]] =
      weekend.toAdjustableDateList.map(element => element.adjusted(REF_DATA))
    val expectedWeekendPerDate: NonEmptyList[Either[Failure, LocalDate]] =
      NonEmptyList.of(Right(MON_2014_07_14), Right(MON_2014_07_14))
    weekendPerDate shouldBe expectedWeekendPerDate
    weekend.toAdjustableDateList.size shouldBe 2

    // De-duplication is not sorting and not a licence to reorder: a run whose dates need no
    // adjustment is returned exactly as it is held.
    sameDatesNoAdjustment.adjusted(REF_DATA) should
      haveValue(NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))

    //-----------------------------------------------------------------------
    // The Java `assertSerialization` was a Joda-Beans binary and JSON round trip. Joda wire
    // compatibility is out of scope for this port, so the round trip is the circe codec pair, and
    // the document it produces is pinned here: an object of the two fields under the names the
    // Java bean declared, in declaration order, the dates as an array of ISO-8601 strings and the
    // adjustment as the object its own codec writes.
    val encoded: Json = test2.asJson
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("unadjusted", "adjustment"))
    encoded.noSpaces shouldBe
      """{"unadjusted":["2014-07-11","2014-07-13"],""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    // Neither field is optional, and the encoder is wrapped in this port's drop-nulls policy in
    // any case, so no null can appear in the document of any run of adjustable dates.
    encoded.noSpaces should not include "null"
    test.asJson.noSpaces should not include "null"

    // The rendering rule of `toString` is not the document: a run carrying the no-adjustment
    // constant still serializes both of its fields, so that what is decoded is the value that was
    // encoded rather than the abbreviation it renders as.
    test.asJson.noSpaces shouldBe
      """{"unadjusted":["2014-07-11"],""" +
        """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

    // The round trip itself, for the value the Java method serialized, for a run carrying the
    // no-adjustment constant, and for the weekend run whose adjustment collapses two dates into
    // one - the decoded value adjusting to what the encoded value adjusted to.
    decode[AdjustableDates](encoded.noSpaces) shouldBe Right(test2)
    decode[AdjustableDates](test.asJson.noSpaces) shouldBe Right(test)
    decode[AdjustableDates](weekend.asJson.noSpaces) shouldBe Right(weekend)
    decode[AdjustableDates](weekend.asJson.noSpaces).map(value => value.adjusted(REF_DATA)) shouldBe
      Right(weekend.adjusted(REF_DATA))

    // Equal values encode to identical bytes: the dates are written in the order the value holds
    // them, which the invariant makes the increasing order, so equal values cannot differ in
    // their arrangement however they were built.
    unwrap(
      AdjustableDates.of(
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarId.of("Sat/Sun")),
        List(FRI_2014_07_11, SUN_2014_07_13)))
      .asJson
      .noSpaces shouldBe encoded.noSpaces

    // The decoder decides whether the fields describe a value exactly as a caller's arguments are
    // decided: the payload is read into the raw shape and handed to the factory, so dates that are
    // out of order are a decoding failure carrying that reason rather than a value this type would
    // not have built.
    val invalidPayload: Json =
      Json.obj(
        "unadjusted" -> Json.arr(Json.fromString("2014-07-13"), Json.fromString("2014-07-11")),
        "adjustment" -> BDA_NONE.asJson)
    val rejected: Either[DecodingFailure, AdjustableDates] = invalidPayload.as[AdjustableDates]
    rejected.isLeft shouldBe true
    rejected.left.map(failure => failure.message) shouldBe Left(OrderMessage)

    // The raw shape carries the dates as a plain array, which can perfectly well be empty - which
    // is exactly the input the factory rejects, and it is rejected as the reason it is rather than
    // as a malformed document.
    val emptyPayload: Json =
      Json.obj("unadjusted" -> Json.arr(), "adjustment" -> BDA_NONE.asJson)
    val rejectedEmpty: Either[DecodingFailure, AdjustableDates] = emptyPayload.as[AdjustableDates]
    rejectedEmpty.isLeft shouldBe true
    rejectedEmpty.left.map(failure => failure.message) shouldBe Left(EmptyMessage)

    // Both fields are required, so a document missing either is rejected rather than defaulted,
    // and a run of adjustable dates is an object rather than the array or the string it renders as.
    decode[AdjustableDates]("""{"unadjusted":["2014-07-11"]}""").isLeft shouldBe true
    decode[AdjustableDates](
      """{"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""").isLeft shouldBe true
    decode[AdjustableDates]("{}").isLeft shouldBe true
    Json.fromString("[2014-07-11]").as[AdjustableDates].isLeft shouldBe true
    Json.arr(Json.fromString("2014-07-11")).as[AdjustableDates].isLeft shouldBe true

    // Each field is read by the codec of its own type, so text that is no date and an adjustment
    // naming no convention of the closed family are both rejected here rather than later.
    decode[AdjustableDates](
      """{"unadjusted":["not-a-date"],""" +
        """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""").isLeft shouldBe true
    decode[AdjustableDates](
      """{"unadjusted":["2014-07-11"],""" +
        """"adjustment":{"convention":"Rubbish","calendar":"Sat/Sun"}}""").isLeft shouldBe true

    // A calendar this library knows nothing about is a fact about the reference data rather than
    // about the document, so it decodes and fails only when the dates are adjusted.
    val unknownCalendar: Either[io.circe.Error, AdjustableDates] =
      decode[AdjustableDates](
        """{"unadjusted":["2014-07-11"],""" +
          """"adjustment":{"convention":"Following","calendar":"XXXX"}}""")
    unknownCalendar.map(value => value.adjustment.calendar) shouldBe Right(UNKNOWN_CALENDAR)
    unknownCalendar.map(value => value.adjusted(REF_DATA).isLeft) shouldBe Right(true)
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the dates out of an outcome that is expected to have produced them.
   *
   * The factories are the only way to build a run of adjustable dates and they report what was
   * wrong with their arguments as a value, so every fixture of this suite arrives wrapped. The
   * outcome is folded rather than unwrapped by a partial accessor, so a fixture that fails to
   * build is reported as a test failure naming every reason it failed instead of raising an error
   * from somewhere else in the suite.
   *
   * @param outcome  the outcome expected to carry a run of adjustable dates
   * @return the adjustable dates it carries
   */
  private def unwrap(outcome: ResultNec[AdjustableDates]): AdjustableDates =
    outcome.fold(
      failures =>
        fail(
          "Expected adjustable dates but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      dates => dates)

  /**
   * Reads the failures out of an outcome that is expected to have produced no dates.
   *
   * Only the rejection assertions need this: the matchers hold when '''some''' failure of an
   * outcome satisfies what was asked, which is the right reading in general but says nothing about
   * how many failures a chain holds. The two conditions this type checks are mutually exclusive, so
   * asserting that exactly one failure is reported - and that it is the whole failure, attributes
   * included - needs the chain itself.
   *
   * @param outcome  the outcome expected to carry failures
   * @return the failures it carries, in the order it holds them
   */
  private def failuresOf(outcome: ResultNec[AdjustableDates]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      dates => fail(s"Expected a failure but the factory built the adjustable dates $dates"))

}
