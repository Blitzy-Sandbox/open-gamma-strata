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
 * Test [[AdjustableDates]].
 *
 * [[AdjustableDates.unadjusted]] is a `cats.data.NonEmptyList`, so the "at least one date"
 * invariant lives in the type and no assertion compares a plain list with one. The factories report
 * what was wrong with their arguments as a value, so every fixture is read out of its outcome.
 */
class AdjustableDatesSpec extends AnyFunSuite with Matchers {

  private val REF_DATA: ReferenceData = ReferenceData.standard

  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /** The wording the factories report for a run of dates that is not strictly increasing. */
  private val OrderMessage: String = "Dates must be in order and without duplicates"

  /** The wording the list factories report for a collection holding no date. */
  private val EmptyMessage: String = "Argument iterable 'unadjusted' must not be empty"

  //-------------------------------------------------------------------------
  test("test_of_noAdjustment") {
    val outcome: ResultNec[AdjustableDates] = AdjustableDates.of(FRI_2014_07_11, SUN_2014_07_13)
    outcome should beSuccess
    val test: AdjustableDates = unwrap(outcome)

    test.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)
    test.adjustment shouldBe BDA_NONE

    test.toAdjustableDateList shouldBe
      NonEmptyList.of(AdjustableDate.of(FRI_2014_07_11), AdjustableDate.of(SUN_2014_07_13))
    test.toAdjustableDateList.map(element => element.adjustment) shouldBe
      NonEmptyList.of(BDA_NONE, BDA_NONE)
    test.toAdjustableDateList.map(element => element.unadjusted) shouldBe test.unadjusted

    // Rendering is the bracketed date list alone when the adjustment is the no-adjustment
    // constant, and otherwise the list, ` adjusted by ` and the adjustment.
    test.toString shouldBe "[2014-07-11, 2014-07-13]"

    // No adjustment applies, so both dates stand whatever reference data they are adjusted against.
    test.adjusted(REF_DATA) should haveValue(NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))
    test.adjusted(ReferenceData.minimal) should
      haveValue(NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))

    val fromList: ResultNec[AdjustableDates] =
      AdjustableDates.of(List(FRI_2014_07_11, SUN_2014_07_13))
    fromList should beSuccess
    unwrap(fromList) shouldBe test
    fromList shouldBe outcome

    val fromNonEmpty: ResultNec[AdjustableDates] =
      AdjustableDates.of(BDA_NONE, NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))
    fromNonEmpty should beSuccess
    unwrap(fromNonEmpty) shouldBe test

    // Naming the no-adjustment constant is the same as naming no adjustment, so the rendering rule
    // above turns on the value of the adjustment rather than on the factory that built the value.
    unwrap(AdjustableDates.of(BDA_NONE, FRI_2014_07_11, SUN_2014_07_13)) shouldBe test
    unwrap(AdjustableDates.of(BDA_NONE, List(FRI_2014_07_11, SUN_2014_07_13))) shouldBe test

    val single: AdjustableDates = unwrap(AdjustableDates.of(FRI_2014_07_11))
    single.unadjusted shouldBe NonEmptyList.one(FRI_2014_07_11)
    single.adjustment shouldBe BDA_NONE
    single.toAdjustableDateList shouldBe NonEmptyList.one(AdjustableDate.of(FRI_2014_07_11))
    single.toString shouldBe "[2014-07-11]"
    single.adjusted(REF_DATA) should haveValue(NonEmptyList.one(FRI_2014_07_11))
  }

  //-------------------------------------------------------------------------
  test("test_of_withAdjustment") {
    val outcome: ResultNec[AdjustableDates] =
      AdjustableDates.of(BDA_FOLLOW_SAT_SUN, FRI_2014_07_11, SUN_2014_07_13)
    outcome should beSuccess
    val test: AdjustableDates = unwrap(outcome)

    test.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // The adjustment is a property of the whole set, so it reaches every element of the expansion
    // rather than only the first of them.
    test.toAdjustableDateList shouldBe
      NonEmptyList.of(
        AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN),
        AdjustableDate.of(SUN_2014_07_13, BDA_FOLLOW_SAT_SUN))
    test.toAdjustableDateList.map(element => element.adjustment) shouldBe
      NonEmptyList.of(BDA_FOLLOW_SAT_SUN, BDA_FOLLOW_SAT_SUN)

    test.toString shouldBe "[2014-07-11, 2014-07-13] adjusted by Following using calendar Sat/Sun"

    // The Sunday rolls forward to the Monday under the following convention, the Friday standing.
    val adjusted: Either[Failure, NonEmptyList[LocalDate]] = test.adjusted(REF_DATA)
    adjusted should haveValue(NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14))

    // Adjusting leaves the unadjusted dates untouched, so both sets stay recoverable.
    test.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)

    // Adjusting the expansion one date at a time reaches the same dates, one resolution per date.
    val perDate: NonEmptyList[Either[Failure, LocalDate]] =
      test.toAdjustableDateList.map(element => element.adjusted(REF_DATA))
    val expectedPerDate: NonEmptyList[Either[Failure, LocalDate]] =
      NonEmptyList.of(Right(FRI_2014_07_11), Right(MON_2014_07_14))
    perDate shouldBe expectedPerDate

    val fromList: ResultNec[AdjustableDates] =
      AdjustableDates.of(BDA_FOLLOW_SAT_SUN, List(FRI_2014_07_11, SUN_2014_07_13))
    fromList should beSuccess
    unwrap(fromList) shouldBe test
    fromList shouldBe outcome
    unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))) shouldBe
      test

    // The reader is the same adjustment awaiting its reference data: it delegates rather than
    // adjusting a second way, so it agrees with the direct path date for date, and readers compose
    // so that the data is supplied once, to the composition, rather than to each of them.
    val reader: RefDataReader[NonEmptyList[LocalDate]] = test.toReader
    reader.run(REF_DATA) should haveValue(NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14))
    reader.run(REF_DATA) shouldBe adjusted

    val saturday: AdjustableDates = unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, SAT_2014_07_12))
    val both: RefDataReader[(NonEmptyList[LocalDate], NonEmptyList[LocalDate])] =
      (test.toReader, saturday.toReader).tupled
    both.run(REF_DATA) should
      haveValue((NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14), NonEmptyList.one(MON_2014_07_14)))

    val sameFinalDate: RefDataReader[Boolean] =
      (test.toReader, saturday.toReader).mapN((first, second) => first.last == second.last)
    sameFinalDate.run(REF_DATA) should haveValue(true)

    reader.map(dates => dates.size).run(REF_DATA) should haveValue(2)

    // Against real holiday data: the London summer bank holiday of 2014 falls on Monday the 25th,
    // so the Saturday before it rolls forward to the Tuesday while the Friday before stands.
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

    // A calendar the reference data does not supply is a missing-data failure through both entry
    // points - asserted by the reason it carries, by value - with nothing thrown; and a composition
    // fails as a whole when one part does not resolve.
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
    val test: AdjustableDates = unwrap(AdjustableDates.of(FRI_2014_07_11))
    val test2: AdjustableDates =
      unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, FRI_2014_07_11, SUN_2014_07_13))
    val same: AdjustableDates = unwrap(AdjustableDates.of(FRI_2014_07_11))

    test.unadjusted shouldBe NonEmptyList.one(FRI_2014_07_11)
    test.adjustment shouldBe BDA_NONE
    test2.unadjusted shouldBe NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13)
    test2.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // `Eq` is obtained from the single `Hash` instance by subtyping, so these checks fix both.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[AdjustableDates].eqv(test, same) shouldBe true
    Hash[AdjustableDates].hash(test) shouldBe Hash[AdjustableDates].hash(same)
    Hash[AdjustableDates].eqv(test, test2) shouldBe false
    test.equals(FRI_2014_07_11) shouldBe false

    val otherDates: AdjustableDates = unwrap(AdjustableDates.of(SUN_2014_07_13))
    val sameDatesNoAdjustment: AdjustableDates =
      unwrap(AdjustableDates.of(FRI_2014_07_11, SUN_2014_07_13))
    Hash[AdjustableDates].eqv(test, otherDates) shouldBe false
    Hash[AdjustableDates].eqv(sameDatesNoAdjustment, test2) shouldBe false
    sameDatesNoAdjustment.unadjusted shouldBe test2.unadjusted

    Show[AdjustableDates].show(test) shouldBe test.toString
    Show[AdjustableDates].show(test2) shouldBe test2.toString
    Show[AdjustableDates].show(test) shouldBe "[2014-07-11]"
    Show[AdjustableDates].show(test2) shouldBe
      "[2014-07-11, 2014-07-13] adjusted by Following using calendar Sat/Sun"

    //-----------------------------------------------------------------------
    // The dates have to be strictly increasing. A date out of order and a date that repeats are the
    // two ways one run can be wrong, and a single check of strict ordering rejects both, so both
    // are reported under one reason - asserted by value rather than by the text of the message.
    val outOfOrder: ResultNec[AdjustableDates] =
      AdjustableDates.of(SUN_2014_07_13, FRI_2014_07_11)
    val duplicated: ResultNec[AdjustableDates] =
      AdjustableDates.of(FRI_2014_07_11, FRI_2014_07_11)
    outOfOrder should beFailureWith(FailureReason.INVALID)
    duplicated should beFailureWith(FailureReason.INVALID)

    // A rejected run reports exactly one failure, and that failure is the whole of what is
    // reported, attributes included; the two conditions this type checks are mutually exclusive.
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
    // express it: the varargs factories take their first date as a parameter of their own.
    val empty: ResultNec[AdjustableDates] = AdjustableDates.of(List.empty[LocalDate])
    empty should beFailureWith(FailureReason.INVALID)
    failuresOf(empty) shouldBe List(Failure.Invalid(EmptyMessage))
    failuresOf(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, List.empty[LocalDate])) shouldBe
      List(Failure.Invalid(EmptyMessage))

    // A run of one is vacuously increasing, and consecutive days are increasing too.
    AdjustableDates.of(List(FRI_2014_07_11)) should beSuccess
    AdjustableDates.of(FRI_2014_07_11, SAT_2014_07_12, SUN_2014_07_13) should beSuccess

    //-----------------------------------------------------------------------
    // `adjusted` de-duplicates: the unadjusted dates are distinct by the invariant above, but two
    // of them a weekend apart adjust onto the same Monday and the result holds that Monday once, so
    // the list returned can be shorter than the unadjusted run.
    val weekend: AdjustableDates =
      unwrap(AdjustableDates.of(BDA_FOLLOW_SAT_SUN, SAT_2014_07_12, SUN_2014_07_13))
    weekend.unadjusted.size shouldBe 2
    weekend.adjusted(REF_DATA) should haveValue(NonEmptyList.one(MON_2014_07_14))
    weekend.toReader.run(REF_DATA) should haveValue(NonEmptyList.one(MON_2014_07_14))

    // Only the dates that collide collapse, and in the order of first occurrence.
    val run: AdjustableDates =
      unwrap(
        AdjustableDates.of(BDA_FOLLOW_SAT_SUN, FRI_2014_07_11, SAT_2014_07_12, SUN_2014_07_13))
    run.unadjusted.size shouldBe 3
    run.adjusted(REF_DATA) should haveValue(NonEmptyList.of(FRI_2014_07_11, MON_2014_07_14))

    // `toAdjustableDateList` is the form that keeps one adjusted date per unadjusted date, so the
    // correspondence the de-duplication discards survives there, both dates reaching the Monday.
    val weekendPerDate: NonEmptyList[Either[Failure, LocalDate]] =
      weekend.toAdjustableDateList.map(element => element.adjusted(REF_DATA))
    val expectedWeekendPerDate: NonEmptyList[Either[Failure, LocalDate]] =
      NonEmptyList.of(Right(MON_2014_07_14), Right(MON_2014_07_14))
    weekendPerDate shouldBe expectedWeekendPerDate
    weekend.toAdjustableDateList.size shouldBe 2

    // De-duplication is not sorting: a run needing no adjustment comes back exactly as it is held.
    sameDatesNoAdjustment.adjusted(REF_DATA) should
      haveValue(NonEmptyList.of(FRI_2014_07_11, SUN_2014_07_13))

    //-----------------------------------------------------------------------
    val encoded: Json = test2.asJson
    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("unadjusted", "adjustment"))
    encoded.noSpaces shouldBe
      """{"unadjusted":["2014-07-11","2014-07-13"],""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    encoded.noSpaces should not include "null"
    test.asJson.noSpaces should not include "null"

    // The rendering rule of `toString` is not the document: a run carrying the no-adjustment
    // constant still serializes both fields, so what is decoded is what was encoded.
    test.asJson.noSpaces shouldBe
      """{"unadjusted":["2014-07-11"],""" +
        """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

    decode[AdjustableDates](encoded.noSpaces) shouldBe Right(test2)
    decode[AdjustableDates](test.asJson.noSpaces) shouldBe Right(test)
    decode[AdjustableDates](weekend.asJson.noSpaces) shouldBe Right(weekend)
    decode[AdjustableDates](weekend.asJson.noSpaces).map(value => value.adjusted(REF_DATA)) shouldBe
      Right(weekend.adjusted(REF_DATA))

    // Equal values encode to identical bytes: the dates are written in the order the value holds
    // them, which the invariant makes the increasing order, however the value was built.
    unwrap(
      AdjustableDates.of(
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarId.of("Sat/Sun")),
        List(FRI_2014_07_11, SUN_2014_07_13)))
      .asJson
      .noSpaces shouldBe encoded.noSpaces

    // The payload is read into the raw shape and handed to the factory, so dates out of order fail
    // with the factory's own reason rather than being carried into a value.
    val invalidPayload: Json =
      Json.obj(
        "unadjusted" -> Json.arr(Json.fromString("2014-07-13"), Json.fromString("2014-07-11")),
        "adjustment" -> BDA_NONE.asJson)
    val rejected: Either[DecodingFailure, AdjustableDates] = invalidPayload.as[AdjustableDates]
    rejected.isLeft shouldBe true
    rejected.left.map(failure => failure.message) shouldBe Left(OrderMessage)

    // The raw shape carries the dates as a plain array, which may be empty - the other input the
    // factory rejects, reported as the reason it is rather than as a malformed document.
    val emptyPayload: Json =
      Json.obj("unadjusted" -> Json.arr(), "adjustment" -> BDA_NONE.asJson)
    val rejectedEmpty: Either[DecodingFailure, AdjustableDates] = emptyPayload.as[AdjustableDates]
    rejectedEmpty.isLeft shouldBe true
    rejectedEmpty.left.map(failure => failure.message) shouldBe Left(EmptyMessage)

    // Both fields are required, so a document missing either is rejected rather than defaulted.
    decode[AdjustableDates]("""{"unadjusted":["2014-07-11"]}""").isLeft shouldBe true
    decode[AdjustableDates](
      """{"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""").isLeft shouldBe true
    decode[AdjustableDates]("{}").isLeft shouldBe true
    Json.fromString("[2014-07-11]").as[AdjustableDates].isLeft shouldBe true
    Json.arr(Json.fromString("2014-07-11")).as[AdjustableDates].isLeft shouldBe true

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
   * Reads the dates out of an outcome expected to carry them, folding rather than using a partial
   * accessor so that a fixture which fails to build is a test failure naming the reasons.
   */
  private def unwrap(outcome: ResultNec[AdjustableDates]): AdjustableDates =
    outcome.fold(
      failures =>
        fail(
          "Expected adjustable dates but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      dates => dates)

  /**
   * Reads the failures out of an outcome expected to carry no dates. The matchers hold when
   * '''some''' failure satisfies what was asked, while these assertions need the whole chain:
   * exactly one failure, attributes included.
   */
  private def failuresOf(outcome: ResultNec[AdjustableDates]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      dates => fail(s"Expected a failure but the factory built the adjustable dates $dates"))

}
