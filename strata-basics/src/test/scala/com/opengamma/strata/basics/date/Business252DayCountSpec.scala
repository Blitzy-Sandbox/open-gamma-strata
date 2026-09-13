/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper._

/**
 * Test the `Bus/252` day count, the one [[DayCount]] member that carries a holiday calendar.
 *
 * It counts the business days of a holiday calendar and divides by 252, so the calendar is part of
 * the convention and appears in its name. Equality is by name and a `Bus/252` name embeds its
 * calendar's identifier, so two over calendars of the same identifier are equal whatever their
 * holidays and two over different calendars unequal without their holidays being compared. Nothing
 * caches, so reference identity is not part of the contract and the equality assertions are values.
 */
class Business252DayCountSpec extends AnyFunSuite with Matchers {

  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** A value of an unrelated type, for the `equals` assertions of `test_equalsHashCode`. */
  private val ANOTHER_TYPE: Any = ""

  private val BUS_252_EUTA: String = "Bus/252 EUTA"

  private val BUS_252_GBLO: String = "Bus/252 GBLO"

  /**
   * The Europe/TARGET calendar, resolved once here rather than inside the 366-iteration walks
   * below; resolution is a lookup in an immutable store, so no expectation changes.
   */
  private val EUTA_CALENDAR: HolidayCalendar = resolved(HolidayCalendarIds.EUTA)

  private val GBLO_CALENDAR: HolidayCalendar = resolved(HolidayCalendarIds.GBLO)

  private val DATE_2014_12_01: LocalDate = date(2014, 12, 1)

  /** The 366 second dates of both walks, 2014-12-01 to 2015-12-01 inclusive, held immutably. */
  private val SECOND_DATES: List[LocalDate] =
    Iterator.iterate(DATE_2014_12_01)(current => current.plusDays(1L)).take(366).toList

  private val UNKNOWN_CALENDAR_ID: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  //-------------------------------------------------------------------------
  test("test_factory_name") {
    // `parse` resolves a `Bus/252 X` name against the calendars built into this library and reports
    // text it cannot resolve as a value: text naming no member is a `PARSING` failure, and a name
    // whose calendar the library does not define fails for the calendar rather than for the prefix.
    val parsed = DayCount.parse(BUS_252_EUTA)
    parsed should beSuccess

    val test = dayCountOf(BUS_252_EUTA)
    test.name shouldBe BUS_252_EUTA
    test.toString shouldBe BUS_252_EUTA
    Show[DayCount].show(test) shouldBe BUS_252_EUTA

    parsed shouldBe Right(test)
    DayCount.parse(BUS_252_EUTA) shouldBe Right(test)
    DayCount.valueOf(BUS_252_EUTA) shouldBe Some(test)

    DayCount.ofBus252(EUTA_CALENDAR) shouldBe test
    DayCount.ofBus252(HolidayCalendarIds.EUTA, REF_DATA) shouldBe Right(test)
    Hash[DayCount].eqv(DayCount.ofBus252(EUTA_CALENDAR), test) shouldBe true

    DayCount.parse("Rubbish") should beFailureWith(FailureReason.PARSING)

    DayCount.parse("Bus/252 XXXX") should beFailureWith(FailureReason.PARSING)
    DayCount.valueOf("Bus/252 XXXX") shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_factory_nameUpper") {
    // The `Bus/252` prefix is matched without regard to case and no lenient rewrite is needed to
    // reach it, while the calendar part is matched as a calendar name - which is why `BUS/252 euta`
    // fails. The name of the result is rebuilt from the resolved calendar, restoring its casing.
    val test = dayCountOf("BUS/252 EUTA")
    test.name shouldBe BUS_252_EUTA
    test.toString shouldBe BUS_252_EUTA

    DayCount.parse(BUS_252_EUTA) shouldBe Right(test)
    DayCount.ofBus252(HolidayCalendarIds.EUTA, REF_DATA) shouldBe Right(test)

    DayCount.valueOf("BUS/252 EUTA") shouldBe Some(test)
    DayCount.valueOf("bus/252 EUTA") shouldBe Some(test)

    DayCount.parse("BUS/252 euta") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_factory_calendar") {
    // `ofBus252(calendar)` is total and takes the resolved calendar, which is what keeps
    // `yearFraction` and `days` pure functions of their arguments; `ofBus252(id, refData)` resolves
    // explicitly and reports an identifier the data cannot answer for as `MISSING_DATA`, as does
    // `parse(name, refData)`, which resolves against whatever the caller supplies.
    val test = DayCount.ofBus252(GBLO_CALENDAR)
    test.name shouldBe BUS_252_GBLO
    test.toString shouldBe BUS_252_GBLO

    DayCount.parse(BUS_252_GBLO) shouldBe Right(test)
    DayCount.ofBus252(HolidayCalendarIds.GBLO, REF_DATA) shouldBe Right(test)
    DayCount.valueOf(BUS_252_GBLO) shouldBe Some(test)

    calendarOf(test) shouldBe GBLO_CALENDAR
    calendarOf(test).id shouldBe HolidayCalendarIds.GBLO

    DayCount.ofBus252(UNKNOWN_CALENDAR_ID, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    DayCount.parse(BUS_252_GBLO, REF_DATA) shouldBe Right(test)
    DayCount.parse("Bus/252 XXXX", REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction") {
    val test = dayCountOf(BUS_252_EUTA)
    val eutaCal = EUTA_CALENDAR

    SECOND_DATES.foreach { secondDate =>
      withClue(s"$DATE_2014_12_01 to $secondDate: ") {
        test.yearFraction(DATE_2014_12_01, secondDate) shouldBe
          eutaCal.daysBetween(DATE_2014_12_01, secondDate).toDouble / 252d
      }
    }

    test.yearFraction(DATE_2014_12_01, DATE_2014_12_01) shouldBe 0d
    SECOND_DATES.last shouldBe date(2015, 12, 1)
    test.yearFraction(DATE_2014_12_01, SECOND_DATES.last) should be > 0d

    // The three-argument overload reads no schedule information for this convention.
    SECOND_DATES.foreach { secondDate =>
      withClue(s"$DATE_2014_12_01 to $secondDate: ") {
        test.yearFraction(DATE_2014_12_01, secondDate, DayCount.ScheduleInfo.simple) shouldBe
          test.yearFraction(DATE_2014_12_01, secondDate)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction_badOrder") {
    val test = dayCountOf(BUS_252_EUTA)
    val date1 = date(2014, 12, 2)
    val date2 = date(2014, 12, 1)

    // Dates supplied in time-line order is a precondition on the caller rather than a property of
    // the data, so it stays a fail-fast `ArgCheck` throw and is the single `intercept` here, while
    // `relativeYearFraction` accepts a reversed pair and negates instead of refusing.
    val thrown = intercept[IllegalArgumentException](test.yearFraction(date1, date2))
    thrown.getMessage should include("time-line order")

    test.relativeYearFraction(date1, date2) shouldBe -test.yearFraction(date2, date1)
  }

  //-------------------------------------------------------------------------
  test("test_days") {
    val test = dayCountOf(BUS_252_EUTA)
    val eutaCal = EUTA_CALENDAR

    SECOND_DATES.foreach { secondDate =>
      withClue(s"$DATE_2014_12_01 to $secondDate: ") {
        test.days(DATE_2014_12_01, secondDate) shouldBe eutaCal.daysBetween(DATE_2014_12_01, secondDate)
      }
    }

    SECOND_DATES.foreach { secondDate =>
      withClue(s"$DATE_2014_12_01 to $secondDate: ") {
        test.yearFraction(DATE_2014_12_01, secondDate) shouldBe test.days(DATE_2014_12_01, secondDate).toDouble / 252d
      }
    }

    test.days(DATE_2014_12_01, DATE_2014_12_01) shouldBe 0
  }

  //-------------------------------------------------------------------------
  test("test_equalsHashCode") {
    val a = dayCountOf(BUS_252_EUTA)
    val b = dayCountOf(BUS_252_GBLO)

    a.equals(a) shouldBe true
    a.equals(b) shouldBe false
    a.equals(ANOTHER_TYPE) shouldBe false
    a.equals(null) shouldBe false
    a.hashCode shouldBe a.hashCode

    a.equals(DayCount.ofBus252(EUTA_CALENDAR)) shouldBe true
    a.hashCode shouldBe BUS_252_EUTA.hashCode
    a.equals(DayCounts.ACT_365F) shouldBe false

    // `Hash` extends `Eq`, so the family publishes a single equality-bearing instance; these values
    // check that it agrees with `equals` and with `hashCode`.
    Hash[DayCount].eqv(a, a) shouldBe true
    Hash[DayCount].eqv(a, b) shouldBe false
    Hash[DayCount].eqv(a, DayCount.ofBus252(EUTA_CALENDAR)) shouldBe true
    Hash[DayCount].hash(a) shouldBe a.hashCode
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val euta = DayCount.ofBus252(EUTA_CALENDAR)
    val gblo = DayCount.ofBus252(GBLO_CALENDAR)

    // The name is derived from the calendar rather than supplied alongside it, so the two cannot be
    // given inconsistently.
    euta.name shouldBe "Bus/252 " + EUTA_CALENDAR.id.name
    gblo.name shouldBe "Bus/252 " + GBLO_CALENDAR.id.name
    calendarOf(euta) shouldBe EUTA_CALENDAR
    calendarOf(gblo) shouldBe GBLO_CALENDAR

    // The member is reachable only through its factories and the parse methods, there being no
    // constructor, no `apply` and no `copy` to forge a name with, and the family is sealed.
    assertDoesNotCompile("new DayCount.Bus252(HolidayCalendars.SAT_SUN)")
    assertDoesNotCompile("DayCount.Bus252(HolidayCalendars.SAT_SUN)")
    assertDoesNotCompile("bus252Of(DayCount.ofBus252(HolidayCalendars.SAT_SUN)).copy()")

    assertDoesNotCompile("class OtherDayCount extends DayCount(\"Other\")")

    // Hashing is by name, so equal values hash equally; unequal values may legally collide, which
    // makes the inequality below an observation about these two particular names rather than a
    // rule. The rendering is the name and agrees with `toString`.
    Hash[DayCount].eqv(euta, gblo) shouldBe false
    Hash[DayCount].hash(euta) should not be Hash[DayCount].hash(gblo)
    Show[DayCount].show(euta) shouldBe BUS_252_EUTA
    Show[DayCount].show(gblo) shouldBe BUS_252_GBLO
    Show[DayCount].show(euta) shouldBe euta.toString

    // `values` holds the 21 standard members and no `Bus/252` day count, there being one of those
    // per calendar.
    DayCount.values.toList should have size 21
    DayCount.values.toList should not contain euta
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Standard members are written as the bare string of their name; this one is written
    // structurally, its calendar being part of it and a calendar an application built itself having
    // to survive the round trip with its holidays intact. A round trip alone cannot tell the two
    // forms apart, which is why the document is asserted in full.
    val test = DayCount.ofBus252(EUTA_CALENDAR)
    val document = test.asJson
    val expected: Json = parse("""{"Bus252":{"name":"Bus/252 EUTA","calendar":"EUTA"}}""")
      .getOrElse(fail("the expected JSON of this test is not valid JSON"))
    document shouldBe expected

    document.isObject shouldBe true
    document.isString shouldBe false
    document.asObject.map(fields => fields.keys.toList) shouldBe Some(List("Bus252"))
    val fields = document.hcursor.downField("Bus252")
    fields.get[String]("name") shouldBe Right(BUS_252_EUTA)
    fields.get[HolidayCalendar]("calendar") shouldBe Right(EUTA_CALENDAR)

    decode[DayCount](document.noSpaces) shouldBe Right(test)

    DayCount.ofBus252(EUTA_CALENDAR).asJson.noSpaces shouldBe document.noSpaces
    DayCount.ofBus252(GBLO_CALENDAR).asJson.noSpaces should not be document.noSpaces

    // The decoder also accepts a bare `Bus/252` name, the form hand-written input carries.
    decode[DayCount](Json.fromString(BUS_252_EUTA).noSpaces) shouldBe Right(test)
    decode[DayCount](Json.fromString("BUS/252 EUTA").noSpaces) shouldBe Right(test)

    DayCounts.ACT_365F.asJson shouldBe Json.fromString("Act/365F")
    decode[DayCount](DayCounts.ACT_365F.asJson.noSpaces) shouldBe Right(DayCounts.ACT_365F)

    // The name and the calendar are not independent, so a document whose two disagree is rejected:
    // the value read back would otherwise count the wrong days under a name that hid it.
    val mismatched = decode[DayCount]("""{"Bus252":{"name":"Bus/252 GBLO","calendar":"EUTA"}}""")
    mismatched.isLeft shouldBe true
    mismatched.swap.map(failure => failure.getMessage).getOrElse("") should include(BUS_252_EUTA)

    decode[DayCount]("""{"Bus252":{"name":"Bus/252 EUTA"}}""").isLeft shouldBe true
    decode[DayCount]("42").isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_jodaConvert") {
    val test = DayCount.ofBus252(EUTA_CALENDAR)
    val shown = Show[DayCount].show(test)

    shown shouldBe BUS_252_EUTA
    shown shouldBe test.name
    shown shouldBe test.toString

    DayCount.parse(shown) shouldBe Right(test)
    DayCount.parse(shown, REF_DATA) shouldBe Right(test)
    DayCount.valueOf(shown) shouldBe Some(test)

    val london = DayCount.ofBus252(GBLO_CALENDAR)
    DayCount.parse(Show[DayCount].show(london)) shouldBe Right(london)
    DayCount.parse(Show[DayCount].show(london)) should not be Right(test)
  }

  //-------------------------------------------------------------------------
  /** Parses a fixture day count, folding a failure into a failed test rather than forcing it. */
  private def dayCountOf(name: String): DayCount =
    DayCount.parse(name) match {
      case Right(dayCount) => dayCount
      case Left(failures) =>
        fail(
          s"Fixture day count '$name' could not be parsed: " +
            failures.toChain.toList.map(failure => failure.message).mkString("; "))
    }

  /** Resolves a fixture calendar, folding a failure into a failed test rather than forcing it. */
  private def resolved(id: HolidayCalendarId): HolidayCalendar =
    id.resolve(REF_DATA) match {
      case Right(calendar) => calendar
      case Left(failure) => fail(s"Fixture calendar '${id.name}' could not be resolved: ${failure.message}")
    }

  /** Narrows a day count to the `Bus/252` member by a type pattern, so no cast is involved. */
  private def bus252Of(dayCount: DayCount): DayCount.Bus252 =
    dayCount match {
      case bus252: DayCount.Bus252 => bus252
      case other => fail(s"Expected a 'Bus/252' day count but was: $other")
    }

  /** Reads the calendar a `Bus/252` day count counts the business days of. */
  private def calendarOf(dayCount: DayCount): HolidayCalendar =
    bus252Of(dayCount).calendar
}
