/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period
import java.util.Locale

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor4

import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[PeriodAdditionConvention]].
 *
 * `NONE` adds a period unchanged. `LAST_DAY` carries the result to the last day of the target
 * month when the base date is the last day of its own month. `LAST_BUSINESS_DAY` carries it to the
 * last business day of the target month when the base date is the last '''business''' day of its
 * own month, which can fall before month end. All three members take a holiday calendar and only
 * `LAST_BUSINESS_DAY` consults it, on both sides of its rule. The family is closed: `values`,
 * `valueOf`, `parse` and the three lenient rewrites are the views of its [[NamedEnum]] instance.
 */
class PeriodAdditionConventionSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val dataTypes: TableFor1[PeriodAdditionConvention] =
    Table(
      "convention",
      PeriodAdditionConventions.NONE,
      PeriodAdditionConventions.LAST_DAY,
      PeriodAdditionConventions.LAST_BUSINESS_DAY)

  /** The note on each row names the day of week of the input date and of the expected date. */
  private val dataConvention: TableFor4[PeriodAdditionConvention, LocalDate, Int, LocalDate] =
    Table(
      ("convention", "input", "months", "expected"),
      (PeriodAdditionConventions.NONE, date(2014, 7, 11), 1, date(2014, 8, 11)), // Fri, Mon
      (PeriodAdditionConventions.NONE, date(2014, 7, 31), 1, date(2014, 8, 31)), // Thu, Sun
      (PeriodAdditionConventions.NONE, date(2014, 6, 30), 2, date(2014, 8, 30)), // Mon, Sat
      (PeriodAdditionConventions.LAST_DAY, date(2014, 7, 11), 1, date(2014, 8, 11)), // Fri, Mon
      (PeriodAdditionConventions.LAST_DAY, date(2014, 7, 31), 1, date(2014, 8, 31)), // Thu, Sun
      (PeriodAdditionConventions.LAST_DAY, date(2014, 6, 30), 2, date(2014, 8, 31)), // Mon, Sun
      (PeriodAdditionConventions.LAST_BUSINESS_DAY, date(2014, 7, 11), 1, date(2014, 8, 11)), // Fri, Mon
      (PeriodAdditionConventions.LAST_BUSINESS_DAY, date(2014, 7, 31), 1, date(2014, 8, 29)), // Thu, Sun to Fri
      (PeriodAdditionConventions.LAST_BUSINESS_DAY, date(2014, 6, 30), 2, date(2014, 8, 29))) // Mon, Sun to Fri

  private val dataName: TableFor2[PeriodAdditionConvention, String] =
    Table(
      ("convention", "name"),
      (PeriodAdditionConventions.NONE, "None"),
      (PeriodAdditionConventions.LAST_DAY, "LastDay"),
      (PeriodAdditionConventions.LAST_BUSINESS_DAY, "LastBusinessDay"))

  //-------------------------------------------------------------------------
  test("test_null") {
    // The date, the period and the calendar are mandatory, so a call omitting one does not compile.
    assertDoesNotCompile(
      "PeriodAdditionConventions.NONE.adjust(date(2014, 7, 11), Period.ofMonths(3))")
    assertDoesNotCompile("PeriodAdditionConventions.LAST_DAY.adjust(Period.ofMonths(3), HolidayCalendars.SAT_SUN)")
    assertDoesNotCompile("PeriodAdditionConventions.LAST_BUSINESS_DAY.adjust()")
    assertCompiles(
      "PeriodAdditionConventions.NONE.adjust(date(2014, 7, 11), Period.ofMonths(3), HolidayCalendars.SAT_SUN)")

    val dates: List[LocalDate] =
      List(
        date(2014, 7, 11),
        date(2014, 7, 31),
        date(2020, 2, 29),
        date(1900, 1, 1),
        date(2099, 12, 31))
    val periods: List[Period] =
      List(Period.ZERO, Period.ofDays(1), Period.ofMonths(3), Period.ofYears(1), Period.ofMonths(-3))
    val calendars: List[HolidayCalendar] =
      List(HolidayCalendars.NO_HOLIDAYS, HolidayCalendars.SAT_SUN, StandardHolidayCalendars.GBLO)

    forAll(dataTypes) { (convention: PeriodAdditionConvention) =>
      for (baseDate <- dates; period <- periods; calendar <- calendars) {
        withClue(s"${convention.name} on $baseDate plus $period against ${calendar.id.name}: ") {
          val adjusted = convention.adjust(baseDate, period, calendar)
          adjusted shouldBe a[LocalDate]
        }
      }
    }

    // Two months from 30 June 2014 lands on Sunday 31 August; only `LAST_BUSINESS_DAY` reads the
    // calendar, which pulls that back to Friday the 29th.
    val monthEnd = date(2014, 6, 30)
    val twoMonths = Period.ofMonths(2)
    PeriodAdditionConventions.NONE.adjust(monthEnd, twoMonths, HolidayCalendars.SAT_SUN) shouldBe
      PeriodAdditionConventions.NONE.adjust(monthEnd, twoMonths, HolidayCalendars.NO_HOLIDAYS)
    PeriodAdditionConventions.LAST_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.SAT_SUN) shouldBe
      PeriodAdditionConventions.LAST_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.NO_HOLIDAYS)
    PeriodAdditionConventions.LAST_BUSINESS_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.SAT_SUN) shouldBe
      date(2014, 8, 29)
    PeriodAdditionConventions.LAST_BUSINESS_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.NO_HOLIDAYS) shouldBe
      date(2014, 8, 31)
  }

  //-------------------------------------------------------------------------
  test("test_convention") {
    forAll(dataConvention) {
      (convention: PeriodAdditionConvention, input: LocalDate, months: Int, expected: LocalDate) =>
        withClue(s"${convention.name} on $input plus $months months: ") {
          convention.adjust(input, Period.ofMonths(months), HolidayCalendars.SAT_SUN) shouldBe expected
        }
    }

    // `LAST_BUSINESS_DAY` reads the calendar twice: to decide whether the base date is the last
    // business day of its own month, and to produce the last business day of the end month.
    val lastBusinessDayCases: TableFor4[LocalDate, Int, HolidayCalendar, LocalDate] =
      Table(
        ("base", "months", "calendar", "expected"),
        (date(2019, 6, 28), 1, HolidayCalendars.SAT_SUN, date(2019, 7, 31)),
        (date(2019, 6, 30), 1, HolidayCalendars.SAT_SUN, date(2019, 7, 30)),
        (date(2019, 8, 30), 4, StandardHolidayCalendars.GBLO, date(2019, 12, 31)),
        // Fri 29 Nov 2019 was the last business day of November
        (date(2019, 11, 29), 1, StandardHolidayCalendars.GBLO, date(2019, 12, 31)),
        // Sat 30 Nov 2019 was not, so the end date stays where arithmetic put it
        (date(2019, 11, 30), 1, StandardHolidayCalendars.GBLO, date(2019, 12, 30)),
        (date(2019, 12, 31), 1, StandardHolidayCalendars.GBLO, date(2020, 1, 31)),
        // with no holidays every day is a business day, so the rule reduces to the last day
        (date(2014, 6, 30), 2, HolidayCalendars.NO_HOLIDAYS, date(2014, 8, 31)))

    forAll(lastBusinessDayCases) { (base: LocalDate, months: Int, calendar: HolidayCalendar, expected: LocalDate) =>
      withClue(s"$base plus $months months against ${calendar.id.name}: ") {
        PeriodAdditionConventions.LAST_BUSINESS_DAY.adjust(base, Period.ofMonths(months), calendar) shouldBe expected
      }
    }

    // `adjust` does not check that the period is month-based - `PeriodAdjustment` and
    // `TenorAdjustment` validate `isMonthBased` when they are built - so a day-based period
    // reaches the end-of-month rules, which test the base date and nothing else.
    val monthEndOfJune2019 = date(2019, 6, 30)
    val tenDays = Period.ofDays(10)
    PeriodAdditionConventions.NONE.adjust(monthEndOfJune2019, tenDays, HolidayCalendars.SAT_SUN) shouldBe
      date(2019, 7, 10)
    PeriodAdditionConventions.LAST_DAY.adjust(monthEndOfJune2019, tenDays, HolidayCalendars.SAT_SUN) shouldBe
      date(2019, 7, 31)
    PeriodAdditionConventions.LAST_BUSINESS_DAY
      .adjust(date(2019, 6, 28), tenDays, HolidayCalendars.SAT_SUN) shouldBe date(2019, 7, 31)

    // Arithmetic alone shortens a day of month the target month lacks, so `NONE` and `LAST_DAY`
    // agree on 31 January plus one month: the base date ends its month, and the last day of the
    // target month is where arithmetic already lands.
    PeriodAdditionConventions.NONE
      .adjust(date(2019, 1, 31), Period.ofMonths(1), HolidayCalendars.SAT_SUN) shouldBe date(2019, 2, 28)
    PeriodAdditionConventions.LAST_DAY
      .adjust(date(2019, 1, 31), Period.ofMonths(1), HolidayCalendars.SAT_SUN) shouldBe date(2019, 2, 28)
    PeriodAdditionConventions.LAST_DAY
      .adjust(date(2019, 11, 30), Period.ofMonths(3), HolidayCalendars.SAT_SUN) shouldBe date(2020, 2, 29)
    PeriodAdditionConventions.LAST_DAY
      .adjust(date(2020, 2, 29), Period.ofMonths(12), HolidayCalendars.SAT_SUN) shouldBe date(2021, 2, 28)
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      convention.name shouldBe name
    }
  }

  test("test_toString") {
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      convention.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // `valueOf` is the exact lookup and answers with an `Option`; `parse` reports an unknown name
    // as a failure. Both are asserted for every member of the family.
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      withClue(s"$name: ") {
        PeriodAdditionConvention.valueOf(name) shouldBe Some(convention)
        PeriodAdditionConvention.parse(name) should haveValue(convention)
        // the canonical name folded to upper case is one of the keys the lookup registers
        PeriodAdditionConvention.valueOf(name.toUpperCase(Locale.ENGLISH)) shouldBe Some(convention)
      }
    }
  }

  test("test_extendedEnum") {
    val lookup: NamedEnum[PeriodAdditionConvention] = NamedEnum[PeriodAdditionConvention]

    lookup.familyName shouldBe "PeriodAdditionConvention"
    lookup.values.toList shouldBe List(
      PeriodAdditionConventions.NONE,
      PeriodAdditionConventions.LAST_DAY,
      PeriodAdditionConventions.LAST_BUSINESS_DAY)
    lookup.values.toList should have size 3
    lookup.values.toList.distinct should have size 3

    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      withClue(s"$name: ") {
        // `byCanonicalName` is keyed by the declared name, `byUpperName` by its upper case
        lookup.byCanonicalName.get(name) shouldBe Some(convention)
        lookup.byUpperName.get(name.toUpperCase(Locale.ENGLISH)) shouldBe Some(convention)
      }
    }
    lookup.byCanonicalName should have size 3

    lookup.alternateNames shouldBe empty
    lookup.externalNameGroups shouldBe empty
    lookup.externalNames("FpML") shouldBe None
    lookup.lenientPatterns should have size 3
  }

  test("test_of_lookup_notFound") {
    // The message is matched only for the text it quotes, so the wording stays free to change.
    PeriodAdditionConvention.valueOf("Rubbish") shouldBe None
    PeriodAdditionConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    PeriodAdditionConvention.parse("Rubbish") should haveFailureMessageMatching(".*Rubbish.*")
    PeriodAdditionConvention.valueOf("LastDayX") shouldBe None
    PeriodAdditionConvention.valueOf("Last Day") shouldBe None
    PeriodAdditionConvention.parse("Last Business Day") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    assertDoesNotCompile("PeriodAdditionConvention.valueOf()")
    assertDoesNotCompile("PeriodAdditionConvention.parse()")
    assertCompiles("""PeriodAdditionConvention.valueOf("None")""")
    assertCompiles("""PeriodAdditionConvention.parse("None")""")

    PeriodAdditionConvention.valueOf("") shouldBe None
    PeriodAdditionConvention.parse("") should beFailureWith(FailureReason.PARSING)
    PeriodAdditionConvention.valueOf("   ") shouldBe None
    PeriodAdditionConvention.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_lenientLookup_constants") {
    // The three lenient rewrites are the constant-style identifiers, resolved in any case.
    val constants: TableFor2[String, PeriodAdditionConvention] =
      Table(
        ("identifier", "convention"),
        ("NONE", PeriodAdditionConventions.NONE),
        ("LAST_DAY", PeriodAdditionConventions.LAST_DAY),
        ("LAST_BUSINESS_DAY", PeriodAdditionConventions.LAST_BUSINESS_DAY))

    forAll(constants) { (identifier: String, convention: PeriodAdditionConvention) =>
      withClue(s"$identifier: ") {
        PeriodAdditionConvention.parse(identifier) should haveValue(convention)
        PeriodAdditionConvention.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        PeriodAdditionConvention.parse(identifier.take(1) + identifier.drop(1).toLowerCase(Locale.ENGLISH)) should
          haveValue(convention)

        // Leniency belongs to `parse`: of the three, only `NONE` is also a key of the exact
        // lookup, being the upper case of the canonical name `None`, where `LastDay` folds to
        // `LASTDAY`.
        val isAlsoAnExactKey = identifier == convention.name.toUpperCase(Locale.ENGLISH)
        PeriodAdditionConvention.valueOf(identifier) shouldBe
          (if (isAlsoAnExactKey) Some(convention) else None)
      }
    }

    PeriodAdditionConvention.valueOf("NONE") shouldBe Some(PeriodAdditionConventions.NONE)
    PeriodAdditionConvention.valueOf("LAST_DAY") shouldBe None
    PeriodAdditionConvention.valueOf("LAST_BUSINESS_DAY") shouldBe None

    // A rewrite matches the whole of the text, so nothing appended to an identifier resolves.
    PeriodAdditionConvention.parse("LAST_BUSINESS_DAY") should haveValue(PeriodAdditionConventions.LAST_BUSINESS_DAY)
    PeriodAdditionConvention.parse("LAST_DAY") should haveValue(PeriodAdditionConventions.LAST_DAY)
    PeriodAdditionConvention.parse("LAST_DAY_X") should beFailureWith(FailureReason.PARSING)
    PeriodAdditionConvention.parse("X_LAST_DAY") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val all: List[PeriodAdditionConvention] = PeriodAdditionConvention.values.toList
    all should have size 3
    all.distinct should have size 3
    all.map(_.name).distinct should have size 3

    all.foreach { convention =>
      withClue(s"${convention.name}: ") {
        PeriodAdditionConvention.valueOf(convention.name) shouldBe Some(convention)
        PeriodAdditionConvention.parse(convention.name) should haveValue(convention)
        Show[PeriodAdditionConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
      }
    }

    // `PeriodAdditionConventions` publishes the members themselves rather than copies of them.
    PeriodAdditionConventions.NONE should be theSameInstanceAs PeriodAdditionConvention.NONE
    PeriodAdditionConventions.LAST_DAY should be theSameInstanceAs PeriodAdditionConvention.LAST_DAY
    PeriodAdditionConventions.LAST_BUSINESS_DAY should be theSameInstanceAs PeriodAdditionConvention.LAST_BUSINESS_DAY
    List(
      PeriodAdditionConventions.NONE,
      PeriodAdditionConventions.LAST_DAY,
      PeriodAdditionConventions.LAST_BUSINESS_DAY) shouldBe PeriodAdditionConvention.values.toList

    // `isMonthBased` is the flag `PeriodAdjustment` and `TenorAdjustment` validate their period
    // against: an end-of-month rule is meaningful only for months and years.
    PeriodAdditionConventions.NONE.isMonthBased shouldBe false
    PeriodAdditionConventions.LAST_DAY.isMonthBased shouldBe true
    PeriodAdditionConventions.LAST_BUSINESS_DAY.isMonthBased shouldBe true

    // `Eq`, `Hash` and `Order` all resolve to one instance, an ordering that is also a hashing.
    for (left <- all; right <- all) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[PeriodAdditionConvention].eqv(left, right) shouldBe sameValue
        Hash[PeriodAdditionConvention].eqv(left, right) shouldBe sameValue
        (Order[PeriodAdditionConvention].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[PeriodAdditionConvention].hash(left) shouldBe Hash[PeriodAdditionConvention].hash(right)
        } else {
          Order[PeriodAdditionConvention].compare(left, right) should not be 0
        }
      }
    }

    // The ordering is by name, which is alphabetical rather than the declaration order of
    // `values`: LastBusinessDay, LastDay, None.
    all.sorted(Order[PeriodAdditionConvention].toOrdering).map(_.name) shouldBe
      List("LastBusinessDay", "LastDay", "None")
  }

  test("test_jodaConvert") {
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      val rendered = Show[PeriodAdditionConvention].show(convention)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe convention.name
        PeriodAdditionConvention.parse(rendered) should haveValue(convention)
      }
    }

    Show[PeriodAdditionConvention].show(PeriodAdditionConventions.NONE) shouldBe "None"
    PeriodAdditionConvention.parse("None") shouldBe Right(PeriodAdditionConventions.NONE)
    Show[PeriodAdditionConvention].show(PeriodAdditionConventions.LAST_BUSINESS_DAY) shouldBe "LastBusinessDay"
    PeriodAdditionConvention.parse("LastBusinessDay") shouldBe Right(PeriodAdditionConventions.LAST_BUSINESS_DAY)
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // A convention encodes as the bare string of its canonical name and never as an object.
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      withClue(s"$name: ") {
        val encoded = convention.asJson
        encoded shouldBe Json.fromString(name)
        encoded.isString shouldBe true
        encoded.asObject shouldBe None
        encoded.asString shouldBe Some(name)
        encoded.noSpaces shouldBe s""""$name""""
        Json.fromString(name).as[PeriodAdditionConvention] shouldBe Right(convention)
        encoded.as[PeriodAdditionConvention] shouldBe Right(convention)
      }
    }
    // Decoding goes through `parse`, so a constant-style identifier or a differently cased name is
    // accepted, while text naming no convention is a decoding failure rather than an exception.
    Json.fromString("LAST_BUSINESS_DAY").as[PeriodAdditionConvention] shouldBe
      Right(PeriodAdditionConventions.LAST_BUSINESS_DAY)
    Json.fromString("lastday").as[PeriodAdditionConvention] shouldBe Right(PeriodAdditionConventions.LAST_DAY)
    Json.fromString("Rubbish").as[PeriodAdditionConvention].isLeft shouldBe true
    Json.fromInt(1).as[PeriodAdditionConvention].isLeft shouldBe true
  }
}
