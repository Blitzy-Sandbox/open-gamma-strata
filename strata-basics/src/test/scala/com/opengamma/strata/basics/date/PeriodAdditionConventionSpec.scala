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

/**
 * Test [[PeriodAdditionConvention]].
 *
 * Every method of the Java original is kept, under its own name, so that the method-level
 * traceability of the migration stays one-to-one; the three data providers of the original
 * become the three tables declared at the top of this spec. The original's
 * `test_serialization` is the one method not here: the test mapping manifest consolidates it
 * into `json.JsonRoundTripSpec`, so what this spec pins instead is the per-type JSON
 * representation, in a test of its own at the end.
 *
 * Four of the ported methods asserted machinery this port does not have, and each is ported
 * as the assertion of the guarantee that machinery gave rather than dropped. The reasoning is
 * repeated at each of them:
 *
 *  - `test_null` asserted that every convention rejected an absent date, period or calendar by
 *    raising an error. None of those arguments can be absent here, so the case becomes the
 *    total-ness of `adjust` over the three members and a matrix of real arguments, together
 *    with the observation that the calendar is accepted by all three members and consulted by
 *    exactly one of them.
 *  - `test_extendedEnum` read the contents of the run-time registry the original assembled
 *    from a configuration resource. There is no registry; the name lookup of the family is the
 *    `NamedEnum` instance, and its views are asserted in its place.
 *  - `test_of_lookup_null` asserted that the factory rejected an absent name by raising. The
 *    factories here answer with a value, so the case becomes the two spellings of an absent
 *    name that can actually be supplied - the empty name and a blank one.
 *  - `coverage` called a reflective sweep over a Java enum and the private constructor of a
 *    constants holder, neither of which a sealed family of case objects offers a target for;
 *    the closed-family properties that sweep stood in for are asserted directly.
 *
 * The expectations that are not the Java provider's own rows were taken from the Java
 * implementation rather than derived by hand: each was evaluated against the published
 * `strata-basics` jar and is recorded with its value at the test that asserts it. That matters
 * most for `LAST_BUSINESS_DAY`, whose rule reads the calendar on both sides, where a
 * plausible-looking expectation is easy to write and wrong.
 */
class PeriodAdditionConventionSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The three conventions, which is the provider the Java original derived from the values of
   * its package-private enum.
   */
  private val dataTypes: TableFor1[PeriodAdditionConvention] =
    Table(
      "convention",
      PeriodAdditionConvention.NONE,
      PeriodAdditionConvention.LAST_DAY,
      PeriodAdditionConvention.LAST_BUSINESS_DAY)

  /**
   * The rows of the Java `data_convention` provider, transcribed verbatim, including the
   * comments that record which day of the week each date fell on. Every row is evaluated
   * against the Saturday/Sunday calendar, as the Java test was.
   */
  private val dataConvention: TableFor4[PeriodAdditionConvention, LocalDate, Int, LocalDate] =
    Table(
      ("convention", "input", "months", "expected"),
      // None
      (PeriodAdditionConvention.NONE, LocalDate.of(2014, 7, 11), 1, LocalDate.of(2014, 8, 11)), // Fri, Mon
      (PeriodAdditionConvention.NONE, LocalDate.of(2014, 7, 31), 1, LocalDate.of(2014, 8, 31)), // Thu, Sun
      (PeriodAdditionConvention.NONE, LocalDate.of(2014, 6, 30), 2, LocalDate.of(2014, 8, 30)), // Mon, Sat
      // LastDay
      (PeriodAdditionConvention.LAST_DAY, LocalDate.of(2014, 7, 11), 1, LocalDate.of(2014, 8, 11)), // Fri, Mon
      (PeriodAdditionConvention.LAST_DAY, LocalDate.of(2014, 7, 31), 1, LocalDate.of(2014, 8, 31)), // Thu, Sun
      (PeriodAdditionConvention.LAST_DAY, LocalDate.of(2014, 6, 30), 2, LocalDate.of(2014, 8, 31)), // Mon, Sun
      // LastBusinessDay
      (PeriodAdditionConvention.LAST_BUSINESS_DAY, LocalDate.of(2014, 7, 11), 1, LocalDate.of(2014, 8, 11)), // Fri, Mon
      (PeriodAdditionConvention.LAST_BUSINESS_DAY, LocalDate.of(2014, 7, 31), 1, LocalDate.of(2014, 8, 29)), // Thu, Sun to Fri
      (PeriodAdditionConvention.LAST_BUSINESS_DAY, LocalDate.of(2014, 6, 30), 2, LocalDate.of(2014, 8, 29))) // Mon, Sun to Fri

  /**
   * The rows of the Java `data_name` provider: each convention with the name it renders as and
   * is looked up by. The names are the contract of the family in text and in JSON, and they are
   * unchanged from the original.
   */
  private val dataName: TableFor2[PeriodAdditionConvention, String] =
    Table(
      ("convention", "name"),
      (PeriodAdditionConvention.NONE, "None"),
      (PeriodAdditionConvention.LAST_DAY, "LastDay"),
      (PeriodAdditionConvention.LAST_BUSINESS_DAY, "LastBusinessDay"))

  //-------------------------------------------------------------------------
  test("test_null") {
    // Reinterpretation: the Java method supplied an absent date, an absent period and an
    // absent calendar in turn and asserted that each raised. This port writes no such value -
    // all three parameters are values of types that have no absent member - so what is
    // asserted in its place is that `adjust` is total over real arguments: every member
    // answers with a date for every combination below, and none of them raises.
    val dates: List[LocalDate] =
      List(
        LocalDate.of(2014, 7, 11),
        LocalDate.of(2014, 7, 31),
        LocalDate.of(2020, 2, 29),
        LocalDate.of(1900, 1, 1),
        LocalDate.of(2099, 12, 31))
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

    // The calendar is part of the signature of every member even though one member's rule
    // consults it, because a caller choosing a convention from data cannot know which member it
    // holds. That is observable: the two arithmetic members ignore the calendar they are given,
    // and the third does not. Two months is what makes the third case visible - 31 August 2014
    // was a Sunday, so the weekend calendar answers with the Friday before it and the calendar
    // with no holidays with the 31st itself, where one month lands on a Thursday both agree on.
    val monthEnd = LocalDate.of(2014, 6, 30)
    val twoMonths = Period.ofMonths(2)
    PeriodAdditionConvention.NONE.adjust(monthEnd, twoMonths, HolidayCalendars.SAT_SUN) shouldBe
      PeriodAdditionConvention.NONE.adjust(monthEnd, twoMonths, HolidayCalendars.NO_HOLIDAYS)
    PeriodAdditionConvention.LAST_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.SAT_SUN) shouldBe
      PeriodAdditionConvention.LAST_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.NO_HOLIDAYS)
    PeriodAdditionConvention.LAST_BUSINESS_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.SAT_SUN) shouldBe
      LocalDate.of(2014, 8, 29)
    PeriodAdditionConvention.LAST_BUSINESS_DAY.adjust(monthEnd, twoMonths, HolidayCalendars.NO_HOLIDAYS) shouldBe
      LocalDate.of(2014, 8, 31)
  }

  //-------------------------------------------------------------------------
  test("test_convention") {
    forAll(dataConvention) {
      (convention: PeriodAdditionConvention, input: LocalDate, months: Int, expected: LocalDate) =>
        withClue(s"${convention.name} on $input plus $months months: ") {
          convention.adjust(input, Period.ofMonths(months), HolidayCalendars.SAT_SUN) shouldBe expected
        }
    }
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
    // The Java factory both looked a name up exactly and reported an unknown one; the port
    // splits those into `valueOf`, the exact lookup, and `parse`, which reports the failure.
    // Both are asserted, so the two entry points cannot drift apart.
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
    // Reinterpretation: the Java method read `extendedEnum().lookupAll()`, the contents of the
    // registry the original assembled at run time from a configuration resource on the class
    // path. No registry exists here - the family is fixed when the source is compiled - and the
    // name lookup that replaces it is the `NamedEnum` instance of the companion, so its views
    // are what this test asserts.
    val lookup: NamedEnum[PeriodAdditionConvention] = NamedEnum[PeriodAdditionConvention]

    lookup.familyName shouldBe "PeriodAdditionConvention"
    lookup.values.toList shouldBe List(
      PeriodAdditionConvention.NONE,
      PeriodAdditionConvention.LAST_DAY,
      PeriodAdditionConvention.LAST_BUSINESS_DAY)

    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      withClue(s"$name: ") {
        // `byCanonicalName` is the normalised view, keyed by the name each member declares
        lookup.byCanonicalName.get(name) shouldBe Some(convention)
        // `byUpperName` is the case-folded view the original built by registering each member
        // a second time under the upper case of its name
        lookup.byUpperName.get(name.toUpperCase(Locale.ENGLISH)) shouldBe Some(convention)
      }
    }
    lookup.byCanonicalName should have size 3

    // Of the three tables a named family may declare, the configuration of the type being
    // ported declared only the lenient rewrites: it named no alternate spelling of a
    // convention and published no group of names for an external protocol. Asserting their
    // absence is asserting that the port added no name the original did not have.
    lookup.alternateNames shouldBe empty
    lookup.externalNameGroups shouldBe empty
    lookup.externalNames("FpML") shouldBe None
    lookup.lenientPatterns should have size 3
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as
    // a value. The reason is compared by value, and the message only for the text it has to
    // quote, so the diagnostic wording stays free to change.
    PeriodAdditionConvention.valueOf("Rubbish") shouldBe None
    PeriodAdditionConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    PeriodAdditionConvention.parse("Rubbish") should haveFailureMessageMatching(".*Rubbish.*")
    // text that is a name of the family with anything appended is not a name of the family
    PeriodAdditionConvention.valueOf("LastDayX") shouldBe None
    PeriodAdditionConvention.valueOf("Last Day") shouldBe None
    PeriodAdditionConvention.parse("Last Business Day") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed the absent reference to the factory and asserted
    // that it raised. This port writes no such reference and its factories take a name they
    // resolve as a value, so the case is asserted as the two spellings of an absent name that
    // can be supplied - the empty name and a blank one.
    PeriodAdditionConvention.valueOf("") shouldBe None
    PeriodAdditionConvention.parse("") should beFailureWith(FailureReason.PARSING)
    PeriodAdditionConvention.valueOf("   ") shouldBe None
    PeriodAdditionConvention.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_lenientLookup_constants") {
    // The Java method swept the public constants of the constants holder reflectively and
    // asserted that the lenient lookup resolved each field's own identifier, in its declared
    // case and in lower case. The identifiers are the three rows the configuration of the
    // original declared as lenient patterns, and they are transcribed into the family, so the
    // same property is asserted here over the three of them by name - which is what the
    // reflective sweep was a way of writing.
    val constants: TableFor2[String, PeriodAdditionConvention] =
      Table(
        ("identifier", "convention"),
        ("NONE", PeriodAdditionConvention.NONE),
        ("LAST_DAY", PeriodAdditionConvention.LAST_DAY),
        ("LAST_BUSINESS_DAY", PeriodAdditionConvention.LAST_BUSINESS_DAY))

    forAll(constants) { (identifier: String, convention: PeriodAdditionConvention) =>
      withClue(s"$identifier: ") {
        PeriodAdditionConvention.parse(identifier) should haveValue(convention)
        PeriodAdditionConvention.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        // the fold to upper case happens before the rewrites, so any mixture of case resolves
        PeriodAdditionConvention.parse(identifier.take(1) + identifier.drop(1).toLowerCase(Locale.ENGLISH)) should
          haveValue(convention)

        // Leniency belongs to `parse`, so an identifier that is not also a key of the exact
        // lookup does not resolve through `valueOf`. `NONE` is the exception, and it is the
        // original's exception too: it is simultaneously the lenient row and the upper-case
        // spelling of the canonical name `None`, which the lookup registers as a key, so the
        // exact lookup answers it - Java's own `find("NONE")` does, while `find("LAST_DAY")`
        // finds nothing because `LastDay` folds to `LASTDAY`.
        val isAlsoAnExactKey = identifier == convention.name.toUpperCase(Locale.ENGLISH)
        PeriodAdditionConvention.valueOf(identifier) shouldBe
          (if (isAlsoAnExactKey) Some(convention) else None)
      }
    }

    // stated for the three rows individually, so the rule above cannot be satisfied by a
    // lookup that resolves everything or nothing
    PeriodAdditionConvention.valueOf("NONE") shouldBe Some(PeriodAdditionConvention.NONE)
    PeriodAdditionConvention.valueOf("LAST_DAY") shouldBe None
    PeriodAdditionConvention.valueOf("LAST_BUSINESS_DAY") shouldBe None

    // A rewrite matches the whole of the text, so the shorter identifier cannot claim the
    // longer one. This is the one pair of rows where that could go wrong.
    PeriodAdditionConvention.parse("LAST_BUSINESS_DAY") should haveValue(PeriodAdditionConvention.LAST_BUSINESS_DAY)
    PeriodAdditionConvention.parse("LAST_DAY") should haveValue(PeriodAdditionConvention.LAST_DAY)
    // and a rewrite is applied to the whole text only, so neither identifier resolves with
    // anything appended to it
    PeriodAdditionConvention.parse("LAST_DAY_X") should beFailureWith(FailureReason.PARSING)
    PeriodAdditionConvention.parse("X_LAST_DAY") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Reinterpretation: the Java method swept the constants of a package-private enum and the
    // private constructor of the constants holder, reflectively. A sealed family of case
    // objects has no such constants to read back and the holder has no constructor to reach,
    // so the properties that sweep stood in for are asserted over the family's own closed
    // surface instead: the membership is exactly three distinct values, each is reachable by
    // its own name through both entry points, the constants holder publishes those same three
    // objects, and the typeclass instances agree with each other.
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

    // The constants holder exists so that a call site written against the original reads
    // unchanged; it publishes the members themselves rather than copies of them.
    PeriodAdditionConventions.NONE should be theSameInstanceAs PeriodAdditionConvention.NONE
    PeriodAdditionConventions.LAST_DAY should be theSameInstanceAs PeriodAdditionConvention.LAST_DAY
    PeriodAdditionConventions.LAST_BUSINESS_DAY should be theSameInstanceAs PeriodAdditionConvention.LAST_BUSINESS_DAY

    // The flag the adjustment types check when they are built: an end-of-month rule is
    // meaningful only for a period measured in months and years.
    PeriodAdditionConvention.NONE.isMonthBased shouldBe false
    PeriodAdditionConvention.LAST_DAY.isMonthBased shouldBe true
    PeriodAdditionConvention.LAST_BUSINESS_DAY.isMonthBased shouldBe true

    // The companion publishes one equality-bearing instance - an ordering that is also a
    // hashing - so summoning the equality, the hashing or the ordering yields that one value
    // and the three can never disagree. The assertions below are the observable form of that.
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
    // Reinterpretation: the Java method asserted the round trip of the reflective
    // string-conversion library the original registered with. The library is gone with the
    // port, and the guarantee it gave is asserted directly: a convention renders as its name,
    // and that rendering reads back as the same convention.
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      val rendered = Show[PeriodAdditionConvention].show(convention)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe convention.name
        PeriodAdditionConvention.parse(rendered) should haveValue(convention)
      }
    }
  }

  //-------------------------------------------------------------------------
  // The tests below are the port's own. The first three pin behaviour the Java provider rows
  // do not reach - the calendar-consulting rule, a period measured in days, and the month-end
  // and leap-year edges - and every expectation in them was evaluated against the published
  // Java `strata-basics` jar rather than derived by hand. The last pins the JSON
  // representation, which replaces the platform serialization the consolidated round-trip spec
  // no longer covers per type.

  test("test_adjust_lastBusinessDay_consultsCalendarOnBothSides") {
    // The rule of LAST_BUSINESS_DAY reads the calendar twice: once to decide whether the base
    // date is the last business day of its own month, and once to produce the last business day
    // of the end date's month. Both halves are visible below, and the fourth and fifth rows are
    // the pair that makes the first half observable - the Friday before a month end is the last
    // business day of November, the Saturday that ends it is not a business day at all, so the
    // two dates one day apart give results a month apart.
    val cases: TableFor4[LocalDate, Int, HolidayCalendar, LocalDate] =
      Table(
        ("base", "months", "calendar", "expected"),
        // Fri 28 Jun 2019 was the last business day of June; Wed 31 Jul 2019 is the last of July
        (LocalDate.of(2019, 6, 28), 1, HolidayCalendars.SAT_SUN, LocalDate.of(2019, 7, 31)),
        // Sun 30 Jun 2019 is not a business day, so the rule leaves arithmetic alone
        (LocalDate.of(2019, 6, 30), 1, HolidayCalendars.SAT_SUN, LocalDate.of(2019, 7, 30)),
        // Fri 30 Aug 2019 was the last business day of August under the London calendar
        (LocalDate.of(2019, 8, 30), 4, StandardHolidayCalendars.GBLO, LocalDate.of(2019, 12, 31)),
        // Fri 29 Nov 2019 was the last business day of November
        (LocalDate.of(2019, 11, 29), 1, StandardHolidayCalendars.GBLO, LocalDate.of(2019, 12, 31)),
        // Sat 30 Nov 2019 was not, so the end date stays where arithmetic put it
        (LocalDate.of(2019, 11, 30), 1, StandardHolidayCalendars.GBLO, LocalDate.of(2019, 12, 30)),
        // 31 Dec 2019 was a business day in London, and the last of its month
        (LocalDate.of(2019, 12, 31), 1, StandardHolidayCalendars.GBLO, LocalDate.of(2020, 1, 31)),
        // with no holidays every day is a business day, so the rule reduces to the last day
        (LocalDate.of(2014, 6, 30), 2, HolidayCalendars.NO_HOLIDAYS, LocalDate.of(2014, 8, 31)))

    forAll(cases) { (base: LocalDate, months: Int, calendar: HolidayCalendar, expected: LocalDate) =>
      withClue(s"$base plus $months months against ${calendar.id.name}: ") {
        PeriodAdditionConvention.LAST_BUSINESS_DAY.adjust(base, Period.ofMonths(months), calendar) shouldBe expected
      }
    }
  }

  test("test_adjust_dayBasedPeriod") {
    // `adjust` does not check that the period it is given is month-based; the adjustment types
    // that carry a convention do that when they are built. So a day-based period reaches the
    // end-of-month rules, and they apply as written - the base date being a month end is what
    // they test, whatever the period was measured in. These are the values the Java
    // implementation produces, and they are the reason the check exists upstream.
    val monthEnd = LocalDate.of(2019, 6, 30)
    val tenDays = Period.ofDays(10)
    PeriodAdditionConvention.NONE.adjust(monthEnd, tenDays, HolidayCalendars.SAT_SUN) shouldBe LocalDate.of(2019, 7, 10)
    PeriodAdditionConvention.LAST_DAY.adjust(monthEnd, tenDays, HolidayCalendars.SAT_SUN) shouldBe
      LocalDate.of(2019, 7, 31)
    PeriodAdditionConvention.LAST_BUSINESS_DAY
      .adjust(LocalDate.of(2019, 6, 28), tenDays, HolidayCalendars.SAT_SUN) shouldBe LocalDate.of(2019, 7, 31)
  }

  test("test_adjust_monthEndAndLeapYear") {
    // Ordinary arithmetic already shortens a day of month the target month does not have, which
    // is why the three members agree on 31 January plus one month; the end-of-month rules differ
    // from it only where the base date is its month's end, and February is where that is worth
    // asserting.
    PeriodAdditionConvention.NONE.adjust(LocalDate.of(2019, 1, 31), Period.ofMonths(1), HolidayCalendars.SAT_SUN) shouldBe
      LocalDate.of(2019, 2, 28)
    PeriodAdditionConvention.LAST_DAY
      .adjust(LocalDate.of(2019, 1, 31), Period.ofMonths(1), HolidayCalendars.SAT_SUN) shouldBe LocalDate.of(2019, 2, 28)
    // November has 30 days and February 2020 had 29, so the rule pulls the end date out to the
    // 29th where arithmetic would have stopped on the 28th
    PeriodAdditionConvention.LAST_DAY
      .adjust(LocalDate.of(2019, 11, 30), Period.ofMonths(3), HolidayCalendars.SAT_SUN) shouldBe LocalDate.of(2020, 2, 29)
    // and a leap day plus twelve months is the end of a February that has no 29th
    PeriodAdditionConvention.LAST_DAY
      .adjust(LocalDate.of(2020, 2, 29), Period.ofMonths(12), HolidayCalendars.SAT_SUN) shouldBe LocalDate.of(2021, 2, 28)
  }

  test("test_codec") {
    // The representation the consolidated JSON round-trip spec exercises generically is pinned
    // per type here: a convention is the bare string of its canonical name and never an object,
    // which is the single-string form the original wrote through its string conversion.
    forAll(dataName) { (convention: PeriodAdditionConvention, name: String) =>
      withClue(s"$name: ") {
        convention.asJson shouldBe Json.fromString(name)
        Json.fromString(name).as[PeriodAdditionConvention] shouldBe Right(convention)
      }
    }
    // decoding goes through `parse`, so a document written with a constant identifier or in
    // another case is accepted, and text naming no convention is a decoding failure rather
    // than an exception
    Json.fromString("LAST_BUSINESS_DAY").as[PeriodAdditionConvention] shouldBe
      Right(PeriodAdditionConvention.LAST_BUSINESS_DAY)
    Json.fromString("lastday").as[PeriodAdditionConvention] shouldBe Right(PeriodAdditionConvention.LAST_DAY)
    Json.fromString("Rubbish").as[PeriodAdditionConvention].isLeft shouldBe true
    Json.fromInt(1).as[PeriodAdditionConvention].isLeft shouldBe true
  }
}
