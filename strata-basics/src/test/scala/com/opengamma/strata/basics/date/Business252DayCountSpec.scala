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
 * Test the `Bus/252` day count, ported from the Java `Business252DayCountTest`.
 *
 * The Java original ran ten annotated methods, and all ten are here under the names they had:
 * `test_factory_name`, `test_factory_nameUpper`, `test_factory_calendar`, `test_yearFraction`,
 * `test_yearFraction_badOrder`, `test_days`, `test_equalsHashCode`, `coverage`,
 * `test_serialization` and `test_jodaConvert`. The names are the join key of the method-level
 * test mapping the migration keeps, so none of them may drift.
 *
 * ===What this spec is about===
 *
 * `Bus/252` counts the business days of a holiday calendar and divides by two hundred and
 * fifty-two, so - alone among the day counts - the calendar is part of the convention and
 * appears in its name. The Java class that implemented it is folded into `DayCount.scala` as
 * the [[DayCount.Bus252]] member of the sealed family, which is why this spec's subject is
 * reached through [[DayCount]] and there is no `Business252DayCount` to name.
 *
 * The twenty-one standard members, the lenient and external name tables and the shared surface
 * of the family belong to `DayCountSpec`; the 1e-9 comparison of `Bus/252 BRBD` against the
 * captured Java baseline belongs to `parity/DayCountParitySpec`; the invalid-input matrices of
 * every validated type belong to `SmartConstructorSpec`; and the property-based round trip over
 * every codec-bearing type belongs to `json/JsonRoundTripSpec`. What is asserted here is what
 * the Java methods asserted, over the one member of the family that carries a calendar.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - '''The calendar is supplied rather than looked up.''' The Java factory `ofBus252(id)`
 *     resolved the identifier against `ReferenceData.standard()` reached from inside itself, and
 *     the name lookup loaded `Bus/252 XXXX` from the same ambient set. This port has
 *     [[DayCount.ofBus252(calendar:com.opengamma.strata.basics.date.HolidayCalendar)*]], which
 *     takes the '''resolved''' calendar and is total, and
 *     [[DayCount.ofBus252(id:com.opengamma.strata.basics.date.HolidayCalendarId,refData:com.opengamma.strata.basics.ReferenceData)*]],
 *     which takes the reference data explicitly and reports an unresolvable identifier as a
 *     failure. Both are exercised below, as are both parse paths: [[DayCount.parse(name:String)*]]
 *     resolves against the calendars built into this library - constant data rather than ambient
 *     state - and the overload taking reference data resolves against whatever the caller
 *     supplies. Removing the ambient lookup is a headline divergence of the migration, and this
 *     spec is where the replacement is proved.
 *   - '''Reference identity is not part of the contract.''' Three of the Java methods asserted
 *     `isSameAs`, which held because that implementation cached one instance per calendar name.
 *     Nothing here caches, so each assertion below is value equality instead - which is the
 *     property a caller can rely on, and which is exactly what the Java assertion was standing
 *     in for. Equality of the family is by name, and a `Bus/252` name embeds its calendar's
 *     identifier, so two day counts built over calendars of the same identifier are equal
 *     whatever their holidays.
 *   - '''Failures that depend on data are values.''' `DayCount.of` raised for text it did not
 *     recognise and for a calendar it could not load; `parse` and the identifier-taking factory
 *     return `Left` instead, so the two assertions on those paths compare a reason of the closed
 *     family of reasons rather than catching anything. The one throw asserted below is the
 *     date-order precondition, which guards a caller's contract rather than reacting to market
 *     data and is therefore retained as a throw by design.
 *   - '''`coverPrivateConstructor` has no target.''' It reflectively invoked the private
 *     constructor of the Java implementation class so that a coverage tool would not report it as
 *     unexercised. That class is folded into the sealed family, this port performs no reflection,
 *     and `coverage` asserts what the call stood for: that the member can be obtained only
 *     through its two factories, that its name follows from its calendar, and that its `Hash` and
 *     `Show` behave over two distinct calendars.
 *   - '''`assertSerialization` and `assertJodaConvert` have no target.''' Neither Java
 *     serialization nor Joda-Convert is supported by this port. `test_serialization` asserts the
 *     JSON form of AAP section 0.6.4, which for this member is structural rather than a bare name
 *     - the asymmetry exists so that a calendar an application built itself survives the round
 *     trip with its holidays intact - and `test_jodaConvert` asserts the pair that replaces the
 *     string conversion, `Show` and `parse`.
 */
class Business252DayCountSpec extends AnyFunSuite with Matchers {

  /** The reference data every resolution in this spec is threaded through, named as in the Java original. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /**
   * A value of an unrelated type, for the equality assertion of `test_equalsHashCode`.
   *
   * Typed as `Any` rather than as `String`, exactly as the Java field was typed as `Object`.
   * Comparing a day count with a `String` through `==` is a comparison of unrelated types, which
   * this build reports as a warning and then treats as an error, so the assertion uses the
   * `equals` method and this field carries the type that makes the call legitimate.
   */
  private val ANOTHER_TYPE: Any = ""

  /** The canonical name of the day count over the Europe/TARGET calendar. */
  private val BUS_252_EUTA: String = "Bus/252 EUTA"

  /** The canonical name of the day count over the London calendar. */
  private val BUS_252_GBLO: String = "Bus/252 GBLO"

  /**
   * The Europe/TARGET calendar, resolved once.
   *
   * The Java methods wrote `EUTA.resolve(REF_DATA)` inside the body of a three-hundred-and-
   * sixty-six iteration loop, which resolved the same identifier on every pass. Resolving it
   * once here changes no expectation - resolution is a lookup in an immutable store - and makes
   * the two walks below measure the day count rather than the store.
   */
  private val EUTA_CALENDAR: HolidayCalendar = resolved(HolidayCalendarIds.EUTA)

  /** The London calendar, resolved once, for the assertions of `test_factory_calendar`. */
  private val GBLO_CALENDAR: HolidayCalendar = resolved(HolidayCalendarIds.GBLO)

  /** The fixed first date of both walks, as in the Java original. */
  private val DATE_2014_12_01: LocalDate = date(2014, 12, 1)

  /**
   * The three hundred and sixty-six second dates of both walks.
   *
   * The Java loop advanced a mutable cursor by one day per iteration, starting from the first
   * date, so the dates run from 2014-12-01 to 2015-12-01 inclusive and the first pass compares a
   * period of no length. The same series is built here as an immutable list, so neither walk can
   * observe a date the other has moved on from.
   */
  private val SECOND_DATES: List[LocalDate] =
    Iterator.iterate(DATE_2014_12_01)(current => current.plusDays(1L)).take(366).toList

  /** An identifier no calendar of the standard reference data answers for. */
  private val UNKNOWN_CALENDAR_ID: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  //-------------------------------------------------------------------------
  test("test_factory_name") {
    // The Java method read the day count out of the registry by name; the port's `parse` reports
    // text it cannot resolve as a value, so the outcome is asserted before it is unwrapped.
    val parsed = DayCount.parse(BUS_252_EUTA)
    parsed should beSuccess

    val test = dayCountOf(BUS_252_EUTA)
    test.name shouldBe BUS_252_EUTA
    test.toString shouldBe BUS_252_EUTA
    Show[DayCount].show(test) shouldBe BUS_252_EUTA

    // The Java assertions here were `isSameAs`, which held because that implementation cached one
    // instance per calendar name. Reference identity is deliberately not part of this port's
    // contract - nothing caches - so the same statement is made as value equality, which is what
    // a caller can rely on and what the identity assertion stood in for.
    parsed shouldBe Right(test)
    DayCount.parse(BUS_252_EUTA) shouldBe Right(test)
    DayCount.valueOf(BUS_252_EUTA) shouldBe Some(test)

    // Both factories answer with the same value as the name lookup: the total one over the
    // resolved calendar, and the one that resolves an identifier against reference data the
    // caller passes - which is the explicit form of the Java `ofBus252(EUTA)` this spec ports.
    DayCount.ofBus252(EUTA_CALENDAR) shouldBe test
    DayCount.ofBus252(HolidayCalendarIds.EUTA, REF_DATA) shouldBe Right(test)
    Hash[DayCount].eqv(DayCount.ofBus252(EUTA_CALENDAR), test) shouldBe true

    // Text this family has never accepted is a failure carrying a reason rather than a raised
    // error, and the reason is compared as a value of the closed family of reasons.
    DayCount.parse("Rubbish") should beFailureWith(FailureReason.PARSING)

    // Including a `Bus/252` name whose calendar this library does not define: the prefix is
    // recognised, so the failure reported is the calendar lookup's own rather than this family's.
    DayCount.parse("Bus/252 XXXX") should beFailureWith(FailureReason.PARSING)
    DayCount.valueOf("Bus/252 XXXX") shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_factory_nameUpper") {
    // The upper-case key space of the name lookup, which the registry being replaced filed every
    // value under as well. The name of the result is rebuilt from the resolved calendar rather
    // than copied from the input, so the canonical casing is restored.
    val test = dayCountOf("BUS/252 EUTA")
    test.name shouldBe BUS_252_EUTA
    test.toString shouldBe BUS_252_EUTA

    // Value equality where the Java method asserted reference identity, for the reason given in
    // `test_factory_name`.
    DayCount.parse(BUS_252_EUTA) shouldBe Right(test)
    DayCount.ofBus252(HolidayCalendarIds.EUTA, REF_DATA) shouldBe Right(test)

    // The exact lookup accepts the upper-case spelling as well, and does not apply any lenient
    // rewrite to reach it - the prefix itself is matched without regard to case.
    DayCount.valueOf("BUS/252 EUTA") shouldBe Some(test)
    DayCount.valueOf("bus/252 EUTA") shouldBe Some(test)

    // The calendar part, in contrast, is a calendar name and is matched as one: the registry
    // being replaced filed calendars under their canonical and upper-case names only.
    DayCount.parse("BUS/252 euta") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_factory_calendar") {
    // The total factory, over a calendar resolved once by this spec. The Java method passed the
    // identifier and let the factory resolve it against ambient reference data; here the caller
    // holds the resolved calendar, which is what keeps `yearFraction` and `days` pure functions
    // of their arguments.
    val test = DayCount.ofBus252(GBLO_CALENDAR)
    test.name shouldBe BUS_252_GBLO
    test.toString shouldBe BUS_252_GBLO

    // Value equality with both of the other two ways of reaching the same day count, where the
    // Java method asserted reference identity.
    DayCount.parse(BUS_252_GBLO) shouldBe Right(test)
    DayCount.ofBus252(HolidayCalendarIds.GBLO, REF_DATA) shouldBe Right(test)
    DayCount.valueOf(BUS_252_GBLO) shouldBe Some(test)

    // The calendar carried is the one that was handed over, so the day count counts the days of
    // that calendar and not of one it looked up for itself.
    calendarOf(test) shouldBe GBLO_CALENDAR
    calendarOf(test).id shouldBe HolidayCalendarIds.GBLO

    // The reference-data overload reports an identifier the data cannot answer for as a failure,
    // with the reason a missing entry carries. This is the whole of the difference between the
    // two factories: the total one cannot fail, and this one fails only through resolution.
    DayCount.ofBus252(UNKNOWN_CALENDAR_ID, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // And the parse overload taking reference data resolves against what the caller supplies
    // rather than against the calendars built into this library.
    DayCount.parse(BUS_252_GBLO, REF_DATA) shouldBe Right(test)
    DayCount.parse("Bus/252 XXXX", REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction") {
    val test = dayCountOf(BUS_252_EUTA)
    val eutaCal = EUTA_CALENDAR

    // The Java walk, transcribed: the first date is fixed and the second advances by one day for
    // three hundred and sixty-six iterations. The expectation is the definition of the
    // convention, so the assertion is that the day count delegates to its calendar and divides
    // by two hundred and fifty-two - the numerator is converted explicitly rather than widened
    // implicitly, and the comparison is of one `Double` with another.
    SECOND_DATES.foreach { secondDate =>
      withClue(s"$DATE_2014_12_01 to $secondDate: ") {
        test.yearFraction(DATE_2014_12_01, secondDate) shouldBe
          eutaCal.daysBetween(DATE_2014_12_01, secondDate).toDouble / 252d
      }
    }

    // The walk starts at a period of no length and ends a year and a day later, which is worth
    // stating once so that the series is not merely asserted against itself.
    test.yearFraction(DATE_2014_12_01, DATE_2014_12_01) shouldBe 0d
    SECOND_DATES.last shouldBe date(2015, 12, 1)
    test.yearFraction(DATE_2014_12_01, SECOND_DATES.last) should be > 0d

    // The three-argument overload reads no schedule information for this convention, so it
    // agrees with the two-argument one for every date of the walk.
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

    // Dates supplied in time-line order is a precondition on the caller rather than a property
    // of market data, so it is one of the two throws this port deliberately retains: it is
    // checked fast through `ArgCheck` and raises `IllegalArgumentException`, exactly as the
    // implementation being ported did, and it is NOT reported as a `Left`. Every data-dependent
    // failure of this spec is asserted as a value instead; this is the single `intercept` here.
    val thrown = intercept[IllegalArgumentException](test.yearFraction(date1, date2))
    thrown.getMessage should include("time-line order")

    // The relative form is the method that accepts a reversed pair, and it negates rather than
    // refusing - which is what makes the check above a statement about `yearFraction` and not
    // about the convention being unable to look backwards.
    test.relativeYearFraction(date1, date2) shouldBe -test.yearFraction(date2, date1)
  }

  //-------------------------------------------------------------------------
  test("test_days") {
    val test = dayCountOf(BUS_252_EUTA)
    val eutaCal = EUTA_CALENDAR

    // The same walk as `test_yearFraction`, over the numerator of the division. Both sides are
    // whole counts of business days, so the comparison is integral throughout.
    SECOND_DATES.foreach { secondDate =>
      withClue(s"$DATE_2014_12_01 to $secondDate: ") {
        test.days(DATE_2014_12_01, secondDate) shouldBe eutaCal.daysBetween(DATE_2014_12_01, secondDate)
      }
    }

    // The count and the year fraction are the two halves of one definition, which is worth
    // stating directly: the fraction is the count over two hundred and fifty-two.
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
    // `equals` rather than `==`, because the static types of a day count and of the field below
    // are unrelated and this build refuses that comparison at compile time.
    a.equals(ANOTHER_TYPE) shouldBe false
    a.equals(null) shouldBe false
    a.hashCode shouldBe a.hashCode

    // Equality of this family is by name, and a `Bus/252` name embeds the identifier of its
    // calendar - which is why two day counts over different calendars are unequal without their
    // holidays ever being compared, and why one built afresh over the same calendar is equal.
    a.equals(DayCount.ofBus252(EUTA_CALENDAR)) shouldBe true
    a.hashCode shouldBe BUS_252_EUTA.hashCode
    a.equals(DayCounts.ACT_365F) shouldBe false

    // The cats instances are the same statement made through the typeclass. `Order` extends `Eq`
    // and `Hash` extends `Eq`, so the family declares exactly one equality-bearing instance and
    // the three can never disagree with each other or with `equals`.
    Hash[DayCount].eqv(a, a) shouldBe true
    Hash[DayCount].eqv(a, b) shouldBe false
    Hash[DayCount].eqv(a, DayCount.ofBus252(EUTA_CALENDAR)) shouldBe true
    Hash[DayCount].hash(a) shouldBe a.hashCode
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverPrivateConstructor(Business252DayCount.class)`: it reflectively
    // invoked the private constructor of the class implementing this convention so that a
    // coverage tool would not report it as unexercised. That class is folded into
    // `DayCount.scala` as the `Bus252` member of the sealed family, this port performs no
    // reflection of any kind, and the call therefore has no target. What it stood for is
    // asserted directly below.
    val euta = DayCount.ofBus252(EUTA_CALENDAR)
    val gblo = DayCount.ofBus252(GBLO_CALENDAR)

    // The name follows from the calendar rather than being supplied alongside it, so the two
    // cannot disagree.
    euta.name shouldBe "Bus/252 " + EUTA_CALENDAR.id.name
    gblo.name shouldBe "Bus/252 " + GBLO_CALENDAR.id.name
    calendarOf(euta) shouldBe EUTA_CALENDAR
    calendarOf(gblo) shouldBe GBLO_CALENDAR

    // The member can be reached only through its two factories and the two parse methods. It is
    // not a case class and its constructor is visible to its own companion alone, so there is no
    // constructor, no `apply` and no `copy` for a caller to bypass the factories with - which is
    // what makes the name above impossible to forge, and which the compiler is asked to confirm.
    // The third snippet is written over a value of the member type rather than of the family, so
    // that the only reason it is rejected is the absence of `copy`.
    assertDoesNotCompile("new DayCount.Bus252(HolidayCalendars.SAT_SUN)")
    assertDoesNotCompile("DayCount.Bus252(HolidayCalendars.SAT_SUN)")
    assertDoesNotCompile("bus252Of(DayCount.ofBus252(HolidayCalendars.SAT_SUN)).copy()")

    // And the family is sealed, so no member of it can be declared outside the file that
    // declares the family itself.
    assertDoesNotCompile("class OtherDayCount extends DayCount(\"Other\")")

    // `Hash` and `Show` over two distinct calendars: distinct names, distinct hashes, and a
    // rendering that is the name and agrees with `toString`.
    Hash[DayCount].eqv(euta, gblo) shouldBe false
    Hash[DayCount].hash(euta) should not be Hash[DayCount].hash(gblo)
    Show[DayCount].show(euta) shouldBe BUS_252_EUTA
    Show[DayCount].show(gblo) shouldBe BUS_252_GBLO
    Show[DayCount].show(euta) shouldBe euta.toString

    // The standard members of the family are enumerable and this one is not - there is one
    // `Bus/252` day count per calendar - which is the line the library being ported drew as
    // well, listing the standard constants from one provider and creating these on demand.
    DayCount.values.toList should have size 21
    DayCount.values.toList should not contain euta
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The Java method asserted a Java-serialization round trip, which no type of this port
    // supports. The equivalent is the JSON form of AAP section 0.6.4.
    //
    // Every other day count is written as the bare string of its name, because the name locates
    // the same value again. This one is written structurally, because its calendar is part of it
    // and a calendar an application built itself has to survive the round trip with its holidays
    // intact. That asymmetry is deliberate and is what this test exists to pin down: a round
    // trip alone cannot tell the two forms apart, so the document is asserted in full.
    val test = DayCount.ofBus252(EUTA_CALENDAR)
    val document = test.asJson
    val expected: Json = parse("""{"Bus252":{"name":"Bus/252 EUTA","calendar":"EUTA"}}""")
      .getOrElse(fail("the expected JSON of this test is not valid JSON"))
    document shouldBe expected

    // Stated on the shape as well, so the assertion above cannot pass for the wrong reason: an
    // object of exactly one field, named for this form, carrying the name and the calendar.
    document.isObject shouldBe true
    document.isString shouldBe false
    document.asObject.map(fields => fields.keys.toList) shouldBe Some(List("Bus252"))
    val fields = document.hcursor.downField("Bus252")
    fields.get[String]("name") shouldBe Right(BUS_252_EUTA)
    fields.get[HolidayCalendar]("calendar") shouldBe Right(EUTA_CALENDAR)

    // And it reads back as the same value.
    decode[DayCount](document.noSpaces) shouldBe Right(test)

    // Equal values encode to identical bytes, which is what makes a stored document comparable
    // with a freshly written one.
    DayCount.ofBus252(EUTA_CALENDAR).asJson.noSpaces shouldBe document.noSpaces
    DayCount.ofBus252(GBLO_CALENDAR).asJson.noSpaces should not be document.noSpaces

    // The decoder also accepts the bare name, which is the form a hand-written document carries.
    // A name is resolved through `parse`, so the calendar of a `Bus/252` name comes from the
    // calendars built into this library.
    decode[DayCount](Json.fromString(BUS_252_EUTA).noSpaces) shouldBe Right(test)
    decode[DayCount](Json.fromString("BUS/252 EUTA").noSpaces) shouldBe Right(test)

    // Contrasted with a standard member, which is a bare string in both directions.
    DayCounts.ACT_365F.asJson shouldBe Json.fromString("Act/365F")
    decode[DayCount](DayCounts.ACT_365F.asJson.noSpaces) shouldBe Right(DayCounts.ACT_365F)

    // The two fields of the structural form are not independent: the name of such a day count
    // follows from its calendar, so a document that names one calendar and carries another
    // describes no day count this library can build and is rejected. Without the check the value
    // read back would count the wrong days under a name that hid it.
    val mismatched = decode[DayCount]("""{"Bus252":{"name":"Bus/252 GBLO","calendar":"EUTA"}}""")
    mismatched.isLeft shouldBe true
    mismatched.swap.map(failure => failure.getMessage).getOrElse("") should include(BUS_252_EUTA)

    // As is a document that is neither of the two accepted shapes.
    decode[DayCount]("""{"Bus252":{"name":"Bus/252 EUTA"}}""").isLeft shouldBe true
    decode[DayCount]("42").isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_jodaConvert") {
    // The Java method asserted a Joda-Convert round trip through the string conversion of the
    // type. Joda-Convert is not on this port's class path; the pair that replaces it is `Show`,
    // which renders a day count as its canonical name, and `parse`, which reads that name back.
    val test = DayCount.ofBus252(EUTA_CALENDAR)
    val shown = Show[DayCount].show(test)

    shown shouldBe BUS_252_EUTA
    shown shouldBe test.name
    shown shouldBe test.toString

    DayCount.parse(shown) shouldBe Right(test)
    DayCount.parse(shown, REF_DATA) shouldBe Right(test)
    DayCount.valueOf(shown) shouldBe Some(test)

    // The same round trip for a day count over a second calendar, so the assertion is about the
    // name carrying the calendar rather than about one name that happens to read back.
    val london = DayCount.ofBus252(GBLO_CALENDAR)
    DayCount.parse(Show[DayCount].show(london)) shouldBe Right(london)
    DayCount.parse(Show[DayCount].show(london)) should not be Right(test)
  }

  //-------------------------------------------------------------------------
  /**
   * Parses the day count of the specified name for use as a fixture.
   *
   * The Java original read a day count out of a registry that raised for text it did not
   * recognise, so a call site had a value to work with. This port reports such text as a `Left`,
   * and a fixture that cannot be built is a defect in this spec rather than a property of the
   * subject - so the outcome is threaded and an unexpected failure is reported as a failed test
   * naming its cause, rather than being forced with `get`. Every assertion this spec makes about
   * a failing parse is made on the outcome itself, not through here.
   *
   * @param name  the canonical or upper-case name of the day count
   * @return the day count of that name
   */
  private def dayCountOf(name: String): DayCount =
    DayCount.parse(name) match {
      case Right(dayCount) => dayCount
      case Left(failures) =>
        fail(
          s"Fixture day count '$name' could not be parsed: " +
            failures.toChain.toList.map(failure => failure.message).mkString("; "))
    }

  /**
   * Resolves a holiday calendar against the standard reference data for use as a fixture.
   *
   * Threaded for the reason given on [[dayCountOf]]: the identifiers this spec resolves are
   * built into the library, so a failure here is a defect in the spec.
   *
   * @param id  the identifier of the calendar
   * @return the resolved calendar
   */
  private def resolved(id: HolidayCalendarId): HolidayCalendar =
    id.resolve(REF_DATA) match {
      case Right(calendar) => calendar
      case Left(failure) => fail(s"Fixture calendar '${id.name}' could not be resolved: ${failure.message}")
    }

  /**
   * Narrows a day count to the `Bus/252` member of the sealed family.
   *
   * Reached by a type pattern over the family, so no cast and no reflection is involved. A day
   * count of any other kind is a defect in the assertion that called this, and is reported as
   * one. The narrowing exists because the factories declare the family as their result type, and
   * two assertions of `coverage` are about the member itself: the calendar it carries, and the
   * `copy` it does not have.
   *
   * @param dayCount  the day count, expected to be a `Bus/252` convention
   * @return the same day count, typed as the `Bus/252` member
   */
  private def bus252Of(dayCount: DayCount): DayCount.Bus252 =
    dayCount match {
      case bus252: DayCount.Bus252 => bus252
      case other => fail(s"Expected a 'Bus/252' day count but was: $other")
    }

  /**
   * Reads the calendar out of a `Bus/252` day count.
   *
   * @param dayCount  the day count, expected to be a `Bus/252` convention
   * @return the calendar it counts the business days of
   */
  private def calendarOf(dayCount: DayCount): HolidayCalendar =
    bus252Of(dayCount).calendar
}
