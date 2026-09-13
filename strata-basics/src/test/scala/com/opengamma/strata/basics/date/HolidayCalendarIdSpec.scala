/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.ImmutableReferenceData
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[HolidayCalendarId]].
 *
 * An identifier is equal, hashed and ordered by its name alone, so identifiers of the same name
 * are equal values without being the same object and nothing here asserts object identity - the
 * structure an identifier carries for resolution is an implementation detail. Its type parameter
 * carries the type of the value it resolves to, so pairing an identifier with a value is checked
 * by the compiler.
 *
 * Resolution tries the whole name first - which lets a host supply `GBLO+USNY` as one
 * pre-combined calendar - and only then the components, combining them with `combinedWith` for
 * `'+'` and `linkedWith` for `'~'`. That assembly lives on `HolidayCalendarId.resolve` and not on
 * the store: reference data is a plain map, so `refData.getValue(composite)` reports a composite
 * as absent even where both its parts are held, and [[HolidaySafeReferenceData]] relies on that
 * division when it leaves a composite unresolved so that its parts are defaulted one by one. A
 * missing simple identifier and a missing component are both `Failure.MissingData`, the second
 * naming the missing part and the composite being built as attributes. Where a component of a
 * linked identifier is itself composite it resolves by its own whole name before its own parts,
 * which `test_of_linked_resolve` states.
 *
 * `defaultByCurrency` is one total lookup returning `Option`, so a currency with no conventional
 * calendar answers `None` rather than failing. The JSON document of an identifier is a bare
 * string of its name and never an object, asserted as concrete text in `test_serialization`.
 */
class HolidayCalendarIdSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The reference data used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The US Independence Day holiday of 2019, a business day in Prague and in London. */
  private val US_HOLIDAY_2019: LocalDate = LocalDate.of(2019, 7, 4)

  /** The Czech Liberation Day holiday of 2019, a business day in New York and in London. */
  private val CZ_HOLIDAY_2019: LocalDate = LocalDate.of(2019, 5, 8)

  /** New Year's Day 2019, a holiday in every centre this spec names. */
  private val NEW_YEAR_2019: LocalDate = LocalDate.of(2019, 1, 1)

  /**
   * A value of another type, typed as `Any` so that `equals` can be handed it: a `String` given
   * directly would let the compiler decide the comparison can never hold.
   */
  private val ANOTHER_TYPE: Any = ""

  /**
   * The market-convention calendar of each currency, transcribed from the holiday-calendar
   * default data in the order that source lists the rows. Some rows name calendars this library
   * does not ship, which is why `test_defaultByCurrency` asserts the two halves separately: the
   * lookup answering is not the same statement as the identifier resolving.
   */
  private val dataDefaultByCurrency: TableFor2[Currency, HolidayCalendarId] = Table(
    ("currency", "calendarId"),
    (Currency.CHF, HolidayCalendarIds.CHZU),
    (Currency.EUR, HolidayCalendarIds.EUTA),
    (Currency.GBP, HolidayCalendarIds.GBLO),
    (Currency.JPY, HolidayCalendarIds.JPTO),
    (Currency.USD, HolidayCalendarIds.USNY),
    (Currency.AUD, HolidayCalendarIds.AUSY),
    (Currency.BRL, HolidayCalendarIds.BRBD),
    (Currency.CAD, HolidayCalendarIds.CATO),
    (Currency.CZK, HolidayCalendarIds.CZPR),
    (Currency.DKK, HolidayCalendarIds.DKCO),
    (Currency.HUF, HolidayCalendarIds.HUBU),
    (Currency.MXN, HolidayCalendarIds.MXMC),
    (Currency.NOK, HolidayCalendarIds.NOOS),
    (Currency.NZD, HolidayCalendarIds.NZAU),
    (Currency.PLN, HolidayCalendarIds.PLWA),
    (Currency.SEK, HolidayCalendarIds.SEST),
    (Currency.ZAR, HolidayCalendarIds.ZAJO),
    (Currency.CLP, HolidayCalendarId.of("CLSA")),
    (Currency.CNY, HolidayCalendarId.of("CNBE")),
    (Currency.COP, HolidayCalendarId.of("COBO")),
    (Currency.HKD, HolidayCalendarId.of("HKHK")),
    (Currency.IDR, HolidayCalendarId.of("IDJA")),
    (Currency.ILS, HolidayCalendarId.of("ILTA")),
    (Currency.INR, HolidayCalendarId.of("INMU")),
    (Currency.KRW, HolidayCalendarId.of("KRSE")),
    (Currency.RUB, HolidayCalendarId.of("RUMO")),
    (Currency.SAR, HolidayCalendarId.of("SARI")),
    (Currency.SGD, HolidayCalendarId.of("SGSI")),
    (Currency.THB, HolidayCalendarIds.THBA),
    (Currency.TRY, HolidayCalendarId.of("TRIS")),
    (Currency.TWD, HolidayCalendarId.of("TWTA"))
  )

  /** Conventional calendars this library does not ship: named by the lookup, but unresolvable. */
  private val notShippedCalendarNames: Set[String] =
    Set(
      "CLSA",
      "CNBE",
      "COBO",
      "HKHK",
      "IDJA",
      "ILTA",
      "INMU",
      "KRSE",
      "RUMO",
      "SARI",
      "SGSI",
      "TRIS",
      "TWTA")

  /** The constants of [[HolidayCalendarIds]], each with the name it carries. */
  private val dataConstants: TableFor2[HolidayCalendarId, String] = Table(
    ("constant", "name"),
    (HolidayCalendarIds.NO_HOLIDAYS, "NoHolidays"),
    (HolidayCalendarIds.SAT_SUN, "Sat/Sun"),
    (HolidayCalendarIds.FRI_SAT, "Fri/Sat"),
    (HolidayCalendarIds.THU_FRI, "Thu/Fri"),
    (HolidayCalendarIds.GBLO, "GBLO"),
    (HolidayCalendarIds.FRPA, "FRPA"),
    (HolidayCalendarIds.DEFR, "DEFR"),
    (HolidayCalendarIds.CHZU, "CHZU"),
    (HolidayCalendarIds.EUTA, "EUTA"),
    (HolidayCalendarIds.USGS, "USGS"),
    (HolidayCalendarIds.USNY, "USNY"),
    (HolidayCalendarIds.NYFD, "NYFD"),
    (HolidayCalendarIds.NYSE, "NYSE"),
    (HolidayCalendarIds.JPTO, "JPTO"),
    (HolidayCalendarIds.AUSY, "AUSY"),
    (HolidayCalendarIds.BRBD, "BRBD"),
    (HolidayCalendarIds.CAMO, "CAMO"),
    (HolidayCalendarIds.CATO, "CATO"),
    (HolidayCalendarIds.CZPR, "CZPR"),
    (HolidayCalendarIds.DKCO, "DKCO"),
    (HolidayCalendarIds.HUBU, "HUBU"),
    (HolidayCalendarIds.MXMC, "MXMC"),
    (HolidayCalendarIds.NOOS, "NOOS"),
    (HolidayCalendarIds.NZAU, "NZAU"),
    (HolidayCalendarIds.NZWE, "NZWE"),
    (HolidayCalendarIds.PLWA, "PLWA"),
    (HolidayCalendarIds.SEST, "SEST"),
    (HolidayCalendarIds.THBA, "THBA"),
    (HolidayCalendarIds.ZAJO, "ZAJO")
  )

  //-------------------------------------------------------------------------
  test("test_of_single") {
    val test: HolidayCalendarId = HolidayCalendarId.of("GB")
    test.name shouldBe "GB"
    test.toString shouldBe "GB"
    test.isComposite shouldBe false

    // The binding below is annotated `HolidayCalendar` deliberately: the test would not build if
    // resolving an identifier of this type answered with anything else.
    val refData: ReferenceData = ImmutableReferenceData.of(test, HolidayCalendars.SAT_SUN)
    val cal: HolidayCalendar = test.resolve(refData) match {
      case Right(calendar) => calendar
      case Left(failure) => fail(s"'GB' should resolve against data holding it: ${failure.message}")
    }
    cal shouldBe HolidayCalendars.SAT_SUN

    // The same typing on the way in: an entry pairing it with another type does not compile.
    assertCompiles("""ReferenceData.Entry(HolidayCalendarId.of("GB"), HolidayCalendars.SAT_SUN)""")
    assertDoesNotCompile("""ReferenceData.Entry(HolidayCalendarId.of("GB"), "GB")""")

    // The factory is total: a name is taken as it stands and reads back through `of`.
    HolidayCalendarId.of(test.name) shouldBe test

    // That totality is the reason every text form of an identifier has to be its name and
    // nothing else. `of` accepts any text and a decoder reads an identifier straight out of a
    // document, so a name may hold a line break, another control character or any length of
    // text; the name is nevertheless the identity of the identifier and the text it travels as,
    // so the text form and the rendering answer with the whole of it, unaltered, and a caller
    // that compares, re-parses or re-serializes what it rendered gets its own text back. Making
    // such text safe for a line-oriented reader belongs to the places that write a diagnostic,
    // which the case below this one states on a failure quoting this very identifier.
    val forgedName: String = "GBLO\nWARN  the calendar resolved\u2028and again\r"
    val forged: HolidayCalendarId = HolidayCalendarId.of(forgedName)

    // The name, the identity, the document, the object key and both text forms are the same
    // text, exactly: this is the identifier the library being ported rendered from `getName()`
    // and `toString()`, and no route to it alters what a caller supplied.
    forged.name shouldBe forgedName
    HolidayCalendarId.of(forgedName) shouldBe forged
    forged.asJson shouldBe Json.fromString(forgedName)
    forged.asJson.asString shouldBe Some(forgedName)
    decode[HolidayCalendarId](forged.asJson.noSpaces) shouldBe Right(forged)
    decode[HolidayCalendarId](forged.asJson.noSpaces).map(id => id.name) shouldBe Right(forgedName)
    KeyEncoder[HolidayCalendarId].apply(forged) shouldBe forgedName
    forged.toString shouldBe forgedName
    Show[HolidayCalendarId].show(forged) shouldBe forgedName

    // And on a name of several thousand characters: the text form is that name too, so nothing
    // is shortened and no marker stands for anything left out, there being nothing left out.
    val longName: String = "Z" * 4096
    val longId: HolidayCalendarId = HolidayCalendarId.of(longName)
    longId.name shouldBe longName
    longId.name.length shouldBe 4096
    longId.toString shouldBe longName
    Show[HolidayCalendarId].show(longId) shouldBe longName
    longId.toString should not include "..."

    // The identifier's text form is what the composite renderings that name a calendar hand on,
    // so they carry the name as it stands as well - while the rendering of an ordinary
    // adjustment is the text it always was, character for character.
    val forgedAdjustment: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, forged)
    forgedAdjustment.toString shouldBe s"Following using calendar $forgedName"
    Show[BusinessDayAdjustment].show(forgedAdjustment) shouldBe forgedAdjustment.toString
    val forgedDays: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, forged)
    forgedDays.toString shouldBe s"3 business days using calendar $forgedName"
    Show[DaysAdjustment].show(forgedDays) shouldBe forgedDays.toString

    BusinessDayAdjustment
      .of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarId.of("GBLO+USNY"))
      .toString shouldBe "ModifiedFollowing using calendar GBLO+USNY"
    DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN).toString shouldBe
      "3 business days using calendar Sat/Sun"
  }

  //-------------------------------------------------------------------------
  // Without a counterpart in the test class being ported: the sink at which a hostile calendar
  // name is neutralised.
  //
  // A name is the identity of an identifier, so every text form of one is that name as it
  // stands, which `test_of_single` states. The property that a name reaching this library from
  // outside can neither fabricate a line of a log that holds a diagnostic about it (CWE-117) nor
  // grow that line to its own size (CWE-400) is therefore a property of the places that write a
  // diagnostic, and this is where it is asserted. The name is driven in through the ordinary
  // resolution API rather than composed into a message here, so the text asserted is the text a
  // caller that parsed such a name would actually have reached.
  test("a failure quoting a hostile calendar name keeps it whole and renders it as one line") {
    // The faithful half: the failure names the identifier it could not find in the message and
    // under the `id` attribute, both holding the whole of the name, because the question a
    // caller has to answer is which item of reference data to supply and a shortened or
    // rewritten name does not answer it.
    val forgedName: String = "GBLO\nWARN  the calendar resolved\u2028and again\r"
    val forgedFailure: Failure = unresolvable(HolidayCalendarId.of(forgedName))
    forgedFailure.reason shouldBe FailureReason.MISSING_DATA
    forgedFailure.message should include(forgedName)
    forgedFailure.attributes("id") shouldBe forgedName

    // The neutralised half, on the same failure: its text is one line holding no control
    // character, the line feed and the Unicode line separator appearing in it as the characters
    // of their escapes, so nothing the name says can be read as a line this library reported.
    val forgedRendering: String = Show[Failure].show(forgedFailure)
    forgedRendering should include("GBLO\\n")
    forgedRendering should include("\\u2028")
    forgedRendering.linesIterator.size shouldBe 1
    forgedRendering.exists(character => character.isControl) shouldBe false
    forgedFailure.toString shouldBe forgedRendering

    // And the bound, on a name of several thousand characters: the failure keeps every character
    // of it while its text carries the marker standing for what was left out and stays shorter
    // than the name, so no name a caller supplies can dominate a line that quotes it.
    val longName: String = "Z" * 4096
    val longFailure: Failure = unresolvable(HolidayCalendarId.of(longName))
    longFailure.message should include(longName)
    longFailure.attributes("id") shouldBe longName

    val longRendering: String = Show[Failure].show(longFailure)
    longRendering should not include longName
    longRendering should include("...")
    longRendering.length should be < longName.length
  }

  test("test_of_combined") {
    val test: HolidayCalendarId = HolidayCalendarId.of("GB+EU")
    test.name shouldBe "EU+GB"
    test.toString shouldBe "EU+GB"
    test.isComposite shouldBe true

    val test2: HolidayCalendarId = HolidayCalendarId.of("EU+GB")
    test shouldBe test2
    test.name shouldBe test2.name
    test.hashCode shouldBe test2.hashCode
    Hash[HolidayCalendarId].eqv(test, test2) shouldBe true

    // Names are sorted and deduplicated; a name containing no separator is taken as it is.
    HolidayCalendarId.of("USNY+GBLO").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of("GBLO+USNY").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of("Anything At All").name shouldBe "Anything At All"

    // An empty part is kept rather than discarded, and sorts before every name, which is why a
    // trailing separator moves to the front.
    HolidayCalendarId.of("GBLO+").name shouldBe "+GBLO"
    HolidayCalendarId.of("").name shouldBe ""
    HolidayCalendarId.of("").isComposite shouldBe false

    // One reference-data entry answers either spelling; the two separators stay distinct.
    HolidayCalendarId.of("USNY+GBLO") shouldBe HolidayCalendarId.of("GBLO+USNY")
    HolidayCalendarId.of("USNY~GBLO") shouldBe HolidayCalendarId.of("GBLO~USNY")
    HolidayCalendarId.of("USNY+GBLO") should not be HolidayCalendarId.of("GBLO~USNY")
  }

  test("test_of_combined_NoHolidays") {
    // The no-holidays calendar removes nothing from a combination, so it drops out of the name.
    val test: HolidayCalendarId = HolidayCalendarId.of("GB+NoHolidays+EU")
    test.name shouldBe "EU+GB"
    test.toString shouldBe "EU+GB"
    test shouldBe HolidayCalendarId.of("GB+EU")

    // And a combination that normalises down to one part is that part, not a composite of one.
    HolidayCalendarId.of("GB+NoHolidays") shouldBe HolidayCalendarId.of("GB")
    HolidayCalendarId.of("GB+NoHolidays").isComposite shouldBe false
    HolidayCalendarId.of("NoHolidays+NoHolidays") shouldBe HolidayCalendarIds.NO_HOLIDAYS
  }

  test("test_of_combined_resolve") {
    val holidayCalendarId: HolidayCalendarId = HolidayCalendarId.of("CZPR+USNY")
    val resolved = holidayCalendarId.resolve(REF_DATA)

    resolved should beSuccess
    resolved.map(calendar => calendar.name) should haveValue("CZPR+USNY")
    resolved.map(calendar => calendar.isBusinessDay(US_HOLIDAY_2019)) should haveValue(false)
    resolved.map(calendar => calendar.isBusinessDay(CZ_HOLIDAY_2019)) should haveValue(false)
    resolved.map(calendar => calendar.isBusinessDay(NEW_YEAR_2019)) should haveValue(false)

    resolved.map(calendar => calendar.id) should haveValue(holidayCalendarId)
  }

  test("test_of_linked") {
    val test: HolidayCalendarId = HolidayCalendarId.of("GB~EU")
    test.name shouldBe "EU~GB"
    test.toString shouldBe "EU~GB"
    test.isComposite shouldBe true

    val test2: HolidayCalendarId = HolidayCalendarId.of("EU~GB")
    test shouldBe test2
    test.name shouldBe test2.name
    test.hashCode shouldBe test2.hashCode
  }

  test("test_of_linked_NoHolidays") {
    // Linking with a calendar in which every day is a business day leaves no day for the others
    // to close, so the no-holidays identifier swallows a linked composite whole.
    val test: HolidayCalendarId = HolidayCalendarId.of("GB~NoHolidays~EU")
    test.name shouldBe "NoHolidays"
    test.toString shouldBe "NoHolidays"
    test shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.isComposite shouldBe false
  }

  test("test_of_linked_combined") {
    // `'+'` binds more tightly than `'~'`, so this name is a link of `GB` with the combination
    // of `EU` and `Fri/Sat`, and the parts of each level are sorted within that level.
    val test: HolidayCalendarId = HolidayCalendarId.of("GB~EU+Fri/Sat")
    test.name shouldBe "EU+Fri/Sat~GB"
    test.toString shouldBe "EU+Fri/Sat~GB"
    test.isComposite shouldBe true
    HolidayCalendarId.of("EU+Fri/Sat~GB") shouldBe test

    HolidayCalendarId.of("GBLO+USNY").linkedWith(HolidayCalendarIds.JPTO).name shouldBe
      "GBLO+USNY~JPTO"
    HolidayCalendarId.of("GBLO+USNY~JPTO") shouldBe
      HolidayCalendarId.of("GBLO+USNY").linkedWith(HolidayCalendarIds.JPTO)
    HolidayCalendarId.of("GBLO+EUTA~USNY").name shouldBe "EUTA+GBLO~USNY"

    HolidayCalendarIds.EUTA
      .combinedWith(HolidayCalendarIds.USNY)
      .combinedWith(HolidayCalendarIds.GBLO)
      .name shouldBe "EUTA+GBLO+USNY"
  }

  test("test_of_linked_resolve") {
    // A linked identifier observes only the holidays both centres agree on, so the two national
    // holidays are business days and only the shared New Year's Day remains a holiday.
    val holidayCalendarId: HolidayCalendarId = HolidayCalendarId.of("CZPR~USNY")
    val resolved = holidayCalendarId.resolve(REF_DATA)

    resolved should beSuccess
    resolved.map(calendar => calendar.name) should haveValue("CZPR~USNY")
    resolved.map(calendar => calendar.isBusinessDay(US_HOLIDAY_2019)) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(CZ_HOLIDAY_2019)) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(NEW_YEAR_2019)) should haveValue(false)
    resolved.map(calendar => calendar.id) should haveValue(holidayCalendarId)

    // The one behavioural divergence this spec states: a part of a linked identifier that is
    // itself '''composite''' resolves by asking that part, so the part's own whole name is tried
    // before its own parts are. `EUTA+GBLO~USNY` therefore resolves against the standard
    // reference data, which holds the three simple calendars and not the composite `EUTA+GBLO`.
    val insideLinked: HolidayCalendarId = HolidayCalendarId.of("EUTA+GBLO~USNY")
    insideLinked.name shouldBe "EUTA+GBLO~USNY"

    val resolvedInsideLinked = insideLinked.resolve(REF_DATA)
    resolvedInsideLinked should beSuccess
    resolvedInsideLinked.map(calendar => calendar.name) should haveValue("EUTA+GBLO~USNY")
    resolvedInsideLinked.map(calendar => calendar.id) should haveValue(insideLinked)

    resolvedInsideLinked.map(calendar => calendar.isHoliday(NEW_YEAR_2019)) should haveValue(true)
    resolvedInsideLinked.map(calendar => calendar.isBusinessDay(US_HOLIDAY_2019)) should haveValue(true)
  }

  //-------------------------------------------------------------------------
  test("test_defaultByCurrency") {
    HolidayCalendarId.defaultByCurrency(Currency.GBP) shouldBe Some(HolidayCalendarIds.GBLO)
    HolidayCalendarId.defaultByCurrency(Currency.CZK) shouldBe Some(HolidayCalendarIds.CZPR)
    HolidayCalendarId.defaultByCurrency(Currency.HKD) shouldBe Some(HolidayCalendarId.of("HKHK"))

    forAll(dataDefaultByCurrency) { (currency: Currency, calendarId: HolidayCalendarId) =>
      withClue(s"$currency: ") {
        HolidayCalendarId.defaultByCurrency(currency) shouldBe Some(calendarId)
        HolidayCalendarId.defaultByCurrency(currency).map(id => id.name) shouldBe Some(calendarId.name)
      }
    }
    dataDefaultByCurrency should have size 31

    // The lookup is total: no conventional calendar is an ordinary `None`, not a failure.
    HolidayCalendarId.defaultByCurrency(Currency.XAG) shouldBe None
    HolidayCalendarId.defaultByCurrency(Currency.XAU) shouldBe None
    noException should be thrownBy HolidayCalendarId.defaultByCurrency(Currency.XAG)

    // The lookup answering and the identifier resolving are two different facts: for the rows
    // naming calendars this library does not ship, the first holds and the second does not.
    forAll(dataDefaultByCurrency) { (currency: Currency, calendarId: HolidayCalendarId) =>
      withClue(s"$currency -> ${calendarId.name}: ") {
        if (notShippedCalendarNames.contains(calendarId.name)) {
          calendarId.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        } else {
          calendarId.resolve(REF_DATA) should beSuccess
          calendarId.resolve(REF_DATA).map(calendar => calendar.id) should haveValue(calendarId)
        }
      }
    }
    notShippedCalendarNames should have size 13
  }

  test("test_findDefaultByCurrency") {
    // This test and `test_defaultByCurrency` above drive the same merged lookup.
    HolidayCalendarId.defaultByCurrency(Currency.GBP) shouldBe Some(HolidayCalendarIds.GBLO)
    HolidayCalendarId.defaultByCurrency(Currency.CZK) shouldBe Some(HolidayCalendarIds.CZPR)
    HolidayCalendarId.defaultByCurrency(Currency.HKD) shouldBe Some(HolidayCalendarId.of("HKHK"))
    HolidayCalendarId.defaultByCurrency(Currency.XAG) shouldBe None

    Currency.values.toList.foreach { currency =>
      withClue(s"$currency: ") {
        noException should be thrownBy HolidayCalendarId.defaultByCurrency(currency)
        HolidayCalendarId.defaultByCurrency(currency) shouldBe
          HolidayCalendarId.defaultByCurrency(currency)
      }
    }
  }

  test("test_defaultByCurrencyPair") {
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.GBP)) shouldBe
      HolidayCalendarIds.USNY.combinedWith(HolidayCalendarIds.GBLO)
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.GBP, Currency.CZK)) shouldBe
      HolidayCalendarId.of("CZPR+GBLO")
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.XAG)) shouldBe
      HolidayCalendarIds.USNY

    // The name is normalised, so the answer does not depend on which currency is the base.
    HolidayCalendarId
      .defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.GBP))
      .name shouldBe "GBLO+USNY"
    HolidayCalendarId
      .defaultByCurrencyPair(CurrencyPair.of(Currency.GBP, Currency.USD))
      .name shouldBe "GBLO+USNY"
    HolidayCalendarId
      .defaultByCurrencyPair(CurrencyPair.of(Currency.GBP, Currency.CZK))
      .name shouldBe "CZPR+GBLO"

    // A currency with no conventional calendar contributes nothing, and where neither has one the
    // answer is the no-holidays identifier, so this lookup always answers with an identifier.
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.XAG)).name shouldBe
      "USNY"
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.XAG, Currency.XAU)) shouldBe
      HolidayCalendarIds.NO_HOLIDAYS

    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.USD)) shouldBe
      HolidayCalendarIds.USNY
  }

  //-------------------------------------------------------------------------
  test("test_isCompositeCalendar") {
    HolidayCalendarId.isCompositeCalendar(HolidayCalendarId.of("GB+EU")) shouldBe true
    HolidayCalendarId.isCompositeCalendar(HolidayCalendarId.of("GB~EU")) shouldBe true
    HolidayCalendarId.isCompositeCalendar(HolidayCalendarId.of("GB")) shouldBe false

    // `HolidayCalendarId.isCompositeCalendar` and `id.isComposite` agree for every identifier.
    List("GB+EU", "GB~EU", "GB", "EU+Fri/Sat~GB", "GB+NoHolidays", "GB~NoHolidays", "NoHolidays")
      .foreach { name =>
        val id = HolidayCalendarId.of(name)
        withClue(s"$name -> ${id.name}: ") {
          HolidayCalendarId.isCompositeCalendar(id) shouldBe id.isComposite
        }
      }

    HolidayCalendarId.of("GB+NoHolidays").isComposite shouldBe false
    HolidayCalendarId.of("GB~NoHolidays").isComposite shouldBe false
    HolidayCalendarId.of("GB+GB").isComposite shouldBe false
    HolidayCalendarId.of("EU+Fri/Sat~GB").isComposite shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_resolve_single") {
    val gb: HolidayCalendarId = HolidayCalendarId.of("GB")
    val eu: HolidayCalendarId = HolidayCalendarId.of("EU")
    val gbCal: HolidayCalendar = HolidayCalendars.SAT_SUN
    val refData: ReferenceData = ImmutableReferenceData.of(gb, gbCal)

    gb.resolve(refData) should haveValue(gbCal)
    refData.getValue(gb) should haveValue(gbCal)

    // An absent identifier is a failure value naming it, reported by the store itself.
    eu.resolve(refData) should beFailureWith(FailureReason.MISSING_DATA)
    eu.resolve(refData) should haveFailureMessageMatching("Reference data not found for identifier 'EU'")
    eu.resolve(refData).swap.map(failure => failure.attributes.get("id")) shouldBe Right(Some("EU"))
    noException should be thrownBy eu.resolve(refData)

    // The reader form resolves the same way, so reference data supplied later answers alike.
    gb.toReader.run(refData) should haveValue(gbCal)
    eu.toReader.run(refData) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_resolve_combined_direct") {
    // Reference data that holds the whole composite identifier has it used as it stands, which is
    // what lets a host supply a calendar pre-combined from a vendor feed.
    val gb: HolidayCalendarId = HolidayCalendarId.of("GB")
    val gbCal: HolidayCalendar = HolidayCalendars.SAT_SUN
    val eu: HolidayCalendarId = HolidayCalendarId.of("EU")
    val euCal: HolidayCalendar = HolidayCalendars.FRI_SAT
    val combined: HolidayCalendarId = gb.combinedWith(eu)
    val combinedCal: HolidayCalendar = euCal.combinedWith(gbCal)
    val refData: ReferenceData = ImmutableReferenceData.of(combined, combinedCal)

    combined.resolve(refData) should haveValue(combinedCal)
    refData.getValue(combined) should haveValue(combinedCal)
    combined.name shouldBe "EU+GB"

    // The whole name is looked up '''before''' the parts, and the marker calendar below is what
    // makes which of the two answered observable: it holds a holiday neither component has and
    // neither of their holidays, so the dates alone say whether the whole name won.
    val gblo: HolidayCalendarId = HolidayCalendarId.of("GBLO")
    val usny: HolidayCalendarId = HolidayCalendarId.of("USNY")
    val whole: HolidayCalendarId = gblo.combinedWith(usny)
    whole.name shouldBe "GBLO+USNY"

    val gbloCal: HolidayCalendar =
      ImmutableHolidayCalendar.of(gblo, List(LocalDate.of(2015, 7, 3)), SATURDAY, SUNDAY)
    val usnyCal: HolidayCalendar =
      ImmutableHolidayCalendar.of(usny, List(LocalDate.of(2015, 7, 6)), SATURDAY, SUNDAY)
    val markerCal: HolidayCalendar =
      ImmutableHolidayCalendar.of(whole, List(LocalDate.of(2015, 7, 7)), SATURDAY, SUNDAY)

    val withWhole: ReferenceData =
      store(
        ReferenceData.Entry(gblo, gbloCal),
        ReferenceData.Entry(usny, usnyCal),
        ReferenceData.Entry(whole, markerCal))
    val resolvedWhole = whole.resolve(withWhole)
    resolvedWhole should haveValue(markerCal)
    resolvedWhole.map(calendar => calendar.isHoliday(LocalDate.of(2015, 7, 7))) should haveValue(true)
    resolvedWhole.map(calendar => calendar.isHoliday(LocalDate.of(2015, 7, 3))) should haveValue(false)
    resolvedWhole.map(calendar => calendar.isHoliday(LocalDate.of(2015, 7, 6))) should haveValue(false)
    withWhole.getValue(whole) shouldBe resolvedWhole
  }

  test("test_resolve_combined_indirect") {
    // Only the parts held: each resolves and the results combine in the normalised name's order.
    val gb: HolidayCalendarId = HolidayCalendarId.of("GB")
    val gbCal: HolidayCalendar = HolidayCalendars.SAT_SUN
    val eu: HolidayCalendarId = HolidayCalendarId.of("EU")
    val euCal: HolidayCalendar = HolidayCalendars.FRI_SAT
    val combined: HolidayCalendarId = gb.combinedWith(eu)
    val combinedCal: HolidayCalendar = euCal.combinedWith(gbCal)
    val refData: ReferenceData =
      store(ReferenceData.Entry(gb, gbCal), ReferenceData.Entry(eu, euCal))

    combined.resolve(refData) should haveValue(combinedCal)
    combined.toReader.run(refData) should haveValue(combinedCal)

    // The store is a plain map and the composite assembly lives on `HolidayCalendarId.resolve`
    // alone, so the store reports the composite as absent even with both parts held - the same
    // fact `HolidaySafeReferenceData` relies on when it leaves a composite unresolved so that its
    // parts can be defaulted one by one.
    refData.getValue(combined) should beFailureWith(FailureReason.MISSING_DATA)
    refData.findValue(combined) shouldBe None
    refData.containsValue(combined) shouldBe false

    val resolved = combined.resolve(refData)
    resolved.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 11))) should haveValue(true)
    resolved.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 12))) should haveValue(true)
    resolved.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 13))) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(LocalDate.of(2014, 7, 14))) should haveValue(true)

    // With the whole name absent, the same identifier resolves from its parts instead.
    val gblo: HolidayCalendarId = HolidayCalendarId.of("GBLO")
    val usny: HolidayCalendarId = HolidayCalendarId.of("USNY")
    val whole: HolidayCalendarId = gblo.combinedWith(usny)
    val gbloCal: HolidayCalendar =
      ImmutableHolidayCalendar.of(gblo, List(LocalDate.of(2015, 7, 3)), SATURDAY, SUNDAY)
    val usnyCal: HolidayCalendar =
      ImmutableHolidayCalendar.of(usny, List(LocalDate.of(2015, 7, 6)), SATURDAY, SUNDAY)

    val withoutWhole: ReferenceData =
      store(ReferenceData.Entry(gblo, gbloCal), ReferenceData.Entry(usny, usnyCal))
    val resolvedParts = whole.resolve(withoutWhole)
    resolvedParts should beSuccess
    resolvedParts.map(calendar => calendar.name) should haveValue("GBLO+USNY")
    resolvedParts.map(calendar => calendar.isHoliday(LocalDate.of(2015, 7, 3))) should haveValue(true)
    resolvedParts.map(calendar => calendar.isHoliday(LocalDate.of(2015, 7, 6))) should haveValue(true)
    resolvedParts.map(calendar => calendar.isHoliday(LocalDate.of(2015, 7, 7))) should haveValue(false)

    // A composite identifier is all-or-nothing - a calendar assembled from some of its parts
    // would silently declare business days that are holidays - and the failure names both the
    // missing part and the identifier being built as attributes, not only in its text.
    val missingComponent: HolidayCalendarId = gblo.combinedWith(HolidayCalendarIds.USNY)
    val partial: ReferenceData = ImmutableReferenceData.of(gblo, gbloCal)

    val failed = missingComponent.resolve(partial)
    failed should beFailureWith(FailureReason.MISSING_DATA)
    failed should haveFailureMessageMatching(
      "Reference data not found for 'USNY' of type 'HolidayCalendarId' when finding 'GBLO\\+USNY'")
    failed.swap.map(failure => failure.attributes.toMap) shouldBe
      Right(Map("id" -> "USNY", "compositeId" -> "GBLO+USNY"))
    noException should be thrownBy missingComponent.resolve(partial)

    gblo.linkedWith(HolidayCalendarIds.USNY).resolve(partial) should haveFailureMessageMatching(
      "Reference data not found for 'USNY' of type 'HolidayCalendarId' when finding 'GBLO~USNY'")

    // How many parts a name may join. A composite identifier names its parts in its own text,
    // and that text comes from outside the program - a document, a convention, a request - so a
    // name joining two thousand calendars would have this method resolve two thousand of them
    // and combine them into a calendar nested two thousand deep, which every later question
    // about a date would recurse through until the stack ran out. The identifier is data, so
    // the width is refused in the error channel rather than raised, and refused before any part
    // is looked up; the calendar family states the same limit as a precondition of nesting that
    // deep directly, which is the route this one would otherwise reach.
    val partNames = (count: Int) => (1 to count).map(index => f"CAL$index%04d").toList
    val partEntries = (names: List[String]) =>
      names.map { name =>
        val partId: HolidayCalendarId = HolidayCalendarId.of(name)
        ReferenceData.Entry(partId, ImmutableHolidayCalendar.of(partId, Nil, SATURDAY, SUNDAY))
      }

    val atTheLimit: List[String] = partNames(HolidayCalendar.MaxCompositeDepth)
    val beyondTheLimit: List[String] = partNames(HolidayCalendar.MaxCompositeDepth + 1)
    val everyPart: ReferenceData = store(partEntries(beyondTheLimit): _*)

    // The widest name that is read: every part resolved, combined in the order the normalised
    // name gives, and answering as the combination of them all.
    val widest: HolidayCalendarId = HolidayCalendarId.of(atTheLimit.mkString("+"))
    val resolvedWidest = widest.resolve(everyPart)
    resolvedWidest should beSuccess
    resolvedWidest.map(calendar => calendar.name) should haveValue(widest.name)
    resolvedWidest.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 12))) should haveValue(true)
    resolvedWidest.map(calendar => calendar.isBusinessDay(LocalDate.of(2014, 7, 14))) should haveValue(true)

    // One part more is refused, and the failure says how many parts were named and how many may
    // be - as attributes as well as in its text - so a caller can act on the number without
    // reading the message.
    val tooWide: HolidayCalendarId = HolidayCalendarId.of(beyondTheLimit.mkString("+"))
    val refused = tooWide.resolve(everyPart)
    refused should beFailureWith(FailureReason.INVALID)
    refused should haveFailureMessageMatching(
      s".*joins ${HolidayCalendar.MaxCompositeDepth + 1} calendars.*more than ${HolidayCalendar.MaxCompositeDepth}")
    refused.swap.map(failure => failure.attributes.toMap.get("components")) shouldBe
      Right(Some((HolidayCalendar.MaxCompositeDepth + 1).toString))
    refused.swap.map(failure => failure.attributes.toMap.get("id")) shouldBe Right(Some(tooWide.name))
    noException should be thrownBy tooWide.resolve(everyPart)

    // The same name through the reader, which is this resolution expressed as a value awaiting
    // reference data, and through a linked name of the same width: the limit is about how many
    // calendars one name asks to be read through, not about which way they combine.
    tooWide.toReader.run(everyPart) should beFailureWith(FailureReason.INVALID)
    HolidayCalendarId.of(beyondTheLimit.mkString("~")).resolve(everyPart) should
      beFailureWith(FailureReason.INVALID)

    // Reference data holding the whole wide name is answered from that entry, as any other name
    // is: what the limit refuses is reading through that many calendars, and an application that
    // has already combined them has nothing left to read through.
    val preCombined: ReferenceData =
      store(ReferenceData.Entry(tooWide, ImmutableHolidayCalendar.of(tooWide, Nil, SATURDAY, SUNDAY)))
    tooWide.resolve(preCombined).map(calendar => calendar.id) should haveValue(tooWide)

    // How deep the calendars turn out to be, which the name cannot show. A name of two parts
    // describes a calendar one deeper than its deeper part, so reference data mapping a part to
    // a calendar that is already as deep as a calendar may be carries an ordinary two-part name
    // past the limit. The width checked above cannot see that - the name joins two calendars -
    // so the depth of what was resolved is checked before each part is read together with the
    // ones before it, and reported in this method's failure channel. Resolution answers with a
    // failure whatever the reference data holds; it never raises out of the result it returns.
    val deepId: HolidayCalendarId = HolidayCalendarId.of("ADEEP")
    val shallowId: HolidayCalendarId = HolidayCalendarId.of("BLEAF")
    val leaf: HolidayCalendar = ImmutableHolidayCalendar.of(shallowId, Nil, SATURDAY, SUNDAY)
    val nested = (depth: Int) =>
      (1 to depth).foldLeft(leaf)((calendar, _) => calendar.combinedWith(HolidayCalendars.FRI_SAT))

    val asDeepAsAllowed: HolidayCalendar = nested(HolidayCalendar.MaxCompositeDepth)
    asDeepAsAllowed.compositeDepth shouldBe HolidayCalendar.MaxCompositeDepth
    val pair: HolidayCalendarId = deepId.combinedWith(shallowId)
    pair.name shouldBe "ADEEP+BLEAF"

    val deepData: ReferenceData =
      store(ReferenceData.Entry(deepId, asDeepAsAllowed), ReferenceData.Entry(shallowId, leaf))
    val tooDeep = pair.resolve(deepData)
    tooDeep should beFailureWith(FailureReason.INVALID)
    tooDeep should haveFailureMessageMatching(
      s".*read through ${HolidayCalendar.MaxCompositeDepth + 1} calendars together.*" +
        s"more than ${HolidayCalendar.MaxCompositeDepth}")
    tooDeep.swap.map(failure => failure.attributes.toMap.get("depth")) shouldBe
      Right(Some((HolidayCalendar.MaxCompositeDepth + 1).toString))
    tooDeep.swap.map(failure => failure.attributes.toMap.get("component")) shouldBe
      Right(Some(shallowId.name))
    tooDeep.swap.map(failure => failure.attributes.toMap.get("id")) shouldBe Right(Some(pair.name))
    noException should be thrownBy pair.resolve(deepData)

    // The same through the reader, and through the linked form of the name, neither of which
    // takes a different route into the combination.
    pair.toReader.run(deepData) should beFailureWith(FailureReason.INVALID)
    noException should be thrownBy pair.toReader.run(deepData)
    val linkedPair: HolidayCalendarId = deepId.linkedWith(shallowId)
    linkedPair.resolve(deepData) should beFailureWith(FailureReason.INVALID)
    noException should be thrownBy linkedPair.resolve(deepData)

    // One calendar shallower and the same name resolves, reaching exactly the depth a calendar
    // may have, and answers about a date - so what is refused above is the calendar that cannot
    // exist rather than the shape of the request.
    val deepEnoughData: ReferenceData = store(
      ReferenceData.Entry(deepId, nested(HolidayCalendar.MaxCompositeDepth - 1)),
      ReferenceData.Entry(shallowId, leaf))
    val resolvedDeep = pair.resolve(deepEnoughData)
    resolvedDeep should beSuccess
    resolvedDeep.map(calendar => calendar.compositeDepth) should
      haveValue(HolidayCalendar.MaxCompositeDepth)
    resolvedDeep.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 12))) should haveValue(true)
    resolvedDeep.map(calendar => calendar.isBusinessDay(LocalDate.of(2014, 7, 14))) should haveValue(true)
  }

  test("testImmutableReferenceDataWithMergedHolidays") {
    // A single pre-merged calendar, filed under its own identifier, is enough to adjust a date.
    val hc: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    hc.id.name shouldBe "Fri/Sat+Sat/Sun"

    val referenceData: ImmutableReferenceData = ImmutableReferenceData.of(hc.id, hc)
    val date = BusinessDayAdjustment
      .of(BusinessDayConventions.PRECEDING, hc.id)
      .adjust(LocalDate.of(2016, 8, 20), referenceData)

    // Friday, Saturday and Sunday are all holidays of the merged calendar.
    date should haveValue(LocalDate.of(2016, 8, 18))
  }

  //-------------------------------------------------------------------------
  test("test_combinedWith") {
    val gb: HolidayCalendarId = HolidayCalendarId.of("GB")
    val eu: HolidayCalendarId = HolidayCalendarId.of("EU")
    val us: HolidayCalendarId = HolidayCalendarId.of("US")

    val combined1: HolidayCalendarId = eu.combinedWith(us).combinedWith(gb)
    val combined2: HolidayCalendarId = us.combinedWith(eu).combinedWith(gb.combinedWith(us))

    combined1.name shouldBe "EU+GB+US"
    combined1.toString shouldBe "EU+GB+US"
    combined2.name shouldBe "EU+GB+US"
    combined2.toString shouldBe "EU+GB+US"
    combined1.equals(combined2) shouldBe true
    combined1 shouldBe combined2
    combined1.hashCode shouldBe combined2.hashCode

    // The parts are deduplicated and sorted, so the result does not depend on the order they were
    // combined in - `us` appears twice in `combined2` and once in the name.
    combined1.isComposite shouldBe true
    HolidayCalendarId.of("US+GB+EU") shouldBe combined1
    HolidayCalendarId.of("EU+GB+US+GB") shouldBe combined1
  }

  test("test_combinedWithSelf") {
    val gb: HolidayCalendarId = HolidayCalendarId.of("GB")
    gb.combinedWith(gb) shouldBe gb
    gb.combinedWith(HolidayCalendarIds.NO_HOLIDAYS) shouldBe gb
    HolidayCalendarIds.NO_HOLIDAYS.combinedWith(gb) shouldBe gb
    HolidayCalendarIds.NO_HOLIDAYS.combinedWith(HolidayCalendarIds.NO_HOLIDAYS) shouldBe
      HolidayCalendarIds.NO_HOLIDAYS

    gb.combinedWith(gb).name shouldBe "GB"
    gb.combinedWith(gb).isComposite shouldBe false

    // Linking an identifier with itself changes nothing, as combining does, but the no-holidays
    // identifier behaves oppositely under the two: it is the '''identity''' of combination, a
    // calendar with no holidays removing nothing, and the '''absorbing element''' of linking, a
    // calendar in which every day is a business day leaving no day for the others to close.
    gb.linkedWith(gb) shouldBe gb
    gb.linkedWith(HolidayCalendarIds.NO_HOLIDAYS) shouldBe HolidayCalendarIds.NO_HOLIDAYS
    HolidayCalendarIds.NO_HOLIDAYS.linkedWith(gb) shouldBe HolidayCalendarIds.NO_HOLIDAYS
    HolidayCalendarIds.NO_HOLIDAYS.linkedWith(HolidayCalendarIds.NO_HOLIDAYS) shouldBe
      HolidayCalendarIds.NO_HOLIDAYS
  }

  //-------------------------------------------------------------------------
  test("test_equalsHashCode") {
    val a: HolidayCalendarId = HolidayCalendarId.of("GB")
    val a2: HolidayCalendarId = HolidayCalendarId.of("GB")
    val b: HolidayCalendarId = HolidayCalendarId.of("EU")

    a.hashCode shouldBe a2.hashCode
    a.equals(a) shouldBe true
    a.equals(a2) shouldBe true
    a.equals(b) shouldBe false
    // `equals` takes any reference, so the absent one and another type are answered, not rejected
    a.equals(null) shouldBe false
    a.equals(ANOTHER_TYPE) shouldBe false

    // The hash is the name's, so a map keyed by an identifier lays out alike between runs.
    a.hashCode shouldBe "GB".hashCode
    Hash[HolidayCalendarId].hash(a) shouldBe "GB".hashCode

    a shouldBe a2
    Hash[HolidayCalendarId].eqv(a, a2) shouldBe true
    Hash[HolidayCalendarId].eqv(a, b) shouldBe false

    // The companion publishes one equality-bearing instance, an `Order` that is also a `Hash`, so
    // the ordering above and the equality come from the same value. It orders the names.
    (Order[HolidayCalendarId].compare(b, a) < 0) shouldBe true
    (Order[HolidayCalendarId].compare(a, b) > 0) shouldBe true
    Order[HolidayCalendarId].compare(a, a2) shouldBe 0

    // An identifier works as a map key, which the reference data store requires of it.
    Map(a -> "first").get(a2) shouldBe Some("first")
    Set(a, a2, b) should have size 2
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    forAll(dataConstants) { (constant: HolidayCalendarId, name: String) =>
      withClue(s"$name: ") {
        constant.name shouldBe name
        // Both text forms of a constant are its name, character for character: the rendering is
        // bounded and single-line for an arbitrary name - `test_of_single` states that - and a
        // name of this shape passes through it untouched, which is what keeps the text this
        // library writes the text the library being ported wrote. Asserted on both forms, and
        // on their agreement, for all twenty-nine constants rather than for a sample of them.
        constant.toString shouldBe name
        Show[HolidayCalendarId].show(constant) shouldBe name
        Show[HolidayCalendarId].show(constant) shouldBe constant.toString
        // each constant is the identifier its own name builds, equality being by name
        constant shouldBe HolidayCalendarId.of(name)
        constant.isComposite shouldBe false
      }
    }

    // The same statement for a composite name, which is the one kind of name this library builds
    // rather than transcribes: a combination and a link render exactly as the normalised name
    // they carry, whichever way round the parts were written and whether the composite was
    // written out or assembled.
    forAll(
      Table(
        ("composite", "name"),
        (HolidayCalendarId.of("GBLO+USNY"), "GBLO+USNY"),
        (HolidayCalendarId.of("USNY+GBLO"), "GBLO+USNY"),
        (HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY), "GBLO+USNY"),
        (HolidayCalendarId.of("GBLO~USNY"), "GBLO~USNY"),
        (HolidayCalendarId.of("GB~EU+Fri/Sat"), "EU+Fri/Sat~GB"))) {
      (composite: HolidayCalendarId, name: String) =>
        withClue(s"$name: ") {
          composite.name shouldBe name
          composite.toString shouldBe name
          Show[HolidayCalendarId].show(composite) shouldBe name
        }
    }

    val constants: List[HolidayCalendarId] = dataConstants.map { case (constant, _) => constant }.toList
    constants should have size 29
    constants.distinct should have size 29
    constants.map(id => id.name).distinct should have size 29

    // One built-in calendar has no constant here: `NZBD`, the artificial New Zealand national
    // bank calendar, which exists because the `NZD-BBR` index is published on both the Wellington
    // (`NZWE`) and the Auckland (`NZAU`) anniversary days, so neither city's calendar describes
    // it. `GlobalHolidayCalendars` generates it and `StandardHolidayCalendars` publishes it all
    // the same, so it resolves from the standard reference data. Its absence is stated over the
    // constants `dataConstants` enumerates, rather than by reflecting over the object's members.
    constants.map(id => id.name) should not contain "NZBD"
    HolidayCalendarId.of("NZBD").resolve(REF_DATA) should beSuccess
    assertDoesNotCompile("HolidayCalendarIds.NZBD")

    // The weekend and no-holiday identifiers are those the calendars themselves carry.
    HolidayCalendarIds.NO_HOLIDAYS shouldBe HolidayCalendars.NO_HOLIDAYS.id
    HolidayCalendarIds.SAT_SUN shouldBe HolidayCalendars.SAT_SUN.id
    HolidayCalendarIds.FRI_SAT shouldBe HolidayCalendars.FRI_SAT.id
    HolidayCalendarIds.THU_FRI shouldBe HolidayCalendars.THU_FRI.id

    // Every constant resolves against the standard reference data, so the holder is usable
    // without an application supplying calendars.
    constants.filterNot(id => id == HolidayCalendarIds.NO_HOLIDAYS).foreach { id =>
      withClue(s"${id.name}: ") {
        id.resolve(REF_DATA) should beSuccess
        id.resolve(REF_DATA).map(calendar => calendar.id) should haveValue(id)
      }
    }
    HolidayCalendarIds.NO_HOLIDAYS.resolve(REF_DATA) should haveValue(HolidayCalendars.NO_HOLIDAYS)

    // The equality, the hashing and the ordering agree over simple and composite identifiers.
    val sample: List[HolidayCalendarId] =
      List(
        HolidayCalendarIds.GBLO,
        HolidayCalendarIds.USNY,
        HolidayCalendarId.of("GBLO+USNY"),
        HolidayCalendarId.of("GBLO~USNY"),
        HolidayCalendarIds.NO_HOLIDAYS)
    for (left <- sample; right <- sample) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Hash[HolidayCalendarId].eqv(left, right) shouldBe sameValue
        (Order[HolidayCalendarId].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[HolidayCalendarId].hash(left) shouldBe Hash[HolidayCalendarId].hash(right)
        } else {
          Order[HolidayCalendarId].compare(left, right) should not be 0
        }
      }
    }

    sample.sorted(Order[HolidayCalendarId].toOrdering).map(id => id.name) shouldBe
      sample.map(id => id.name).sorted
    (Order[HolidayCalendarId].compare(HolidayCalendarIds.EUTA, HolidayCalendarIds.GBLO) < 0) shouldBe true
  }

  test("test_serialization") {
    // The circe codec writes an identifier as the bare string of its name, never as an object.
    HolidayCalendarId.of("US").asJson shouldBe Json.fromString("US")
    decode[HolidayCalendarId]("\"US\"") shouldBe Right(HolidayCalendarId.of("US"))
    decode[HolidayCalendarId](HolidayCalendarId.of("US").asJson.noSpaces) shouldBe
      Right(HolidayCalendarId.of("US"))

    // The name is the whole content of a composite identifier, so a reader compares two documents
    // without repeating the normalisation.
    HolidayCalendarId.of("GBLO+USNY").asJson shouldBe Json.fromString("GBLO+USNY")
    decode[HolidayCalendarId]("\"GBLO+USNY\"") shouldBe Right(HolidayCalendarId.of("GBLO+USNY"))
    HolidayCalendarId.of("GBLO~USNY").asJson shouldBe Json.fromString("GBLO~USNY")
    decode[HolidayCalendarId]("\"GBLO~USNY\"") shouldBe Right(HolidayCalendarId.of("GBLO~USNY"))

    // The shape and the exact text, which a generated `decode(encode(a)) == a` round trip does
    // not state: it is asserted here so that a change to an object form fails in this test.
    HolidayCalendarIds.GBLO.asJson shouldBe Json.fromString("GBLO")
    HolidayCalendarId.of("GBLO+USNY").asJson shouldBe Json.fromString("GBLO+USNY")
    HolidayCalendarIds.GBLO.asJson.isString shouldBe true

    // Reading goes through the factory, so a name is normalised on the way in.
    decode[HolidayCalendarId]("\"GBLO\"") shouldBe Right(HolidayCalendarIds.GBLO)
    decode[HolidayCalendarId]("\"USNY+GBLO\"") shouldBe Right(HolidayCalendarId.of("GBLO+USNY"))
    HolidayCalendarId.of("USNY+GBLO").asJson.noSpaces shouldBe
      HolidayCalendarId.of("GBLO+USNY").asJson.noSpaces

    // Any name is accepted: a missing calendar is reported when the identifier is resolved.
    decode[HolidayCalendarId]("\"XXXX\"") shouldBe Right(HolidayCalendarId.of("XXXX"))

    // A document of the wrong JSON type is rejected: the codec reads a string and nothing else.
    Json.fromInt(1).as[HolidayCalendarId].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("GBLO")).as[HolidayCalendarId].isLeft shouldBe true
    Json.arr(Json.fromString("GBLO")).as[HolidayCalendarId].isLeft shouldBe true

    // An identifier also keys a JSON object, and the key normalises as the value does.
    KeyEncoder[HolidayCalendarId].apply(HolidayCalendarId.of("USNY+GBLO")) shouldBe "GBLO+USNY"
    KeyDecoder[HolidayCalendarId].apply("USNY+GBLO") shouldBe Some(HolidayCalendarId.of("GBLO+USNY"))
    KeyDecoder[HolidayCalendarId].apply("GBLO") shouldBe Some(HolidayCalendarIds.GBLO)
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a set of reference data holding exactly the entries given.
   *
   * `ImmutableReferenceData.of` is used rather than `ReferenceData.of` because the latter layers
   * the four built-in weekend calendars underneath the caller's entries, and the assertions above
   * about a calendar that is '''absent''' depend on the store holding nothing it was not given.
   * That factory reports the one way it can fail - two entries filed under the same identifier -
   * so a fixture that cannot be built is reported as a failed test naming the cause.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the store holding exactly those entries
   */
  private def store(entries: ReferenceData.Entry[_]*): ImmutableReferenceData =
    ImmutableReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }

  /**
   * Answers with the failure an identifier resolves to against reference data holding nothing.
   *
   * This is the shortest route from an identifier to a failure that quotes it, and it is an
   * ordinary one: resolution against a store that does not hold the calendar is what an
   * application reaches whenever it has not supplied the entry, so the text driven into the
   * failure is the identifier's own rather than a string composed by this spec.
   *
   * @param calendarId  the identifier to resolve against reference data holding nothing
   * @return the failure reporting that the calendar is absent
   */
  private def unresolvable(calendarId: HolidayCalendarId): Failure =
    calendarId.resolve(ReferenceData.empty) match {
      case Left(failure) => failure
      case Right(calendar) =>
        fail(s"Reference data holding nothing resolved a calendar: $calendar")
    }
}
