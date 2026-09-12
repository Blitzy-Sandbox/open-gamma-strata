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
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[HolidayCalendarId]], ported from the Java `HolidayCalendarIdTest`.
 *
 * All twenty-one methods of the Java class are kept under the names they had, and this spec
 * publishes exactly those twenty-one tests and nothing else, so the method-level traceability
 * recorded in `manifest/java-test-mapping.csv` stays one-to-one and every name a row of that
 * manifest carries is a test that exists: `test_of_single`, `test_of_combined`,
 * `test_of_combined_NoHolidays`, `test_of_combined_resolve`, `test_of_linked`,
 * `test_of_linked_NoHolidays`, `test_of_linked_combined`, `test_of_linked_resolve`,
 * `test_defaultByCurrency`, `test_findDefaultByCurrency`, `test_defaultByCurrencyPair`,
 * `test_isCompositeCalendar`, `test_resolve_single`, `test_resolve_combined_direct`,
 * `test_resolve_combined_indirect`, `testImmutableReferenceDataWithMergedHolidays`,
 * `test_combinedWith`, `test_combinedWithSelf`, `test_equalsHashCode`, `coverage` and
 * `test_serialization`.
 *
 * The Java `test_serialization` asserted a Java-serialization round trip, which is not ported;
 * what replaces it here is the '''concrete document''' an identifier serializes to - a bare
 * JSON string of its name, and never an object - because that is the statement the
 * property-based sweep in `json.JsonRoundTripSpec` cannot make. The two are complementary:
 * that sweep asserts `decode(encode(a)) == a` over generated identifiers, and this test asserts
 * the shape and the exact text.
 *
 * Statements about the port that have no Java method of their own are folded into the test
 * whose subject they belong to rather than added as tests of their own, so that the count of
 * tests here stays the count of Java methods: normalisation edge cases sit in
 * `test_of_combined`, the algebra of `linkedWith` in `test_combinedWithSelf` and
 * `test_of_linked_combined`, the precedence of a whole composite name in
 * `test_resolve_combined_direct`, an unresolvable component in `test_resolve_combined_indirect`,
 * the resolution of the thirty-one conventional calendars in `test_defaultByCurrency`, and the
 * one behavioural divergence in `test_of_linked_resolve`.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - '''Identifiers are not interned.''' The Java factory kept a process-wide cache, so
 *     `of("GB+EU")` and `of("EU+GB")` were the ''same object'' and the Java methods asserted
 *     `isSameAs`. Equality here is by name and nothing needs interning, so every such assertion
 *     becomes an assertion of equality and of equal names. Nothing in this spec asserts object
 *     identity, for an identifier or for anything derived from one: the structure an identifier
 *     carries for resolution is an implementation detail that may be computed once or many
 *     times, and only the observable answer is a property of the type.
 *   - `getReferenceDataType` is not ported. It existed so that the reference data store could
 *     check a value's class at run time; here the identifier's type parameter does that at
 *     compile time, which `test_of_single` states with a pair of compilation assertions.
 *   - `resolve` returns `Either[Failure, HolidayCalendar]` where the Java method returned a
 *     calendar or threw `ReferenceDataNotFoundException`, which is not ported. A missing simple
 *     identifier and a missing component of a composite are both `Failure.MissingData`, and the
 *     second carries the message and the two attributes the Java exception carried in its text.
 *   - '''Composite assembly lives on `resolve`, not on the store.''' The Java store delegated
 *     every lookup to a low-level query primitive which this identifier overrode to resolve a
 *     composite's components, so `refData.getValue(composite)` assembled the calendar there. That
 *     primitive is not ported, so the store is a plain map: `test_resolve_combined_indirect`
 *     asserts the assembly through `resolve` and asserts that the store itself reports the
 *     composite as absent - which is the same fact `HolidaySafeReferenceData` relies on when it
 *     leaves a composite unresolved so that its parts are defaulted one by one.
 *   - `defaultByCurrency` threw for a currency with no conventional calendar and a second Java
 *     method, `findDefaultByCurrency`, returned an optional value. The two are merged here into
 *     one total lookup returning `Option`, so both ported tests drive that one member; the
 *     currency Java threw for is asserted to be `None`.
 *   - `coverage` called `coverPrivateConstructor(HolidayCalendarIds.class)`, which reflectively
 *     invoked the private constructor of a static holder. A Scala `object` has none, so what the
 *     call stood for is asserted directly over all twenty-nine constants the holder publishes.
 *
 * ===One deliberate behavioural divergence===
 *
 * A '''composite component inside a linked identifier''' resolves recursively here and succeeds
 * where the Java implementation failed; the closing part of `test_of_linked_resolve` records the
 * Java outcome alongside the port's. The divergence is strictly more permissive - it can only
 * turn a Java failure into a success - and is divergence 20 of `SCALA_MIGRATION.md`, discussed
 * there in section (c)-20.
 *
 * @see [[HolidayCalendarsSpec]] for the calendars an identifier resolves to
 * @see [[HolidaySafeReferenceDataSpec]] for resolution against data that defaults what it lacks
 */
class HolidayCalendarIdSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The US Independence Day holiday of 2019, a business day in Prague and in London. */
  private val US_HOLIDAY_2019: LocalDate = LocalDate.of(2019, 7, 4)

  /** The Czech Liberation Day holiday of 2019, a business day in New York and in London. */
  private val CZ_HOLIDAY_2019: LocalDate = LocalDate.of(2019, 5, 8)

  /** New Year's Day 2019, a holiday in every centre this spec names. */
  private val NEW_YEAR_2019: LocalDate = LocalDate.of(2019, 1, 1)

  /**
   * A value of another type, which is the `ANOTHER_TYPE` constant of the Java test.
   *
   * Typed as `Any` so that `equals` can be handed it: the method takes `Any`, and giving it a
   * `String` directly would let the compiler decide the comparison can never hold.
   */
  private val ANOTHER_TYPE: Any = ""

  /**
   * The market-convention calendar of each currency, all thirty-one rows of the table the Java
   * implementation loaded from its holiday-calendar default data.
   *
   * The rows are in the order that source lists them. Thirteen of them name calendars this
   * library does not ship, which is why `test_defaultByCurrency` asserts the two halves of the
   * table separately: the lookup answering is not the same statement as the identifier
   * resolving.
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

  /**
   * The names of the thirteen conventional calendars this library does not ship.
   *
   * They are returned by the lookup exactly as the Java implementation returned them, and they
   * fail to resolve against `ReferenceData.standard` there as they do here.
   */
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

  /** The twenty-nine constants of [[HolidayCalendarIds]], each with the name it carries. */
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

    // The Java method also asserted `getReferenceDataType() == HolidayCalendar.class`, a
    // reflection token the store used to check a value's class while the program ran. It has no
    // port, because this port performs no reflection at all; what it stood for - that resolving
    // an identifier of this type yields a holiday calendar - is stated by the type system, and
    // the binding below is that statement. It is annotated `HolidayCalendar` deliberately: the
    // annotation is checked by the compiler, so the test would not build if resolution answered
    // with anything else.
    val refData: ReferenceData = ImmutableReferenceData.of(test, HolidayCalendars.SAT_SUN)
    val cal: HolidayCalendar = test.resolve(refData) match {
      case Right(calendar) => calendar
      case Left(failure) => fail(s"'GB' should resolve against data holding it: ${failure.message}")
    }
    cal shouldBe HolidayCalendars.SAT_SUN

    // The same type safety on the way in: an entry pairing this identifier with a calendar
    // compiles, and one pairing it with a value of any other type does not.
    assertCompiles("""ReferenceData.Entry(HolidayCalendarId.of("GB"), HolidayCalendars.SAT_SUN)""")
    assertDoesNotCompile("""ReferenceData.Entry(HolidayCalendarId.of("GB"), "GB")""")

    // A name is taken as it stands - the factory is total, because a calendar's name is not this
    // library's to judge - and reads back through `of`.
    HolidayCalendarId.of(test.name) shouldBe test
  }

  test("test_of_combined") {
    val test: HolidayCalendarId = HolidayCalendarId.of("GB+EU")
    test.name shouldBe "EU+GB"
    test.toString shouldBe "EU+GB"
    test.isComposite shouldBe true

    // The Java method asserted `isSameAs` here, which held because the Java factory interned its
    // identifiers. This port does not intern, so what is asserted is equality and the normalised
    // name - which is the property the interning existed to support.
    val test2: HolidayCalendarId = HolidayCalendarId.of("EU+GB")
    test shouldBe test2
    test.name shouldBe test2.name
    test.hashCode shouldBe test2.hashCode
    Hash[HolidayCalendarId].eqv(test, test2) shouldBe true

    // The edge cases of the normalisation this factory performs, each of them a value captured
    // from the library being ported, so that normalisation cannot drift. They belong to this
    // test because they are the same property it states - that a composite name and its
    // rearrangements are one value - taken to the boundaries of the name space.
    //
    // Names are sorted and deduplicated, and a name containing no separator is taken as it is.
    HolidayCalendarId.of("USNY+GBLO").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of("GBLO+USNY").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of("Anything At All").name shouldBe "Anything At All"

    // An empty part is kept rather than discarded - the Java splitter kept it - and sorts before
    // every name, which is why a trailing separator moves to the front.
    HolidayCalendarId.of("GBLO+").name shouldBe "+GBLO"
    HolidayCalendarId.of("").name shouldBe ""
    HolidayCalendarId.of("").isComposite shouldBe false

    // The equality that normalisation buys: differently written composites are one value, so one
    // reference-data entry answers for either spelling, while the two separators stay distinct.
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
    // Transcribed from the Java method, including the three dates it named. A combined identifier
    // observes the holidays of both centres, so each of the three is a non-business day.
    val holidayCalendarId: HolidayCalendarId = HolidayCalendarId.of("CZPR+USNY")
    val resolved = holidayCalendarId.resolve(REF_DATA)

    resolved should beSuccess
    resolved.map(calendar => calendar.name) should haveValue("CZPR+USNY")
    resolved.map(calendar => calendar.isBusinessDay(US_HOLIDAY_2019)) should haveValue(false)
    resolved.map(calendar => calendar.isBusinessDay(CZ_HOLIDAY_2019)) should haveValue(false)
    resolved.map(calendar => calendar.isBusinessDay(NEW_YEAR_2019)) should haveValue(false)

    // The identifier the resolved calendar carries is the one that was asked for, so resolution
    // does not quietly rename the data.
    resolved.map(calendar => calendar.id) should haveValue(holidayCalendarId)
  }

  test("test_of_linked") {
    val test: HolidayCalendarId = HolidayCalendarId.of("GB~EU")
    test.name shouldBe "EU~GB"
    test.toString shouldBe "EU~GB"
    test.isComposite shouldBe true

    // As `test_of_combined`: equality replaces the Java `isSameAs`, the interning being gone.
    val test2: HolidayCalendarId = HolidayCalendarId.of("EU~GB")
    test shouldBe test2
    test.name shouldBe test2.name
    test.hashCode shouldBe test2.hashCode
  }

  test("test_of_linked_NoHolidays") {
    // Linking with a calendar in which every day is a business day leaves no day for the others
    // to close, so the no-holidays identifier swallows a linked composite whole. This is the
    // point at which the two separators differ in how they absorb it.
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

    // The same two levels reached through `linkedWith` rather than through a written name, which
    // is the other way a caller builds one: a composite may be linked as a whole, and the name
    // keeps the levels apart in the same order.
    HolidayCalendarId.of("GBLO+USNY").linkedWith(HolidayCalendarIds.JPTO).name shouldBe
      "GBLO+USNY~JPTO"
    HolidayCalendarId.of("GBLO+USNY~JPTO") shouldBe
      HolidayCalendarId.of("GBLO+USNY").linkedWith(HolidayCalendarIds.JPTO)
    HolidayCalendarId.of("GBLO+EUTA~USNY").name shouldBe "EUTA+GBLO~USNY"

    // And a combination of three sorts by the whole name of each part, the combination binding
    // more tightly than the link.
    HolidayCalendarIds.EUTA
      .combinedWith(HolidayCalendarIds.USNY)
      .combinedWith(HolidayCalendarIds.GBLO)
      .name shouldBe "EUTA+GBLO+USNY"
  }

  test("test_of_linked_resolve") {
    // Transcribed from the Java method. A linked identifier observes only the holidays both
    // centres agree on, so the two national holidays are business days and only the shared New
    // Year's Day remains a holiday.
    val holidayCalendarId: HolidayCalendarId = HolidayCalendarId.of("CZPR~USNY")
    val resolved = holidayCalendarId.resolve(REF_DATA)

    resolved should beSuccess
    resolved.map(calendar => calendar.name) should haveValue("CZPR~USNY")
    resolved.map(calendar => calendar.isBusinessDay(US_HOLIDAY_2019)) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(CZ_HOLIDAY_2019)) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(NEW_YEAR_2019)) should haveValue(false)
    resolved.map(calendar => calendar.id) should haveValue(holidayCalendarId)

    // The one deliberate behavioural divergence of this type, asserted here so that it cannot be
    // changed silently. Where a part of a linked identifier is itself '''composite''', the Java
    // implementation split the linked name on `'~'` and then performed one raw lookup per part
    // with no recursion, so the composite part `EUTA+GBLO` - which the standard reference data
    // does not hold as a whole - failed, and
    // `of("EUTA+GBLO~USNY").resolve(ReferenceData.standard())` threw
    // `ReferenceDataNotFoundException: Reference data not found for 'EUTA+GBLO' of type
    // 'HolidayCalendarId' when finding 'EUTA+GBLO~USNY'` (measured against the Java jar).
    //
    // This port resolves a composite part by asking it, so its own whole name is tried before
    // its parts are and the resolution succeeds. The behaviour is strictly more permissive - it
    // can only turn a Java failure into a success, never the reverse - it matches the resolution
    // flow of AAP section 0.6.5, and it is divergence 20 of `SCALA_MIGRATION.md`, discussed
    // there in section (c)-20.
    val insideLinked: HolidayCalendarId = HolidayCalendarId.of("EUTA+GBLO~USNY")
    insideLinked.name shouldBe "EUTA+GBLO~USNY"

    val resolvedInsideLinked = insideLinked.resolve(REF_DATA)
    resolvedInsideLinked should beSuccess
    resolvedInsideLinked.map(calendar => calendar.name) should haveValue("EUTA+GBLO~USNY")
    resolvedInsideLinked.map(calendar => calendar.id) should haveValue(insideLinked)

    // And what it answers: a day is a holiday only where the combination of the two European
    // centres and New York are all closed, so New Year's Day is a holiday while US Independence
    // Day - a business day in both European centres - is not.
    resolvedInsideLinked.map(calendar => calendar.isHoliday(NEW_YEAR_2019)) should haveValue(true)
    resolvedInsideLinked.map(calendar => calendar.isBusinessDay(US_HOLIDAY_2019)) should haveValue(true)
  }

  //-------------------------------------------------------------------------
  test("test_defaultByCurrency") {
    // The three rows the Java method asserted, followed by the whole thirty-one-row table.
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

    // Where the Java method asserted that the lookup '''threw''' for a currency with no
    // conventional calendar, this port answers `None`. The Java class offered the lookup twice -
    // one overload throwing `IllegalArgumentException`, one returning an optional value - and
    // this port merges them into the single total `Option`-returning member, so the absent
    // convention is an ordinary answer rather than a failure of the program. That is the
    // functional-error-handling requirement of the plan, Rule 5: a result the caller can act on
    // rather than an exception it must catch.
    HolidayCalendarId.defaultByCurrency(Currency.XAG) shouldBe None
    HolidayCalendarId.defaultByCurrency(Currency.XAU) shouldBe None
    noException should be thrownBy HolidayCalendarId.defaultByCurrency(Currency.XAG)

    // The sharpest statement about the thirteen rows that name calendars this library does not
    // ship: the lookup answers for them, and the identifier it answers with fails to resolve
    // against the standard reference data - exactly as in the library being ported, where those
    // rows were also data without calendars behind them. The lookup answering and the identifier
    // resolving are two different facts, so they are asserted separately.
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
    // The Java class had two lookups - one throwing, one optional - and this port has one, so
    // this test and `test_defaultByCurrency` above drive the same member. Both names are kept
    // because both Java methods are mapped in the migration manifest, and the rows asserted here
    // are the rows that Java method asserted.
    HolidayCalendarId.defaultByCurrency(Currency.GBP) shouldBe Some(HolidayCalendarIds.GBLO)
    HolidayCalendarId.defaultByCurrency(Currency.CZK) shouldBe Some(HolidayCalendarIds.CZPR)
    HolidayCalendarId.defaultByCurrency(Currency.HKD) shouldBe Some(HolidayCalendarId.of("HKHK"))
    HolidayCalendarId.defaultByCurrency(Currency.XAG) shouldBe None

    // The merged lookup is total over every currency: it answers for all of them, and answers
    // the same way however often it is asked.
    Currency.values.toList.foreach { currency =>
      withClue(s"$currency: ") {
        noException should be thrownBy HolidayCalendarId.defaultByCurrency(currency)
        HolidayCalendarId.defaultByCurrency(currency) shouldBe
          HolidayCalendarId.defaultByCurrency(currency)
      }
    }
  }

  test("test_defaultByCurrencyPair") {
    // The three rows the Java method asserted. The conventional calendars of the two currencies
    // are combined, so a day is a business day for the pair only where it is one for both.
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.GBP)) shouldBe
      HolidayCalendarIds.USNY.combinedWith(HolidayCalendarIds.GBLO)
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.GBP, Currency.CZK)) shouldBe
      HolidayCalendarId.of("CZPR+GBLO")
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.XAG)) shouldBe
      HolidayCalendarIds.USNY

    // The names, which are normalised, so the answer does not depend on which currency is the
    // base of the pair.
    HolidayCalendarId
      .defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.GBP))
      .name shouldBe "GBLO+USNY"
    HolidayCalendarId
      .defaultByCurrencyPair(CurrencyPair.of(Currency.GBP, Currency.USD))
      .name shouldBe "GBLO+USNY"
    HolidayCalendarId
      .defaultByCurrencyPair(CurrencyPair.of(Currency.GBP, Currency.CZK))
      .name shouldBe "CZPR+GBLO"

    // A currency with no conventional calendar contributes nothing, and where neither currency
    // has one the answer is the no-holidays identifier - the identity of combination - so this
    // lookup, unlike the one for a single currency, always answers with an identifier.
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.XAG)).name shouldBe
      "USNY"
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.XAG, Currency.XAU)) shouldBe
      HolidayCalendarIds.NO_HOLIDAYS

    // A pair of one currency with itself is that currency's calendar, the combination of an
    // identifier with itself being itself.
    HolidayCalendarId.defaultByCurrencyPair(CurrencyPair.of(Currency.USD, Currency.USD)) shouldBe
      HolidayCalendarIds.USNY
  }

  //-------------------------------------------------------------------------
  test("test_isCompositeCalendar") {
    HolidayCalendarId.isCompositeCalendar(HolidayCalendarId.of("GB+EU")) shouldBe true
    HolidayCalendarId.isCompositeCalendar(HolidayCalendarId.of("GB~EU")) shouldBe true
    HolidayCalendarId.isCompositeCalendar(HolidayCalendarId.of("GB")) shouldBe false

    // The static test of the Java class is the instance method here, kept under its old name so
    // that call sites read unchanged; the two must agree for every identifier.
    List("GB+EU", "GB~EU", "GB", "EU+Fri/Sat~GB", "GB+NoHolidays", "GB~NoHolidays", "NoHolidays")
      .foreach { name =>
        val id = HolidayCalendarId.of(name)
        withClue(s"$name -> ${id.name}: ") {
          HolidayCalendarId.isCompositeCalendar(id) shouldBe id.isComposite
        }
      }

    // A name that normalises to a single part reports itself as simple, which is the agreement
    // between the test and the structure the identifier carries.
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

    // Where the Java method asserted that resolving an absent identifier threw
    // `ReferenceDataNotFoundException`, this port reports it as a value: the failure is the one
    // the store itself reports, naming the identifier that could not be found.
    eu.resolve(refData) should beFailureWith(FailureReason.MISSING_DATA)
    eu.resolve(refData) should haveFailureMessageMatching("Reference data not found for identifier 'EU'")
    eu.resolve(refData).swap.map(failure => failure.attributes.get("id")) shouldBe Right(Some("EU"))
    noException should be thrownBy eu.resolve(refData)

    // The reader form resolves the same way, so reference data supplied later answers alike.
    gb.toReader.run(refData) should haveValue(gbCal)
    eu.toReader.run(refData) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_resolve_combined_direct") {
    // Reference data that holds the whole composite identifier has it used as it stands, which
    // is what lets a host supply a pre-combined calendar from a vendor feed.
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

    // The rule AAP section 0.6.5 states - the whole name is looked up '''before''' the parts -
    // asserted so that which of the two answered is observable. The marker calendar below is
    // deliberately different from the combination of the two components: it holds a holiday
    // neither component has, and neither of their holidays, so the dates alone say whether the
    // whole name won. This is what lets a host supply a pre-combined calendar - one merged from
    // a vendor feed - and have it used as it stands. The companion case, the same identifier
    // resolving from its parts once that entry is gone, is in `test_resolve_combined_indirect`.
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
    // Reference data that holds only the parts resolves each of them and combines the results in
    // the order the normalised name gives.
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

    // The Java method additionally asserted `refData.getValue(combined)`, which worked there
    // because the Java store delegated every lookup to a low-level query primitive that the
    // identifier overrode to perform this very assembly. That primitive is not ported - it
    // signalled absence by returning a reference to nothing - so in this port the store is a
    // plain map and the composite assembly lives on `HolidayCalendarId.resolve` alone. The
    // store therefore reports the composite as absent, which is the same fact
    // `HolidaySafeReferenceData` relies on when it leaves a composite unresolved so that its
    // parts can be defaulted one by one. Resolution through the identifier, asserted above, is
    // the path every adjustment and schedule of this library takes.
    refData.getValue(combined) should beFailureWith(FailureReason.MISSING_DATA)
    refData.findValue(combined) shouldBe None
    refData.containsValue(combined) shouldBe false

    // The combination observes the holidays of both parts, which is what the resolved value is
    // for: Friday, Saturday and Sunday are all holidays under it.
    val resolved = combined.resolve(refData)
    resolved.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 11))) should haveValue(true)
    resolved.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 12))) should haveValue(true)
    resolved.map(calendar => calendar.isHoliday(LocalDate.of(2014, 7, 13))) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(LocalDate.of(2014, 7, 14))) should haveValue(true)

    // The companion of the precedence case in `test_resolve_combined_direct`: with the entry for
    // the whole name absent, the same identifier resolves from its parts instead, and the answer
    // changes accordingly - both of their holidays, and not the marker's. The two halves together
    // are the only executable proof that the whole name is tried first.
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

    // A component that cannot be resolved at all, where the Java implementation threw. A
    // composite identifier is all-or-nothing - a calendar assembled from some of its parts would
    // silently declare business days that are holidays - and the failure names both the part that
    // was missing and the identifier being built, as attributes and not only in its text, which
    // is the context the caller needs.
    val missingComponent: HolidayCalendarId = gblo.combinedWith(HolidayCalendarIds.USNY)
    val partial: ReferenceData = ImmutableReferenceData.of(gblo, gbloCal)

    val failed = missingComponent.resolve(partial)
    failed should beFailureWith(FailureReason.MISSING_DATA)
    failed should haveFailureMessageMatching(
      "Reference data not found for 'USNY' of type 'HolidayCalendarId' when finding 'GBLO\\+USNY'")
    failed.swap.map(failure => failure.attributes.toMap) shouldBe
      Right(Map("id" -> "USNY", "compositeId" -> "GBLO+USNY"))
    noException should be thrownBy missingComponent.resolve(partial)

    // A linked identifier reports the same way, and the message names the linked identifier.
    gblo.linkedWith(HolidayCalendarIds.USNY).resolve(partial) should haveFailureMessageMatching(
      "Reference data not found for 'USNY' of type 'HolidayCalendarId' when finding 'GBLO~USNY'")
  }

  test("testImmutableReferenceDataWithMergedHolidays") {
    // Transcribed from the Java method: reference data holding a single pre-merged calendar,
    // filed under the identifier that calendar carries, is enough to adjust a date - the
    // identifier of a merged calendar resolves to the merged calendar itself.
    val hc: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    hc.id.name shouldBe "Fri/Sat+Sat/Sun"

    val referenceData: ImmutableReferenceData = ImmutableReferenceData.of(hc.id, hc)
    val date = BusinessDayAdjustment
      .of(BusinessDayConventions.PRECEDING, hc.id)
      .adjust(LocalDate.of(2016, 8, 20), referenceData)

    // Saturday the 20th of August 2016 precedes to the Thursday, because Friday, Saturday and
    // Sunday are all holidays of the merged calendar.
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

    // The parts are deduplicated and sorted, which is what makes the result independent of the
    // order the identifiers were combined in - `us` appears twice in `combined2` and once in the
    // name.
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

    // None of the four builds a composite, which is observable in the name and in the test.
    gb.combinedWith(gb).name shouldBe "GB"
    gb.combinedWith(gb).isComposite shouldBe false

    // The other half of the same algebra, which the Java class asserted only through `of`.
    // Linking an identifier with itself changes nothing, as combining does, but the no-holidays
    // identifier behaves oppositely under the two operations: it is the '''identity''' of
    // combination, since a calendar with no holidays removes nothing, and the '''absorbing
    // element''' of linking, since a calendar in which every day is a business day leaves no day
    // for the others to close.
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
    // the two hostile arguments of the Java method: the absent reference, and the object of
    // another type its `ANOTHER_TYPE` constant held
    a.equals(null) shouldBe false
    a.equals(ANOTHER_TYPE) shouldBe false

    // The hash is the name's, which is what the identifier being ported hashed and what makes the
    // layout of a map keyed by an identifier repeatable between runs.
    a.hashCode shouldBe "GB".hashCode
    Hash[HolidayCalendarId].hash(a) shouldBe "GB".hashCode

    // Two identifiers of the same name are equal without being the same object: this port does
    // not intern identifiers, and nothing needs it to, since equality is the name.
    a shouldBe a2
    Hash[HolidayCalendarId].eqv(a, a2) shouldBe true
    Hash[HolidayCalendarId].eqv(a, b) shouldBe false

    // The companion publishes one equality-bearing instance, an `Order` that is also a `Hash`,
    // so the ordering cannot disagree with the equality above. It is the ordering of the names:
    // `"EU"` precedes `"GB"`, and two identifiers compare equal exactly when they are equal.
    (Order[HolidayCalendarId].compare(b, a) < 0) shouldBe true
    (Order[HolidayCalendarId].compare(a, b) > 0) shouldBe true
    Order[HolidayCalendarId].compare(a, a2) shouldBe 0

    // An identifier works as a key of a map, which is the contract the reference data store
    // requires of it.
    Map(a -> "first").get(a2) shouldBe Some("first")
    Set(a, a2, b) should have size 2
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverPrivateConstructor(HolidayCalendarIds.class)`: it reflectively
    // invoked the private constructor of a static holder so that a coverage tool would not report
    // the class as unexercised. A Scala `object` has no such constructor to reach, so what the
    // call stood for is asserted directly - the holder publishes the twenty-nine identifiers the
    // library being ported published, each carrying its own name.
    forAll(dataConstants) { (constant: HolidayCalendarId, name: String) =>
      withClue(s"$name: ") {
        constant.name shouldBe name
        constant.toString shouldBe name
        // each constant is the identifier its own name builds, equality being by name
        constant shouldBe HolidayCalendarId.of(name)
        constant.isComposite shouldBe false
        Show[HolidayCalendarId].show(constant) shouldBe name
      }
    }

    val constants: List[HolidayCalendarId] = dataConstants.map { case (constant, _) => constant }.toList
    constants should have size 29
    constants.distinct should have size 29
    constants.map(id => id.name).distinct should have size 29

    // The membership of the set is the membership the library being ported published, and the
    // one name it is asked about most is the one that is '''not''' there: the Wellington
    // anniversary calendar `NZBD` is generated by `StandardHolidayCalendars` and resolves from
    // the standard reference data, yet the original never gave it a constant here, and this port
    // does not add one. The absence is asserted over the names the holder does publish - which
    // is a statement about this list, exhaustively enumerated above - rather than by reflecting
    // over the object's members, which this port never does.
    constants.map(id => id.name) should not contain "NZBD"
    HolidayCalendarId.of("NZBD").resolve(REF_DATA) should beSuccess
    assertDoesNotCompile("HolidayCalendarIds.NZBD")

    // The four weekend and no-holiday identifiers are those the calendars themselves carry, so
    // the holder is a set of names for existing data rather than a second source of names.
    HolidayCalendarIds.NO_HOLIDAYS shouldBe HolidayCalendars.NO_HOLIDAYS.id
    HolidayCalendarIds.SAT_SUN shouldBe HolidayCalendars.SAT_SUN.id
    HolidayCalendarIds.FRI_SAT shouldBe HolidayCalendars.FRI_SAT.id
    HolidayCalendarIds.THU_FRI shouldBe HolidayCalendars.THU_FRI.id

    // Every constant other than the no-holidays one resolves against the standard reference data,
    // which is what makes the holder usable without an application supplying calendars.
    constants.filterNot(id => id == HolidayCalendarIds.NO_HOLIDAYS).foreach { id =>
      withClue(s"${id.name}: ") {
        id.resolve(REF_DATA) should beSuccess
        id.resolve(REF_DATA).map(calendar => calendar.id) should haveValue(id)
      }
    }
    HolidayCalendarIds.NO_HOLIDAYS.resolve(REF_DATA) should haveValue(HolidayCalendars.NO_HOLIDAYS)

    // The companion publishes one equality-bearing instance - an ordering that is also a hashing -
    // so summoning the equality, the hashing or the ordering yields that one value and the three
    // can never disagree. Asserted over every ordered pair of a sample that mixes simple and
    // composite identifiers.
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

    // The ordering is the ordering of the names, so a sorted collection reads alphabetically and
    // agrees with the order the parts of a composite name are written in.
    sample.sorted(Order[HolidayCalendarId].toOrdering).map(id => id.name) shouldBe
      sample.map(id => id.name).sorted
    (Order[HolidayCalendarId].compare(HolidayCalendarIds.EUTA, HolidayCalendarIds.GBLO) < 0) shouldBe true
  }

  test("test_serialization") {
    // The Java method was `assertSerialization(of("US"))`, a Java-serialization round trip.
    // Neither Java serialization nor Joda-Beans wire compatibility is ported, so the round trip
    // that replaces it is the one this port does support: the circe codec, which writes an
    // identifier as the bare string of its name - never as an object - which is also the text
    // the library being ported produced through its own string conversion. The identifier the
    // Java method used is asserted first, so the ported method is recognisable.
    HolidayCalendarId.of("US").asJson shouldBe Json.fromString("US")
    decode[HolidayCalendarId]("\"US\"") shouldBe Right(HolidayCalendarId.of("US"))
    decode[HolidayCalendarId](HolidayCalendarId.of("US").asJson.noSpaces) shouldBe
      Right(HolidayCalendarId.of("US"))

    // A composite name round-trips the same way, which matters because the name is the whole
    // content of a composite identifier: were it written structurally, the normalisation that
    // makes `USNY+GBLO` and `GBLO+USNY` one value would have to be repeated by every reader.
    HolidayCalendarId.of("GBLO+USNY").asJson shouldBe Json.fromString("GBLO+USNY")
    decode[HolidayCalendarId]("\"GBLO+USNY\"") shouldBe Right(HolidayCalendarId.of("GBLO+USNY"))
    HolidayCalendarId.of("GBLO~USNY").asJson shouldBe Json.fromString("GBLO~USNY")
    decode[HolidayCalendarId]("\"GBLO~USNY\"") shouldBe Right(HolidayCalendarId.of("GBLO~USNY"))

    // The shape, stated so that a future change to an object form fails here. The exhaustive
    // property-based round trip over generated identifiers belongs to `json.JsonRoundTripSpec`;
    // what that sweep cannot say is what the document looks like, which is asserted here.
    HolidayCalendarIds.GBLO.asJson shouldBe Json.fromString("GBLO")
    HolidayCalendarId.of("GBLO+USNY").asJson shouldBe Json.fromString("GBLO+USNY")
    HolidayCalendarIds.GBLO.asJson.isString shouldBe true

    // Reading goes through the factory, so a name is normalised on the way in and two
    // differently written composites encode to identical bytes.
    decode[HolidayCalendarId]("\"GBLO\"") shouldBe Right(HolidayCalendarIds.GBLO)
    decode[HolidayCalendarId]("\"USNY+GBLO\"") shouldBe Right(HolidayCalendarId.of("GBLO+USNY"))
    HolidayCalendarId.of("USNY+GBLO").asJson.noSpaces shouldBe
      HolidayCalendarId.of("GBLO+USNY").asJson.noSpaces

    // Any name is accepted, including one this library knows nothing about - a missing calendar
    // is a fact about the reference data and is reported when the identifier is resolved.
    decode[HolidayCalendarId]("\"XXXX\"") shouldBe Right(HolidayCalendarId.of("XXXX"))

    // A document of the wrong JSON type is rejected: the codec reads a string and nothing else.
    Json.fromInt(1).as[HolidayCalendarId].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("GBLO")).as[HolidayCalendarId].isLeft shouldBe true
    Json.arr(Json.fromString("GBLO")).as[HolidayCalendarId].isLeft shouldBe true

    // An identifier also keys a JSON object, which is what lets a set of calendars be written as
    // an object keyed by the identifier each is held under. The key normalises as the value does.
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
   *
   * That factory reports the one way it can fail - two entries filed under the same identifier -
   * so there is an outcome to unwrap. A fixture that cannot be built is a defect in this spec
   * rather than a property of the subject, so it is reported as a failed test naming the cause.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the store holding exactly those entries
   */
  private def store(entries: ReferenceData.Entry[_]*): ImmutableReferenceData =
    ImmutableReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }
}
