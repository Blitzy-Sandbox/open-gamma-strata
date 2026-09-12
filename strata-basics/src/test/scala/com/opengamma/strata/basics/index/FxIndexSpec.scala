/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[FxIndex]].
 *
 * Every method of the Java original is kept, under its own name, so that the method-level
 * traceability of the migration stays one-to-one: the test inventory joins each row of the
 * migration's test mapping to a test of a named suite, so a renamed or omitted method is an
 * unmapped row rather than a tidier spec. The four parameterised methods of the original were
 * driven from a single provider, `data_name`; that shape is preserved - the provider becomes
 * one shared table, declared once below, and each of the four methods keeps its own test
 * driven from it rather than becoming eight tests of its own.
 *
 * Sixteen tests are declared here, in the order the Java class declared them.
 *
 * ===Why this family is the interesting one===
 *
 * Of the four index families this is the only one whose Java factory could *create* a member
 * that no reference data published. Asked for the index of a currency pair it had no row for,
 * that factory minted one on the spot - named after the pair, with the pair's default holiday
 * calendar and a settlement offset of two business days. This port does not reproduce that
 * fallback: under AAP §0.7 Rule 4 the family is the closed set of the sixteen published rows,
 * so a pair nothing is published for is reported as a failure. That is AAP §0.8.1 Conflict 7,
 * and this spec is its declared test owner; `test_of_lookup_parse_currency` is where the
 * divergence is asserted.
 *
 * Two neighbouring tests are easy to confuse, so each says in its own body which side of the
 * divergence it is on:
 *
 *  - `test_of_lookup_parse_currency` asks for `USD/CAD`, a pair nothing is published for. The
 *    Java method asserted the minted index; here it is a failure. This is the divergence.
 *  - `test_of_lookup_currency_pair_from_extendedEnum` asks for `USD/COP`, a pair that *is*
 *    published. It succeeded in Java and it succeeds here, unchanged.
 *
 * ===Methods asserting machinery this port does not have===
 *
 * Six of the sixteen methods asserted machinery the port replaces rather than reproduces, so
 * each is ported as the assertion of the guarantee that machinery gave. None is dropped, and
 * the reasoning is repeated at each of them:
 *
 *  - `test_extendedEnum` read the run-time registry the family was published through. Under
 *    the closed-family design (Rule 4) there is no registry to read, so the test asserts the
 *    closed-family equivalent of what the registry was consulted for.
 *  - `test_of_lookup_null` asserted that the factory rejected an absent name by raising an
 *    error. This port writes no such reference and its factories answer with a value, so the
 *    case becomes the two spellings of an absent name that can actually be supplied.
 *  - `test_of_lookup_parse_currency` and `test_equals` built indices through a builder, and
 *    `test_dates` built one over a calendar-day offset no published row uses. The family being
 *    closed, none of the three is representable; each is ported as ruled in its own body.
 *  - `coverage` called reflective sweeps over a Java bean and over the private constructor of
 *    its constants holder. No such helper exists here - the testkit of `strata-collect`
 *    publishes exactly five, none of them reflective - so the properties those sweeps stood in
 *    for are asserted directly.
 *  - `test_jodaConvert` and `test_serialization` asserted round trips through the reflective
 *    string-conversion library and the serialization mechanism of the platform, neither of
 *    which this port depends on (AAP §0.2.2). Each is ported as the round trip that replaces
 *    it - text and JSON respectively - and the two are kept apart, since the representations
 *    they pin are independent of each other.
 *
 * ===How the failing cases and the dates are written===
 *
 * Text or a pair naming no member is reported as a value and asserted through the outcome
 * matchers, never caught as an exception (AAP §0.7 Rule 5), and the reason is compared by
 * value rather than by matching a message, so the diagnostic wording stays free to change.
 * The date calculations answer with a value too, so each is unwrapped through the same
 * matchers rather than through an ad-hoc projection. No index is ever constructed here: every
 * value under test is reached through the family's own lookup or through the constants that
 * directory publishes (Rule 4).
 *
 * Every date asserted below was reproduced from the Java implementation before it was written
 * here, by running that implementation against the same index and the same reference data, so
 * a date in this file is a statement about the behaviour being ported rather than a transcript
 * of what this port happens to do.
 *
 * ===A note on the obligations cited above===
 *
 * `review_rules` reports that no user-specified rules document was provided for this project.
 * The numbered rules cited in this file are therefore requirements of the user's original
 * request, carried by AAP §0.7 and §0.8.1, and not entries of such a document; their absence
 * is not a licence to assert less.
 *
 * @see [[FxIndexDataSpec]] for the transcription of the sixteen published rows, which this
 *   spec deliberately does not repeat
 */
class FxIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The reference data every date calculation below is resolved against.
   *
   * This is the standard set, holding every built-in holiday calendar, which is what the Java
   * original used. Eight of the sixteen published indices name a calendar this library ships no
   * data for - as the original shipped none either - so those indices do not resolve against
   * this set; that is a property of the published data rather than of this spec, and where it
   * matters below it is asserted rather than worked around.
   */
  private val RefData: ReferenceData = ReferenceData.standard

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row pairs one of the eight published constants with the name it renders as and is
   * looked up by. The order is the order of the Java provider, which is also the declaration
   * order of the published index data except that the provider lists the euro/dollar rate of
   * the London market before the sterling/dollar one; it is kept as the provider wrote it.
   *
   * The rows are written out rather than derived from `FxIndex.values`, deliberately: the table
   * and the family's own enumeration are then two independent statements about the same
   * members, so a mistake in either cannot be masked by the other. `test_extendedEnum` is where
   * the two are compared.
   *
   * Eight of the sixteen published indices deliberately have no constant, so this table is not
   * the family; the two indices reached by name in `test_cny` and `test_inr` are among the eight
   * that are absent from it, and `test_extendedEnum` pins the size of the family itself.
   */
  private val dataName: TableFor2[FxIndex, String] = Table(
    ("index", "name"),
    (FxIndices.EUR_CHF_ECB, "EUR/CHF-ECB"),
    (FxIndices.EUR_GBP_ECB, "EUR/GBP-ECB"),
    (FxIndices.EUR_JPY_ECB, "EUR/JPY-ECB"),
    (FxIndices.EUR_USD_ECB, "EUR/USD-ECB"),
    (FxIndices.USD_CHF_WM, "USD/CHF-WM"),
    (FxIndices.EUR_USD_WM, "EUR/USD-WM"),
    (FxIndices.GBP_USD_WM, "GBP/USD-WM"),
    (FxIndices.USD_JPY_WM, "USD/JPY-WM")
  )

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: FxIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    // The rendering of an index is its name, which matters beyond being tidy: it is the form
    // the name lookup reads back, so the two are asserted together in `test_jodaConvert`.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The Java method called the factory that also reads text as a currency pair, so it is that
    // factory which is exercised here; the exact name lookup behind it is exercised as well, so
    // a name resolving through one route and not the other cannot pass.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      withClue(s"$name: ") {
        FxIndex.of(name) should haveValue(index)
        FxIndex.valueOf(name) shouldBe Some(index)
        FxIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // Ruling (AAP §0.4.1). The Java method reached for the run-time registry the family was
    // published through, asked it for the map of every member it had loaded, and looked each
    // provided name up in that map. Under Rule 4 there is no registry to reach for: the family
    // is a closed set created once from the transcribed index data, and the map that lookup
    // produced is now the family itself.
    //
    // So the test asserts what the registry was consulted for. Each provided name resolves,
    // through the family's own lookup, to the very constant the provider pairs it with; and the
    // family holds exactly the sixteen published indices, under sixteen distinct names, of
    // which the eight named by the provider are a subset.
    //
    // Three specs meet here and none may be weakened on another's account: the fidelity of the
    // sixteen rows against the Java-captured manifest belongs to the root
    // `ReferenceDataManifestSpec`, closedness across the families to `NamedEnumClosedSpec`, and
    // the family's own lookup to this test.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      withClue(s"$name: ") {
        FxIndex.valueOf(name) shouldBe Some(index)
      }
    }

    val all: List[FxIndex] = FxIndex.values.toList
    all should have size 16
    all.distinct should have size 16
    all.map(_.name).distinct should have size 16

    // every member of the family is reachable under its own name, including the eight that have
    // no constant, so the family and its lookup agree in both directions
    all.foreach { index =>
      withClue(s"${index.name}: ") {
        FxIndex.valueOf(index.name) shouldBe Some(index)
      }
    }

    val constants: List[FxIndex] = dataName.toList.map { case (index, _) => index }
    constants.map(_.name).toSet.subsetOf(all.map(_.name).toSet) shouldBe true
    // eight of the sixteen published indices deliberately have no constant; giving one to them
    // would be adding to the published API of this library rather than porting it
    constants.distinct should have size 8
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as
    // a value (Rule 5), with the reason compared by value rather than by matching the message.
    //
    // This is *not* the Conflict 7 case: `Rubbish` is neither the name of an index nor readable
    // as a currency pair, so the Java factory failed here too and the port merely changes how
    // the failure is reported. The divergence lives in `test_of_lookup_parse_currency`, where
    // the text does read as a pair.
    FxIndex.valueOf("Rubbish") shouldBe None
    FxIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    FxIndex.of("Rubbish") should beFailureWith(FailureReason.PARSING)
    CurrencyPair.parse("Rubbish").isLeft shouldBe true
  }

  test("test_of_lookup_null") {
    // Ruling. The Java method passed the absent reference to the factory and asserted that it
    // raised an error. This port writes no such reference (Rule 5) and its factories take a name
    // they resolve as a value, so the case is asserted as the spellings of an absent name that
    // can actually be supplied - the empty name and a blank one - each of which names no member
    // and is not readable as a currency pair either.
    FxIndex.valueOf("") shouldBe None
    FxIndex.parse("") should beFailureWith(FailureReason.PARSING)
    FxIndex.of("") should beFailureWith(FailureReason.PARSING)
    FxIndex.valueOf("   ") shouldBe None
    FxIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
    FxIndex.of("   ") should beFailureWith(FailureReason.PARSING)

    // the second clause of that sentence, asserted rather than assumed: neither spelling reads
    // as a currency pair, so the factory that falls back to reading one has nothing to fall back
    // on and the failure is reported for the name itself
    CurrencyPair.parse("").isLeft shouldBe true
    CurrencyPair.parse("   ").isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_of_lookup_parse_currency") {
    // Ruling - this is the assertion of AAP §0.8.1 Conflict 7, and it is the one deliberate
    // behavioural divergence of this family.
    //
    // The Java method built a dollar/Canadian-dollar index through a builder - named after the
    // pair, carrying the pair's default holiday calendar and a settlement offset of two
    // business days - and asserted that asking the factory for the text `USD/CAD` produced an
    // index equal to it. That worked because the Java factory, finding no published row for a
    // pair, minted one on the spot. Running the implementation being ported confirms it: it
    // answers `USD/CAD` with an index of that name whose calendar is `CATO+USNY`.
    //
    // Under Rule 4 the family is the closed set of the sixteen published rows, so that minting
    // path is deliberately not ported and is recorded as a divergence in `SCALA_MIGRATION.md`.
    // A pair nothing is published for is now reported as a failure, and that is what this test
    // asserts. No expected index is constructed here - none can be, and none should be: were
    // this test ever "fixed" by building one, it would be asserting the very behaviour the port
    // set out to remove.
    //
    // Note what is *not* asserted: `HolidayCalendarId.defaultByCurrencyPair`, which the Java
    // fallback used to pick the calendar, remains in production because other callers use it.
    // Only the minting of an index is gone.
    val usdCad: CurrencyPair = CurrencyPair.of(Currency.USD, Currency.CAD)
    usdCad.toString shouldBe "USD/CAD"

    // the text names no member of the family, so neither entry point of the name lookup
    // resolves it - and, the family being closed, nothing stands behind them to mint one
    FxIndex.valueOf(usdCad.toString) shouldBe None
    FxIndex.parse(usdCad.toString) should beFailureWith(FailureReason.PARSING)

    // the factory that reads text as a pair - the Java method's own entry point - reports the
    // failure rather than answering with an index; the reason is compared as a value of the
    // closed family of reasons, never by matching the message, so the wording stays free
    FxIndex.of(usdCad.toString) should beFailureWith(FailureReason.PARSING)

    // and so does the factory taking the pair itself, which is where the fallback used to be
    FxIndex.of(usdCad) should beFailureWith(FailureReason.PARSING)

    // the pair is a perfectly ordinary pair of published currencies, so the failure is about
    // the absence of a published index for it and not about the text being unreadable; this is
    // what distinguishes this test from `test_of_lookup_notFound`
    CurrencyPair.parse(usdCad.toString) shouldBe Right(usdCad)
    FxIndex.values.toList.map(_.currencyPair) should not contain usdCad
  }

  test("test_of_lookup_currency_pair_from_extendedEnum") {
    // The other side of Conflict 7, and the one that is unchanged: `USD/COP` *is* published, so
    // asking for the index of that pair succeeds here exactly as it did in Java. Keeping this
    // next to `test_of_lookup_parse_currency` is deliberate - together they say that the port
    // narrowed the factory to the published data and nothing more.
    //
    // The Java method built the expected index through a builder and compared field by field,
    // because equality of an FX index is by name alone and so would have compared too little.
    // Field by field is therefore the faithful form here too, and it is written out rather than
    // read from the transcribed row, so that a mistranscribed row fails this test.
    val usdCop: CurrencyPair = CurrencyPair.of(Currency.USD, Currency.COP)
    val cobo: HolidayCalendarId = HolidayCalendarId.of("COBO")
    val resolved: FxIndex =
      FxIndex.of(usdCop).getOrElse(fail("the FX index family publishes no index for USD/COP"))

    resolved.name shouldBe "USD/COP-TRM-COP02"
    resolved.currencyPair shouldBe usdCop
    resolved.fixingCalendar shouldBe cobo
    resolved.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(0, cobo)

    // the index reached from the pair is the very value the name lookup answers with; equality
    // being by name, this is asserted as identity of value and then again field by field
    FxIndex.valueOf("USD/COP-TRM-COP02") shouldBe Some(resolved)
    FxIndex.of("USD/COP-TRM-COP02") should haveValue(resolved)
    FxIndex.parse("USD/COP-TRM-COP02") should haveValue(resolved)
    val byName: FxIndex =
      FxIndex.valueOf("USD/COP-TRM-COP02").getOrElse(fail("no index named USD/COP-TRM-COP02"))
    byName.currencyPair shouldBe resolved.currencyPair
    byName.fixingCalendar shouldBe resolved.fixingCalendar
    byName.maturityDateOffset shouldBe resolved.maturityDateOffset

    // This index is the only published one whose settlement falls on the fixing date itself,
    // which is why the Java method chose it: the offset is zero business days rather than the
    // two every other published index but the Chilean one carries.
    resolved.maturityDateOffset should not be DaysAdjustment.ofBusinessDays(2, cobo)

    //-----------------------------------------------------------------------
    // The selection rule, which the Java suite never isolated but which Conflict 7 turns into a
    // contract of this family: a pair does not identify an index, because two administrators
    // publish the euro against the dollar. The factory has to choose, and it chooses the
    // candidate whose name sorts first - which is how the implementation being ported chose,
    // and which running it confirms. With the fallback gone this rule is the whole of
    // `of(pair)`, so it is pinned here rather than left to the family's documentation.
    val eurUsd: CurrencyPair = CurrencyPair.of(Currency.EUR, Currency.USD)
    FxIndices.EUR_USD_ECB.currencyPair shouldBe eurUsd
    FxIndices.EUR_USD_WM.currencyPair shouldBe eurUsd
    FxIndex.values.toList.filter(_.currencyPair == eurUsd).map(_.name) should contain theSameElementsAs
      List("EUR/USD-ECB", "EUR/USD-WM")

    FxIndex.of(eurUsd) should haveValue(FxIndices.EUR_USD_ECB)
    // stated the other way round as well, since "the lower name wins" is only meaningful if the
    // other candidate is the one that loses
    FxIndex.of(eurUsd) should not (haveValue(FxIndices.EUR_USD_WM))
    FxIndex.of(eurUsd).map(_.name) shouldBe Right("EUR/USD-ECB")
    FxIndex.of(eurUsd.toString) should haveValue(FxIndices.EUR_USD_ECB)

    // the two candidates are reachable individually by name, so the selection narrows what
    // `of(pair)` answers with and never what the family holds
    FxIndex.valueOf("EUR/USD-WM") shouldBe Some(FxIndices.EUR_USD_WM)
  }

  //-------------------------------------------------------------------------
  test("test_ecb_eur_gbp_dates") {
    // Transcribed date for date from the Java method, and every expectation below was
    // reproduced by running the implementation being ported against this index and the standard
    // reference data before it was written here.
    //
    // The two offsets mirror each other: settlement is two business days after the fixing, the
    // fixing two business days before the settlement, both counted in the calendar that combines
    // the euro-area and London calendars. The backward offset is derived by this port rather
    // than published, so asserting it is asserting that derivation.
    val test: FxIndex = FxIndices.EUR_GBP_ECB
    val eutaGblo: HolidayCalendarId = HolidayCalendarIds.EUTA.combinedWith(HolidayCalendarIds.GBLO)

    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, eutaGblo)
    test.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, eutaGblo)

    // The calculations answer with a value, so each is unwrapped through the outcome matchers
    // rather than through an ad-hoc projection (Rule 5); a calendar that failed to resolve would
    // be reported as a failure here and not mistaken for a date.
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromMaturity(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 13))
    // weekend
    test.calculateMaturityFromFixing(date(2014, 10, 16), RefData) should haveValue(date(2014, 10, 20))
    test.calculateFixingFromMaturity(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 16))
    test.calculateMaturityFromFixing(date(2014, 10, 17), RefData) should haveValue(date(2014, 10, 21))
    test.calculateFixingFromMaturity(date(2014, 10, 21), RefData) should haveValue(date(2014, 10, 17))
    // input date is Sunday
    test.calculateMaturityFromFixing(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 22))
    test.calculateFixingFromMaturity(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 16))
    // skip maturity over EUR (1st May) and GBP (5th May) holiday
    test.calculateMaturityFromFixing(date(2014, 4, 30), RefData) should haveValue(date(2014, 5, 6))
    test.calculateFixingFromMaturity(date(2014, 5, 6), RefData) should haveValue(date(2014, 4, 30))

    // resolve
    //
    // The Java method resolved the index into a function over fixing dates and asserted that
    // applying it agreed with observing the fixing directly. This port splits that into the
    // index resolving its calendars once - which is the whole point of the operation - and the
    // observation type building the result, so the two are composed here exactly as the
    // observation type documents. Resolution answers with a value, so the comparison is between
    // two outcomes; it is also asserted to be a success, without which two failures would
    // compare equal and the assertion would say nothing.
    val observationOf = test.resolveWith(RefData)(FxIndexObservation.create(test, _, _))
    val fromResolution = observationOf.map(build => build(date(2014, 5, 6)))
    fromResolution should beSuccess
    fromResolution shouldBe FxIndexObservation.of(test, date(2014, 5, 6), RefData)
  }

  test("test_dates") {
    // Ruling - custom-instance substitution, citing Rule 4 and AAP §0.8.1 Conflict 7.
    //
    // The Java method built an index named "Test" over the euro and sterling, fixing on a
    // calendar with no holidays at all and settling two *calendar* days after the fixing, then
    // asserted that arithmetic across a weekday, a weekend and a Sunday. Neither half of that
    // is representable here. The family is closed, so an index named "Test" cannot exist - its
    // class is sealed with a constructor unavailable outside its own file, and there is no
    // builder - and no published row settles on a calendar-day offset, so the behaviour that
    // index was built to exhibit is not the behaviour of anything this library publishes.
    //
    // The prompt's suggestion of a zero-lag published row is asserted below, but it cannot
    // carry the arithmetic: the only such row is the Colombian peso index, whose calendar this
    // library ships no data for, so its calculations report missing data rather than dates.
    //
    // So the same three shapes - a plain weekday, a settlement crossing a weekend, and an input
    // that is not a business day at all - are covered against a *published* index instead:
    // the euro/dollar rate of the European Central Bank, which fixes on the euro-area calendar
    // and settles two business days later counted in the euro-area and New York calendars
    // combined. That index also brings a case the Java method could not have: Columbus Day
    // 2014-10-13 is a holiday in New York but not in the euro area, so two different fixing
    // dates settle on the same day, and the backward calculation has to pick the later of them.
    // Every expected date below was reproduced from the implementation being ported.
    val test: FxIndex = FxIndices.EUR_USD_ECB
    test.fixingCalendar shouldBe HolidayCalendarIds.EUTA
    test.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(
      2,
      HolidayCalendarIds.EUTA.combinedWith(HolidayCalendarIds.USNY))

    // a plain weekday: Wednesday 15th fixes and settles on Friday 17th, and the backward
    // calculation returns the fixing date it came from
    test.calculateMaturityFromFixing(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 17))
    test.calculateFixingFromMaturity(date(2014, 10, 17), RefData) should haveValue(date(2014, 10, 15))

    // settlement crossing the weekend: Thursday 16th settles on Monday 20th, Friday 17th on
    // Tuesday 21st, and each backward calculation returns its own fixing date
    test.calculateMaturityFromFixing(date(2014, 10, 16), RefData) should haveValue(date(2014, 10, 20))
    test.calculateFixingFromMaturity(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 16))
    test.calculateMaturityFromFixing(date(2014, 10, 17), RefData) should haveValue(date(2014, 10, 21))
    test.calculateFixingFromMaturity(date(2014, 10, 21), RefData) should haveValue(date(2014, 10, 17))

    // an input that is not a business day of the fixing calendar is accepted rather than
    // rejected, exactly as the library being ported accepted it: Saturday 18th and Sunday 19th
    // are both moved onto Monday 20th and settle on Wednesday 22nd
    test.calculateMaturityFromFixing(date(2014, 10, 18), RefData) should haveValue(date(2014, 10, 22))
    test.calculateMaturityFromFixing(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 22))
    test.calculateMaturityFromFixing(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 22))
    // and a settlement date that is not a business day is likewise moved forward before the
    // fixing behind it is sought, so the weekend and the Monday all answer with Thursday 16th
    test.calculateFixingFromMaturity(date(2014, 10, 18), RefData) should haveValue(date(2014, 10, 16))
    test.calculateFixingFromMaturity(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 16))
    test.calculateFixingFromMaturity(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 16))

    // the holiday case, which is why the backward calculation is a search rather than an offset:
    // Friday 10th and Monday 13th both settle on Wednesday 15th, because the 13th is a holiday
    // in New York, and the fixing behind that settlement is the later of the two
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 15))
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromMaturity(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 13))

    // The zero-lag published row, asserted for what it is. The Colombian peso index settles on
    // the fixing date itself, and its calendar is one of the eight this library ships no data
    // for - as the library being ported shipped none either, where the same call raised an error
    // about the identifier it could not find. Here that is a value, so it is asserted as one;
    // this is also why the arithmetic above is carried by a different index.
    val zeroLag: FxIndex =
      FxIndex.valueOf("USD/COP-TRM-COP02").getOrElse(fail("no index named USD/COP-TRM-COP02"))
    zeroLag.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(0, HolidayCalendarId.of("COBO"))
    zeroLag.calculateMaturityFromFixing(date(2014, 10, 15), RefData) should
      beFailureWith(FailureReason.MISSING_DATA)
    zeroLag.calculateFixingFromMaturity(date(2014, 10, 15), RefData) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_cny") {
    // This index has no constant - it is one of the eight published rows the constants holder
    // deliberately does not name - so it is reached by name, which is the only route to it.
    val test: FxIndex =
      FxIndex.valueOf("USD/CNY-SAEC-CNY01").getOrElse(fail("no index named USD/CNY-SAEC-CNY01"))
    test.name shouldBe "USD/CNY-SAEC-CNY01"
    FxIndex.of("USD/CNY-SAEC-CNY01") should haveValue(test)
    FxIndex.parse("USD/CNY-SAEC-CNY01") should haveValue(test)
    test.currencyPair shouldBe CurrencyPair.of(Currency.USD, Currency.CNY)
  }

  test("test_inr") {
    // As with the Chinese index, this one has no constant and is reached by name.
    val test: FxIndex =
      FxIndex.valueOf("USD/INR-FBIL-INR01").getOrElse(fail("no index named USD/INR-FBIL-INR01"))
    test.name shouldBe "USD/INR-FBIL-INR01"
    FxIndex.of("USD/INR-FBIL-INR01") should haveValue(test)
    test.currencyPair shouldBe CurrencyPair.of(Currency.USD, Currency.INR)

    // The family declares exactly one alternate name, and it is asserted here because the
    // reference data that declared it is gone: the rate is named after the administrator that
    // publishes it, and a trade written before the benchmark was reformed names the previous
    // administrator. Both spellings resolve to the same value, and the canonical name is the
    // one the index reports - an alternate spelling never becomes the name of anything.
    FxIndex.valueOf("USD/INR-RBIB-INR01") shouldBe Some(test)
    FxIndex.of("USD/INR-RBIB-INR01") should haveValue(test)
    FxIndex.parse("USD/INR-RBIB-INR01") should haveValue(test)
    FxIndex.valueOf("USD/INR-RBIB-INR01").map(_.name) shouldBe Some("USD/INR-FBIL-INR01")
    FxIndex.values.toList.map(_.name) should not contain "USD/INR-RBIB-INR01"
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    // Ruling - and this family admits the ideal substitution, citing Rule 4 and AAP §0.3.3 kind
    // [R]. The Java method built an index named "GBP-EUR" through a builder, copied it under the
    // name "EUR-GBP" and asserted that the two were unequal - that is, that a rename alone
    // breaks equality, every other field being identical. The family being closed, neither index
    // is representable here.
    //
    // The published data contains the faithful replacement, and it is exact: the euro/dollar
    // rate is published by two administrators, so `EUR_USD_ECB` and `EUR_USD_WM` are two
    // distinct indices quoting one and the same currency pair. Asserting that they are unequal
    // while their pairs are equal says precisely what the Java method said - equality is by name
    // and by nothing else - over values this library actually publishes.
    val left: FxIndex = FxIndices.EUR_USD_ECB
    val right: FxIndex = FxIndices.EUR_USD_WM

    left.currencyPair shouldBe right.currencyPair
    left.name should not be right.name
    left should not be right
    Eq[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].hash(left) should not be Hash[FxIndex].hash(right)
    left.hashCode should not be right.hashCode

    // equality is not merely "not by pair": it is by name, so the value the lookup answers with
    // for a name is equal to the constant of that name, which is the positive half of the same
    // statement
    val looked: FxIndex =
      FxIndex.valueOf("EUR/USD-ECB").getOrElse(fail("no index named EUR/USD-ECB"))
    Eq[FxIndex].eqv(looked, left) shouldBe true
    looked shouldBe left
    looked.hashCode shouldBe left.hashCode
    Eq[FxIndex].eqv(looked, right) shouldBe false

    // equality is with an FX index and not with anything that merely carries the same name, so
    // the name itself is not equal to the index that reports it
    left should not be left.name
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Ruling. The Java method swept the private constructor of the constants holder reflectively
    // and then swept the index itself as a Java bean. Neither survives: the constants holder is
    // a Scala `object`, which has no constructor a caller could reach, and no reflective bean
    // sweep exists here - the testkit of `strata-collect` publishes exactly five helpers, none
    // of them reflective - because this port derives nothing reflectively (Rule 6).
    //
    // What those sweeps stood in for is asserted directly. First, the directory and the family
    // agree: the eight constants are distinct values, and each one is the very value the family
    // answers with for its own name. The holder resolves its constants through the exact name
    // lookup and deliberately not through the factory that reads a currency pair, which is what
    // keeps the two euro/dollar constants apart; both are asserted to exist and to differ, since
    // resolving them by pair would have collapsed them onto one.
    val constants: List[FxIndex] = dataName.toList.map { case (index, _) => index }
    constants should have size 8
    constants.distinct should have size 8
    constants.foreach { index =>
      withClue(s"${index.name}: ") {
        FxIndex.valueOf(index.name) shouldBe Some(index)
        FxIndex.parse(index.name) should haveValue(index)
        Show[FxIndex].show(index) shouldBe index.name
        index.toString shouldBe index.name
      }
    }
    FxIndices.EUR_USD_ECB should not be FxIndices.EUR_USD_WM
    FxIndex.of(CurrencyPair.of(Currency.EUR, Currency.USD)) should haveValue(FxIndices.EUR_USD_ECB)

    // Second, the equality cover the bean sweep provided, over two distinct published indices -
    // the euro/Swiss-franc reference rate and the sterling/dollar closing rate. The companion
    // publishes a single equality-bearing implicit, an ordering that is also a hashing, so
    // summoning the equality, the hashing or the ordering yields that one value and the three
    // can never disagree; the assertions below are the observable form of that.
    val left: FxIndex = FxIndices.EUR_CHF_ECB
    val right: FxIndex = FxIndices.GBP_USD_WM
    left should not be right
    Eq[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].hash(left) should not be Hash[FxIndex].hash(right)
    Show[FxIndex].show(left) shouldBe "EUR/CHF-ECB"
    Show[FxIndex].show(right) shouldBe "GBP/USD-WM"
    Order[FxIndex].compare(left, right) should not be 0
    // the ordering is by name, so its direction is that of the names it compares
    Order[FxIndex].compare(left, right) should be < 0
    Order[FxIndex].compare(right, left) should be > 0
    Order[FxIndex].compare(left, left) shouldBe 0

    // the ordering agrees with equality over every ordered pair of the family: it compares equal
    // exactly when the two values are equal, and equal values hash alike. This covers the two
    // euro/dollar indices as a pair, which is the case a name-only equality could get wrong.
    val all: List[FxIndex] = FxIndex.values.toList
    for (first <- all; second <- all) {
      val sameValue = first == second
      withClue(s"${first.name} against ${second.name}: ") {
        Eq[FxIndex].eqv(first, second) shouldBe sameValue
        Hash[FxIndex].eqv(first, second) shouldBe sameValue
        (Order[FxIndex].compare(first, second) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[FxIndex].hash(first) shouldBe Hash[FxIndex].hash(second)
        } else {
          Order[FxIndex].compare(first, second) should not be 0
        }
      }
    }
  }

  test("test_jodaConvert") {
    // The Java method asserted the round trip of the reflective string-conversion library the
    // original registered its two annotations with. That library is gone with the port (AAP
    // §0.2.2, §0.7 Rule 1), and the guarantee it gave is asserted directly: an index renders as
    // its name, and that rendering reads back as the same index.
    //
    // This is the text round trip. The JSON round trip is `test_serialization` below, and the two
    // are kept apart because the representations they pin are independent of each other - the
    // text form is what a caller writes and reads through `Show` and the name lookup, the JSON
    // form is what the codec writes and reads.
    //
    // Every name of this family contains a solidus, which is why the round trip is asserted for
    // all eight constants rather than for one: a renderer or reader that mishandled that
    // character would fail on every one of them, and a reader that split a name on it would
    // resolve the pair instead of the index - which for the euro/dollar rates would answer with
    // the wrong administrator's index rather than with a failure.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      val rendered = Show[FxIndex].show(index)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe index.name
        rendered should include("/")
        FxIndex.parse(rendered) should haveValue(index)
        FxIndex.valueOf(rendered) shouldBe Some(index)
        FxIndex.of(rendered) should haveValue(index)
      }
    }
  }

  test("test_serialization") {
    // The Java method asserted a round trip through the serialization mechanism of the platform,
    // which this port does not support (AAP §0.2.2). Its replacement is the JSON codec, and what
    // this test pins is the shape that codec is required to have for this family under AAP
    // §0.6.4: an index is written as the bare string of its name and never as an object, so a
    // document naming an index reads back here as the same member, solidus and all.
    //
    // The property-based round trip over every codec-bearing type of the module belongs to the
    // consolidated json.JsonRoundTripSpec, which is where the test mapping routes the
    // traceability of this Java method; this test deliberately asserts the per-family
    // representation rather than repeating that sweep, so that neither the shape nor the sweep
    // can be lost with the other.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      val encoded = index.asJson
      withClue(s"$name: ") {
        encoded shouldBe Json.fromString(name)
        encoded.asString shouldBe Some(name)
        encoded.as[FxIndex] shouldBe Right(index)
      }
    }

    // the alternate spelling of a name is readable, since the reader is the family's own lookup,
    // but it is never written: the index encodes under its canonical name
    val inr: FxIndex =
      FxIndex.valueOf("USD/INR-FBIL-INR01").getOrElse(fail("no index named USD/INR-FBIL-INR01"))
    Json.fromString("USD/INR-RBIB-INR01").as[FxIndex] shouldBe Right(inr)
    inr.asJson shouldBe Json.fromString("USD/INR-FBIL-INR01")

    // a string naming no member of the family is rejected by the reader rather than decoded into
    // some nearby index, and so is a structural form the encoder never writes. `USD/CAD` is the
    // Conflict 7 case again: readable as a currency pair, but naming no published index, so a
    // document carrying it fails to decode instead of minting an index.
    Json.fromString("Rubbish").as[FxIndex].isLeft shouldBe true
    Json.fromString("USD/CAD").as[FxIndex].isLeft shouldBe true
    Json.fromString("EUR/USD").as[FxIndex].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("EUR/CHF-ECB")).as[FxIndex].isLeft shouldBe true
  }
}
