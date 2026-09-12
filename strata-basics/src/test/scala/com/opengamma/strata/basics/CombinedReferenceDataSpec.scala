/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.HolidayCalendars
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[CombinedReferenceData]], ported from the Java `CombinedReferenceDataTest`.
 *
 * ===What the two fixtures are for===
 *
 * The subject of this spec is a single rule: when two sets of reference data are combined,
 * the first one asked wins. A spec cannot observe that rule unless the two sides disagree,
 * so the two fixtures are built to overlap. `BASE_DATA1` holds `ID1 -> VAL1` and
 * `ID2 -> VAL2`; `BASE_DATA2` holds `ID1 -> VAL3` and `ID3 -> VAL3`. The clash on `ID1` is
 * the whole point of the arrangement and is transcribed from the Java original unchanged:
 * every assertion about `ID1` below is an assertion about precedence, and it is the reason
 * `ID1` is asserted to resolve to `123d` and never to `999d`. `ID4` is held by neither side,
 * so it is the identifier that establishes what a combination does when it cannot answer.
 *
 * The identifier is [[TestingReferenceDataId]] rather than any identifier family this
 * library ships, because those carry resolution rules of their own - a `HolidayCalendarId`
 * resolves its components separately, for instance - and a lookup that failed could then
 * have failed in the identifier rather than in the combination.
 *
 * ===The third fixture, and why it is a calendar===
 *
 * The library applies this same rule on a caller's behalf in one place, and that place is
 * `ReferenceData.of`: it lays a caller's entries over the four built-in weekend and
 * no-holiday calendars, the caller's entry winning where the two meet. `test_combination`
 * asserts it, because a combination preferring the caller is exactly what it is, and because
 * nothing else in this module asserts which way round those two sets go - an assertion that
 * only read back values the built-in set and the caller agree about would pass just as
 * happily with the two laid the other way.
 *
 * So [[OVERRIDING_SAT_SUN]] is a deliberately different `Sat/Sun` calendar: it carries the
 * identifier of the built-in one and a holiday the built-in one does not have. It has to be
 * a calendar, because the minimal set holds nothing else, and the assertions about it have to
 * be about behaviour rather than equality: a holiday calendar is equal to any other calendar
 * of the same identifier whatever holidays it holds, so `getValue(SAT_SUN)` answering with
 * '''a''' `Sat/Sun` calendar says nothing, while its answer about [[OVERRIDDEN_DATE]] says
 * which one.
 *
 * ===How the shape of the port changes the assertions===
 *
 * Three differences from the original are structural rather than a matter of taste, and each
 * is noted again at the test it affects:
 *
 *   - `getValue` reports a missing item of reference data as a `Left` rather than throwing
 *     `ReferenceDataNotFoundException`, which is not ported. A lookup that cannot be
 *     satisfied is data about the request, not a defect in the program, so the assertion
 *     that was `assertThatExceptionOfType` is an assertion that the outcome is a failure
 *     carrying `FailureReason.MISSING_DATA`. Nothing in this API throws, so no test here
 *     asserts a throw.
 *   - `findValue` returns an `Option`, so the assertions written against `Optional.of` and
 *     `Optional.empty` are written against `Some` and `None`. The low-level
 *     `queryValueOrNull` the Java interface defined those in terms of has no counterpart.
 *   - The reflective bean sweep and Java serialization have no counterpart either, so
 *     `coverage` and `serialization` keep their names and assert the properties those two
 *     helpers stood for; each test says which, and why, at the point it does so.
 *
 * Each of the three ported tests keeps the name of the Java method it comes from -
 * `test_combination`, `coverage` and `serialization` - which is what keeps the method-level
 * traceability of the migration exact. A fourth, `test_combination_wrongValueType`, is added:
 * it asserts a property of this port that the Java original could not have had, since the case
 * it describes - two sides of a combination holding values of different types under one key -
 * is checked at retrieval here and was checked reflectively at construction there.
 */
class CombinedReferenceDataSpec extends AnyFunSuite with Matchers {

  import GenericTestingReferenceDataId.count
  import GenericTestingReferenceDataId.text

  /** An identifier held by both fixtures, under a different value in each. */
  private val ID1: TestingReferenceDataId = TestingReferenceDataId("1")

  /** An identifier held by the first fixture alone. */
  private val ID2: TestingReferenceDataId = TestingReferenceDataId("2")

  /** An identifier held by the second fixture alone. */
  private val ID3: TestingReferenceDataId = TestingReferenceDataId("3")

  /** An identifier held by neither fixture. */
  private val ID4: TestingReferenceDataId = TestingReferenceDataId("4")

  /**
   * The value held under `ID1` by the first fixture, and the value a combination preferring
   * that fixture must answer with.
   *
   * The three values are boxed explicitly because [[TestingReferenceDataId]] refers to a
   * `java.lang.Number`, exactly as the Java fixture did: the identifier fixes the type of
   * the value it can be paired with, and boxing at the point the value is written is what
   * that type costs. The literals carry the `d` suffix, so each is a `Double` as written
   * rather than an integer widened to one, which this build rejects.
   */
  private val VAL1: java.lang.Number = Double.box(123d)

  /** The value held under `ID2` by the first fixture. */
  private val VAL2: java.lang.Number = Double.box(234d)

  /** The value held under `ID1` and `ID3` by the second fixture. */
  private val VAL3: java.lang.Number = Double.box(999d)

  /** The first set of reference data, whose values a combination built from it prefers. */
  private val BASE_DATA1: ImmutableReferenceData = baseData1()

  /** The second set of reference data, consulted for identifiers the first does not hold. */
  private val BASE_DATA2: ImmutableReferenceData = baseData2()

  /**
   * Every identifier the two overlapping fixtures mention, in the order the assertions below
   * use them.
   *
   * The three tests each walk this list at least once, which is what makes "every entry" a
   * statement about a known set rather than about whichever entries a test happened to name.
   */
  private val ALL_IDS: List[TestingReferenceDataId] = List(ID1, ID2, ID3, ID4)

  /**
   * The date that tells the caller's `Sat/Sun` calendar apart from the built-in one.
   *
   * A Tuesday, so neither calendar's weekend covers it: the built-in `Sat/Sun` calendar
   * reports it as a business day and [[OVERRIDING_SAT_SUN]], which names it as a holiday,
   * reports it as a holiday. Every assertion about which of the two answered a lookup is
   * asked about this date, and `test_combination` asserts the built-in calendar's answer
   * alongside, so the two really do differ rather than being assumed to.
   */
  private val OVERRIDDEN_DATE: LocalDate = LocalDate.of(2015, 6, 30)

  /**
   * A caller's own definition of the `Sat/Sun` calendar, differing from the built-in one.
   *
   * It carries the identifier of one of the four calendars `ReferenceData.of` lays underneath
   * a caller's entries, and it observes [[OVERRIDDEN_DATE]] as a holiday, which the built-in
   * calendar of that identifier does not. Substituting a definition of a weekend calendar is
   * the case that factory's contract explicitly allows, and this is the substitution.
   */
  private val OVERRIDING_SAT_SUN: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(HolidayCalendarIds.SAT_SUN, List(OVERRIDDEN_DATE), Set(SATURDAY, SUNDAY))

  //-------------------------------------------------------------------------
  test("test_combination") {
    val test = CombinedReferenceData(BASE_DATA1, BASE_DATA2)

    // Membership: the combination contains an identifier held by either side, and only
    // those. `ID4` is held by neither, so it is the one that answers false.
    test.containsValue(ID1) shouldBe true
    test.containsValue(ID2) shouldBe true
    test.containsValue(ID3) shouldBe true
    test.containsValue(ID4) shouldBe false

    // Retrieval. The first assertion is the rule this spec exists for: `ID1` is held by both
    // sides, and the value returned is the one the first side holds - 123d, not the 999d of
    // the second. The two that follow show that an identifier held by one side alone is
    // answered from whichever side holds it.
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
    test.getValue(ID3) should haveValue(VAL3)

    // The Java original asserted that this threw `ReferenceDataNotFoundException`. That type
    // is not ported: an identifier no source of reference data holds is data about the
    // request rather than a defect in the program, so it is reported as a failure the caller
    // can act on. The reason is compared as a value of the closed family of reasons, never
    // as text.
    test.getValue(ID4) should beFailureWith(FailureReason.MISSING_DATA)

    // The same four lookups through the primitive, which reports absence as `None` where
    // `getValue` reports it as a failure. `Optional.of` and `Optional.empty` in the original.
    test.findValue(ID1) shouldBe Some(VAL1)
    test.findValue(ID2) shouldBe Some(VAL2)
    test.findValue(ID3) shouldBe Some(VAL3)
    test.findValue(ID4) shouldBe None

    //-----------------------------------------------------------------------
    // The same rule where the library applies it for a caller. `ReferenceData.of` combines
    // the caller's entries with the four built-in weekend and no-holiday calendars, the
    // caller's side preferred, so an entry filed under one of those four identifiers wins
    // and the other three remain. See the note on this class for why the discriminating
    // entry is a calendar and why these assertions are about behaviour.
    val layered = layeredOverMinimal(
      ReferenceData.Entry(HolidayCalendarIds.SAT_SUN, OVERRIDING_SAT_SUN),
      ReferenceData.Entry(ID1, VAL1))

    // The calendar that answers for `Sat/Sun` is the caller's: it treats 2015-06-30 as a
    // holiday. The next line is what makes that discriminating - the built-in calendar this
    // entry replaced treats the same Tuesday as a business day - so laying the built-in set
    // over the caller's entries instead of underneath them would turn the first assertion
    // false.
    layered.getValue(HolidayCalendarIds.SAT_SUN).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(true)
    HolidayCalendars.SAT_SUN.isHoliday(OVERRIDDEN_DATE) shouldBe false

    // The other three minimal identifiers are still present, still answer with the built-in
    // calendars, and are untouched by the override: none of them observes that Tuesday.
    layered.getValue(HolidayCalendarIds.NO_HOLIDAYS) should haveValue(HolidayCalendars.NO_HOLIDAYS)
    layered.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(HolidayCalendars.FRI_SAT)
    layered.getValue(HolidayCalendarIds.THU_FRI) should haveValue(HolidayCalendars.THU_FRI)
    layered.getValue(HolidayCalendarIds.NO_HOLIDAYS).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(false)
    layered.getValue(HolidayCalendarIds.FRI_SAT).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(false)
    layered.getValue(HolidayCalendarIds.THU_FRI).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(false)

    // Seeding the four calendars costs the caller nothing it supplied: the entry that is not
    // a calendar answers exactly as it was given.
    layered.getValue(ID1) should haveValue(VAL1)
    layered.findValue(ID4) shouldBe None

    // And it is the minimal set that is laid underneath, not the whole built-in one: `GBLO`
    // is a calendar of `ReferenceData.standard` and is absent here.
    layered.containsValue(HolidayCalendarIds.GBLO) shouldBe false
    layered.getValue(HolidayCalendarIds.GBLO) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts what a combination does when its two sides hold values of different types under
   * one key.
   *
   * This is the case that only combining can produce, and it is why the retrieval check of
   * `ReferenceDataId.valueType` is asserted here as well as in `ReferenceDataSpec`. A store is
   * keyed by an identifier whose type argument is erased, so the two instantiations of the
   * generic identifier family [[GenericTestingReferenceDataId]] are one key; a combination
   * consults two stores in order, and each may hold a value of a different type under that
   * key. Precedence alone would then decide the '''type''' a caller received, which is not
   * something precedence is entitled to decide.
   *
   * It does not, and the two orderings below are the assertion: each instantiation is answered
   * with the value of its own type, from whichever side holds one, because the side that holds
   * the other type reports an absence and the combination goes on to ask the next. So the rule
   * this spec exists for - the first side asked wins - is a rule about values of the requested
   * type, and a preferred side holding data of another type does not shadow the side that can
   * answer.
   */
  test("test_combination_wrongValueType") {
    val textId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val countId: GenericTestingReferenceDataId[Int] =
      GenericTestingReferenceDataId[Int]("shared")

    // one key, two value types: the store cannot tell the identifiers apart, and the witness
    // each carries is what the lookup tells them apart by
    (textId: ReferenceDataId[_]) shouldBe (countId: ReferenceDataId[_])

    val textData: ImmutableReferenceData = store(ReferenceData.Entry(textId, "a value"))
    val countData: ImmutableReferenceData = store(ReferenceData.Entry(countId, 42))

    val textFirst = CombinedReferenceData(textData, countData)
    textFirst.findValue(textId) shouldBe Some("a value")
    textFirst.findValue(countId) shouldBe Some(42)
    textFirst.getValue(textId) should haveValue("a value")
    textFirst.getValue(countId) should haveValue(42)

    // reversed, and the answers are the same: which side is preferred decides nothing here,
    // because the preferred side holds nothing of the type that was asked for
    val countFirst = CombinedReferenceData(countData, textData)
    countFirst.findValue(textId) shouldBe Some("a value")
    countFirst.findValue(countId) shouldBe Some(42)

    // a third instantiation, held by neither side, is absent from the combination rather than
    // answered with either value - the membership test agreeing with retrieval as ever. Its
    // witness is declared inline, which is what a host does for data of a type this library
    // knows nothing about; the bound name is `flag` rather than `value`, which the matcher
    // vocabulary this suite mixes in already binds in this scope.
    val flagId: GenericTestingReferenceDataId[Boolean] =
      GenericTestingReferenceDataId[Boolean]("shared")(
        ReferenceDataType.of("Boolean") { case flag: Boolean => flag })
    textFirst.containsValue(flagId) shouldBe false
    textFirst.findValue(flagId) shouldBe None
    textFirst.getValue(flagId) should beFailureWith(FailureReason.MISSING_DATA)
  }

  /**
   * Asserts the same over the route a caller actually takes, which is `combinedWith` rather
   * than this constructor.
   *
   * `ImmutableReferenceData.combinedWith` merges two materialised stores into one where it can,
   * so that a lookup stays a single map lookup. A merge is keyed by the identifier and an
   * identifier's type argument is erased, so the two instantiations below are one key: merging
   * would keep one value and drop the other, and the lookup for the dropped one would report
   * nothing although its value had been supplied. The store declines to merge for exactly that
   * reason, and this asserts the consequence rather than the reason - that the combination
   * answers for '''both''' identifiers, through every route a caller has, in either order.
   *
   * The last group is the other half of the condition: two stores whose shared key refers to
   * data of the same type still merge, so the assertion above is a statement about erased
   * disagreement and not the abandonment of the optimisation.
   */
  test("test_combinedWith_wrongValueType") {
    val textId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val countId: GenericTestingReferenceDataId[Int] =
      GenericTestingReferenceDataId[Int]("shared")
    (textId: ReferenceDataId[_]) shouldBe (countId: ReferenceDataId[_])

    val textData: ImmutableReferenceData = store(ReferenceData.Entry(textId, "a value"))
    val countData: ImmutableReferenceData = store(ReferenceData.Entry(countId, 42))

    // the public route, preferring the text store: the two identifiers disagree about the type
    // of the data they name, so the combination is a chain and neither value is lost
    val textFirst: ReferenceData = textData.combinedWith(countData)
    textFirst should not be an[ImmutableReferenceData]
    textFirst.findValue(textId) shouldBe Some("a value")
    textFirst.findValue(countId) shouldBe Some(42)
    textFirst.containsValue(textId) shouldBe true
    textFirst.containsValue(countId) shouldBe true
    textFirst.getValue(textId) should haveValue("a value")
    textFirst.getValue(countId) should haveValue(42)
    textId.resolve(textFirst) should haveValue("a value")
    countId.resolve(textFirst) should haveValue(42)
    textId.toReader.run(textFirst) should haveValue("a value")
    countId.toReader.run(textFirst) should haveValue(42)

    // and preferring the count store, which answers the same: the preferred side holds nothing
    // of the type the other identifier asks for, so preference decides nothing here
    val countFirst: ReferenceData = countData.combinedWith(textData)
    countFirst should not be an[ImmutableReferenceData]
    countFirst.findValue(textId) shouldBe Some("a value")
    countFirst.findValue(countId) shouldBe Some(42)
    countFirst.containsValue(textId) shouldBe true
    countFirst.containsValue(countId) shouldBe true
    countFirst.getValue(textId) should haveValue("a value")
    countFirst.getValue(countId) should haveValue(42)
    textId.resolve(countFirst) should haveValue("a value")
    countId.resolve(countFirst) should haveValue(42)
    textId.toReader.run(countFirst) should haveValue("a value")
    countId.toReader.run(countFirst) should haveValue(42)

    // the agreeing case, which does merge: one key, one value type, so the store that is asked
    // first wins it and a single map answers for both
    val otherTextId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val otherTextData: ImmutableReferenceData =
      store(ReferenceData.Entry(otherTextId, "another value"))
    val merged: ReferenceData = textData.combinedWith(otherTextData)
    merged shouldBe an[ImmutableReferenceData]
    merged.findValue(textId) shouldBe Some("a value")
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java test swept the immutable bean reflectively with `coverImmutableBean` - which
    // read every property and exercised equality, hashing and rendering - and then compared
    // the instance with its argument-swapped counterpart through `coverBeanEquals`. Neither
    // helper exists here, and neither could: there is no bean, and this port performs no
    // reflection. What that pair of calls stood for is asserted directly below, and the last
    // group is the part worth having - that the inequality of the two orderings is not a
    // formality but a difference in what they answer.
    val test = CombinedReferenceData(BASE_DATA1, BASE_DATA2)
    val swapped = CombinedReferenceData(BASE_DATA2, BASE_DATA1)

    // Both properties read back as they were supplied, in the order they were supplied.
    test.refData1 shouldBe BASE_DATA1
    test.refData2 shouldBe BASE_DATA2

    // Equality is by value. `same` is built from stores constructed afresh, so this asserts
    // that two combinations holding equal data are one value - not merely that an object
    // equals itself - and that equal values hash alike, which is what filing one in a map
    // requires.
    val same = CombinedReferenceData(baseData1(), baseData2())
    test shouldBe same
    test.hashCode shouldBe same.hashCode

    // The order of the operands is part of the value, so the swapped combination is a
    // different value and renders differently.
    test should not be swapped
    test.toString should not be swapped.toString

    // And the asymmetry is observable, which is why it matters: the clashing identifier
    // resolves to whichever value the first operand holds. Every other identifier is held by
    // one side alone, so both orderings agree about all of them.
    test.findValue(ID1) shouldBe Some(VAL1)
    swapped.findValue(ID1) shouldBe Some(VAL3)
    test.getValue(ID1) should haveValue(VAL1)
    swapped.getValue(ID1) should haveValue(VAL3)
    ALL_IDS.filterNot(id => id == ID1).foreach(id => swapped.findValue(id) shouldBe test.findValue(id))
  }

  //-------------------------------------------------------------------------
  test("serialization") {
    // The Java test asserted a Java-serialization round trip. Serialization support is
    // deliberately absent here in both of the senses that could apply, and this test is
    // what stands in its place:
    //
    //   - Java serialization is supported by no type of this port (AAP section 0.2.2), so
    //     there is nothing for a combination to take part in;
    //   - reference data additionally has no JSON codec (AAP section 0.6.4), and that is a
    //     property of the type rather than an omission: a store maps a type-erased
    //     identifier to a value whose type is known only through that identifier, so no
    //     encoder for an arbitrary store could be written. The one kind of reference data
    //     this library does serialize, a holiday calendar, carries its own codec, so a
    //     caller that knows which identifiers it expects rebuilds a store from serialized
    //     calendars - which is precisely the cycle asserted below.
    //
    // So the nearest meaningful property is asserted: the combination survives a
    // construct -> read-back -> reconstruct cycle with every entry intact, the absent
    // identifier still absent, and the precedence of the first operand preserved.
    val test = CombinedReferenceData(BASE_DATA1, BASE_DATA2)

    // Read back every entry the combination holds, through its public reading surface and
    // nothing else. An identifier it does not hold contributes no entry, so `ID4` is absent
    // from the result rather than present with an empty value.
    val readBack: Map[TestingReferenceDataId, java.lang.Number] =
      ALL_IDS.flatMap(id => test.findValue(id).map(value => id -> value)).toMap
    readBack shouldBe Map(ID1 -> VAL1, ID2 -> VAL2, ID3 -> VAL3)

    // Reconstruct a store from what was read back, and assert it answers exactly as the
    // combination did for every identifier - including `ID4`, which both report as absent.
    val rebuilt = store(readBack.toSeq.map { case (id, value) => ReferenceData.Entry(id, value) }: _*)
    ALL_IDS.foreach(id => rebuilt.findValue(id) shouldBe test.findValue(id))

    // Stated once more on the entry that could have been lost: the value carried across the
    // cycle is the first operand's 123d, not the second operand's 999d.
    rebuilt.getValue(ID1) should haveValue(VAL1)
    rebuilt.getValue(ID4) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  /**
   * Builds the first set of reference data, as the Java original's `baseData1` did.
   *
   * It is a method rather than an inlined expression for the reason the Java original made
   * it one: `coverage` needs a second, independently constructed store holding the same
   * entries, so that the equality it asserts is equality of two values and not two names for
   * one object.
   *
   * @return reference data holding `ID1 -> VAL1` and `ID2 -> VAL2`, and nothing else
   */
  private def baseData1(): ImmutableReferenceData =
    store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

  /**
   * Builds the second set of reference data, as the Java original's `baseData2` did.
   *
   * Note the deliberate clash: this store holds `ID1` as well, under a different value. See
   * the note on the class.
   *
   * @return reference data holding `ID1 -> VAL3` and `ID3 -> VAL3`, and nothing else
   */
  private def baseData2(): ImmutableReferenceData =
    store(ReferenceData.Entry(ID1, VAL3), ReferenceData.Entry(ID3, VAL3))

  /**
   * Builds a store holding exactly the entries given.
   *
   * `ImmutableReferenceData.of` is used rather than `ReferenceData.of` because the latter
   * layers the caller's entries over the four built-in weekend calendars, and a spec that
   * asserts `ID4` is absent needs a store that holds nothing it was not given.
   *
   * That factory reports the one way it can fail - two entries filed under the same
   * identifier - so this helper has an outcome to unwrap. A fixture that cannot be built is
   * a defect in this spec rather than a property of the subject, so it is reported as a
   * failed test naming the cause, which is the one place this file mentions failure handling
   * at all: the Java original could not express the case, since the `Map` it passed in had
   * already resolved any duplicate by insertion order.
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
   * Builds a set of reference data the way an application does, through `ReferenceData.of`.
   *
   * This is the counterpart of [[store]] and the difference between the two is the subject of
   * the last group of assertions in `test_combination`: this factory combines the entries
   * given with the four built-in weekend and no-holiday calendars, preferring the entries,
   * where `store` yields a set holding nothing it was not given.
   *
   * It reports the same single failure mode - two entries filed under one identifier - and a
   * fixture that cannot be built is a defect in this spec rather than a property of the
   * subject, so it is reported as a failed test naming the cause.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the entries combined with the minimal set of built-in calendars
   */
  private def layeredOverMinimal(entries: ReferenceData.Entry[_]*): ReferenceData =
    ReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }
}
