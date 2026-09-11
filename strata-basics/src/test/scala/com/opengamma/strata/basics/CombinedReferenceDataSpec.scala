/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

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
 * Each of the three tests keeps the name of the Java method it comes from - `test_combination`,
 * `coverage` and `serialization` - which is what keeps the method-level traceability of the
 * migration exact.
 */
class CombinedReferenceDataSpec extends AnyFunSuite with Matchers {

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
   * Every identifier the fixtures mention, in the order the assertions below use them.
   *
   * The three tests each walk this list at least once, which is what makes "every entry" a
   * statement about a known set rather than about whichever entries a test happened to name.
   */
  private val ALL_IDS: List[TestingReferenceDataId] = List(ID1, ID2, ID3, ID4)

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
}
