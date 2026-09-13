/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import scala.util.matching.Regex

import cats.Hash
import cats.Show
import cats.syntax.apply._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/** Test [[ReferenceData]] and [[ImmutableReferenceData]]. */
final class ReferenceDataSpec extends AnyFunSuite with Matchers {

  import GenericTestingReferenceDataId.count
  import GenericTestingReferenceDataId.text
  import ReferenceDataSpec.FallbackReferenceDataId
  import ReferenceDataSpec.ProbeResolvable
  import ReferenceDataSpec.ProbeResolvableTarget
  import ReferenceDataSpec.ProbeTarget
  import ReferenceDataSpec.RenderedReferenceDataId

  private val ID1: TestingReferenceDataId = TestingReferenceDataId("1")

  private val ID2: TestingReferenceDataId = TestingReferenceDataId("2")

  private val ID3: TestingReferenceDataId = TestingReferenceDataId("3")

  private val VAL1: java.lang.Number = Int.box(1)

  private val VAL2: java.lang.Number = Int.box(2)

  private val VAL3: java.lang.Number = Int.box(3)

  /**
   * Reference data holding `ID1 -> VAL1`, and nothing else.
   *
   * Each of the four providers declared here implements `findValue` alone, so `containsValue`,
   * `getValue` and `combinedWith` under test below are the trait's own defaults.
   */
  private val REF_DATA1: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = findIn(id, ID1 -> VAL1)
  }

  private val REF_DATA2: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = findIn(id, ID2 -> VAL2)
  }

  /** Reference data holding `ID1 -> VAL3`, so it disagrees with [[REF_DATA1]] about `ID1`. */
  private val REF_DATA3: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = findIn(id, ID1 -> VAL3)
  }

  private val REF_DATA12: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] =
      findIn(id, ID1 -> VAL1, ID2 -> VAL2)
  }

  //-------------------------------------------------------------------------
  test("test_standard") {
    val test: ReferenceData = ReferenceData.standard
    test.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe true
    test.containsValue(HolidayCalendarIds.SAT_SUN) shouldBe true
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe true
    test.containsValue(HolidayCalendarIds.THU_FRI) shouldBe true
    test.containsValue(HolidayCalendarIds.GBLO) shouldBe true
  }

  test("test_minimal") {
    val test: ReferenceData = ReferenceData.minimal
    test.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe true
    test.containsValue(HolidayCalendarIds.SAT_SUN) shouldBe true
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe true
    test.containsValue(HolidayCalendarIds.THU_FRI) shouldBe true

    // `standard` holds every built-in calendar while `minimal` holds only these four, so
    // `GBLO` is the one identifier that tells the two apart
    test.containsValue(HolidayCalendarIds.GBLO) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_of_RD") {
    val test: ReferenceData =
      layeredOverMinimal(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    // `ReferenceData.of` lays the caller's entries over the four built-in weekend and
    // no-holiday calendars, where `ImmutableReferenceData.of` in `test_of_IRD` is the raw
    // store and answers false for all four
    test.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe true
    test.containsValue(HolidayCalendarIds.SAT_SUN) shouldBe true
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe true
    test.containsValue(HolidayCalendarIds.THU_FRI) shouldBe true

    test.containsValue(ID1) shouldBe true
    test.getValue(ID1) should haveValue(VAL1)
    test.findValue(ID1) shouldBe Some(VAL1)

    test.containsValue(ID2) shouldBe true
    test.getValue(ID2) should haveValue(VAL2)
    test.findValue(ID2) shouldBe Some(VAL2)

    test.containsValue(ID3) shouldBe false
    test.getValue(ID3) should beFailureWith(FailureReason.MISSING_DATA)
    test.findValue(ID3) shouldBe None

    val duplicated: Either[Failure, ReferenceData] =
      ReferenceData.of(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID1, VAL2))
    duplicated should beFailureWith(FailureReason.INVALID)
    duplicated should haveFailureMessageMatching(
      Regex.quote(s"Duplicate reference data identifiers: $ID1"))
    failureOf(duplicated).attributes.get("duplicateIds") shouldBe Some(ID1.toString)
  }

  test("test_of_IRD") {
    val test: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    test.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false
    test.containsValue(HolidayCalendarIds.SAT_SUN) shouldBe false
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe false
    test.containsValue(HolidayCalendarIds.THU_FRI) shouldBe false

    test.containsValue(ID1) shouldBe true
    test.getValue(ID1) should haveValue(VAL1)
    test.findValue(ID1) shouldBe Some(VAL1)

    test.containsValue(ID2) shouldBe true
    test.getValue(ID2) should haveValue(VAL2)
    test.findValue(ID2) shouldBe Some(VAL2)

    test.containsValue(ID3) shouldBe false
    test.getValue(ID3) should beFailureWith(FailureReason.MISSING_DATA)
    test.findValue(ID3) shouldBe None
  }

  test("test_of_single") {
    val test: ReferenceData = ImmutableReferenceData.of(ID1, VAL1)

    test.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false
    test.containsValue(HolidayCalendarIds.SAT_SUN) shouldBe false
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe false
    test.containsValue(HolidayCalendarIds.THU_FRI) shouldBe false

    test.containsValue(ID1) shouldBe true
    test.getValue(ID1) should haveValue(VAL1)
    test.findValue(ID1) shouldBe Some(VAL1)

    test.containsValue(ID2) shouldBe false
    test.getValue(ID2) should beFailureWith(FailureReason.MISSING_DATA)
    test.findValue(ID2) shouldBe None
  }

  test("test_empty") {
    val test: ReferenceData = ReferenceData.empty

    test.containsValue(ID1) shouldBe false
    test.getValue(ID1) should beFailureWith(FailureReason.MISSING_DATA)
    test.findValue(ID1) shouldBe None

    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    test.combinedWith(data).findValue(ID1) shouldBe Some(VAL1)
    data.combinedWith(test).findValue(ID1) shouldBe Some(VAL1)
    test.combinedWith(data).findValue(ID3) shouldBe None
    data.combinedWith(test).findValue(ID3) shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_values") {
    val test: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    test.values shouldBe Map[ReferenceDataId[_], Any](ID1 -> VAL1, ID2 -> VAL2)
    test.values.get(ID3) shouldBe None

    test.values.keys.foreach(id => test.containsValue(id) shouldBe true)
    test.values.get(ID1) shouldBe test.findValue(ID1)
    test.values.get(ID2) shouldBe test.findValue(ID2)

    ImmutableReferenceData.empty.values shouldBe Map.empty[ReferenceDataId[_], Any]
    viewOf(ReferenceData.empty) shouldBe Map.empty[ReferenceDataId[_], Any]

    val minimalView: Map[ReferenceDataId[_], Any] = ReferenceData.minimal.values
    minimalView should have size 4
    viewOf(layeredOverMinimal(ReferenceData.Entry(ID1, VAL1))) shouldBe
      (minimalView + ((ID1: ReferenceDataId[_]) -> (VAL1: Any)))
    minimalView.get(HolidayCalendarIds.GBLO) shouldBe None

    val merged: ReferenceData =
      store(ReferenceData.Entry(ID1, VAL1)).combinedWith(store(ReferenceData.Entry(ID2, VAL2)))
    viewOf(merged) shouldBe Map[ReferenceDataId[_], Any](ID1 -> VAL1, ID2 -> VAL2)

    val clashing: ReferenceData =
      store(ReferenceData.Entry(ID1, VAL1)).combinedWith(store(ReferenceData.Entry(ID1, VAL3)))
    viewOf(clashing) shouldBe Map[ReferenceDataId[_], Any](ID1 -> VAL1)
  }

  //-------------------------------------------------------------------------
  test("test_of_badType") {
    assertDoesNotCompile("""ReferenceData.Entry(ID1, "67")""")
    assertDoesNotCompile("""ImmutableReferenceData.of(ID1, "67")""")
    assertCompiles("""ReferenceData.Entry(ID1, VAL1)""")
    assertCompiles("""ImmutableReferenceData.of(ID1, VAL1)""")
  }

  test("test_of_null") {
    assertDoesNotCompile("""ReferenceData.Entry(ID1, Option.empty[java.lang.Number])""")
    assertDoesNotCompile("""ReferenceData.Entry(ID1)""")
    assertDoesNotCompile("""ImmutableReferenceData.of(ID1)""")
    assertCompiles("""ReferenceData.Entry(ID1, VAL1)""")
  }

  // Two instantiations of `GenericTestingReferenceDataId` are one store key - equality is the
  // id alone, the type argument erased - so what tells them apart is retrieval: the asking
  // identifier narrows what the store holds with its own witness, and a value of the other
  // type is reported as an absence rather than handed back at the wrong type.
  test("test_findValue_genericIdFamily") {
    val textId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val countId: GenericTestingReferenceDataId[Int] =
      GenericTestingReferenceDataId[Int]("shared")

    (textId: ReferenceDataId[_]) shouldBe (countId: ReferenceDataId[_])
    textId.hashCode shouldBe countId.hashCode

    textId.valueType should not be countId.valueType

    val data: ImmutableReferenceData = store(ReferenceData.Entry(textId, "a value"))

    data.findValue(textId) shouldBe Some("a value")
    data.containsValue(textId) shouldBe true
    data.getValue(textId) should haveValue("a value")
    textId.resolve(data) should haveValue("a value")
    textId.toReader.run(data) should haveValue("a value")

    data.findValue(countId) shouldBe None
    data.containsValue(countId) shouldBe false
    data.getValue(countId) should beFailureWith(FailureReason.MISSING_DATA)
    countId.resolve(data) should beFailureWith(FailureReason.MISSING_DATA)
    countId.toReader.run(data) should beFailureWith(FailureReason.MISSING_DATA)

    data.findValue(countId).map(count => count + 1) shouldBe None

    assertDoesNotCompile("""ReferenceData.Entry(countId, "a value")""")
    assertCompiles("""ReferenceData.Entry(countId, 1)""")
  }

  //-------------------------------------------------------------------------
  test("test_defaultMethods") {
    REF_DATA1.containsValue(ID1) shouldBe true
    REF_DATA1.containsValue(ID2) shouldBe false

    REF_DATA1.getValue(ID1) should haveValue(VAL1)
    REF_DATA1.getValue(ID2) should beFailureWith(FailureReason.MISSING_DATA)

    REF_DATA1.findValue(ID1) shouldBe Some(VAL1)
    REF_DATA1.findValue(ID2) shouldBe None

    ID1.resolve(REF_DATA1) should haveValue(VAL1)
    ID2.resolve(REF_DATA1) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_combinedWith_other_other_noClash") {
    val test: ReferenceData = REF_DATA1.combinedWith(REF_DATA2)
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
  }

  test("test_combinedWith_other_other_noClashSame") {
    val test: ReferenceData = REF_DATA1.combinedWith(REF_DATA12)
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
  }

  test("test_combinedWith_other_other_clash") {
    val combined: ReferenceData = REF_DATA1.combinedWith(REF_DATA3)
    // the two fixtures disagree about `ID1`, and the receiver of `combinedWith` is asked first
    combined.getValue(ID1) should haveValue(VAL1)
  }

  //-------------------------------------------------------------------------
  // Combining two stores merges them into one store where their shared keys agree about the
  // type of data, so a lookup stays a single map lookup; a provider that computes its answers
  // has no map to merge, so combining with one yields a `CombinedReferenceData` consulting
  // the two in turn. That is what the type assertions in this and the three tests that follow
  // state.
  test("test_combinedWith_IRD_IRD_noClash") {
    val test1: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val test2: ImmutableReferenceData = store(ReferenceData.Entry(ID2, VAL2))

    val test: ReferenceData = test1.combinedWith(test2)
    test shouldBe a[ImmutableReferenceData]
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
  }

  test("test_combinedWith_IRD_IRD_noClashSame") {
    val test1: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val test2: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    val test: ReferenceData = test1.combinedWith(test2)
    test shouldBe a[ImmutableReferenceData]
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
  }

  test("test_combinedWith_IRD_IRD_clash") {
    val test1: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val test2: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL3))

    val combined: ReferenceData = test1.combinedWith(test2)
    combined shouldBe a[ImmutableReferenceData]
    combined.getValue(ID1) should haveValue(VAL1)
  }

  //-------------------------------------------------------------------------
  test("test_combinedWith_IRD_other_noClash") {
    val test1: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    val test: ReferenceData = test1.combinedWith(REF_DATA2)
    test shouldBe a[CombinedReferenceData]
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val same: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val other: ImmutableReferenceData = store(ReferenceData.Entry(ID2, VAL2))

    test.containsValue(ID1) shouldBe true
    test.findValue(ID1) shouldBe Some(VAL1)
    test.getValue(ID1) should haveValue(VAL1)

    test shouldBe same
    test.hashCode shouldBe same.hashCode
    test should not be other

    // the store is a map and renders its entries in sorted order, so neither the value nor
    // the text depends on the order the entries were supplied in
    val forward: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))
    val reversed: ImmutableReferenceData =
      store(ReferenceData.Entry(ID2, VAL2), ReferenceData.Entry(ID1, VAL1))
    forward shouldBe reversed
    forward.hashCode shouldBe reversed.hashCode
    forward.toString shouldBe reversed.toString

    test.toString shouldBe s"ImmutableReferenceData{values={$ID1=$VAL1}}"
    forward.toString shouldBe s"ImmutableReferenceData{values={$ID1=$VAL1, $ID2=$VAL2}}"

    val entry: ReferenceData.Entry[java.lang.Number] = ReferenceData.Entry(ID1, VAL1)
    val sameEntry: ReferenceData.Entry[java.lang.Number] = ReferenceData.Entry(ID1, VAL1)
    val otherEntry: ReferenceData.Entry[java.lang.Number] = ReferenceData.Entry(ID2, VAL2)
    entry.id shouldBe ID1
    entry.value shouldBe VAL1
    Hash[ReferenceData.Entry[java.lang.Number]].eqv(entry, sameEntry) shouldBe true
    Hash[ReferenceData.Entry[java.lang.Number]].eqv(entry, otherEntry) shouldBe false
    Hash[ReferenceData.Entry[java.lang.Number]].hash(entry) shouldBe
      Hash[ReferenceData.Entry[java.lang.Number]].hash(sameEntry)
    Show[ReferenceData.Entry[java.lang.Number]].show(entry) shouldBe entry.toString
    Show[ReferenceData.Entry[java.lang.Number]].show(entry) should include(ID1.toString)

    val standardFirst: ImmutableReferenceData = ReferenceData.standard
    val standardSecond: ImmutableReferenceData = ReferenceData.standard
    (standardFirst eq standardSecond) shouldBe true
    val minimalFirst: ImmutableReferenceData = ReferenceData.minimal
    val minimalSecond: ImmutableReferenceData = ReferenceData.minimal
    (minimalFirst eq minimalSecond) shouldBe true
  }

  test("test_serialization") {
    val test: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    assertDoesNotCompile("""implicitly[io.circe.Encoder[ImmutableReferenceData]]""")
    assertDoesNotCompile("""implicitly[io.circe.Decoder[ImmutableReferenceData]]""")

    assertCompiles("""implicitly[io.circe.Encoder[Int]]""")
    assertCompiles("""implicitly[io.circe.Decoder[Int]]""")

    val readBack: Option[java.lang.Number] = test.findValue(ID1)
    readBack shouldBe Some(VAL1)

    val rebuilt: ImmutableReferenceData =
      store(readBack.toList.map(value => ReferenceData.Entry(ID1, value)): _*)
    rebuilt shouldBe test
    rebuilt.getValue(ID1) should haveValue(VAL1)
    rebuilt.getValue(ID3) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_id_resolve") {
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    ID1.resolve(data) should haveValue(VAL1)
    ID1.resolve(data) shouldBe data.getValue(ID1)

    val missing: Either[Failure, java.lang.Number] = ID3.resolve(data)
    missing should beFailureWith(FailureReason.MISSING_DATA)
    missing should haveFailureMessageMatching(
      Regex.quote(s"Reference data not found for identifier '$ID3'"))
    failureOf(missing).attributes.get("id") shouldBe Some(ID3.toString)

    val fallback: FallbackReferenceDataId = FallbackReferenceDataId("9", VAL3)
    fallback.resolve(ReferenceData.empty) should haveValue(VAL3)
    ReferenceData.empty.getValue(fallback) should beFailureWith(FailureReason.MISSING_DATA)

    fallback.resolve(store(ReferenceData.Entry(fallback, VAL1))) should haveValue(VAL1)
  }

  test("test_id_toReader") {
    val data: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    val both: RefDataReader[(java.lang.Number, java.lang.Number)] =
      (ID1.toReader, ID2.toReader).tupled
    both.run(data) shouldBe Right((VAL1, VAL2))

    val summed: RefDataReader[Int] =
      (ID1.toReader, ID2.toReader).mapN((first, second) => first.intValue + second.intValue)
    summed.run(data) shouldBe Right(3)

    val sequenced: RefDataReader[Int] = for {
      first <- ID1.toReader
      second <- ID2.toReader
    } yield first.intValue + second.intValue
    sequenced.run(data) shouldBe Right(3)

    val failing: RefDataReader[(java.lang.Number, java.lang.Number)] =
      (ID1.toReader, ID3.toReader).tupled
    val outcome: Either[Failure, (java.lang.Number, java.lang.Number)] = failing.run(data)
    outcome should beFailureWith(FailureReason.MISSING_DATA)
    outcome should haveFailureMessageMatching(
      Regex.quote(s"Reference data not found for identifier '$ID3'"))

    FallbackReferenceDataId("9", VAL3).toReader.run(ReferenceData.empty) should haveValue(VAL3)
  }

  test("test_resolvable") {
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    ProbeResolvable(ID1).resolve(data) should haveValue("resolved 1")

    ProbeResolvable(ID3).resolve(data) should beFailureWith(FailureReason.MISSING_DATA)

    val composed: RefDataReader[String] =
      (ProbeResolvable(ID1).toReader, ID1.toReader).mapN((text, value) =>
        s"$text/${value.intValue}")
    composed.run(data) shouldBe Right("resolved 1/1")
    ProbeResolvable(ID3).toReader.run(data) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_resolvableCalculationTarget") {
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val probe: ProbeResolvableTarget = ProbeResolvableTarget(ID1)

    val asTarget: CalculationTarget = probe
    asTarget shouldBe probe

    val resolved: Either[Failure, CalculationTarget] = probe.resolveTarget(data)
    resolved should haveValue(ProbeTarget(VAL1))

    ProbeResolvableTarget(ID3).resolveTarget(data) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  // `ReferenceDataId` constrains `toString` in no way, so an identifier that renders as two
  // lines is one a host can define. Both halves of the failure policy are asserted of it: the
  // failure carries the rendering exactly, in its message and under its `id` attribute, while
  // every text form of the failure is a single line holding no control character - so a log
  // line carrying it cannot be split (CWE-117), and the line feed and the carriage return
  // appear there as the two-character escapes `escaped` spells.
  test("test_notFound_injectedIdentifier") {
    val rendering: String = "InjectedId [id=1]\nMISSING_DATA: forged by the caller\r"
    val id: RenderedReferenceDataId = RenderedReferenceDataId(rendering)

    val missing: Either[Failure, java.lang.Number] = ReferenceData.empty.getValue(id)
    missing should beFailureWith(FailureReason.MISSING_DATA)

    val failure: Failure = failureOf(missing)
    failure.message shouldBe s"Reference data not found for identifier '$rendering'"
    failure.attributes.get("id") shouldBe Some(rendering)

    id.resolve(ReferenceData.empty) shouldBe missing

    val escaped: String = "InjectedId [id=1]\\nMISSING_DATA: forged by the caller\\r"
    val rendered: String = Show[Failure].show(failure)
    rendered shouldBe
      s"MISSING_DATA: Reference data not found for identifier '$escaped' [id=$escaped]"
    failure.toString shouldBe rendered
    rendered.exists(_.isControl) shouldBe false
  }

  // The failure keeps the whole rendering, while its text form stays bounded: the ceiling
  // asserted is above the fixed text plus the two bounded parts and far below the ten
  // thousand characters held, so the case pins the property rather than the constant.
  test("test_notFound_oversizedIdentifier") {
    val rendering: String = "9" * 10000
    val id: RenderedReferenceDataId = RenderedReferenceDataId(rendering)

    val failure: Failure = failureOf(ReferenceData.empty.getValue(id))
    failure.message shouldBe s"Reference data not found for identifier '$rendering'"
    failure.message.length should be > 10000
    failure.attributes("id").length shouldBe 10000

    val rendered: String = Show[Failure].show(failure)
    failure.toString shouldBe rendered
    rendered.length should be < 1500
    rendered should startWith("MISSING_DATA: Reference data not found for identifier '9")
    rendered should endWith("...]")
    rendered.exists(_.isControl) shouldBe false
  }

  // Duplicated identifiers are reported sorted by their rendering, so the same set of entries
  // is the same failure however the caller arranged them. `I` sorts before `O`, so the
  // expected order is stated here rather than computed from the values under test.
  test("test_duplicateIds_injectedAndOversizedIdentifiers") {
    val injected: String = "InjectedId [id=1]\nINVALID: forged by the caller"
    val oversized: String = s"OversizedId ${"9" * 10000}"
    val first: RenderedReferenceDataId = RenderedReferenceDataId(injected)
    val second: RenderedReferenceDataId = RenderedReferenceDataId(oversized)

    val duplicated: Either[Failure, ImmutableReferenceData] = ImmutableReferenceData.of(
      ReferenceData.Entry(first, VAL1),
      ReferenceData.Entry(first, VAL2),
      ReferenceData.Entry(second, VAL1),
      ReferenceData.Entry(second, VAL2))
    duplicated should beFailureWith(FailureReason.INVALID)

    val failure: Failure = failureOf(duplicated)
    val expected: String = s"$injected, $oversized"
    failure.message shouldBe s"Duplicate reference data identifiers: $expected"
    failure.attributes.get("duplicateIds") shouldBe Some(expected)
    failure.attributes("duplicateIds").length should be > 10000

    val reversed: Either[Failure, ImmutableReferenceData] = ImmutableReferenceData.of(
      ReferenceData.Entry(second, VAL2),
      ReferenceData.Entry(second, VAL1),
      ReferenceData.Entry(first, VAL2),
      ReferenceData.Entry(first, VAL1))
    failureOf(reversed) shouldBe failure

    val layered: Either[Failure, ReferenceData] = ReferenceData.of(
      ReferenceData.Entry(first, VAL1),
      ReferenceData.Entry(first, VAL2),
      ReferenceData.Entry(second, VAL1),
      ReferenceData.Entry(second, VAL2))
    failureOf(layered) shouldBe failure

    val rendered: String = Show[Failure].show(failure)
    failure.toString shouldBe rendered
    rendered.length should be < 1500
    rendered should startWith(
      "INVALID: Duplicate reference data identifiers: " +
        "InjectedId [id=1]\\nINVALID: forged by the caller, OversizedId 9")
    rendered should endWith("...]")
    rendered.exists(_.isControl) shouldBe false
  }

  //-------------------------------------------------------------------------
  /**
   * Finds an identifier among the entries a provider holds.
   *
   * The body of all four providers above, written as [[ImmutableReferenceData.findValue]] is
   * written: the matching entry is found by equality, as a map lookup finds it, and the value
   * is then narrowed by the witness the asking identifier carries - so this needs no cast, and
   * a provider reports a value of another type as absent for the reason a store does.
   *
   * @return the value held for the identifier, empty if the provider holds none
   */
  private def findIn[T](
      id: ReferenceDataId[T],
      entries: (ReferenceDataId[_], java.lang.Number)*): Option[T] =
    entries
      .collectFirst { case (entryId, entryValue) if entryId == id => entryValue }
      .flatMap(entryValue => id.valueType.narrow(entryValue))

  /**
   * Builds a store holding exactly the entries given.
   *
   * `ImmutableReferenceData.of` rather than `ReferenceData.of`: the latter lays the four
   * built-in weekend calendars underneath, and the assertions that an identifier is absent
   * need a store holding nothing it was not given. The one failure that factory reports - two
   * entries under one identifier - is a defect in this fixture, so it fails the test; the case
   * itself is asserted, as a property of the subject, in `test_of_RD`.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   */
  private def store(entries: ReferenceData.Entry[_]*): ImmutableReferenceData =
    ImmutableReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }

  private def layeredOverMinimal(entries: ReferenceData.Entry[_]*): ReferenceData =
    ReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }

  private def viewOf(data: ReferenceData): Map[ReferenceDataId[_], Any] =
    data match {
      case immutable: ImmutableReferenceData => immutable.values
      case other => fail(s"Expected a materialised store but the reference data was $other")
    }

  private def failureOf(outcome: Either[Failure, Any]): Failure =
    outcome.fold(
      failure => failure,
      value => fail(s"Expected a failure but the outcome carried the value $value"))
}

/**
 * The fixtures of [[ReferenceDataSpec]] that are types rather than values.
 *
 * They sit in the companion object because a case class nested in a class carries a reference
 * to the instance that declared it, which its generated equality cannot check.
 */
object ReferenceDataSpec {
  /**
   * An identifier with a resolution rule of its own, in miniature.
   *
   * It consults the reference data it is given and falls back to a value it carries, so the
   * override is observable in both directions: it answers where `getValue` fails, and defers
   * to the store where the store can answer.
   *
   * @param fallback  the value resolution answers with when the reference data holds none
   */
  final case class FallbackReferenceDataId(id: String, fallback: java.lang.Number)
      extends ReferenceDataId[java.lang.Number] {

    override def valueType: ReferenceDataType[java.lang.Number] = TestingReferenceDataId.number

    override def resolve(refData: ReferenceData): Either[Failure, java.lang.Number] =
      Right(refData.findValue(this).getOrElse(fallback))

    override def toString: String = s"FallbackReferenceDataId [id=$id]"
  }

  /**
   * An identifier that renders as exactly the text it was built with.
   *
   * `ReferenceDataId` constrains `toString` in no way, so this is the identifier the three
   * diagnostic tests hand a lookup when they need a rendering holding a line feed or ten
   * thousand characters.
   *
   * @param rendering  the text this identifier renders as, and which alone determines equality
   */
  final case class RenderedReferenceDataId(rendering: String)
      extends ReferenceDataId[java.lang.Number] {

    override def valueType: ReferenceDataType[java.lang.Number] = TestingReferenceDataId.number

    override def toString: String = rendering
  }

  final case class ProbeResolvable(id: ReferenceDataId[java.lang.Number])
      extends Resolvable[String] {

    override def resolve(refData: ReferenceData): Either[Failure, String] =
      id.resolve(refData).map(value => s"resolved ${value.intValue}")
  }

  final case class ProbeTarget(value: java.lang.Number) extends CalculationTarget

  final case class ProbeResolvableTarget(id: ReferenceDataId[java.lang.Number])
      extends ResolvableCalculationTarget {

    override def resolveTarget(refData: ReferenceData): Either[Failure, CalculationTarget] =
      id.resolve(refData).map(value => ProbeTarget(value))
  }
}
