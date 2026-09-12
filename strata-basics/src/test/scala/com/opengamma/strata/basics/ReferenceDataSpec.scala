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

/**
 * Test [[ReferenceData]] and [[ImmutableReferenceData]], ported from the Java `ReferenceDataTest`.
 *
 * Every one of the eighteen methods of the original is kept, under the name the original gave it,
 * so that a Java test method and a test of this suite stay in one-to-one correspondence in the
 * migration manifest. Four further tests are added at the end, for the three members of this
 * package that the Java test inventory covers nowhere - `ReferenceDataId.resolve`,
 * `ReferenceDataId.toReader` and the two resolution contracts [[Resolvable]] and
 * [[ResolvableCalculationTarget]] - because those exist only in this port and would otherwise
 * carry no regression coverage at all. Two more sit in the middle, with the group of tests each
 * belongs to: `test_values` asserts the public view of a store's content, which the original
 * read only through its reflective bean sweep, and `test_findValue_genericIdFamily` asserts the
 * retrieval check `ReferenceDataId.valueType` supplies, which the original performed
 * reflectively at construction time instead.
 *
 * Three further tests follow those, over the two diagnostics this file reports - the
 * missing-identifier failure and the duplicate-identifier failure. Both name text that
 * reaches the library from a host's own `ReferenceDataId`, whose rendering nothing
 * constrains, so each of the three asserts both halves of the port's message policy at once:
 * the failure carries the identifier exactly as the identifier renders itself, untruncated,
 * and every text form of that failure is a single bounded line. The identifier they use
 * renders as whatever the test hands it, which is what lets them state a rendering holding a
 * line feed or ten thousand characters.
 *
 * ===The fixtures, and why four of them are hand-written implementations===
 *
 * The identifier is [[TestingReferenceDataId]] rather than any identifier family this library
 * ships, because those carry resolution rules of their own - a `HolidayCalendarId` resolves its
 * components separately, for instance - and a lookup that failed could then have failed in the
 * identifier rather than in the reference data. The values are boxed integers, because that
 * fixture refers to a `java.lang.Number`, exactly as the Java fixture did and exactly as the Java
 * test stored `1`, `2` and `3` under it.
 *
 * The Java test declared four anonymous `ReferenceData` classes, each overriding the low-level
 * `queryValueOrNull`. That primitive has no counterpart here, so the four become four
 * implementations overriding [[ReferenceData.findValue]], which is the single abstract member of
 * the trait. They are what the `other` half of the six `combinedWith` tests means - a set of
 * reference data that is not a materialised store, so that combining takes the general path
 * rather than the store-to-store merge - and they double as the proof the trait is open, which
 * is a requirement of the port rather than an accident: reference data is an extension point,
 * and an application backing it with a database or a service writes exactly what these four
 * fixtures write.
 *
 * ===How the shape of the port changes the assertions===
 *
 * Three differences from the original are structural rather than a matter of taste, and each is
 * noted again at the test it affects:
 *
 *   - `getValue` reports a missing item of reference data as a `Left` rather than throwing
 *     `ReferenceDataNotFoundException`, which is not ported. Every `assertThatExceptionOfType`
 *     of the original therefore becomes an assertion that the outcome is a failure carrying
 *     `FailureReason.MISSING_DATA`, with the reason compared as a value of the closed family of
 *     reasons and never as text. Nothing in this API throws, so no test here asserts a throw.
 *   - `findValue` returns an `Option`, so the assertions written against `Optional.of` and
 *     `Optional.empty` are written against `Some` and `None`.
 *   - `test_of_badType` and `test_of_null` asserted run-time rejections - a value of the wrong
 *     type, and a value that was absent - that [[ReferenceData.Entry]] makes unrepresentable.
 *     Both therefore become compile-time proofs; see the note on each.
 *
 * The reflective bean sweep and the Java-serialization round trip have no counterpart either, so
 * `coverage` and `test_serialization` keep their names and assert the properties those two
 * helpers stood for; each says which, and why, at the point it does so.
 *
 * ===What is asserted elsewhere===
 *
 * `CombinedReferenceDataSpec` owns the combination itself: which side of a clash wins, and the
 * demonstration that `ReferenceData.of` lays a caller's entries over the four built-in weekend
 * and no-holiday calendars with a substituted `Sat/Sun` calendar. This suite asserts the cases
 * of the Java test it is ported from and does not restate those. `date.HolidaySafeReferenceDataSpec`
 * owns the defaulting implementation, and the property-based sweeps over codecs and typeclass
 * laws live in their own specs at the root of the test tree.
 *
 * @see [[ReferenceData]] and [[ImmutableReferenceData]] for the types under test
 * @see [[ReferenceDataId]] for the identifiers a lookup is made with
 * @see [[CombinedReferenceData]] for the implementation `combinedWith` returns
 */
final class ReferenceDataSpec extends AnyFunSuite with Matchers {

  import GenericTestingReferenceDataId.count
  import GenericTestingReferenceDataId.text
  import ReferenceDataSpec.FallbackReferenceDataId
  import ReferenceDataSpec.ProbeResolvable
  import ReferenceDataSpec.ProbeResolvableTarget
  import ReferenceDataSpec.ProbeTarget
  import ReferenceDataSpec.RenderedReferenceDataId

  /** The identifier the first and the third hand-written fixture hold, under different values. */
  private val ID1: TestingReferenceDataId = TestingReferenceDataId("1")

  /** The identifier the second hand-written fixture holds. */
  private val ID2: TestingReferenceDataId = TestingReferenceDataId("2")

  /** The identifier no fixture holds, which is what establishes the behaviour of an absence. */
  private val ID3: TestingReferenceDataId = TestingReferenceDataId("3")

  /**
   * The value held under `ID1`, boxed as the identifier requires.
   *
   * [[TestingReferenceDataId]] refers to a `java.lang.Number`, so the identifier fixes the type
   * of the value it can be paired with and boxing at the point the value is written is what that
   * type costs. The Java test stored the integer `1` under `ID1`, and `Int.box` is that value,
   * written as the boxing it is rather than left to an implicit conversion.
   */
  private val VAL1: java.lang.Number = Int.box(1)

  /** The value held under `ID2`, boxed as [[VAL1]] is. */
  private val VAL2: java.lang.Number = Int.box(2)

  /** The value the clashing fixture holds under `ID1`, boxed as [[VAL1]] is. */
  private val VAL3: java.lang.Number = Int.box(3)

  /**
   * Reference data holding `ID1 -> VAL1`, and nothing else.
   *
   * The first of the four implementations described on this class. It overrides `findValue` and
   * inherits `containsValue`, `getValue` and `combinedWith`, which is what `test_defaultMethods`
   * asserts: implementing the one primitive keeps the other three correct.
   */
  private val REF_DATA1: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = findIn(id, ID1 -> VAL1)
  }

  /** Reference data holding `ID2 -> VAL2`, the fixture that clashes with none. */
  private val REF_DATA2: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = findIn(id, ID2 -> VAL2)
  }

  /**
   * Reference data holding `ID1 -> VAL3`, the fixture that clashes with [[REF_DATA1]].
   *
   * The clash is the whole point of it: `test_combinedWith_other_other_clash` combines it with
   * [[REF_DATA1]] and asserts that `ID1` answers with `VAL1`, which is an assertion about
   * precedence and would pass just as happily against any fixture if the two agreed.
   */
  private val REF_DATA3: ReferenceData = new ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = findIn(id, ID1 -> VAL3)
  }

  /** Reference data holding both `ID1 -> VAL1` and `ID2 -> VAL2`, agreeing with the first two. */
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

    // the one identifier that tells the two built-in sets apart: `GBLO` is a calendar of the
    // standard set and is not one of the four the minimal set holds
    test.containsValue(HolidayCalendarIds.GBLO) shouldBe false
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the caller-facing factory, and the one failure it can report.
   *
   * The original passed a `Map` of identifiers to values, in which two entries under one
   * identifier had already been resolved by insertion order. This factory takes
   * [[ReferenceData.Entry]] values instead, so the ambiguity survives long enough to be
   * reported, and the last group of assertions is that report: it is behaviour the port adds
   * rather than a case of the original, and `CombinedReferenceDataSpec` does not assert it.
   */
  test("test_of_RD") {
    val test: ReferenceData =
      layeredOverMinimal(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    // this factory seeds the four built-in weekend and no-holiday calendars underneath the
    // caller's entries, so a caller supplying securities does not have to remember to supply
    // those as well
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

    // the original asserted that this threw `ReferenceDataNotFoundException`; that type is not
    // ported, so an identifier the reference data does not hold is reported as a failure the
    // caller can act on
    test.containsValue(ID3) shouldBe false
    test.getValue(ID3) should beFailureWith(FailureReason.MISSING_DATA)
    test.findValue(ID3) shouldBe None

    // two entries under one identifier leave the caller's intent unknowable, so construction
    // fails rather than keeping one of the two values. The message names the duplicated
    // identifier and the attribute carries it, so a caller can act on it without reading text.
    val duplicated: Either[Failure, ReferenceData] =
      ReferenceData.of(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID1, VAL2))
    duplicated should beFailureWith(FailureReason.INVALID)
    duplicated should haveFailureMessageMatching(
      Regex.quote(s"Duplicate reference data identifiers: $ID1"))
    failureOf(duplicated).attributes.get("duplicateIds") shouldBe Some(ID1.toString)
  }

  /**
   * Asserts that the store factory holds exactly what it was given.
   *
   * This is the distinction the original drew between the two `of` methods and it is the reason
   * both exist: the four assertions that open this test are the same four that open `test_of_RD`,
   * with every answer reversed, because this factory adds no built-in calendars.
   */
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

  /**
   * Asserts the single-entry factory, which cannot fail and so returns the store directly.
   *
   * One entry cannot duplicate an identifier and the identifier fixes the type of its value, so
   * there is nothing to unwrap here - which is why this test builds its subject inline while
   * every other test of this suite goes through [[store]].
   */
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

    // it is documented as the identity of `combinedWith`, and it is one on both sides: combining
    // it with a set of reference data answers exactly as that set does, whichever operand it is
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    test.combinedWith(data).findValue(ID1) shouldBe Some(VAL1)
    data.combinedWith(test).findValue(ID1) shouldBe Some(VAL1)
    test.combinedWith(data).findValue(ID3) shouldBe None
    data.combinedWith(test).findValue(ID3) shouldBe None
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the public view of a store's content, which the Java `getValues()` property named.
   *
   * `findValue` answers one question about one identifier; this view is the mapping itself,
   * which is what a caller enumerating what it was given needs. It is asserted over each of
   * the four ways a store comes into being, because each assembles its content differently:
   * the entry-based factory holds exactly what it was handed, `ReferenceData.of` lays those
   * entries over the four built-in weekend and no-holiday calendars, [[ReferenceData.empty]]
   * holds nothing, and combining two stores merges their entries with the preferred side
   * winning a clash - so the view of a combination is where the precedence rule becomes
   * visible in the content rather than only in an answer.
   *
   * The view is also asserted to agree with the reading surface, which is what makes it a view
   * of the store and not a second store: an identifier it names is one the store holds, and
   * the value it maps to is the value `findValue` answers with.
   */
  test("test_values") {
    val test: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

    // exactly the entries supplied, keyed by the identifier each was filed under
    test.values shouldBe Map[ReferenceDataId[_], Any](ID1 -> VAL1, ID2 -> VAL2)
    test.values.get(ID3) shouldBe None

    // and it agrees with the three members of the reading surface for every identifier it names
    test.values.keys.foreach(id => test.containsValue(id) shouldBe true)
    test.values.get(ID1) shouldBe test.findValue(ID1)
    test.values.get(ID2) shouldBe test.findValue(ID2)

    // the empty store holds nothing to show, `ReferenceData.empty` being that store
    ImmutableReferenceData.empty.values shouldBe Map.empty[ReferenceDataId[_], Any]
    viewOf(ReferenceData.empty) shouldBe Map.empty[ReferenceDataId[_], Any]

    // `ReferenceData.of` lays the caller's entries over the four built-in calendars, so its
    // view is the minimal set's view plus the entry supplied - which also states, in content
    // rather than in an answer, that the four are seeded and that `GBLO` is not among them
    val minimalView: Map[ReferenceDataId[_], Any] = ReferenceData.minimal.values
    minimalView should have size 4
    viewOf(layeredOverMinimal(ReferenceData.Entry(ID1, VAL1))) shouldBe
      (minimalView + ((ID1: ReferenceDataId[_]) -> (VAL1: Any)))
    minimalView.get(HolidayCalendarIds.GBLO) shouldBe None

    // combining two stores merges their content, and the preferred side's value is the one the
    // view shows for an identifier both hold
    val merged: ReferenceData =
      store(ReferenceData.Entry(ID1, VAL1)).combinedWith(store(ReferenceData.Entry(ID2, VAL2)))
    viewOf(merged) shouldBe Map[ReferenceDataId[_], Any](ID1 -> VAL1, ID2 -> VAL2)

    val clashing: ReferenceData =
      store(ReferenceData.Entry(ID1, VAL1)).combinedWith(store(ReferenceData.Entry(ID1, VAL3)))
    viewOf(clashing) shouldBe Map[ReferenceDataId[_], Any](ID1 -> VAL1)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts at compile time the run-time type check the original asserted.
   *
   * The Java factory took a `Map` of type-erased identifiers to plain objects and validated each
   * pairing by asking the identifier for the `Class` of the data it referred to, throwing a
   * `ClassCastException` for a value of another type. [[ReferenceData.Entry]] moves that check to
   * compile time: an entry pairs an identifier only with a value of the type the identifier
   * promises, so a text value under an identifier of boxed numbers is not a program that runs
   * and fails - it is a program that does not compile, and the reference data store needs no
   * reflection to be sound. The valid call is asserted to compile alongside, so the proofs
   * cannot be passing for some unrelated reason.
   *
   * Dropping that `Class` token rather than porting it is AAP decision D-5 and its Rule 6, which
   * hold that nothing on the path that reads or writes data may reflect. The check the token
   * performed at construction time is not lost by dropping it: it is recovered by the type
   * parameter of [[ReferenceData.Entry]], and so is discharged by the compiler instead.
   */
  test("test_of_badType") {
    assertDoesNotCompile("""ReferenceData.Entry(ID1, "67")""")
    assertDoesNotCompile("""ImmutableReferenceData.of(ID1, "67")""")
    assertCompiles("""ReferenceData.Entry(ID1, VAL1)""")
    assertCompiles("""ImmutableReferenceData.of(ID1, VAL1)""")
  }

  /**
   * Records what replaced the absent-value check of the original.
   *
   * The Java factory rejected a `Map` entry whose value was an absent reference with an
   * `IllegalArgumentException`, and the port drops that `notNull` family of checks along with the
   * exceptions they raised: an absence is modelled by `Option`, and an `Option` is not a value
   * this store can be given - which is the first proof below. The second is that the value
   * cannot be omitted, an entry being the pair and not the identifier alone.
   *
   * One thing this test deliberately does not assert. A null literal still conforms to
   * `java.lang.Number`, so the compiler cannot refuse an entry spelled that way, and no run-time
   * guard replaces the one that was dropped. Supplying one is outside the contract of every
   * public entry point of this port - the same position the port takes for `CurrencyAmount.parse`
   * and the other text factories, recorded as such in `SCALA_MIGRATION.md`.
   */
  test("test_of_null") {
    assertDoesNotCompile("""ReferenceData.Entry(ID1, Option.empty[java.lang.Number])""")
    assertDoesNotCompile("""ReferenceData.Entry(ID1)""")
    assertDoesNotCompile("""ImmutableReferenceData.of(ID1)""")
    assertCompiles("""ReferenceData.Entry(ID1, VAL1)""")
  }

  /**
   * Asserts the half of the type-safety argument that closed construction cannot supply.
   *
   * `test_of_badType` above is about what can be filed, and the compiler settles it. This test
   * is about what can be '''found''', which the compiler cannot settle: a store is a map keyed
   * by an identifier whose type argument is erased, a map lookup compares keys with ordinary
   * `equals`, and `ReferenceDataId` is open - so a host may declare an identifier family
   * parameterized in its value type, and two instantiations of such a family are one key.
   * [[GenericTestingReferenceDataId]] is that family, and the two identifiers below are equal
   * values referring to different types of data.
   *
   * Without a retrieval check, the value filed by one instantiation would be returned through
   * the other at the other's type - a `String` handed back as an `Int` - and would fail in the
   * first expression the caller wrote with it, arbitrarily far from the lookup and on the
   * resolution path of every adjustment, schedule and observation in the library. The check is
   * `ReferenceDataId.valueType`: the identifier that asks narrows what was found with its own
   * witness, so the mismatch is reported as an absence.
   *
   * Every member of the reading surface is asserted, because each is a way a consumer reaches
   * that value, and the last two assertions record that filing is still settled at compile
   * time - the retrieval check is added to that guarantee and does not replace it.
   */
  test("test_findValue_genericIdFamily") {
    val textId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val countId: GenericTestingReferenceDataId[Int] =
      GenericTestingReferenceDataId[Int]("shared")

    // the two instantiations are one key: equality is the id alone, the type argument having
    // been erased, and a store keyed by identifier cannot tell them apart
    (textId: ReferenceDataId[_]) shouldBe (countId: ReferenceDataId[_])
    textId.hashCode shouldBe countId.hashCode

    // what does tell them apart is the witness each carries, which is why that member is
    // abstract on the trait: a generic family has to demand one per instantiation
    textId.valueType should not be countId.valueType

    val data: ImmutableReferenceData = store(ReferenceData.Entry(textId, "a value"))

    // the instantiation that filed the value finds it, through every member
    data.findValue(textId) shouldBe Some("a value")
    data.containsValue(textId) shouldBe true
    data.getValue(textId) should haveValue("a value")
    textId.resolve(data) should haveValue("a value")
    textId.toReader.run(data) should haveValue("a value")

    // the instantiation that did not is told the store holds nothing for it, rather than
    // handed a `String` at type `Int`
    data.findValue(countId) shouldBe None
    data.containsValue(countId) shouldBe false
    data.getValue(countId) should beFailureWith(FailureReason.MISSING_DATA)
    countId.resolve(data) should beFailureWith(FailureReason.MISSING_DATA)
    countId.toReader.run(data) should beFailureWith(FailureReason.MISSING_DATA)

    // and the consumer the mistyped value used to reach is reached with nothing: the miss is
    // data about the request, so this arithmetic is never performed and nothing is thrown
    data.findValue(countId).map(count => count + 1) shouldBe None

    // filing is unchanged and still settled by the compiler: the entry that would put a
    // `String` under the identifier of counts is not a program that runs
    assertDoesNotCompile("""ReferenceData.Entry(countId, "a value")""")
    assertCompiles("""ReferenceData.Entry(countId, 1)""")
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the three members defined in terms of the one primitive, over a hand-written
   * implementation that supplies the primitive alone.
   *
   * The last two assertions are the identifier-side entry point, which is where the original
   * called `id.queryValueOrNull(refData)`. That primitive is not ported: an identifier resolves
   * itself against reference data with `resolve`, which reports a missing value rather than
   * returning a reference to nothing.
   */
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
    // the two fixtures disagree about `ID1`, and the side asked first wins
    combined.getValue(ID1) should haveValue(VAL1)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the store-to-store merge, which is the case this implementation optimises.
   *
   * Combining two materialised stores returns a single merged store rather than a chain of
   * wrappers, so a lookup against the result stays one map lookup however many times reference
   * data has been combined. That is documented behaviour of
   * [[ImmutableReferenceData.combinedWith]], so the type of the result is asserted here and in
   * the two tests that follow.
   */
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
    // the entries of the store asked first are laid over the other's, so `ID1` answers with the
    // value held here and the merge cannot disagree with the general implementation about it
    combined.getValue(ID1) should haveValue(VAL1)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that a store combined with any other implementation takes the general path.
   *
   * There is no map to merge on the other side - it computes its answers - so the result is a
   * [[CombinedReferenceData]] consulting the two lazily, which is what makes an implementation
   * answering for identifiers no finite map could enumerate combinable at all.
   */
  test("test_combinedWith_IRD_other_noClash") {
    val test1: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    val test: ReferenceData = test1.combinedWith(REF_DATA2)
    test shouldBe a[CombinedReferenceData]
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the properties the two reflective sweeps of the original stood for.
   *
   * The Java test swept the immutable bean with `coverImmutableBean` - which read every property
   * and exercised equality, hashing and rendering - compared the instance with a second one
   * through `coverBeanEquals`, and then swept the private constructor of `StandardReferenceData`
   * with `coverPrivateConstructor` to record that the holder was not meant to be instantiated.
   * None of the three exists here: there is no bean, this port performs no reflection, and the
   * holder has no counterpart at all, its two constants being `ReferenceData.standard` and
   * `ReferenceData.minimal` - each computed once on first use, which is the property asserted in
   * place of the constructor sweep at the end of this test.
   *
   * The store publishes its content as [[ImmutableReferenceData.values]] - the counterpart of
   * the property the bean sweep would have read - and that view has a test of its own,
   * `test_values`, so the entries are read back here the way a caller ordinarily reads them,
   * through the three members of the reading surface. The two properties of an
   * [[ReferenceData.Entry]] are read back from the entry itself.
   */
  test("coverage") {
    val test: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val same: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val other: ImmutableReferenceData = store(ReferenceData.Entry(ID2, VAL2))

    // the entry read back through the whole reading surface, each member agreeing with the others
    test.containsValue(ID1) shouldBe true
    test.findValue(ID1) shouldBe Some(VAL1)
    test.getValue(ID1) should haveValue(VAL1)

    // equality is by value, so `same` - built from an entry constructed afresh - is one value
    // with `test` rather than a second name for one object, and equal values hash alike, which
    // is what filing a store in a map requires
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    test should not be other

    // the order entries were supplied in is not part of the value, the store being a map, so two
    // stores built from the same entries the other way round are one value and render alike
    val forward: ImmutableReferenceData =
      store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))
    val reversed: ImmutableReferenceData =
      store(ReferenceData.Entry(ID2, VAL2), ReferenceData.Entry(ID1, VAL1))
    forward shouldBe reversed
    forward.hashCode shouldBe reversed.hashCode
    forward.toString shouldBe reversed.toString

    // the rendering follows the bean rendering of the original, which named the property and
    // listed the entries, and the entries are ordered by identifier so the text is determined by
    // what the store holds and not by the iteration order of the underlying map
    test.toString shouldBe s"ImmutableReferenceData{values={$ID1=$VAL1}}"
    forward.toString shouldBe s"ImmutableReferenceData{values={$ID1=$VAL1, $ID2=$VAL2}}"

    // the entry type: both properties read back, and the two instances its companion publishes
    // behave. Both are given for every value type rather than built from instances for it,
    // because entries of differing value types are held together in one store.
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

    // in place of the private-constructor sweep: the two built-in sets are values computed once
    // on first use, so each access is the same instance rather than a set rebuilt per caller
    val standardFirst: ImmutableReferenceData = ReferenceData.standard
    val standardSecond: ImmutableReferenceData = ReferenceData.standard
    (standardFirst eq standardSecond) shouldBe true
    val minimalFirst: ImmutableReferenceData = ReferenceData.minimal
    val minimalSecond: ImmutableReferenceData = ReferenceData.minimal
    (minimalFirst eq minimalSecond) shouldBe true
  }

  /**
   * Asserts what replaced the Java-serialization round trip of the original.
   *
   * Serialization is absent here in both of the senses that could apply, and the two halves of
   * this test are those two senses:
   *
   *   - Java serialization is supported by no type of this port (AAP section 0.2.2), so there is
   *     nothing for a store to take part in;
   *   - reference data additionally has no JSON codec (AAP section 0.6.4), and that is a property
   *     of the type rather than an omission: a store maps a type-erased identifier to a value
   *     whose type is known only through that identifier, so no encoder for an arbitrary store
   *     could be written. That absence is asserted at compile time below, which is the only place
   *     it can be asserted at all.
   *
   * The second half is the nearest meaningful property: the value a store holds survives being
   * read back out through the public reading surface and handed to the factory again. The subject
   * here is a single store, deliberately, where `CombinedReferenceDataSpec.serialization` puts
   * the same cycle round a combination and so asserts that precedence survives it.
   */
  test("test_serialization") {
    val test: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    assertDoesNotCompile("""implicitly[io.circe.Encoder[ImmutableReferenceData]]""")
    assertDoesNotCompile("""implicitly[io.circe.Decoder[ImmutableReferenceData]]""")

    // the control the two proofs above need: the same summons, written the same way, resolve for
    // a type that does have a codec, so what they show is the absence of an instance for a store
    // and not a misspelled summon or an absent JSON library
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
  /**
   * Asserts `ReferenceDataId.resolve`, which no Java test class covers.
   *
   * The Java interface had the caller ask the reference data for a value, or ask the identifier
   * for a raw `queryValueOrNull`; neither is this method, and the Java test inventory therefore
   * has nothing to port here. Three things are asserted: that the default is exactly
   * `getValue`, so an identifier that is only an identity implements no member at all; that a
   * missing value is reported with the identifier in the message and under the `id` attribute,
   * which is what lets a caller act on the identifier without parsing text; and that a family
   * with a richer rule has its override honoured, which is the extension point
   * `HolidayCalendarId` uses for composite identifiers.
   */
  test("test_id_resolve") {
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    ID1.resolve(data) should haveValue(VAL1)
    ID1.resolve(data) shouldBe data.getValue(ID1)

    val missing: Either[Failure, java.lang.Number] = ID3.resolve(data)
    missing should beFailureWith(FailureReason.MISSING_DATA)
    missing should haveFailureMessageMatching(
      Regex.quote(s"Reference data not found for identifier '$ID3'"))
    failureOf(missing).attributes.get("id") shouldBe Some(ID3.toString)

    // the override is consulted in place of the default, so an identifier with a rule of its own
    // answers where the store alone could not
    val fallback: FallbackReferenceDataId = FallbackReferenceDataId("9", VAL3)
    fallback.resolve(ReferenceData.empty) should haveValue(VAL3)
    ReferenceData.empty.getValue(fallback) should beFailureWith(FailureReason.MISSING_DATA)

    // and it defers to the store where the store does hold the identifier, so the override
    // extends resolution rather than replacing it
    fallback.resolve(store(ReferenceData.Entry(fallback, VAL1))) should haveValue(VAL1)
  }

  /**
   * Asserts `ReferenceDataId.toReader`, which no Java test class covers.
   *
   * A reader is resolution expressed as a value awaiting its reference data - the
   * `RefDataReader` alias of this package - so several lookups compose while the data is still
   * unknown and the composition is run once against the data actually available. That is a type
   * the Java interface had no counterpart for, so the assertions below are of the port's own
   * making: the three ways `cats` composes two readers each supply one set of reference data to
   * both lookups, a reader that cannot be satisfied short-circuits with the missing identifier's
   * own message, and the override asserted in `test_id_resolve` is picked up here too, because
   * `toReader` is defined in terms of `resolve`.
   */
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

    // one failing lookup is the failure of the composition, and it carries the message of the
    // identifier that could not be found rather than a message about the composition
    val failing: RefDataReader[(java.lang.Number, java.lang.Number)] =
      (ID1.toReader, ID3.toReader).tupled
    val outcome: Either[Failure, (java.lang.Number, java.lang.Number)] = failing.run(data)
    outcome should beFailureWith(FailureReason.MISSING_DATA)
    outcome should haveFailureMessageMatching(
      Regex.quote(s"Reference data not found for identifier '$ID3'"))

    // the reader of an identifier with a resolution rule of its own resolves by that rule
    FallbackReferenceDataId("9", VAL3).toReader.run(ReferenceData.empty) should haveValue(VAL3)
  }

  /**
   * Asserts the [[Resolvable]] contract over a probe implementation, which no Java test covers.
   *
   * `Resolvable` is one of the forward-path types of this port: nothing inside this module
   * consumes one, and the types that will implement it - the trades, positions and products of
   * the modules migrated in later slices - do not exist yet. So the only way to assert the
   * contract now is to implement it, which is what [[ProbeResolvable]] does in the one method
   * the trait requires. The probe resolves to a `String` rather than to a resolved form of
   * itself, because the result type of the trait is free and a probe is the right place to show
   * it.
   */
  test("test_resolvable") {
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))

    ProbeResolvable(ID1).resolve(data) should haveValue("resolved 1")

    // resolution fails with the failure of the identifier it could not resolve, which is the
    // `Left(Failure.MissingData(...))` the trait documents in place of the Java
    // `throws ReferenceDataNotFoundException`
    ProbeResolvable(ID3).resolve(data) should beFailureWith(FailureReason.MISSING_DATA)

    // `toReader` is defined in terms of `resolve`, so the implementation above is picked up
    // without the probe overriding anything further, and it composes with any other reader
    val composed: RefDataReader[String] =
      (ProbeResolvable(ID1).toReader, ID1.toReader).mapN((text, value) =>
        s"$text/${value.intValue}")
    composed.run(data) shouldBe Right("resolved 1/1")
    ProbeResolvable(ID3).toReader.run(data) should beFailureWith(FailureReason.MISSING_DATA)
  }

  /**
   * Asserts the [[ResolvableCalculationTarget]] contract over a probe, which no Java test covers.
   *
   * The two resolution contracts are kept separate, as the Java interfaces keep them separate:
   * the method is named `resolveTarget`, it returns some [[CalculationTarget]] rather than a
   * resolved form of any particular type, and the trait is itself a `CalculationTarget`. All
   * three are asserted below - the last by holding the probe at that type, which is the only way
   * to state it - and, as with [[Resolvable]], a probe is the only implementation available until
   * the modules that carry trades and positions are migrated.
   */
  test("test_resolvableCalculationTarget") {
    val data: ImmutableReferenceData = store(ReferenceData.Entry(ID1, VAL1))
    val probe: ProbeResolvableTarget = ProbeResolvableTarget(ID1)

    val asTarget: CalculationTarget = probe
    asTarget shouldBe probe

    // the result is declared as a calculation target of some type, not as a resolved form of the
    // probe, which is the freedom that keeps this contract distinct from `Resolvable`
    val resolved: Either[Failure, CalculationTarget] = probe.resolveTarget(data)
    resolved should haveValue(ProbeTarget(VAL1))

    ProbeResolvableTarget(ID3).resolveTarget(data) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the missing-identifier failure over an identifier that renders as two lines.
   *
   * `ReferenceDataId` is an open contract and nothing in it constrains `toString`, so the
   * rendering below - which holds a line feed, a plausible second log line and a carriage
   * return - is a legal identifier that a host application could define, deliberately or by
   * accident. Two properties are asserted of it, and they are the two halves of the port's
   * message policy:
   *
   *   - the failure carries the rendering exactly, in its message and under its `id`
   *     attribute, because a caller acting on missing reference data has to be told which
   *     item to supply and an identifier rewritten on the way into the message would not
   *     tell it;
   *   - every text form of that failure - `Show[Failure]`, and `Failure.toString`, which is
   *     defined as it - is one line in which no character is a control character, so the
   *     second line the rendering asks for cannot appear in a log holding the failure
   *     (CWE-117).
   */
  test("test_notFound_injectedIdentifier") {
    val rendering: String = "InjectedId [id=1]\nMISSING_DATA: forged by the caller\r"
    val id: RenderedReferenceDataId = RenderedReferenceDataId(rendering)

    val missing: Either[Failure, java.lang.Number] = ReferenceData.empty.getValue(id)
    missing should beFailureWith(FailureReason.MISSING_DATA)

    val failure: Failure = failureOf(missing)
    failure.message shouldBe s"Reference data not found for identifier '$rendering'"
    failure.attributes.get("id") shouldBe Some(rendering)

    // the identifier-side entry point reports the same failure, `resolve` defaulting to
    // `getValue`, so the policy holds on both paths into the diagnostic
    id.resolve(ReferenceData.empty) shouldBe missing

    // the two characters `\n`, written here as the escape a reader sees rather than as the
    // character the identifier holds
    val escaped: String = "InjectedId [id=1]\\nMISSING_DATA: forged by the caller\\r"
    val rendered: String = Show[Failure].show(failure)
    rendered shouldBe
      s"MISSING_DATA: Reference data not found for identifier '$escaped' [id=$escaped]"
    failure.toString shouldBe rendered
    rendered.exists(_.isControl) shouldBe false
  }

  /**
   * Asserts the missing-identifier failure over an identifier that renders as ten thousand
   * characters.
   *
   * The failure keeps the whole rendering, which is what makes it a faithful report of what
   * was asked for, and the text form of the failure is bounded regardless: the message and
   * the attribute are each written up to a fixed number of characters and then marked as cut
   * short, so a log line holding this failure cannot be made large by the size of an
   * identifier. The ceiling asserted below is a concrete number well above what two bounded
   * parts and the fixed text around them come to, and well below the ten thousand characters
   * the failure holds, so the case pins the property rather than the constant the bound
   * happens to be set to.
   */
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

  /**
   * Asserts the duplicate-identifier failure over identifiers whose renderings are hostile.
   *
   * Two identifiers are duplicated, one rendering as two lines and one as ten thousand
   * characters, which is the case the port adds - the Java factory took a `Map` in which the
   * ambiguity had already been resolved - and the two properties asserted are again those of
   * `test_notFound_injectedIdentifier`, with the determinism of the report added: the failure
   * names every duplicated identifier exactly, ordered by its rendering, so the same set of
   * entries is the same failure however the caller arranged them, and the text form of that
   * failure stays one bounded line.
   *
   * The expected order is stated rather than computed from the values under test: `I` sorts
   * before `O`, so `InjectedId` precedes `OversizedId` and a sort that stopped happening
   * would be a failed assertion rather than an expectation that moved with it.
   */
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

    // the sort is what makes the failure a function of the set of entries: the same four
    // entries supplied the other way round are the same value, not a message in a new order
    val reversed: Either[Failure, ImmutableReferenceData] = ImmutableReferenceData.of(
      ReferenceData.Entry(second, VAL2),
      ReferenceData.Entry(second, VAL1),
      ReferenceData.Entry(first, VAL2),
      ReferenceData.Entry(first, VAL1))
    failureOf(reversed) shouldBe failure

    // and `ReferenceData.of`, being defined in terms of this factory, reports it unchanged
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
   * Finds an identifier among the entries a hand-written fixture holds.
   *
   * This is the body of all four hand-written fixtures above, written once, and it is written
   * the way [[ImmutableReferenceData.findValue]] is written: the matching entry is found by
   * equality, exactly as a map lookup finds it, and the value is then narrowed by the witness
   * the asking identifier carries. So it needs no cast - where the Java fixtures carried an
   * unchecked-cast suppression annotation on this very expression - and a fixture reports a
   * value of another type as absent for the same reason a store does.
   *
   * The entries are searched rather than looked up in a map, because a fixture holds one or two
   * of them and the search states the fixture's content at the point the fixture is declared.
   *
   * The two bound names are spelled `entryId` and `entryValue` rather than `key` and `value`,
   * which the matcher vocabulary this suite mixes in already binds in this scope.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier to find
   * @param entries  the entries the fixture holds, each a value under an identifier of its type
   * @return the value held for the identifier, empty if the fixture holds none
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
   * `ImmutableReferenceData.of` is used rather than `ReferenceData.of` because the latter layers
   * the caller's entries over the four built-in weekend calendars, and most tests of this suite
   * assert that an identifier is absent, which needs a store holding nothing it was not given.
   *
   * That factory reports the one way it can fail - two entries filed under the same identifier -
   * so this helper has an outcome to unwrap. A fixture that cannot be built is a defect in this
   * spec rather than a property of the subject, so it is reported as a failed test naming the
   * cause; the case itself is asserted, as a property of the subject, in `test_of_RD`.
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
   * Builds reference data the way an application does, through `ReferenceData.of`.
   *
   * The counterpart of [[store]]: this factory lays the entries given over the four built-in
   * weekend and no-holiday calendars, preferring the entries, where [[store]] yields a store
   * holding nothing it was not given. The difference between the two is the subject of the first
   * group of assertions in `test_of_RD`.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the entries laid over the minimal set of built-in calendars
   */
  private def layeredOverMinimal(entries: ReferenceData.Entry[_]*): ReferenceData =
    ReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }

  /**
   * Reads the public view of a set of reference data that is expected to be a store.
   *
   * `ImmutableReferenceData.values` is a member of the store, while `ReferenceData.of`,
   * `ReferenceData.empty` and `combinedWith` are all declared to return the trait - the last
   * of them because it returns a chain of wrappers when the other side is not a store. Every
   * subject `test_values` reads is a store all the same, so the narrowing is asserted here
   * rather than assumed: a subject that turned out to be anything else is a change in the
   * behaviour those three factories document, and is reported as a failed test naming what
   * was found instead.
   *
   * @param data  the reference data expected to be a materialised store
   * @return the public view of its content
   */
  private def viewOf(data: ReferenceData): Map[ReferenceDataId[_], Any] =
    data match {
      case immutable: ImmutableReferenceData => immutable.values
      case other => fail(s"Expected a materialised store but the reference data was $other")
    }

  /**
   * Reads the failure out of an outcome that is expected to have produced one.
   *
   * The matchers of the test kit assert the reason and the message of a failure, which is what
   * almost every assertion of this suite needs. The two tests that assert an '''attribute''' need
   * the failure itself, because an attribute is how a caller acts on a failure without parsing
   * its message, and that is the only reason this helper exists.
   *
   * @param outcome  the outcome expected to carry a failure
   * @return the failure it carries
   */
  private def failureOf(outcome: Either[Failure, Any]): Failure =
    outcome.fold(
      failure => failure,
      value => fail(s"Expected a failure but the outcome carried the value $value"))
}

/**
 * The fixtures of [[ReferenceDataSpec]] that have to be types rather than values.
 *
 * The four below are declared here, and not in the suite, for the reason the Java original
 * declared its fixtures as nested classes: a probe that exists only to be resolved has no place
 * in the package, where another suite could reach it and come to depend on it. They are case
 * classes in an object rather than in the suite because a case class nested in a class carries
 * a reference to the instance that declared it, which its generated equality cannot check at
 * run time - the pattern `CalculationTargetListSpec` established for the same reason.
 */
object ReferenceDataSpec {
  /**
   * An identifier whose resolution rule is richer than a single lookup.
   *
   * The counterpart of `HolidayCalendarId`, in miniature: it consults the reference data it is
   * given and falls back to a value of its own where the data holds nothing for it, so the
   * override is observable in both directions - it answers where `getValue` would have failed,
   * and it defers to the store where the store can answer. That is the extension point
   * `ReferenceDataId.resolve` exists for, and asserting it needs an identifier that uses it.
   *
   * It is a case class for the reason [[TestingReferenceDataId]] is one: a store is keyed by
   * identifier, so an identifier that inherited reference equality could never find the value
   * filed under an equal-but-not-identical instance.
   *
   * @param id  the identifier, which together with the fallback determines equality
   * @param fallback  the value resolution answers with when the reference data holds none
   */
  final case class FallbackReferenceDataId(id: String, fallback: java.lang.Number)
      extends ReferenceDataId[java.lang.Number] {

    /**
     * The witness by which reference data recognises a value this identifier may answer with.
     *
     * The same witness [[TestingReferenceDataId]] carries, this probe referring to the same
     * type of data: an identifier of a fixed value type names the one witness for that type
     * rather than declaring a second equal to it.
     *
     * @return the witness for a boxed number
     */
    override def valueType: ReferenceDataType[java.lang.Number] = TestingReferenceDataId.number

    /**
     * Resolves this identifier, falling back to the value it carries.
     *
     * @param refData  the reference data to resolve against
     * @return the value the data holds for this identifier, or this identifier's fallback
     */
    override def resolve(refData: ReferenceData): Either[Failure, java.lang.Number] =
      Right(refData.findValue(this).getOrElse(fallback))

    /**
     * Renders this identifier in the form the other test identifier uses.
     *
     * @return the identifier in the form `FallbackReferenceDataId [id=9]`
     */
    override def toString: String = s"FallbackReferenceDataId [id=$id]"
  }

  /**
   * An identifier that renders as exactly the text it was built with.
   *
   * [[TestingReferenceDataId]] renders itself in a fixed form, which is what the ported cases
   * of this suite want; the three diagnostic cases want the opposite - a rendering the test
   * chooses character for character, so that an identifier holding a line feed or ten
   * thousand characters can be handed to a lookup. `ReferenceDataId` is open and constrains
   * `toString` in no way, so an identifier of this shape is a legal one for a host to define
   * and is precisely the input those cases are about.
   *
   * It is a case class for the reason [[TestingReferenceDataId]] is one: reference data is
   * keyed by identifier, so an identifier with reference equality could not find the value
   * filed under an equal instance, and the duplicate-identifier case needs two equal
   * instances to be recognised as one key.
   *
   * @param rendering  the text this identifier renders as, and which alone determines equality
   */
  final case class RenderedReferenceDataId(rendering: String)
      extends ReferenceDataId[java.lang.Number] {

    /**
     * The witness by which reference data recognises a value this identifier may answer with.
     *
     * The one witness of the fixture family this identifier belongs to: it names a boxed number
     * and nothing else, so every instance answers with the same witness.
     *
     * @return the witness for a boxed number
     */
    override def valueType: ReferenceDataType[java.lang.Number] = TestingReferenceDataId.number

    /**
     * Renders this identifier as the text it was built with, unaltered.
     *
     * @return the rendering supplied at construction
     */
    override def toString: String = rendering
  }

  /**
   * A probe implementation of [[Resolvable]], resolving an identifier into text.
   *
   * One method long, which is what the trait's documentation says an implementation is: `resolve`
   * is the only member without a default, and `toReader` is defined in terms of it.
   *
   * @param id  the identifier this description holds instead of the value it describes
   */
  final case class ProbeResolvable(id: ReferenceDataId[java.lang.Number])
      extends Resolvable[String] {

    /**
     * Resolves this description into its resolved form.
     *
     * @param refData  the reference data to resolve against
     * @return the resolved text, or the failure naming the identifier that could not be found
     */
    override def resolve(refData: ReferenceData): Either[Failure, String] =
      id.resolve(refData).map(value => s"resolved ${value.intValue}")
  }

  /**
   * The resolved form [[ProbeResolvableTarget]] resolves to.
   *
   * A calculation target of a different type from the unresolved one, which is the freedom
   * [[ResolvableCalculationTarget]] is declared to allow.
   *
   * @param value  the value the target was resolved from
   */
  final case class ProbeTarget(value: java.lang.Number) extends CalculationTarget

  /**
   * A probe implementation of [[ResolvableCalculationTarget]], holding an identifier.
   *
   * It stands for the case the trait describes - a position whose security is referred to only by
   * identifier - and it is itself a [[CalculationTarget]], because the trait extends that marker.
   *
   * @param id  the identifier this target holds instead of the value it refers to
   */
  final case class ProbeResolvableTarget(id: ReferenceDataId[java.lang.Number])
      extends ResolvableCalculationTarget {

    /**
     * Resolves this target into an equivalent target holding the value.
     *
     * @param refData  the reference data to resolve against
     * @return the resolved target, or the failure naming the identifier that could not be found
     */
    override def resolveTarget(refData: ReferenceData): Either[Failure, CalculationTarget] =
      id.resolve(refData).map(value => ProbeTarget(value))
  }
}
