/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

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
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[FxIndexObservation]], ported from the Java `FxIndexObservationTest`.
 *
 * The original holds three test methods and so does this suite, under the names it gave them and
 * in the order it declared them, so that a Java test method and a test of this suite stay in
 * one-to-one correspondence in the migration manifest. Nothing is renamed, split or added.
 *
 * ===Two shapes of this type that a port gets wrong===
 *
 * An observation of an FX index differs from the observations of the other three index families
 * in two ways, and both are asserted here rather than assumed:
 *
 *   - it reports a currency pair and not a single currency, an exchange rate being a relation
 *     between two currencies rather than an amount in one, so [[FxIndexObservation.currencyPair]]
 *     is the accessor `test_of` reads - the accessor its three siblings offer does not exist on
 *     this type and reaching for it is the mistake this suite is written to catch;
 *   - its maturity date is derived from its '''fixing''' date, through
 *     [[FxIndex.calculateMaturityFromFixing]], where the Ibor and overnight observations route
 *     through an effective date instead.
 *
 * The expected maturity date is therefore obtained the way the original obtained it - by asking
 * the index - rather than frozen as a literal, so that this suite pins the derivation chain and
 * not one calendar's answer. The literal it does resolve to, 2016-02-24 for a fixing on
 * 2016-02-22, is asserted alongside it, so that a change of the chain is reported as a change of
 * the date rather than as two expectations moving together.
 *
 * ===Equality ignores the maturity date, so assertions cannot rely on it===
 *
 * Two observations are equal when their index and fixing date are equal; the maturity date takes
 * no part in equality or hashing, which is the documented behaviour of the bean being ported.
 * Every assertion here is written in that knowledge: `test_of` and `test_serialization` compare
 * `maturityDate` explicitly, because an assertion that compared whole observations - including
 * the round trip of `test_serialization` - would be satisfied without that field ever being
 * carried, and `coverage` states the design at the point where it is exercised.
 *
 * ===What the port changes, and why===
 *
 * Two of the three methods asserted machinery this port does not have, and each keeps its name
 * while asserting what replaced it, so nothing the Java test covered is dropped:
 *
 *   - `coverage` drove the Joda-Beans reflective sweeps `coverImmutableBean` and
 *     `coverBeanEquals`. Neither exists here, so what they stood for is asserted directly over
 *     the same two subjects the original used: the accessors, the rendering through `Show`, the
 *     agreement of hashing with equality, and the closed construction surface of a validated
 *     type.
 *   - `test_serialization` asserted Java serialization, which this port does not support. The
 *     JSON codec takes its place, and this suite asserts the concrete document of one
 *     observation in both directions together with the cross-check the decoder performs; the
 *     property-based sweep over every codec-bearing type belongs to `json/JsonRoundTripSpec`,
 *     which the migration manifest records as the consolidated home of this method.
 *
 * ===Failures are values here===
 *
 * Construction of an observation resolves the calendars of its index against reference data, and
 * that resolution can fail, so [[FxIndexObservation.of]] reports its outcome rather than
 * throwing. `test_of` therefore also asserts the failing side - reference data holding no
 * calendars - which is a behaviour the original could not express, its factory having thrown.
 * The outcome is asserted through the matchers of
 * [[com.opengamma.strata.collect.testkit.ResultMatchers]] with the reason compared as a member of
 * the closed family of reasons rather than as text, and nothing here catches an exception,
 * because nothing this suite calls throws one.
 *
 * @see [[FxIndexObservation]] for the type under test
 * @see [[FxIndexSpec]] for the closedness of the index family and for the cross-check of the
 *      observations an index builds when it is resolved once
 */
class FxIndexObservationSpec extends AnyFunSuite with Matchers {

  /** The reference data every fixture is resolved against, as in the Java test. */
  private val RefData: ReferenceData = ReferenceData.standard

  /** The fixing date of the Java test, a Monday and a business day of every calendar involved. */
  private val FixingDate: LocalDate = date(2016, 2, 22)

  /**
   * The maturity date the index derives from [[FixingDate]], obtained as the Java test obtained
   * it.
   *
   * The original called the index's own derivation rather than writing the date out, and this
   * does the same, so that the expectation follows the fixing calendar and the maturity offset of
   * the index instead of standing apart from them. The derivation reports its outcome, because a
   * calendar may be absent from the reference data, so it is unwrapped here - once, for the whole
   * suite - through [[required]].
   */
  private val MaturityDate: LocalDate =
    required("maturity date", FxIndices.GBP_USD_WM.calculateMaturityFromFixing(FixingDate, RefData))

  /**
   * The expected JSON of the observation of the Java test.
   *
   * Three fields under the three property names, in their declaration order: the index as a bare
   * string holding its canonical name, which contains a slash, and the two dates in their ISO
   * forms. This literal is parsed before it is used and the comparison is made between documents,
   * so what it pins is the fields present, their names and their decoded values - not how the
   * printed text spells them, a decoded string being the same whether or not its slash arrived
   * escaped.
   */
  private val ExpectedJson: String =
    """{"index":"GBP/USD-WM","fixingDate":"2016-02-22","maturityDate":"2016-02-24"}"""

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome that is expected to hold one.
   *
   * The factories of this type report a rejection as a value, so a fixture built through one of
   * them is an outcome rather than the value wanted. This is the one place this suite turns the
   * first into the second, and it handles both sides: a fixture that cannot be built is a defect
   * in this suite and is reported as one, naming what was being built and the failure that
   * prevented it, rather than raising an error from a partial accessor that would name nothing.
   *
   * @param what  the fixture being built, named for the diagnostic
   * @param result  the outcome of a factory, expected to hold a value
   * @tparam A  the type of the value
   * @return the value the outcome holds
   */
  private def required[A](what: String, result: FailureOr[A]): A =
    result.fold(
      failure => fail(s"the $what fixture of this spec could not be built: ${failure.message}"),
      value => value)

  /**
   * Parses the expected JSON of this suite into the JSON model.
   *
   * Comparing documents rather than printed text is what makes an assertion about the encoding
   * rather than about its rendering: whitespace and the spelling of a value are matters of
   * presentation, while the fields present, their names and their values are the contract. A
   * literal in this file that does not parse is a defect in the suite itself.
   *
   * Both sides are handled, as in [[required]] and for the same reason: an outcome is read by
   * deciding what each of its two cases means, never by a partial accessor that would report an
   * unreadable literal as an error naming nothing.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).fold(
      error =>
        fail(s"the expected JSON of this spec is not itself valid JSON: $text (${error.getMessage})"),
      document => document)

  //-------------------------------------------------------------------------
  test("test_of") {
    // The factory reports its outcome, because the fixing calendar of the index has to be
    // resolved against the reference data supplied; the standard set holds it, so this succeeds.
    val result: FailureOr[FxIndexObservation] =
      FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, RefData)
    result should beSuccess

    val test: FxIndexObservation = required("observation", result)
    test.index shouldBe FxIndices.GBP_USD_WM
    test.fixingDate shouldBe FixingDate

    // Equality ignores the maturity date, so this comparison is the only thing pinning that
    // field: comparing two observations with `==` would not catch a wrong maturity date. It is
    // asserted both against the index's own derivation, as the Java test did, and against the
    // date that derivation resolves to, so that a change in either is reported.
    test.maturityDate shouldBe MaturityDate
    test.maturityDate shouldBe date(2016, 2, 24)

    // The accessor of this observation is the currency pair of its index, and its static type is
    // the pair type - the assertion below would not compile otherwise. An FX index quotes a rate
    // between two currencies and holds no single one of them, which is why the three sibling
    // observations report something this one cannot, and why the pair is read through this
    // accessor rather than through the one they offer.
    val pair = test.currencyPair
    pair shouldBe FxIndices.GBP_USD_WM.currencyPair
    pair.base.toString shouldBe "GBP"
    pair.counter.toString shouldBe "USD"
    pair.toString shouldBe "GBP/USD"

    // The rendering of the bean being ported, character for character, slash included.
    test.toString shouldBe "FxIndexObservation[GBP/USD-WM on 2016-02-22]"

    //-----------------------------------------------------------------------
    // The failing side, which the Java test could not express because its factory threw: an
    // observation cannot be built against reference data that resolves no calendar. The index
    // names `USNY` as its fixing calendar and counts its maturity offset in the composite
    // `GBLO+USNY`; a composite identifier resolves part by part, so a store holding nothing
    // fails on the parts as well, and the first calendar reached - the simple fixing calendar -
    // is the one reported.
    val missing: FailureOr[FxIndexObservation] =
      FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, ReferenceData.empty)
    missing should beFailureWith(FailureReason.MISSING_DATA)

    // Which calendar was missing is carried as an attribute rather than only described in the
    // message, so it is asserted as a value; the message itself is deliberately not matched.
    missing.swap.toOption.flatMap(failure => failure.attributes.get("id")) shouldBe Some("USNY")
  }

  //-------------------------------------------------------------------------
  test("test_resolve") {
    // The batch route into the type, which the index being ported published as `resolve` and
    // which this port publishes on both the type and the index: the fixing calendar and the
    // maturity offset are resolved once and the function that comes back observes any fixing
    // without consulting reference data again. Equality of an observation ignores the maturity
    // date, so each resolved value is compared to the per-fixing factory's value field by field
    // as well - a maturity date derived wrongly by this route would otherwise be invisible.
    val index = FxIndices.GBP_USD_WM
    val observe = required("resolved observation", FxIndexObservation.resolve(index, RefData))
    val observeFromIndex = required("resolved observation", index.resolve(RefData))

    List(FixingDate, FixingDate.plusDays(1L), FixingDate.plusDays(4L), FixingDate.plusMonths(2L))
      .foreach { fixingDate =>
        withClue(s"$fixingDate: ") {
          val direct = required("observation", FxIndexObservation.of(index, fixingDate, RefData))
          List(observe(fixingDate), observeFromIndex(fixingDate)).foreach { resolved =>
            resolved shouldBe direct
            resolved.index shouldBe index
            resolved.fixingDate shouldBe fixingDate
            resolved.maturityDate shouldBe direct.maturityDate
            resolved.maturityDate shouldBe
              required("maturity date", index.calculateMaturityFromFixing(fixingDate, RefData))
          }
        }
      }

    // A fixing date that is not a fixing date of the index is carried as given while the maturity
    // date follows from the fixing date the index would use, exactly as the per-fixing factory
    // treats it.
    val saturday = FixingDate.`with`(java.time.DayOfWeek.SATURDAY)
    saturday.getDayOfWeek shouldBe java.time.DayOfWeek.SATURDAY
    observe(saturday).fixingDate shouldBe saturday
    observe(saturday).maturityDate shouldBe
      required("maturity date", index.calculateMaturityFromFixing(saturday, RefData))

    // Reference data that cannot supply a calendar is reported once, by the resolution, rather
    // than per fixing - which is the reason a caller observing a series reaches for it.
    FxIndexObservation.resolve(index, ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
    index.resolve(ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java sweeps walked the properties of a bean through its meta-bean and compared one
    // bean with another. There is no meta-bean here, so what they stood for is asserted directly
    // over the two subjects the original used: an observation of the sterling/dollar rate on the
    // fixing date, and one of the euro/sterling rate on the day after it.
    val result: FailureOr[FxIndexObservation] =
      FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, RefData)
    val otherResult: FailureOr[FxIndexObservation] =
      FxIndexObservation.of(FxIndices.EUR_GBP_ECB, FixingDate.plusDays(1), RefData)
    result should beSuccess
    otherResult should beSuccess

    val test: FxIndexObservation = required("observation", result)
    val test2: FxIndexObservation = required("second observation", otherResult)

    // Every field goes in and comes back, for both subjects, the maturity date of each following
    // from its own index and fixing date.
    test.index shouldBe FxIndices.GBP_USD_WM
    test.fixingDate shouldBe FixingDate
    test.maturityDate shouldBe date(2016, 2, 24)
    test2.index shouldBe FxIndices.EUR_GBP_ECB
    test2.fixingDate shouldBe date(2016, 2, 23)
    test2.maturityDate shouldBe date(2016, 2, 25)

    // Two observations that differ in both index and fixing date are not equal. `Hash` is the
    // type's single equality-bearing instance, from which `Eq` is obtained by subtyping, and it
    // reports the same relation as platform equality does. The hash contract runs one way - equal
    // observations share a hash, which is asserted further down on two observations that are
    // equal - so the differing hash codes of these two fixtures are an observation about them and
    // not a requirement: unequal values are free to collide.
    test should not be test2
    test.hashCode should not be test2.hashCode
    Hash[FxIndexObservation].eqv(test, test2) shouldBe false
    Hash[FxIndexObservation].hash(test) shouldBe test.hashCode
    Hash[FxIndexObservation].hash(test2) shouldBe test2.hashCode

    // `Show` renders what `toString` renders, so the two ways of putting an observation into a
    // message agree, and both renderings are pinned as literals.
    Show[FxIndexObservation].show(test) shouldBe test.toString
    Show[FxIndexObservation].show(test2) shouldBe test2.toString
    test.toString shouldBe "FxIndexObservation[GBP/USD-WM on 2016-02-22]"
    test2.toString shouldBe "FxIndexObservation[EUR/GBP-ECB on 2016-02-23]"

    // The relation only this type's equality makes meaningful: two observations of the same index
    // and fixing date are equal and share a hash. The maturity date is excluded from equality and
    // from hashing by design, matching the bean being ported - it is a function of the other two
    // fields, so observations agreeing on those agree on it as well, and an assertion that
    // compared whole observations would never notice the field at all. That is why the maturity
    // date of the observation built again below is compared on its own, and why the round trip of
    // `test_serialization` compares it explicitly rather than relying on equality.
    val again: FxIndexObservation =
      required("repeated observation", FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, RefData))
    test shouldBe again
    again.hashCode shouldBe test.hashCode
    Hash[FxIndexObservation].eqv(test, again) shouldBe true
    again.maturityDate shouldBe test.maturityDate

    // Reading the three fields by pattern is unaffected by the closed construction surface -
    // `unapply` is retained - which is what keeps a ported call site that matched on the bean
    // readable.
    val destructured: (FxIndex, LocalDate, LocalDate) = test match {
      case FxIndexObservation(index, fixingDate, maturityDate) => (index, fixingDate, maturityDate)
    }
    destructured shouldBe ((FxIndices.GBP_USD_WM, FixingDate, MaturityDate))

    // A validated value has no copier: the primary constructor is not public and no `copy` is
    // generated, so the only way to an observation is a factory that derives the maturity date.
    // A copier would let a caller pair a fixing date with a maturity date that does not follow
    // from it, which is precisely the state this type exists to rule out. The claim is proved by
    // compiling a snippet and requiring it to fail; the wider proofs about this and the other
    // validated types - that no public `apply` exists either - belong to the module's
    // `ApiSurfaceSpec`.
    assertDoesNotCompile(
      """FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, RefData)
        |  .map(_.copy(fixingDate = FixingDate.plusDays(1)))""".stripMargin)

    // The same snippet with the absent member replaced by one that exists, which is what keeps
    // the assertion above from passing for the wrong reason: a snippet naming an identifier this
    // scope cannot resolve would also fail to compile and would prove nothing. This compiles, so
    // every other name in it resolves and the only difference is the member reached for.
    assertCompiles(
      """FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, RefData).map(_.fixingDate)""")
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not part of this port; the JSON codec takes its place. Both
    // directions are asserted, because an encoding that is wrong and a decoding that is wrong in
    // the same way would still round trip, and the documents are compared as parsed JSON rather
    // than as printed text.
    val test: FxIndexObservation =
      required("observation", FxIndexObservation.of(FxIndices.GBP_USD_WM, FixingDate, RefData))

    test.asJson shouldBe json(ExpectedJson)

    // The three fields of the bean being ported, under their own names and in their declaration
    // order, with no field added and none dropped.
    test.asJson.asObject.map(_.keys.toList) shouldBe
      Some(List("index", "fixingDate", "maturityDate"))

    // The index rides its family's codec and is written as a bare string, not as an object: the
    // value read back is its canonical name, slash included. Both dates are read back as their
    // ISO forms. These are decoded values, so they say nothing about how the printed document
    // spells them.
    test.asJson.hcursor.downField("index").as[String] shouldBe Right("GBP/USD-WM")
    test.asJson.hcursor.downField("fixingDate").as[String] shouldBe Right("2016-02-22")
    test.asJson.hcursor.downField("maturityDate").as[String] shouldBe Right("2016-02-24")

    // The round trip, from the encoded document and from the literal above alike.
    val decoded: Either[io.circe.Error, FxIndexObservation] =
      decode[FxIndexObservation](test.asJson.noSpaces)
    decoded shouldBe Right(test)
    decode[FxIndexObservation](ExpectedJson) shouldBe Right(test)

    // Equality ignores the maturity date, so the two assertions above would hold of a codec that
    // never carried that field. These compare it explicitly, which is what makes the round trip
    // of this type mean what it says; the index is compared as a value for the same reason, so
    // that a name whose slash had been mangled on the way out or in would be reported.
    decoded.map(_.maturityDate) shouldBe Right(MaturityDate)
    decoded.map(_.maturityDate) shouldBe Right(date(2016, 2, 24))
    decoded.map(_.index) shouldBe Right(FxIndices.GBP_USD_WM)
    decoded.map(_.index.name) shouldBe Right("GBP/USD-WM")

    // The decoder derives the maturity date from the index and the fixing date it reads, then
    // checks the date the document declared against it, so a document declaring any other date is
    // rejected rather than decoded into a value whose dates disagree. The check is needed
    // precisely because the equality of this type would not notice such a value, and the
    // rejection names both dates.
    val mismatch: Either[io.circe.Error, FxIndexObservation] = decode[FxIndexObservation](
      """{"index":"GBP/USD-WM","fixingDate":"2016-02-22","maturityDate":"2016-02-25"}""")
    mismatch.isLeft shouldBe true

    val reported: String = mismatch.swap.toOption.fold("")(error => error.getMessage)
    reported should include("2016-02-24")
    reported should include("2016-02-25")
  }
}
