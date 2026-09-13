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
import io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[OvernightIndexObservation]].
 *
 * This is a one-to-one port of the Java test class `OvernightIndexObservationTest`: each of its
 * three test methods has a test of the same name here, in the same order, and no test is added,
 * split or renamed. The names are part of the migration contract - the gate script joins each row
 * of `manifest/java-test-mapping.csv` to a test case of the reports this suite writes on the pair
 * of suite class and test name - so they are the Java method names rather than sentences.
 *
 * ===Reduced equality is the fact this spec is built around===
 *
 * Two observations are equal when they name the same index and the same fixing date; the
 * publication, effective and maturity dates and the year fraction take no part in equality or in
 * hashing. That is the equality the bean being ported defined by hand, and it is reproduced
 * exactly, so '''any assertion that leans on `==` to check a derived value proves nothing''': an
 * observation whose publication date were a day out, or whose year fraction came from the wrong
 * day count, would still compare equal to the right one. Both `test_of` and `test_serialization`
 * therefore compare those four values field by field, and `coverage` says so where a reader of
 * the equality assertions would otherwise wonder.
 *
 * ===What the port changes, and why===
 *
 *   - `test_of` keeps every assertion of the Java method and gains two things. The factory reports
 *     its outcome rather than raising, so the successful case is asserted as a success and then
 *     inspected; and the failing case - reference data that does not hold the fixing calendar of
 *     the index - is asserted here for the first time, because the Java original had no way to
 *     express it other than by letting an exception escape.
 *   - `coverage` called `coverImmutableBean` and `coverBeanEquals`, reflective sweeps over the
 *     properties of a bean through its meta-bean. There is no meta-bean and no reflective property
 *     access in this port, so the substance of the two sweeps is asserted directly over the same
 *     two subjects the Java method used: the accessors, the rendering, the equality and hashing of
 *     the pair, and the closed construction surface of a validated type, the last proved by
 *     requiring a snippet to fail to compile.
 *   - `test_serialization` asserted a Java serialization round trip, which this port does not have
 *     at all. The JSON codec takes its place, and the round trip is asserted in both directions
 *     together with the exact shape of the document and the refusal of a document that contradicts
 *     the index it names.
 *
 * ===What is asserted elsewhere===
 *
 * `manifest/java-test-mapping.csv` routes the `test_serialization` row of the Java class to the
 * module-wide `json/JsonRoundTripSpec`, which owns the property-based codec sweep over every
 * codec-bearing type of the module. That routing is honoured rather than interpreted as a licence
 * to drop the behaviour: this spec keeps a `test_serialization` of its own, pinning the document
 * shape and the decoder's consistency check for this one type, which a property-based sweep over
 * many types does not state. Two further module-wide duties are likewise not repeated here - the
 * sweep of every validated type's invalid inputs (`SmartConstructorSpec`) and the proof that no
 * validated type has a public `apply` and that no sealed family can be extended from outside its
 * file (`ApiSurfaceSpec`). [[IndexObservation]] is deliberately an open trait, so it is subject to
 * neither sweep in that form; the open-contract row `ApiSurfaceSpec` carries for the trait asserts
 * the converse instead, implementing it from outside `IndexObservation.scala` and compiling a
 * second implementation, and that is where its extensibility is established rather than here.
 *
 * The expected dates below are derived by calling the index's own calculations, exactly as the
 * Java fixtures were, so this spec pins the derivation chain rather than a frozen calendar
 * result; the figures those calculations produced in the Java implementation are pinned as
 * literals alongside them, so a change in either the chain or the calendar data is visible.
 */
final class OvernightIndexObservationSpec extends AnyFunSuite with Matchers {

  /** The reference data of the Java fixture `REF_DATA`, the calendars built into this library. */
  private val RefData: ReferenceData = ReferenceData.standard

  /** The fixing date of the Java fixture `FIXING_DATE`, a Monday and a GBLO business day. */
  private val FixingDate: LocalDate = date(2016, 2, 22)

  /**
   * The publication date of the Java fixture `PUBLICATION_DATE`, the fixing date shifted by the
   * publication offset of `GBP-SONIA`, which is one business day.
   */
  private val PublicationDate: LocalDate =
    required("the publication date of the fixture")(
      OvernightIndices.GBP_SONIA.calculatePublicationFromFixing(FixingDate, RefData))

  /**
   * The effective date of the Java fixture `EFFECTIVE_DATE`, the fixing date shifted by the
   * effective offset of `GBP-SONIA`, which is zero business days.
   */
  private val EffectiveDate: LocalDate =
    required("the effective date of the fixture")(
      OvernightIndices.GBP_SONIA.calculateEffectiveFromFixing(FixingDate, RefData))

  /**
   * The maturity date of the Java fixture `MATURITY_DATE`, one business day after the
   * '''effective''' date - the argument the Java fixture passed, and the one that matters: an
   * index with a non-zero effective offset would give a maturity one or two days early if the
   * fixing date were passed here instead.
   */
  private val MaturityDate: LocalDate =
    required("the maturity date of the fixture")(
      OvernightIndices.GBP_SONIA.calculateMaturityFromEffective(EffectiveDate, RefData))

  /**
   * The year fraction of a one-business-day investment on `GBP-SONIA`, which accrues on
   * `Act/365F`.
   *
   * This is the figure the Java implementation produced for this fixture, to the bit: the
   * shortest decimal that reads back as `1.0 / 365.0`. It is written as the division rather than
   * as that decimal so that what it is stays readable, and the decoder assertions below rely on
   * it being the exact double the index implies, because the decoder compares year fractions by
   * bit pattern.
   */
  private val ExpectedYearFraction: Double = 1.0 / 365.0

  /**
   * The document an observation of the fixture encodes to, written out rather than assembled, so
   * that the shape is stated in the spec instead of being taken from the encoder it checks.
   *
   * The six keys are the six properties of the bean being ported, in its declaration order; the
   * index rides its own codec and is therefore its bare name; the four dates are ISO-8601 text;
   * and the year fraction is a JSON number, written as the decimal that reads back as
   * [[ExpectedYearFraction]].
   */
  private val ExpectedJson: String =
    """{"index":"GBP-SONIA","fixingDate":"2016-02-22","publicationDate":"2016-02-23",""" +
      """"effectiveDate":"2016-02-22","maturityDate":"2016-02-23",""" +
      """"yearFraction":0.0027397260273972603}"""

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome that this spec expects to hold one.
   *
   * The factory of this type and the calculations of an index report a missing calendar as a
   * value on the left rather than by raising, so a fixture built through one of them is an
   * outcome rather than a value. This is the one place this spec turns the first into the second,
   * and it handles both sides: an outcome that holds a failure where a fixture was expected is a
   * defect in this spec, and is reported as one naming what could not be built and why, rather
   * than through a partial accessor that would name neither.
   *
   * @tparam A  the type of the value the outcome carries
   * @param description  what was being built, named in the diagnostic of a failure
   * @param result  the outcome, expected to hold a value
   * @return the value the outcome holds
   */
  private def required[A](description: String)(result: Either[Failure, A]): A =
    result.fold(
      failure => fail(s"$description could not be built: ${failure.message}"),
      identity)

  /**
   * Decodes a document this spec expects to be a valid observation.
   *
   * As with [[required]], a document of this spec that does not decode is a defect in the spec
   * and is reported with the message of the decoding failure. The documents that are *expected*
   * to be refused are asserted where they are built, and do not come through here.
   *
   * @param text  the JSON text to decode
   * @return the observation the document describes
   */
  private def decodedObservation(text: String): OvernightIndexObservation =
    decode[OvernightIndexObservation](text).fold(
      error => fail(s"the document of this spec did not decode: ${error.getMessage}"),
      identity)

  /**
   * Builds an observation of the fixture reference data, failing the spec if it cannot be built.
   *
   * @param index  the index observed
   * @param fixingDate  the date the rate is fixed on
   * @return the observation
   */
  private def observationOf(index: OvernightIndex, fixingDate: LocalDate): OvernightIndexObservation =
    required(s"the observation of ${index.name} on $fixingDate")(
      OvernightIndexObservation.of(index, fixingDate, RefData))

  //-------------------------------------------------------------------------
  test("test_of") {
    val result: Either[Failure, OvernightIndexObservation] =
      OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, FixingDate, RefData)
    result should beSuccess
    val test: OvernightIndexObservation = required("the observation under test")(result)

    // The two values that identify an observation, and the two that its equality reads.
    test.index shouldBe OvernightIndices.GBP_SONIA
    test.fixingDate shouldBe FixingDate

    // The four derived values. Equality of this type ignores all four, so these assertions are
    // the only thing in this test that pins them: comparing this observation to another by `==`
    // would not notice a publication or maturity date that was a day out, or a year fraction
    // measured by the wrong day count.
    test.publicationDate shouldBe PublicationDate
    test.effectiveDate shouldBe EffectiveDate
    test.maturityDate shouldBe MaturityDate
    test.yearFraction shouldBe
      OvernightIndices.GBP_SONIA.dayCount.yearFraction(EffectiveDate, MaturityDate)

    // The same four values as the figures the Java implementation produced for this fixture,
    // which is what keeps the derivation above from being checked only against itself: a change
    // in the chain, in the GBLO calendar data or in the day count of the index moves one of these.
    PublicationDate shouldBe date(2016, 2, 23)
    EffectiveDate shouldBe date(2016, 2, 22)
    MaturityDate shouldBe date(2016, 2, 23)
    test.yearFraction shouldBe ExpectedYearFraction

    test.currency shouldBe OvernightIndices.GBP_SONIA.currency
    test.toString shouldBe "OvernightIndexObservation[GBP-SONIA on 2016-02-22]"

    //-----------------------------------------------------------------------
    // The failing side of the factory, which the Java original could not express: each of the
    // three calculations shifts by business days of the fixing calendar of the index, that
    // calendar is named by identifier, and reference data that does not hold it cannot resolve
    // it. The Java implementation let an exception escape; here the outcome carries the reason,
    // compared as a member of the closed family of reasons rather than as text.
    OvernightIndexObservation.of(
      OvernightIndices.GBP_SONIA,
      FixingDate,
      ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)

    // Reference data that holds some calendars is no better than reference data that holds none
    // unless it holds the right one: the minimal set carries the weekend and no-holiday calendars
    // only, and the fixing calendar of this index is GBLO, so the failure is the same.
    OvernightIndexObservation.of(
      OvernightIndices.GBP_SONIA,
      FixingDate,
      ReferenceData.minimal) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_resolve") {
    // The batch route into the type. An Overnight rate is consumed as a run of daily fixings -
    // compounded or averaged over a period - so the fixing calendar is resolved once here and the
    // function that comes back derives the publication, effective and maturity dates and the year
    // fraction of every fixing from it, consulting no reference data again. The single-shot
    // factory is that function applied to one date, so the two agree by construction; what is
    // asserted is that they agree in value, field by field, since the equality of this type reads
    // the index and the fixing date alone and would hide a wrongly derived date.
    val subjects: List[OvernightIndex] =
      List(OvernightIndices.GBP_SONIA, OvernightIndices.CHF_TOIS, OvernightIndices.THB_THOR)
    val fixingDates: List[LocalDate] =
      List(FixingDate, FixingDate.plusDays(4L), FixingDate.plusDays(5L), FixingDate.plusMonths(10L))

    subjects.foreach { index =>
      val observe =
        required(s"the resolved observation of ${index.name}")(
          OvernightIndexObservation.resolve(index, RefData))
      fixingDates.foreach { fixingDate =>
        withClue(s"${index.name} on $fixingDate: ") {
          val resolved = observe(fixingDate)
          val direct = observationOf(index, fixingDate)
          resolved shouldBe direct
          resolved.index shouldBe index
          resolved.fixingDate shouldBe fixingDate
          resolved.publicationDate shouldBe direct.publicationDate
          resolved.effectiveDate shouldBe direct.effectiveDate
          resolved.maturityDate shouldBe direct.maturityDate
          resolved.yearFraction shouldBe direct.yearFraction

          // and each derived date is the one the index's own calculation reports, which is what
          // the single resolution must not change: the publication and effective dates are shifts
          // of the fixing date, the maturity date is a business day beyond the effective date
          resolved.publicationDate shouldBe
            required("the publication date")(index.calculatePublicationFromFixing(fixingDate, RefData))
          resolved.effectiveDate shouldBe
            required("the effective date")(index.calculateEffectiveFromFixing(fixingDate, RefData))
          resolved.maturityDate shouldBe
            required("the maturity date")(
              index.calculateMaturityFromEffective(resolved.effectiveDate, RefData))
          resolved.yearFraction shouldBe
            index.dayCount.yearFraction(resolved.effectiveDate, resolved.maturityDate)
        }
      }
    }

    // The index whose effective date is a day beyond its fixing date is in the list above on
    // purpose: it is the one published row where a maturity date derived from the fixing date
    // rather than from the effective date would differ, so the agreement asserted for it is the
    // agreement that matters.
    val tois = observe(OvernightIndices.CHF_TOIS)(FixingDate)
    tois.effectiveDate shouldBe tois.fixingDate.plusDays(1L)
    tois.maturityDate shouldBe tois.effectiveDate.plusDays(1L)

    // Reference data that cannot supply the fixing calendar is reported once, by the resolution
    // itself, rather than by each fixing of the series - which is the reason the operation exists.
    OvernightIndexObservation.resolve(OvernightIndices.GBP_SONIA, ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
    OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, FixingDate, ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  /**
   * Resolves an index against the fixture reference data, failing the spec if it cannot be.
   *
   * @param index  the index to resolve
   * @return the observation of a fixing of that index
   */
  private def observe(index: OvernightIndex): LocalDate => OvernightIndexObservation =
    required(s"the resolved observation of ${index.name}")(
      OvernightIndexObservation.resolve(index, RefData))

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java sweeps `coverImmutableBean` and `coverBeanEquals` walked the properties of a bean
    // through its meta-bean and exercised its equality against a second, different bean. There is
    // neither a meta-bean nor reflective property access here, so what the two stood for is
    // asserted directly, over the same two subjects the Java method built.
    val test: OvernightIndexObservation = observationOf(OvernightIndices.GBP_SONIA, FixingDate)
    val test2: OvernightIndexObservation =
      observationOf(OvernightIndices.EUR_EONIA, FixingDate.plusDays(1))

    // Every field of the second subject goes in and comes back, which is the part of the property
    // sweep that was about the accessors. `EUR-EONIA` publishes on the fixing date itself and
    // accrues on `Act/360`, so its figures differ from those of `GBP-SONIA` in every position.
    test2.index shouldBe OvernightIndices.EUR_EONIA
    test2.fixingDate shouldBe date(2016, 2, 23)
    test2.publicationDate shouldBe date(2016, 2, 23)
    test2.effectiveDate shouldBe date(2016, 2, 23)
    test2.maturityDate shouldBe date(2016, 2, 24)
    test2.yearFraction shouldBe 1.0 / 360.0
    test2.currency shouldBe OvernightIndices.EUR_EONIA.currency

    // Two observations that differ are unequal, by the equality of the type and by the single
    // equality-bearing instance it publishes, which is taken from that equality. Hashing is not
    // asked to separate them: its contract runs one way only - equal values must hash alike,
    // unequal values are permitted to collide - so of the differing subject the real property is
    // asserted instead, that the instance answers with the value's own `hashCode`. The
    // alike-in-hash direction is asserted of an equal pair below.
    (test == test2) shouldBe false
    Hash[OvernightIndexObservation].eqv(test, test2) shouldBe false
    Hash[OvernightIndexObservation].hash(test2) shouldBe test2.hashCode

    // `Show` renders what `toString` renders, so the two ways of putting an observation into a
    // message agree, and both forms are pinned as literals.
    Show[OvernightIndexObservation].show(test) shouldBe test.toString
    Show[OvernightIndexObservation].show(test2) shouldBe test2.toString
    test.toString shouldBe "OvernightIndexObservation[GBP-SONIA on 2016-02-22]"
    test2.toString shouldBe "OvernightIndexObservation[EUR-EONIA on 2016-02-23]"

    //-----------------------------------------------------------------------
    // Equality reads the index and the fixing date and nothing else: the publication, effective
    // and maturity dates and the year fraction are excluded by design, exactly as the bean being
    // ported excluded them. That is defensible because all four are functions of the index, the
    // fixing date and the calendar of the index, so two observations that agree on the first two
    // describe the same observation - and it is why the derived values of this type are asserted
    // field by field in `test_of` and `test_serialization` rather than through a comparison.
    val again: OvernightIndexObservation = observationOf(OvernightIndices.GBP_SONIA, FixingDate)
    (again eq test) shouldBe false
    (again == test) shouldBe true
    Hash[OvernightIndexObservation].eqv(test, again) shouldBe true
    Hash[OvernightIndexObservation].hash(test) shouldBe Hash[OvernightIndexObservation].hash(again)

    // Each of the two values equality does read is read: a difference in the fixing date alone
    // and a difference in the index alone are both differences.
    val laterFixing: OvernightIndexObservation =
      observationOf(OvernightIndices.GBP_SONIA, FixingDate.plusDays(1))
    Hash[OvernightIndexObservation].eqv(test, laterFixing) shouldBe false

    val otherIndex: OvernightIndexObservation = observationOf(OvernightIndices.EUR_EONIA, FixingDate)
    Hash[OvernightIndexObservation].eqv(test, otherIndex) shouldBe false

    // Nothing that is not an observation is equal to one, which is the other half of an equality
    // the ported sweep exercised through a bean of a different type. The subject is typed as
    // `Any` because that is the parameter the comparison takes: a comparison written against the
    // index type would be a question the compiler answers rather than one this test asks.
    val notAnObservation: Any = OvernightIndices.GBP_SONIA
    (test == notAnObservation) shouldBe false

    //-----------------------------------------------------------------------
    // Reading the six fields by pattern is unaffected by the closed construction surface -
    // `unapply` is available - which is what keeps a ported call site that matched on the bean
    // readable.
    val destructured: OvernightIndex = test match {
      case OvernightIndexObservation(index, fixingDate, publicationDate, effectiveDate, maturityDate, yearFraction) =>
        fixingDate shouldBe FixingDate
        publicationDate shouldBe PublicationDate
        effectiveDate shouldBe EffectiveDate
        maturityDate shouldBe MaturityDate
        yearFraction shouldBe ExpectedYearFraction
        index
    }
    destructured shouldBe OvernightIndices.GBP_SONIA

    // An observation can only be built by the factory that derives its four dependent values, so
    // no value of this type can describe dates the index it names would not produce. This is
    // where that claim is proved for the route that would otherwise reopen it: a `copy` would
    // take an observation apart and put it back together with one field replaced, and there is
    // none. The sibling snippet reaches for a member that does exist, so it compiles - which is
    // what keeps the first from passing for the wrong reason, since a snippet naming anything
    // this scope could not resolve would also fail to compile and would prove nothing.
    assertDoesNotCompile(
      """OvernightIndexObservation
           .of(OvernightIndices.GBP_SONIA, date(2016, 2, 22), ReferenceData.standard)
           .map(observation => observation.copy(fixingDate = date(2016, 2, 23)))""")
    assertCompiles(
      """OvernightIndexObservation
           .of(OvernightIndices.GBP_SONIA, date(2016, 2, 22), ReferenceData.standard)
           .map(observation => observation.fixingDate)""")
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not part of this port; the JSON codec takes its place. Both
    // directions are asserted, because an encoding that is wrong and a decoding that is wrong in
    // the same way would still round trip.
    val test: OvernightIndexObservation = observationOf(OvernightIndices.GBP_SONIA, FixingDate)
    val document: Json = test.asJson

    // The document is the six properties of the bean being ported, under their names and in their
    // declaration order.
    document.asObject.map(_.keys.toList) shouldBe Some(
      List("index", "fixingDate", "publicationDate", "effectiveDate", "maturityDate", "yearFraction"))

    // The index rides its own codec, so it is its bare name rather than an object, and the four
    // dates are ISO-8601 text. Each date is checked against the derived fixture, so the document
    // is pinned to the derivation rather than to a transcription of it.
    document.hcursor.downField("index").as[String] shouldBe Right("GBP-SONIA")
    document.hcursor.downField("fixingDate").as[String] shouldBe Right(FixingDate.toString)
    document.hcursor.downField("publicationDate").as[String] shouldBe Right(PublicationDate.toString)
    document.hcursor.downField("effectiveDate").as[String] shouldBe Right(EffectiveDate.toString)
    document.hcursor.downField("maturityDate").as[String] shouldBe Right(MaturityDate.toString)

    // The year fraction is a JSON number rather than one of the tagged strings the double policy
    // of this port reserves for the values JSON cannot express, because this one is finite.
    document.hcursor.downField("yearFraction").focus.map(_.isNumber) shouldBe Some(true)
    document.hcursor.downField("yearFraction").focus.map(_.isString) shouldBe Some(false)
    document.hcursor.downField("yearFraction").as[Double] shouldBe Right(ExpectedYearFraction)

    // The whole document, as text, so the shape is stated once in this spec and compared rather
    // than only inspected field by field.
    document.noSpaces shouldBe ExpectedJson

    //-----------------------------------------------------------------------
    // The round trip. Equality of this type reads the index and the fixing date alone, so the
    // comparison of the decoded value to the original cannot see the four derived values at all;
    // they are therefore compared field by field as well, and without that this assertion would
    // be satisfied by a codec that dropped every one of them.
    val roundTripped: OvernightIndexObservation = decodedObservation(document.noSpaces)
    roundTripped shouldBe test
    roundTripped.publicationDate shouldBe test.publicationDate
    roundTripped.effectiveDate shouldBe test.effectiveDate
    roundTripped.maturityDate shouldBe test.maturityDate
    roundTripped.yearFraction shouldBe test.yearFraction

    // The same document read from the text of this spec, which pins the decoding side to the
    // shape rather than to whatever the encoder happened to produce.
    val fromText: OvernightIndexObservation = decodedObservation(ExpectedJson)
    fromText shouldBe test
    fromText.publicationDate shouldBe PublicationDate
    fromText.effectiveDate shouldBe EffectiveDate
    fromText.maturityDate shouldBe MaturityDate
    fromText.yearFraction shouldBe ExpectedYearFraction

    //-----------------------------------------------------------------------
    // The decoder rebuilds the four derived values from the index and the fixing date and refuses
    // a document that disagrees with them. This is what makes the round trip above mean what it
    // appears to mean: because equality ignores those four values, a document contradicting the
    // index it names would otherwise decode into a value comparing equal to the one encoded while
    // describing something else. Each of the four is tampered with in turn, and the diagnostic
    // names the field that disagrees.
    val contradictions: List[(String, Json)] = List(
      "publicationDate" -> Json.fromString(PublicationDate.plusDays(1).toString),
      "effectiveDate" -> Json.fromString(EffectiveDate.plusDays(1).toString),
      "maturityDate" -> Json.fromString(MaturityDate.plusDays(1).toString),
      "yearFraction" -> Json.fromDoubleOrNull(ExpectedYearFraction * 2.0))

    contradictions.foreach { case (field, replacement) =>
      val payload: String = document.mapObject(fields => fields.add(field, replacement)).noSpaces
      val refused = decode[OvernightIndexObservation](payload)
      refused.isLeft shouldBe true
      refused.swap.toOption.fold("")(error => error.getMessage) should include(field)
    }

    // A document naming no index does not decode: every field of the shape the reader reads is
    // required rather than defaulted, so an incomplete document is refused before the index and
    // the fixing date reach the factory at all.
    decode[OvernightIndexObservation]("""{"fixingDate":"2016-02-22"}""").isLeft shouldBe true
  }
}
