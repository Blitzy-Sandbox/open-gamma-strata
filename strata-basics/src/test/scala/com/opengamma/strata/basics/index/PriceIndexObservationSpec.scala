/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.YearMonth

import cats.Eq
import cats.Hash
import cats.Show

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency

/**
 * Test [[PriceIndexObservation]].
 *
 * The three methods of the Java original are kept, under their own names and in their own
 * order, so that the method-level traceability of this migration stays one-to-one: the test
 * inventory joins each row of its mapping to a test of a named suite, so a renamed, merged or
 * omitted method reads as an unmapped row rather than as a tidier spec. Nothing is added to the
 * set of three, and no method of the original is dropped.
 *
 * ===Why this is the shortest of the four observation specs===
 *
 * A price index publishes a level once a month, so a month is the whole of what identifies the
 * point of fixing. There is no fixing date to offset, no publication or effective date to
 * derive and no maturity to compute, which means there is no calendar to resolve and therefore
 * nothing about this pairing that construction could reject. That is what makes the subject the
 * `[T]` (total) construction kind of AAP §0.3.3 - the only one of the four observations that is
 * not `[V]` - and this spec is written to that classification rather than to the shape of its
 * three siblings:
 *
 *  - `PriceIndexObservation.of` answers with the observation itself, not with an `Either`, so
 *    no result matcher appears here and no failing construction is manufactured.
 *  - There is deliberately no reference-data fixture, in contrast to every sibling spec of this
 *    package. Its absence is not an omission; it is the observable form of the classification,
 *    since a type that derives no date has no data to derive it from.
 *  - The generated `apply` and `copy` are public on purpose, and `test_of` asserts that
 *    positively. For the other three observations a public constructor would admit a value
 *    whose derived dates disagreed with its index, which is why theirs are suppressed; here
 *    there is nothing to disagree, so suppressing them would buy nothing and would cost the
 *    idiomatic surface of a case class. Asserting it keeps a later change from tightening the
 *    type silently.
 *  - Equality is the all-field equality the case class generates, so a round trip of a value is
 *    checked by comparing the value alone. The Overnight and exchange-rate observations reduce
 *    their equality to the index and the fixing date, and their specs therefore compare their
 *    derived fields explicitly; that extra comparison would be noise here, just as this
 *    simpler form would be a gap there.
 *
 * ===The two methods whose machinery this port does not have===
 *
 * Neither is dropped; each is ported as an assertion of the guarantee its machinery stood in
 * for, and the reasoning is repeated at the test itself:
 *
 *  - `coverage` swept the value reflectively as a Java bean and compared it against a second
 *    instance. No such helper exists here - the testkit of `strata-collect` publishes exactly
 *    five helpers, none of them reflective - because this port derives nothing reflectively
 *    (AAP §0.8.1 D-5). The properties those sweeps stood in for are asserted directly, over the
 *    very two subjects the Java method used.
 *  - `test_serialization` asserted a round trip through the serialization mechanism of the
 *    platform, which no type of this port supports (AAP §0.2.2). Its replacement is the JSON
 *    codec of AAP §0.6.4, and what the test pins is the exact document the codec is required to
 *    write, together with the rejections its reader is required to make. The property-based
 *    sweep over every codec-bearing type of the module belongs to the consolidated
 *    `json.JsonRoundTripSpec`, which is where the mapping routes the traceability of this Java
 *    method; this test deliberately pins the per-type representation instead of repeating that
 *    sweep, so that neither the shape nor the sweep can be lost with the other.
 *
 * ===Obligations of the request that govern this spec (AAP §0.7 / §0.8.1)===
 *
 * These are requirements of the original request carried by the plan, not entries of a user
 * rules document: `review_rules` reports that none was provided.
 *
 *  - Rule 5 (explicit functional error handling) reaches this file only through the codec. The
 *    subject appears in no entry of the failable surface of AAP §0.3.3 and has no documented
 *    precondition, so no error is raised anywhere in the three tests and none is intercepted;
 *    the two rejections of `test_serialization` are asserted as values on the left of the
 *    reader's result.
 *  - Rule 4 (closed families) does not govern an observation, which is an ordinary value built
 *    on demand rather than a member of a published set. [[IndexObservation]] is open by design,
 *    its four implementations being four files, and the open-contract row `ApiSurfaceSpec`
 *    carries for the trait is where that is established: it implements the trait from outside
 *    `IndexObservation.scala` and compiles a second implementation. Nothing here speaks to
 *    whether the trait can be extended. The closedness of [[PriceIndex]], which the subject
 *    carries, belongs to `PriceIndexSpec` and `NamedEnumClosedSpec`.
 *  - Rule 7 keeps effects at the edges: this spec is pure, and runs no effect type.
 *  - Rule 3 (immutability) is the subject's own property, not this spec's: a `final case class`
 *    over a `PriceIndex` and a `java.time.YearMonth`, both immutable, so no value of it can be
 *    mutated after construction. This spec binds its fixtures with `val` and mutates nothing,
 *    which is consistent with that property rather than evidence of it.
 *
 * @see [[PriceIndexObservation]] for the subject
 * @see [[PriceIndices]] for the two indices used as subjects
 */
class PriceIndexObservationSpec extends AnyFunSuite with Matchers {

  // The fixing month of the Java original, and the whole of the fixture: this spec needs no
  // reference data, which is the point of the [T] classification recorded above.
  private val FixingMonth: YearMonth = YearMonth.of(2016, 2)

  test("test_of") {
    // The factory is total. It answers with the observation itself rather than with a result to
    // be unwrapped, so what follows reads directly off the value.
    val index: PriceIndex = PriceIndices.GB_HICP
    val test: PriceIndexObservation = PriceIndexObservation.of(index, FixingMonth)

    test.index shouldBe index
    test.fixingMonth shouldBe FixingMonth

    // the currency is the index's own, and is never absent; it is named twice so that the
    // delegation and the value it delegates to are both pinned
    val expectedCurrency: Currency = index.currency
    test.currency shouldBe expectedCurrency
    test.currency shouldBe Currency.GBP

    // the rendering of the original, to the character: the index renders as its name and the
    // month in ISO-8601 form
    test.toString shouldBe "PriceIndexObservation[GB-HICP on 2016-02]"

    // The value reaches its abstraction, which asks for the index and nothing else.
    val asObservation: IndexObservation = test
    asObservation.index shouldBe index

    // [T] fact one (AAP §0.3.3): the generated `apply` is public by design, because an index
    // paired with a month cannot be inconsistent, and it builds the same value as `of`.
    val applied: PriceIndexObservation = PriceIndexObservation(index, FixingMonth)
    applied shouldBe test
    Eq[PriceIndexObservation].eqv(applied, test) shouldBe true

    // [T] fact two: `copy` is public for the same reason. The three sibling observations are
    // `[V]` and must have neither member, since each derives dates that a copy could contradict.
    val copiedIndex: PriceIndexObservation = test.copy(index = PriceIndices.CH_CPI)
    copiedIndex.index shouldBe PriceIndices.CH_CPI
    copiedIndex.fixingMonth shouldBe FixingMonth
    copiedIndex.currency shouldBe Currency.CHF
    copiedIndex should not be test

    val copiedMonth: PriceIndexObservation = test.copy(fixingMonth = FixingMonth.plusMonths(1))
    copiedMonth.index shouldBe index
    copiedMonth.fixingMonth shouldBe YearMonth.of(2016, 3)
    copiedMonth.toString shouldBe "PriceIndexObservation[GB-HICP on 2016-03]"
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Ruling - typeclass cover in place of two reflective sweeps. The Java method swept the
    // value as a bean and then compared it against a second instance differing in both fields.
    // Neither helper exists here, so the cover is written over the very two subjects the
    // original used, and asserts what those sweeps were there to guarantee: that equality,
    // hashing and rendering exist, agree with one another, and that equality and rendering
    // separate the two values. Hashing is not asked to separate them: its contract runs one way
    // only - equal values must hash alike, unequal values are permitted to collide - so of the
    // differing subject the real property is asserted instead, that the instance answers with
    // the value's own `hashCode`. The alike-in-hash direction is asserted of an equal pair below.
    val test: PriceIndexObservation = PriceIndexObservation.of(PriceIndices.GB_HICP, FixingMonth)
    val test2: PriceIndexObservation =
      PriceIndexObservation.of(PriceIndices.CH_CPI, FixingMonth.plusMonths(1))

    test should not be test2
    Eq[PriceIndexObservation].eqv(test, test2) shouldBe false
    Hash[PriceIndexObservation].eqv(test, test2) shouldBe false
    Hash[PriceIndexObservation].hash(test2) shouldBe test2.hashCode

    // Equality is the all-field equality the case class generates, so each field on its own
    // distinguishes two observations. This is the property the Overnight and exchange-rate
    // observations do not have, their equality being reduced to two of their fields.
    val sameIndexOtherMonth: PriceIndexObservation =
      PriceIndexObservation.of(PriceIndices.GB_HICP, FixingMonth.plusMonths(1))
    val sameMonthOtherIndex: PriceIndexObservation =
      PriceIndexObservation.of(PriceIndices.CH_CPI, FixingMonth)
    Eq[PriceIndexObservation].eqv(test, sameIndexOtherMonth) shouldBe false
    Eq[PriceIndexObservation].eqv(test, sameMonthOtherIndex) shouldBe false
    Eq[PriceIndexObservation].eqv(sameIndexOtherMonth, sameMonthOtherIndex) shouldBe false

    // Two observations built from the same arguments are one value: equal, equal under the
    // instance, and alike in hash. The instance is taken from the generated hashing, so the two
    // can never disagree, and that is asserted rather than assumed.
    val rebuilt: PriceIndexObservation =
      PriceIndexObservation.of(PriceIndices.GB_HICP, YearMonth.of(2016, 2))
    rebuilt shouldBe test
    Eq[PriceIndexObservation].eqv(rebuilt, test) shouldBe true
    Hash[PriceIndexObservation].eqv(rebuilt, test) shouldBe true
    Hash[PriceIndexObservation].hash(rebuilt) shouldBe Hash[PriceIndexObservation].hash(test)
    Hash[PriceIndexObservation].hash(test) shouldBe test.hashCode

    // The rendering instance renders what the value renders, for both subjects.
    Show[PriceIndexObservation].show(test) shouldBe test.toString
    Show[PriceIndexObservation].show(test) shouldBe "PriceIndexObservation[GB-HICP on 2016-02]"
    Show[PriceIndexObservation].show(test2) shouldBe test2.toString
    Show[PriceIndexObservation].show(test2) shouldBe "PriceIndexObservation[CH-CPI on 2016-03]"

    // The companion publishes a hashing and a rendering, and deliberately no ordering: the Java
    // type is not comparable, and ordering by month and ordering by index are both defensible,
    // so neither is a caller's to assume (AAP §0.3.3). That absence is asserted at compile time.
    assertDoesNotCompile("implicitly[cats.Order[PriceIndexObservation]]")
  }

  test("test_serialization") {
    // Ruling - the JSON codec of AAP §0.6.4 replaces the platform serialization the Java method
    // asserted. The document is pinned exactly, not merely round-tripped: the two keys are the
    // property names of the Java bean, in declaration order, the index riding the bare-string
    // codec of its family and the month the ISO-8601 form of the JSON library. Neither field is
    // optional, so the wrapper that drops absent fields has nothing to drop.
    val test: PriceIndexObservation = PriceIndexObservation.of(PriceIndices.GB_HICP, FixingMonth)

    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe """{"index":"GB-HICP","fixingMonth":"2016-02"}"""
    encoded shouldBe Json.obj(
      "index" -> Json.fromString("GB-HICP"),
      "fixingMonth" -> Json.fromString("2016-02"))

    // The round trip. Equality being all-field, comparing the decoded value against the
    // original is the whole of the check - both fields are compared by it.
    encoded.as[PriceIndexObservation] shouldBe Right(test)
    decode[PriceIndexObservation]("""{"index":"GB-HICP","fixingMonth":"2016-02"}""") shouldBe
      Right(test)

    // the second subject of the Java coverage method travels the same way
    val test2: PriceIndexObservation =
      PriceIndexObservation.of(PriceIndices.CH_CPI, FixingMonth.plusMonths(1))
    test2.asJson.noSpaces shouldBe """{"index":"CH-CPI","fixingMonth":"2016-03"}"""
    test2.asJson.as[PriceIndexObservation] shouldBe Right(test2)

    // A document naming no published index is reported on the left as a decoding failure rather
    // than decoded into some nearby index or raised as an error (Rule 5). The rejection comes
    // from the index codec, which is asserted beside it; the wording of the failure is left
    // free to change, as elsewhere in this port.
    val unknownIndex = Json.obj(
      "index" -> Json.fromString("XX-RUBBISH"),
      "fixingMonth" -> Json.fromString("2016-02")).as[PriceIndexObservation]
    unknownIndex.isLeft shouldBe true
    val unknownIndexFailure: DecodingFailure =
      unknownIndex.swap.getOrElse(fail("expected a decoding failure for an unpublished index"))
    unknownIndexFailure.message should not be empty
    Json.fromString("XX-RUBBISH").as[PriceIndex].isLeft shouldBe true

    // A month that is not an ISO-8601 year and month is reported the same way.
    val malformedMonth = Json.obj(
      "index" -> Json.fromString("GB-HICP"),
      "fixingMonth" -> Json.fromString("February 2016")).as[PriceIndexObservation]
    malformedMonth.isLeft shouldBe true
    val malformedMonthFailure: DecodingFailure =
      malformedMonth.swap.getOrElse(fail("expected a decoding failure for a malformed month"))
    malformedMonthFailure.message should not be empty

    // Both fields are required, and the object form is the only form this type is written in,
    // so an incomplete document and the bare string of the index alone are both rejected.
    Json.obj("index" -> Json.fromString("GB-HICP")).as[PriceIndexObservation].isLeft shouldBe true
    Json
      .obj("fixingMonth" -> Json.fromString("2016-02"))
      .as[PriceIndexObservation]
      .isLeft shouldBe true
    Json.fromString("GB-HICP").as[PriceIndexObservation].isLeft shouldBe true
  }
}
