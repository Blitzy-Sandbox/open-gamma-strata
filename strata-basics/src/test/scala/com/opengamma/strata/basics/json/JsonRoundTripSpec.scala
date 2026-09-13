/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.json

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.LocalTime
import java.time.Period
import java.time.YearMonth
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.List
import scala.collection.immutable.SortedMap
import scala.collection.immutable.Vector

import cats.Eq
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Codec
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.basics.Arbitraries._
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.StandardId
import com.opengamma.strata.basics.currency.AdjustablePayment
import com.opengamma.strata.basics.currency.BigMoney
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyAmountArray
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxMatrix
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.Money
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.basics.currency.MultiCurrencyAmountArray
import com.opengamma.strata.basics.currency.Payment
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.AdjustableDates
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DateSequence
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.FriSat
import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.basics.date.MarketTenor
import com.opengamma.strata.basics.date.NoHolidays
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.PeriodAdjustment
import com.opengamma.strata.basics.date.SatSun
import com.opengamma.strata.basics.date.SequenceDate
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.basics.date.ThuFri
import com.opengamma.strata.basics.index.FloatingRateName
import com.opengamma.strata.basics.index.FloatingRateType
import com.opengamma.strata.basics.index.FxIndex
import com.opengamma.strata.basics.index.FxIndexObservation
import com.opengamma.strata.basics.index.FxIndices
import com.opengamma.strata.basics.index.IborIndex
import com.opengamma.strata.basics.index.IborIndexObservation
import com.opengamma.strata.basics.index.IborIndices
import com.opengamma.strata.basics.index.OvernightIndex
import com.opengamma.strata.basics.index.OvernightIndexObservation
import com.opengamma.strata.basics.index.OvernightIndices
import com.opengamma.strata.basics.index.PriceIndex
import com.opengamma.strata.basics.index.PriceIndexObservation
import com.opengamma.strata.basics.index.PriceIndices
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.PeriodicSchedule
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.basics.value.NoRounding
import com.opengamma.strata.basics.value.Rounding
import com.opengamma.strata.basics.value.ValueAdjustment
import com.opengamma.strata.basics.value.ValueAdjustmentType
import com.opengamma.strata.basics.value.ValueDerivatives
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * The record standing in for the mock bean of the serialization test this suite consolidates.
 *
 * Three of that bean's four properties are typed fields here; the fourth, a list of bare objects,
 * has no counterpart, for the reason given at `test_jodaBeans_serialize`. It is declared outside
 * the suite so that the derivation of its codec sees a stable type rather than one dependent on
 * an instance of the suite.
 *
 * @param bdConvention  the business day convention, carried as its name
 * @param holidayCalendar  the holiday calendar, carried as its name or its structure
 * @param dayCount  the day count, carried as its name or its structure
 */
private[json] final case class MockSerRecord(
    bdConvention: BusinessDayConvention,
    holidayCalendar: HolidayCalendar,
    dayCount: DayCount)

/**
 * Holds the derived codec of [[MockSerRecord]].
 *
 * Both halves are derived semi-automatically from the declared shape of the record, and the
 * encoder is wrapped in the same policy for products as every other product here, though this
 * record declares no optional field.
 */
private[json] object MockSerRecord {

  implicit val encoder: Encoder[MockSerRecord] = Codecs.dropNulls(deriveEncoder[MockSerRecord])

  implicit val decoder: Decoder[MockSerRecord] = deriveDecoder[MockSerRecord]
}

/**
 * A type that deliberately carries no codec, and the reason it carries none. The reason is one
 * line, because it is written as the tail of a line of the printed report.
 *
 * @param fqcn  the fully qualified name of the type
 * @param reason  why the type carries no codec, in one line
 */
private[json] final case class ExcludedType(fqcn: String, reason: String)

/**
 * A value of a type the inventory does not cover, generated in place of the covered values an
 * audit run is not allowed to construct.
 *
 * Its fields are the leaf types the covered generators are built from, so generating and rendering
 * one exercises the same machinery while naming no type of either module.
 *
 * @param text  arbitrary text
 * @param count  an arbitrary whole number
 * @param magnitude  an arbitrary real number
 * @param date  an arbitrary date
 * @param time  an arbitrary time of day
 * @param zone  an arbitrary time zone
 * @param day  an arbitrary day of the week
 */
private[json] final case class AuditProbe(
    text: String,
    count: Int,
    magnitude: Double,
    date: LocalDate,
    time: LocalTime,
    zone: ZoneId,
    day: DayOfWeek)

/**
 * The tally an audit run accumulates over one type, and over the whole suite by addition.
 *
 * @param planned  how many values the inventory plans for the type, read from the inventory rather
 *   than from what was generated and therefore the same in both runs
 * @param digest  the digest of the work both runs do, which names no covered type
 * @param generated  how many values of the covered type were constructed, zero unless the run was
 *   asked to encode
 * @param roundTrips  how many of them encoded and decoded back to themselves, zero unless the run
 *   was asked to encode
 */
private[json] final case class AuditTally(
    planned: Int,
    digest: Int,
    generated: Int,
    roundTrips: Int) {

  /**
   * Adds another tally to this one.
   *
   * @param other  the tally to add
   * @return the sum of the two
   */
  def combine(other: AuditTally): AuditTally =
    AuditTally(
      planned + other.planned,
      digest + other.digest,
      generated + other.generated,
      roundTrips + other.roundTrips)
}

/**
 * One covered type, its place in the inventory, and the three things this suite does with it.
 *
 * The type itself appears in none of the fields: it is captured by the closures [[codecCase]]
 * builds while the type is still known, because a list of cases carrying their element type would
 * have to be a list of an existential or of `Any`, and this build rejects both.
 *
 * @param category  which of the five routes into JSON the type takes
 * @param typeName  the simple name of the type, used in the name of its test
 * @param fqcn  the fully qualified name of the type, used in the printed report
 * @param roundTrip  asserts that decoding what the type encoded yields the value again
 * @param reEncode  asserts that the document a decoded value writes is the document it was read from
 * @param audit  performs the audit work, encoding as well when asked to
 */
private[json] final case class CodecCase(
    category: String,
    typeName: String,
    fqcn: String,
    roundTrip: () => Assertion,
    reEncode: () => Assertion,
    audit: Boolean => AuditTally)

/**
 * The serialization suite of `strata-basics`: every type of either module that carries a JSON
 * codec, round-tripped through generated values, and every type that carries none, proved to have
 * none.
 *
 * The inventory is closed and written down once: [[codecCases]] holds the covered half - 14 named
 * families, 9 values identified by text, 2 hand-written codecs, 29 derived products, 4 explicit
 * shapes, 58 types - and [[excludedTypes]] the rest, so every public data or contract type of
 * `strata-collect` and `strata-basics` stands in exactly one of them, and both are printed in a
 * parseable form. Four things are asserted of them: coverage, a round trip per covered type and an
 * absence proof per excluded one; shape, each document form pinned against a literal; stability,
 * bytes that depend on the value alone; and refusal, reported rather than raised.
 *
 * Elsewhere: the [[com.opengamma.strata.collect.json.Codecs]] helpers in isolation in
 * `CodecsSpec`, closedness in `NamedEnumClosedSpec`, factory failures in `SmartConstructorSpec`
 * and `FailableSurfaceSpec`, the instance laws in `TypeclassLawsSpec`, and apply, copy and
 * subtyping in `ApiSurfaceSpec`; the compile-time proofs here are confined to codec absence.
 *
 * The test-mapping manifest joins a row to a test on the pair of suite class and test name, so the
 * names follow a fixed convention:
 *
 * {{{
 * test_jodaBeans_serialize                  the consolidated serialization test
 * round-trip: <SimpleTypeName>              one per covered type
 * shape: <subject>                          a document form pinned against a literal
 * invalid: <subject>                        a payload refused as a reported failure
 * byte-stability: <subject>                 a document that depends on the value alone
 * excluded from JSON: <SimpleTypeName>      one per deliberately unserializable type
 * not published: <subject>                  a codec that exists but is not part of the API
 * codec coverage report                     the printed inventory
 * codec audit: deterministic value enumeration   the class-load audit mode
 * }}}
 *
 * The round trip runs one way only, `decode(encode(a)) == a`: the document-first direction is
 * false by design for two types, because the forms a holiday calendar and a `Bus/252` day count
 * ''accept'' are wider than the forms they ''write'' - `"GBLO+USNY"` reads as a combined calendar
 * that writes itself structurally - and asserting it would forbid that deliberate leniency.
 *
 * The `codec.audit` system property selects a run that measures what serialization loads. Both
 * modes do the same common work - the probe values of [[auditCommonWork]] and the textual digest
 * of every entry of the inventory - and `baseline` stops there, constructing no value of a covered
 * type and acquiring no encoder, decoder or equality. `codec` additionally draws every covered
 * value from its `Arbitraries` generator and encodes and decodes it, so the difference between the
 * classes the two runs load is the covered generators, the codecs, and everything constructing one
 * reaches; that difference must hold no class of any reflection package and none that refers to
 * the reflection API. The property gates the registration of the ''whole'' suite rather than of
 * one test, so nothing else can differ between the runs; `registerAuditMode` states what the
 * difference attributes and asserts the counts, two of them zero, that make the statement
 * checkable.
 */
class JsonRoundTripSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  //-------------------------------------------------------------------------
  // Phase A - the closed inventory of the covered types.
  //
  // The five categories are the five routes a type takes into JSON, and a type's category is
  // therefore a statement about how its codec is built rather than a label attached to it here.

  /** A closed family of named values, written as the member's name. */
  private val NamedEnumCategory: String = "named-enum"

  /** An open value identified by text, written as its canonical form. */
  private val ParsedStringCategory: String = "parsed-string"

  /** A family whose codec is written out by hand because it mixes two forms. */
  private val HandWrittenCategory: String = "hand-written"

  /** A product whose codec is derived from its field shape at compile time. */
  private val SemiautoCategory: String = "semiauto-product"

  /** A value whose document form is stated explicitly rather than derived. */
  private val ExplicitCategory: String = "explicit-shape"

  private val ExpectedNamedEnumTypes: Int = 14

  private val ExpectedParsedStringTypes: Int = 9

  private val ExpectedHandWrittenTypes: Int = 2

  private val ExpectedSemiautoTypes: Int = 29

  private val ExpectedExplicitTypes: Int = 4

  /** The reason shared by the reference data store and everything that populates it. */
  private val StoreReason: String =
    "heterogeneous identifier-to-value store; only calendars are serializable and HolidayCalendar carries them"

  /** The reason shared by the function and contract types, which carry no data. */
  private val ContractReason: String = "function or contract type with no data of its own"

  /** The reason shared by the typeclasses, helper objects and effect edges. */
  private val MachineryReason: String = "typeclass, helper or effect rather than data"

  /** The reason shared by the abstract heads of the index families. */
  private val AbstractHeadReason: String = "abstract head of a family whose leaf types are covered"

  /**
   * The types of both modules that carry no codec, each with its reason.
   *
   * Together with the covered types of [[codecCases]] this list closes the inventory: every public
   * data type and every public contract type of `strata-collect` and `strata-basics` appears in
   * exactly one of the two. Built on first use, so an audit run - which prints no report - builds
   * nothing it does not read.
   *
   * The modules publish more than data and contracts, and the rest is closed elsewhere: an alias
   * is the same type under a second name, a named partial application is a generic container of
   * the standard library, and `ApiSurfaceSpec` records one row per public surface of either
   * module.
   */
  private lazy val excludedTypes: List[ExcludedType] = List(
    ExcludedType("com.opengamma.strata.basics.ReferenceData", StoreReason),
    ExcludedType("com.opengamma.strata.basics.ImmutableReferenceData", StoreReason),
    ExcludedType("com.opengamma.strata.basics.CombinedReferenceData", StoreReason),
    ExcludedType("com.opengamma.strata.basics.date.HolidaySafeReferenceData", StoreReason),
    ExcludedType("com.opengamma.strata.basics.ReferenceData.Entry", "same reason as the store it populates"),
    ExcludedType(
      "com.opengamma.strata.basics.ReferenceDataId",
      "behavioural abstraction; HolidayCalendarId is the one identifier with a codec"),
    ExcludedType(
      "com.opengamma.strata.basics.date.DayCount.ScheduleInfo",
      "behavioural interface, implemented by the covered Schedule"),
    ExcludedType("com.opengamma.strata.basics.date.DateAdjuster", ContractReason),
    ExcludedType("com.opengamma.strata.basics.currency.FxRateProvider", ContractReason),
    ExcludedType("com.opengamma.strata.basics.currency.LazyFxRateProvider", ContractReason),
    ExcludedType("com.opengamma.strata.basics.currency.FxConvertible", ContractReason),
    ExcludedType("com.opengamma.strata.basics.Resolvable", ContractReason),
    ExcludedType("com.opengamma.strata.basics.ResolvableCalculationTarget", ContractReason),
    ExcludedType("com.opengamma.strata.basics.CalculationTarget", ContractReason),
    ExcludedType("com.opengamma.strata.basics.CalculationTargetList", "its element type is excluded"),
    ExcludedType("com.opengamma.strata.collect.array.Matrix", "trait; DoubleMatrix is covered"),
    // the single-abstract-method callbacks the two numeric wrappers take instead of boxing an
    // index: contract types by the same reading as DateAdjuster above, and the construction-kind
    // inventory audits each of them as a function surface
    ExcludedType("com.opengamma.strata.collect.array.DoubleArray.DoubleTernaryOperator", ContractReason),
    ExcludedType("com.opengamma.strata.collect.array.DoubleMatrix.ElementAction", ContractReason),
    ExcludedType("com.opengamma.strata.collect.array.DoubleMatrix.ElementFunction", ContractReason),
    ExcludedType("com.opengamma.strata.collect.array.DoubleMatrix.RowArrayFunction", ContractReason),
    ExcludedType("com.opengamma.strata.collect.array.DoubleMatrix.RowArrayObjectFunction", ContractReason),
    ExcludedType("com.opengamma.strata.collect.Named", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.named.NamedEnum", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.TypedStringCompanion", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.ArgCheck", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.Validate", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.Collections", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.DoubleArrayMath", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.io.Resources", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.json.Codecs", MachineryReason),
    ExcludedType("com.opengamma.strata.collect.result.FailureOr", "generic container; no port-owned codec"),
    ExcludedType("com.opengamma.strata.collect.result.ResultNec", "generic container; no port-owned codec"),
    ExcludedType("com.opengamma.strata.collect.result.ValidatedFailures", "generic container; no port-owned codec"),
    ExcludedType("com.opengamma.strata.collect.result.ValueWithFailures", "generic container; no port-owned codec"),
    ExcludedType("com.opengamma.strata.basics.index.Index", AbstractHeadReason),
    ExcludedType("com.opengamma.strata.basics.index.RateIndex", AbstractHeadReason),
    ExcludedType("com.opengamma.strata.basics.index.FloatingRateIndex", AbstractHeadReason),
    ExcludedType("com.opengamma.strata.basics.index.FloatingRate", AbstractHeadReason),
    ExcludedType("com.opengamma.strata.basics.index.IndexObservation", AbstractHeadReason))

  //-------------------------------------------------------------------------
  // Fixtures.
  //
  // A validated factory reports a value it would not build rather than raising it, so a fixture
  // is unwrapped through one of the two helpers below and one that could not be built is a failed
  // test naming the cause.
  //
  // Each is computed on first use. They serve the document-form tests, which an audit run does
  // not register, so building them eagerly would load classes in both runs and cancel out of the
  // difference the audit measures.

  /** The reference data every observation and every named calendar of this suite resolves against. */
  private lazy val refData: ReferenceData = ReferenceData.standard

  /**
   * Unwraps a fixture whose factory reports a single cause.
   *
   * @param outcome  the outcome of the factory
   * @param what  the fixture being built, named in the failure
   * @tparam A  the type of the fixture
   * @return the value, where the factory produced one
   */
  private def required[A](outcome: Either[Failure, A], what: String): A =
    outcome.fold(failure => fail(s"The fixture '$what' could not be built: ${failure.message}"), identity)

  /**
   * Unwraps a fixture whose factory reports every cause.
   *
   * @param outcome  the outcome of the factory
   * @param what  the fixture being built, named in the failure
   * @tparam A  the type of the fixture
   * @return the value, where the factory produced one
   */
  private def requiredNec[A](outcome: EitherNec[Failure, A], what: String): A =
    outcome.fold(
      failures =>
        fail(
          s"The fixture '$what' could not be built: " +
            failures.toNonEmptyList.toList.map(failure => failure.message).mkString("; ")),
      identity)

  private def amount(currency: Currency, value: Double): CurrencyAmount =
    required(CurrencyAmount.of(currency, value), s"$currency $value")

  private def decimal(text: String): Decimal = required(Decimal.of(text), s"Decimal $text")

  private lazy val testCalendarId: HolidayCalendarId = HolidayCalendarId.of("Test1")

  /** The two holidays of the calendar fixture below, in ascending order. */
  private lazy val testHolidays: List[LocalDate] =
    List(LocalDate.of(2014, 7, 14), LocalDate.of(2014, 7, 16))

  /**
   * A calendar carrying two holidays of its own, under an identifier that names no built-in
   * calendar - which is what makes it a subject of the structural document form.
   */
  private lazy val testCalendar: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(testCalendarId, testHolidays, DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)

  /** A calendar declaring one weekend date to be a business day, so the fourth field is not empty. */
  private lazy val workingWeekendCalendar: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("XCAL"),
      List(LocalDate.of(2020, 1, 1), LocalDate.of(2020, 12, 25)),
      Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY),
      List(LocalDate.of(2020, 3, 7)))

  //-------------------------------------------------------------------------
  // The two numeric types of `strata-collect` state their document form in that module's codec
  // support rather than in their own companions, so that the plain numeric encoding of a double
  // cannot be picked up by accident at a derivation site; the remaining fifty-six types publish
  // their own instances. These two are the only codec instances this suite owns rather than reads
  // from a companion, and they are read on first use for the reason every case below is: an audit
  // run that is not encoding must reach for no codec at all.

  /** The document form of an immutable array of doubles: a JSON array of tagged doubles. */
  private implicit lazy val doubleArrayCodec: Codec[DoubleArray] = Codecs.doubleArrayCodec

  /** The document form of an immutable matrix of doubles: a JSON array of rows. */
  private implicit lazy val doubleMatrixCodec: Codec[DoubleMatrix] = Codecs.doubleMatrixCodec

  //-------------------------------------------------------------------------
  // The per-type case, and the generic builder of one.

  /** The seed the audit mode generates from, fixed so that two runs generate the same values. */
  private val AuditSeed: Long = 20240101L

  /**
   * How many values the audit mode plans for each covered type, and draws of the probe value.
   *
   * It belongs to the plan rather than to what happened, so it is the same in both runs.
   */
  private val AuditValuesPerCase: Int = 8

  /** The generation size the audit mode uses, fixed for the same reason as the seed. */
  private val AuditGenerationSize: Int = 16

  /**
   * Draws the values of one type from the fixed seeds.
   *
   * A value is drawn from the fixed seed plus its index - never from the clock, a random source or
   * the property-check machinery, whose seeds vary between runs - so the two audit runs generate
   * the same values. A generator that yields nothing from every fixed seed fails the run naming
   * the type.
   *
   * @param generator  the generator of the type
   * @param typeName  the simple name of the type, named in the failure
   * @tparam A  the type being generated
   * @return the values generated, at least one
   */
  private def auditDraw[A](generator: Gen[A], typeName: String): List[A] = {
    val parameters = Gen.Parameters.default.withSize(AuditGenerationSize)
    val drawn = (0 until AuditValuesPerCase).toList
      .flatMap(index => generator(parameters, Seed(AuditSeed + index.toLong)))
    if (drawn.isEmpty) {
      fail(s"The generator of '$typeName' produced no value from any of the fixed audit seeds")
    } else {
      drawn
    }
  }

  /**
   * How many values of a covered type this suite has constructed since it was constructed.
   *
   * This counts the '''cause''' of codec construction. Every covered value is drawn through
   * [[auditValues]] from a generator of `Arbitraries`, and such a generator builds through the
   * type's own factories, so a run whose count is zero has initialised none of the companions
   * that publish those factories and the codecs beside them. It is atomic for the same reason
   * [[codecAcquisitions]] is: no increment may be lost, and none depends on being ordered.
   */
  private val generatedValues: AtomicInteger = new AtomicInteger(0)

  /**
   * Draws the values of one covered type, counting the construction.
   *
   * @param generator  the generator of the type
   * @param typeName  the simple name of the type, named in the failure
   * @tparam A  the covered type
   * @return the values generated, at least one
   */
  private def auditValues[A](generator: Gen[A], typeName: String): List[A] = {
    val drawn = auditDraw(generator, typeName)
    val _ = generatedValues.addAndGet(drawn.size)
    drawn
  }

  /**
   * The two runtime classes both audit runs load, named here by class constants.
   *
   * Each refers to the reflection API in its own body for reasons of its own, and the serializing
   * run would otherwise be the first to load it, leaving it in the class-load difference. Naming
   * is the only way to reach them because no expression this suite can write does:
   *
   *   - the bootstrap of a symbol literal, which only generated code calls and which reaches the
   *     reflection API for the symbol class;
   *   - the platform's random-number class, which the test framework's own event reporting pulls
   *     in through the platform's thread-local generator.
   *
   * A `classOf` is a class constant rather than a lookup: the compiler emits a constant-pool class
   * reference which resolves where it is read, loading the class and initialising nothing.
   */
  private val AuditRuntimeClasses: List[Class[_]] =
    List(classOf[scala.runtime.SymbolLiteral], classOf[java.util.Random])

  /**
   * The zones an audit run asks the platform for, chosen to reach the zone-rule provider.
   *
   * The platform finds its zone rules through a reflective service lookup and the codecs reach it
   * through the zone of an index, so both runs ask for a zone.
   */
  private val AuditZones: List[String] =
    List("Europe/London", "America/New_York", "Asia/Tokyo", "UTC")

  /**
   * The generator of the value an audit run is allowed to construct.
   *
   * Its combinators are those the covered generators are built from, so a run drawing from it
   * loads the generator machinery the other run also needs.
   */
  private val auditProbeGenerator: Gen[AuditProbe] =
    for {
      text <- Gen.alphaNumStr
      count <- Gen.choose(-1000, 1000)
      magnitude <- Gen.frequency(3 -> Gen.choose(-1.0e6, 1.0e6), 1 -> Gen.const(0.0))
      dayOfYear <- Gen.choose(0, 364)
      hour <- Gen.choose(0, 23)
      minute <- Gen.choose(0, 59)
      zone <- Gen.oneOf(AuditZones)
      day <- Gen.oneOf(DayOfWeek.values().toList)
    } yield AuditProbe(
      text = text,
      count = count,
      magnitude = magnitude,
      date = LocalDate.of(2024, 1, 1).plusDays(dayOfYear.toLong),
      time = LocalTime.of(hour, minute),
      zone = ZoneId.of(zone),
      day = day)

  /**
   * Performs the work the two audit runs have in common, and returns its digest. None of the
   * steps names a covered type:
   *
   *   - values of [[AuditProbe]] are drawn from the fixed seeds and rendered, which is the
   *     generator machinery and the rendering of a product;
   *   - they are put into a set, copied into an array and collected into a buffer, which is the
   *     collection machinery whose generic array support refers to the reflection API;
   *   - every method of that collection and of that buffer which carries a lambda of its own is
   *     called, for the reason given where the calls are made;
   *   - their dates and times are formatted and their zones resolved, which is the platform's
   *     date, format and zone machinery, the last of which finds its provider reflectively;
   *   - the class constants of [[AuditRuntimeClasses]] are read, which loads both classes.
   *
   * The digest is a function of that work alone, so the two runs produce the same digest; two
   * printed lines that differ mean a generator that is not deterministic or an inventory that
   * changed between the runs.
   *
   * @return the digest of the common work
   */
  private def auditCommonWork(): Int = {
    val drawn = auditDraw(auditProbeGenerator, "AuditProbe")
    val rendered = drawn.foldLeft(0)((total, probe) => total + probe.toString.length)
    val distinct = drawn.map(_.text).toSet.size
    val copied = drawn.toArray.length
    val formatted = drawn.foldLeft(0) { (total, probe) =>
      total + DateTimeFormatter.ISO_LOCAL_DATE.format(probe.date).length +
        DateTimeFormatter.ISO_LOCAL_TIME.format(probe.time).length +
        probe.zone.getRules.isFixedOffset.toString.length +
        probe.day.toString.length
    }
    val named = AuditRuntimeClasses.foldLeft(0)((total, loaded) => total + loaded.getName.length)
    rendered + distinct + copied + formatted + named + auditCollectionWork(drawn.map(_.count))
  }

  /**
   * Exercises the two standard-library classes that reach the reflection API, and returns a digest
   * of the results.
   *
   * Both call the reflection API to read the length of an array of unknown element type. Loading
   * them in both runs is not sufficient: the check that reads the class-load difference cannot
   * disassemble a lambda under its own name, so it inspects the whole of the class the lambda was
   * defined in, and a lambda first run in the serializing run would put that class in front of it.
   * The methods called below are every method of those two classes that carries a lambda.
   *
   * @param numbers  the numbers to aggregate, at least one
   * @return the digest of the aggregation
   */
  private def auditCollectionWork(numbers: List[Int]): Int = {
    val ordering: Ordering[Int] = Ordering.Int
    // rendered rather than added, so that a product wide enough to wrap around cannot make the
    // digest depend on how an overflow happened to land
    val aggregated = List(
      numbers.sum,
      numbers.product,
      numbers.max(ordering),
      numbers.min(ordering),
      numbers.maxBy(identity[Int])(ordering),
      numbers.minBy(identity[Int])(ordering),
      numbers.foldRight(0)((value, total) => total + value),
      numbers.reduceRight((value, total) => total + value))
      .foldLeft(0)((total, value) => total + value.toString.length)
    val optional =
      numbers.maxOption(ordering).size + numbers.minOption(ordering).size +
        numbers.maxByOption(identity[Int])(ordering).size +
        numbers.minByOption(identity[Int])(ordering).size
    val buffer = numbers.toBuffer
    val buffered =
      buffer.reduceRight((value, total) => total + value) +
        buffer.sliding(2, 1).size + buffer.view.size
    aggregated + optional + buffered
  }

  /**
   * The digest of one entry of the inventory.
   *
   * Both modes read it for every covered type, so the printed digest covers the inventory as well
   * as the common work and a type added or removed between the runs shows up as differing
   * digests. It names the type only as text, so reading it constructs nothing.
   *
   * @param category  which of the five routes into JSON the type takes
   * @param typeName  the simple name of the type
   * @param fqcn  the fully qualified name of the type
   * @return the digest of the entry
   */
  private def auditInventoryDigest(category: String, typeName: String, fqcn: String): Int =
    (0 until AuditValuesPerCase).foldLeft(0) { (total, index) =>
      total + s"$category/$typeName/$fqcn@${AuditSeed + index.toLong}".length
    }

  /**
   * How many instances of one case the encoding half of an audit run acquires: both halves of the
   * codec and the equality the round trip compares with.
   */
  private val CodecInstancesPerCase: Int = 3

  /**
   * How many codec-dependent instances this suite has acquired since it was constructed.
   *
   * Three of the five instances a case needs - both halves of the codec and the equality the
   * round trip compares with - are acquired where they are used, and each acquisition is counted
   * here. The audit asserts the count: none in the baseline mode, exactly
   * [[CodecInstancesPerCase]] per covered type in the codec mode.
   */
  private val codecAcquisitions: AtomicInteger = new AtomicInteger(0)

  /**
   * Acquires one codec-dependent instance, counting the acquisition.
   *
   * @param instance  the instance, evaluated by this call and not before it
   * @tparam A  the type of the instance
   * @return the instance
   */
  private def acquired[A](instance: => A): A = {
    val _ = codecAcquisitions.incrementAndGet()
    instance
  }

  /**
   * Builds the case of one covered type from the instances that type publishes.
   *
   * The five instances are implicit parameters, so requiring them here is itself a compile-time
   * part of the coverage: a type whose codec was forgotten cannot be listed as covered at all.
   *
   * All five arrive '''by name'''. The cases populate a list in the class body, built before the
   * constructor reaches the switch that decides what this run is; taken by value they would summon
   * every encoder and decoder of both modules there, and whatever the codec path loads on the way
   * would cancel out of the difference the audit measures. Each is bound to a value computed on
   * first use, and the three the encoding half needs are read through [[acquired]] so that the
   * reading is counted.
   *
   * @param category  which of the five routes into JSON the type takes
   * @param typeName  the simple name of the type
   * @param fqcn  the fully qualified name of the type
   * @param generatedInstance  the generator of values of the type
   * @param renderingInstance  the rendering used in the clue of a failure
   * @param encoderInstance  the encoding half of the type's codec
   * @param decoderInstance  the decoding half of the type's codec
   * @param equalityInstance  the equality the round trip compares with
   * @tparam A  the covered type
   * @return the case of that type
   */
  private def codecCase[A](
      category: String,
      typeName: String,
      fqcn: String)(
      implicit generatedInstance: => Arbitrary[A],
      renderingInstance: => Show[A],
      encoderInstance: => Encoder[A],
      decoderInstance: => Decoder[A],
      equalityInstance: => Eq[A]): CodecCase = {

    lazy val generated: Arbitrary[A] = generatedInstance
    lazy val rendering: Show[A] = renderingInstance
    lazy val encoder: Encoder[A] = acquired(encoderInstance)
    lazy val decoder: Decoder[A] = acquired(decoderInstance)
    lazy val equality: Eq[A] = acquired(equalityInstance)

    // The round trip is asserted value-first and in that direction only: a value is encoded, the
    // document read back, and the result compared with the value. The two notions of equality are
    // both checked and are required to agree - where they do not, the type's own instances
    // contradict each other and that is reported as such rather than papered over by choosing one.
    val roundTrip: () => Assertion = () => {
      forAll(generated.arbitrary) { (value: A) =>
        val json = encoder(value)
        decoder.decodeJson(json) match {
          case Right(back) =>
            val byEq = equality.eqv(value, back)
            val byEquals = back == value
            withClue(s"$typeName '${rendering.show(value)}' written as ${json.noSpaces}: ") {
              if (byEq != byEquals) {
                fail(
                  s"the two notions of equality disagree about the decoded value - Eq says " +
                    s"$byEq and == says $byEquals - which is a defect of $fqcn itself rather " +
                    s"than of its codec")
              } else {
                byEq shouldBe true
              }
            }
          case Left(failure) =>
            fail(
              s"$typeName could not decode the document it had just written, " +
                s"${json.noSpaces}: ${failure.getMessage}")
        }
      }
      succeed
    }

    // A value's document is a function of the value, so encoding what was decoded reproduces the
    // bytes that were decoded. This is the half of stability that holds of every type; the half
    // that concerns the order fields were supplied in is asserted per type, where it has content.
    val reEncode: () => Assertion = () => {
      forAll(generated.arbitrary) { (value: A) =>
        val json = encoder(value)
        decoder.decodeJson(json) match {
          case Right(back) =>
            withClue(s"$typeName re-encoded: ")(encoder(back).noSpaces shouldBe json.noSpaces)
          case Left(failure) =>
            fail(s"$typeName could not decode ${json.noSpaces}: ${failure.getMessage}")
        }
      }
      succeed
    }

    // What an audit run does with this case, in whichever of the two modes is asking. Both runs
    // share the digest of this entry, which is text: reading it constructs no value, initialises
    // no companion and builds no codec. Drawing a value, reading the codec, encoding and decoding
    // happen in the serializing run alone, so the classes each of those reaches are loaded there
    // only and sit in the difference the check inspects.
    val audit: Boolean => AuditTally = encodeAndDecode => {
      val digest = auditInventoryDigest(category, typeName, fqcn)
      if (encodeAndDecode) {
        val drawn = auditValues(generated.arbitrary, typeName)
        val roundTrips = drawn.count(value =>
          decoder.decodeJson(encoder(value)).exists(back => equality.eqv(back, value)))
        AuditTally(AuditValuesPerCase, digest, drawn.size, roundTrips)
      } else {
        AuditTally(AuditValuesPerCase, digest, 0, 0)
      }
    }

    CodecCase(category, typeName, fqcn, roundTrip, reEncode, audit)
  }

  //-------------------------------------------------------------------------
  /**
   * The covered half of the inventory: every type of both modules that carries a codec.
   *
   * This is the '''one''' place the covered inventory is written down: the printed report, the
   * per-type tests and the audit all read it, so the report cannot claim a type the tests do not
   * exercise and a test cannot exercise a type the report does not name. The order is by category
   * and then by name, which is the order the report prints.
   */
  private lazy val codecCases: List[CodecCase] = List(
    // The fourteen closed named families: a member is the JSON string of its name.
    codecCase[Currency](NamedEnumCategory, "Currency", "com.opengamma.strata.basics.currency.Currency"),
    codecCase[BusinessDayConvention](
      NamedEnumCategory,
      "BusinessDayConvention",
      "com.opengamma.strata.basics.date.BusinessDayConvention"),
    codecCase[RollConvention](
      NamedEnumCategory,
      "RollConvention",
      "com.opengamma.strata.basics.schedule.RollConvention"),
    codecCase[PeriodAdditionConvention](
      NamedEnumCategory,
      "PeriodAdditionConvention",
      "com.opengamma.strata.basics.date.PeriodAdditionConvention"),
    codecCase[DateSequence](NamedEnumCategory, "DateSequence", "com.opengamma.strata.basics.date.DateSequence"),
    codecCase[StubConvention](
      NamedEnumCategory,
      "StubConvention",
      "com.opengamma.strata.basics.schedule.StubConvention"),
    codecCase[FloatingRateType](
      NamedEnumCategory,
      "FloatingRateType",
      "com.opengamma.strata.basics.index.FloatingRateType"),
    codecCase[ValueAdjustmentType](
      NamedEnumCategory,
      "ValueAdjustmentType",
      "com.opengamma.strata.basics.value.ValueAdjustmentType"),
    codecCase[FailureReason](
      NamedEnumCategory,
      "FailureReason",
      "com.opengamma.strata.collect.result.FailureReason"),
    codecCase[IborIndex](NamedEnumCategory, "IborIndex", "com.opengamma.strata.basics.index.IborIndex"),
    codecCase[OvernightIndex](
      NamedEnumCategory,
      "OvernightIndex",
      "com.opengamma.strata.basics.index.OvernightIndex"),
    codecCase[PriceIndex](NamedEnumCategory, "PriceIndex", "com.opengamma.strata.basics.index.PriceIndex"),
    codecCase[FxIndex](NamedEnumCategory, "FxIndex", "com.opengamma.strata.basics.index.FxIndex"),
    codecCase[FloatingRateName](
      NamedEnumCategory,
      "FloatingRateName",
      "com.opengamma.strata.basics.index.FloatingRateName"),
    // The nine values identified by text, parsed by their own companions rather than by a family
    // lookup: a pair of currencies, a country, a calendar identifier, three periods, an
    // identifier and the two decimals.
    codecCase[CurrencyPair](
      ParsedStringCategory,
      "CurrencyPair",
      "com.opengamma.strata.basics.currency.CurrencyPair"),
    codecCase[Country](ParsedStringCategory, "Country", "com.opengamma.strata.basics.location.Country"),
    codecCase[HolidayCalendarId](
      ParsedStringCategory,
      "HolidayCalendarId",
      "com.opengamma.strata.basics.date.HolidayCalendarId"),
    codecCase[Tenor](ParsedStringCategory, "Tenor", "com.opengamma.strata.basics.date.Tenor"),
    codecCase[MarketTenor](ParsedStringCategory, "MarketTenor", "com.opengamma.strata.basics.date.MarketTenor"),
    codecCase[Frequency](ParsedStringCategory, "Frequency", "com.opengamma.strata.basics.schedule.Frequency"),
    codecCase[StandardId](ParsedStringCategory, "StandardId", "com.opengamma.strata.basics.StandardId"),
    codecCase[Decimal](ParsedStringCategory, "Decimal", "com.opengamma.strata.collect.Decimal"),
    codecCase[FixedScaleDecimal](
      ParsedStringCategory,
      "FixedScaleDecimal",
      "com.opengamma.strata.collect.FixedScaleDecimal"),
    // The two hand-written codecs, each for a family that mixes a name-based form with a
    // structural one and whose structural member has no public constructor to derive from.
    codecCase[DayCount](HandWrittenCategory, "DayCount", "com.opengamma.strata.basics.date.DayCount"),
    codecCase[HolidayCalendar](
      HandWrittenCategory,
      "HolidayCalendar",
      "com.opengamma.strata.basics.date.HolidayCalendar"),
    // The twenty-nine products whose shape is derived at compile time from their field shape,
    // twenty-eight of this module and `Failure` of `strata-collect`.
    codecCase[CurrencyAmount](
      SemiautoCategory,
      "CurrencyAmount",
      "com.opengamma.strata.basics.currency.CurrencyAmount"),
    codecCase[Money](SemiautoCategory, "Money", "com.opengamma.strata.basics.currency.Money"),
    codecCase[BigMoney](SemiautoCategory, "BigMoney", "com.opengamma.strata.basics.currency.BigMoney"),
    codecCase[Payment](SemiautoCategory, "Payment", "com.opengamma.strata.basics.currency.Payment"),
    codecCase[AdjustablePayment](
      SemiautoCategory,
      "AdjustablePayment",
      "com.opengamma.strata.basics.currency.AdjustablePayment"),
    codecCase[FxRate](SemiautoCategory, "FxRate", "com.opengamma.strata.basics.currency.FxRate"),
    codecCase[AdjustableDate](
      SemiautoCategory,
      "AdjustableDate",
      "com.opengamma.strata.basics.date.AdjustableDate"),
    codecCase[AdjustableDates](
      SemiautoCategory,
      "AdjustableDates",
      "com.opengamma.strata.basics.date.AdjustableDates"),
    codecCase[BusinessDayAdjustment](
      SemiautoCategory,
      "BusinessDayAdjustment",
      "com.opengamma.strata.basics.date.BusinessDayAdjustment"),
    codecCase[DaysAdjustment](
      SemiautoCategory,
      "DaysAdjustment",
      "com.opengamma.strata.basics.date.DaysAdjustment"),
    codecCase[PeriodAdjustment](
      SemiautoCategory,
      "PeriodAdjustment",
      "com.opengamma.strata.basics.date.PeriodAdjustment"),
    codecCase[TenorAdjustment](
      SemiautoCategory,
      "TenorAdjustment",
      "com.opengamma.strata.basics.date.TenorAdjustment"),
    codecCase[SequenceDate](SemiautoCategory, "SequenceDate", "com.opengamma.strata.basics.date.SequenceDate"),
    codecCase[PeriodicSchedule](
      SemiautoCategory,
      "PeriodicSchedule",
      "com.opengamma.strata.basics.schedule.PeriodicSchedule"),
    codecCase[SchedulePeriod](
      SemiautoCategory,
      "SchedulePeriod",
      "com.opengamma.strata.basics.schedule.SchedulePeriod"),
    codecCase[Schedule](SemiautoCategory, "Schedule", "com.opengamma.strata.basics.schedule.Schedule"),
    codecCase[ValueAdjustment](
      SemiautoCategory,
      "ValueAdjustment",
      "com.opengamma.strata.basics.value.ValueAdjustment"),
    codecCase[ValueDerivatives](
      SemiautoCategory,
      "ValueDerivatives",
      "com.opengamma.strata.basics.value.ValueDerivatives"),
    codecCase[ValueSchedule](SemiautoCategory, "ValueSchedule", "com.opengamma.strata.basics.value.ValueSchedule"),
    codecCase[ValueStep](SemiautoCategory, "ValueStep", "com.opengamma.strata.basics.value.ValueStep"),
    codecCase[ValueStepSequence](
      SemiautoCategory,
      "ValueStepSequence",
      "com.opengamma.strata.basics.value.ValueStepSequence"),
    codecCase[IborIndexObservation](
      SemiautoCategory,
      "IborIndexObservation",
      "com.opengamma.strata.basics.index.IborIndexObservation"),
    codecCase[OvernightIndexObservation](
      SemiautoCategory,
      "OvernightIndexObservation",
      "com.opengamma.strata.basics.index.OvernightIndexObservation"),
    codecCase[FxIndexObservation](
      SemiautoCategory,
      "FxIndexObservation",
      "com.opengamma.strata.basics.index.FxIndexObservation"),
    codecCase[PriceIndexObservation](
      SemiautoCategory,
      "PriceIndexObservation",
      "com.opengamma.strata.basics.index.PriceIndexObservation"),
    codecCase[CurrencyAmountArray](
      SemiautoCategory,
      "CurrencyAmountArray",
      "com.opengamma.strata.basics.currency.CurrencyAmountArray"),
    codecCase[MultiCurrencyAmountArray](
      SemiautoCategory,
      "MultiCurrencyAmountArray",
      "com.opengamma.strata.basics.currency.MultiCurrencyAmountArray"),
    codecCase[Rounding](SemiautoCategory, "Rounding", "com.opengamma.strata.basics.value.Rounding"),
    codecCase[Failure](SemiautoCategory, "Failure", "com.opengamma.strata.collect.result.Failure"),
    // The four values whose document form is stated outright rather than derived from a field
    // shape: the two numeric runs, the sorted collection of amounts and the matrix of rates.
    codecCase[DoubleArray](ExplicitCategory, "DoubleArray", "com.opengamma.strata.collect.array.DoubleArray"),
    codecCase[DoubleMatrix](ExplicitCategory, "DoubleMatrix", "com.opengamma.strata.collect.array.DoubleMatrix"),
    codecCase[MultiCurrencyAmount](
      ExplicitCategory,
      "MultiCurrencyAmount",
      "com.opengamma.strata.basics.currency.MultiCurrencyAmount"),
    codecCase[FxMatrix](ExplicitCategory, "FxMatrix", "com.opengamma.strata.basics.currency.FxMatrix"))

  //-------------------------------------------------------------------------
  /**
   * The printed inventory, built as one string and emitted by one call.
   *
   * One call, because suites may run beside one another and two calls could interleave. Every line
   * derives from the two inventory lists alone - no time, no hash code, no path, no iteration
   * order of a hashed collection - and both sections are sorted explicitly, so two runs print
   * byte-identical blocks.
   *
   * @return the report between its two markers, newline-separated
   */
  private def coverageReport: String = {
    val coveredLines = codecCases.map(entry => s"COVERED ${entry.category} ${entry.fqcn}").sorted
    val excludedLines = excludedTypes.map(entry => s"EXCLUDED ${entry.fqcn} ${entry.reason}").sorted
    val counts = List(
      s"COVERED-COUNT ${codecCases.size}",
      s"EXCLUDED-COUNT ${excludedTypes.size}")
    (List("CODEC-COVERAGE-BEGIN") ++ coveredLines ++ excludedLines ++ counts ++ List("CODEC-COVERAGE-END"))
      .mkString("\n")
  }

  //-------------------------------------------------------------------------
  // Phase B - the printed inventory.

  /**
   * Registers the report and the invariants of the inventory it prints: no type named twice, no
   * type both covered and excluded, the five categories holding their stated numbers, and the
   * report reproducible within a run. Registered first, so the block reaches the output before
   * the properties run.
   */
  private def registerCoverageReport(): Unit =
    test("codec coverage report") {
      val first = coverageReport
      println(first)
      val second = coverageReport
      withClue("the report is built from the inventory alone, so two builds agree: ")(second shouldBe first)

      val coveredNames = codecCases.map(entry => entry.fqcn)
      val excludedNames = excludedTypes.map(entry => entry.fqcn)
      withClue("no covered type is named twice: ")(coveredNames.distinct.size shouldBe coveredNames.size)
      withClue("no excluded type is named twice: ")(excludedNames.distinct.size shouldBe excludedNames.size)
      withClue("no type is both covered and excluded: ") {
        coveredNames.toSet.intersect(excludedNames.toSet) shouldBe Set.empty[String]
      }

      codecCases.count(entry => entry.category == NamedEnumCategory) shouldBe ExpectedNamedEnumTypes
      codecCases.count(entry => entry.category == ParsedStringCategory) shouldBe ExpectedParsedStringTypes
      codecCases.count(entry => entry.category == HandWrittenCategory) shouldBe ExpectedHandWrittenTypes
      codecCases.count(entry => entry.category == SemiautoCategory) shouldBe ExpectedSemiautoTypes
      codecCases.count(entry => entry.category == ExplicitCategory) shouldBe ExpectedExplicitTypes
      withClue("every covered type falls in one of the five categories: ") {
        codecCases.size shouldBe
          (ExpectedNamedEnumTypes + ExpectedParsedStringTypes + ExpectedHandWrittenTypes +
            ExpectedSemiautoTypes + ExpectedExplicitTypes)
      }

      val lines = first.split("\n").toList
      lines.head shouldBe "CODEC-COVERAGE-BEGIN"
      lines.last shouldBe "CODEC-COVERAGE-END"
      withClue("the block holds one line per type, two counts and two markers: ") {
        lines.size shouldBe (coveredNames.size + excludedNames.size + 4)
      }
      withClue("a reason is one line, because the report is read by lines: ") {
        excludedTypes.filter(entry => entry.reason.isEmpty || entry.reason.contains("\n")) shouldBe
          List.empty[ExcludedType]
      }
    }

  //-------------------------------------------------------------------------
  // Phase C - the round trip and the stability of the bytes.

  /**
   * Registers one round-trip test for each covered type, rather than one test over the list: a
   * failure then names the type in its own right, and a manifest row can join to it.
   */
  private def registerRoundTrips(): Unit =
    codecCases.foreach(entry => test(s"round-trip: ${entry.typeName}")(entry.roundTrip()))

  /**
   * Registers the property that a value's document depends on the value and on nothing else. One
   * test over every covered type, because it is one property rather than fifty-eight; the
   * per-type detail that could fail on its own is covered by the round trip above.
   */
  private def registerReEncodeStability(): Unit =
    test("byte-stability: re-encoding a decoded value reproduces its bytes") {
      val checked = codecCases.map(entry => withClue(s"${entry.typeName}: ")(entry.reEncode()))
      checked.size shouldBe codecCases.size
    }

  //-------------------------------------------------------------------------
  // Phase H - the mode switch of the reflection audit.

  /** The system property that selects an audit run in place of the suite. */
  private val AuditModeProperty: String = "codec.audit"

  /** The audit run that serializes nothing and constructs no value of a covered type. */
  private val BaselineMode: String = "baseline"

  /** The audit run that generates every covered value and encodes and decodes it. */
  private val CodecMode: String = "codec"

  private val AuditTestName: String = "codec audit: deterministic value enumeration"

  /**
   * Registers the single test of an audit run.
   *
   * The run performs the work the two modes have in common - [[auditCommonWork]], plus the digest
   * of every entry of the inventory - and, in the serializing mode only, generates the values of
   * every covered type and encodes and decodes them. Nothing else differs between the two runs,
   * which is what makes the difference between the classes they load attributable to
   * serialization. Three lines are printed, and every figure on them is asserted as well:
   *
   *   - the mode, the number of values the inventory plans for and the digest of the common work,
   *     both read from the inventory and the common work alone and therefore ''identical'' in the
   *     two runs;
   *   - the mode and how many codec-dependent instances the run acquired, `0` for the baseline run
   *     and three per covered type for the serializing one;
   *   - the mode and how many values of a covered type the run '''constructed''', `0` for the
   *     baseline run and one per planned value for the serializing one.
   *
   * Both modes finish on the same comparisons of integers, so each loads the same assertion
   * machinery, and what the difference attributes is the acquisition and execution of the codecs
   * and the initialisation of the companions publishing them: every covered value comes from a
   * generator of `Arbitraries`, which builds it through the type's own factories, and every
   * encoder, decoder and equality from the implicit instance published for that type - its
   * companion's, or for the two numeric types the codec support bound above - taken by name in
   * [[codecCase]] and read on first use. The baseline run draws from no covered generator and
   * reads none of those instances, so it initialises none of those companions, builds none of
   * their codecs and loads nothing that building one reaches; the two counts asserted to be zero
   * are the evidence.
   *
   * The boundary is the language runtime and the platform: [[auditCommonWork]] exercises the
   * generator, collection, date, format and zone machinery and reads the class constants of
   * [[AuditRuntimeClasses]] in ''both'' runs, so the classes that reach the reflection API for
   * reasons of their own stay out of the difference.
   *
   * The work runs while this method runs, before any test is registered, so the runner's reporting
   * thread cannot overlap it and put classes into the difference that no codec touched. The cost
   * is that a generator yielding nothing from every fixed seed aborts the run while the suite is
   * constructed rather than failing its test; the run fails either way, and the check that
   * consumes it reads the exit status.
   *
   * @param mode  the mode selected by the system property
   */
  private def registerAuditMode(mode: String): Unit = {
    val encodeAndDecode = mode == CodecMode
    val tally = codecCases.foldLeft(AuditTally(0, auditCommonWork(), 0, 0)) { (total, entry) =>
      total.combine(entry.audit(encodeAndDecode))
    }
    // read after the work, so they count what this run did and not what it was about to do
    val acquisitions = codecAcquisitions.get()
    val constructed = generatedValues.get()
    val expectedPlanned = AuditValuesPerCase * codecCases.size
    val expectedAcquisitions = if (encodeAndDecode) CodecInstancesPerCase * codecCases.size else 0
    test(AuditTestName) {
      println(s"CODEC-AUDIT-DIGEST $mode ${tally.planned} ${tally.digest}")
      println(s"CODEC-AUDIT-ACQUIRED $mode $acquisitions")
      println(s"CODEC-AUDIT-CONSTRUCTED $mode $constructed")
      tally.planned shouldBe expectedPlanned
      tally.planned should be >= codecCases.size
      withClue(
        s"the $mode run constructed $constructed value(s) of a covered type, where every such " +
          s"value is drawn from a generator of Arbitraries and every codec is the instance " +
          s"supplied by name to codecCase: ") {
        if (encodeAndDecode) {
          constructed should be >= codecCases.size
          tally.generated shouldBe constructed
          tally.roundTrips shouldBe constructed
        } else {
          constructed shouldBe 0
          tally.generated shouldBe 0
          tally.roundTrips shouldBe 0
        }
      }
      withClue(
        s"the $mode run acquired $acquisitions codec-dependent instance(s) for ${codecCases.size} " +
          s"covered type(s), where $expectedAcquisitions were expected: ") {
        acquisitions shouldBe expectedAcquisitions
      }
    }
  }

  /**
   * Registers the failure reported for a mode this suite does not define. A misspelled property
   * would otherwise run the whole suite under a class loading log and produce a difference of
   * every class the codecs touch, which reads exactly like a passing audit of the wrong thing.
   *
   * @param mode  the value the property carried
   */
  private def registerUnknownAuditMode(mode: String): Unit =
    test("codec audit: unrecognised mode") {
      fail(
        s"The system property '$AuditModeProperty' selects an audit run and accepts " +
          s"'$BaselineMode' or '$CodecMode', but it was set to '$mode'. Leave it unset to run " +
          s"the suite.")
    }

  //-------------------------------------------------------------------------
  // Phase D - the document forms, each pinned against a literal document.

  /** Asserts that a value writes exactly the document given. */
  private def encodesTo[A: Encoder](value: A, expected: String): Assertion =
    withClue(s"the document written for '$value': ")(value.asJson.noSpaces shouldBe expected)

  /**
   * Reads a document that is expected to describe a value.
   *
   * @param payload  the document
   * @param what  what the document was expected to describe, named in the failure
   * @tparam A  the type the document describes
   * @return the value read
   */
  private def decoded[A: Decoder](payload: String, what: String): A =
    decode[A](payload).fold(
      failure => fail(s"The document '$payload' should have described a $what: ${failure.getMessage}"),
      identity)

  /**
   * Asserts that a document is refused, and that the refusal says why.
   *
   * The expected text is matched as a fragment: the wording belongs to the factory that reported
   * it, and the shape the codec support wraps it in is asserted separately.
   *
   * @param payload  the document
   * @param expectedMessage  a fragment the refusal is expected to carry
   * @tparam A  the type the document failed to describe
   * @return the assertion
   */
  private def refused[A: Decoder](payload: String, expectedMessage: String): Assertion =
    decode[A](payload) match {
      case Left(failure) =>
        withClue(s"the refusal of '$payload': ")(failure.getMessage should include(expectedMessage))
      case Right(value) =>
        fail(s"The document '$payload' should have been refused but described: $value")
    }

  /** The refusal of a payload that is neither a number nor one of the three accepted strings. */
  private val TaggedDoubleMessage: String =
    "Expected a JSON number or one of the strings NaN, Infinity, -Infinity"

  /** The refusal of text that is not the constant name of a day of the week. */
  private val DayNameMessage: String = "Expected one of the day names"

  /** The document of the calendar fixture carrying two holidays of its own. */
  private val TestCalendarDocument: String =
    """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":2014,""" +
      """"holidays":["2014-07-14","2014-07-16"],"workingWeekendDays":[]}}"""

  /** The document of the calendar declaring a weekend date to be a business day. */
  private val WorkingWeekendDocument: String =
    """{"Immutable":{"id":"XCAL","weekendDays":["SATURDAY","SUNDAY"],"startYear":2020,""" +
      """"holidays":["2020-01-01","2020-12-25"],"workingWeekendDays":["2020-03-07"]}}"""

  /** A modified-following adjustment over the weekend-only calendar. */
  private lazy val satSunModifiedFollowing: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)

  /** An Ibor observation over an index whose calendars the built-in data resolves. */
  private lazy val iborObservation: IborIndexObservation =
    required(
      IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, LocalDate.of(2014, 6, 30), refData),
      "IborIndexObservation of GBP-LIBOR-3M on 2014-06-30")

  /** An overnight observation over an index whose calendars the built-in data resolves. */
  private lazy val overnightObservation: OvernightIndexObservation =
    required(
      OvernightIndexObservation.of(OvernightIndices.USD_FED_FUND, LocalDate.of(2016, 2, 22), refData),
      "OvernightIndexObservation of USD-FED-FUND on 2016-02-22")

  /** An FX observation over an index whose calendars the built-in data resolves. */
  private lazy val fxObservation: FxIndexObservation =
    required(
      FxIndexObservation.of(FxIndices.GBP_USD_WM, LocalDate.of(2016, 2, 22), refData),
      "FxIndexObservation of GBP/USD-WM on 2016-02-22")

  /**
   * A connected matrix of seven rates whose fifth rate is zero.
   *
   * The zero is the point of it: the reciprocal recorded for a zero rate is infinite, so the
   * matrix holds a value JSON has no syntax for and exercises the tagged form inside a structure
   * of two dimensions. One rate arrives before it can be placed, which the factory holds back.
   */
  private lazy val zeroRateMatrix: FxMatrix =
    required(
      FxMatrix.ofRates(
        Vector(
          CurrencyPair.of(Currency.GBP, Currency.USD) -> 1.6d,
          CurrencyPair.of(Currency.EUR, Currency.USD) -> 1.4d,
          CurrencyPair.of(Currency.CHF, Currency.AUD) -> 1.2d,
          CurrencyPair.of(Currency.SEK, Currency.AUD) -> 0.1d,
          CurrencyPair.of(Currency.JPY, Currency.CAD) -> 0.0d,
          CurrencyPair.of(Currency.EUR, Currency.CHF) -> 1.2d,
          CurrencyPair.of(Currency.JPY, Currency.USD) -> 0.008d)),
      "the connected matrix holding a zero rate")

  /**
   * Asserts that every member of a named family is the JSON string of its name.
   *
   * The members come from the family's own `values`, so the assertion covers whatever the family
   * holds rather than a list transcribed here - all forty-five roll conventions and all two
   * hundred and seventy-one Ibor indices, without a name of either appearing in this file. Those
   * are memberships rather than counts of published constants: `IborIndices` publishes one
   * hundred and thirteen of the two hundred and seventy-one, and `values` sweeps all of them.
   *
   * @param values  the members of the family
   * @param typeName  the simple name of the family, named in a failure
   * @tparam A  the type of the members
   * @return the assertion
   */
  private def namedFamilyIsItsName[A <: Named: Encoder: Decoder](
      values: NonEmptyList[A],
      typeName: String): Assertion = {

    val checked = values.toList.map { value =>
      withClue(s"$typeName member '${value.name}': ") {
        value.asJson shouldBe Json.fromString(value.name)
        decoded[A](value.asJson.noSpaces, s"a $typeName") shouldBe value
      }
    }
    withClue(s"$typeName has members to check: ")(checked.size shouldBe values.length)
  }

  /**
   * Asserts that a named family refuses text that names no member of it.
   *
   * @param typeName  the simple name of the family, named in a failure
   * @tparam A  the type of the members
   * @return the assertion
   */
  private def refusesUnknownName[A: Decoder](typeName: String): Assertion =
    decode[A](""""XYZ-NAMES-NOTHING"""") match {
      case Left(_) => succeed
      case Right(value) => fail(s"$typeName accepted text that names no member of it: $value")
    }

  /**
   * Asserts that two calendars answer alike on every date of a range of years.
   *
   * Two calendars of one identifier are equal whatever dates they hold, so a codec that dropped
   * every holiday would still satisfy `decode(encode(a)) == a`; walking the years compares what
   * the calendars actually say.
   *
   * @param expected  the calendar that was encoded
   * @param actual  the calendar that was decoded
   * @param firstYear  the first year of the range to walk, inclusive
   * @param lastYear  the last year of the range to walk, inclusive
   * @return the assertion
   */
  private def sameHolidays(
      expected: HolidayCalendar,
      actual: HolidayCalendar,
      firstYear: Int,
      lastYear: Int): Assertion = {

    val dates = Iterator
      .iterate(LocalDate.of(firstYear, 1, 1))(date => date.plusDays(1L))
      .takeWhile(date => date.getYear <= lastYear)
      .toList
    val differing = dates.filter(date =>
      expected.isHoliday(date) != actual.isHoliday(date) ||
        expected.isBusinessDay(date) != actual.isBusinessDay(date))
    withClue(s"the dates '${expected.name}' and '${actual.name}' disagree about: ") {
      differing shouldBe List.empty[LocalDate]
    }
  }

  /**
   * The years every calendar the generator produces falls within, with a year of margin: the
   * generator draws its first year from a fixed window and spans three years from it, and a year
   * on each side is where both calendars fall back to their weekend and must still agree.
   */
  private val GeneratedCalendarFirstYear: Int = 2009

  private val GeneratedCalendarLastYear: Int = 2033

  //-------------------------------------------------------------------------
  /**
   * Registers the shape a closed family takes, and the absence of a discriminator.
   *
   * A closed family is a single-key object whose key names the member, which is the default the
   * derivation produces. The absence of a discriminator field is worth asserting in its own
   * right, because a discriminator layout would require a library this build does not depend on.
   */
  private def registerFamilyShapes(): Unit = {
    test("shape: a closed family is a single-key object naming its member") {
      encodesTo[Rounding](NoRounding, """{"NoRounding":{}}""")
      encodesTo[Rounding](
        requiredNec(Rounding.ofFractionalDecimalPlaces(2, 0), "a half-up rounding to two places"),
        """{"HalfUp":{"decimalPlaces":2,"fraction":0}}""")
      encodesTo[Failure](
        Failure.MissingData("No holiday calendar"),
        """{"MissingData":{"message":"No holiday calendar","attributes":{}}}""")
    }

    test("shape: no closed family carries a discriminator field") {
      val documents = List[Json](
        (NoRounding: Rounding).asJson,
        (Failure.MissingData("gone"): Failure).asJson,
        (testCalendar: HolidayCalendar).asJson,
        DayCount.ofBus252(StandardHolidayCalendars.EUTA).asJson)
      documents.foreach { document =>
        val keys = document.asObject.map(fields => fields.keys.toList).getOrElse(List.empty[String])
        withClue(s"the wrapper of ${document.noSpaces}: ") {
          keys.size shouldBe 1
          keys.exists(key => key == "type" || key == "_type") shouldBe false
        }
      }
      // A field genuinely named `type` exists, on the value adjustment, and is not a
      // discriminator: its value is a member of a named family rather than the name of a
      // constructor.
      encodesTo(ValueAdjustment.ofReplace(1.5d), """{"modifyingValue":1.5,"type":"Replace"}""")
    }
  }

  /**
   * Registers the treatment of a field that holds no value, and of one that holds nothing. The two
   * differ visibly: a field holding no value is left out of the document, while a field holding an
   * empty collection is written as one, because the policy removes absent values rather than empty
   * ones.
   */
  private def registerOptionalFieldShapes(): Unit = {
    test("shape: a field holding no value is left out of the document") {
      val minimal = requiredNec(
        PeriodicSchedule.of(
          LocalDate.of(2014, 6, 17),
          LocalDate.of(2014, 9, 17),
          Frequency.P1M,
          satSunModifiedFollowing),
        "a schedule definition declaring none of its optional properties")
      val document = minimal.asJson
      val keys = document.asObject.map(fields => fields.keys.toList).getOrElse(List.empty[String])
      withClue(s"the properties written for ${document.noSpaces}: ") {
        keys shouldBe List("startDate", "endDate", "frequency", "businessDayAdjustment")
      }
      encodesTo(
        minimal,
        """{"startDate":"2014-06-17","endDate":"2014-09-17","frequency":"P1M",""" +
          """"businessDayAdjustment":{"convention":"ModifiedFollowing","calendar":"Sat/Sun"}}""")
    }

    test("shape: an absent property and one written as JSON's absent value read alike") {
      val minimal = requiredNec(
        PeriodicSchedule.of(
          LocalDate.of(2014, 6, 17),
          LocalDate.of(2014, 9, 17),
          Frequency.P1M,
          satSunModifiedFollowing),
        "a schedule definition declaring none of its optional properties")
      val withoutKeys = minimal.asJson.noSpaces
      // the second document states every optional property as JSON's literal for an absent
      // value. It is assembled from that literal rather than written out as text, so the
      // absent-value spelling in this file is the one the JSON library itself defines and the
      // document cannot drift from what a derived encoder would have produced.
      val optionalProperties = List(
        "startDateBusinessDayAdjustment",
        "endDateBusinessDayAdjustment",
        "stubConvention",
        "rollConvention",
        "firstRegularStartDate",
        "lastRegularEndDate",
        "overrideStartDate")
      val withAbsentValues = minimal.asJson.deepMerge(
        Json.obj(optionalProperties.map(property => property -> Json.Null): _*))
      withClue("the second document states each optional property explicitly: ") {
        withAbsentValues.asObject.map(fields => fields.keys.size).getOrElse(0) shouldBe
          (4 + optionalProperties.size)
      }
      decoded[PeriodicSchedule](withoutKeys, "a schedule definition") shouldBe minimal
      decoded[PeriodicSchedule](withAbsentValues.noSpaces, "a schedule definition") shouldBe minimal
    }

    test("shape: an empty collection is written rather than dropped") {
      // a schedule of no steps writes an empty list and a failure with no attributes an empty
      // object, so a reader can tell "none" from "not stated"; `ALWAYS_1` is the constant for the
      // schedule `ValueSchedule.of(1.0d)` builds, read here in place of that factory's outcome
      encodesTo(ValueSchedule.ALWAYS_1, """{"initialValue":1.0,"steps":[]}""")
      encodesTo[Failure](Failure.Other("nothing to add"), """{"Other":{"message":"nothing to add","attributes":{}}}""")
      encodesTo[HolidayCalendar](testCalendar, TestCalendarDocument)
    }
  }

  /**
   * Registers the single policy for a double, and the two numeric shapes. The three values JSON
   * has no syntax for are carried as strings and exactly those three spellings are read back: a
   * differently cased tag, a tag with a sign it does not use, and the digits of a number delivered
   * as text are all refused.
   */
  private def registerNumericShapes(): Unit = {
    test("shape: an array of doubles tags the three values JSON cannot express") {
      encodesTo(
        DoubleArray.of(1.0d, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity),
        """[1.0,"NaN","Infinity","-Infinity"]""")
      val read = decoded[DoubleArray]("""[1,"NaN","Infinity","-Infinity"]""", "an array of doubles")
      read.get(0) shouldBe 1.0d
      read.get(1).isNaN shouldBe true
      read.get(2) shouldBe Double.PositiveInfinity
      read.get(3) shouldBe Double.NegativeInfinity
    }

    test("shape: the numeric runs are arrays, and a matrix an array of rows") {
      encodesTo(DoubleArray.of(1.0d, 2.0d), "[1.0,2.0]")
      encodesTo(DoubleArray.EMPTY, "[]")
      encodesTo(DoubleMatrix.of(2, 2, 1.0d, 2.0d, 3.0d, 4.0d), "[[1.0,2.0],[3.0,4.0]]")
      encodesTo(DoubleMatrix.of(), "[]")
      encodesTo(ValueDerivatives.of(1.0d, DoubleArray.of(1.0d, 2.0d)), """{"value":1.0,"derivatives":[1.0,2.0]}""")
    }

    test("round-trip: ValueDerivatives carrying the longest array a document may state") {
      // How long an array a document may state is a published figure, `Codecs.MaximumArrayElements`,
      // and the property this test pins is that the figure is not in the way of the serialization
      // contract: the ''largest'' array the codec support will read round-trips through a nested
      // product, and a document stating one element more is refused before an element is read. Both
      // halves matter, and they are asserted here together, because a ceiling that refused a length
      // the encoder produces would make `decode(encode(x))` fail for a value this library itself
      // creates, while no ceiling at all would let a document decide how much is allocated.
      //
      // The value is nested rather than bare, which is the half no other test covers: the array is
      // a field of a derived product, so both the reading and the refusal arrive from inside the
      // product's decoder, positioned at the field the array occupies. The value itself is the
      // realistic one - a function and its derivatives, as produced by a differentiation of a curve
      // with one point per basis point.
      //
      // One such value, and one only: the document holds a million numbers and every one of them is
      // a JSON value while it is being read, so this is the largest payload in the suite by two
      // orders of magnitude and there is nothing a second one would add. The oversized document
      // beside it is built from a single shared element rather than from a million distinct ones,
      // since a payload refused before its elements are read needs no readable element at all, and
      // filling a million positions with a freshly built value would cost the heap this suite runs
      // under for nothing.
      //
      // The round trip is asserted through the JSON tree rather than through its text, because the
      // ceiling is applied to the tree - it measures the array of the cursor before reading it - so
      // this is the path that exercises it.
      val elements = 1 << 20
      elements shouldBe Codecs.MaximumArrayElements
      val value = ValueDerivatives.of(2.5d, DoubleArray.tabulate(elements)(index => index.toDouble))
      val document = value.asJson
      withClue("the document is an object whose 'derivatives' property holds every element: ") {
        document.asObject
          .flatMap(fields => fields("derivatives"))
          .flatMap(array => array.asArray)
          .map(array => array.size)
          .getOrElse(0) shouldBe elements
      }
      implicitly[Decoder[ValueDerivatives]].decodeJson(document) match {
        case Right(restored) =>
          // compared property by property rather than as a whole: the two values are equal or
          // they are not, and a failure that rendered a million elements into its message would
          // report the defect by burying it
          restored.value shouldBe value.value
          restored.derivatives.size shouldBe elements
          withClue("every element of the decoded array is the element that was written: ") {
            (restored.derivatives == value.derivatives) shouldBe true
          }
          withClue("and the decoded value is the value that was encoded: ")((restored == value) shouldBe true)
        case Left(failure) =>
          fail(
            s"a ValueDerivatives carrying $elements derivatives could not be decoded from the " +
              s"document it had just written: ${failure.getMessage}")
      }
      // and one element more than the published ceiling is refused, at the field the array
      // occupies, before an element of it is read - the elements are a single shared JSON string
      // that no double could hold, so a refusal naming the count is a refusal reached without them
      val beyondCeiling = Json.obj(
        "value" -> Json.fromDoubleOrNull(2.5d),
        "derivatives" -> repeatedElements(elements + 1))
      implicitly[Decoder[ValueDerivatives]].decodeJson(beyondCeiling) match {
        case Left(failure) =>
          withClue(s"the refusal of the oversized document was '${failure.getMessage}': ") {
            failure.message shouldBe
              s"Expected at most ${Codecs.MaximumArrayElements} elements in the array, " +
                s"but the payload states ${elements + 1}"
            failure.history shouldBe List[CursorOp](CursorOp.DownField("derivatives"))
          }
        case Right(_) =>
          fail(
            s"a document stating ${elements + 1} derivatives is beyond " +
              s"Codecs.MaximumArrayElements and should have been refused")
      }
    }

    test("shape: validity is decided by the factory and not by the codec") {
      // the sharpest pair in the suite: one document carries a value outside the finite range
      // and is accepted, the other carries a different such value and is refused, and the codec
      // treats the two identically - what differs is what the factory of the type admits
      val infinite = decoded[CurrencyAmount]("""{"currency":"GBP","amount":"Infinity"}""", "an amount")
      infinite.amount shouldBe Double.PositiveInfinity
      refused[CurrencyAmount]("""{"currency":"GBP","amount":"NaN"}""", "must not be NaN")
      // and the normalisation the factory applies is visible in what the value writes
      encodesTo(amount(Currency.GBP, -0.0d), """{"currency":"GBP","amount":0.0}""")
    }
  }

  /**
   * Registers the forms of the date and time fields, asserted through the types that carry them.
   *
   * Four of the five are taken from the JSON library unchanged and one - the day of the week -
   * from this build's own codec support, and both are asserted through a domain type rather than
   * in isolation.
   */
  private def registerDateTimeShapes(): Unit = {
    test("shape: the date and time properties take their ISO forms") {
      encodesTo(
        AdjustableDate.of(LocalDate.of(2024, 1, 31)),
        """{"unadjusted":"2024-01-31","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      encodesTo(
        PriceIndexObservation.of(PriceIndices.GB_RPI, YearMonth.of(2024, 1)),
        """{"index":"GB-RPI","fixingMonth":"2024-01"}""")
      encodesTo(
        requiredNec(
          PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.NONE, BusinessDayAdjustment.NONE),
          "a three-month period adjustment"),
        """{"period":"P3M","additionConvention":"None",""" +
          """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
    }

    test("shape: a day of the week is its constant name and nothing else") {
      encodesTo[HolidayCalendar](testCalendar, TestCalendarDocument)
      Codecs.dayOfWeekCodec(DayOfWeek.SATURDAY) shouldBe Json.fromString("SATURDAY")
      val wrongCase =
        """{"Immutable":{"id":"XCAL","weekendDays":["Saturday"],"startYear":2020,""" +
          """"holidays":["2020-01-01"],"workingWeekendDays":[]}}"""
      refused[HolidayCalendar](wrongCase, DayNameMessage)
      refused[HolidayCalendar](wrongCase.replace("Saturday", "SAT"), DayNameMessage)
      refused[HolidayCalendar](wrongCase.replace("Saturday", "saturday"), DayNameMessage)
    }

    test("shape: an Ibor index is a bare name, so its time and zone never appear") {
      val document = (IborIndices.GBP_LIBOR_3M: IborIndex).asJson
      document shouldBe Json.fromString("GBP-LIBOR-3M")
      document.isString shouldBe true
      document.noSpaces should not include "fixingTime"
      document.noSpaces should not include "Europe/London"
    }
  }

  /**
   * Registers the identity of every named value: the name it has always had. The general case is
   * driven from each family's own members, so the assertion covers whatever the family holds; the
   * handful of identities written out are the ones the plan pins by name.
   */
  private def registerNamedShapes(): Unit = {
    test("shape: every member of every named family is the string of its name") {
      namedFamilyIsItsName(Currency.values, "Currency")
      namedFamilyIsItsName(BusinessDayConvention.values, "BusinessDayConvention")
      namedFamilyIsItsName(RollConvention.values, "RollConvention")
      namedFamilyIsItsName(PeriodAdditionConvention.values, "PeriodAdditionConvention")
      namedFamilyIsItsName(DateSequence.values, "DateSequence")
      namedFamilyIsItsName(StubConvention.values, "StubConvention")
      namedFamilyIsItsName(FloatingRateType.values, "FloatingRateType")
      namedFamilyIsItsName(ValueAdjustmentType.values, "ValueAdjustmentType")
      namedFamilyIsItsName(FailureReason.values, "FailureReason")
      namedFamilyIsItsName(IborIndex.values, "IborIndex")
      namedFamilyIsItsName(OvernightIndex.values, "OvernightIndex")
      namedFamilyIsItsName(PriceIndex.values, "PriceIndex")
      namedFamilyIsItsName(FxIndex.values, "FxIndex")
      namedFamilyIsItsName(FloatingRateName.values, "FloatingRateName")
    }

    test("shape: the name identities the migration plan pins") {
      encodesTo(DayCounts.ACT_365F, "\"Act/365F\"")
      encodesTo(IborIndices.GBP_LIBOR_3M, "\"GBP-LIBOR-3M\"")
      encodesTo(CurrencyPair.of(Currency.EUR, Currency.USD), "\"EUR/USD\"")
      encodesTo(Frequency.P3M, "\"P3M\"")
      encodesTo(Tenor.TENOR_3M, "\"3M\"")
      encodesTo(HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY), "\"GBLO+USNY\"")
    }

    test("shape: a tenor is written without the period prefix and read with or without it") {
      encodesTo(Tenor.TENOR_3M, "\"3M\"")
      decoded[Tenor]("\"3M\"", "a tenor") shouldBe Tenor.TENOR_3M
      decoded[Tenor]("\"P3M\"", "a tenor") shouldBe Tenor.TENOR_3M
      encodesTo(Tenor.TENOR_1W, "\"1W\"")
      encodesTo(Tenor.TENOR_1Y, "\"1Y\"")
    }
  }

  /**
   * Registers the one family whose members do not all take the same form: twenty-one day counts
   * are named and nothing else, while the twenty-second carries a calendar, which is part of the
   * value and has to survive the round trip even where an application built that calendar itself.
   */
  private def registerDayCountShapes(): Unit = {
    test("shape: a standard day count is the string of its name") {
      encodesTo(DayCounts.ACT_364, "\"Act/364\"")
      encodesTo(DayCounts.ACT_360, "\"Act/360\"")
      decoded[DayCount]("\"Act/364\"", "a day count") shouldBe DayCounts.ACT_364
    }

    test("shape: DayCount Bus252 carries its calendar") {
      encodesTo(
        DayCount.ofBus252(StandardHolidayCalendars.EUTA),
        """{"Bus252":{"name":"Bus/252 EUTA","calendar":"EUTA"}}""")
      encodesTo(
        DayCount.ofBus252(StandardHolidayCalendars.BRBD),
        """{"Bus252":{"name":"Bus/252 BRBD","calendar":"BRBD"}}""")
    }

    test("shape: a Bus252 name resolves its calendar against the built-in set") {
      decoded[DayCount]("\"Bus/252 EUTA\"", "a day count") shouldBe
        DayCount.ofBus252(StandardHolidayCalendars.EUTA)
    }

    test("shape: a Bus252 day count over a calendar of its own survives the round trip") {
      // this is the case the string form cannot carry, and the reason the structural form exists
      val custom = DayCount.ofBus252(workingWeekendCalendar)
      val document = custom.asJson.noSpaces
      document should include("""{"Immutable":""")
      document should include(""""name":"Bus/252 XCAL"""")
      withClue("the dates of the nested calendar travel with the day count: ") {
        document should include(""""holidays":["2020-01-01","2020-12-25"]""")
        document should include(""""workingWeekendDays":["2020-03-07"]""")
      }
      decoded[DayCount](document, "a day count") shouldBe custom
    }
  }

  /**
   * Registers the hybrid form of a holiday calendar, and the structural check its equality needs.
   *
   * A calendar this library defines is its name, because the name locates the same calendar again;
   * any other is written structurally, because its dates are the whole of what it is. Two
   * calendars of one identifier are equal, so the round trip of the structural form is checked by
   * walking the years as well, which [[sameHolidays]] does.
   */
  private def registerHolidayCalendarShapes(): Unit = {
    test("shape: a calendar this library defines is its identifier") {
      encodesTo[HolidayCalendar](NoHolidays, "\"NoHolidays\"")
      encodesTo[HolidayCalendar](SatSun, "\"Sat/Sun\"")
      encodesTo[HolidayCalendar](FriSat, "\"Fri/Sat\"")
      encodesTo[HolidayCalendar](ThuFri, "\"Thu/Fri\"")
      encodesTo[HolidayCalendar](StandardHolidayCalendars.GBLO, "\"GBLO\"")
      HolidayCalendarIds.NO_HOLIDAYS.name shouldBe "NoHolidays"
      HolidayCalendarIds.SAT_SUN.name shouldBe "Sat/Sun"
    }

    test("shape: a calendar of its own holidays is written structurally") {
      encodesTo[HolidayCalendar](testCalendar, TestCalendarDocument)
      val fields = (testCalendar: HolidayCalendar).asJson.asObject
        .flatMap(wrapper => wrapper("Immutable"))
        .flatMap(payload => payload.asObject)
        .map(payload => payload.keys.toList)
        .getOrElse(List.empty[String])
      fields shouldBe List("id", "weekendDays", "startYear", "holidays", "workingWeekendDays")
      // the first year is derived from the holidays rather than chosen: a calendar whose
      // holidays fall later declares a later first year
      val later = ImmutableHolidayCalendar.of(
        HolidayCalendarId.of("Test2"),
        List(LocalDate.of(2016, 5, 2)),
        DayOfWeek.SATURDAY,
        DayOfWeek.SUNDAY)
      (later: HolidayCalendar).asJson.noSpaces should include(""""startYear":2016""")
    }

    test("shape: a calendar declaring a weekend date to be a business day") {
      encodesTo[HolidayCalendar](workingWeekendCalendar, WorkingWeekendDocument)
      decoded[HolidayCalendar](WorkingWeekendDocument, "a calendar") shouldBe
        (workingWeekendCalendar: HolidayCalendar)
      sameHolidays(
        workingWeekendCalendar,
        decoded[HolidayCalendar](WorkingWeekendDocument, "a calendar"),
        2019,
        2021)
    }

    test("shape: a composite calendar names its two parts") {
      encodesTo[HolidayCalendar](
        StandardHolidayCalendars.GBLO.combinedWith(StandardHolidayCalendars.USNY),
        """{"Combined":{"a":"GBLO","b":"USNY"}}""")
      encodesTo[HolidayCalendar](
        StandardHolidayCalendars.GBLO.linkedWith(StandardHolidayCalendars.USNY),
        """{"Linked":{"a":"GBLO","b":"USNY"}}""")
    }

    test("shape: a composite identifier reads as a composite calendar and then writes structurally") {
      // the round trip of this suite is value-first for exactly this reason: the forms a calendar
      // accepts are wider than the forms it writes, and asserting the document-first direction
      // would forbid the leniency the decoder exists to provide
      val read = decoded[HolidayCalendar]("\"GBLO+USNY\"", "a calendar")
      read shouldBe StandardHolidayCalendars.GBLO.combinedWith(StandardHolidayCalendars.USNY)
      read.asJson.noSpaces shouldBe """{"Combined":{"a":"GBLO","b":"USNY"}}"""
    }

    test("shape: every generated calendar of its own holidays keeps them through the round trip") {
      // equality is by identifier alone, so this walk of the years is the assertion that has
      // content: a codec that dropped every holiday would satisfy the equality round trip
      forAll(genCustomHolidayCalendar) { (calendar: ImmutableHolidayCalendar) =>
        val document = (calendar: HolidayCalendar).asJson.noSpaces
        val read = decoded[HolidayCalendar](document, "a calendar")
        read shouldBe (calendar: HolidayCalendar)
        sameHolidays(calendar, read, GeneratedCalendarFirstYear, GeneratedCalendarLastYear)
      }
      succeed
    }

    test("shape: a calendar of its own holidays under a built-in identifier stays structural") {
      // The choice between the two forms is made by identity against the built-in set rather
      // than by equality with it, and this is the case that makes the difference: an application
      // supplying its own `GBLO` is written structurally and keeps its holidays. Were the test
      // equality, such a calendar would be written as the bare name and read back as this
      // library's London calendar, silently losing everything it declared.
      val shadowing = ImmutableHolidayCalendar.of(
        HolidayCalendarIds.GBLO,
        List(LocalDate.of(2020, 1, 1)),
        DayOfWeek.SATURDAY,
        DayOfWeek.SUNDAY)
      encodesTo[HolidayCalendar](
        shadowing,
        """{"Immutable":{"id":"GBLO","weekendDays":["SATURDAY","SUNDAY"],"startYear":2020,""" +
          """"holidays":["2020-01-01"],"workingWeekendDays":[]}}""")
      val read = decoded[HolidayCalendar]((shadowing: HolidayCalendar).asJson.noSpaces, "a calendar")
      read shouldBe (shadowing: HolidayCalendar)
      withClue("the calendar read back is the application's own and not this library's: ") {
        (read eq StandardHolidayCalendars.GBLO) shouldBe false
      }
      sameHolidays(shadowing, read, 2019, 2021)
    }
  }

  /**
   * Registers the forms of the currency values, and the one distinction between them: an amount
   * carries a number while the two money types carry the text of a decimal, which is the
   * representation they exist for, so it is pinned in one test where a reader sees both.
   */
  private def registerCurrencyShapes(): Unit = {
    test("shape: MultiCurrencyAmount lists its amounts in currency order") {
      val value = required(
        MultiCurrencyAmount.of(amount(Currency.USD, 2.0d), amount(Currency.GBP, 1.0d)),
        "an amount of sterling and dollars")
      encodesTo(value, """{"amounts":[{"currency":"GBP","amount":1.0},{"currency":"USD","amount":2.0}]}""")
    }

    test("shape: money carries the text of a decimal where an amount carries a number") {
      encodesTo(Money.of(Currency.GBP, decimal("12.34")), """{"currency":"GBP","amount":"12.34"}""")
      encodesTo(BigMoney.of(Currency.GBP, decimal("12.34")), """{"currency":"GBP","amount":"12.34"}""")
      encodesTo(amount(Currency.GBP, 12.34d), """{"currency":"GBP","amount":12.34}""")
      encodesTo(decimal("12.34"), "\"12.34\"")
    }

    test("shape: an amount nested in a payment keeps its own form") {
      encodesTo(
        Payment.of(amount(Currency.GBP, 100.0d), LocalDate.of(2024, 1, 31)),
        """{"value":{"currency":"GBP","amount":100.0},"date":"2024-01-31"}""")
      encodesTo(
        AdjustablePayment.of(amount(Currency.GBP, 100.0d), LocalDate.of(2024, 1, 31)),
        """{"value":{"currency":"GBP","amount":100.0},"date":{"unadjusted":"2024-01-31",""" +
          """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}}""")
    }
  }

  /**
   * Registers the form of a matrix of rates, and what a document of one is checked for. The checks
   * are structural and only structural, which is a decision rather than an omission: the factory
   * accepts whatever rates it is given, so a matrix this decoder refused for being unrealistic
   * would be one that could be built but not read back.
   */
  private def registerFxMatrixShapes(): Unit = {
    test("shape: FxMatrix writes its currencies in matrix order and its rates tagged") {
      encodesTo(
        FxMatrix.of(Currency.GBP, Currency.USD, 1.6d),
        """{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]}""")
      val document = zeroRateMatrix.asJson
      val currencies = document.asObject
        .flatMap(fields => fields("currencies"))
        .flatMap(array => array.as[List[String]].toOption)
        .getOrElse(List.empty[String])
      currencies shouldBe List("GBP", "USD", "EUR", "CHF", "AUD", "SEK", "JPY", "CAD")
      withClue("the reciprocal of the zero rate is infinite and is carried as a tagged string: ") {
        document.noSpaces should include("\"Infinity\"")
      }
      decoded[FxMatrix](document.noSpaces, "a matrix of rates") shouldBe zeroRateMatrix
    }

    test("shape: a matrix whose rates neither reciprocate nor triangulate is accepted") {
      val document = """{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[1.9,1.0]]}"""
      val matrix = decoded[FxMatrix](document, "a matrix of rates")
      matrix.asJson.noSpaces shouldBe document
    }
  }

  /**
   * Registers the form of the observations, and the check that makes their round trip mean
   * something.
   *
   * Three of the four are rebuilt from the two properties a document really determines, and the
   * derived properties the document states are compared against what the index derives; without
   * that, a document stating another index's dates would decode into a value comparing equal to
   * the one encoded, because the equality of those types ignores the derived properties.
   */
  private def registerObservationShapes(): Unit = {
    test("shape: a derived observation states the values its index derives") {
      encodesTo(
        iborObservation,
        """{"index":"GBP-LIBOR-3M","fixingDate":"2014-06-30","effectiveDate":"2014-06-30",""" +
          """"maturityDate":"2014-09-30","yearFraction":0.25205479452054796}""")
      encodesTo(
        overnightObservation,
        """{"index":"USD-FED-FUND","fixingDate":"2016-02-22","publicationDate":"2016-02-23",""" +
          """"effectiveDate":"2016-02-22","maturityDate":"2016-02-23",""" +
          """"yearFraction":0.002777777777777778}""")
      encodesTo(fxObservation, """{"index":"GBP/USD-WM","fixingDate":"2016-02-22","maturityDate":"2016-02-24"}""")
    }

    test("shape: a price index observation needs no reference data") {
      val observation = PriceIndexObservation.of(PriceIndices.GB_RPI, YearMonth.of(2024, 1))
      encodesTo(observation, """{"index":"GB-RPI","fixingMonth":"2024-01"}""")
      decoded[PriceIndexObservation](observation.asJson.noSpaces, "an observation") shouldBe observation
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Registers the cases in which a document depends on the value and not on how it was built.
   *
   * Three of the four are the reason the collections inside those types are sorted ones. The
   * fourth is the deliberate exception: a matrix of rates holds its currencies in the order they
   * occupy in it and that order is part of the value, so two matrices built in different orders
   * are '''not equal''' and each writes its own order. The document of a matrix is therefore
   * stable per value without being canonical across the orders one could be built in.
   */
  private def registerByteStability(): Unit = {
    test("byte-stability: the attributes of a failure") {
      val forwards = SortedMap.empty[String, String] ++ List("a" -> "1", "b" -> "2", "c" -> "3")
      val backwards = SortedMap.empty[String, String] ++ List("c" -> "3", "b" -> "2", "a" -> "1")
      val first: Failure = Failure.Invalid("bad", forwards)
      val second: Failure = Failure.Invalid("bad", backwards)
      first shouldBe second
      first.asJson.noSpaces shouldBe second.asJson.noSpaces
      first.asJson.noSpaces shouldBe
        """{"Invalid":{"message":"bad","attributes":{"a":"1","b":"2","c":"3"}}}"""
    }

    test("byte-stability: the amounts of a multi-currency amount") {
      val forwards = required(
        MultiCurrencyAmount.of(amount(Currency.GBP, 1.0d), amount(Currency.USD, 2.0d)),
        "sterling then dollars")
      val backwards = required(
        MultiCurrencyAmount.of(amount(Currency.USD, 2.0d), amount(Currency.GBP, 1.0d)),
        "dollars then sterling")
      forwards shouldBe backwards
      forwards.asJson.noSpaces shouldBe backwards.asJson.noSpaces
      forwards.asJson.noSpaces should include("""[{"currency":"GBP","amount":1.0},{"currency":"USD","amount":2.0}]""")
    }

    test("byte-stability: the holidays of a calendar") {
      val shuffled = ImmutableHolidayCalendar.of(
        testCalendarId,
        List(LocalDate.of(2014, 7, 16), LocalDate.of(2014, 7, 14), LocalDate.of(2014, 7, 16)),
        DayOfWeek.SATURDAY,
        DayOfWeek.SUNDAY)
      (shuffled: HolidayCalendar).asJson.noSpaces shouldBe (testCalendar: HolidayCalendar).asJson.noSpaces
      (shuffled: HolidayCalendar).asJson.noSpaces shouldBe TestCalendarDocument
    }

    test("byte-stability: a matrix of rates is stable per value and not canonical across orders") {
      val oneWay = FxMatrix.of(Currency.GBP, Currency.USD, 1.6d)
      val otherWay = FxMatrix.of(Currency.USD, Currency.GBP, 0.625d)
      withClue("the order of the currencies is part of the value: ")(oneWay should not be otherWay)
      oneWay.asJson.noSpaces shouldBe """{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]}"""
      otherWay.asJson.noSpaces shouldBe """{"currencies":["USD","GBP"],"rates":[[1.0,0.625],[1.6,1.0]]}"""
      decoded[FxMatrix](oneWay.asJson.noSpaces, "a matrix of rates") shouldBe oneWay
      decoded[FxMatrix](otherWay.asJson.noSpaces, "a matrix of rates") shouldBe otherWay
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Registers the documents these codecs refuse, one test per family that can refuse one.
   *
   * Every refusal is a '''reported''' failure - a left-hand result carrying the reason - and no
   * test here catches anything: routing a validated type's decoder through its own factory turns a
   * document the factory forbids into a decoding failure carrying the factory's own wording,
   * asserted as a fragment. Whether a particular input is acceptable belongs to the
   * smart-constructor suite; what is tested here is that the decoder consults the factory.
   */
  private def registerInvalidPayloads(): Unit = {
    test("invalid: an amount naming one currency twice") {
      refused[MultiCurrencyAmount](
        """{"amounts":[{"currency":"GBP","amount":1.0},{"currency":"GBP","amount":2.0}]}""",
        "Currency is duplicated: GBP")
    }

    test("invalid: the structural checks of a matrix of rates") {
      refused[FxMatrix](
        """{"currencies":["GBP","GBP"],"rates":[[1.0,1.6],[0.625,1.0]]}""",
        "to be distinct")
      refused[FxMatrix](
        """{"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625]]}""",
        "the same number of elements")
      refused[FxMatrix](
        """{"currencies":["GBP","USD","EUR"],"rates":[[1.0,1.6],[0.625,1.0]]}""",
        "to be a 3 by 3 matrix")
      refused[FxMatrix](
        """{"currencies":["GBP","USD"],"rates":[[2.0,1.6],[0.625,1.0]]}""",
        "against itself to be one")
    }

    test("invalid: a rate that is not positive, and a rate between one currency and itself") {
      refused[FxRate]("""{"pair":"EUR/USD","rate":-1.0}""", "must not be negative or zero")
      refused[FxRate]("""{"pair":"EUR/EUR","rate":2.0}""", "identical currencies must be one")
    }

    test("invalid: an amount that is not a number") {
      refused[CurrencyAmount]("""{"currency":"GBP","amount":"NaN"}""", "must not be NaN")
      refused[CurrencyAmount]("""{"currency":"XYZ","amount":1.0}""", "Currency name not found")
    }

    test("invalid: the fields of an array of amounts") {
      // the two field codecs refuse what a document can get wrong inside a field: a currency
      // code this port does not hold, and an element that is neither a number nor one of the
      // three tagged strings
      refused[CurrencyAmountArray]("""{"currency":"XYZ","values":[1.0]}""", "Currency name not found")
      refused[CurrencyAmountArray]("""{"currency":"GBP","values":["nan"]}""", TaggedDoubleMessage)
    }

    test("invalid: an element of an array of amounts that is not a number") {
      // the tagged string the policy of this port reads as a not-a-number value decodes to a
      // value the elements of a run do not include, so the decoder reports it naming the element
      // rather than building a run whose every later reader would fail on it
      refused[CurrencyAmountArray](
        """{"currency":"GBP","values":[1.0,"NaN"]}""",
        "Argument 'values' must not be NaN at index 1")
      refused[CurrencyAmountArray](
        """{"currency":"GBP","values":["NaN"]}""",
        "Argument 'values' must not be NaN at index 0")
      // the infinities are elements a run does hold, so the refusal is of the one value it
      // rejects rather than of non-finite elements in general
      decoded[CurrencyAmountArray](
        """{"currency":"GBP","values":[1.0,"Infinity","-Infinity"]}""",
        "a run of amounts") shouldBe
        CurrencyAmountArray.of(
          Currency.GBP,
          DoubleArray.of(1.0d, Double.PositiveInfinity, Double.NegativeInfinity))
    }

    test("invalid: a run of amounts whose size disagrees with its arrays") {
      refused[MultiCurrencyAmountArray](
        """{"size":2,"values":{"GBP":[1.0]}}""",
        "Arrays must have the same size")
      refused[MultiCurrencyAmountArray]("""{"size":-1,"values":{}}""", "must not be negative")
    }

    test("invalid: an element of a run of multi-currency amounts that is not a number") {
      // the same refusal one layer up, where the element is located by its currency as well as
      // by its index, and one reason is reported per offending currency in currency order
      refused[MultiCurrencyAmountArray](
        """{"size":2,"values":{"GBP":[1.0,"NaN"]}}""",
        "Argument 'values' for GBP must not be NaN at index 1")
      refused[MultiCurrencyAmountArray](
        """{"size":1,"values":{"USD":["NaN"],"GBP":["NaN"]}}""",
        "Argument 'values' for GBP must not be NaN at index 0; " +
          "Argument 'values' for USD must not be NaN at index 0")
      decoded[MultiCurrencyAmountArray](
        """{"size":2,"values":{"GBP":[1.0,"Infinity"]}}""",
        "a run of multi-currency amounts").getValues(Currency.GBP) shouldBe
        Right(DoubleArray.of(1.0d, Double.PositiveInfinity))
    }

    test("invalid: a schedule period whose dates are the wrong way round") {
      refused[SchedulePeriod](
        """{"startDate":"2014-09-17","endDate":"2014-06-17","unadjustedStartDate":"2014-09-17",""" +
          """"unadjustedEndDate":"2014-06-17"}""",
        "Invalid order")
    }

    test("invalid: a schedule holding a period whose dates are the wrong way round") {
      refused[Schedule](
        """{"periods":[{"startDate":"2014-09-17","endDate":"2014-06-17",""" +
          """"unadjustedStartDate":"2014-09-17","unadjustedEndDate":"2014-06-17"}],""" +
          """"frequency":"P3M","rollConvention":"Day17"}""",
        "Invalid order")
    }

    test("invalid: a schedule definition ending before it starts") {
      refused[PeriodicSchedule](
        """{"startDate":"2014-09-17","endDate":"2014-06-17","frequency":"P1M",""" +
          """"businessDayAdjustment":{"convention":"ModifiedFollowing","calendar":"Sat/Sun"}}""",
        "Invalid order: Expected 'startDate' < 'endDate'")
    }

    test("invalid: a value step with both of its selectors, and with neither") {
      refused[ValueStep](
        """{"periodIndex":2,"date":"2024-01-31","value":{"modifyingValue":1.0,"type":"Replace"}}""",
        "not both")
      refused[ValueStep](
        """{"value":{"modifyingValue":1.0,"type":"Replace"}}""",
        "Either the 'periodIndex' or 'date' must be set")
      refused[ValueStep](
        """{"periodIndex":0,"value":{"modifyingValue":1.0,"type":"Replace"}}""",
        "must not be zero or negative")
    }

    test("invalid: a step sequence replacing its value, and one whose dates are reversed") {
      refused[ValueStepSequence](
        """{"firstStepDate":"2024-01-31","lastStepDate":"2024-07-31","frequency":"P3M",""" +
          """"adjustment":{"modifyingValue":1.0,"type":"Replace"}}""",
        "must not be 'Replace'")
      refused[ValueStepSequence](
        """{"firstStepDate":"2024-07-31","lastStepDate":"2024-01-31","frequency":"P3M",""" +
          """"adjustment":{"modifyingValue":1.0,"type":"DeltaAmount"}}""",
        "Invalid order")
    }

    test("invalid: a value schedule carrying a step that describes none") {
      // this type's own factory does validate - it refuses a position two steps name with
      // different adjustments - but that is not the refusal asserted here: the step in this
      // document names neither a period index nor a date, which the nested step's decoder
      // refuses before the schedule's factory is reached, so the failure is reported at the
      // position of that step
      refused[ValueSchedule](
        """{"initialValue":1.0,"steps":[{"value":{"modifyingValue":1.0,"type":"Replace"}}]}""",
        "Either the 'periodIndex' or 'date' must be set")
    }

    test("invalid: a rounding convention outside the range it permits") {
      refused[Rounding]("""{"HalfUp":{"decimalPlaces":256,"fraction":0}}""", "Invalid decimal places")
      refused[Rounding]("""{"HalfUp":{"decimalPlaces":2,"fraction":257}}""", "Invalid fraction")
    }

    test("invalid: a rounding document that does not match the member it names") {
      refused[Rounding]("""{"NoRounding":123}""", "must hold an empty object")
      refused[Rounding]("""{"Nope":{}}""", "exactly one of 'NoRounding' or 'HalfUp'")
    }

    test("invalid: text naming no decimal") {
      refused[Decimal]("\"NaN\"", "Decimal string is invalid")
      refused[Decimal]("\"abc\"", "Decimal string is invalid")
    }

    test("invalid: a fixed-scale decimal stating more places than the type carries") {
      refused[FixedScaleDecimal]("\"12.3456789012345678901\"", "Scale must be 18 or less")
    }

    test("invalid: a country code that is not two upper-case letters") {
      refused[Country]("\"gb\"", "must match pattern: [A-Z][A-Z]")
      refused[Country]("\"GBR\"", "must match pattern: [A-Z][A-Z]")
    }

    test("invalid: an identifier without its separator") {
      refused[StandardId]("\"no-separator\"", "Invalid identifier format")
    }

    test("the largest identifier the factories admit survives its own document") {
      // The codec of this type is its text form, and its decoder is `StandardId.parse`, so the
      // round trip this section claims for the type holds only if the ceiling `parse` puts on
      // the text it reads admits every text the factories can produce. The two are related
      // rather than equal - two parts of 65,536 characters are admitted, and they render as
      // `scheme~value` - and this is the boundary where that relation is load-bearing: the
      // generator of this suite draws parts of a dozen characters and would never reach it, so
      // a text ceiling set at the part ceiling would leave a value that encodes and does not
      // decode, and every property here would still pass.
      val part: String = "H" * 65536
      val maximal: StandardId =
        StandardId.of(part, part).toOption.getOrElse(fail("the maximal identifier is admitted"))
      val document: String = maximal.asJson.noSpaces
      document.length shouldBe 2 * 65536 + 1 + 2 // the text, its separator, and the two quotes
      decoded[StandardId](document, "an identifier") shouldBe maximal
    }

    test("invalid: a tenor whose period is not positive") {
      refused[Tenor]("\"P0D\"", "Tenor period must not be zero")
      refused[Tenor]("\"nonsense\"", "Unable to parse tenor")
    }

    test("invalid: text naming no frequency") {
      refused[Frequency]("\"nonsense\"", "Unable to parse frequency")
    }

    test("invalid: text naming no market tenor") {
      refused[MarketTenor]("\"nonsense\"", "Unable to parse tenor")
    }

    test("invalid: a day count whose name disagrees with the calendar it carries") {
      refused[DayCount](
        """{"Bus252":{"name":"Bus/252 BRBD","calendar":"EUTA"}}""",
        "does not match the calendar it carries")
    }

    test("invalid: a day count document of no known shape") {
      refused[DayCount]("\"nonsense\"", "DayCount name not found")
      refused[DayCount]("""{"Nope":{}}""", "an object of one field named 'Bus252'")
    }

    test("invalid: a calendar name that resolves to nothing, and a document of no known shape") {
      refused[HolidayCalendar]("\"NOT-A-CALENDAR\"", "Reference data not found for identifier")
      refused[HolidayCalendar](
        """{"Nope":{}}""",
        "an object holding exactly one of 'Immutable', 'Combined' or 'Linked'")
    }

    test("invalid: a derived observation stating a value its index does not derive") {
      refused[IborIndexObservation](
        """{"index":"GBP-LIBOR-3M","fixingDate":"2014-06-30","effectiveDate":"2014-06-30",""" +
          """"maturityDate":"2014-10-01","yearFraction":0.25205479452054796}""",
        "the index does not derive")
      refused[OvernightIndexObservation](
        """{"index":"USD-FED-FUND","fixingDate":"2016-02-22","publicationDate":"2016-02-24",""" +
          """"effectiveDate":"2016-02-22","maturityDate":"2016-02-23",""" +
          """"yearFraction":0.002777777777777778}""",
        "disagrees with the index")
      refused[FxIndexObservation](
        """{"index":"GBP/USD-WM","fixingDate":"2016-02-22","maturityDate":"2016-02-25"}""",
        "but the value read declared")
    }

    test("invalid: a tagged double in any other spelling") {
      refused[DoubleArray]("""["nan"]""", TaggedDoubleMessage)
      refused[DoubleArray]("""["+Infinity"]""", TaggedDoubleMessage)
      refused[DoubleArray]("""["1.0"]""", TaggedDoubleMessage)
      refused[DoubleMatrix]("""[["Inf"]]""", TaggedDoubleMessage)
    }

    test("invalid: text naming no member of any named family") {
      refusesUnknownName[Currency]("Currency")
      refusesUnknownName[BusinessDayConvention]("BusinessDayConvention")
      refusesUnknownName[RollConvention]("RollConvention")
      refusesUnknownName[PeriodAdditionConvention]("PeriodAdditionConvention")
      refusesUnknownName[DateSequence]("DateSequence")
      refusesUnknownName[StubConvention]("StubConvention")
      refusesUnknownName[FloatingRateType]("FloatingRateType")
      refusesUnknownName[ValueAdjustmentType]("ValueAdjustmentType")
      refusesUnknownName[FailureReason]("FailureReason")
      refusesUnknownName[IborIndex]("IborIndex")
      refusesUnknownName[OvernightIndex]("OvernightIndex")
      refusesUnknownName[PriceIndex]("PriceIndex")
      refusesUnknownName[FxIndex]("FxIndex")
      refusesUnknownName[FloatingRateName]("FloatingRateName")
    }

    test("invalid: several problems in one document are reported together") {
      // the codec support joins accumulated messages with a semicolon and a space, and the
      // factories that accumulate are the reason it has to: a reader of the failure is told
      // everything that is wrong with the document rather than the first thing
      val reversedPeriod =
        """{"startDate":"2014-09-17","endDate":"2014-06-17","unadjustedStartDate":"2014-09-17",""" +
          """"unadjustedEndDate":"2014-06-17"}"""
      decode[SchedulePeriod](reversedPeriod) match {
        case Left(failure) =>
          withClue(s"the refusal of the reversed period was '${failure.getMessage}': ") {
            failure.getMessage should include("; ")
            failure.getMessage should include("'unadjustedStartDate' < 'unadjustedEndDate'")
            failure.getMessage should include("'startDate' < 'endDate'")
          }
        case Right(value) => fail(s"a period whose dates are reversed should have been refused: $value")
      }
      decode[Rounding]("""{"HalfUp":{"decimalPlaces":-1,"fraction":257}}""") match {
        case Left(failure) =>
          withClue(s"the refusal of the out-of-range rounding was '${failure.getMessage}': ") {
            failure.getMessage should include("; ")
            failure.getMessage should include("Invalid decimal places")
            failure.getMessage should include("Invalid fraction")
          }
        case Right(value) => fail(s"a rounding outside both ranges should have been refused: $value")
      }
    }

    test("invalid: a document naming a date no calendar can be asked about") {
      // A derived observation is built by asking the index's fixing calendar for the dates that
      // follow from the fixing date, and a calendar states as a precondition - as the library
      // being ported did - that it deals only in the years 0 to 9999. A document is data, so a
      // fixing date beyond those years must be reported as the document being wrong; before the
      // reader was wrapped, that precondition was raised out of the decoding library itself, so
      // a caller decoding a document could be handed an exception by a call whose type says it
      // answers with a failure. Every such refusal now arrives as a failure, and it carries what
      // the factory said, so the document can be corrected.
      val yearTenThousand: String = "+10000-01-05"
      val raised: String = "The payload was refused by the type it describes"
      val precondition: String = "outside the accepted range"

      val iborPayload: String =
        s"""{"index":"GBP-LIBOR-3M","fixingDate":"$yearTenThousand","effectiveDate":"2014-06-30",""" +
          """"maturityDate":"2014-10-01","yearFraction":0.25205479452054796}"""
      refused[IborIndexObservation](iborPayload, raised)
      refused[IborIndexObservation](iborPayload, precondition)

      val overnightPayload: String =
        s"""{"index":"USD-FED-FUND","fixingDate":"$yearTenThousand","publicationDate":"2016-02-24",""" +
          """"effectiveDate":"2016-02-22","maturityDate":"2016-02-23",""" +
          """"yearFraction":0.002777777777777778}"""
      refused[OvernightIndexObservation](overnightPayload, raised)
      refused[OvernightIndexObservation](overnightPayload, precondition)

      val fxPayload: String =
        s"""{"index":"GBP/USD-WM","fixingDate":"$yearTenThousand","maturityDate":"2016-02-25"}"""
      refused[FxIndexObservation](fxPayload, raised)
      refused[FxIndexObservation](fxPayload, precondition)

      // The other end of the range reports the same way, so what is refused is the year lying
      // outside the range rather than a year too large to hold.
      refused[FxIndexObservation](
        """{"index":"GBP/USD-WM","fixingDate":"-0001-12-31","maturityDate":"2016-02-25"}""",
        raised)

      // The wrapping refuses nothing a factory accepts, which is what keeps every value
      // writable and readable: a type that consults no calendar while being built takes such a
      // date as it takes any other, and the precondition is stated later by whatever calendar is
      // eventually asked. An adjustable date is that case - it carries the date and the
      // adjustment to apply, and applies nothing until it is resolved.
      decoded[AdjustableDate](
        s"""{"unadjusted":"$yearTenThousand","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""",
        "adjustable date").unadjusted.getYear shouldBe 10000
    }

    test("invalid: a calendar closed on every day of the week") {
      // Such a calendar has no business day in any year, so it can answer neither "the next
      // business day from here" nor any shift, adjustment, schedule or observation built on one.
      // Its factory refuses to build it, and a document describing it is refused as the document
      // being wrong - in every position a calendar appears in, since the refusal belongs to the
      // reader of the calendar and not to the document that encloses it.
      val everyDayClosed: String =
        """{"Immutable":{"id":"XCAL","weekendDays":["MONDAY","TUESDAY","WEDNESDAY","THURSDAY",""" +
          """"FRIDAY","SATURDAY","SUNDAY"],"holidays":[]}}"""
      val noBusinessDay: String = "leave at least one day of the week open"

      refused[HolidayCalendar](everyDayClosed, noBusinessDay)
      refused[HolidayCalendar](s"""{"Combined":{"a":$everyDayClosed,"b":"Sat/Sun"}}""", noBusinessDay)
      refused[HolidayCalendar](s"""{"Linked":{"a":"GBLO","b":$everyDayClosed}}""", noBusinessDay)
      refused[DayCount](s"""{"Bus252":{"name":"Bus/252 XCAL","calendar":$everyDayClosed}}""", noBusinessDay)

      // Six closed days is a calendar - one business day a week - and is read, so what is refused
      // is the week with nothing left of it rather than a long weekend.
      val oneOpenDay: String =
        """{"Immutable":{"id":"XCAL","weekendDays":["MONDAY","TUESDAY","WEDNESDAY","FRIDAY",""" +
          """"SATURDAY","SUNDAY"],"holidays":[]}}"""
      val sixDayWeekend = decoded[HolidayCalendar](oneOpenDay, "calendar with one open day")
      sixDayWeekend.isBusinessDay(LocalDate.of(2014, 7, 17)) shouldBe true
      sixDayWeekend.isBusinessDay(LocalDate.of(2014, 7, 18)) shouldBe false
    }

    test("invalid: calendars nested deeper than a calendar can be read through") {
      // A composite calendar is a pair of calendars, so a document may nest one inside another
      // without limit, and reading such a document is itself a descent - as is every later
      // question about a date, the identifier of the result and the document written back out.
      // A document nesting them past the family's limit would therefore exhaust the stack while
      // being read, before anything it describes exists to be judged, which is why the reader
      // counts its descent and refuses rather than waiting for the value to be built.
      val nest = (levels: Int) =>
        (1 to levels).foldLeft("\"Sat/Sun\"")((inner, _) => s"""{"Combined":{"a":$inner,"b":"GBLO"}}""")

      // The deepest document that is read: the limit is what the family allows a calendar to
      // read through, and the calendar built from it answers as its parts do.
      val atTheLimit = decoded[HolidayCalendar](
        nest(HolidayCalendar.MaxCompositeDepth),
        s"calendar nested ${HolidayCalendar.MaxCompositeDepth} deep")
      atTheLimit.isHoliday(LocalDate.of(2014, 7, 12)) shouldBe true
      atTheLimit.isBusinessDay(LocalDate.of(2014, 7, 15)) shouldBe true

      // One level more is refused, and the refusal says how deep the document went and how deep
      // one may go.
      val tooDeepMessage: String =
        s"cannot read through more than ${HolidayCalendar.MaxCompositeDepth} calendars"
      refused[HolidayCalendar](nest(HolidayCalendar.MaxCompositeDepth + 1), tooDeepMessage)
      refused[HolidayCalendar](
        nest(HolidayCalendar.MaxCompositeDepth + 1),
        s"at least ${HolidayCalendar.MaxCompositeDepth + 1} deep")

      // Linked composites are counted the same way, as is a mixture of the two: what is bounded
      // is the depth of the descent rather than which way the calendars combine.
      val linkedNest = (levels: Int) =>
        (1 to levels).foldLeft("\"Sat/Sun\"")((inner, _) => s"""{"Linked":{"a":$inner,"b":"GBLO"}}""")
      refused[HolidayCalendar](linkedNest(HolidayCalendar.MaxCompositeDepth + 1), tooDeepMessage)
      val mixedNest = (1 to HolidayCalendar.MaxCompositeDepth + 1).foldLeft("\"Sat/Sun\"") { (inner, level) =>
        if (level % 2 == 0) s"""{"Combined":{"a":$inner,"b":"GBLO"}}"""
        else s"""{"Linked":{"a":$inner,"b":"USNY"}}"""
      }
      refused[HolidayCalendar](mixedNest, tooDeepMessage)

      // A nested calendar also appears inside the types that carry one, and is refused there in
      // the same way rather than by the type that encloses it.
      refused[DayCount](
        s"""{"Bus252":{"name":"Bus/252 XCAL","calendar":${nest(HolidayCalendar.MaxCompositeDepth + 1)}}}""",
        tooDeepMessage)

      // A document nesting them far beyond the limit - the shape an attack would take - is
      // refused as well, and by the first layer that reaches its own bound: the text of such a
      // document is itself nested, so the JSON parser refuses to build a value from it before
      // this decoder is asked to read one. Either refusal is a failure rather than a stack
      // overflow, which is what this asserts; which of the two reports it is not this module's
      // to fix.
      decode[HolidayCalendar](nest(20000)).isLeft shouldBe true
      decode[HolidayCalendar](linkedNest(20000)).isLeft shouldBe true
    }

    test("invalid: what a raised refusal may write into the failure that reports it") {
      // The refusals above reach a reader as text: a factory states what it refused, and the
      // wrapping that turns a raised refusal into a decoding failure carries that statement into
      // the failure. The statement comes from inside the library, but the value it names came
      // out of the document - a date, a name, a number - so its content and its length are the
      // document's, and a document is written by whoever sends it.
      //
      // Two things are therefore true of the text a failure carries, and are asserted here
      // rather than left to the shapes above, which all describe well-behaved payloads: it is
      // one line, so a payload cannot add a line to a log that holds the failure, and it is
      // bounded, so a payload cannot fill one. The decoder is driven directly, a factory that
      // raises exactly what is wanted being the only way to state either.
      val raising = (message: String) =>
        Codecs.guardedDecoder[Int](Decoder.instance[Int](_ => throw new IllegalArgumentException(message)))
      // Both directions of a decoder are wrapped, so both are asked for every case: the one that
      // answers with the first failure, and the one that accumulates.
      val firstFailure = (message: String) =>
        raising(message)
          .decodeJson(Json.fromInt(1))
          .fold(failure => failure.message, value => fail(s"the decoder answered with $value"))
      val accumulatedFailure = (message: String) =>
        raising(message)
          .decodeAccumulating(Json.fromInt(1).hcursor)
          .fold(failures => failures.head.message, value => fail(s"the decoder answered with $value"))
      val diagnostics = (message: String) => List(firstFailure(message), accumulatedFailure(message))
      // The account of the refusal is what follows the one introduction the wrapping writes,
      // which is the first `": "` of the failure - none of the messages raised below contains
      // one of its own.
      val accountOf = (diagnostic: String) => diagnostic.substring(diagnostic.indexOf(": ") + 2)
      // A surrogate that is not half of a pair: half of a character, which turns into a
      // replacement glyph wherever it is written, and the thing a cut in the wrong place makes.
      val unpaired = (text: String) =>
        text.toList.zipWithIndex.count { case (ch, index) =>
          (Character.isHighSurrogate(ch) &&
            !(index + 1 < text.length && Character.isLowSurrogate(text.charAt(index + 1)))) ||
            (Character.isLowSurrogate(ch) &&
              !(index > 0 && Character.isHighSurrogate(text.charAt(index - 1))))
        }

      // Every character a reader could take for the end of a line is written as an escape. The
      // two Unicode separators matter as much as the control characters: a reader that splits on
      // them sees two lines where the failure is one.
      val forged: String =
        "refused \n INFO faked log line \r\u2028\u2029\u0000\u001b[31m and a tab \t here"
      diagnostics(forged).foreach { diagnostic =>
        withClue(s"the diagnostic of a forged message [$diagnostic]: ") {
          diagnostic should include("refused")
          diagnostic.exists(ch => Character.isISOControl(ch)) shouldBe false
          diagnostic.contains('\u2028') shouldBe false
          diagnostic.contains('\u2029') shouldBe false
          diagnostic.linesIterator.size shouldBe 1
          // ... and written as the escapes a reader recognises, so the account is still legible
          diagnostic should include("\\n")
          diagnostic should include("\\r")
          diagnostic should include("\\t")
          diagnostic should include("\\u2028")
          diagnostic should include("\\u2029")
          diagnostic should include("\\u0000")
          diagnostic should include("\\u001b")
        }
      }

      // The length of the account is bounded at 256 characters of rendering, and an account that
      // was cut says so with three more. The bound is on the rendering rather than on the text
      // behind it, so an escape counts as the characters it is written with.
      val longPlainText: String = "a" * 300
      diagnostics(longPlainText).foreach { diagnostic =>
        withClue(s"the diagnostic of an over-long message: ") {
          accountOf(diagnostic).length shouldBe 259
          accountOf(diagnostic) should endWith("...")
          accountOf(diagnostic).count(ch => ch == 'a') shouldBe 256
        }
      }
      // The same bound over text that is mostly escapes. The line feeds are interleaved rather
      // than trailing, because the account is trimmed before it is rendered - a refusal that
      // ends in white space says nothing about it - so text ending in line feeds would lose
      // them to the trim rather than to the bound.
      val longEscapedText: String = "\nx" * 200
      diagnostics(longEscapedText).foreach { diagnostic =>
        withClue("the diagnostic of an over-long message of escapes: ") {
          // 86 characters and 85 escapes of two characters each reach the bound exactly, and the
          // next escape would pass it: 399 characters of text render to 256 and are cut there,
          // so the bound is on the rendering and not on the text behind it
          accountOf(diagnostic).length shouldBe 259
          accountOf(diagnostic) should endWith("...")
          accountOf(diagnostic).sliding(2).count(pair => pair == "\\n") shouldBe 85
          accountOf(diagnostic).count(ch => ch == 'x') shouldBe 86
        }
      }

      // A character written with a surrogate pair is one character, and the cut falls between
      // characters: the pair below would carry the rendering one past the bound, so it is left
      // out whole rather than halved.
      val pairAtTheBoundary: String = "a" * 255 + "\uD83D\uDE00"
      diagnostics(pairAtTheBoundary).foreach { diagnostic =>
        withClue(s"the diagnostic of a message whose last character straddles the bound: ") {
          unpaired(diagnostic) shouldBe 0
          accountOf(diagnostic).count(ch => ch == 'a') shouldBe 255
          accountOf(diagnostic) should endWith("...")
          accountOf(diagnostic).length shouldBe 258
        }
      }
      // ... and a pair that fits is kept, so what the bound refuses is the character that does
      // not fit rather than every character outside the Latin alphabet.
      val pairWithinTheBound: String = "a" * 254 + "\uD83D\uDE00"
      diagnostics(pairWithinTheBound).foreach { diagnostic =>
        withClue("the diagnostic of a message ending exactly at the bound: ") {
          unpaired(diagnostic) shouldBe 0
          diagnostic should include("\uD83D\uDE00")
          accountOf(diagnostic) should not endWith "..."
          accountOf(diagnostic).length shouldBe 256
        }
      }
      // A surrogate standing on its own is not a character at all, and is escaped rather than
      // written out, so nothing a payload can say puts one into a failure.
      diagnostics("before \uD83D after").foreach { diagnostic =>
        withClue("the diagnostic of a message holding half of a character: ") {
          unpaired(diagnostic) shouldBe 0
          diagnostic should include("\\ud83d")
          diagnostic should include("before")
          diagnostic should include("after")
        }
      }

      // A refusal that said nothing at all is reported as such, rather than as an empty account,
      // and so is one whose whole message is white space.
      diagnostics("   ").foreach(diagnostic => diagnostic should include("no reason was given"))
      diagnostics("\n\r\t ").foreach(diagnostic => diagnostic should include("no reason was given"))
      // A separator is not white space to a reader of text, and is not treated as any: a message
      // that is nothing but one is written out as the escape of it rather than as nothing.
      diagnostics("\u2028").foreach { diagnostic =>
        diagnostic should include("\\u2028")
        diagnostic.contains('\u2028') shouldBe false
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * A JSON array of a stated length, every position holding one shared unreadable value.
   *
   * The element is text that no field of any document below can hold - not a date, not a day of
   * the week, not a number, not an object - which is what makes these payloads evidence rather
   * than illustration: a decoder that reached the elements of one of them would report an element,
   * so a refusal that names the count instead is a refusal reached without reading any.
   *
   * The element is bound once and the positions share it. That is not a micro-optimisation but a
   * requirement of the arrays below being a hundred thousand positions long: the argument of
   * `Vector.fill` is taken by name, so building a fresh value per position would allocate a
   * hundred thousand of them for a document whose elements are never read, and the suite runs
   * under a heap that has a million-element array of doubles in it already.
   *
   * @param length  the number of elements the array states
   * @return the JSON array of that length
   */
  private def repeatedElements(length: Int): Json = {
    val element = Json.fromString("rubbish")
    Json.fromValues(Vector.fill(length)(element))
  }

  /**
   * An array stating one element more than the ceiling a decoded collection is read under.
   *
   * Computed on first use and shared by every assertion that needs it, because it is the same
   * array in each of them - a count and nothing else - and nothing that reads it can alter it.
   */
  private lazy val beyondCollectionCeiling: Json =
    repeatedElements(Codecs.MaximumCollectionElements + 1)

  /** An array stating exactly that ceiling, which the measurement passes on to the decoder. */
  private lazy val atCollectionCeiling: Json = repeatedElements(Codecs.MaximumCollectionElements)

  /**
   * Asserts that a document stating too much of something is refused before any of it is read.
   *
   * @param payload  the document, stating more than one of the published ceilings allows
   * @param what  what the refusal is expected to say was counted
   * @param limit  the ceiling the refusal is expected to name
   * @param stated  the count the document states, which the refusal is expected to report
   * @param position  the field names of the path to the offending collection, innermost first
   * @tparam A  the type the document fails to describe
   * @return the assertion
   */
  private def refusesBeyondCeiling[A: Decoder](
      payload: Json,
      what: String,
      limit: Int,
      stated: Int,
      position: List[String]): Assertion =
    implicitly[Decoder[A]].decodeJson(payload) match {
      case Left(failure) =>
        withClue(s"the refusal of the oversized document was '${failure.getMessage}': ") {
          failure.message shouldBe s"Expected at most $limit $what, but the payload states $stated"
          failure.history shouldBe position.map(name => CursorOp.DownField(name))
        }
      case Right(_) =>
        fail(
          s"a document stating $stated $what is beyond the ceiling of $limit and should have " +
            s"been refused")
    }

  /**
   * Asserts that a document stating one element too many is refused before an element is read.
   *
   * @param payload  the document, stating one element more than the ceiling in one collection
   * @param what  what the refusal is expected to say was counted
   * @param position  the field names of the path to the offending collection, innermost first
   * @tparam A  the type the document fails to describe
   * @return the assertion
   */
  private def refusesBeyondCollectionCeiling[A: Decoder](
      payload: Json,
      what: String,
      position: List[String]): Assertion =
    refusesBeyondCeiling[A](
      payload,
      what,
      Codecs.MaximumCollectionElements,
      Codecs.MaximumCollectionElements + 1,
      position)

  /**
   * Asserts that a document stating exactly the ceiling is not refused by the measurement.
   *
   * The document is still refused - its elements are unreadable, and in some of these documents a
   * field the decoder needs is absent as well - and that is the point: what is asserted is that the
   * refusal is no longer the measurement's, which is what places the boundary at ''more than'' the
   * ceiling rather than at the ceiling itself, and is the half of the boundary a test asserting
   * only the refusal would leave unpinned.
   *
   * @param payload  the document, stating exactly the ceiling in one collection
   * @param what  what a refusal by the measurement would have said was counted
   * @tparam A  the type the document does not describe either
   * @return the assertion
   */
  private def readsAtCollectionCeiling[A: Decoder](payload: Json, what: String): Assertion = {
    val refusal = implicitly[Decoder[A]].decodeJson(payload).left.toOption.map(failure => failure.message)
    withClue(
      s"a document stating exactly ${Codecs.MaximumCollectionElements} $what was answered " +
        s"with '$refusal': ") {
      refusal.exists(message => message.startsWith("Expected at most")) shouldBe false
    }
  }

  /**
   * Registers the ceiling on how many elements a collection of a document may state.
   *
   * Every document of this module that carries a collection carries its cardinality as well, which
   * is to say the reader is told how much to allocate by the document rather than by the library.
   * The codec support therefore publishes one figure, `Codecs.MaximumCollectionElements`, and each
   * collection-bearing decoder measures the payload against it '''before''' the collection is
   * decoded. These tests pin that, per field, in the only form that distinguishes a bound applied
   * before the decode from one applied after it: the elements of the oversized arrays are a single
   * shared string that no field could hold, and the documents state nothing beyond what the decoder
   * has to read to reach the collection under test, so a refusal naming the count is a refusal that
   * read neither the elements nor anything that would have followed them.
   *
   * What is deliberately not asserted is a collection of a hundred thousand ''valid'' values. The
   * property being tested is the refusal, the figure is checked against the two expansion ceilings
   * of this library by `CodecsSpec`, and a test that materialised a hundred thousand domain values
   * would spend the heap of the suite proving something neither of those needs.
   */
  private def registerCollectionCeilings(): Unit = {
    test("invalid: a currency document stating more entries than the collection ceiling") {
      refusesBeyondCollectionCeiling[MultiCurrencyAmount](
        Json.obj("amounts" -> beyondCollectionCeiling),
        "elements in the amounts field",
        List("amounts"))
      refusesBeyondCollectionCeiling[FxMatrix](
        Json.obj("currencies" -> beyondCollectionCeiling),
        "elements in the currencies field",
        List("currencies"))
      // the rates of a matrix carry no collection ceiling of their own, and this is the assertion
      // that records why rather than leaving it to the reader: the field is a matrix of doubles,
      // whose codec measures the rows the payload states against its own, tighter ceiling before
      // it reads a row, so a second bound here would refuse nothing the matrix codec admits
      refusesBeyondCeiling[FxMatrix](
        Json.obj("currencies" -> Json.arr(Json.fromString("GBP")), "rates" -> beyondCollectionCeiling),
        "rows in the matrix",
        Codecs.MaximumMatrixRows,
        Codecs.MaximumCollectionElements + 1,
        List("rates"))
      readsAtCollectionCeiling[MultiCurrencyAmount](
        Json.obj("amounts" -> atCollectionCeiling),
        "elements in the amounts field")
      readsAtCollectionCeiling[FxMatrix](
        Json.obj("currencies" -> atCollectionCeiling),
        "elements in the currencies field")
    }

    test("invalid: a date, schedule or value document stating more entries than the collection ceiling") {
      refusesBeyondCollectionCeiling[AdjustableDates](
        Json.obj("unadjusted" -> beyondCollectionCeiling),
        "elements in the unadjusted field",
        List("unadjusted"))
      refusesBeyondCollectionCeiling[Schedule](
        Json.obj("periods" -> beyondCollectionCeiling),
        "elements in the periods field",
        List("periods"))
      refusesBeyondCollectionCeiling[ValueSchedule](
        Json.obj("steps" -> beyondCollectionCeiling),
        "elements in the steps field",
        List("steps"))
      readsAtCollectionCeiling[AdjustableDates](
        Json.obj("unadjusted" -> atCollectionCeiling),
        "elements in the unadjusted field")
      readsAtCollectionCeiling[Schedule](
        Json.obj("periods" -> atCollectionCeiling),
        "elements in the periods field")
      readsAtCollectionCeiling[ValueSchedule](
        Json.obj("steps" -> atCollectionCeiling),
        "elements in the steps field")
    }

    test("invalid: a calendar document stating more dates or weekend days than the collection ceiling") {
      // the three collections of a structural calendar are read by two list decoders, each of
      // which is bounded, and the refusal is positioned at the field being read rather than at the
      // decoder - so the path names the wrapper object as well, which is where a reader of the
      // failure has to look
      refusesBeyondCollectionCeiling[HolidayCalendar](
        calendarDocument(weekendDays = smallWeekend, holidays = beyondCollectionCeiling),
        "dates in the list",
        List("holidays", "Immutable"))
      refusesBeyondCollectionCeiling[HolidayCalendar](
        calendarDocument(
          weekendDays = smallWeekend,
          holidays = Json.arr(Json.fromString("2020-01-01")),
          workingWeekendDays = Some(beyondCollectionCeiling)),
        "dates in the list",
        List("workingWeekendDays", "Immutable"))
      refusesBeyondCollectionCeiling[HolidayCalendar](
        calendarDocument(weekendDays = beyondCollectionCeiling, holidays = Json.arr()),
        "days of the week in the list",
        List("weekendDays", "Immutable"))
      readsAtCollectionCeiling[HolidayCalendar](
        calendarDocument(weekendDays = smallWeekend, holidays = atCollectionCeiling),
        "dates in the list")
      readsAtCollectionCeiling[HolidayCalendar](
        calendarDocument(weekendDays = atCollectionCeiling, holidays = Json.arr()),
        "days of the week in the list")
    }
  }

  /** The weekend of the calendar documents above, which has to be readable to reach the rest. */
  private lazy val smallWeekend: Json =
    Json.arr(Json.fromString("SATURDAY"), Json.fromString("SUNDAY"))

  /**
   * The structural form of a calendar, assembled around the collection under test.
   *
   * The fields are read in the order the decoder reads them - the identifier, the weekend, the
   * holidays, then the weekend dates declared working - so a document probing one collection has
   * to state readable values for the ones before it and nothing at all for the ones after.
   *
   * @param weekendDays  the weekend days the document states
   * @param holidays  the holidays the document states
   * @param workingWeekendDays  the weekend dates declared working, where the document states them
   * @return the document of a calendar carrying its own data
   */
  private def calendarDocument(
      weekendDays: Json,
      holidays: Json,
      workingWeekendDays: Option[Json] = None): Json = {
    val declared = List(
      "id" -> Json.fromString("XCAL"),
      "weekendDays" -> weekendDays,
      "holidays" -> holidays) ++
      workingWeekendDays.map(dates => "workingWeekendDays" -> dates).toList
    Json.obj("Immutable" -> Json.obj(declared: _*))
  }

  //-------------------------------------------------------------------------
  /**
   * Registers the serialization test this suite consolidates. The equality of a value with the one
   * read back from its document is asserted for three calendars - no holidays, weekend-only, and
   * London - under one convention and day count; the fourth property of the record it stands for
   * is discussed below.
   */
  private def registerJodaBeansConsolidation(): Unit =
    test("test_jodaBeans_serialize") {
      // the three calendars of that test, the last of them reached by name
      val calendars = List[HolidayCalendar](NoHolidays, SatSun, StandardHolidayCalendars.GBLO)
      val checked = calendars.map { calendar =>
        val record = MockSerRecord(BusinessDayConventions.MODIFIED_FOLLOWING, calendar, DayCounts.ACT_360)
        val document = record.asJson.noSpaces
        withClue(s"the record carrying '${calendar.name}' written as $document: ") {
          decoded[MockSerRecord](document, "the mock record") shouldBe record
        }
      }
      checked.size shouldBe calendars.size

      // each of the three properties is carried by the codec of its own type, so a built-in
      // calendar is nested as its bare name rather than as a structure
      val london = MockSerRecord(
        BusinessDayConventions.MODIFIED_FOLLOWING,
        StandardHolidayCalendars.GBLO,
        DayCounts.ACT_360)
      encodesTo(
        london,
        """{"bdConvention":"ModifiedFollowing","holidayCalendar":"GBLO","dayCount":"Act/360"}""")

      // A list of bare objects has no counterpart here by design: writing one means asking each
      // element for its runtime class and looking up a writer for it, which is reflection no
      // codec here performs. The absence is a compile-time fact rather than an omission - no
      // encoder for such a list exists, while a list of a type that has one serializes.
      assertDoesNotCompile("implicitly[io.circe.Encoder[List[Any]]]")
      assertCompiles("implicitly[io.circe.Encoder[List[com.opengamma.strata.basics.date.DayCount]]]")
    }

  //-------------------------------------------------------------------------
  /**
   * The control that the summon every absence proof uses compiles for a type that has a codec.
   *
   * A snippet asserted not to compile can fail for the wrong reason - a misspelled package, a
   * malformed summon - and the proof would then pass while proving nothing. Two controls guard
   * that: this one summons the same thing for a type that has a codec, and each proof names its
   * own type in a declaration that '''does''' compile.
   *
   * @return the assertion
   */
  private def summonControl(): Assertion = {
    assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.currency.CurrencyAmount]]")
    assertCompiles("implicitly[io.circe.Decoder[com.opengamma.strata.basics.currency.CurrencyAmount]]")
  }

  /**
   * Registers the proof, for each deliberately unserializable type, that it has no codec.
   *
   * The claim is not that encoding such a type fails at run time but that there is nothing to
   * call, so the proof can only be that the call does not compile. With the round trips above
   * these close the inventory in both directions. They are confined to the absence of a codec; the
   * other compile-time facts about these types belong to the API surface suite.
   */
  private def registerExclusionProofs(): Unit = {
    test("excluded from JSON: ReferenceData") {
      assertCompiles("type Probe = com.opengamma.strata.basics.ReferenceData")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.ReferenceData]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.ReferenceData]]")
      summonControl()
    }

    test("excluded from JSON: ImmutableReferenceData") {
      assertCompiles("type Probe = com.opengamma.strata.basics.ImmutableReferenceData")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.ImmutableReferenceData]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.ImmutableReferenceData]]")
      summonControl()
    }

    test("excluded from JSON: CombinedReferenceData") {
      assertCompiles("type Probe = com.opengamma.strata.basics.CombinedReferenceData")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.CombinedReferenceData]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.CombinedReferenceData]]")
      summonControl()
    }

    test("excluded from JSON: HolidaySafeReferenceData") {
      assertCompiles("type Probe = com.opengamma.strata.basics.date.HolidaySafeReferenceData")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.date.HolidaySafeReferenceData]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.date.HolidaySafeReferenceData]]")
      summonControl()
    }

    test("excluded from JSON: ReferenceData.Entry") {
      assertCompiles(
        "type Probe = com.opengamma.strata.basics.ReferenceData.Entry[com.opengamma.strata.basics.date.HolidayCalendar]")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.ReferenceData.Entry[" +
          "com.opengamma.strata.basics.date.HolidayCalendar]]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.ReferenceData.Entry[" +
          "com.opengamma.strata.basics.date.HolidayCalendar]]]")
      summonControl()
    }

    test("excluded from JSON: ReferenceDataId") {
      // the one identifier that does carry a codec is the calendar identifier, and it is the
      // control here: the exclusion is of the abstraction and of the identifiers an application
      // defines for its own data, of which the fixture below is this module's own example
      assertCompiles("type Probe = com.opengamma.strata.basics.TestingReferenceDataId")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.TestingReferenceDataId]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.TestingReferenceDataId]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.date.HolidayCalendarId]]")
      summonControl()
    }

    test("excluded from JSON: DayCount.ScheduleInfo") {
      assertCompiles("type Probe = com.opengamma.strata.basics.date.DayCount.ScheduleInfo")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]]")
      // the one implementation of it that carries data is the schedule, which is covered
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.schedule.Schedule]]")
      summonControl()
    }

    test("excluded from JSON: DateAdjuster") {
      assertCompiles("type Probe = com.opengamma.strata.basics.date.DateAdjuster")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.date.DateAdjuster]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.date.DateAdjuster]]")
      summonControl()
    }

    test("excluded from JSON: FxRateProvider") {
      assertCompiles("type Probe = com.opengamma.strata.basics.currency.FxRateProvider")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.currency.FxRateProvider]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.currency.FxRateProvider]]")
      // two implementations of this contract are data rather than behaviour - a rate and a
      // matrix of rates - and both are covered types with codecs of their own
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.currency.FxRate]]")
      summonControl()
    }

    test("excluded from JSON: LazyFxRateProvider") {
      // This one is excluded for a stronger reason than a missing codec, and the proof says so:
      // the class is visible only within its own package, so no code outside that package can
      // name the type at all, let alone ask for its codec. Its name therefore fails even the
      // control every other proof here passes, and what a caller actually holds - the provider
      // interface that `FxRateProvider.lazily` returns - has no codec either.
      assertDoesNotCompile("type Probe = com.opengamma.strata.basics.currency.LazyFxRateProvider")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.currency.LazyFxRateProvider]]")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.currency.FxRateProvider]]")
      assertCompiles("type Probe = com.opengamma.strata.basics.currency.FxRateProvider")
      summonControl()
    }

    test("excluded from JSON: FxConvertible") {
      assertCompiles(
        "type Probe = com.opengamma.strata.basics.currency.FxConvertible[" +
          "com.opengamma.strata.basics.currency.CurrencyAmount]")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.currency.FxConvertible[" +
          "com.opengamma.strata.basics.currency.CurrencyAmount]]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.currency.FxConvertible[" +
          "com.opengamma.strata.basics.currency.CurrencyAmount]]]")
      summonControl()
    }

    test("excluded from JSON: Resolvable") {
      assertCompiles(
        "type Probe = com.opengamma.strata.basics.Resolvable[com.opengamma.strata.basics.currency.Payment]")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.Resolvable[" +
          "com.opengamma.strata.basics.currency.Payment]]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.Resolvable[" +
          "com.opengamma.strata.basics.currency.Payment]]]")
      summonControl()
    }

    test("excluded from JSON: ResolvableCalculationTarget") {
      assertCompiles("type Probe = com.opengamma.strata.basics.ResolvableCalculationTarget")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.ResolvableCalculationTarget]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.ResolvableCalculationTarget]]")
      summonControl()
    }

    test("excluded from JSON: CalculationTarget") {
      assertCompiles("type Probe = com.opengamma.strata.basics.CalculationTarget")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.CalculationTarget]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.CalculationTarget]]")
      summonControl()
    }

    test("excluded from JSON: CalculationTargetList") {
      // a product is only as serializable as its fields, and the element type of this one is a
      // marker with no data, so the list follows it out of the inventory
      assertCompiles("type Probe = com.opengamma.strata.basics.CalculationTargetList")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.CalculationTargetList]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.CalculationTargetList]]")
      summonControl()
    }

    test("excluded from JSON: Matrix") {
      assertCompiles("type Probe = com.opengamma.strata.collect.array.Matrix")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.array.Matrix]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.array.Matrix]]")
      // the one implementation of it is covered, through the codec support of its module
      assertCompiles("com.opengamma.strata.collect.json.Codecs.doubleMatrixCodec")
      summonControl()
    }

    test("excluded from JSON: Named") {
      assertCompiles("type Probe = com.opengamma.strata.collect.Named")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.Named]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.Named]]")
      summonControl()
    }

    test("excluded from JSON: NamedEnum") {
      assertCompiles(
        "type Probe = com.opengamma.strata.collect.named.NamedEnum[com.opengamma.strata.basics.currency.Currency]")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.collect.named.NamedEnum[" +
          "com.opengamma.strata.basics.currency.Currency]]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.collect.named.NamedEnum[" +
          "com.opengamma.strata.basics.currency.Currency]]]")
      summonControl()
    }

    test("excluded from JSON: TypedStringCompanion") {
      assertCompiles(
        "type Probe = com.opengamma.strata.collect.TypedStringCompanion[com.opengamma.strata.collect.Named]")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.collect.TypedStringCompanion[" +
          "com.opengamma.strata.collect.Named]]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.collect.TypedStringCompanion[" +
          "com.opengamma.strata.collect.Named]]]")
      summonControl()
    }

    test("excluded from JSON: ArgCheck") {
      assertCompiles("type Probe = com.opengamma.strata.collect.ArgCheck.type")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.ArgCheck.type]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.ArgCheck.type]]")
      summonControl()
    }

    test("excluded from JSON: Validate") {
      assertCompiles("type Probe = com.opengamma.strata.collect.Validate.type")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.Validate.type]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.Validate.type]]")
      summonControl()
    }

    test("excluded from JSON: Collections") {
      assertCompiles("type Probe = com.opengamma.strata.collect.Collections.type")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.Collections.type]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.Collections.type]]")
      summonControl()
    }

    test("excluded from JSON: DoubleArrayMath") {
      assertCompiles("type Probe = com.opengamma.strata.collect.DoubleArrayMath.type")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.DoubleArrayMath.type]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.DoubleArrayMath.type]]")
      summonControl()
    }

    test("excluded from JSON: Resources") {
      assertCompiles("type Probe = com.opengamma.strata.collect.io.Resources.type")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.io.Resources.type]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.io.Resources.type]]")
      summonControl()
    }

    test("excluded from JSON: Codecs") {
      assertCompiles("type Probe = com.opengamma.strata.collect.json.Codecs.type")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.collect.json.Codecs.type]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.collect.json.Codecs.type]]")
      summonControl()
    }

    // The four aliases are handled differently: an alias has no companion, so nothing here can
    // own a codec for one, and whether the JSON library publishes an instance for the underlying
    // sum or accumulating type is that library's business. Asserting either outcome would make
    // this suite brittle about something it does not own, so what is asserted is the pair of facts
    // that matter: the name is an alias, and the failure type it carries is itself covered.

    test("excluded from JSON: FailureOr") {
      assertCompiles("type Probe = com.opengamma.strata.collect.result.FailureOr[Int]")
      assertDoesNotCompile("com.opengamma.strata.collect.result.FailureOr.encoder")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.collect.result.Failure]]")
      assertCompiles("implicitly[io.circe.Decoder[com.opengamma.strata.collect.result.Failure]]")
    }

    test("excluded from JSON: ResultNec") {
      assertCompiles("type Probe = com.opengamma.strata.collect.result.ResultNec[Int]")
      assertDoesNotCompile("com.opengamma.strata.collect.result.ResultNec.encoder")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.collect.result.Failure]]")
      assertCompiles("implicitly[io.circe.Decoder[com.opengamma.strata.collect.result.Failure]]")
    }

    test("excluded from JSON: ValidatedFailures") {
      assertCompiles("type Probe = com.opengamma.strata.collect.result.ValidatedFailures[Int]")
      assertDoesNotCompile("com.opengamma.strata.collect.result.ValidatedFailures.encoder")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.collect.result.Failure]]")
      assertCompiles("implicitly[io.circe.Decoder[com.opengamma.strata.collect.result.Failure]]")
    }

    test("excluded from JSON: ValueWithFailures") {
      assertCompiles("type Probe = com.opengamma.strata.collect.result.ValueWithFailures[Int]")
      assertDoesNotCompile("com.opengamma.strata.collect.result.ValueWithFailures.encoder")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.collect.result.Failure]]")
      assertCompiles("implicitly[io.circe.Decoder[com.opengamma.strata.collect.result.Failure]]")
    }

    test("excluded from JSON: Index") {
      assertCompiles("type Probe = com.opengamma.strata.basics.index.Index")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.Index]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.index.Index]]")
      // the four leaf families the head closes over are each covered
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.IborIndex]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.OvernightIndex]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.PriceIndex]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.FxIndex]]")
      summonControl()
    }

    test("excluded from JSON: RateIndex") {
      assertCompiles("type Probe = com.opengamma.strata.basics.index.RateIndex")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.RateIndex]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.index.RateIndex]]")
      summonControl()
    }

    test("excluded from JSON: FloatingRateIndex") {
      assertCompiles("type Probe = com.opengamma.strata.basics.index.FloatingRateIndex")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.FloatingRateIndex]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.index.FloatingRateIndex]]")
      summonControl()
    }

    test("excluded from JSON: FloatingRate") {
      assertCompiles("type Probe = com.opengamma.strata.basics.index.FloatingRate")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.FloatingRate]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.index.FloatingRate]]")
      // the name that is data rather than an abstraction is covered
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.FloatingRateName]]")
      summonControl()
    }

    test("excluded from JSON: IndexObservation") {
      assertCompiles("type Probe = com.opengamma.strata.basics.index.IndexObservation")
      assertDoesNotCompile("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.IndexObservation]]")
      assertDoesNotCompile("implicitly[io.circe.Decoder[com.opengamma.strata.basics.index.IndexObservation]]")
      // each of the four observations that carry data is covered
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.IborIndexObservation]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.OvernightIndexObservation]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.FxIndexObservation]]")
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.index.PriceIndexObservation]]")
      summonControl()
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Registers the proof that the codec surface of these modules is the inventory and nothing more.
   *
   * The inventory names a serializable ''family'' where one exists, and the calendars are where
   * the difference shows: every calendar built from holiday dates is covered by the
   * `HolidayCalendar` row, while the narrowing of that family's codec to the one member is visible
   * only inside the package declaring it. The surface a consumer sees therefore carries the
   * fifty-eight codecs of the inventory and not a fifty-ninth for a member of one of them.
   *
   * This is '''not''' an exclusion: a row in [[excludedTypes]] would state that the type has no
   * codec, contradicting the round trip the family performs over these very values.
   */
  private def registerUnpublishedCodecProofs(): Unit =
    test("not published: the narrowing of the calendar codec to one member") {
      // the type itself is public - the summons below fail for the codec and for nothing else
      assertCompiles("type Probe = com.opengamma.strata.basics.date.ImmutableHolidayCalendar")
      assertDoesNotCompile(
        "implicitly[io.circe.Encoder[com.opengamma.strata.basics.date.ImmutableHolidayCalendar]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Decoder[com.opengamma.strata.basics.date.ImmutableHolidayCalendar]]")
      assertDoesNotCompile(
        "implicitly[io.circe.Codec[com.opengamma.strata.basics.date.ImmutableHolidayCalendar]]")
      // nor by name, which is the narrower fact: the member is declared, and not for this package
      assertDoesNotCompile("com.opengamma.strata.basics.date.ImmutableHolidayCalendar.codec")
      // and the positive control, which is what makes the three refusals mean something: the
      // family's codec is published, and one of these very calendars reaches JSON through it
      assertCompiles("implicitly[io.circe.Encoder[com.opengamma.strata.basics.date.HolidayCalendar]]")
      assertCompiles("implicitly[io.circe.Decoder[com.opengamma.strata.basics.date.HolidayCalendar]]")
      encodesTo[HolidayCalendar](testCalendar, TestCalendarDocument)
      decoded[HolidayCalendar](TestCalendarDocument, "a calendar") shouldBe (testCalendar: HolidayCalendar)
      summonControl()
    }

  //-------------------------------------------------------------------------
  /**
   * Registers the whole suite, which is what an ordinary run performs.
   *
   * The order is the order of the phases of the plan: the report first, so that it reaches the
   * output before the properties begin, then the round trips, the stability of the bytes, the
   * document forms, the refusals - of a document describing a value no factory would build, and
   * then of a document asking for more of a collection than this port reads - the consolidated
   * Java test, the proofs of absence, and the proof that the codec surface is the inventory and
   * nothing besides.
   */
  private def registerFullSuite(): Unit = {
    registerCoverageReport()
    registerRoundTrips()
    registerReEncodeStability()
    registerByteStability()
    registerFamilyShapes()
    registerOptionalFieldShapes()
    registerNumericShapes()
    registerDateTimeShapes()
    registerNamedShapes()
    registerDayCountShapes()
    registerHolidayCalendarShapes()
    registerCurrencyShapes()
    registerFxMatrixShapes()
    registerObservationShapes()
    registerInvalidPayloads()
    registerCollectionCeilings()
    registerJodaBeansConsolidation()
    registerExclusionProofs()
    registerUnpublishedCodecProofs()
  }

  //-------------------------------------------------------------------------
  // The registration itself, which is the last thing this constructor does. Every value above is
  // defined and none computed - the fixtures, the two inventory lists and the five instances of
  // each case are read on first use - so this switch is reached having built nothing, which the
  // count of codec acquisitions the audit prints asserts.
  //
  // An ordinary run registers the whole suite; an audit run registers exactly one test, which is
  // why the switch is here rather than inside a test: round trips running in the baseline mode
  // would load every codec class and leave the audit's difference empty.
  sys.props.get(AuditModeProperty) match {
    case None => registerFullSuite()
    case Some(BaselineMode) => registerAuditMode(BaselineMode)
    case Some(CodecMode) => registerAuditMode(CodecMode)
    case Some(other) => registerUnknownAuditMode(other)
  }
}
