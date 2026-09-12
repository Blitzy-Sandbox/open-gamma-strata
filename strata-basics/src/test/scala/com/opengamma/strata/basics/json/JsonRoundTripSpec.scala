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
 * The record standing in for the mock bean of the Java serialization test.
 *
 * The Java `MockSerBean` carried four properties: a business day convention, a holiday calendar,
 * a day count, and a list of bare objects. The first three are reproduced here as typed fields
 * of a product whose codec is derived when this file is compiled. The fourth has no counterpart
 * and cannot have one, for the reason given at `test_jodaBeans_serialize`.
 *
 * It is declared at the top of this file rather than inside the suite so that the compile-time
 * derivation of its codec sees a stable type rather than one dependent on an instance of the
 * suite, and it is visible only within this package because nothing outside this file has any
 * use for it.
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
 * Both halves are derived semi-automatically from the declared shape of the record, which is
 * the derivation every product of this port uses; the automatic variant is used nowhere. The
 * encoder is wrapped in the port's policy for products so that the record is written exactly as
 * a product of the library would be, even though it declares no optional field.
 */
private[json] object MockSerRecord {

  /** The JSON encoding of the record, derived at compile time. */
  implicit val encoder: Encoder[MockSerRecord] = Codecs.dropNulls(deriveEncoder[MockSerRecord])

  /** The JSON decoding of the record, derived at compile time. */
  implicit val decoder: Decoder[MockSerRecord] = deriveDecoder[MockSerRecord]
}

/**
 * A type this port deliberately does not serialize, and the reason it does not.
 *
 * The reason is one line of text, because it is written into the printed report as the tail of
 * a line and a reader of that report parses the report by lines.
 *
 * @param fqcn  the fully qualified name of the type
 * @param reason  why the type carries no codec, in one line
 */
private[json] final case class ExcludedType(fqcn: String, reason: String)

/**
 * A value of a type the inventory does not cover, which an audit run generates in place of the
 * covered values it is not allowed to construct.
 *
 * Its fields are the leaf types the generators of the covered types are built from - text, a
 * whole number, a real number, a date, a time of day, a time zone and a day of the week - so
 * generating and rendering one exercises the generator machinery, the collection machinery and
 * the platform's date, time and zone machinery that generating and rendering a covered value
 * exercises. It names no type of either module, so constructing one initialises no companion
 * and therefore no codec. Those two properties together are what make it useful: it is the work
 * the two audit runs have in common, and having it in common is what leaves the difference
 * between them holding the codecs and nothing else.
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
 * @param planned  how many values the inventory plans for the type, which is read from the
 *   inventory rather than from what was generated and is therefore the same in both audit runs
 * @param digest  the digest of the work both runs do, which names no covered type
 * @param generated  how many values of the covered type were constructed, which is zero unless
 *   the run was asked to encode
 * @param roundTrips  how many of them encoded and decoded back to themselves, zero unless the
 *   run was asked to encode
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
 * The type itself appears in none of the fields: it is captured by the closures, which
 * [[codecCase]] builds while the type is still known. That is deliberate and not incidental -
 * a list of cases that carried its element type would have to be a list of an existential or
 * of `Any`, and this build rejects both.
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
 * The serialization suite of `strata-basics`, and the consolidation target of every Java
 * serialization test of this port.
 *
 * The library being ported serialized a value by reading its properties back from its own class
 * while the program ran, and its test suite checked that reflectively too: every immutable bean
 * was swept through `coverImmutableBean`, `coverBeanEquals` and `assertSerialization`, the last
 * of which wrote a value to an object stream, read it back and asserted the two were equal.
 * None of that machinery survives here, because reflection is exactly what this migration
 * removes from the serialization path. What replaces it is this suite: every codec is built by
 * the compiler from the declared shape of a type, and every type that has one is round-tripped
 * through generated values, with `decode(encode(a)) == a` standing in one for one for the
 * assertion the object-stream round trip made.
 *
 * ===What this suite is responsible for===
 *
 *   - '''Coverage.''' The set of types that are serializable, and the set that deliberately are
 *     not, is a closed inventory. This suite holds that inventory, prints it in a parseable
 *     form, and proves both halves of it: a covered type is round-tripped, and an excluded type
 *     is shown to have no codec at all by a proof the compiler performs.
 *   - '''Shape.''' The document a value produces is a contract, not an implementation detail, so
 *     the forms this port chose - a bare name for a named value, a wrapper object for a closed
 *     family, a tagged string for a value JSON cannot express - are pinned here against literal
 *     documents.
 *   - '''Stability.''' A value's document depends on the value and on nothing else: two equal
 *     values built in different orders encode to identical bytes, and re-encoding what was
 *     decoded reproduces what was written.
 *   - '''Refusal.''' A document that describes a value a factory of this library would not have
 *     built is refused, as a reported failure rather than a raised one.
 *
 * ===What it deliberately leaves to its siblings===
 *
 * The codec ''support'' - the helpers of
 * [[com.opengamma.strata.collect.json.Codecs]] exercised on their own - belongs to `CodecsSpec`
 * of `strata-collect`, so the shapes those helpers produce are asserted here only ''through''
 * the domain types that use them. Closedness of the named families and their alias tables belong
 * to `NamedEnumClosedSpec`; the failures of the factories themselves to `SmartConstructorSpec`
 * and `FailableSurfaceSpec`; the laws of the `cats` instances to `TypeclassLawsSpec`; and the
 * absence of a public `apply`, a `copy` or an external subtype to `ApiSurfaceSpec`. The
 * compile-time proofs written here are confined to the ''absence of a codec''.
 *
 * ===The naming convention of the tests===
 *
 * The migration manifest joins a Java test method to a test of this port on the pair of suite
 * class and test name, so the names below follow a fixed convention, which a generator of that
 * manifest can mirror:
 *
 * {{{
 * test_jodaBeans_serialize                  the consolidated Java method, named verbatim
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
 * Every name is unique within the suite and stable across runs.
 *
 * ===The round trip runs one way only===
 *
 * The property asserted is always `decode(encode(a)) == a`, never `encode(decode(j)) == j`. The
 * second is false by design for two types: the forms a holiday calendar and a `Bus/252` day
 * count ''accept'' are wider than the forms they ''write'', so `"GBLO+USNY"` is read as a
 * combined calendar and that calendar writes itself structurally. Asserting the document-first
 * direction would therefore forbid the leniency those two decoders exist to provide.
 *
 * ===The audit mode===
 *
 * Rule 6 of the migration prohibits reflection on the codec path, and the check for it is
 * performed on the classes the machine actually loads: this suite is run twice under a class
 * loading log, once without serializing anything and once serializing every covered value, and
 * the difference between the two sets of loaded classes must hold no class of any reflection
 * package and no class that refers to the reflection API. That is what the `codec.audit` system
 * property selects, and because the difference has to be attributable to serialization alone,
 * the property gates the registration of the ''whole'' suite rather than of one test.
 *
 * What the two runs have in common is deliberately drawn at the boundary of this port rather
 * than at the boundary of this suite's own bookkeeping. The run that is not serializing
 * constructs '''no value of any covered type''': it generates and renders values of
 * [[AuditProbe]] instead, which exercises the generator, collection, date and matcher machinery
 * that the other run also uses while naming no type of either module. It has to be that way
 * because a value of a covered type can only be built through that type's companion, and a
 * companion is where the codec of the type is held - so a run that built one would construct
 * that codec, and everything the construction of a codec touches would appear in ''both'' logs
 * and cancel out of the difference the audit exists to measure. Nothing in this class is
 * computed while it is constructed either: the fixtures and the five instances of every case are
 * read on first use. `registerAuditMode` below states what the difference attributes, and
 * asserts the three counts that make the statement checkable.
 */
class JsonRoundTripSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  //-------------------------------------------------------------------------
  // Phase A - the closed inventory of AAP section 0.6.4.
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

  /** The number of closed named families with a codec, from AAP section 0.6.4. */
  private val ExpectedNamedEnumTypes: Int = 14

  /** The number of text-identified values with a codec, from AAP section 0.6.4. */
  private val ExpectedParsedStringTypes: Int = 9

  /** The number of hand-written codecs, from AAP section 0.6.4. */
  private val ExpectedHandWrittenTypes: Int = 2

  /** The number of derived products with a codec, from AAP section 0.6.4. */
  private val ExpectedSemiautoTypes: Int = 29

  /** The number of explicitly shaped values with a codec, from AAP section 0.6.4. */
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
   * Together with the covered types derived from [[codecCases]] this list closes the inventory:
   * every public data type and every public contract type of `strata-collect` and
   * `strata-basics` appears in exactly one of the two, which is what makes the printed report a
   * statement about the modules rather than a list of what happened to be tested.
   *
   * Built on first use, like every other value of this class body, so that an audit run - which
   * prints no report - builds nothing it does not read. See the note on the audit mode below.
   *
   * What closure means here, precisely, because the modules publish more than data and contracts:
   * a type alias a module root re-exports is the '''same''' type under a second name, so it is
   * closed by the entry for the type it names; a named partial application such as
   * `RefDataReader` is a generic container of the standard library; and the value-type witness an
   * identifier carries holds a pattern rather than data. Those are audited by the port's
   * construction-kind inventory - `ApiSurfaceSpec`, which records one row per public surface of
   * either module - and the two inventories together account for the whole of it. Transcribed
   * reference-data rows are `private[basics]` and published by neither.
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
  // Every factory of a validated type reports a value it would not build rather than raising
  // it, so a fixture is unwrapped once through one of the two helpers below and a fixture that
  // could not be built is a failed test naming the cause - never a thrown exception and never a
  // `get` on an absent value.
  //
  // Every one of them is computed on first use. The fixtures below serve the document-form
  // tests and nothing else, and an audit run registers none of those tests, so building them
  // eagerly would have an audit run construct calendars, observations and a matrix of rates
  // that it never looks at - work that loads classes in both audit runs and so cancels out of
  // the very difference the audit measures. Computing them on demand keeps an audit run down to
  // the generation, and the encoding, of the values it is measuring.

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

  /** Builds an amount of the currency and value given. */
  private def amount(currency: Currency, value: Double): CurrencyAmount =
    required(CurrencyAmount.of(currency, value), s"$currency $value")

  /** Builds a decimal from its canonical text. */
  private def decimal(text: String): Decimal = required(Decimal.of(text), s"Decimal $text")

  /** The identifier of the calendar the Java holiday tests built their fixtures under. */
  private lazy val testCalendarId: HolidayCalendarId = HolidayCalendarId.of("Test1")

  /** The two holidays of the Java `HOLCAL_MON_WED` fixture, in ascending order. */
  private lazy val testHolidays: List[LocalDate] =
    List(LocalDate.of(2014, 7, 14), LocalDate.of(2014, 7, 16))

  /**
   * The calendar of the Java `ImmutableHolidayCalendarTest` fixture.
   *
   * Its identifier names no calendar this library builds in, which is what makes it a subject
   * of the structural document form: a calendar carrying a built-in identifier is a different
   * case, covered by its own test.
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
  // The two numeric types of `strata-collect` state their document form in the codec support of
  // that module rather than in their own companions, deliberately, so that the plain numeric
  // encoding of a double cannot be picked up by accident at a derivation site. The two are bound
  // here by name, which is the import the support intends and which keeps the choice visible in
  // the file that makes it; the remaining fifty-six types publish their own instances.
  //
  // These two are the only codec instances this suite owns rather than imports from a companion,
  // and they are read on first use for the same reason every case below acquires its instances on
  // first use: an audit run that is not encoding must not reach for a codec at all.

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
   * The serializing run generates that many values of every covered type; the other run
   * generates that many probe values and none of any covered type. The figure is part of the
   * plan rather than of what happened, so it is the same in both runs.
   */
  private val AuditValuesPerCase: Int = 8

  /** The generation size the audit mode uses, fixed for the same reason as the seed. */
  private val AuditGenerationSize: Int = 16

  /**
   * Draws the values of one type from the fixed seeds.
   *
   * Nothing here is drawn from the clock, from a random source or from the property-check
   * machinery, whose seeds vary between runs: a value is drawn from the fixed seed plus its
   * index, so a generator asked twice for the same index yields the same value.
   *
   * A generator that yields nothing from every one of the fixed seeds is a defect of the
   * generator rather than an empty case, so it fails the run naming the type.
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
   * This is the count the audit rests on, and it counts the '''cause''' of codec construction
   * rather than an acquisition of a codec that has already been built. A value of a covered type
   * can only be made by calling a factory on that type's companion; calling it initialises the
   * companion; and a companion is where the type's codec is held, in a field computed once when
   * the companion initialises. So a run that constructs no value of a covered type has
   * initialised no covered companion, has built none of their codecs, and has loaded nothing
   * that building one touches - which is what puts codec construction, and the code of every
   * dependency it reaches, inside the difference between the two runs rather than common to
   * them.
   *
   * It is an atomic counter for the same reason [[codecAcquisitions]] is: nothing here depends on
   * the increments being ordered, only on none of them being lost.
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
   * The runtime classes an audit run loads by name.
   *
   * A class here refers to the reflection API in its own body, for its own reasons and none of
   * this port's, and the serializing run would otherwise be the first to load it - which would
   * leave it in the class-load difference and have the check that reads the difference report
   * the platform's internals as though they were this port's codecs. Loading it in both runs
   * keeps it out. Each is named rather than provoked because no expression this suite can write
   * reaches it:
   *
   *   - the bootstrap of a symbol literal, which only generated code calls, and which finds the
   *     symbol class by name;
   *   - the platform's random-number class, which the test framework's own event serialization
   *     pulls in through the platform's thread-local generator.
   *
   * A class constant is a constant and not a reflective lookup: it resolves where it is read,
   * which loads the class and initialises nothing.
   */
  private val AuditRuntimeClasses: List[Class[_]] =
    List(classOf[scala.runtime.SymbolLiteral], classOf[java.util.Random])

  /**
   * The zones an audit run asks the platform for, chosen to reach the zone-rule provider.
   *
   * The platform finds its zone rules through a service lookup that is reflective, and the
   * codecs reach it through the zone of an index. Asking for a zone in both runs keeps the
   * platform's own lookup out of the difference.
   */
  private val AuditZones: List[String] =
    List("Europe/London", "America/New_York", "Asia/Tokyo", "UTC")

  /**
   * The generator of the value an audit run is allowed to construct.
   *
   * The combinators used here - a comprehension over choices, a weighted choice, a selection
   * from a collection and a string generator - are the combinators the generators of the covered
   * types are built from, so a run that draws from this generator has loaded the generator
   * machinery that the other run's covered generators also need.
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
   * Performs the work the two audit runs have in common, and returns its digest.
   *
   * Every step here is deliberate, and none of them names a covered type:
   *
   *   - values of [[AuditProbe]] are drawn from the fixed seeds and rendered, which is the
   *     generator machinery and the rendering of a product;
   *   - they are put into a set, copied into an array and collected into a buffer, which is the
   *     collection machinery whose generic array support refers to the reflection API;
   *   - every method of that collection and of that buffer which carries a lambda of its own is
   *     called, for the reason given where the calls are made;
   *   - their dates and times are formatted and their zones resolved, which is the platform's
   *     date, format and zone machinery, the last of which finds its provider reflectively;
   *   - the classes of [[AuditRuntimeClasses]] are loaded by name.
   *
   * The digest is a function of that work and of nothing else, so the two runs produce the same
   * digest. An operator who diffs the two printed lines and finds them differing has found
   * either a generator that is not deterministic or an inventory that changed between the runs,
   * both of which would make the class-load difference mean something other than what the audit
   * claims for it.
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
   * Exercises the two standard-library classes that reach the reflection API, and returns a
   * digest of the results.
   *
   * The two are the aggregating half of every collection and the growable buffer, both of which
   * call the reflection API to read the length of an array of unknown element type. They are
   * loaded by the work above in either run, but that is not sufficient on its own: the check
   * that reads the class-load difference cannot disassemble a lambda under its own name, so it
   * inspects the '''whole''' of the class the lambda was defined in - and a lambda of one of
   * these two classes that runs for the first time in the serializing run would put that class
   * in front of the check. The methods called below are exactly the methods of those two classes
   * that carry a lambda, read from the compiled standard library of the version this build pins,
   * so every one of those lambdas runs in both runs and none of them can land in the difference.
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
   * An audit run reads this for every covered type, in both modes, so the digest the run prints
   * covers the inventory as well as the common work: a type added to or removed from the
   * inventory between the two runs changes it, and the check that compares the two runs' digests
   * would report that rather than proceeding on a difference measured against a different
   * inventory. It names the type only as text, so reading it constructs nothing.
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
   * How many instances of one case the encoding half of an audit run acquires.
   *
   * The three are both halves of the codec and the equality the round trip compares with, which
   * are exactly the instances a run that does not encode has no use for.
   */
  private val CodecInstancesPerCase: Int = 3

  /**
   * How many codec-dependent instances this suite has acquired since it was constructed.
   *
   * Three of the five instances a case needs - both halves of the codec and the equality the
   * round trip compares with - are acquired only where they are used, and each acquisition is
   * counted here. The count is therefore a measurement of the suite's own behaviour, and the
   * audit run asserts it: the baseline mode must finish having acquired '''none''' of them, and
   * the codec mode exactly [[CodecInstancesPerCase]] for each covered type. That assertion is
   * the standing evidence that the baseline run's class loading excludes this suite's codec
   * acquisition, which is what makes the difference between the two runs attributable to the
   * codecs rather than to the bookkeeping around them.
   *
   * It is an atomic counter rather than a plain counter because a suite may run beside another
   * and because a `forAll` property may evaluate its body on more than one thread; nothing here
   * depends on the increments being ordered, only on none of them being lost.
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
   * The five instances required are exactly the five this suite needs of a serializable type,
   * and requiring them here is itself part of the coverage: a type that reaches this method has
   * a generator, both halves of a codec, an equality and a rendering, so a type whose codec was
   * forgotten cannot be listed as covered. That remains a '''compile-time''' requirement: the
   * five are implicit parameters, so a type missing one of them cannot be listed at all.
   *
   * ===Why all five arrive by name===
   *
   * Taking them by value would summon all five the moment a case is built, and the cases are
   * built to populate a list in the class body - before the constructor reaches the switch that
   * decides what this run is. A baseline audit run would then have acquired every encoder and
   * every decoder of both modules before its only test started, and whatever the codec path
   * loads on the way to those instances would appear in ''both'' class-load logs and be
   * subtracted from the difference the audit exists to measure. By name, building the list
   * evaluates nothing at all: each instance is summoned by the compiler here, exactly as before,
   * and read at the first use of the parameter.
   *
   * Each is bound to a value computed on first use, so a parameter is read at most once however
   * many times a closure below refers to it, and the three that the encoding half needs are read
   * through [[acquired]] so that the reading is counted. The generator and the rendering are by
   * name for the same reason and are counted differently: the values a generator produces are
   * counted by [[auditValues]], because constructing one is what initialises the companion that
   * holds the codec, and the audit asserts that the run which is not serializing constructs
   * none.
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

    // What an audit run does with this case, in whichever of the two modes is asking.
    //
    // What the two runs have in common here is the digest of this entry of the inventory, and
    // that entry is text: reading it constructs no value, initialises no companion and builds no
    // codec. Everything that would - drawing a value through the type's factory, reading the two
    // halves of its codec, encoding and decoding - happens in the serializing run alone. So the
    // classes brought in by the companion, by the construction of the codec held in it, and by
    // every dependency either of those reaches are loaded in that run only, which is what leaves
    // them inside the difference between the two runs where the check can inspect them.
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
   * This list is the transcription of the covered inventory of AAP section 0.6.4, and it is the
   * '''one''' place that inventory is written down: the printed report, the per-type tests and
   * the audit all read it, so the report cannot claim a type the tests do not exercise and a
   * test cannot exercise a type the report does not name. The alternative - a list of names
   * beside a list of tests - is two things to keep in step and one of them silently wrong.
   *
   * The order is by category and then by name, which is also the order the report prints, so a
   * reader comparing the two reads them in the same sequence.
   */
  private lazy val codecCases: List[CodecCase] = List(
    // The fourteen closed named families: a member is the JSON string of its name, which is the
    // same text the ported library wrote through its string conversion.
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
   * The printed inventory, which is the artifact the acceptance gate reads.
   *
   * The block is built as one string and emitted by one call, because suites may run beside one
   * another and two calls could interleave with a third party's output between them. Every line
   * is derived from the two inventory lists and from nothing else - no time, no hash code, no
   * path, and no iteration order of a hashed collection - and both sections are sorted
   * explicitly, so two runs of this suite print byte-identical blocks.
   *
   * Sorting the rendered lines sorts the covered section by category and then by name, the
   * prefix being constant, which is the order the gate expects to read.
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
   * Registers the report and the invariants of the inventory it prints.
   *
   * Registered first, so that the block reaches the output early in the run and an operator
   * collecting it does not have to wait for the properties. The invariants asserted alongside it
   * are the ones that make the report trustworthy: no type is named twice, no type is both
   * covered and excluded, the five categories hold the numbers the specification states, and the
   * report is reproducible within a run.
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
   * Registers one round-trip test for each covered type.
   *
   * One test per type rather than one test over the list: a failure then names the type in its
   * own right, and the migration manifest can join a row to it.
   */
  private def registerRoundTrips(): Unit =
    codecCases.foreach(entry => test(s"round-trip: ${entry.typeName}")(entry.roundTrip()))

  /**
   * Registers the property that a value's document depends on the value and on nothing else.
   *
   * This runs over every covered type in one test, because it is one property rather than
   * fifty-eight: what it asserts is a statement about the codecs as a body of work, and the
   * per-type detail that could fail on its own is already covered by the round trip above.
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

  /** The name of the single test an audit run registers. */
  private val AuditTestName: String = "codec audit: deterministic value enumeration"

  /**
   * Registers the single test of an audit run.
   *
   * The run performs the work the two modes have in common - [[auditCommonWork]], plus the
   * digest of every entry of the inventory - and, in the serializing mode only, generates the
   * values of every covered type and encodes and decodes them. Nothing else differs between the
   * two runs, which is what makes the difference between the classes they load attributable to
   * serialization.
   *
   * Three lines are printed, and each of the figures on them is asserted rather than merely
   * printed:
   *
   *   - the mode, the number of values the inventory plans for and the digest of the common
   *     work. Both are read from the inventory and the common work alone, so they are expected
   *     to be ''identical'' in the two runs: an operator who diffs the two lines and finds them
   *     differing has found either a generator that is not deterministic or an inventory that
   *     changed between the runs, and either would make the class-load difference mean something
   *     other than what this audit claims for it.
   *   - the mode and how many codec-dependent instances the run acquired, which is `0` for the
   *     baseline run and three per covered type for the serializing run.
   *   - the mode and how many values of a covered type the run '''constructed''', which is `0`
   *     for the baseline run and one per planned value for the serializing run.
   *
   * Both modes finish on the same comparisons of integers, so the assertion machinery each run
   * loads is the same as well.
   *
   * ===What the class-load difference attributes===
   *
   * The difference attributes everything this port does to turn a value into a document and back,
   * and the count of constructed values asserted to be zero is what establishes that:
   *
   *   - '''the acquisition and execution of the codecs.''' The baseline run reads no encoder, no
   *     decoder and no equality - the count of acquisitions asserted to be zero is the evidence -
   *     so every class that reading or running one brings in appears in the serializing run alone.
   *   - '''the initialisation of the covered companions, and the construction of the codecs they
   *     hold.''' A value of a covered type can only be built through a factory on that type's
   *     companion, and a companion is where its codec is held in a field computed once when the
   *     companion initialises. The baseline run constructs no such value, so it initialises no
   *     such companion; the serializing run initialises all of them. Every class reached while a
   *     codec is constructed - including the code of a dependency, which no scan of this port's
   *     own classes would ever look at - is therefore loaded in the serializing run only and sits
   *     in the difference, where the check that reads it disassembles it.
   *
   * The boundary is the language runtime and the platform, and it is drawn deliberately rather
   * than left where it fell. [[auditCommonWork]] exercises the generator, collection, date,
   * format and zone machinery, and loads the classes of [[AuditRuntimeClasses]], in ''both''
   * runs. Those classes refer to the reflection API in their own bodies for their own reasons -
   * the standard library's generic array support calls it to read the length of an array, the
   * bootstrap of a symbol literal and the platform's zone-rule provider call it to find a class
   * by name - and none of that is this port's serialization. Loading them in both runs keeps
   * them out of the difference, so what the check reports on is the codecs.
   *
   * ===Why the work is done here and not inside the test===
   *
   * The audit work runs while this method runs - that is, while the suite is being constructed -
   * and the single test it registers only prints and asserts what the work produced. That is a
   * decision about what the difference is allowed to pick up, and it was made from measured
   * behaviour rather than taste.
   *
   * The runner reports through a thread of its own, and that thread loads classes while the
   * thread running a test loads its own. Doing the codec work inside the test body overlaps the
   * two, and an overlap puts classes into the difference that no codec ever touched: the
   * reporting thread submits its work to a shared pool, the pool asks the platform for this
   * thread's probe, and the platform's random-number class is loaded - in the encoding run only,
   * because that is the run whose test body is long enough to overlap. Measured over eight runs
   * of the encoding mode, that happened in seven of them, and since the class in question
   * initialises itself by reading one of its own fields through the reflection API, a check that
   * disassembles every class of the difference would attribute the reporting thread's behaviour
   * to this port's codecs. Doing the work before any test is registered - and therefore before
   * the first event is reported - removes the overlap: over three further runs of the encoding
   * mode the difference held nothing of the sort, while remaining the same set of codec classes
   * it always held.
   *
   * The cost is stated rather than hidden: a generator that yields nothing from every fixed seed
   * now aborts the audit run while the suite is constructed instead of failing its test. The run
   * fails either way, and the check that consumes these runs reads their exit status, so what
   * changes is the shape of the report and not whether the defect is caught.
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
        s"the $mode run constructed $constructed value(s) of a covered type, and a value can " +
          s"only be built through the companion that holds the type's codec: ") {
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
   * Registers the failure reported for a mode this suite does not define.
   *
   * A misspelled property would otherwise run the whole suite under a class loading log and
   * produce a difference of every class the codecs touch, which reads exactly like a passing
   * audit of the wrong thing. Naming the two modes it accepts is cheaper than that.
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
   * The expected text is matched as a fragment rather than as the whole message: the wording of
   * a failure belongs to the factory that reported it, and the shape the codec support wraps it
   * in - several accumulated messages joined by a semicolon and a space - is asserted separately.
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

  /** The document of the calendar of the Java `ImmutableHolidayCalendarTest` fixture. */
  private val TestCalendarDocument: String =
    """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":2014,""" +
      """"holidays":["2014-07-14","2014-07-16"],"workingWeekendDays":[]}}"""

  /** The document of the calendar declaring a weekend date to be a business day. */
  private val WorkingWeekendDocument: String =
    """{"Immutable":{"id":"XCAL","weekendDays":["SATURDAY","SUNDAY"],"startYear":2020,""" +
      """"holidays":["2020-01-01","2020-12-25"],"workingWeekendDays":["2020-03-07"]}}"""

  /** The adjustment the Java serialization test used, over the weekend-only calendar. */
  private lazy val satSunModifiedFollowing: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)

  /** The Ibor observation of the Java `IborIndexObservationTest` fixture. */
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
   * The connected matrix the Java `FxMatrixTest` builds, whose fifth rate is zero.
   *
   * The rate of zero is the point of it: the reciprocal recorded for a zero rate is infinite, so
   * this matrix holds a value JSON has no syntax for and exercises the tagged form inside a
   * structure of two dimensions. The rates arrive in an order in which one of them cannot yet be
   * placed, which the factory tolerates by holding it back - the behaviour the builder being
   * ported had.
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
   * holds rather than a list transcribed here: all forty-five roll conventions and all two
   * hundred and seventy-one Ibor indices are asserted without any of their names appearing in
   * this file. Those two figures are the ''memberships'' of the closed families, which is what
   * `values` answers with and therefore what this method sweeps. They are not the counts of
   * named constants: `RollConventions` publishes a constant for each of its forty-five members,
   * while `IborIndices` publishes one hundred and thirteen and the remaining one hundred and
   * fifty-eight indices are reached through the family's own lookup - and all two hundred and
   * seventy-one are swept here either way.
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
   * This is the check the round trip of a calendar carrying holiday data needs and the equality
   * of the type cannot give: two calendars of one identifier are equal whatever dates they hold,
   * as they were in the library being ported, so a codec that dropped every holiday would still
   * satisfy `decode(encode(a)) == a`. Walking the years instead compares what the calendars
   * actually say.
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
   * The years every calendar the generator produces falls within, with a year of margin.
   *
   * The generator draws its first year from a fixed window and spans three years from it, so a
   * walk of this range covers every holiday a generated calendar can hold and a year on each
   * side of it, where both calendars fall back to their weekend and must still agree.
   */
  private val GeneratedCalendarFirstYear: Int = 2009

  /** The last year of the range walked for a generated calendar. */
  private val GeneratedCalendarLastYear: Int = 2033

  //-------------------------------------------------------------------------
  /**
   * Registers the shape a closed family takes, and the absence of a discriminator.
   *
   * A closed family is a single-key object whose key names the member, which is the default the
   * derivation produces and the shape this port adopted. The absence of a discriminator field is
   * worth asserting in its own right, because a discriminator layout would require a library
   * this build deliberately does not depend on: finding one would mean that dependency had
   * arrived.
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
      // discriminator: it is the property of that name the ported bean declared, and its value
      // is a member of a named family rather than the name of a constructor.
      encodesTo(ValueAdjustment.ofReplace(1.5d), """{"modifyingValue":1.5,"type":"Replace"}""")
    }
  }

  /**
   * Registers the treatment of a field that holds no value, and of one that holds nothing.
   *
   * The two are different and the difference is visible: a field holding no value is left out of
   * the document, while a field holding an empty collection is written as an empty collection.
   * That follows from the policy being the removal of absent values rather than of empty ones,
   * and it is asserted rather than assumed because a reader of a document needs to know which.
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
      // the policy removes a property holding no value, which is not the same as removing an
      // empty one: a schedule of no steps writes an empty list, and a failure with no attributes
      // writes an empty object, so a reader can tell "none" from "not stated"
      // `ALWAYS_1` is the schedule `ValueSchedule.of(1.0d)` builds, named as a constant so that
      // this assertion reads a schedule rather than the outcome that validated factory answers
      // with
      encodesTo(ValueSchedule.ALWAYS_1, """{"initialValue":1.0,"steps":[]}""")
      encodesTo[Failure](Failure.Other("nothing to add"), """{"Other":{"message":"nothing to add","attributes":{}}}""")
      encodesTo[HolidayCalendar](testCalendar, TestCalendarDocument)
    }
  }

  /**
   * Registers the single policy of this port for a double, and the two numeric shapes.
   *
   * The three values JSON has no syntax for are carried as strings, and exactly those three
   * spellings are read back. The strictness is the assertion: a differently cased tag, a tag
   * with a sign it does not use, and the digits of a number delivered as text are all refused,
   * which is what keeps the representation a decision rather than a guess.
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

    test("round-trip: ValueDerivatives carrying an array of more than a million elements") {
      // How long an array a document may state is decided by what the factories of this port can
      // build and not by a figure the reader chose: a decoder that refused a length the encoder
      // produces would make `decode(encode(x))` fail for a value this library itself creates. The
      // length below is that statement made concrete. It is one element beyond the ceiling a
      // previous revision of the codec support imposed on an array - that ceiling was 1 << 20
      // elements, applied to the payload before a single element was read - so this is the exact
      // payload that revision refused, and the assertion here is that it round-trips.
      //
      // The value is nested rather than bare, which is the half of the defect no other test
      // covers: the array is a field of a derived product, so the refusal would have arrived
      // from inside the product's decoder and the value carrying it - a function and its
      // derivatives, as produced by a differentiation of a curve with one point per basis
      // point - could not have been read back at all.
      //
      // One such value, and one only: the document holds over a million numbers and every one of
      // them is a JSON value while it is being read, so this is the largest payload in the suite
      // by two orders of magnitude and there is nothing a second one would add.
      //
      // The round trip is asserted through the JSON tree rather than through its text, because
      // the withdrawn ceiling was applied to the tree - it measured the array of the cursor
      // before reading it - so this is the path that exercised it.
      val elements = (1 << 20) + 1
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
   * Four of the five date and time types are taken from the JSON library unchanged and one - the
   * day of the week - is supplied by the codec support of this port. Both are asserted here
   * through a domain type rather than in isolation, because the isolated assertions belong to the
   * suite of that support and what matters here is what a document of ''this'' module looks like.
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
   * Registers the identity of every named value: the name it has always had.
   *
   * The general case is driven from each family's own members, so the assertion covers whatever
   * the family holds rather than a list of names transcribed here. Only the identities the
   * migration plan names explicitly are written out, because those are the ones a reader of the
   * plan will look for.
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
   * Registers the one family whose members do not all take the same form.
   *
   * Twenty-one day counts are named and nothing else; the twenty-second carries a calendar,
   * which is part of the value and has to survive the round trip even where an application built
   * that calendar itself. The two forms, and the check that the name of the structural form
   * agrees with the calendar beside it, are what this covers.
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
   * any other calendar is written structurally, because its dates are the whole of what it is.
   * Two calendars of one identifier being equal - the equality the library being ported had -
   * means the round trip of the structural form has to be checked by walking the years as well,
   * which [[sameHolidays]] does.
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
   * Registers the forms of the currency values, and the one distinction between them.
   *
   * An amount carries a number, while the two money types carry the text of a decimal. The
   * difference is the point of those types - a JSON number is read back through a binary
   * floating point value by most readers, which is the representation they exist to avoid - so
   * it is pinned in one test where a reader sees both.
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
   * Registers the form of a matrix of rates, and what a document of one is checked for.
   *
   * The checks are structural and only structural, which is a decision rather than an omission:
   * the builder being ported accepted whatever rates it was given, so a matrix that this decoder
   * refused for being unrealistic would be a matrix that could be built but not read back.
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
   * Three of the four observations are rebuilt from the two properties a document really
   * determines, and the derived properties the document states are compared against what the
   * index derives. Without that comparison a document stating the dates of some other index
   * would decode into a value that compared equal to the one encoded while describing something
   * else, because the equality of those types ignores the derived properties.
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
   * Three of the four are the reason the collections inside those types are sorted ones, and the
   * fourth is the deliberate exception. A matrix of rates holds its currencies in the order they
   * occupy in it, and that order is part of the value: two matrices built in different orders are
   * '''not equal''', so each writes its own order and there is nothing to compare between them.
   * The document of a matrix is therefore stable per value without being canonical across the
   * orders one could be built in, which is asserted here rather than assumed.
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
   * Registers the documents this port refuses, one test per family that can refuse one.
   *
   * Every refusal here is a '''reported''' failure - a left-hand result carrying the reason -
   * and none of these tests catches anything. That is the whole point of routing a validated
   * type's decoder through its own factory: a document that would build a value the factory
   * forbids becomes a decoding failure carrying the reason the factory gave, and the reason is
   * asserted as a fragment of the message so that the wording stays the factory's to own.
   *
   * What is ''not'' tested here is the factory itself: whether a particular input is acceptable
   * belongs to the smart-constructor suite. What is tested is that the decoder consults it.
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
      // the factory of this type is total - a currency and a run of values always describe a run
      // of amounts - so its decoder can only be refused by the codecs of its two fields, and
      // that is what is asserted rather than a validation that does not exist
      refused[CurrencyAmountArray]("""{"currency":"XYZ","values":[1.0]}""", "Currency name not found")
      refused[CurrencyAmountArray]("""{"currency":"GBP","values":["nan"]}""", TaggedDoubleMessage)
    }

    test("invalid: a run of amounts whose size disagrees with its arrays") {
      refused[MultiCurrencyAmountArray](
        """{"size":2,"values":{"GBP":[1.0]}}""",
        "Arrays must have the same size")
      refused[MultiCurrencyAmountArray]("""{"size":-1,"values":{}}""", "must not be negative")
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
      // as with the array of amounts, this type's own factory is total, so the refusal comes
      // from the step nested inside the document and is reported at that step's position
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
  }

  //-------------------------------------------------------------------------
  /**
   * Registers the Java serialization test this suite consolidates.
   *
   * The original built a mock bean of four properties, wrote it through the bean library and
   * read it back, asserting the two were equal, and it did that for three calendars: the one with
   * no holidays, the weekend-only one, and London. The three typed properties are reproduced
   * exactly, over the same convention and day count, and the round trip asserts the same
   * equality; the fourth property is discussed below.
   */
  private def registerJodaBeansConsolidation(): Unit =
    test("test_jodaBeans_serialize") {
      // the three calendars of the original, the last of which the original reached by name
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

      // The fourth property of the original was a list of bare objects, and it has no
      // counterpart here by design. A bean library could write such a list only by asking each
      // element for its runtime class and looking up a writer for it, which is precisely the
      // reflection this migration removes from the serialization path; and a list of bare
      // objects is also the collection type the port's public surface may not name. So the
      // absence is a compile-time fact rather than an omission: no encoder for such a list
      // exists, while a list of a type that has one is perfectly serializable.
      assertDoesNotCompile("implicitly[io.circe.Encoder[List[Any]]]")
      assertCompiles("implicitly[io.circe.Encoder[List[com.opengamma.strata.basics.date.DayCount]]]")
    }

  //-------------------------------------------------------------------------
  /**
   * The control that the summon every absence proof uses compiles for a type that has a codec.
   *
   * Every proof below asserts that a snippet does '''not''' compile, and a snippet can fail to
   * compile for the wrong reason - a misspelled package, a malformed summon - in which case the
   * proof passes while proving nothing. Two controls guard against that, and both are needed:
   * this one shows that the form of the summon is right, by summoning the same thing for a type
   * that has one, and each proof additionally names its own type in a type declaration that
   * '''does''' compile, so a misspelled type name fails the test rather than passing it.
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
   * These are compile-time proofs and could not be anything else: the claim is not that encoding
   * such a type fails at run time but that there is nothing to call, and the only way to state
   * that is to show that the call does not compile. Together with the round trips above they
   * close the inventory in both directions - every covered type has a codec that works, and no
   * excluded type has a codec at all.
   *
   * The proofs here are confined to the absence of a codec. The other compile-time facts about
   * these types - that a validated type has no public constructor, that a sealed family cannot be
   * extended from outside its file, that the unsafe array operations are not visible - belong to
   * the API surface suite and are not repeated.
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
      // the one provider that is data rather than behaviour is a rate, and it is covered
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

    // The four aliases are handled differently, and deliberately so. An alias has no companion,
    // so nothing of this port can own a codec for one: whether the JSON library happens to
    // publish an instance for the underlying sum or accumulating type is that library's business
    // and depends on choices - how it keys a left-hand value, for one - that have nothing to do
    // with this port's contract. Asserting either outcome would make this suite brittle about
    // something it does not own. What is asserted instead is the substantive pair of facts: the
    // name is an alias rather than a type with a companion to hold an instance, and the failure
    // type these containers carry is itself covered.

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
   * The inventory names a serializable ''family'' where a family exists, and the calendars are the
   * case where the difference is visible: every calendar built from holiday dates is serializable
   * and is covered by the `HolidayCalendar` row, but the narrowing of that family's codec to the
   * one member - which the calendars' own tests use to pin the member's document form against a
   * literal - is visible only inside the package that declares it. So the surface a consumer of
   * this module sees carries the fifty-eight codecs of the inventory and not a fifty-ninth for a
   * member of one of them, and that is a compile-time fact rather than a convention.
   *
   * This is deliberately '''not''' an exclusion. The type is not listed in [[excludedTypes]] and
   * must not be: a row there would state that the type has no codec, which is false and would
   * contradict the round trip the family already performs over exactly these values, and it would
   * change counts that are compared against the closed inventory of the migration plan.
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
   * document forms, the refusals, the consolidated Java test, the proofs of absence, and the
   * proof that the codec surface is the inventory and nothing besides.
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
    registerJodaBeansConsolidation()
    registerExclusionProofs()
    registerUnpublishedCodecProofs()
  }

  //-------------------------------------------------------------------------
  // The registration itself, which is the last thing this constructor does.
  //
  // Every value and every case above is defined by now, and none of them has been computed: the
  // fixtures, the two inventory lists and the five instances of each case are all read on first
  // use, so this switch is reached having built nothing. That ordering is what the audit needs -
  // a value built before the switch is built in both runs and cancels out of the difference
  // between them - and it is asserted by the count of codec acquisitions the audit run prints.
  //
  // An ordinary run registers the whole suite. An audit run registers exactly one test and
  // nothing else, which is the point of switching here rather than inside a test: were the round
  // trips still to run in the baseline mode they would load every class of every codec, and the
  // difference between the two runs - the thing the audit measures - would be empty.
  sys.props.get(AuditModeProperty) match {
    case None => registerFullSuite()
    case Some(BaselineMode) => registerAuditMode(BaselineMode)
    case Some(CodecMode) => registerAuditMode(CodecMode)
    case Some(other) => registerUnknownAuditMode(other)
  }
}
