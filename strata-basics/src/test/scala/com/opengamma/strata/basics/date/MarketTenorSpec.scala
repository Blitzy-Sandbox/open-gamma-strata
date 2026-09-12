/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import scala.util.Random

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.date.Tenor.TENOR_1D
import com.opengamma.strata.basics.date.Tenor.TENOR_1W
import com.opengamma.strata.basics.date.Tenor.TENOR_2M
import com.opengamma.strata.basics.date.Tenor.TENOR_3Y
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests for the market tenor class.
 *
 * The Java original held seventeen test-shaped methods and sixteen annotations: `test_on` carries
 * a full body of assertions and no `@Test`, so the build that ran these tests never executed it.
 * All seventeen survive here under their Java names - `test_on` included, for the reasons given
 * where it is asserted - so the method-level traceability of the migration stays one to one. The
 * two parameterised methods each become a single test holding the provider they were driven by.
 *
 * ===What the port changes, and why===
 *
 * Four shapes of assertion could not be carried over literally, and each is reshaped rather than
 * dropped, so no fact the Java test established is lost.
 *
 *   - '''A rejected input is a value, not an exception.''' Every factory of [[MarketTenor]]
 *     reports a rejected input as a failure rather than by throwing, so the five exception
 *     assertions of the original become assertions that the outcome is a failure carrying an
 *     expected reason. The reason is compared as a value of the closed family of reasons rather
 *     than by matching message text, which is what keeps such an assertion meaningful when a
 *     message is reworded. No assertion in this file expects an exception.
 *   - '''Sorting is deterministic.''' The original shuffled its list with an unseeded shuffle.
 *     `test_compare` uses two fixed permutations instead - a shuffle with a fixed seed and the
 *     reverse of the list - so a failure here is reproducible rather than intermittent.
 *   - '''Java serialization is not supported.''' The JSON codec is the serialized form of this
 *     port, and it writes the bare code, so `test_serialization` round-trips through it.
 *   - '''Annotation-driven string conversion is not supported.''' Its two halves are the
 *     rendering instance and the parsing factory, so `test_jodaConvert` asserts the round trip
 *     through them.
 *
 * ===Duties that belong elsewhere===
 *
 * `ApiSurfaceSpec` proves that no public `apply` or `copy` exists on this type, `TypeclassLawsSpec`
 * runs the law suites of its `Order` and `Hash` instances, `SmartConstructorSpec` sweeps the
 * invalid-input classes of every validated type, `JsonRoundTripSpec` owns the property-based codec
 * round trips, `TenorSpec` covers [[Tenor]] and `DaysAdjustmentSpec` covers [[DaysAdjustment]].
 * What is asserted here is the behaviour of [[MarketTenor]] itself.
 */
class MarketTenorSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The market conventional spot lag of two business days, as the Java test declared it.
   *
   * Two business days against a single calendar is the spot lag of most currencies, and it is the
   * input `adjustSpotLag` is expected to return unchanged for every market tenor that starts at
   * spot. Its [[DaysAdjustment.resultCalendar]] is `GBLO`: the trailing business day adjustment
   * names no calendar, so the calendar the addition walked had the last word.
   */
  private val SPOT_LAG_2: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO)

  /** The spot lag of one business day, which is also what `TN` overrides a lag to. */
  private val SPOT_LAG_1: DaysAdjustment = DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.GBLO)

  /**
   * The spot lag of no business days, which is also what `ON` overrides a lag to.
   *
   * Adding zero business days names no day, so this factory reads the request as "the next
   * business day of this calendar, or this date if it already is one" and holds
   * `(0, NoHolidays, Following using GBLO)` rather than `(0, GBLO, no adjustment)`. That is the
   * zero-day case of [[DaysAdjustment.ofBusinessDays]] rather than anything this test arranges,
   * and it is what `SPOT_LAG_0` means here: its [[DaysAdjustment.resultCalendar]] is still `GBLO`,
   * which is why `ON.adjustSpotLag(SPOT_LAG_0)` reproduces it exactly.
   */
  private val SPOT_LAG_0: DaysAdjustment = DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO)

  /**
   * A value of an unrelated type, for the equality contract of `test_equals_bad`.
   *
   * This is the `ANOTHER_TYPE` constant the tests of this migration use, and it is typed as `Any`
   * for the same reason the Java test needed no such care: comparing a market tenor with a string
   * through `==` is a compile-time warning in this build, and a warning is an error here. Typing
   * the value as `Any` and calling `equals` asserts what the original asserted - that the equality
   * of a market tenor rejects a value of another type - without asking the compiler to approve the
   * comparison.
   */
  private val ANOTHER_TYPE: Any = ""

  /** The `"BAD"` string of the Java test, held in an `Any` for the reason above. */
  private val BAD_TEXT: Any = "BAD"

  /**
   * The code of a market tenor as bare text, for the equality contract of `test_equals_bad`.
   *
   * The code '''is''' the identity of a market tenor, which makes this the interesting negative
   * case: a market tenor must not be equal to the string it renders itself as, however faithfully
   * that string names it.
   */
  private val CODE_AS_TEXT: Any = "ON"

  //-------------------------------------------------------------------------
  /**
   * Returns the market tenor a factory produced, failing the test if it failed instead.
   *
   * Every factory of [[MarketTenor]] reports a rejected input as a failure rather than by
   * throwing, so a test that means to use the resulting market tenor has to say what should happen
   * if there is none. Every call below passes an input the factory is expected to accept, and the
   * answer is therefore that the test fails, naming the message that came back - which is strictly
   * more informative than the Java original, where an unexpected rejection surfaced as an
   * `IllegalArgumentException` from the line that made the call.
   *
   * The parameter is written as the underlying `Either` rather than through the alias the library
   * publishes for it, because this spec depends on the failure type alone.
   *
   * @param result  the outcome of a factory call that is expected to succeed
   * @return the market tenor the factory produced
   */
  private def accepted(result: Either[Failure, MarketTenor]): MarketTenor =
    result.fold(
      failure => fail(s"Expected a market tenor but the factory failed with: ${failure.message}"),
      marketTenor => marketTenor)

  //-------------------------------------------------------------------------
  /**
   * The `data_parseGood` provider, transcribed row for row.
   *
   * Each row is the text handed to the parse and the market tenor it names. The four market codes
   * are accepted as they stand, and the last two rows are the same market tenor written both ways
   * the tenor parse accepts - `2M`, which is the canonical name a tenor renders itself as, and
   * `P2M`, which is the ISO-8601 form of the same period.
   */
  private val data_parseGood: TableFor2[String, MarketTenor] = Table(
    ("input", "expected"),
    ("ON", MarketTenor.ON),
    ("TN", MarketTenor.TN),
    ("SN", MarketTenor.SN),
    ("SW", MarketTenor.SW),
    ("2M", accepted(MarketTenor.ofSpot(TENOR_2M))),
    ("P2M", accepted(MarketTenor.ofSpot(TENOR_2M)))
  )

  /**
   * The `data_parseBad` provider, transcribed row for row and extended with the reason.
   *
   * The Java provider held the rejected text alone, because every row raised the same exception
   * type. Parsing here reports a reason, and the rows do not all report the same one, so the
   * expected reason is part of the table:
   *
   *   - the empty string is rejected by the argument check the parse performs first, so the caller
   *     that supplied nothing is told that rather than being told that nothing is not a period;
   *   - a bare number, a number with an unknown unit and text that only looks like a period -
   *     `PON` being `ON` with the ISO-8601 prefix - name no period at all, which is a parsing
   *     failure;
   *   - `-2D` does name a period, and a valid one as far as `java.time` is concerned. It is
   *     rejected one step later, for being negative, and so reports the reason its rejection as a
   *     tenor carries rather than a parsing reason. Telling a caller that its period is negative
   *     rather than that its text was bad is the point of the distinction.
   */
  private val data_parseBad: TableFor2[String, FailureReason] = Table(
    ("input", "reason"),
    ("", FailureReason.INVALID),
    ("2", FailureReason.PARSING),
    ("2K", FailureReason.PARSING),
    ("-2D", FailureReason.INVALID),
    ("PON", FailureReason.PARSING)
  )

  //-------------------------------------------------------------------------
  test("test_on") {
    // THE JAVA METHOD THIS PORTS WAS NEVER RUN. `MarketTenorTest.test_on` declares a full body of
    // assertions and carries no `@Test` annotation, so JUnit never discovered it and the Maven
    // build reported it neither as a pass nor as a failure - it is the only test-shaped method in
    // the Java date package without an annotation. The body is ported here as an ordinary test and
    // does run, which closes a real coverage hole: `ON` is the one market tenor whose override
    // sets the lag to zero, and that behaviour was previously asserted nowhere.
    val test = MarketTenor.ON
    test.code shouldBe "ON"
    test.tenor shouldBe TENOR_1D
    test.isNonStandardSpotLag shouldBe true
    // the indicator is the number of business days between the trade date and the start date,
    // which is zero for overnight: today to tomorrow needs no wait for spot
    test.spotLagIndicator shouldBe 0
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_0
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_0
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0
  }

  test("test_tn") {
    val test = MarketTenor.TN
    test.code shouldBe "TN"
    test.tenor shouldBe TENOR_1D
    test.isNonStandardSpotLag shouldBe true
    // one business day between the trade date and the start date: tomorrow to the next day
    test.spotLagIndicator shouldBe 1
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_1

    // Every fixture of the Java test named `GBLO`, so none of its rows could show which calendar
    // the overriding lag is expressed against. It is the result calendar of the lag supplied - the
    // calendar the market convention would have landed on - and the day count of that lag is
    // discarded, which is the whole of what `adjustSpotLag` does for a non-standard lag.
    test.adjustSpotLag(DaysAdjustment.ofBusinessDays(5, HolidayCalendarIds.USNY)) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.USNY)
  }

  test("test_sn") {
    val test = MarketTenor.SN
    test.code shouldBe "SN"
    test.tenor shouldBe TENOR_1D
    test.isNonStandardSpotLag shouldBe false
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_2
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0

    // a market tenor that starts at spot leaves the market conventional lag alone, and "alone"
    // here means the very value it was given rather than one equal to it
    (test.adjustSpotLag(SPOT_LAG_2) eq SPOT_LAG_2) shouldBe true
  }

  test("test_sw") {
    val test = MarketTenor.SW
    test.code shouldBe "SW"
    test.tenor shouldBe TENOR_1W
    test.isNonStandardSpotLag shouldBe false
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_2
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0
  }

  //-------------------------------------------------------------------------
  test("test_ofSpot") {
    val test = accepted(MarketTenor.ofSpot(TENOR_3Y))
    test.code shouldBe "3Y"
    test.tenor shouldBe TENOR_3Y
    test.isNonStandardSpotLag shouldBe false
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_2
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0
  }

  test("test_ofSpot_special") {
    // a spot-starting day and a spot-starting week are what the market writes `SN` and `SW`, so
    // those two tenors give the constants rather than market tenors coded `1D` and `1W`
    MarketTenor.ofSpot(TENOR_1W) should haveValue(MarketTenor.SW)
    MarketTenor.ofSpot(TENOR_1D) should haveValue(MarketTenor.SN)
  }

  //-------------------------------------------------------------------------
  test("test_ofSpotDays") {
    accepted(MarketTenor.ofSpotDays(1)).code shouldBe "SN"
    accepted(MarketTenor.ofSpotDays(7)).code shouldBe "SW"
    accepted(MarketTenor.ofSpotDays(3)).code shouldBe "3D"
    accepted(MarketTenor.ofSpotDays(20)).code shouldBe "20D"
    // the exception assertion of the original: a count that is not a tenor is reported as a
    // failure, and the reason says the period was invalid rather than that the text was bad
    MarketTenor.ofSpotDays(-1) should beFailureWith(FailureReason.INVALID)
  }

  test("test_ofSpotMonths") {
    accepted(MarketTenor.ofSpotMonths(3)).code shouldBe "3M"
    accepted(MarketTenor.ofSpotMonths(20)).code shouldBe "20M"
    // The Java body asserts the rejection of `ofSpotDays(-1)` here, which is a copy-paste slip
    // from the method above: the negative month count this method exists to cover was never
    // asserted. Both are asserted here - the one the method means, and the one the original
    // actually ran - so the port neither carries the slip forward nor loses a line of coverage.
    MarketTenor.ofSpotMonths(-1) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotDays(-1) should beFailureWith(FailureReason.INVALID)
  }

  test("test_ofSpotYears") {
    accepted(MarketTenor.ofSpotYears(3)).code shouldBe "3Y"
    accepted(MarketTenor.ofSpotYears(20)).code shouldBe "20Y"
    MarketTenor.ofSpotYears(-1) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_roundTrip") {
    // the text a market tenor renders itself as parses back to that same market tenor, asserted
    // over the five values of the original: the four market codes and one normal tenor
    val representatives: List[MarketTenor] = List(
      MarketTenor.ON,
      MarketTenor.TN,
      MarketTenor.SN,
      MarketTenor.SW,
      accepted(MarketTenor.ofSpot(TENOR_3Y))
    )
    val failed = representatives.filterNot(tenor => MarketTenor.parse(tenor.toString) == Right(tenor))
    failed shouldBe empty
  }

  test("test_parse_String_good_noP") {
    forAll(data_parseGood) { (input: String, expected: MarketTenor) =>
      MarketTenor.parse(input) should haveValue(expected)
    }
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, reason: FailureReason) =>
      // the exception assertion of the original, as a failure whose reason is compared as a value
      MarketTenor.parse(input) should beFailureWith(reason)
    }
  }

  //-------------------------------------------------------------------------
  test("test_compare") {
    // The list of the Java test, in its order. The original built the four spot-starting rows as
    // `ofSpot(Tenor.ofDays(n))` and `ofSpot(Tenor.ofMonths(1))`; the counted factories used here
    // are that same path - each hands its count to the tenor factory and then to `ofSpot` - and
    // they thread one error channel rather than two, which is why they are preferred here.
    val marketTenors: List[MarketTenor] = List(
      MarketTenor.ON,
      MarketTenor.TN,
      MarketTenor.SN,
      accepted(MarketTenor.ofSpotDays(2)),
      accepted(MarketTenor.ofSpotDays(6)),
      MarketTenor.SW,
      accepted(MarketTenor.ofSpotDays(8)),
      accepted(MarketTenor.ofSpotMonths(1))
    )
    marketTenors should have size 8
    marketTenors.map(_.code) shouldBe List("ON", "TN", "SN", "2D", "6D", "SW", "8D", "1M")

    // The expected order is the Java order, unaltered. The ordering of this port is the comparison
    // of the original followed by a tie-break on the code, and the tie-break is consulted only
    // where that comparison returns zero - which it does for no pair in this list: `ON` and `TN`
    // override the conventional lag and so sort by lag, ahead of everything that starts at spot,
    // and the spot-starting values sort by tenor, each of these being of a distinct length. So the
    // divergence recorded in `SCALA_MIGRATION.md` moves nothing here, and the pair it does move is
    // asserted below. The adjacency check states the expectation directly, independently of any
    // sort: each market tenor ranks strictly before the one after it.
    val ordering = Order[MarketTenor].toOrdering
    val outOfOrder = marketTenors.zip(marketTenors.tail).filterNot {
      case (earlier, later) => Order[MarketTenor].compare(earlier, later) < 0
    }
    outOfOrder shouldBe empty

    // The original shuffled with an unseeded shuffle, which makes a failure irreproducible. Two
    // fixed permutations are used instead: a shuffle with a fixed seed, and the reverse of the
    // list, which is the worst case for any comparison sort.
    val shuffled = new Random(20200101L).shuffle(marketTenors)
    shuffled shouldNot be(marketTenors)
    shuffled.sorted(ordering) shouldBe marketTenors
    marketTenors.reverse.sorted(ordering) shouldBe marketTenors

    // DIVERGENCE: twelve months and one year, both starting at spot. The original ranks them equal
    // while holding them unequal, which no `cats.Order` may do - its laws require `compare` to
    // return zero exactly when the values are equal - so the ordering of this port breaks that tie
    // by code, placing `12M` immediately before `1Y`. The unrefined comparison of the original
    // remains available as `compareTo` and still ranks them equal, as asserted here. The tie-break
    // cannot reorder a pair the original ranked strictly, because it is consulted only after that
    // comparison has returned zero. Recorded in `SCALA_MIGRATION.md`.
    val twelveMonths = accepted(MarketTenor.ofSpotMonths(12))
    val oneYear = accepted(MarketTenor.ofSpotYears(1))
    twelveMonths.code shouldBe "12M"
    oneYear.code shouldBe "1Y"
    twelveMonths.compareTo(oneYear) shouldBe 0
    Order[MarketTenor].compare(twelveMonths, oneYear) should be < 0
    Order[MarketTenor].compare(oneYear, twelveMonths) should be > 0
    Order[MarketTenor].eqv(twelveMonths, oneYear) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = MarketTenor.SN
    val a2 = accepted(MarketTenor.ofSpot(TENOR_1D))
    val b = MarketTenor.ON
    a1.equals(a1) shouldBe true
    a1.equals(b) shouldBe false
    a1.equals(a2) shouldBe true

    a2.equals(a1) shouldBe true
    a2.equals(a2) shouldBe true
    a2.equals(b) shouldBe false

    b.equals(a1) shouldBe false
    b.equals(a2) shouldBe false
    b.equals(b) shouldBe true

    a1.hashCode shouldBe a2.hashCode

    // the same statements through the instance that carries equality for this port, which is where
    // the rest of the library reads it from
    Order[MarketTenor].eqv(a1, a2) shouldBe true
    Order[MarketTenor].eqv(a1, b) shouldBe false
    Hash[MarketTenor].hash(a1) shouldBe Hash[MarketTenor].hash(a2)

    // The code is the whole of the identity of a market tenor: equality and hashing consider
    // nothing else, so two market tenors with the same code are equal however they were built.
    // Each of these three routes runs different code and none of them returns a shared constant,
    // which is what makes the statement worth asserting rather than assuming - the tenor and the
    // spot lag indicator they carry agree because the code determines both.
    val fromTenor = accepted(MarketTenor.ofSpot(TENOR_3Y))
    val fromText = accepted(MarketTenor.parse("3Y"))
    val fromCount = accepted(MarketTenor.ofSpotYears(3))
    List(fromTenor, fromText, fromCount).map(_.code) shouldBe List("3Y", "3Y", "3Y")
    fromTenor.equals(fromText) shouldBe true
    fromTenor.equals(fromCount) shouldBe true
    fromText.equals(fromCount) shouldBe true
    fromTenor.hashCode shouldBe fromText.hashCode
    fromTenor.hashCode shouldBe fromCount.hashCode
    Order[MarketTenor].eqv(fromTenor, fromText) shouldBe true
    Hash[MarketTenor].hash(fromTenor) shouldBe Hash[MarketTenor].hash(fromCount)
    // and the elements the code determines agree across the routes
    fromText.tenor shouldBe TENOR_3Y
    fromText.isNonStandardSpotLag shouldBe false
    fromCount.tenor shouldBe TENOR_3Y

    // a market tenor is not equal to another one whose code differs, even where the two describe
    // the same number of days: `SN` and `ON` both carry a tenor of one day and differ in the lag
    a1.tenor shouldBe b.tenor
    a1.equals(b) shouldBe false
  }

  test("test_equals_bad") {
    // `equals` is called as a method, on values typed as `Any`, because this build rejects a `==`
    // between unrelated types as an error; the assertions are those of the original
    MarketTenor.ON.equals(MarketTenor.ON) shouldBe true
    MarketTenor.ON.equals(MarketTenor.TN) shouldBe false
    MarketTenor.ON.equals(MarketTenor.SN) shouldBe false
    MarketTenor.ON.equals(MarketTenor.SW) shouldBe false
    MarketTenor.ON.equals(BAD_TEXT) shouldBe false
    MarketTenor.ON.equals(ANOTHER_TYPE) shouldBe false
    MarketTenor.ON.equals(null) shouldBe false
    // the code is the identity of a market tenor, and it is still not equal to that code as text
    MarketTenor.ON.equals(CODE_AS_TEXT) shouldBe false
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not supported by this port; the JSON codec is the serialized form, and
    // it writes the bare code, so the two constants the original round-tripped are round-tripped
    // through it instead, together with a normal tenor
    val threeYears = accepted(MarketTenor.ofSpot(TENOR_3Y))
    MarketTenor.ON.asJson shouldBe Json.fromString("ON")
    MarketTenor.SN.asJson shouldBe Json.fromString("SN")
    threeYears.asJson shouldBe Json.fromString("3Y")

    // the encoded form is a string rather than an object, which is what makes a document holding a
    // market tenor readable by anything that knows the market codes
    MarketTenor.ON.asJson.isString shouldBe true
    MarketTenor.ON.asJson.isObject shouldBe false

    Json.fromString("ON").as[MarketTenor] shouldBe Right(MarketTenor.ON)
    Json.fromString("SN").as[MarketTenor] shouldBe Right(MarketTenor.SN)
    Json.fromString("3Y").as[MarketTenor] shouldBe Right(threeYears)

    // reading goes through the same parsing the text form uses, so a document holding either form
    // the parse accepts is decoded - `1W` naming the value this port writes as `SW`
    Json.fromString("P3Y").as[MarketTenor] shouldBe Right(threeYears)
    Json.fromString("1W").as[MarketTenor] shouldBe Right(MarketTenor.SW)

    // and text that names no market tenor is rejected rather than decoded into one
    Json.fromString("2K").as[MarketTenor].isLeft shouldBe true
    Json.fromString("").as[MarketTenor].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // annotation-driven string conversion is not supported by this port either. Its two halves are
    // the rendering instance and the parsing factory, so the round trip the original asserted
    // through that mechanism is asserted through them.
    val threeYears = accepted(MarketTenor.ofSpot(TENOR_3Y))
    Show[MarketTenor].show(MarketTenor.ON) shouldBe "ON"
    Show[MarketTenor].show(MarketTenor.SW) shouldBe "SW"
    Show[MarketTenor].show(threeYears) shouldBe "3Y"
    // the rendering is the code itself, which is also what `toString` produces, so a message, a
    // log and a JSON document all carry one string
    Show[MarketTenor].show(MarketTenor.ON) shouldBe MarketTenor.ON.code
    Show[MarketTenor].show(threeYears) shouldBe threeYears.toString

    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.ON)) should haveValue(MarketTenor.ON)
    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.TN)) should haveValue(MarketTenor.TN)
    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.SN)) should haveValue(MarketTenor.SN)
    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.SW)) should haveValue(MarketTenor.SW)
    MarketTenor.parse(Show[MarketTenor].show(threeYears)) should haveValue(threeYears)
  }

  //-------------------------------------------------------------------------
  // Mapping from the Java test class, for the record: all seventeen test-shaped methods are
  // represented above under their Java names, and none is dropped, so the method-level
  // traceability of the migration stays one to one. Sixteen of them were annotated and ran;
  // `test_on` was not annotated and did not run, and it runs here - an added testcase, which the
  // gate that joins a mapping row to a testcase accepts, since it joins in that direction only.
  //
  // The two parameterised methods, `test_parse_String_good_noP` and `test_parse_String_bad`, each
  // hold the provider they were driven by - `data_parseGood` and `data_parseBad` - as a table, so
  // the rows are asserted exactly as the annotations asserted them.
}
