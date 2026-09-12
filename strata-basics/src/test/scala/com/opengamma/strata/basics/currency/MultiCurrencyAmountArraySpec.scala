/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

import scala.util.matching.Regex

import cats.Eq
import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.CHF
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.Outcome
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[MultiCurrencyAmountArray]], ported from the Java `MultiCurrencyAmountArrayTest`.
 *
 * The original holds twenty-two test methods and this suite holds every one of them, each under
 * the name the original gave it - including the five it wrote without the `test_` prefix,
 * `serializeSize`, `coverage`, `collector`, `total` and `collectorDifferentArrayLengths` - so
 * that a Java test method and a test of this suite stay in one-to-one correspondence in the
 * migration manifest. Ten tests of this port's own follow them, under names of their own because
 * the original has no counterpart for any of them, and the section below says what they are for.
 * One of the ten pins the negative size the function factory refuses, which is a contract that
 * class declared and its hand-written constructor did not enforce.
 *
 * ===What this suite exists to pin===
 *
 * The type holds one array of values per currency plus a size of its own, rather than a
 * collection of amounts, and there are exactly two places where that layout is observable. Both
 * are asserted here, and a port that derived the size from the values it holds would pass every
 * other test of this file:
 *
 *   - '''zero padding''': a currency the run holds is held for the whole run, so an amount that
 *     names no value for it at some index contributes `0.0` there rather than a gap. `test_of`
 *     builds a run from amounts whose currency sets differ and reads every padded zero back, and
 *     `test_get` reassembles an amount that names a padded currency with a zero amount.
 *   - '''the stored size''': a run of amounts that name no currency at all has an empty map of
 *     values, so its size cannot be recovered from them. `test_empty_amounts`,
 *     `test_of_function_empty_amounts` and `serializeSize` each build such a run and assert the
 *     size that only a stored field can answer with.
 *
 * ===Three tests keep their name and change what they assert===
 *
 * Each says so at the test itself:
 *
 *   - `serializeSize` round-tripped through Java serialization, which no type of this port
 *     supports. It round-trips through the codec that replaced it, which is the substitution that
 *     keeps the point of the test - a size that survives a round trip only because it is
 *     serialized in its own right.
 *   - `coverage` drove the reflective bean sweep of the library being ported, which no type of
 *     this port has. It asserts the equality, hashing and rendering instances that replaced the
 *     sweep, over the same two values the original built.
 *   - `collector` aggregated through the `Collector` the ported class published. This port has no
 *     collector; `MultiCurrencyAmountArray.total` is the member that took over that role, and the
 *     test asserts the same aggregate over the same input.
 *
 * ===Nine further tests follow the twenty-two===
 *
 * Each carries a name of its own, so the correspondence between a Java test method and a test of
 * this suite stays one-to-one and the migration manifest keeps joining them by name. They pin the
 * boundary between the amounts a caller holds and the primitive arrays the run keeps their values
 * in, which the ported twenty-two exercise only for short runs of whole numbers: what
 * reconstructing an index and transposing amounts produce and raise, how many rates a conversion
 * asks for and in what order it accumulates them, and how many of its inputs the aggregation
 * reads and in what order it adds the runs of one currency.
 *
 * ===Failure is a value, so the fixtures are unwrapped===
 *
 * The original asserted an exception at seven places - five `assertThatIllegalArgumentException`
 * and two `assertThatExceptionOfType` - and each of the five is a `Left` here: a currency the run
 * does not hold, arrays that disagree about their length, and two runs of different sizes are all
 * properties of the values a caller holds, so each is reported rather than thrown. The reason is
 * compared as a value of the closed family of reasons and the wording is pinned through
 * `Regex.quote`, so what is asserted is the literal message rather than a pattern that happens to
 * match it. A rate a provider cannot supply is reported with the currency-conversion reason
 * instead, which is what tells a caller a conversion it could fix by supplying a rate from one it
 * could not; the original had no such case, because the matrix it built its providers from threw
 * for a missing rate.
 *
 * The two remaining assertions, `assertThatExceptionOfType(IndexOutOfBoundsException)` over
 * `get(3)` and `get(-1)`, are not `Left` here: reading outside a run is a caller-contract
 * invariant of the underlying array, which this port keeps as the index exception of the runtime
 * exactly as the implementation being ported had it, so `get` stays total in signature. They are
 * asserted all the same, as intercepted throws in `test_get`, rather than delegated to the
 * module-wide invariant sweep of `FailableSurfaceSpec` - the assertions a mapped Java method made
 * belong to the suite that method maps to, which is this one, so all seven of the original's
 * exception assertions are accounted for here: five as a reported failure, two as a throw.
 *
 * Fixtures are built through [[unwrap]], the single unwrapping helper of the suite, so a fixture
 * that fails to build is reported as a failed test naming the reason rather than raising from
 * somewhere else. [[MultiCurrencyAmountArray]] is a validated type - it has no public `apply` and
 * no `copy` - so every run here is built through `of` or `total`.
 *
 * ===What is asserted elsewhere===
 *
 * The compile-time proofs that this type has no public `apply`, no `copy` and no reachable escape
 * hatch into a backing array belong to `ApiSurfaceSpec`; the sweep over every validated factory of
 * the module to `SmartConstructorSpec`; the sweep over every failable method and every documented
 * invariant to `FailableSurfaceSpec` - a module-wide sweep, which is why the index invariant of
 * `get` is asserted here as well, at the Java method it maps to, and not left to it; the typeclass
 * laws to `TypeclassLawsSpec`; the property-based round trip of every codec to
 * `json.JsonRoundTripSpec` - `serializeSize` here is one targeted example of size preservation,
 * not a sweep; the numerical parity of currency arithmetic against the Java baseline to
 * `parity.CurrencyMathParitySpec`. This suite asserts the cases of the Java test it is ported
 * from.
 *
 * @see [[MultiCurrencyAmountArray]] for the type under test
 * @see [[MultiCurrencyAmount]] for a single multi-currency amount, which `get` produces
 * @see [[CurrencyAmountArray]] for a run in one currency, which `convertedTo` produces
 */
final class MultiCurrencyAmountArraySpec extends AnyFunSuite with Matchers {

  /**
   * The fixture of the original: three amounts, each naming the same three currencies.
   *
   * Held in the order the original wrote them, so the values of a currency read back in index
   * order are the values of that currency as they are written here.
   */
  private val VALUES_ARRAY: MultiCurrencyAmountArray =
    MultiCurrencyAmountArray.of(
      List(
        multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d)),
        multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
        multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))))

  /**
   * The runs of single-currency amounts the aggregation tests total, as the original wrote them.
   *
   * Two of the five name USD and two name GBP, so the aggregation has both a currency to carry
   * through and a currency to add up. Every value is a whole number, which is what makes the
   * total independent of the order the input is traversed in and lets the aggregation be asserted
   * for equality rather than within a tolerance.
   */
  private val CollectorArrays: List[CurrencyAmountArray] =
    List(
      CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
      CurrencyAmountArray.of(USD, DoubleArray.of(5d, 6d, 7d)),
      CurrencyAmountArray.of(EUR, DoubleArray.of(2d, 4d, 6d)),
      CurrencyAmountArray.of(GBP, DoubleArray.of(11d, 12d, 13d)),
      CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 2d, 3d)))

  /** The aggregate of [[CollectorArrays]], as the original wrote it. */
  private val CollectorExpected: MultiCurrencyAmountArray =
    arrayOf(
      USD -> DoubleArray.of(15d, 26d, 37d),
      EUR -> DoubleArray.of(2d, 4d, 6d),
      GBP -> DoubleArray.of(12d, 14d, 16d))

  /**
   * The tolerance of the conversion into a currency the run already holds.
   *
   * It is the offset the original passed to its per-element assertion, kept because the
   * expectation of that test is written as a sum of quotients rather than as the products the
   * conversion computes.
   */
  private val Tolerance: Double = 1e-6d

  /** The wording reported when two runs that have to be combined differ in size. */
  private val SizeMismatchMessage: String = "Sizes must be equal, this size is 2, other size is 3"

  /** The prefix the original asserted for arrays that disagree about their length. */
  private val ArrayLengthPrefix: String = "Arrays must have the same size"

  /**
   * The wording raised when the function form is handed a size no run can have.
   *
   * It is the wording of the argument check the factory performs, which names the argument and
   * the value it was given, so the whole message is pinned here rather than a fragment of it.
   */
  private val NegativeSizeMessage: String =
    "Argument 'size' must not be negative but has value -1"

  /**
   * A value of a type unrelated to a run, for the equality assertions that need one.
   *
   * Held at the type `Any` so that the comparison reads as one against a foreign value rather
   * than as one the compiler could reject outright.
   */
  private val ForeignValue: Any = ""

  //-------------------------------------------------------------------------
  /**
   * Asserts the factories that read amounts, and with them the zero padding of the type.
   *
   * The first half is the original's: the values of each currency of the fixture, read back one
   * currency at a time. The second half is where the padding is pinned - a run built from amounts
   * whose currency sets differ holds every currency any amount names, with `0.0` at the indices
   * where an amount does not name it, so the ragged input produces three full-length arrays and
   * not a gap anywhere. Reading index zero back names all three currencies, two of them with a
   * zero amount, which is the visible consequence of that.
   *
   * A currency no amount named is not held at all, and reading its values is reported rather than
   * answered with zeroes - the distinction the padding makes necessary, since a currency that is
   * held and happens to be zero everywhere would otherwise be indistinguishable from one that was
   * never involved.
   */
  test("test_of") {
    VALUES_ARRAY.getValues(GBP) should haveValue(DoubleArray.of(20d, 21d, 22d))
    VALUES_ARRAY.getValues(USD) should haveValue(DoubleArray.of(30d, 32d, 33d))
    VALUES_ARRAY.getValues(EUR) should haveValue(DoubleArray.of(40d, 43d, 44d))
    VALUES_ARRAY.size shouldBe 3
    VALUES_ARRAY.getCurrencies shouldBe Set(GBP, USD, EUR)

    val raggedArray: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        List(
          multiOf(amountOf(EUR, 4d)),
          multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
          multiOf(amountOf(EUR, 44d))))

    raggedArray.size shouldBe 3
    raggedArray.getCurrencies shouldBe Set(GBP, USD, EUR)
    raggedArray.getValues(GBP) should haveValue(DoubleArray.of(0d, 21d, 0d))
    raggedArray.getValues(USD) should haveValue(DoubleArray.of(0d, 32d, 0d))
    raggedArray.getValues(EUR) should haveValue(DoubleArray.of(4d, 43d, 44d))

    // the padding read the other way round: an index names every currency of the run
    raggedArray.get(0) shouldBe multiOf(amountOf(EUR, 4d), amountOf(GBP, 0d), amountOf(USD, 0d))

    val unknown: FailureOr[DoubleArray] = raggedArray.getValues(AUD)
    unknown should beFailureWith(FailureReason.INVALID)
    unknown should haveFailureMessageMatching(Regex.quote("No values available for AUD"))

    // the varargs form of the factory reads the same amounts as the collection form
    MultiCurrencyAmountArray.of(
      multiOf(amountOf(EUR, 4d)),
      multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
      multiOf(amountOf(EUR, 44d))) shouldBe raggedArray
  }

  /**
   * Asserts the run of amounts that name no currency at all.
   *
   * This is the first of the two places the stored size is observable: two empty amounts describe
   * a run of size two that holds no values, so nothing about the values could answer how long it
   * is. Each index reads back as the empty amount, which is what a run holding no currency has to
   * answer with.
   */
  test("test_empty_amounts") {
    val array: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(MultiCurrencyAmount.empty, MultiCurrencyAmount.empty)
    array.size shouldBe 2
    array.get(0) shouldBe MultiCurrencyAmount.empty
    array.get(1) shouldBe MultiCurrencyAmount.empty
    array.values shouldBe Map.empty[Currency, DoubleArray]
    array.getCurrencies shouldBe Set.empty[Currency]
  }

  /**
   * Asserts the function form, including the one-evaluation-per-index guarantee it documents.
   *
   * The three expectations are the original's, and they are written as they were there - each
   * amount with the currencies it does not name added at zero - which is the padding of this type
   * stated as an equality between what was put in and what comes out.
   *
   * The count of evaluations is asserted because the values are held per currency: a factory that
   * produced each currency's array by calling the function again would evaluate it once per
   * currency and index, which would be wrong for a function that counts its calls, reads a
   * sequence of inputs or is expensive. It is kept in an atomic integer rather than in a mutable
   * local, both because neither the domain nor the test code of this port holds one and because
   * the counter is written from inside a function the factory calls and read after it returns.
   */
  test("test_of_function") {
    val mca1: MultiCurrencyAmount = multiOf(amountOf(GBP, 10d), amountOf(USD, 20d))
    val mca2: MultiCurrencyAmount = multiOf(amountOf(GBP, 10d), amountOf(EUR, 30d))
    val mca3: MultiCurrencyAmount = multiOf(amountOf(USD, 40d))
    val amounts: List[MultiCurrencyAmount] = List(mca1, mca2, mca3)

    val calls: AtomicInteger = new AtomicInteger(0)
    val counted: Int => MultiCurrencyAmount = index => {
      val _ = calls.incrementAndGet()
      amounts(index)
    }
    val test: MultiCurrencyAmountArray = MultiCurrencyAmountArray.of(3, counted)

    test.size shouldBe 3
    test.getCurrencies shouldBe Set(GBP, USD, EUR)
    test.get(0) shouldBe mca1.plus(EUR, 0d)
    test.get(1) shouldBe mca2.plus(USD, 0d)
    test.get(2) shouldBe mca3.plus(GBP, 0d).plus(EUR, 0d)
    // the factory documents one evaluation per index, in index order, and nothing more
    calls.get() shouldBe 3
  }

  /**
   * Asserts the size the function form refuses, and the boundary size it admits.
   *
   * This is the one test of the suite that is not the original's, and it sits here because the
   * factory it pins is the one above. The class being ported declared its size as a property that
   * must not be negative and documented this factory as raising an argument exception for a size
   * it does not admit, but the constructor written by hand for that class skipped the validation
   * its declaration asked for: a negative size there produced a run whose declared size
   * contradicted the values it held and whose every index was unreadable. This port enforces the
   * declared contract, and nothing else in this suite or in the basics tests passes a negative
   * size anywhere - so without this test the argument check that enforces it could be deleted and
   * every test of this module would still pass.
   *
   * A negative size is refused as a caller contract rather than reported as a failure, since no
   * data a caller holds makes a run of minus one amounts meaningful. It is therefore asserted as
   * the raised argument exception and its whole message, rather than through the matchers the
   * reported failures of this suite are asserted with.
   *
   * The function is asserted never to have been evaluated, which is what distinguishes a size
   * checked before the amounts are materialised from one checked after: building the amounts of a
   * negative size yields the empty sequence rather than raising, so a factory that built them
   * first would answer a size of minus one with the run of size zero instead of refusing it. The
   * counter is the atomic integer of `test_of_function`, for the reason given there.
   *
   * Size zero is the boundary on the admitted side of the same check: it describes the run of
   * size zero holding no values, and evaluates the function for no index at all. No other test of
   * this suite reaches '''this''' factory with a size of zero - `test_of_map` and `total` reach
   * the run of size zero through the two factories that count their own input instead of being
   * told a size.
   */
  test("of(size, valueFunction) refuses a negative size and admits zero") {
    val calls: AtomicInteger = new AtomicInteger(0)
    val counted: Int => MultiCurrencyAmount = index => {
      val _ = calls.incrementAndGet()
      multiOf(amountOf(GBP, index.toDouble))
    }

    val refused: IllegalArgumentException =
      intercept[IllegalArgumentException](MultiCurrencyAmountArray.of(-1, counted))
    refused.getMessage shouldBe NegativeSizeMessage
    // the size is checked before any amount is asked for
    calls.get() shouldBe 0

    val none: MultiCurrencyAmountArray = MultiCurrencyAmountArray.of(0, counted)
    none.size shouldBe 0
    none.values shouldBe Map.empty[Currency, DoubleArray]
    none.getCurrencies shouldBe Set.empty[Currency]
    calls.get() shouldBe 0
  }

  /**
   * Asserts that a round trip preserves the size, which is why the size is a field.
   *
   * The original round-tripped through Java serialization and asserted nothing but the size, for
   * a reason its comment gave: the size had to be restored rather than recomputed. Java
   * serialization has no target in this port, so the round trip is performed through the codec
   * that replaced it - the substitution keeps exactly what the test was for, because the same
   * question is asked of a document as was asked of a byte stream.
   *
   * The second half is the point of the test and the reason it is not covered by any other test
   * of this file. A run of two amounts that name no currency encodes with an empty object of
   * values, so nothing in the document describes how long it is except the `size` field itself: a
   * codec that omitted that field, or recomputed it from the values, would read the document back
   * as a run of size zero and fail here and nowhere else.
   *
   * The shape of the document is asserted as well as the equality of what was read back, so a
   * change of shape is a failure of this test rather than something a self-round-trip would
   * conceal. The currencies appear in the order of their codes because that is the order the run
   * holds them in.
   */
  test("serializeSize") {
    val encoded: Json = VALUES_ARRAY.asJson
    encoded.noSpaces shouldBe
      """{"size":3,"values":{"EUR":[40.0,43.0,44.0],"GBP":[20.0,21.0,22.0],"USD":[30.0,32.0,33.0]}}"""
    val deserialized: MultiCurrencyAmountArray = decoded(encoded)
    deserialized.size shouldBe 3
    deserialized shouldBe VALUES_ARRAY

    val empty: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(MultiCurrencyAmount.empty, MultiCurrencyAmount.empty)
    val emptyEncoded: Json = empty.asJson
    emptyEncoded.noSpaces shouldBe """{"size":2,"values":{}}"""
    val deserializedEmpty: MultiCurrencyAmountArray = decoded(emptyEncoded)
    deserializedEmpty.size shouldBe 2
    deserializedEmpty shouldBe empty
    deserializedEmpty.values shouldBe Map.empty[Currency, DoubleArray]
  }

  /**
   * Asserts the function form where every amount names no currency.
   *
   * This is the stored size again, reached through the factory that is told the size rather than
   * counting the amounts it was handed: three empty amounts describe a run of size three holding
   * no values. The round trip is asserted here too, because this is the only factory that can
   * produce such a run of any size at all.
   */
  test("test_of_function_empty_amounts") {
    val test: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(3, (_: Int) => MultiCurrencyAmount.empty)
    test.size shouldBe 3
    test.values shouldBe Map.empty[Currency, DoubleArray]
    test.get(0) shouldBe MultiCurrencyAmount.empty
    test.get(2) shouldBe MultiCurrencyAmount.empty
    decoded(test.asJson).size shouldBe 3
  }

  /**
   * Asserts the factory that reads the representation directly, and what it rejects.
   *
   * This is the one factory with something to check - every array has to hold one value per index
   * of the run - so it is the one that reports rather than answering, and the original's
   * `assertThatIllegalArgumentException` is a `Left` here.
   *
   * Two wordings are asserted for the rejection. The prefix is the one the original asserted, and
   * the full message is asserted as well because this port makes it deterministic: the size is
   * settled by the array of the first currency in code order, so `EUR` with three values is the
   * reference and `GBP` with two is what disagrees. The implementation being ported read an
   * unordered map here and its message depended on the iteration order it happened to get.
   *
   * The empty map is the original's last assertion: it describes the run of size zero rather than
   * being rejected, which is the documented behaviour of this factory and the reason a run of
   * non-zero size holding no currency has to be built with one of the other three.
   */
  test("test_of_map") {
    val array: MultiCurrencyAmountArray =
      arrayOf(GBP -> DoubleArray.of(20d, 21d, 22d), EUR -> DoubleArray.of(40d, 43d, 44d))

    val expected: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        List(
          multiOf(amountOf(GBP, 20d), amountOf(EUR, 40d)),
          multiOf(amountOf(GBP, 21d), amountOf(EUR, 43d)),
          multiOf(amountOf(GBP, 22d), amountOf(EUR, 44d))))

    array.size shouldBe 3
    array shouldBe expected

    val unequal: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(GBP -> DoubleArray.of(20d, 21d), EUR -> DoubleArray.of(40d, 43d, 44d)))
    unequal should beFailureWith(FailureReason.INVALID)
    unequal should haveFailureMessageMatching(Regex.quote(ArrayLengthPrefix) + ".*")
    unequal should haveFailureMessageMatching(
      Regex.quote("Arrays must have the same size but found sizes 3 and 2"))

    val empty: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.of(Map.empty[Currency, DoubleArray]))
    empty.size shouldBe 0
    empty.values shouldBe Map.empty[Currency, DoubleArray]
  }

  /**
   * Asserts the values of the run, both as a whole and one currency at a time.
   *
   * The original read them as a whole through an accessor; here they are the `values` field of
   * the type, which is the same map, and the comparison is by content so the map written in the
   * test does not have to be sorted. That the run itself holds them sorted is asserted separately
   * through the order of its keys, because that ordering is what makes the rendering, the JSON
   * and the order of the rates a conversion asks for reproducible.
   *
   * The per-currency accessor is asserted alongside, for a currency the run holds and for one it
   * does not: the second is a reported failure rather than an array of zeroes.
   */
  test("test_getValues") {
    val expected: Map[Currency, DoubleArray] = Map(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d))
    VALUES_ARRAY.values shouldBe expected
    VALUES_ARRAY.values.keys.toList shouldBe List(EUR, GBP, USD)

    VALUES_ARRAY.getValues(EUR) should haveValue(DoubleArray.of(40d, 43d, 44d))
    val unknown: FailureOr[DoubleArray] = VALUES_ARRAY.getValues(CHF)
    unknown should beFailureWith(FailureReason.INVALID)
    unknown should haveFailureMessageMatching(Regex.quote("No values available for CHF"))
  }

  /**
   * Asserts that an index reassembles the amount held at it, padded currencies included, and
   * that an index outside the run raises rather than answering with an amount.
   *
   * The first assertion is the original's. The two that follow are the padding read back through
   * this member: a run built from amounts whose currency sets differ names every currency at
   * every index, the ones an amount did not name with a zero amount, so an index of a ragged run
   * has more currencies than the amount it was built from.
   *
   * The original's two `assertThatExceptionOfType(IndexOutOfBoundsException)` assertions over
   * `get(3)` and `get(-1)` close the test, over the same fixture the original used: it holds
   * three values per currency, so `3` is one index past the end and `-1` one before the start.
   * This member is total in signature in this port, as it was there, because an index outside the
   * run is a caller-contract invariant rather than a property of the data - so it stays the index
   * exception the underlying array raises instead of widening into the reported-failure channel
   * the rest of this suite asserts against, and it is asserted here as the throw it is. What is
   * pinned is the type the member documents, `IndexOutOfBoundsException`; the instance that
   * arrives is the `ArrayIndexOutOfBoundsException` of the backing `DoubleArray`, which is a
   * subtype of it, and its message is the runtime's own wording rather than this library's, so
   * the message is not asserted. Without these two an implementation that clamped the index,
   * wrapped it, or answered an out-of-range index with an empty amount would pass every other
   * assertion of this suite.
   */
  test("test_get") {
    val expected: MultiCurrencyAmount =
      multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))
    VALUES_ARRAY.get(2) shouldBe expected
    VALUES_ARRAY.get(0) shouldBe
      multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d))

    val ragged: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(multiOf(amountOf(EUR, 4d)), multiOf(amountOf(GBP, 21d)))
    ragged.get(0) shouldBe multiOf(amountOf(EUR, 4d), amountOf(GBP, 0d))
    ragged.get(1) shouldBe multiOf(amountOf(EUR, 0d), amountOf(GBP, 21d))

    intercept[IndexOutOfBoundsException](VALUES_ARRAY.get(3))
    intercept[IndexOutOfBoundsException](VALUES_ARRAY.get(-1))
  }

  /**
   * Asserts the amounts of the run, in order.
   *
   * The stream of the original is an iterator here - a lazy sequence traversed once, which is
   * what a stream is - and the eager form of it is `toList`. Both are asserted against the same
   * list, so the elements and their order are pinned: a list comparison is ordered, which is what
   * makes this an assertion about index order and not only about membership.
   */
  test("test_stream") {
    val expected: List[MultiCurrencyAmount] = List(
      multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d)),
      multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
      multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d)))

    VALUES_ARRAY.iterator.toList shouldBe expected
    VALUES_ARRAY.toList shouldBe expected
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the conversion, which collapses the currencies into one array of values.
   *
   * The result is a [[CurrencyAmountArray]] and not another run of this type, because converting
   * leaves one currency. The rates are the original's, supplied here through a provider built
   * from a function rather than through a matrix, so what is asserted is the arithmetic of the
   * conversion and not the triangulation of a rate source.
   *
   * The expectation is written as the sum of products the original wrote, in that order: typing
   * the decimals those products round to would introduce a rounding the conversion never performs
   * and would hide a real difference behind it. The conversion accumulates in the order of the
   * currency codes, and for these values the two orders agree bit for bit, which is why this can
   * be asserted for equality while the test below needs a tolerance.
   *
   * A rate the provider cannot supply fails the whole conversion rather than converting the
   * currencies it can, since a partially converted run would be numbers with no meaning, and it
   * carries the currency-conversion reason rather than the general invalid-argument one.
   */
  test("test_convertedTo") {
    val provider: FxRateProvider =
      rateProvider(Map((GBP, CAD) -> 2d, (USD, CAD) -> 1.3d, (EUR, CAD) -> 1.4d))
    val expected: DoubleArray = DoubleArray.of(
      20d * 2d + 30d * 1.3d + 40d * 1.4d,
      21d * 2d + 32d * 1.3d + 43d * 1.4d,
      22d * 2d + 33d * 1.3d + 44d * 1.4d)

    val converted: CurrencyAmountArray = unwrap(VALUES_ARRAY.convertedTo(CAD, provider))
    converted.currency shouldBe CAD
    converted.size shouldBe 3
    converted.values shouldBe expected
    VALUES_ARRAY.convertedTo(CAD, provider) should haveValue(CurrencyAmountArray.of(CAD, expected))

    val missingRate: FailureOr[CurrencyAmountArray] =
      VALUES_ARRAY.convertedTo(CAD, rateProvider(Map((GBP, CAD) -> 2d, (USD, CAD) -> 1.3d)))
    missingRate should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    missingRate should haveFailureMessageMatching(Regex.quote("No FX rate found for EUR/CAD"))
  }

  /**
   * Asserts the conversion into a currency the run already holds.
   *
   * The run's own GBP values are carried through at a rate of one, which the provider answers for
   * a currency and itself exactly as the matrix of the original did, and the other two currencies
   * are converted and added to them.
   *
   * The expectation is the original's, written as a sum of quotients, and it is compared within
   * the tolerance the original passed to its per-element assertion rather than for equality: a
   * quotient by 1.5 and a product by its reciprocal are the same number only to within rounding,
   * and asserting equality would be asserting which of the two the conversion happens to compute.
   */
  test("test_convertedTo_existingCurrency") {
    val provider: FxRateProvider = rateProvider(Map((USD, GBP) -> 1d / 1.5d, (EUR, GBP) -> 0.7d))
    val converted: CurrencyAmountArray = unwrap(VALUES_ARRAY.convertedTo(GBP, provider))
    val expected: DoubleArray = DoubleArray.of(
      20d + 30d / 1.5d + 40d * 0.7d,
      21d + 32d / 1.5d + 43d * 0.7d,
      22d + 33d / 1.5d + 44d * 0.7d)

    converted.currency shouldBe GBP
    converted.size shouldBe 3
    converted.values.equalWithTolerance(expected, Tolerance) shouldBe true
  }


  //-------------------------------------------------------------------------
  /**
   * Asserts the equality and hashing the original wrote this test for.
   *
   * The comment of the original named what it was checking - hand-written `equals` and `hashCode`
   * that handle a map whose values are arrays - and that is exactly what is at stake here: the
   * values are compared element by element on their bit patterns rather than by array identity,
   * so two separately built runs holding equal values are equal and hash alike.
   *
   * Three cases the original did not write are asserted with it, because they are what make the
   * equality meaningful rather than accidental: a run built from the map form in a different
   * order is equal to one built from amounts, a run of the same currencies and a different size
   * is not, and a value of an unrelated type is not.
   */
  test("test_equalsHashCode") {
    val array: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        List(
          multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d)),
          multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d)),
          multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))))

    array shouldBe VALUES_ARRAY
    array.hashCode shouldBe VALUES_ARRAY.hashCode
    Eq[MultiCurrencyAmountArray].eqv(array, VALUES_ARRAY) shouldBe true
    Hash[MultiCurrencyAmountArray].hash(array) shouldBe
      Hash[MultiCurrencyAmountArray].hash(VALUES_ARRAY)

    // the same run assembled from the representation directly, the currencies given in yet
    // another order: a run holds them sorted, so neither equality nor hashing can see the order
    val fromMap: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      GBP -> DoubleArray.of(20d, 21d, 22d))
    fromMap shouldBe VALUES_ARRAY
    fromMap.hashCode shouldBe VALUES_ARRAY.hashCode

    val shorter: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d),
      USD -> DoubleArray.of(30d, 32d),
      EUR -> DoubleArray.of(40d, 43d))
    Eq[MultiCurrencyAmountArray].eqv(VALUES_ARRAY, shorter) shouldBe false
    VALUES_ARRAY.equals(ForeignValue) shouldBe false
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the instances that replaced the reflective bean sweep of the original.
   *
   * The original drove `coverImmutableBean` over the fixture and `coverBeanEquals` over the
   * fixture and a second, deliberately different run. Neither has a target here - nothing of this
   * port inspects a class while the program runs - so the second run is built as the original
   * built it and the equality, hashing and rendering instances are asserted over the two
   * directly, which is what the sweep was standing in for.
   *
   * The two properties the sweep read reflectively are read here as what they are, and the
   * rendering is asserted in full: it is the form the generated bean produced, the two fields
   * named in declaration order with the values as the sorted map of the library being ported
   * rendered them.
   */
  test("coverage") {
    val test2: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(
        multiOf(amountOf(GBP, 21d), amountOf(USD, 31d), amountOf(EUR, 41d)),
        multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d)))

    Eq[MultiCurrencyAmountArray].eqv(VALUES_ARRAY, VALUES_ARRAY) shouldBe true
    Eq[MultiCurrencyAmountArray].eqv(test2, test2) shouldBe true
    Eq[MultiCurrencyAmountArray].eqv(VALUES_ARRAY, test2) shouldBe false

    Hash[MultiCurrencyAmountArray].hash(VALUES_ARRAY) shouldBe VALUES_ARRAY.hashCode
    Hash[MultiCurrencyAmountArray].hash(test2) shouldBe test2.hashCode

    // the two properties the reflective sweep read, read as what they are
    VALUES_ARRAY.size shouldBe 3
    VALUES_ARRAY.values.keys.toList shouldBe List(EUR, GBP, USD)
    test2.size shouldBe 2
    test2.values.keys.toList shouldBe List(EUR, GBP, USD)

    Show[MultiCurrencyAmountArray].show(VALUES_ARRAY) shouldBe VALUES_ARRAY.toString
    Show[MultiCurrencyAmountArray].show(test2) shouldBe test2.toString
    VALUES_ARRAY.toString shouldBe
      "MultiCurrencyAmountArray{size=3, values={EUR=[40.0, 43.0, 44.0], " +
      "GBP=[20.0, 21.0, 22.0], USD=[30.0, 32.0, 33.0]}}"
    test2.toString shouldBe
      "MultiCurrencyAmountArray{size=2, values={EUR=[41.0, 44.0], GBP=[21.0, 22.0], " +
      "USD=[31.0, 33.0]}}"
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the element-wise addition of two runs over the union of their currencies.
   *
   * The fixtures are the original's, chosen so that each run holds a currency the other does not
   * and two they share: a shared currency is added element by element, and an unshared one is
   * carried through as it stands - which is the padding of this type read through addition, since
   * the other run holds zero for that currency at every index and adding zero changes nothing.
   *
   * Adding the two the other way round is asserted with it. The result is the same because the
   * union is symmetric and the sums of these whole numbers are exact, which is what makes the
   * carried-through currencies a property of the operation rather than of which operand it was
   * called on.
   */
  test("test_plusArray") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(180d, 186d, 188d),
      CHF -> DoubleArray.of(300d, 308d, 312d))

    array1.plus(array2) should haveValue(expected)
    array2.plus(array1) should haveValue(expected)
  }

  /**
   * Asserts the addition of one amount to every index of a run.
   *
   * This shifts the whole run rather than combining two runs, so there is no size to disagree
   * about. The fixtures are the original's: a currency the amount names and the run does not
   * becomes a currency of the result holding that one value at every index, a currency the run
   * holds and the amount does not is carried through unchanged, and a currency both name is
   * shifted at every index.
   */
  test("test_plusAmount") {
    val array: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val amount: MultiCurrencyAmount =
      unwrap(MultiCurrencyAmount.of(Map(GBP -> 21d, EUR -> 143d, CHF -> 254d)))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(21d, 21d, 21d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(183d, 186d, 187d),
      CHF -> DoubleArray.of(304d, 308d, 310d))

    array.plus(amount) should haveValue(expected)
    // an amount naming no currency leaves the run as it stands
    array.plus(MultiCurrencyAmount.empty) should haveValue(array)
  }

  /**
   * Asserts that adding runs of different sizes is reported rather than thrown.
   *
   * There is no value to add at an index only one run has, and which run is longer is a property
   * of the values a caller holds, so this is the original's `assertThatIllegalArgumentException`
   * as a `Left`. The wording is that of the implementation being ported and names both sizes, so
   * it is asserted in both directions: the run the operation was called on is named first.
   */
  test("test_plusDifferentSize") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d),
      EUR -> DoubleArray.of(40d, 43d),
      CHF -> DoubleArray.of(50d, 54d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))

    val shorterPlusLonger: FailureOr[MultiCurrencyAmountArray] = array1.plus(array2)
    shorterPlusLonger should beFailureWith(FailureReason.INVALID)
    shorterPlusLonger should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))

    val longerPlusShorter: FailureOr[MultiCurrencyAmountArray] = array2.plus(array1)
    longerPlusShorter should beFailureWith(FailureReason.INVALID)
    longerPlusShorter should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
  }

  /**
   * Asserts the element-wise subtraction of two runs over the union of their currencies.
   *
   * This is addition read in the other direction and behaves the same way in every respect but
   * one, which the fixtures of the original are chosen to show: a currency only the other run
   * holds is carried through '''negated''' rather than as it stands, because this run holds zero
   * for it at every index and nothing minus those values is their negation.
   */
  test("test_minusArray") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(-20d, -21d, -22d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(-100d, -100d, -100d),
      CHF -> DoubleArray.of(-200d, -200d, -200d))

    array1.minus(array2) should haveValue(expected)
  }

  /**
   * Asserts the subtraction of one amount from every index of a run.
   *
   * The fixtures are the original's, and they cover the same three-way choice its addition
   * covered: a currency only the amount names becomes a currency of the result holding the
   * negation of that value at every index, a currency only the run holds is carried through
   * unchanged, and a currency both name is reduced at every index.
   */
  test("test_minusAmount") {
    val array: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(40d, 43d, 44d),
      CHF -> DoubleArray.of(50d, 54d, 56d))
    val amount: MultiCurrencyAmount =
      unwrap(MultiCurrencyAmount.of(Map(GBP -> 21d, EUR -> 143d, CHF -> 254d)))
    val expected: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(-21d, -21d, -21d),
      USD -> DoubleArray.of(30d, 32d, 33d),
      EUR -> DoubleArray.of(-103d, -100d, -99d),
      CHF -> DoubleArray.of(-204d, -200d, -198d))

    array.minus(amount) should haveValue(expected)
    // an amount naming no currency leaves the run as it stands
    array.minus(MultiCurrencyAmount.empty) should haveValue(array)
  }

  /**
   * Asserts that subtracting runs of different sizes is reported rather than thrown.
   *
   * The check is the one addition performs and the wording is identical, which is why both
   * directions are asserted here as well: the two members share it, so a change to one of them
   * that did not change the other would fail here.
   */
  test("test_minusDifferentSize") {
    val array1: MultiCurrencyAmountArray = arrayOf(
      USD -> DoubleArray.of(30d, 32d),
      EUR -> DoubleArray.of(40d, 43d),
      CHF -> DoubleArray.of(50d, 54d))
    val array2: MultiCurrencyAmountArray = arrayOf(
      GBP -> DoubleArray.of(20d, 21d, 22d),
      EUR -> DoubleArray.of(140d, 143d, 144d),
      CHF -> DoubleArray.of(250d, 254d, 256d))

    val shorterMinusLonger: FailureOr[MultiCurrencyAmountArray] = array1.minus(array2)
    shorterMinusLonger should beFailureWith(FailureReason.INVALID)
    shorterMinusLonger should haveFailureMessageMatching(Regex.quote(SizeMismatchMessage))

    val longerMinusShorter: FailureOr[MultiCurrencyAmountArray] = array2.minus(array1)
    longerMinusShorter should beFailureWith(FailureReason.INVALID)
    longerMinusShorter should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the aggregation that replaced the collector of the original.
   *
   * The original collected a stream of single-currency runs through the `Collector` the ported
   * class published. This port publishes no collector - a collector is the reduction interface of
   * a Java stream, and the aggregation it performed is a factory here - so
   * [[MultiCurrencyAmountArray.total]] is the substitution, and it is handed the original's
   * input and asserted against the original's expectation: each run contributes its values under
   * its own currency, and a currency that appears more than once has its runs added element by
   * element.
   *
   * The reversed input is asserted with it, which is what the parallel-collection test of a
   * collector was for: these values are whole numbers, so their sums are exact and the aggregate
   * cannot depend on the order the input is traversed in.
   */
  test("collector") {
    MultiCurrencyAmountArray.total(CollectorArrays) should haveValue(CollectorExpected)
    MultiCurrencyAmountArray.total(CollectorArrays.reverse) should haveValue(CollectorExpected)
  }

  /**
   * Asserts the aggregating factory in its own right.
   *
   * The original asserted it over the same input as its collector, which is the first assertion
   * here. The cases that follow are the ones the aggregation has of its own and that the
   * collector test does not reach: no runs at all, which describes the run of size zero; a single
   * run, which is carried through under its currency; the same run twice, which is added to
   * itself; and the order of the currencies of the result, which is the order of their codes
   * whatever order the input arrived in.
   */
  test("total") {
    MultiCurrencyAmountArray.total(CollectorArrays) should haveValue(CollectorExpected)

    val none: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.total(List.empty[CurrencyAmountArray]))
    none.size shouldBe 0
    none.values shouldBe Map.empty[Currency, DoubleArray]

    val gbpRun: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(11d, 12d, 13d))
    val single: MultiCurrencyAmountArray = unwrap(MultiCurrencyAmountArray.total(List(gbpRun)))
    single.size shouldBe 3
    single.getCurrencies shouldBe Set(GBP)
    single.getValues(GBP) should haveValue(DoubleArray.of(11d, 12d, 13d))

    val doubled: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.total(List(gbpRun, gbpRun)))
    doubled.getValues(GBP) should haveValue(DoubleArray.of(22d, 24d, 26d))

    unwrap(MultiCurrencyAmountArray.total(CollectorArrays)).values.keys.toList shouldBe
      List(EUR, GBP, USD)
  }

  /**
   * Asserts that aggregating runs of different lengths is reported rather than thrown.
   *
   * The original asserted that its collector rejected such input; the aggregating factory that
   * replaced it reports it, and the reason depends on where the disagreement is found, which is
   * why both routes are asserted:
   *
   *   - runs of '''different''' currencies are never added to each other, so the lengths
   *     disagree only when they are held together in one run, and it is the checking factory that
   *     reports it. The reference length is that of the first currency in code order, so GBP with
   *     two values settles the length and USD with three is what disagrees.
   *   - runs of the '''same''' currency are added element by element first, so the single-currency
   *     addition reports it before the checking factory is reached, with its own wording.
   */
  test("collectorDifferentArrayLengths") {
    val differentCurrencies: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(1d, 2d))))
    differentCurrencies should beFailureWith(FailureReason.INVALID)
    differentCurrencies should haveFailureMessageMatching(Regex.quote(ArrayLengthPrefix) + ".*")
    differentCurrencies should haveFailureMessageMatching(
      Regex.quote("Arrays must have the same size but found sizes 2 and 3"))

    val sameCurrency: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
          CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d))))
    sameCurrency should beFailureWith(FailureReason.INVALID)
    sameCurrency should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
  }

  //-------------------------------------------------------------------------
  // The nine tests below are additions to the ported set rather than ports of Java test methods,
  // so each carries a name of its own and none of the names above changes: the migration manifest
  // joins a Java test method to a test of this suite by name. They pin the boundary between the
  // amounts a caller holds and the primitive arrays this type keeps their values in - what
  // reconstruction produces and raises, what the three total factories transpose amounts into, how
  // many rates a conversion asks for and in what order it adds them up, and how much of its input
  // the aggregation reads and in what order it adds the runs of one currency. The tests above
  // exercise that boundary only for short runs of whole numbers.

  /**
   * Asserts that an index is reassembled from the stored values alone, and in currency order.
   *
   * This is `test_get` read as a contract about what reconstruction produces rather than about
   * one fixture: an index names every currency of the run exactly once, in the order of the
   * currency codes, taking each currency's value at that position and nothing else. The entries
   * are read as the map the amount holds, so what is asserted is that no currency is merged away,
   * none is renamed and none is reordered by the route the values take out of the arrays.
   *
   * The disjoint case is the padding at its sharpest: two amounts naming one currency each
   * describe a run of two currencies, so each index names a currency that the amount it came from
   * did not, with a zero amount. A run holding no currency reads no array at all, so it answers
   * with the empty amount at every index, whether or not that index is inside the run.
   */
  test("get reassembles every currency of the run, in currency-code order") {
    VALUES_ARRAY.get(0) shouldBe multiOf(amountOf(GBP, 20d), amountOf(USD, 30d), amountOf(EUR, 40d))
    VALUES_ARRAY.get(1) shouldBe multiOf(amountOf(GBP, 21d), amountOf(USD, 32d), amountOf(EUR, 43d))
    VALUES_ARRAY.get(2) shouldBe multiOf(amountOf(GBP, 22d), amountOf(USD, 33d), amountOf(EUR, 44d))
    // one entry per currency of the run, in the order the run holds them
    VALUES_ARRAY.get(0).toMap.keys.toList shouldBe List(EUR, GBP, USD)
    VALUES_ARRAY.get(0).toMap shouldBe Map(EUR -> 40d, GBP -> 20d, USD -> 30d)

    // the zero padding: a currency no amount named at that index is named with a zero amount
    val disjoint: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(multiOf(amountOf(GBP, 1d)), multiOf(amountOf(USD, 2d)))
    disjoint.get(0) shouldBe multiOf(amountOf(GBP, 1d), amountOf(USD, 0d))
    disjoint.get(1) shouldBe multiOf(amountOf(GBP, 0d), amountOf(USD, 2d))
    disjoint.get(0).toMap shouldBe Map(GBP -> 1d, USD -> 0d)

    // a run holding no currency reads no array, so every index answers with the empty amount
    val noCurrencies: MultiCurrencyAmountArray =
      MultiCurrencyAmountArray.of(2, (_: Int) => MultiCurrencyAmount.empty)
    noCurrencies.get(0) shouldBe MultiCurrencyAmount.empty
    noCurrencies.get(1) shouldBe MultiCurrencyAmount.empty
    noCurrencies.get(7) shouldBe MultiCurrencyAmount.empty
    noCurrencies.get(-1) shouldBe MultiCurrencyAmount.empty
  }

  /**
   * Asserts the two documented invariants reconstruction raises rather than reports.
   *
   * Both are caller-contract invariants of the implementation being ported, kept as such by this
   * port, and `test_get` says where the original asserted the first of them. They are asserted
   * here because they are properties of the route an index takes out of the arrays: an index
   * outside the run is the index exception of the underlying array, raised as the array is read,
   * and a value that is not a number is the amount invariant, raised as that value is turned into
   * an amount and carrying the message the invariant reports for it.
   *
   * The run that holds such a value is built through the factory that reads the representation
   * directly, which is the only route by which one can enter a run - the factories that read
   * amounts cannot, because no amount holds such a value - and the index that holds a number is
   * read back first, so what fires is the value and not the run.
   */
  test("get raises its two documented invariants as it reads the values") {
    an[IndexOutOfBoundsException] should be thrownBy VALUES_ARRAY.get(3)
    an[IndexOutOfBoundsException] should be thrownBy VALUES_ARRAY.get(-1)

    val withNotANumber: MultiCurrencyAmountArray =
      arrayOf(GBP -> DoubleArray.of(1d, Double.NaN), USD -> DoubleArray.of(2d, 3d))
    withNotANumber.get(0) shouldBe multiOf(amountOf(GBP, 1d), amountOf(USD, 2d))
    the[IllegalArgumentException] thrownBy withNotANumber.get(1) should have message
      "Argument 'amount' must not be NaN"
  }

  /**
   * Asserts that the three total factories agree, one full-length array per currency.
   *
   * The three read their amounts through one transposition, so this is where that transposition
   * is pinned: the same two disjoint amounts are handed to the collection form, the varargs form
   * and the function form, and each produces the same two full-length arrays with the padded zero
   * where the amount named no value. A transposition that read an absent currency as anything but
   * zero, or that produced the currencies in another order, would differ here for all three.
   */
  test("the three total factories transpose amounts into one array per currency") {
    val disjointAmounts: List[MultiCurrencyAmount] =
      List(multiOf(amountOf(GBP, 1d)), multiOf(amountOf(USD, 2d)))

    assertDisjointRun(MultiCurrencyAmountArray.of(disjointAmounts))
    assertDisjointRun(MultiCurrencyAmountArray.of(disjointAmounts: _*))
    assertDisjointRun(MultiCurrencyAmountArray.of(2, index => disjointAmounts(index)))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that a conversion asks for each currency's rate exactly once, in code order.
   *
   * The documented contract of the conversion is one lookup per currency of the run, not one per
   * currency and index: the rate is applied to a whole array, which is what guarantees that a run
   * is converted at a single rate and what makes the cost of a conversion independent of how long
   * the run is. The provider counts what it is asked and records the order, so a conversion that
   * asked per element, or that asked twice for a currency, would fail here whatever numbers it
   * produced.
   *
   * The order is asserted with the count because it is the order the sum is accumulated in, and
   * the test below depends on it.
   */
  test("convertedTo asks for each currency's rate exactly once, in currency-code order") {
    val asked: AtomicReference[Vector[Currency]] = new AtomicReference(Vector.empty[Currency])
    val provider: FxRateProvider =
      recordingRateProvider(Map((EUR, CAD) -> 1.4d, (GBP, CAD) -> 2d, (USD, CAD) -> 1.3d), asked)

    val converted: CurrencyAmountArray = unwrap(VALUES_ARRAY.convertedTo(CAD, provider))

    converted.size shouldBe 3
    converted.values shouldBe DoubleArray.of(
      0d + 20d * 2d + 30d * 1.3d + 40d * 1.4d,
      0d + 21d * 2d + 32d * 1.3d + 43d * 1.4d,
      0d + 22d * 2d + 33d * 1.3d + 44d * 1.4d)
    // three currencies, three lookups - not one per currency and index, which would be nine -
    // and in the order of the currency codes, which is the order the sum is accumulated in
    asked.get().size shouldBe 3
    asked.get() shouldBe Vector(EUR, GBP, USD)
  }

  /**
   * Asserts that a refused rate returns at once, with nothing asked after it.
   *
   * A partially converted run would be numbers with no meaning, so the first rate the provider
   * cannot supply fails the whole conversion. What is pinned here is that no rate after it is
   * asked for: the provider supplies the first currency in code order and refuses the second, and
   * it is asked exactly twice although the run holds three currencies, so the number of lookups a
   * failing conversion costs does not depend on how many currencies follow the one that failed.
   *
   * The failure returned is the one the provider reported for that second currency, not for any
   * other, which is what tells a caller which rate to supply.
   */
  test("convertedTo stops at the first rate the provider cannot supply") {
    val asked: AtomicReference[Vector[Currency]] = new AtomicReference(Vector.empty[Currency])
    val provider: FxRateProvider =
      recordingRateProvider(Map((EUR, CAD) -> 1.4d, (USD, CAD) -> 1.3d), asked)

    val refused: FailureOr[CurrencyAmountArray] = VALUES_ARRAY.convertedTo(CAD, provider)

    refused should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    refused should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/CAD"))
    // EUR answered, GBP refused, USD never asked about
    asked.get().size shouldBe 2
    asked.get() shouldBe Vector(EUR, GBP)
  }

  /**
   * Asserts the converted values to the bit, against the sum computed in the test.
   *
   * The expectation is built here rather than typed as decimals, in the order and with the
   * operands the conversion documents - from zero, the currencies by code, each term the value
   * multiplied by the rate - and it is compared for equality rather than within a tolerance. The
   * values and rates are chosen so that the order matters: accumulating the same three terms in
   * the reverse order differs in the last bit, which the test asserts as well, so a conversion
   * that summed the currencies in any other order would fail here.
   */
  test("convertedTo accumulates the currencies in currency-code order, to the last bit") {
    val run: MultiCurrencyAmountArray = arrayOf(
      EUR -> DoubleArray.of(1d, 3d),
      GBP -> DoubleArray.of(1d, 5d),
      USD -> DoubleArray.of(1d, 7d))
    val rates: Map[(Currency, Currency), Double] =
      Map((EUR, CAD) -> 0.1d, (GBP, CAD) -> 0.2d, (USD, CAD) -> 0.3d)

    val expectedFirst: Double = 0d + 1d * 0.1d + 1d * 0.2d + 1d * 0.3d
    val expectedSecond: Double = 0d + 3d * 0.1d + 5d * 0.2d + 7d * 0.3d
    val reversedFirst: Double = 0d + 1d * 0.3d + 1d * 0.2d + 1d * 0.1d

    val converted: CurrencyAmountArray = unwrap(run.convertedTo(CAD, rateProvider(rates)))
    converted.values shouldBe DoubleArray.of(expectedFirst, expectedSecond)
    converted.values.get(0) shouldBe expectedFirst
    // the order is observable: the same terms added the other way round are a different number
    reversedFirst should not be expectedFirst
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that runs of one currency are added left to right, to the bit, and only once each.
   *
   * The aggregation is documented to add the runs of a currency in the order the input presents
   * them, and that order is observable: the three values are chosen so that adding them the other
   * way round differs in the last bit, which the test asserts as well, so an aggregation that
   * accumulated in any other order - or that started from a zero array rather than from the first
   * run - would fail here rather than only in the parity baseline.
   *
   * A currency offered a single run is asserted with them: it contributes the values it already
   * holds, unchanged and compared by bit pattern.
   */
  test("total adds the runs of one currency left to right, to the last bit") {
    val first: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(0.1d, 1d))
    val second: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(0.2d, 2d))
    val third: CurrencyAmountArray = CurrencyAmountArray.of(GBP, DoubleArray.of(0.3d, 3d))

    val expectedFirst: Double = (0.1d + 0.2d) + 0.3d
    val reversedFirst: Double = (0.3d + 0.2d) + 0.1d

    val totalled: MultiCurrencyAmountArray =
      unwrap(MultiCurrencyAmountArray.total(List(first, second, third)))
    totalled.size shouldBe 2
    totalled.getCurrencies shouldBe Set(GBP)
    totalled.getValues(GBP) should haveValue(DoubleArray.of(expectedFirst, (1d + 2d) + 3d))
    // the order is observable: the same three values added the other way round differ in the last
    // bit, so this is an assertion about the order and not only about the sum
    reversedFirst should not be expectedFirst

    val single: MultiCurrencyAmountArray = unwrap(MultiCurrencyAmountArray.total(List(second)))
    single.getValues(GBP) should haveValue(second.values)
  }

  /**
   * Asserts that a mixture of repeated and single currencies keeps the arrays in currency order.
   *
   * The input names three currencies, two of them more than once and interleaved, so what is
   * asserted is that each currency's runs are added among themselves and that the result holds
   * the currencies in code order whatever order the input arrived in - the order the rendering,
   * the JSON and the rates a conversion asks for all depend on.
   */
  test("total keeps the per-currency arrays in currency-code order") {
    val totalled: MultiCurrencyAmountArray = unwrap(
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(10d, 20d)),
          CurrencyAmountArray.of(USD, DoubleArray.of(3d, 4d)),
          CurrencyAmountArray.of(EUR, DoubleArray.of(100d, 200d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(30d, 40d)))))

    totalled.size shouldBe 2
    totalled.values.keys.toList shouldBe List(EUR, GBP, USD)
    totalled.getValues(EUR) should haveValue(DoubleArray.of(100d, 200d))
    totalled.getValues(GBP) should haveValue(DoubleArray.of(40d, 60d))
    totalled.getValues(USD) should haveValue(DoubleArray.of(4d, 6d))
  }

  /**
   * Asserts that the aggregation reads no further than the outcome needs.
   *
   * Two runs of one currency whose lengths differ decide the whole call, so nothing after the
   * second of them may be read: the input is an `Iterable` whose iterator records what it hands
   * over and fails the test if it is asked for anything past the pair that decided the answer, so
   * an aggregation that kept consuming its input after the reason was settled - which a very
   * large or lazily generated input makes expensive and an endless one makes fatal - fails here
   * rather than merely taking longer.
   *
   * The reason is asserted in full, in the wording of the single-currency addition, and the same
   * input read as an ordinary finite list is asserted to give the same reason, so the
   * short-circuit is a property of how much is read and not of what is reported.
   *
   * The disagreement across currencies is asserted alongside, because it is the other reason and
   * it is decided elsewhere: runs of different currencies are never added to each other, so their
   * lengths are compared only when they are held together in one run, and every one that
   * disagrees with the first in currency order is reported, in currency order.
   */
  test("total reads no further than the outcome needs") {
    val handed: AtomicReference[Vector[Int]] = new AtomicReference(Vector.empty[Int])
    val decisive: List[CurrencyAmountArray] = List(
      CurrencyAmountArray.of(USD, DoubleArray.of(10d, 20d, 30d)),
      CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d)))

    val shortCircuited: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(readOnceThenFailing(decisive, handed))
    shortCircuited should beFailureWith(FailureReason.INVALID)
    shortCircuited should haveFailureMessageMatching(
      Regex.quote("Sizes must be equal, this size is 3, other size is 2"))
    // the two runs that decided the answer, and nothing after them
    handed.get() shouldBe Vector(3, 2)

    // read as an ordinary list, the same input reports the same single reason
    failureMessages(MultiCurrencyAmountArray.total(decisive)) shouldBe
      List("Sizes must be equal, this size is 3, other size is 2")

    val acrossCurrencies: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(EUR, DoubleArray.of(1d, 2d)),
          CurrencyAmountArray.of(GBP, DoubleArray.of(1d)),
          CurrencyAmountArray.of(USD, DoubleArray.of(1d, 2d, 3d))))
    acrossCurrencies should beFailureWith(FailureReason.INVALID)
    // EUR settles the length, and GBP and USD are reported in that order
    failureMessages(acrossCurrencies) shouldBe List(
      "Arrays must have the same size but found sizes 2 and 1",
      "Arrays must have the same size but found sizes 2 and 3")
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a run from values per currency, failing the test if they describe none.
   *
   * The factory that reads the representation directly is the one factory of this type that can
   * report, so every fixture built from arrays goes through this rather than repeating the
   * unwrapping. The pairs are given in any order; a run holds its currencies sorted.
   *
   * @param values  the values per currency, each array holding one value per index of the run
   * @return the run holding those values
   */
  private def arrayOf(values: (Currency, DoubleArray)*): MultiCurrencyAmountArray =
    unwrap(MultiCurrencyAmountArray.of(values.toMap))

  /**
   * Asserts the run the three total factories build from one GBP amount and one USD amount.
   *
   * The expectation is shared by the three because the three share the transposition that
   * produces it, and it is written once here so that a difference between them is a difference
   * from this and not from each other.
   *
   * @param run  the run built from `[GBP 1]` at index zero and `[USD 2]` at index one
   * @return the assertion that it holds the two padded arrays in currency order
   */
  private def assertDisjointRun(run: MultiCurrencyAmountArray): Assertion = {
    run.size shouldBe 2
    run.values.keys.toList shouldBe List(GBP, USD)
    run.getValues(GBP) should haveValue(DoubleArray.of(1d, 0d))
    run.getValues(USD) should haveValue(DoubleArray.of(0d, 2d))
  }

  /**
   * Reads a run back out of a document, failing the test if it cannot be read.
   *
   * This is the decoding half of the round trip that replaced the Java serialization of the
   * original. A document that cannot be read is reported as a failed test naming the reason the
   * codec gave, so a change of shape is a readable failure rather than a match error.
   *
   * @param json  the document expected to describe a run
   * @return the run it describes
   */
  private def decoded(json: Json): MultiCurrencyAmountArray =
    json
      .as[MultiCurrencyAmountArray]
      .fold(
        failure => fail(s"Expected a run but decoding failed with: ${failure.message}"),
        value => value)

  /**
   * Builds a provider answering from a table of rates, and failing for anything else.
   *
   * This is the substitution for the matrix the original built its providers with: the
   * conversions asserted here are the arithmetic of the conversion itself, so the provider is the
   * least a provider can be - a lookup of the pairs the test names. A currency and itself answers
   * with one, as the matrix of the original did and as every provider of this port does, which is
   * what makes a conversion into a currency the run already holds work; anything else is reported
   * with the currency-conversion reason, so a conversion this test expects to fail fails for the
   * reason a missing rate has rather than for a reason the test invented.
   *
   * @param rates  the rate of each base and counter currency pair the provider knows
   * @return the provider answering from that table
   */
  private def rateProvider(rates: Map[(Currency, Currency), Double]): FxRateProvider =
    FxRateProvider.fromFunction((baseCurrency, counterCurrency) =>
      if (baseCurrency == counterCurrency) {
        Right(1d)
      } else {
        rates
          .get((baseCurrency, counterCurrency))
          .toRight(
            Failure.CurrencyConversion(s"No FX rate found for $baseCurrency/$counterCurrency"))
      })

  /**
   * Builds a provider that records the base currency of every lookup it is asked to perform.
   *
   * This is [[rateProvider]] with the questions written down: the base currency of each lookup is
   * appended to the given reference as the lookup happens, so the length of what it holds is the
   * number of rates the conversion asked for and its order is the order it asked in. A table that
   * does not name a pair reports the missing rate exactly as [[rateProvider]] does, which is what
   * lets one provider serve both the counting and the short-circuit assertions.
   *
   * The record is kept in an atomic reference rather than in a mutable local, both because
   * neither the domain nor the test code of this port holds one and because it is written from
   * inside a function the conversion calls and read after it returns.
   *
   * @param rates  the rate of each base and counter currency pair the provider knows
   * @param asked  the reference the base currency of each lookup is appended to, in order
   * @return the provider answering from that table and recording what it was asked
   */
  private def recordingRateProvider(
      rates: Map[(Currency, Currency), Double],
      asked: AtomicReference[Vector[Currency]]): FxRateProvider =
    FxRateProvider.fromFunction { (baseCurrency, counterCurrency) =>
      val _ = asked.updateAndGet(seen => seen :+ baseCurrency)
      rates
        .get((baseCurrency, counterCurrency))
        .toRight(Failure.CurrencyConversion(s"No FX rate found for $baseCurrency/$counterCurrency"))
    }

  /**
   * Wraps runs in an `Iterable` that records what it hands over and fails if asked for more.
   *
   * The aggregating factory documents that it reads its input only as far as the outcome needs,
   * which is a property of how much is pulled rather than of what is returned, so asserting it
   * takes an input that can tell: the size of each run handed over is appended to the given
   * reference, and an attempt to pull anything after the last of them fails the test where it
   * happens. The iterator is endless rather than merely exhausted, so a factory that kept reading
   * would not run out of input and quietly succeed.
   *
   * @param elements  the runs to hand over, in order
   * @param handed  the reference the size of each run handed over is appended to, in order
   * @return the collection handing over exactly those runs and failing the test beyond them
   */
  private def readOnceThenFailing(
      elements: List[CurrencyAmountArray],
      handed: AtomicReference[Vector[Int]]): Iterable[CurrencyAmountArray] =
    new Iterable[CurrencyAmountArray] {
      override def iterator: Iterator[CurrencyAmountArray] =
        elements.iterator.map { element =>
          val _ = handed.updateAndGet(seen => seen :+ element.size)
          element
        } ++ Iterator.continually[CurrencyAmountArray](
          fail("the aggregation read an input after the outcome had already been decided"))
    }

  /**
   * Reads the messages of the failures an outcome carries, in the order it carries them.
   *
   * This is [[unwrap]] read the other way round, for the assertions that pin how many reasons an
   * outcome gives and in what order: the message matchers of this port answer whether some
   * failure matches, which cannot distinguish one reason from several or say which came first.
   *
   * @param outcome  the outcome to read the failures of
   * @param shape  the view of that outcome as failures and a value
   * @tparam R  the type of the outcome
   * @tparam A  the type of the value the outcome carries
   * @return the message of each failure it carries, in order
   */
  private def failureMessages[R, A](outcome: R)(implicit shape: Outcome.Aux[R, A]): List[String] =
    shape.failures(outcome).map(failure => failure.message)

  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite and it covers every shape a factory of this
   * port returns - the single-failure outcome of [[CurrencyAmount.of]] and [[MultiCurrencyAmount.of]]
   * and the accumulating outcome of [[MultiCurrencyAmountArray.of]] and
   * [[MultiCurrencyAmountArray.total]] - through the same type class the matchers of this port are
   * resolved by. An outcome carrying failures is reported as a failed test naming every one of
   * them, so a fixture that cannot be built never appears as an error raised from an unrelated
   * line.
   *
   * @param outcome  the outcome expected to carry a value
   * @param shape  the view of that outcome as failures and a value
   * @tparam R  the type of the outcome
   * @tparam A  the type of the value the outcome carries
   * @return the value it carries
   */
  private def unwrap[R, A](outcome: R)(implicit shape: Outcome.Aux[R, A]): A = {
    val failures: List[String] = shape.failures(outcome).map(failure => failure.message)
    shape.value(outcome) match {
      case Some(value) if failures.isEmpty => value
      case _ =>
        fail(s"Expected a value but the factory failed with: ${failures.mkString(", ")}")
    }
  }

  /**
   * Builds a multi-currency amount, failing the test if the amounts describe none.
   *
   * @param amounts  the amounts, expected to name distinct currencies
   * @return the multi-currency amount naming them
   */
  private def multiOf(amounts: CurrencyAmount*): MultiCurrencyAmount =
    unwrap(MultiCurrencyAmount.of(amounts: _*))

  /**
   * Builds an amount, failing the test if the currency and value describe none.
   *
   * @param currency  the currency the amount is in
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the amount
   */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))
}
