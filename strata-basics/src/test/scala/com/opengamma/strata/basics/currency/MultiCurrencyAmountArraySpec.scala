/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger

import scala.util.matching.Regex

import cats.Eq
import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

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
 * The original holds twenty-two test methods and so does this suite, each under the name the
 * original gave it - including the five it wrote without the `test_` prefix, `serializeSize`,
 * `coverage`, `collector`, `total` and `collectorDifferentArrayLengths` - so that a Java test
 * method and a test of this suite stay in one-to-one correspondence in the migration manifest.
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
 * `get(3)` and `get(-1)`, are not `Left` here and are deliberately not asserted in this suite:
 * reading outside a run is a caller-contract invariant of the underlying array, which this port
 * keeps as the index exception of the runtime exactly as the implementation being ported had it,
 * and the sweep over every such documented invariant of the module belongs to
 * `FailableSurfaceSpec`. `test_get` says the same thing where the assertions were.
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
 * invariant to `FailableSurfaceSpec`; the typeclass laws to `TypeclassLawsSpec`; the
 * property-based round trip of every codec to `json.JsonRoundTripSpec` - `serializeSize` here is
 * one targeted example of size preservation, not a sweep; the numerical parity of currency
 * arithmetic against the Java baseline to `parity.CurrencyMathParitySpec`. This suite asserts the
 * cases of the Java test it is ported from.
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
   * Asserts that an index reassembles the amount held at it, padded currencies included.
   *
   * The first assertion is the original's. The two that follow are the padding read back through
   * this member: a run built from amounts whose currency sets differ names every currency at
   * every index, the ones an amount did not name with a zero amount, so an index of a ragged run
   * has more currencies than the amount it was built from.
   *
   * The original's two `assertThatExceptionOfType(IndexOutOfBoundsException)` assertions over
   * `get(3)` and `get(-1)` are not reproduced here. This member is total in signature in this
   * port, as it was there, and an index outside the run is the index exception of the underlying
   * array - a caller-contract invariant rather than a property of the data, so it is not a
   * reported failure and there is nothing in this suite's vocabulary to assert it with. The sweep
   * over every documented invariant of the module, this one included, is `FailableSurfaceSpec`.
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

