/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.SortedMap
import scala.util.matching.Regex

import cats.Hash
import cats.Monoid
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
import com.opengamma.strata.basics.currency.Currency.JPY
import com.opengamma.strata.basics.currency.Currency.NZD
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[MultiCurrencyAmount]], ported from the Java `MultiCurrencyAmountTest`.
 *
 * All fifty-four methods of the original are here, one test each, under the name the Java method
 * had, so that a Java test method and a test of this suite stay in one-to-one correspondence in
 * the migration manifest. The fixtures keep the original's names and values as well - `CCY1`,
 * `CCY2`, `CCY3` and the three amounts `101`, `103`, `107` - so that a reader comparing the two
 * files line by line is comparing the same numbers.
 *
 * ===The three tests that carry the contract===
 *
 * Almost everything this type promises is decided by three of the fifty-four:
 *
 *   - `test_of_Iterable_duplicate` - [[MultiCurrencyAmount.of]] '''refuses''' a collection naming
 *     one currency twice, reporting it as a value;
 *   - `test_total_Iterable_duplicate` - [[MultiCurrencyAmount.total]] '''adds''' the repeated
 *     currency up instead, and cannot fail;
 *   - `test_stream` and `test_toMap` - the amounts are always in the alphabetical order of their
 *     currency codes, whatever order they were supplied in, which is what makes two equal values
 *     serialize to identical bytes.
 *
 * The first two are the same input with opposite outcomes, which is why the original had both
 * factories and why the port keeps both.
 *
 * ===Failure is a value, so the fixtures are unwrapped===
 *
 * Every `of` of this type reports what is wrong with its arguments rather than raising it, so each
 * fixture is built through [[multiOf]] or [[amountOf]], which fold the outcome once through
 * [[unwrap]] and report a fixture that could not be built as a failed test naming the cause. The
 * original could write `MultiCurrencyAmount.of(CA1, CA3)` as a constant because its factory threw.
 *
 * Two groups of assertions change shape with that, and each is noted again at the test it affects:
 *
 *   - '''a repeated currency''' was an `IllegalArgumentException` from `of` and from the bean
 *     builder, and is a `Left` carrying `FailureReason.INVALID` here, with the wording the
 *     original threw with;
 *   - '''an amount asked for in a currency that is not held''' was an `IllegalArgumentException`
 *     from `getAmount`, and is a `Left` carrying `INVALID`. It is asserted for every value this
 *     suite builds, because [[assertMCA]] - the port of the original's private helper of the same
 *     name - asserts it on every call, exactly as the original did.
 *
 * None of the fifty-four asserts a thrown exception: the numeric invariant of the arithmetic, which
 * is the one throw this type documents, is asserted in the root `SmartConstructorSpec` alongside
 * every other numeric edge of the port. One of the five pins described below does assert it, for
 * the aggregation path specifically, because that path adds numbers rather than amounts and so has
 * to reach the invariant itself.
 *
 * ===Five pins this port adds===
 *
 * After the fifty-four there is a short section with no method of the original behind it. The
 * original's aggregation was a `Collector` and its traversal a stream, so the properties that
 * decide whether an aggregate is faithful - which order the amounts of one currency are added in,
 * that aggregating in one pass agrees with combining pairwise, that the numeric invariant is
 * reached on the aggregation path, and how far a conversion traverses before an unavailable rate
 * decides it - were properties of that machinery rather than of the API and went unstated. Each
 * pin names what it pins, asserts the exact bit pattern where a floating point association is at
 * stake, and is deliberately outside the one-to-one correspondence of the fifty-four with the
 * migration manifest.
 *
 * ===Substitutions, and why each is the faithful port===
 *
 * Five groups of the original's methods test machinery this port does not have. Each keeps its
 * name and asserts the thing that replaced it:
 *
 *   - '''the collector''' (`test_collector`, `test_collector_parallel`). The original published a
 *     `Collector` for use with a stream. Aggregation here is
 *     [[MultiCurrencyAmount.total]] and `Monoid[MultiCurrencyAmount].combineAll`, so those are
 *     what the two tests assert. The parallel test becomes the statement that actually mattered
 *     about a parallel stream: the result does not depend on the order the amounts are combined
 *     in.
 *   - '''the bean builder''' (`test_beanBuilder`, `test_beanBuilder_invalid`). The original built
 *     a value through the Joda-Beans builder, which has no target in this port; a value is built
 *     through `of` instead, and the invalid input the builder rejected - a repeated currency - is
 *     a `Left`.
 *   - '''serialization''' (`test_serialization`). The original round-tripped through Java
 *     serialization, which no type of this port supports. The migration manifest records that
 *     method as consolidated into `json.JsonRoundTripSpec`, where the property-based round trip of
 *     every codec of the module lives; the `test_serialization` of this suite is the one concrete
 *     example of the JSON shape of this type, kept under its own name so the correspondence is
 *     visible at the type.
 *   - '''the reflective bean sweep''' (`coverage`). There is no meta-bean to walk. What the sweep
 *     stood in for - that the value behaves as a value - is asserted directly over distinct
 *     instances through the instances the companion publishes.
 *   - '''the seventeen absent-argument methods'''. Each asserted a run-time rejection of Java's
 *     absent reference. That case cannot be written against this API and needs no run-time guard:
 *     the arguments are required values, and the `ArgChecker.notNull` family was dropped in the
 *     port because an absence is modelled by `Option` rather than by an absent reference. Each of
 *     the seventeen therefore asserts at compile time what replaced the check - an argument of the
 *     wrong type, and an argument omitted altogether, are rejected before the program runs - with
 *     the valid call asserted to compile alongside so the proof cannot be passing for an unrelated
 *     reason. None of them is written as an absent-reference literal, which would still conform to
 *     a reference type in this language and so would prove nothing.
 *
 * ===One divergence visible in the fixtures===
 *
 * The original's helper asked for an amount in `Currency.of("FRZ")`, a code it minted on the spot.
 * The currency family of this port is closed, so no such value exists; [[NON_EXISTING]] is an
 * ordinary currency that none of these tests holds an amount in, which asserts the same thing -
 * that reading an unheld currency is reported - against a value the family admits.
 *
 * ===What is asserted elsewhere===
 *
 * The laws of the `Monoid` (over finite amounts, with a tolerant equality) belong to
 * `TypeclassLawsSpec`; this suite asserts its concrete behaviour only - the identity, one
 * aggregation and order-independence. The compile-time sweep over the construction surface of
 * every validated type belongs to `ApiSurfaceSpec`, the numeric-edge throws to
 * `SmartConstructorSpec`, the property-based codec round trips to `json.JsonRoundTripSpec`, and the
 * fixture-driven numerical parity of currency arithmetic to `parity.CurrencyMathParitySpec`.
 *
 * @see [[MultiCurrencyAmount]] for the type under test
 * @see [[CurrencyAmount]] for the single-currency amount it holds
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 */
final class MultiCurrencyAmountSpec extends AnyFunSuite with Matchers {

  /** The first currency of the fixtures, as the original named and valued it. */
  private val CCY1: Currency = AUD

  /** The second currency of the fixtures. */
  private val CCY2: Currency = CAD

  /** The third currency of the fixtures. */
  private val CCY3: Currency = CHF

  /** The first amount, written with the `d` suffix this build requires of a `Double`. */
  private val AMT1: Double = 101d

  /** The second amount. */
  private val AMT2: Double = 103d

  /** The third amount. */
  private val AMT3: Double = 107d

  /** The first fixture amount, `AUD 101`. */
  private val CA1: CurrencyAmount = amountOf(CCY1, AMT1)

  /** The second fixture amount, `CAD 103`. */
  private val CA2: CurrencyAmount = amountOf(CCY2, AMT2)

  /** The third fixture amount, `CHF 107`. */
  private val CA3: CurrencyAmount = amountOf(CCY3, AMT3)

  /**
   * The two-amount value the original rebuilt locally in every absent-argument test.
   *
   * It is hoisted into one fixture here because eleven tests refer to it and none of them
   * changes it - the type is immutable, so one value serves them all.
   */
  private val MTA: MultiCurrencyAmount = multiOf(CA1, CA2)

  /**
   * A currency that no value of this suite holds an amount in.
   *
   * This is the port of the original's `Currency.of("FRZ")`, which minted a currency outside the
   * configured set. The family is closed here, so the role is filled by a configured currency that
   * appears in none of the fixtures; what the original's helper asserted with it - that an unheld
   * currency is reported by `getAmount` and read as zero by `getAmountOrZero` - is asserted
   * unchanged.
   */
  private val NON_EXISTING: Currency = JPY

  /**
   * A value of a type unrelated to this one, for the equality assertion that needs one.
   *
   * Held at the type `Any` and named as the sibling suites name it, so that the assertion reads as
   * a comparison against a foreign value rather than as one the compiler would reject as a
   * comparison of unrelated types.
   */
  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  test("test_empty") {
    assertMCA(MultiCurrencyAmount.empty)
    MultiCurrencyAmount.empty.size shouldBe 0
    MultiCurrencyAmount.empty.toString shouldBe "[]"

    // the value holding nothing is the identity of the additive instance, which is the property
    // the rest of the library relies on when it aggregates a collection that turns out to be empty
    Monoid[MultiCurrencyAmount].empty shouldBe MultiCurrencyAmount.empty
    Monoid[MultiCurrencyAmount].combine(MultiCurrencyAmount.empty, MTA) shouldBe MTA
    Monoid[MultiCurrencyAmount].combine(MTA, MultiCurrencyAmount.empty) shouldBe MTA
  }

  //-------------------------------------------------------------------------
  test("test_of_CurrencyDouble") {
    assertMCA(unwrap(MultiCurrencyAmount.of(CCY1, AMT1)), CA1)

    // the original threw for a number that is not an amount; this factory reports it, which is the
    // recorded divergence of the port, and it reports exactly what `CurrencyAmount.of` reports
    MultiCurrencyAmount.of(CCY1, Double.NaN) should beFailureWith(FailureReason.INVALID)
    MultiCurrencyAmount.of(CCY1, Double.NaN) should haveFailureMessageMatching(
      Regex.quote("Argument 'amount' must not be NaN"))
  }

  /**
   * Asserts at compile time the absent-currency rejection the original asserted at run time.
   *
   * See the note on this class: the `notNull` family of checks was dropped in the port, so there is
   * no run-time guard to assert. The currency is a required [[Currency]] - the text of a code does
   * not stand in for one, an `Option` does not either, and the argument cannot be omitted - and the
   * valid call is asserted to compile so that the three proofs cannot be passing for an unrelated
   * reason.
   */
  test("test_of_CurrencyDouble_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of("AUD", AMT1)""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[Currency], AMT1)""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(AMT1)""")
    assertCompiles("""MultiCurrencyAmount.of(CCY1, AMT1)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_VarArgs_empty") {
    assertMCA(multiOf())
    MultiCurrencyAmount.of() shouldBe Right(MultiCurrencyAmount.empty)
  }

  test("test_of_VarArgs") {
    assertMCA(multiOf(CA1, CA3), CA1, CA3)
  }

  /**
   * Asserts that the varargs factory '''refuses''' a currency named twice.
   *
   * This is one half of the contrast that defines the two factories of this type: `of` reports a
   * repeated currency, because two entries of one currency usually mean a key was lost upstream,
   * while `total` adds them up - see `test_total_Iterable_duplicate`, which runs the same input
   * through the other factory and asserts the sum. The original threw here; the failure carries
   * the wording it threw with, and its reason is compared as a value of the closed family of
   * reasons.
   */
  test("test_of_VarArgs_duplicate") {
    val duplicated: FailureOr[MultiCurrencyAmount] =
      MultiCurrencyAmount.of(CA1, amountOf(CCY1, AMT2))
    duplicated should beFailureWith(FailureReason.INVALID)
    duplicated should haveFailureMessageMatching(Regex.quote("Currency is duplicated: AUD"))
  }

  /**
   * Asserts at compile time the absent-array rejection the original asserted at run time.
   *
   * The original passed an absent array where the varargs were expected. Here every element of the
   * varargs is a required [[CurrencyAmount]]: a currency is not one, and neither is an `Option` of
   * one.
   */
  test("test_of_VarArgs_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of(CA1, CCY2)""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[CurrencyAmount], CA2)""")
    assertCompiles("""MultiCurrencyAmount.of(CA1, CA2)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_Iterable") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, CA3)
    assertMCA(unwrap(MultiCurrencyAmount.of(iterable)), CA1, CA3)
  }

  /**
   * Asserts that the collection factory '''refuses''' a currency named twice.
   *
   * The other half of the contrast described on `test_of_VarArgs_duplicate`: the same three
   * amounts run through [[MultiCurrencyAmount.total]] in `test_total_Iterable_duplicate` produce
   * the sum rather than this failure. The traversal stops where the repeat is found, so the
   * currency the failure names is the first one that repeated.
   */
  test("test_of_Iterable_duplicate") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, amountOf(CCY1, AMT2))
    val duplicated: FailureOr[MultiCurrencyAmount] = MultiCurrencyAmount.of(iterable)
    duplicated should beFailureWith(FailureReason.INVALID)
    duplicated should haveFailureMessageMatching(Regex.quote("Currency is duplicated: AUD"))
  }

  /**
   * Asserts at compile time the absent-collection rejection the original asserted at run time.
   *
   * The collection is a required `Iterable[CurrencyAmount]`: a collection of anything else is not
   * one, and neither is an `Option` of a collection.
   */
  test("test_of_Iterable_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[Iterable[CurrencyAmount]])""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(List("AUD 101"))""")
    assertCompiles("""MultiCurrencyAmount.of(List(CA1, CA2))""")
  }

  /**
   * Asserts at compile time what the original asserted of a collection holding an absent element.
   *
   * This is the more interesting of the two `containsNull` methods, because the original was
   * asserting something about the '''contents''' of the collection rather than about the
   * collection itself. An `Iterable[CurrencyAmount]` cannot hold an absent element in any way this
   * API admits: a collection mixing amounts with anything else is not one of them, and a
   * collection of `Option`s is not either. A caller holding possibly-absent amounts has to resolve
   * the absence before the call - which the final proof asserts is the way that is open to it - so
   * the check the original performed has nothing left to check.
   */
  test("test_of_Iterable_containsNull") {
    assertDoesNotCompile(
      """MultiCurrencyAmount.of(List(CA1, Option.empty[CurrencyAmount], CA2))""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(List(Option(CA1), Option(CA2)))""")
    assertCompiles("""MultiCurrencyAmount.of(List(Option(CA1), Option(CA2)).flatten)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_Map") {
    val map: Map[Currency, Double] = Map(CCY1 -> AMT1, CCY3 -> AMT3)
    assertMCA(unwrap(MultiCurrencyAmount.of(map)), CA1, CA3)

    // a map cannot name a currency twice, so the only thing left for this factory to decide is
    // whether each number is an amount. The original threw for one that is not; this reports it.
    val notNumbers: Map[Currency, Double] = Map(CCY1 -> Double.NaN)
    MultiCurrencyAmount.of(notNumbers) should beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts at compile time the absent-map rejection the original asserted at run time.
   *
   * The argument is a required `Map[Currency, Double]`: a map keyed by anything else is not one,
   * and neither is an `Option` of a map.
   */
  test("test_of_Map_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.of(Option.empty[Map[Currency, Double]])""")
    assertDoesNotCompile("""MultiCurrencyAmount.of(Map("AUD" -> AMT1))""")
    assertCompiles("""MultiCurrencyAmount.of(Map(CCY1 -> AMT1))""")
  }

  //-------------------------------------------------------------------------
  test("test_total_Iterable") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, CA3)
    assertMCA(MultiCurrencyAmount.total(iterable), CA1, CA3)
  }

  /**
   * Asserts that the totalling factory '''adds''' a currency named twice.
   *
   * This is the second half of the contrast the two factories of this type exist for. The input is
   * the input of `test_of_Iterable_duplicate` with a third amount added: run through `of` it is a
   * failure naming the repeated currency, and run through `total` it is the sum, which is what
   * aggregating a collection of cash flows needs. Both outcomes are asserted here so that the
   * contrast is visible in one place, and neither factory is a safe default for the other's use.
   */
  test("test_total_Iterable_duplicate") {
    val iterable: Iterable[CurrencyAmount] = List(CA1, amountOf(CCY1, AMT2), CA2)
    assertMCA(MultiCurrencyAmount.total(iterable), amountOf(CCY1, AMT1 + AMT2), CA2)
    MultiCurrencyAmount.total(iterable).getAmountOrZero(CCY1).amount shouldBe 204d

    // the same input refused by the other factory
    MultiCurrencyAmount.of(iterable) should beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts at compile time the absent-collection rejection the original asserted at run time.
   *
   * As for the collection-taking `of`: the argument is a required `Iterable[CurrencyAmount]` and
   * cannot be omitted.
   */
  test("test_total_Iterable_null") {
    assertDoesNotCompile("""MultiCurrencyAmount.total(Option.empty[Iterable[CurrencyAmount]])""")
    assertDoesNotCompile("""MultiCurrencyAmount.total()""")
    assertCompiles("""MultiCurrencyAmount.total(List(CA1, CA2))""")
  }

  /**
   * Asserts at compile time what the original asserted of a collection holding an absent element.
   *
   * The reasoning is that of `test_of_Iterable_containsNull`, applied to the totalling factory:
   * the element type is required, so a collection mixing amounts with anything else - or holding
   * `Option`s of them - is not a collection this factory accepts, and the absence has to be
   * resolved by the caller before the call.
   */
  test("test_total_Iterable_containsNull") {
    assertDoesNotCompile(
      """MultiCurrencyAmount.total(List(CA1, Option.empty[CurrencyAmount], CA2))""")
    assertDoesNotCompile("""MultiCurrencyAmount.total(List(Option(CA1), Option(CA2)))""")
    assertCompiles("""MultiCurrencyAmount.total(List(Option(CA1), Option(CA2)).flatten)""")
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the aggregation that replaced the original's `Collector`.
   *
   * The original collected a stream of amounts through `toMultiCurrencyAmount()`. This port has no
   * collector and needs none: [[MultiCurrencyAmount.total]] aggregates a collection, and
   * `Monoid[MultiCurrencyAmount].combineAll` aggregates a collection of whole values. Both are
   * asserted over the original's input, and both must produce the value the original expected -
   * the two amounts of `CCY1` added and the amount of `CCY2` carried across.
   */
  test("test_collector") {
    val amount: List[CurrencyAmount] =
      List(amountOf(CCY1, 100d), amountOf(CCY1, 150d), amountOf(CCY2, 100d))
    val expected: MultiCurrencyAmount = multiOf(amountOf(CCY1, 250d), amountOf(CCY2, 100d))

    MultiCurrencyAmount.total(amount) shouldBe expected
    Monoid[MultiCurrencyAmount].combineAll(amount.map(single => multiOf(single))) shouldBe expected
    assertMCA(MultiCurrencyAmount.total(amount), amountOf(CCY1, 250d), amountOf(CCY2, 100d))
  }

  /**
   * Asserts the order-independence that the original's parallel-stream test was really about.
   *
   * A parallel stream collects its input in an unspecified order, so what the original asserted by
   * running one was that the aggregation gives the same answer whatever order it happens in. There
   * is no parallel stream here, so the property is asserted directly: the amounts of the test
   * above, combined in three different orders and through both aggregation routes, give equal
   * values. It holds because the amounts of one currency are added into a map keyed by currency
   * rather than folded in arrival order, and because the amounts of this input are whole numbers,
   * which double addition reorders exactly - the general statement, that addition is associative
   * only to within a tolerance, is the law this suite deliberately leaves to `TypeclassLawsSpec`.
   */
  test("test_collector_parallel") {
    val amount: List[CurrencyAmount] =
      List(amountOf(CCY1, 100d), amountOf(CCY1, 150d), amountOf(CCY2, 100d))
    val reordered: List[CurrencyAmount] = List(amount(2), amount(0), amount(1))
    val reversed: List[CurrencyAmount] = amount.reverse
    val expected: MultiCurrencyAmount = multiOf(amountOf(CCY1, 250d), amountOf(CCY2, 100d))

    MultiCurrencyAmount.total(reordered) shouldBe expected
    MultiCurrencyAmount.total(reversed) shouldBe expected
    Monoid[MultiCurrencyAmount].combineAll(
      reordered.map(single => multiOf(single))) shouldBe expected
    Monoid[MultiCurrencyAmount].combineAll(
      reversed.map(single => multiOf(single))) shouldBe expected

    // and the aggregate does not depend on how the input was grouped either, which is the other
    // freedom a parallel stream takes: two partial totals combined equal one total of everything
    Monoid[MultiCurrencyAmount].combine(
      MultiCurrencyAmount.total(amount.take(1)),
      MultiCurrencyAmount.total(amount.drop(1))) shouldBe expected
  }

  /**
   * Asserts at compile time what the original asserted of a stream carrying an absent element.
   *
   * The original collected a list holding an absent amount. Neither aggregation route here admits
   * one: `total` requires amounts, and `combineAll` requires whole values of this type rather than
   * the amounts they hold.
   */
  test("test_collector_null") {
    assertDoesNotCompile(
      """MultiCurrencyAmount.total(List(amountOf(CCY1, 100d), Option.empty[CurrencyAmount]))""")
    assertDoesNotCompile("""Monoid[MultiCurrencyAmount].combineAll(List(CA1, CA2))""")
    assertCompiles(
      """Monoid[MultiCurrencyAmount].combineAll(List(MTA, MultiCurrencyAmount.empty))""")
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the construction that replaced the original's bean builder.
   *
   * The original set the single property of the meta-bean - the sorted set of amounts - and built.
   * There is no meta-bean and no builder in this port: a value is built by a factory, and the
   * three routes into it must agree, which is what this test asserts. The value built is the value
   * the original's builder produced, and the sorted set it set is now the order the value holds its
   * amounts in whichever route built it.
   */
  test("test_beanBuilder") {
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)
    assertMCA(test, CA1, CA2, CA3)

    // the collection-taking and map-taking factories build the same value, in any input order
    unwrap(MultiCurrencyAmount.of(List(CA3, CA1, CA2))) shouldBe test
    unwrap(
      MultiCurrencyAmount.of(Map(CCY3 -> AMT3, CCY1 -> AMT1, CCY2 -> AMT2))) shouldBe test
    MultiCurrencyAmount.total(List(CA2, CA3, CA1)) shouldBe test
  }

  /**
   * Asserts that the input the original's builder rejected is rejected here.
   *
   * The original built a sorted set holding two amounts of `CCY1` and asserted that `build` threw.
   * The equivalent input to the factory is a collection naming that currency twice, and the
   * outcome is the failure the two duplicate tests above assert, reported as a value.
   */
  test("test_beanBuilder_invalid") {
    val invalid: FailureOr[MultiCurrencyAmount] =
      MultiCurrencyAmount.of(List(CA1, CA2, amountOf(CA1.currency, AMT3)))
    invalid should beFailureWith(FailureReason.INVALID)
    invalid should haveFailureMessageMatching(Regex.quote("Currency is duplicated: AUD"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts at compile time the absent-currency rejection the original asserted at run time.
   *
   * The currency asked about is a required [[Currency]], so there is no absent value to reject and
   * nothing for a run-time check to do.
   */
  test("test_contains_null") {
    assertDoesNotCompile("""MTA.contains("AUD")""")
    assertDoesNotCompile("""MTA.contains(Option.empty[Currency])""")
    assertDoesNotCompile("""MTA.contains()""")
    assertCompiles("""MTA.contains(CCY1)""")
  }

  //-------------------------------------------------------------------------
  test("test_plus_CurrencyDouble_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(AUD, 3d)
    assertMCA(test, cb, amountOf(AUD, 120d))
  }

  test("test_plus_CurrencyDouble_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(NZD, 3d)
    assertMCA(test, ca, cb, amountOf(NZD, 3d))
  }

  /**
   * Asserts at compile time the absent-currency rejection the original asserted at run time.
   *
   * The original cast an absent reference to `Currency` to pick this overload. The currency here
   * is required, and neither the text of a code nor an `Option` stands in for it.
   */
  test("test_plus_CurrencyDouble_null") {
    assertDoesNotCompile("""MTA.plus("AUD", 1d)""")
    assertDoesNotCompile("""MTA.plus(Option.empty[Currency], 1d)""")
    assertCompiles("""MTA.plus(CCY1, 1d)""")
  }

  //-------------------------------------------------------------------------
  test("test_plus_CurrencyAmount_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(cc)
    assertMCA(test, cb, amountOf(AUD, 120d))
  }

  test("test_plus_CurrencyAmount_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.plus(cc)
    assertMCA(test, ca, cb, cc)
  }

  /**
   * Asserts at compile time the absent-amount rejection the original asserted at run time.
   *
   * The amount is a required [[CurrencyAmount]]: a currency alone is not one, and neither is an
   * `Option` of an amount.
   */
  test("test_plus_CurrencyAmount_null") {
    assertDoesNotCompile("""MTA.plus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""MTA.plus(CCY1)""")
    assertCompiles("""MTA.plus(CA1)""")
  }

  //-------------------------------------------------------------------------
  test("test_plus_MultiCurrencyAmount_mergeAndAdd") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val cd: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf(cc, cd)
    val test: MultiCurrencyAmount = mc1.plus(mc2)
    assertMCA(test, cb, cd, amountOf(AUD, 120d))
  }

  test("test_plus_MultiCurrencyAmount_empty") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf()
    val test: MultiCurrencyAmount = mc1.plus(mc2)
    assertMCA(test, ca, cb)
  }

  /**
   * Asserts at compile time the absent-value rejection the original asserted at run time.
   *
   * The value added is a required [[MultiCurrencyAmount]] and the argument cannot be omitted.
   */
  test("test_plus_MultiCurrencyAmount_null") {
    assertDoesNotCompile("""MTA.plus(Option.empty[MultiCurrencyAmount])""")
    assertDoesNotCompile("""MTA.plus()""")
    assertCompiles("""MTA.plus(MultiCurrencyAmount.empty)""")
  }

  //-------------------------------------------------------------------------
  test("test_minus_CurrencyDouble_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(AUD, 3d)
    assertMCA(test, cb, amountOf(AUD, 114d))
  }

  test("test_minus_CurrencyDouble_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(NZD, 3d)
    // a currency the value does not hold is inserted negated, which is what keeps subtraction the
    // exact inverse of addition
    assertMCA(test, ca, cb, amountOf(NZD, -3d))
  }

  /**
   * Asserts at compile time the absent-currency rejection the original asserted at run time.
   *
   * The reasoning is that of `test_plus_CurrencyDouble_null`, on the subtracting overload.
   */
  test("test_minus_CurrencyDouble_null") {
    assertDoesNotCompile("""MTA.minus("AUD", 1d)""")
    assertDoesNotCompile("""MTA.minus(Option.empty[Currency], 1d)""")
    assertCompiles("""MTA.minus(CCY1, 1d)""")
  }

  //-------------------------------------------------------------------------
  test("test_minus_CurrencyAmount_merge") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(cc)
    assertMCA(test, cb, amountOf(AUD, 114d))
  }

  test("test_minus_CurrencyAmount_add") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val test: MultiCurrencyAmount = mc1.minus(cc)
    assertMCA(test, ca, cb, cc.negated)
  }

  /**
   * Asserts at compile time the absent-amount rejection the original asserted at run time.
   *
   * The reasoning is that of `test_plus_CurrencyAmount_null`, on the subtracting overload.
   */
  test("test_minus_CurrencyAmount_null") {
    assertDoesNotCompile("""MTA.minus(Option.empty[CurrencyAmount])""")
    assertDoesNotCompile("""MTA.minus(CCY1)""")
    assertCompiles("""MTA.minus(CA1)""")
  }

  //-------------------------------------------------------------------------
  test("test_minus_MultiCurrencyAmount_mergeAndAdd") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val cc: CurrencyAmount = amountOf(AUD, 3d)
    val cd: CurrencyAmount = amountOf(NZD, 3d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf(cc, cd)
    val test: MultiCurrencyAmount = mc1.minus(mc2)
    assertMCA(test, cb, cd.negated, amountOf(AUD, 114d))
  }

  test("test_minus_MultiCurrencyAmount_empty") {
    val ca: CurrencyAmount = amountOf(AUD, 117d)
    val cb: CurrencyAmount = amountOf(USD, 12d)
    val mc1: MultiCurrencyAmount = multiOf(ca, cb)
    val mc2: MultiCurrencyAmount = multiOf()
    val test: MultiCurrencyAmount = mc1.minus(mc2)
    assertMCA(test, ca, cb)
  }

  /**
   * Asserts at compile time the absent-value rejection the original asserted at run time.
   *
   * The reasoning is that of `test_plus_MultiCurrencyAmount_null`, on the subtracting overload.
   */
  test("test_minus_MultiCurrencyAmount_null") {
    assertDoesNotCompile("""MTA.minus(Option.empty[MultiCurrencyAmount])""")
    assertDoesNotCompile("""MTA.minus()""")
    assertCompiles("""MTA.minus(MultiCurrencyAmount.empty)""")
  }

  //-------------------------------------------------------------------------
  test("test_multipliedBy") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.multipliedBy(2.5d)
    assertMCA(test, CA1.multipliedBy(2.5d), CA2.multipliedBy(2.5d))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts negation, including the two zero cases the original asserted.
   *
   * An amount of zero negates to zero rather than to a negative zero, so a value holding only
   * zeros negates to itself. The original stated that with an equality assertion, which cannot
   * distinguish the two zeros - `-0.0 == 0.0` holds - so the sign is asserted here by bit pattern
   * as well, which is what the statement was actually about.
   */
  test("test_negated") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.negated
    assertMCA(test, CA1.negated, CA2.negated)

    val zeros: MultiCurrencyAmount = multiOf(CurrencyAmount.zero(USD), CurrencyAmount.zero(EUR))
    zeros.negated shouldBe zeros
    multiOf(amountOf(USD, -0d), amountOf(EUR, -0d)).negated shouldBe zeros

    // the sign of every zero the negation produced, by bit pattern rather than by equality
    zeros.negated.toMap.valuesIterator.foreach { amount =>
      withClue(s"$amount: ") {
        java.lang.Double.doubleToLongBits(amount) shouldBe java.lang.Double.doubleToLongBits(0d)
      }
    }
    java.lang.Double.doubleToLongBits(
      multiOf(amountOf(USD, -0d)).negated.getAmountOrZero(USD).amount) shouldBe
      java.lang.Double.doubleToLongBits(0d)
  }

  //-------------------------------------------------------------------------
  test("test_mapAmounts") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.mapAmounts(amount => amount * 2.5d + 1d)
    assertMCA(
      test,
      CA1.mapAmount(amount => amount * 2.5d + 1d),
      CA2.mapAmount(amount => amount * 2.5d + 1d))
  }

  /**
   * Asserts at compile time the absent-operation rejection the original asserted at run time.
   *
   * The operation is a required function from one amount to another: it cannot be omitted, and a
   * function that does not answer with a number is not one of them.
   */
  test("test_mapAmounts_null") {
    assertDoesNotCompile("""MTA.mapAmounts()""")
    assertDoesNotCompile("""MTA.mapAmounts((amount: Double) => amount.toString)""")
    assertCompiles("""MTA.mapAmounts((amount: Double) => amount * 2d)""")
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the mapping that may change a currency, and therefore merges.
   *
   * The original mapped both amounts onto one currency and expected their sum, which is the whole
   * difference between this member and `mapAmounts`: two amounts mapped onto the same currency are
   * added rather than reported as a duplicate.
   */
  test("test_mapCurrencyAmounts") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount = base.mapCurrencyAmounts(_ => amountOf(CCY3, 1d))
    assertMCA(test, amountOf(CCY3, 2d))
  }

  /**
   * Asserts at compile time the absent-operation rejection the original asserted at run time.
   *
   * The reasoning is that of `test_mapAmounts_null`: the operation is required, and it has to
   * answer with an amount rather than with the number one holds.
   */
  test("test_mapCurrencyAmounts_null") {
    assertDoesNotCompile("""MTA.mapCurrencyAmounts()""")
    assertDoesNotCompile("""MTA.mapCurrencyAmounts((amount: CurrencyAmount) => amount.amount)""")
    assertCompiles("""MTA.mapCurrencyAmounts((amount: CurrencyAmount) => amount.negated)""")
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the traversal that replaced the original's stream, and the order it runs in.
   *
   * The original mapped its stream and collected the result; here the amounts are walked with
   * [[MultiCurrencyAmount.iterator]] and totalled, which is the same computation. The second half
   * of the test is the more important one: the amounts always arrive in the alphabetical order of
   * their currency codes, whatever order the value was built in. This is the only place that order
   * is directly observable, and the byte-stable serialized form of this type rests on it.
   */
  test("test_stream") {
    val base: MultiCurrencyAmount = multiOf(CA1, CA2)
    val test: MultiCurrencyAmount =
      MultiCurrencyAmount.total(base.iterator.map(ca => ca.mapAmount(amount => amount * 3d)).toList)
    assertMCA(
      test,
      CA1.mapAmount(amount => amount * 3d),
      CA2.mapAmount(amount => amount * 3d))

    base.iterator.toList shouldBe List(CA1, CA2)
    multiOf(CA2, CA1).iterator.toList shouldBe List(CA1, CA2)
    multiOf(CA3, CA2, CA1).iterator.toList shouldBe List(CA1, CA2, CA3)
    multiOf(CA3, CA2, CA1).getAmounts.toList shouldBe List(CA1, CA2, CA3)
    MultiCurrencyAmount.empty.iterator.toList shouldBe List.empty[CurrencyAmount]
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the map view of the amounts, and that it is ordered.
   *
   * The original asserted the size, the keys and the two values. The map is a
   * `SortedMap[Currency, Double]` here - the representation the type holds, handed back as it is -
   * so its key order is asserted as well, for a value built in the reverse of it.
   */
  test("test_toMap") {
    val test: SortedMap[Currency, Double] = multiOf(CA1, CA2).toMap
    test.size shouldBe 2
    test.contains(CA1.currency) shouldBe true
    test.contains(CA2.currency) shouldBe true
    test.get(CA1.currency) shouldBe Some(CA1.amount)
    test.get(CA2.currency) shouldBe Some(CA2.amount)
    test.keysIterator.toList shouldBe List(CCY1, CCY2)

    multiOf(CA3, CA2, CA1).toMap.keysIterator.toList shouldBe List(CCY1, CCY2, CCY3)
    multiOf(CA3, CA2, CA1).toMap.valuesIterator.toList shouldBe List(AMT1, AMT2, AMT3)
    MultiCurrencyAmount.empty.toMap.isEmpty shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts that a value of one amount already in the requested currency needs no rate.
   *
   * The original supplied a provider whose every lookup threw, and asserted that the conversion
   * succeeded anyway. Here the provider reports rather than throws, and the assertion is the same:
   * a value holding exactly one amount converts that amount through
   * [[CurrencyAmount.convertedTo]], which returns an amount already in the requested currency
   * without consulting the provider at all. `FxRateProvider.noConversion()` - the provider of this
   * port that refuses every lookup, the identity included - is asserted alongside the hand-built
   * one, because it is the provider a caller forbidden from converting would actually pass.
   */
  test("test_convertedTo_rateProvider_noConversionSize1") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((base, counter) =>
      Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter")))
    val test: MultiCurrencyAmount = multiOf(CA2)
    test.convertedTo(CCY2, provider) should haveValue(CA2)
    test.convertedTo(CCY2, FxRateProvider.noConversion()) should haveValue(CA2)
  }

  /**
   * Asserts the conversion of a value holding one amount in another currency.
   *
   * The rate is the original's, and so is the expected product. The second half asserts what the
   * original's throwing provider left unstated: a rate the provider does not hold fails the
   * conversion as a value, carrying the reason the provider reported.
   */
  test("test_convertedTo_rateProvider_conversionSize1") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((base, counter) =>
      if (base == CCY1 && counter == CCY2) {
        Right(2.5d)
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount = multiOf(CA1)
    test.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, AMT1 * 2.5d))
    test.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, 252.5d))

    // a rate the provider cannot supply is reported rather than thrown
    test.convertedTo(CCY3, provider) should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    test.convertedTo(CCY3, provider) should haveFailureMessageMatching(
      Regex.quote("No rate is available for AUD/CHF"))
  }

  /**
   * Asserts the conversion of a value holding two amounts, one of them already in the target.
   *
   * The original's provider answered with one for a currency against itself, and it had to: a
   * value holding more than one amount asks the provider for a rate for '''every''' amount it
   * holds, the one already in the requested currency included. That is why the provider built here
   * carries the identity case, and why the last assertion - a provider that refuses the identity
   * failing this conversion - is the other half of the same statement.
   *
   * The total is `CAD 103` plus `AUD 101` at `2.5`, summed in the alphabetical order of the
   * currency codes from zero, which is the order the implementation being ported summed in. Both
   * the exact expected number and the original's way of writing it are asserted.
   */
  test("test_convertedTo_rateProvider_conversionSize2") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((base, counter) =>
      if (base == counter) {
        Right(1d)
      } else if (base == CCY1 && counter == CCY2) {
        Right(2.5d)
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount = multiOf(CA1, CA2)
    test.convertedTo(CCY2, provider) should
      haveValue(unwrap(CA2.plus(amountOf(CCY2, AMT1 * 2.5d))))
    test.convertedTo(CCY2, provider) should haveValue(amountOf(CCY2, 355.5d))

    // the identity rate is asked for, so a provider that refuses it fails this conversion, where
    // it succeeded for the single-amount value above
    test.convertedTo(CCY2, FxRateProvider.noConversion()) should
      beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // and a value holding nothing takes the same branch, converting to zero of the target without
    // any rate being needed
    MultiCurrencyAmount.empty.convertedTo(CCY2, FxRateProvider.noConversion()) should
      haveValue(CurrencyAmount.zero(CCY2))
  }

  //-----------------------------------------------------------------------
  /**
   * Asserts the serialized form that replaced the original's Java serialization round trip.
   *
   * The migration manifest records the original's `test_serialization` as consolidated into
   * `json.JsonRoundTripSpec`, which owns the property-based round trip of every codec of the
   * module and the property that equal values encode to identical bytes. This test is the one
   * concrete example of the shape at the type: an object of a single field holding the amounts,
   * each written as the codec of [[CurrencyAmount]] writes it, in currency order.
   *
   * The order is the point of the second assertion. It is a property of the value rather than a
   * convention of the encoder, so a value built in a different order encodes to the same bytes -
   * and a document naming one currency twice is refused, because the decoder routes through
   * [[MultiCurrencyAmount.of]] rather than letting the last amount silently win.
   */
  test("test_serialization") {
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe
      """{"amounts":[{"currency":"AUD","amount":101.0},{"currency":"CAD","amount":103.0},""" +
        """{"currency":"CHF","amount":107.0}]}"""
    encoded.as[MultiCurrencyAmount] shouldBe Right(test)

    // equal values encode to identical bytes however they were built, and a document may list the
    // amounts in any order
    multiOf(CA3, CA1, CA2).asJson.noSpaces shouldBe encoded.noSpaces
    Json
      .obj("amounts" -> Json.arr(CA3.asJson, CA2.asJson, CA1.asJson))
      .as[MultiCurrencyAmount] shouldBe Right(test)

    // the value holding nothing, and a document naming one currency twice
    MultiCurrencyAmount.empty.asJson.noSpaces shouldBe """{"amounts":[]}"""
    Json.obj("amounts" -> Json.arr(CA1.asJson, CA1.asJson)).as[MultiCurrencyAmount].isLeft shouldBe
      true
  }

  /**
   * Asserts that the value behaves as a value, which is what the original's bean sweep stood for.
   *
   * The original walked the meta-bean reflectively. There is none here, so the properties that
   * sweep was standing in for are asserted directly through the instances the companion publishes:
   * two values built from the same amounts in different orders are equal and hash alike, a value
   * holding different amounts is not equal to them, a value of another type is not equal at all,
   * and the rendering instance renders what `toString` renders.
   *
   * Inequality is stated through `eqv` rather than through the hashes: the instance guarantees
   * that equal values hash equally and says nothing whatever about unequal ones, so asserting that
   * two distinct values hash differently would make a correct hash fail here.
   */
  test("coverage") {
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)
    val same: MultiCurrencyAmount = multiOf(CA3, CA2, CA1)
    val other: MultiCurrencyAmount = multiOf(CA1, CA2)

    Hash[MultiCurrencyAmount].eqv(test, test) shouldBe true
    Hash[MultiCurrencyAmount].eqv(test, same) shouldBe true
    Hash[MultiCurrencyAmount].eqv(test, other) shouldBe false
    Hash[MultiCurrencyAmount].eqv(other, MultiCurrencyAmount.empty) shouldBe false
    Hash[MultiCurrencyAmount].hash(test) shouldBe Hash[MultiCurrencyAmount].hash(same)
    test.equals(ANOTHER_TYPE) shouldBe false

    // the rendering instance renders the text form, which is the form documents, log lines and
    // expectations written before the port carry
    Show[MultiCurrencyAmount].show(test) shouldBe test.toString
    Show[MultiCurrencyAmount].show(test) shouldBe "[AUD 101, CAD 103, CHF 107]"
    Show[MultiCurrencyAmount].show(MultiCurrencyAmount.empty) shouldBe "[]"
    Show[MultiCurrencyAmount].show(
      multiOf(amountOf(GBP, 100d), amountOf(USD, 200d))) shouldBe "[GBP 100, USD 200]"
  }

  //-------------------------------------------------------------------------
  // The tests below have no method of the original behind them. They pin the numbers and the
  // traversals of the aggregation and the conversion, which the original's own tests left implicit
  // because they were properties of a collector and of a stream rather than of an API. Each is
  // named for what it pins rather than for a Java method, so the one-to-one correspondence of the
  // fifty-four tests above with the migration manifest is unaffected.
  //-------------------------------------------------------------------------
  /**
   * Pins the order the aggregate adds a repeated currency in, to the last bit.
   *
   * Every route that merges - [[MultiCurrencyAmount.total]], the three
   * [[MultiCurrencyAmount.plus]] members, [[MultiCurrencyAmount.mapCurrencyAmounts]] and
   * `Monoid[MultiCurrencyAmount].combineAll` - adds what arrives to what has accumulated, in the
   * order the amounts arrive. The three numbers here are chosen so that the two associations of
   * the same sum '''differ in their last bit''': added left to right they give
   * `0.6000000000000001` and added right to left `0.6`. An implementation that reordered the
   * additions, or that seeded the sum with a zero instead of the first amount, would produce the
   * other number and fail here, which is what makes this a pin on the arithmetic rather than a
   * restatement of it.
   *
   * The association is also the one the captured parity baseline of this port was recorded under,
   * so a change of it would move that baseline as well as this test.
   */
  test("the aggregate adds a repeated currency left to right, to the last bit") {
    val first: CurrencyAmount = amountOf(CCY1, 0.1d)
    val second: CurrencyAmount = amountOf(CCY1, 0.2d)
    val third: CurrencyAmount = amountOf(CCY1, 0.3d)

    // the two associations of the same three numbers, which differ in their last bit
    val leftToRight: Double = 0.6000000000000001d
    (0.1d + 0.2d) + 0.3d shouldBe leftToRight
    0.1d + (0.2d + 0.3d) shouldBe 0.6d
    leftToRight should not be 0.6d

    // total, and the two single-amount forms of plus
    bitsOf(MultiCurrencyAmount.total(List(first, second, third)), CCY1) shouldBe bits(leftToRight)
    bitsOf(multiOf(first).plus(CCY1, 0.2d).plus(CCY1, 0.3d), CCY1) shouldBe bits(leftToRight)
    bitsOf(multiOf(first).plus(second).plus(third), CCY1) shouldBe bits(leftToRight)

    // the whole-value form of plus, and the aggregate over whole values
    bitsOf(multiOf(first).plus(multiOf(second)).plus(multiOf(third)), CCY1) shouldBe
      bits(leftToRight)
    bitsOf(
      Monoid[MultiCurrencyAmount].combineAll(
        List(multiOf(first), multiOf(second), multiOf(third))),
      CCY1) shouldBe bits(leftToRight)

    // and the mapping that moves three currencies onto one, which merges in currency-code order
    val spread: MultiCurrencyAmount =
      multiOf(amountOf(CCY1, 0.1d), amountOf(CCY2, 0.2d), amountOf(CCY3, 0.3d))
    bitsOf(spread.mapCurrencyAmounts(amount => amountOf(USD, amount.amount)), USD) shouldBe
      bits(leftToRight)
  }

  /**
   * Pins that aggregating in one pass agrees with combining the same values pairwise.
   *
   * `combineAll` aggregates a collection of whole values in a single pass over their amounts,
   * while `combine` adds two values at a time; both are published, so the two must not be able to
   * disagree. The values here overlap in two currencies and hold numbers whose sums are not exact
   * in binary, so an aggregate that added them in a different order or through a different number
   * of intermediate roundings would show up as a differing bit pattern rather than as an
   * approximate mismatch. The comparison is made entry by entry on the bits, and on the whole
   * values, which compare their amounts by bit pattern too.
   */
  test("combineAll agrees with folding combine over the same values, entry by entry") {
    val additive: Monoid[MultiCurrencyAmount] = Monoid[MultiCurrencyAmount]
    val values: List[MultiCurrencyAmount] = List(
      multiOf(amountOf(CCY1, 0.1d), amountOf(CCY2, 1.1d)),
      multiOf(amountOf(CCY1, 0.2d), amountOf(CCY3, 2.2d)),
      multiOf(amountOf(CCY2, 0.3d), amountOf(CCY3, 3.3d)),
      multiOf(amountOf(CCY1, 0.4d)))

    val aggregated: MultiCurrencyAmount = additive.combineAll(values)
    val folded: MultiCurrencyAmount = values.foldLeft(additive.empty)(additive.combine)

    aggregated.toMap.keysIterator.toList shouldBe folded.toMap.keysIterator.toList
    folded.toMap.foreach { case (currency, amount) =>
      withClue(s"$currency: ") {
        bitsOf(aggregated, currency) shouldBe bits(amount)
      }
    }
    aggregated shouldBe folded
  }

  /**
   * Pins that the aggregate still applies the numeric invariant of an amount.
   *
   * The aggregation adds numbers rather than amounts, so the one check every amount of this
   * library passes has to be reached on that path too. Two infinities of opposite sign added
   * together produce a value that is not a number, which no amount may hold, and every merging
   * route raises the invariant of [[CurrencyAmount]] with the wording that type reports. A
   * negative zero, the other half of the same invariant, is normalised away by every route that
   * takes a number from outside.
   *
   * This is the one test of this suite that asserts a throw, and it is here rather than only in
   * the root `SmartConstructorSpec` because what it pins is the aggregation path of this type:
   * the general numeric edges of every type of the port stay in that suite.
   */
  test("the aggregate keeps the numeric invariant of an amount") {
    val positive: CurrencyAmount = amountOf(CCY1, Double.PositiveInfinity)
    val negative: CurrencyAmount = amountOf(CCY1, Double.NegativeInfinity)
    val notNumber: String = "Argument 'amount' must not be NaN"

    the[IllegalArgumentException] thrownBy MultiCurrencyAmount.total(
      List(positive, negative)) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive).plus(
      CCY1,
      Double.NegativeInfinity) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive).plus(
      negative) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive).plus(
      multiOf(negative)) should have message notNumber
    the[IllegalArgumentException] thrownBy Monoid[MultiCurrencyAmount].combineAll(
      List(multiOf(positive), multiOf(negative))) should have message notNumber
    the[IllegalArgumentException] thrownBy multiOf(positive, amountOf(CCY2, 1d)).mapAmounts(
      amount => amount - amount) should have message notNumber

    // an infinity on its own is an amount, so the invariant fires on the sum and not before
    bitsOf(MultiCurrencyAmount.total(List(positive, positive)), CCY1) shouldBe
      bits(Double.PositiveInfinity)

    // and the other half of the invariant: a negative zero arriving from outside is normalised
    bitsOf(unwrap(MultiCurrencyAmount.of(CCY1, -0d)), CCY1) shouldBe bits(0d)
    bitsOf(multiOf(CA1).mapAmounts(_ => -0d), CCY1) shouldBe bits(0d)
    bitsOf(unwrap(MultiCurrencyAmount.of(Map(CCY1 -> -0d))), CCY1) shouldBe bits(0d)
  }

  //-------------------------------------------------------------------------
  /**
   * Pins that a conversion asks for no rate after the first one it cannot get.
   *
   * A conversion of a value holding several amounts needs a rate for every one of them, so a
   * single unavailable rate decides the outcome and nothing after it can change it - a partial
   * total would be a number with no meaning. The provider here counts its lookups and refuses the
   * second currency in code order, so the count states exactly how far the traversal ran: two
   * lookups for three amounts. The failure returned is the one the provider reported for that
   * currency, not for a later one.
   */
  test("convertedTo asks for no rate after the first one the provider cannot supply") {
    val provider: CountingRateProvider = new CountingRateProvider((base, counter) =>
      if (base == CCY1 && counter == USD) {
        Right(2.5d)
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount = multiOf(CA1, CA2, CA3)

    val converted: FailureOr[CurrencyAmount] = test.convertedTo(USD, provider)
    converted should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    converted should haveFailureMessageMatching(Regex.quote("No rate is available for CAD/USD"))
    provider.callCount shouldBe 2
  }

  /**
   * Pins the order a conversion totals in, to the last bit, and that it asks once per amount.
   *
   * The converted amounts are added from zero in the alphabetical order of their currency codes.
   * The rates here are chosen so that the sum in that order, `0.6000000000000001`, differs in its
   * last bit from the sum in the reverse order, `0.6`, so a traversal that visited the amounts in
   * any other order would fail here. The count pins the other half of the contract: every amount
   * the value holds costs exactly one lookup, none is asked for twice, and the identity is asked
   * for like any other - which is why a value of more than one amount needs a provider that
   * answers for a currency against itself.
   */
  test("convertedTo totals the converted amounts in currency-code order, to the last bit") {
    val rates: Map[Currency, Double] = Map(CCY1 -> 0.1d, CCY2 -> 0.2d, CCY3 -> 0.3d)
    val provider: CountingRateProvider = new CountingRateProvider((base, counter) =>
      if (counter == USD) {
        rates.get(base).toRight(Failure.CurrencyConversion(s"No rate is available for $base/USD"))
      } else {
        Left(Failure.CurrencyConversion(s"No rate is available for $base/$counter"))
      })
    val test: MultiCurrencyAmount =
      multiOf(amountOf(CCY1, 1d), amountOf(CCY2, 1d), amountOf(CCY3, 1d))

    // the two orders of the same three terms, which differ in their last bit
    ((0d + 0.1d) + 0.2d) + 0.3d shouldBe 0.6000000000000001d
    ((0d + 0.3d) + 0.2d) + 0.1d shouldBe 0.6d

    val converted: CurrencyAmount = unwrap(test.convertedTo(USD, provider))
    converted.currency shouldBe USD
    bits(converted.amount) shouldBe bits(0.6000000000000001d)
    provider.callCount shouldBe 3
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts of a value everything the original's private helper of this name asserted of one.
   *
   * The helper is the reason so many of the fifty-four tests are a single line: it states the
   * whole of what a value means - its size, the amounts it holds, the currencies it holds them in,
   * and what it answers for a currency it does not hold - so each test only has to name the
   * amounts it expects.
   *
   * Two of its assertions changed shape with the port and are the ones worth reading. The amount
   * of a currency the value does not hold was an `IllegalArgumentException` and is a `Left` here,
   * asserted by reason and by the wording the original threw with; and the currency it is asked
   * for is [[NON_EXISTING]], a configured currency none of these tests holds, because the code the
   * original minted on the spot has no counterpart in the closed currency family of this port.
   *
   * @param actual  the value to assert over
   * @param expected  the amounts the value is expected to hold, in any order
   * @return the assertion that the value holds exactly those amounts and nothing else
   */
  private def assertMCA(actual: MultiCurrencyAmount, expected: CurrencyAmount*): Assertion = {
    actual.size shouldBe expected.size
    actual.getAmounts.size shouldBe expected.size
    actual.getAmounts.toList should contain theSameElementsAs expected
    actual.iterator.toList should contain theSameElementsAs expected

    expected.foreach { expectedAmount =>
      withClue(s"$expectedAmount: ") {
        actual.contains(expectedAmount.currency) shouldBe true
        actual.getAmount(expectedAmount.currency) should haveValue(expectedAmount)
        actual.getAmountOrZero(expectedAmount.currency) shouldBe expectedAmount
      }
    }
    actual.getCurrencies shouldBe expected.map(amount => amount.currency).toSet

    // and the currency the value does not hold: absent, reported by `getAmount`, zero by
    // `getAmountOrZero` - the distinction the original drew between those two members
    actual.contains(NON_EXISTING) shouldBe false
    actual.getAmount(NON_EXISTING) should beFailureWith(FailureReason.INVALID)
    actual.getAmount(NON_EXISTING) should haveFailureMessageMatching(
      Regex.quote(s"Unknown currency $NON_EXISTING"))
    actual.getAmountOrZero(NON_EXISTING) shouldBe CurrencyAmount.zero(NON_EXISTING)
  }

  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite, and it exists because every factory of both
   * types this suite builds reports what was wrong with its arguments as a value. The outcome is
   * folded rather than unwrapped by a partial accessor, so a fixture that fails to build is
   * reported as a test failure naming the reason instead of raising an error from somewhere else
   * in the suite.
   *
   * @param outcome  the outcome expected to carry a value
   * @tparam A  the type of the value
   * @return the value it carries
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the factory failed with: ${failure.message}"),
      value => value)

  /**
   * Builds a value from the specified amounts, failing the test if they describe none.
   *
   * The amounts have to be of distinct currencies, since this is the refusing factory; a test that
   * means to aggregate a repeated currency calls [[MultiCurrencyAmount.total]] directly.
   *
   * @param amounts  the amounts, each of a different currency
   * @return the value holding those amounts
   */
  private def multiOf(amounts: CurrencyAmount*): MultiCurrencyAmount =
    unwrap(MultiCurrencyAmount.of(amounts: _*))

  /**
   * Builds an amount, failing the test if the currency and number describe none.
   *
   * @param currency  the currency of the amount
   * @param amount  the number of that currency, expected to be one an amount may hold
   * @return the amount
   */
  private def amountOf(currency: Currency, amount: Double): CurrencyAmount =
    unwrap(CurrencyAmount.of(currency, amount))

  /**
   * Returns the bit pattern of the amount a value holds of the specified currency.
   *
   * The pins added by this port compare amounts by their bit pattern rather than by `==`, because
   * what they are asserting is the association of a floating point sum: two numbers that differ in
   * their last bit are the two outcomes such a pin distinguishes, and `shouldBe` on a `Double`
   * would report them as "0.6 was not equal to 0.6". A currency the value does not hold fails the
   * test naming it, rather than being read as a zero that would make the comparison meaningless.
   *
   * @param value  the value to read the amount from
   * @param currency  the currency whose amount is wanted
   * @return the bit pattern of the amount held of that currency
   */
  private def bitsOf(value: MultiCurrencyAmount, currency: Currency): Long =
    bits(
      value.toMap.getOrElse(
        currency,
        fail(s"Expected an amount of $currency but the value held $value")))

  /**
   * Returns the bit pattern of a number.
   *
   * @param amount  the number to read the bit pattern of
   * @return the bit pattern of that number
   */
  private def bits(amount: Double): Long = java.lang.Double.doubleToLongBits(amount)

  /**
   * A rate provider that answers with a function and counts how often it was asked.
   *
   * This is what the conversion pins observe. A conversion of a value holding several amounts
   * needs one rate per amount, and the two things worth asserting about the traversal - that it
   * stops at the first rate it cannot get, and that it asks exactly once for each amount - are
   * both statements about the number of lookups rather than about the result, so the provider has
   * to be able to report that number.
   *
   * The counter is an `AtomicInteger`, as it is in `LazyFxRateProviderSpec`: it keeps the suite
   * free of mutable fields and of mutable collections, as the rest of this port is, and it counts
   * correctly however the provider is called. A fresh instance is built by each test, so no count
   * depends on which test ran first.
   *
   * @param rates  the function that answers a pair, or reports that no rate is available for it
   */
  private final class CountingRateProvider(rates: (Currency, Currency) => Either[Failure, Double])
      extends FxRateProvider {

    /** The number of lookups this provider has been asked for. */
    private val calls: AtomicInteger = new AtomicInteger(0)

    /**
     * Answers the pair with the function this provider holds, counting the lookup.
     *
     * @param baseCurrency  the base currency, to convert from
     * @param counterCurrency  the counter currency, to convert to
     * @return whatever the function answers for the pair
     */
    override def fxRate(
        baseCurrency: Currency,
        counterCurrency: Currency): Either[Failure, Double] = {
      // Bound to a wildcard because the new count is of no interest here; the tests read it
      // through callCount.
      val _ = calls.incrementAndGet()
      rates(baseCurrency, counterCurrency)
    }

    /**
     * Returns the number of lookups this provider has been asked for.
     *
     * @return the lookup count, zero if it has never been asked
     */
    def callCount: Int = calls.get()
  }
}
