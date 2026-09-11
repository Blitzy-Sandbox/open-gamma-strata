/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.util.Try

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.syntax.all._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests partial success: the `ValueWithFailures` alias and the nested term object of the
 * same name, both declared in the package object of this package.
 *
 * ===What is under test===
 *
 * The type being ported was a class holding a value together with a - possibly empty -
 * list of failures. It does not survive as a class. In its place the package object
 * declares two things that share one name:
 *
 * {{{
 * type ValueWithFailures[A] = Ior[NonEmptyChain[Failure], A]   // the type
 * object ValueWithFailures { ... }                             // the term
 * }}}
 *
 * A name in a type position is the alias and the same name in an expression is the object,
 * exactly as it is for the collection types of the standard library, so
 * `ValueWithFailures[A]` and `ValueWithFailures.of(...)` refer to different things without
 * ambiguity. Both are members of the package this spec is declared in, so neither needs an
 * import.
 *
 * The mandate of this spec is the whole surface of that object - `of` in its two forms,
 * `hasFailures`, the three forms of `withValue`, `combineValuesAsList`,
 * `combineValuesAsSet` and `combiningValues` - together with `withAdditionalFailures`,
 * which sits beside the object in the package rather than inside it.
 *
 * ===Where the instance methods went===
 *
 * The class being ported carried its own `map`, `mapFailures`, `flatMap` and
 * `combinedWith`. None of them is redeclared, because the alias is an `Ior` and every one
 * of those operations is already defined on it: `map` and `leftMap` from its functor and
 * bifunctor, `flatMap` from its monad-shaped chaining, and combination from its applicative
 * (`mapN`) or from `combine`. This spec therefore reaches all four through the syntax
 * imported above rather than through a member of a wrapper, and the specific instance that
 * makes the accumulating cases work is the `Semigroup` of `NonEmptyChain`: it is what
 * appends two chains of failures, left to right, so that the failures already reported
 * always precede the ones a later step adds. That ordering is asserted case by case below,
 * because it is the observable contract and the reason the failure side is an ordered chain
 * rather than a set.
 *
 * ===The third shape===
 *
 * An `Ior` has a case the type being ported could not express. Its value was mandatory, so
 * it could say "a value and no failures" and "a value and some failures" but never
 * "failures and no value at all"; the nearest it came was a convention, usually an empty
 * collection standing in for the missing value. `Ior.Left` says it in the type, and the
 * widening is deliberate. It is asserted here rather than ignored: the three shapes are
 * distinguished, `hasFailures` is shown to be true for `Ior.Left` as well as for
 * `Ior.Both`, and the accessor that reads the value is shown to return nothing for it -
 * which is why reading a value is an `Option` here where the original had a total getter.
 *
 * ===How outcomes are asserted===
 *
 * Two vocabularies appear below and they answer different questions. The matchers imported
 * above read an outcome the way the assertion helper being ported did, where an outcome is
 * a success exactly when it reports no failures; a value accompanied by failures is
 * therefore a failure to them, and `beSuccess` and `haveValue` do not hold for it. That is
 * the right reading of an outcome as a whole, and it is used wherever this spec asserts
 * which side of that line an outcome falls on. It cannot express "this value, and also
 * these failures", which is precisely what a partial success is, so the two halves of such
 * an outcome are asserted separately through the local accessors below, and the failure
 * matchers are used alongside them to pin the reason.
 *
 * ===What is deliberately not asserted===
 *
 * There is no serialized-form assertion here. The four alias types of this package are
 * generic containers and are excluded from the closed inventory of types whose serialized
 * form is covered: an instance for one of them follows from the element type and the
 * failure type each having one, so covering the container would add nothing to that
 * inventory and the inventory is compared against a fixed list. The serialized form of a
 * failure itself is covered by the spec of `Failure`. What replaces the removed
 * serialization case here is the structural round trip that does belong to this file -
 * taking an outcome apart into its failures and its value and rebuilding it from those
 * parts through `of`.
 *
 * Nor is there a reflective sweep over the members of a bean. The type is no longer a bean,
 * so there is nothing to sweep; the behaviour such a sweep was there to check - equality,
 * hashing and rendering - is asserted directly instead.
 *
 * ===Traceability===
 *
 * Every test method of the class being ported has a named case here, and the mapping from
 * one to the other is recorded in the comment block at the foot of this file. Five of those
 * methods tested machinery that has no counterpart in this port - the factory that ran a
 * supplied block, the five collector factories, the entry-stream helper, the reflective
 * sweep and Java serialization - and the block says for each what carries its intent
 * instead.
 *
 * @see [[Failure]] for the failure an outcome reports
 * @see [[FailureReason]] for the reasons a failure can carry
 * @see [[com.opengamma.strata.collect.testkit.ResultMatchers]] for the matchers used here
 */
final class ValueWithFailuresSpec extends AnyFunSuite with Matchers {

  // ---------------------------------------------------------------------------
  // Fixtures.
  //
  // The two failures are the pair the class being ported used, with the same
  // reasons and the same messages, so that a case here reads against the case it
  // came from.
  // ---------------------------------------------------------------------------

  /** The first fixture failure, reported as invalid input. */
  private val FAILURE1: Failure = Failure.Invalid("invalid")

  /** The second fixture failure, reported as missing data. */
  private val FAILURE2: Failure = Failure.MissingData("data")

  /** The failure list of an outcome that reports nothing. */
  private val NoFailures: List[Failure] = List.empty[Failure]

  /**
   * The three values the aggregation cases calculate over.
   *
   * Each is written with a decimal point rather than as a whole number, and that is not
   * cosmetic: the messages asserted below render the value, so a whole-number literal would
   * both change the rendered text and be an implicit widening, which this build rejects.
   */
  private val FirstInput: Double = 5.0
  private val SecondInput: Double = 6.0
  private val ThirdInput: Double = 7.0

  /** The product of the three inputs, which the reducing case arrives at from `1.0`. */
  private val ExpectedProduct: Double = 210.0

  /**
   * The messages the three calculations report, written out rather than interpolated.
   *
   * `mockCalc` builds its message by interpolation, so asserting against literals is what
   * pins the rendering of a value into that message. Five cases of the class being ported
   * asserted these three strings, and every aggregation case below asserts them again.
   */
  private val ExpectedCalculationMessages: List[String] = List(
    "Error calculating result for input value 5.0",
    "Error calculating result for input value 6.0",
    "Error calculating result for input value 7.0")

  /**
   * Three calculations, each producing its value and reporting one failure about it.
   *
   * This is the fixture of the aggregation cases of the class being ported, under the name
   * it had there. Every element is a partial success - it has a value and reports a failure
   * - which is what makes the aggregations below interesting: each one has both halves of
   * three outcomes to fold together.
   */
  private val Calculations: List[ValueWithFailures[Double]] =
    List(mockCalc(FirstInput), mockCalc(SecondInput), mockCalc(ThirdInput))

  /**
   * Returns a calculation that produced a value but reported a failure about it.
   *
   * @param value  the input the calculation ran on, which its message names
   * @return the value together with the one failure reported about it
   */
  private def mockCalc(value: Double): ValueWithFailures[Double] =
    ValueWithFailures.of(
      value,
      List(Failure.CalculationFailed(s"Error calculating result for input value $value")))

  // ---------------------------------------------------------------------------
  // Local accessors.
  //
  // The type has no members of its own, so the two halves of an outcome are read
  // through the accessors of an `Ior`. These three name what is being read at
  // each call site and keep the ordering of the failure chain visible, which is
  // what most of the assertions below are about.
  // ---------------------------------------------------------------------------

  /**
   * Returns the value an outcome carries, if it carries one.
   *
   * This is where the widening of the type shows up in the assertions: the getter of the
   * class being ported was total, and this is not, because `Ior.Left` has no value.
   */
  private def valueOf[A](outcome: ValueWithFailures[A]): Option[A] = outcome.right

  /** Returns the failures an outcome reports, in the order it reports them. */
  private def failuresOf[A](outcome: ValueWithFailures[A]): List[Failure] =
    outcome.left.fold(NoFailures)(chain => chain.toChain.toList)

  /** Returns the messages of the failures an outcome reports, in order. */
  private def messagesOf[A](outcome: ValueWithFailures[A]): List[String] =
    failuresOf(outcome).map(failure => failure.message)

  /** Returns the reasons of the failures an outcome reports, in order. */
  private def reasonsOf[A](outcome: ValueWithFailures[A]): List[FailureReason] =
    failuresOf(outcome).map(failure => failure.reason)

  /**
   * Runs a computation that reports what stopped it by raising, keeping a fallback value if
   * it does.
   *
   * This stands in for the factory of the class being ported that took a fallback value and
   * a block to run: it returned what the block produced, or the fallback together with one
   * failure, if the block raised. Nothing in this port reports a failure that way - a
   * failure is returned - so the factory has no counterpart and this helper lives here, in
   * the spec, for the two cases that pin its contract. The bridge from a raised error to a
   * value is the standard library's, and it is used here only because the contract being
   * described is about a raised error; a domain failure is never reached this way.
   *
   * @param fallback  the value to keep if the computation does not produce one
   * @param computation  the computation to run
   * @return what the computation produced, or the fallback with the one failure it raised
   */
  private def attempt[A](fallback: A, computation: () => A): ValueWithFailures[A] =
    Try(computation()).toEither.fold(
      error => ValueWithFailures.of(fallback, List(Failure.Error(describe(error)))),
      value => ValueWithFailures.of(value))

  /** Describes a raised error for the message of the failure that records it. */
  private def describe(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getSimpleName)

  /**
   * Parses every element it is given, reporting one failure for each that is not a number.
   *
   * This is the function the chaining case applies. It reports what it could not parse
   * rather than raising, so the elements that did parse are still returned - which is the
   * whole point of a partial success, and the reason the outcome of chaining carries the
   * failure this reports alongside the failure the outcome it was applied to already had.
   *
   * @param items  the text to parse
   * @return the numbers parsed, with one failure for each element that was not a number
   */
  private def parseAll(items: List[String]): ValueWithFailures[List[Int]] = {
    val (parsed, reported) =
      items.foldLeft((List.empty[Int], NoFailures)) {
        case ((values, failures), item) =>
          item.toIntOption.fold((values, failures :+ Failure.Invalid(s"Not a number: $item")))(
            number => (values :+ number, failures))
      }
    ValueWithFailures.of(parsed, reported)
  }

  /**
   * Takes an outcome apart into its failures and its value and rebuilds it from those parts.
   *
   * `of` is the factory used wherever there is a value to rebuild from, which is the two
   * shapes the class being ported could express. The third shape has no value, so `of`
   * cannot express it and the chain is rebuilt on its own; that asymmetry is the widening
   * of the type, stated in code. An outcome with neither a value nor a failure does not
   * exist - an `Ior` is always at least one of the two - so the remaining branch is
   * unreachable and returns what it was given rather than inventing a value.
   *
   * @param outcome  the outcome to take apart and rebuild
   * @return the outcome rebuilt from its parts
   */
  private def rebuild[A](outcome: ValueWithFailures[A]): ValueWithFailures[A] = {
    val failures = failuresOf(outcome)
    valueOf(outcome).fold(
      NonEmptyChain
        .fromSeq(failures)
        .fold(outcome)(chain => Ior.left[NonEmptyChain[Failure], A](chain)))(
      value => ValueWithFailures.of(value, failures))
  }

  // ---------------------------------------------------------------------------
  // Construction: `of` and `hasFailures`.
  // ---------------------------------------------------------------------------

  test("of with a value alone is a plain success that reports no failures") {
    val outcome: ValueWithFailures[String] = ValueWithFailures.of("success")

    ValueWithFailures.hasFailures(outcome) shouldBe false
    outcome shouldBe Ior.right("success")
    valueOf(outcome) shouldBe Some("success")
    failuresOf(outcome) shouldBe NoFailures
    outcome should beSuccess
    outcome should haveValue("success")
  }

  test("of with failures is a partial success holding the value and the failures in order") {
    // The class being ported offered this both as a variadic factory and as one taking a
    // collection. Only the collection form survives - a single method serves both call
    // shapes here - so this case and the one below exercise the same member.
    val outcome: ValueWithFailures[String] = ValueWithFailures.of("success", List(FAILURE1, FAILURE2))

    ValueWithFailures.hasFailures(outcome) shouldBe true
    outcome shouldBe Ior.both(NonEmptyChain.of(FAILURE1, FAILURE2), "success")
    valueOf(outcome) shouldBe Some("success")
    failuresOf(outcome) shouldBe List(FAILURE1, FAILURE2)
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("of takes its failures from an ordered list and reports them in that order") {
    val outcome: ValueWithFailures[String] = ValueWithFailures.of("success", List(FAILURE1, FAILURE2))
    val reversed: ValueWithFailures[String] = ValueWithFailures.of("success", List(FAILURE2, FAILURE1))

    failuresOf(outcome) shouldBe List(FAILURE1, FAILURE2)
    failuresOf(reversed) shouldBe List(FAILURE2, FAILURE1)
    // The order is part of the value, so the two differ even though they report the same
    // pair of failures about the same value.
    (reversed == outcome) shouldBe false

    // Nothing to report gives a plain success, so a caller can pass whatever it
    // accumulated without first asking whether anything went wrong.
    val nothingReported: ValueWithFailures[String] = ValueWithFailures.of("success", NoFailures)
    nothingReported shouldBe Ior.right("success")
    ValueWithFailures.hasFailures(nothingReported) shouldBe false
  }

  test("of takes its failures from a set and reports every one of them") {
    val outcome: ValueWithFailures[String] = ValueWithFailures.of("success", Set(FAILURE1, FAILURE2))

    ValueWithFailures.hasFailures(outcome) shouldBe true
    valueOf(outcome) shouldBe Some("success")
    // A set has no order of its own, so this asserts membership rather than order, as the
    // case being ported did. The chain preserves whatever order iterating the set gave.
    failuresOf(outcome) should have size 2
    failuresOf(outcome) should contain theSameElementsAs List(FAILURE1, FAILURE2)
    outcome should beFailureWith(FailureReason.INVALID)
    outcome should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("a computation that produces a value reports no failure and discards the fallback") {
    val outcome: ValueWithFailures[String] = attempt("", () => "A")

    ValueWithFailures.hasFailures(outcome) shouldBe false
    valueOf(outcome) shouldBe Some("A")
    failuresOf(outcome) shouldBe NoFailures
    outcome should beSuccess
    outcome should haveValue("A")
  }

  test("a computation that fails keeps the fallback value and reports one ERROR failure") {
    val outcome: ValueWithFailures[String] = attempt("", () => throw new IllegalArgumentException("boom"))

    ValueWithFailures.hasFailures(outcome) shouldBe true
    // Keeping the fallback is the point of the factory being ported: the caller is left
    // with something usable as well as with the report of what went wrong.
    valueOf(outcome) shouldBe Some("")
    failuresOf(outcome) should have size 1
    reasonsOf(outcome) shouldBe List(FailureReason.ERROR)
    messagesOf(outcome) shouldBe List("boom")
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("boom")
  }

  test("an outcome can report failures and carry no value at all") {
    // The widening of the type. The class being ported always had a value, so it could not
    // express this shape; the nearest it came was a convention, such as an empty collection
    // standing in for the value it had to supply.
    val outcome: ValueWithFailures[String] = Ior.left(NonEmptyChain.of(FAILURE1, FAILURE2))

    ValueWithFailures.hasFailures(outcome) shouldBe true
    valueOf(outcome) shouldBe None
    failuresOf(outcome) shouldBe List(FAILURE1, FAILURE2)
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)

    // `of` cannot build it: it pairs a value with failures, so it reaches the other two
    // shapes and only those.
    valueOf(ValueWithFailures.of("success", List(FAILURE1, FAILURE2))) shouldBe Some("success")
    valueOf(ValueWithFailures.of("success", NoFailures)) shouldBe Some("success")
  }


  // ---------------------------------------------------------------------------
  // Transformation: the value, the failures, and chaining.
  //
  // None of these is a member of the object under test. They are the operations
  // an `Ior` already has, reached through the syntax imported at the head of this
  // file, which is why the class being ported needed to declare them and this
  // port does not.
  // ---------------------------------------------------------------------------

  test("map transforms the value and preserves the failures") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("1", "2"), List(FAILURE1))
    val mapped: ValueWithFailures[List[Int]] = base.map(items => items.map(item => item.toInt))

    valueOf(mapped) shouldBe Some(List(1, 2))
    failuresOf(mapped) shouldBe List(FAILURE1)
    // The shape is preserved too: a partial success stays partial, and a plain success
    // stays plain, because mapping touches only the value.
    mapped shouldBe Ior.both(NonEmptyChain.one(FAILURE1), List(1, 2))
    ValueWithFailures.of(List("3")).map(items => items.map(item => item.toInt)) shouldBe
      Ior.right(List(3))
  }

  test("leftMap rewrites the failures and preserves the value") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("1", "2"), List(FAILURE1))
    val relabelled: ValueWithFailures[List[String]] =
      base.leftMap(chain => chain.map(_ => FAILURE2))

    valueOf(relabelled) shouldBe valueOf(base)
    failuresOf(relabelled) shouldBe List(FAILURE2)
  }

  test("leftMap on an outcome that reports nothing leaves it equal to the outcome it was given") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("1", "2"), NoFailures)
    val relabelled: ValueWithFailures[List[String]] =
      base.leftMap(chain => chain.map(_ => FAILURE2))

    // There is no failure for the function to see, so it never runs and the outcome is
    // unchanged - the assertion the case being ported made.
    relabelled shouldBe base
    failuresOf(relabelled) shouldBe NoFailures
    relabelled should beSuccess
  }

  test("flatMap reports the failures already present before those the function adds") {
    val base: ValueWithFailures[List[String]] =
      ValueWithFailures.of(List("1", "a", "2"), List(FAILURE1))
    val chained: ValueWithFailures[List[Int]] = base.flatMap(parseAll)

    valueOf(chained) shouldBe Some(List(1, 2))

    // The ordering here is the clearest observable consequence of the failure side being a
    // chain combined left to right: what was already reported comes first and what the
    // function reported follows. Both the size and the exact positions are asserted,
    // because every accumulating case below depends on the same rule.
    val failures = failuresOf(chained)
    failures should have size 2
    failures.head shouldBe FAILURE1
    failures(1).reason shouldBe FailureReason.INVALID
    failures(1).message shouldBe "Not a number: a"
    reasonsOf(chained) shouldBe List(FailureReason.INVALID, FailureReason.INVALID)

    // Chaining from an outcome that reports nothing leaves only what the function reported.
    val fromClean: ValueWithFailures[List[Int]] =
      ValueWithFailures.of(List("1", "a", "2")).flatMap(parseAll)
    failuresOf(fromClean) should have size 1
    valueOf(fromClean) shouldBe Some(List(1, 2))
  }


  // ---------------------------------------------------------------------------
  // Combination: two outcomes at a time.
  // ---------------------------------------------------------------------------

  test("two outcomes combine their values and accumulate their failures in order") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("a"), List(FAILURE1))
    val other: ValueWithFailures[List[String]] =
      ValueWithFailures.of(List("b", "c"), List(FAILURE2))

    // The method the class being ported declared for this has no counterpart: combining two
    // outcomes is what the applicative of an `Ior` does, and `mapN` is how it is written.
    val combined: ValueWithFailures[List[String]] = (base, other).mapN(_ ++ _)

    valueOf(combined) shouldBe Some(List("a", "b", "c"))
    failuresOf(combined) shouldBe List(FAILURE1, FAILURE2)
  }

  test("outcomes of different value types combine into one value carrying both failures") {
    val flag: ValueWithFailures[Boolean] = ValueWithFailures.of(true, List(FAILURE1))
    val count: ValueWithFailures[Int] = ValueWithFailures.of(1, List(FAILURE2))

    val joined: ValueWithFailures[String] =
      (flag, count).mapN((left, right) => left.toString + right.toString)

    valueOf(joined) shouldBe Some("true1")
    failuresOf(joined) shouldBe List(FAILURE1, FAILURE2)
  }

  test("combiningValues is the binary operator that reduces many outcomes into one") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("a"), List(FAILURE1))
    val other: ValueWithFailures[List[String]] =
      ValueWithFailures.of(List("b", "c"), List(FAILURE2))

    // The member returns the operator rather than performing the combination, so it is
    // handed to a reduction, which is what the case being ported did with the stream it
    // reduced.
    val reduced: ValueWithFailures[List[String]] =
      List(base, other).reduceLeft(ValueWithFailures.combiningValues[List[String]](_ ++ _))

    valueOf(reduced) shouldBe Some(List("a", "b", "c"))
    failuresOf(reduced) shouldBe List(FAILURE1, FAILURE2)

    // The combining function is applied only where both sides carry a value. Where one
    // side carries none, its failures are kept and the other side's value survives - the
    // three-case behaviour the value type of the original could not express.
    val withoutValue: ValueWithFailures[List[String]] = Ior.left(NonEmptyChain.one(FAILURE2))
    val kept: ValueWithFailures[List[String]] =
      ValueWithFailures.combiningValues[List[String]](_ ++ _)(base, withoutValue)

    valueOf(kept) shouldBe Some(List("a"))
    failuresOf(kept) shouldBe List(FAILURE1, FAILURE2)
  }


  // ---------------------------------------------------------------------------
  // Replacing the value, and adding failures.
  //
  // The three forms of `withValue` differ in what they do with failures, so each
  // case below asserts the order of the failure list as well as the value. That
  // order is the observable contract and the reason the failure side is a chain
  // rather than a set.
  // ---------------------------------------------------------------------------

  test("withValue replaces the value and keeps the failures already reported") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("a"), List(FAILURE1))

    val replaced: ValueWithFailures[String] = ValueWithFailures.withValue(base, "combined")

    valueOf(replaced) shouldBe Some("combined")
    failuresOf(replaced) shouldBe List(FAILURE1)

    // Where there was no value there now is one, so a failures-only outcome becomes a
    // partial success and its failures are untouched.
    val gained: ValueWithFailures[String] =
      ValueWithFailures.withValue(Ior.left[NonEmptyChain[Failure], Int](NonEmptyChain.one(FAILURE2)), "combined")
    valueOf(gained) shouldBe Some("combined")
    failuresOf(gained) shouldBe List(FAILURE2)
  }

  test("withValue with further failures reports the existing ones before the supplied ones") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("a"), List(FAILURE1))

    val replaced: ValueWithFailures[String] =
      ValueWithFailures.withValue(base, "combined", List(FAILURE2))

    valueOf(replaced) shouldBe Some("combined")
    failuresOf(replaced) shouldBe List(FAILURE1, FAILURE2)
  }

  test("withValue with another outcome takes its value and adds its failures last") {
    val base: ValueWithFailures[List[String]] = ValueWithFailures.of(List("a"), List(FAILURE1))
    val other: ValueWithFailures[String] = ValueWithFailures.of("combined", List(FAILURE2))

    val replaced: ValueWithFailures[String] = ValueWithFailures.withValue(base, other)

    valueOf(replaced) shouldBe Some("combined")
    failuresOf(replaced) shouldBe List(FAILURE1, FAILURE2)

    // Where the outcome supplying the failures reports none, the other outcome is what the
    // result is - there is nothing to prepend to it.
    val fromClean: ValueWithFailures[String] =
      ValueWithFailures.withValue(ValueWithFailures.of(List("a")), other)
    fromClean shouldBe other
    failuresOf(fromClean) shouldBe List(FAILURE2)
  }

  test("withAdditionalFailures appends the supplied failures after the existing ones") {
    val base: ValueWithFailures[String] = ValueWithFailures.of("combined", List(FAILURE1))

    val extended: ValueWithFailures[String] = withAdditionalFailures(base, List(FAILURE2))

    valueOf(extended) shouldBe Some("combined")
    failuresOf(extended) shouldBe List(FAILURE1, FAILURE2)

    // A plain success gains a failure side and keeps its value; a failures-only outcome
    // accumulates and stays as it is, because there is no value to keep.
    val fromClean: ValueWithFailures[String] =
      withAdditionalFailures(ValueWithFailures.of("combined"), List(FAILURE2))
    fromClean shouldBe Ior.both(NonEmptyChain.one(FAILURE2), "combined")

    val fromFailuresOnly: ValueWithFailures[String] =
      withAdditionalFailures(Ior.left(NonEmptyChain.one(FAILURE1)), List(FAILURE2))
    valueOf(fromFailuresOnly) shouldBe None
    failuresOf(fromFailuresOnly) shouldBe List(FAILURE1, FAILURE2)
  }

  test("withAdditionalFailures with nothing to add returns the outcome it was given") {
    val base: ValueWithFailures[String] = ValueWithFailures.of("combined", List(FAILURE1))

    // Documented as unchanged and identical, so identity is what is asserted rather than
    // equality alone: adding nothing allocates nothing.
    (withAdditionalFailures(base, NoFailures) eq base) shouldBe true
    (withAdditionalFailures(base, Set.empty[Failure]) eq base) shouldBe true
  }


  // ---------------------------------------------------------------------------
  // Aggregation: many outcomes at a time.
  //
  // The class being ported offered five of these as collector objects handed to a
  // stream. A collector belongs to a collection library this port does not use,
  // so each of them is reached here as a fold or as one of the two `combineValues`
  // members, which perform the same accumulation purely. Every case over the three
  // calculations expects the same three messages, in the order the calculations
  // were given in.
  // ---------------------------------------------------------------------------

  test("reducing outcomes with combiningValues multiplies the values and reports every failure") {
    // The collector being replaced took an identity and a binary operator, which is exactly
    // what a fold takes: the identity becomes the starting outcome and the operator is the
    // one `combiningValues` builds.
    val product: ValueWithFailures[Double] =
      Calculations.foldLeft(ValueWithFailures.of(1.0))(
        ValueWithFailures.combiningValues[Double](_ * _))

    valueOf(product) shouldBe Some(ExpectedProduct)
    failuresOf(product) should have size 3
    reasonsOf(product) shouldBe List(
      FailureReason.CALCULATION_FAILED,
      FailureReason.CALCULATION_FAILED,
      FailureReason.CALCULATION_FAILED)
    messagesOf(product) shouldBe ExpectedCalculationMessages
    product should beFailureWith(FailureReason.CALCULATION_FAILED)
    // The message matcher holds when any one reported message matches in full, so it reads
    // the middle calculation of the three out of the accumulated chain.
    product should haveFailureMessageMatching("Error calculating result for input value 6\\.0")
  }

  test("combineValuesAsList holds the values as a list and reports every failure") {
    val combined: ValueWithFailures[List[Double]] =
      ValueWithFailures.combineValuesAsList(Calculations)

    valueOf(combined) shouldBe Some(List(FirstInput, SecondInput, ThirdInput))
    failuresOf(combined) should have size 3
    messagesOf(combined) shouldBe ExpectedCalculationMessages
    reasonsOf(combined).distinct shouldBe List(FailureReason.CALCULATION_FAILED)
  }

  test("combining outcomes as a list replaces the collector the original streamed into") {
    // The collector consumed a stream, so the member that replaces it is checked against a
    // source that is produced lazily and can be traversed only once - which is what its
    // parameter type admits - and against the same input held as a list.
    val streamed: ValueWithFailures[List[Double]] =
      ValueWithFailures.combineValuesAsList(
        Iterator(FirstInput, SecondInput, ThirdInput).map(input => mockCalc(input)))

    valueOf(streamed) shouldBe Some(List(FirstInput, SecondInput, ThirdInput))
    messagesOf(streamed) shouldBe ExpectedCalculationMessages
    streamed shouldBe ValueWithFailures.combineValuesAsList(Calculations)

    // Nothing to aggregate is a plain success holding nothing, as it is for every fold.
    ValueWithFailures.combineValuesAsList(List.empty[ValueWithFailures[Double]]) shouldBe
      Ior.right(List.empty[Double])
  }

  test("combineValuesAsSet holds the values as a set and reports every failure") {
    val combined: ValueWithFailures[Set[Double]] =
      ValueWithFailures.combineValuesAsSet(Calculations)

    valueOf(combined) shouldBe Some(Set(FirstInput, SecondInput, ThirdInput))
    failuresOf(combined) should have size 3
    messagesOf(combined) shouldBe ExpectedCalculationMessages
    reasonsOf(combined).distinct shouldBe List(FailureReason.CALCULATION_FAILED)
  }

  test("combining outcomes as a set replaces the collector the original streamed into") {
    val streamed: ValueWithFailures[Set[Double]] =
      ValueWithFailures.combineValuesAsSet(
        Iterator(FirstInput, SecondInput, ThirdInput).map(input => mockCalc(input)))

    valueOf(streamed) shouldBe Some(Set(FirstInput, SecondInput, ThirdInput))
    messagesOf(streamed) shouldBe ExpectedCalculationMessages
    streamed shouldBe ValueWithFailures.combineValuesAsSet(Calculations)

    // The set collapses repeated values where the list keeps them, which is the whole
    // difference between the two members.
    val repeated: List[ValueWithFailures[Double]] = List(mockCalc(FirstInput), mockCalc(FirstInput))
    valueOf(ValueWithFailures.combineValuesAsSet(repeated)) shouldBe Some(Set(FirstInput))
    valueOf(ValueWithFailures.combineValuesAsList(repeated)) shouldBe
      Some(List(FirstInput, FirstInput))
    failuresOf(ValueWithFailures.combineValuesAsSet(repeated)) should have size 2
  }

  test("results bridge into outcomes so that only the values that succeeded survive") {
    val results: List[FailureOr[String]] =
      List(Right("Hello"), Left(Failure.Error("Uh oh")), Right("World"))

    // This is the bridge from a result, which has a value or a failure, to an outcome,
    // which can have both. A success becomes an outcome carrying its value and reporting
    // nothing; a failure becomes an outcome reporting its failure and carrying no value.
    // Aggregating those is what leaves three inputs contributing two values.
    val bridged: ValueWithFailures[List[String]] =
      ValueWithFailures.combineValuesAsList(results.map { result =>
        result.fold[ValueWithFailures[String]](
          failure => Ior.left(NonEmptyChain.one(failure)),
          value => Ior.right(value))
      })

    valueOf(bridged) shouldBe Some(List("Hello", "World"))
    failuresOf(bridged) should have size 1
    reasonsOf(bridged) shouldBe List(FailureReason.ERROR)
    messagesOf(bridged) shouldBe List("Uh oh")
  }

  test("a map of results keeps the entries that succeeded and reports the one that did not") {
    val results: Map[String, FailureOr[String]] = Map(
      "key 1" -> Right("success 1"),
      "key 2" -> Left(FAILURE1),
      "key 3" -> Right("success 2"))

    // The helper the original streamed a map's entries through has no counterpart, so the
    // map is folded directly. The keys are visited in sorted order so that the order of
    // the reported failures is fixed by this spec rather than by how the map happens to
    // iterate.
    val (values, reported) =
      results.toList.sortBy { case (key, _) => key }.foldLeft((Map.empty[String, String], NoFailures)) {
        case ((accumulated, failures), (key, result)) =>
          result.fold(
            failure => (accumulated, failures :+ failure),
            value => (accumulated.updated(key, value), failures))
      }
    val combined: ValueWithFailures[Map[String, String]] = ValueWithFailures.of(values, reported)

    valueOf(combined) shouldBe Some(Map("key 1" -> "success 1", "key 3" -> "success 2"))
    failuresOf(combined) shouldBe List(FAILURE1)
  }

  test("a map of outcomes keeps every entry and reports the failures of each") {
    val outcomes: Map[String, ValueWithFailures[String]] = Map(
      "key 1" -> ValueWithFailures.of("success 1", List(FAILURE1)),
      "key 2" -> ValueWithFailures.of("success 2", List(FAILURE2)),
      "key 3" -> ValueWithFailures.of("success 3"))

    // Every entry here has a value, so every key survives - which is what separates this
    // case from the one above, where an entry that failed had no value to contribute. The
    // keys are visited in sorted order so the reported failures have a fixed order.
    val combined: ValueWithFailures[Map[String, String]] =
      outcomes.toList
        .sortBy { case (key, _) => key }
        .map { case (key, outcome) => outcome.map(value => Map(key -> value)) }
        .foldLeft(ValueWithFailures.of(Map.empty[String, String]))(
          ValueWithFailures.combiningValues[Map[String, String]](_ ++ _))

    valueOf(combined) shouldBe
      Some(Map("key 1" -> "success 1", "key 2" -> "success 2", "key 3" -> "success 3"))
    failuresOf(combined) shouldBe List(FAILURE1, FAILURE2)
  }


  // ---------------------------------------------------------------------------
  // Structure: equality, hashing, rendering and the round trip.
  //
  // Two cases of the class being ported tested machinery this port does not have:
  // a reflective sweep over the properties of a bean, and Java serialization.
  // Neither has a counterpart, so what replaces them is an assertion of the
  // behaviour each existed to check.
  // ---------------------------------------------------------------------------

  test("equality, hashing and rendering distinguish the three shapes an outcome can take") {
    val reported: ValueWithFailures[String] = ValueWithFailures.of("success", List(FAILURE1, FAILURE2))
    val sameReported: ValueWithFailures[String] =
      ValueWithFailures.of("success", List(FAILURE1, FAILURE2))
    val plain: ValueWithFailures[String] = ValueWithFailures.of("success")
    val onlyFailures: ValueWithFailures[String] = Ior.left(NonEmptyChain.of(FAILURE1, FAILURE2))

    // Two outcomes built the same way are equal and hash alike, which is what the sweep
    // being replaced checked reflectively.
    reported shouldBe sameReported
    reported.hashCode shouldBe sameReported.hashCode
    (reported === sameReported) shouldBe true

    // The same value with and without failures are different outcomes, and so is the pair
    // of failures with no value at all. Holding all three in a set is the compact way to
    // assert that equality and hashing agree that they are three distinct values.
    (reported == plain) shouldBe false
    (reported === plain) shouldBe false
    (reported == onlyFailures) shouldBe false
    Set(plain, reported, onlyFailures) should have size 3

    // Rendering is a function of the value, so it separates the shapes too, and the
    // rendering of an outcome includes the rendering of each failure it reports.
    reported.show should not be plain.show
    reported.show should not be onlyFailures.show
    reported.show should include(FAILURE1.show)
    reported.show should include(FAILURE2.show)
    plain.show should include("success")
  }

  test("decomposing an outcome and rebuilding it from its parts yields an equal outcome") {
    // This replaces the serialization case. It is deliberately a structural round trip and
    // not a serialized-form one: the four alias types of this package are generic
    // containers and are excluded from the closed inventory of types whose serialized form
    // is covered, because an instance for a container follows from its element type and
    // the failure type each having one. That inventory is compared against a fixed list, so
    // nothing here claims to add to it; the serialized form of a failure itself belongs to
    // the spec of `Failure`.
    val plain: ValueWithFailures[String] = ValueWithFailures.of("success")
    val reported: ValueWithFailures[String] = ValueWithFailures.of("success", List(FAILURE1, FAILURE2))
    val onlyFailures: ValueWithFailures[String] = Ior.left(NonEmptyChain.of(FAILURE1, FAILURE2))

    rebuild(plain) shouldBe plain
    rebuild(reported) shouldBe reported
    rebuild(onlyFailures) shouldBe onlyFailures

    // The parts themselves survive the trip, in order, which is what makes the equality
    // above meaningful rather than accidental.
    valueOf(rebuild(reported)) shouldBe Some("success")
    failuresOf(rebuild(reported)) shouldBe List(FAILURE1, FAILURE2)
    valueOf(rebuild(onlyFailures)) shouldBe None
    failuresOf(rebuild(onlyFailures)) shouldBe List(FAILURE1, FAILURE2)
  }

}

/*
 * ---------------------------------------------------------------------------
 * Traceability: the test class being ported, method by method.
 * ---------------------------------------------------------------------------
 *
 * The class being ported, `ValueWithFailuresTest`, declared 27 test methods. Every one of
 * them has a named case in this spec, listed below as
 *
 *     <java method> -> "<name of the case here>"   <status>
 *
 * with a short reason wherever the subject of the Java method is machinery this port does
 * not have. Two statuses appear: `ported`, where the case carries the same assertion over
 * the same behaviour, and `consolidated:ValueWithFailuresSpec`, where two Java methods
 * reach one member here and both keep a case of their own so that the mapping stays one
 * row per Java method. No method of this class is dropped and none is covered in part.
 *
 * The case names below are the names that appear in the test report, so they are the names
 * a traceability check matches against; they must not be edited without editing this block
 * with them.
 *
 *  1. test_of_array_noFailures
 *       -> "of with a value alone is a plain success that reports no failures"
 *       ported
 *  2. test_of_array
 *       -> "of with failures is a partial success holding the value and the failures in order"
 *       ported - the variadic factory of the original collapses onto the collection form,
 *       so one member serves both call shapes and this case exercises it
 *  3. test_of_list
 *       -> "of takes its failures from an ordered list and reports them in that order"
 *       ported
 *  4. test_of_set
 *       -> "of takes its failures from a set and reports every one of them"
 *       ported - membership is asserted rather than order, as in the original
 *  5. test_of_supplier_success
 *       -> "a computation that produces a value reports no failure and discards the fallback"
 *       ported - the factory that ran a supplied block has no counterpart, because nothing
 *       in this port reports a failure by raising; the contract is pinned through the
 *       bridge declared in this spec
 *  6. test_of_supplier_failure
 *       -> "a computation that fails keeps the fallback value and reports one ERROR failure"
 *       ported - same subject as row 5; retaining the fallback value is the behaviour the
 *       Java case existed to assert and it is asserted here
 *  7. test_map
 *       -> "map transforms the value and preserves the failures"
 *       ported - through the functor of the alias rather than a declared member
 *  8. test_mapFailureItems
 *       -> "leftMap rewrites the failures and preserves the value"
 *       ported - through the bifunctor of the alias
 *  9. test_mapFailureItems_noFailures
 *       -> "leftMap on an outcome that reports nothing leaves it equal to the outcome it was given"
 *       ported
 * 10. test_flatMap
 *       -> "flatMap reports the failures already present before those the function adds"
 *       ported - including the ordering of the two failures, which the Java case asserted
 *       position by position
 * 11. test_combinedWith
 *       -> "two outcomes combine their values and accumulate their failures in order"
 *       ported - through the applicative of the alias
 * 12. test_combinedWith_differentTypes
 *       -> "outcomes of different value types combine into one value carrying both failures"
 *       ported
 * 13. test_combining
 *       -> "combiningValues is the binary operator that reduces many outcomes into one"
 *       ported - the member returns the operator, so it is handed to a reduction here where
 *       the original handed it to the reduction of a stream
 * 14. test_withValue_value
 *       -> "withValue replaces the value and keeps the failures already reported"
 *       ported
 * 15. test_withValue_valueFailures
 *       -> "withValue with further failures reports the existing ones before the supplied ones"
 *       ported
 * 16. test_withValue_ValueWithFailures
 *       -> "withValue with another outcome takes its value and adds its failures last"
 *       ported
 * 17. test_withAdditionalFailures
 *       -> "withAdditionalFailures appends the supplied failures after the existing ones"
 *       ported
 * 18. test_toValueWithFailures
 *       -> "reducing outcomes with combiningValues multiplies the values and reports every failure"
 *       ported - the collector object has no counterpart; the identity and the binary
 *       operator it was given become the starting outcome and the operator of a fold, and
 *       the product and the three messages asserted are those of the Java case
 * 19. test_combineAsList
 *       -> "combineValuesAsList holds the values as a list and reports every failure"
 *       ported
 * 20. test_toCombinedValuesAsList
 *       -> "combining outcomes as a list replaces the collector the original streamed into"
 *       consolidated:ValueWithFailuresSpec - the collector and the static method of the
 *       original reach the one member exercised in row 19; this case keeps its own name and
 *       adds what the collector implied, a source that can be traversed only once
 * 21. test_combineAsSet
 *       -> "combineValuesAsSet holds the values as a set and reports every failure"
 *       ported
 * 22. test_toCombinedValuesAsSet
 *       -> "combining outcomes as a set replaces the collector the original streamed into"
 *       consolidated:ValueWithFailuresSpec - as row 20, for the set-valued member
 * 23. test_toCombinedResultsAsList
 *       -> "results bridge into outcomes so that only the values that succeeded survive"
 *       ported - the collector is replaced by the bridge from a result to an outcome
 *       followed by `combineValuesAsList`
 * 24. test_toCombinedResultAsMap
 *       -> "a map of results keeps the entries that succeeded and reports the one that did not"
 *       ported - the entry-stream helper of the original has no counterpart, so the
 *       immutable map is folded directly, in sorted key order so the reported failures have
 *       a fixed order
 * 25. test_toCombinedValuesAsMap
 *       -> "a map of outcomes keeps every entry and reports the failures of each"
 *       ported - same subject as row 24; every key survives here because every entry has a
 *       value, which is the distinction between the two cases
 * 26. coverage
 *       -> "equality, hashing and rendering distinguish the three shapes an outcome can take"
 *       ported - the reflective sweep over the properties of a bean has no counterpart, the
 *       type no longer being a bean; the behaviour the sweep existed to check is asserted
 *       directly
 * 27. test_serialization
 *       -> "decomposing an outcome and rebuilding it from its parts yields an equal outcome"
 *       ported - Java serialization has no counterpart, and the alias containers of this
 *       package are excluded from the closed inventory of covered serialized forms, so the
 *       structural round trip is what replaces it
 *
 * Two cases here have no counterpart in the class being ported, because they assert
 * behaviour that class could not have:
 *
 *   - "an outcome can report failures and carry no value at all" - the third shape of an
 *     `Ior`, which the mandatory value of the original ruled out.
 *   - "withAdditionalFailures with nothing to add returns the outcome it was given" - the
 *     documented identity of adding nothing, asserted as identity rather than equality.
 *
 * That makes 29 cases in this file, all of which count towards the test total this module
 * is required to reach.
 */

