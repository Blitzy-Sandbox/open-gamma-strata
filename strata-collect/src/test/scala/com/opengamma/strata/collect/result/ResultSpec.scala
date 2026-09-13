/*
 * Copyright (C) 2013 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import cats.Eq
import cats.Hash
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries._
import com.opengamma.strata.collect.testkit.Outcome
import com.opengamma.strata.collect.testkit.ResultMatchers
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests the error channel of this module: the four aliases declared by the package object,
 * the conversions between them, and the collection-level operations over them.
 *
 * ===What is under test, and what is not===
 *
 * The class this spec is ported from held either a value or a failure behind some forty
 * instance methods, and it does not survive the port as a type. `FailureOr[A]` and
 * `ResultNec[A]` replace it, and both are aliases for a type the effect library already
 * provides, so there is no wrapper here to call `map`, `flatMap`, `leftMap` or a combining
 * method on. Every one of those operations is reached through the standard algebra of
 * `Either` instead - the member methods of the standard library where it has them, and the
 * syntax imported above where it does not - which is the whole point of dropping the class:
 * the port inherits an algebra that is already specified and already tested rather than
 * re-implementing one.
 *
 * What remains for this spec to exercise is therefore the small surface the package object
 * does define - `toNec`, `toValidated`, `toResult`, `sequence`, `combine`, `flatCombine`,
 * `allSuccessful`, `anyFailures` and `countFailures` - together with the behaviour of the
 * standard algebra at the points where the class being ported behaved differently. Those
 * points are where most of the value of this spec lies, and there are three of them.
 *
 * ===Three concerns the original conflated===
 *
 * The type being ported mixed up three things that the port keeps apart, and every case
 * below falls into one of the three.
 *
 *   - '''The value channel.''' One failure, fail-fast. `FailureOr[A]` says so in its type,
 *     and combining two failures on this channel keeps the first and discards the second.
 *   - '''The accumulating channel.''' Many failures. `ResultNec[A]` carries a chain of them,
 *     and combining two failures keeps both. Because the combination that accumulates is
 *     the one defined for the validating type rather than for `Either`, a case that wants
 *     accumulation goes through `toValidated` and comes back through `toResult`; that is
 *     the shape a validating factory of this library is written in, and the helper
 *     `accumulate` below is exactly it. Presenting such a chain as one failure is
 *     `Failure.collapse`, which is where the reason `MULTIPLE` and the `", "`-joined message
 *     of the original come from.
 *   - '''Exception capture.''' Removed outright. The original ran a supplied function, caught
 *     whatever it threw and turned it into a failure carrying the exception's type and stack
 *     trace. Nothing in this port does that, so the five cases that tested it, and the four
 *     that derived a failure from a thrown exception, are re-expressed against the contract
 *     that replaces them: a step that can fail says so by returning a result, and its
 *     failure arrives on the value channel where a caller has to deal with it. A failure
 *     here holds a reason, a message and attributes, and nothing else.
 *
 * ===A note on the operations over collections===
 *
 * `sequence`, `combine` and `flatCombine` require the failure type to be combinable, which
 * a chain of failures is and a single failure deliberately is not. Every aggregation case
 * below therefore runs on the accumulating channel and lifts its inputs with `toNec` first.
 * That is not an accident of this spec: it is the restriction that makes the accumulation
 * well defined, and asking for two single failures to be merged into one is meant to be
 * unwriteable.
 *
 * The three predicates are generic over any `Either`, so each is exercised on both channels.
 * They take a collection and have no variadic form, a single form being enough once the
 * argument is a Scala collection; the pair of cases the original had for each predicate is
 * kept, with the inline-argument case building its collection at the call site.
 *
 * ===Fixed cases and generated cases===
 *
 * Most of this file asserts against literal data, because most of what it covers is a named
 * member applied to a chosen input. The equality section is the exception. Equality of an
 * outcome is not one behaviour but a set of laws - reflexive, symmetric, transitive, agreeing
 * with hashing - and every other spec of both modules relies on those laws holding of
 * outcomes it never wrote down, so they are asserted over outcomes drawn from the shared
 * generators of [[com.opengamma.strata.collect.Arbitraries]] rather than over a table of
 * hand-written values. The generators are bounded, so the values they draw stay small enough
 * to read in a failure report, and they reach both shapes of each of the three channels this
 * file names.
 *
 * ===Determinism===
 *
 * Nothing here reads a clock, a file, the class path or the environment, nothing depends on
 * iteration order beyond the order the spec itself writes, and no case shares mutable state
 * with another, so the outcome of this spec does not depend on the order its cases run in.
 * The fixed cases are pure functions of literal data. The property cases of the equality
 * section are pure functions of the data drawn for them, which differs from run to run by
 * design: a law is asserted of every value a generator can produce, not of one the spec
 * happened to choose, and a counterexample found in any run is reported with the value that
 * produced it.
 *
 * @see [[Failure]] for the failure each outcome carries
 * @see [[FailureReason]] for the ten reasons a failure can carry
 * @see [[com.opengamma.strata.collect.testkit.ResultMatchers]] for the matchers used throughout
 */
final class ResultSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with TableDrivenPropertyChecks {

  /**
   * The number of outcomes each generated property of the equality section is checked against.
   *
   * The default of the framework is a handful, which is too few for what those cases assert.
   * Each of the three channels generates both of its shapes, the failure side of two of them
   * is a chain of one to four failures, and a chain has to be drawn whose reversal is not
   * itself before the order-sensitivity of equality is exercised at all. A hundred draws
   * reaches every one of those combinations many times over while keeping the whole file well
   * inside the second it runs in.
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  // ---------------------------------------------------------------------------
  // Fixtures, carried over from the test class being ported.
  //
  // The two successes hold the same text there as well - that is not a typo in
  // the original - and the two failures differ in reason as well as in message,
  // which is what makes them collapse to `MULTIPLE` rather than to a shared
  // reason. Each is ascribed to the alias rather than left to inference: the
  // matchers resolve their view of an outcome from the static type of the
  // expression on the left of `should`, so a bare `Right(...)`, whose type is
  // the subclass rather than the alias, would not resolve one at all.
  // ---------------------------------------------------------------------------

  /** The first of the two successes, which carry the same value in the original. */
  private val success1: FailureOr[String] = "success 1".asRight[Failure]

  /** The second of the two successes, equal to the first, as it is in the original. */
  private val success2: FailureOr[String] = "success 1".asRight[Failure]

  /** The first failure: missing data, so that the two failures disagree on their reason. */
  private val failure1: FailureOr[String] = Failure.MissingData("failure 1").asLeft[String]

  /** The second failure: a general error, disagreeing with the first. */
  private val failure2: FailureOr[String] = Failure.Error("failure 2").asLeft[String]

  /** The total mapping of the original: the length of a string. */
  private val mapStrLen: String => Int = _.length

  /** The result-returning mapping of the original, which always succeeds. */
  private val functionStrLen: String => FailureOr[Int] = input => input.length.asRight[Failure]

  /** The combining function of the original, which joins its two inputs with a space. */
  private val functionMerge: (String, String) => FailureOr[String] =
    (first, second) => s"$first $second".asRight[Failure]

  /** The first interpolated argument of the message-formatting cases. */
  private val colour: String = "blue"

  /** The second interpolated argument of the message-formatting cases. */
  private val animal: String = "rabbit"

  /** The third interpolated argument of the message-formatting cases. */
  private val vegetable: String = "carrot"

  // ---------------------------------------------------------------------------
  // Helpers.
  //
  // These four stand in for accessors the type being ported had and the port
  // does not, and each is total where the original threw: reading the failures
  // of an outcome that has none gives an empty list, and collapsing the
  // failures of a success gives no failure rather than an error.
  // ---------------------------------------------------------------------------

  /** Returns the failures an outcome on the accumulating channel holds, in order. */
  private def failuresOf[A](result: ResultNec[A]): List[Failure] =
    result.fold(chain => chain.toChain.toList, _ => List.empty[Failure])

  /** Presents the failures of an outcome as the single failure describing them, if any. */
  private def collapsedFailure[A](result: ResultNec[A]): Option[Failure] =
    result.swap.toOption.map(Failure.collapse)

  /**
   * Combines two outcomes on the accumulating channel, keeping the failures of both.
   *
   * This is the shape a validating factory of this library is written in: the checks are
   * converted to the accumulating form, assembled there, and converted back once at the end.
   * Assembling them on the value channel instead would stop at the first failure, which is
   * the difference the cases below are written to make observable.
   */
  private def accumulate[A, B, C](first: ResultNec[A], second: ResultNec[B])(
      f: (A, B) => C): ResultNec[C] =
    toResult((toValidated(first), toValidated(second)).mapN(f))

  /**
   * Extracts the failures reported by a collection of outcomes, if any were.
   *
   * The original threw when asked to build a failure out of outcomes that had all succeeded.
   * Here the absence of any failure is a representable answer, so the caller decides what to
   * do about it rather than being told by an exception.
   */
  private def extractFailures[A](results: List[FailureOr[A]]): Option[NonEmptyChain[Failure]] =
    NonEmptyChain.fromSeq(results.collect { case Left(failure) => failure })

  //-------------------------------------------------------------------------
  // A success, and what can be read from one.

  test("a success holds its value and yields it from every total accessor") {
    val outcome: FailureOr[String] = "success".asRight[Failure]
    outcome.isRight shouldBe true
    outcome.isLeft shouldBe false
    outcome should beSuccess
    outcome should haveValue("success")
    outcome.toOption shouldBe Some("success")
    // The original had two further accessors here, each taking a fallback that it rejected
    // when handed a reference naming no value. Neither has a counterpart: the fallback of
    // `getOrElse` is a value of the same type and cannot be absent, and the recovering form
    // is `fold`, whose left branch is a total function of the failure.
    outcome.getOrElse("blue") shouldBe "success"
    outcome.fold(_.message, identity) shouldBe "success"
  }

  test("foreach runs on a success and sees its value") {
    val outcome: FailureOr[String] = "success".asRight[Failure]
    outcome.foreach(value => value shouldBe "success")
    // `foreach` reports nothing back, so the assertion inside it would pass vacuously had it
    // never run. That it ran is observed separately, from the same right-biased view.
    outcome.toSeq should have size 1
    outcome.exists(_ == "success") shouldBe true
    outcome.forall(_ == "success") shouldBe true
  }

  test("the left projection runs on a failure and sees its reason and message") {
    val outcome: FailureOr[String] = Failure.Invalid("no success").asLeft[String]
    outcome.left.foreach { failure =>
      failure.reason shouldBe FailureReason.INVALID
      failure.message shouldBe "no success"
    }
    // As above, the assertion inside the projection is backed by an observation of the
    // failure itself, so the case cannot pass by never running.
    outcome.swap.toOption.map(_.reason) shouldBe Some(FailureReason.INVALID)
    outcome.swap.toOption.map(_.message) shouldBe Some("no success")
    outcome should beFailureWith(FailureReason.INVALID)
  }

  test("a success has no failure to read") {
    val outcome: FailureOr[String] = "success".asRight[Failure]
    // Reading the failure of a success threw in the original. Here the question is answered
    // by a value: there is no failure, and the absence is representable.
    outcome.swap.toOption shouldBe None
    outcome.swap.toSeq shouldBe empty
    failuresOf(toNec(outcome)) shouldBe empty
    // There is no partial accessor to reach for in the first place.
    assertDoesNotCompile("""
      val readFailure: Failure = "success".asRight[Failure].getFailure
      readFailure
    """)
  }

  //-------------------------------------------------------------------------
  // Mapping and chaining over a success.

  test("map applies the mapping function to the value of a success") {
    val success: FailureOr[String] = "success".asRight[Failure]
    val outcome: FailureOr[Int] = success.map(mapStrLen)
    outcome should beSuccess
    outcome should haveValue(7)
    outcome shouldBe Right(7)
  }

  test("mapping the failure of a success leaves it untouched") {
    val success: FailureOr[String] = "success".asRight[Failure]
    val outcome: FailureOr[String] = success.leftMap(_ => Failure.NotApplicable("Failure"))
    outcome shouldBe success
    outcome shouldBe Right("success")
    outcome should haveValue("success")
  }

  test("rebuilding the failure of a success is the identity however the rebuild is written") {
    val success: FailureOr[String] = "success".asRight[Failure]
    // The original had a second mapping member that reached inside the failure to its parts.
    // Both arrive at the same place here, because both are `leftMap` over a value that has
    // no left side to map.
    success.leftMap(_.mapMessage(_ => "Failure")) shouldBe success
    success.leftMap(_.withAttribute("key", "value")) shouldBe success
    // The same holds once the outcome has been lifted onto the accumulating channel, where
    // the mapping applies to every failure of a chain that is likewise not there.
    toNec(success).leftMap(_.map(_.mapMessage(_ => "Failure"))) shouldBe Right("success")
    toNec(success) should haveValue("success")
  }

  test("flatMap applies a result-returning function to the value of a success") {
    val success: FailureOr[String] = "success".asRight[Failure]
    val outcome: FailureOr[Int] = success.flatMap(functionStrLen)
    outcome should beSuccess
    outcome should haveValue(7)
    outcome shouldBe Right(7)
  }

  //-------------------------------------------------------------------------
  // Combining two outcomes.

  test("two successes combine into the merged value") {
    val first: FailureOr[String] = "Hello".asRight[Failure]
    val second: FailureOr[String] = "World".asRight[Failure]
    val outcome: FailureOr[String] = (first, second).tupled.flatMap {
      case (left, right) => functionMerge(left, right)
    }
    outcome should beSuccess
    outcome should haveValue("Hello World")
  }

  test("combining a success with a failure yields that failure") {
    val success: FailureOr[String] = "Hello".asRight[Failure]
    val failure: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val outcome: FailureOr[String] = (success, failure).tupled.flatMap {
      case (left, right) => functionMerge(left, right)
    }
    outcome should beFailureWith(FailureReason.ERROR)
    outcome.toOption shouldBe None
    failuresOf(toNec(outcome)) should have size 1
  }

  test("combining a failure with a success yields the failure and never calls the combiner") {
    val failure: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val success: FailureOr[String] = "World".asRight[Failure]
    val outcome: FailureOr[String] = (failure, success).tupled.flatMap {
      case (left, right) => functionMerge(left, right)
    }
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("failure")
    outcome shouldBe Left(Failure.Error("failure"))
  }

  test("combining two failures short-circuits on the value channel and accumulates on the chain channel") {
    val first: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val second: FailureOr[String] = Failure.Error("fail").asLeft[String]

    // The value channel carries one failure, so combining keeps the first and the second is
    // never seen. This is the fail-fast half of the behaviour the original merged into one
    // type, and it is what the type of `FailureOr` promises.
    val shortCircuited: FailureOr[String] = (first, second).tupled.flatMap {
      case (left, right) => functionMerge(left, right)
    }
    shortCircuited shouldBe Left(Failure.Error("failure"))
    failuresOf(toNec(shortCircuited)) shouldBe List(Failure.Error("failure"))

    // The accumulating channel keeps both, in the order they were given, and presenting the
    // pair as a single failure reproduces the original's outcome exactly: a shared reason is
    // kept and the messages are joined.
    val accumulated: ResultNec[String] =
      accumulate(toNec(first), toNec(second))((left, right) => s"$left $right")
    failuresOf(accumulated) shouldBe List(Failure.Error("failure"), Failure.Error("fail"))
    collapsedFailure(accumulated).map(_.reason) shouldBe Some(FailureReason.ERROR)
    collapsedFailure(accumulated).map(_.message) shouldBe Some("failure, fail")
  }

  //-------------------------------------------------------------------------
  // An outcome viewed as a sequence of at most one value.

  test("a success iterates over its single value") {
    val success: FailureOr[String] = "Hello".asRight[Failure]
    success.toSeq shouldBe Seq("Hello")
    success.toSeq should have size 1
    success.toSeq.toList shouldBe List("Hello")
  }

  test("a failure iterates over nothing") {
    val failure: FailureOr[String] = Failure.Error("failure").asLeft[String]
    failure.toSeq shouldBe empty
    failure.toSeq shouldBe Seq.empty[String]
    failure.toSeq.toList shouldBe List.empty[String]
  }

  //-------------------------------------------------------------------------
  // A failure, and what can be read from one.
  //
  // The two cases the original had here each started from something thrown - a
  // checked exception in one, an unchecked one in the other - and went on to
  // assert the captured type of that throwable and its rendered stack trace.
  // A failure in this port holds a reason, a message and attributes and
  // nothing else, so what is asserted instead is the part that survives: the
  // reason and the message chosen where the failure was reported, and the two
  // ways of recovering from one.

  test("a failure carries the reason and message chosen at its construction site") {
    val failure = Failure.Error("failure")
    failure.reason shouldBe FailureReason.ERROR
    failure.message shouldBe "failure"
    failure.attributes shouldBe empty

    val outcome: FailureOr[String] = failure.asLeft[String]
    outcome.isLeft shouldBe true
    outcome.isRight shouldBe false
    outcome should beFailure
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("failure")
    outcome.toOption shouldBe None
    failuresOf(toNec(outcome)) shouldBe List(failure)
  }

  test("a failure recovers a value through getOrElse and through fold") {
    val outcome: FailureOr[String] = Failure.Error("failure").asLeft[String]
    outcome.getOrElse("blue") shouldBe "blue"
    outcome.fold(_ => "blue", identity) shouldBe "blue"
    // The recovering form of the original that applied a function to the failure is `fold`
    // with a left branch, and reading the message out of it reproduces that case exactly.
    outcome.fold(_.message, identity) shouldBe "failure"
    outcome.fold(_.reason.name, identity) shouldBe "ERROR"
  }

  test("mapping the failure of a failure replaces it") {
    val base: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val replacement = Failure.Error("failure2")
    val outcome: FailureOr[String] = base.leftMap(_ => replacement)
    outcome shouldBe Left(replacement)
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("failure2")
    outcome.toOption shouldBe None
  }

  test("rebuilding the failure of a failure preserves its class and therefore its reason") {
    val base: FailureOr[String] = Failure.MissingData("failure").asLeft[String]

    // The original's second mapping member reached inside a failure to its parts and left
    // the enclosing reason and message alone. The port has one rebuilding route, and it has
    // the same property by construction: a rebuilt failure is a copy of the failure it came
    // from, so its class - and with it its reason - cannot change.
    val withAttribute: FailureOr[String] = base.leftMap(_.withAttribute("id", "GBLO"))
    withAttribute should beFailureWith(FailureReason.MISSING_DATA)
    withAttribute should haveFailureMessageMatching("failure")
    withAttribute.swap.toOption.map(_.attributes.get("id")) shouldBe Some(Some("GBLO"))

    // On the accumulating channel the rebuild applies to every failure of the chain.
    val rebuilt: ResultNec[String] =
      toNec(base).leftMap(_.map(_.mapMessage(message => s"$message (rebuilt)")))
    rebuilt should beFailureWith(FailureReason.MISSING_DATA)
    rebuilt should haveFailureMessageMatching("failure \\(rebuilt\\)")
    failuresOf(rebuilt) shouldBe List(Failure.MissingData("failure (rebuilt)"))
  }

  test("map, flatMap and foreach are all skipped on a failure and the failure is preserved") {
    val outcome: FailureOr[String] = Failure.Error("failure").asLeft[String]
    outcome.map(mapStrLen) shouldBe Left(Failure.Error("failure"))
    outcome.flatMap(functionStrLen) shouldBe Left(Failure.Error("failure"))
    outcome.foreach(value => fail(s"foreach must not run on a failure, but it saw $value"))
    outcome.map(mapStrLen) should beFailureWith(FailureReason.ERROR)
    outcome.flatMap(functionStrLen) should haveFailureMessageMatching("failure")
    failuresOf(toNec(outcome.map(mapStrLen))) shouldBe List(Failure.Error("failure"))
  }

  test("a failure has no value to read") {
    val outcome: FailureOr[String] = Failure.Error("failure").asLeft[String]
    // Reading the value of a failure threw in the original; here the absence is a value.
    outcome.toOption shouldBe None
    outcome.toSeq shouldBe empty
    outcome shouldNot beSuccess
    assertDoesNotCompile("""
      val readValue: String = Failure.Error("failure").asLeft[String].getValue
      readValue
    """)
  }

  //-------------------------------------------------------------------------
  // The replacement for exception capture.
  //
  // Five cases of the original tested machinery this port does not have: a
  // mapping or combining function was allowed to throw, and the type caught
  // whatever came out and turned it into a failure carrying the exception's
  // type and stack trace. Nothing here catches anything, and the five cases
  // below pin down the contract that takes its place instead.
  //
  // The contract is that a step which can fail says so in its type. It returns
  // a result rather than a value, its failure travels on the value channel, and
  // the ordinary chaining operations carry it to the caller. Two consequences
  // are asserted directly: the failure of such a step is an ordinary `Failure`
  // with the reason and message its author chose, and a step of any kind is
  // simply not reached once an outcome has already failed - which is why a step
  // written here to abort the test if it runs never does.

  test("a combining step that can fail returns its failure on the value channel") {
    val first: FailureOr[String] = "Hello".asRight[Failure]
    val second: FailureOr[String] = "World".asRight[Failure]
    val failingMerge: (String, String) => FailureOr[String] =
      (_, _) => Failure.Error("Ooops").asLeft[String]

    val outcome: FailureOr[String] = (first, second).tupled.flatMap {
      case (left, right) => failingMerge(left, right)
    }
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Ooops")
    outcome.toOption shouldBe None
  }

  test("map takes a total function, so a mapping step that can fail is written to return a result") {
    val success: FailureOr[String] = "success".asRight[Failure]

    // `map` cannot fail: the function it takes returns a value, so there is no failure for
    // it to report and none for the outcome to acquire.
    success.map(mapStrLen) should beSuccess
    success.map(mapStrLen) should haveValue(7)

    // A step that can fail returns a result instead, and is composed with `flatMap`. Its
    // failure is then an ordinary value on the left, indistinguishable from any other.
    val failingStep: String => FailureOr[Int] =
      value => Failure.Error("Big bad error").asLeft[Int].leftMap(_.withAttribute("input", value))
    val outcome: FailureOr[Int] = success.flatMap(failingStep)
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Big bad error")
    outcome.swap.toOption.map(_.attributes.get("input")) shouldBe Some(Some("success"))
  }

  test("a chained step that can fail reports its failure on the accumulating channel too") {
    val success: ResultNec[String] = toNec("success".asRight[Failure])
    val failingStep: String => ResultNec[Int] =
      _ => toNec(Failure.Error("Big bad error").asLeft[Int])

    val outcome: ResultNec[Int] = success.flatMap(failingStep)
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Big bad error")
    failuresOf(outcome) shouldBe List(Failure.Error("Big bad error"))
    collapsedFailure(outcome).map(_.reason) shouldBe Some(FailureReason.ERROR)
  }

  test("a mapping step is never reached once an outcome has failed") {
    val outcome: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val neverMaps: String => Int =
      value => fail(s"the mapping must not run on a failure, but it saw $value")

    outcome.map(neverMaps) shouldBe Left(Failure.Error("failure"))
    outcome.map(neverMaps) should beFailureWith(FailureReason.ERROR)
    outcome.map(neverMaps) should haveFailureMessageMatching("failure")
  }

  test("a chained step is never reached once an outcome has failed") {
    val outcome: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val neverChains: String => FailureOr[Int] =
      value => fail(s"the chained step must not run on a failure, but it saw $value")

    outcome.flatMap(neverChains) shouldBe Left(Failure.Error("failure"))
    outcome.flatMap(neverChains) should beFailureWith(FailureReason.ERROR)
    outcome.flatMap(neverChains) should haveFailureMessageMatching("failure")
  }

  //-------------------------------------------------------------------------
  // Composing the message of a failure.
  //
  // The original built a message from a template carrying `{}` markers and a
  // variable number of arguments, resolved when the failure was constructed.
  // That helper has no counterpart: a message is written with an interpolated
  // string, so the arguments are substituted while this file is compiled.
  //
  // Two of the six cases below state the result of the original directly,
  // because interpolation reproduces them character for character. The other
  // four described what the helper did when the markers and the arguments did
  // not agree in number - it left a marker unfilled, or appended the arguments
  // it could not place in a bracketed list - and neither outcome is reachable
  // here at all. Each of those four therefore asserts the positive consequence:
  // a message composed by interpolation carries no unfilled marker and no
  // appended list, because a mismatch is rejected before the program runs.

  test("an interpolated message with one value reads as the original formatted message") {
    Failure.Error(s"my $colour failure").message shouldBe "my blue failure"
    Failure.Error(s"my $colour failure").reason shouldBe FailureReason.ERROR
    Failure.of(FailureReason.ERROR, s"my $colour failure").message shouldBe "my blue failure"
  }

  test("an interpolated message with two values reads as the original formatted message") {
    Failure.Error(s"my $colour $animal failure").message shouldBe "my blue rabbit failure"
    Failure.of(FailureReason.ERROR, s"my $colour $animal failure").message shouldBe
      "my blue rabbit failure"
  }

  test("an interpolated message never leaves a placeholder unfilled") {
    val message = Failure.Error(s"my $colour $animal failure").message
    message shouldBe "my blue rabbit failure"
    message.contains("{}") shouldBe false

    // The reason it cannot happen is that a placeholder and the value that fills it are one
    // and the same piece of syntax: there is no way to write the former without the latter,
    // and a template whose markers outnumber its arguments is rejected by the compiler.
    assertDoesNotCompile("""
      val underSupplied: String = f"my %s %s failure"
      underSupplied
    """)
    assertDoesNotCompile("""
      val unfilled: String = s"my $colour $thereIsNoSuchArgument failure"
      unfilled
    """)
  }

  test("an interpolated message never appends one surplus argument") {
    val message = Failure.Error(s"my $colour failure").message
    message shouldBe "my blue failure"
    message.contains(" - [") shouldBe false
    message.contains(animal) shouldBe false
  }

  test("an interpolated message never appends several surplus arguments") {
    val message = Failure.Error(s"my $colour failure").message
    message shouldBe "my blue failure"
    message.contains(" - [") shouldBe false
    message.contains(s"[$animal, $vegetable]") shouldBe false
    List(animal, vegetable).foreach(surplus => message.contains(surplus) shouldBe false)
  }

  test("a message with no placeholders never gains an appended argument list") {
    val message = Failure.Error("my failure").message
    message shouldBe "my failure"
    message.contains(" - [") shouldBe false
    List(colour, animal, vegetable).foreach(surplus => message.contains(surplus) shouldBe false)
  }

  //-------------------------------------------------------------------------
  // Moving a failure between outcomes, and the three bridges.
  //
  // The original had a family of factories for building a failed outcome from
  // something that had already failed: another outcome, a failure, one of the
  // items a failure was made of, or an exception carrying one of those items.
  // The port has one failure type and no item type and no exception type, so
  // all four collapse into putting a value on the left of the outcome that
  // needs it. What is worth testing in their place is the part that is not
  // trivial: the three conversions the package object defines between the
  // channels, and that each of them leaves a success alone.

  test("the failure of one outcome is carried into an outcome of another value type") {
    val failure: FailureOr[String] = Failure.Error("my failure").asLeft[String]

    // A failure is re-channelled by folding: the left branch carries the failure over
    // untouched and the right branch supplies a value of the new type. Nothing else of the
    // original outcome survives, which is exactly why the value type is free to change.
    val carried: FailureOr[Int] =
      failure.fold(_.asLeft[Int], value => value.length.asRight[Failure])

    carried should beFailureWith(FailureReason.ERROR)
    carried should haveFailureMessageMatching("my failure")
    carried.swap.toOption shouldBe Some(Failure.Error("my failure"))
    failuresOf(toNec(carried)) should have size 1
  }

  test("a success has no failure to re-channel, and every bridge passes it through unchanged") {
    val success: FailureOr[String] = "Hello".asRight[Failure]

    // Asking the original for the failure of a success threw. Here the answer is an absence.
    success.swap.toOption shouldBe None
    NonEmptyChain.fromSeq(success.swap.toSeq) shouldBe None

    // The three bridges are value-preserving on a success, in both directions.
    toNec(success) shouldBe Right("Hello")
    toValidated(toNec(success)) shouldBe Validated.Valid("Hello")
    toResult(toValidated(toNec(success))) shouldBe Right("Hello")
    toNec(success) should haveValue("Hello")
    toValidated(toNec(success)) should haveValue("Hello")
  }

  test("a failure value builds a failed outcome, and toNec lifts it into a chain of one") {
    val failure = Failure.Error("my failure")
    val outcome: FailureOr[Int] = failure.asLeft[Int]

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("my failure")

    val lifted: ResultNec[Int] = toNec(outcome)
    lifted shouldBe Left(NonEmptyChain.one(failure))
    failuresOf(lifted) shouldBe List(failure)
    lifted should beFailureWith(FailureReason.ERROR)
  }

  test("a failure needs no item wrapper, and the accumulating bridges round-trip both ways") {
    // The original split a failure into items, so that several could be held together. The
    // chain does that here, and a failure is itself the unit rather than a container of one.
    val failure = Failure.Error("my failure")
    val lifted: ResultNec[Int] = toNec(failure.asLeft[Int])
    val accumulating: ValidatedFailures[Int] = toValidated(lifted)

    accumulating should beFailureWith(FailureReason.ERROR)
    toResult(accumulating) shouldBe lifted
    toValidated(toResult(accumulating)) shouldBe accumulating
    failuresOf(toResult(accumulating)) shouldBe List(failure)
    accumulating.fold(chain => chain.toChain.toList, _ => List.empty[Failure]) shouldBe List(failure)
  }

  test("a failure travels on the value channel with no exception carrier") {
    val failure = Failure.Invalid("my failure")
    val outcome: FailureOr[Int] = failure.asLeft[Int]

    outcome.swap.toOption shouldBe Some(failure)
    outcome should beFailureWith(FailureReason.INVALID)

    // The original had an exception type whose whole purpose was to carry a failure across
    // a boundary that could not return one, and a factory for unwrapping it again. Neither
    // exists here: a failure is not throwable, so there is nothing to unwrap.
    assertDoesNotCompile("""
      val thrownFailure: Throwable = Failure.Invalid("my failure")
      thrownFailure
    """)

    // The de-duplication that unwrapping relied on is `collapse`, which describes a failure
    // reported twice exactly once.
    Failure.collapse(NonEmptyChain(failure, failure)) shouldBe failure
  }

  //-------------------------------------------------------------------------
  // An absent value.
  //
  // The original had two factories here, one for a reference that named no
  // value and one for the platform's optional type. The port has one answer to
  // both, because an absence is an `Option` and putting a failure in its place
  // is `toRight`. The two messages of the original are kept verbatim, so a
  // caller reading a log sees the same words it always did.

  test("toRight keeps the value of a present option") {
    val present: Option[Int] = Some(6)
    val outcome: FailureOr[Int] =
      present.toRight(Failure.MissingData("Found null where a value was expected"))

    outcome should beSuccess
    outcome should haveValue(6)
    outcome shouldBe Right(6)
    outcome.toOption shouldBe Some(6)
  }

  test("the vocabulary of the absent-reference failure is retained, and the state it guarded is unrepresentable") {
    // The wording of the original is kept so that a caller reading a log sees the same words
    // it always did. It survives as message text only: the reference this message was about
    // is not something this port can produce, and no code here names one.
    val retainedMessage = "Found null where a value was expected"
    val absent: Option[Int] = Option.empty[Int]
    val outcome: FailureOr[Int] = absent.toRight(Failure.MissingData(retainedMessage))

    absent shouldBe None
    outcome should beFailureWith(FailureReason.MISSING_DATA)
    outcome.swap.toOption.map(_.reason) shouldBe Some(FailureReason.MISSING_DATA)
    outcome.swap.toOption.map(_.message) shouldBe Some(retainedMessage)
    failuresOf(toNec(outcome)) shouldBe List(Failure.MissingData(retainedMessage))

    // The state the original factory guarded against cannot arise: an absence has its own
    // type, and the compiler will not let one stand where a value is required.
    assertDoesNotCompile("""
      val absentValue: Int = Option.empty[Int]
      absentValue
    """)
  }

  test("toRight keeps the value of a non-empty option and composes with the bridge") {
    val present: Option[Int] = Some(6)
    val outcome: FailureOr[Int] =
      present.toRight(Failure.MissingData("Found empty where a value was expected"))

    outcome should beSuccess
    outcome should haveValue(6)
    toNec(outcome) should haveValue(6)
    toValidated(toNec(outcome)) should haveValue(6)
  }

  test("toRight turns an empty option into a missing-data failure carrying the retained message") {
    val retainedMessage = "Found empty where a value was expected"
    val outcome: FailureOr[Int] = Option.empty[Int].toRight(Failure.MissingData(retainedMessage))

    outcome should beFailureWith(FailureReason.MISSING_DATA)
    outcome should haveFailureMessageMatching("Found empty where a value was expected")
    outcome.swap.toOption.map(_.message) shouldBe Some(retainedMessage)
    outcome.toOption shouldBe None
  }

  //-------------------------------------------------------------------------
  // A step that can fail returns an outcome.
  //
  // Nothing in this module throws in order to report a failure, so there is no
  // factory here that runs a supplied block and turns what it threw into a
  // failure. The contract sits on the step rather than on a member: a step that
  // can fail is *written* to return an outcome, so its failure arrives on the
  // value channel as `Left(Failure...)` and there is nothing to catch.
  //
  // The five cases below state that contract, and each is asserted through the
  // members of this package alone - the bridges, the aggregations and the
  // predicates - so that what it exercises is production code and not a helper
  // of this file.

  test("a fallible step written to return an outcome gives a success holding its value") {
    // The step says in its type that it can fail. This run of it does not, so the value is
    // on the right of the outcome and every total accessor yields it.
    val step: String => FailureOr[String] = input => input.asRight[Failure]
    val outcome: FailureOr[String] = step("success")

    outcome should beSuccess
    outcome should haveValue("success")
    outcome.isLeft shouldBe false
    outcome shouldBe Right("success")

    // The value survives the members a caller reaches for next, which is the whole point of
    // getting a value onto this channel.
    sequence(List(toNec(outcome))) should haveValue(List("success"))
    combine(List(toNec(outcome)))(values => values.mkString) should haveValue("success")
  }

  test("a fallible step that cannot produce a value gives an ERROR failure carrying its message") {
    // The step reports what stopped it by returning it. The message is the step's own, and
    // the reason is the general error this module's vocabulary uses for a step that simply
    // could not go on.
    val step: String => FailureOr[String] = _ => Failure.Error("Big bad error").asLeft[String]
    val outcome: FailureOr[String] = step("success")

    outcome should beFailure
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Big bad error")
    outcome.toOption shouldBe None

    // A failure is a value, so there is nothing to catch in order to see it: it is visible
    // to the predicates of this package, which is how a caller asks whether a step failed.
    anyFailures(List(outcome)) shouldBe true
    countFailures(List(outcome)) shouldBe 1
  }

  test("an outcome nested in an outcome flattens to that success") {
    // A step that itself returns an outcome, reached from a step that already returns one,
    // leaves two channels to be flattened into one. The chaining the standard algebra
    // already has does that, so no member of this package is needed for it.
    val nested: FailureOr[FailureOr[String]] = "success".asRight[Failure].asRight[Failure]
    val outcome: FailureOr[String] = nested.flatMap(identity)

    outcome should beSuccess
    outcome should haveValue("success")
    outcome shouldBe Right("success")
  }

  test("an outcome nested in an outcome flattens to that failure") {
    val nested: FailureOr[FailureOr[String]] =
      Failure.Error("Something failed").asLeft[String].asRight[Failure]
    val outcome: FailureOr[String] = nested.flatMap(identity)

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Something failed")
    outcome.toOption shouldBe None
  }

  test("a step that reports its own failure hands its caller one channel rather than two") {
    // A step that reports its own failure by returning it leaves its caller one way for a
    // step to fail rather than two: the failure it reports arrives on the channel, and in
    // the shape, its inputs' failures arrive on.
    val step: List[String] => FailureOr[String] =
      _ => Failure.Error("Big bad error").asLeft[String]
    val nested: FailureOr[FailureOr[String]] = step(List("success")).asRight[Failure]
    val outcome: FailureOr[String] = nested.flatMap(identity)

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Big bad error")
    outcome.toOption shouldBe None
    failuresOf(toNec(outcome)) shouldBe List(Failure.Error("Big bad error"))

    // The same step driven through the aggregation of this package over inputs that all
    // succeeded: its own failure is the whole answer, on the one channel.
    val successes: List[ResultNec[String]] = List(toNec(success1), toNec(success2))
    val fallible: List[String] => ResultNec[String] = values => toNec(step(values))

    failuresOf(flatCombine(successes)(fallible)) shouldBe List(Failure.Error("Big bad error"))
    flatCombine(successes)(fallible) should beFailureWith(FailureReason.ERROR)
    flatCombine(successes)(fallible) should haveFailureMessageMatching("Big bad error")
  }

  //-------------------------------------------------------------------------
  // The three predicates over a collection of outcomes.
  //
  // The original offered each of these twice, once taking a variable number of
  // arguments and once taking a collection, and had a case for each form. The
  // port offers the collection form alone: a Scala collection literal is as
  // short to write at a call site as a variable argument list, and the two
  // overloads could not coexist anyway once their element types erase to the
  // same thing. Both cases of each pair are therefore kept and both call the
  // one member, the first building its collection where the original wrote its
  // arguments - which is what the change amounts to at a call site.
  //
  // All three are generic over any `Either`, not just over the two aliases of
  // this module, so each case also exercises the accumulating channel.

  test("anyFailures answers over outcomes listed at the call site") {
    anyFailures(List(failure1, failure2)) shouldBe true
    anyFailures(List(failure1, success1)) shouldBe true
    anyFailures(List(success1, success2)) shouldBe false
  }

  test("anyFailures answers over a collection of outcomes") {
    val bothFailed: List[FailureOr[String]] = List(failure1, failure2)
    val oneFailed: List[FailureOr[String]] = List(failure1, success1)
    val noneFailed: List[FailureOr[String]] = List(success1, success2)

    anyFailures(bothFailed) shouldBe true
    anyFailures(oneFailed) shouldBe true
    anyFailures(noneFailed) shouldBe false

    // An empty collection holds no failure, as the member documents.
    anyFailures(List.empty[FailureOr[String]]) shouldBe false

    // The same answers on the accumulating channel, the predicate being generic.
    anyFailures(oneFailed.map(toNec)) shouldBe true
    anyFailures(noneFailed.map(toNec)) shouldBe false
  }

  test("countFailures counts the failures among outcomes listed at the call site") {
    countFailures(List(failure1, failure2)) shouldBe 2
    countFailures(List(failure1, success1)) shouldBe 1
    countFailures(List(success1, success2)) shouldBe 0
  }

  test("countFailures counts the failures in a collection of outcomes") {
    val bothFailed: List[FailureOr[String]] = List(failure1, failure2)
    val oneFailed: List[FailureOr[String]] = List(failure1, success1)
    val noneFailed: List[FailureOr[String]] = List(success1, success2)

    countFailures(bothFailed) shouldBe 2
    countFailures(oneFailed) shouldBe 1
    countFailures(noneFailed) shouldBe 0
    countFailures(List.empty[FailureOr[String]]) shouldBe 0

    // The count of the original was a 64-bit number because the stream it counted over
    // produced one. This counts with the collection library of the language instead, and
    // returns the width that library uses, without widening it on the way out.
    countFailures(bothFailed.map(toNec)) shouldBe 2
    countFailures(noneFailed.map(toNec)) shouldBe 0
  }

  test("allSuccessful holds only when every outcome listed at the call site succeeded") {
    allSuccessful(List(failure1, failure2)) shouldBe false
    allSuccessful(List(failure1, success1)) shouldBe false
    allSuccessful(List(success1, success2)) shouldBe true
  }

  test("allSuccessful holds only when every outcome in a collection succeeded") {
    val bothFailed: List[FailureOr[String]] = List(failure1, failure2)
    val oneFailed: List[FailureOr[String]] = List(failure1, success1)
    val noneFailed: List[FailureOr[String]] = List(success1, success2)

    allSuccessful(bothFailed) shouldBe false
    allSuccessful(oneFailed) shouldBe false
    allSuccessful(noneFailed) shouldBe true

    // An empty collection is all successes, as every universally quantified predicate is.
    allSuccessful(List.empty[FailureOr[String]]) shouldBe true

    allSuccessful(noneFailed.map(toNec)) shouldBe true
    allSuccessful(oneFailed.map(toNec)) shouldBe false

    // The three predicates agree with one another on every input, which is the invariant a
    // caller relies on when it chooses between them.
    List(bothFailed, oneFailed, noneFailed, List.empty[FailureOr[String]]).foreach { results =>
      allSuccessful(results) shouldBe !anyFailures(results)
      anyFailures(results) shouldBe (countFailures(results) > 0)
    }
  }

  //-------------------------------------------------------------------------
  // Gathering the failures reported by several outcomes.
  //
  // The original built one failed outcome out of many, taking the failures of
  // whichever inputs had failed and ignoring the rest. It had the same pair of
  // forms as the predicates above, and it threw when handed inputs that had
  // all succeeded - there being no failure to build the result from.
  //
  // The port answers with an `Option` of a non-empty chain, which is the whole
  // difference: the case the original could only report by throwing is the case
  // where the answer is `None`, and a caller decides what that means. This is
  // the clearest example in this file of what the port does with a partial
  // operation - it makes the missing answer representable rather than fatal.

  test("the failures of a mixed collection are extracted independently of their positions") {
    val extracted = extractFailures(List(success1, success2, failure1, failure2))

    extracted.map(_.toChain.toList) shouldBe
      Some(List(Failure.MissingData("failure 1"), Failure.Error("failure 2")))
    extracted.map(_.toChain.toList.size) shouldBe Some(2)
    // Both failures are there, and presenting them as one describes both.
    extracted.map(chain => Failure.collapse(chain).reason) shouldBe Some(FailureReason.MULTIPLE)
    extracted.map(chain => Failure.collapse(chain).message) shouldBe Some("failure 1, failure 2")
  }

  test("a different interleaving of the same outcomes extracts the same failures") {
    val grouped = extractFailures(List(success1, success2, failure1, failure2))
    val interleaved = extractFailures(List(success1, failure1, success2, failure2))

    // The successes sit in different places, and the failures that come out do not depend
    // on where they sat: that is the property the original asserted with two cases.
    interleaved.map(_.toChain.toList.toSet) shouldBe grouped.map(_.toChain.toList.toSet)
    interleaved.map(_.toChain.toList) shouldBe
      Some(List(Failure.MissingData("failure 1"), Failure.Error("failure 2")))
    interleaved.map(Failure.collapse) shouldBe grouped.map(Failure.collapse)
  }

  test("the failures of an explicit collection are extracted in the order they were given") {
    // Writing the collection out is what showed, in the original, why the member had the
    // signature it did. Here it shows that the order of the chain is the order of the input.
    val results: List[FailureOr[String]] = List(success1, success2, failure1, failure2)
    val extracted = extractFailures(results)

    extracted.map(_.head) shouldBe Some(Failure.MissingData("failure 1"))
    extracted.map(_.toChain.toList.last) shouldBe Some(Failure.Error("failure 2"))
    extracted.map(_.toChain.toList) shouldBe
      Some(List(Failure.MissingData("failure 1"), Failure.Error("failure 2")))
    countFailures(results) shouldBe 2
  }

  test("extracting the failures of outcomes listed at the call site that all succeeded yields no chain") {
    // The original threw here. There is nothing exceptional about the situation, though: it
    // is simply the answer that no failure was reported, and a chain cannot be empty, so the
    // answer is the absent one.
    extractFailures(List(success1, success2)) shouldBe None
    allSuccessful(List(success1, success2)) shouldBe true
  }

  test("extracting the failures of a collection that all succeeded yields no chain") {
    val results: List[FailureOr[String]] = List(success1, success2)

    extractFailures(results) shouldBe None
    extractFailures(results).map(Failure.collapse) shouldBe None
    anyFailures(results) shouldBe false
    // An empty input is the same answer for the same reason.
    extractFailures(List.empty[FailureOr[String]]) shouldBe None
  }

  //-------------------------------------------------------------------------
  // Aggregating a collection of outcomes.
  //
  // Three members do this: `sequence` turns a collection of outcomes into an
  // outcome of a collection, and `combine` and `flatCombine` follow it with a
  // function over the values, total in the first case and fallible in the
  // second. All three accumulate rather than stop at the first failure, and all
  // three require the failure type to be combinable - which a chain is and a
  // single failure deliberately is not. Every case here therefore runs on the
  // accumulating channel and lifts its inputs with `toNec` first, and the first
  // case shows that doing otherwise is not merely discouraged but unwriteable.
  //
  // `withAdditionalFailures`, the remaining member of the package object, has
  // no form over this channel: it takes and returns a partial success, so it
  // belongs with the cases for that type rather than here.

  test("aggregating a mixed collection accumulates every failure and presents them as MULTIPLE") {
    val results: List[ResultNec[String]] = List(
      toNec(success1),
      toNec("success 2".asRight[Failure]),
      toNec(failure1),
      toNec(failure2))

    val sequenced: ResultNec[List[String]] = sequence(results)
    sequenced should beFailure
    failuresOf(sequenced) shouldBe
      List(Failure.MissingData("failure 1"), Failure.Error("failure 2"))
    collapsedFailure(sequenced).map(_.reason) shouldBe Some(FailureReason.MULTIPLE)

    // `combine` adds a function over the values, which a failed input means is never reached.
    val combined: ResultNec[String] = combine(results)(_.mkString(", "))
    combined should beFailure
    collapsedFailure(combined).map(_.reason) shouldBe Some(FailureReason.MULTIPLE)
    failuresOf(combined) should have size 2

    // Accumulation is only defined where the failure type can be combined, and a single
    // failure cannot be: asking for two of them to be merged into one is not expressible.
    assertDoesNotCompile("""
      val notCombinable = sequence(List(failure1, failure2))
      notCombinable
    """)
  }

  test("combining a collection of successes applies the function to every value") {
    val results: List[ResultNec[Int]] = List(1, 2, 3, 4).map(value => toNec(value.asRight[Failure]))

    val combined: ResultNec[String] =
      combine(results)(values => s"res${values.foldLeft(1)((left, right) => left * right)}")

    combined should beSuccess
    combined should haveValue("res24")
    // The function sees every value, in the order the collection held them.
    combine(results)(values => values) should haveValue(List(1, 2, 3, 4))

    // An empty collection has no failure to report, so the function still runs - on no
    // values - and its answer is the answer. That it ran is read off the value it returned,
    // which is the count of the values it was handed.
    val overNothing: ResultNec[Int] = combine(List.empty[ResultNec[Int]])(values => values.size)
    overNothing shouldBe Right(0)
    overNothing should beSuccess
    overNothing should haveValue(0)
    combine(List.empty[ResultNec[Int]])(values => values) should haveValue(List.empty[Int])
  }

  test("combine takes a total function, so a combiner that can fail is handed to flatCombine") {
    val results: List[ResultNec[Int]] = List(1, 2, 3, 4).map(value => toNec(value.asRight[Failure]))

    // A total function cannot fail, so `combine` over successful inputs always succeeds.
    // The original also caught whatever the function threw and turned it into a failure;
    // nothing here does that, and nothing needs to.
    combine(results)(values => values.sum) should haveValue(10)
    combine(results)(values => values.sum) should beSuccess

    // A step that can fail says so by returning an outcome, and `flatCombine` is where such
    // a function belongs. Its failure then arrives on the same channel as any other.
    val fallible: List[Int] => ResultNec[String] =
      _ => toNec(Failure.Error("Ooops").asLeft[String])
    val outcome: ResultNec[String] = flatCombine(results)(fallible)
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Ooops")
  }

  test("flat-combining a mixed collection accumulates every failure and presents them as MULTIPLE") {
    val results: List[ResultNec[String]] = List(
      toNec(success1),
      toNec("success 2".asRight[Failure]),
      toNec(failure1),
      toNec(failure2))

    val outcome: ResultNec[String] =
      flatCombine(results)(values => toNec(values.mkString(", ").asRight[Failure]))

    outcome should beFailure
    failuresOf(outcome) should have size 2
    collapsedFailure(outcome).map(_.reason) shouldBe Some(FailureReason.MULTIPLE)
    collapsedFailure(outcome).map(_.message) shouldBe Some("failure 1, failure 2")
  }

  test("a combiner that fails surfaces its own failure with its own reason") {
    val results: List[ResultNec[Int]] = List(1, 2, 3, 4).map(value => toNec(value.asRight[Failure]))

    val outcome: ResultNec[String] =
      flatCombine(results)(_ => toNec(Failure.CalculationFailed("Could not do it").asLeft[String]))

    outcome should beFailureWith(FailureReason.CALCULATION_FAILED)
    outcome should haveFailureMessageMatching("Could not do it")
    failuresOf(outcome) shouldBe List(Failure.CalculationFailed("Could not do it"))
    collapsedFailure(outcome).map(_.reason) shouldBe Some(FailureReason.CALCULATION_FAILED)

    // Over an empty collection there is no input failure to stop at, so the combiner runs
    // on no values and its own failure is what propagates - the only way its failure could
    // have arrived here, so this also shows that it ran.
    val overNothing: ResultNec[String] =
      flatCombine(List.empty[ResultNec[Int]])(_ => toNec(Failure.CalculationFailed("empty").asLeft[String]))

    overNothing should beFailureWith(FailureReason.CALCULATION_FAILED)
    overNothing should haveFailureMessageMatching("empty")
    failuresOf(overNothing) shouldBe List(Failure.CalculationFailed("empty"))
    overNothing.toOption shouldBe None
  }

  test("a combiner that succeeds produces its value") {
    val results: List[ResultNec[Int]] = List(1, 2, 3, 4).map(value => toNec(value.asRight[Failure]))

    val outcome: ResultNec[String] = flatCombine(results)(values =>
      toNec(s"res${values.foldLeft(1)((left, right) => left * right)}".asRight[Failure]))

    outcome should beSuccess
    outcome should haveValue("res24")

    // The success direction over an empty collection: the combiner runs on no values and
    // the value it produces is the value of the whole aggregation.
    val overNothing: ResultNec[String] = flatCombine(List.empty[ResultNec[Int]])(values =>
      toNec(s"res${values.foldLeft(1)((left, right) => left * right)}".asRight[Failure]))

    overNothing should beSuccess
    overNothing should haveValue("res1")
    overNothing shouldBe Right("res1")
  }

  test("the failures of the inputs and the failure of the combiner never mix") {
    val successes: List[ResultNec[Int]] =
      List(1, 2, 3, 4).map(value => toNec(value.asRight[Failure]))
    val failing: List[Int] => ResultNec[String] =
      _ => toNec(Failure.Error("Ooops").asLeft[String])

    // Over successful inputs the combiner's own failure is the whole answer.
    failuresOf(flatCombine(successes)(failing)) shouldBe List(Failure.Error("Ooops"))

    // Add a failed input and the combiner is not reached at all, so only the input failures
    // are reported. The two sets are never merged, whichever way round the inputs are.
    val withFailure: List[ResultNec[Int]] =
      successes :+ toNec(Failure.MissingData("failure 1").asLeft[Int])
    failuresOf(flatCombine(withFailure)(failing)) shouldBe
      List(Failure.MissingData("failure 1"))
    flatCombine(withFailure)(failing) should beFailureWith(FailureReason.MISSING_DATA)
    flatCombine(withFailure)(failing) shouldNot haveFailureMessageMatching("Ooops")
  }

  test("sequence turns a collection of successes into a success holding the values in order") {
    val results: List[ResultNec[Int]] = List(1, 2, 3).map(value => toNec(value.asRight[Failure]))

    val combined: ResultNec[List[Int]] = sequence(results)
    combined should beSuccess
    combined should haveValue(List(1, 2, 3))
    combined shouldBe Right(List(1, 2, 3))
    // An empty collection sequences to an empty list, there being no failure to report.
    sequence(List.empty[ResultNec[Int]]) shouldBe Right(List.empty[Int])
  }

  test("sequence over a mixed collection accumulates the failures and joins their messages") {
    val results: List[ResultNec[Int]] = List(
      toNec(1.asRight[Failure]),
      toNec(Failure.MissingData("failure 1").asLeft[Int]),
      toNec(Failure.Error("failure 2").asLeft[Int]))

    val combined: ResultNec[List[Int]] = sequence(results)
    combined should beFailure
    combined.toOption shouldBe None
    failuresOf(combined) shouldBe
      List(Failure.MissingData("failure 1"), Failure.Error("failure 2"))

    // The original produced a single failure holding both items. Presenting the chain the
    // same way reaches the same outcome: the reasons differ, so the reason is `MULTIPLE`,
    // and the messages are joined in the order they were reported.
    collapsedFailure(combined) shouldBe Some(Failure.Multiple("failure 1, failure 2"))
    collapsedFailure(combined).map(_.reason) shouldBe Some(FailureReason.MULTIPLE)
    collapsedFailure(combined).map(_.message) shouldBe Some("failure 1, failure 2")
  }

  //-------------------------------------------------------------------------
  // Presenting several failures as one.
  //
  // These three cases of the original are properties of `Failure.collapse`, and
  // the spec of the failure type is where they are asserted in depth. They are
  // kept here as well, reached the way a caller of this channel reaches them -
  // gather the failures of several outcomes, then collapse them - so that the
  // route from a collection of outcomes to a single reported failure is itself
  // covered and not merely the function at the end of it.

  test("collapsing the same failure reported twice describes it once") {
    val failure = Failure.MissingData("failure")
    val extracted = extractFailures(List(failure.asLeft[String], failure.asLeft[String]))

    // Both failures are gathered: de-duplication happens when they are presented as one,
    // not when they are collected, so nothing is lost on the way.
    extracted.map(_.toChain.toList.size) shouldBe Some(2)

    extracted.map(Failure.collapse) shouldBe Some(failure)
    extracted.map(chain => Failure.collapse(chain).message) shouldBe Some("failure")
    extracted.map(chain => Failure.collapse(chain).reason) shouldBe Some(FailureReason.MISSING_DATA)
  }

  test("collapsing failures that agree on a reason keeps that reason") {
    val results: List[FailureOr[String]] = List(
      Failure.MissingData("message 1").asLeft[String],
      Failure.MissingData("message 2").asLeft[String],
      Failure.MissingData("message 3").asLeft[String])

    val collapsed = extractFailures(results).map(Failure.collapse)
    collapsed.map(_.reason) shouldBe Some(FailureReason.MISSING_DATA)
    collapsed.map(_.message) shouldBe Some("message 1, message 2, message 3")
    collapsed shouldBe Some(Failure.MissingData("message 1, message 2, message 3"))
  }

  test("collapsing failures that disagree on a reason gives MULTIPLE") {
    val results: List[FailureOr[String]] = List(
      Failure.MissingData("message 1").asLeft[String],
      Failure.CalculationFailed("message 2").asLeft[String],
      Failure.Error("message 3").asLeft[String])

    val collapsed = extractFailures(results).map(Failure.collapse)
    collapsed.map(_.reason) shouldBe Some(FailureReason.MULTIPLE)
    collapsed.map(_.message) shouldBe Some("message 1, message 2, message 3")
    collapsed shouldBe Some(Failure.Multiple("message 1, message 2, message 3"))
  }

  //-------------------------------------------------------------------------
  // Where a failure's reason and message come from.
  //
  // Four cases of the original derived a failure from a thrown exception, in
  // the four combinations of a supplied reason and a supplied message. The port
  // derives nothing: the reason is fixed by the class of failure chosen, and
  // the message is written at the site that reports it. The four cases below
  // assert that replacement, and the two that supplied a reason do it across
  // every reason there is rather than the one the original happened to pick.

  test("a failure carries the message chosen where it is reported") {
    val outcome: FailureOr[String] = Failure.Error("something went wrong").asLeft[String]

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("something went wrong")
    outcome.swap.toOption.map(_.message) shouldBe Some("something went wrong")
    // Nothing is inherited from anywhere: a failure with no attributes has none.
    outcome.swap.toOption.map(_.attributes.isEmpty) shouldBe Some(true)
  }

  test("the reporting site supplies its own message rather than inheriting one") {
    val outcome: FailureOr[String] = Failure.Error("my message").asLeft[String]

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("my message")

    // The one route by which a message changes afterwards is an explicit rebuild, which
    // keeps the class of the failure and therefore its reason.
    val rewritten: FailureOr[String] = outcome.leftMap(_.mapMessage(_ => "something went wrong"))
    rewritten should haveFailureMessageMatching("something went wrong")
    rewritten should beFailureWith(FailureReason.ERROR)
  }

  test("every one of the ten reasons is reachable through Failure.of") {
    val reasons = Table("reason", FailureReason.values.toList: _*)

    forAll(reasons) { reason =>
      val failure = Failure.of(reason, "something went wrong")
      failure.reason shouldBe reason
      failure.message shouldBe "something went wrong"
      failure.attributes shouldBe empty
      failure.asLeft[String] should beFailureWith(reason)
    }

    // The family is closed and the mapping from a reason to its failure is total, so the
    // round trip holds for every member and for no other value.
    FailureReason.values.toList.map(reason => Failure.of(reason, "m").reason) shouldBe
      FailureReason.values.toList
    FailureReason.values.toList should have size 10
  }

  test("a reason and a message are chosen together at the reporting site") {
    val reasons = Table("reason", FailureReason.values.toList: _*)

    forAll(reasons) { reason =>
      val message = s"my message for ${reason.name}"
      val failure = Failure.of(reason, message)
      failure.reason shouldBe reason
      failure.message shouldBe message

      val outcome: FailureOr[String] = failure.asLeft[String]
      outcome should beFailureWith(reason)
      outcome should haveFailureMessageMatching(s"my message for ${reason.name}")
      failuresOf(toNec(outcome)) shouldBe List(failure)
    }
  }

  //-------------------------------------------------------------------------
  // What a failure does not hold.
  //
  // The original captured the stack of the call that reported a failure and
  // exposed it, together with the type of any throwable behind it. Neither is
  // retained: a failure is a value, it is not an event, and the two cases below
  // assert the whole of what it carries rather than any part of what it used to.

  test("a failure holds a reason, a message and attributes and nothing else") {
    val failure = Failure.Invalid("my big bad failure").withAttribute("definition", "P3M")

    failure.reason shouldBe FailureReason.INVALID
    failure.message shouldBe "my big bad failure"
    failure.attributes shouldBe Map("definition" -> "P3M")

    assertDoesNotCompile("""
      val trace: String = Failure.Invalid("my big bad failure").stackTrace
      trace
    """)
    assertDoesNotCompile("""
      val cause: Class[_] = Failure.Invalid("my big bad failure").causeType
      cause
    """)
  }

  test("a failure renders as its reason and message, with no trace") {
    val failure = Failure.of(FailureReason.INVALID, "my big bad failure")

    failure.reason shouldBe FailureReason.INVALID
    failure.message shouldBe "my big bad failure"
    failure.attributes shouldBe empty
    // This is the rendering the original produced for the part of a failure it kept, and
    // there is nothing after it because there is nothing else held.
    failure.show shouldBe "INVALID: my big bad failure"
    // The rendering is published for the type as a whole rather than per member, so it is
    // reached through the type of the family - naming a member does not narrow it away.
    val named: Failure = Failure.Invalid("my big bad failure")
    named.show shouldBe "INVALID: my big bad failure"
    named shouldBe failure
  }

  //-------------------------------------------------------------------------
  // The two states that cannot be built.
  //
  // The original was an immutable bean whose builder could be handed a value,
  // a failure, both or neither, so two of its four combinations had to be
  // rejected when the bean was assembled. There is no builder here and no
  // rejection to test: the outcome type has exactly two cases and is sealed, so
  // the two invalid combinations are not states the program can reach.

  test("an outcome with neither a value nor a failure cannot be built") {
    val outcome: FailureOr[String] = "A".asRight[Failure]

    // Exactly one of the two holds, always, and the compiler checks that a match over them
    // is complete - which is the same fact stated the other way round.
    (outcome.isLeft != outcome.isRight) shouldBe true
    outcome.fold(_ => "failure", _ => "value") shouldBe "value"

    assertDoesNotCompile("""
      val neither: FailureOr[String] = new Either[Failure, String] {}
      neither
    """)
  }

  test("an outcome holding both a value and a failure cannot be built") {
    val outcome: FailureOr[String] = "A".asRight[Failure]

    outcome.toOption shouldBe Some("A")
    outcome.swap.toOption shouldBe None

    // The type that can hold both is a different type on purpose, and it is the one the
    // fourth alias of this module names. An outcome of that shape cannot be passed off as
    // one of this shape, so the combination the builder had to reject is unrepresentable.
    assertDoesNotCompile("""
      val both: FailureOr[String] = cats.data.Ior.both(Failure.CalculationFailed("Fail"), "A")
      both
    """)
  }

  //-------------------------------------------------------------------------
  // Equality.

  test("equality and hashing of outcomes follow the value or the failure they hold") {
    val a1: FailureOr[String] = Failure.MissingData("Problem").asLeft[String]
    val a2: FailureOr[String] = Failure.MissingData("Problem").asLeft[String]
    val b: FailureOr[String] = Failure.Error("message 2").asLeft[String]
    val c: FailureOr[String] = "Foo".asRight[Failure]
    val d: FailureOr[String] = "Bar".asRight[Failure]

    a1 shouldBe a1
    a1 shouldBe a2
    a1 should not be b
    a1 should not be c
    a1 should not be d

    b should not be a1
    b shouldBe b
    b should not be c

    c should not be a1
    c should not be b
    c shouldBe c
    c should not be d

    d should not be c
    d shouldBe d

    // Equal outcomes hash equally, which is what makes one usable as a key.
    a1.hashCode shouldBe a2.hashCode
    Hash[Failure].hash(Failure.MissingData("Problem")) shouldBe
      Hash[Failure].hash(Failure.MissingData("Problem"))

    // The structural equality above and the equality the typeclass publishes agree, there
    // being exactly one equality-bearing instance for a failure.
    Eq[FailureOr[String]].eqv(a1, a2) shouldBe true
    Eq[FailureOr[String]].eqv(a1, b) shouldBe false
    Eq[FailureOr[String]].eqv(c, d) shouldBe false
    Eq[FailureOr[String]].eqv(a1, c) shouldBe false
  }

  //-------------------------------------------------------------------------
  // Equality over generated outcomes.
  //
  // The case above fixes five outcomes and asserts the table of comparisons
  // between them, which is what the reflective sweep of the original did for
  // the bean it walked. A table of five values states no law, though, and the
  // laws are what the rest of this port relies on: an outcome is compared
  // against an expected value in almost every spec of both modules, is held in
  // a set, and reaches a map as a key. The cases below assert those laws over
  // outcomes drawn from the generators of this module rather than over values
  // chosen here - reflexivity, symmetry and transitivity of equality, the
  // agreement of hashing and of the published instances with it, the
  // distinction of the two shapes, and the structural round trip - and they do
  // it on all three channels this file names, because the accumulating channels
  // carry an ordered chain on their failure side and an equality that ignored
  // that order would pass every fixed case in this file.
  //
  // Transitivity needs three values that can genuinely be equal, which three
  // independent draws never are, so the second and third are rebuilt from the
  // parts of the first by the helpers below.

  /**
   * Rebuilds a failure from the reason, the message and the attributes it holds.
   *
   * Those three are the whole of what a failure holds, so the rebuilt failure shares nothing
   * with the one it came from beyond the strings inside it, and an assertion that the two are
   * equal is an assertion about their parts rather than about a reference. `Failure.of` maps
   * the reason back onto the member of the family that carries it, so the class of the
   * failure survives the trip as well.
   */
  private def rebuiltFailure(failure: Failure): Failure =
    Failure.of(failure.reason, failure.message, failure.attributes)

  /** Rebuilds an outcome on the value channel from the one side it holds. */
  private def rebuiltOutcome[A](outcome: FailureOr[A]): FailureOr[A] =
    outcome.fold(failure => rebuiltFailure(failure).asLeft[A], value => value.asRight[Failure])

  /**
   * Rebuilds an outcome on the accumulating channel, failure by failure, in order.
   *
   * The chain is taken apart into the non-empty list of its failures and rebuilt from it, so
   * the order of the failures is carried by the rebuild rather than preserved by sharing the
   * chain, and the non-emptiness the type guarantees is never in question - there is no
   * absent value to unwrap on the way back.
   */
  private def rebuiltChainOutcome[A](outcome: ResultNec[A]): ResultNec[A] =
    outcome.fold(
      failures =>
        NonEmptyChain.fromNonEmptyList(failures.toNonEmptyList.map(rebuiltFailure)).asLeft[A],
      value => value.asRight[NonEmptyChain[Failure]])

  /**
   * Rebuilds an accumulating outcome through the two conversions this package declares.
   *
   * Going out through `toResult` and back through `toValidated` is the route a validating
   * factory of this library takes, so this helper rebuilds such an outcome the way the
   * library itself does rather than by reaching into the type.
   */
  private def rebuiltAccumulating[A](outcome: ValidatedFailures[A]): ValidatedFailures[A] =
    toValidated(rebuiltChainOutcome(toResult(outcome)))

  test("equality over arbitrary outcomes is reflexive, symmetric and transitive") {
    forAll { (outcome: FailureOr[String]) =>
      val copy: FailureOr[String] = rebuiltOutcome(outcome)
      val furtherCopy: FailureOr[String] = rebuiltOutcome(copy)

      // Reflexivity, then symmetry and transitivity over three values that are equal by
      // construction rather than by having been written out three times.
      outcome shouldBe outcome
      outcome shouldBe copy
      copy shouldBe outcome
      copy shouldBe furtherCopy
      outcome shouldBe furtherCopy
    }

    // Symmetry has to hold of outcomes that are not equal as well, where it is the answer
    // `false` that has to be the same in both directions, and that needs two independent
    // draws rather than a value and its rebuild.
    forAll { (first: FailureOr[String], second: FailureOr[String]) =>
      (first == second) shouldBe (second == first)
      Eq[FailureOr[String]].eqv(first, second) shouldBe Eq[FailureOr[String]].eqv(second, first)
    }
  }

  test("equal arbitrary outcomes hash alike and the published instances agree with equality") {
    forAll { (outcome: FailureOr[String]) =>
      val copy: FailureOr[String] = rebuiltOutcome(outcome)

      // Hashing agrees with equality, which is what makes an outcome usable as a key. Both
      // hashings are asserted: the universal one a hash-based collection uses, and the one
      // the `Hash` instance publishes for code that works through the typeclass.
      copy.hashCode shouldBe outcome.hashCode
      Hash[FailureOr[String]].hash(copy) shouldBe Hash[FailureOr[String]].hash(outcome)
      Set(outcome, copy) should have size 1
    }

    forAll { (first: FailureOr[String], second: FailureOr[String]) =>
      // The agreement runs in both directions: the instance holds exactly where structural
      // equality holds, so neither can drift from the other without this failing.
      Eq[FailureOr[String]].eqv(first, second) shouldBe (first == second)
      if (first == second) {
        first.hashCode shouldBe second.hashCode
        Hash[FailureOr[String]].hash(first) shouldBe Hash[FailureOr[String]].hash(second)
      } else {
        Set(first, second) should have size 2
      }
    }
  }

  test("a failed outcome is never equal to a successful one, whatever each of them carries") {
    forAll { (failure: Failure, value: String) =>
      val failed: FailureOr[String] = failure.asLeft[String]
      val succeeded: FailureOr[String] = value.asRight[Failure]

      (failed == succeeded) shouldBe false
      (succeeded == failed) shouldBe false
      Eq[FailureOr[String]].eqv(failed, succeeded) shouldBe false
      failed should beFailureWith(failure.reason)
      succeeded should haveValue(value)
    }

    // Stated the other way round, over two arbitrary outcomes: equal outcomes are always of
    // the same shape, which is the fact the two cases of the sealed type give this channel.
    forAll { (first: FailureOr[String], second: FailureOr[String]) =>
      if (first == second) {
        first.isLeft shouldBe second.isLeft
        first.isRight shouldBe second.isRight
      } else {
        // And equality is no finer than what an outcome holds: two outcomes that are not
        // equal differ in their shape, in the value they carry or in the failure they carry.
        val sameParts: Boolean =
          first.toOption == second.toOption && first.swap.toOption == second.swap.toOption
        sameParts shouldBe false
      }
    }
  }

  test("taking an arbitrary outcome apart and rebuilding it from its parts yields an equal outcome") {
    forAll { (outcome: FailureOr[String]) =>
      val rebuilt: FailureOr[String] = rebuiltOutcome(outcome)

      rebuilt shouldBe outcome
      // The parts survive the trip, which is what makes the equality above a statement about
      // the value rather than about the route taken to it. A failure keeps its reason - and
      // with it the member of the family that carries it - along with its message and every
      // one of its attributes, so the whole failure is asserted as well as each part of it.
      rebuilt.toOption shouldBe outcome.toOption
      rebuilt.swap.toOption shouldBe outcome.swap.toOption
      rebuilt.swap.toOption.map(_.reason) shouldBe outcome.swap.toOption.map(_.reason)
      rebuilt.swap.toOption.map(_.message) shouldBe outcome.swap.toOption.map(_.message)
      rebuilt.swap.toOption.map(_.attributes) shouldBe outcome.swap.toOption.map(_.attributes)
    }
  }

  test("the conversions between the channels preserve the value and the ordered failures of any outcome") {
    forAll { (outcome: FailureOr[String]) =>
      val chained: ResultNec[String] = toNec(outcome)
      val accumulating: ValidatedFailures[String] = toValidated(chained)
      val back: ResultNec[String] = toResult(accumulating)

      // The three conversions of this package compose into the identity on the accumulating
      // channel, and lifting a single failure onto that channel gives a chain of exactly one.
      back shouldBe chained
      chained.toOption shouldBe outcome.toOption
      failuresOf(chained) shouldBe outcome.swap.toOption.toList
      failuresOf(back) shouldBe failuresOf(chained)
    }

    forAll { (outcome: ResultNec[String]) =>
      val roundTripped: ResultNec[String] = toResult(toValidated(outcome))

      // An outcome that already carries a chain keeps every failure of it, in order, which
      // is the property the accumulating shape exists for.
      roundTripped shouldBe outcome
      roundTripped.toOption shouldBe outcome.toOption
      failuresOf(roundTripped) shouldBe failuresOf(outcome)
    }
  }

  test("equality and hashing on the accumulating channel follow the ordered chain an outcome holds") {
    forAll { (outcome: ResultNec[String]) =>
      val copy: ResultNec[String] = rebuiltChainOutcome(outcome)
      val furtherCopy: ResultNec[String] = rebuiltChainOutcome(copy)

      outcome shouldBe copy
      copy shouldBe outcome
      copy shouldBe furtherCopy
      outcome shouldBe furtherCopy
      copy.hashCode shouldBe outcome.hashCode
      Hash[ResultNec[String]].hash(copy) shouldBe Hash[ResultNec[String]].hash(outcome)
      Eq[ResultNec[String]].eqv(outcome, copy) shouldBe true
      failuresOf(copy) shouldBe failuresOf(outcome)
    }

    // The order of the chain is part of the outcome. This is where an equality that compared
    // the failures as a bag rather than as a sequence would show, and no fixed case in this
    // file could show it: a chain reversed is a different outcome unless reversing it leaves
    // the same sequence of failures, which is the one case where it must stay equal.
    forAll { (failures: NonEmptyChain[Failure]) =>
      val forward: ResultNec[String] = failures.asLeft[String]
      val reversed: ResultNec[String] = failures.reverse.asLeft[String]
      val ordered: List[Failure] = failuresOf(forward)

      failuresOf(reversed) shouldBe ordered.reverse
      if (ordered == ordered.reverse) {
        reversed shouldBe forward
      } else {
        (reversed == forward) shouldBe false
        Eq[ResultNec[String]].eqv(reversed, forward) shouldBe false
      }
    }
  }

  test("equality and hashing of the accumulating form follow the value or the chain it holds") {
    forAll { (outcome: ValidatedFailures[String]) =>
      val copy: ValidatedFailures[String] = rebuiltAccumulating(outcome)

      // The rebuild goes out through `toResult` and back through `toValidated`, so this is
      // both the equality law and the statement that the pair of conversions loses nothing.
      outcome shouldBe copy
      copy shouldBe outcome
      copy.hashCode shouldBe outcome.hashCode
      Eq[ValidatedFailures[String]].eqv(outcome, copy) shouldBe true
      copy.toOption shouldBe outcome.toOption
      failuresOf(toResult(copy)) shouldBe failuresOf(toResult(outcome))
    }

    forAll { (first: ValidatedFailures[String], second: ValidatedFailures[String]) =>
      Eq[ValidatedFailures[String]].eqv(first, second) shouldBe (first == second)
      if (first.isValid != second.isValid) {
        // A valid value and an accumulated chain are never the same outcome, whatever each
        // of them carries, exactly as a success is never a failure on the value channel.
        (first == second) shouldBe false
      } else {
        // Where they are of the same shape, their equality is the equality of what they
        // carry, which the conversion to the value channel makes directly comparable.
        (first == second) shouldBe (toResult(first) == toResult(second))
      }
    }
  }

  //-------------------------------------------------------------------------
  // The matchers.
  //
  // The matcher vocabulary of this module's test kit -
  // `testkit/ResultMatchers.scala` and the `Outcome` type class it reads an
  // outcome through - is what the cases of this section assert, each against
  // this file's fixtures.
  //
  // The test kit is machinery rather than a subject: it belongs to the
  // test-scope helpers of this module, which carry no spec of their own, so the
  // contract of the matchers has to be carried by a spec that does, and this is
  // the one. Every case above, and almost every case in the rest of this file,
  // asserts something about the error channel *through* these matchers, which
  // makes this the file whose conclusions depend on them most heavily and the
  // place where their own negative controls belong.
  //
  // Those negative controls are the point. A matcher that silently agreed with
  // everything - a `beFailureWith` that ignored the reason it was given, an
  // `Outcome` instance that called a value accompanied by failures a success -
  // would leave every suite of both modules green while the assertions inside
  // them stopped discriminating, this file included. Each matcher is therefore
  // asserted below on an outcome it must *reject* as well as on one it must
  // accept, and the text of every diagnostic is written out as a literal rather
  // than rebuilt by calling the rendering the matcher uses, because a rendering
  // compared against itself would agree however it changed.
  //
  // Two styles appear, deliberately. An ordinary assertion - `outcome should
  // beFailureWith(...)`, `outcome shouldNot beSuccess` - is what a consuming spec
  // writes, and it is what most cases here use; where the point of a case is the
  // text a failed assertion reports, the assertion is provoked and its report
  // caught with `intercept`, which is the only way to observe the message a spec
  // author would actually read. `MatcherFactory1#matcher` is applied directly
  // where a diagnostic has to be read without an assertion failing: both
  // sentences of a matcher, positive and negated, exist for every outcome while
  // only the one belonging to the side that failed is ever displayed, and
  // applying the matcher is also the only way to reach the phrase for an outcome
  // with no failures at all.

  test("beSuccess and haveValue describe a success") {
    val outcome: FailureOr[String] = "success".asRight[Failure]
    outcome should beSuccess
    outcome should haveValue("success")
    outcome shouldNot beFailure
    outcome shouldNot haveValue("failure")
  }

  test("beFailure does not describe a success") {
    val outcome: FailureOr[String] = "success".asRight[Failure]

    outcome shouldNot beFailure
    outcome shouldNot beFailureWith(FailureReason.ERROR)
    outcome shouldNot haveFailureMessageMatching(".*")

    // Asserting it the other way round is itself an error, which is what the original
    // checked by catching the assertion it provoked. Catching the framework's own report
    // is not the same thing as catching a failure of the code under test: a failure here
    // is a value and is never thrown.
    val reported = intercept[TestFailedException](outcome should beFailure)
    reported.getMessage.contains("Expected Failure but was Success") shouldBe true
    reported.getMessage.contains("success") shouldBe true
  }

  test("the matchers describe the outcome of a mapping step") {
    val outcome: FailureOr[Int] = ("success".asRight[Failure]: FailureOr[String]).map(mapStrLen)
    outcome should beSuccess
    outcome should haveValue(7)
  }

  test("the matchers describe the outcome of a chained step") {
    val outcome: FailureOr[Int] =
      ("success".asRight[Failure]: FailureOr[String]).flatMap(functionStrLen)
    outcome should beSuccess
    outcome should haveValue(7)
  }

  test("the matchers describe the outcome of a combining step") {
    val first: FailureOr[String] = "Hello".asRight[Failure]
    val second: FailureOr[String] = "World".asRight[Failure]
    val outcome: FailureOr[String] = (first, second).tupled.flatMap {
      case (left, right) => functionMerge(left, right)
    }
    outcome should beSuccess
    outcome should haveValue("Hello World")
  }

  test("beFailureWith names the reason a combined outcome failed for") {
    val success: FailureOr[String] = "Hello".asRight[Failure]
    val failure: FailureOr[String] = Failure.Error("failure").asLeft[String]
    val outcome: FailureOr[String] = (success, failure).tupled.flatMap {
      case (left, right) => functionMerge(left, right)
    }

    outcome should beFailureWith(FailureReason.ERROR)
    outcome shouldNot beSuccess
    failuresOf(toNec(outcome)) should have size 1
  }

  test("beFailureWith and haveFailureMessageMatching describe a failure") {
    val outcome: FailureOr[String] = Failure.Error("failure").asLeft[String]

    outcome should beFailure
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("failure")
    // The match is against the whole message, as it was in the assertion being ported, so a
    // pattern naming only part of it does not match.
    outcome shouldNot haveFailureMessageMatching("fail")
    outcome should haveFailureMessageMatching("fail.*")
  }

  //-------------------------------------------------------------------------
  // The matcher vocabulary: fixtures.
  //
  // Four failures with four distinct reasons, so that a case can ask for a
  // reason that is genuinely absent from an outcome, and a chain of three of
  // them - the general error, then the parsing failure, then the missing-data
  // failure - in that fixed order, so that a diagnostic can be asserted to
  // report that order rather than any order. Two further failures carry
  // attributes, one and two of them, for the attribute clause of a
  // single-failure phrase and for the key order that clause lists them in; a
  // whole number is the value every successful and partially successful outcome
  // of this section carries; and the closed family of ten reasons, with one
  // message given to every failure built from it, is what the matrix case
  // enumerates.
  //
  // These fixtures are kept apart from the fixtures at the head of this file,
  // and no failure here holds an apostrophe, a brace or a comma in any message
  // or attribute value. That is not stylistic: a diagnostic assembled from
  // several failures joins them with a comma, so a comma inside a message would
  // make an assertion about their order ambiguous, and the framework routes a
  // raw message through a format step when it carries arguments, which braces
  // and apostrophes are the syntax of.

  private val ErrorFailure: Failure = Failure.Error("Rate lookup failed")

  private val ParsingFailure: Failure = Failure.Parsing("Text does not name a tenor")

  private val MissingFailure: Failure = Failure.MissingData("No holiday calendar for GBLO")

  private val InvalidFailure: Failure = Failure.Invalid("Schedule is invalid")

  private val AttributedFailure: Failure =
    Failure.MissingData("No holiday calendar").withAttribute("id", "GBLO")

  /**
   * A failure carrying two attributes, added in the reverse of their key order.
   *
   * The order they are added in is deliberate: the attributes of a failure are held in key
   * order, so a diagnostic built from this failure lists them in key order too, and a case
   * that built them in key order could not tell the two apart.
   */
  private val TwiceAttributedFailure: Failure =
    Failure
      .Invalid("Schedule is invalid")
      .withAttribute("definition", "P3M from 2024-01-15")
      .withAttribute("calendar", "GBLO")

  /**
   * Three failures in a fixed order, the accumulated chain the multi-failure cases assert
   * over.
   *
   * No failure here carries `INVALID` or `UNSUPPORTED`, which is what makes those two
   * reasons usable as the reason a negative control asks for.
   */
  private val ThreeFailures: NonEmptyChain[Failure] =
    NonEmptyChain.of(ErrorFailure, ParsingFailure, MissingFailure)

  private val Answer: Int = 7

  private val AllReasons: List[FailureReason] = FailureReason.values.toList

  private val MatrixMessage: String = "Reason matrix fixture"

  //-------------------------------------------------------------------------
  // The matcher vocabulary: rendered phrases.
  //
  // The text a diagnostic is expected to carry, written out once because
  // several cases assert the same phrase inside different sentences. A single
  // failure renders as its reason and message; several render as a count
  // followed by the rendering the failure type publishes for itself, in the
  // order the outcome holds them.

  private val ErrorPhrase: String = "reason: <ERROR> and message: <Rate lookup failed>"

  private val InvalidPhrase: String = "reason: <INVALID> and message: <Schedule is invalid>"

  private val ThreeFailuresPhrase: String =
    "3 failures: <ERROR: Rate lookup failed>, <PARSING: Text does not name a tenor>, " +
      "<MISSING_DATA: No holiday calendar for GBLO>"

  /**
   * The phrase describing the messages of the three failures of the chain, in that order.
   *
   * The message matcher reports the messages alone, without their reasons, which is the
   * difference between this phrase and the one above.
   */
  private val ThreeMessagesPhrase: String =
    "messages: <Rate lookup failed>, <Text does not name a tenor>, " +
      "<No holiday calendar for GBLO>"

  //-------------------------------------------------------------------------
  // The matcher vocabulary: helpers.
  //
  // Each names the alias the matchers resolve their view of an outcome from: a
  // bare `Left(...)` or `Validated.invalid(...)` has the type of the subclass,
  // for which no instance is found, so every outcome in this section is built
  // through one of these and arrives at its declared type.

  private def singleFailure(failure: Failure): FailureOr[Int] = Left(failure)

  private def singleSuccess(value: Int): FailureOr[Int] = Right(value)

  private def chainFailure(failures: NonEmptyChain[Failure]): ResultNec[Int] = Left(failures)

  private def chainSuccess(value: Int): ResultNec[Int] = Right(value)

  private def validatedFailure(failures: NonEmptyChain[Failure]): ValidatedFailures[Int] =
    Validated.invalid(failures)

  private def validatedSuccess(value: Int): ValidatedFailures[Int] = Validated.valid(value)

  private def partial(failures: NonEmptyChain[Failure], value: Int): ValueWithFailures[Int] =
    Ior.both(failures, value)

  private def failuresOnly(failures: NonEmptyChain[Failure]): ValueWithFailures[Int] =
    Ior.left(failures)

  private def valueOnly(value: Int): ValueWithFailures[Int] = Ior.right(value)

  /**
   * Returns twice the value of any outcome that carries a whole number, if it carries one.
   *
   * This is the helper a caller writes over the type class, and it is written in terms of
   * [[Outcome.Aux]] because it has to name the value type: the arithmetic below needs the
   * compiler to know the value is a whole number, which plain `Outcome[R]`, whose value type
   * is abstract, does not say. The alias is a published member for exactly this, so
   * exercising it here exercises it as the API it is.
   *
   * @param result  the outcome to read
   * @param outcome  the instance describing outcomes of that type, whose value is a whole number
   * @tparam R  the outcome type
   * @return twice the value the outcome carries, or `None` when it carries none
   */
  private def doubledValue[R](result: R)(implicit outcome: Outcome.Aux[R, Int]): Option[Int] =
    outcome.value(result).map(found => found * 2)

  /**
   * Returns the value of an outcome, whatever its type, together with how many failures it
   * holds.
   *
   * The value type is a parameter here rather than a fixed type, which is the other way the
   * alias is used: the value arrives at the caller's own type instead of as something
   * opaque, so the pair below can be asserted without a cast.
   *
   * @param result  the outcome to read
   * @param outcome  the instance describing outcomes of that type
   * @tparam R  the outcome type
   * @tparam A  the type of the value the outcome carries
   * @return the value of the outcome, if any, and the number of failures it holds
   */
  private def valueAndFailureCount[R, A](result: R)(
      implicit outcome: Outcome.Aux[R, A]): (Option[A], Int) =
    (outcome.value(result), outcome.failures(result).size)

  /**
   * An outcome type belonging to a caller rather than to this library, used to exercise the
   * extension point of [[Outcome]].
   *
   * The four instances published by the type class are the four shapes this library returns,
   * and the type class is public so that a module built on it can describe a shape of its
   * own. This is such a shape: a lookup that answers with a value, with problems, or with
   * both. Nothing in this file brings an instance for it into scope, and the only
   * `Outcome[Lookup]` that exists anywhere is the one in the companion below - which is
   * where the documented extension point says to put it, and where an implicit search finds
   * it with no import.
   *
   * It is a plain final class with an explicit factory rather than a case class because a
   * case class declared inside a spec class carries a synthetic type test on its outer
   * reference, which this build's warning settings reject; nothing here needs the members a
   * case class would add, the cases below only building a lookup and reading it back.
   *
   * @param answer  the value the lookup found, if it found one
   * @param problems  the problems the lookup reported, in the order it reported them
   */
  private final class Lookup(val answer: Option[Int], val problems: List[Failure])

  private object Lookup {

    /**
     * Returns the lookup holding the specified answer and problems.
     *
     * @param answer  the value the lookup found, if it found one
     * @param problems  the problems the lookup reported, in the order it reported them
     * @return the lookup holding them
     */
    def apply(answer: Option[Int], problems: List[Failure]): Lookup =
      new Lookup(answer, problems)

    /**
     * The instance describing a lookup.
     *
     * It respects the rule every instance must respect: the problems reported are the
     * failures, so a lookup that answered and reported a problem is a failure, and the value
     * it found is still reported for a diagnostic to name.
     *
     * @return the instance describing a lookup
     */
    implicit val outcome: Outcome.Aux[Lookup, Int] =
      new Outcome[Lookup] {
        type Value = Int

        override def failures(result: Lookup): List[Failure] = result.problems

        override def value(result: Lookup): Option[Int] = result.answer
      }
  }

  /**
   * The matcher vocabulary reached by mixing in the trait, which is the second of the two
   * documented ways in.
   *
   * This file takes the first way in - the wildcard import of the companion, at its head,
   * which is what the consuming specs of both modules write - so the mixing-in form needs a
   * scope of its own, and this object is it. Keeping the two apart also keeps each of the
   * five names of the vocabulary bound exactly once wherever it is referred to without a
   * qualifier, and the case at the foot of this section compares the sentences the two forms
   * produce.
   */
  private object VocabularyByMixin extends ResultMatchers

  //-------------------------------------------------------------------------
  // `beFailureWith`: the reason is what it matches on.
  //
  // The cases below are the negative control the consuming suites cannot
  // provide, this file's own cases above included. Each builds a genuine
  // failure - not a success - carrying a reason other than the one asked for,
  // and asserts that the matcher rejects it. A `beFailureWith` reduced to the
  // question `beFailure` already answers would pass every assertion in every
  // other suite of both modules and fail here.

  test("beFailureWith rejects a single failure that carries a different reason") {
    val outcome: FailureOr[Int] = singleFailure(ErrorFailure)

    // The outcome is a failure, so the matcher is not being asked to tell a failure from a
    // success here: it is being asked for the reason, which is the whole of its contract.
    outcome should beFailure
    outcome should beFailureWith(FailureReason.ERROR)
    outcome shouldNot beFailureWith(FailureReason.PARSING)
    outcome shouldNot beFailureWith(FailureReason.INVALID)
    outcome shouldNot beFailureWith(FailureReason.MULTIPLE)
  }

  test("beFailureWith rejects an accumulated chain that does not carry the reason asked for") {
    val outcome: ResultNec[Int] = chainFailure(ThreeFailures)

    outcome should beFailure
    outcome shouldNot beFailureWith(FailureReason.INVALID)
    outcome shouldNot beFailureWith(FailureReason.UNSUPPORTED)
    // Every reason the chain does carry is still accepted, so the rejection above is about
    // the reason asked for rather than about the chain having several failures.
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("beFailureWith accepts an accumulated chain where one failure of several carries the reason") {
    // The documented rule for an outcome holding several failures is that at least one of
    // them must satisfy what was asked. This case is what keeps the rejections above from
    // over-constraining it: a chain whose head does not carry the reason still matches when
    // a later failure does.
    val headElsewhere: ResultNec[Int] =
      chainFailure(NonEmptyChain.of(ErrorFailure, ParsingFailure, InvalidFailure))

    headElsewhere should beFailureWith(FailureReason.INVALID)
    headElsewhere should beFailureWith(FailureReason.ERROR)
    headElsewhere shouldNot beFailureWith(FailureReason.MISSING_DATA)

    val onlyOne: ResultNec[Int] =
      chainFailure(NonEmptyChain.of(MissingFailure, MissingFailure, ParsingFailure))
    onlyOne should beFailureWith(FailureReason.PARSING)
    onlyOne shouldNot beFailureWith(FailureReason.ERROR)
  }

  test("beFailureWith rejects a validated failure that carries a different reason") {
    val single: ValidatedFailures[Int] = validatedFailure(NonEmptyChain.one(ParsingFailure))
    val several: ValidatedFailures[Int] = validatedFailure(ThreeFailures)

    single should beFailureWith(FailureReason.PARSING)
    single shouldNot beFailureWith(FailureReason.ERROR)
    several should beFailureWith(FailureReason.MISSING_DATA)
    several shouldNot beFailureWith(FailureReason.INVALID)
  }

  test("beFailureWith rejects a partial success whose failures carry a different reason") {
    // A partial success is the shape where a matcher that read the value rather than the
    // failures would be most easily fooled, so the reason is asserted on it too.
    val outcome: ValueWithFailures[Int] = partial(ThreeFailures, Answer)

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should beFailureWith(FailureReason.MISSING_DATA)
    outcome shouldNot beFailureWith(FailureReason.INVALID)
    outcome shouldNot beFailureWith(FailureReason.CALCULATION_FAILED)
  }

  test("beFailureWith rejects a failure-only outcome that carries a different reason") {
    val outcome: ValueWithFailures[Int] = failuresOnly(NonEmptyChain.of(InvalidFailure, ErrorFailure))

    outcome should beFailureWith(FailureReason.INVALID)
    outcome should beFailureWith(FailureReason.ERROR)
    outcome shouldNot beFailureWith(FailureReason.PARSING)
    outcome shouldNot beFailureWith(FailureReason.MISSING_DATA)
  }

  test("the diagnostic of a wrong reason names the reason asked for and the failure that was there") {
    val outcome: FailureOr[Int] = singleFailure(ErrorFailure)
    val result: MatchResult = beFailureWith(FailureReason.PARSING).matcher[FailureOr[Int]].apply(outcome)

    result.matches shouldBe false
    result.failureMessage shouldBe
      s"Expected Failure with reason: <PARSING> but was Failure with $ErrorPhrase"
    // The negated sentence describes the same mismatch from the other side, and is the one a
    // spec sees when it asserts `shouldNot beFailureWith` against the reason that is there.
    result.negatedFailureMessage shouldBe
      s"Expected Failure with a reason other than <PARSING> but was Failure with $ErrorPhrase"

    // The message the framework reports for the provoked assertion is that same sentence, so
    // the reason asked for and the reason found are both in front of whoever reads it.
    val reported = intercept[TestFailedException](outcome should beFailureWith(FailureReason.PARSING))
    reported.getMessage shouldBe
      s"Expected Failure with reason: <PARSING> but was Failure with $ErrorPhrase"
  }

  test("the diagnostic of a wrong reason lists every failure of a chain in the order the outcome holds them") {
    val outcome: ResultNec[Int] = chainFailure(ThreeFailures)
    val result: MatchResult = beFailureWith(FailureReason.INVALID).matcher[ResultNec[Int]].apply(outcome)

    result.matches shouldBe false
    result.failureMessage shouldBe
      s"Expected Failure with reason: <INVALID> but was Failure with $ThreeFailuresPhrase"
    result.negatedFailureMessage shouldBe
      s"Expected Failure with a reason other than <INVALID> but was Failure with $ThreeFailuresPhrase"

    // Reversing the chain reverses the listing, which is what makes the sentence above an
    // assertion about order rather than about membership.
    val reversed: ResultNec[Int] =
      chainFailure(NonEmptyChain.of(MissingFailure, ParsingFailure, ErrorFailure))
    beFailureWith(FailureReason.INVALID)
      .matcher[ResultNec[Int]]
      .apply(reversed)
      .failureMessage shouldBe
      "Expected Failure with reason: <INVALID> but was Failure with 3 failures: " +
        "<MISSING_DATA: No holiday calendar for GBLO>, <PARSING: Text does not name a tenor>, " +
        "<ERROR: Rate lookup failed>"

    val reported = intercept[TestFailedException](outcome should beFailureWith(FailureReason.INVALID))
    reported.getMessage shouldBe
      s"Expected Failure with reason: <INVALID> but was Failure with $ThreeFailuresPhrase"
  }

  test("beFailureWith accepts the reason a failure carries and rejects each of the other nine") {
    // The matrix is the exhaustive form of the cases above: for every one of the ten reasons
    // a failure can carry, the matcher accepts that reason and rejects the nine it does not.
    // A reason comparison that was accidentally constant in either direction fails here.
    AllReasons should have size 10
    val reasons = Table("carried", AllReasons: _*)
    forAll(reasons) { carried =>
      val outcome: FailureOr[Int] = singleFailure(Failure.of(carried, MatrixMessage))
      outcome should beFailureWith(carried)
      AllReasons.filterNot(other => other == carried).foreach { other =>
        outcome shouldNot beFailureWith(other)
      }
    }
  }

  test("beFailureWith matches an arbitrary failure exactly when the reason asked for is the one it carries") {
    // The generated failures vary in message and attributes as well as in reason, so this
    // property states the contract of the matcher as an equivalence rather than as a pair of
    // examples: it matches if and only if the reasons are equal.
    forAll(genFailure, genFailureReason) { (failure: Failure, requested: FailureReason) =>
      val outcome: FailureOr[Int] = singleFailure(failure)
      val matched = beFailureWith(requested).matcher[FailureOr[Int]].apply(outcome).matches
      matched shouldBe (failure.reason == requested)
    }
  }

  //-------------------------------------------------------------------------
  // `beSuccess` and `haveValue`: a value accompanied by failures is not a
  // success.
  //
  // This is the rule of the type class that surprises, and it is the one an
  // implementation could get wrong without any other suite noticing: an
  // outcome that holds a value *and* failures would be called a success by any
  // matcher that decided from the presence of a value. Every assertion below
  // therefore names the value the outcome actually carries, so that deciding
  // from the value rather than from the failures cannot pass.

  test("a partial success is rejected by beSuccess and by haveValue for the value it carries") {
    val outcome: ValueWithFailures[Int] = partial(ThreeFailures, Answer)

    // The value is genuinely there - a case further down reads it back through the type
    // class - and the outcome is still not a success, because it holds failures.
    outcome shouldNot beSuccess
    outcome shouldNot haveValue(Answer)
    outcome shouldNot haveValue(Answer + 1)
    outcome should beFailure
  }

  test("the diagnostic of beSuccess on a partial success reports its failures rather than its value") {
    val outcome: ValueWithFailures[Int] = partial(NonEmptyChain.one(InvalidFailure), Answer)
    val result: MatchResult = beSuccess.matcher[ValueWithFailures[Int]].apply(outcome)

    result.matches shouldBe false
    result.failureMessage shouldBe s"Expected Success but was Failure with $InvalidPhrase"
    // The value is not lost: it is what the other side of the sentence reports, which is
    // what a spec reads when it asserts `shouldNot beSuccess` against a real success.
    result.negatedFailureMessage shouldBe s"Expected Failure but was Success with value: <$Answer>"

    val reported = intercept[TestFailedException](outcome should beSuccess)
    reported.getMessage shouldBe s"Expected Success but was Failure with $InvalidPhrase"
  }

  test("the diagnostic of haveValue on a partial success reports its failures rather than a value mismatch") {
    val outcome: ValueWithFailures[Int] = partial(NonEmptyChain.one(InvalidFailure), Answer)
    val result: MatchResult = haveValue(Answer).matcher[ValueWithFailures[Int]].apply(outcome)

    result.matches shouldBe false
    // The failure branch of this matcher's message is the one taken, not the mismatch
    // branch: the value asked for is the value present, so a mismatch is not what happened
    // and saying so would misdescribe the outcome.
    result.failureMessage shouldBe s"Expected Success but was Failure with $InvalidPhrase"
    result.failureMessage should not include "Expected Success with value"
    result.negatedFailureMessage shouldBe
      s"Expected Success with a value other than <$Answer> but was: <$Answer>"

    val reported = intercept[TestFailedException](outcome should haveValue(Answer))
    reported.getMessage shouldBe s"Expected Success but was Failure with $InvalidPhrase"
  }

  test("the three shapes of a partial result are classified by their failures alone") {
    val onlyValue: ValueWithFailures[Int] = valueOnly(Answer)
    val onlyFailures: ValueWithFailures[Int] = failuresOnly(NonEmptyChain.one(InvalidFailure))
    val both: ValueWithFailures[Int] = partial(NonEmptyChain.one(InvalidFailure), Answer)

    // A value and no failures is a success, and it is the only one of the three.
    onlyValue should beSuccess
    onlyValue should haveValue(Answer)
    onlyValue shouldNot beFailure
    onlyValue shouldNot beFailureWith(FailureReason.INVALID)
    onlyValue shouldNot haveFailureMessageMatching("Schedule is invalid")

    // Failures and no value is a failure, and matches every failure matcher that fits.
    onlyFailures shouldNot beSuccess
    onlyFailures shouldNot haveValue(Answer)
    onlyFailures should beFailure
    onlyFailures should beFailureWith(FailureReason.INVALID)
    onlyFailures shouldNot beFailureWith(FailureReason.PARSING)
    onlyFailures should haveFailureMessageMatching("Schedule is invalid")

    // A value together with failures is asserted exactly as the failure-only shape is,
    // which is the whole content of the rule: the value it carries changes nothing.
    both shouldNot beSuccess
    both shouldNot haveValue(Answer)
    both should beFailure
    both should beFailureWith(FailureReason.INVALID)
    both shouldNot beFailureWith(FailureReason.PARSING)
    both should haveFailureMessageMatching("Schedule is invalid")
    // The message match is against the whole message, so a pattern naming only part of it
    // does not match even though the failure is there to be matched.
    both shouldNot haveFailureMessageMatching("Schedule")
    both should haveFailureMessageMatching("Schedule.*")
  }

  //-------------------------------------------------------------------------
  // The `Outcome` instances.
  //
  // The type class is the whole of what the matchers know about an outcome, so
  // each of its four instances is read directly here rather than only through
  // an assertion: which failures are present, in which order, and whether
  // there is a value. Every matcher sentence asserted in this section is a
  // rendering of exactly these two answers.

  test("the instance for the value channel reports the one failure of a failure and the value of a success") {
    val instance = implicitly[Outcome[FailureOr[Int]]]

    instance.failures(singleFailure(ErrorFailure)) shouldBe List(ErrorFailure)
    instance.value(singleFailure(ErrorFailure)) shouldBe None
    // A success holds no failures at all, which is the answer that makes it a success.
    instance.failures(singleSuccess(Answer)) shouldBe List.empty[Failure]
    instance.value(singleSuccess(Answer)) shouldBe Some(Answer)
  }

  test("the instance for the accumulating channel reports every failure in the order the chain holds them") {
    val instance = implicitly[Outcome[ResultNec[Int]]]

    instance.failures(chainFailure(ThreeFailures)) shouldBe
      List(ErrorFailure, ParsingFailure, MissingFailure)
    // The same three failures in another order extract in that other order, so the list
    // above is an assertion about order rather than about membership.
    instance.failures(chainFailure(NonEmptyChain.of(MissingFailure, ErrorFailure, ParsingFailure))) shouldBe
      List(MissingFailure, ErrorFailure, ParsingFailure)
    instance.value(chainFailure(ThreeFailures)) shouldBe None
    instance.failures(chainSuccess(Answer)) shouldBe List.empty[Failure]
    instance.value(chainSuccess(Answer)) shouldBe Some(Answer)

    // The order of the extraction is the order of the diagnostic: a chain read here reads
    // the same way in the sentence a failed assertion reports.
    beSuccess.matcher[ResultNec[Int]].apply(chainFailure(ThreeFailures)).failureMessage shouldBe
      s"Expected Success but was Failure with $ThreeFailuresPhrase"
  }

  test("the instance for the validating channel reports the failures and the value the conversion preserves") {
    val instance = implicitly[Outcome[ValidatedFailures[Int]]]

    instance.failures(validatedFailure(ThreeFailures)) shouldBe
      List(ErrorFailure, ParsingFailure, MissingFailure)
    instance.value(validatedFailure(ThreeFailures)) shouldBe None
    instance.failures(validatedSuccess(Answer)) shouldBe List.empty[Failure]
    instance.value(validatedSuccess(Answer)) shouldBe Some(Answer)

    // The accumulating form and the form it converts to describe the same outcome, so the
    // two instances answer identically for the same failures.
    instance.failures(validatedFailure(ThreeFailures)) shouldBe
      implicitly[Outcome[ResultNec[Int]]].failures(chainFailure(ThreeFailures))
  }

  test("the instance for a partial result reports a value whenever one is present") {
    val instance = implicitly[Outcome[ValueWithFailures[Int]]]

    instance.failures(valueOnly(Answer)) shouldBe List.empty[Failure]
    instance.value(valueOnly(Answer)) shouldBe Some(Answer)
    // The point of this instance: a partial success answers both questions with something,
    // and it is the failures that decide what the matchers call it.
    instance.failures(partial(ThreeFailures, Answer)) shouldBe
      List(ErrorFailure, ParsingFailure, MissingFailure)
    instance.value(partial(ThreeFailures, Answer)) shouldBe Some(Answer)
    instance.failures(failuresOnly(NonEmptyChain.one(InvalidFailure))) shouldBe List(InvalidFailure)
    instance.value(failuresOnly(NonEmptyChain.one(InvalidFailure))) shouldBe None
  }

  test("the value type named by Outcome.Aux is the type the outcome carries") {
    // Each of the four instances declares its value type through the alias, so a helper
    // written over `Aux[R, Int]` accepts all four shapes and gets a whole number back.
    doubledValue(singleSuccess(Answer)) shouldBe Some(14)
    doubledValue(chainSuccess(Answer)) shouldBe Some(14)
    doubledValue(validatedSuccess(Answer)) shouldBe Some(14)
    doubledValue(partial(ThreeFailures, Answer)) shouldBe Some(14)
    doubledValue(singleFailure(ErrorFailure)) shouldBe None
    doubledValue(failuresOnly(NonEmptyChain.one(InvalidFailure))) shouldBe None

    valueAndFailureCount(partial(ThreeFailures, Answer)) shouldBe ((Some(Answer), 3))
    valueAndFailureCount(singleFailure(ErrorFailure)) shouldBe ((None, 1))
    valueAndFailureCount(validatedSuccess(Answer)) shouldBe ((Some(Answer), 0))

    val text: FailureOr[String] = Right("seven")
    text should haveValue("seven")
    // The alias names the value type to the compiler rather than to the reader alone: the
    // helper above is about whole numbers, so an outcome carrying text is rejected where it
    // is written rather than reported as a failed assertion.
    assertDoesNotCompile("doubledValue(text)")
  }

  test("the single-failure phrase of a diagnostic renders the attributes of a failure in key order") {
    beSuccess.matcher[FailureOr[Int]].apply(singleFailure(AttributedFailure)).failureMessage shouldBe
      "Expected Success but was Failure with reason: <MISSING_DATA> and " +
        "message: <No holiday calendar> and attributes: <id=GBLO>"

    // The two attributes were added in the reverse of their key order and render in key
    // order, which is what makes a diagnostic depend on the failure rather than on the way
    // it was assembled.
    beSuccess.matcher[FailureOr[Int]].apply(singleFailure(TwiceAttributedFailure)).failureMessage shouldBe
      "Expected Success but was Failure with reason: <INVALID> and " +
        "message: <Schedule is invalid> and attributes: " +
        "<calendar=GBLO, definition=P3M from 2024-01-15>"

    // Where several failures are reported the attributes arrive through the rendering the
    // failure type publishes for itself, which brackets them after the message instead.
    beSuccess
      .matcher[ResultNec[Int]]
      .apply(chainFailure(NonEmptyChain.of(TwiceAttributedFailure, ErrorFailure)))
      .failureMessage shouldBe
      "Expected Success but was Failure with 2 failures: <INVALID: Schedule is invalid " +
        "[calendar=GBLO, definition=P3M from 2024-01-15]>, <ERROR: Rate lookup failed>"

    // A failure with no attributes renders without the clause at all.
    beSuccess.matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure)).failureMessage shouldBe
      s"Expected Success but was Failure with $ErrorPhrase"
  }

  //-------------------------------------------------------------------------
  // The diagnostics.
  //
  // Each matcher carries two sentences, one for the assertion it was written
  // for and one for its negation, and only the sentence belonging to the side
  // that failed is ever displayed. Both are read here, for an outcome with no
  // failures, one failure and several, by applying the matcher to the outcome
  // and inspecting the result it returns - which is also the only way to reach
  // the phrase for an outcome with no failures, no assertion being able to
  // display it. Every expected sentence is a literal, so a change to any
  // template is a failure here.

  test("beSuccess describes no failures, one failure and many failures exactly") {
    val onNone: MatchResult = beSuccess.matcher[FailureOr[Int]].apply(singleSuccess(Answer))
    onNone.matches shouldBe true
    onNone.failureMessage shouldBe "Expected Success but was Failure with no failures"
    onNone.negatedFailureMessage shouldBe s"Expected Failure but was Success with value: <$Answer>"

    val onOne: MatchResult = beSuccess.matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure))
    onOne.matches shouldBe false
    onOne.failureMessage shouldBe s"Expected Success but was Failure with $ErrorPhrase"
    onOne.negatedFailureMessage shouldBe "Expected Failure but was Success with value: <no value>"

    val onMany: MatchResult = beSuccess.matcher[ResultNec[Int]].apply(chainFailure(ThreeFailures))
    onMany.matches shouldBe false
    onMany.failureMessage shouldBe s"Expected Success but was Failure with $ThreeFailuresPhrase"
    onMany.negatedFailureMessage shouldBe "Expected Failure but was Success with value: <no value>"
  }

  test("haveValue describes a value mismatch, a failed outcome and a negated match exactly") {
    val onMatch: MatchResult = haveValue(Answer).matcher[FailureOr[Int]].apply(singleSuccess(Answer))
    onMatch.matches shouldBe true
    onMatch.negatedFailureMessage shouldBe
      s"Expected Success with a value other than <$Answer> but was: <$Answer>"

    // A success carrying another value is the one case this matcher reports as a mismatch,
    // and the sentence names both values so the difference is visible.
    val onMismatch: MatchResult =
      haveValue(Answer + 1).matcher[FailureOr[Int]].apply(singleSuccess(Answer))
    onMismatch.matches shouldBe false
    onMismatch.failureMessage shouldBe s"Expected Success with value: <8> but was: <$Answer>"
    onMismatch.negatedFailureMessage shouldBe
      s"Expected Success with a value other than <8> but was: <$Answer>"

    val onOne: MatchResult = haveValue(Answer).matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure))
    onOne.matches shouldBe false
    onOne.failureMessage shouldBe s"Expected Success but was Failure with $ErrorPhrase"
    onOne.negatedFailureMessage shouldBe
      s"Expected Success with a value other than <$Answer> but was: <no value>"

    val onMany: MatchResult = haveValue(Answer).matcher[ResultNec[Int]].apply(chainFailure(ThreeFailures))
    onMany.matches shouldBe false
    onMany.failureMessage shouldBe s"Expected Success but was Failure with $ThreeFailuresPhrase"
  }

  test("beFailure describes no failures, one failure and many failures exactly") {
    val onNone: MatchResult = beFailure.matcher[FailureOr[Int]].apply(singleSuccess(Answer))
    onNone.matches shouldBe false
    onNone.failureMessage shouldBe s"Expected Failure but was Success with value: <$Answer>"
    onNone.negatedFailureMessage shouldBe "Expected Success but was Failure with no failures"

    val onOne: MatchResult = beFailure.matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure))
    onOne.matches shouldBe true
    onOne.failureMessage shouldBe "Expected Failure but was Success with value: <no value>"
    onOne.negatedFailureMessage shouldBe s"Expected Success but was Failure with $ErrorPhrase"

    val onMany: MatchResult = beFailure.matcher[ResultNec[Int]].apply(chainFailure(ThreeFailures))
    onMany.matches shouldBe true
    onMany.negatedFailureMessage shouldBe s"Expected Success but was Failure with $ThreeFailuresPhrase"
  }

  test("beFailureWith describes a success, a matching reason and a chain of failures exactly") {
    val onNone: MatchResult =
      beFailureWith(FailureReason.ERROR).matcher[FailureOr[Int]].apply(singleSuccess(Answer))
    onNone.matches shouldBe false
    // A successful outcome is reported as a success rather than as a reason mismatch, the
    // failure test coming first.
    onNone.failureMessage shouldBe
      s"Expected Failure with reason: <ERROR> but was Success with value: <$Answer>"
    onNone.negatedFailureMessage shouldBe
      "Expected Failure with a reason other than <ERROR> but was Failure with no failures"

    val onOne: MatchResult =
      beFailureWith(FailureReason.ERROR).matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure))
    onOne.matches shouldBe true
    onOne.failureMessage shouldBe
      s"Expected Failure with reason: <ERROR> but was Failure with $ErrorPhrase"
    onOne.negatedFailureMessage shouldBe
      s"Expected Failure with a reason other than <ERROR> but was Failure with $ErrorPhrase"

    val onMany: MatchResult =
      beFailureWith(FailureReason.ERROR).matcher[ResultNec[Int]].apply(chainFailure(ThreeFailures))
    onMany.matches shouldBe true
    onMany.negatedFailureMessage shouldBe
      s"Expected Failure with a reason other than <ERROR> but was Failure with $ThreeFailuresPhrase"
  }

  test("haveFailureMessageMatching describes the messages of an outcome in each of its three diagnostics") {
    val onNone: MatchResult =
      haveFailureMessageMatching(".*").matcher[FailureOr[Int]].apply(singleSuccess(Answer))
    onNone.matches shouldBe false
    onNone.failureMessage shouldBe
      s"Expected Failure with message matching: <.*> but was Success with value: <$Answer>"
    onNone.negatedFailureMessage shouldBe
      "Expected Failure with no message matching: <.*> but was Failure with no failures"

    // The phrase of this matcher carries the messages alone, without their reasons, which is
    // what distinguishes it from the phrase every other matcher reports.
    val onOne: MatchResult =
      haveFailureMessageMatching("Rate.*").matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure))
    onOne.matches shouldBe true
    onOne.failureMessage shouldBe
      "Expected Failure with message matching: <Rate.*> but was Failure with message: <Rate lookup failed>"
    onOne.negatedFailureMessage shouldBe
      "Expected Failure with no message matching: <Rate.*> but was Failure with message: <Rate lookup failed>"

    val onMissed: MatchResult =
      haveFailureMessageMatching("Rate").matcher[FailureOr[Int]].apply(singleFailure(ErrorFailure))
    onMissed.matches shouldBe false
    onMissed.failureMessage shouldBe
      "Expected Failure with message matching: <Rate> but was Failure with message: <Rate lookup failed>"

    val onMany: MatchResult =
      haveFailureMessageMatching("Text does not name a tenor")
        .matcher[ResultNec[Int]]
        .apply(chainFailure(ThreeFailures))
    onMany.matches shouldBe true
    onMany.failureMessage shouldBe
      s"Expected Failure with message matching: <Text does not name a tenor> but was Failure with $ThreeMessagesPhrase"
    onMany.negatedFailureMessage shouldBe
      s"Expected Failure with no message matching: <Text does not name a tenor> but was Failure with $ThreeMessagesPhrase"
  }

  //-------------------------------------------------------------------------
  // The two published ways in, and the extension point.

  test("an outcome type of the caller supplies its own instance and every matcher works on it") {
    val found: Lookup = Lookup(Some(Answer), List.empty[Failure])
    val missing: Lookup = Lookup(None, List(MissingFailure, ParsingFailure))
    val incomplete: Lookup = Lookup(Some(Answer), List(InvalidFailure))

    // No import brings the instance into scope: it sits in the companion of the type it
    // describes, which is where the documented extension point says to put it.
    implicitly[Outcome[Lookup]].failures(missing) shouldBe List(MissingFailure, ParsingFailure)
    implicitly[Outcome[Lookup]].value(found) shouldBe Some(Answer)

    found should beSuccess
    found should haveValue(Answer)
    found shouldNot beFailure
    found shouldNot beFailureWith(FailureReason.MISSING_DATA)
    found shouldNot haveFailureMessageMatching("No holiday calendar for GBLO")

    missing shouldNot beSuccess
    missing shouldNot haveValue(Answer)
    missing should beFailure
    missing should beFailureWith(FailureReason.MISSING_DATA)
    missing shouldNot beFailureWith(FailureReason.ERROR)
    missing should haveFailureMessageMatching("No holiday calendar for GBLO")

    // An instance has to respect the rule the four published ones follow, and this one does,
    // so a caller's partial success is a failure here too and reports the same phrase.
    incomplete shouldNot beSuccess
    incomplete shouldNot haveValue(Answer)
    incomplete should beFailureWith(FailureReason.INVALID)
    beSuccess.matcher[Lookup].apply(incomplete).failureMessage shouldBe
      s"Expected Success but was Failure with $InvalidPhrase"
    beSuccess.matcher[Lookup].apply(found).negatedFailureMessage shouldBe
      s"Expected Failure but was Success with value: <$Answer>"
  }

  test("the vocabulary is supplied both by mixing in the trait and by importing the companion") {
    val success: FailureOr[Int] = singleSuccess(Answer)
    val failed: FailureOr[Int] = singleFailure(ErrorFailure)

    // This file imports the companion, which is one of the two documented ways in and the
    // one every consuming spec of both modules writes; `VocabularyByMixin` is the other,
    // mixing the trait in, in a scope of its own. Each of the five names answers the same
    // way through both.
    VocabularyByMixin.beSuccess.matcher[FailureOr[Int]].apply(success).matches shouldBe true
    VocabularyByMixin.beSuccess.matcher[FailureOr[Int]].apply(failed).matches shouldBe false
    VocabularyByMixin.haveValue(Answer).matcher[FailureOr[Int]].apply(success).matches shouldBe true
    VocabularyByMixin.haveValue(Answer + 1).matcher[FailureOr[Int]].apply(success).matches shouldBe false
    VocabularyByMixin.beFailure.matcher[FailureOr[Int]].apply(failed).matches shouldBe true
    VocabularyByMixin.beFailure.matcher[FailureOr[Int]].apply(success).matches shouldBe false
    VocabularyByMixin
      .beFailureWith(FailureReason.ERROR)
      .matcher[FailureOr[Int]]
      .apply(failed)
      .matches shouldBe true
    VocabularyByMixin
      .beFailureWith(FailureReason.PARSING)
      .matcher[FailureOr[Int]]
      .apply(failed)
      .matches shouldBe false
    VocabularyByMixin
      .haveFailureMessageMatching("Rate lookup failed")
      .matcher[FailureOr[Int]]
      .apply(failed)
      .matches shouldBe true
    VocabularyByMixin
      .haveFailureMessageMatching("Rate")
      .matcher[FailureOr[Int]]
      .apply(failed)
      .matches shouldBe false

    // The two ways in are the same matchers, so a sentence read through one is the sentence
    // read through the other. The right-hand side of each of these is the imported form -
    // the names this file has used unqualified throughout.
    VocabularyByMixin.beSuccess.matcher[FailureOr[Int]].apply(failed).failureMessage shouldBe
      beSuccess.matcher[FailureOr[Int]].apply(failed).failureMessage
    VocabularyByMixin.beSuccess.matcher[FailureOr[Int]].apply(success).negatedFailureMessage shouldBe
      beSuccess.matcher[FailureOr[Int]].apply(success).negatedFailureMessage
    VocabularyByMixin.haveValue(Answer).matcher[FailureOr[Int]].apply(failed).failureMessage shouldBe
      haveValue(Answer).matcher[FailureOr[Int]].apply(failed).failureMessage
    VocabularyByMixin.beFailure.matcher[FailureOr[Int]].apply(success).failureMessage shouldBe
      beFailure.matcher[FailureOr[Int]].apply(success).failureMessage
    VocabularyByMixin
      .beFailureWith(FailureReason.PARSING)
      .matcher[FailureOr[Int]]
      .apply(failed)
      .failureMessage shouldBe
      beFailureWith(FailureReason.PARSING).matcher[FailureOr[Int]].apply(failed).failureMessage
    VocabularyByMixin
      .haveFailureMessageMatching("Rate")
      .matcher[FailureOr[Int]]
      .apply(failed)
      .failureMessage shouldBe
      haveFailureMessageMatching("Rate").matcher[FailureOr[Int]].apply(failed).failureMessage

    // The object that supplies the imported names is the trait, made concrete, so the
    // vocabulary reached either way is one implementation rather than two.
    ResultMatchers shouldBe a[ResultMatchers]
  }

  //-------------------------------------------------------------------------
  // The surface of the error channel.

  test("the aliases of the error channel describe the same outcome and convert between one another") {
    // The original closed with a reflective sweep over its own bean properties. There is no
    // bean to walk here, and the property worth asserting in its place is the one the whole
    // design rests on: the names of this channel are aliases for types that already exist,
    // so a value of one is a value of another and the conversions between them lose nothing.
    val single: FailureOr[String] = Failure.MissingData("message 1").asLeft[String]
    val chained: ResultNec[String] = toNec(single)
    val accumulating: ValidatedFailures[String] = toValidated(chained)
    val back: ResultNec[String] = toResult(accumulating)

    single should beFailureWith(FailureReason.MISSING_DATA)
    chained should beFailureWith(FailureReason.MISSING_DATA)
    accumulating should beFailureWith(FailureReason.MISSING_DATA)
    back shouldBe chained
    failuresOf(back) shouldBe List(Failure.MissingData("message 1"))
    collapsedFailure(back) shouldBe Some(Failure.MissingData("message 1"))

    val success: FailureOr[String] = "Hello".asRight[Failure]
    toResult(toValidated(toNec(success))) shouldBe Right("Hello")
    toResult(toValidated(toNec(success))) should haveValue("Hello")

    // The reasons a failure can carry are the closed set of ten, each with a distinct name.
    FailureReason.values.toList should have size 10
    FailureReason.values.toList.map(_.name).distinct should have size 10
    FailureReason.values.toList.foreach { reason =>
      FailureReason.valueOf(reason.name) shouldBe Some(reason)
    }
  }

  test("the module root's re-export names the same four outcome types as this package") {
    // The four names of this channel are declared twice: here, in the package object of this
    // package, and again in the package object of the module root, which re-exports them so
    // that one wildcard import of `com.opengamma.strata.collect` supplies the error
    // vocabulary along with everything else the module offers. That is the import route this
    // library asks its callers to take, and it is the route every module built on this one
    // takes, so the two sets of names agreeing is part of this module's public contract.
    //
    // This file cannot state that contract in unqualified names. It is declared in the
    // `result` package, so an unqualified `FailureOr` here is the local declaration and a
    // case written with it would assert nothing about the re-export. The root names are
    // therefore written out in full below, and the assertion is made to the compiler rather
    // than at run time: a value of the local name is bound to a binding of the root name and
    // a value of the root name is bound back to a binding of the local name, for each of the
    // four. Were either name to drift - a different failure type, a second type parameter, a
    // definition that shadowed rather than aliased - one of those eight bindings would stop
    // compiling. The run-time assertions that follow each pair keep the case from being
    // vacuous and pin the value that travelled through both names.

    val localSingle: FailureOr[String] = Failure.MissingData("message 1").asLeft[String]
    val rootSingle: com.opengamma.strata.collect.FailureOr[String] = localSingle
    val singleAgain: FailureOr[String] = rootSingle
    singleAgain shouldBe localSingle
    rootSingle should beFailureWith(FailureReason.MISSING_DATA)

    val localChained: ResultNec[String] = toNec(singleAgain)
    val rootChained: com.opengamma.strata.collect.ResultNec[String] = localChained
    val chainedAgain: ResultNec[String] = rootChained
    chainedAgain shouldBe localChained
    failuresOf(chainedAgain) shouldBe List(Failure.MissingData("message 1"))

    val localAccumulating: ValidatedFailures[String] = toValidated(chainedAgain)
    val rootAccumulating: com.opengamma.strata.collect.ValidatedFailures[String] = localAccumulating
    val accumulatingAgain: ValidatedFailures[String] = rootAccumulating
    accumulatingAgain shouldBe localAccumulating
    toResult(accumulatingAgain) shouldBe localChained

    // The fourth name is the one nothing in this repository referred to before this port, so
    // it is the one a missing consumer would leave untested. Only the type is re-exported:
    // the term of the same name - the factory object used on the right below - stays in this
    // package, which is why the value is built through the local object and then named by the
    // root alias. That is exactly how a caller outside this package writes it.
    val localPartial: ValueWithFailures[String] =
      ValueWithFailures.of("some rows", List(Failure.MissingData("message 1")))
    val rootPartial: com.opengamma.strata.collect.ValueWithFailures[String] = localPartial
    val partialAgain: ValueWithFailures[String] = rootPartial
    partialAgain shouldBe localPartial
    partialAgain.right shouldBe Some("some rows")
    rootPartial should beFailureWith(FailureReason.MISSING_DATA)

    // The eight assignments above are only a proof because each root name denotes one
    // particular type rather than something everything conforms to, so the negative is stated
    // as well: an outcome of a different shape is rejected by the root name, exactly as it is
    // by the local one.
    assertDoesNotCompile("""
      val wrongShape: com.opengamma.strata.collect.ValueWithFailures[String] =
        Failure.MissingData("message 1").asLeft[String]
      wrongShape
    """)

    // The re-export is an alias and adds no members of its own, so the root name reaches the
    // combinators of the type it names and nothing else. Naming the factory object through
    // the root is therefore an error, and stating that here is what records the asymmetry
    // between the type and the term for the next reader of the package object.
    assertDoesNotCompile("""
      val throughTheRoot = com.opengamma.strata.collect.ValueWithFailures.of("some rows")
      throughTheRoot
    """)
  }
}
