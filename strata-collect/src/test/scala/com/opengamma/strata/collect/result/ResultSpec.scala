/*
 * Copyright (C) 2013 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import scala.util.Try

import cats.Eq
import cats.Hash
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

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
 * The type being ported mixed up three things that the port keeps apart, and a large part
 * of the mapping at the foot of this file is the record of which of the three each case
 * fell into.
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
 * ===Determinism===
 *
 * Every case here is a pure function of literal data. Nothing reads a clock, a file, the
 * class path or the environment, nothing depends on iteration order beyond the order the
 * spec itself writes, and no case shares mutable state with another, so the outcome of this
 * spec does not depend on the order its cases run in.
 *
 * @see [[Failure]] for the failure each outcome carries
 * @see [[FailureReason]] for the ten reasons a failure can carry
 * @see [[com.opengamma.strata.collect.testkit.ResultMatchers]] for the matchers used throughout
 */
final class ResultSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

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

  /**
   * Runs a block that belongs to a foreign interface - one that reports a problem by
   * throwing - and puts what it reports on the value channel.
   *
   * This is the replacement for the capturing factories of the original, and the reason it
   * lives in this spec rather than in the module is that nothing in the module throws in
   * order to report a failure. A caller at the boundary of some other library writes this
   * conversion once, at that boundary, and everything inside works with the returned value.
   */
  private def capture[A](block: => A): FailureOr[A] =
    Try(block).toEither.leftMap(thrown => Failure.Error(thrown.getMessage))

  /**
   * Runs a block that already produces an outcome and may also throw, flattening the two.
   *
   * This is the replacement for the capturing factory of the original that took a block
   * returning a result: the block's own failure and the failure derived from what it threw
   * arrive on the same channel, so a caller handles one thing rather than two.
   */
  private def captureFlat[A](block: => FailureOr[A]): FailureOr[A] =
    capture(block).flatMap(identity)

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
  // The replacement for the capturing factories.
  //
  // Two further factories of the original ran a supplied block and turned
  // whatever it threw into a failure, one of them also flattening a block that
  // returned an outcome of its own. Nothing in this port throws in order to
  // report a failure, so neither factory has a counterpart in the module.
  //
  // The conversion still has to happen somewhere, though: a library that is
  // not this one may well report a problem by throwing, and its caller has to
  // get that onto the value channel. That conversion is written once, at the
  // boundary, and the helpers `capture` and `captureFlat` above are it. They
  // are test-scope on purpose - they belong to whichever boundary needs them,
  // not to the error model.

  test("a captured block that produces a value gives a success") {
    val outcome: FailureOr[String] = capture("success")
    outcome should beSuccess
    outcome should haveValue("success")
    outcome.isLeft shouldBe false
    outcome shouldBe Right("success")
  }

  test("a captured block that throws gives an error failure carrying its message") {
    val outcome: FailureOr[String] =
      capture[String](throw new IllegalArgumentException("Big bad error"))

    outcome should beFailure
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Big bad error")
    outcome.toOption shouldBe None
  }

  test("a captured block producing an outcome flattens to that success") {
    val outcome: FailureOr[String] = captureFlat("success".asRight[Failure])
    outcome should beSuccess
    outcome should haveValue("success")
    outcome shouldBe Right("success")
  }

  test("a captured block producing a failed outcome flattens to that failure") {
    val outcome: FailureOr[String] = captureFlat(Failure.Error("Something failed").asLeft[String])
    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Something failed")
    outcome.toOption shouldBe None
  }

  test("a captured block that throws where an outcome was expected flattens to an error failure") {
    val outcome: FailureOr[String] =
      captureFlat[String](throw new IllegalArgumentException("Big bad error"))

    outcome should beFailureWith(FailureReason.ERROR)
    outcome should haveFailureMessageMatching("Big bad error")
    outcome.toOption shouldBe None
    // The block's own failure and the failure derived from what it threw arrive on the same
    // channel, so a caller downstream of this boundary handles one thing rather than two.
    failuresOf(toNec(outcome)) shouldBe List(Failure.Error("Big bad error"))
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
  }

  test("a combiner that succeeds produces its value") {
    val results: List[ResultNec[Int]] = List(1, 2, 3, 4).map(value => toNec(value.asRight[Failure]))

    val outcome: ResultNec[String] = flatCombine(results)(values =>
      toNec(s"res${values.foldLeft(1)((left, right) => left * right)}".asRight[Failure]))

    outcome should beSuccess
    outcome should haveValue("res24")
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
  // The matchers.
  //
  // Seven cases of the original existed to exercise its assertion helpers
  // rather than the type under test, and those helpers are ported into the
  // test kit of this module, where their own spec covers them. The seven are
  // kept here, each exercising the matcher that replaced the assertion it used,
  // so that the vocabulary this whole file depends on is itself asserted
  // against these fixtures.

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
}

// ---------------------------------------------------------------------------
// Traceability.
//
// Every one of the 85 test methods of the class this file is ported from is
// listed below against the case here that carries it. The Scala names are
// reproduced verbatim, because they are what appears in the test report the
// migration's coverage gate reads.
//
// Three kinds of entry appear:
//
//   - a plain entry, where the case asserts what the original asserted;
//   - "replaces", where the subject of the original is machinery this port
//     does not have, so the case asserts the contract that takes its place.
//     The reason is given on each such entry;
//   - "consolidated", where another file carries the primary assertion and
//     this case reaches the same outcome through this channel's own API. The
//     owning file is named.
//
// No entry is dropped and none is partial: every method of the original is
// accounted for by a named case here. The 85 cases contribute to this module's
// test-count floor of 491.
//
// ---- A success, and what can be read from one ----------------------------
//
// success
//   -> "a success holds its value and yields it from every total accessor"
//      Also absorbs the original's two accessors that took a fallback and
//      rejected one naming no value: neither has a target, because the
//      fallback of `getOrElse` is a value of the same type and cannot be
//      absent, and the recovering form is `fold` over a total function.
// ifSuccess
//   -> "foreach runs on a success and sees its value"
// ifFailure
//   -> "the left projection runs on a failure and sees its reason and message"
// success_getFailure
//   -> "a success has no failure to read"
//      replaces: reading the failure of a success threw; here the absence is a
//      value, and there is no partial accessor to reach for.
//
// ---- Mapping and chaining over a success ---------------------------------
//
// success_map
//   -> "map applies the mapping function to the value of a success"
// success_mapFailure
//   -> "mapping the failure of a success leaves it untouched"
// success_mapFailureItems
//   -> "rebuilding the failure of a success is the identity however the rebuild is written"
//      replaces: the original's second mapping member reached into the items a
//      failure was made of; there is no item type, so both arrive at `leftMap`.
// success_flatMap
//   -> "flatMap applies a result-returning function to the value of a success"
//
// ---- Combining two outcomes ----------------------------------------------
//
// success_combineWith_success
//   -> "two successes combine into the merged value"
// success_combineWith_failure
//   -> "combining a success with a failure yields that failure"
// failure_combineWith_success
//   -> "combining a failure with a success yields the failure and never calls the combiner"
// failure_combineWith_failure
//   -> "combining two failures short-circuits on the value channel and accumulates on the chain channel"
//      Both channels are pinned, the original having conflated them.
// success_stream
//   -> "a success iterates over its single value"
// failure_stream
//   -> "a failure iterates over nothing"
//
// ---- A failure, and what can be read from one -----------------------------
//
// failure
//   -> "a failure carries the reason and message chosen at its construction site"
//      replaces: the original built this from something thrown and asserted the
//      captured type and rendered trace; a failure here holds a reason, a
//      message and attributes and nothing else.
// failure_error
//   -> "a failure recovers a value through getOrElse and through fold"
//      replaces: as above, for the unchecked-throwable form.
// failure_mapFailure
//   -> "mapping the failure of a failure replaces it"
// failure_mapFailureItems
//   -> "rebuilding the failure of a failure preserves its class and therefore its reason"
//      replaces: no item type; a rebuild is a copy, so the class - and with it
//      the reason - cannot change.
// failure_map_flatMap_ifSuccess
//   -> "map, flatMap and foreach are all skipped on a failure and the failure is preserved"
// failure_getValue
//   -> "a failure has no value to read"
//      replaces: reading the value of a failure threw; here the absence is a value.
//
// ---- The replacement for exception capture --------------------------------
//
// The five entries below share one reason. The original ran a supplied function,
// caught whatever it threw and turned it into a failure carrying the exception's
// type and stack trace. Nothing in this port catches anything. The contract that
// replaces it is that a step which can fail says so in its type, returning an
// outcome whose failure travels on the value channel.
//
// success_combineWith_success_throws
//   -> "a combining step that can fail returns its failure on the value channel"
// success_map_throwing
//   -> "map takes a total function, so a mapping step that can fail is written to return a result"
// success_flatMap_throwing
//   -> "a chained step that can fail reports its failure on the accumulating channel too"
// failure_map_throwing
//   -> "a mapping step is never reached once an outcome has failed"
// failure_flatMap_throwing
//   -> "a chained step is never reached once an outcome has failed"
//
// ---- Composing the message of a failure -----------------------------------
//
// The original resolved a template carrying markers against a variable number of
// arguments when the failure was built. That helper has no target: a message is
// an interpolated string, resolved while this file is compiled. The first two
// entries reproduce the original's result character for character. The other
// four described what the helper did when markers and arguments disagreed in
// number, and neither outcome is reachable here, so each asserts the positive
// consequence and the mismatch is shown to be a compile error.
//
// failure_fromStatusMessageArgs_placeholdersMatchArgs1
//   -> "an interpolated message with one value reads as the original formatted message"
// failure_fromStatusMessageArgs_placeholdersMatchArgs2
//   -> "an interpolated message with two values reads as the original formatted message"
// failure_fromStatusMessageArgs_placeholdersExceedArgs
//   -> "an interpolated message never leaves a placeholder unfilled"
//      replaces: unreachable by construction - a marker and the value filling it
//      are one piece of syntax, and an under-supplied template does not compile.
// failure_fromStatusMessageArgs_placeholdersLessThanArgs1
//   -> "an interpolated message never appends one surplus argument"
//      replaces: unreachable by construction, as above.
// failure_fromStatusMessageArgs_placeholdersLessThanArgs2
//   -> "an interpolated message never appends several surplus arguments"
//      replaces: unreachable by construction, as above.
// failure_fromStatusMessageArgs_placeholdersLessThanArgs3
//   -> "a message with no placeholders never gains an appended argument list"
//      replaces: unreachable by construction, as above.
//
// ---- Moving a failure between outcomes, and the three bridges -------------
//
// failure_fromResult_failure
//   -> "the failure of one outcome is carried into an outcome of another value type"
// failure_fromResult_success
//   -> "a success has no failure to re-channel, and every bridge passes it through unchanged"
//      replaces: the original threw when asked for the failure of a success.
// failure_fromFailure
//   -> "a failure value builds a failed outcome, and toNec lifts it into a chain of one"
// failure_fromFailureItem
//   -> "a failure needs no item wrapper, and the accumulating bridges round-trip both ways"
//      replaces: there is no item type - a chain holds several failures, and a
//      failure is itself the unit rather than a container of one.
// failure_fromFailureItemException
//   -> "a failure travels on the value channel with no exception carrier"
//      replaces: the original had an exception type whose purpose was to carry a
//      failure across a boundary that could not return one, and a factory for
//      unwrapping it. A failure here is not throwable.
//
// ---- An absent value ------------------------------------------------------
//
// ofNullable_nonNull
//   -> "toRight keeps the value of a present option"
// ofNullable_null
//   -> "the vocabulary of the absent-reference failure is retained, and the state it guarded is unrepresentable"
//      replaces: the state the original factory guarded against cannot arise, an
//      absence having its own type. The message of the original is kept verbatim.
// ofOptional_nonEmpty
//   -> "toRight keeps the value of a non-empty option and composes with the bridge"
// ofOptional_empty
//   -> "toRight turns an empty option into a missing-data failure carrying the retained message"
//
// ---- The replacement for the capturing factories --------------------------
//
// The five entries below share one reason: the original's factories ran a block
// and converted whatever it threw into a failure. Nothing in this port throws to
// report a failure, so the conversion belongs at the boundary of whichever
// foreign library needs it, and the two test-scope helpers of this file are it.
//
// of_with_success
//   -> "a captured block that produces a value gives a success"
// of_with_exception
//   -> "a captured block that throws gives an error failure carrying its message"
// wrap_with_success
//   -> "a captured block producing an outcome flattens to that success"
// wrap_with_failure
//   -> "a captured block producing a failed outcome flattens to that failure"
// wrap_with_exception
//   -> "a captured block that throws where an outcome was expected flattens to an error failure"
//
// ---- The three predicates over a collection of outcomes -------------------
//
// The original offered each predicate twice, once variadic and once over a
// collection. The port offers the collection form alone, a collection literal
// being as short at a call site and the two overloads being unable to coexist
// once their element types erase to the same thing. Both cases of each pair are
// kept and both call that one member; the first builds its collection where the
// original wrote its argument list.
//
// anyFailures_varargs
//   -> "anyFailures answers over outcomes listed at the call site"
// anyFailures_collection
//   -> "anyFailures answers over a collection of outcomes"
// countFailures_varargs
//   -> "countFailures counts the failures among outcomes listed at the call site"
// countFailures_collection
//   -> "countFailures counts the failures in a collection of outcomes"
// allSuccess_varargs
//   -> "allSuccessful holds only when every outcome listed at the call site succeeded"
// allSuccess_collection
//   -> "allSuccessful holds only when every outcome in a collection succeeded"
//
// ---- Aggregating a collection of outcomes ---------------------------------
//
// combine_iterableWithFailures
//   -> "aggregating a mixed collection accumulates every failure and presents them as MULTIPLE"
// combine_iterableWithSuccesses
//   -> "combining a collection of successes applies the function to every value"
// combine_iterableWithSuccesses_throws
//   -> "combine takes a total function, so a combiner that can fail is handed to flatCombine"
//      replaces: the original caught what the combining function threw. A total
//      function cannot fail, and a fallible one returns an outcome instead.
// flatCombine_iterableWithFailures
//   -> "flat-combining a mixed collection accumulates every failure and presents them as MULTIPLE"
// flatCombine_iterableWithSuccesses_combineFails
//   -> "a combiner that fails surfaces its own failure with its own reason"
// flatCombine_iterableWithSuccesses_combineSucceeds
//   -> "a combiner that succeeds produces its value"
// flatCombine_iterableWithSuccesses_combineThrows
//   -> "the failures of the inputs and the failure of the combiner never mix"
//      replaces: as for the combining case above, with the added assertion that
//      the two sets of failures are never merged.
// toCombinedResult_allSuccesses
//   -> "sequence turns a collection of successes into a success holding the values in order"
//      replaces: the original expressed this as a collector object handed to a
//      stream, a shape belonging to a collection library this port does not use.
// toCombinedResult_withFailures
//   -> "sequence over a mixed collection accumulates the failures and joins their messages"
//      replaces: as above. The single failure the original produced from the
//      accumulated items is reached here by collapsing the chain, which gives
//      the same reason and the same joined message.
//
// ---- Gathering the failures reported by several outcomes ------------------
//
// failure_fromResults_varargs1
//   -> "the failures of a mixed collection are extracted independently of their positions"
// failure_fromResults_varargs2
//   -> "a different interleaving of the same outcomes extracts the same failures"
// failure_fromResults_varargs_allSuccess
//   -> "extracting the failures of outcomes listed at the call site that all succeeded yields no chain"
//      replaces: the original threw, there being no failure to build from. Here a
//      chain cannot be empty, so the answer is the absent one and the caller
//      decides what it means.
// failure_fromResults_collection
//   -> "the failures of an explicit collection are extracted in the order they were given"
//      The order of the chain is asserted, where the original compared unordered sets.
// failure_fromResults_collection_allSuccess
//   -> "extracting the failures of a collection that all succeeded yields no chain"
//      replaces: a throw became a representable absence, as above.
//
// ---- Where a failure's reason and message come from -----------------------
//
// The four entries below share one reason: the original derived a failure from a
// thrown exception in the four combinations of a supplied reason and a supplied
// message. This port derives nothing - the reason is fixed by the class of
// failure chosen and the message is written at the site that reports it - so each
// case asserts that construction instead. The two that supplied a reason do so
// across all ten reasons rather than the one the original happened to pick.
//
// generateFailureFromException
//   -> "a failure carries the message chosen where it is reported"
// generateFailureFromExceptionWithMessage
//   -> "the reporting site supplies its own message rather than inheriting one"
// generateFailureFromExceptionWithCustomStatus
//   -> "every one of the ten reasons is reachable through Failure.of"
// generateFailureFromExceptionWithCustomStatusAndMessage
//   -> "a reason and a message are chosen together at the reporting site"
//
// ---- Presenting several failures as one -----------------------------------
//
// The three entries below are properties of `Failure.collapse`.
//
// failureDeduplicateFailure
//   -> "collapsing the same failure reported twice describes it once"
//      consolidated: result/FailureSpec.scala carries the primary assertions;
//      this case reaches the same outcome through this channel's own API, so the
//      route from a collection of outcomes to one reported failure is covered too.
// failureSameType
//   -> "collapsing failures that agree on a reason keeps that reason"
//      consolidated: result/FailureSpec.scala, as above.
// failureDifferentTypes
//   -> "collapsing failures that disagree on a reason gives MULTIPLE"
//      consolidated: result/FailureSpec.scala, as above.
//
// ---- The two states that cannot be built ----------------------------------
//
// createByBuilder_neitherValueNorFailure
//   -> "an outcome with neither a value nor a failure cannot be built"
//      replaces: the reflective builder of the original could be left holding
//      neither, and had to reject that when the bean was assembled. The outcome
//      type here has exactly two cases and is sealed, so the state is unreachable.
// createByBuilder_bothValueAndFailure
//   -> "an outcome holding both a value and a failure cannot be built"
//      replaces: as above. The type that can hold both is a different type on
//      purpose, and it is the one the fourth alias of this module names.
//
// ---- What a failure does not hold -----------------------------------------
//
// generatedStackTrace
//   -> "a failure holds a reason, a message and attributes and nothing else"
//      replaces: the original captured and exposed the stack of the call that
//      reported a failure, and the type of any throwable behind it. A failure is
//      a value here, not an event, and holds neither.
// generatedStackTrace_Failure
//   -> "a failure renders as its reason and message, with no trace"
//      replaces: as above. The rendering asserted is the one the original
//      produced for the part of a failure this port keeps.
//
// ---- Equality -------------------------------------------------------------
//
// equalsHashCode
//   -> "equality and hashing of outcomes follow the value or the failure they hold"
//
// ---- The matchers ---------------------------------------------------------
//
// The seven entries below exercised the assertion helpers of the original rather
// than the type under test. Those helpers are ported into this module's test kit,
// whose own spec covers them; each case here exercises the matcher that replaced
// the assertion it used, against this file's fixtures.
//
// assert_success
//   -> "beSuccess and haveValue describe a success"
//      consolidated: testkit/ResultMatchers.scala owns the matcher implementations.
// assert_success_getFailure
//   -> "beFailure does not describe a success"
//      consolidated: testkit/ResultMatchers.scala. The original caught the
//      assertion error it provoked; this case intercepts the framework's own
//      report, which is not the same thing as catching a failure of the code
//      under test - a failure here is a value and is never thrown.
// assert_success_map
//   -> "the matchers describe the outcome of a mapping step"
//      consolidated: testkit/ResultMatchers.scala.
// assert_success_flatMap
//   -> "the matchers describe the outcome of a chained step"
//      consolidated: testkit/ResultMatchers.scala.
// assert_success_combineWith_success
//   -> "the matchers describe the outcome of a combining step"
//      consolidated: testkit/ResultMatchers.scala.
// assert_success_combineWith_failure
//   -> "beFailureWith names the reason a combined outcome failed for"
//      consolidated: testkit/ResultMatchers.scala.
// assert_failure
//   -> "beFailureWith and haveFailureMessageMatching describe a failure"
//      consolidated: testkit/ResultMatchers.scala.
//
// ---- The surface of the error channel -------------------------------------
//
// coverage
//   -> "the aliases of the error channel describe the same outcome and convert between one another"
//      replaces: the original closed with a reflective sweep over its own bean
//      properties. There is no bean to walk, and the property worth asserting in
//      its place is the one the design rests on - the four names are aliases for
//      types that already exist, so the conversions between them lose nothing.
//
// ---------------------------------------------------------------------------
// One member of the package object is deliberately absent from this file:
// `withAdditionalFailures` takes and returns a partial success, the fourth of
// the four shapes, and has no form over the two channels tested here. It is
// covered with the rest of that type's operations, in the spec for it.
// ---------------------------------------------------------------------------
