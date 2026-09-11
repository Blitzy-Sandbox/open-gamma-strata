/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.testkit

import scala.collection.immutable.List

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.Validated

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries.genFailure
import com.opengamma.strata.collect.Arbitraries.genFailureReason
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.result.ValidatedFailures
import com.opengamma.strata.collect.result.ValueWithFailures

/**
 * Tests the matchers of this test kit and the [[Outcome]] type class they read an outcome
 * through.
 *
 * ===Why a spec for test code===
 *
 * Every other spec of these two modules asserts something about the library through these
 * matchers, which makes them the one piece of test code whose own correctness cannot be
 * inferred from the suites that use it. A matcher that silently agreed with everything - a
 * `beFailureWith` that ignored the reason it was given, an [[Outcome]] instance that called
 * a value accompanied by failures a success - would leave every one of those suites green
 * while the assertions inside them stopped discriminating. This file is where that cannot
 * happen: each matcher is asserted here on an outcome it must *reject* as well as on one it
 * must accept, so a matcher weakened to a coarser question fails a case in this file rather
 * than passing silently everywhere else.
 *
 * ===How the cases are written===
 *
 * Two styles appear below, deliberately.
 *
 *   - An ordinary assertion - `outcome should beFailureWith(...)`, `outcome shouldNot
 *     beSuccess` - is what a consuming spec writes, and it is what most cases here use.
 *     Where the point of a case is the text a failed assertion reports, the assertion is
 *     provoked and its report is caught with `intercept`, which is the only way to observe
 *     the message a spec author would actually read.
 *   - `MatcherFactory1#matcher` is applied directly where a diagnostic has to be read
 *     without an assertion failing - both diagnostics of a matcher, positive and negated,
 *     exist for every outcome, but only one of them is ever displayed - and that is how the
 *     branches of the rendering helpers are reached exhaustively, including the phrase for
 *     an outcome with no failures at all.
 *
 * The expected text of every diagnostic is written out as a literal rather than rebuilt by
 * calling the renderings the matchers use, because a rendering compared against itself
 * would agree however it changed.
 *
 * ===Fixtures===
 *
 * The failures below carry no apostrophe, brace or comma in any message or attribute value.
 * That is not stylistic: a diagnostic assembled from several failures joins them with a
 * comma, so a comma inside a message would make the assertion about their order ambiguous,
 * and the framework routes a raw message through a format step when a message carries
 * arguments, which braces and apostrophes are the syntax of.
 *
 * @see [[ResultMatchers]] for the matchers under test
 * @see [[Outcome]] for the rule that an outcome is a success exactly when it holds no
 *   failures, which is what every case here rests on
 */
final class ResultMatchersSpec
    extends AnyFunSuite
    with Matchers
    with ResultMatchers
    with ScalaCheckPropertyChecks {

  // ---------------------------------------------------------------------------
  // Fixtures.
  //
  // Four failures with four distinct reasons, so that a case can ask for a
  // reason that is genuinely absent from an outcome, and a chain of three of
  // them in a fixed order, so that a diagnostic can be asserted to report that
  // order rather than any order.
  // ---------------------------------------------------------------------------

  /** A general error, the first failure of the chain below. */
  private val ErrorFailure: Failure = Failure.Error("Rate lookup failed")

  /** A parsing failure, the second failure of the chain below. */
  private val ParsingFailure: Failure = Failure.Parsing("Text does not name a tenor")

  /** A missing-data failure, the third failure of the chain below. */
  private val MissingFailure: Failure = Failure.MissingData("No holiday calendar for GBLO")

  /** A failure whose reason belongs to none of the three above, so it is always absent. */
  private val InvalidFailure: Failure = Failure.Invalid("Schedule is invalid")

  /** A failure carrying one attribute, for the attribute clause of a single-failure phrase. */
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

  /** The value the successful and partially successful outcomes below carry. */
  private val Answer: Int = 7

  /** The reasons the matrix case enumerates, which is the whole closed family of ten. */
  private val AllReasons: List[FailureReason] = FailureReason.values.toList

  /** The message the matrix case gives every failure it builds. */
  private val MatrixMessage: String = "Reason matrix fixture"

  // ---------------------------------------------------------------------------
  // Rendered phrases.
  //
  // The text a diagnostic is expected to carry, written out once because
  // several cases assert the same phrase inside different sentences. A single
  // failure renders as its reason and message; several render as a count
  // followed by the rendering the failure type publishes for itself, in the
  // order the outcome holds them.
  // ---------------------------------------------------------------------------

  /** The phrase describing the single general error. */
  private val ErrorPhrase: String = "reason: <ERROR> and message: <Rate lookup failed>"

  /** The phrase describing the single invalid-input failure. */
  private val InvalidPhrase: String = "reason: <INVALID> and message: <Schedule is invalid>"

  /** The phrase describing the three failures of the chain, in the order the chain holds them. */
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

  // ---------------------------------------------------------------------------
  // Helpers.
  //
  // Each names the alias the matchers resolve their view of an outcome from: a
  // bare `Left(...)` or `Validated.invalid(...)` has the type of the subclass,
  // for which no instance is found, so every outcome in this file is built
  // through one of these and arrives at its declared type.
  // ---------------------------------------------------------------------------

  /** Returns the single-failure outcome holding the specified failure. */
  private def singleFailure(failure: Failure): FailureOr[Int] = Left(failure)

  /** Returns the single-failure outcome holding the specified value. */
  private def singleSuccess(value: Int): FailureOr[Int] = Right(value)

  /** Returns the accumulating-channel outcome holding the specified chain of failures. */
  private def chainFailure(failures: NonEmptyChain[Failure]): ResultNec[Int] = Left(failures)

  /** Returns the accumulating-channel outcome holding the specified value. */
  private def chainSuccess(value: Int): ResultNec[Int] = Right(value)

  /** Returns the validating outcome holding the specified chain of failures. */
  private def validatedFailure(failures: NonEmptyChain[Failure]): ValidatedFailures[Int] =
    Validated.invalid(failures)

  /** Returns the validating outcome holding the specified value. */
  private def validatedSuccess(value: Int): ValidatedFailures[Int] = Validated.valid(value)

  /** Returns the partial-success outcome holding both the specified failures and value. */
  private def partial(failures: NonEmptyChain[Failure], value: Int): ValueWithFailures[Int] =
    Ior.both(failures, value)

  /** Returns the partial-success outcome holding failures and no value at all. */
  private def failuresOnly(failures: NonEmptyChain[Failure]): ValueWithFailures[Int] =
    Ior.left(failures)

  /** Returns the partial-success outcome holding a value and no failures. */
  private def valueOnly(value: Int): ValueWithFailures[Int] = Ior.right(value)

  /**
   * Returns twice the value of any outcome that carries a whole number, if it carries one.
   *
   * This is the helper a spec of its own writes over the type class, and it is written in
   * terms of [[Outcome.Aux]] because it has to name the value type: the arithmetic below
   * needs the compiler to know the value is a whole number, which plain `Outcome[R]`, whose
   * value type is abstract, does not say. The alias is a published member for exactly this,
   * so exercising it here exercises it as the API it is.
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

  // ---------------------------------------------------------------------------
  // `beFailureWith`: the reason is what it matches on.
  //
  // The cases below are the negative control the consuming suites cannot
  // provide. Each builds a genuine failure - not a success - carrying a reason
  // other than the one asked for, and asserts that the matcher rejects it. A
  // `beFailureWith` reduced to the question `beFailure` already answers would
  // pass every assertion in every other suite of both modules and fail here.
  // ---------------------------------------------------------------------------

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

  // ---------------------------------------------------------------------------
  // `beSuccess` and `haveValue`: a value accompanied by failures is not a
  // success.
  //
  // This is the rule of the type class that surprises, and it is the one an
  // implementation could get wrong without any other suite noticing: an
  // outcome that holds a value *and* failures would be called a success by any
  // matcher that decided from the presence of a value. Every assertion below
  // therefore names the value the outcome actually carries, so that deciding
  // from the value rather than from the failures cannot pass.
  // ---------------------------------------------------------------------------

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

  // ---------------------------------------------------------------------------
  // The `Outcome` instances.
  //
  // The type class is the whole of what the matchers know about an outcome, so
  // each of its four instances is read directly here rather than only through
  // an assertion: which failures are present, in which order, and whether
  // there is a value. Every matcher sentence asserted further down is a
  // rendering of exactly these two answers.
  // ---------------------------------------------------------------------------

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

  // ---------------------------------------------------------------------------
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
  // ---------------------------------------------------------------------------

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

  // ---------------------------------------------------------------------------
  // The two published ways in, and the extension point.
  // ---------------------------------------------------------------------------

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

    // This suite mixes the trait in, which is one of the two documented ways in. The other
    // is the wildcard import of the companion, which is what the consuming specs of both
    // modules write, and `ImportedMatchers` is that path - in a scope of its own, so that
    // the two ways in do not bind the same five names in one place.
    ImportedMatchers.successOf(success).matches shouldBe true
    ImportedMatchers.successOf(failed).matches shouldBe false
    ImportedMatchers.valueOf(success, Answer).matches shouldBe true
    ImportedMatchers.valueOf(success, Answer + 1).matches shouldBe false
    ImportedMatchers.failureOf(failed).matches shouldBe true
    ImportedMatchers.failureOf(success).matches shouldBe false
    ImportedMatchers.reasonOf(failed, FailureReason.ERROR).matches shouldBe true
    ImportedMatchers.reasonOf(failed, FailureReason.PARSING).matches shouldBe false
    ImportedMatchers.messageOf(failed, "Rate lookup failed").matches shouldBe true
    ImportedMatchers.messageOf(failed, "Rate").matches shouldBe false

    // The two ways in are the same matchers, so a sentence read through one is the sentence
    // read through the other.
    ImportedMatchers.successOf(failed).failureMessage shouldBe
      beSuccess.matcher[FailureOr[Int]].apply(failed).failureMessage
  }
}

/**
 * An outcome type belonging to a caller rather than to this library, used to exercise the
 * extension point of [[Outcome]].
 *
 * The four instances published by the type class are the four shapes this library returns,
 * and the type class is public so that a module built on it can describe a shape of its
 * own. This is such a shape: a lookup that answers with a value, with problems, or with
 * both. It is declared outside the spec class so that the instance in its companion is the
 * only thing that makes the matchers work on it - nothing the spec brings into scope can be
 * what resolves it.
 *
 * @param answer  the value the lookup found, if it found one
 * @param problems  the problems the lookup reported, in the order it reported them
 */
private final case class Lookup(answer: Option[Int], problems: List[Failure])

/**
 * Provides the [[Outcome]] instance for [[Lookup]], in the companion of the type it
 * describes, which is where an instance is found without an import.
 */
private object Lookup {

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
 * The matcher vocabulary reached through the wildcard import of the companion, which is the
 * path every consuming spec of both modules uses.
 *
 * The spec above mixes the trait in instead, those being the two documented ways in, and
 * the two cannot be used in one scope: each would bind the same five names, and a reference
 * to one of them would be ambiguous. This object is therefore the scope where the import
 * form is compiled and exercised, and the spec compares what it produces with what the
 * mixed-in form produces.
 */
private object ImportedMatchers {

  import ResultMatchers._

  /** Applies the imported `beSuccess` to an outcome of the value channel. */
  def successOf(result: FailureOr[Int]): MatchResult =
    beSuccess.matcher[FailureOr[Int]].apply(result)

  /** Applies the imported `haveValue` to an outcome of the value channel. */
  def valueOf(result: FailureOr[Int], expected: Int): MatchResult =
    haveValue(expected).matcher[FailureOr[Int]].apply(result)

  /** Applies the imported `beFailure` to an outcome of the value channel. */
  def failureOf(result: FailureOr[Int]): MatchResult =
    beFailure.matcher[FailureOr[Int]].apply(result)

  /** Applies the imported `beFailureWith` to an outcome of the value channel. */
  def reasonOf(result: FailureOr[Int], reason: FailureReason): MatchResult =
    beFailureWith(reason).matcher[FailureOr[Int]].apply(result)

  /** Applies the imported `haveFailureMessageMatching` to an outcome of the value channel. */
  def messageOf(result: FailureOr[Int], regex: String): MatchResult =
    haveFailureMessageMatching(regex).matcher[FailureOr[Int]].apply(result)
}

// ---------------------------------------------------------------------------
// Traceability.
//
// The assertion helpers this test kit is ported from - the two classes named
// on `testkit/ResultMatchers.scala` - carry no test of their own in the tree
// being ported: nothing there asserts that the reason an assertion was given
// is the reason it checked. That absence is why this file exists, and it is
// why none of the 27 cases above maps to a Java test method. They divide as:
//
//   - 10 cases on `beFailureWith`, which are the negative control the
//     consuming suites cannot provide - a genuine failure carrying a reason
//     other than the one asked for, in all four outcome shapes and in both
//     shapes a partial result can fail in, with the exhaustive matrix over the
//     ten reasons and the equivalence stated as a property;
//   - 4 cases on partial success, which pin the rule that a value accompanied
//     by failures is not a success and is not reported as a value mismatch;
//   - 6 cases on the type class itself, its four instances, the ordered
//     extraction of an accumulated chain, the value type named by the alias
//     and the attribute clause of a diagnostic;
//   - 5 cases reading both sentences of each of the five matchers for an
//     outcome with no failures, one failure and several;
//   - 2 cases on the published surface: an outcome type of the caller with its
//     own instance, and the two ways the vocabulary is brought into scope.
//
// All 27 count towards the test total this module is required to reach.
// ---------------------------------------------------------------------------
