/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.testkit

import scala.collection.immutable.List
import scala.util.matching.Regex

import cats.syntax.show._

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.dsl.MatcherFactory1

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.result.ValidatedFailures
import com.opengamma.strata.collect.result.ValueWithFailures

/**
 * The view of an outcome that the matchers of [[ResultMatchers]] assert over.
 *
 * An operation in this library that cannot produce a value returns the failures that
 * explain why rather than abandoning the call stack, and it does so in one of four shapes
 * depending on how many failures it can report and whether it can report a value and
 * failures together. This type class is what lets one vocabulary of matchers cover all
 * four: it reduces any of them to the two questions a matcher asks - which failures are
 * present, and is there a value - and an instance is published below for each shape.
 *
 * {{{
 * FailureOr[A]          a value, or the single failure that explains its absence
 * ResultNec[A]          a value, or a non-empty chain of failures
 * ValidatedFailures[A]  the same pair of possibilities in accumulating form
 * ValueWithFailures[A]  a value, failures, or both
 * }}}
 *
 * ===Success means no failures===
 *
 * The one rule the whole vocabulary rests on is that an outcome is a success exactly when
 * no failures are present. `failures` is therefore the primary operation and `value` is
 * secondary: a matcher decides success or failure from the emptiness of the failure list
 * alone, and reads the value only to describe what it found.
 *
 * The rule has one consequence worth stating twice, because it is the one that surprises:
 * a `ValueWithFailures` that holds a value *and* failures is not a success. `beSuccess`
 * and `haveValue` fail on it while `beFailure`, `beFailureWith` and
 * `haveFailureMessageMatching` succeed on it. That is the only reading consistent with the
 * type being ported, whose value accessor was reached only after its success check had
 * passed, and it leaves the partial-success case perfectly assertable - a spec that wants
 * to check both halves of such an outcome asserts its value and its failures separately,
 * through the ordinary accessors of the type, rather than through a matcher that would
 * have to call it one thing or the other.
 *
 * ===The value type===
 *
 * The type of the value is an abstract type member rather than a second type parameter, so
 * that an instance is keyed on the outcome type alone. That is what allows the matchers to
 * be resolved from the static type of the expression on the left of `should`, which is the
 * only place the outcome type is known. [[Outcome.Aux]] recovers the value type where it
 * needs to be named.
 *
 * ===Adding an instance===
 *
 * The four instances below are the four shapes this library returns, and a module built on
 * it needs no more than they cover. Nothing prevents a further instance being written for
 * a type of its own, however: the type class is public, its two operations are total, and
 * an instance placed in the companion of the type it describes is found without an import.
 * An instance must respect the rule above - it reports a failure if and only if the outcome
 * it describes is not a success - or the matchers will describe that type incorrectly.
 *
 * @tparam R  the outcome type this instance describes
 * @see [[ResultMatchers]] for the matchers built on this type class
 */
trait Outcome[R] {

  /** The type of the value the outcome carries when it has one. */
  type Value

  /**
   * Returns the failures the outcome holds, in the order it holds them.
   *
   * An empty list means the outcome is a success; anything else means it is a failure. The
   * order of a chain of accumulated failures is preserved, so a diagnostic built from this
   * list reads in the order the failures were produced.
   *
   * @param result  the outcome to inspect
   * @return the failures of the outcome, in order, empty when it is a success
   */
  def failures(result: R): List[Failure]

  /**
   * Returns the value the outcome carries, if it carries one.
   *
   * This is not the success test - `failures` is - because an outcome can carry a value and
   * failures at the same time. A value is returned whenever one is present, so that a
   * diagnostic can report it whichever side of the assertion it falls on.
   *
   * @param result  the outcome to inspect
   * @return the value of the outcome, or `None` when it has none
   */
  def value(result: R): Option[Value]
}

/**
 * Provides the instances of [[Outcome]] for the four shapes this library returns, together
 * with the alias that names the value type of an instance.
 *
 * The instances live here, in the companion of the type class, so that every one of them is
 * found by an ordinary implicit search with no import beyond the matchers themselves.
 */
object Outcome {

  /**
   * The type of an [[Outcome]] instance whose value type is known.
   *
   * `Outcome[R]` keeps the type of the value abstract, which is what makes an instance
   * resolvable from the outcome type alone. This alias is how the value type is named again
   * where it matters - in the declared type of each instance below, which is what documents
   * that, say, the outcome `ResultNec[A]` carries an `A` - and it is available to a spec
   * that wants to write a helper of its own over an outcome and its value.
   *
   * @tparam R  the outcome type
   * @tparam A  the type of the value the outcome carries
   */
  type Aux[R, A] = Outcome[R] { type Value = A }

  /**
   * The instance for an outcome that either produces a value or fails for a single reason.
   *
   * @tparam A  the type of the value produced when the operation succeeds
   * @return the instance describing `FailureOr[A]`
   */
  implicit def failureOr[A]: Aux[FailureOr[A], A] =
    new Outcome[FailureOr[A]] {
      type Value = A

      override def failures(result: FailureOr[A]): List[Failure] =
        result.fold(failure => List(failure), _ => List.empty)

      override def value(result: FailureOr[A]): Option[A] = result.toOption
    }

  /**
   * The instance for an outcome that either produces a value or fails for one or more
   * reasons.
   *
   * This is the shape a validating factory of this library returns, so it is the instance
   * most assertions go through. The chain is flattened in its own order.
   *
   * @tparam A  the type of the value produced when the operation succeeds
   * @return the instance describing `ResultNec[A]`
   */
  implicit def resultNec[A]: Aux[ResultNec[A], A] =
    new Outcome[ResultNec[A]] {
      type Value = A

      override def failures(result: ResultNec[A]): List[Failure] =
        result.fold(chain => chain.toChain.toList, _ => List.empty)

      override def value(result: ResultNec[A]): Option[A] = result.toOption
    }

  /**
   * The instance for the accumulating form of an outcome, used while several checks over
   * one input are being combined.
   *
   * @tparam A  the type of the value produced when every check passes
   * @return the instance describing `ValidatedFailures[A]`
   */
  implicit def validated[A]: Aux[ValidatedFailures[A], A] =
    new Outcome[ValidatedFailures[A]] {
      type Value = A

      override def failures(result: ValidatedFailures[A]): List[Failure] =
        result.fold(chain => chain.toChain.toList, _ => List.empty)

      override def value(result: ValidatedFailures[A]): Option[A] = result.toOption
    }

  /**
   * The instance for an outcome that can produce a value, failures, or both.
   *
   * The value of an outcome that holds both is reported by `value`, while `failures`
   * reports its failures, which by the rule of this type class makes it a failure. Neither
   * half is lost: both are available to the matcher that describes it.
   *
   * @tparam A  the type of the value, typically a collection type
   * @return the instance describing `ValueWithFailures[A]`
   */
  implicit def ior[A]: Aux[ValueWithFailures[A], A] =
    new Outcome[ValueWithFailures[A]] {
      type Value = A

      override def failures(result: ValueWithFailures[A]): List[Failure] =
        result.left.fold(List.empty[Failure])(chain => chain.toChain.toList)

      override def value(result: ValueWithFailures[A]): Option[A] = result.right
    }
}

/**
 * Matchers for the outcome of an operation that can fail, for use in the specs of this
 * module and of every module built on it.
 *
 * The five matchers here are the complete vocabulary of the assertion helper this file is
 * ported from, under names adapted to the matcher style, and they apply to all four outcome
 * shapes through the [[Outcome]] type class:
 *
 *   - `beSuccess` - the outcome holds no failures.
 *   - `haveValue(expected)` - the outcome holds no failures and its value equals `expected`.
 *   - `beFailure` - the outcome holds at least one failure.
 *   - `beFailureWith(reason)` - the outcome holds at least one failure carrying `reason`.
 *   - `haveFailureMessageMatching(regex)` - the outcome holds at least one failure whose
 *     message matches `regex` in full.
 *
 * ===Using them===
 *
 * Bring them into scope either by importing the members of the companion, which is what a
 * spec of this port conventionally does, or by mixing in this trait:
 *
 * {{{
 * import com.opengamma.strata.collect.testkit.ResultMatchers._
 *
 * class SomethingSpec extends AnyFunSuite with Matchers {
 *   test("parsing") {
 *     val parsed: ResultNec[Int] = parseCount("42")
 *     parsed should beSuccess
 *     parsed should haveValue(42)
 *     parseCount("x") should beFailureWith(FailureReason.PARSING)
 *     parseCount("x") should haveFailureMessageMatching(".*not a number.*")
 *   }
 * }
 * }}}
 *
 * They compose and negate like any other matcher - `should (beFailure and
 * haveFailureMessageMatching("Ooops"))` is the chained assertion of the helper being
 * ported, and `should not (beSuccess)` and `shouldNot beSuccess` both read the outcome the
 * other way round - and each of them reports what it actually found when it fails, which is
 * the whole reason this file exists. Asserting `outcome.isRight shouldBe true` says only
 * that the outcome was not a success; asserting `outcome should beSuccess` names the reason
 * and the message of every failure that was there instead.
 *
 * ===What counts as a success===
 *
 * An outcome is a success exactly when it holds no failures, so a value accompanied by
 * failures is a failure here: see [[Outcome]], where that rule and its consequences are
 * set out. Where an outcome can hold more than one failure, the three failure matchers hold
 * if *at least one* failure satisfies what was asked. That keeps every assertion ported
 * from a single-failure outcome exact, while remaining meaningful for an accumulated chain,
 * and it is why a diagnostic lists the whole chain rather than only its head.
 *
 * ===Why these are matcher factories===
 *
 * Each matcher is a `MatcherFactory1` over [[Outcome]] rather than a `Matcher`, because a
 * `Matcher` is contravariant in the type it matches: a bare `Matcher`-returning method with
 * an implicit type-class parameter leaves the outcome type undetermined at the point the
 * instance is searched for, and every instance then matches, ambiguously. A factory defers
 * the search to `should`, which knows the type of the expression on its left, so each of
 * the call sites above compiles with no type ascription and no explicit type argument.
 *
 * One consequence is worth knowing: the factory fixes the outcome type but not the type of
 * the value inside it, so `haveValue` compares the value it finds with the value it was
 * given using universal equality - exactly as the untyped assertion it is ported from did.
 * A comparison against a value of an unrelated type is therefore reported as a failed
 * assertion rather than rejected by the compiler. The [[Outcome.Aux]] alias remains the way
 * to name the value type where a spec wants a helper of its own that the compiler checks.
 *
 * The other consequence is that an outcome must be seen at its declared type: the instances
 * are invariant, so an expression whose static type is a subtype such as `Right[Nothing,
 * Int]` needs the ascription `(outcome: FailureOr[Int])` that the specs of this port apply
 * anyway by holding results in values of the alias types the library returns.
 *
 * ===A frozen contract===
 *
 * This trait, its companion, the [[Outcome]] type class and its instances are compiled in
 * the test scope of this module and made visible to the test scope of the dependent module
 * by the `test->test` edge of the build, which replaces the test-artifact dependency the
 * original build declared. Specs in both modules import these names, so every member is
 * public and no name may drift. Nothing specific to a dependent module belongs here: a
 * matcher for a type of that module lives in that module's own testkit and composes with
 * these at the call site, by importing both.
 *
 * @see [[Outcome]] for the four outcome shapes and the rule that decides success
 */
trait ResultMatchers {

  /**
   * Matches an outcome that holds no failures.
   *
   * A value accompanied by failures does not match, as set out in [[Outcome]]. On failure
   * the reason and message of every failure the outcome holds are reported; negated, the
   * value that was found is reported instead.
   *
   * {{{
   * outcome should beSuccess
   * outcome should not (beSuccess)
   * }}}
   *
   * @return the matcher factory for a successful outcome
   */
  val beSuccess: MatcherFactory1[Any, Outcome] =
    new MatcherFactory1[Any, Outcome] {
      override def matcher[R <: Any](implicit outcome: Outcome[R]): Matcher[R] =
        new Matcher[R] {
          override def apply(left: R): MatchResult = {
            val failures = outcome.failures(left)
            MatchResult(
              failures.isEmpty,
              s"Expected Success but was Failure with ${describeFailures(failures)}",
              s"Expected Failure but was Success with value: ${describeValue(outcome.value(left))}"
            )
          }
        }
    }

  /**
   * Matches an outcome that holds no failures and carries the specified value.
   *
   * The success test comes first, as it did in the assertion being ported, so an outcome
   * holding failures is reported as a failure rather than as a value mismatch - including
   * an outcome that holds this very value alongside failures.
   *
   * The comparison is universal equality, which is why the expected value is not checked
   * against the value type of the outcome at compile time; the reasoning is on
   * [[ResultMatchers]]. Every type of this library defines equality consistently with its
   * `Eq` instance, so the two notions cannot disagree about a value asserted here.
   *
   * {{{
   * outcome should haveValue(42)
   * }}}
   *
   * @param expected  the value the outcome is expected to carry
   * @tparam A  the type of the expected value
   * @return the matcher factory for a successful outcome carrying that value
   */
  def haveValue[A](expected: A): MatcherFactory1[Any, Outcome] =
    new MatcherFactory1[Any, Outcome] {
      override def matcher[R <: Any](implicit outcome: Outcome[R]): Matcher[R] =
        new Matcher[R] {
          override def apply(left: R): MatchResult = {
            val failures = outcome.failures(left)
            val actual = outcome.value(left)
            val described = describeValue(actual)
            MatchResult(
              failures.isEmpty && actual.exists(found => found == expected),
              if (failures.isEmpty) {
                s"Expected Success with value: <$expected> but was: $described"
              } else {
                s"Expected Success but was Failure with ${describeFailures(failures)}"
              },
              s"Expected Success with a value other than <$expected> but was: $described"
            )
          }
        }
    }

  /**
   * Matches an outcome that holds at least one failure.
   *
   * A value accompanied by failures matches, as set out in [[Outcome]]. On failure the
   * value that was found is reported; negated, the reason and message of every failure are
   * reported instead.
   *
   * {{{
   * outcome should beFailure
   * }}}
   *
   * @return the matcher factory for a failed outcome
   */
  val beFailure: MatcherFactory1[Any, Outcome] =
    new MatcherFactory1[Any, Outcome] {
      override def matcher[R <: Any](implicit outcome: Outcome[R]): Matcher[R] =
        new Matcher[R] {
          override def apply(left: R): MatchResult = {
            val failures = outcome.failures(left)
            MatchResult(
              failures.nonEmpty,
              s"Expected Failure but was Success with value: ${describeValue(outcome.value(left))}",
              s"Expected Success but was Failure with ${describeFailures(failures)}"
            )
          }
        }
    }

  /**
   * Matches an outcome that holds at least one failure carrying the specified reason.
   *
   * The failure test comes first, as it did in the assertion being ported, so a successful
   * outcome is reported as such rather than as a reason mismatch. Where the outcome holds
   * several failures it is enough that one of them carries the reason; the diagnostic of a
   * mismatch lists the reason and message of every failure present, in the order the
   * outcome holds them, so an accumulated chain can be read at a glance.
   *
   * The reason is compared as a value of the closed family of reasons, so a reason spelled
   * as text cannot be passed here and a member that does not exist cannot be named.
   *
   * {{{
   * outcome should beFailureWith(FailureReason.PARSING)
   * }}}
   *
   * @param reason  the reason a failure of the outcome is expected to carry
   * @return the matcher factory for a failed outcome carrying that reason
   */
  def beFailureWith(reason: FailureReason): MatcherFactory1[Any, Outcome] =
    new MatcherFactory1[Any, Outcome] {
      override def matcher[R <: Any](implicit outcome: Outcome[R]): Matcher[R] =
        new Matcher[R] {
          override def apply(left: R): MatchResult = {
            val failures = outcome.failures(left)
            val described = describeFailures(failures)
            MatchResult(
              failures.exists(failure => failure.reason == reason),
              if (failures.isEmpty) {
                s"Expected Failure with reason: <${reason.show}> but was Success with value: " +
                  describeValue(outcome.value(left))
              } else {
                s"Expected Failure with reason: <${reason.show}> but was Failure with $described"
              },
              s"Expected Failure with a reason other than <${reason.show}> but was Failure with $described"
            )
          }
        }
    }

  /**
   * Matches an outcome that holds at least one failure whose message matches the specified
   * regular expression.
   *
   * The failure test comes first, as it did in the assertion being ported. The match is
   * against the whole message, which is the behaviour of the string match the ported
   * assertion used: the pattern `bad` does not match the message `very bad input`, while
   * `.*bad.*` does. Where the outcome holds several failures it is enough that one message
   * matches; the diagnostic of a mismatch lists every message present, in the order the
   * outcome holds them.
   *
   * The pattern is compiled once, when the matcher is built, so a malformed pattern is
   * reported against the line of the spec that wrote it.
   *
   * {{{
   * outcome should haveFailureMessageMatching(".*not a number.*")
   * }}}
   *
   * @param regex  the regular expression a failure message is expected to match in full
   * @return the matcher factory for a failed outcome whose message matches
   */
  def haveFailureMessageMatching(regex: String): MatcherFactory1[Any, Outcome] = {
    val pattern: Regex = regex.r
    new MatcherFactory1[Any, Outcome] {
      override def matcher[R <: Any](implicit outcome: Outcome[R]): Matcher[R] =
        new Matcher[R] {
          override def apply(left: R): MatchResult = {
            val failures = outcome.failures(left)
            val described = describeMessages(failures)
            MatchResult(
              failures.exists(failure => pattern.matches(failure.message)),
              if (failures.isEmpty) {
                s"Expected Failure with message matching: <$regex> but was Success with value: " +
                  describeValue(outcome.value(left))
              } else {
                s"Expected Failure with message matching: <$regex> but was Failure with $described"
              },
              s"Expected Failure with no message matching: <$regex> but was Failure with $described"
            )
          }
        }
    }
  }

  /**
   * Renders the failures of an outcome as the reason-and-message phrase of a diagnostic.
   *
   * A single failure renders in the form the assertion being ported used, extended with its
   * attributes when it has any, so that the data the message refers to is visible too.
   * Several failures render as a count followed by each failure in the order the outcome
   * holds them, using the rendering the failure type publishes for itself. The empty case
   * cannot be reached by a diagnostic that is displayed - every use of this phrase is
   * guarded by the presence of a failure - and is rendered rather than rejected so that
   * this helper is total.
   *
   * @param failures  the failures to render
   * @return the phrase describing them
   */
  private def describeFailures(failures: List[Failure]): String =
    failures match {
      case Nil =>
        "no failures"
      case single :: Nil =>
        val rendered = s"reason: <${single.reason.show}> and message: <${single.message}>"
        if (single.attributes.isEmpty) {
          rendered
        } else {
          val attributes = single.attributes.iterator.map { case (key, value) => s"$key=$value" }
          s"$rendered and attributes: <${attributes.mkString(", ")}>"
        }
      case several =>
        val rendered = several.iterator.map(failure => s"<${failure.show}>")
        s"${several.size} failures: ${rendered.mkString(", ")}"
    }

  /**
   * Renders the messages of the failures of an outcome as the message phrase of a
   * diagnostic.
   *
   * This is the phrase the message matcher reports, so it carries the messages alone,
   * without their reasons, in the form the assertion being ported used. Several failures
   * render as every message in the order the outcome holds them.
   *
   * @param failures  the failures whose messages are to be rendered
   * @return the phrase describing their messages
   */
  private def describeMessages(failures: List[Failure]): String =
    failures match {
      case Nil =>
        "no failures"
      case single :: Nil =>
        s"message: <${single.message}>"
      case several =>
        val rendered = several.iterator.map(failure => s"<${failure.message}>")
        s"messages: ${rendered.mkString(", ")}"
    }

  /**
   * Renders the value of an outcome for a diagnostic.
   *
   * The delimiters are those of the assertion being ported and are kept because they make
   * an empty or blank value visible in the output. An outcome with no value at all - the
   * failing side of any of the four shapes - is described as such rather than omitted, so
   * the phrase reads completely either way.
   *
   * @param value  the value to render, if there is one
   * @return the phrase describing it
   */
  private def describeValue(value: Option[Any]): String =
    value.fold("<no value>")(actual => s"<$actual>")
}

/**
 * The matchers of [[ResultMatchers]], ready to be imported.
 *
 * This object is what makes the single wildcard import
 *
 * {{{
 * import com.opengamma.strata.collect.testkit.ResultMatchers._
 * }}}
 *
 * supply the whole vocabulary, which is the role the helper this file is ported from played
 * for the assertion style it belonged to. It supplies these matchers and nothing else: the
 * matchers of the test framework itself are brought in by the spec, by extending the
 * framework's own matcher trait, so that a spec sees exactly one definition of each name.
 */
object ResultMatchers extends ResultMatchers
