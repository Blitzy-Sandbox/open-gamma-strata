/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.collection.immutable.List
import scala.collection.immutable.Set

import cats.Semigroup
import cats.data.Chain
import cats.data.EitherNec
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec

/**
 * The result model: how an operation reports that it could not produce a value, and what a
 * caller does with that report.
 *
 * Code in this package, and in the modules built on it, is written in a functional style.
 * An operation that can fail returns a value describing its outcome rather than abandoning
 * the call stack, so the decision of what to do about a failure belongs to the caller that
 * has the context to make it. Every such outcome is ordinary immutable data built from a
 * [[Failure]], and takes one of four shapes:
 *
 * {{{
 * FailureOr[A]          // a value, or a single failure
 * ResultNec[A]          // a value, or one or more failures
 * ValidatedFailures[A]  // the same pair of possibilities, in accumulating form
 * ValueWithFailures[A]  // a value, failures, or both - partial success
 * }}}
 *
 * The four names are aliases, not classes. Each names a type that cats already provides,
 * with the failure type applied, and that is the point: every combinator cats defines on
 * `Either`, `Validated` and `Ior` applies directly to the outcome of an operation in this
 * library. `map`, `flatMap`, `leftMap`, `traverse`, `mapN`, `getOrElse`, `fold` and the
 * rest are inherited rather than written again here, so this file adds only the few
 * operations that have no counterpart among them.
 *
 * ===Which shape to return===
 *
 * The choice follows the question being answered rather than the taste of the author:
 *
 *   - `FailureOr[A]` when there is exactly one way to fail. Looking a name up, resolving an
 *     identifier against supplied data or converting between two currencies either works or
 *     reports the one thing that stopped it.
 *   - `ResultNec[A]` when several independent checks apply to the same input and a caller is
 *     better served by hearing about all of them at once. This is what the validating
 *     factory of a type returns, which is why it is the most common of the four.
 *   - `ValidatedFailures[A]` while the checks are being combined, because that is the form
 *     in which cats accumulates: several checks are assembled with `mapN` or `product`, and
 *     the assembled value is handed back to the caller as a `ResultNec` through `toResult`.
 *     `Validate` produces values of this type for exactly that purpose.
 *   - `ValueWithFailures[A]` when partial success is meaningful - an operation over many
 *     inputs where the ones that worked are worth returning alongside a report of the ones
 *     that did not.
 *
 * A chain of failures that has to be presented as a single failure is reduced with
 * `Failure.collapse`, which is the one place that aggregation is written.
 *
 * ===Importing the names===
 *
 * The module root re-exports all four aliases, so each of them is reachable both as
 * `com.opengamma.strata.collect.FailureOr` and as
 * `com.opengamma.strata.collect.result.FailureOr`. They are the same type either way. A file
 * that wildcard-imports both packages at the same nesting level, however, sees each name
 * twice and every use of it becomes an ambiguous reference, so a file should import these
 * names from exactly one of the two packages - conventionally the module root, alongside
 * whatever else it needs from there.
 *
 * ===What became of the two classes being ported===
 *
 * The original expressed both shapes as immutable beans: one class holding either a value or
 * a failure with some forty instance methods over that pair, and another holding a value
 * together with a list of failures. Neither survives as a class, because the language this
 * port targets has the sum type and the accumulating applicative that the original had to
 * simulate. What is left is this file: four names, three conversions between them, the
 * handful of collection-level operations the original provided as statics, and the helpers
 * that build a partial success.
 *
 * Three groups of members from those classes have no counterpart here, by design. The
 * factories that ran a supplied block and turned a thrown exception into a failure are gone,
 * because nothing in this port throws in order to report a failure - a failure is returned.
 * The factories that turned an absent value into a failure are gone, because an absence is
 * an `Option` and `Either.fromOption` already bridges the two. And the collector factories,
 * with the mutable accumulators behind them, are replaced by `sequence` and the two
 * `combineValues` helpers, which do the same accumulation as pure folds.
 *
 * @see [[Failure]] for the failure a result carries
 * @see [[FailureReason]] for the ten reasons a failure can carry
 */
package object result {

  // ---------------------------------------------------------------------------
  // The four names that make up the error channel of this library.
  // ---------------------------------------------------------------------------

  /**
   * The outcome of an operation that either produces a value or fails for a single reason.
   *
   * `Right` carries the value; `Left` carries the one [[Failure]] that describes why there
   * is no value. This is the shape to prefer wherever an operation has a single way of
   * failing, because it says so in the type: a caller reading it knows there is exactly one
   * failure to handle and can do so without folding over a collection.
   *
   * The alias takes one type parameter, with the failure type already applied, which is what
   * makes it usable in a position that requires a type constructor of one parameter. A
   * function that reads some ambient context before it can produce a value is written as a
   * `Kleisli` over precisely this alias, and no compiler plugin is needed to write it:
   *
   * {{{
   * type Reader[A] = Kleisli[FailureOr, Context, A]
   * }}}
   *
   * That arity is part of the contract of this module. Spelling the failure type out at the
   * use site instead would take the alias to two parameters and break every such position.
   *
   * @tparam A  the type of the value produced when the operation succeeds
   */
  type FailureOr[A] = Either[Failure, A]

  /**
   * The outcome of an operation that either produces a value or fails for one or more
   * reasons.
   *
   * `Right` carries the value; `Left` carries a non-empty chain of failures, in the order in
   * which they were found. This is what the validating factory of a type returns, because a
   * value is usually rejected by more than one check at once and a caller that is told about
   * all of them can correct its input in one pass rather than several.
   *
   * A single failure is lifted into this shape with `toNec`, and the accumulating form used
   * while the checks are being combined is reached with `toValidated`:
   *
   * {{{
   * val lifted: ResultNec[A] = toNec(resultWithOneFailure)
   * val accumulated: ResultNec[A] = toResult(checksCombinedWithMapN)
   * }}}
   *
   * @tparam A  the type of the value produced when the operation succeeds
   */
  type ResultNec[A] = EitherNec[Failure, A]

  /**
   * The accumulating form of a result, used while several checks over the same input are
   * being combined.
   *
   * `Valid` carries the value; `Invalid` carries a non-empty chain of failures. This differs
   * from [[ResultNec]] in how it combines rather than in what it can hold: combining two
   * `Either` values stops at the first `Left`, whereas combining two `Validated` values keeps
   * the failures of both. That is why the checks of a validating factory are written over
   * this type and the assembled value is converted once, at the end:
   *
   * {{{
   * (checkTheScheme, checkTheValue).mapN(build).toEither  // a ResultNec, failures of both
   * }}}
   *
   * `Validate` returns values of this type for exactly this reason, and `toResult` performs
   * the final conversion where the assembled expression has not already done so.
   *
   * @tparam A  the type of the value produced when every check passes
   */
  type ValidatedFailures[A] = ValidatedNec[Failure, A]

  /**
   * A value, failures, or both: the outcome of an operation that can partly succeed.
   *
   * The classic use is reading many records, where some are usable and some are not: the
   * usable ones are the value, and one failure is reported for each record that was not.
   * Because `Ior` has three cases, the outcome says which of the three happened:
   *
   *   - `Ior.Right(value)` - everything worked, and there is nothing to report.
   *   - `Ior.Both(failures, value)` - part of the work succeeded and part did not.
   *   - `Ior.Left(failures)` - nothing usable came out of the operation.
   *
   * The third case is a deliberate widening of the type being ported, which always held a
   * value alongside its (possibly empty) list of failures and therefore had no way to say
   * "nothing usable at all" other than by convention, typically an empty collection as the
   * value. Every helper in the nested `ValueWithFailures` object below accepts all three
   * cases and preserves the behaviour of the original for the two it could express.
   *
   * Being an `Ior`, this type maps over its value, maps over its failures with `leftMap`,
   * chains with `flatMap` and combines with `combine`, all from cats; the helpers below cover
   * only what cats does not already provide.
   *
   * @tparam A  the type of the value, typically a collection type
   */
  type ValueWithFailures[A] = Ior[NonEmptyChain[Failure], A]

  // ---------------------------------------------------------------------------
  // Conversions between the four shapes.
  //
  // Each conversion has its own name rather than being an overload of a shared
  // one: a parameter of type `FailureOr[A]` and a parameter of type
  // `ResultNec[A]` are both an `Either` once the types are erased, so two such
  // overloads could not coexist. Naming them separately also makes a call site
  // say which direction it is converting in.
  // ---------------------------------------------------------------------------

  /**
   * Lifts a result that carries a single failure into one that carries a chain of failures.
   *
   * The value of a success passes through untouched, and the failure of a failure becomes the
   * only element of the chain. This is the bridge to take when a fail-fast operation feeds
   * into an accumulating one:
   *
   * {{{
   * val single: FailureOr[Int] = Left(Failure.Parsing("Not a number"))
   * toNec(single) // Left(NonEmptyChain(Failure.Parsing("Not a number")))
   * }}}
   *
   * @tparam A  the type of the value
   * @param result  the result to lift
   * @return the same outcome, with its failure held in a chain
   */
  def toNec[A](result: FailureOr[A]): ResultNec[A] = result.left.map(NonEmptyChain.one)

  /**
   * Converts a result into the accumulating form that combines with `mapN`.
   *
   * This is defined on [[ResultNec]] alone, for the erasure reason given above; a
   * [[FailureOr]] reaches the same destination through `toNec` first.
   *
   * @tparam A  the type of the value
   * @param result  the result to convert
   * @return the accumulating form of the result
   */
  def toValidated[A](result: ResultNec[A]): ValidatedFailures[A] = Validated.fromEither(result)

  /**
   * Converts an accumulated set of checks back into a result.
   *
   * This is the conversion a validating factory performs last, so that what a caller receives
   * is the ordinary right-biased type it can `flatMap` over.
   *
   * @tparam A  the type of the value
   * @param validated  the accumulated outcome to convert
   * @return the result form of the outcome
   */
  def toResult[A](validated: ValidatedFailures[A]): ResultNec[A] = validated.toEither

  // ---------------------------------------------------------------------------
  // Operations over a collection of results.
  //
  // These are generic in the failure type, bounded only where accumulation
  // requires it, so that one member serves `FailureOr`, `ResultNec` and any
  // other `Either` a caller has to hand.
  // ---------------------------------------------------------------------------

  /**
   * Checks whether every result in the collection is a success.
   *
   * An empty collection is all successes, as it is for every universally quantified
   * predicate.
   *
   * @tparam E  the type of the failure held by a failed result
   * @tparam A  the type of the value held by a successful result
   * @param results  the results to examine
   * @return true if every result is a success
   */
  def allSuccessful[E, A](results: IterableOnce[Either[E, A]]): Boolean =
    results.iterator.forall(_.isRight)

  /**
   * Checks whether any result in the collection is a failure.
   *
   * An empty collection contains no failures, so this returns false for one.
   *
   * @tparam E  the type of the failure held by a failed result
   * @tparam A  the type of the value held by a successful result
   * @param results  the results to examine
   * @return true if at least one result is a failure
   */
  def anyFailures[E, A](results: IterableOnce[Either[E, A]]): Boolean =
    results.iterator.exists(_.isLeft)

  /**
   * Counts the results in the collection that are failures.
   *
   * The method being ported returned a 64-bit count because the stream it counted over did.
   * This returns an `Int`, which is what the collection library of this language counts with,
   * and it is returned as such rather than widened: this build rejects an implicit widening
   * of a number, so a caller that genuinely needs a wider count converts the result itself.
   * A collection large enough for the difference to matter cannot be held in memory anyway.
   *
   * @tparam E  the type of the failure held by a failed result
   * @tparam A  the type of the value held by a successful result
   * @param results  the results to examine
   * @return the number of results that are failures
   */
  def countFailures[E, A](results: IterableOnce[Either[E, A]]): Int =
    results.iterator.count(_.isLeft)

  /**
   * Turns a collection of results into a result holding the collection of values,
   * accumulating every failure.
   *
   * When every result is a success the values are returned in the order they were given in.
   * When any result is a failure the failures of all of them are combined - not just the
   * first - which is why this is written over the accumulating form internally and why the
   * failure type has to be combinable:
   *
   * {{{
   * sequence(List(Right(1), Left(chainA), Left(chainB))) // Left(chainA ++ chainB)
   * sequence(List(Right(1), Right(2)))                   // Right(List(1, 2))
   * }}}
   *
   * The `Semigroup` bound is what makes that accumulation well defined, and it is also a
   * useful restriction: it is satisfied by the chain of failures that [[ResultNec]] carries,
   * and not by a bare [[Failure]], so the ill-defined case of asking for two single failures
   * to be accumulated into one cannot be written. A collection of [[FailureOr]] values is
   * sequenced by mapping it through `toNec` first.
   *
   * This carries the responsibility of the collector factory of the original, whose shape -
   * a collector object handed to a stream - belongs to a collection library this port does
   * not use.
   *
   * @tparam E  the type of the failure, which must be combinable
   * @tparam A  the type of the values
   * @param results  the results to sequence
   * @return the values of every result in order, or the combined failures
   */
  def sequence[E: Semigroup, A](results: IterableOnce[Either[E, A]]): Either[E, List[A]] =
    results.iterator
      .foldLeft(Validated.valid[E, List[A]](List.empty[A])) { (accumulated, result) =>
        // `product` is the accumulating combination: a failure on either side survives, and
        // two failures are combined left to right, which keeps them in input order. The
        // values are prepended and reversed once at the end rather than appended n times.
        accumulated.product(Validated.fromEither(result)).map { case (values, value) =>
          value :: values
        }
      }
      .map(_.reverse)
      .toEither

  /**
   * Applies a function to the values of a collection of results, if every one of them is a
   * success.
   *
   * This is `sequence` followed by `map`, and it reproduces the method being ported: the
   * function sees the values of all the results, and if any result was a failure the function
   * is not called and the combined failures are returned instead.
   *
   * {{{
   * combine(amounts)(values => values.sum)  // amounts: List[ResultNec[Double]]
   * }}}
   *
   * The original also caught any exception the function raised and converted it to a failure.
   * Nothing here does that, and nothing needs to: a function that can fail in this port says
   * so by returning a result, which is what `flatCombine` is for.
   *
   * @tparam E  the type of the failure, which must be combinable
   * @tparam A  the type of the values held by the results
   * @tparam B  the type of the combined value
   * @param results  the results to combine
   * @param f  the function applied to the values when every result is a success
   * @return the combined value, or the combined failures
   */
  def combine[E: Semigroup, A, B](results: IterableOnce[Either[E, A]])(f: List[A] => B): Either[E, B] =
    sequence(results).map(f)

  /**
   * Applies a result-returning function to the values of a collection of results, if every
   * one of them is a success.
   *
   * This is `sequence` followed by `flatMap`, and it differs from `combine` only in that the
   * function can itself fail. The failures of the input and the failure of the function are
   * never mixed: the function runs only when every input was a success, so the outcome carries
   * either the input failures or the function's own.
   *
   * {{{
   * flatCombine(amounts)(values => toNec(Decimal.of(values.sum)))
   * }}}
   *
   * The conversion in that example is what the shared failure type asks for: the function has
   * to fail in the same shape as the inputs, and a function that fails in one way reaches that
   * shape through `toNec`.
   *
   * @tparam E  the type of the failure, which must be combinable
   * @tparam A  the type of the values held by the results
   * @tparam B  the type of the combined value
   * @param results  the results to combine
   * @param f  the function applied to the values when every result is a success
   * @return the outcome of the function, or the combined failures of the results
   */
  def flatCombine[E: Semigroup, A, B](
      results: IterableOnce[Either[E, A]])(
      f: List[A] => Either[E, B]): Either[E, B] =
    sequence(results).flatMap(f)

  // ---------------------------------------------------------------------------
  // Partial success.
  // ---------------------------------------------------------------------------

  /**
   * Returns a partial success with the specified failures added to those it already carries,
   * keeping its value.
   *
   * The failures already present come first and the additional ones follow, so the order in
   * which failures were reported is the order in which they are held. What happens to each of
   * the three cases follows from that one rule:
   *
   *   - adding no failures returns the value given, unchanged and identical;
   *   - `Ior.Right` gains a failure side and becomes `Ior.Both`, keeping its value;
   *   - `Ior.Both` keeps its value and concatenates;
   *   - `Ior.Left` concatenates and stays `Ior.Left`, since there is no value to keep.
   *
   * {{{
   * withAdditionalFailures(Ior.right(rows), List(Failure.Parsing("Row 7")))
   * // Ior.Both(NonEmptyChain(Failure.Parsing("Row 7")), rows)
   * }}}
   *
   * @tparam A  the type of the value
   * @param vwf  the partial success to add to
   * @param additionalFailures  the failures to add, possibly none
   * @return the partial success carrying both sets of failures
   */
  def withAdditionalFailures[A](
      vwf: ValueWithFailures[A],
      additionalFailures: IterableOnce[Failure]): ValueWithFailures[A] =
    failureChain(additionalFailures).fold(vwf)(appendFailures(vwf, _))

  /**
   * Collects failures into a chain, which is empty as an `Option` rather than as a chain.
   *
   * Every helper that takes failures as a plain collection starts here: this is the single
   * point at which a collection that may be empty meets a chain that may not.
   */
  private def failureChain(failures: IterableOnce[Failure]): Option[NonEmptyChain[Failure]] =
    NonEmptyChain.fromSeq(failures.iterator.toVector)

  /** Adds failures after those a partial success already carries, keeping any value. */
  private def appendFailures[A](
      vwf: ValueWithFailures[A],
      later: NonEmptyChain[Failure]): ValueWithFailures[A] =
    vwf match {
      case Ior.Left(earlier) => Ior.left(earlier ++ later)
      case Ior.Right(value) => Ior.both(later, value)
      case Ior.Both(earlier, value) => Ior.both(earlier ++ later, value)
    }

  /** Adds failures before those a partial success already carries, keeping any value. */
  private def prependFailures[A](
      earlier: NonEmptyChain[Failure],
      vwf: ValueWithFailures[A]): ValueWithFailures[A] =
    vwf match {
      case Ior.Left(later) => Ior.left(earlier ++ later)
      case Ior.Right(value) => Ior.both(earlier, value)
      case Ior.Both(later, value) => Ior.both(earlier ++ later, value)
    }

  /**
   * Pairs a value with failures that may be absent, which is where partial success is decided:
   * no failures makes a plain success, and any failure makes it partial.
   */
  private def iorOf[A](failures: Chain[Failure], value: A): ValueWithFailures[A] =
    NonEmptyChain
      .fromChain(failures)
      .fold(Ior.right[NonEmptyChain[Failure], A](value))(Ior.both(_, value))

  /**
   * Splits a collection of partial successes into every failure reported and every value
   * available, both in the order they were given in.
   *
   * A value is taken from `Ior.Right` and from `Ior.Both` and failures from `Ior.Left` and
   * from `Ior.Both`, so nothing that either case holds is dropped. Values are accumulated by
   * prepending and reversed once, and failures into a chain, whose concatenation is constant
   * time; the fold is therefore linear in the number of items.
   */
  private def gather[A](items: IterableOnce[ValueWithFailures[A]]): (Chain[Failure], List[A]) = {
    val (failures, reversedValues) =
      items.iterator.foldLeft((Chain.empty[Failure], List.empty[A])) {
        case ((accumulatedFailures, accumulatedValues), item) =>
          (
            item.left.fold(accumulatedFailures)(accumulatedFailures ++ _.toChain),
            item.right.fold(accumulatedValues)(_ :: accumulatedValues))
      }
    (failures, reversedValues.reverse)
  }

  /**
   * Builds and combines partial successes.
   *
   * These are the operations the type being ported provided as statics, kept under the name
   * it used so that a call site reads as it did before. The type itself is the alias declared
   * above - an `Ior` - so everything cats defines on an `Ior` remains available on the values
   * these produce, and nothing cats already provides is repeated here.
   *
   * A term and a type of the same name coexist without ambiguity, as they do for the
   * collection types of the standard library: `ValueWithFailures[A]` in a type position is the
   * alias, and `ValueWithFailures.of(...)` in an expression is this object.
   */
  object ValueWithFailures {

    /**
     * Returns a plain success: a value with nothing to report.
     *
     * @tparam A  the type of the value
     * @param value  the value
     * @return the value with no failures
     */
    def of[A](value: A): ValueWithFailures[A] = Ior.right(value)

    /**
     * Returns a value together with the failures reported while producing it.
     *
     * An empty collection of failures gives a plain success, so a caller can pass whatever it
     * accumulated without first asking whether anything went wrong.
     *
     * {{{
     * ValueWithFailures.of(rows, Nil)                    // Ior.Right(rows)
     * ValueWithFailures.of(rows, List(rowSevenFailure))  // Ior.Both(chain, rows)
     * }}}
     *
     * @tparam A  the type of the value
     * @param value  the value
     * @param failures  the failures reported while producing the value, possibly none
     * @return the value with its failures
     */
    def of[A](value: A, failures: IterableOnce[Failure]): ValueWithFailures[A] =
      failureChain(failures).fold(of(value))(Ior.both(_, value))

    /**
     * Checks whether a partial success reports any failure.
     *
     * This is true of `Ior.Both` and of `Ior.Left`, and false only of `Ior.Right`.
     *
     * @tparam A  the type of the value
     * @param vwf  the partial success to examine
     * @return true if any failure is reported
     */
    def hasFailures[A](vwf: ValueWithFailures[A]): Boolean = !vwf.isRight

    /**
     * Replaces the value, keeping the failures already reported.
     *
     * Where there was no value - `Ior.Left` - there now is one, so the outcome becomes
     * `Ior.Both`; the failures are untouched in every case. This is the inline alternative to
     * mapping over the value when the new value does not depend on the old one.
     *
     * @tparam A  the type of the current value
     * @tparam B  the type of the new value
     * @param vwf  the partial success to take the failures from
     * @param value  the new value
     * @return the new value with the failures of the given partial success
     */
    def withValue[A, B](vwf: ValueWithFailures[A], value: B): ValueWithFailures[B] =
      vwf.putRight(value)

    /**
     * Replaces the value and adds further failures to those already reported.
     *
     * The failures already present come first, as they do for `withAdditionalFailures`.
     *
     * @tparam A  the type of the current value
     * @tparam B  the type of the new value
     * @param vwf  the partial success to take the failures from
     * @param value  the new value
     * @param additionalFailures  the failures to add, possibly none
     * @return the new value with both sets of failures
     */
    def withValue[A, B](
        vwf: ValueWithFailures[A],
        value: B,
        additionalFailures: IterableOnce[Failure]): ValueWithFailures[B] =
      withAdditionalFailures(vwf.putRight(value), additionalFailures)

    /**
     * Takes the value of another partial success, adding its failures after those already
     * reported.
     *
     * This is the inline alternative to chaining: the first outcome contributes its failures
     * and the second contributes its value and its own failures. Where the first reports no
     * failure the second is returned as it stands.
     *
     * Note that this and the two-argument form above overlap when the new value is itself a
     * partial success, and this one is then chosen as the more specific of the two - the same
     * choice the language being ported from made between the corresponding overloads. Calling
     * the other deliberately, to nest one partial success inside another as a value, is done
     * by naming the type parameters.
     *
     * @tparam A  the type of the current value
     * @tparam B  the type of the value of the other partial success
     * @param vwf  the partial success to take the failures from
     * @param other  the partial success to take the value and further failures from
     * @return the value of the other partial success with both sets of failures
     */
    def withValue[A, B](
        vwf: ValueWithFailures[A],
        other: ValueWithFailures[B]): ValueWithFailures[B] =
      vwf.left.fold(other)(prependFailures(_, other))

    /**
     * Combines many partial successes into one holding a list of their values.
     *
     * Every value available is collected, in the order it was given in, and every failure
     * reported is accumulated; the outcome is a plain success when nothing was reported and a
     * partial success otherwise. Items that carry no value contribute their failures alone,
     * which is the one respect in which this goes beyond the method being ported - that method
     * took items which always had a value.
     *
     * {{{
     * combineValuesAsList(List(Ior.right(1), Ior.both(chain, 2), Ior.left(other)))
     * // Ior.Both(chain ++ other, List(1, 2))
     * }}}
     *
     * @tparam A  the type of the values
     * @param items  the partial successes to combine
     * @return one partial success holding the list of values
     */
    def combineValuesAsList[A](items: IterableOnce[ValueWithFailures[A]]): ValueWithFailures[List[A]] = {
      val (failures, values) = gather(items)
      iorOf(failures, values)
    }

    /**
     * Combines many partial successes into one holding a set of their values.
     *
     * This behaves as `combineValuesAsList` does, except that the values are collected into an
     * immutable set: duplicates collapse into one element, and the set has no order, whereas
     * the list keeps the order of the items.
     *
     * @tparam A  the type of the values
     * @param items  the partial successes to combine
     * @return one partial success holding the set of values
     */
    def combineValuesAsSet[A](items: IterableOnce[ValueWithFailures[A]]): ValueWithFailures[Set[A]] = {
      val (failures, values) = gather(items)
      iorOf(failures, values.toSet)
    }

    /**
     * Returns the function that combines two partial successes, combining their values with
     * the specified function and accumulating their failures.
     *
     * This is the reducing function to hand to a fold over many partial successes, which is
     * what the method being ported existed for:
     *
     * {{{
     * items.reduce(ValueWithFailures.combiningValues[List[String]](_ ++ _))
     * items.foldLeft(base)(ValueWithFailures.combiningValues[List[String]](_ ++ _))
     * }}}
     *
     * The combining function is applied only where both sides carry a value. Where one side
     * carries none its failures are kept and the other side's value survives, and where
     * neither does the outcome is the accumulated failures alone - the three-case behaviour
     * the value type of the original could not express, reached here through the combination
     * cats defines for an `Ior`.
     *
     * @tparam A  the type of the values
     * @param combiner  the function combining two values
     * @return the function combining two partial successes
     */
    def combiningValues[A](
        combiner: (A, A) => A): (ValueWithFailures[A], ValueWithFailures[A]) => ValueWithFailures[A] = {
      val values: Semigroup[A] = Semigroup.instance(combiner)
      (first, second) => first.combine(second)(Semigroup[NonEmptyChain[Failure]], values)
    }
  }
}
