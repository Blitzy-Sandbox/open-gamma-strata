/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.Order
import cats.data.Validated

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.result.ValidatedFailures

/**
 * Accumulating checks of the data handed to a validating factory.
 *
 * Every check here asks the same question as the check of the same name in `ArgCheck`, and
 * answers it as a value rather than by interrupting the caller. A check that passes returns
 * the argument it checked in `Valid`; a check that fails returns a [[Failure]] describing
 * what was wrong in `Invalid`. Nothing in this object throws, so the outcome of a check is
 * something a caller receives, inspects and combines:
 *
 * {{{
 * object Rate {
 *   def of(rate: Double, tenor: Int): ResultNec[Rate] =
 *     (Validate.notNaN(rate, "rate"), Validate.notNegativeOrZero(tenor, "tenor"))
 *       .mapN((checkedRate, checkedTenor) => new Rate(checkedRate, checkedTenor) {})
 *       .toEither
 * }
 * }}}
 *
 * That example is the shape every validating factory in this library takes, and it shows the
 * three steps: the checks are written independently, `mapN` combines them into the finished
 * value, and `toEither` hands the outcome back as a [[ResultNec]]. `toResult` performs the
 * last step for an expression that has not already done so.
 *
 * ===Why the outcome accumulates===
 *
 * The checks are combined as `Validated` rather than as `Either`, and the difference is the
 * whole point of this object. Combining two `Either` values stops at the first failure, so a
 * caller that supplied two bad arguments is told about one of them, corrects it, and is then
 * told about the other. Combining two `Validated` values keeps the failures of both, in the
 * order the expression names them, so one pass over the input reports everything wrong with
 * it. That is why every member here returns [[ValidatedFailures]]: the accumulating form is
 * what the checks compose in, and the conversion to a right-biased result happens once, at
 * the end, where the finished value is handed over.
 *
 * Accumulation is not always the right answer, and two situations call for sequencing
 * instead. Where one check is meaningless unless another has already passed, the checks are
 * chained with `andThen`, which runs the second only if the first succeeded - the
 * tolerance-bearing checks below do exactly that. Where the value being checked has to be
 * built by something that can itself fail, `fromResult` lifts that outcome into this form so
 * it can take part in the same expression.
 *
 * ===Which half of the vocabulary to use===
 *
 * This object and `ArgCheck` divide the work by what a failure says about the caller, not by
 * how serious it is. A check belongs here when whether it passes depends on the ''data'' that
 * reached the program: text that may not parse, an identifier that may not be configured,
 * amounts whose currencies may not match, a schedule definition that may not be consistent,
 * a number outside the range the domain allows. None of those means the calling code is
 * wrong, so none of them is an error to abandon the call stack for - the caller is expected
 * to receive the failure, report it or recover from it.
 *
 * A check belongs to `ArgCheck` when it states an invariant that the calling code is
 * required to honour regardless of the data: an index inside an array, a pair of dates
 * supplied in order by code that already sorted them, a state established upstream, or a
 * numeric-domain edge such as arithmetic that has run out of significant digits. Those are
 * programming mistakes, and failing fast at the call that caused one is what makes it
 * findable.
 *
 * The two objects deliberately keep the same member names, parameter order and message
 * wording, so moving a check from one half to the other is a change of object name and of
 * how the outcome is handled, and of nothing else.
 *
 * ===The failure a check reports===
 *
 * Every check here reports `Failure.Invalid`, the reason this library assigns to input that
 * does not satisfy the constraints of the type being built, and gives it the message of the
 * Java check it is ported from, word for word, because that text reaches logs and test
 * expectations. The attributes of the failure are left empty: the name of the argument is
 * already inside the message, and an empty attribute map keeps the serialized form of the
 * failure stable.
 *
 * A caller that needs something else - another reason, or an attribute naming the data that
 * was rejected - builds the failure itself and reports it through `cond`, `invalid` or
 * `invalidNec`, which are the escape hatches from the fixed vocabulary above:
 *
 * {{{
 * Validate.cond(
 *   periods.nonEmpty,
 *   periods,
 *   Failure.Invalid("Schedule must not be empty").withAttribute("definition", definition))
 * }}}
 *
 * ===Deliberate differences from the Java original===
 *
 * The Java class that these checks are ported from threw on failure and returned the checked
 * argument, so that a check could be written inline in a field assignment. Here the argument
 * comes back inside `Valid`, which serves the same purpose in the composed expression: the
 * value that `mapN` hands to the constructor is the value that passed the check.
 *
 * Six shapes differ, each for a stated reason:
 *
 *   - the checks for a missing reference are not ported at all. A value that may be absent
 *     is an `Option` here, so those checks would have no subject; `notPositiveIfPresent`
 *     keeps its name and takes an `Option`;
 *   - the matrix form of `notEmpty` is not ported either. No validating factory in either
 *     module checks a matrix, and leaving it out keeps this object independent of the array
 *     package;
 *   - the four message-bearing `isTrue` forms and the two `isFalse` forms became one member
 *     each, taking the message by name, so the text is built only on the failing path and
 *     the message template of the original is replaced by interpolation that the compiler
 *     checks;
 *   - the separate check for a collection folded into the check for an iterable, because
 *     `Iterable` covers both. A collection reported as empty therefore carries the iterable
 *     wording;
 *   - the checks that compare values take a `cats.Order` for the type being compared, which
 *     keeps the Java comparison interface out of this API and works for any type this
 *     library already orders;
 *   - the two tolerance-bearing checks report an unusable tolerance as a failure rather than
 *     throwing, which is what makes them total, and are sequential for the reason given on
 *     each.
 *
 * ===Thread safety===
 *
 * Every member is a pure function of its arguments and this object holds no state, so it is
 * safe to use from any number of threads.
 *
 * @see [[ArgCheck]] for the fail-fast half of the same vocabulary
 * @see [[Failure]] for the failure a check reports
 */
object Validate {

  //-------------------------------------------------------------------------
  /**
   * Returns the outcome of a check that passed, carrying the checked value.
   *
   * This is the shape every successful check in this object returns, exposed because a
   * factory occasionally has a value that needs no checking and has to take part in a
   * combined expression anyway.
   *
   * @tparam A  the type of the checked value
   * @param value  the value that passed
   * @return the value, as a passing outcome
   */
  def valid[A](value: A): ValidatedFailures[A] = Validated.valid(value)

  /**
   * Returns the outcome of a check that failed, carrying the specified failure.
   *
   * The failure is used exactly as given, which is what makes this the escape hatch from the
   * fixed vocabulary of this object: a caller that needs a reason other than `Invalid`, or
   * attributes naming the data it rejected, builds the failure and reports it here.
   *
   * @tparam A  the type the check would have produced
   * @param failure  the failure describing what was wrong
   * @return the failure, as a failing outcome
   */
  def invalid[A](failure: Failure): ValidatedFailures[A] = Validated.invalidNec(failure)

  /**
   * Returns the outcome of a check that failed, describing it with the specified message.
   *
   * The failure reported is `Failure.Invalid` with no attributes, which is what every check
   * in this object reports, so a caller that only needs its own wording reaches for this
   * rather than building the failure itself.
   *
   * @tparam A  the type the check would have produced
   * @param message  the message describing what was wrong
   * @return the failure, as a failing outcome
   */
  def invalidNec[A](message: String): ValidatedFailures[A] = invalid(Failure.Invalid(message))

  /**
   * Checks the specified condition, producing the specified value or the specified failure.
   *
   * This is the general form of every check in this object, for the conditions that none of
   * them expresses. Both the value and the failure are passed by name, so neither is built
   * unless the branch that needs it is taken:
   *
   * {{{
   * Validate.cond(
   *   amounts.forall(_.currency == currency),
   *   amounts,
   *   Failure.Invalid(s"Amounts must all be in $currency"))
   * }}}
   *
   * @tparam A  the type of the checked value
   * @param test  the condition that has to hold
   * @param value  the value to produce when the condition holds
   * @param failure  the failure to report when it does not
   * @return the value if the condition holds, otherwise the failure
   */
  def cond[A](test: Boolean, value: => A, failure: => Failure): ValidatedFailures[A] =
    if (test) valid(value) else invalid(failure)

  /**
   * Lifts a result that fails for a single reason into the accumulating form.
   *
   * This is how a value produced by something that can itself fail joins a combined
   * expression: the outcome of building it becomes one more check, and its failure
   * accumulates alongside the others rather than short-circuiting them.
   *
   * {{{
   * (Validate.fromResult(Decimal.of(amount)), Validate.notBlank(label, "label"))
   *   .mapN((checkedAmount, checkedLabel) => new Entry(checkedAmount, checkedLabel) {})
   * }}}
   *
   * @tparam A  the type of the value
   * @param outcome  the result to lift
   * @return the same outcome, in the form the checks here combine in
   */
  def fromResult[A](outcome: FailureOr[A]): ValidatedFailures[A] =
    result.toValidated(result.toNec(outcome))

  /**
   * Converts a combined set of checks into the result a factory returns.
   *
   * This is the conversion performed last, so that what a caller receives is the ordinary
   * right-biased type it can `flatMap` over. It is the same conversion that `toEither`
   * performs on the combined expression directly; it exists here so that a factory needs
   * nothing but this object in scope.
   *
   * @tparam A  the type of the checked value
   * @param checks  the combined outcome of the checks
   * @return the result form of the outcome
   */
  def toResult[A](checks: ValidatedFailures[A]): ResultNec[A] = result.toResult(checks)

  /**
   * Produces the checked value or the failure with the message, which is the single point at
   * which every check below reports.
   *
   * Funnelling the outcome through one method keeps the reason and the empty attributes
   * consistent across every check, and keeps the message exactly as the check built it. The
   * message is taken by name so that an interpolated message costs nothing while the check
   * passes.
   *
   * @tparam A  the type of the checked value
   * @param holds  whether the check passed
   * @param argument  the value that was checked
   * @param message  the error message, evaluated only if the check failed
   * @return the argument if the check passed, otherwise the failure describing it
   */
  private def checked[A](holds: Boolean, argument: A, message: => String): ValidatedFailures[A] =
    if (holds) valid(argument) else invalidNec(message)

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified boolean is true.
   *
   * This returns a passing outcome only if the argument is true. This is typically the
   * result of a caller-specific check:
   *
   * {{{
   * Validate.isTrue(values.contains(key))
   * }}}
   *
   * It is strongly recommended to use the two-argument form instead, so that the failure
   * explains itself. There is no value worth returning from a check of a boolean, so the
   * outcome carries `Unit`, which combines with the other checks of an expression exactly as
   * any other value does.
   *
   * @param validIfTrue  a boolean resulting from testing an argument
   * @return a passing outcome if the test value is true, otherwise the failure
   */
  def isTrue(validIfTrue: Boolean): ValidatedFailures[Unit] =
    checked(validIfTrue, (), "Invalid argument, expression must be true")

  /**
   * Checks that the specified boolean is true, reporting the specified message.
   *
   * This returns a passing outcome only if the argument is true:
   *
   * {{{
   * Validate.isTrue(values.contains(key), s"Values must contain the requested key: $values")
   * }}}
   *
   * The message is passed by name, so an interpolated message costs nothing while the check
   * passes: the string is built only on the failing path. This one member replaces the four
   * message-bearing forms of the Java original, whose extra parameters existed only to defer
   * the same string building through a message template.
   *
   * @param validIfTrue  a boolean resulting from testing an argument
   * @param message  the error message, evaluated only if the check fails
   * @return a passing outcome if the test value is true, otherwise the failure
   */
  def isTrue(validIfTrue: Boolean, message: => String): ValidatedFailures[Unit] =
    checked(validIfTrue, (), message)

  /**
   * Checks that the specified boolean is false, reporting the specified message.
   *
   * This returns a passing outcome only if the argument is false:
   *
   * {{{
   * Validate.isFalse(values.contains(key), s"Values must not contain the rejected key: $values")
   * }}}
   *
   * As with `isTrue` the message is passed by name and is built only on the failing path.
   * Matching the Java original, there is no form of this check without a message: a negated
   * condition is worth explaining.
   *
   * @param validIfFalse  a boolean resulting from testing an argument
   * @param message  the error message, evaluated only if the check fails
   * @return a passing outcome if the test value is false, otherwise the failure
   */
  def isFalse(validIfFalse: Boolean, message: => String): ValidatedFailures[Unit] =
    checked(!validIfFalse, (), message)

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified argument matches the specified pattern in full.
   *
   * This returns a passing outcome only if the whole argument matches the regular
   * expression, not merely some part of it. It is the check behind every identifier whose
   * permitted shape is written as a pattern:
   *
   * {{{
   * Validate.matches(SchemeRegex, scheme, "scheme")
   * }}}
   *
   * @param pattern  the pattern to check against
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it matches the pattern, otherwise the failure
   */
  def matches(pattern: Regex, argument: String, name: String): ValidatedFailures[String] =
    checked(pattern.matches(argument), argument, matchesMsg(pattern.toString, name, argument))

  /**
   * Checks that the specified argument has an allowed length and is built only from
   * characters that the specified predicate accepts.
   *
   * This returns a passing outcome only if the length of the argument lies between the two
   * bounds inclusive and every one of its characters satisfies the predicate:
   *
   * {{{
   * Validate.matches(c => c >= 'A' && c <= 'Z', 1, 3, code, "code", "[A-Z]{1,3}")
   * }}}
   *
   * The predicate replaces the character matcher of the Java original, which is why the
   * equivalent regular expression is still passed separately: a function cannot describe
   * itself, so the caller supplies the readable form that the error message quotes. The
   * three ways of failing share one message, as they do in the original, because what the
   * caller has to correct is the same in each case.
   *
   * @param matcher  the predicate that every character has to satisfy
   * @param minLength  the minimum length to allow
   * @param maxLength  the maximum length to allow
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @param equivalentRegex  the equivalent regular expression, used in the error message
   * @return the argument if it is an allowed length and holds only accepted characters,
   *   otherwise the failure
   */
  def matches(
      matcher: Char => Boolean,
      minLength: Int,
      maxLength: Int,
      argument: String,
      name: String,
      equivalentRegex: String): ValidatedFailures[String] = {

    val holds = argument.length >= minLength && argument.length <= maxLength && argument.forall(matcher)
    checked(holds, argument, matchesMsg(equivalentRegex, name, argument))
  }

  // extracted so that the wording exists once for both forms of the check
  private def matchesMsg(pattern: String, name: String, value: String): String =
    s"Argument '$name' with value '$value' must match pattern: $pattern"

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified argument is not blank.
   *
   * This returns a passing outcome only if the argument holds at least one character that is
   * not whitespace. The argument is trimmed to decide that, but the argument itself is left
   * alone: the value returned is the value passed in, so a caller that wants the trimmed
   * text trims it.
   *
   * {{{
   * Validate.notBlank(name, "name")
   * }}}
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not blank, otherwise the failure
   */
  def notBlank(argument: String, name: String): ValidatedFailures[String] =
    checked(argument.trim.nonEmpty, argument, s"Argument '$name' must not be blank")

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified argument is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one character, which
   * may be whitespace. See also `notBlank`.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty(argument: String, name: String): ValidatedFailures[String] =
    checked(argument.nonEmpty, argument, notEmptyMsg(name))

  /**
   * Checks that the specified argument array is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one element. The
   * elements themselves are not examined.
   *
   * @tparam T  the element type of the array
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty[T](argument: Array[T], name: String): ValidatedFailures[Array[T]] =
    checked(argument.length > 0, argument, notEmptyArrayMsg(name))

  /**
   * Checks that the specified argument array is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one element.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty(argument: Array[Int], name: String): ValidatedFailures[Array[Int]] =
    checked(argument.length > 0, argument, notEmptyArrayMsg(name))

  /**
   * Checks that the specified argument array is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one element.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty(argument: Array[Long], name: String): ValidatedFailures[Array[Long]] =
    checked(argument.length > 0, argument, notEmptyArrayMsg(name))

  /**
   * Checks that the specified argument array is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one element.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty(argument: Array[Double], name: String): ValidatedFailures[Array[Double]] =
    checked(argument.length > 0, argument, notEmptyArrayMsg(name))

  /**
   * Checks that the specified argument iterable is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one element. The
   * elements themselves are not examined.
   *
   * This one member covers every ordinary Scala collection, sequences, sets and maps alike,
   * which is why the Java original's separate check for a collection is not ported; a map
   * has a check of its own only because its wording names it.
   *
   * The outcome carries the argument at the type declared here, as it did in the original,
   * so a more precise collection type is not carried through: an expression that has to
   * build its value from a `List` or a `SortedSet` uses the collection it passed in, which
   * is the same object and is already in scope. Keeping the precise type instead would mean
   * a type parameter for the collection itself, and that cannot be done while a map keeps
   * its own wording - the two bounded signatures agree once the types are erased.
   *
   * @tparam T  the element type of the iterable
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty[T](argument: Iterable[T], name: String): ValidatedFailures[Iterable[T]] =
    checked(argument.nonEmpty, argument, s"Argument iterable '$name' must not be empty")

  /**
   * Checks that the specified argument map is not empty.
   *
   * This returns a passing outcome only if the argument holds at least one mapping. The keys
   * and values themselves are not examined.
   *
   * A map satisfies the iterable form as well, and this one is chosen over it because it is
   * the more specific of the two, which is what gives a map its own wording. As with the
   * iterable form the outcome carries the argument at the type declared here, so a sorted
   * map arrives back as a map.
   *
   * @tparam K  the key type of the map
   * @tparam V  the value type of the map
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not empty, otherwise the failure
   */
  def notEmpty[K, V](argument: Map[K, V], name: String): ValidatedFailures[Map[K, V]] =
    checked(argument.nonEmpty, argument, s"Argument map '$name' must not be empty")

  // extracted so that each wording exists once across the overloads above
  private def notEmptyMsg(name: String): String =
    s"Argument '$name' must not be empty"

  private def notEmptyArrayMsg(name: String): String =
    s"Argument array '$name' must not be empty"

  //-----------------------------------------------------------------------
  /**
   * Checks that the specified argument array holds no duplicate values.
   *
   * This returns a passing outcome only if no value occurs twice. The array may be in any
   * order; when it is known to be sorted increasing, `noDuplicatesSorted` answers the same
   * question in constant space.
   *
   * Two values count as the same when their bit patterns agree, which is the comparison that
   * the set of boxed values in the Java original performed. Two consequences follow, and
   * both are intentional: positive and negative zero are ''different'' values here, and a
   * not-a-number value is equal to itself, so an array holding it twice is rejected.
   * `noDuplicatesSorted` compares with arithmetic equality instead and therefore answers the
   * opposite way on both of those inputs, exactly as the Java original does.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it holds no duplicate value, otherwise the failure
   */
  def noDuplicates(argument: Array[Double], name: String): ValidatedFailures[Array[Double]] =
    checked(
      argument.length <= 1 || !containsDuplicate(argument, 0, Set.empty[Long]),
      argument,
      noDuplicatesArrayMsg(name))

  /**
   * Checks that the specified argument array is sorted increasing and holds no duplicate
   * values.
   *
   * This returns a passing outcome only if each value is strictly greater than the one
   * before it. The two ways of failing are reported differently, so that a caller can tell
   * an unsorted array from one that repeats a value, and the first breach found is the one
   * reported: the scan describes what to correct rather than every consequence of it.
   *
   * Values are compared arithmetically, as in the Java original, so positive and negative
   * zero count as a duplicate of each other while a not-a-number value neither equals nor
   * orders against its neighbours and is therefore passed over. `noDuplicates` compares bit
   * patterns and answers the opposite way on both of those inputs.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is sorted and holds no duplicate value, otherwise the failure
   */
  def noDuplicatesSorted(argument: Array[Double], name: String): ValidatedFailures[Array[Double]] =
    sortedFrom(argument, 1, name)

  /**
   * Scans for a repeated bit pattern, carrying the patterns already seen as a parameter so
   * that the scan needs no mutable state.
   *
   * The scan stops at the first repeat, so the work is proportional to the position of the
   * earliest duplicate rather than to the whole array, matching the Java original.
   *
   * @param values  the values being scanned
   * @param index  the index to examine next
   * @param seen  the bit patterns of the values examined so far
   * @return true if a value at or after `index` repeats an earlier value
   */
  @tailrec
  private def containsDuplicate(values: Array[Double], index: Int, seen: Set[Long]): Boolean =
    if (index >= values.length) {
      false
    } else {
      val bits = java.lang.Double.doubleToLongBits(values(index))
      if (seen.contains(bits)) {
        true
      } else {
        containsDuplicate(values, index + 1, seen + bits)
      }
    }

  /**
   * Checks each value against the one before it, reporting the first breach found.
   *
   * The index is threaded as a parameter rather than held in a mutable counter, and the
   * recursive call is the last thing the method does, so the compiler turns it into a jump.
   *
   * @param values  the values being checked
   * @param index  the index to compare against its predecessor, at least one
   * @param name  the name of the argument to use in the error message
   * @return the values if every one of them is greater than the one before, otherwise the
   *   failure describing the first breach
   */
  @tailrec
  private def sortedFrom(
      values: Array[Double],
      index: Int,
      name: String): ValidatedFailures[Array[Double]] =
    if (index >= values.length) {
      valid(values)
    } else {
      val previous = values(index - 1)
      val current = values(index)
      if (current == previous) {
        invalidNec(noDuplicatesArrayMsg(name))
      } else if (current < previous) {
        invalidNec(s"Argument array '$name' must be sorted and not contain duplicates")
      } else {
        sortedFrom(values, index + 1, name)
      }
    }

  // extracted so that the wording exists once for both duplicate checks
  private def noDuplicatesArrayMsg(name: String): String =
    s"Argument array '$name' must not contain duplicates"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is not positive.
   *
   * This returns a passing outcome only if the argument is zero or less.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not positive, otherwise the failure
   */
  def notPositive(argument: Int, name: String): ValidatedFailures[Int] =
    checked(argument <= 0, argument, notPositiveMsg(name, argument))

  /**
   * Checks that the argument is not positive.
   *
   * This returns a passing outcome only if the argument is zero or less.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not positive, otherwise the failure
   */
  def notPositive(argument: Long, name: String): ValidatedFailures[Long] =
    checked(argument <= 0L, argument, notPositiveMsg(name, argument))

  /**
   * Checks that the argument is not positive.
   *
   * This returns a passing outcome only if the argument is zero or less. A not-a-number
   * argument is neither positive nor negative, so it passes this check; combine it with
   * `notNaN` in the same expression where a real number is what the type allows.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not positive, otherwise the failure
   */
  def notPositive(argument: Double, name: String): ValidatedFailures[Double] =
    checked(!(argument > 0.0), argument, notPositiveMsg(name, argument))

  /**
   * Checks that the argument is not positive.
   *
   * This returns a passing outcome only if the argument is zero or less. The sign is read
   * from the decimal itself, so the scale of the value does not affect the outcome.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not positive, otherwise the failure
   */
  def notPositive(argument: Decimal, name: String): ValidatedFailures[Decimal] =
    checked(argument.signum <= 0, argument, notPositiveMsg(name, argument))

  /**
   * Checks that the argument is not positive when a value is present.
   *
   * This returns a passing outcome if no value is present, and otherwise applies the same
   * check as `notPositive`. The Java original took a reference that was allowed to be
   * absent; an `Option` states that intent in the type, and the outcome carries the argument
   * as given so that an absent value stays absent.
   *
   * @param argument  the argument to check, absent always passes
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is absent or not positive, otherwise the failure
   */
  def notPositiveIfPresent(argument: Option[Decimal], name: String): ValidatedFailures[Option[Decimal]] =
    argument match {
      case Some(value) => notPositive(value, name).map(Some(_))
      case None => valid(argument)
    }

  private def notPositiveMsg(name: String, value: Any): String =
    s"Argument '$name' must not be positive but has value $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is not negative.
   *
   * This returns a passing outcome only if the argument is zero or greater.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not negative, otherwise the failure
   */
  def notNegative(argument: Int, name: String): ValidatedFailures[Int] =
    checked(argument >= 0, argument, notNegativeMsg(name, argument))

  /**
   * Checks that the argument is not negative.
   *
   * This returns a passing outcome only if the argument is zero or greater.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not negative, otherwise the failure
   */
  def notNegative(argument: Long, name: String): ValidatedFailures[Long] =
    checked(argument >= 0L, argument, notNegativeMsg(name, argument))

  /**
   * Checks that the argument is not negative.
   *
   * This returns a passing outcome only if the argument is zero or greater. Negative zero
   * compares equal to zero, so it passes, and a not-a-number argument passes as well because
   * it does not order against zero; combine it with `notNaN` in the same expression where a
   * real number is what the type allows.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not negative, otherwise the failure
   */
  def notNegative(argument: Double, name: String): ValidatedFailures[Double] =
    checked(!(argument < 0.0), argument, notNegativeMsg(name, argument))

  /**
   * Checks that the argument is not negative.
   *
   * This returns a passing outcome only if the argument is zero or greater. The sign is read
   * from the decimal itself, so the scale of the value does not affect the outcome.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not negative, otherwise the failure
   */
  def notNegative(argument: Decimal, name: String): ValidatedFailures[Decimal] =
    checked(argument.signum >= 0, argument, notNegativeMsg(name, argument))

  private def notNegativeMsg(name: String, value: Any): String =
    s"Argument '$name' must not be negative but has value $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is a number.
   *
   * This returns a passing outcome only if the argument is an actual number. The infinities
   * are numbers for this purpose and pass; only a not-a-number value fails, which is the
   * value that arithmetic produces when it has no meaningful answer.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is a number, otherwise the failure
   */
  def notNaN(argument: Double, name: String): ValidatedFailures[Double] =
    checked(!argument.isNaN, argument, s"Argument '$name' must not be NaN")

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is neither negative nor zero.
   *
   * This returns a passing outcome only if the argument is greater than zero.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is greater than zero, otherwise the failure
   */
  def notNegativeOrZero(argument: Int, name: String): ValidatedFailures[Int] =
    checked(argument > 0, argument, notNegativeOrZeroMsg(name, argument))

  /**
   * Checks that the argument is neither negative nor zero.
   *
   * This returns a passing outcome only if the argument is greater than zero.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is greater than zero, otherwise the failure
   */
  def notNegativeOrZero(argument: Long, name: String): ValidatedFailures[Long] =
    checked(argument > 0L, argument, notNegativeOrZeroMsg(name, argument))

  /**
   * Checks that the argument is neither negative nor zero.
   *
   * This returns a passing outcome only if the argument is greater than zero. Both signed
   * zeros fail, since they compare equal to zero, and a not-a-number argument passes because
   * it does not order against zero; combine it with `notNaN` in the same expression where a
   * real number is what the type allows.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is greater than zero, otherwise the failure
   */
  def notNegativeOrZero(argument: Double, name: String): ValidatedFailures[Double] =
    checked(!(argument <= 0.0), argument, notNegativeOrZeroMsg(name, argument))

  /**
   * Checks that the argument is greater than zero, treating anything within the specified
   * tolerance of zero as zero.
   *
   * This returns a passing outcome only if the argument lies further than the tolerance
   * above zero. The two ways of failing are reported differently: a value close to zero is
   * reported as zero, and a value clearly below zero is reported as too small.
   *
   * {{{
   * Validate.notNegativeOrZero(amount, 0.0001, "amount")
   * }}}
   *
   * The tolerance is itself checked first, because neither a not-a-number tolerance nor a
   * negative one describes an interval, and the comparison against either would mean nothing;
   * the numeric helper that the Java original called rejected both the same way. The checks
   * are therefore chained rather than accumulated: an unusable tolerance is reported on its
   * own, as a single failure, since the outcome of the check it governs would be meaningless.
   * The two tolerance checks are chained with each other for the same reason, being a number
   * first, so a not-a-number tolerance is reported as not being a number and a negative one
   * as being negative, never as both. A zero tolerance is usable and is accepted, and so is
   * negative zero, which is not a negative value. A not-a-number argument is not near zero
   * and is not below zero, so it passes both tests here, as it did there.
   *
   * @param argument  the argument to check
   * @param tolerance  the tolerance to use for zero, which has to be a number and has to not
   *   be negative
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies above the tolerance, otherwise the single failure
   *   describing the tolerance - not a number, or negative - or the failure describing the
   *   argument
   */
  def notNegativeOrZero(argument: Double, tolerance: Double, name: String): ValidatedFailures[Double] =
    notNaN(tolerance, "tolerance")
      .andThen(checkedNumber => notNegative(checkedNumber, "tolerance"))
      .andThen { checkedTolerance =>
        if (isNearZero(argument, checkedTolerance)) {
          invalidNec(notZeroMsg(name))
        } else if (argument < 0.0) {
          invalidNec(s"Argument '$name' must be greater than zero but has value $argument")
        } else {
          valid(argument)
        }
      }

  /**
   * Checks that the argument is neither negative nor zero.
   *
   * This returns a passing outcome only if the argument is greater than zero. The sign is
   * read from the decimal itself, so the scale of the value does not affect the outcome.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is greater than zero, otherwise the failure
   */
  def notNegativeOrZero(argument: Decimal, name: String): ValidatedFailures[Decimal] =
    checked(argument.signum > 0, argument, notNegativeOrZeroMsg(name, argument))

  private def notNegativeOrZeroMsg(name: String, value: Any): String =
    s"Argument '$name' must not be negative or zero but has value $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is not zero.
   *
   * This returns a passing outcome only if the argument differs from zero. One comparison
   * covers both signed zeros, because they compare equal to each other.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @return the argument if it is not zero, otherwise the failure
   */
  def notZero(argument: Double, name: String): ValidatedFailures[Double] =
    checked(argument != 0.0, argument, notZeroMsg(name))

  /**
   * Checks that the argument is not zero, treating anything within the specified tolerance
   * of zero as zero.
   *
   * This returns a passing outcome only if the argument lies further than the tolerance from
   * zero, in either direction.
   *
   * {{{
   * Validate.notZero(amount, 0.0001, "amount")
   * }}}
   *
   * As in the check above, the tolerance is examined first and the checks are chained rather
   * than accumulated, because neither a not-a-number tolerance nor a negative one describes
   * an interval; being a number is checked before being non-negative, so an unusable
   * tolerance is reported as what it is and always as a single failure. A zero tolerance, of
   * either sign, is usable and is accepted. A not-a-number argument is not near zero, so it
   * passes.
   *
   * @param argument  the argument to check
   * @param tolerance  the tolerance to use for zero, which has to be a number and has to not
   *   be negative
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies further than the tolerance from zero, otherwise the
   *   single failure describing the tolerance - not a number, or negative - or the failure
   *   describing the argument
   */
  def notZero(argument: Double, tolerance: Double, name: String): ValidatedFailures[Double] =
    notNaN(tolerance, "tolerance")
      .andThen(checkedNumber => notNegative(checkedNumber, "tolerance"))
      .andThen { checkedTolerance =>
        checked(!isNearZero(argument, checkedTolerance), argument, notZeroMsg(name))
      }

  private def notZeroMsg(name: String): String =
    s"Argument '$name' must not be zero"

  /**
   * Decides whether a value counts as zero at the specified tolerance.
   *
   * This reproduces, for the single comparison that the two checks above need, the scalar
   * fuzzy-comparison semantics of the numeric helper that the Java original called: a value
   * is near zero when its magnitude does not exceed the tolerance, or when it equals zero
   * exactly. The second clause is what makes the comparison total, and the two clauses
   * together give the behaviour at the edges of the domain. A not-a-number argument is never
   * near zero: its magnitude does not compare against the tolerance and it does not equal
   * zero, so both checks above admit it, exactly as the Java checks did. An infinity is near
   * zero only when the tolerance is itself infinite, since that is the only tolerance an
   * infinite magnitude does not exceed; at every finite tolerance it is clear of zero and
   * passes. The callers establish, before reaching here, that the tolerance is a number and
   * is not negative.
   *
   * The array-oriented forms of the comparison live with the rest of the array arithmetic, in
   * `DoubleArrayMath`, and a reader comparing the two should expect them to differ: that
   * object states the migration plan's reading of a fuzzy comparison, under which a
   * not-a-number value is equal to nothing at all and each infinity is equal only to itself
   * at any tolerance, including an infinite one. This local zero test deliberately keeps the
   * Java behaviour instead, because the Java check is the authority for these two checks and
   * their messages. They are also deliberately not called from here, so that this object
   * depends on nothing but the failure model it reports through.
   *
   * @param argument  the value to test
   * @param tolerance  the tolerance to use for zero, already checked to be a number that is
   *   not negative
   * @return true if the value counts as zero
   */
  private def isNearZero(argument: Double, tolerance: Double): Boolean =
    math.abs(argument) <= tolerance || argument == 0.0

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is in the range defined by `low <= x < high`.
   *
   * This returns a passing outcome only if the argument is at or above the lower bound and
   * strictly below the upper bound.
   *
   * {{{
   * Validate.inRange(fraction, 0.0, 1.0, "fraction")
   * }}}
   *
   * The condition is the exact negation of the one the Java original rejects on, which is
   * what fixes the behaviour at the edges of the domain: a not-a-number argument orders
   * against neither bound, so it is not outside the range and passes, as it did there.
   * Combine the check with `notNaN` in the same expression where a real number is what the
   * type allows. The same holds for the two checks below.
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRange(
      argument: Double,
      lowInclusive: Double,
      highExclusive: Double,
      name: String): ValidatedFailures[Double] =
    checked(
      !(argument < lowInclusive) && !(argument >= highExclusive),
      argument,
      inRangeMsg(lowInclusive, name, highExclusive, argument))

  /**
   * Checks that the argument is in the range defined by `low <= x <= high`.
   *
   * This returns a passing outcome only if the argument lies within the bounds, both of
   * which are allowed.
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highInclusive  the high value of the range, allowed
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeInclusive(
      argument: Double,
      lowInclusive: Double,
      highInclusive: Double,
      name: String): ValidatedFailures[Double] =
    checked(
      !(argument < lowInclusive) && !(argument > highInclusive),
      argument,
      inRangeInclusiveMsg(lowInclusive, name, highInclusive, argument))

  /**
   * Checks that the argument is in the range defined by `low < x < high`.
   *
   * This returns a passing outcome only if the argument lies strictly within the bounds,
   * neither of which is allowed.
   *
   * @param argument  the argument to check
   * @param lowExclusive  the low value of the range, not allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeExclusive(
      argument: Double,
      lowExclusive: Double,
      highExclusive: Double,
      name: String): ValidatedFailures[Double] =
    checked(
      !(argument <= lowExclusive) && !(argument >= highExclusive),
      argument,
      inRangeExclusiveMsg(lowExclusive, name, highExclusive, argument))

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is in the range defined by `low <= x < high`.
   *
   * This returns a passing outcome only if the argument is at or above the lower bound and
   * strictly below the upper bound. Where the range is the extent of something being indexed
   * the check belongs to `ArgCheck` instead: an index out of bounds is a mistake in the
   * calling code rather than a property of the data, which is the distinction the two
   * objects are divided by.
   *
   * {{{
   * Validate.inRange(decimalPlaces, 0, 10, "decimalPlaces")
   * }}}
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRange(argument: Int, lowInclusive: Int, highExclusive: Int, name: String): ValidatedFailures[Int] =
    checked(
      argument >= lowInclusive && argument < highExclusive,
      argument,
      inRangeMsg(lowInclusive, name, highExclusive, argument))

  /**
   * Checks that the argument is in the range defined by `low <= x <= high`.
   *
   * This returns a passing outcome only if the argument lies within the bounds, both of
   * which are allowed.
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highInclusive  the high value of the range, allowed
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeInclusive(
      argument: Int,
      lowInclusive: Int,
      highInclusive: Int,
      name: String): ValidatedFailures[Int] =
    checked(
      argument >= lowInclusive && argument <= highInclusive,
      argument,
      inRangeInclusiveMsg(lowInclusive, name, highInclusive, argument))

  /**
   * Checks that the argument is in the range defined by `low < x < high`.
   *
   * This returns a passing outcome only if the argument lies strictly within the bounds,
   * neither of which is allowed.
   *
   * @param argument  the argument to check
   * @param lowExclusive  the low value of the range, not allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeExclusive(
      argument: Int,
      lowExclusive: Int,
      highExclusive: Int,
      name: String): ValidatedFailures[Int] =
    checked(
      argument > lowExclusive && argument < highExclusive,
      argument,
      inRangeExclusiveMsg(lowExclusive, name, highExclusive, argument))

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is in the range defined by `low <= x < high`, ordering the
   * values with the ordering of their type.
   *
   * This returns a passing outcome only if the argument is at or above the lower bound and
   * strictly below the upper bound:
   *
   * {{{
   * Validate.inRangeComparable(tenor, Tenor.TENOR_1M, Tenor.TENOR_1Y, "tenor")
   * }}}
   *
   * @tparam T  the type of the values being compared
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @param order  the ordering of the type being compared
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeComparable[T](argument: T, lowInclusive: T, highExclusive: T, name: String)(
      implicit order: Order[T]): ValidatedFailures[T] =
    checked(
      order.gteqv(argument, lowInclusive) && order.lt(argument, highExclusive),
      argument,
      inRangeMsg(lowInclusive, name, highExclusive, argument))

  /**
   * Checks that the argument is in the range defined by `low <= x <= high`, ordering the
   * values with the ordering of their type.
   *
   * This returns a passing outcome only if the argument lies within the bounds, both of
   * which are allowed.
   *
   * @tparam T  the type of the values being compared
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highInclusive  the high value of the range, allowed
   * @param name  the name of the argument to use in the error message
   * @param order  the ordering of the type being compared
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeComparableInclusive[T](argument: T, lowInclusive: T, highInclusive: T, name: String)(
      implicit order: Order[T]): ValidatedFailures[T] =
    checked(
      order.gteqv(argument, lowInclusive) && order.lteqv(argument, highInclusive),
      argument,
      inRangeInclusiveMsg(lowInclusive, name, highInclusive, argument))

  /**
   * Checks that the argument is in the range defined by `low < x < high`, ordering the
   * values with the ordering of their type.
   *
   * This returns a passing outcome only if the argument lies strictly within the bounds,
   * neither of which is allowed.
   *
   * @tparam T  the type of the values being compared
   * @param argument  the argument to check
   * @param lowExclusive  the low value of the range, not allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @param order  the ordering of the type being compared
   * @return the argument if it lies in the range, otherwise the failure
   */
  def inRangeComparableExclusive[T](argument: T, lowExclusive: T, highExclusive: T, name: String)(
      implicit order: Order[T]): ValidatedFailures[T] =
    checked(
      order.gt(argument, lowExclusive) && order.lt(argument, highExclusive),
      argument,
      inRangeExclusiveMsg(lowExclusive, name, highExclusive, argument))

  // extracted so that each range wording exists once across the nine range checks
  private def inRangeMsg(low: Any, name: String, high: Any, value: Any): String =
    s"Expected $low <= '$name' < $high, but found $value"

  private def inRangeInclusiveMsg(low: Any, name: String, high: Any, value: Any): String =
    s"Expected $low <= '$name' <= $high, but found $value"

  private def inRangeExclusiveMsg(low: Any, name: String, high: Any, value: Any): String =
    s"Expected $low < '$name' < $high, but found $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that two values are in order and not equal.
   *
   * This returns a passing outcome only if the first value orders strictly before the
   * second. Equal values fail, which is what distinguishes this check from `inOrderOrEqual`.
   * The outcome carries both values as a pair, so a combined expression can name them once
   * and use what it checked:
   *
   * {{{
   * Validate.inOrderNotEqual(startDate, endDate, "startDate", "endDate")
   * }}}
   *
   * Where the two values were produced by the code doing the checking rather than supplied
   * to it, the equivalent check in `ArgCheck` is the one to use: their order is then an
   * invariant of that code and not a property of the data.
   *
   * @tparam T  the type of the values being compared
   * @param obj1  the first value, expected to order before the second
   * @param obj2  the second value
   * @param name1  the name of the first argument to use in the error message
   * @param name2  the name of the second argument to use in the error message
   * @param order  the ordering of the type being compared
   * @return both values as a pair if they are in order, otherwise the failure
   */
  def inOrderNotEqual[T](obj1: T, obj2: T, name1: String, name2: String)(
      implicit order: Order[T]): ValidatedFailures[(T, T)] =
    checked(
      order.lt(obj1, obj2),
      (obj1, obj2),
      s"Invalid order: Expected '$name1' < '$name2', but found: '$obj1' >= '$obj2'")

  /**
   * Checks that two values are in order or equal.
   *
   * This returns a passing outcome only if the first value orders before the second or
   * equals it. This is the check for a bound that is allowed to be degenerate, such as a
   * period whose two ends may coincide, and as above the outcome carries both values:
   *
   * {{{
   * Validate.inOrderOrEqual(firstDate, secondDate, "firstDate", "secondDate")
   * }}}
   *
   * @tparam T  the type of the values being compared
   * @param obj1  the first value, expected to order before the second or equal it
   * @param obj2  the second value
   * @param name1  the name of the first argument to use in the error message
   * @param name2  the name of the second argument to use in the error message
   * @param order  the ordering of the type being compared
   * @return both values as a pair if they are in order, otherwise the failure
   */
  def inOrderOrEqual[T](obj1: T, obj2: T, name1: String, name2: String)(
      implicit order: Order[T]): ValidatedFailures[(T, T)] =
    checked(
      order.lteqv(obj1, obj2),
      (obj1, obj2),
      s"Invalid order: Expected '$name1' <= '$name2', but found: '$obj1' > '$obj2'")
}
