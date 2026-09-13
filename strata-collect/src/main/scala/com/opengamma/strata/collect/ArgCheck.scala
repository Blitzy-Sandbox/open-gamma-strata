/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.Order

import com.opengamma.strata.collect.array.Matrix

/**
 * Fail-fast checks of the arguments passed to a method.
 *
 * Every check in this object states an invariant that the caller is required to honour. A
 * check returns quietly when the invariant holds and throws `IllegalArgumentException`
 * when it does not, so a violation surfaces at the call that caused it rather than as a
 * wrong answer computed much later:
 *
 * {{{
 * def scaled(values: Array[Double], factor: Double): Array[Double] = {
 *   ArgCheck.notEmpty(values, "values")
 *   ArgCheck.notNaN(factor, "factor")
 *   values.map(_ * factor)
 * }
 * }}}
 *
 * ===Fail fast here, accumulate elsewhere===
 *
 * This object is one half of the validation vocabulary of this library, and the half to
 * reach for the less often. It covers exactly two kinds of condition:
 *
 *   - a caller-contract invariant, such as an index that has to lie inside an array, a
 *     pair of dates that has to be supplied in order, or a state that the surrounding code
 *     establishes upstream and can therefore treat as unreachable;
 *   - a numeric-domain edge, such as a value that has to be a real number rather than a
 *     not-a-number result, or an arithmetic result that has run out of significant digits.
 *
 * Neither depends on the ''data'' that a user supplied: both say that the calling code is
 * wrong, which is why a thrown error is the right outcome and why no caller is expected to
 * recover from one. Everything else - text that may not parse, an identifier that may not
 * be configured, amounts whose currencies may not match, a schedule definition that may
 * not be consistent - is a data-dependent failure. Those belong to `Validate` and to the
 * validated factories built on it, which hand the failure back as a value so that a caller
 * can inspect it, combine it with others and report it.
 *
 * Because of that split this object is the single place in either module where a throw is
 * written. Any other file that has to enforce an invariant calls a check here.
 *
 * One boundary within the first kind is worth stating, because the numeric array types of
 * this module sit on both sides of it. A violation of ''shape'' - a negative size or
 * dimension asked of a factory, two arrays that have to match in length and do not, a
 * sub-array boundary beyond the end of an array, a row whose length does not match a column
 * count the caller stated, or the smallest element of an array that has none - is checked
 * here, before anything is allocated or read, and is therefore reported as this object's
 * exception. A violation of ''index'' - a single element addressed outside an array - is left
 * to the array access itself, which raises the index exception of the runtime: a check in
 * front of it would change the exception a caller sees for no gain. `DoubleArray` and
 * `DoubleMatrix` record which of their members falls on which side.
 *
 * ===Shape of these checks===
 *
 * Every check returns `Unit`. The build treats a discarded value as an error, so a check
 * that answered the argument it was handed would force every call into a binding that
 * exists only to be thrown away, whereas returning `Unit` lets a check read as the
 * statement that it is. Nothing is lost: the argument is already in scope at the call
 * site, and validated construction goes through a factory that returns the finished value.
 *
 * A value that may be absent is modelled as `Option`, so absence is described by the type
 * and handled by matching on it, and there is therefore no check here for a missing
 * reference. `notPositiveIfPresent` takes an `Option` for that reason.
 *
 * Three further points of shape are worth stating:
 *
 *   - one message-bearing `isTrue` serves every condition, and it takes the message by
 *     name, so the text is built only when the check fails. The message is written as
 *     ordinary interpolation at the call site, which the compiler checks;
 *   - `isFalse` exists only in the message-bearing form: a negated condition is worth
 *     explaining;
 *   - emptiness of a collection is checked through `Iterable`, which covers every
 *     collection an argument may be, so a collection reported as empty carries the
 *     iterable wording.
 *
 * Message text is stable, because it reaches logs and test expectations. That stability is
 * also why a check throws the exception directly instead of delegating to the standard
 * `require`, which prefixes every message it is handed with wording of its own.
 *
 * ===Ordering-based checks===
 *
 * The checks that compare values - `inRangeComparable`, `inOrderNotEqual` and their
 * siblings - take a `cats.Order` for the type being compared, which keeps the runtime's
 * comparison interfaces out of this API and lets the check work for any type that the
 * library already orders. A type that declares its natural comparison against a supertype
 * has no such instance in scope by default; `Order.fromOrdering` derives one from the
 * standard library ordering, which those types do have. Where deriving an instance is not
 * worth it, `isTrue` with an explicit comparison expresses the same invariant.
 *
 * ===Thread safety===
 *
 * Every member is a pure function of its arguments and this object holds no state, so it
 * is safe to use from any number of threads.
 */
object ArgCheck {

  //-------------------------------------------------------------------------
  /**
   * Throws the failure that every check in this object reports.
   *
   * Funnelling the throw through one method keeps the exception type consistent across
   * every check and keeps the message exactly as the caller built it. The parameter is
   * by-value because every caller reaches this method only from inside a branch that has
   * already established that the check failed, so the message is never built for an
   * argument that passes.
   *
   * @param message  the complete error message
   * @return never returns normally
   * @throws java.lang.IllegalArgumentException always
   */
  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(message)

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified boolean is true.
   *
   * Given the input argument, this returns normally only if it is true. This is typically
   * the result of a caller-specific check:
   *
   * {{{
   * ArgCheck.isTrue(values.contains(key))
   * }}}
   *
   * It is strongly recommended to use the two-argument form
   * instead, so that the failure explains itself.
   *
   * @param validIfTrue  a boolean resulting from testing an argument
   * @throws java.lang.IllegalArgumentException if the test value is false
   */
  def isTrue(validIfTrue: Boolean): Unit =
    if (!validIfTrue) {
      fail("Invalid argument, expression must be true")
    }

  /**
   * Checks that the specified boolean is true, reporting the specified message.
   *
   * Given the input argument, this returns normally only if it is true:
   *
   * {{{
   * ArgCheck.isTrue(values.contains(key), "Values must contain the requested key: " + values)
   * }}}
   *
   * The message is passed by name, so an interpolated message costs nothing while the
   * check passes: the string is built only on the failing path. This one member replaces
   * the four message-bearing forms of the Java original, whose extra parameters existed
   * only to defer the same string building through a message template.
   *
   * @param validIfTrue  a boolean resulting from testing an argument
   * @param message  the error message, evaluated only if the check fails
   * @throws java.lang.IllegalArgumentException if the test value is false
   */
  def isTrue(validIfTrue: Boolean, message: => String): Unit =
    if (!validIfTrue) {
      fail(message)
    }

  /**
   * Checks that the specified boolean is false, reporting the specified message.
   *
   * Given the input argument, this returns normally only if it is false:
   *
   * {{{
   * ArgCheck.isFalse(values.contains(key), "Values must not contain the rejected key: " + values)
   * }}}
   *
   * As with `isTrue` the message is passed by name and is built only on
   * the failing path. Matching the Java original, there is no form of this check without a
   * message: a negated condition is worth explaining.
   *
   * @param validIfFalse  a boolean resulting from testing an argument
   * @param message  the error message, evaluated only if the check fails
   * @throws java.lang.IllegalArgumentException if the test value is true
   */
  def isFalse(validIfFalse: Boolean, message: => String): Unit =
    if (validIfFalse) {
      fail(message)
    }

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified argument matches the specified pattern in full.
   *
   * Given the input argument, this returns normally only if the whole argument matches the
   * regular expression, not merely some part of it:
   *
   * {{{
   * ArgCheck.matches(SchemeRegex, scheme, "scheme")
   * }}}
   *
   * @param pattern  the pattern to check against
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument does not match the pattern
   */
  def matches(pattern: Regex, argument: String, name: String): Unit =
    if (!pattern.matches(argument)) {
      fail(matchesMsg(pattern.toString, name, argument))
    }

  /**
   * Checks that the specified argument has an allowed length and is built only from
   * characters that the specified predicate accepts.
   *
   * Given the input argument, this returns normally only if its length lies between the two
   * bounds inclusive and every one of its characters satisfies the predicate:
   *
   * {{{
   * ArgCheck.matches(c => c >= 'A' && c <= 'Z', 1, 3, code, "code", "[A-Z]{1,3}")
   * }}}
   *
   * The predicate replaces the character matcher of the Java original, which is why the
   * equivalent regular expression is still passed separately: a function cannot describe
   * itself, so the caller supplies the readable form that the error message quotes.
   *
   * @param matcher  the predicate that every character must satisfy
   * @param minLength  the minimum length to allow
   * @param maxLength  the maximum length to allow
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @param equivalentRegex  the equivalent regular expression, used in the error message
   * @throws java.lang.IllegalArgumentException if the argument is the wrong length or contains a
   *   character that the predicate rejects
   */
  def matches(
      matcher: Char => Boolean,
      minLength: Int,
      maxLength: Int,
      argument: String,
      name: String,
      equivalentRegex: String): Unit = {

    if (argument.length < minLength || argument.length > maxLength || !argument.forall(matcher)) {
      fail(matchesMsg(equivalentRegex, name, argument))
    }
  }

  // extracted so that the wording exists once for both forms of the check
  private def matchesMsg(pattern: String, name: String, value: String): String =
    s"Argument '$name' with value '$value' must match pattern: $pattern"

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified argument is not blank.
   *
   * Given the input argument, this returns normally only if it holds at least one character
   * that is not whitespace. The argument is trimmed to decide that, but the argument itself
   * is left alone: a caller that wants the trimmed text trims it.
   *
   * {{{
   * ArgCheck.notBlank(name, "name")
   * }}}
   *
   * @param argument  the argument to check, blank throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty or entirely whitespace
   */
  def notBlank(argument: String, name: String): Unit =
    if (argument.trim.isEmpty) {
      fail(s"Argument '$name' must not be blank")
    }

  //-------------------------------------------------------------------------
  /**
   * Checks that the specified argument is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one
   * character, which may be whitespace. See also `notBlank`.
   *
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty(argument: String, name: String): Unit =
    if (argument.isEmpty) {
      fail(notEmptyMsg(name))
    }

  /**
   * Checks that the specified argument array is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one element.
   * The elements themselves are not examined.
   *
   * @tparam T  the element type of the array
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty[T](argument: Array[T], name: String): Unit =
    if (argument.length == 0) {
      fail(notEmptyArrayMsg(name))
    }

  /**
   * Checks that the specified matrix is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one element,
   * counted across all of its dimensions. It applies to every matrix type, one-dimensional
   * arrays included.
   *
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument holds no elements
   */
  def notEmpty(argument: Matrix, name: String): Unit =
    if (argument.size == 0) {
      fail(notEmptyArrayMsg(name))
    }

  /**
   * Checks that the specified argument array is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one element.
   *
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty(argument: Array[Int], name: String): Unit =
    if (argument.length == 0) {
      fail(notEmptyArrayMsg(name))
    }

  /**
   * Checks that the specified argument array is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one element.
   *
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty(argument: Array[Long], name: String): Unit =
    if (argument.length == 0) {
      fail(notEmptyArrayMsg(name))
    }

  /**
   * Checks that the specified argument array is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one element.
   *
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty(argument: Array[Double], name: String): Unit =
    if (argument.length == 0) {
      fail(notEmptyArrayMsg(name))
    }

  /**
   * Checks that the specified argument iterable is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one element.
   * The elements themselves are not examined.
   *
   * This one member covers every ordinary Scala collection, sequences, sets and maps
   * alike, which is why the Java original's separate check for a collection is not ported;
   * a map has a check of its own only because its wording names it.
   *
   * @tparam T  the element type of the iterable
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty[T](argument: Iterable[T], name: String): Unit =
    if (argument.isEmpty) {
      fail(s"Argument iterable '$name' must not be empty")
    }

  /**
   * Checks that the specified argument map is not empty.
   *
   * Given the input argument, this returns normally only if it holds at least one mapping.
   * The keys and values themselves are not examined.
   *
   * @tparam K  the key type of the map
   * @tparam V  the value type of the map
   * @param argument  the argument to check, empty throws an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is empty
   */
  def notEmpty[K, V](argument: Map[K, V], name: String): Unit =
    if (argument.isEmpty) {
      fail(s"Argument map '$name' must not be empty")
    }

  // extracted so that each wording exists once across the overloads above
  private def notEmptyMsg(name: String): String =
    s"Argument '$name' must not be empty"

  private def notEmptyArrayMsg(name: String): String =
    s"Argument array '$name' must not be empty"

  //-----------------------------------------------------------------------
  /**
   * Checks that the specified argument array holds no duplicate values.
   *
   * Given the input argument, this returns normally only if no value occurs twice. The
   * array may be in any order; when it is known to be sorted increasing,
   * `noDuplicatesSorted` answers the same question in constant space.
   *
   * Two values count as the same when their bit patterns agree, which is the comparison
   * that the Java original's set of boxed values performed. Two consequences follow, and
   * both are intentional: positive and negative zero are ''different'' values here, and a
   * not-a-number value is equal to itself, so an array holding it twice is rejected.
   * `noDuplicatesSorted` compares with arithmetic equality instead and therefore answers
   * the opposite way on both of those inputs, exactly as the Java original does.
   *
   * @param argument  the argument to check, duplicate values throw an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument holds a duplicate value
   */
  def noDuplicates(argument: Array[Double], name: String): Unit =
    if (argument.length > 1 && containsDuplicate(argument, 0, Set.empty)) {
      fail(noDuplicatesArrayMsg(name))
    }

  /**
   * Checks that the specified argument array is sorted increasing and holds no duplicate
   * values.
   *
   * Given the input argument, this returns normally only if each value is strictly greater
   * than the one before it. The two ways of failing are reported differently, so that a
   * caller can tell an unsorted array from one that repeats a value.
   *
   * Values are compared arithmetically, as in the Java original, so positive and negative
   * zero count as a duplicate of each other while a not-a-number value neither equals nor
   * orders against its neighbours and is therefore passed over. `noDuplicates` compares
   * bit patterns and answers the opposite way on both of those inputs.
   *
   * @param argument  the argument to check, unsorted or duplicate values throw an exception
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is unsorted or holds a duplicate value
   */
  def noDuplicatesSorted(argument: Array[Double], name: String): Unit =
    checkSortedFrom(argument, 1, name)

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
   * recursive call is the last thing the method does, so the compiler turns it into a
   * jump.
   *
   * @param values  the values being checked
   * @param index  the index to compare against its predecessor, at least one
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if a value is not strictly greater than the one before
   */
  @tailrec
  private def checkSortedFrom(values: Array[Double], index: Int, name: String): Unit =
    if (index < values.length) {
      val previous = values(index - 1)
      val current = values(index)
      if (current == previous) {
        fail(noDuplicatesArrayMsg(name))
      } else if (current < previous) {
        fail(s"Argument array '$name' must be sorted and not contain duplicates")
      } else {
        checkSortedFrom(values, index + 1, name)
      }
    }

  // extracted so that the wording exists once for both duplicate checks
  private def noDuplicatesArrayMsg(name: String): String =
    s"Argument array '$name' must not contain duplicates"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is not positive.
   *
   * Given the input argument, this returns normally only if it is zero or less.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is positive
   */
  def notPositive(argument: Int, name: String): Unit =
    if (argument > 0) {
      fail(notPositiveMsg(name, argument))
    }

  /**
   * Checks that the argument is not positive.
   *
   * Given the input argument, this returns normally only if it is zero or less.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is positive
   */
  def notPositive(argument: Long, name: String): Unit =
    if (argument > 0L) {
      fail(notPositiveMsg(name, argument))
    }

  /**
   * Checks that the argument is not positive.
   *
   * Given the input argument, this returns normally only if it is zero or less. A
   * not-a-number argument is neither positive nor negative, so it passes this check; use
   * `notNaN` alongside it where a real number is required.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is positive
   */
  def notPositive(argument: Double, name: String): Unit =
    if (argument > 0.0) {
      fail(notPositiveMsg(name, argument))
    }

  /**
   * Checks that the argument is not positive.
   *
   * Given the input argument, this returns normally only if it is zero or less. The sign is
   * read from the decimal itself, so the scale of the value does not affect the outcome.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is positive
   */
  def notPositive(argument: Decimal, name: String): Unit =
    if (argument.signum > 0) {
      fail(notPositiveMsg(name, argument))
    }

  /**
   * Checks that the argument is not positive when a value is present.
   *
   * Given the input argument, this returns normally if no value is present, and otherwise
   * applies the same check as `notPositive`. The Java original took a reference that was
   * allowed to be absent; an `Option` states that intent in the type.
   *
   * @param argument  the argument to check, absent always passes
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if a value is present and is positive
   */
  def notPositiveIfPresent(argument: Option[Decimal], name: String): Unit =
    argument.foreach(value => notPositive(value, name))

  private def notPositiveMsg(name: String, value: Any): String =
    s"Argument '$name' must not be positive but has value $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is not negative.
   *
   * Given the input argument, this returns normally only if it is zero or greater.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative
   */
  def notNegative(argument: Int, name: String): Unit =
    if (argument < 0) {
      fail(notNegativeMsg(name, argument))
    }

  /**
   * Checks that the argument is not negative.
   *
   * Given the input argument, this returns normally only if it is zero or greater.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative
   */
  def notNegative(argument: Long, name: String): Unit =
    if (argument < 0L) {
      fail(notNegativeMsg(name, argument))
    }

  /**
   * Checks that the argument is not negative.
   *
   * Given the input argument, this returns normally only if it is zero or greater. Negative
   * zero compares equal to zero, so it passes, and a not-a-number argument passes as well
   * because it does not order against zero; use `notNaN` alongside it where a real number
   * is required.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative
   */
  def notNegative(argument: Double, name: String): Unit =
    if (argument < 0.0) {
      fail(notNegativeMsg(name, argument))
    }

  /**
   * Checks that the argument is not negative.
   *
   * Given the input argument, this returns normally only if it is zero or greater. The sign
   * is read from the decimal itself, so the scale of the value does not affect the outcome.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative
   */
  def notNegative(argument: Decimal, name: String): Unit =
    if (argument.signum < 0) {
      fail(notNegativeMsg(name, argument))
    }

  private def notNegativeMsg(name: String, value: Any): String =
    s"Argument '$name' must not be negative but has value $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is a number.
   *
   * Given the input argument, this returns normally only if it is an actual number. The
   * infinities are numbers for this purpose and pass; only a not-a-number value fails,
   * which is the value that arithmetic produces when it has no meaningful answer.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is not a number
   */
  def notNaN(argument: Double, name: String): Unit =
    if (argument.isNaN) {
      fail(s"Argument '$name' must not be NaN")
    }

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is neither negative nor zero.
   *
   * Given the input argument, this returns normally only if it is greater than zero.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative or zero
   */
  def notNegativeOrZero(argument: Int, name: String): Unit =
    if (argument <= 0) {
      fail(notNegativeOrZeroMsg(name, argument))
    }

  /**
   * Checks that the argument is neither negative nor zero.
   *
   * Given the input argument, this returns normally only if it is greater than zero.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative or zero
   */
  def notNegativeOrZero(argument: Long, name: String): Unit =
    if (argument <= 0L) {
      fail(notNegativeOrZeroMsg(name, argument))
    }

  /**
   * Checks that the argument is neither negative nor zero.
   *
   * Given the input argument, this returns normally only if it is greater than zero. Both
   * signed zeros fail, since they compare equal to zero, and a not-a-number argument
   * passes because it does not order against zero; use `notNaN` alongside it where a real
   * number is required.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative or zero
   */
  def notNegativeOrZero(argument: Double, name: String): Unit =
    if (argument <= 0.0) {
      fail(notNegativeOrZeroMsg(name, argument))
    }

  /**
   * Checks that the argument is greater than zero, treating anything within the specified
   * tolerance of zero as zero.
   *
   * Given the input argument, this returns normally only if it lies further than the
   * tolerance above zero. The two ways of failing are reported differently: a value close
   * to zero is reported as zero, and a value clearly below zero is reported as too small.
   *
   * {{{
   * ArgCheck.notNegativeOrZero(amount, 0.0001, "amount")
   * }}}
   *
   * The tolerance is itself checked, because a not-a-number tolerance describes no interval
   * and a negative one describes none either; the numeric helper that the Java original
   * delegated the comparison to rejected both the same way. The tolerance is examined for
   * being a number first, so an unusable tolerance is reported as what it is: a
   * not-a-number tolerance is reported as not being a number, a negative one as being
   * negative. A zero tolerance is usable and is accepted, and so is negative zero, which is
   * not a negative value. A not-a-number argument is not near zero and is not below zero, so
   * it passes both tests here, as it did there.
   *
   * @param argument  the argument to check
   * @param tolerance  the tolerance to use for zero, must be a number and must not be
   *   negative
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the tolerance is not a number or is negative, or if
   *   the argument is within the tolerance of zero or is below it
   */
  def notNegativeOrZero(argument: Double, tolerance: Double, name: String): Unit = {
    notNaN(tolerance, "tolerance")
    notNegative(tolerance, "tolerance")
    if (isNearZero(argument, tolerance)) {
      fail(s"Argument '$name' must not be zero")
    }
    if (argument < 0.0) {
      fail(s"Argument '$name' must be greater than zero but has value $argument")
    }
  }

  /**
   * Checks that the argument is neither negative nor zero.
   *
   * Given the input argument, this returns normally only if it is greater than zero. The
   * sign is read from the decimal itself, so the scale of the value does not affect the
   * outcome.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is negative or zero
   */
  def notNegativeOrZero(argument: Decimal, name: String): Unit =
    if (argument.signum <= 0) {
      fail(notNegativeOrZeroMsg(name, argument))
    }

  private def notNegativeOrZeroMsg(name: String, value: Any): String =
    s"Argument '$name' must not be negative or zero but has value $value"

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is not zero.
   *
   * Given the input argument, this returns normally only if it differs from zero. One
   * comparison covers both signed zeros, because they compare equal to each other.
   *
   * @param argument  the argument to check
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is zero
   */
  def notZero(argument: Double, name: String): Unit =
    if (argument == 0.0) {
      fail(notZeroMsg(name))
    }

  /**
   * Checks that the argument is not zero, treating anything within the specified tolerance
   * of zero as zero.
   *
   * Given the input argument, this returns normally only if it lies further than the
   * tolerance from zero, in either direction.
   *
   * {{{
   * ArgCheck.notZero(amount, 0.0001, "amount")
   * }}}
   *
   * As in the check above, the tolerance is itself checked, and for the same reason: neither
   * a not-a-number tolerance nor a negative one describes an interval, and the numeric helper
   * that the Java original delegated the comparison to rejected both. Being a number is
   * checked first, so each of the two unusable tolerances is reported as what it is, and a
   * zero tolerance - either signed zero - is usable and accepted. A not-a-number argument is
   * not near zero, so it passes, as it did there.
   *
   * @param argument  the argument to check
   * @param tolerance  the tolerance to use for zero, must be a number and must not be
   *   negative
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the tolerance is not a number or is negative, or if
   *   the argument is within the tolerance of zero
   */
  def notZero(argument: Double, tolerance: Double, name: String): Unit = {
    notNaN(tolerance, "tolerance")
    notNegative(tolerance, "tolerance")
    if (isNearZero(argument, tolerance)) {
      fail(notZeroMsg(name))
    }
  }

  private def notZeroMsg(name: String): String =
    s"Argument '$name' must not be zero"

  /**
   * Decides whether a value counts as zero at the specified tolerance.
   *
   * This reproduces, for the single comparison that the checks above need, the scalar
   * fuzzy-comparison semantics of the numeric helper that the Java original called: a value
   * is near zero when its magnitude does not exceed the tolerance, or when it equals zero
   * exactly. The second clause is what makes the comparison total - it is the clause that
   * lets a value equal itself even when subtraction would not be informative - and the two
   * clauses together give the behaviour at the edges of the domain. A not-a-number argument
   * is never near zero: its magnitude does not compare against the tolerance and it does not
   * equal zero, so both checks above admit it, exactly as the Java checks did. An infinity is
   * near zero only when the tolerance is itself infinite, since that is the only tolerance an
   * infinite magnitude does not exceed; at every finite tolerance it is clear of zero and
   * passes. The callers guarantee, before reaching here, that the tolerance is a number and
   * is not negative.
   *
   * The array-oriented forms of the comparison live with the rest of the array arithmetic, in
   * `DoubleArrayMath`, and a reader comparing the two should expect them to differ: that
   * object states the migration plan's reading of a fuzzy comparison, under which a
   * not-a-number value is equal to nothing at all and each infinity is equal only to itself
   * at any tolerance, including an infinite one. This local zero test deliberately keeps the
   * Java behaviour instead, because the Java check is the authority for these two checks and
   * their messages. They are also deliberately not called from here: that object checks its
   * own tolerance through this one, and calling back into it would tie the two together in a
   * cycle.
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
   * Given the input argument, this returns normally only if it is at or above the lower
   * bound and strictly below the upper bound.
   *
   * {{{
   * ArgCheck.inRange(fraction, 0.0, 1.0, "fraction")
   * }}}
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRange(argument: Double, lowInclusive: Double, highExclusive: Double, name: String): Unit =
    if (argument < lowInclusive || argument >= highExclusive) {
      fail(inRangeMsg(lowInclusive, name, highExclusive, argument))
    }

  /**
   * Checks that the argument is in the range defined by `low <= x <= high`.
   *
   * Given the input argument, this returns normally only if it lies within the bounds, both
   * of which are allowed.
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highInclusive  the high value of the range, allowed
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeInclusive(argument: Double, lowInclusive: Double, highInclusive: Double, name: String): Unit =
    if (argument < lowInclusive || argument > highInclusive) {
      fail(inRangeInclusiveMsg(lowInclusive, name, highInclusive, argument))
    }

  /**
   * Checks that the argument is in the range defined by `low < x < high`.
   *
   * Given the input argument, this returns normally only if it lies strictly within the
   * bounds, neither of which is allowed.
   *
   * @param argument  the argument to check
   * @param lowExclusive  the low value of the range, not allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeExclusive(argument: Double, lowExclusive: Double, highExclusive: Double, name: String): Unit =
    if (argument <= lowExclusive || argument >= highExclusive) {
      fail(inRangeExclusiveMsg(lowExclusive, name, highExclusive, argument))
    }

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is in the range defined by `low <= x < high`.
   *
   * Given the input argument, this returns normally only if it is at or above the lower
   * bound and strictly below the upper bound. This is the check to use for an index, whose
   * valid range is the size of the thing being indexed taken as the exclusive upper bound:
   *
   * {{{
   * ArgCheck.inRange(index, 0, size, "index")
   * }}}
   *
   * As in the Java original the failure is an `IllegalArgumentException` even for an index,
   * because the caller broke the contract of the method it called rather than the contract
   * of an underlying array.
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRange(argument: Int, lowInclusive: Int, highExclusive: Int, name: String): Unit =
    if (argument < lowInclusive || argument >= highExclusive) {
      fail(inRangeMsg(lowInclusive, name, highExclusive, argument))
    }

  /**
   * Checks that the argument is in the range defined by `low <= x <= high`.
   *
   * Given the input argument, this returns normally only if it lies within the bounds, both
   * of which are allowed.
   *
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highInclusive  the high value of the range, allowed
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeInclusive(argument: Int, lowInclusive: Int, highInclusive: Int, name: String): Unit =
    if (argument < lowInclusive || argument > highInclusive) {
      fail(inRangeInclusiveMsg(lowInclusive, name, highInclusive, argument))
    }

  /**
   * Checks that the argument is in the range defined by `low < x < high`.
   *
   * Given the input argument, this returns normally only if it lies strictly within the
   * bounds, neither of which is allowed.
   *
   * @param argument  the argument to check
   * @param lowExclusive  the low value of the range, not allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeExclusive(argument: Int, lowExclusive: Int, highExclusive: Int, name: String): Unit =
    if (argument <= lowExclusive || argument >= highExclusive) {
      fail(inRangeExclusiveMsg(lowExclusive, name, highExclusive, argument))
    }

  //-------------------------------------------------------------------------
  /**
   * Checks that the argument is in the range defined by `low <= x < high`, ordering the
   * values with the ordering of their type.
   *
   * Given the input argument, this returns normally only if it is at or above the lower
   * bound and strictly below the upper bound:
   *
   * {{{
   * ArgCheck.inRangeComparable(tenor, Tenor.TENOR_1M, Tenor.TENOR_1Y, "tenor")
   * }}}
   *
   * @tparam T  the type of the values being compared
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @param order  the ordering of the type being compared
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeComparable[T](argument: T, lowInclusive: T, highExclusive: T, name: String)(
      implicit order: Order[T]): Unit =
    if (order.lt(argument, lowInclusive) || order.gteqv(argument, highExclusive)) {
      fail(inRangeMsg(lowInclusive, name, highExclusive, argument))
    }

  /**
   * Checks that the argument is in the range defined by `low <= x <= high`, ordering the
   * values with the ordering of their type.
   *
   * Given the input argument, this returns normally only if it lies within the bounds, both
   * of which are allowed.
   *
   * @tparam T  the type of the values being compared
   * @param argument  the argument to check
   * @param lowInclusive  the low value of the range, allowed
   * @param highInclusive  the high value of the range, allowed
   * @param name  the name of the argument to use in the error message
   * @param order  the ordering of the type being compared
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeComparableInclusive[T](argument: T, lowInclusive: T, highInclusive: T, name: String)(
      implicit order: Order[T]): Unit =
    if (order.lt(argument, lowInclusive) || order.gt(argument, highInclusive)) {
      fail(inRangeInclusiveMsg(lowInclusive, name, highInclusive, argument))
    }

  /**
   * Checks that the argument is in the range defined by `low < x < high`, ordering the
   * values with the ordering of their type.
   *
   * Given the input argument, this returns normally only if it lies strictly within the
   * bounds, neither of which is allowed.
   *
   * @tparam T  the type of the values being compared
   * @param argument  the argument to check
   * @param lowExclusive  the low value of the range, not allowed
   * @param highExclusive  the high value of the range, not allowed
   * @param name  the name of the argument to use in the error message
   * @param order  the ordering of the type being compared
   * @throws java.lang.IllegalArgumentException if the argument is outside the range
   */
  def inRangeComparableExclusive[T](argument: T, lowExclusive: T, highExclusive: T, name: String)(
      implicit order: Order[T]): Unit =
    if (order.lteqv(argument, lowExclusive) || order.gteqv(argument, highExclusive)) {
      fail(inRangeExclusiveMsg(lowExclusive, name, highExclusive, argument))
    }

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
   * Given two values, this returns normally only if the first orders strictly before the
   * second. Equal values fail, which is what distinguishes this check from
   * `inOrderOrEqual`:
   *
   * {{{
   * ArgCheck.inOrderNotEqual(startDate, endDate, "startDate", "endDate")
   * }}}
   *
   * @tparam T  the type of the values being compared
   * @param obj1  the first value, expected to order before the second
   * @param obj2  the second value
   * @param name1  the name of the first argument to use in the error message
   * @param name2  the name of the second argument to use in the error message
   * @param order  the ordering of the type being compared
   * @throws java.lang.IllegalArgumentException if the values are equal or out of order
   */
  def inOrderNotEqual[T](obj1: T, obj2: T, name1: String, name2: String)(implicit order: Order[T]): Unit =
    if (order.gteqv(obj1, obj2)) {
      fail(s"Invalid order: Expected '$name1' < '$name2', but found: '$obj1' >= '$obj2'")
    }

  /**
   * Checks that two values are in order or equal.
   *
   * Given two values, this returns normally only if the first orders before the second or
   * equals it. This is the check for a bound that is allowed to be degenerate, such as a
   * period whose two ends may coincide:
   *
   * {{{
   * ArgCheck.inOrderOrEqual(firstDate, secondDate, "firstDate", "secondDate")
   * }}}
   *
   * @tparam T  the type of the values being compared
   * @param obj1  the first value, expected to order before the second or equal it
   * @param obj2  the second value
   * @param name1  the name of the first argument to use in the error message
   * @param name2  the name of the second argument to use in the error message
   * @param order  the ordering of the type being compared
   * @throws java.lang.IllegalArgumentException if the values are out of order
   */
  def inOrderOrEqual[T](obj1: T, obj2: T, name1: String, name2: String)(implicit order: Order[T]): Unit =
    if (order.gt(obj1, obj2)) {
      fail(s"Invalid order: Expected '$name1' <= '$name2', but found: '$obj1' > '$obj2'")
    }
}

//-----------------------------------------------------------------------------
// The JVM-level closure of construction and of Java serialization.
//
// These two declarations sit beside the fail-fast checks above because they are
// fail-fast checks: each states an invariant a caller is required to honour and
// raises `IllegalArgumentException` through `ArgCheck.fail` when it does not
// hold. What distinguishes them is who the caller is. The checks above are
// called by this library's own methods about the arguments they were handed;
// these are called by a type about ITSELF, as it is constructed, and the caller
// they hold to account is whatever compiled the class being constructed - which
// need not be this language, and need not be this library.
//-----------------------------------------------------------------------------

/**
 * Refuses to take part in Java serialization.
 *
 * Mixed into a value type, this closes the one route into that type which does not pass through
 * its factory: `java.io.ObjectInputStream`. The port supports no Java serialization of any kind -
 * JSON through the circe codecs is the only serialized form of a value - and this trait is what
 * makes that statement true of the compiled classes rather than only of the documented API.
 *
 * ===Why a mixin is needed at all===
 *
 * Every value type of this library is a `case class`, because the construction policy of the port
 * is built on what the compiler synthesises for one - `unapply` for pattern matching, structural
 * equality, and, for a validated type declared `sealed abstract case class`, the deliberate
 * absence of `apply` and `copy` - and because the JSON codecs are derived from the product shape
 * at compile time. A `case class` extends `scala.Product with java.io.Serializable`, and that
 * second supertype cannot be removed: it is added by the compiler.
 *
 * Declaring `java.io.Serializable` has a consequence that the rest of the port's construction
 * policy does not cover. `ObjectInputStream` does not call a constructor of the class it is
 * reading; it allocates the instance and assigns its fields from the stream. A stream is ordinary
 * input - it can be written by hand - so without this trait a caller able to hand a stream to the
 * library could obtain a value whose fields no factory would have accepted: a `CurrencyAmount`
 * holding `NaN`, a `SchedulePeriod` whose dates run backwards, a `Currency` outside the published
 * set. Every invariant the smart constructors establish would be reachable around.
 *
 * ===How it refuses===
 *
 * The two hooks below are the ones the serialization mechanism looks up on the class being
 * written or read, and - unlike `writeObject`/`readObject`, which have to be `private` and are
 * therefore invisible to subclasses - they may be inherited. A mixed-in trait is compiled to a
 * concrete method on each implementing class, so both hooks are found on every type that mixes
 * this trait in and on every subclass of one, including the hidden implementation class of a
 * validated type.
 *
 * Both are `final`, which is the part of this the JVM does enforce: the compiler emits the
 * inherited method as `ACC_FINAL` on every implementing class, so a subclass - in this language or
 * any other - cannot override the refusal, and a class file that tries is rejected at class load
 * with `IncompatibleClassChangeError`. Without that, an external subclass could override both
 * hooks to answer with itself and take part in serialization after all, which is the one way a
 * refusal carried by an inherited method can be undone:
 *
 *   - `writeReplace` is consulted before anything is written, so an attempt to serialize a value
 *     of this library fails before a single byte of it reaches the stream;
 *   - `readResolve` is consulted after a stream's fields have been read but before the object is
 *     handed to the caller, so a hand-written stream cannot deliver a forged value. The refusal
 *     is raised there rather than being allowed to return something else, because a caller that
 *     receives no value is in a defined state while one that silently receives a different value
 *     is not.
 *
 * ===What it does not change===
 *
 * Nothing about the in-memory value: the hooks run only inside the serialization mechanism.
 * Equality, hashing, pattern matching, `Show`, the derived JSON codecs and the construction
 * policy of the mixing type are all untouched, and mixing the trait into a family's base class
 * covers the `case object` members of that family, whose singleton-resolving proxy is replaced by
 * the same refusal.
 *
 * {{{
 * final case class Payment(value: CurrencyAmount, date: LocalDate) extends NoJavaSerialization
 * }}}
 */
private[strata] trait NoJavaSerialization extends java.io.Serializable {

  /**
   * Refuses to write this value to an `ObjectOutputStream`.
   *
   * Consulted by the serialization mechanism in place of writing the fields of this value, so the
   * refusal happens before any part of it is written.
   *
   * @return never returns normally
   */
  protected final def writeReplace(): AnyRef = refuseJavaSerialization()

  /**
   * Refuses to hand back a value read from an `ObjectInputStream`.
   *
   * Consulted by the serialization mechanism once a stream's fields have been read and before the
   * instance is handed to the caller, which is the last point at which a value assembled without
   * a factory can be stopped from escaping.
   *
   * @return never returns normally
   */
  protected final def readResolve(): AnyRef = refuseJavaSerialization()

  /**
   * Reports the attempt as the broken precondition it is.
   *
   * Raised through [[ArgCheck]], the one place in this module that raises, because an attempt to
   * serialize a value of this library is a caller using an unsupported mechanism rather than a
   * data-dependent outcome that a result type could carry: the hooks are called by the JVM and
   * have nowhere to return a failure to.
   *
   * The value named after the check is never produced. It exists because the signature of a hook
   * requires one, and it is deliberately an object of a type that is itself not serializable, so
   * that the refusal still holds if this method were ever changed to return instead of raising.
   *
   * @return never returns normally
   */
  private def refuseJavaSerialization(): AnyRef = {
    ArgCheck.isTrue(
      false,
      "Java serialization is not supported by this library, and no type takes part in it: " +
        s"${JvmClosure.describe(this)} can be serialized only through its circe codec")
    JvmClosure.SerializationRefused
  }
}

/**
 * The checks that hold a closed type closed at run time, on the JVM rather than in the source.
 *
 * The construction policy of this port is expressed with `sealed`, with private constructors and
 * with companion-private implementation classes, and the compiler enforces all three - against
 * Scala. None of them survives into the class file as a restriction the JVM applies: `sealed`
 * leaves no trace in the bytecode of Scala 2.13, and a constructor that is `private` or private to
 * a package is emitted as public, because the JVM has no access level that matches either. A class
 * compiled against this library by any other means - another language on the JVM, or a class file
 * written by hand - can therefore declare itself a subtype of a validated type or of a closed
 * family and, through its own constructor, produce an instance carrying whatever state it likes.
 *
 * The two checks below close that gap where it can be closed: in the constructor of the abstract
 * type itself. A subclass constructor must call a constructor of its superclass - the JVM verifier
 * requires it - so a check placed in the body of the abstract class runs for every instance of
 * every subtype, whatever compiled it. An instance whose runtime class is not one this library
 * declared therefore cannot be brought into existence at all.
 *
 * ===How the two checks differ===
 *
 * Which check a type uses follows from how many concrete classes it has:
 *
 *   - a validated or normalising type publishes exactly one, the `Impl` class its companion
 *     declares and hides, so [[requireSoleImplementation]] compares the runtime class with it
 *     directly. This admits nothing else at all;
 *   - a closed named family has one concrete class per member - a `case object` for a member the
 *     source names, a hidden class for the members built from a data table - so
 *     [[requireDeclaredMember]] instead requires the runtime class to be one declared inside the
 *     family's own companion. A class nested inside a type belonging to this library is something
 *     only this library's sources can produce: a compiler emits that relationship only for a class
 *     actually declared there.
 *
 * ===What this does not claim===
 *
 * These are run-time checks, and they are the strongest closure Scala 2.13 can emit: the language
 * has no way to write the `PermittedSubclasses` attribute that would let the JVM reject the
 * subtype at class-load time. A hostile class can still be loaded; what it cannot do is hold an
 * instance of one of these types.
 *
 * Two gaps in the checks above are closed by [[requireInvariant]] rather than by them, and both
 * come from the same fact: a class the compiler was asked to keep private is emitted `ACC_PUBLIC`
 * with an `ACC_PUBLIC` constructor, and only its `InnerClasses` entry records the request. A Java
 * compiler honours that entry and refuses to name the class - reporting `Impl has private access` -
 * but a class file that names it directly is not refused by anything. So:
 *
 *   - `new X$Impl(...)`, emitted by hand or compiled against a declaration that claims the
 *     constructor is public, produces an instance whose runtime class is exactly the class
 *     [[requireSoleImplementation]] admits, and whose fields are whatever the caller passed;
 *   - the same holds for the hidden class a family builds its members from, so a family could
 *     regain the dynamic members this port removed.
 *
 * The answer is that identity is not the only thing checked. Every type that runs one of these
 * guards also states its own invariant through [[requireInvariant]], over the fields the instance
 * was actually built with, so a value that reaches existence by any route holds what its factory
 * would have accepted - and a forged one raises in the same constructor. The negative gates of
 * `scripts/verify-gates.sh` compile and run all three attempts: the external subclass, the direct
 * call of a hidden constructor, and the hand-written object stream.
 */
private[strata] object JvmClosure {

  /** The longest class name a refusal message quotes, before it is elided. */
  private val MaxDescribedNameLength: Int = 200

  /**
   * The value the refusal hooks of [[NoJavaSerialization]] would answer with.
   *
   * A plain object, and so not itself serializable, on the path that cannot be reached while those
   * hooks raise. Nothing observes it: it exists so that the hooks have a value of the type their
   * signature requires, and so that returning one could never amount to permitting serialization.
   */
  private[collect] object SerializationRefused

  /**
   * Requires that a value being constructed is the one implementation its type publishes.
   *
   * Called from the body of a `sealed abstract case class`, where it runs as part of constructing
   * any instance of any subtype of that class. The companion of such a type declares exactly one
   * concrete class and keeps it private, so the comparison below admits the values the type's own
   * factories build and nothing else - in particular no subclass compiled outside this library,
   * whose instance would otherwise carry fields that no factory had validated.
   *
   * @param value  the value being constructed, which is `this` at the call site
   * @param implementation  the one implementation class the type publishes
   * @throws IllegalArgumentException if the value is an instance of any other class
   */
  def requireSoleImplementation(value: AnyRef, implementation: Class[_]): Unit =
    ArgCheck.isTrue(
      value.getClass eq implementation,
      s"${typeOf(implementation)} admits only the implementation it publishes, and " +
        s"${describe(value)} is not it: a value of this type is obtained from its factory")

  /**
   * Names the type an implementation class belongs to.
   *
   * The implementation of a validated type is declared inside that type's companion, so the type
   * is its enclosing class. A class handed in from anywhere else names itself, which keeps this
   * usable for a refusal message rather than making the message itself fail.
   *
   * @param implementation  the implementation class
   * @return the name of the type it implements
   */
  private def typeOf(implementation: Class[_]): String =
    Option(implementation.getEnclosingClass).map(_.getName).getOrElse(implementation.getName)

  /**
   * Requires that a value being constructed is a member its family declares.
   *
   * Called from the body of the abstract class at the head of a closed named family, where it runs
   * as part of constructing any member of it. Every member of such a family is declared inside the
   * family's companion - as a `case object` the source names, or as an instance of a hidden class
   * built from the family's data table - and a class declared there is one only these sources can
   * declare. A subtype compiled anywhere else is nested in something else, or in nothing, and is
   * refused here, so the published members remain the whole of the family at run time as well as
   * in the source.
   *
   * @param value  the member being constructed, which is `this` at the call site
   * @param family  the class at the head of the family
   * @throws IllegalArgumentException if the value's class is not declared inside the family
   */
  def requireDeclaredMember(value: AnyRef, family: Class[_]): Unit = {
    val declaredIn: Option[String] = Option(value.getClass.getEnclosingClass).map(_.getName)
    ArgCheck.isTrue(
      declaredIn.exists(name => name == family.getName || name == s"${family.getName}$$"),
      s"${family.getName} is a closed family and ${describe(value)} is not one of its published " +
        "members: a member of this family is obtained from the family's companion")
  }

  /**
   * Names the class of a value for a refusal message, bounded and free of control characters.
   *
   * The class named here is the one thing in these messages that the caller chose, and a refusal
   * is a line that reaches a log, so the name is truncated to a length a log line can hold and
   * every control character in it is replaced. A class name is ordinarily plain text and passes
   * through unchanged; one built to carry a newline or to be pathologically long cannot forge or
   * flood the record of its own rejection.
   *
   * @param value  the value whose class is to be named
   * @return the name of its class, bounded and printable
   */
  private[collect] def describe(value: AnyRef): String = {
    val name: String = value.getClass.getName
    val bounded: String =
      if (name.length > MaxDescribedNameLength) s"${name.take(MaxDescribedNameLength)}..." else name
    bounded.map(character => if (character.isControl) '?' else character)
  }

  /**
   * Requires that a value being constructed belongs to one of the subtypes its root permits.
   *
   * The root of a closed hierarchy has no members of its own: every value of it is a member of one
   * of the families beneath it, each of which closes itself with [[requireDeclaredMember]]. What
   * this closes is the root: a class compiled outside this library could otherwise extend the root
   * directly, skipping every family, and be accepted everywhere the root's type is - which for the
   * index hierarchy would mean an index belonging to no family, resolving through no lookup, and
   * absent from a match the compiler proved exhaustive.
   *
   * Called from the body of the root, so it runs for every instance of every subtype of it.
   *
   * @param value  the value being constructed, which is `this` at the call site
   * @param permitted  the subtypes the root admits, which are the families beneath it
   * @throws IllegalArgumentException if the value belongs to none of them
   */
  def requirePermittedSubtype(value: AnyRef, permitted: Class[_]*): Unit =
    ArgCheck.isTrue(
      permitted.exists(subtype => subtype.isInstance(value)),
      s"${describe(value)} is not one of the subtypes this hierarchy admits, which are " +
        s"${permitted.map(subtype => subtype.getName).mkString(", ")}: a value of this type is " +
        "obtained from one of those families")

  /**
   * Requires that the invariant of a value being constructed holds.
   *
   * This is the second half of the construction closure, and the half that does not depend on the
   * runtime class of what is being built. [[requireSoleImplementation]] and
   * [[requireDeclaredMember]] admit the classes this library declares, and a class file written
   * outside it can name one of those classes directly - the compiler emits them `ACC_PUBLIC`
   * whatever the source asked for - so identity alone would let a forged instance of the right
   * class carry fields no factory ever saw.
   *
   * A type therefore states here what its own factory establishes: the condition that makes its
   * fields a value of it. Called from the body of the type, where its fields are in scope, it runs
   * for every instance of every subtype however that instance came to be constructed, so the
   * statement "a value of this type satisfies this" holds of every value that exists rather than
   * only of the ones built through the published factory.
   *
   * Which half of the call is deferred follows from when each half is read. The condition is the
   * invariant, and every call evaluates it exactly once - [[ArgCheck.isTrue]] tests it whether it
   * holds or not - so it is taken strictly: by name it would buy no laziness and would cost one
   * closure, capturing the value under construction, on every construction. The description
   * completes the sentence "a value of this type requires that ..." in the refusal, which is read
   * only when the refusal happens, so it is taken by name and the sentence is built only then.
   * Together with the reasoning above, that leaves the closure of construction paying for the
   * checks it runs and for nothing else.
   *
   * @param description  what the invariant requires, as a phrase completing "requires that ...",
   *   built only if the invariant does not hold
   * @param condition  the invariant, evaluated once by the caller
   * @throws IllegalArgumentException if the invariant does not hold
   */
  def requireInvariant(description: => String, condition: Boolean): Unit =
    ArgCheck.isTrue(
      condition,
      s"a value of this type requires that $description, and the value being constructed does " +
        "not: a value of this type is obtained from its factory")

}
