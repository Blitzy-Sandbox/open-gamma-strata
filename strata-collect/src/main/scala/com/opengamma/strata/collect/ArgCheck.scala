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
 * ===Deliberate differences from the Java original===
 *
 * The Java class that this is ported from returned the checked argument, so that a check
 * could be written inline in a field assignment. Here every check returns `Unit` instead.
 * The build treats a discarded value as an error, so returning the argument would force
 * every call into a binding that exists only to be thrown away, whereas returning `Unit`
 * lets a check read as the statement that it is. Nothing is lost: the argument is already
 * in scope at the call site, and validated construction goes through a factory that
 * returns the finished value.
 *
 * The Java checks for a missing reference are not ported at all. A value that may be
 * absent is modelled as `Option` here and a reference is never empty of a referent, so
 * those checks would have no subject: absence is described by the type and handled by
 * matching on it. `notPositiveIfPresent` keeps its name and takes an `Option`.
 *
 * Three further shapes collapsed, each because Scala expresses the variants with a single
 * member:
 *
 *   - the four message-bearing `isTrue` overloads became one member that takes the message
 *     by name, so the text is built only when the check fails. The message template
 *     of the Java original is replaced by ordinary interpolation at the call site, which
 *     the compiler checks;
 *   - the `isFalse` overloads collapsed the same way. As in Java, there is no form of
 *     `isFalse` without a message;
 *   - the separate check for a collection folded into the check for an iterable, because
 *     `Iterable` covers both - Java needed two members only because its collection type
 *     adds an emptiness test of its own. A collection reported as empty therefore carries
 *     the iterable wording.
 *
 * Message text is otherwise reproduced word for word from the Java original, because it
 * reaches logs and test expectations. That fidelity is also why a check throws the
 * exception directly instead of delegating to the standard `require`, which prefixes every
 * message it is handed with wording of its own.
 *
 * ===Ordering-based checks===
 *
 * The checks that compare values - `inRangeComparable`, `inOrderNotEqual` and their
 * siblings - take a `cats.Order` for the type being compared, which keeps the Java
 * comparison interface out of this API and lets the check work for any type that the
 * library already orders. A type whose Java comparison is declared against a supertype has
 * no such instance in scope by default; `Order.fromOrdering` derives one from the standard
 * library ordering, which those types do have. Where deriving an instance is not worth it,
 * `isTrue` with an explicit comparison expresses the same invariant.
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
   * @throws IllegalArgumentException always
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
   * @throws IllegalArgumentException if the test value is false
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
   * @throws IllegalArgumentException if the test value is false
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
   * @throws IllegalArgumentException if the test value is true
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
   * @throws IllegalArgumentException if the argument does not match the pattern
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
   * @throws IllegalArgumentException if the argument is the wrong length or contains a
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
   * @throws IllegalArgumentException if the argument is empty or entirely whitespace
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument holds no elements
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument is empty
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
   * @throws IllegalArgumentException if the argument holds a duplicate value
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
   * @throws IllegalArgumentException if the argument is unsorted or holds a duplicate value
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
   * @throws IllegalArgumentException if a value is not strictly greater than the one before
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
   * @throws IllegalArgumentException if the argument is positive
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
   * @throws IllegalArgumentException if the argument is positive
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
   * @throws IllegalArgumentException if the argument is positive
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
   * @throws IllegalArgumentException if the argument is positive
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
   * @throws IllegalArgumentException if a value is present and is positive
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
   * @throws IllegalArgumentException if the argument is negative
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
   * @throws IllegalArgumentException if the argument is negative
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
   * @throws IllegalArgumentException if the argument is negative
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
   * @throws IllegalArgumentException if the argument is negative
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
   * @throws IllegalArgumentException if the argument is not a number
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
   * @throws IllegalArgumentException if the argument is negative or zero
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
   * @throws IllegalArgumentException if the argument is negative or zero
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
   * @throws IllegalArgumentException if the argument is negative or zero
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
   * The tolerance is itself checked, because a negative tolerance describes no interval;
   * the third-party helper that the Java original used for the comparison rejected one the
   * same way. A not-a-number argument is not near zero and is not below zero, so it passes
   * both tests here, as it did there.
   *
   * @param argument  the argument to check
   * @param tolerance  the tolerance to use for zero, must not be negative
   * @param name  the name of the argument to use in the error message
   * @throws IllegalArgumentException if the tolerance is negative, or if the argument is
   *   within the tolerance of zero or is below it
   */
  def notNegativeOrZero(argument: Double, tolerance: Double, name: String): Unit = {
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
   * @throws IllegalArgumentException if the argument is negative or zero
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
   * @throws IllegalArgumentException if the argument is zero
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
   * The tolerance is itself checked, because a negative tolerance describes no interval;
   * the third-party helper that the Java original used for the comparison rejected one the
   * same way. A not-a-number argument is not near zero, so it passes, as it did there.
   *
   * @param argument  the argument to check
   * @param tolerance  the tolerance to use for zero, must not be negative
   * @param name  the name of the argument to use in the error message
   * @throws IllegalArgumentException if the tolerance is negative, or if the argument is
   *   within the tolerance of zero
   */
  def notZero(argument: Double, tolerance: Double, name: String): Unit = {
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
   * clauses together give the behaviour that matters at the edges of the domain: a
   * not-a-number value is never near zero, and neither infinity is either.
   *
   * The array-oriented forms of the same comparison live with the rest of the array
   * arithmetic, in `DoubleArrayMath`. They are deliberately not called from here: that
   * object checks its own arguments through this one, and calling back into it would tie
   * the two together in a cycle.
   *
   * @param argument  the value to test
   * @param tolerance  the tolerance to use for zero
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the argument is outside the range
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
   * @throws IllegalArgumentException if the values are equal or out of order
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
   * @throws IllegalArgumentException if the values are out of order
   */
  def inOrderOrEqual[T](obj1: T, obj2: T, name1: String, name2: String)(implicit order: Order[T]): Unit =
    if (order.gt(obj1, obj2)) {
      fail(s"Invalid order: Expected '$name1' <= '$name2', but found: '$obj1' > '$obj2'")
    }
}
