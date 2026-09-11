/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.time.Duration
import java.time.LocalDate

import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.SortedMap
import scala.collection.immutable.Vector
import scala.util.matching.Regex

import cats.Order

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4
import org.scalatest.prop.TableFor5
import org.scalatest.prop.TableFor6
import org.scalatest.prop.Tables
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.ArgCheckTables.durationOrder
import com.opengamma.strata.collect.ArgCheckTables.localDateOrder
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.Matrix

/**
 * The inputs of every check that [[ArgCheck]] and [[Validate]] share, with the message each
 * one reports.
 *
 * ===Why this exists===
 *
 * The two objects are one predicate vocabulary with two answers. [[ArgCheck]] states a
 * caller-contract invariant and throws when it is broken; [[Validate]] states the same
 * predicate over data a user supplied and hands the breach back as a value. Their message
 * text is identical word for word, deliberately, because the message is what reaches a log
 * or a report whichever object produced it.
 *
 * Holding the inputs here rather than in either spec is what keeps that true. A row added
 * for one object is checked against the other on the next compile, and a message reworded in
 * one place fails both specs rather than silently drifting.
 *
 * ===The shape of a table, and the promise it makes===
 *
 * Every table is named for the check it feeds and for the sign of its rows:
 *
 *   - `invalid<Check>` rows end with `expectedMessage`, the '''complete''' message text -
 *     never a fragment and never a pattern - preceded by the arguments of the check in the
 *     order the check declares them. [[ArgCheckSpec]] asserts that text is the message of the
 *     thrown error; a spec for [[Validate]] asserts the same text is the message of the
 *     accumulated failure, whose attributes are empty.
 *   - `valid<Check>` rows are the arguments alone, in the same order. [[ArgCheckSpec]] asserts
 *     the check returns quietly; a spec for [[Validate]] asserts it returns the argument.
 *
 * No row carries the argument ''name'': every check in every row is called under
 * [[ArgumentName]], and the two order checks under [[FirstName]] and [[SecondName]], so that
 * the names built into the message text stay in step with the calls that produce it.
 *
 * Several valid tables hold a not-a-number value, because most of the numeric checks document
 * that they admit one: it does not order against zero. A spec that asserts the value a check
 * gave back therefore cannot compare it with equality, since that value does not equal itself;
 * compare such a row by its bit pattern, or assert only that the check passed.
 *
 * Nothing here mentions either object, so neither a throw nor a failure value appears in a
 * table. That is what lets both specs consume the same rows.
 *
 * ===Changing a table===
 *
 * These names are published API within this package: more than one spec resolves them, so a
 * rename or a change of arity breaks a spec that this file does not contain. Add rows freely
 * - both specs iterate whatever is there - and treat a name, an arity and a column order as
 * fixed once written. Where a check exists on only one of the two objects its table says so,
 * as [[invalidNotEmptyMatrix]] does.
 */
private[collect] object ArgCheckTables extends Tables {

  /**
   * The argument name every row of every table is checked under.
   *
   * The message text of each row contains this name, so a spec has to pass exactly this
   * value to the check it is exercising.
   */
  val ArgumentName: String = "name"

  /** The name the two order checks report their first argument under. */
  val FirstName: String = "a"

  /** The name the two order checks report their second argument under. */
  val SecondName: String = "b"

  /**
   * The name the two tolerance-bearing checks report their own tolerance argument under.
   *
   * A tolerance that is not a number and a tolerance that is negative are both rejected by
   * the checks themselves, each with its own wording, and the message names the tolerance
   * rather than the argument being checked, so a spec can tell the failures of those checks
   * apart.
   */
  val ToleranceName: String = "tolerance"

  //-------------------------------------------------------------------------
  // Decimal fixtures.
  //
  // Every Decimal factory returns an Either, because text and a Double can both name a value
  // the type cannot hold. Addition and subtraction of a Long or a Double are total, so these
  // build from the zero constant and need no unwrapping - which also keeps the expected
  // message text of the Decimal rows honest, since it is the rendering of these very values.

  /** Zero as a decimal, which is neither positive nor negative. */
  val DecimalZero: Decimal = Decimal.ZERO

  /** One as a decimal, rendered `1`. */
  val DecimalOne: Decimal = Decimal.ZERO.plus(1L)

  /** Minus one as a decimal, rendered `-1`. */
  val DecimalMinusOne: Decimal = Decimal.ZERO.minus(1L)

  /** A positive decimal with a fraction, rendered `1.2`. */
  val DecimalPositive: Decimal = Decimal.ZERO.plus(1.2)

  /** A negative decimal with a fraction, rendered `-1.2`. */
  val DecimalNegative: Decimal = Decimal.ZERO.minus(1.2)

  //-------------------------------------------------------------------------
  // Matrix fixtures.
  //
  // The matrix check reads the total number of elements, across every dimension, so it needs
  // a value of more than one dimension to be covered properly. The trait is open and carries
  // no data, which is why these two are declared here rather than borrowed from a concrete
  // matrix type that only ever has one dimension.

  /** A one-dimensional matrix holding no elements. */
  val EmptyMatrix: Matrix = DoubleArray.EMPTY

  /** A one-dimensional matrix holding one element. */
  val SingletonMatrix: Matrix = DoubleArray.of(1.0)

  /** A two-dimensional matrix holding no elements, which reports two dimensions and no size. */
  val EmptyTwoDimensionalMatrix: Matrix = new Matrix {
    override def dimensions: Int = 2
    override def size: Int = 0
  }

  /** A two-dimensional matrix holding six elements across its two dimensions. */
  val PopulatedTwoDimensionalMatrix: Matrix = new Matrix {
    override def dimensions: Int = 2
    override def size: Int = 6
  }

  //-------------------------------------------------------------------------
  /**
   * The character predicate the `matches` rows use, admitting an upper-case letter.
   *
   * The check takes a predicate over characters, so the readable form of the same condition
   * is passed separately and appears in the message; the rows pair the two.
   */
  val UpperCaseLetter: Char => Boolean = c => c >= 'A' && c <= 'Z'

  /**
   * The ordering of a date, derived from the comparison the type already defines.
   *
   * The order-based checks take the ordering of the type being compared rather than a
   * comparison interface, and a date carries no such instance of its own, so both specs
   * resolve this one.
   */
  implicit val localDateOrder: Order[LocalDate] = Order.from((a, b) => a.compareTo(b))

  /**
   * The ordering of a duration, derived the same way as the ordering of a date.
   *
   * The comparable range checks are exercised over durations because that is the type the
   * ported tests used, and it orders below zero as readily as above it.
   */
  implicit val durationOrder: Order[Duration] = Order.from((a, b) => a.compareTo(b))

  //-------------------------------------------------------------------------
  /** Patterns and arguments that do not match, with the message reporting the mismatch. */
  val invalidMatchesRegex: TableFor3[Regex, String, String] =
    Table(
      ("pattern", "argument", "expectedMessage"),
      ("[A-Z]+".r, "", "Argument 'name' with value '' must match pattern: [A-Z]+"),
      ("[A-Z]+".r, "123", "Argument 'name' with value '123' must match pattern: [A-Z]+"),
      ("[A-Z]+".r, "og", "Argument 'name' with value 'og' must match pattern: [A-Z]+"),
      ("[A-Z]+".r, "OG1", "Argument 'name' with value 'OG1' must match pattern: [A-Z]+"),
      ("[A-Z]+".r, " OG", "Argument 'name' with value ' OG' must match pattern: [A-Z]+"),
      ("[0-9]{2}".r, "1", "Argument 'name' with value '1' must match pattern: [0-9]{2}"),
      ("[0-9]{2}".r, "123", "Argument 'name' with value '123' must match pattern: [0-9]{2}"))

  /** Patterns and arguments that match, so the check returns quietly. */
  val validMatchesRegex: TableFor2[Regex, String] =
    Table(
      ("pattern", "argument"),
      ("[A-Z]+".r, "OG"),
      ("[A-Z]+".r, "O"),
      ("[A-Z]+".r, "OPENGAMMA"),
      ("[0-9]{2}".r, "12"),
      ("[A-Z]*".r, ""))

  /**
   * Arguments the character form rejects, with the message reporting the rejection.
   *
   * The columns are the arguments of the check in the order it declares them, less the name:
   * the predicate, the two length bounds, the argument and the readable form of the
   * predicate that the message quotes.
   */
  val invalidMatchesPredicate: TableFor6[Char => Boolean, Int, Int, String, String, String] =
    Table(
      ("matcher", "minLength", "maxLength", "argument", "equivalentRegex", "expectedMessage"),
      (UpperCaseLetter, 1, 2, "", "[A-Z]{1,2}",
          "Argument 'name' with value '' must match pattern: [A-Z]{1,2}"),
      (UpperCaseLetter, 1, 2, "abc", "[A-Z]{1,2}",
          "Argument 'name' with value 'abc' must match pattern: [A-Z]{1,2}"),
      (UpperCaseLetter, 1, 2, "ABC", "[A-Z]{1,2}",
          "Argument 'name' with value 'ABC' must match pattern: [A-Z]{1,2}"),
      (UpperCaseLetter, 2, 3, "A", "[A-Z]{2,3}",
          "Argument 'name' with value 'A' must match pattern: [A-Z]{2,3}"),
      (UpperCaseLetter, 1, Int.MaxValue, "123", "[A-Z]+",
          "Argument 'name' with value '123' must match pattern: [A-Z]+"),
      (UpperCaseLetter, 1, Int.MaxValue, "Og", "[A-Z]+",
          "Argument 'name' with value 'Og' must match pattern: [A-Z]+"))

  /** Arguments the character form accepts, so the check returns quietly. */
  val validMatchesPredicate: TableFor5[Char => Boolean, Int, Int, String, String] =
    Table(
      ("matcher", "minLength", "maxLength", "argument", "equivalentRegex"),
      (UpperCaseLetter, 1, Int.MaxValue, "OG", "[A-Z]+"),
      (UpperCaseLetter, 1, 2, "OG", "[A-Z]{1,2}"),
      (UpperCaseLetter, 1, 2, "O", "[A-Z]{1,2}"),
      (UpperCaseLetter, 2, 3, "OGA", "[A-Z]{2,3}"),
      (UpperCaseLetter, 0, 0, "", "[A-Z]{0}"))

  //-------------------------------------------------------------------------
  /** Arguments holding nothing but whitespace, with the message reporting them blank. */
  val invalidNotBlank: TableFor2[String, String] =
    Table(
      ("argument", "expectedMessage"),
      ("", "Argument 'name' must not be blank"),
      (" ", "Argument 'name' must not be blank"),
      ("   ", "Argument 'name' must not be blank"),
      ("\t", "Argument 'name' must not be blank"),
      ("\n", "Argument 'name' must not be blank"),
      (" \t\n ", "Argument 'name' must not be blank"))

  /** Arguments holding at least one character that is not whitespace. */
  val validNotBlank: TableFor1[String] =
    Table("argument", "OG", " OG ", "O", "  a  ", "-")

  //-------------------------------------------------------------------------
  /** The only empty text, with the message reporting it empty. */
  val invalidNotEmptyString: TableFor2[String, String] =
    Table(
      ("argument", "expectedMessage"),
      ("", "Argument 'name' must not be empty"))

  /** Text holding at least one character, whitespace included. */
  val validNotEmptyString: TableFor1[String] =
    Table("argument", "OG", " ", "\t", "  ", "a")

  /** An empty array of references, with the message reporting the array empty. */
  val invalidNotEmptyObjectArray: TableFor2[Array[String], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array.empty[String], "Argument array 'name' must not be empty"))

  /** Arrays of references holding at least one element. */
  val validNotEmptyObjectArray: TableFor1[Array[String]] =
    Table("argument", Array("Element"), Array("A", "B"))

  /**
   * An empty array of arrays, with the message reporting the array empty.
   *
   * A nested array is checked by the same member as any other array of references, the
   * element type being of no interest to a check that counts elements; the rows exist because
   * the ported tests covered the shape separately.
   */
  val invalidNotEmptyNestedArray: TableFor2[Array[Array[String]], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array.empty[Array[String]], "Argument array 'name' must not be empty"))

  /** Arrays of arrays holding at least one element, whose own elements may be absent. */
  val validNotEmptyNestedArray: TableFor1[Array[Array[String]]] =
    Table("argument", Array(Array.empty[String]), Array(Array("A"), Array("B")))

  /** An empty array of ints, with the message reporting the array empty. */
  val invalidNotEmptyIntArray: TableFor2[Array[Int], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array.empty[Int], "Argument array 'name' must not be empty"))

  /** Arrays of ints holding at least one element. */
  val validNotEmptyIntArray: TableFor1[Array[Int]] =
    Table("argument", Array(6), Array(0), Array(1, 2, 3))

  /** An empty array of longs, with the message reporting the array empty. */
  val invalidNotEmptyLongArray: TableFor2[Array[Long], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array.empty[Long], "Argument array 'name' must not be empty"))

  /** Arrays of longs holding at least one element. */
  val validNotEmptyLongArray: TableFor1[Array[Long]] =
    Table("argument", Array(6L), Array(0L), Array(1L, 2L, 3L))

  /** An empty array of doubles, with the message reporting the array empty. */
  val invalidNotEmptyDoubleArray: TableFor2[Array[Double], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array.empty[Double], "Argument array 'name' must not be empty"))

  /** Arrays of doubles holding at least one element, a not-a-number element included. */
  val validNotEmptyDoubleArray: TableFor1[Array[Double]] =
    Table("argument", Array(6.0), Array(0.0), Array(Double.NaN), Array(1.0, 2.0, 3.0))

  /**
   * Matrices holding no elements, with the message reporting the matrix empty.
   *
   * This check exists on [[ArgCheck]] alone: a matrix is a caller-supplied numeric buffer
   * whose emptiness is an invariant of the calling code rather than a property of user data,
   * so [[Validate]] has no counterpart and a spec for it does not read this table.
   */
  val invalidNotEmptyMatrix: TableFor2[Matrix, String] =
    Table(
      ("argument", "expectedMessage"),
      (EmptyMatrix, "Argument array 'name' must not be empty"),
      (EmptyTwoDimensionalMatrix, "Argument array 'name' must not be empty"))

  /** Matrices holding at least one element, counted across every dimension. */
  val validNotEmptyMatrix: TableFor1[Matrix] =
    Table("argument", SingletonMatrix, DoubleArray.of(1.0, 2.0), PopulatedTwoDimensionalMatrix)

  /**
   * Empty iterables, with the message reporting the iterable empty.
   *
   * Every ordinary collection shape reaches the same member, which is why a list, a vector
   * and a set all appear here and all carry the iterable wording. The ported original had a
   * second member for a collection, with wording of its own; a collection is an iterable
   * here, so a collection reported as empty carries this message.
   */
  val invalidNotEmptyIterable: TableFor2[Iterable[String], String] =
    Table(
      ("argument", "expectedMessage"),
      (List.empty[String], "Argument iterable 'name' must not be empty"),
      (Vector.empty[String], "Argument iterable 'name' must not be empty"),
      (Set.empty[String], "Argument iterable 'name' must not be empty"))

  /** Iterables holding at least one element. */
  val validNotEmptyIterable: TableFor1[Iterable[String]] =
    Table("argument", List("Element"), Vector("A", "B"), Set("A"), List(""))

  /** Empty maps, with the message reporting the map empty. */
  val invalidNotEmptyMap: TableFor2[Map[String, String], String] =
    Table(
      ("argument", "expectedMessage"),
      (Map.empty[String, String], "Argument map 'name' must not be empty"),
      (SortedMap.empty[String, String], "Argument map 'name' must not be empty"))

  /** Maps holding at least one mapping. */
  val validNotEmptyMap: TableFor1[Map[String, String]] =
    Table(
      "argument",
      Map("Element" -> "Element"),
      SortedMap("Element" -> "Element"),
      Map("A" -> "B", "C" -> "D"))

  //-------------------------------------------------------------------------
  /**
   * Arrays holding a repeated value, with the message reporting the duplicate.
   *
   * Values are compared here by their bit patterns, which is how the ported original's set of
   * boxed values compared them, so an array holding a not-a-number value twice repeats a
   * value while one holding both signed zeros does not.
   */
  val invalidNoDuplicates: TableFor2[Array[Double], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array(1.0, 1.0), "Argument array 'name' must not contain duplicates"),
      (Array(0.0, 1.0, 10.0, 5.0, 1.0), "Argument array 'name' must not contain duplicates"),
      (Array(5.0, 1.0, 5.0), "Argument array 'name' must not contain duplicates"),
      (Array(0.0, Double.NaN, Double.NaN), "Argument array 'name' must not contain duplicates"))

  /** Arrays holding no repeated value, in any order. */
  val validNoDuplicates: TableFor1[Array[Double]] =
    Table(
      "argument",
      Array.empty[Double],
      Array(1.0),
      Array(0.0, 1.0, 10.0, 5.0),
      Array(0.0, 1.0, 10.0, Double.NaN),
      Array(0.0, -0.0))

  /**
   * Arrays that repeat a value or are out of order, with the message reporting which.
   *
   * Values are compared arithmetically here, as in the ported original, so both signed zeros
   * count as a repeat of each other while a not-a-number value neither equals nor orders
   * against its neighbours and is passed over.
   */
  val invalidNoDuplicatesSorted: TableFor2[Array[Double], String] =
    Table(
      ("argument", "expectedMessage"),
      (Array(1.0, 1.0), "Argument array 'name' must not contain duplicates"),
      (Array(0.0, 1.0, 5.0, 5.0, 10.0), "Argument array 'name' must not contain duplicates"),
      (Array(0.0, -0.0), "Argument array 'name' must not contain duplicates"),
      (Array(1.0, 0.0), "Argument array 'name' must be sorted and not contain duplicates"),
      (Array(0.0, 1.0, 5.0, 10.0, 4.0), "Argument array 'name' must be sorted and not contain duplicates"))

  /** Arrays whose values increase strictly, a not-a-number value being passed over. */
  val validNoDuplicatesSorted: TableFor1[Array[Double]] =
    Table(
      "argument",
      Array.empty[Double],
      Array(1.0),
      Array(0.0, 1.0, 5.0, 10.0),
      Array(0.0, 1.0, 5.0, Double.NaN, 10.0),
      Array(Double.NegativeInfinity, 0.0, Double.PositiveInfinity))

  //-------------------------------------------------------------------------
  /** Positive ints, with the message reporting the value found. */
  val invalidNotPositiveInt: TableFor2[Int, String] =
    Table(
      ("argument", "expectedMessage"),
      (1, "Argument 'name' must not be positive but has value 1"),
      (2, "Argument 'name' must not be positive but has value 2"),
      (Int.MaxValue, "Argument 'name' must not be positive but has value 2147483647"))

  /** Ints that are zero or less. */
  val validNotPositiveInt: TableFor1[Int] =
    Table("argument", 0, -1, -2, Int.MinValue)

  /** Positive longs, with the message reporting the value found. */
  val invalidNotPositiveLong: TableFor2[Long, String] =
    Table(
      ("argument", "expectedMessage"),
      (1L, "Argument 'name' must not be positive but has value 1"),
      (2L, "Argument 'name' must not be positive but has value 2"),
      (Long.MaxValue, "Argument 'name' must not be positive but has value 9223372036854775807"))

  /** Longs that are zero or less. */
  val validNotPositiveLong: TableFor1[Long] =
    Table("argument", 0L, -1L, -2L, Long.MinValue)

  /** Positive doubles, with the message reporting the value found. */
  val invalidNotPositiveDouble: TableFor2[Double, String] =
    Table(
      ("argument", "expectedMessage"),
      (1.0, "Argument 'name' must not be positive but has value 1.0"),
      (1.0E-9, "Argument 'name' must not be positive but has value 1.0E-9"),
      (Double.PositiveInfinity, "Argument 'name' must not be positive but has value Infinity"))

  /**
   * Doubles that are not positive.
   *
   * A not-a-number value is neither positive nor negative, so it belongs here: it does not
   * order against zero and this check therefore admits it, as the ported original did.
   */
  val validNotPositiveDouble: TableFor1[Double] =
    Table("argument", 0.0, -0.0, -1.0, Double.NegativeInfinity, Double.NaN)

  /** Positive decimals, with the message reporting the value found. */
  val invalidNotPositiveDecimal: TableFor2[Decimal, String] =
    Table(
      ("argument", "expectedMessage"),
      (DecimalOne, "Argument 'name' must not be positive but has value 1"),
      (DecimalPositive, "Argument 'name' must not be positive but has value 1.2"))

  /** Decimals that are zero or less. */
  val validNotPositiveDecimal: TableFor1[Decimal] =
    Table("argument", DecimalZero, DecimalMinusOne, DecimalNegative)

  /** A present positive decimal, with the message reporting the value found. */
  val invalidNotPositiveIfPresent: TableFor2[Option[Decimal], String] =
    Table(
      ("argument", "expectedMessage"),
      (Some(DecimalOne), "Argument 'name' must not be positive but has value 1"),
      (Some(DecimalPositive), "Argument 'name' must not be positive but has value 1.2"))

  /** An absent decimal, which always passes, and present ones that are zero or less. */
  val validNotPositiveIfPresent: TableFor1[Option[Decimal]] =
    Table("argument", None, Some(DecimalZero), Some(DecimalMinusOne), Some(DecimalNegative))

  //-------------------------------------------------------------------------
  /** Negative ints, with the message reporting the value found. */
  val invalidNotNegativeInt: TableFor2[Int, String] =
    Table(
      ("argument", "expectedMessage"),
      (-1, "Argument 'name' must not be negative but has value -1"),
      (-2, "Argument 'name' must not be negative but has value -2"),
      (Int.MinValue, "Argument 'name' must not be negative but has value -2147483648"))

  /** Ints that are zero or greater. */
  val validNotNegativeInt: TableFor1[Int] =
    Table("argument", 0, 1, 2, Int.MaxValue)

  /** Negative longs, with the message reporting the value found. */
  val invalidNotNegativeLong: TableFor2[Long, String] =
    Table(
      ("argument", "expectedMessage"),
      (-1L, "Argument 'name' must not be negative but has value -1"),
      (-2L, "Argument 'name' must not be negative but has value -2"),
      (Long.MinValue, "Argument 'name' must not be negative but has value -9223372036854775808"))

  /** Longs that are zero or greater. */
  val validNotNegativeLong: TableFor1[Long] =
    Table("argument", 0L, 1L, 2L, Long.MaxValue)

  /** Negative doubles, with the message reporting the value found. */
  val invalidNotNegativeDouble: TableFor2[Double, String] =
    Table(
      ("argument", "expectedMessage"),
      (-1.0, "Argument 'name' must not be negative but has value -1.0"),
      (-1.0E-9, "Argument 'name' must not be negative but has value -1.0E-9"),
      (Double.NegativeInfinity, "Argument 'name' must not be negative but has value -Infinity"))

  /**
   * Doubles that are not negative.
   *
   * Negative zero compares equal to zero, so it passes, and a not-a-number value passes for
   * the same reason it passes the check for a positive value.
   */
  val validNotNegativeDouble: TableFor1[Double] =
    Table("argument", 0.0, -0.0, 1.0, Double.PositiveInfinity, Double.NaN)

  /** Negative decimals, with the message reporting the value found. */
  val invalidNotNegativeDecimal: TableFor2[Decimal, String] =
    Table(
      ("argument", "expectedMessage"),
      (DecimalMinusOne, "Argument 'name' must not be negative but has value -1"),
      (DecimalNegative, "Argument 'name' must not be negative but has value -1.2"))

  /** Decimals that are zero or greater. */
  val validNotNegativeDecimal: TableFor1[Decimal] =
    Table("argument", DecimalZero, DecimalOne, DecimalPositive)

  //-------------------------------------------------------------------------
  /** The only value that is not a number, with the message reporting it. */
  val invalidNotNaN: TableFor2[Double, String] =
    Table(
      ("argument", "expectedMessage"),
      (Double.NaN, "Argument 'name' must not be NaN"))

  /** Actual numbers, the two infinities included. */
  val validNotNaN: TableFor1[Double] =
    Table("argument", 0.0, -0.0, 1.0, -1.0, Double.PositiveInfinity, Double.NegativeInfinity)

  //-------------------------------------------------------------------------
  /** Ints that are negative or zero, with the message reporting the value found. */
  val invalidNotNegativeOrZeroInt: TableFor2[Int, String] =
    Table(
      ("argument", "expectedMessage"),
      (0, "Argument 'name' must not be negative or zero but has value 0"),
      (-1, "Argument 'name' must not be negative or zero but has value -1"),
      (Int.MinValue, "Argument 'name' must not be negative or zero but has value -2147483648"))

  /** Ints greater than zero. */
  val validNotNegativeOrZeroInt: TableFor1[Int] =
    Table("argument", 1, 2, Int.MaxValue)

  /** Longs that are negative or zero, with the message reporting the value found. */
  val invalidNotNegativeOrZeroLong: TableFor2[Long, String] =
    Table(
      ("argument", "expectedMessage"),
      (0L, "Argument 'name' must not be negative or zero but has value 0"),
      (-1L, "Argument 'name' must not be negative or zero but has value -1"),
      (Long.MinValue, "Argument 'name' must not be negative or zero but has value -9223372036854775808"))

  /** Longs greater than zero. */
  val validNotNegativeOrZeroLong: TableFor1[Long] =
    Table("argument", 1L, 2L, Long.MaxValue)

  /**
   * Doubles that are negative or zero, with the message reporting the value found.
   *
   * Both signed zeros fail, because they compare equal to zero, and the message distinguishes
   * them by rendering the sign.
   */
  val invalidNotNegativeOrZeroDouble: TableFor2[Double, String] =
    Table(
      ("argument", "expectedMessage"),
      (0.0, "Argument 'name' must not be negative or zero but has value 0.0"),
      (-0.0, "Argument 'name' must not be negative or zero but has value -0.0"),
      (-1.0, "Argument 'name' must not be negative or zero but has value -1.0"),
      (Double.NegativeInfinity, "Argument 'name' must not be negative or zero but has value -Infinity"))

  /** Doubles greater than zero, and the not-a-number value that does not order against it. */
  val validNotNegativeOrZeroDouble: TableFor1[Double] =
    Table("argument", 1.0, 0.1, Double.PositiveInfinity, Double.NaN)

  /** Decimals that are negative or zero, with the message reporting the value found. */
  val invalidNotNegativeOrZeroDecimal: TableFor2[Decimal, String] =
    Table(
      ("argument", "expectedMessage"),
      (DecimalZero, "Argument 'name' must not be negative or zero but has value 0"),
      (DecimalMinusOne, "Argument 'name' must not be negative or zero but has value -1"),
      (DecimalNegative, "Argument 'name' must not be negative or zero but has value -1.2"))

  /** Decimals greater than zero. */
  val validNotNegativeOrZeroDecimal: TableFor1[Decimal] =
    Table("argument", DecimalOne, DecimalPositive)

  /**
   * Arguments and tolerances the tolerant check rejects, with the message reporting which.
   *
   * The check reports four distinct failures, all of which appear here: a tolerance that is
   * not a number, a tolerance that is negative - neither of which describes an interval - an
   * argument the tolerance counts as zero, and an argument clearly below zero. The two
   * tolerance rows pair their unusable tolerance with an argument the check would otherwise
   * accept, so the row proves which of the two checks reported.
   */
  val invalidNotNegativeOrZeroWithTolerance: TableFor3[Double, Double, String] =
    Table(
      ("argument", "tolerance", "expectedMessage"),
      (0.0, 0.0001, "Argument 'name' must not be zero"),
      (0.0, 0.0, "Argument 'name' must not be zero"),
      (0.0000001, 0.0001, "Argument 'name' must not be zero"),
      (-0.00005, 0.0001, "Argument 'name' must not be zero"),
      (0.0001, 0.0001, "Argument 'name' must not be zero"),
      (-1.0, 0.0001, "Argument 'name' must be greater than zero but has value -1.0"),
      (-2.5, 0.0001, "Argument 'name' must be greater than zero but has value -2.5"),
      (1.0, -0.1, "Argument 'tolerance' must not be negative but has value -0.1"),
      (1.0, Double.NaN, "Argument 'tolerance' must not be NaN"))

  /** Arguments the tolerant check accepts, paired with the tolerance it accepted them at. */
  val validNotNegativeOrZeroWithTolerance: TableFor2[Double, Double] =
    Table(
      ("argument", "tolerance"),
      (1.0, 0.0001),
      (0.1, 0.0001),
      (0.001, 0.0001),
      (1.0, 0.0),
      (Double.PositiveInfinity, 0.0001),
      (Double.NaN, 0.0001))

  //-------------------------------------------------------------------------
  /** Both signed zeros, with the message reporting the argument zero. */
  val invalidNotZero: TableFor2[Double, String] =
    Table(
      ("argument", "expectedMessage"),
      (0.0, "Argument 'name' must not be zero"),
      (-0.0, "Argument 'name' must not be zero"))

  /** Doubles that differ from zero, however slightly. */
  val validNotZero: TableFor1[Double] =
    Table(
      "argument",
      1.0,
      -1.0,
      1.0E-300,
      Double.PositiveInfinity,
      Double.NegativeInfinity,
      Double.NaN)

  /**
   * Arguments and tolerances the tolerant check rejects, with the message reporting which.
   *
   * Unlike the check for a value above zero, this one has no interest in the sign: an
   * argument within the tolerance of zero in either direction is zero to it. It rejects the
   * same two unusable tolerances as that check, in the same wording, so a row for each
   * appears here as well.
   */
  val invalidNotZeroWithTolerance: TableFor3[Double, Double, String] =
    Table(
      ("argument", "tolerance", "expectedMessage"),
      (0.0, 0.1, "Argument 'name' must not be zero"),
      (-0.0, 0.1, "Argument 'name' must not be zero"),
      (0.0, 0.0, "Argument 'name' must not be zero"),
      (0.05, 0.1, "Argument 'name' must not be zero"),
      (-0.05, 0.1, "Argument 'name' must not be zero"),
      (0.1, 0.1, "Argument 'name' must not be zero"),
      (1.0, -0.1, "Argument 'tolerance' must not be negative but has value -0.1"),
      (1.0, Double.NaN, "Argument 'tolerance' must not be NaN"))

  /** Arguments the tolerant check accepts, paired with the tolerance it accepted them at. */
  val validNotZeroWithTolerance: TableFor2[Double, Double] =
    Table(
      ("argument", "tolerance"),
      (1.0, 0.1),
      (-1.0, 0.1),
      (0.2, 0.1),
      (-0.2, 0.1),
      (1.0, 0.0),
      (Double.PositiveInfinity, 0.1),
      (Double.NaN, 0.1))

  //-------------------------------------------------------------------------
  // The nine range checks.
  //
  // Each of the three interval shapes is covered over doubles, over ints and over a type
  // ordered by its own ordering, and the message of each shape names its bounds with the
  // comparisons that describe it, so the rows of a shape read as the interval they reject.

  /** Doubles outside `low <= x < high`, with the message reporting the interval. */
  val invalidInRangeDouble: TableFor4[Double, Double, Double, String] =
    Table(
      ("argument", "lowInclusive", "highExclusive", "expectedMessage"),
      (-1.0, 0.0, 1.0, "Expected 0.0 <= 'name' < 1.0, but found -1.0"),
      (1.0, 0.0, 1.0, "Expected 0.0 <= 'name' < 1.0, but found 1.0"),
      (2.0, 0.0, 1.0, "Expected 0.0 <= 'name' < 1.0, but found 2.0"))

  /** Doubles inside `low <= x < high`, the low bound included. */
  val validInRangeDouble: TableFor3[Double, Double, Double] =
    Table(
      ("argument", "lowInclusive", "highExclusive"),
      (0.5, 0.0, 1.0),
      (0.0, 0.0, 1.0),
      (0.99999999999, 0.0, 1.0))

  /** Doubles outside `low <= x <= high`, with the message reporting the interval. */
  val invalidInRangeInclusiveDouble: TableFor4[Double, Double, Double, String] =
    Table(
      ("argument", "lowInclusive", "highInclusive", "expectedMessage"),
      (-1.0, 0.0, 1.0, "Expected 0.0 <= 'name' <= 1.0, but found -1.0"),
      (2.0, 0.0, 1.0, "Expected 0.0 <= 'name' <= 1.0, but found 2.0"))

  /** Doubles inside `low <= x <= high`, both bounds included. */
  val validInRangeInclusiveDouble: TableFor3[Double, Double, Double] =
    Table(
      ("argument", "lowInclusive", "highInclusive"),
      (0.5, 0.0, 1.0),
      (0.0, 0.0, 1.0),
      (1.0, 0.0, 1.0))

  /** Doubles outside `low < x < high`, with the message reporting the interval. */
  val invalidInRangeExclusiveDouble: TableFor4[Double, Double, Double, String] =
    Table(
      ("argument", "lowExclusive", "highExclusive", "expectedMessage"),
      (-1.0, 0.0, 1.0, "Expected 0.0 < 'name' < 1.0, but found -1.0"),
      (0.0, 0.0, 1.0, "Expected 0.0 < 'name' < 1.0, but found 0.0"),
      (1.0, 0.0, 1.0, "Expected 0.0 < 'name' < 1.0, but found 1.0"),
      (2.0, 0.0, 1.0, "Expected 0.0 < 'name' < 1.0, but found 2.0"))

  /** Doubles strictly inside `low < x < high`, neither bound included. */
  val validInRangeExclusiveDouble: TableFor3[Double, Double, Double] =
    Table(
      ("argument", "lowExclusive", "highExclusive"),
      (0.5, 0.0, 1.0),
      (0.00000000001, 0.0, 1.0),
      (0.99999999999, 0.0, 1.0))

  /** Ints outside `low <= x < high`, with the message reporting the interval. */
  val invalidInRangeInt: TableFor4[Int, Int, Int, String] =
    Table(
      ("argument", "lowInclusive", "highExclusive", "expectedMessage"),
      (-1, 0, 2, "Expected 0 <= 'name' < 2, but found -1"),
      (2, 0, 2, "Expected 0 <= 'name' < 2, but found 2"),
      (3, 0, 2, "Expected 0 <= 'name' < 2, but found 3"))

  /** Ints inside `low <= x < high`, which is the shape an index is checked with. */
  val validInRangeInt: TableFor3[Int, Int, Int] =
    Table(
      ("argument", "lowInclusive", "highExclusive"),
      (1, 0, 2),
      (0, 0, 2),
      (0, 0, 1))

  /** Ints outside `low <= x <= high`, with the message reporting the interval. */
  val invalidInRangeInclusiveInt: TableFor4[Int, Int, Int, String] =
    Table(
      ("argument", "lowInclusive", "highInclusive", "expectedMessage"),
      (-1, 0, 2, "Expected 0 <= 'name' <= 2, but found -1"),
      (3, 0, 2, "Expected 0 <= 'name' <= 2, but found 3"))

  /** Ints inside `low <= x <= high`, both bounds included. */
  val validInRangeInclusiveInt: TableFor3[Int, Int, Int] =
    Table(
      ("argument", "lowInclusive", "highInclusive"),
      (1, 0, 2),
      (0, 0, 2),
      (2, 0, 2))

  /** Ints outside `low < x < high`, with the message reporting the interval. */
  val invalidInRangeExclusiveInt: TableFor4[Int, Int, Int, String] =
    Table(
      ("argument", "lowExclusive", "highExclusive", "expectedMessage"),
      (-1, 0, 2, "Expected 0 < 'name' < 2, but found -1"),
      (0, 0, 2, "Expected 0 < 'name' < 2, but found 0"),
      (2, 0, 2, "Expected 0 < 'name' < 2, but found 2"))

  /** Ints strictly inside `low < x < high`, neither bound included. */
  val validInRangeExclusiveInt: TableFor3[Int, Int, Int] =
    Table(
      ("argument", "lowExclusive", "highExclusive"),
      (1, 0, 2),
      (2, 0, 3),
      (-1, -2, 0))

  /** Durations outside `low <= x < high`, with the message reporting the interval. */
  val invalidInRangeComparable: TableFor4[Duration, Duration, Duration, String] =
    Table(
      ("argument", "lowInclusive", "highExclusive", "expectedMessage"),
      (Duration.ofSeconds(-1), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S <= 'name' < PT2S, but found PT-1S"),
      (Duration.ofSeconds(2), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S <= 'name' < PT2S, but found PT2S"),
      (Duration.ofSeconds(3), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S <= 'name' < PT2S, but found PT3S"))

  /** Durations inside `low <= x < high`, the low bound included. */
  val validInRangeComparable: TableFor3[Duration, Duration, Duration] =
    Table(
      ("argument", "lowInclusive", "highExclusive"),
      (Duration.ofSeconds(1), Duration.ZERO, Duration.ofSeconds(2)),
      (Duration.ZERO, Duration.ZERO, Duration.ofSeconds(2)),
      (Duration.ofMillis(1999), Duration.ZERO, Duration.ofSeconds(2)))

  /** Durations outside `low <= x <= high`, with the message reporting the interval. */
  val invalidInRangeComparableInclusive: TableFor4[Duration, Duration, Duration, String] =
    Table(
      ("argument", "lowInclusive", "highInclusive", "expectedMessage"),
      (Duration.ofSeconds(-1), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S <= 'name' <= PT2S, but found PT-1S"),
      (Duration.ofSeconds(3), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S <= 'name' <= PT2S, but found PT3S"))

  /** Durations inside `low <= x <= high`, both bounds included. */
  val validInRangeComparableInclusive: TableFor3[Duration, Duration, Duration] =
    Table(
      ("argument", "lowInclusive", "highInclusive"),
      (Duration.ofSeconds(1), Duration.ZERO, Duration.ofSeconds(2)),
      (Duration.ZERO, Duration.ZERO, Duration.ofSeconds(2)),
      (Duration.ofSeconds(2), Duration.ZERO, Duration.ofSeconds(2)))

  /** Durations outside `low < x < high`, with the message reporting the interval. */
  val invalidInRangeComparableExclusive: TableFor4[Duration, Duration, Duration, String] =
    Table(
      ("argument", "lowExclusive", "highExclusive", "expectedMessage"),
      (Duration.ofSeconds(-1), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S < 'name' < PT2S, but found PT-1S"),
      (Duration.ZERO, Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S < 'name' < PT2S, but found PT0S"),
      (Duration.ofSeconds(2), Duration.ZERO, Duration.ofSeconds(2),
          "Expected PT0S < 'name' < PT2S, but found PT2S"))

  /** Durations strictly inside `low < x < high`, neither bound included. */
  val validInRangeComparableExclusive: TableFor3[Duration, Duration, Duration] =
    Table(
      ("argument", "lowExclusive", "highExclusive"),
      (Duration.ofSeconds(1), Duration.ZERO, Duration.ofSeconds(2)),
      (Duration.ofMillis(1), Duration.ZERO, Duration.ofSeconds(2)),
      (Duration.ofMillis(1999), Duration.ZERO, Duration.ofSeconds(2)))

  //-------------------------------------------------------------------------
  /**
   * Pairs of dates that are equal or out of order, with the message reporting the breach.
   *
   * Equal values fail this check, which is what distinguishes it from the one that allows
   * them, so both ways of failing appear here.
   */
  val invalidInOrderNotEqual: TableFor3[LocalDate, LocalDate, String] =
    Table(
      ("first", "second", "expectedMessage"),
      (LocalDate.of(2011, 7, 3), LocalDate.of(2011, 7, 2),
          "Invalid order: Expected 'a' < 'b', but found: '2011-07-03' >= '2011-07-02'"),
      (LocalDate.of(2011, 7, 3), LocalDate.of(2011, 7, 3),
          "Invalid order: Expected 'a' < 'b', but found: '2011-07-03' >= '2011-07-03'"),
      (LocalDate.of(2012, 1, 1), LocalDate.of(2011, 12, 31),
          "Invalid order: Expected 'a' < 'b', but found: '2012-01-01' >= '2011-12-31'"))

  /** Pairs of dates where the first orders strictly before the second. */
  val validInOrderNotEqual: TableFor2[LocalDate, LocalDate] =
    Table(
      ("first", "second"),
      (LocalDate.of(2011, 7, 2), LocalDate.of(2011, 7, 3)),
      (LocalDate.of(2011, 12, 31), LocalDate.of(2012, 1, 1)),
      (LocalDate.of(2011, 7, 2), LocalDate.of(2099, 7, 2)))

  /** Pairs of dates that are out of order, with the message reporting the breach. */
  val invalidInOrderOrEqual: TableFor3[LocalDate, LocalDate, String] =
    Table(
      ("first", "second", "expectedMessage"),
      (LocalDate.of(2011, 7, 3), LocalDate.of(2011, 7, 2),
          "Invalid order: Expected 'a' <= 'b', but found: '2011-07-03' > '2011-07-02'"),
      (LocalDate.of(2012, 1, 1), LocalDate.of(2011, 12, 31),
          "Invalid order: Expected 'a' <= 'b', but found: '2012-01-01' > '2011-12-31'"))

  /** Pairs of dates where the first orders before the second or equals it. */
  val validInOrderOrEqual: TableFor2[LocalDate, LocalDate] =
    Table(
      ("first", "second"),
      (LocalDate.of(2011, 7, 2), LocalDate.of(2011, 7, 3)),
      (LocalDate.of(2011, 7, 2), LocalDate.of(2011, 7, 2)),
      (LocalDate.of(2011, 7, 3), LocalDate.of(2011, 7, 3)))
}

/**
 * Tests [[ArgCheck]], the fail-fast half of the validation vocabulary of this module.
 *
 * ===What is under test===
 *
 * Every member of [[ArgCheck]]: the two boolean checks, the two forms of `matches`, the
 * blankness check, the eight `notEmpty` overloads, the two duplicate checks, the four sign
 * families over int, long, double and decimal, the not-a-number check, the two
 * tolerance-bearing checks, the nine range checks and the two order checks.
 *
 * The source of the case inventory is the test class of the Java original, whose 134 methods
 * are each answered here. Of those, 116 exercise a check that was ported and are reproduced
 * case for case; the remaining 18 exercise the checks for an absent reference - the two
 * present-reference checks, the thirteen no-absent-element checks and the reflective proof
 * that the original was an uninstantiable holder of static methods - which have no target,
 * because absence is described by `Option` here and a reference is never empty of a referent.
 * Those 18 are answered rather than dropped: each has a test that proves the member does not
 * exist, with the expression that would have called it rejected at compile time, and that
 * exercises the expression which replaces it at a call site. That keeps the decision recorded
 * and, unlike a comment, keeps it true.
 *
 * ===Where each group of the original's cases went===
 *
 * The sections below run in the order of the original, so a case there is found from the name
 * of the check it exercised:
 *
 *   - `isTrue` (10 cases) and `isFalse` (4) reach the two boolean sections, and the four
 *     messages the original asserted exactly are asserted exactly here;
 *   - `notNull` (2), `notNullItem` (2), `noNulls` (13) and `validUtilityClass` (1) reach the
 *     section of checks that have no target;
 *   - `matches` (11) reaches the two `matches` sections, the character form of the original
 *     having taken a character matcher where this one takes a predicate over characters;
 *   - `notBlank` (5) and `notEmpty` (29, plus one case the original named for an array of longs
 *     while passing an array of doubles) reach the blankness section and the five `notEmpty`
 *     sections;
 *   - `notPositive` (7, one of them under a misspelling in the original),
 *     `notPositiveIfPresent` (2), `notNegative` (7), `notNaN` (2), `notNegativeOrZero` (13) and
 *     `notZero` (6) reach the sign sections;
 *   - the range cases over doubles (2), over ints (2) and over an ordered type (2) reach the
 *     three range sections, where each case of the original that asserted several intervals at
 *     once becomes one test per interval shape;
 *   - the duplicate cases (7) and the order cases (5) reach the last two sections.
 *
 * The sections end with the table-driven tests and, last of all, with properties over inputs
 * that no table can enumerate.
 *
 * ===How a passing case is written===
 *
 * The Java original returned the argument it had checked, so a passing case there asserted
 * the returned value. Every check here returns `Unit` - returning the argument would force
 * each call into a binding that exists only to be discarded, which this build treats as an
 * error - so a passing case asserts instead that no error is thrown, and, where the original
 * asserted the returned value, that the argument itself is unchanged and usable afterwards.
 *
 * ===How a failing case is written===
 *
 * [[ArgCheck]] is the one place in either module where a throw is written, and it throws
 * `IllegalArgumentException`, so that is the error every failing case here expects. The Java
 * original matched most of its messages by pattern and only four of them exactly. This spec
 * asserts the '''complete''' message text of every failing case, which is strictly stronger
 * and is what lets the same expectations be shared with the spec for `Validate`: the two
 * objects answer differently but word their failures identically.
 *
 * ===What is shared with the spec for `Validate`===
 *
 * The inputs and messages live in [[ArgCheckTables]], at the top of this file, and both specs
 * iterate them. Each predicate section below therefore ends with two table-driven tests - one
 * over the invalid rows and one over the valid rows - which are what prove the shared fixture
 * describes this object correctly. See the documentation of [[ArgCheckTables]] before changing
 * a table: the names are resolved from more than one spec.
 */
class ArgCheckSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /**
   * The argument name every check in this spec is called under.
   *
   * The expected message text of every row of [[ArgCheckTables]] names this argument, so the
   * calls and the expectations are tied to one another through this value rather than through
   * a literal repeated in both places.
   */
  private val Name: String = ArgCheckTables.ArgumentName

  /**
   * Runs a check that is expected to fail and reads the message it reported.
   *
   * The test fails if the check returns normally, or if it throws anything other than the
   * error [[ArgCheck]] documents, so the type of the error is asserted by every failing case
   * that uses this.
   *
   * @param check  the check expected to fail
   * @return the message of the error it threw
   */
  private def messageOf(check: => Unit): String =
    intercept[IllegalArgumentException](check).getMessage

  //-------------------------------------------------------------------------
  // The published fixture.

  test("the shared tables name the argument that their expected messages quote") {
    ArgCheckTables.ArgumentName shouldBe "name"
    ArgCheckTables.FirstName shouldBe "a"
    ArgCheckTables.SecondName shouldBe "b"
    ArgCheckTables.ToleranceName shouldBe "tolerance"
  }

  test("the decimals of the shared tables render as their expected messages quote them") {
    ArgCheckTables.DecimalZero.toString shouldBe "0"
    ArgCheckTables.DecimalOne.toString shouldBe "1"
    ArgCheckTables.DecimalMinusOne.toString shouldBe "-1"
    ArgCheckTables.DecimalPositive.toString shouldBe "1.2"
    ArgCheckTables.DecimalNegative.toString shouldBe "-1.2"
  }

  test("the matrices of the shared tables report the dimensions and sizes they stand for") {
    ArgCheckTables.EmptyMatrix.size shouldBe 0
    ArgCheckTables.EmptyMatrix.dimensions shouldBe 1
    ArgCheckTables.SingletonMatrix.size shouldBe 1
    ArgCheckTables.EmptyTwoDimensionalMatrix.size shouldBe 0
    ArgCheckTables.EmptyTwoDimensionalMatrix.dimensions shouldBe 2
    ArgCheckTables.PopulatedTwoDimensionalMatrix.size shouldBe 6
    ArgCheckTables.PopulatedTwoDimensionalMatrix.dimensions shouldBe 2
  }

  test("the character predicate of the shared tables admits an upper-case letter and nothing else") {
    ArgCheckTables.UpperCaseLetter('A') shouldBe true
    ArgCheckTables.UpperCaseLetter('Z') shouldBe true
    ArgCheckTables.UpperCaseLetter('a') shouldBe false
    ArgCheckTables.UpperCaseLetter('1') shouldBe false
    ArgCheckTables.UpperCaseLetter(' ') shouldBe false
  }

  test("the orderings of the shared tables order as the types they cover compare") {
    localDateOrder.compare(LocalDate.of(2011, 7, 2), LocalDate.of(2011, 7, 3)) should be < 0
    localDateOrder.compare(LocalDate.of(2011, 7, 3), LocalDate.of(2011, 7, 3)) shouldBe 0
    localDateOrder.compare(LocalDate.of(2011, 7, 4), LocalDate.of(2011, 7, 3)) should be > 0
    durationOrder.compare(Duration.ZERO, Duration.ofSeconds(1)) should be < 0
    durationOrder.compare(Duration.ofSeconds(1), Duration.ofSeconds(1)) shouldBe 0
    durationOrder.compare(Duration.ofSeconds(2), Duration.ofSeconds(1)) should be > 0
  }

  //-------------------------------------------------------------------------
  // isTrue.

  test("isTrue without a message returns quietly for a true expression") {
    noException should be thrownBy ArgCheck.isTrue(true)
  }

  test("isTrue without a message reports a false expression with the standard wording") {
    messageOf(ArgCheck.isTrue(false)) shouldBe "Invalid argument, expression must be true"
  }

  test("isTrue with a message returns quietly for a true expression") {
    noException should be thrownBy ArgCheck.isTrue(true, "Message")
  }

  test("isTrue with a message reports that message and nothing else") {
    messageOf(ArgCheck.isTrue(false, "Message")) shouldBe "Message"
  }

  test("isTrue with an interpolated message returns quietly for a true expression") {
    val text = "A"
    val count = 2
    val amount = 3.0
    noException should be thrownBy ArgCheck.isTrue(true, s"Message $text $count $amount")
  }

  test("isTrue reports a message interpolating three values of different types") {
    val text = "A"
    val count = 2
    val amount = 3.0
    messageOf(ArgCheck.isTrue(false, s"Message $text $count $amount")) shouldBe "Message A 2 3.0"
  }

  test("isTrue with a message interpolating a long returns quietly for a true expression") {
    val days = 3L
    noException should be thrownBy ArgCheck.isTrue(true, s"Message $days")
  }

  test("isTrue reports a message interpolating a long as that long") {
    val days = 3L
    messageOf(ArgCheck.isTrue(false, s"Message $days")) shouldBe "Message 3"
  }

  test("isTrue with a message interpolating a double returns quietly for a true expression") {
    val amount = 3.0
    noException should be thrownBy ArgCheck.isTrue(true, s"Message $amount")
  }

  test("isTrue reports a message interpolating a double as that double") {
    val amount = 3.0
    messageOf(ArgCheck.isTrue(false, s"Message $amount")) shouldBe "Message 3.0"
  }

  test("isTrue builds its message only on the failing path") {
    noException should be thrownBy {
      ArgCheck.isTrue(true, throw new IllegalStateException("the message was built"))
    }
    an[IllegalStateException] should be thrownBy {
      ArgCheck.isTrue(false, throw new IllegalStateException("the message was built"))
    }
  }

  //-------------------------------------------------------------------------
  // isFalse.

  test("isFalse returns quietly for a false expression") {
    noException should be thrownBy ArgCheck.isFalse(false, "Message")
  }

  test("isFalse reports its message and nothing else for a true expression") {
    messageOf(ArgCheck.isFalse(true, "Message")) shouldBe "Message"
  }

  test("isFalse with an interpolated message returns quietly for a false expression") {
    val text = "A"
    val count = 2
    val amount = 3.0
    noException should be thrownBy ArgCheck.isFalse(false, s"Message $text $count $amount")
  }

  test("isFalse reports a message interpolating three values of different types") {
    val text = "A"
    val count = 2
    val amount = 3.0
    messageOf(ArgCheck.isFalse(true, s"Message $text $count $amount")) shouldBe "Message A 2 3.0"
  }

  test("isFalse builds its message only on the failing path") {
    noException should be thrownBy {
      ArgCheck.isFalse(false, throw new IllegalStateException("the message was built"))
    }
    an[IllegalStateException] should be thrownBy {
      ArgCheck.isFalse(true, throw new IllegalStateException("the message was built"))
    }
  }

  test("isFalse has no form without a message, as in the original") {
    assertDoesNotCompile("ArgCheck.isFalse(false)")
  }

  //-------------------------------------------------------------------------
  // The checks for an absent reference, which have no target.

  test("the check for a present reference has no target, because a reference is never absent") {
    assertDoesNotCompile("""ArgCheck.notNull("OG", "name")""")
    val present: Option[String] = Some("OG")
    noException should be thrownBy present.foreach(value => ArgCheck.notEmpty(value, Name))
    present shouldBe Some("OG")
  }

  test("an absent value is an empty Option, and the check it guards never runs") {
    val absent: Option[String] = None
    noException should be thrownBy absent.foreach(value => ArgCheck.notEmpty(value, Name))
    absent shouldBe None
  }

  test("the item form of the present-reference check has no target either") {
    assertDoesNotCompile("""ArgCheck.notNullItem("OG")""")
    assertDoesNotCompile("""ArgCheck.notNullItem("OG", "name")""")
  }

  test("an absent item is an empty Option and reaches no check") {
    val items: List[Option[String]] = List(Some("A"), None, Some("B"))
    items.flatten shouldBe List("A", "B")
    noException should be thrownBy ArgCheck.notEmpty(items.flatten, Name)
  }

  test("the array form of the check for absent elements has no target") {
    assertDoesNotCompile("""ArgCheck.noNulls(Array("Element"), "name")""")
  }

  test("an empty array of optional elements narrows to no elements and needs no absence check") {
    val elements: Array[Option[String]] = Array.empty
    elements.flatten.toList shouldBe List.empty[String]
    val message = messageOf(ArgCheck.notEmpty(elements.flatten, Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("an absent array is an empty Option, so only a present array is checked") {
    val absent: Option[Array[String]] = None
    val present: Option[Array[String]] = Some(Array.empty[String])
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(values => ArgCheck.notEmpty(values, Name))
    }
  }

  test("an array of optional elements is narrowed to its present elements rather than checked") {
    val elements: Array[Option[String]] = Array(Some("Element"), None)
    elements.flatten.toList shouldBe List("Element")
    noException should be thrownBy ArgCheck.notEmpty(elements.flatten, Name)
  }

  test("the iterable form of the check for absent elements has no target") {
    assertDoesNotCompile("""ArgCheck.noNulls(List("Element"), "name")""")
  }

  test("an empty iterable of optional elements narrows to no elements") {
    val elements: List[Option[String]] = List.empty
    elements.flatten shouldBe List.empty[String]
    val message = messageOf(ArgCheck.notEmpty(elements.flatten, Name))
    message shouldBe "Argument iterable 'name' must not be empty"
  }

  test("an absent iterable is an empty Option, so only a present iterable is checked") {
    val absent: Option[List[String]] = None
    val present: Option[List[String]] = Some(List.empty[String])
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(values => ArgCheck.notEmpty(values, Name))
    }
  }

  test("an iterable of optional elements is narrowed to its present elements rather than checked") {
    val elements: List[Option[String]] = List(Some("Element"), None)
    elements.flatten shouldBe List("Element")
    noException should be thrownBy ArgCheck.notEmpty(elements.flatten, Name)
  }

  test("the map form of the check for absent entries has no target") {
    assertDoesNotCompile("""ArgCheck.noNulls(Map("A" -> "B"), "name")""")
  }

  test("an empty map of optional values narrows to no entries") {
    val entries: Map[String, Option[String]] = Map.empty
    val present = entries.collect { case (mapKey, Some(mapValue)) => mapKey -> mapValue }
    present shouldBe Map.empty[String, String]
    messageOf(ArgCheck.notEmpty(present, Name)) shouldBe "Argument map 'name' must not be empty"
  }

  test("an absent map is an empty Option, so only a present map is checked") {
    val absent: Option[Map[String, String]] = None
    val present: Option[Map[String, String]] = Some(Map.empty[String, String])
    noException should be thrownBy absent.foreach(entries => ArgCheck.notEmpty(entries, Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(entries => ArgCheck.notEmpty(entries, Name))
    }
  }

  test("a map built from optional keys keeps only the entries whose key is present") {
    val entries: List[(Option[String], String)] = List((Some("A"), "B"), (None, "Z"))
    val present = entries.collect { case (Some(mapKey), mapValue) => mapKey -> mapValue }.toMap
    present shouldBe Map("A" -> "B")
    noException should be thrownBy ArgCheck.notEmpty(present, Name)
  }

  test("a map built from optional values keeps only the entries whose value is present") {
    val entries: Map[String, Option[String]] = Map("A" -> Some("B"), "Z" -> None)
    val present = entries.collect { case (mapKey, Some(mapValue)) => mapKey -> mapValue }
    present shouldBe Map("A" -> "B")
    noException should be thrownBy ArgCheck.notEmpty(present, Name)
  }

  test("ArgCheck is a singleton object, so there is neither a constructor nor a subclass of it") {
    assertDoesNotCompile("new ArgCheck")
    assertDoesNotCompile("class Extended extends ArgCheck")
    noException should be thrownBy ArgCheck.isTrue(true)
  }


  //-------------------------------------------------------------------------
  // matches, against a pattern.

  test("matches accepts an argument the pattern matches in full") {
    noException should be thrownBy ArgCheck.matches("[A-Z]+".r, "OG", Name)
  }

  test("matches requires the whole argument to match, not a part of it") {
    val message = messageOf(ArgCheck.matches("[A-Z]+".r, "OG1", Name))
    message shouldBe "Argument 'name' with value 'OG1' must match pattern: [A-Z]+"
  }

  test("an absent pattern is an empty Option, so only a present pattern is matched against") {
    val absent: Option[Regex] = None
    val present: Option[Regex] = Some("[A-Z]+".r)
    noException should be thrownBy absent.foreach(pattern => ArgCheck.matches(pattern, "123", Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(pattern => ArgCheck.matches(pattern, "123", Name))
    }
  }

  test("an absent argument is an empty Option, so only a present argument is matched") {
    val absent: Option[String] = None
    val present: Option[String] = Some("123")
    noException should be thrownBy {
      absent.foreach(argument => ArgCheck.matches("[A-Z]+".r, argument, Name))
    }
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(argument => ArgCheck.matches("[A-Z]+".r, argument, Name))
    }
  }

  test("matches rejects empty text against a pattern that requires a character") {
    val message = messageOf(ArgCheck.matches("[A-Z]+".r, "", Name))
    message shouldBe "Argument 'name' with value '' must match pattern: [A-Z]+"
  }

  test("matches names the argument, its value and the pattern when the two disagree") {
    val message = messageOf(ArgCheck.matches("[A-Z]+".r, "123", Name))
    message shouldBe "Argument 'name' with value '123' must match pattern: [A-Z]+"
  }

  test("matches rejects every mismatching row of the shared table with the message it names") {
    forAll(ArgCheckTables.invalidMatchesRegex) { (pattern, argument, expectedMessage) =>
      messageOf(ArgCheck.matches(pattern, argument, Name)) shouldBe expectedMessage
    }
  }

  test("matches accepts every matching row of the shared table") {
    forAll(ArgCheckTables.validMatchesRegex) { (pattern, argument) =>
      noException should be thrownBy ArgCheck.matches(pattern, argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // matches, against a character predicate and a length range.

  test("the character form accepts an argument of allowed length that the predicate admits") {
    noException should be thrownBy {
      ArgCheck.matches(ArgCheckTables.UpperCaseLetter, 1, Int.MaxValue, "OG", Name, "[A-Z]+")
    }
  }

  test("the character form rejects an argument shorter than the minimum length") {
    val message =
      messageOf(ArgCheck.matches(ArgCheckTables.UpperCaseLetter, 1, 2, "", Name, "[A-Z]{1,2}"))
    message shouldBe "Argument 'name' with value '' must match pattern: [A-Z]{1,2}"
  }

  test("the character form rejects an argument longer than the maximum length") {
    val message =
      messageOf(ArgCheck.matches(ArgCheckTables.UpperCaseLetter, 1, 2, "ABC", Name, "[A-Z]{1,2}"))
    message shouldBe "Argument 'name' with value 'ABC' must match pattern: [A-Z]{1,2}"
  }

  test("an absent predicate is an empty Option, so only a present predicate is applied") {
    val absent: Option[Char => Boolean] = None
    val present: Option[Char => Boolean] = Some(ArgCheckTables.UpperCaseLetter)
    noException should be thrownBy {
      absent.foreach(matcher => ArgCheck.matches(matcher, 1, 2, "123", Name, "[A-Z]{1,2}"))
    }
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(matcher => ArgCheck.matches(matcher, 1, 2, "123", Name, "[A-Z]{1,2}"))
    }
  }

  test("an absent argument reaches neither form of matches") {
    val absent: Option[String] = None
    noException should be thrownBy {
      absent.foreach(argument =>
        ArgCheck.matches(ArgCheckTables.UpperCaseLetter, 1, 2, argument, Name, "[A-Z]{1,2}"))
    }
    noException should be thrownBy {
      absent.foreach(argument => ArgCheck.matches("[A-Z]+".r, argument, Name))
    }
  }

  test("the character form names the argument and its value when a character is rejected") {
    val message = messageOf(
      ArgCheck.matches(ArgCheckTables.UpperCaseLetter, 1, Int.MaxValue, "123", Name, "[A-Z]+"))
    message shouldBe "Argument 'name' with value '123' must match pattern: [A-Z]+"
  }

  test("the character form quotes the readable pattern it was handed, not the predicate") {
    val message = messageOf(
      ArgCheck.matches(ArgCheckTables.UpperCaseLetter, 1, 3, "og", Name, "an upper-case letter"))
    message shouldBe "Argument 'name' with value 'og' must match pattern: an upper-case letter"
  }

  test("the character form rejects every row of the shared table with the message it names") {
    forAll(ArgCheckTables.invalidMatchesPredicate) {
      (matcher, minLength, maxLength, argument, equivalentRegex, expectedMessage) =>
        def check(): Unit =
          ArgCheck.matches(matcher, minLength, maxLength, argument, Name, equivalentRegex)
        messageOf(check()) shouldBe expectedMessage
    }
  }

  test("the character form accepts every row of the shared table of admitted arguments") {
    forAll(ArgCheckTables.validMatchesPredicate) {
      (matcher, minLength, maxLength, argument, equivalentRegex) =>
        noException should be thrownBy {
          ArgCheck.matches(matcher, minLength, maxLength, argument, Name, equivalentRegex)
        }
    }
  }

  //-------------------------------------------------------------------------
  // notBlank.

  test("notBlank accepts text holding a character that is not whitespace") {
    noException should be thrownBy ArgCheck.notBlank("OG", Name)
  }

  test("notBlank accepts text that is not trimmed, and leaves it as it was") {
    val argument = " OG "
    noException should be thrownBy ArgCheck.notBlank(argument, Name)
    argument shouldBe " OG "
  }

  test("absent text reaches no blankness check") {
    val absent: Option[String] = None
    val present: Option[String] = Some("  ")
    noException should be thrownBy absent.foreach(value => ArgCheck.notBlank(value, Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(value => ArgCheck.notBlank(value, Name))
    }
  }

  test("notBlank rejects empty text") {
    messageOf(ArgCheck.notBlank("", Name)) shouldBe "Argument 'name' must not be blank"
  }

  test("notBlank rejects text that is nothing but spaces") {
    messageOf(ArgCheck.notBlank("  ", Name)) shouldBe "Argument 'name' must not be blank"
  }

  test("notBlank rejects every blank row of the shared table with the message it names") {
    forAll(ArgCheckTables.invalidNotBlank) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notBlank(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notBlank accepts every row of the shared table of text that is not blank") {
    forAll(ArgCheckTables.validNotBlank) { argument =>
      noException should be thrownBy ArgCheck.notBlank(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over text.

  test("notEmpty accepts text holding a character, whitespace included") {
    noException should be thrownBy ArgCheck.notEmpty("OG", Name)
    noException should be thrownBy ArgCheck.notEmpty(" ", Name)
  }

  test("absent text reaches no emptiness check") {
    val absent: Option[String] = None
    val present: Option[String] = Some("")
    noException should be thrownBy absent.foreach(value => ArgCheck.notEmpty(value, Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(value => ArgCheck.notEmpty(value, Name))
    }
  }

  test("notEmpty rejects empty text, naming neither an array nor an iterable") {
    messageOf(ArgCheck.notEmpty("", Name)) shouldBe "Argument 'name' must not be empty"
  }

  test("notEmpty rejects every empty row of the shared table of text") {
    forAll(ArgCheckTables.invalidNotEmptyString) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notEmpty accepts every row of the shared table of text that is not empty") {
    forAll(ArgCheckTables.validNotEmptyString) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over a matrix.

  test("notEmpty accepts a matrix holding an element, and leaves it as it was") {
    val argument = DoubleArray.of(1.0)
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument.toList shouldBe List(1.0)
  }

  test("an absent matrix reaches no emptiness check") {
    val absent: Option[Matrix] = None
    val present: Option[Matrix] = Some(DoubleArray.EMPTY)
    noException should be thrownBy absent.foreach(matrix => ArgCheck.notEmpty(matrix, Name))
    an[IllegalArgumentException] should be thrownBy {
      present.foreach(matrix => ArgCheck.notEmpty(matrix, Name))
    }
  }

  test("notEmpty rejects a matrix holding no elements, with the array wording") {
    val message = messageOf(ArgCheck.notEmpty(DoubleArray.EMPTY, Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("the matrix check counts elements across every dimension, not along one of them") {
    noException should be thrownBy {
      ArgCheck.notEmpty(ArgCheckTables.PopulatedTwoDimensionalMatrix, Name)
    }
    val message = messageOf(ArgCheck.notEmpty(ArgCheckTables.EmptyTwoDimensionalMatrix, Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("notEmpty rejects every empty row of the shared table of matrices") {
    forAll(ArgCheckTables.invalidNotEmptyMatrix) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notEmpty accepts every row of the shared table of matrices that hold elements") {
    forAll(ArgCheckTables.validNotEmptyMatrix) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over an array.

  test("notEmpty accepts an array of references holding an element, and leaves it as it was") {
    val argument = Array("Element")
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument.toList shouldBe List("Element")
  }

  test("an absent array of references reaches no emptiness check") {
    val absent: Option[Array[String]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty array of references, naming it an array") {
    val message = messageOf(ArgCheck.notEmpty(Array.empty[String], Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("an absent array of arrays reaches no emptiness check") {
    val absent: Option[Array[Array[String]]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty array of arrays, the element type being of no interest") {
    val message = messageOf(ArgCheck.notEmpty(Array.empty[Array[String]], Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("notEmpty accepts an array of arrays whose own elements are empty") {
    noException should be thrownBy ArgCheck.notEmpty(Array(Array.empty[String]), Name)
  }

  test("notEmpty rejects every empty row of the shared tables of reference arrays") {
    forAll(ArgCheckTables.invalidNotEmptyObjectArray) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotEmptyNestedArray) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notEmpty accepts every row of the shared tables of reference arrays holding elements") {
    forAll(ArgCheckTables.validNotEmptyObjectArray) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
    forAll(ArgCheckTables.validNotEmptyNestedArray) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
  }


  //-------------------------------------------------------------------------
  // notEmpty, over an array of a primitive type.

  test("notEmpty accepts an array of ints holding an element, and leaves it as it was") {
    val argument = Array(6)
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument.toList shouldBe List(6)
  }

  test("an absent array of ints reaches no emptiness check") {
    val absent: Option[Array[Int]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty array of ints, naming it an array") {
    val message = messageOf(ArgCheck.notEmpty(Array.empty[Int], Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("notEmpty accepts an array of longs holding an element, and leaves it as it was") {
    val argument = Array(6L)
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument.toList shouldBe List(6L)
  }

  test("an absent array of longs reaches no emptiness check") {
    val absent: Option[Array[Long]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty array of longs, naming it an array") {
    val message = messageOf(ArgCheck.notEmpty(Array.empty[Long], Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("notEmpty accepts an array of doubles holding an element, and leaves it as it was") {
    val argument = Array(6.0)
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument.toList shouldBe List(6.0)
  }

  test("an absent array of doubles reaches no emptiness check") {
    val absent: Option[Array[Double]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty array of doubles, naming it an array") {
    val message = messageOf(ArgCheck.notEmpty(Array.empty[Double], Name))
    message shouldBe "Argument array 'name' must not be empty"
  }

  test("every array overload reports an empty array with the one array wording") {
    val expected = "Argument array 'name' must not be empty"
    messageOf(ArgCheck.notEmpty(Array.empty[String], Name)) shouldBe expected
    messageOf(ArgCheck.notEmpty(Array.empty[Int], Name)) shouldBe expected
    messageOf(ArgCheck.notEmpty(Array.empty[Long], Name)) shouldBe expected
    messageOf(ArgCheck.notEmpty(Array.empty[Double], Name)) shouldBe expected
    messageOf(ArgCheck.notEmpty(DoubleArray.EMPTY, Name)) shouldBe expected
  }

  test("notEmpty rejects every empty row of the shared tables of primitive arrays") {
    forAll(ArgCheckTables.invalidNotEmptyIntArray) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotEmptyLongArray) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotEmptyDoubleArray) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notEmpty accepts every row of the shared tables of primitive arrays holding elements") {
    forAll(ArgCheckTables.validNotEmptyIntArray) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
    forAll(ArgCheckTables.validNotEmptyLongArray) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
    forAll(ArgCheckTables.validNotEmptyDoubleArray) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over an iterable and over a map.

  test("notEmpty accepts an iterable holding an element, and leaves it as it was") {
    val argument = List("Element")
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument shouldBe List("Element")
  }

  test("an absent iterable reaches no emptiness check") {
    val absent: Option[Iterable[String]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty iterable, naming it an iterable") {
    val message = messageOf(ArgCheck.notEmpty(List.empty[String], Name))
    message shouldBe "Argument iterable 'name' must not be empty"
  }

  test("notEmpty accepts a collection holding an element through the iterable overload") {
    val argument = Vector("Element")
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument shouldBe Vector("Element")
  }

  test("an absent collection reaches no emptiness check") {
    val absent: Option[Vector[String]] = None
    noException should be thrownBy absent.foreach(values => ArgCheck.notEmpty(values, Name))
    absent shouldBe None
  }

  test("a collection reported as empty carries the iterable wording, the two checks having merged") {
    val message = messageOf(ArgCheck.notEmpty(Vector.empty[String], Name))
    message shouldBe "Argument iterable 'name' must not be empty"
  }

  test("notEmpty reads every ordinary collection shape through the one iterable overload") {
    val expected = "Argument iterable 'name' must not be empty"
    messageOf(ArgCheck.notEmpty(List.empty[String], Name)) shouldBe expected
    messageOf(ArgCheck.notEmpty(Vector.empty[String], Name)) shouldBe expected
    messageOf(ArgCheck.notEmpty(Set.empty[String], Name)) shouldBe expected
    noException should be thrownBy ArgCheck.notEmpty(List("A"), Name)
    noException should be thrownBy ArgCheck.notEmpty(Vector("A"), Name)
    noException should be thrownBy ArgCheck.notEmpty(Set("A"), Name)
  }

  test("notEmpty accepts a map holding a mapping, and leaves it as it was") {
    val argument = SortedMap("Element" -> "Element")
    noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    argument shouldBe SortedMap("Element" -> "Element")
  }

  test("an absent map reaches no emptiness check") {
    val absent: Option[Map[String, String]] = None
    noException should be thrownBy absent.foreach(entries => ArgCheck.notEmpty(entries, Name))
    absent shouldBe None
  }

  test("notEmpty rejects an empty map, naming it a map rather than an iterable") {
    val message = messageOf(ArgCheck.notEmpty(Map.empty[String, String], Name))
    message shouldBe "Argument map 'name' must not be empty"
  }

  test("notEmpty chooses the map wording for a map, although a map is an iterable") {
    messageOf(ArgCheck.notEmpty(SortedMap.empty[String, String], Name)) shouldBe
      "Argument map 'name' must not be empty"
  }

  test("notEmpty rejects every empty row of the shared tables of iterables and maps") {
    forAll(ArgCheckTables.invalidNotEmptyIterable) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotEmptyMap) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notEmpty accepts every row of the shared tables of iterables and maps holding elements") {
    forAll(ArgCheckTables.validNotEmptyIterable) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
    forAll(ArgCheckTables.validNotEmptyMap) { argument =>
      noException should be thrownBy ArgCheck.notEmpty(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notPositive.

  test("notPositive accepts an int that is zero or less") {
    noException should be thrownBy ArgCheck.notPositive(0, Name)
    noException should be thrownBy ArgCheck.notPositive(-1, Name)
  }

  test("notPositive reports a positive int with the value it found") {
    messageOf(ArgCheck.notPositive(1, Name)) shouldBe
      "Argument 'name' must not be positive but has value 1"
  }

  test("notPositive accepts a long that is zero or less") {
    noException should be thrownBy ArgCheck.notPositive(0L, Name)
    noException should be thrownBy ArgCheck.notPositive(-1L, Name)
  }

  test("notPositive reports a positive long with the value it found") {
    messageOf(ArgCheck.notPositive(1L, Name)) shouldBe
      "Argument 'name' must not be positive but has value 1"
  }

  test("notPositive accepts a double that is zero or less") {
    noException should be thrownBy ArgCheck.notPositive(0.0, Name)
    noException should be thrownBy ArgCheck.notPositive(-1.0, Name)
  }

  test("notPositive reports a positive double with the value it found") {
    messageOf(ArgCheck.notPositive(1.0, Name)) shouldBe
      "Argument 'name' must not be positive but has value 1.0"
  }

  test("notPositive admits a not-a-number double, which is neither positive nor negative") {
    noException should be thrownBy ArgCheck.notPositive(Double.NaN, Name)
  }

  test("notPositive accepts a decimal that is zero or less") {
    noException should be thrownBy ArgCheck.notPositive(ArgCheckTables.DecimalZero, Name)
    noException should be thrownBy ArgCheck.notPositive(ArgCheckTables.DecimalMinusOne, Name)
  }

  test("notPositive reports a positive decimal with the value it found") {
    messageOf(ArgCheck.notPositive(ArgCheckTables.DecimalOne, Name)) shouldBe
      "Argument 'name' must not be positive but has value 1"
  }

  test("notPositiveIfPresent accepts an absent decimal and a present one that is not positive") {
    noException should be thrownBy ArgCheck.notPositiveIfPresent(None, Name)
    noException should be thrownBy {
      ArgCheck.notPositiveIfPresent(Some(ArgCheckTables.DecimalZero), Name)
    }
    noException should be thrownBy {
      ArgCheck.notPositiveIfPresent(Some(ArgCheckTables.DecimalMinusOne), Name)
    }
  }

  test("notPositiveIfPresent reports a present positive decimal with the value it found") {
    messageOf(ArgCheck.notPositiveIfPresent(Some(ArgCheckTables.DecimalOne), Name)) shouldBe
      "Argument 'name' must not be positive but has value 1"
  }

  test("notPositive rejects every positive row of the shared tables with the message it names") {
    forAll(ArgCheckTables.invalidNotPositiveInt) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notPositive(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotPositiveLong) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notPositive(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotPositiveDouble) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notPositive(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotPositiveDecimal) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notPositive(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotPositiveIfPresent) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notPositiveIfPresent(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notPositive accepts every row of the shared tables of values that are not positive") {
    forAll(ArgCheckTables.validNotPositiveInt) { argument =>
      noException should be thrownBy ArgCheck.notPositive(argument, Name)
    }
    forAll(ArgCheckTables.validNotPositiveLong) { argument =>
      noException should be thrownBy ArgCheck.notPositive(argument, Name)
    }
    forAll(ArgCheckTables.validNotPositiveDouble) { argument =>
      noException should be thrownBy ArgCheck.notPositive(argument, Name)
    }
    forAll(ArgCheckTables.validNotPositiveDecimal) { argument =>
      noException should be thrownBy ArgCheck.notPositive(argument, Name)
    }
    forAll(ArgCheckTables.validNotPositiveIfPresent) { argument =>
      noException should be thrownBy ArgCheck.notPositiveIfPresent(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notNegative.

  test("notNegative accepts an int that is zero or greater") {
    noException should be thrownBy ArgCheck.notNegative(0, Name)
    noException should be thrownBy ArgCheck.notNegative(1, Name)
  }

  test("notNegative reports a negative int with the value it found") {
    messageOf(ArgCheck.notNegative(-1, Name)) shouldBe
      "Argument 'name' must not be negative but has value -1"
  }

  test("notNegative accepts a long that is zero or greater") {
    noException should be thrownBy ArgCheck.notNegative(0L, Name)
    noException should be thrownBy ArgCheck.notNegative(1L, Name)
  }

  test("notNegative reports a negative long with the value it found") {
    messageOf(ArgCheck.notNegative(-1L, Name)) shouldBe
      "Argument 'name' must not be negative but has value -1"
  }

  test("notNegative accepts a double that is zero or greater") {
    noException should be thrownBy ArgCheck.notNegative(0.0, Name)
    noException should be thrownBy ArgCheck.notNegative(1.0, Name)
  }

  test("notNegative reports a negative double with the value it found") {
    messageOf(ArgCheck.notNegative(-1.0, Name)) shouldBe
      "Argument 'name' must not be negative but has value -1.0"
  }

  test("notNegative admits negative zero, which compares equal to zero, and a not-a-number value") {
    noException should be thrownBy ArgCheck.notNegative(-0.0, Name)
    noException should be thrownBy ArgCheck.notNegative(Double.NaN, Name)
  }

  test("notNegative reads the sign of a decimal, whatever its scale") {
    noException should be thrownBy ArgCheck.notNegative(ArgCheckTables.DecimalZero, Name)
    noException should be thrownBy ArgCheck.notNegative(ArgCheckTables.DecimalPositive, Name)
    messageOf(ArgCheck.notNegative(ArgCheckTables.DecimalNegative, Name)) shouldBe
      "Argument 'name' must not be negative but has value -1.2"
  }

  test("notNegative rejects every negative row of the shared tables with the message it names") {
    forAll(ArgCheckTables.invalidNotNegativeInt) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegative(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotNegativeLong) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegative(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotNegativeDouble) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegative(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotNegativeDecimal) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegative(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notNegative accepts every row of the shared tables of values that are not negative") {
    forAll(ArgCheckTables.validNotNegativeInt) { argument =>
      noException should be thrownBy ArgCheck.notNegative(argument, Name)
    }
    forAll(ArgCheckTables.validNotNegativeLong) { argument =>
      noException should be thrownBy ArgCheck.notNegative(argument, Name)
    }
    forAll(ArgCheckTables.validNotNegativeDouble) { argument =>
      noException should be thrownBy ArgCheck.notNegative(argument, Name)
    }
    forAll(ArgCheckTables.validNotNegativeDecimal) { argument =>
      noException should be thrownBy ArgCheck.notNegative(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notNaN.

  test("notNaN accepts an actual number") {
    noException should be thrownBy ArgCheck.notNaN(0.0, Name)
    noException should be thrownBy ArgCheck.notNaN(1.0, Name)
  }

  test("notNaN reports the one value that is not a number") {
    messageOf(ArgCheck.notNaN(Double.NaN, Name)) shouldBe "Argument 'name' must not be NaN"
  }

  test("notNaN admits both infinities, which are numbers for its purpose") {
    noException should be thrownBy ArgCheck.notNaN(Double.PositiveInfinity, Name)
    noException should be thrownBy ArgCheck.notNaN(Double.NegativeInfinity, Name)
  }

  test("notNaN rejects every row of the shared table that is not a number") {
    forAll(ArgCheckTables.invalidNotNaN) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNaN(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notNaN accepts every row of the shared table of actual numbers") {
    forAll(ArgCheckTables.validNotNaN) { argument =>
      noException should be thrownBy ArgCheck.notNaN(argument, Name)
    }
  }


  //-------------------------------------------------------------------------
  // notNegativeOrZero.

  test("notNegativeOrZero accepts an int above zero") {
    noException should be thrownBy ArgCheck.notNegativeOrZero(1, Name)
  }

  test("notNegativeOrZero reports an int of zero") {
    messageOf(ArgCheck.notNegativeOrZero(0, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value 0"
  }

  test("notNegativeOrZero reports a negative int") {
    messageOf(ArgCheck.notNegativeOrZero(-1, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value -1"
  }

  test("notNegativeOrZero accepts a long above zero") {
    noException should be thrownBy ArgCheck.notNegativeOrZero(1L, Name)
  }

  test("notNegativeOrZero reports a long of zero") {
    messageOf(ArgCheck.notNegativeOrZero(0L, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value 0"
  }

  test("notNegativeOrZero reports a negative long") {
    messageOf(ArgCheck.notNegativeOrZero(-1L, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value -1"
  }

  test("notNegativeOrZero accepts a double above zero") {
    noException should be thrownBy ArgCheck.notNegativeOrZero(1.0, Name)
  }

  test("notNegativeOrZero reports a double of zero") {
    messageOf(ArgCheck.notNegativeOrZero(0.0, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value 0.0"
  }

  test("notNegativeOrZero reports a negative double") {
    messageOf(ArgCheck.notNegativeOrZero(-1.0, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value -1.0"
  }

  test("notNegativeOrZero reports both signed zeros, rendering the sign of each") {
    messageOf(ArgCheck.notNegativeOrZero(0.0, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value 0.0"
    messageOf(ArgCheck.notNegativeOrZero(-0.0, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value -0.0"
  }

  test("the tolerant form accepts a double that lies further than the tolerance above zero") {
    noException should be thrownBy ArgCheck.notNegativeOrZero(1.0, 0.0001, Name)
    noException should be thrownBy ArgCheck.notNegativeOrZero(0.1, 0.0001, Name)
  }

  test("the tolerant form reports a double the tolerance counts as zero") {
    messageOf(ArgCheck.notNegativeOrZero(0.0000001, 0.0001, Name)) shouldBe
      "Argument 'name' must not be zero"
  }

  test("the tolerant form reports a double clearly below zero as too small, not as zero") {
    messageOf(ArgCheck.notNegativeOrZero(-1.0, 0.0001, Name)) shouldBe
      "Argument 'name' must be greater than zero but has value -1.0"
  }

  test("the tolerant form rejects a negative tolerance, naming the tolerance rather than the argument") {
    messageOf(ArgCheck.notNegativeOrZero(1.0, -0.1, Name)) shouldBe
      "Argument 'tolerance' must not be negative but has value -0.1"
  }

  test("the tolerant form rejects a not-a-number tolerance, which describes no interval") {
    messageOf(ArgCheck.notNegativeOrZero(1.0, Double.NaN, Name)) shouldBe
      "Argument 'tolerance' must not be NaN"
  }

  test("the tolerant form names a not-a-number tolerance before judging the argument") {
    messageOf(ArgCheck.notNegativeOrZero(0.0, Double.NaN, Name)) shouldBe
      "Argument 'tolerance' must not be NaN"
    messageOf(ArgCheck.notNegativeOrZero(-1.0, Double.NaN, Name)) shouldBe
      "Argument 'tolerance' must not be NaN"
  }

  test("the tolerant form admits a zero tolerance of either sign, neither being negative") {
    noException should be thrownBy ArgCheck.notNegativeOrZero(1.0, 0.0, Name)
    noException should be thrownBy ArgCheck.notNegativeOrZero(1.0, -0.0, Name)
  }

  test("the tolerant form admits a not-a-number double, which is neither near zero nor below it") {
    noException should be thrownBy ArgCheck.notNegativeOrZero(Double.NaN, 0.0001, Name)
  }

  test("notNegativeOrZero reads the sign of a decimal, whatever its scale") {
    noException should be thrownBy {
      ArgCheck.notNegativeOrZero(ArgCheckTables.DecimalPositive, Name)
    }
    messageOf(ArgCheck.notNegativeOrZero(ArgCheckTables.DecimalZero, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value 0"
    messageOf(ArgCheck.notNegativeOrZero(ArgCheckTables.DecimalNegative, Name)) shouldBe
      "Argument 'name' must not be negative or zero but has value -1.2"
  }

  test("notNegativeOrZero rejects every row of the shared tables with the message it names") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroInt) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegativeOrZero(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroLong) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegativeOrZero(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroDouble) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegativeOrZero(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroDecimal) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notNegativeOrZero(argument, Name)) shouldBe expectedMessage
    }
  }

  test("notNegativeOrZero accepts every row of the shared tables of values above zero") {
    forAll(ArgCheckTables.validNotNegativeOrZeroInt) { argument =>
      noException should be thrownBy ArgCheck.notNegativeOrZero(argument, Name)
    }
    forAll(ArgCheckTables.validNotNegativeOrZeroLong) { argument =>
      noException should be thrownBy ArgCheck.notNegativeOrZero(argument, Name)
    }
    forAll(ArgCheckTables.validNotNegativeOrZeroDouble) { argument =>
      noException should be thrownBy ArgCheck.notNegativeOrZero(argument, Name)
    }
    forAll(ArgCheckTables.validNotNegativeOrZeroDecimal) { argument =>
      noException should be thrownBy ArgCheck.notNegativeOrZero(argument, Name)
    }
  }

  test("the tolerant form rejects every row of its shared table with the message it names") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroWithTolerance) {
      (argument, tolerance, expectedMessage) =>
        messageOf(ArgCheck.notNegativeOrZero(argument, tolerance, Name)) shouldBe expectedMessage
    }
  }

  test("the tolerant form accepts every row of its shared table of accepted arguments") {
    forAll(ArgCheckTables.validNotNegativeOrZeroWithTolerance) { (argument, tolerance) =>
      noException should be thrownBy ArgCheck.notNegativeOrZero(argument, tolerance, Name)
    }
  }

  //-------------------------------------------------------------------------
  // notZero.

  test("notZero accepts a double above zero") {
    noException should be thrownBy ArgCheck.notZero(1.0, Name)
  }

  test("notZero reports a double of zero") {
    messageOf(ArgCheck.notZero(0.0, Name)) shouldBe "Argument 'name' must not be zero"
  }

  test("notZero accepts a double below zero, having no interest in the sign") {
    noException should be thrownBy ArgCheck.notZero(-1.0, Name)
  }

  test("notZero reports negative zero, which compares equal to zero") {
    messageOf(ArgCheck.notZero(-0.0, Name)) shouldBe "Argument 'name' must not be zero"
  }

  test("the tolerant form of notZero accepts a double further than the tolerance from zero") {
    noException should be thrownBy ArgCheck.notZero(1.0, 0.1, Name)
  }

  test("the tolerant form of notZero reports a double the tolerance counts as zero") {
    messageOf(ArgCheck.notZero(0.0, 0.1, Name)) shouldBe "Argument 'name' must not be zero"
  }

  test("the tolerant form of notZero accepts a double below zero by more than the tolerance") {
    noException should be thrownBy ArgCheck.notZero(-1.0, 0.1, Name)
  }

  test("the tolerant form of notZero counts a double near zero from either direction as zero") {
    messageOf(ArgCheck.notZero(0.05, 0.1, Name)) shouldBe "Argument 'name' must not be zero"
    messageOf(ArgCheck.notZero(-0.05, 0.1, Name)) shouldBe "Argument 'name' must not be zero"
  }

  test("the tolerant form of notZero rejects a negative tolerance, naming the tolerance") {
    messageOf(ArgCheck.notZero(1.0, -0.1, Name)) shouldBe
      "Argument 'tolerance' must not be negative but has value -0.1"
  }

  test("the tolerant form of notZero rejects a not-a-number tolerance, naming the tolerance") {
    messageOf(ArgCheck.notZero(1.0, Double.NaN, Name)) shouldBe
      "Argument 'tolerance' must not be NaN"
  }

  test("the tolerant form of notZero names a not-a-number tolerance before judging the argument") {
    messageOf(ArgCheck.notZero(0.0, Double.NaN, Name)) shouldBe "Argument 'tolerance' must not be NaN"
  }

  test("the tolerant form of notZero admits a zero tolerance of either sign") {
    noException should be thrownBy ArgCheck.notZero(1.0, 0.0, Name)
    noException should be thrownBy ArgCheck.notZero(1.0, -0.0, Name)
  }

  test("the tolerant form of notZero admits a not-a-number double, which is not near zero") {
    noException should be thrownBy ArgCheck.notZero(Double.NaN, 0.1, Name)
  }

  test("notZero rejects every row of its shared tables with the message it names") {
    forAll(ArgCheckTables.invalidNotZero) { (argument, expectedMessage) =>
      messageOf(ArgCheck.notZero(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNotZeroWithTolerance) { (argument, tolerance, expectedMessage) =>
      messageOf(ArgCheck.notZero(argument, tolerance, Name)) shouldBe expectedMessage
    }
  }

  test("notZero accepts every row of its shared tables of values that differ from zero") {
    forAll(ArgCheckTables.validNotZero) { argument =>
      noException should be thrownBy ArgCheck.notZero(argument, Name)
    }
    forAll(ArgCheckTables.validNotZeroWithTolerance) { (argument, tolerance) =>
      noException should be thrownBy ArgCheck.notZero(argument, tolerance, Name)
    }
  }

  //-------------------------------------------------------------------------
  // The three range checks over doubles.

  test("inRange over doubles accepts the low bound and every value below the high bound") {
    noException should be thrownBy ArgCheck.inRange(0.5, 0.0, 1.0, Name)
    noException should be thrownBy ArgCheck.inRange(0.0, 0.0, 1.0, Name)
    noException should be thrownBy ArgCheck.inRange(1.0 - 0.00000000001, 0.0, 1.0, Name)
  }

  test("inRange over doubles rejects a value below the low bound and the high bound itself") {
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRange(0.0 - 0.00000000001, 0.0, 1.0, Name)
    }
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRange(1.0, 0.0, 1.0, Name)
  }

  test("inRange over doubles names the interval it expected and the value it found") {
    messageOf(ArgCheck.inRange(2.0, 0.0, 1.0, Name)) shouldBe
      "Expected 0.0 <= 'name' < 1.0, but found 2.0"
  }

  test("inRangeInclusive over doubles accepts both of its bounds") {
    noException should be thrownBy ArgCheck.inRangeInclusive(0.5, 0.0, 1.0, Name)
    noException should be thrownBy ArgCheck.inRangeInclusive(0.0, 0.0, 1.0, Name)
    noException should be thrownBy ArgCheck.inRangeInclusive(1.0, 0.0, 1.0, Name)
  }

  test("inRangeInclusive over doubles rejects a value outside either bound") {
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeInclusive(0.0 - 0.00000000001, 0.0, 1.0, Name)
    }
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeInclusive(1.0 + 0.00000000001, 0.0, 1.0, Name)
    }
  }

  test("inRangeInclusive over doubles names an interval whose bounds are both allowed") {
    messageOf(ArgCheck.inRangeInclusive(2.0, 0.0, 1.0, Name)) shouldBe
      "Expected 0.0 <= 'name' <= 1.0, but found 2.0"
  }

  test("inRangeExclusive over doubles accepts everything between its bounds") {
    noException should be thrownBy ArgCheck.inRangeExclusive(0.5, 0.0, 1.0, Name)
    noException should be thrownBy ArgCheck.inRangeExclusive(0.00000000001, 0.0, 1.0, Name)
    noException should be thrownBy ArgCheck.inRangeExclusive(1.0 - 0.00000000001, 0.0, 1.0, Name)
  }

  test("inRangeExclusive over doubles rejects both of its bounds") {
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRangeExclusive(0.0, 0.0, 1.0, Name)
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRangeExclusive(1.0, 0.0, 1.0, Name)
  }

  test("inRangeExclusive over doubles names an interval whose bounds are both disallowed") {
    messageOf(ArgCheck.inRangeExclusive(2.0, 0.0, 1.0, Name)) shouldBe
      "Expected 0.0 < 'name' < 1.0, but found 2.0"
  }

  test("the range checks over doubles reject every row of their shared tables") {
    forAll(ArgCheckTables.invalidInRangeDouble) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRange(argument, low, high, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInRangeInclusiveDouble) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRangeInclusive(argument, low, high, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInRangeExclusiveDouble) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRangeExclusive(argument, low, high, Name)) shouldBe expectedMessage
    }
  }

  test("the range checks over doubles accept every row of their shared tables") {
    forAll(ArgCheckTables.validInRangeDouble) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRange(argument, low, high, Name)
    }
    forAll(ArgCheckTables.validInRangeInclusiveDouble) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRangeInclusive(argument, low, high, Name)
    }
    forAll(ArgCheckTables.validInRangeExclusiveDouble) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRangeExclusive(argument, low, high, Name)
    }
  }

  //-------------------------------------------------------------------------
  // The three range checks over ints.

  test("inRange over ints accepts the low bound and every value below the high bound") {
    noException should be thrownBy ArgCheck.inRange(1, 0, 2, Name)
    noException should be thrownBy ArgCheck.inRange(0, 0, 2, Name)
  }

  test("inRange over ints is the shape an index is checked with, the size being the high bound") {
    val size = 3
    noException should be thrownBy ArgCheck.inRange(0, 0, size, Name)
    noException should be thrownBy ArgCheck.inRange(size - 1, 0, size, Name)
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRange(size, 0, size, Name)
  }

  test("inRange over ints rejects a value below the low bound and the high bound itself") {
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRange(-1, 0, 1, Name)
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRange(1, 0, 1, Name)
  }

  test("inRange over ints names the interval it expected and the value it found") {
    messageOf(ArgCheck.inRange(3, 0, 2, Name)) shouldBe "Expected 0 <= 'name' < 2, but found 3"
  }

  test("inRangeInclusive over ints accepts both of its bounds") {
    noException should be thrownBy ArgCheck.inRangeInclusive(1, 0, 2, Name)
    noException should be thrownBy ArgCheck.inRangeInclusive(0, 0, 2, Name)
    noException should be thrownBy ArgCheck.inRangeInclusive(2, 0, 2, Name)
  }

  test("inRangeInclusive over ints rejects a value outside either bound") {
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRangeInclusive(-1, 0, 1, Name)
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRangeInclusive(2, 0, 1, Name)
  }

  test("inRangeInclusive over ints names an interval whose bounds are both allowed") {
    messageOf(ArgCheck.inRangeInclusive(3, 0, 2, Name)) shouldBe
      "Expected 0 <= 'name' <= 2, but found 3"
  }

  test("inRangeExclusive over ints accepts everything between its bounds") {
    noException should be thrownBy ArgCheck.inRangeExclusive(1, 0, 2, Name)
    noException should be thrownBy ArgCheck.inRangeExclusive(-1, -2, 0, Name)
  }

  test("inRangeExclusive over ints rejects both of its bounds") {
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRangeExclusive(0, 0, 1, Name)
    an[IllegalArgumentException] should be thrownBy ArgCheck.inRangeExclusive(1, 0, 1, Name)
  }

  test("inRangeExclusive over ints names an interval whose bounds are both disallowed") {
    messageOf(ArgCheck.inRangeExclusive(2, 0, 2, Name)) shouldBe
      "Expected 0 < 'name' < 2, but found 2"
  }

  test("the range checks over ints reject every row of their shared tables") {
    forAll(ArgCheckTables.invalidInRangeInt) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRange(argument, low, high, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInRangeInclusiveInt) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRangeInclusive(argument, low, high, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInRangeExclusiveInt) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRangeExclusive(argument, low, high, Name)) shouldBe expectedMessage
    }
  }

  test("the range checks over ints accept every row of their shared tables") {
    forAll(ArgCheckTables.validInRangeInt) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRange(argument, low, high, Name)
    }
    forAll(ArgCheckTables.validInRangeInclusiveInt) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRangeInclusive(argument, low, high, Name)
    }
    forAll(ArgCheckTables.validInRangeExclusiveInt) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRangeExclusive(argument, low, high, Name)
    }
  }

  //-------------------------------------------------------------------------
  // The three range checks over a type ordered by its own ordering.

  test("inRangeComparable accepts the low bound and every value below the high bound") {
    noException should be thrownBy {
      ArgCheck.inRangeComparable(Duration.ofSeconds(1), Duration.ZERO, Duration.ofSeconds(2), Name)
    }
    noException should be thrownBy {
      ArgCheck.inRangeComparable(Duration.ZERO, Duration.ZERO, Duration.ofSeconds(2), Name)
    }
  }

  test("inRangeComparable rejects a value below the low bound and the high bound itself") {
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeComparable(Duration.ofSeconds(-1), Duration.ZERO, Duration.ofSeconds(1), Name)
    }
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeComparable(Duration.ofSeconds(1), Duration.ZERO, Duration.ofSeconds(1), Name)
    }
  }

  test("inRangeComparable names the interval it expected and the value it found") {
    def check(): Unit =
      ArgCheck.inRangeComparable(Duration.ofSeconds(3), Duration.ZERO, Duration.ofSeconds(2), Name)
    messageOf(check()) shouldBe "Expected PT0S <= 'name' < PT2S, but found PT3S"
  }

  test("inRangeComparableInclusive accepts both of its bounds") {
    noException should be thrownBy {
      ArgCheck.inRangeComparableInclusive(
        Duration.ofSeconds(1),
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name)
    }
    noException should be thrownBy {
      ArgCheck.inRangeComparableInclusive(
        Duration.ofSeconds(2),
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name)
    }
  }

  test("inRangeComparableInclusive rejects a value outside either bound") {
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeComparableInclusive(
        Duration.ofSeconds(-1),
        Duration.ZERO,
        Duration.ofSeconds(1),
        Name)
    }
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeComparableInclusive(
        Duration.ofSeconds(2),
        Duration.ZERO,
        Duration.ofSeconds(1),
        Name)
    }
  }

  test("inRangeComparableInclusive names an interval whose bounds are both allowed") {
    def check(): Unit = ArgCheck.inRangeComparableInclusive(
      Duration.ofSeconds(3),
      Duration.ZERO,
      Duration.ofSeconds(2),
      Name)
    messageOf(check()) shouldBe "Expected PT0S <= 'name' <= PT2S, but found PT3S"
  }

  test("inRangeComparableExclusive accepts everything between its bounds") {
    noException should be thrownBy {
      ArgCheck.inRangeComparableExclusive(
        Duration.ofSeconds(1),
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name)
    }
    noException should be thrownBy {
      ArgCheck.inRangeComparableExclusive(
        Duration.ofMillis(1),
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name)
    }
  }

  test("inRangeComparableExclusive rejects both of its bounds") {
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeComparableExclusive(
        Duration.ZERO,
        Duration.ZERO,
        Duration.ofSeconds(1),
        Name)
    }
    an[IllegalArgumentException] should be thrownBy {
      ArgCheck.inRangeComparableExclusive(
        Duration.ofSeconds(1),
        Duration.ZERO,
        Duration.ofSeconds(1),
        Name)
    }
  }

  test("inRangeComparableExclusive names an interval whose bounds are both disallowed") {
    def check(): Unit = ArgCheck.inRangeComparableExclusive(
      Duration.ofSeconds(2),
      Duration.ZERO,
      Duration.ofSeconds(2),
      Name)
    messageOf(check()) shouldBe "Expected PT0S < 'name' < PT2S, but found PT2S"
  }

  test("the comparable range checks work for a type this library already orders") {
    noException should be thrownBy ArgCheck.inRangeComparable(1, 0, 2, Name)
    noException should be thrownBy ArgCheck.inRangeComparable("B", "A", "C", Name)
    messageOf(ArgCheck.inRangeComparable("D", "A", "C", Name)) shouldBe
      "Expected A <= 'name' < C, but found D"
  }

  test("the comparable range checks reject every row of their shared tables") {
    forAll(ArgCheckTables.invalidInRangeComparable) { (argument, low, high, expectedMessage) =>
      messageOf(ArgCheck.inRangeComparable(argument, low, high, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInRangeComparableInclusive) {
      (argument, low, high, expectedMessage) =>
        def check(): Unit = ArgCheck.inRangeComparableInclusive(argument, low, high, Name)
        messageOf(check()) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInRangeComparableExclusive) {
      (argument, low, high, expectedMessage) =>
        def check(): Unit = ArgCheck.inRangeComparableExclusive(argument, low, high, Name)
        messageOf(check()) shouldBe expectedMessage
    }
  }

  test("the comparable range checks accept every row of their shared tables") {
    forAll(ArgCheckTables.validInRangeComparable) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRangeComparable(argument, low, high, Name)
    }
    forAll(ArgCheckTables.validInRangeComparableInclusive) { (argument, low, high) =>
      noException should be thrownBy {
        ArgCheck.inRangeComparableInclusive(argument, low, high, Name)
      }
    }
    forAll(ArgCheckTables.validInRangeComparableExclusive) { (argument, low, high) =>
      noException should be thrownBy {
        ArgCheck.inRangeComparableExclusive(argument, low, high, Name)
      }
    }
  }

  //-------------------------------------------------------------------------
  // noDuplicates and noDuplicatesSorted.

  test("noDuplicates accepts an array of distinct values in any order") {
    val values = Array(0.0, 1.0, 10.0, 5.0)
    noException should be thrownBy ArgCheck.noDuplicates(values, Name)
    values.toList shouldBe List(0.0, 1.0, 10.0, 5.0)
  }

  test("noDuplicates accepts an array holding one not-a-number value") {
    val values = Array(0.0, 1.0, 10.0, Double.NaN)
    noException should be thrownBy ArgCheck.noDuplicates(values, Name)
    values.length shouldBe 4
  }

  test("noDuplicates reports an array that repeats a value") {
    val values = Array(0.0, 1.0, 10.0, 5.0, 1.0)
    messageOf(ArgCheck.noDuplicates(values, Name)) shouldBe
      "Argument array 'name' must not contain duplicates"
  }

  test("noDuplicates compares bit patterns, so the two signed zeros are different values") {
    noException should be thrownBy ArgCheck.noDuplicates(Array(0.0, -0.0), Name)
  }

  test("noDuplicates compares bit patterns, so a not-a-number value repeated is a duplicate") {
    messageOf(ArgCheck.noDuplicates(Array(Double.NaN, Double.NaN), Name)) shouldBe
      "Argument array 'name' must not contain duplicates"
  }

  test("noDuplicates accepts an empty array and an array of one value") {
    noException should be thrownBy ArgCheck.noDuplicates(Array.empty[Double], Name)
    noException should be thrownBy ArgCheck.noDuplicates(Array(1.0), Name)
  }

  test("noDuplicatesSorted accepts an array whose values increase strictly") {
    val values = Array(0.0, 1.0, 5.0, 10.0)
    noException should be thrownBy ArgCheck.noDuplicatesSorted(values, Name)
    values.toList shouldBe List(0.0, 1.0, 5.0, 10.0)
  }

  test("noDuplicatesSorted passes over a not-a-number value, which orders against nothing") {
    val values = Array(0.0, 1.0, 5.0, Double.NaN, 10.0)
    noException should be thrownBy ArgCheck.noDuplicatesSorted(values, Name)
    values.length shouldBe 5
  }

  test("noDuplicatesSorted reports an array that repeats a value") {
    messageOf(ArgCheck.noDuplicatesSorted(Array(0.0, 1.0, 5.0, 5.0, 10.0), Name)) shouldBe
      "Argument array 'name' must not contain duplicates"
  }

  test("noDuplicatesSorted reports an array that is out of order differently from one that repeats") {
    messageOf(ArgCheck.noDuplicatesSorted(Array(0.0, 1.0, 5.0, 10.0, 4.0), Name)) shouldBe
      "Argument array 'name' must be sorted and not contain duplicates"
  }

  test("noDuplicatesSorted compares arithmetically, so the two signed zeros are a duplicate") {
    messageOf(ArgCheck.noDuplicatesSorted(Array(0.0, -0.0), Name)) shouldBe
      "Argument array 'name' must not contain duplicates"
  }

  test("noDuplicatesSorted accepts an empty array and an array of one value") {
    noException should be thrownBy ArgCheck.noDuplicatesSorted(Array.empty[Double], Name)
    noException should be thrownBy ArgCheck.noDuplicatesSorted(Array(1.0), Name)
  }

  test("the two duplicate checks reject every row of their shared tables") {
    forAll(ArgCheckTables.invalidNoDuplicates) { (argument, expectedMessage) =>
      messageOf(ArgCheck.noDuplicates(argument, Name)) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidNoDuplicatesSorted) { (argument, expectedMessage) =>
      messageOf(ArgCheck.noDuplicatesSorted(argument, Name)) shouldBe expectedMessage
    }
  }

  test("the two duplicate checks accept every row of their shared tables") {
    forAll(ArgCheckTables.validNoDuplicates) { argument =>
      noException should be thrownBy ArgCheck.noDuplicates(argument, Name)
    }
    forAll(ArgCheckTables.validNoDuplicatesSorted) { argument =>
      noException should be thrownBy ArgCheck.noDuplicatesSorted(argument, Name)
    }
  }

  //-------------------------------------------------------------------------
  // inOrderNotEqual and inOrderOrEqual.

  test("inOrderNotEqual accepts two values where the first orders strictly before the second") {
    val first = LocalDate.of(2011, 7, 2)
    val second = LocalDate.of(2011, 7, 3)
    noException should be thrownBy {
      ArgCheck.inOrderNotEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    }
  }

  test("inOrderNotEqual reports two values in the wrong order, naming both and their values") {
    val first = LocalDate.of(2011, 7, 2)
    val second = LocalDate.of(2011, 7, 3)
    def check(): Unit =
      ArgCheck.inOrderNotEqual(second, first, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    messageOf(check()) shouldBe
      "Invalid order: Expected 'a' < 'b', but found: '2011-07-03' >= '2011-07-02'"
  }

  test("inOrderNotEqual reports two equal values, which is what separates it from the other check") {
    val date = LocalDate.of(2011, 7, 3)
    def check(): Unit =
      ArgCheck.inOrderNotEqual(date, date, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    messageOf(check()) shouldBe
      "Invalid order: Expected 'a' < 'b', but found: '2011-07-03' >= '2011-07-03'"
  }

  test("inOrderOrEqual accepts two values in order and two that are equal") {
    val first = LocalDate.of(2011, 7, 2)
    val second = LocalDate.of(2011, 7, 3)
    noException should be thrownBy {
      ArgCheck.inOrderOrEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    }
    noException should be thrownBy {
      ArgCheck.inOrderOrEqual(first, first, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    }
    noException should be thrownBy {
      ArgCheck.inOrderOrEqual(second, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    }
  }

  test("inOrderOrEqual reports two values in the wrong order with its own wording") {
    val first = LocalDate.of(2011, 7, 3)
    val second = LocalDate.of(2011, 7, 2)
    def check(): Unit =
      ArgCheck.inOrderOrEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
    messageOf(check()) shouldBe
      "Invalid order: Expected 'a' <= 'b', but found: '2011-07-03' > '2011-07-02'"
  }

  test("the order checks work for a type this library already orders as readily as for a date") {
    noException should be thrownBy ArgCheck.inOrderNotEqual(1, 2, "first", "second")
    noException should be thrownBy ArgCheck.inOrderOrEqual(2, 2, "first", "second")
    messageOf(ArgCheck.inOrderNotEqual(2, 1, "first", "second")) shouldBe
      "Invalid order: Expected 'first' < 'second', but found: '2' >= '1'"
  }

  test("the order checks reject every row of their shared tables with the message it names") {
    forAll(ArgCheckTables.invalidInOrderNotEqual) { (first, second, expectedMessage) =>
      def check(): Unit =
        ArgCheck.inOrderNotEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
      messageOf(check()) shouldBe expectedMessage
    }
    forAll(ArgCheckTables.invalidInOrderOrEqual) { (first, second, expectedMessage) =>
      def check(): Unit =
        ArgCheck.inOrderOrEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
      messageOf(check()) shouldBe expectedMessage
    }
  }

  test("the order checks accept every row of their shared tables") {
    forAll(ArgCheckTables.validInOrderNotEqual) { (first, second) =>
      noException should be thrownBy {
        ArgCheck.inOrderNotEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
      }
    }
    forAll(ArgCheckTables.validInOrderOrEqual) { (first, second) =>
      noException should be thrownBy {
        ArgCheck.inOrderOrEqual(first, second, ArgCheckTables.FirstName, ArgCheckTables.SecondName)
      }
    }
  }

  //-------------------------------------------------------------------------
  // Properties, over inputs the tables above cannot enumerate.

  test("notNegative throws for an int exactly when the int is below zero") {
    forAll { (argument: Int) =>
      if (argument < 0) {
        messageOf(ArgCheck.notNegative(argument, Name)) shouldBe
          s"Argument 'name' must not be negative but has value $argument"
      } else {
        noException should be thrownBy ArgCheck.notNegative(argument, Name)
      }
    }
  }

  test("notPositive throws for an int exactly when the int is above zero") {
    forAll { (argument: Int) =>
      if (argument > 0) {
        messageOf(ArgCheck.notPositive(argument, Name)) shouldBe
          s"Argument 'name' must not be positive but has value $argument"
      } else {
        noException should be thrownBy ArgCheck.notPositive(argument, Name)
      }
    }
  }

  test("notNegativeOrZero throws for an int exactly when the int is not above zero") {
    forAll { (argument: Int) =>
      if (argument <= 0) {
        messageOf(ArgCheck.notNegativeOrZero(argument, Name)) shouldBe
          s"Argument 'name' must not be negative or zero but has value $argument"
      } else {
        noException should be thrownBy ArgCheck.notNegativeOrZero(argument, Name)
      }
    }
  }

  test("notNegative throws for a long exactly when the long is below zero") {
    forAll { (argument: Long) =>
      if (argument < 0L) {
        messageOf(ArgCheck.notNegative(argument, Name)) shouldBe
          s"Argument 'name' must not be negative but has value $argument"
      } else {
        noException should be thrownBy ArgCheck.notNegative(argument, Name)
      }
    }
  }

  test("notEmpty throws for text exactly when the text holds no character") {
    forAll { (argument: String) =>
      if (argument.isEmpty) {
        messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe
          "Argument 'name' must not be empty"
      } else {
        noException should be thrownBy ArgCheck.notEmpty(argument, Name)
      }
    }
  }

  test("notBlank throws for text exactly when the text holds nothing but whitespace") {
    forAll { (argument: String) =>
      if (argument.trim.isEmpty) {
        messageOf(ArgCheck.notBlank(argument, Name)) shouldBe
          "Argument 'name' must not be blank"
      } else {
        noException should be thrownBy ArgCheck.notBlank(argument, Name)
      }
    }
  }

  test("notEmpty throws for a list exactly when the list holds no element") {
    forAll { (argument: List[String]) =>
      if (argument.isEmpty) {
        messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe
          "Argument iterable 'name' must not be empty"
      } else {
        noException should be thrownBy ArgCheck.notEmpty(argument, Name)
      }
    }
  }

  test("notEmpty throws for an array of doubles exactly when the array holds no element") {
    forAll { (argument: Array[Double]) =>
      if (argument.length == 0) {
        messageOf(ArgCheck.notEmpty(argument, Name)) shouldBe
          "Argument array 'name' must not be empty"
      } else {
        noException should be thrownBy ArgCheck.notEmpty(argument, Name)
      }
    }
  }

  test("inRange over ints throws exactly when the value is outside the half-open interval") {
    forAll { (argument: Int, low: Int, high: Int) =>
      if (argument < low || argument >= high) {
        messageOf(ArgCheck.inRange(argument, low, high, Name)) shouldBe
          s"Expected $low <= 'name' < $high, but found $argument"
      } else {
        noException should be thrownBy ArgCheck.inRange(argument, low, high, Name)
      }
    }
  }

  test("inOrderNotEqual over ints throws exactly when the first does not precede the second") {
    forAll { (first: Int, second: Int) =>
      if (first >= second) {
        messageOf(ArgCheck.inOrderNotEqual(first, second, "first", "second")) shouldBe
          s"Invalid order: Expected 'first' < 'second', but found: '$first' >= '$second'"
      } else {
        noException should be thrownBy ArgCheck.inOrderNotEqual(first, second, "first", "second")
      }
    }
  }

  test("inOrderOrEqual over ints throws exactly when the first follows the second") {
    forAll { (first: Int, second: Int) =>
      if (first > second) {
        messageOf(ArgCheck.inOrderOrEqual(first, second, "first", "second")) shouldBe
          s"Invalid order: Expected 'first' <= 'second', but found: '$first' > '$second'"
      } else {
        noException should be thrownBy ArgCheck.inOrderOrEqual(first, second, "first", "second")
      }
    }
  }

  test("noDuplicates throws for an array of doubles exactly when a bit pattern repeats") {
    forAll { (argument: Array[Double]) =>
      val patterns = argument.iterator.map(java.lang.Double.doubleToLongBits).toList
      if (patterns.distinct.sizeIs < patterns.size) {
        messageOf(ArgCheck.noDuplicates(argument, Name)) shouldBe
          "Argument array 'name' must not contain duplicates"
      } else {
        noException should be thrownBy ArgCheck.noDuplicates(argument, Name)
      }
    }
  }
}
