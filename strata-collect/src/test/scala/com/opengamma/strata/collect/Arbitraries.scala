/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.collection.immutable.SortedMap

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * A small closed family of named values, used by the specs of both Scala modules wherever a
 * `Named` family is needed but no domain family belongs.
 *
 * The family exists so that the generic machinery of this module - the `NamedEnum` typeclass,
 * the name-keyed JSON codecs, the name-derived `Order`, `Hash` and `Show` instances - can be
 * exercised on a family that carries no meaning of its own. `strata-collect` deliberately
 * knows nothing about currencies, indices or calendars, so its specs cannot reach for a real
 * family, and inventing a throwaway one inside each spec would leave several
 * near-identical fixtures to keep in step.
 *
 * The members are deliberately unremarkable except in their names, which cover the three
 * spellings a name-keyed codec has to survive: a plain word, a hyphenated name and a name
 * holding a space. No alternate, external or lenient table is declared here; the lookup
 * semantics those tables drive are the subject of `NamedEnumSpec`, which declares its own
 * richer fixtures for exactly that purpose, and duplicating them here would give two
 * descriptions of one behaviour. That spec's family of the same simple name is a separate
 * type, nested inside its own fixture object and visible only through it, so the two never
 * meet: a spec reaching a value of this family does so through
 * [[Arbitraries.genSampleNamed]] or through this companion, both of which name this type.
 *
 * Instances of the family are reached only through the companion, which holds every member
 * and the `NamedEnum` describing them:
 *
 * {{{
 * SampleNamed.values.toList             // every member, in declaration order
 * NamedEnum[SampleNamed].valueOf("More")  // Some(SampleNamed.More)
 * }}}
 *
 * @param name  the unique name of the member
 */
sealed abstract class SampleNamed private (val name: String) extends Named

/**
 * Holds the members of [[SampleNamed]] together with its name lookup and its instances.
 */
object SampleNamed {

  /** A member whose name is a single plain word, as most named values are. */
  case object Standard extends SampleNamed("Standard")

  /** A second plain member, so that a property has more than one value to choose between. */
  case object More extends SampleNamed("More")

  /** A third plain member, so that ordering by name has something to order. */
  case object Other extends SampleNamed("Other")

  /** A member whose name holds a hyphen, as the names of the date sequences do. */
  case object Hyphenated extends SampleNamed("Sample-Hyphenated")

  /** A member whose name holds a space, as the names of the business-day conventions do. */
  case object Spaced extends SampleNamed("Sample Spaced")

  /**
   * The members of this family, in declaration order.
   *
   * This is the single source of the family's membership: the name lookup below is built from
   * it, and [[Arbitraries.genSampleNamed]] chooses from it, so a member added here is reached
   * by every spec without any of them being edited.
   */
  val values: NonEmptyList[SampleNamed] =
    NonEmptyList.of(Standard, More, Other, Hyphenated, Spaced)

  /**
   * The name lookup of this family, declaring no alternate, external or lenient table.
   *
   * @return the name lookup over [[values]]
   */
  implicit val namedEnum: NamedEnum[SampleNamed] =
    NamedEnum.of(values = values, familyName = "SampleNamed")

  /**
   * The ordering of the members by name, which is also their hashing and their equality.
   *
   * One equality-bearing instance is declared, as everywhere else in this library: `Order`
   * and `Hash` both extend `Eq`, so the three cannot disagree.
   *
   * @return the ordering of the members by name
   */
  implicit val order: Order[SampleNamed] with Hash[SampleNamed] =
    NamedEnum.orderByName[SampleNamed]

  /**
   * The text of a member, which is its name.
   *
   * @return the rendering of a member as its name
   */
  implicit val show: Show[SampleNamed] = NamedEnum.showByName[SampleNamed]
}

/**
 * ScalaCheck generators for the value types of `strata-collect`.
 *
 * This object is a cross-module contract. It is compiled in the test scope of
 * `strata-collect` and made visible to the test scope of the dependent module by the
 * `test->test` dependency edge of the build, so the specs of `strata-basics` build their own
 * generators on top of these ones with
 * `import com.opengamma.strata.collect.Arbitraries._`. Every member is public and no name may
 * drift once a dependent spec refers to it.
 *
 * ===What is generated, and how===
 *
 * Every value comes out of the validated factory of its type - [[Decimal.ofScaled]],
 * [[FixedScaleDecimal.of]], [[DoubleArray.copyOf]], [[DoubleMatrix.tabulate]],
 * [[Failure.of]] - and the inputs handed to those factories are drawn from ranges the
 * factory accepts. Two consequences follow, and the specs depend on both:
 *
 *   - a generated value is always a valid value of its type, so a property never has to ask
 *     whether the value it was given makes sense before asserting anything about it;
 *   - no outcome of a factory is ever unwrapped by force. The one place a factory's outcome
 *     is turned into a generated value is `fromEither`, which produces the value of a
 *     `Right` and discards a `Left`, so a mistake in a range shows up as a discarded
 *     generation rather than as a thrown exception or a fabricated value.
 *
 * Nothing here reads the clock, a random source of its own, or any ambient state: a
 * generator is a pure function of the ScalaCheck seed, so a failing property shrinks and
 * replays exactly.
 *
 * ===Finite values and IEEE-754 edge values===
 *
 * The types holding `double`s appear twice. The `Finite` generators produce only finite
 * values of moderate magnitude, which is what arithmetic and numerical-parity properties
 * need: a sum of two such values cannot overflow to an infinity and a tolerance comparison
 * against it is meaningful. The unqualified generators mix in `NaN`, both infinities and
 * both signed zeroes, which is what the equality design of the port needs: equality on a
 * double-bearing type follows `java.lang.Double.doubleToLongBits`, so `NaN` equals itself
 * and `-0.0` does not equal `0.0`, and only a generator that reaches those values can
 * establish it. The implicit `Arbitrary` of each type is the finite generator, so a property
 * written without a generator of its own gets the well-behaved values; a property about
 * equality names the edge-bearing generator explicitly.
 *
 * Sizes are small by design - arrays hold at most sixteen elements and matrices at most five
 * rows and five columns - because the properties here are about behaviour rather than
 * throughput, and a small case shrinks to something a person can read.
 *
 * ===Cogen instances===
 *
 * Each type also publishes a `Cogen`, which is what ScalaCheck needs in order to generate a
 * ''function'' of that type. The law suites of the dependent module ask for exactly that -
 * `OrderTests[A].order` requires an `Arbitrary[A => A]`, which is derived from
 * `Arbitrary[A]` and `Cogen[A]` - so publishing the pair here is what lets those suites be
 * written at all. Each `Cogen` is derived from a canonical, injective rendering of the value,
 * so two values that are equal perturb the seed identically.
 */
object Arbitraries {

  //-------------------------------------------------------------------------
  /** The largest array a generator produces, which keeps property runs short and readable. */
  private val MaxArraySize: Int = 16

  /** The largest number of rows, or of columns, a generated matrix has. */
  private val MaxMatrixDimension: Int = 5

  /** The largest number of attributes a generated failure carries. */
  private val MaxAttributes: Int = 3

  /** The largest number of failures a generated chain holds. */
  private val MaxChainSize: Int = 4

  /**
   * The largest unscaled value a [[Decimal]] holds, read from the type rather than restated.
   *
   * The constant itself is private to the type, so it is recovered from the largest decimal,
   * which is that value at scale zero.
   */
  private val MaxUnscaled: Long = Decimal.MAX_VALUE.unscaledValue

  /**
   * Turns the outcome of a validated factory into a generated value.
   *
   * A `Right` generates its value. A `Left` generates nothing: the case is discarded and
   * ScalaCheck draws again, which is the honest response to an input the factory rejected.
   * Every call site below supplies inputs from a range its factory accepts, so the discarding
   * branch is unreachable in practice; it is written rather than assumed away because the
   * alternative - unwrapping the outcome by force - would turn a mistake in a range into a
   * thrown exception in an unrelated property.
   *
   * The method is generic in the failure type, so it serves the factories returning
   * `Either[Failure, A]` and those returning `EitherNec[Failure, A]` alike.
   *
   * @param outcome  the outcome of a validated factory
   * @tparam E  the type describing why the factory rejected its input
   * @tparam A  the type of the value produced
   * @return a generator of the value, or one that discards the case
   */
  private def fromEither[E, A](outcome: Either[E, A]): Gen[A] =
    outcome match {
      case Right(value) => Gen.const(value)
      case Left(_) => Gen.fail[A]
    }

  //-------------------------------------------------------------------------
  /**
   * Generates a finite `double` of moderate magnitude.
   *
   * The distribution mixes three ranges with a handful of exact values, so that properties
   * see both the ordinary case and the small integral values whose text form the port's
   * rendering rules single out. Nothing generated here is `NaN` or an infinity, and nothing
   * is large enough for the sum or difference of two draws to overflow to one, so this is the
   * generator arithmetic and tolerance properties are written against.
   *
   * @return a generator of finite values
   */
  val genFiniteDouble: Gen[Double] = Gen.frequency(
    4 -> Gen.choose(-1.0e3, 1.0e3),
    2 -> Gen.choose(-1.0e9, 1.0e9),
    2 -> Gen.choose(-1.0, 1.0),
    1 -> Gen.oneOf(0.0, 1.0, -1.0, 0.5, -0.5, 100.0, -100.0))

  /**
   * Generates one of the five IEEE-754 values the equality design of the port turns on.
   *
   * `NaN` is equal to itself under the `doubleToLongBits` comparison the double-bearing types
   * use, the two infinities are equal only to themselves, and the two signed zeroes are
   * distinct from one another. A property about any of those facts needs the value in hand,
   * and a generator of ordinary numbers will not produce one in any number of draws, so this
   * generator produces nothing else.
   *
   * @return a generator of the IEEE-754 edge values
   */
  val genEdgeDouble: Gen[Double] =
    Gen.oneOf(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, -0.0, 0.0)

  /**
   * Generates a `double` that is usually finite and occasionally an IEEE-754 edge value.
   *
   * This is the element generator behind the edge-bearing array and matrix generators: a
   * collection of a dozen elements drawn from it holds an edge value more often than not,
   * while still being mostly ordinary numbers, so one property covers both.
   *
   * @return a generator of finite values mixed with the edge values
   */
  val genDouble: Gen[Double] = Gen.frequency(3 -> genFiniteDouble, 1 -> genEdgeDouble)

  //-------------------------------------------------------------------------
  /** Generates a scale a decimal accepts, from none to the eighteen it supports. */
  private val genScale: Gen[Int] = Gen.choose(0, Decimal.MAX_SCALE)

  /**
   * Generates an unscaled value a decimal accepts, across four magnitudes.
   *
   * The ranges overlap deliberately: the narrow ones make small values common, since those
   * are the ones a reader of a failing case can check by hand, while the widest one reaches
   * the eighteen-digit limit of the type, where rescaling a value for comparison or addition
   * is at its most delicate.
   */
  private val genUnscaled: Gen[Long] = Gen.frequency(
    3 -> Gen.choose(-999L, 999L),
    2 -> Gen.choose(-999999999L, 999999999L),
    2 -> Gen.choose(-MaxUnscaled, MaxUnscaled),
    1 -> Gen.oneOf(0L, 1L, -1L, MaxUnscaled, -MaxUnscaled))

  /**
   * Generates a decimal from an unscaled value and a scale, which is the widest route in.
   *
   * Both inputs are drawn from the ranges [[Decimal.ofScaled]] accepts without adjustment, so
   * the factory answers `Right` for every pair, and the value it answers with is normalised -
   * a generated decimal therefore never holds a trailing zero in its fraction.
   */
  private val genScaledDecimal: Gen[Decimal] =
    for {
      unscaled <- genUnscaled
      scale <- genScale
      decimal <- fromEither(Decimal.ofScaled(unscaled, scale))
    } yield decimal

  /**
   * Generates a decimal from a finite `double`, which is the route most callers use.
   *
   * The magnitude is kept well inside the eighteen digits a decimal holds, so the conversion
   * - which goes through the shortest text of the `double` - always succeeds. This branch is
   * what puts values whose scale comes from a binary fraction into the distribution, rather
   * than only the scales chosen directly by [[genScale]].
   */
  private val genDoubleDecimal: Gen[Decimal] =
    for {
      value <- Gen.choose(-1.0e9, 1.0e9)
      decimal <- fromEither(Decimal.of(value))
    } yield decimal

  /** Generates one of the three decimals the type publishes as a constant. */
  private val genConstantDecimal: Gen[Decimal] =
    Gen.oneOf(Decimal.ZERO, Decimal.MAX_VALUE, Decimal.MIN_VALUE)

  /**
   * Generates a decimal, spanning the domain of the type.
   *
   * The distribution covers zero, both signs, magnitudes from a single digit to the
   * eighteen-digit limit, every scale from zero to eighteen, and the published constants, so
   * a property over this generator sees the boundaries of the type as well as its interior.
   * Every value is built by a factory of the type and is therefore normalised and in range.
   *
   * @return a generator of decimals
   */
  val genDecimal: Gen[Decimal] = Gen.frequency(
    5 -> genScaledDecimal,
    2 -> genDoubleDecimal,
    1 -> genConstantDecimal)

  /**
   * The implicit generator of decimals, which is [[genDecimal]].
   *
   * @return the arbitrary decimal
   */
  implicit val arbDecimal: Arbitrary[Decimal] = Arbitrary(genDecimal)

  /**
   * Perturbs a seed by a decimal, through the canonical text of the value.
   *
   * The text is injective over the type because a decimal is normalised: two decimals share
   * their text exactly when they are equal, which is what a `Cogen` has to respect.
   *
   * @return the cogenerator of decimals
   */
  implicit val cogenDecimal: Cogen[Decimal] =
    implicitly[Cogen[String]].contramap[Decimal](_.toString)

  //-------------------------------------------------------------------------
  /**
   * Generates a decimal paired with a scale that can present it.
   *
   * The scale is drawn from the range [[FixedScaleDecimal.of]] accepts for the decimal in
   * hand - at least the scale of the decimal, so that no digit it holds is dropped, and at
   * most the eighteen a decimal supports - so the factory answers `Right` for every pair and
   * the generated values cover both the exact scale of the decimal and every padding of it.
   *
   * @return a generator of fixed-scale decimals
   */
  val genFixedScaleDecimal: Gen[FixedScaleDecimal] =
    for {
      decimal <- genDecimal
      fixedScale <- Gen.choose(decimal.scale, Decimal.MAX_SCALE)
      fixed <- fromEither(FixedScaleDecimal.of(decimal, fixedScale))
    } yield fixed

  /**
   * The implicit generator of fixed-scale decimals, which is [[genFixedScaleDecimal]].
   *
   * @return the arbitrary fixed-scale decimal
   */
  implicit val arbFixedScaleDecimal: Arbitrary[FixedScaleDecimal] =
    Arbitrary(genFixedScaleDecimal)

  /**
   * Perturbs a seed by a fixed-scale decimal, through the canonical text of the value.
   *
   * The text shows the fixed number of decimal places, so it distinguishes the same decimal
   * presented at two different scales - which are two different values of this type.
   *
   * @return the cogenerator of fixed-scale decimals
   */
  implicit val cogenFixedScaleDecimal: Cogen[FixedScaleDecimal] =
    implicitly[Cogen[String]].contramap[FixedScaleDecimal](_.toString)

  //-------------------------------------------------------------------------
  /**
   * Generates an array of the given minimum size whose elements come from the given generator.
   *
   * The array is built by [[DoubleArray.copyOf]] from an immutable list, so the value handed
   * to a property holds no reference to anything the generator can still reach: the
   * `private[collect]` factories that wrap an array without copying it are deliberately not
   * used here, even though this file could reach them, because a generated value that shared
   * its backing array with the generator would make an aliasing bug invisible.
   */
  private def genArrayOf(element: Gen[Double], minimumSize: Int): Gen[DoubleArray] =
    for {
      size <- Gen.choose(minimumSize, MaxArraySize)
      values <- Gen.listOfN(size, element)
    } yield DoubleArray.copyOf(values)

  /**
   * Generates an array of finite values, including the empty array.
   *
   * This is the generator arithmetic and parity properties are written against, and the one
   * the implicit [[arbDoubleArray]] uses.
   *
   * @return a generator of arrays of finite values, of size zero to sixteen
   */
  val genFiniteDoubleArray: Gen[DoubleArray] = genArrayOf(genFiniteDouble, 0)

  /**
   * Generates a non-empty array of finite values.
   *
   * The operations that reduce an array to a single value - its minimum, its maximum, a fold
   * over it - are defined only where there is an element to return, so a property about one
   * of them draws from here rather than filtering the empty case out afterwards.
   *
   * @return a generator of arrays of finite values, of size one to sixteen
   */
  val genNonEmptyFiniteDoubleArray: Gen[DoubleArray] = genArrayOf(genFiniteDouble, 1)

  /**
   * Generates an array whose elements may be `NaN`, an infinity or a signed zero.
   *
   * This is the generator the equality, hashing and text properties of the type are written
   * against, because those are the properties the IEEE-754 edge values decide. It includes
   * the empty array and, as a matter of distribution rather than of guarantee, most arrays of
   * more than a few elements hold at least one edge value.
   *
   * @return a generator of arrays of finite and edge values, of size zero to sixteen
   */
  val genDoubleArray: Gen[DoubleArray] = genArrayOf(genDouble, 0)

  /**
   * Generates a non-empty array whose elements may be `NaN`, an infinity or a signed zero.
   *
   * @return a generator of arrays of finite and edge values, of size one to sixteen
   */
  val genNonEmptyDoubleArray: Gen[DoubleArray] = genArrayOf(genDouble, 1)

  /** Generates two arrays of one size, both drawn from the given element generator. */
  private def genArrayPairOf(element: Gen[Double]): Gen[(DoubleArray, DoubleArray)] =
    for {
      size <- Gen.choose(0, MaxArraySize)
      left <- Gen.listOfN(size, element)
      right <- Gen.listOfN(size, element)
    } yield (DoubleArray.copyOf(left), DoubleArray.copyOf(right))

  /**
   * Generates two arrays of equal size holding finite values.
   *
   * The element-wise operations of the type - `plus`, `minus`, `combine` and their kin -
   * require their operands to agree on size and report a caller that supplies two that do
   * not, so a property about what they compute needs a pair that agrees by construction. The
   * mismatched case is a precondition of the operation and is tested directly rather than
   * generated.
   *
   * @return a generator of pairs of equal-sized arrays of finite values
   */
  val genFiniteDoubleArrayPair: Gen[(DoubleArray, DoubleArray)] =
    genArrayPairOf(genFiniteDouble)

  /**
   * Generates two arrays of equal size whose elements may be edge values.
   *
   * @return a generator of pairs of equal-sized arrays of finite and edge values
   */
  val genDoubleArrayPair: Gen[(DoubleArray, DoubleArray)] = genArrayPairOf(genDouble)

  /**
   * The implicit generator of arrays, which is the finite one, [[genFiniteDoubleArray]].
   *
   * A property that names no generator of its own is nearly always about what an operation
   * computes rather than about the IEEE-754 edges, so the well-behaved values are the safer
   * default; a property about the edges names [[genDoubleArray]] explicitly.
   *
   * @return the arbitrary array
   */
  implicit val arbDoubleArray: Arbitrary[DoubleArray] = Arbitrary(genFiniteDoubleArray)

  /**
   * Perturbs a seed by an array, through its elements in order.
   *
   * @return the cogenerator of arrays
   */
  implicit val cogenDoubleArray: Cogen[DoubleArray] =
    implicitly[Cogen[List[Double]]].contramap[DoubleArray](_.toList)

  //-------------------------------------------------------------------------
  /**
   * Generates a matrix of the given shape whose elements come from the given generator.
   *
   * Exactly `rows * columns` elements are drawn and laid out in row-major order through
   * [[DoubleMatrix.tabulate]], which is the factory every element-wise operation of the type
   * is defined in terms of. Building the matrix this way makes it rectangular by
   * construction - the type admits no other shape - and copy-safe, since the elements are
   * read out of an immutable vector as the factory fills its own rows.
   *
   * A shape with no row or no column is the empty matrix: the factory collapses both to it,
   * and no element is drawn.
   */
  private def genMatrixOf(element: Gen[Double], rows: Int, columns: Int): Gen[DoubleMatrix] =
    Gen.listOfN(rows * columns, element).map { values =>
      val elements = values.toVector
      DoubleMatrix.tabulate(rows, columns)((row, column) => elements(row * columns + column))
    }

  /** Generates a matrix of a freely chosen shape, drawn from the given element generator. */
  private def genShapedMatrixOf(element: Gen[Double]): Gen[DoubleMatrix] =
    for {
      rows <- Gen.choose(0, MaxMatrixDimension)
      columns <- Gen.choose(0, MaxMatrixDimension)
      matrix <- genMatrixOf(element, rows, columns)
    } yield matrix

  /** Generates a square matrix, drawn from the given element generator. */
  private def genSquareMatrixOf(element: Gen[Double]): Gen[DoubleMatrix] =
    for {
      size <- Gen.choose(0, MaxMatrixDimension)
      matrix <- genMatrixOf(element, size, size)
    } yield matrix

  /**
   * Generates a rectangular matrix of finite values.
   *
   * Shapes run from no rows and no columns - the empty matrix - to five by five, and both
   * degenerate shapes, a row count with no columns and a column count with no rows, arise and
   * are the empty matrix, which is what the type defines them to be. This is the generator
   * the implicit [[arbDoubleMatrix]] uses.
   *
   * @return a generator of matrices of finite values, of shape up to five by five
   */
  val genFiniteDoubleMatrix: Gen[DoubleMatrix] = genShapedMatrixOf(genFiniteDouble)

  /**
   * Generates a rectangular matrix whose elements may be `NaN`, an infinity or a signed zero.
   *
   * @return a generator of matrices of finite and edge values, of shape up to five by five
   */
  val genDoubleMatrix: Gen[DoubleMatrix] = genShapedMatrixOf(genDouble)

  /**
   * Generates a square matrix of finite values, including the empty matrix.
   *
   * The operations that require a square matrix, and the properties of the ones that produce
   * one - a transposition being its own inverse, an identity multiplying to no effect - need
   * the shape guaranteed rather than filtered for, since a freely shaped matrix is square
   * only about one time in five.
   *
   * @return a generator of square matrices of finite values, of side up to five
   */
  val genSquareFiniteDoubleMatrix: Gen[DoubleMatrix] = genSquareMatrixOf(genFiniteDouble)

  /**
   * Generates a square matrix whose elements may be edge values.
   *
   * @return a generator of square matrices of finite and edge values, of side up to five
   */
  val genSquareDoubleMatrix: Gen[DoubleMatrix] = genSquareMatrixOf(genDouble)

  /** Generates two matrices of one shape, both drawn from the given element generator. */
  private def genMatrixPairOf(element: Gen[Double]): Gen[(DoubleMatrix, DoubleMatrix)] =
    for {
      rows <- Gen.choose(0, MaxMatrixDimension)
      columns <- Gen.choose(0, MaxMatrixDimension)
      left <- genMatrixOf(element, rows, columns)
      right <- genMatrixOf(element, rows, columns)
    } yield (left, right)

  /**
   * Generates two matrices of the same shape holding finite values.
   *
   * As with the array pairs, the element-wise operations of the type require their operands to
   * agree on shape, so a property about what they compute is written over a pair that agrees
   * by construction.
   *
   * @return a generator of pairs of same-shaped matrices of finite values
   */
  val genFiniteDoubleMatrixPair: Gen[(DoubleMatrix, DoubleMatrix)] =
    genMatrixPairOf(genFiniteDouble)

  /**
   * Generates two matrices of the same shape whose elements may be edge values.
   *
   * @return a generator of pairs of same-shaped matrices of finite and edge values
   */
  val genDoubleMatrixPair: Gen[(DoubleMatrix, DoubleMatrix)] = genMatrixPairOf(genDouble)

  /**
   * The implicit generator of matrices, which is the finite one, [[genFiniteDoubleMatrix]].
   *
   * @return the arbitrary matrix
   */
  implicit val arbDoubleMatrix: Arbitrary[DoubleMatrix] = Arbitrary(genFiniteDoubleMatrix)

  /**
   * Perturbs a seed by a matrix, through its rows in order.
   *
   * The rows carry the shape as well as the elements, since a matrix of no rows has no
   * elements to show and one of several rows shows each of them separately.
   *
   * @return the cogenerator of matrices
   */
  implicit val cogenDoubleMatrix: Cogen[DoubleMatrix] =
    implicitly[Cogen[List[List[Double]]]].contramap[DoubleMatrix](rowsOf)

  /** Reads the rows of a matrix as lists, which is the rendering the cogenerator perturbs by. */
  private def rowsOf(matrix: DoubleMatrix): List[List[Double]] =
    List.tabulate(matrix.rowCount)(row => matrix.row(row).toList)

  //-------------------------------------------------------------------------
  /**
   * Generates one of the ten failure reasons, uniformly.
   *
   * The members come from the family's own `values`, so the generator covers every reason the
   * family declares and covers any reason added to it later without being edited.
   *
   * @return a generator of failure reasons
   */
  val genFailureReason: Gen[FailureReason] = Gen.oneOf(FailureReason.values.toList)

  /**
   * The implicit generator of failure reasons, which is [[genFailureReason]].
   *
   * @return the arbitrary failure reason
   */
  implicit val arbFailureReason: Arbitrary[FailureReason] = Arbitrary(genFailureReason)

  /**
   * Perturbs a seed by a failure reason, through its name.
   *
   * @return the cogenerator of failure reasons
   */
  implicit val cogenFailureReason: Cogen[FailureReason] =
    implicitly[Cogen[String]].contramap[FailureReason](_.name)

  /**
   * Generates one of the attribute keys the failures of this module use.
   *
   * The keys are the generic ones - the value a message refers to, the name of the thing that
   * failed, the type of a cause - because this module describes no domain of its own and a
   * generated failure should not pretend otherwise.
   */
  private val genAttributeKey: Gen[String] =
    Gen.oneOf("value", "name", "id", "reason", "exceptionType", "scale")

  /** Generates an attribute value, including the empty one, which is a value a caller may set. */
  private val genAttributeValue: Gen[String] =
    Gen.oneOf("first", "second", "12", "", "sample-value", "2024-01-31")

  /** Generates a word for a failure message, drawn from the vocabulary of this module. */
  private val genMessageWord: Gen[String] =
    Gen.oneOf("value", "data", "input", "element", "scale", "text", "range")

  /**
   * Generates the attributes of a failure: sometimes none, sometimes up to three.
   *
   * The map is sorted, as the type requires: the order of the keys is what makes the JSON of a
   * failure byte-stable, so a generated value that was merely a `Map` would let a property
   * about that stability pass for the wrong reason. Keys are drawn from a small pool, so a
   * generated map occasionally holds fewer entries than were drawn, the later value of a
   * repeated key winning - which is the merging rule of the type itself.
   *
   * @return a generator of failure attributes
   */
  val genFailureAttributes: Gen[SortedMap[String, String]] =
    for {
      count <- Gen.choose(0, MaxAttributes)
      entries <- Gen.listOfN(count, Gen.zip(genAttributeKey, genAttributeValue))
    } yield SortedMap.from(entries)

  /**
   * Generates a non-empty failure message.
   *
   * A message is written for a person reading a log or a report, so a generated one is a
   * short phrase rather than random text: a property that renders a failure and asserts on the
   * result is easier to read when the message looks like one.
   *
   * @return a generator of failure messages
   */
  val genFailureMessage: Gen[String] =
    for {
      prefix <- Gen.oneOf(
        "Unable to resolve",
        "Invalid input for",
        "Unsupported operation on",
        "Missing data for")
      count <- Gen.choose(1, 3)
      words <- Gen.listOfN(count, genMessageWord)
    } yield (prefix :: words).mkString(" ")

  /**
   * Generates a failure of the given reason.
   *
   * The failure is built by [[Failure.of]], which answers the member of the family that
   * carries the reason asked for, so naming a reason here names a case class: a property about
   * one kind of failure - a parsing failure, a missing-data failure - draws from this method
   * rather than filtering a general generator.
   *
   * @param reason  the reason the generated failures carry
   * @return a generator of failures of that reason
   */
  def genFailureWithReason(reason: FailureReason): Gen[Failure] =
    for {
      message <- genFailureMessage
      attributes <- genFailureAttributes
    } yield Failure.of(reason, message, attributes)

  /**
   * Generates a failure of any of the ten kinds.
   *
   * The reason is drawn uniformly from the family and [[Failure.of]] maps it to the member
   * that carries it, so all ten case classes of the type are reachable and roughly equally
   * likely. Every generated failure carries a non-empty message and attributes that are
   * sometimes empty and sometimes populated.
   *
   * @return a generator of failures
   */
  val genFailure: Gen[Failure] = genFailureReason.flatMap(genFailureWithReason)

  /**
   * The implicit generator of failures, which is [[genFailure]].
   *
   * @return the arbitrary failure
   */
  implicit val arbFailure: Arbitrary[Failure] = Arbitrary(genFailure)

  /**
   * Perturbs a seed by a failure, through its reason, message and attributes.
   *
   * The rendering joins the three parts with a separator that cannot occur inside any of
   * them, so two failures perturb the seed identically exactly when they are equal.
   *
   * @return the cogenerator of failures
   */
  implicit val cogenFailure: Cogen[Failure] =
    implicitly[Cogen[String]].contramap[Failure](renderingOf)

  /** Renders a failure injectively, which is the form the cogenerator perturbs by. */
  private def renderingOf(failure: Failure): String = {
    val attributes = failure.attributes.iterator.map { case (key, value) => key + "\u0000" + value }
    (failure.reason.name :: failure.message :: attributes.toList).mkString("\u0001")
  }

  /**
   * Generates a chain of one to four failures.
   *
   * A chain is what an operation reporting more than one cause answers with, so the
   * accumulating combinators and `Failure.collapse` are written against it. The chain is
   * non-empty by construction, since that is what the type guarantees, and repeated draws of
   * one failure are possible, which is the case `collapse` folds together.
   *
   * @return a generator of non-empty chains of failures
   */
  val genFailures: Gen[NonEmptyChain[Failure]] =
    for {
      head <- genFailure
      count <- Gen.choose(0, MaxChainSize - 1)
      tail <- Gen.listOfN(count, genFailure)
    } yield NonEmptyChain.of(head, tail: _*)

  /**
   * The implicit generator of chains of failures, which is [[genFailures]].
   *
   * @return the arbitrary chain of failures
   */
  implicit val arbFailures: Arbitrary[NonEmptyChain[Failure]] = Arbitrary(genFailures)

  //-------------------------------------------------------------------------
  /**
   * Generates one of the members of [[SampleNamed]], uniformly.
   *
   * The members come from the family's own `values`, which is also what its `NamedEnum` is
   * built from, so a property may assert that a generated member resolves from its own name.
   *
   * @return a generator of members of the sample family
   */
  val genSampleNamed: Gen[SampleNamed] = Gen.oneOf(SampleNamed.values.toList)

  /**
   * The implicit generator of members of the sample family, which is [[genSampleNamed]].
   *
   * @return the arbitrary member of the sample family
   */
  implicit val arbSampleNamed: Arbitrary[SampleNamed] = Arbitrary(genSampleNamed)

  /**
   * Perturbs a seed by a member of the sample family, through its name.
   *
   * A named value is identified by its name, so the name is the injective rendering of it.
   *
   * @return the cogenerator of members of the sample family
   */
  implicit val cogenSampleNamed: Cogen[SampleNamed] =
    implicitly[Cogen[String]].contramap[SampleNamed](_.name)
}
