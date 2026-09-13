/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import cats.Hash
import cats.Order
import cats.Show
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.Validated

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.rng.Seed
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

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
 * The shared generators are small by design - arrays hold at most sixteen elements and matrices
 * at most five rows and five columns - because the properties written over them are about
 * behaviour rather than throughput, and a small case shrinks to something a person can read.
 *
 * ===Large inputs===
 *
 * Smallness has a cost: an operation whose loop is wrong only past some length is never put
 * under strain by a sixteen-element input, and a five-by-five matrix cannot tell an operation
 * that indexes rows and columns correctly from one that happens to agree on a small square. A
 * separate, deliberately large generator is therefore published beside each small one -
 * `genLargeFiniteDoubleArray`, `genLargeDoubleArray`, `genLargeFiniteDoubleArrayPair`,
 * `genLargeFiniteDoubleMatrix`, `genLargeSquareFiniteDoubleMatrix` and
 * `genLargeFiniteDoubleMatrixPair` - producing arrays of hundreds of elements and matrices of
 * tens of rows and columns. They are named explicitly by the properties that want size, so no
 * existing property changes size and no implicit `Arbitrary` draws from them; the small
 * generators remain the default and remain the shrink-friendly ones. Neither size bound of the
 * small generators is affected by their existence, and the two populations do not overlap: the
 * floor of a large generator is above the ceiling of the small one it sits beside.
 *
 * ===Cogen instances===
 *
 * Each type also publishes a `Cogen`, which is what ScalaCheck needs in order to generate a
 * ''function'' of that type. The law suites of the dependent module ask for exactly that -
 * `OrderTests[A].order` requires an `Arbitrary[A => A]`, which is derived from
 * `Arbitrary[A]` and `Cogen[A]` - so publishing the pair here is what lets those suites be
 * written at all. Each `Cogen` is derived from a canonical, injective rendering of the value,
 * so two values that are equal perturb the seed identically.
 *
 * ===Shrink instances===
 *
 * Each type also publishes a `Shrink`, which is what ScalaCheck minimises a failing case
 * with. Without one it falls back on `shrinkAny`, which offers no candidate at all, so a
 * property that fails on a sixteen-element array reports that array rather than the two
 * elements that actually break it, and one that fails on an eighteen-digit decimal reports
 * all eighteen digits. Every instance here obeys three rules, and each rule is there because
 * breaking it would turn minimising a real failure into reporting a spurious one:
 *
 *   - a candidate is built by the same validated factory the generator uses -
 *     [[Decimal.ofScaled]], [[FixedScaleDecimal.of]], [[DoubleArray.tabulate]],
 *     [[DoubleMatrix.tabulate]], [[Failure.of]] - and an outcome the factory rejects
 *     contributes no candidate rather than being unwrapped by force, exactly as in the
 *     generators above;
 *   - a candidate keeps the invariant the generator it came from promises: an array never
 *     shrinks to the empty array, a matrix stays rectangular and a square one stays square, a
 *     pair keeps its two sides in agreement, a chain of failures stays non-empty, and a
 *     failure keeps its reason and a non-empty message. A property written over
 *     [[genNonEmptyFiniteDoubleArray]] or [[genSquareFiniteDoubleMatrix]] would otherwise be
 *     "minimised" into an input its own operation rejects, and the minimised case would fail
 *     for a reason the original never had;
 *   - every candidate is strictly smaller than its input under a measure named in the
 *     scaladoc of the instance, and each measure is a non-negative whole number, so repeated
 *     shrinking terminates at a value that has no candidates - the documented floor of the
 *     type, which is zero for a decimal, a single zero element for an array, a zero matrix
 *     with a dimension of one (one by one where the input was square), and a chain of one
 *     attribute-free single-word failure.
 *
 * The floor named in the scaladoc of each instance is the value repeated shrinking arrives at,
 * so a failing property reports a minimised case a reader can both act on and predict.
 *
 * What the three rules guarantee is observed twice over. Every property of either module runs
 * over these instances, so an instance that produced an invalid value, or that minimised a
 * case into one its own operation rejects, shows up as a failure of the property that drew on
 * it; and [[ArbitrariesSpec]], declared below in this same file, asserts the rules directly,
 * generator by generator, so a generator that stopped drawing the edge values documented here
 * is reported by a red test of its own rather than by every property quietly passing.
 *
 * @see [[SampleNamed]] for the sample named family generated here
 * @see [[ArbitrariesSpec]] for the suite that executes every instance declared here
 */
object Arbitraries {

  //-------------------------------------------------------------------------
  /** The largest array a generator produces, which keeps property runs short and readable. */
  private val MaxArraySize: Int = 16

  /** The largest number of rows, or of columns, a generated matrix has. */
  private val MaxMatrixDimension: Int = 5

  /**
   * The smallest and largest array a ''large'' generator produces.
   *
   * These bound the deliberately large generators below, which exist so that the numeric
   * wrappers are also exercised at a size the shared generators never reach. The floor is above
   * the ceiling of the shared generators, so a value drawn from a large generator is always
   * larger than any value drawn from a small one and the two populations do not overlap.
   *
   * The ceiling is a few hundred elements rather than a few thousand: an element-wise operation
   * is linear, so a few hundred elements exercises the same loop boundaries a few thousand
   * would - a first index, a last index, and every index between - while keeping a property of a
   * hundred cases well under a second.
   */
  private val MinLargeArraySize: Int = 64

  /** The largest array a large generator produces. See [[MinLargeArraySize]]. */
  private val MaxLargeArraySize: Int = 512

  /**
   * The smallest and largest dimension a ''large'' generated matrix has.
   *
   * A matrix is quadratic in its dimension, and the operations that pair two of them -
   * `multipliedBy` and the element-wise arithmetic - and the one that transposes are all linear
   * in the element count, so a dimension in the tens gives an element count in the thousands.
   * That is the same order as the large arrays above and costs the same order of time.
   */
  private val MinLargeMatrixDimension: Int = 8

  /** The largest dimension a large generated matrix has. See [[MinLargeMatrixDimension]]. */
  private val MaxLargeMatrixDimension: Int = 48

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

  /**
   * Turns the outcome of a validated factory into a shrink candidate.
   *
   * This is [[fromEither]] in the shape shrinking needs. A `Right` contributes the one
   * candidate it holds; a `Left` contributes none, so an outcome the factory rejected is
   * dropped rather than forced into a value. Every call site below hands its factory a
   * smaller version of parts the input already carried, so the dropping branch is unreachable
   * in practice; it is written rather than assumed away for the same reason `fromEither` is -
   * the alternative would turn a mistake here into a thrown exception while an unrelated
   * property was being minimised, which is the hardest kind of failure to read.
   *
   * @param outcome  the outcome of a validated factory
   * @tparam E  the type describing why the factory rejected its input
   * @tparam A  the type of the value produced
   * @return the single candidate, or no candidate at all
   */
  private def candidateOf[E, A](outcome: Either[E, A]): LazyList[A] =
    outcome match {
      case Right(value) => LazyList(value)
      case Left(_) => LazyList.empty
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

  /**
   * The bit pattern of positive zero, the value shrinking simplifies an element towards.
   *
   * The comparison below is made on bit patterns rather than with `==` because that is the
   * equality the double-bearing types use: a negative zero and a positive zero compare equal
   * as numbers but are different values of an array or a matrix, so replacing a negative zero
   * with a positive one is a real simplification and has to be recognised as one. `NaN`,
   * which compares equal to nothing at all under `==`, is recognised the same way.
   */
  private val ZeroBits: Long = java.lang.Double.doubleToLongBits(0.0)

  /**
   * Whether an element is already the positive zero that shrinking simplifies towards.
   *
   * This is the per-element part of the shrinking measure of both double-bearing types. An
   * element that answers true here cannot be simplified further; one that answers false - an
   * ordinary number, a negative zero, an infinity or `NaN` - is simplified in a single step,
   * which is what makes the measure fall by one for each element the shrinking touches.
   */
  private def isSimplified(element: Double): Boolean =
    java.lang.Double.doubleToLongBits(element) == ZeroBits

  //-------------------------------------------------------------------------
  /** The longest text a generator here produces, short enough for a case to be read at a glance. */
  private val MaxTextLength: Int = 24

  /** The longest text of upper-case letters a generator here produces. */
  private val MaxUpperLetterLength: Int = 12

  /**
   * The characters free text is drawn from: letters, digits and the punctuation of names.
   *
   * The punctuation is the punctuation this library's names actually hold - the hyphen of an
   * index name, the slash of a day count or a currency pair, the plus of a combined calendar,
   * the tilde of a standard identifier, the dot of a decimal, the underscore and colon of the
   * keys a properties file uses - together with the space, since several convention names
   * hold one. Random text drawn from this pool therefore looks like the text this library
   * reads, which is what makes a counterexample recognisable.
   */
  private val TextCharacters: List[Char] =
    ('A' to 'Z').toList ::: ('a' to 'z').toList ::: ('0' to '9').toList :::
      List('-', '/', '+', '~', '.', '_', ':', ' ')

  /** Generates text of one to twenty-four characters drawn from [[TextCharacters]]. */
  private val genFreeText: Gen[String] =
    for {
      length <- Gen.choose(1, MaxTextLength)
      characters <- Gen.listOfN(length, Gen.oneOf(TextCharacters))
    } yield characters.mkString

  /**
   * Generates a single character as text, which is the shortest text that is not empty.
   *
   * The one-character case is the boundary of every emptiness check in the library, so it is
   * worth reaching often rather than only when a length happens to be drawn as one.
   */
  private val genSingleCharacterText: Gen[String] =
    Gen.oneOf("A", "z", "1", "0", "-", "/", "~", " ", "\u00e9")

  /** Generates the decimal digits of a number as text, which is what a numeric field holds. */
  private val genDigitText: Gen[String] = Gen.choose(0L, 999999999L).map(_.toString)

  /**
   * Generates text made only of whitespace.
   *
   * Whitespace-only text is not empty, so the emptiness checks of this module - which test
   * `isEmpty` rather than trimming first - accept it, while the blankness checks beside them
   * reject it. That difference is the reason this branch exists: a property over
   * [[genNonEmptyText]] must see the values that separate the two checks.
   */
  private val genWhitespaceText: Gen[String] = Gen.oneOf(" ", "  ", "\t", " \t ")

  /** Generates short text made only of punctuation, which no name form uses on its own. */
  private val genPunctuationText: Gen[String] =
    Gen.oneOf("-", "/", "+", "~", ".", ",", ":", "_", "()", "[]")

  /**
   * Generates text holding characters outside ASCII.
   *
   * Written as escapes rather than as the characters themselves so that the meaning of this
   * generator does not depend on how a tool reading this file decodes it. The values are
   * `é`, `Ünïcödé`, `München`, the yen ideograph, `Ø` and `ß`.
   */
  private val genNonAsciiText: Gen[String] = Gen.oneOf(
    "\u00e9",
    "\u00dcn\u00efc\u00f6d\u00e9",
    "M\u00fcnchen",
    "\u5186",
    "\u00d8",
    "\u00df")

  /**
   * Generates the canonical name forms this library reads and writes.
   *
   * Each is a real name of the port: a day count, a floating rate index, a currency pair, a
   * tenor or period, a combined holiday calendar and a standard identifier. A validation or a
   * round trip that holds for random text but not for these is broken in the way that matters,
   * so they are drawn far more often than their number would suggest.
   */
  private val genNameFormText: Gen[String] =
    Gen.oneOf("Act/365F", "GBP-LIBOR-3M", "EUR/USD", "P3M", "GBLO+USNY", "scheme~value")

  /**
   * Generates non-empty text, of the shapes the text-bearing types of this library meet.
   *
   * Every value is between one and twenty-four characters long, so it passes the emptiness
   * check every text-bearing factory of this library starts with - the check tests `isEmpty`,
   * which whitespace-only text passes - and is short enough to read in a failure report. The
   * distribution mixes the canonical name forms of the library with single characters,
   * digits, whitespace, punctuation, text outside ASCII and free text drawn from the
   * characters those names are made of, so a property over this generator sees the text a
   * caller really supplies as well as the text that merely satisfies the type.
   *
   * @return a generator of non-empty text of at most twenty-four characters
   */
  val genNonEmptyText: Gen[String] = Gen.frequency(
    4 -> genNameFormText,
    3 -> genFreeText,
    2 -> genSingleCharacterText,
    1 -> genDigitText,
    1 -> genWhitespaceText,
    1 -> genPunctuationText,
    1 -> genNonAsciiText)

  /**
   * Generates non-empty text made only of the letters `A` to `Z`.
   *
   * This is the shape the identifier-like fields of the library require - a currency code, a
   * holiday calendar identifier, the name of a named value in a table - so it is the
   * generator a property about text that ''passes'' an upper-case-letters validation is
   * written over. Lengths run from one to twelve, which spans the single letter, the
   * three-letter currency code and the longer calendar identifiers.
   *
   * @return a generator of non-empty text of one to twelve upper-case letters
   */
  val genUpperLetterText: Gen[String] =
    for {
      length <- Gen.choose(1, MaxUpperLetterLength)
      letters <- Gen.listOfN(length, Gen.alphaUpperChar)
    } yield letters.mkString

  /**
   * Generates non-empty text holding at least one character outside `A` to `Z`.
   *
   * This is the counterpart of [[genUpperLetterText]]: a value it produces passes the
   * emptiness check and fails an upper-case-letters validation, which is what a property
   * about the ''rejecting'' branch of such a validation needs. The offending character is
   * placed between two runs of upper-case letters, either of which may be empty, so the
   * rejection is reached with the offending character at the start, in the middle and at the
   * end of the text rather than in one position only. Both a lower-case letter and a
   * character that is not a letter at all are used, since a validation written as a character
   * range and one written as a letter test differ on exactly that distinction.
   *
   * @return a generator of non-empty text that is not made only of upper-case letters
   */
  val genNonUpperText: Gen[String] =
    for {
      prefixLength <- Gen.choose(0, 6)
      prefix <- Gen.listOfN(prefixLength, Gen.alphaUpperChar)
      intruder <- Gen.oneOf('a', 'z', 'm', '1', '9', '-', '/', ' ', '_', '\u00e9')
      suffixLength <- Gen.choose(0, 6)
      suffix <- Gen.listOfN(suffixLength, Gen.alphaUpperChar)
    } yield (prefix ::: (intruder :: suffix)).mkString

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

  /**
   * Shrinks a decimal towards zero, towards the positive, and towards fewer decimal places.
   *
   * The measure reduced is twice the sum of the magnitude of the unscaled value and the
   * scale, plus one when the value is negative: `-12.3`, whose unscaled value is `-123` and
   * whose scale is one, measures `2 * (123 + 1) + 1`. The four candidates - zero, the
   * absolute value of a negative input, the value with its unscaled part halved, and the same
   * unscaled part at one scale less - each lose a sign, digits or a decimal place, so each
   * measures strictly less than the input. The measure is a non-negative whole number and
   * zero, the smallest of them, has no candidates, so repeated shrinking reaches zero in a
   * finite number of steps.
   *
   * Zero comes first, so a property that fails for every decimal reports zero immediately.
   * Halving the unscaled part is what keeps the path short in the other case, where zero is
   * not itself a counterexample and the digits have to be taken away one at a time: an
   * eighteen-digit value loses its digits in about sixty steps rather than in a number of
   * steps proportional to its magnitude.
   *
   * @return the shrinking of decimals
   */
  implicit val shrinkDecimal: Shrink[Decimal] = Shrink.withLazyList(decimalCandidates)

  /**
   * The candidates a decimal shrinks to, in the order ScalaCheck tries them.
   *
   * Every candidate is built by [[Decimal.ofScaled]], which normalises what it is given and
   * reports a value it cannot hold, so a candidate is a valid decimal on exactly the terms a
   * generated one is. Halving truncates towards zero, so the sign of a candidate is the sign
   * of the input or zero, and a value with a single digit halves to zero rather than
   * circling. The final filter drops any candidate that came out equal to the input, which
   * repeated candidates of a value near zero otherwise would.
   */
  private def decimalCandidates(value: Decimal): LazyList[Decimal] = {
    val zero = if (value.isZero) LazyList.empty else LazyList(Decimal.ZERO)
    val positive =
      if (value.signum < 0) {
        candidateOf(Decimal.ofScaled(-value.unscaledValue, value.scale))
      } else {
        LazyList.empty
      }
    val halved =
      if (value.isZero) {
        LazyList.empty
      } else {
        candidateOf(Decimal.ofScaled(value.unscaledValue / 2L, value.scale))
      }
    val fewerPlaces =
      if (value.scale > 0) {
        candidateOf(Decimal.ofScaled(value.unscaledValue, value.scale - 1))
      } else {
        LazyList.empty
      }
    (zero #::: positive #::: halved #::: fewerPlaces).distinct.filterNot(_ == value)
  }

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

  /**
   * Shrinks a fixed-scale decimal by simplifying its decimal and then by showing fewer places.
   *
   * The measure reduced is the measure of the underlying decimal, as
   * [[Arbitraries.shrinkDecimal]] defines it, plus the fixed scale. A candidate is either the
   * input paired with a simpler decimal, which reduces the first part, or the same decimal
   * shown at one place less, which reduces the second; both parts are non-negative whole
   * numbers, so repeated shrinking terminates.
   *
   * Both candidate kinds are built by [[FixedScaleDecimal.of]], so the invariant of the type
   * is checked rather than assumed: the fixed scale of a candidate still shows every digit
   * its decimal holds - no candidate of a decimal has a larger scale than the decimal it came
   * from, and the scale is reduced only while it exceeds the scale of the decimal - and it is
   * still at most the eighteen places a decimal supports.
   *
   * @return the shrinking of fixed-scale decimals
   */
  implicit val shrinkFixedScaleDecimal: Shrink[FixedScaleDecimal] =
    Shrink.withLazyList(fixedScaleDecimalCandidates)

  /** The candidates a fixed-scale decimal shrinks to, in the order ScalaCheck tries them. */
  private def fixedScaleDecimalCandidates(
      value: FixedScaleDecimal): LazyList[FixedScaleDecimal] = {

    val simplerDecimal = decimalCandidates(value.decimal)
      .flatMap(decimal => candidateOf(FixedScaleDecimal.of(decimal, value.fixedScale)))
    val fewerPlaces =
      if (value.fixedScale > value.decimal.scale) {
        candidateOf(FixedScaleDecimal.of(value.decimal, value.fixedScale - 1))
      } else {
        LazyList.empty
      }
    (simplerDecimal #::: fewerPlaces).distinct.filterNot(_ == value)
  }

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

  //-------------------------------------------------------------------------
  /**
   * Generates a deliberately large array whose elements come from the given generator.
   *
   * Every generator above this point is small on purpose - at most [[MaxArraySize]] elements -
   * because a property is about behaviour rather than throughput and a failing case has to
   * shrink to something a person can read. Smallness has a cost of its own, though: an
   * operation whose loop is wrong only beyond some length, or whose result is assembled in
   * blocks, is never put under strain by a sixteen-element input. The generators in this section
   * pay that cost deliberately. They produce arrays of [[MinLargeArraySize]] to
   * [[MaxLargeArraySize]] elements, which is large enough to run an element-wise loop over
   * hundreds of indices and cheap enough to leave a property of a hundred cases well inside a
   * second, and the shared generators above stay exactly as small and as shrink-friendly as they
   * were.
   *
   * The array is built by [[DoubleArray.copyOf]] from an immutable list, as the small generators
   * are and for the same reason: the value handed to a property shares nothing the generator can
   * still reach.
   */
  private def genLargeArrayOf(element: Gen[Double]): Gen[DoubleArray] =
    for {
      size <- Gen.choose(MinLargeArraySize, MaxLargeArraySize)
      values <- Gen.listOfN(size, element)
    } yield DoubleArray.copyOf(values)

  /**
   * Generates a large array of finite values.
   *
   * This is the generator a property about what an operation ''computes'' uses at size, since
   * finite elements let a result be compared against an independently computed expectation
   * without an IEEE-754 edge value deciding the outcome instead.
   *
   * @return a generator of arrays of finite values, of size sixty-four to five hundred and twelve
   */
  val genLargeFiniteDoubleArray: Gen[DoubleArray] = genLargeArrayOf(genFiniteDouble)

  /**
   * Generates a large array whose elements may be `NaN`, an infinity or a signed zero.
   *
   * At this size an array holds several edge values rather than at most one, which is what a
   * property about equality, hashing or rendering at size needs.
   *
   * @return a generator of arrays of finite and edge values, of size sixty-four to five hundred
   *   and twelve
   */
  val genLargeDoubleArray: Gen[DoubleArray] = genLargeArrayOf(genDouble)

  /**
   * Generates two large arrays of equal size holding finite values.
   *
   * The element-wise operations that take a second operand require the two to agree on size, so
   * a property about them at size needs a pair that agrees by construction, exactly as
   * [[genFiniteDoubleArrayPair]] provides at the small sizes.
   *
   * @return a generator of pairs of equal-sized large arrays of finite values
   */
  val genLargeFiniteDoubleArrayPair: Gen[(DoubleArray, DoubleArray)] =
    for {
      size <- Gen.choose(MinLargeArraySize, MaxLargeArraySize)
      left <- Gen.listOfN(size, genFiniteDouble)
      right <- Gen.listOfN(size, genFiniteDouble)
    } yield (DoubleArray.copyOf(left), DoubleArray.copyOf(right))

  //-------------------------------------------------------------------------
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

  /**
   * Shrinks an array by dropping its trailing element and by simplifying its elements to zero.
   *
   * The measure reduced is the size of the array plus the number of elements that are not
   * already positive zero, so `[1.0, NaN, 0.0]` measures five. A candidate is either the array
   * without its last element, which reduces the size, or the array with one element replaced
   * by `0.0`, which reduces the count; both parts are non-negative whole numbers, so repeated
   * shrinking terminates - at a single zero element, which measures one and has no candidates.
   *
   * '''A candidate is never the empty array.''' Elements are dropped down to a floor of one,
   * because [[genNonEmptyFiniteDoubleArray]] and [[genNonEmptyDoubleArray]] have that same
   * floor: the operations those generators exist for - `min`, `max`, a fold over the elements
   * - are undefined on an empty array, so a property written over one of them would otherwise
   * be "minimised" into an input its own operation rejects, and the minimised case would
   * report a precondition failure in place of the behaviour that actually broke.
   *
   * The empty array itself has no candidates, since there is nothing left to drop or to
   * simplify, and that is the one case where shrinking an array yields nothing.
   *
   * @return the shrinking of arrays
   */
  implicit val shrinkDoubleArray: Shrink[DoubleArray] = Shrink.withLazyList(arrayCandidates)

  /**
   * The candidates an array shrinks to, the shorter array first and then the simpler ones.
   *
   * The shorter candidate is taken with [[DoubleArray.subArray]] and each simplified one with
   * [[DoubleArray.tabulate]], both of which copy, so no candidate shares its backing array
   * with the value it came from or with another candidate. Only an element that is not
   * already positive zero is offered for simplification, which is what makes every candidate
   * differ from the input.
   */
  private def arrayCandidates(value: DoubleArray): LazyList[DoubleArray] = {
    val shorter =
      if (value.size > 1) LazyList(value.subArray(0, value.size - 1)) else LazyList.empty
    val simplified = LazyList
      .range(0, value.size)
      .filterNot(index => isSimplified(value.get(index)))
      .map(index => zeroedElement(value, index))
    (shorter #::: simplified).filterNot(_ == value)
  }

  /** Copies an array with the element at the index replaced by positive zero. */
  private def zeroedElement(value: DoubleArray, index: Int): DoubleArray =
    DoubleArray.tabulate(value.size)(position =>
      if (position == index) 0.0 else value.get(position))

  /**
   * Shrinks a pair of arrays in lockstep, so that the two sides keep agreeing on their size.
   *
   * The measure reduced is the sum of the measures of the two arrays. Both candidate kinds
   * move both sides at once: the trailing element is dropped from each, or one index is
   * simplified to `0.0` in each. That is what [[genFiniteDoubleArrayPair]] and
   * [[genDoubleArrayPair]] promise their consumers - the element-wise operations of the type
   * report a caller that supplies two arrays of different sizes - so a candidate that
   * shortened one side alone would be minimised into an input `plus` or `combine` rejects.
   *
   * The floor is a pair of one-element arrays, and neither side is ever emptied: the trailing
   * element is dropped only while both sides hold more than one. As with a single array, an
   * index is offered for simplification only while at least one of the two elements at it is
   * not already positive zero, so every candidate differs from the pair it came from.
   *
   * This instance outranks `Shrink.shrinkTuple2` - which would shrink the two sides
   * independently - because it is imported explicitly while that one is found in implicit
   * scope, so a property over a generated pair really is minimised this way.
   *
   * @return the shrinking of pairs of arrays
   */
  implicit val shrinkDoubleArrayPair: Shrink[(DoubleArray, DoubleArray)] =
    Shrink.withLazyList(arrayPairCandidates)

  /** The candidates a pair of arrays shrinks to, both sides moving together. */
  private def arrayPairCandidates(
      value: (DoubleArray, DoubleArray)): LazyList[(DoubleArray, DoubleArray)] = {

    val (left, right) = value
    // the generated pairs agree on size; the smaller of the two is used so that this is
    // well defined for any pair, and so that neither side can be emptied or read past
    val shared = math.min(left.size, right.size)
    val shorter =
      if (shared > 1) {
        LazyList((left.subArray(0, left.size - 1), right.subArray(0, right.size - 1)))
      } else {
        LazyList.empty
      }
    val simplified = LazyList
      .range(0, shared)
      .filterNot(index => isSimplified(left.get(index)) && isSimplified(right.get(index)))
      .map(index => (zeroedElement(left, index), zeroedElement(right, index)))
    (shorter #::: simplified).filterNot(_ == value)
  }

  //-------------------------------------------------------------------------
  /**
   * Generates a matrix of the given shape whose elements come from the given generator.
   *
   * Exactly `rows * columns` elements are drawn and laid out in row-major order through
   * [[DoubleMatrix.tabulate]], which is the factory every element-wise operation of the type
   * is defined in terms of. Building the matrix this way makes it rectangular by
   * construction and copy-safe, since the elements are read out of an immutable vector as the
   * factory fills its own rows. Every matrix these generators produce is therefore
   * rectangular, which is what the shrinkings of the type rely on when they walk a shape.
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

  //-------------------------------------------------------------------------
  /**
   * Generates a deliberately large rectangular matrix of finite values.
   *
   * This is the matrix counterpart of [[genLargeFiniteDoubleArray]], and it is here for the same
   * reason: every matrix generator above is at most [[MaxMatrixDimension]] by
   * [[MaxMatrixDimension]], so no property ever puts a row walk, a column walk or a
   * transposition under strain, and a twenty-five-element matrix cannot distinguish an operation
   * that indexes rows and columns correctly from one that happens to agree on a small square.
   * Both dimensions are drawn independently between [[MinLargeMatrixDimension]] and
   * [[MaxLargeMatrixDimension]], so the shapes generated are mostly non-square and a row count
   * confused for a column count is found rather than hidden.
   *
   * The elements are finite so that a result can be compared against an independently computed
   * expectation. Construction goes through [[DoubleMatrix.tabulate]], as the small generators do,
   * which makes the value rectangular by construction and copy-safe.
   *
   * @return a generator of matrices of finite values, of shape between eight by eight and
   *   forty-eight by forty-eight
   */
  val genLargeFiniteDoubleMatrix: Gen[DoubleMatrix] =
    for {
      rows <- Gen.choose(MinLargeMatrixDimension, MaxLargeMatrixDimension)
      columns <- Gen.choose(MinLargeMatrixDimension, MaxLargeMatrixDimension)
      matrix <- genMatrixOf(genFiniteDouble, rows, columns)
    } yield matrix

  /**
   * Generates a deliberately large square matrix of finite values.
   *
   * The operations that require a square matrix, and the properties of the ones that produce one,
   * need the shape guaranteed rather than filtered for - which at these dimensions a filter would
   * practically never satisfy, since both are drawn from a range of forty-one values.
   *
   * @return a generator of square matrices of finite values, of side eight to forty-eight
   */
  val genLargeSquareFiniteDoubleMatrix: Gen[DoubleMatrix] =
    for {
      size <- Gen.choose(MinLargeMatrixDimension, MaxLargeMatrixDimension)
      matrix <- genMatrixOf(genFiniteDouble, size, size)
    } yield matrix

  /**
   * Generates two large matrices of one shape, both holding finite values.
   *
   * The element-wise operations taking a second matrix require the two to agree on shape, so a
   * property about them at size needs a pair that agrees by construction, exactly as
   * [[genFiniteDoubleMatrixPair]] provides at the small shapes.
   *
   * @return a generator of pairs of same-shaped large matrices of finite values
   */
  val genLargeFiniteDoubleMatrixPair: Gen[(DoubleMatrix, DoubleMatrix)] =
    for {
      rows <- Gen.choose(MinLargeMatrixDimension, MaxLargeMatrixDimension)
      columns <- Gen.choose(MinLargeMatrixDimension, MaxLargeMatrixDimension)
      left <- genMatrixOf(genFiniteDouble, rows, columns)
      right <- genMatrixOf(genFiniteDouble, rows, columns)
    } yield (left, right)

  //-------------------------------------------------------------------------
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

  /**
   * Shrinks a matrix by dropping its last row and column together, and by zeroing its entries.
   *
   * The measure reduced is the row count plus the column count plus the number of entries
   * that are not already positive zero, so a two-by-two matrix of ordinary numbers measures
   * eight. A candidate is either the matrix without its last row and last column, which
   * reduces the first two parts by one each, or the matrix with one entry replaced by `0.0`,
   * which reduces the third; all three are non-negative whole numbers, so repeated shrinking
   * terminates. The value it terminates at holds nothing but zeroes and has one of its
   * dimensions reduced to one: a one-by-one zero matrix when the input was square, and a
   * single zero row or column when it was not, since the shape rule below stops as soon as
   * either dimension reaches one.
   *
   * '''The only shape reduction is dropping a row and a column together, down to a floor of
   * one by one.''' [[genSquareFiniteDoubleMatrix]] and [[genSquareDoubleMatrix]] guarantee a
   * square matrix, and the properties written over them - a transposition being its own
   * inverse, multiplication by an identity having no effect - are stated only of square
   * matrices, so a candidate that dropped a row alone would be minimised into an input those
   * properties do not apply to. A matrix that is not square keeps its shape and is minimised
   * by its entries alone once one of its dimensions reaches one, which costs nothing: the
   * entries are where a counterexample is read.
   *
   * Every candidate is built by [[DoubleMatrix.tabulate]], so it is rectangular by
   * construction and copy-safe. The empty matrix has no candidates, and neither has a matrix
   * whose rows differ in length: `DoubleMatrix.copyOf` shapes a ragged array by its first row
   * and leaves every other row at its own length, so such a value states a column count that
   * one of its rows may not reach. Minimising it would mean reading a position that is not
   * there, and there is no smaller shape of the same kind to reduce it to, so it is offered as
   * the floor it already is.
   *
   * @return the shrinking of matrices
   */
  implicit val shrinkDoubleMatrix: Shrink[DoubleMatrix] = Shrink.withLazyList(matrixCandidates)

  /** The candidates a matrix shrinks to, the smaller shape first and then the simpler ones. */
  private def matrixCandidates(value: DoubleMatrix): LazyList[DoubleMatrix] =
    if (!isRectangular(value)) {
      // the shape states a column count a row of this value does not reach, for the reason
      // `isRectangular` sets out; such a value is the floor it already is
      LazyList.empty
    } else {
      val smaller =
        if (value.rowCount > 1 && value.columnCount > 1) {
          LazyList(DoubleMatrix.tabulate(value.rowCount - 1, value.columnCount - 1)(value.get))
        } else {
          LazyList.empty
        }
      val simplified = for {
        row <- LazyList.range(0, value.rowCount)
        column <- LazyList.range(0, value.columnCount)
        if !isSimplified(value.get(row, column))
      } yield zeroedEntry(value, row, column)
      (smaller #::: simplified).filterNot(_ == value)
    }

  /**
   * Whether every row of a matrix is as long as the column count its shape states.
   *
   * Every matrix these generators yield is rectangular: each one is built by
   * [[DoubleMatrix.tabulate]] from a row count and a column count, so every row holds exactly
   * as many elements as the shape states, and a ragged shape is drawn by none of them. A
   * ragged value reaches this type by one route only - `DoubleMatrix.copyOf` shapes an array
   * whose rows differ in length by its first row and clones every other row at its own length
   * - so a value of this type can state a column count one of its rows does not reach. Reading
   * such a position is an index failure, and a shrinking that walked the stated shape would
   * replace a counterexample with an exception raised by the minimisation itself. Both matrix
   * shrinkings therefore ask this question before they read anything, and offer no candidate
   * when the answer is no, which is what lets the specs that draw from the generators state
   * their properties over rectangular matrices alone.
   *
   * The rows are measured through `row`, which hands back each row at its own length, so the
   * question is answered without reading a position that may not exist. The empty matrix has
   * no row to measure and is rectangular.
   *
   * @param value  the matrix to measure
   * @return true if every row holds exactly as many elements as the shape states
   */
  private def isRectangular(value: DoubleMatrix): Boolean =
    LazyList.range(0, value.rowCount).forall(row => value.row(row).size == value.columnCount)

  /** Copies a matrix with the entry at the position replaced by positive zero. */
  private def zeroedEntry(value: DoubleMatrix, row: Int, column: Int): DoubleMatrix =
    DoubleMatrix.tabulate(value.rowCount, value.columnCount) { (entryRow, entryColumn) =>
      if (entryRow == row && entryColumn == column) 0.0 else value.get(entryRow, entryColumn)
    }

  /**
   * Shrinks a pair of matrices in lockstep, so that the two sides keep agreeing on their shape.
   *
   * The measure reduced is the sum of the measures of the two matrices, and both candidate
   * kinds move both sides at once: the last row and column are dropped from each, or one
   * position is zeroed in each. That agreement is what [[genFiniteDoubleMatrixPair]] and
   * [[genDoubleMatrixPair]] promise their consumers, since `plus`, `minus` and `combine`
   * report a caller that supplies two matrices of different shapes, and it is also what keeps
   * a pair of square matrices square.
   *
   * The floor is a pair of one-by-one matrices; a dimension is reduced only while both sides
   * hold more than one row and more than one column, so neither side is ever emptied.
   *
   * This instance outranks `Shrink.shrinkTuple2`, which would shrink the two sides
   * independently, because it is imported explicitly while that one is found in implicit
   * scope.
   *
   * @return the shrinking of pairs of matrices
   */
  implicit val shrinkDoubleMatrixPair: Shrink[(DoubleMatrix, DoubleMatrix)] =
    Shrink.withLazyList(matrixPairCandidates)

  /** The candidates a pair of matrices shrinks to, both sides moving together. */
  private def matrixPairCandidates(
      value: (DoubleMatrix, DoubleMatrix)): LazyList[(DoubleMatrix, DoubleMatrix)] = {

    val (left, right) = value
    if (!isRectangular(left) || !isRectangular(right)) {
      // one side states a column count a row of it does not reach, for the reason
      // `isRectangular` sets out; a pair holding such a side is the floor it already is
      LazyList.empty
    } else {
      // the generated pairs agree on shape; the smaller of each dimension is used so that this
      // is well defined for any pair, and so that no position is read outside either side
      val sharedRows = math.min(left.rowCount, right.rowCount)
      val sharedColumns = math.min(left.columnCount, right.columnCount)
      val smaller =
        if (sharedRows > 1 && sharedColumns > 1) {
          LazyList(
            (
              DoubleMatrix.tabulate(left.rowCount - 1, left.columnCount - 1)(left.get),
              DoubleMatrix.tabulate(right.rowCount - 1, right.columnCount - 1)(right.get)))
        } else {
          LazyList.empty
        }
      val simplified = for {
        row <- LazyList.range(0, sharedRows)
        column <- LazyList.range(0, sharedColumns)
        if !(isSimplified(left.get(row, column)) && isSimplified(right.get(row, column)))
      } yield (zeroedEntry(left, row, column), zeroedEntry(right, row, column))
      (smaller #::: simplified).filterNot(_ == value)
    }
  }

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
   * Shrinks a failure by dropping its attributes and then by shortening its message.
   *
   * The measure reduced is the number of attributes plus the number of words in the message,
   * both non-negative whole numbers, so repeated shrinking terminates - at a failure with no
   * attributes and a one-word message, which has no candidates. The candidates are, in the
   * order ScalaCheck tries them: the failure with no attributes at all, the failure with one
   * attribute removed for each attribute it holds, the failure whose message is its first
   * word, and the failure whose message has lost its last word.
   *
   * '''The reason is never changed.''' A reason is the identity of a failure here - it decides
   * which member of the closed family the value is, and a property about parsing failures or
   * about missing data is written over [[genFailureWithReason]] - so a candidate carrying a
   * different reason would be minimised into a value of a different kind from the one that
   * failed. Every candidate goes through [[Failure.of]] with the reason of the input, which is
   * the factory that maps a reason onto its member.
   *
   * '''A candidate message is never empty.''' It is either the first word of the message or
   * the message without its last word, and a word is non-empty by construction, so the
   * property of `FailureSpec` asserting that a collapsed chain has a non-empty message -
   * which holds because no generated message is empty - still holds of every minimised case.
   *
   * @return the shrinking of failures
   */
  implicit val shrinkFailure: Shrink[Failure] = Shrink.withLazyList(failureCandidates)

  /**
   * The candidates a failure shrinks to, the attribute reductions before the message ones.
   *
   * Attributes are reduced first because they are the part a reader of a failing case skips:
   * a failure of two attributes and a four-word message is easier to read once the attributes
   * are gone, and dropping them cannot change which member of the family the value is. The
   * message is reduced only down to a single word, never to nothing.
   */
  private def failureCandidates(value: Failure): LazyList[Failure] = {
    val words = wordsOf(value.message)
    val withoutAttributes =
      if (value.attributes.nonEmpty) {
        LazyList(Failure.of(value.reason, value.message, SortedMap.empty[String, String]))
      } else {
        LazyList.empty
      }
    val fewerAttributes = LazyList
      .from(value.attributes.keys)
      .map(key => Failure.of(value.reason, value.message, value.attributes - key))
    val shorterMessage =
      if (words.sizeIs > 1) {
        LazyList(
          Failure.of(value.reason, words.head, value.attributes),
          Failure.of(value.reason, words.init.mkString(" "), value.attributes))
      } else {
        LazyList.empty
      }
    (withoutAttributes #::: fewerAttributes #::: shorterMessage).distinct.filterNot(_ == value)
  }

  /**
   * The non-empty words of a message, which is what the shrinking measure of a failure counts.
   *
   * Splitting on the space and dropping the empty parts means the count does not depend on
   * how many spaces separate two words, so joining the words back with a single space - which
   * is how a shortened message is built - reduces the count by exactly the number of words
   * dropped.
   */
  private def wordsOf(message: String): List[String] =
    message.split(' ').iterator.filter(_.nonEmpty).toList

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

  /**
   * Perturbs a seed by a chain of failures, through the rendering of each failure in order.
   *
   * The rendering is the same injective one [[Arbitraries.cogenFailure]] perturbs by, applied
   * to the failures in the order the chain holds them, so two chains perturb a seed
   * identically exactly when they hold equal failures in equal order - which is the equality
   * of the chain itself. A function of a chain of failures, which a law suite over an
   * accumulating outcome needs, is generated from this instance and [[arbFailures]].
   *
   * @return the cogenerator of chains of failures
   */
  implicit val cogenFailures: Cogen[NonEmptyChain[Failure]] =
    implicitly[Cogen[List[String]]]
      .contramap[NonEmptyChain[Failure]](_.toNonEmptyList.toList.map(renderingOf))

  /**
   * Shrinks a chain of failures by dropping failures and by simplifying the ones that remain.
   *
   * The measure reduced is the number of failures in the chain plus the sum of the measures of
   * those failures, as [[Arbitraries.shrinkFailure]] defines them. A candidate is the chain of
   * its first failure alone, the chain without its last failure, or the chain with one of its
   * failures replaced by a candidate of that failure; the first two reduce the count and the
   * third reduces the sum, and both parts are non-negative whole numbers, so repeated
   * shrinking terminates - at a chain of one attribute-free single-word failure.
   *
   * '''A candidate chain is never empty.''' Failures are dropped down to a floor of one,
   * which is what the type itself guarantees: the accumulating combinators and
   * `Failure.collapse` are defined only on a chain that holds a failure, so there is no
   * smaller value to shrink towards and an empty candidate could not even be built.
   *
   * Offering the first failure on its own before offering the chain without its last failure
   * is what makes a long chain minimise quickly: a property that fails because of any single
   * failure in the chain reaches a chain of one in a couple of steps rather than in as many
   * steps as the chain is long.
   *
   * @return the shrinking of chains of failures
   */
  implicit val shrinkFailures: Shrink[NonEmptyChain[Failure]] =
    Shrink.withLazyList(failuresCandidates)

  /** The candidates a chain of failures shrinks to, the shorter chains before the simpler ones. */
  private def failuresCandidates(
      value: NonEmptyChain[Failure]): LazyList[NonEmptyChain[Failure]] = {

    val all = value.toNonEmptyList.toList
    val shorter =
      if (all.sizeIs > 1) {
        // `all` holds at least two failures here, so both of these chains hold at least one
        LazyList(chainOf(all.head, Nil), chainOf(all.head, all.init.tail))
      } else {
        LazyList.empty
      }
    val simplified = for {
      index <- LazyList.range(0, all.size)
      candidate <- failureCandidates(all(index))
      updated = all.updated(index, candidate)
    } yield chainOf(updated.head, updated.tail)
    (shorter #::: simplified).distinct.filterNot(_ == value)
  }

  /**
   * Rebuilds a chain from a head and a tail, which is how every candidate chain is made.
   *
   * Taking the head separately is what makes the non-emptiness of the result a fact about the
   * arguments rather than something to check: there is no route here through an `Option` to
   * unwrap or a rejected outcome to force.
   */
  private def chainOf(head: Failure, tail: List[Failure]): NonEmptyChain[Failure] =
    NonEmptyChain.of(head, tail: _*)

  //-------------------------------------------------------------------------
  // The four outcome types of this module.
  //
  // Their names are written here unqualified - `FailureOr`, `ResultNec`,
  // `ValidatedFailures`, `ValueWithFailures` - and that is deliberate rather
  // than incidental. This file is in package `com.opengamma.strata.collect`, so
  // an unqualified name resolves to the re-export the module root declares in
  // its package object, which is the import route this library asks its callers
  // to take. Naming them that way makes this file a consumer of that re-export,
  // so the four aliases are exercised by the compiler rather than merely
  // declared. Nothing here imports them from the `result` package, and nothing
  // qualifies them: a file that saw both paths at once would see each of the
  // four names twice and every use of one would be ambiguous.
  //
  // Each generator takes the generator of the value side as a parameter rather
  // than an implicit `Arbitrary`, so a spec can put a domain generator on the
  // value side - a decimal, an array, a list of parsed rows - and still get the
  // failure side and the distribution over shapes from here. The implicit
  // `Arbitrary` of each is that generator applied to the arbitrary of the value
  // type, for the properties that need nothing more specific.
  //-------------------------------------------------------------------------
  /**
   * Generates an outcome holding either a value or the single failure that explains its absence.
   *
   * The two shapes arise about equally often, so a property over this generator sees both
   * branches of every combinator it exercises without the frequency having to be argued about.
   * The failure side carries one failure, as the type does: this is the outcome of an operation
   * that stops at the first thing wrong rather than accumulating.
   *
   * @param values  the generator of the value side
   * @tparam A  the type of the value produced when the operation succeeds
   * @return a generator of outcomes carrying a single failure
   */
  def genFailureOr[A](values: Gen[A]): Gen[FailureOr[A]] = Gen.frequency(
    1 -> genFailure.map(failure => Left(failure): FailureOr[A]),
    1 -> values.map(value => Right(value): FailureOr[A]))

  /**
   * The implicit generator of single-failure outcomes over an arbitrary value type.
   *
   * @tparam A  the type of the value produced when the operation succeeds
   * @return the arbitrary outcome carrying a single failure
   */
  implicit def arbFailureOr[A: Arbitrary]: Arbitrary[FailureOr[A]] =
    Arbitrary(genFailureOr(Arbitrary.arbitrary[A]))

  /**
   * Generates an outcome holding either a value or a chain of the failures that stopped it.
   *
   * This is the outcome a validating factory of this library answers with, so it is the most
   * frequently generated of the four. The failure side is built by [[genFailures]], so it
   * holds one to four failures and a property over it sees both the single-cause case and the
   * several-cause case that the accumulating combinators exist for.
   *
   * @param values  the generator of the value side
   * @tparam A  the type of the value produced when every check passes
   * @return a generator of outcomes carrying a chain of failures
   */
  def genResultNec[A](values: Gen[A]): Gen[ResultNec[A]] = Gen.frequency(
    1 -> genFailures.map(failures => Left(failures): ResultNec[A]),
    1 -> values.map(value => Right(value): ResultNec[A]))

  /**
   * The implicit generator of chain-carrying results over an arbitrary value type.
   *
   * @tparam A  the type of the value produced when every check passes
   * @return the arbitrary result carrying a chain of failures
   */
  implicit def arbResultNec[A: Arbitrary]: Arbitrary[ResultNec[A]] =
    Arbitrary(genResultNec(Arbitrary.arbitrary[A]))

  /**
   * Generates the accumulating form of a result: a valid value, or a chain of failures.
   *
   * The shapes are the two of `Validated` and arise about equally often. This is the form
   * several checks over one input are combined in, so a property over this generator is what
   * establishes that combining accumulates rather than short-circuits: the chains of two
   * invalid values have to appear in the outcome one after the other.
   *
   * @param values  the generator of the valid side
   * @tparam A  the type of the value produced when every check passes
   * @return a generator of accumulating outcomes
   */
  def genValidatedFailures[A](values: Gen[A]): Gen[ValidatedFailures[A]] = Gen.frequency(
    1 -> genFailures.map(failures => Validated.invalid[NonEmptyChain[Failure], A](failures)),
    1 -> values.map(value => Validated.valid[NonEmptyChain[Failure], A](value)))

  /**
   * The implicit generator of accumulating outcomes over an arbitrary value type.
   *
   * @tparam A  the type of the value produced when every check passes
   * @return the arbitrary accumulating outcome
   */
  implicit def arbValidatedFailures[A: Arbitrary]: Arbitrary[ValidatedFailures[A]] =
    Arbitrary(genValidatedFailures(Arbitrary.arbitrary[A]))

  /**
   * Generates the outcome of an operation that can partly succeed: failures, a value, or both.
   *
   * All three shapes of the underlying `Ior` are reachable and arise about equally often:
   * failures alone, where nothing usable came out; a value alone, where nothing was worth
   * reporting; and both, where part of the work succeeded and part did not. The third shape is
   * the reason this type exists - a file of which some rows parsed, a curve of which some
   * points calibrated - and a property that only ever saw the other two would pass while
   * leaving the interesting half of every combinator untested, so it is generated as often as
   * they are rather than as an afterthought.
   *
   * @param values  the generator of the value side
   * @tparam A  the type of the value, typically a collection type
   * @return a generator of partly-successful outcomes, of all three shapes
   */
  def genValueWithFailures[A](values: Gen[A]): Gen[ValueWithFailures[A]] = Gen.frequency(
    1 -> genFailures.map(failures => Ior.left[NonEmptyChain[Failure], A](failures)),
    1 -> values.map(value => Ior.right[NonEmptyChain[Failure], A](value)),
    1 -> Gen.zip(genFailures, values).map { case (failures, value) => Ior.both(failures, value) })

  /**
   * The implicit generator of partly-successful outcomes over an arbitrary value type.
   *
   * @tparam A  the type of the value, typically a collection type
   * @return the arbitrary partly-successful outcome
   */
  implicit def arbValueWithFailures[A: Arbitrary]: Arbitrary[ValueWithFailures[A]] =
    Arbitrary(genValueWithFailures(Arbitrary.arbitrary[A]))

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

  /**
   * Shrinks a member of the sample family towards the first member declared.
   *
   * The measure reduced is the position of the member in [[SampleNamed.values]], and the
   * candidates are the members that precede it there, so `Spaced` offers the four members
   * declared before it and `Standard`, the first, offers none. The measure is a non-negative
   * whole number, so repeated shrinking terminates at that first member.
   *
   * Declaration order is the right order to shrink along because it runs from the plain names
   * to the awkward ones: the first members are single words, the last hold a hyphen and a
   * space. A property that fails for every member therefore reports the plainest one, while a
   * property that fails only on the punctuated names keeps the name that broke it.
   *
   * @return the shrinking of members of the sample family
   */
  implicit val shrinkSampleNamed: Shrink[SampleNamed] =
    Shrink.withLazyList(sampleNamedCandidates)

  /**
   * The candidates a member of the sample family shrinks to, in declaration order.
   *
   * A value that is not a member of `values` - which the sealed family makes impossible -
   * would be reported as absent by `indexOf` and offer no candidate at all, rather than
   * offering every member as `takeWhile` on an absent value would.
   */
  private def sampleNamedCandidates(value: SampleNamed): LazyList[SampleNamed] = {
    val members = SampleNamed.values.toList
    LazyList.from(members.take(math.max(members.indexOf(value), 0)))
  }
}

/**
 * Tests [[Arbitraries]], the shared ScalaCheck surface of this module.
 *
 * Every other property in both Scala modules is written over the generators, cogenerators and
 * shrinkings that object publishes, so what those properties establish is only as good as what
 * it produces. That is why this suite exists and why it asserts by ''executing'' the instances
 * rather than by reading them: a generator that quietly stopped drawing `NaN`, that produced a
 * decimal outside the range of its type, or a shrinking that minimised a failing case into a
 * value its own operation rejects, would leave every property built on it green while it
 * asserted far less than it reads as asserting.
 *
 * ===What is established here===
 *
 * Four things, for every member of that object:
 *
 *   - '''the documented edge values are reached'''. `NaN`, both infinities and both signed
 *     zeroes come out of the element generators and out of the edge-bearing array, pair and
 *     matrix generators; zero, both published extremes, the largest unscaled value and every
 *     supported scale come out of the decimal generator; the empty and the largest collection
 *     both arise wherever a generator admits both, and at least one element arises wherever one
 *     does not; every canonical name form, whitespace, punctuation, digits and text outside
 *     ASCII come out of the text generator; every failure reason, a chain of one failure and a
 *     chain of several, and every shape of each of the four outcome types, all arise.
 *   - '''the generated values are valid'''. A decimal is in range and normalised, a fixed-scale
 *     decimal shows every digit its decimal holds and no more than eighteen places, an array
 *     and a matrix stay inside the size and shape bounds documented for the generator that
 *     drew them, a matrix is rectangular and a square one is square, a pair agrees on size or
 *     shape, a large value is larger than any small one the neighbouring generator could have
 *     produced, a failure carries the reason its member is for with a non-empty message and
 *     attributes in key order, and a chain of failures is non-empty.
 *   - '''the shrinkings are well behaved'''. Every candidate of every instance is a valid
 *     value, is different from the value it came from, is strictly smaller under the measure
 *     the instance documents, and comes from a finite list of candidates. Repeated shrinking is
 *     followed to a fixed point under a bounded loop, which is what makes termination a fact
 *     here rather than an argument, and the fixed point reached is the floor the instance
 *     documents.
 *   - '''the cogenerators agree exactly where the values do'''. Two equal values perturb one
 *     seed identically and two different values do not, checked over values the generators
 *     reach rather than over a handful of literals.
 *
 * ===The measures are recomputed here===
 *
 * Each shrinking of `Arbitraries` documents the measure its candidates reduce. This suite
 * computes those measures itself, from the parts of the value, rather than asking the instance
 * for them: a measure computed by the code under test could agree with a broken shrinking,
 * while one written out from the documented definition cannot. The functions below are
 * therefore a transcription of that documentation and are deliberately not shared with the
 * object they check, and the bounds they are checked against are restated here for the same
 * reason.
 *
 * ===Determinism, and the two pinned seeds===
 *
 * Every case is a pure function of literal data or of generated data drawn from the generators
 * being tested. Nothing reads a clock, a file or the environment, and no case shares mutable
 * state with another, so the outcome does not depend on the order the cases run in.
 *
 * The cases that assert a generator ''reaches'' a particular value are where that needs care,
 * and each of them is written over a sample drawn from the literal `SampleSeed`, `20260911153000`,
 * rather than as a property. A property draws a fresh sample every run, so such a case would
 * fail for a correct generator once in some large number of runs - rarely enough never to be
 * reproduced, often enough to be seen - and a test that can do that is not deterministic. The
 * fixed sample makes each of those cases pass in every run or fail in every run. The cases
 * about the cogenerators perturb the literal `CogenSeed`, `4132297845901`, for the same reason.
 * Both seeds are stated here so that a failure is replayable: drawing
 * `Gen.listOfN(count, generator).pureApply(Gen.Parameters.default, Seed(20260911153000L))` in a
 * console reproduces exactly the sample the failing case saw.
 *
 * ===Why this suite shares a file with its subject===
 *
 * The target layout of the migration lists one file for the generator surface of this module
 * and none for its spec, and that layout is authoritative, so this suite is declared beside the
 * object it tests rather than in a file of its own. Nothing else about it differs from a spec
 * with its own file: it is an ordinary suite in the ordinary package, it is discovered and
 * reported under its own name, and the generator surface it draws on is reached through the
 * import at the head of the class rather than through a file-level one, so no member of
 * [[Arbitraries]] is ever shadowed inside the object that declares it.
 *
 * @see [[Arbitraries]] for the instances under test
 * @see [[SampleNamed]] for the sample named family generated there
 */
final class ArbitrariesSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  import Arbitraries._

  // ---------------------------------------------------------------------------
  // Fixtures and bounds.
  //
  // The bounds are the ones `Arbitraries` documents: sixteen elements, five rows
  // and columns, sixty-four to five hundred and twelve elements and eight to
  // forty-eight rows and columns for the large generators, three attributes,
  // four failures in a chain, twenty-four characters of text and twelve
  // upper-case letters. They are restated here rather than read from that
  // object, since it holds them privately and since a suite that read its
  // expectations from its subject would pass whatever the subject did.
  // ---------------------------------------------------------------------------

  /** The largest array a shared generator of that object produces. */
  private val MaxArraySize: Int = 16

  /** The largest number of rows, or of columns, a shared generated matrix has. */
  private val MaxMatrixDimension: Int = 5

  /** The smallest array a ''large'' generator produces, above the shared ceiling. */
  private val MinLargeArraySize: Int = 64

  /** The largest array a ''large'' generator produces. */
  private val MaxLargeArraySize: Int = 512

  /** The smallest dimension a ''large'' generated matrix has, above the shared ceiling. */
  private val MinLargeMatrixDimension: Int = 8

  /** The largest dimension a ''large'' generated matrix has. */
  private val MaxLargeMatrixDimension: Int = 48

  /** The largest number of attributes a generated failure carries. */
  private val MaxAttributes: Int = 3

  /** The largest number of failures a generated chain holds. */
  private val MaxChainSize: Int = 4

  /** The longest text [[Arbitraries.genNonEmptyText]] produces. */
  private val MaxTextLength: Int = 24

  /** The longest text [[Arbitraries.genUpperLetterText]] produces. */
  private val MaxUpperLetterLength: Int = 12

  /** The largest magnitude [[Arbitraries.genFiniteDouble]] draws, being its widest range. */
  private val MaxFiniteMagnitude: Double = 1.0e9

  /** The canonical name forms [[Arbitraries.genNonEmptyText]] documents drawing. */
  private val NameForms: List[String] =
    List("Act/365F", "GBP-LIBOR-3M", "EUR/USD", "P3M", "GBLO+USNY", "scheme~value")

  /** The characters the punctuation branch of the text generator draws from. */
  private val PunctuationCharacters: Set[Char] =
    Set('-', '/', '+', '~', '.', ',', ':', '_', '(', ')', '[', ']')

  /** The keys [[Arbitraries.genFailureAttributes]] documents drawing from. */
  private val AttributeKeys: List[String] =
    List("value", "name", "id", "reason", "exceptionType", "scale")

  /** The four phrases [[Arbitraries.genFailureMessage]] begins a message with. */
  private val MessagePrefixes: List[String] = List(
    "Unable to resolve",
    "Invalid input for",
    "Unsupported operation on",
    "Missing data for")

  /** The five IEEE-754 values the equality design of the port turns on. */
  private val EdgeValues: List[Double] =
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, -0.0, 0.0)

  /**
   * The bit patterns of [[EdgeValues]], which is how a case asserts one was reached.
   *
   * Bit patterns rather than values, because that is the equality the double-bearing types use
   * and because `NaN` compares equal to nothing at all under `==`: a case asking whether a
   * sample holds `NaN`, or whether it holds a negative zero as distinct from a positive one,
   * cannot ask with `contains` on the values themselves.
   */
  private val EdgeBits: List[Long] = EdgeValues.map(element => bitsOf(element))

  /**
   * The number of shrinking steps every termination case is bounded by.
   *
   * The longest path any instance here takes is the nineteen steps a fixed-scale decimal needs
   * - one to reach zero and eighteen to drop its places - so a bound of two hundred is far
   * above what a terminating shrinking uses and far below what a circling one would.
   */
  private val StepBound: Int = 200

  /**
   * The seed every cogeneration case perturbs.
   *
   * A literal, so that the perturbations compared in a case are compared under one seed and the
   * case gives the same answer on every run.
   */
  private val CogenSeed: Seed = Seed(4132297845901L)

  /**
   * The seed every reachability sample below is drawn from.
   *
   * A case that asserts a generator ''reaches'' a particular value cannot be written as an
   * ordinary property: a property draws a fresh sample each time it runs, so a correct
   * generator would omit the value asked for once in some large number of runs and the case
   * would fail for no reason at all. Drawing the sample from a literal seed instead makes such
   * a case a function of the generator alone - it passes in every run or it fails in every run
   * - which is the determinism the specs of this module are held to.
   */
  private val SampleSeed: Seed = Seed(20260911153000L)

  /** The number of values a reachability case draws when the value asked for is a common one. */
  private val Draws: Int = 200

  /**
   * The number of values a reachability case draws when the value asked for is a rare one.
   *
   * A five-by-five shape is one of thirty-six, a text form one of several branches of a
   * frequency, and an edge value one of five inside one branch in four, so those cases draw
   * more widely. Even the widest sample here is a few hundred small values and costs
   * milliseconds.
   */
  private val WideDraws: Int = 500

  /**
   * The number of decimals a reachability case draws.
   *
   * The scale of a generated decimal is one of nineteen inside one branch, and the case below
   * asserts that every one of the nineteen is reached, so this sample is the largest of them.
   */
  private val DecimalDraws: Int = 1000

  /** The number of ''large'' values a case draws, each holding hundreds of elements. */
  private val LargeDraws: Int = 20

  /**
   * Draws a fixed sample from a generator, identically in every run.
   *
   * The sample is the one the literal [[SampleSeed]] produces, so a case written over it sees
   * exactly the same values on every run. `pureApply` is the route taken because none of these
   * generators rejects an input - each is built from the factories of its type over ranges those
   * factories accept - so no draw is discarded and the sample is always the size asked for.
   *
   * @param values  the generator to draw from
   * @param count  the number of values to draw
   * @tparam A  the type of value generated
   * @return the sample, which is the same one on every run for a given generator and count
   */
  private def sampleOf[A](values: Gen[A], count: Int): List[A] =
    Gen.listOfN(count, values).pureApply(Gen.Parameters.default, SampleSeed)

  // ---------------------------------------------------------------------------
  // The measures, transcribed from the documentation of each shrinking.
  // ---------------------------------------------------------------------------

  /** The bit pattern of a `double`, which is the identity both double-bearing types use. */
  private def bitsOf(element: Double): Long = java.lang.Double.doubleToLongBits(element)

  /**
   * Whether an element is the positive zero the double-bearing shrinkings simplify towards.
   *
   * Bit patterns rather than `==`, because that is the equality both double-bearing types use:
   * this reports false for a negative zero, which is a different value of an array, and false
   * for `NaN`, which `==` reports as equal to nothing at all.
   */
  private def isSimplifiedElement(element: Double): Boolean = bitsOf(element) == bitsOf(0.0)

  /**
   * Whether an element is one of the edge values a finite generator cannot have drawn.
   *
   * A positive zero is deliberately not counted: it is drawn by the finite generator as well as
   * by the edge one, so its presence is no evidence that the edge branch was taken. `NaN`, the
   * two infinities and the negative zero are drawn by the edge branch alone.
   */
  private def isEdgeElement(element: Double): Boolean =
    !element.isFinite || bitsOf(element) == bitsOf(-0.0)

  /** Twice the magnitude of the unscaled value plus the scale, and one more when negative. */
  private def decimalMeasure(value: Decimal): Long =
    2L * (math.abs(value.unscaledValue) + value.scale.toLong) + (if (value.signum < 0) 1L else 0L)

  /** The measure of the decimal plus the scale it is presented at. */
  private def fixedScaleMeasure(value: FixedScaleDecimal): Long =
    decimalMeasure(value.decimal) + value.fixedScale.toLong

  /** The size of the array plus the number of elements that are not already positive zero. */
  private def arrayMeasure(value: DoubleArray): Long =
    value.size.toLong + value.toList.count(element => !isSimplifiedElement(element)).toLong

  /** The measures of the two sides of a pair of arrays, added. */
  private def arrayPairMeasure(value: (DoubleArray, DoubleArray)): Long =
    arrayMeasure(value._1) + arrayMeasure(value._2)

  /** The entries of a matrix in row-major order, which the measure below counts. */
  private def entriesOf(value: DoubleMatrix): List[Double] =
    List.tabulate(value.rowCount)(row => value.row(row).toList).flatten

  /** The row count plus the column count plus the number of entries that are not zero. */
  private def matrixMeasure(value: DoubleMatrix): Long =
    value.rowCount.toLong + value.columnCount.toLong +
      entriesOf(value).count(entry => !isSimplifiedElement(entry)).toLong

  /** The measures of the two sides of a pair of matrices, added. */
  private def matrixPairMeasure(value: (DoubleMatrix, DoubleMatrix)): Long =
    matrixMeasure(value._1) + matrixMeasure(value._2)

  /** The words of a message, separated by spaces, the empty parts dropped. */
  private def wordsOf(message: String): List[String] =
    message.split(' ').iterator.filter(_.nonEmpty).toList

  /** The number of attributes plus the number of words in the message. */
  private def failureMeasure(value: Failure): Long =
    value.attributes.size.toLong + wordsOf(value.message).size.toLong

  /** The failures of a chain as a list, which is the order the chain holds them in. */
  private def failuresOf(value: NonEmptyChain[Failure]): List[Failure] =
    value.toNonEmptyList.toList

  /** The number of failures plus the measures of all of them. */
  private def failuresMeasure(value: NonEmptyChain[Failure]): Long =
    failuresOf(value).size.toLong + failuresOf(value).map(failure => failureMeasure(failure)).sum

  /** The position of a member in the declaration order of the sample family. */
  private def sampleNamedMeasure(value: SampleNamed): Long =
    SampleNamed.values.toList.indexOf(value).toLong

  // ---------------------------------------------------------------------------
  // Helpers that assert the rules over one value.
  // ---------------------------------------------------------------------------

  /**
   * Asserts that every candidate of a value is valid, different and strictly smaller.
   *
   * Reading the candidates as a list is itself part of the assertion: a shrinking that offered
   * candidates without end would not return from here, and the bound on the size of the list
   * pins the number each instance is documented to offer.
   *
   * @param value  the value to shrink
   * @param measure  the measure the candidates must reduce
   * @param bound  the largest number of candidates the instance offers for this value
   * @param check  the validity the candidates must have
   * @return the assertion that every candidate obeys all three rules
   */
  private def assertCandidates[A](
      value: A,
      measure: A => Long,
      bound: Int,
      check: A => Assertion)(implicit shrink: Shrink[A]): Assertion = {

    val candidates = Shrink.shrink(value).toList
    withClue(s"shrinking '$value' gave ${candidates.size} candidates: ") {
      candidates.size should be <= bound
      candidates.foreach { candidate =>
        withClue(s"candidate '$candidate': ") {
          check(candidate)
          candidate should not be value
          measure(candidate) should be < measure(value)
        }
      }
      succeed
    }
  }

  /**
   * Follows the first candidate of a value repeatedly until none is offered.
   *
   * The loop is bounded, so a shrinking that circled between two values, or that offered a
   * candidate for every value it produced, returns the bound rather than running forever - and
   * the case that called it fails on the step count instead of hanging.
   *
   * @param value  the value to start from
   * @param steps  the number of steps taken so far
   * @return the value reached and the number of steps it took to reach it
   */
  @tailrec
  private def fixedPointOf[A](value: A, steps: Int)(implicit shrink: Shrink[A]): (A, Int) =
    if (steps >= StepBound) {
      (value, steps)
    } else {
      Shrink.shrink(value).headOption match {
        case Some(candidate) => fixedPointOf(candidate, steps + 1)
        case None => (value, steps)
      }
    }

  /**
   * Asserts that repeated shrinking of a value terminates, and at the value expected.
   *
   * @param value  the value to shrink repeatedly
   * @param check  the floor the shrinking is expected to reach
   * @return the assertion that a fixed point is reached within the bound and is that floor
   */
  private def assertTerminates[A](value: A, check: A => Assertion)(
      implicit shrink: Shrink[A]): Assertion = {

    val (floor, steps) = fixedPointOf(value, 0)
    withClue(s"shrinking '$value' reached '$floor' in $steps steps: ") {
      steps should be < StepBound
      Shrink.shrink(floor).toList shouldBe empty
      check(floor)
    }
  }

  /** Perturbs the one seed of this suite by a value, and reads the number that comes out. */
  private def perturbationOf[A](value: A)(implicit cogen: Cogen[A]): Long =
    cogen.perturb(CogenSeed, value).long._1

  /**
   * Asserts that no candidate of a value perturbs the seed the way the value itself does.
   *
   * Every candidate is a different value of the type - the cases above establish that - so this
   * is the disagreement half of the cogeneration contract, checked over values the generators
   * reach rather than over a handful of literals.
   */
  private def assertDistinctPerturbations[A](value: A)(
      implicit shrink: Shrink[A],
      cogen: Cogen[A]): Assertion = {

    Shrink.shrink(value).toList.foreach { candidate =>
      withClue(s"candidate '$candidate' of '$value': ") {
        perturbationOf(candidate) should not be perturbationOf(value)
      }
    }
    succeed
  }

  // ---------------------------------------------------------------------------
  // Validity of a generated value, by type.
  // ---------------------------------------------------------------------------

  /** Asserts that no element of a collection drawn from a finite generator is an edge value. */
  private def assertFinite(elements: List[Double]): Assertion =
    elements.filterNot(element => element.isFinite) shouldBe empty

  /** Asserts that a decimal is in range, at a supported scale, and normalised. */
  private def assertValidDecimal(value: Decimal): Assertion = {
    // normalisation is what makes the text of a decimal injective and its equality agree with
    // its comparison: a normalised value holds no trailing zero in its fraction
    if (value.scale > 0) {
      value.unscaledValue % 10L should not be 0L
    }
    math.abs(value.unscaledValue) should be <= Decimal.MAX_VALUE.unscaledValue
    value.scale should be >= 0
    value.scale should be <= Decimal.MAX_SCALE
  }

  /** Asserts that a fixed-scale decimal shows every digit of its decimal and no more than 18. */
  private def assertValidFixedScaleDecimal(value: FixedScaleDecimal): Assertion = {
    assertValidDecimal(value.decimal)
    value.fixedScale should be >= value.decimal.scale
    value.fixedScale should be <= Decimal.MAX_SCALE
  }

  /** Asserts that a matrix is rectangular, which is the one shape these generators produce. */
  private def assertRectangular(value: DoubleMatrix): Assertion = {
    List.tabulate(value.rowCount)(row => value.row(row).size) shouldBe
      List.fill(value.rowCount)(value.columnCount)
    entriesOf(value) should have size (value.rowCount.toLong * value.columnCount.toLong)
  }

  /** Asserts that an array is larger than any array the shared generators could have drawn. */
  private def assertLargeArray(value: DoubleArray): Assertion = {
    value.size should be > MaxArraySize
    value.size should be >= MinLargeArraySize
    value.size should be <= MaxLargeArraySize
  }

  /** Asserts that a matrix is rectangular and shaped beyond the reach of the shared generators. */
  private def assertLargeMatrix(value: DoubleMatrix): Assertion = {
    assertRectangular(value)
    value.rowCount should be > MaxMatrixDimension
    value.columnCount should be > MaxMatrixDimension
    value.rowCount should be >= MinLargeMatrixDimension
    value.rowCount should be <= MaxLargeMatrixDimension
    value.columnCount should be >= MinLargeMatrixDimension
    value.columnCount should be <= MaxLargeMatrixDimension
  }

  /** Asserts that a failure carries the reason of its own member, with a message and order. */
  private def assertValidFailure(value: Failure): Assertion = {
    value.message should not be empty
    // `of` answers the member that carries the reason, so rebuilding the value from its parts
    // recovers it exactly when the member and the reason it reports agree
    Failure.of(value.reason, value.message, value.attributes) shouldBe value
    value.attributes.keys.toList shouldBe value.attributes.keys.toList.sorted
  }

  /** Asserts that a generated failure also stays inside the documented attribute bound. */
  private def assertGeneratedFailure(value: Failure): Assertion = {
    assertValidFailure(value)
    value.attributes.size should be <= MaxAttributes
  }

  /** Asserts that a generated chain is non-empty and inside the documented length bound. */
  private def assertGeneratedChain(value: NonEmptyChain[Failure]): Assertion = {
    val failures = failuresOf(value)
    failures should not be empty
    failures.size should be <= MaxChainSize
    failures.foreach(failure => assertGeneratedFailure(failure))
    succeed
  }

  /** Whether a character is one of the letters `A` to `Z`, which is the upper-case shape. */
  private def isUpperLetter(character: Char): Boolean = character >= 'A' && character <= 'Z'

  // ===========================================================================
  // The element generators - the IEEE-754 values the equality design turns on
  // ===========================================================================

  test("every finite double is finite and small enough for two of them to be added") {
    forAll(genFiniteDouble) { element =>
      element.isFinite shouldBe true
      math.abs(element) should be <= MaxFiniteMagnitude
    }
  }

  test("the edge generator produces nothing but the five IEEE-754 values it documents") {
    forAll(genEdgeDouble) { element =>
      EdgeBits should contain(bitsOf(element))
    }
  }

  test("the edge generator reaches each of the five IEEE-754 values") {
    // a fixed sample rather than a property, for the reason given at `SampleSeed`: a case that
    // asserts a value is reached has to give the same answer on every run
    val drawn = sampleOf(genEdgeDouble, Draws).map(element => bitsOf(element))
    EdgeBits.foreach { bits =>
      withClue(s"the value whose bit pattern is $bits: ") {
        drawn should contain(bits)
      }
    }
    succeed
  }

  test("the mixed double generator reaches ordinary numbers and every edge value") {
    val drawn = sampleOf(genDouble, WideDraws)
    drawn.count(element => element.isFinite && !isSimplifiedElement(element)) should be > 0
    EdgeBits.foreach { bits =>
      withClue(s"the value whose bit pattern is $bits: ") {
        drawn.map(element => bitsOf(element)) should contain(bits)
      }
    }
    succeed
  }

  // ===========================================================================
  // The text generators
  // ===========================================================================

  test("every generated text is non-empty and short enough to read in a failure report") {
    forAll(genNonEmptyText) { text =>
      text should not be empty
      text.length should be <= MaxTextLength
    }
  }

  test("the text generator reaches every canonical name form of the library") {
    val texts = sampleOf(genNonEmptyText, WideDraws)
    NameForms.foreach { form =>
      withClue(s"the name form '$form': ") {
        texts should contain(form)
      }
    }
    succeed
  }

  test("the text generator reaches whitespace, punctuation, digits, non-ASCII and free text") {
    val texts = sampleOf(genNonEmptyText, WideDraws)
    // whitespace-only text passes an emptiness check and fails a blankness check, which is the
    // distinction a property over this generator has to be able to see
    texts.exists(text => text.nonEmpty && text.trim.isEmpty) shouldBe true
    texts.exists(text =>
      text.forall(character => PunctuationCharacters.contains(character))) shouldBe true
    texts.exists(text => text.forall(character => character.isDigit)) shouldBe true
    texts.exists(text => text.exists(character => character > '\u007f')) shouldBe true
    // the one-character case is the boundary of every emptiness check in the library
    texts.exists(text => text.length == 1) shouldBe true
    // only the free-text branch runs past the longest name form, which is twelve characters, so
    // a longer text is the evidence that branch is reached
    texts.exists(text => text.length > MaxUpperLetterLength) shouldBe true
  }

  test("every generated upper-case text holds only the letters A to Z") {
    forAll(genUpperLetterText) { text =>
      text should not be empty
      text.length should be <= MaxUpperLetterLength
      text.filterNot(character => isUpperLetter(character)) shouldBe ""
    }
  }

  test("the upper-case generator reaches both the single letter and the longest text") {
    val lengths = sampleOf(genUpperLetterText, Draws).map(text => text.length)
    lengths should contain(1)
    lengths should contain(MaxUpperLetterLength)
  }

  test("every generated non-upper text is non-empty and fails an upper-case-letters check") {
    forAll(genNonUpperText) { text =>
      text should not be empty
      text.exists(character => !isUpperLetter(character)) shouldBe true
      // six upper-case letters either side of the one offending character
      text.length should be <= 13
    }
  }

  test("the non-upper generator offends at the start, in the middle and at the end") {
    val texts = sampleOf(genNonUpperText, Draws)
    texts.exists(text => !isUpperLetter(text.head)) shouldBe true
    texts.exists(text => !isUpperLetter(text.last)) shouldBe true
    texts.exists(text =>
      text.length > 2 && isUpperLetter(text.head) && isUpperLetter(text.last)) shouldBe true
    // a validation written as a character range and one written as a letter test differ on
    // exactly this distinction, so both kinds of offending character have to arise
    texts.exists(text =>
      text.exists(character => character >= 'a' && character <= 'z')) shouldBe true
    texts.exists(text => text.exists(character => !character.isLetter)) shouldBe true
  }

  // ===========================================================================
  // The decimals
  // ===========================================================================

  test("every arbitrary decimal is in range and normalised") {
    forAll { (value: Decimal) =>
      assertValidDecimal(value)
    }
  }

  test("the decimal generator reaches zero, both extremes and the largest unscaled value") {
    val decimals = sampleOf(genDecimal, DecimalDraws)
    decimals should contain(Decimal.ZERO)
    decimals should contain(Decimal.MAX_VALUE)
    decimals should contain(Decimal.MIN_VALUE)
    decimals.map(value => math.abs(value.unscaledValue)).max shouldBe
      Decimal.MAX_VALUE.unscaledValue
    decimals.map(value => value.signum).distinct.sorted shouldBe List(-1, 0, 1)
  }

  test("the decimal generator reaches every scale the type supports") {
    sampleOf(genDecimal, DecimalDraws).map(value => value.scale).distinct.sorted shouldBe
      (0 to Decimal.MAX_SCALE).toList
  }

  test("every arbitrary fixed-scale decimal shows every digit its decimal holds") {
    forAll { (value: FixedScaleDecimal) =>
      assertValidFixedScaleDecimal(value)
    }
  }

  test("the fixed-scale generator reaches the exact scale, a padding of it and the largest") {
    val values = sampleOf(genFixedScaleDecimal, Draws)
    values.exists(value => value.fixedScale == value.decimal.scale) shouldBe true
    values.exists(value => value.fixedScale > value.decimal.scale) shouldBe true
    values.exists(value => value.fixedScale == Decimal.MAX_SCALE) shouldBe true
  }

  // ===========================================================================
  // The arrays
  // ===========================================================================

  test("every arbitrary array is within the documented size bound and copies back from its list") {
    forAll { (value: DoubleArray) =>
      value.size should be <= MaxArraySize
      value.toList should have size value.size.toLong
      DoubleArray.copyOf(value.toList) shouldBe value
    }
  }

  test("every array of the finite generators holds only finite elements") {
    forAll(genFiniteDoubleArray) { value =>
      value.size should be <= MaxArraySize
      assertFinite(value.toList)
    }
    forAll(genNonEmptyFiniteDoubleArray) { value =>
      value.size should be >= 1
      assertFinite(value.toList)
    }
  }

  test("every array of the non-empty generators holds at least one element") {
    forAll(genNonEmptyDoubleArray) { value =>
      value.size should be >= 1
      value.size should be <= MaxArraySize
    }
    // the invariant is also asserted over a fixed sample, since a property of ten cases cannot
    // establish that the empty array is never drawn where a generator excludes it
    sampleOf(genNonEmptyDoubleArray, Draws).filter(value => value.isEmpty) shouldBe empty
    sampleOf(genNonEmptyFiniteDoubleArray, Draws).filter(value => value.isEmpty) shouldBe empty
  }

  test("the edge-bearing array generator reaches every IEEE-754 value") {
    val elements =
      sampleOf(genNonEmptyDoubleArray, Draws).flatMap(array => array.toList).map(bitsOf)
    EdgeBits.foreach { bits =>
      withClue(s"the element whose bit pattern is $bits: ") {
        elements should contain(bits)
      }
    }
    succeed
  }

  test("both the empty array and the largest array arise where a generator allows both") {
    List(genFiniteDoubleArray, genDoubleArray).foreach { values =>
      val sizes = sampleOf(values, WideDraws).map(array => array.size)
      sizes should contain(0)
      sizes should contain(MaxArraySize)
    }
    succeed
  }

  test("every generated pair of arrays agrees on size") {
    forAll(genFiniteDoubleArrayPair) { case (left, right) =>
      left.size shouldBe right.size
      left.size should be <= MaxArraySize
      assertFinite(left.toList ::: right.toList)
    }
    forAll(genDoubleArrayPair) { case (left, right) =>
      left.size shouldBe right.size
      left.size should be <= MaxArraySize
    }
  }

  test("the edge-bearing pair generator reaches an edge value on each side") {
    val pairs = sampleOf(genDoubleArrayPair, Draws)
    pairs.flatMap { case (left, _) => left.toList }.exists(element =>
      isEdgeElement(element)) shouldBe true
    pairs.flatMap { case (_, right) => right.toList }.exists(element =>
      isEdgeElement(element)) shouldBe true
  }

  // ===========================================================================
  // The large arrays - the population the shared generators never reach
  // ===========================================================================

  test("every large finite array is larger than any small one and holds only finite elements") {
    forAll(genLargeFiniteDoubleArray) { value =>
      assertLargeArray(value)
      assertFinite(value.toList)
    }
  }

  test("every large edge-bearing array is within the documented size bound") {
    forAll(genLargeDoubleArray) { value =>
      assertLargeArray(value)
    }
  }

  test("every large edge-bearing array holds several edge values rather than at most one") {
    sampleOf(genLargeDoubleArray, LargeDraws).foreach { value =>
      withClue(s"an array of ${value.size} elements: ") {
        value.toList.count(element => isEdgeElement(element)) should be >= 2
      }
    }
    succeed
  }

  test("every large pair of arrays agrees on size and is large on both sides") {
    forAll(genLargeFiniteDoubleArrayPair) { case (left, right) =>
      left.size shouldBe right.size
      assertLargeArray(left)
      assertLargeArray(right)
      assertFinite(left.toList ::: right.toList)
    }
  }

  // ===========================================================================
  // The matrices
  // ===========================================================================

  test("every arbitrary matrix is rectangular and within the documented shape bound") {
    forAll { (value: DoubleMatrix) =>
      assertRectangular(value)
      value.rowCount should be <= MaxMatrixDimension
      value.columnCount should be <= MaxMatrixDimension
      assertFinite(entriesOf(value))
    }
    forAll(genDoubleMatrix) { value =>
      assertRectangular(value)
      value.rowCount should be <= MaxMatrixDimension
      value.columnCount should be <= MaxMatrixDimension
    }
  }

  test("every matrix of the square generators is square") {
    forAll(genSquareFiniteDoubleMatrix) { value =>
      value.isSquare shouldBe true
      value.rowCount should be <= MaxMatrixDimension
      assertFinite(entriesOf(value))
    }
    forAll(genSquareDoubleMatrix) { value =>
      value.isSquare shouldBe true
      assertRectangular(value)
    }
  }

  test("a generated matrix holding no entry states neither a row nor a column") {
    // a shape with no row or no column is the empty matrix, which the factory collapses both
    // degenerate shapes to, so a generated matrix never states a dimension it cannot fill
    forAll(genFiniteDoubleMatrix) { value =>
      if (value.isEmpty) {
        value.rowCount shouldBe 0
        value.columnCount shouldBe 0
      } else {
        value.rowCount should be > 0
        value.columnCount should be > 0
      }
    }
  }

  test("both the empty matrix and the largest shape arise from the shaped generators") {
    List(genFiniteDoubleMatrix, genDoubleMatrix).foreach { values =>
      val shapes =
        sampleOf(values, WideDraws).map(value => (value.rowCount, value.columnCount))
      shapes should contain((0, 0))
      shapes should contain((MaxMatrixDimension, MaxMatrixDimension))
    }
    succeed
  }

  test("the edge-bearing matrix generators reach every IEEE-754 value") {
    List(genDoubleMatrix, genSquareDoubleMatrix).foreach { values =>
      val entries = sampleOf(values, WideDraws).flatMap(value => entriesOf(value)).map(bitsOf)
      EdgeBits.foreach { bits =>
        withClue(s"the entry whose bit pattern is $bits: ") {
          entries should contain(bits)
        }
      }
    }
    succeed
  }

  test("every generated pair of matrices agrees on shape") {
    forAll(genFiniteDoubleMatrixPair) { case (left, right) =>
      left.rowCount shouldBe right.rowCount
      left.columnCount shouldBe right.columnCount
      assertFinite(entriesOf(left) ::: entriesOf(right))
    }
    forAll(genDoubleMatrixPair) { case (left, right) =>
      left.rowCount shouldBe right.rowCount
      left.columnCount shouldBe right.columnCount
    }
  }

  // ===========================================================================
  // The large matrices
  // ===========================================================================

  test("every large matrix is shaped beyond the shared bound and holds finite entries") {
    forAll(genLargeFiniteDoubleMatrix) { value =>
      assertLargeMatrix(value)
      assertFinite(entriesOf(value))
    }
  }

  test("the large rectangular generator reaches shapes that are not square") {
    sampleOf(genLargeFiniteDoubleMatrix, LargeDraws).count(value => !value.isSquare) should be > 0
  }

  test("every large square matrix is square and shaped beyond the shared bound") {
    forAll(genLargeSquareFiniteDoubleMatrix) { value =>
      value.isSquare shouldBe true
      assertLargeMatrix(value)
      assertFinite(entriesOf(value))
    }
  }

  test("every large pair of matrices agrees on shape and is large on both sides") {
    forAll(genLargeFiniteDoubleMatrixPair) { case (left, right) =>
      left.rowCount shouldBe right.rowCount
      left.columnCount shouldBe right.columnCount
      assertLargeMatrix(left)
      assertLargeMatrix(right)
      assertFinite(entriesOf(left) ::: entriesOf(right))
    }
  }

  // ===========================================================================
  // The failures
  // ===========================================================================

  test("every arbitrary failure reason is a member of the closed family") {
    forAll { (value: FailureReason) =>
      FailureReason.values.toList should contain(value)
      FailureReason.valueOf(value.name) shouldBe Some(value)
    }
  }

  test("the failure reason generator reaches every member the family declares") {
    sampleOf(genFailureReason, Draws).distinct.map(reason => reason.name).sorted shouldBe
      FailureReason.values.toList.map(reason => reason.name).sorted
  }

  test("every arbitrary failure carries its reason, a non-empty message and sorted attributes") {
    forAll { (value: Failure) =>
      assertGeneratedFailure(value)
    }
  }

  test("the failure generator reaches a failure of every one of the reasons") {
    sampleOf(genFailure, Draws).map(failure => failure.reason).distinct should have size
      FailureReason.values.length.toLong
  }

  test("a failure generated for a reason carries exactly that reason") {
    FailureReason.values.toList.foreach { reason =>
      withClue(s"the reason '${reason.name}': ") {
        sampleOf(genFailureWithReason(reason), LargeDraws).foreach { failure =>
          failure.reason shouldBe reason
          assertGeneratedFailure(failure)
        }
      }
    }
    succeed
  }

  test("every generated set of attributes is sorted, bounded and drawn from the documented keys") {
    forAll(genFailureAttributes) { attributes =>
      attributes.size should be <= MaxAttributes
      // the order of the keys is what makes the JSON of a failure byte-stable
      attributes.keys.toList shouldBe attributes.keys.toList.sorted
      attributes.keys.toList.filterNot(key => AttributeKeys.contains(key)) shouldBe empty
    }
  }

  test("the attribute generator reaches no attribute, the largest map and the empty value") {
    val attributes = sampleOf(genFailureAttributes, Draws)
    attributes.exists(entries => entries.isEmpty) shouldBe true
    attributes.exists(entries => entries.size == MaxAttributes) shouldBe true
    attributes.exists(entries => entries.values.exists(value => value.isEmpty)) shouldBe true
  }

  test("every generated failure message is a non-empty phrase of the documented shape") {
    forAll(genFailureMessage) { message =>
      message should not be empty
      MessagePrefixes.exists(prefix => message.startsWith(prefix)) shouldBe true
      // one of the four three-word prefixes followed by one to three words of vocabulary
      wordsOf(message).size should be >= 4
      wordsOf(message).size should be <= 6
    }
  }

  test("the message generator reaches every documented prefix") {
    val messages = sampleOf(genFailureMessage, Draws)
    MessagePrefixes.foreach { prefix =>
      withClue(s"the prefix '$prefix': ") {
        messages.exists(message => message.startsWith(prefix)) shouldBe true
      }
    }
    succeed
  }

  test("every arbitrary chain holds between one and four failures") {
    forAll { (value: NonEmptyChain[Failure]) =>
      assertGeneratedChain(value)
    }
  }

  test("the chain generator reaches a chain of one and chains of more than one") {
    val sizes = sampleOf(genFailures, Draws).map(chain => failuresOf(chain).size)
    sizes should contain(1)
    sizes should contain(MaxChainSize)
    sizes.count(size => size > 1) should be > 0
  }

  test("every arbitrary member of the sample family is one of its declared values") {
    forAll { (value: SampleNamed) =>
      SampleNamed.values.toList should contain(value)
      SampleNamed.namedEnum.valueOf(value.name) shouldBe Some(value)
    }
  }

  test("the sample family generator reaches every member the family declares") {
    sampleOf(genSampleNamed, Draws).distinct.map(member => member.name).sorted shouldBe
      SampleNamed.values.toList.map(member => member.name).sorted
  }

  test("the sample family orders, hashes and renders its members by name") {
    // the family exists so that the generic machinery of this module can be exercised on values
    // that carry no meaning of their own, and its three instances are all derived from the name,
    // so a member that ordered, hashed or rendered by anything else would make every spec
    // reaching for the family through `genSampleNamed` assert something other than it reads as
    val members = SampleNamed.values.toList
    members.map(member => SampleNamed.show.show(member)) shouldBe
      members.map(member => member.name)
    members.foreach { left =>
      members.foreach { right =>
        withClue(s"'${left.name}' against '${right.name}': ") {
          SampleNamed.order.compare(left, right).sign shouldBe
            left.name.compareTo(right.name).sign
          SampleNamed.order.eqv(left, right) shouldBe (left == right)
          if (left == right) {
            SampleNamed.order.hash(left) shouldBe SampleNamed.order.hash(right)
          } else {
            succeed
          }
        }
      }
    }
    succeed
  }

  // ===========================================================================
  // The four outcome types of the module
  //
  // The four names are written unqualified, as they are in `Arbitraries`, so
  // these cases resolve them through the re-export of the module root rather
  // than through the `result` package that defines them.
  // ===========================================================================

  test("every arbitrary single-failure outcome is one of its two shapes, and both arise") {
    forAll { (outcome: FailureOr[Decimal]) =>
      outcome match {
        case Left(failure) => assertGeneratedFailure(failure)
        case Right(value) => assertValidDecimal(value)
      }
    }
    val outcomes = sampleOf(genFailureOr(genDecimal), Draws)
    outcomes.count(outcome => outcome.isLeft) should be > 0
    outcomes.count(outcome => outcome.isRight) should be > 0
  }

  test("every arbitrary result is one of its two shapes, and both arise") {
    forAll { (outcome: ResultNec[Decimal]) =>
      outcome match {
        case Left(failures) => assertGeneratedChain(failures)
        case Right(value) => assertValidDecimal(value)
      }
    }
    val outcomes = sampleOf(genResultNec(genDecimal), Draws)
    outcomes.count(outcome => outcome.isLeft) should be > 0
    outcomes.count(outcome => outcome.isRight) should be > 0
  }

  test("every arbitrary accumulating outcome is one of its two shapes, and both arise") {
    forAll { (outcome: ValidatedFailures[Decimal]) =>
      outcome match {
        case Validated.Invalid(failures) => assertGeneratedChain(failures)
        case Validated.Valid(value) => assertValidDecimal(value)
      }
    }
    val outcomes = sampleOf(genValidatedFailures(genDecimal), Draws)
    outcomes.count(outcome => outcome.isInvalid) should be > 0
    outcomes.count(outcome => outcome.isValid) should be > 0
  }

  test("every arbitrary partly-successful outcome is one of its three shapes") {
    forAll { (outcome: ValueWithFailures[Decimal]) =>
      outcome match {
        case Ior.Left(failures) => assertGeneratedChain(failures)
        case Ior.Right(value) => assertValidDecimal(value)
        case Ior.Both(failures, value) =>
          assertGeneratedChain(failures)
          assertValidDecimal(value)
      }
    }
  }

  test("all three shapes of a partly-successful outcome arise from its generator") {
    // the both-shape is the reason the type exists, so it is asserted to arise rather than left
    // to chance: a generator that produced only the two Either-like shapes would leave half of
    // every combinator of this type untested
    val outcomes = sampleOf(genValueWithFailures(genDecimal), Draws)
    outcomes.count(outcome => outcome.isLeft) should be > 0
    outcomes.count(outcome => outcome.isRight) should be > 0
    outcomes.count(outcome => outcome.isBoth) should be > 0
  }

  // ===========================================================================
  // The Shrink instances - every candidate is valid, different and smaller
  // ===========================================================================

  test("every candidate of a decimal is a smaller decimal in range") {
    forAll(genDecimal) { value =>
      assertCandidates(value, decimalMeasure, 4, assertValidDecimal)
    }
  }

  test("every candidate of a fixed-scale decimal still shows every digit of its decimal") {
    forAll(genFixedScaleDecimal) { value =>
      assertCandidates(value, fixedScaleMeasure, 5, assertValidFixedScaleDecimal)
    }
  }

  test("every candidate of an array is a smaller non-empty array") {
    forAll(genDoubleArray) { value =>
      assertCandidates(
        value,
        arrayMeasure,
        MaxArraySize + 1,
        (candidate: DoubleArray) => {
          // the floor of one element is what the non-empty generators promise their consumers,
          // so a candidate may never be the empty array
          candidate.size should be >= 1
          candidate.size should be <= value.size
        })
    }
  }

  test("every candidate of a pair of arrays keeps the two sides the same size") {
    forAll(genDoubleArrayPair) { value =>
      assertCandidates(
        value,
        arrayPairMeasure,
        MaxArraySize + 1,
        (candidate: (DoubleArray, DoubleArray)) => {
          candidate._1.size shouldBe candidate._2.size
          candidate._1.size should be >= 1
          candidate._1.size should be <= value._1.size
        })
    }
  }

  test("every candidate of a matrix is a smaller rectangular matrix") {
    forAll(genDoubleMatrix) { value =>
      assertCandidates(
        value,
        matrixMeasure,
        MaxMatrixDimension * MaxMatrixDimension + 1,
        (candidate: DoubleMatrix) => {
          assertRectangular(candidate)
          candidate.rowCount should be >= 1
          candidate.columnCount should be >= 1
          candidate.rowCount should be <= value.rowCount
          candidate.columnCount should be <= value.columnCount
        })
    }
  }

  test("every candidate of a square matrix is square") {
    forAll(genSquareDoubleMatrix) { value =>
      assertCandidates(
        value,
        matrixMeasure,
        MaxMatrixDimension * MaxMatrixDimension + 1,
        (candidate: DoubleMatrix) => {
          candidate.isSquare shouldBe true
          candidate.rowCount should be >= 1
          candidate.rowCount should be <= value.rowCount
        })
    }
  }

  test("every candidate of a pair of matrices keeps the two sides the same shape") {
    forAll(genDoubleMatrixPair) { value =>
      assertCandidates(
        value,
        matrixPairMeasure,
        MaxMatrixDimension * MaxMatrixDimension + 1,
        (candidate: (DoubleMatrix, DoubleMatrix)) => {
          candidate._1.rowCount shouldBe candidate._2.rowCount
          candidate._1.columnCount shouldBe candidate._2.columnCount
          candidate._1.rowCount should be >= 1
          candidate._1.columnCount should be >= 1
        })
    }
  }

  test("every candidate of a failure keeps its reason and a non-empty message") {
    forAll(genFailure) { value =>
      assertCandidates(
        value,
        failureMeasure,
        MaxAttributes + 3,
        (candidate: Failure) => {
          // the reason is the identity of the member, and a candidate message is never empty
          candidate.reason shouldBe value.reason
          assertValidFailure(candidate)
        })
    }
  }

  test("every candidate of a chain of failures is a smaller non-empty chain") {
    forAll(genFailures) { value =>
      assertCandidates(
        value,
        failuresMeasure,
        2 + MaxChainSize * (MaxAttributes + 3),
        (candidate: NonEmptyChain[Failure]) => {
          val failures = failuresOf(candidate)
          failures should not be empty
          failures.size should be <= failuresOf(value).size
          failures.foreach(failure => assertValidFailure(failure))
          succeed
        })
    }
  }

  test("every candidate of a member of the sample family precedes it in declaration order") {
    forAll(genSampleNamed) { value =>
      assertCandidates(
        value,
        sampleNamedMeasure,
        SampleNamed.values.toList.size,
        (candidate: SampleNamed) =>
          SampleNamed.values.toList.take(SampleNamed.values.toList.indexOf(value)) should
            contain(candidate))
    }
  }

  // ===========================================================================
  // The Shrink instances - repeated shrinking terminates, at the documented floor
  // ===========================================================================

  test("repeated shrinking of a decimal terminates at zero") {
    forAll(genDecimal) { value =>
      assertTerminates(value, (floor: Decimal) => floor shouldBe Decimal.ZERO)
    }
  }

  test("repeated shrinking of a fixed-scale decimal terminates at zero at no decimal places") {
    forAll(genFixedScaleDecimal) { value =>
      assertTerminates(
        value,
        (floor: FixedScaleDecimal) => {
          floor.decimal shouldBe Decimal.ZERO
          floor.fixedScale shouldBe 0
        })
    }
  }

  test("repeated shrinking of an array terminates at a single zero element") {
    forAll(genNonEmptyDoubleArray) { value =>
      assertTerminates(
        value,
        (floor: DoubleArray) => {
          floor.size shouldBe 1
          isSimplifiedElement(floor.get(0)) shouldBe true
        })
    }
  }

  test("repeated shrinking of a square matrix terminates at a one-by-one zero matrix") {
    forAll(genSquareDoubleMatrix.filter(matrix => !matrix.isEmpty)) { value =>
      assertTerminates(
        value,
        (floor: DoubleMatrix) => {
          floor.rowCount shouldBe 1
          floor.columnCount shouldBe 1
          isSimplifiedElement(floor.get(0, 0)) shouldBe true
        })
    }
  }

  test("repeated shrinking of a matrix terminates with every entry zero and a dimension of one") {
    forAll(genDoubleMatrix.filter(matrix => !matrix.isEmpty)) { value =>
      assertTerminates(
        value,
        (floor: DoubleMatrix) => {
          entriesOf(floor).filterNot(entry => isSimplifiedElement(entry)) shouldBe empty
          math.min(floor.rowCount, floor.columnCount) shouldBe 1
        })
    }
  }

  test("repeated shrinking of a failure terminates at an attribute-free one-word failure") {
    forAll(genFailure) { value =>
      assertTerminates(
        value,
        (floor: Failure) => {
          floor.reason shouldBe value.reason
          floor.attributes shouldBe empty
          wordsOf(floor.message) should have size 1L
        })
    }
  }

  test("repeated shrinking of a chain of failures terminates at a chain of one such failure") {
    forAll(genFailures) { value =>
      assertTerminates(
        value,
        (floor: NonEmptyChain[Failure]) => {
          val failures = failuresOf(floor)
          failures should have size 1L
          failures.head.attributes shouldBe empty
          wordsOf(failures.head.message) should have size 1L
        })
    }
  }

  test("repeated shrinking of a member of the sample family terminates at the first declared") {
    forAll(genSampleNamed) { value =>
      assertTerminates(value, (floor: SampleNamed) => floor shouldBe SampleNamed.values.head)
    }
  }

  test("repeated shrinking of a pair of arrays moves both sides to the floor together") {
    forAll(genDoubleArrayPair) { value =>
      assertTerminates(
        value,
        (floor: (DoubleArray, DoubleArray)) => {
          floor._1.size shouldBe floor._2.size
          floor._1.size should be <= 1
          floor._1.toList.filterNot(element => isSimplifiedElement(element)) shouldBe empty
          floor._2.toList.filterNot(element => isSimplifiedElement(element)) shouldBe empty
        })
    }
  }

  test("repeated shrinking of a pair of matrices moves both sides to the floor together") {
    forAll(genDoubleMatrixPair) { value =>
      assertTerminates(
        value,
        (floor: (DoubleMatrix, DoubleMatrix)) => {
          floor._1.rowCount shouldBe floor._2.rowCount
          floor._1.columnCount shouldBe floor._2.columnCount
          entriesOf(floor._1).filterNot(entry => isSimplifiedElement(entry)) shouldBe empty
          entriesOf(floor._2).filterNot(entry => isSimplifiedElement(entry)) shouldBe empty
        })
    }
  }

  // ===========================================================================
  // The Shrink instances - a non-trivial case really is minimised
  //
  // This is the group `shrinkAny` fails: it offers no candidate at all, so each
  // of these cases would report the value it started with.
  // ===========================================================================

  test("a sixteen-element array is minimised to a single zero element") {
    val array = DoubleArray.tabulate(MaxArraySize)(index => index.toDouble + 1.0)
    val candidates = Shrink.shrink(array).toList
    candidates should not be empty
    candidates.head shouldBe array.subArray(0, MaxArraySize - 1)
    val (floor, steps) = fixedPointOf(array, 0)
    floor shouldBe DoubleArray.of(0.0)
    steps should be < StepBound
  }

  test("a five-by-five matrix is minimised to a one-by-one zero matrix") {
    val matrix =
      DoubleMatrix.tabulate(MaxMatrixDimension, MaxMatrixDimension)((row, column) =>
        (row + 1).toDouble * 10.0 + (column + 1).toDouble)
    val candidates = Shrink.shrink(matrix).toList
    candidates should not be empty
    candidates.head shouldBe
      DoubleMatrix.tabulate(MaxMatrixDimension - 1, MaxMatrixDimension - 1)(matrix.get)
    val (floor, steps) = fixedPointOf(matrix, 0)
    floor shouldBe DoubleMatrix.tabulate(1, 1)((_, _) => 0.0)
    steps should be < StepBound
  }

  test("an eighteen-digit decimal is minimised to zero") {
    val candidates = Shrink.shrink(Decimal.MAX_VALUE).toList
    candidates should not be empty
    candidates should contain(Decimal.ZERO)
    // the halved candidate is the one that makes a long minimisation short: it loses a digit of
    // the eighteen at every step rather than counting down
    candidates.map(candidate => candidate.unscaledValue) should
      contain(Decimal.MAX_VALUE.unscaledValue / 2L)
    val (floor, steps) = fixedPointOf(Decimal.MAX_VALUE, 0)
    floor shouldBe Decimal.ZERO
    steps should be < StepBound
  }

  test("a four-failure chain is minimised to a chain of one") {
    // the chain is ascribed to the trait rather than left at the least upper bound of its four
    // members, which would be `Failure with Product with Serializable` and would resolve the
    // shrinking of this file to none at all
    val chain: NonEmptyChain[Failure] = NonEmptyChain.of(
      Failure.Parsing("Unable to read row one", SortedMap("row" -> "1", "value" -> "x")),
      Failure.Parsing("Unable to read row two", SortedMap("row" -> "2")),
      Failure.MissingData("Missing data for value three"),
      Failure.Invalid("Invalid input for value four", SortedMap("value" -> "4")))
    val candidates = Shrink.shrink(chain).toList
    candidates should not be empty
    candidates.head shouldBe NonEmptyChain.of(failuresOf(chain).head)
    val (floor, steps) = fixedPointOf(chain, 0)
    failuresOf(floor) should have size 1L
    failuresOf(floor).head.reason shouldBe FailureReason.PARSING
    failuresOf(floor).head.attributes shouldBe empty
    wordsOf(failuresOf(floor).head.message) should have size 1L
    steps should be < StepBound
  }

  // ===========================================================================
  // The Cogen instances - equal values agree, different values do not
  // ===========================================================================

  test("equal decimals perturb a seed identically, and the candidates of one do not") {
    forAll(genDecimal) { value =>
      val copy = Decimal.ofScaled(value.unscaledValue, value.scale)
      copy shouldBe Right(value)
      copy.map(decimal => perturbationOf(decimal)) shouldBe Right(perturbationOf(value))
      assertDistinctPerturbations(value)
    }
  }

  test("equal fixed-scale decimals perturb a seed identically, and the candidates of one do not") {
    forAll(genFixedScaleDecimal) { value =>
      val copy = FixedScaleDecimal.of(value.decimal, value.fixedScale)
      copy shouldBe Right(value)
      copy.map(fixed => perturbationOf(fixed)) shouldBe Right(perturbationOf(value))
      assertDistinctPerturbations(value)
    }
  }

  test("equal arrays perturb a seed identically, and the candidates of one do not") {
    forAll(genDoubleArray) { value =>
      val copy = DoubleArray.copyOf(value.toList)
      copy shouldBe value
      perturbationOf(copy) shouldBe perturbationOf(value)
      assertDistinctPerturbations(value)
    }
  }

  test("equal matrices perturb a seed identically, and the candidates of one do not") {
    forAll(genDoubleMatrix) { value =>
      val copy = DoubleMatrix.tabulate(value.rowCount, value.columnCount)(value.get)
      copy shouldBe value
      perturbationOf(copy) shouldBe perturbationOf(value)
      assertDistinctPerturbations(value)
    }
  }

  test("equal failures perturb a seed identically, and the candidates of one do not") {
    forAll(genFailure) { value =>
      val copy = Failure.of(value.reason, value.message, value.attributes)
      copy shouldBe value
      perturbationOf(copy) shouldBe perturbationOf(value)
      assertDistinctPerturbations(value)
    }
  }

  test("equal chains of failures perturb a seed identically, and the candidates of one do not") {
    forAll(genFailures) { value =>
      val failures = failuresOf(value)
      val copy = NonEmptyChain.of(failures.head, failures.tail: _*)
      copy shouldBe value
      perturbationOf(copy) shouldBe perturbationOf(value)
      assertDistinctPerturbations(value)
    }
  }

  test("two different failure reasons do not perturb a seed the same way") {
    val reasons = FailureReason.values.toList
    reasons.foreach { left =>
      reasons.foreach { right =>
        withClue(s"'${left.name}' against '${right.name}': ") {
          (perturbationOf(left) == perturbationOf(right)) shouldBe (left == right)
        }
      }
    }
    succeed
  }

  test("two different members of the sample family do not perturb a seed the same way") {
    val members = SampleNamed.values.toList
    members.foreach { left =>
      members.foreach { right =>
        withClue(s"'${left.name}' against '${right.name}': ") {
          (perturbationOf(left) == perturbationOf(right)) shouldBe (left == right)
        }
      }
    }
    succeed
  }
}

