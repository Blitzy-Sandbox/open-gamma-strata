/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.Locale

import scala.util.matching.Regex

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.DoubleArrayMath
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A single foreign exchange rate between two currencies, such as `EUR/USD 1.25`.
 *
 * This represents a rate of foreign exchange. The rate `EUR/USD 1.25` consists of three
 * elements - the base currency `EUR`, the counter currency `USD` and the rate `1.25`. When
 * performing a conversion a rate of `1.25` means that `1 EUR = 1.25 USD`.
 *
 * See [[CurrencyPair]] for the representation that does not carry a rate.
 *
 * ===Obtaining one===
 *
 * The constructor is private and the type publishes neither `apply` nor `copy`, so `FxRate.of`
 * and [[FxRate.parse]] are the only ways to obtain a rate. Both report what was wrong with their
 * input as a value instead of interrupting the caller, and the members that derive one rate from
 * another - [[inverse]], [[toConventional]] and [[crossRate]] - are held to the same two
 * constraints, which together are what make every rate in existence satisfy them and why nothing
 * downstream re-checks them:
 *
 * {{{
 * FxRate.of(Currency.GBP, Currency.USD, 1.25d)   // Right(GBP/USD 1.25)
 * FxRate.of(CurrencyPair.of(Currency.GBP, Currency.GBP), 2d)
 * // Left - a rate between identical currencies has to be one
 * FxRate.parse("gbp/usd 1.25")                   // Right(GBP/USD 1.25) - case insensitive
 * }}}
 *
 * Both constraints are checked in one expression, so a caller that breaks both is told about
 * both rather than about whichever happened to be tested first:
 *
 *   - the rate has to be greater than zero. Both signed zeros and every negative rate are
 *     rejected. A rate that is not a number is ''accepted'', because the check compares the rate
 *     against zero and a not-a-number rate does not order against anything.
 *     `Double.PositiveInfinity` is accepted for the same reason - it is greater than zero -
 *     although its reciprocal is not a rate, which is the one numeric edge [[inverse]]
 *     documents;
 *   - two identical currencies force a rate of exactly one. `GBP/GBP 1` is the identity rate and
 *     `GBP/GBP 1.5` describes nothing, so it is rejected.
 *
 * ===Where a rate reports a failure===
 *
 * Reading a rate out of this object can fail, and the type says so. A rate holds one pair, so a
 * question about any other pair has no answer: `fxRate` and the conversions it defines in
 * [[FxRateProvider]] return a
 * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming the pair that could
 * not be converted, and [[crossRate]] returns one when the two rates share no single currency to
 * cross through.
 *
 * Three members return a rate or its text rather than an outcome: [[inverse]],
 * [[toConventional]] and [[toString]]. Their signatures carry no failure channel because a rate
 * derived from a rate that exists is a rate, for every input but one - and that one, the
 * reciprocal of an infinite rate, is refused, so no rate this object can hand out breaks the two
 * constraints above. See [[inverse]] for the edge and what it costs.
 *
 * ===Equality is bit for bit===
 *
 * The equality synthesised for a case class would compare the rate with the numeric comparison
 * of the platform, under which a rate that is not a number is not equal to itself and a negative
 * zero equals a positive one. [[equals]] and [[hashCode]] compare the bit patterns instead: a
 * not-a-number rate equals itself, and the two signed zeros are distinct - the zeros being
 * reachable through no route into the type at all, and both rules keeping equality reflexive for
 * a rate that already exists.
 *
 * This class is immutable and thread-safe.
 *
 * @param pair  the currency pair, formed of a base and a counter currency; in the pair `AAA/BBB`
 *   the base is `AAA` and the counter is `BBB`
 * @param rate  the rate applicable to the currency pair, greater than zero; one unit of the base
 *   currency is exchanged for this amount of the counter currency. The accessor is public, as is
 *   the `unapply` that pattern matching uses, so a caller can read the rate directly as well as
 *   through `fxRate` and `toString`
 * @see [[FxRateProvider]] for the lookup and conversion contract this type implements
 * @see [[CurrencyPair]] for the pair without a rate
 */
sealed abstract case class FxRate private (pair: CurrencyPair, rate: Double)
    extends FxRateProvider
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[FxRate.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the implementation class carries a public
  // constructor in the class file whatever the source asked for: a class compiled outside this
  // library can call it directly, and identity alone would then admit a rate of zero or below -
  // whose inverse is no rate at all - or a pair naming one currency twice at a rate other than
  // one, which would convert a currency into itself at a profit. These are exactly the two
  // conditions `FxRate.create` establishes for every rate, the derived ones included, and they
  // are stated with its comparisons: a rate that is not a number is neither negative nor zero,
  // so it passes here exactly as it passes there.
  JvmClosure.requireInvariant("its rate is not negative or zero", !(rate <= 0d))
  JvmClosure.requireInvariant(
    "a pair naming one currency twice carries a rate of one",
    !pair.isIdentity || rate == 1d)

  /**
   * Gets the inverse rate.
   *
   * The inverse rate has the same currencies in the opposite order and the reciprocal rate, so
   * `GBP/USD 1.25` inverts to `USD/GBP 0.8`. Inverting the identity rate `AAA/AAA 1` gives it
   * back, since its reciprocal is one.
   *
   * The reciprocal is computed as the single division `1d / rate`, so the result is one `Double`
   * and not a value rounded through an intermediate representation. Inverting twice therefore
   * recovers a rate to within the rounding of two divisions and not always exactly -
   * `1d / (1d / 0.1d)` is not `0.1d` - which is why a caller comparing a doubly-inverted rate
   * with the rate it came from should compare with a tolerance.
   *
   * This returns a rate rather than an outcome, and one numeric edge is the price of that. The
   * reciprocal of a rate greater than zero is greater than zero, except for one input: the
   * reciprocal of `Double.PositiveInfinity` is zero, which is not a rate this type admits. This
   * member refuses it, because the check lives on the single creation route of the companion and
   * therefore holds for a derived rate as much as for a supplied one. The refusal raises an
   * `IllegalArgumentException` rather than returning a failure, which is the treatment a numeric
   * edge reachable only from a value a caller chose to supply is given here: no rate a caller can
   * obtain from this method breaks the constraints of the type.
   *
   * The same edge is reached one step later by an inversion of a rate small enough that its
   * reciprocal is infinite: `FxRate.of(pair, Double.MinPositiveValue)` inverts to an infinite
   * rate, which the type admits, and inverting '''that''' is the refusal above.
   *
   * @return the inverse rate
   * @throws java.lang.IllegalArgumentException if the reciprocal is not a rate, which happens for
   *   exactly one input - a rate of `Double.PositiveInfinity`
   */
  def inverse: FxRate = FxRate.create(pair.inverse, 1d / rate)

  /**
   * Gets the FX rate for the specified currency pair.
   *
   * The rate returned is the rate from the base currency to the counter currency as defined by
   * this formula: `(1 * baseCurrency = fxRate * counterCurrency)`.
   *
   * This is the single abstract member of [[FxRateProvider]], so implementing it is what makes a
   * rate usable wherever a provider is expected, including for the conversions the trait defines
   * in terms of it. The four cases are tested in this order: two identical currencies convert at
   * one whether or not this rate mentions them, this pair answers with its rate, the inverted
   * pair answers with the reciprocal, and any other pair has no answer here.
   *
   * {{{
   * val rate = FxRate.of(Currency.GBP, Currency.USD, 1.25d)   // Right(GBP/USD 1.25)
   * rate.map(_.fxRate(Currency.GBP, Currency.USD))            // Right(Right(1.25))
   * rate.map(_.fxRate(Currency.USD, Currency.GBP))            // Right(Right(0.8))
   * rate.map(_.fxRate(Currency.AUD, Currency.AUD))            // Right(Right(1.0))
   * rate.map(_.fxRate(Currency.GBP, Currency.AUD))            // Right(Left(...))
   * }}}
   *
   * @param baseCurrency  the base currency, to convert from
   * @param counterCurrency  the counter currency, to convert to
   * @return the FX rate for the currency pair, or the failure naming the pair this rate cannot
   *   convert
   */
  override def fxRate(baseCurrency: Currency, counterCurrency: Currency): FailureOr[Double] =
    if (baseCurrency == counterCurrency) {
      Right(1d)
    } else if (baseCurrency == pair.base && counterCurrency == pair.counter) {
      Right(rate)
    } else if (counterCurrency == pair.base && baseCurrency == pair.counter) {
      Right(1d / rate)
    } else {
      Left(Failure.CurrencyConversion(s"No FX rate found for $baseCurrency/$counterCurrency"))
    }

  /**
   * Derives an FX rate from this rate and another related rate.
   *
   * Given two rates it is possible to derive a third if they have one currency in common. For
   * example, given rates for `EUR/GBP` and `EUR/CHF` it is possible to derive a rate for
   * `GBP/CHF`. The result always carries its currency pair in market convention order, whichever
   * way round the two input rates are written, so crossing `EUR/USD` with `USD/GBP` and crossing
   * it with `GBP/USD` produce the same `EUR/GBP` rate.
   *
   * A cross exists only when the two pairs name three currencies in total, which is the condition
   * [[CurrencyPair.cross]] decides:
   *
   *   - `AAA/BBB` and `BBB/CCC` - valid, producing `AAA/CCC`
   *   - `AAA/BBB` and `CCC/BBB` - valid, producing `AAA/CCC`
   *   - `AAA/BBB` and `BBB/AAA` - no cross, only two currencies are named
   *   - `AAA/BBB` and `BBB/BBB` - no cross, one pair is an identity
   *   - `AAA/BBB` and `CCC/DDD` - no cross, no currency is shared
   *
   * The failure for those three cases is a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming both pairs. A cross
   * that does exist can still fail, for the one reason the product of two positive rates can be
   * unusable: two rates small enough that their product underflows to zero produce a rate
   * `FxRate.of` rejects.
   *
   * @param other  the other rate
   * @return the rate derived from these two rates, or the failure naming two pairs that do not
   *   share exactly one currency - because no currency is shared, because one pair is an identity
   *   or because only two currencies are named between them - or a product of the two rates that
   *   is not greater than zero
   */
  def crossRate(other: FxRate): FailureOr[FxRate] =
    pair.cross(other.pair) match {
      case Some(crossPairAC) =>
        FxRate
          .computeCross(this, other, crossPairAC)
          .left
          .map(failures => Failure.collapse(failures))
      case None =>
        Left(
          Failure.CurrencyConversion(
            s"Unable to cross when no unique common currency: $pair and ${other.pair}"))
    }

  /**
   * Returns the rate expressed in the market convention direction for its two currencies.
   *
   * This rate is returned when its pair is already the market convention pair, and a rate with
   * the inverted pair and the reciprocal rate otherwise, so `USD/GBP 0.8` becomes `GBP/USD 1.25`.
   * The identity pair is conventional - [[CurrencyPair.isConventional]] answers true for it - so
   * the identity rate is returned unchanged rather than inverted.
   *
   * The reciprocal is the same division [[inverse]] performs and carries the same single numeric
   * edge, documented there: an infinite rate whose pair is written the other way round has no
   * conventional form, and asking for one is refused. An infinite rate whose pair is already
   * conventional is returned untouched, since no division is performed.
   *
   * @return the rate in the market convention direction
   * @throws java.lang.IllegalArgumentException if the pair has to be inverted and the reciprocal
   *   of the rate is not a rate, which happens for exactly one input - a rate of
   *   `Double.PositiveInfinity`
   */
  def toConventional: FxRate =
    if (pair.isConventional) this else FxRate.create(pair.toConventional, 1d / rate)

  /**
   * Checks whether this rate equals another object.
   *
   * Another rate is equal when it holds the same pair and a rate with the same bit pattern. That
   * bit comparison differs from the comparison a case class would have synthesised for exactly
   * two rates: one that is not a number, which here equals itself, and a negative zero, which
   * here differs from a positive zero. An object of any other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is a rate holding the same pair and the same rate
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: FxRate =>
      (this eq other) ||
        (pair == other.pair && java.lang.Double.compare(rate, other.rate) == 0)
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * The mixing is a seed, then each field in declaration order, with the rate hashed by its bit
   * pattern so that two rates which [[equals]] calls equal always agree here too. The seed is the
   * hash of the type's own name rather than the identity hash of its class, so the hash of a rate
   * is determined by the pair and the rate alone and is identical in every run of every program.
   *
   * @return the hash code of the pair and the rate held
   */
  override def hashCode: Int =
    (FxRate.HashSeed * 31 + pair.hashCode) * 31 + java.lang.Double.hashCode(rate)

  /**
   * Returns the formatted string form of this rate.
   *
   * The form is the base code, a slash, the counter code, a space and the rate - `EUR/USD 1.25` -
   * which is what [[FxRate.parse]] reads back for every rate whose text the expression it matches
   * admits. A rate that is a whole number is written without a fractional part - `EUR/USD 5`
   * rather than `EUR/USD 5.0`. The three values outside the real numbers are written as the
   * platform writes them: `NaN`, `Infinity` and `-Infinity`.
   *
   * @return the formatted string
   */
  override def toString: String = s"$pair ${FxRate.formatRate(rate)}"
}

/**
 * Provides the ways of obtaining a rate, the cross-rate derivation, and the instances for the
 * type.
 *
 * This companion is the only place a [[FxRate]] is created. Every route into the type either
 * validates its input - `of` and [[parse]] - or derives a rate from one that was already
 * validated, which is what the private creation below exists for and why it is not offered to
 * callers.
 *
 * The positivity of the rate and the identity constraint are the two accumulating checks of
 * `of`; the text form is the pattern and the two wordings of [[parse]]; and the wire form is the
 * two codecs at the end of this object, which are assembled at compile time and perform no
 * reflection.
 */
object FxRate {

  /**
   * The text form of a rate: a pair of three-letter codes and a rate, separated by a slash and a
   * space.
   *
   * The expression is applied to the whole of the text rather than to part of it, so leading or
   * trailing characters are a rejection rather than something to ignore.
   *
   * The rate group admits only digits, a sign and a dot, which is deliberately narrower than the
   * text the platform can parse as a number: `EUR/USD 1e3` and `EUR/USD 1.25d` name no rate at
   * all, and are rejected by this expression rather than by the number parsing that follows it.
   * That distinction is what decides which of the two wordings of [[parse]] a caller is given.
   */
  private val RateFormat: Regex = """([A-Z]{3})[/]([A-Z]{3})[ ]([0-9+.-]+)""".r

  /**
   * The length of the pair that precedes the space, which the expression above fixes at seven.
   *
   * Three code characters, a slash and three more is seven characters, and the space that
   * separates the pair from the rate follows them. Folding a text never produces fewer
   * characters than it was given, and no character folds to a space, so the first space of the
   * folded text is the first space of the text as given and cannot have moved earlier: text
   * whose fold puts that space at the eighth position therefore carries it at the eighth
   * position or earlier before folding.
   *
   * That is what [[FxRate.parse]] tests before it folds anything - the text holds a space, and
   * no more than seven characters precede it - because the test is decided by a single scan that
   * allocates nothing, where the fold copies the whole of the text (CWE-400/CWE-770). It is an
   * upper bound rather than an equality precisely because folding can lengthen the pair: the
   * single character `\ufb01` folds to `FI`, so `\ufb01M/USD` is six characters that fold to the
   * seven of `FIM/USD` - a pair this library defines - and a text of six characters therefore
   * still reaches the expression and still names the rate it always named.
   *
   * This bounds the front of the text only. The rate that follows the space is bounded by
   * [[MaxRateTextLength]], which [[FxRate.parse]] tests in the same place and for the same
   * reason.
   */
  private val PairTextLength: Int = 7

  /**
   * The longest the rate may be, as text.
   *
   * The rate group of the expression admits digits, signs and points without limit, and the
   * reading that follows is a double, which has no longest spelling: a caller may write any
   * number of digits and the reading rounds them. So the pair test above bounds the front of the
   * text and, until this, nothing bounded its tail - a sender could choose how much text was
   * case-folded, matched against the expression and read as a number, in each case only for the
   * result to be discarded.
   *
   * The number is what it takes to write a double exactly, as it is for the amount of an
   * [[CurrencyAmount]], and is the same thousand characters: the longest exact decimal spelling
   * of a finite double is that of the smallest subnormal, at 767 significant digits after a
   * leading zero and a point, and every other value needs fewer. No spelling that names a rate
   * exactly is refused for its size; text carrying digits that cannot change the value it names
   * is, and that is all.
   *
   * Text past it is reported with the wording of a rate that could not be read rather than one
   * of its own, so what reaches a caller is what has always reached one for a rate that names no
   * legal value - which the ten-thousand-digit rate of the test suite, refused here for its size
   * where it used to be refused for being zero, still reads as.
   */
  private val MaxRateTextLength: Int = 1024

  /**
   * The rejection of a rate other than one between two identical currencies.
   *
   * The wording is written once here, so the two places that check the constraint - the
   * accumulating check of `of` and the check on the single creation route - report the same text.
   */
  private val IdenticalCurrencyMessage: String =
    "Conversion rate between identical currencies must be one"

  /**
   * The name of the rate, used as the name of the checked argument.
   *
   * It is the name the positivity check reports in a failure. The JSON key is the same word, and
   * it comes from the field of the raw shape at the end of this object, which the codecs derive
   * from: the text a caller reads in a failure is written here and the text a document carries is
   * written there, and the two are deliberately the same.
   */
  private val RateField: String = "rate"

  /** The seed the hash of a rate mixes from, the hash of the name of the type. */
  private val HashSeed: Int = "FxRate".hashCode

  /**
   * Obtains an instance from two currencies and a rate.
   *
   * The first currency is the base and the second is the counter. The two currencies may be the
   * same, but if they are then the rate has to be one.
   *
   * This is the currency-pair form of `of` applied to the pair of the two currencies, so the two
   * forms accept and reject exactly the same rates.
   *
   * @param base  the base currency
   * @param counter  the counter currency
   * @param rate  the conversion rate, greater than zero
   * @return the FX rate, or every constraint the arguments break: a rate that is not greater
   *   than zero, and a rate other than one for two identical currencies
   */
  def of(base: Currency, counter: Currency, rate: Double): ResultNec[FxRate] =
    of(CurrencyPair.of(base, counter), rate)

  /**
   * Obtains an instance from a currency pair and a rate.
   *
   * The two currencies of the pair may be the same, but if they are then the rate has to be one.
   *
   * Both constraints of the type are checked here, and they accumulate: a caller that supplies a
   * negative rate for a pair of identical currencies is told about the rate ''and'' about the
   * identity, in that order, in one failure chain.
   *
   * {{{
   * FxRate.of(CurrencyPair.of(Currency.GBP, Currency.USD), 1.25d)  // Right(GBP/USD 1.25)
   * FxRate.of(CurrencyPair.of(Currency.GBP, Currency.USD), 0d)     // Left - one failure
   * FxRate.of(CurrencyPair.of(Currency.GBP, Currency.GBP), 1d)     // Right(GBP/GBP 1)
   * FxRate.of(CurrencyPair.of(Currency.GBP, Currency.GBP), -1d)    // Left - two failures
   * }}}
   *
   * @param pair  the currency pair
   * @param rate  the conversion rate, greater than zero
   * @return the FX rate, or every constraint the arguments break: a rate that is not greater
   *   than zero, and a rate other than one for a pair of two identical currencies
   */
  def of(pair: CurrencyPair, rate: Double): ResultNec[FxRate] =
    (Validate.notNegativeOrZero(rate, RateField), checkedIdentityRate(pair, rate))
      .mapN((checkedRate, _) => create(pair, checkedRate))
      .toEither

  /**
   * Parses a rate from text of the form `AAA/BBB RATE`.
   *
   * The parsed form is the base code, a slash, the counter code, a space and the rate, which is
   * the form [[FxRate.toString]] writes. The text is folded to upper case in the English locale
   * before it is matched, so parsing is insensitive to the case of the input; the English locale
   * is named explicitly so that the fold is the same in every locale a program might run in.
   *
   * Two wordings are reported, and the shape of the text decides which. Text the expression above
   * does not match is `Invalid rate: <text>`; text that matches but does not name a rate -
   * because a code names no currency, because the digits do not form a number, or because the
   * rate is one this type rejects - is `Unable to parse rate: <text>`. Both quote the text as it
   * was supplied rather than as it was folded, so a caller sees back what they wrote.
   *
   * {{{
   * FxRate.parse("USD/EUR 205.123")   // Right(USD/EUR 205.123)
   * FxRate.parse("cAd/GbP 1.25")      // Right(CAD/GBP 1.25) - case insensitive
   * FxRate.parse("EUR/USD +1.25")     // Right(EUR/USD 1.25) - a leading sign is a number
   * FxRate.parse("EUR/USD")           // Left(Failure.Parsing("Invalid rate: EUR/USD"))
   * FxRate.parse("EUR/USD X")         // Left - the rate group admits no letters
   * FxRate.parse("EUR/GBP 0")         // Left(Failure.Parsing("Unable to parse rate: EUR/GBP 0"))
   * FxRate.parse("EUR/EUR 1.25")      // Left - matches, but names no legal rate
   * }}}
   *
   * Text whose codes are well formed but name a code outside the closed set of [[Currency]],
   * such as `EUR/ZZZ 1.25`, is one of the inputs the second wording reports: [[Currency.parse]]
   * rejects the code and no currency is invented for it.
   *
   * The cause of a rejection is deliberately not carried in the failure. The message names only
   * the text, which keeps two failures over the same text equal and keeps the message a caller
   * reads and logs to one line.
   *
   * ===The shape of the pair is tested before the text is folded===
   *
   * The expression puts the space that separates the pair from the rate after the seventh
   * character. Folding never makes a text shorter and no character folds to a space, so text
   * that holds no space at all, or that holds more than [[PairTextLength]] characters before its
   * first one, cannot match however it is folded. That is tested before the fold - by one scan
   * for the space, which allocates nothing - and text failing it is refused with the
   * invalid-rate wording it was always refused with. Nothing else moves: every text that could
   * still match is folded and matched exactly as before, a pair that lengthens under folding
   * included; the rate group is left unbounded, so a rate written with ten thousand digits still
   * reaches the expression and is still reported with the other wording; and folding cannot
   * affect that group, which holds only digits, a sign and a dot. The saving is the copy - text
   * arriving from outside this library is no longer folded in full before its shape is looked at
   * (CWE-400/CWE-770).
   *
   * @param rateStr  the rate as text, in the form `AAA/BBB RATE`, in any case
   * @return the FX rate the text names, or the failure naming text that is not two three-letter
   *   codes, a slash, a space and a rate of digits with an optional sign and point, or that
   *   holds a code no currency of the closed [[Currency]] set has, digits that do not form a
   *   number, or a rate the two constraints of this type reject
   */
  def parse(rateStr: String): FailureOr[FxRate] = {
    // the position of the first space, which folding can move later but never earlier, so a text
    // whose fold carries it at the eighth position carries it here at the eighth or before
    val spaceIndex: Int = rateStr.indexOf(' ')
    if (spaceIndex < 0 || spaceIndex > PairTextLength) {
      // the text is rendered rather than interpolated as it stands, which bounds the message and
      // keeps it to one line while leaving an in-bound spelling quoted as it was given
      Left(Failure.Parsing(s"Invalid rate: $rateStr"))
    } else if (rateStr.length - (spaceIndex + 1) > MaxRateTextLength) {
      // The rate group of the expression admits digits without limit, so the pair test above
      // bounds the front of the text and nothing bounds its tail: a well-formed pair followed by
      // a tail of a sender's choosing would be case-folded in full, matched in full and then
      // read as a number in full (CWE-400/CWE-770). The tail is therefore measured here, before
      // any of that, and text past the bound names no rate the type holds - which is what the
      // wording below says, and why the ceiling reports through it rather than through a wording
      // of its own.
      Left(Failure.Parsing(s"Unable to parse rate: $rateStr"))
    } else {
      // the fold is bounded by the two tests above: the pair is at most PairTextLength
      // characters and the rate at most MaxRateTextLength, whatever arrived
      rateStr.toUpperCase(Locale.ENGLISH) match {
        case RateFormat(baseCode, counterCode, rateText) =>
          val parsed: Option[FxRate] = for {
            base <- Currency.parse(baseCode).toOption
            counter <- Currency.parse(counterCode).toOption
            parsedRate <- rateText.toDoubleOption
            fxRate <- of(CurrencyPair.of(base, counter), parsedRate).toOption
          } yield fxRate
          // the text is rendered rather than interpolated as it stands, which bounds both
          // messages and keeps them to one line while leaving an in-bound spelling quoted as it
          // was given
          parsed.toRight(Failure.Parsing(s"Unable to parse rate: $rateStr"))
        case _ =>
          Left(Failure.Parsing(s"Invalid rate: $rateStr"))
      }
    }
  }

  /**
   * Creates a rate, holding it to both constraints of the type, which every route funnels
   * through.
   *
   * This is the only instantiation of the type and it is private, so the routes above are the
   * only way into it from outside this file - and because the two constraints are checked here,
   * they hold for '''every''' rate that exists rather than only for the ones a caller's arguments
   * produced, the derived rates included.
   *
   * The checks are the reason [[FxRate.inverse]] and [[FxRate.toConventional]] can keep returning
   * a rate rather than an outcome while the type stays sound: a rate derived from a rate that
   * exists is one the type admits for every input but the reciprocal of an infinite rate, and
   * that one input is refused here. They cost nothing that matters - `of` reaches this method only
   * when its own accumulating checks have passed, so the checks here can fail only on a derived
   * value.
   *
   * @param pair  the currency pair
   * @param rate  the rate, checked here against both constraints of the type
   * @return the rate
   * @throws java.lang.IllegalArgumentException if the rate is not greater than zero, or the pair
   *   names one currency twice and the rate is not one
   */
  private def create(pair: CurrencyPair, rate: Double): FxRate = {
    ArgCheck.notNegativeOrZero(rate, RateField)
    ArgCheck.isTrue(!pair.isIdentity || rate == 1d, IdenticalCurrencyMessage)
    new Impl(pair, rate)
  }

  /**
   * The one implementation of a rate.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[FxRate]] refuse in its own constructor to be any other implementation.
   *
   * @param pair  the currency pair
   * @param rate  the rate, already checked against both constraints of the type by [[create]]
   */
  private final class Impl(pair: CurrencyPair, rate: Double)
      extends FxRate(pair, rate)

  /**
   * Checks that a pair of identical currencies carries a rate of one.
   *
   * The check has nothing to return, since the pair and the rate it reads are both already in the
   * caller's hands, so its outcome carries `Unit` and combines with the positivity check as any
   * other value would. A rate that is not a number fails this check for a pair of identical
   * currencies, because it is not equal to one.
   *
   * @param pair  the currency pair
   * @param rate  the rate to check against the pair
   * @return a passing outcome, or the failure naming a rate other than one held against a pair
   *   of two identical currencies
   */
  private def checkedIdentityRate(pair: CurrencyPair, rate: Double): ValidatedFailures[Unit] =
    Validate.isTrue(!pair.isIdentity || rate == 1d, IdenticalCurrencyMessage)

  /**
   * Computes the cross rate of two rates over the cross pair their currencies determine.
   *
   * The aim is to turn `AAA/BBB` and `BBB/CCC` into `AAA/CCC`, where `AAA/CCC` is the cross pair
   * [[CurrencyPair.cross]] has already chosen - in market convention order, so neither input pair
   * is necessarily the right way round. The orientation is therefore derived from the cross pair:
   * which input holds the base currency of the cross decides which rate is read towards the
   * shared currency and which is read away from it, and each rate is inverted if its own pair
   * runs the other way.
   *
   * That derivation depends on the branch order of [[CurrencyPair.cross]], which is part of its
   * contract for this reason. The operands of the multiplication and of the two divisions are not
   * reassociated: floating-point multiplication of a rate and a reciprocal is not associative, so
   * a rearranged expression agrees with this one only to within rounding.
   *
   * @param fx1  the first rate
   * @param fx2  the second rate
   * @param crossPairAC  the cross pair, in market convention order
   * @return the cross rate, or the failure naming a product of the two rates that is not greater
   *   than zero, which two rates small enough to underflow produce
   */
  private def computeCross(fx1: FxRate, fx2: FxRate, crossPairAC: CurrencyPair): ResultNec[FxRate] = {
    val currA = crossPairAC.base
    val currC = crossPairAC.counter
    // given the conventional cross rate pair, order the two rates to match
    val crossBaseCurrencyInFx1 = fx1.pair.contains(currA)
    val fxABorBA = if (crossBaseCurrencyInFx1) fx1 else fx2
    val fxBCorCB = if (crossBaseCurrencyInFx1) fx2 else fx1
    // extract the rates, taking the inverse if the pair is in the inverse order
    val rateAB = if (fxABorBA.pair.base == currA) fxABorBA.rate else 1d / fxABorBA.rate
    val rateBC = if (fxBCorCB.pair.counter == currC) fxBCorCB.rate else 1d / fxBCorCB.rate
    of(crossPairAC, rateAB * rateBC)
  }

  /**
   * Renders a rate as the text form of this type writes it.
   *
   * A rate that is a whole number is written without a fractional part, which is the one thing
   * this rendering does that the platform's own does not.
   * [[com.opengamma.strata.collect.DoubleArrayMath.isMathematicalInteger]] decides which rates
   * those are, and it answers false for each of the three values outside the real numbers, so
   * each of them is written as the platform writes it.
   *
   * A whole number beyond the range of a 64-bit integer is written as that range's largest
   * value, because the narrowing conversion saturates - a rate of `1e20` renders as
   * `9223372036854775807`.
   *
   * @param rate  the rate to render
   * @return the rate as text, without a fractional part when it is a whole number
   */
  private def formatRate(rate: Double): String =
    if (DoubleArrayMath.isMathematicalInteger(rate)) rate.toLong.toString else rate.toString

  /**
   * The hashing and equality of rates.
   *
   * This is the only equality-bearing instance of the type: `Hash` extends `Eq`, so a separate
   * `Eq` would be a second answer to the same question. Both are taken from the [[FxRate.equals]]
   * and [[FxRate.hashCode]] of the type, which compare the rate by its bit pattern.
   *
   * No `Order` is offered, because there is no ordering of rates that means anything across
   * pairs - a rate of `1.25` for `EUR/USD` is neither above nor below a rate of `0.8` for
   * `USD/JPY`. A caller that needs a reproducible sequence of rates should sort by the pair,
   * which [[CurrencyPair]] orders.
   *
   * @return the hashing and equality of rates
   */
  implicit val hash: Hash[FxRate] = Hash.fromUniversalHashCode[FxRate]

  /**
   * The rendering of rates as text.
   *
   * Renders what [[FxRate.toString]] renders, the `EUR/USD 1.25` form that [[parse]] reads back,
   * so the text of a rate is the same however it reaches a message.
   *
   * @return the rendering of a rate
   */
  implicit val show: Show[FxRate] = Show.show(_.toString)

  // The codec of the double field, brought into scope for the two derivations below and for
  // nothing else: the rate goes through the single policy for a double, which writes the values
  // JSON cannot express as tagged strings. Importing it here is what keeps that choice
  // deliberate and local, as the codec support of `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the JSON
   * shape of a rate is stated exactly once. It is private and never returned - the only values of
   * it that exist are the ones the two codecs build. Its field names are the JSON keys, and they
   * are the names of the two fields of [[FxRate]] itself, which keeps the derived shape and the
   * type from drifting apart.
   *
   * @param pair  the currency pair, whose own codec carries it as the `EUR/USD` string form
   * @param rate  the rate, unvalidated on the way in and already checked on the way out
   */
  private final case class Raw(pair: CurrencyPair, rate: Double) extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder.AsObject[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of rates.
   *
   * A rate is an object of two fields, the pair as its text form and the rate as a number:
   *
   * {{{
   * {"pair":"EUR/USD","rate":1.25}
   * }}}
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping a rate
   * onto that shape. Deriving from [[FxRate]] itself is not possible - the constructor of a
   * validated type is not public, so there is no public shape to derive from - and writing the
   * fields out by hand instead would state the same contract a second time.
   *
   * The result is wrapped so that a field holding no value would be omitted, which is the policy
   * every product of this module follows - this type has no optional field, so the wrapping
   * changes nothing about its output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of a rate
   */
  implicit val encoder: Encoder[FxRate] =
    Codecs.dropNulls(rawEncoder.contramap[FxRate](fxRate => Raw(fxRate.pair, fxRate.rate)))

  /**
   * The JSON decoding of rates.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a rate
   * exactly as a caller's arguments are decided: the payload is read into the raw shape and
   * handed to `of`, so a document naming a rate that is not greater than zero, or a rate other
   * than one between two identical currencies, is a decoding failure carrying every reason rather
   * than a value this type would not have built.
   *
   * @return the JSON decoding of a rate
   */
  implicit val decoder: Decoder[FxRate] =
    Codecs.validatedDecoder[Raw, FxRate](raw => of(raw.pair, raw.rate))(rawDecoder)
}
