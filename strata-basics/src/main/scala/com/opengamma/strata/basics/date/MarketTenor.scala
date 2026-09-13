/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.Period

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Codec

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A code used in the market to indicate both the start date and the tenor of a financial
 * instrument.
 *
 * A [[Tenor]] is the actual tenor of an instrument, from its start to its end. This type is the
 * code used in the market, which also effectively describes the start date. Four dates are needed
 * to understand how such a code works:
 *
 *   - the '''trade date''', the date the trade is agreed;
 *   - the '''spot date''', the base for date calculations, typically two business days after the
 *     trade date, the gap being known as the spot lag;
 *   - the '''start date''', the date accrual starts, generally the spot date unless the instrument
 *     is forward starting;
 *   - the '''end date''', the date accrual ends.
 *
 * The period from the start date to the end date is the [[Tenor]]. A market tenor carries that
 * tenor and additionally allows the market conventional spot lag to be overridden, which is what
 * the four special codes of the market do:
 *
 *   - `ON` - Overnight, from the trade date to the next day: a spot lag of 0 and a tenor of 1 day;
 *   - `TN` - Tomorrow-Next, from tomorrow to the next day: a spot lag of 1 and a tenor of 1 day;
 *   - `SN` - Spot-Next, from spot to the next day: the market conventional spot lag and a tenor of
 *     1 day;
 *   - `SW` - Spot-Week, one week from spot: the market conventional spot lag and a tenor of 1 week;
 *   - "normal" tenors - `2W`, `1M`, `1Y` and the rest - one period from spot, with the market
 *     conventional spot lag.
 *
 * Note that where the market conventional spot lag is one day, `TN` and `SN` resolve to the same
 * dates, and that `SN` and `SW` exist for clarity - they could also be written `1D` and `1W` with
 * the spot implied, which is exactly what [[MarketTenor.ofSpot]] does with those tenors. Other
 * combinations are possible in theory but tend not to exist in the market: a three day trade
 * starting tomorrow would need a code such as `T3D`, and no such code is defined.
 *
 * ===Identity and equality===
 *
 * The [[code]] '''is''' the identity of a market tenor, and equality and hashing consider nothing
 * else. That is sound rather than merely convenient, because the code determines the other two
 * elements: the four codes above are fixed values, and the code of any other market tenor is the
 * name of its tenor, which is itself a total and injective function of that tenor's period. Two
 * market tenors with the same code therefore always carry the same tenor and the same spot lag.
 *
 * The code is consequently the single element of this type, with the tenor and the spot lag
 * indicator supplied by the companion alongside it, which is how equality is kept to the code
 * without a hand-written `equals` that could drift from `hashCode`. The same shape is used by
 * [[Tenor]], whose name is likewise a function of its single element.
 *
 * ===Ordering, failure and text===
 *
 *   - '''Two comparisons are published, and they differ deliberately.''' [[compareTo]] ranks two
 *     codes as the market ranks them, and the `cats.Order` instance on the companion refines that
 *     ranking into a total order, as its own documentation describes.
 *   - '''Nothing raises.''' A count or a text the type cannot accept is reported as a
 *     [[com.opengamma.strata.collect.result.Failure]] on the left of the result, by whichever
 *     factory of the companion was called.
 *   - '''The code is the whole of the text form.''' The JSON codec on the companion is the only
 *     serialized form of a market tenor, and it writes the bare code.
 *
 * There is no public constructor, no `apply` and no `copy`: a market tenor exists only because one
 * of the companion's factories produced it, which is what keeps the code, the tenor and the spot
 * lag indicator of every value in agreement.
 *
 * This type is immutable and every member is a pure function of the value and its arguments, so it
 * is safe to share between threads without synchronisation.
 *
 * @param code  the code of the market tenor, such as `ON`, `SW` or `3M`, which is its identity
 * @see [[Tenor]] for the tenor of the instrument, from its start date to its end date
 * @see [[DaysAdjustment]] for the spot lag a market tenor may override
 */
sealed abstract case class MarketTenor private (code: String) extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry a code disagreeing with its tenor or with its spot lag - can be stopped is here.
  // The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[MarketTenor.Impl])

  // The invariant of this type, stated over the three fields the instance actually holds rather
  // than over the arguments a factory was given, because the class file of the implementation
  // carries a public constructor whatever the source asked for: a class compiled outside this
  // library can reach it directly, and the check above would admit what it built, its runtime
  // class being the one class that check admits. What is left to state is the agreement the
  // constants and [[MarketTenor.ofSpot]] establish between a code, the tenor it names and the
  // spot lag it implies - `ON` and `TN` are one day out at a named lag, `SN` and `SW` are the
  // spot-starting day and week, and any other code is the name of its own tenor - which is what
  // stops a value carrying `ON` with a tenor of three years, or a code that its own tenor is not
  // named by.
  JvmClosure.requireInvariant(
    "its tenor and its spot lag indicator are the ones its code names",
    MarketTenor.codeAgrees(code, tenor, spotLagIndicator))

  /**
   * The tenor of the instrument, from its start date to its end date.
   *
   * The tenor of `ON`, `TN` and `SN` is one day and the tenor of `SW` is one week; the tenor of
   * any other market tenor is the tenor its code names.
   *
   * @return the tenor of the instrument
   */
  def tenor: Tenor

  /**
   * The spot lag this market tenor imposes, or the marker standing for the market convention.
   *
   * The value is 0 for `ON` and 1 for `TN`, each naming the number of business days between the
   * trade date and the start date outright, and the marker value `Int.MaxValue` for `SN`, `SW` and
   * every normal tenor, standing for "whatever lag the market convention gives". The marker is a
   * value of this element rather than a missing one because it must sort after every real lag,
   * which is what places the spot-starting tenors after `ON` and `TN` in [[compareTo]].
   *
   * [[isNonStandardSpotLag]] is the query callers want; this element is exposed because it is what
   * [[compareTo]] reads.
   *
   * @return the spot lag in business days, or the marker for the market conventional lag
   */
  def spotLagIndicator: Int

  /**
   * Checks whether the market tenor implies a non-standard spot lag.
   *
   * This is true for `ON` and `TN`, which need special date handling because they start before the
   * spot date, and false for `SN`, `SW` and every normal tenor, all of which imply the market
   * conventional spot.
   *
   * @return true if this market tenor overrides the market conventional spot lag
   */
  def isNonStandardSpotLag: Boolean = spotLagIndicator != MarketTenor.MarketConventionLag

  /**
   * Adjusts the market conventional spot lag to match this market tenor.
   *
   * The resulting lag is zero business days for `ON` and one business day for `TN`; for `SN`, `SW`
   * and every normal tenor the lag supplied is returned unchanged. The overriding lag is expressed
   * as a business day adjustment against the '''result calendar''' of the lag supplied - rather
   * than as plain date arithmetic - so that the date it produces is a valid business day of the
   * same calendar the market convention would have used.
   *
   * {{{
   * val conventional = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO)
   * MarketTenor.ON.adjustSpotLag(conventional)  // 0 business days, GBLO
   * MarketTenor.TN.adjustSpotLag(conventional)  // 1 business day, GBLO
   * MarketTenor.SN.adjustSpotLag(conventional)  // the conventional lag, unchanged
   * }}}
   *
   * This cannot fail: it reads a calendar identifier from the adjustment supplied and names a new
   * adjustment over it, and neither step resolves a calendar or validates anything. Resolution
   * happens later, when the adjustment returned is applied to a date against some reference data.
   *
   * @param marketConventionalSpotLag  the market conventional spot lag of the instrument
   * @return the spot lag to use for this market tenor
   */
  def adjustSpotLag(marketConventionalSpotLag: DaysAdjustment): DaysAdjustment =
    if (isNonStandardSpotLag) {
      // going through the business day form ensures the result is a valid business day
      DaysAdjustment.ofBusinessDays(spotLagIndicator, marketConventionalSpotLag.resultCalendar)
    } else {
      marketConventionalSpotLag
    }

  /**
   * Compares this market tenor to another by when it starts and how long it runs.
   *
   * Comparing tenors is a hard problem in general, but for the codes in common use the outcome is
   * the expected one: `ON` and `TN` come first, in that order, because they start before the spot
   * date, and everything that starts at spot follows in the order [[Tenor.compareTo]] gives its
   * tenors. That is achieved by comparing the spot lag indicators whenever either side overrides
   * the market convention - the marker for the conventional lag being larger than any real lag -
   * and the tenors otherwise.
   *
   * This comparison is '''not''' the `cats.Order` of the companion, and the difference is the point
   * of having both. This method can return zero for market tenors that are not equal, `12M` against
   * `1Y` being the standard example, because it compares estimated length; the instance breaks such
   * a tie by code, which its laws require. A caller ranking two codes the way the market ranks them
   * wants this method, and a caller sorting or keying a collection wants the instance.
   *
   * @param other  the other market tenor
   * @return negative if this market tenor is earlier or shorter, zero if the two rank equal, and
   *   positive if it is later or longer
   */
  def compareTo(other: MarketTenor): Int =
    if (isNonStandardSpotLag || other.isNonStandardSpotLag) {
      java.lang.Integer.compare(spotLagIndicator, other.spotLagIndicator)
    } else {
      tenor.compareTo(other.tenor)
    }

  /**
   * Returns the code of the market tenor, such as `ON`, `SW` or `3M`.
   *
   * This is [[code]]: the text the market tenor is known by, rather than the structural rendering a
   * case class would otherwise produce.
   *
   * @return the code of the market tenor
   */
  override def toString: String = code

}

/**
 * Provides the four market codes as constants, the factories that build any other market tenor, and
 * the instances of the type.
 *
 * ===Constants===
 *
 * The four constants are named for the codes they carry - `ON`, `TN`, `SN`, `SW`. They are values
 * rather than results, because their codes and tenors are known to be acceptable, and they are
 * built by the same private code the factories use, so a constant and the equivalent factory call
 * produce equal market tenors.
 *
 * ===Factories===
 *
 * Every factory reports a rejected input as a failure rather than by throwing, and every one of
 * them answers with the single-failure form, which is what makes them compose:
 *
 * {{{
 * for {
 *   tenor <- Tenor.ofMonths(months)
 *   market <- MarketTenor.ofSpot(tenor)
 * } yield market
 * }}}
 *
 * Only the counted factories and [[MarketTenor.parse]] can actually fail, and they fail for one
 * reason each: a count that is not a tenor, or text that names neither a market code nor a tenor.
 * [[MarketTenor.ofSpot]] cannot fail, because its argument is already a tenor and every tenor has a
 * spot-starting market tenor; it answers in the same form as the rest so that a caller threading a
 * tenor through it writes one `flatMap` rather than choosing between two shapes, and so that the
 * whole factory surface of this type has one shape. A caller holding a tenor and wanting no error
 * channel can read the result of `ofSpot` as the total function it is, since it is documented never
 * to fail.
 *
 * Where the underlying tenor factory reports several reasons at once - a count can be both zero and
 * negative in principle, though not in practice - they are combined into one failure by
 * [[com.opengamma.strata.collect.result.Failure.collapse]], because a market tenor is parsed and
 * built through a single error channel.
 *
 * ===Instances===
 *
 * The companion declares one equality-bearing instance, one rendering and one codec, which is the
 * convention of this library: `Order` and `Hash` both extend `Eq`, so declaring them as a single
 * value makes it impossible for equality, hashing and ordering to disagree, and there is
 * deliberately no separate `Eq`.
 */
object MarketTenor {

  /**
   * The marker held as the spot lag indicator of every market tenor that starts at spot.
   *
   * The value is `Int.MaxValue`, chosen because the comparison relies on the marker sorting after
   * every lag a market tenor could name outright. It is private: outside this file the question is
   * asked through [[MarketTenor.isNonStandardSpotLag]], which is what the value means, rather than
   * by comparing against the marker.
   *
   * This is declared before the constants below because they read it while this object initialises.
   */
  private final val MarketConventionLag: Int = Int.MaxValue

  /**
   * The period of one day, against which a tenor is tested for the `SN` code.
   *
   * Held once rather than rebuilt per call; `java.time.Period` is immutable.
   */
  private val OneDay: Period = Period.ofDays(1)

  /**
   * The longest text a market tenor is parsed from, which is the tenor grammar's own bound.
   *
   * Every text this type reads is either one of the four two-character market codes or the name
   * of a tenor, so its grammar is the tenor grammar plus four literals and its ceiling is the
   * tenor's: 256 characters, which is four times the longest ISO-8601 period that can name a
   * tenor at all. Holding the number here rather than reading
   * [[com.opengamma.strata.basics.date.Tenor]]'s own private constant keeps this type's ceiling
   * a statement about this type's grammar, and the two are the same value because the grammars
   * are.
   *
   * It bounds work rather than meaning. The four comparisons and the tenor parse that follows
   * them each read text that arrived from outside this library, and the tenor parse copies it
   * (CWE-400/CWE-770); testing the length first means none of that is reached by text no market
   * tenor could be named by.
   */
  private val MaxTextLength: Int = 256

  /**
   * Reported for text that is longer than a market tenor can be.
   *
   * The message names the ceiling and not the text, which is the one place this port departs
   * from quoting what it refused: the text is refused precisely for being too large to write
   * anywhere, and the caller needs the bound rather than the input to correct it. This is the
   * wording [[com.opengamma.strata.collect.Decimal]] reports for the same condition, with the
   * name of this grammar in place of its own.
   */
  private val MaxTextLengthMessage: String =
    s"Market tenor string must not exceed $MaxTextLength characters"

  /**
   * The period of one week, against which a tenor is tested for the `SW` code.
   *
   * This is the period of seven days, `Period.ofWeeks(1)` and `Period.ofDays(7)` being the same
   * period, so a tenor of `1W` and a tenor built from seven days both match it.
   */
  private val OneWeek: Period = Period.ofDays(7)

  /** A market tenor code for Overnight, meaning from the trade date to the next day. */
  val ON: MarketTenor = create("ON", Tenor.TENOR_1D, 0)

  /** A market tenor code for Tomorrow-Next, meaning from tomorrow to the next day. */
  val TN: MarketTenor = create("TN", Tenor.TENOR_1D, 1)

  /**
   * A market tenor code for Spot-Next, meaning from the spot date to the next day.
   *
   * The spot date is usually two working days after the trade date, but this varies by currency.
   */
  val SN: MarketTenor = create("SN", Tenor.TENOR_1D, MarketConventionLag)

  /**
   * A market tenor code for Spot-Week, meaning one week starting from the spot date.
   *
   * The spot date is usually two working days after the trade date, but this varies by currency.
   */
  val SW: MarketTenor = create("SW", Tenor.TENOR_1W, MarketConventionLag)

  /**
   * Obtains a market tenor from a tenor, with the spot date implied.
   *
   * A tenor of one day gives [[SN]] and a tenor of one week gives [[SW]], those codes being how the
   * market writes a spot-starting day and week; any other tenor gives a market tenor whose code is
   * the name of that tenor. Since a tenor of seven days is a tenor of one week, `1W` and `7D` both
   * give `SW`.
   *
   * {{{
   * MarketTenor.ofSpot(Tenor.TENOR_1D)  // Right(SN)
   * MarketTenor.ofSpot(Tenor.TENOR_1W)  // Right(SW)
   * MarketTenor.ofSpot(Tenor.TENOR_3Y)  // Right(3Y)
   * }}}
   *
   * This cannot fail: every tenor has a spot-starting market tenor, and the argument is already a
   * tenor, so nothing about it is left to check. It answers in the form the rest of the factory
   * surface uses so that the two compose, as the companion's documentation explains.
   *
   * @param tenor  the tenor of the instrument
   * @return the market tenor starting at spot for that tenor, always a `Right`
   */
  def ofSpot(tenor: Tenor): FailureOr[MarketTenor] =
    if (tenor.period == OneDay) {
      Right(SN)
    } else if (tenor.period == OneWeek) {
      Right(SW)
    } else {
      Right(create(tenor.name, tenor, MarketConventionLag))
    }

  /**
   * Obtains a market tenor from a number of days from spot.
   *
   * One day gives [[SN]] and seven days gives [[SW]]; any other count gives the market tenor of the
   * tenor of that many days, whose code is named in weeks where the count is a multiple of seven -
   * fourteen days gives `2W`, and twenty days gives `20D`.
   *
   * The count has to be a tenor, so it must be positive and non-zero; zero and negative counts are
   * rejected with the reason the tenor factory gives.
   *
   * @param days  the number of days from spot, which must be positive and non-zero
   * @return the market tenor, or the failure naming the broken constraint: a count of zero or a
   *   negative count is not a tenor
   */
  def ofSpotDays(days: Int): FailureOr[MarketTenor] =
    // the two special counts are answered without building a tenor; routing them through the
    // general path below would produce the same two values
    if (days == 1) {
      Right(SN)
    } else if (days == 7) {
      Right(SW)
    } else {
      spotStarting(Tenor.ofDays(days))
    }

  /**
   * Obtains a market tenor from a number of months from spot.
   *
   * Months are not normalised into years, so twelve months gives the code `12M` rather than `1Y`,
   * which is the naming of the tenor it carries.
   *
   * @param months  the number of months from spot, which must be positive and non-zero
   * @return the market tenor, or the failure naming the broken constraint: a count of zero or a
   *   negative count is not a tenor
   */
  def ofSpotMonths(months: Int): FailureOr[MarketTenor] = spotStarting(Tenor.ofMonths(months))

  /**
   * Obtains a market tenor from a number of years from spot.
   *
   * @param years  the number of years from spot, which must be positive and non-zero
   * @return the market tenor, or the failure naming the broken constraint: a count of zero or a
   *   negative count is not a tenor
   */
  def ofSpotYears(years: Int): FailureOr[MarketTenor] = spotStarting(Tenor.ofYears(years))

  /**
   * Parses a market tenor from text.
   *
   * The four market codes `ON`, `TN`, `SN` and `SW` are accepted as they stand, and any other text
   * is parsed as a tenor and read as starting at spot, which means every form [[Tenor.parse]]
   * accepts: the canonical name of a tenor, such as `2M`, and its ISO-8601 period form, such as
   * `P2M`. Because the code of a market tenor is always one of those forms, the text this type
   * renders itself as always parses back to the same value.
   *
   * {{{
   * MarketTenor.parse("ON")   // Right(ON)
   * MarketTenor.parse("2M")   // Right(2M)
   * MarketTenor.parse("P2M")  // Right(2M), the same value
   * MarketTenor.parse("1W")   // Right(SW), a spot-starting week being written SW
   * MarketTenor.parse("")     // Left(Failure.Invalid)
   * MarketTenor.parse("2K")   // Left(Failure.Parsing)
   * MarketTenor.parse("-2D")  // Left(Failure.Invalid), a tenor being positive and non-zero
   * }}}
   *
   * Empty text is rejected before anything else is attempted, so a caller that supplied nothing is
   * told that rather than being told that nothing is not a period. Anything else that names no
   * market tenor is reported with the reason the tenor parse gives, which distinguishes text that
   * is not a period at all from text that names a period no tenor can carry.
   *
   * Text longer than [[MaxTextLength]] characters is refused ahead of all of that, naming the
   * ceiling rather than the text: no market tenor is named by text of that size - the grammar is
   * the tenor grammar and four two-character codes - and refusing it first is what keeps the
   * four comparisons and the copy the tenor parse makes off text written to be large
   * (CWE-400/CWE-770). Empty text still reaches the argument check above, being well inside the
   * ceiling, and every text within the ceiling reads exactly as it did.
   *
   * @param toParse  the text to parse
   * @return the market tenor the text names, or the failure naming the broken constraint: the text
   *   must not be empty, and text that is none of the four market codes must name a period that is
   *   positive and non-zero
   */
  def parse(toParse: String): FailureOr[MarketTenor] =
    if (toParse.length > MaxTextLength) {
      Left(Failure.Parsing(MaxTextLengthMessage))
    } else {
      Validate
        .notEmpty(toParse, "toParse")
        .toEither
        .left
        .map(Failure.collapse)
        .flatMap {
          case "ON" => Right(ON)
          case "TN" => Right(TN)
          case "SN" => Right(SN)
          case "SW" => Right(SW)
          case text => Tenor.parse(text).flatMap(ofSpot)
        }
    }

  // Reads a tenor that has just been built from a count and gives the spot-starting market tenor
  // for it, combining the reasons the count was rejected for into the single failure this type
  // reports. This is the shared tail of the three counted factories, so they cannot disagree about
  // either the code they produce or the shape of the failure they report.
  private def spotStarting(built: ResultNec[Tenor]): FailureOr[MarketTenor] =
    built.left.map(Failure.collapse).flatMap(ofSpot)

  // The only construction of the type. The implementation class supplies the tenor and the spot
  // lag indicator, which is why they can be vals on every instance while staying out of the
  // equality the case class derives from its single code element.
  //
  // The constructor of a `sealed abstract case class` is reachable only from inside this file, and
  // this is the sole place in the file that reaches it, so no caller anywhere can build a market
  // tenor whose code disagrees with its tenor or with its spot lag.
  private def create(code: String, marketTenor: Tenor, spotLag: Int): MarketTenor =
    new Impl(code, marketTenor, spotLag)

  // Whether a code, a tenor and a spot lag indicator are a triple this companion pairs, which is
  // the invariant a market tenor states in its own constructor. It restates as a function of the
  // three fields what the four constants and [[ofSpot]] decide between them, because the
  // invariant reads the fields an instance holds and has no memory of which of the five built it:
  //
  //   - the four market codes carry the tenor and the lag their constant was declared with, the
  //     tenors being compared by period, which is their equality;
  //   - any other code is the name of the tenor it carries, at the market conventional lag, and
  //     its tenor is neither a day nor a week - those two being the periods `ofSpot` answers with
  //     `SN` and `SW` instead of with a named tenor.
  //
  // The two statements cannot drift apart: every market tenor is built by `create`, and every one
  // built runs this check as part of its construction, so a pairing recorded here that disagreed
  // with the one the constants declare would refuse the first constant this companion builds.
  private def codeAgrees(code: String, marketTenor: Tenor, spotLag: Int): Boolean =
    code match {
      case "ON" => marketTenor.period == OneDay && spotLag == 0
      case "TN" => marketTenor.period == OneDay && spotLag == 1
      case "SN" => marketTenor.period == OneDay && spotLag == MarketConventionLag
      case "SW" => marketTenor.period == OneWeek && spotLag == MarketConventionLag
      case named =>
        named == marketTenor.name && spotLag == MarketConventionLag &&
          marketTenor.period != OneDay && marketTenor.period != OneWeek
    }

  /**
   * The one implementation of a market tenor.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[MarketTenor]] refuse in its own constructor to be any other implementation.
   *
   * The tenor and the spot lag indicator the abstract class declares are supplied here as
   * constructor `val`s, which is what keeps them off the single case element of the type and
   * therefore out of its equality.
   *
   * @param code  the code of the market tenor, already checked by the factory that built it
   * @param tenor  the tenor the code names
   * @param spotLagIndicator  the spot lag the code implies, as the factory settled it
   */
  private final class Impl(
      code: String,
      override val tenor: Tenor,
      override val spotLagIndicator: Int)
      extends MarketTenor(code)

  /**
   * The ordering of market tenors, which is also their hashing and equality.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend `Eq`, so
   * declaring them together is what makes it impossible for the ordering, the hashing and the
   * equality of a market tenor to disagree, and it is why no separate `Eq` is declared.
   *
   * Equality and hashing are those of the value itself, which means the code alone, since that is
   * the only element the case class carries. Ordering is [[MarketTenor.compareTo]] followed by a
   * comparison of codes where that comparison returns zero, and the tie-break is required rather
   * than cosmetic:
   *
   *   - [[MarketTenor.compareTo]] ranks market tenors of equal estimated length equal while they
   *     remain unequal values - `12M` against `1Y` - so on its own it cannot be an `Order`, whose
   *     laws demand that `compare` return zero exactly when the values are equal;
   *   - the code is the whole of the value, so two market tenors share a code exactly when they are
   *     equal, and the tie-break therefore returns zero in precisely the cases equality holds;
   *   - the tie-break cannot reorder a pair that [[MarketTenor.compareTo]] ranked strictly, because
   *     it is consulted only after that comparison has returned zero.
   *
   * The effect is a total order in which `ON` comes first, `TN` second and the spot-starting tenors
   * follow in order of length, with `12M` immediately before `1Y` and any other pair of equal
   * estimated length ordered by code.
   *
   * @return the ordering, hashing and equality of market tenors
   */
  implicit val order: Order[MarketTenor] with Hash[MarketTenor] =
    new Order[MarketTenor] with Hash[MarketTenor] {

      private val universal: Hash[MarketTenor] = Hash.fromUniversalHashCode[MarketTenor]

      override def compare(x: MarketTenor, y: MarketTenor): Int = {
        val byLength = x.compareTo(y)
        if (byLength != 0) byLength else x.code.compareTo(y.code)
      }

      override def eqv(x: MarketTenor, y: MarketTenor): Boolean = universal.eqv(x, y)

      override def hash(x: MarketTenor): Int = universal.hash(x)
    }

  /**
   * The rendering of market tenors as text.
   *
   * A market tenor renders as its bare code - `ON`, `SW`, `3M` - which is what `toString` produces
   * and what [[parse]] accepts.
   *
   * @return the rendering of a market tenor
   */
  implicit val show: Show[MarketTenor] = Show.show(_.code)

  /**
   * The JSON codec for market tenors.
   *
   * A market tenor is written as the bare string of its code, so the document holds `"ON"` or
   * `"3M"` rather than an object. Reading goes through [[parse]], so a document holding `"1W"` or
   * `"P3M"` is accepted as well - both naming values this type writes as `"SW"` and `"3M"` - and
   * text that names no market tenor is reported as a decoding failure.
   *
   * Because the encoded form is a function of the value alone, two equal market tenors always
   * encode to identical bytes, so the encoding is stable across a decode and re-encode.
   *
   * @return the codec reading and writing a market tenor as its code
   */
  implicit val codec: Codec[MarketTenor] = Codecs.parsedStringCodec(parse, _.code)

}
