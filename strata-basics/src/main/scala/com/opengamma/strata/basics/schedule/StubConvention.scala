/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.temporal.ChronoField

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Codec

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A convention defining how to calculate stub periods.
 *
 * A [[PeriodicSchedule periodic schedule]] is determined using a periodic frequency. This splits
 * the schedule into "regular" periods of a fixed length, such as every 3 months. Any remaining
 * days are allocated to irregular "stubs" at the start and/or end.
 *
 * The stub convention is provided as a simple declarative mechanism to define stubs. The
 * convention handles the case of no stubs, or a single stub at the start or end. If there is a
 * stub at both the start and end, then explicit stub dates must be used.
 *
 * For example, dividing a 24 month (2 year) swap into 3 month periods is easy as it splits
 * exactly. However, a 23 month swap cannot be split into even 3 month periods. Instead, there
 * will be a 2 month "initial" stub at the start, a 2 month "final" stub at the end or both an
 * initial and final stub with a combined length of 2 months.
 *
 * The `ShortInitial`, `LongInitial` or `SmartInitial` convention causes the regular periods to be
 * determined ''backwards'' from the end date of the schedule, with remaining days allocated to
 * the stub. The `ShortFinal`, `LongFinal` or `SmartFinal` convention causes the regular periods
 * to be determined ''forwards'' from the start date of the schedule, with remaining days
 * allocated to the stub. The `None` convention may be used to explicitly indicate there are no
 * stubs, and the `Both` convention to indicate there is both an initial and a final stub, which
 * must then be identified by dates.
 *
 * A convention is pure: every operation below is a function of its arguments and of the fixed
 * identity of the member, so the same inputs always produce the same result. Nothing is read from
 * reference data, from configuration or from the class path, and every member is immutable and
 * safe to share between threads.
 *
 * ===A closed family===
 *
 * The family has exactly eight members, every one of them declared in this file:
 *
 *  - `None`, which states that the schedule divides exactly;
 *  - `ShortInitial`, `LongInitial` and `SmartInitial`, which roll backwards and stub at the start;
 *  - `ShortFinal`, `LongFinal` and `SmartFinal`, which roll forwards and stub at the end;
 *  - `Both`, which states that the two stubs are given by dates.
 *
 * The type is `sealed`, its constructor is not visible outside this package, and the name lookup
 * is built from those eight members alone, so nothing can add a ninth. A `match` over a
 * convention is therefore checked for exhaustiveness by the compiler.
 *
 * The members are reached in two ways, both of which yield the same objects:
 *
 * {{{
 * StubConvention.SHORT_INITIAL            // the member itself
 * StubConvention.parse("ShortInitial")    // text, leniently resolved
 * }}}
 *
 * ===What this replaces===
 *
 * The type being ported is a Java `enum` whose text form was derived from its constant
 * identifiers at class-initialization time by a shared, reflective name helper, and whose `of`
 * factory raised an error for text naming no constant. This port keeps both name forms and the
 * whole of the accepted name space, and discards the two mechanisms behind them. Each name is now
 * a string written out beside the member it belongs to, so it is legible in this file rather than
 * computed from an identifier; and rejected text is reported as a value, `valueOf` answering with
 * an `Option` and [[StubConvention.parse]] with a [[com.opengamma.strata.collect.result.Failure]]
 * on the left of an `EitherNec`. The annotations that registered the two directions with the
 * reflective string-conversion library of the original are gone with that library; the JSON codec
 * on the companion is their replacement and is built by the compiler.
 *
 * ===Divergences from the ported type===
 *
 * These are the deliberate differences, recorded here because they belong in the migration note:
 *
 *  - '''An invalid stub declaration is reported rather than raised.''' The ported `toImplicit`
 *    raised a `ScheduleException` carrying the offending schedule definition. That exception type
 *    has no counterpart in this port, so [[toImplicit]] returns `Either[Failure, StubConvention]`
 *    and the six rejections become a [[com.opengamma.strata.collect.result.Failure.Invalid]]
 *    carrying the ported message unchanged, together with the rendered definition under the
 *    attribute `definition`.
 *  - '''The definition is passed as rendered text.''' The ported method took the
 *    [[PeriodicSchedule]] itself, as an argument it permitted to be absent and used only for the
 *    error. This one takes the text that definition renders to, by name, so the schedule is
 *    rendered only when a rejection actually occurs, this file depends on no other type of its
 *    package, and the absent case the ported signature allowed cannot arise. A caller holding a
 *    definition passes `definition.toString`, which is the same text the ported exception carried.
 *  - '''The throwing factory is replaced by a reported one.''' The ported `of(String)` raised an
 *    error for unknown text; [[StubConvention.parse]] reports it, and [[StubConvention.valueOf]]
 *    answers with an `Option`. No throwing factory is published.
 *  - '''Absent arguments are not checked.''' The ported `toRollConvention` checked each of its
 *    three arguments for absence before using them, as a Java reference may be absent. A Scala
 *    reference of these types cannot be, so the three checks are dropped rather than reproduced,
 *    and the method keeps no error channel.
 *  - '''Accessors are renamed to Scala form.''' `getName` is [[name]]. The value it answers with
 *    is unchanged, and `toString` still returns it.
 *  - '''Java serialization is gone.''' No member is serializable through the Java mechanism. JSON
 *    is the wire form, through the codec on the companion, and a member is written as the bare
 *    string of its name.
 *
 * @param name  the unique name of the convention, which is its identity in text and on the wire
 * @see [[RollConvention]] for the convention a stub convention implies
 * @see [[PeriodicSchedule]] for the schedule definition a stub convention is declared in
 */
sealed abstract class StubConvention private[schedule] (val name: String) extends Named {

  /**
   * Converts this stub convention to the appropriate roll convention.
   *
   * This converts a stub convention to a [[RollConvention]] based on the start date, end date,
   * frequency and preference for end-of-month. The net result is to imply the roll convention
   * from the schedule data.
   *
   * The rules are as follows.
   *
   * If the input frequency is month-based, then the implied convention is based on the
   * day-of-month of the initial date, where the initial date is the start date if rolling forwards
   * or the end date otherwise. If that date is on the 31st day, or if `preferEndOfMonth` is true
   * and the relevant date is at the end of the month, then the implied convention is `EOM`. For
   * example, if the initial date of the sequence is 2014-06-20 and the periodic frequency is `P3M`
   * (month-based), then the implied convention is `Day20`.
   *
   * If the input frequency is week-based, then the implied convention is based on the day-of-week
   * of the initial date, again the start date if rolling forwards and the end date otherwise. For
   * example, if the initial date of the sequence is 2014-06-20 and the periodic frequency is `P2W`
   * (week-based), then the implied convention is `DayFri`, because 2014-06-20 is a Friday.
   *
   * In all other cases, the implied convention is `None`.
   *
   * `None` is rolled forwards, so it derives its convention from the start date - except in the
   * one case this convention has to settle on its own. Where `None` is given a month-based
   * frequency and two dates that disagree about the day of the month, lie in different months and
   * have at least one of them at a month end, neither date's day-of-month can be taken at face
   * value: 2014-01-31 to 2014-04-30 is a month-end schedule rather than a 31st-of-the-month one.
   * Such a pair therefore yields `EOM` when the end of the month is preferred, and otherwise the
   * later of the two days of the month, which is the day that generates the longer month:
   *
   * {{{
   * NONE.toRollConvention(2014-01-31, 2014-04-30, P1M, true)   // EOM
   * NONE.toRollConvention(2014-06-30, 2014-09-30, P3M, false)  // Day30 - the two days agree
   * NONE.toRollConvention(2016-03-16, 2016-03-31, P6M, true)   // Day16 - one month, so no doubt
   * NONE.toRollConvention(2016-03-16, 2017-03-31, P6M, true)   // EOM
   * SHORT_INITIAL.toRollConvention(2014-01-14, 2014-06-30, P1M, false)  // Day30, from the end
   * SHORT_FINAL.toRollConvention(2014-06-30, 2014-08-16, P1M, false)    // Day30, from the start
   * }}}
   *
   * The result is total: every date has a day-of-month between 1 and 31 and a day-of-week, so
   * every combination of arguments names a convention and there is nothing to report.
   *
   * @param start  the start date of the schedule
   * @param end  the end date of the schedule
   * @param frequency  the periodic frequency of the schedule
   * @param preferEndOfMonth  whether to prefer the end-of-month when rolling
   * @return the derived roll convention
   */
  def toRollConvention(
      start: LocalDate,
      end: LocalDate,
      frequency: Frequency,
      preferEndOfMonth: Boolean): RollConvention =
    if (this == StubConvention.NONE && frequency.isMonthBased &&
      StubConvention.straddlesMonthEnd(start, end)) {
      if (preferEndOfMonth) {
        RollConventions.EOM
      } else {
        // Both days of the month come from a LocalDate, so the greater of them is between 1 and
        // 31 and the total factory is the right one; 31 normalises to EOM, as it does for any
        // other caller.
        RollConvention.ofDayOfMonthUnsafe(math.max(start.getDayOfMonth, end.getDayOfMonth))
      }
    } else if (isCalculateBackwards) {
      StubConvention.impliedRollConvention(end, frequency, preferEndOfMonth)
    } else {
      StubConvention.impliedRollConvention(start, frequency, preferEndOfMonth)
    }

  //-------------------------------------------------------------------------
  /**
   * Converts this stub convention to one that creates implicit stubs, validating that any explicit
   * stubs are correct.
   *
   * Stubs can be specified in two ways, using dates or using this convention. This method is
   * passed flags indicating whether explicit stubs have been specified using dates. It validates
   * that such stubs are compatible, and returns a convention suitable for creating stubs
   * implicitly during rolling. For example, an invalid stub convention would be to specify two
   * stubs using explicit dates but declaring the convention as `ShortFinal`.
   *
   * The result is the implicit stub convention to apply between the two calculation dates. For
   * example, if an initial stub is defined by dates then it cannot also be created automatically,
   * thus the implicit stub convention is `None`.
   *
   * Where the ported method raised a `ScheduleException`, this one reports the rejection as a
   * [[com.opengamma.strata.collect.result.Failure.Invalid]] whose message is the ported one and
   * whose `definition` attribute holds the rendered definition. The definition is taken by name,
   * so it is rendered only when a rejection occurs; a caller holding a [[PeriodicSchedule]] passes
   * `definition.toString`.
   *
   * Each member states its own answer rather than inheriting one, so the eight decisions are
   * legible beside the members they belong to and a member added later cannot silently adopt
   * another's rule.
   *
   * @param definition  the text the schedule definition renders to, evaluated only on rejection
   * @param explicitInitialStub  an initial stub has been explicitly defined by dates
   * @param explicitFinalStub  a final stub has been explicitly defined by dates
   * @return the effective stub convention, or the failure describing why the stubs are invalid
   */
  private[schedule] def toImplicit(
      definition: => String,
      explicitInitialStub: Boolean,
      explicitFinalStub: Boolean): Either[Failure, StubConvention]

  /**
   * Decides if the period between two dates implies a long stub.
   *
   * This is used by the smart conventions: `SmartInitial` and `SmartFinal` absorb a stub shorter
   * than seven days into the neighbouring period and retain one of seven days or more, while
   * `LongInitial` and `LongFinal` always absorb it and the remaining four members never do. The
   * seven days are counted on unadjusted dates, as they are in the library being ported.
   *
   * The two dates are the boundaries of the candidate stub, in order; the answer says whether the
   * boundary between them should be deleted so that the stub merges with the period next to it.
   *
   * @param date1  the first date
   * @param date2  the second date
   * @return true if a long stub should be created by deleting one of the two input dates
   */
  private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean

  //-------------------------------------------------------------------------
  /**
   * Checks if the schedule is calculated forwards from the start date to the end date.
   *
   * If true, then there will typically be a stub at the end of the schedule.
   *
   * The `None`, `ShortFinal`, `LongFinal` and `SmartFinal` conventions return true. Other
   * conventions return false. `None` is among them: a schedule that declares no stub is still
   * generated from its start date, which is what makes this question, rather than [[isFinal]], the
   * one a generator asks to choose its direction.
   *
   * @return true if calculation occurs forwards from the start date to the end date
   */
  def isCalculateForwards: Boolean =
    this == StubConvention.SHORT_FINAL || this == StubConvention.LONG_FINAL ||
      this == StubConvention.SMART_FINAL || this == StubConvention.NONE

  /**
   * Checks if the schedule is calculated backwards from the end date to the start date.
   *
   * If true, then there will typically be a stub at the start of the schedule.
   *
   * The `ShortInitial`, `LongInitial` and `SmartInitial` conventions return true. Other
   * conventions return false, so `Both` - whose stubs are given by dates - is neither forwards nor
   * backwards by this pair of questions.
   *
   * @return true if calculation occurs backwards from the end date to the start date
   */
  def isCalculateBackwards: Boolean =
    this == StubConvention.SHORT_INITIAL || this == StubConvention.LONG_INITIAL ||
      this == StubConvention.SMART_INITIAL

  /**
   * Checks if this convention tries to produce a final stub.
   *
   * If true, then there will typically be a stub at the end of the schedule.
   *
   * The `ShortFinal`, `LongFinal` and `SmartFinal` conventions return true. Other conventions
   * return false, `None` included, which is what distinguishes this question from
   * [[isCalculateForwards]].
   *
   * @return true if this convention tries to produce a stub at the end of the schedule
   */
  def isFinal: Boolean =
    this == StubConvention.SHORT_FINAL || this == StubConvention.LONG_FINAL ||
      this == StubConvention.SMART_FINAL

  /**
   * Checks if this convention tries to produce a long stub.
   *
   * The `LongInitial` and `LongFinal` conventions return true. Other conventions return false,
   * the two smart conventions among them: they produce a long stub only for a gap of less than
   * seven days, which is a question about dates and is answered by [[isStubLong]].
   *
   * @return true if there may be a long stub
   */
  def isLong: Boolean =
    this == StubConvention.LONG_INITIAL || this == StubConvention.LONG_FINAL

  /**
   * Checks if this convention tries to produce a short stub.
   *
   * The `ShortInitial` and `ShortFinal` conventions return true. Other conventions return false.
   *
   * @return true if there may be a short stub
   */
  def isShort: Boolean =
    this == StubConvention.SHORT_INITIAL || this == StubConvention.SHORT_FINAL

  /**
   * Checks if this convention uses smart rules to create a stub.
   *
   * The `SmartInitial` and `SmartFinal` conventions return true. Other conventions return false.
   *
   * @return true if this convention decides the length of its stub from the dates
   */
  def isSmart: Boolean =
    this == StubConvention.SMART_INITIAL || this == StubConvention.SMART_FINAL

  //-------------------------------------------------------------------------
  /**
   * Returns the formatted name of the type.
   *
   * This is the same string as [[name]], so a convention interpolated into a message renders as
   * the mixed-case form the type being ported rendered, and agrees with the `Show` instance and
   * with the JSON representation.
   *
   * @return the formatted string representing the type
   */
  override def toString: String = name
}

/**
 * Provides the eight stub conventions, together with the name lookup, typeclass instances and
 * JSON codec for them.
 *
 * The members are declared in the order of the enum constants being ported, and [[values]]
 * preserves that order. Each member carries its own answer for the two package-private decisions
 * - the validation of explicit stubs and the seven-day smart rule - so the behaviour of a
 * convention is read beside the convention itself, exactly as it was read beside the constant it
 * is ported from.
 *
 * The identifiers are the screaming-snake names of the ported constants, so a reader moving
 * between the two implementations finds the same identifiers, while the canonical names remain
 * the mixed-case strings the ported name helper produced.
 */
object StubConvention {

  /**
   * Explicitly states that there are no stubs.
   *
   * This is used to indicate that the term of the schedule evenly divides by the periodic
   * frequency leaving no stubs. For example, a 6 month trade can be exactly divided by a 3 month
   * frequency.
   *
   * If the term of the schedule is less than the frequency, then only one period exists. In this
   * case, the period is not treated as a stub.
   *
   * When creating a schedule, there must be no explicit stubs.
   */
  case object NONE extends StubConvention("None") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitInitialStub || explicitFinalStub) {
        rejected("Dates specify an explicit stub, but stub convention is 'None'", definition)
      } else {
        Right(NONE)
      }

    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean = false
  }

  /**
   * A short initial stub.
   *
   * The schedule periods will be determined backwards from the regular period end date. Any
   * remaining period, shorter than the standard frequency, will be allocated at the start. For
   * example, an 8 month trade with a 3 month periodic frequency would result in a 2 month initial
   * short stub followed by two periods of 3 months.
   *
   * If there is no remaining period when calculating, then there is no stub. For example, a 6
   * month trade can be exactly divided by a 3 month frequency.
   *
   * When creating a schedule, there must be no explicit final stub. If there is an explicit
   * initial stub, then this convention is considered to be matched and the remaining period is
   * calculated using the stub convention `None`.
   */
  case object SHORT_INITIAL extends StubConvention("ShortInitial") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitFinalStub) {
        rejected(
          "Dates specify an explicit final stub, but stub convention is 'ShortInitial'",
          definition)
      } else if (explicitInitialStub) {
        Right(NONE)
      } else {
        Right(SHORT_INITIAL)
      }

    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean = false
  }

  /**
   * A long initial stub.
   *
   * The schedule periods will be determined backwards from the regular period end date. Any
   * remaining period, shorter than the standard frequency, will be allocated at the start and
   * combined with the next period, making a total period longer than the standard frequency. For
   * example, an 8 month trade with a 3 month periodic frequency would result in a 5 month initial
   * long stub followed by one period of 3 months.
   *
   * If there is no remaining period when calculating, then there is no stub. For example, a 6
   * month trade can be exactly divided by a 3 month frequency.
   *
   * When creating a schedule, there must be no explicit final stub. If there is an explicit
   * initial stub, then this convention is considered to be matched and the remaining period is
   * calculated using the stub convention `None`.
   */
  case object LONG_INITIAL extends StubConvention("LongInitial") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitFinalStub) {
        rejected(
          "Dates specify an explicit final stub, but stub convention is 'LongInitial'",
          definition)
      } else if (explicitInitialStub) {
        Right(NONE)
      } else {
        Right(LONG_INITIAL)
      }

    /** A long initial stub is always merged into the period next to it, whatever the dates are. */
    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean = true
  }

  /**
   * A smart initial stub.
   *
   * The schedule periods will be determined backwards from the regular period end date. Any
   * remaining period, shorter than the standard frequency, will be allocated at the start. If this
   * results in a stub of less than 7 days, the stub will be combined with the next period. If this
   * results in a stub of 7 days or more, the stub will be retained. This is the equivalent of
   * [[LONG_INITIAL]] up to 7 days and [[SHORT_INITIAL]] beyond that. The 7 days are calculated
   * based on unadjusted dates. This convention appears to match that used by Bloomberg.
   *
   * If there is no remaining period when calculating, then there is no stub. For example, a 6
   * month trade can be exactly divided by a 3 month frequency.
   *
   * If there is an explicit initial stub, then this convention is considered to be matched and the
   * remaining period is calculated using the stub convention `None`. An explicit final stub is
   * accepted rather than rejected, and combines with an explicit initial stub to give `Both`,
   * which is what makes this convention - and [[SMART_FINAL]] - the two that never reject a
   * definition.
   */
  case object SMART_INITIAL extends StubConvention("SmartInitial") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitFinalStub) {
        Right(if (explicitInitialStub) BOTH else SMART_INITIAL)
      } else {
        Right(if (explicitInitialStub) NONE else SMART_INITIAL)
      }

    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean =
      isShorterThanSmartThreshold(date1, date2)
  }

  /**
   * A short final stub.
   *
   * The schedule periods will be determined forwards from the regular period start date. Any
   * remaining period, shorter than the standard frequency, will be allocated at the end. For
   * example, an 8 month trade with a 3 month periodic frequency would result in two periods of 3
   * months followed by a 2 month final short stub.
   *
   * If there is no remaining period when calculating, then there is no stub. For example, a 6
   * month trade can be exactly divided by a 3 month frequency.
   *
   * When creating a schedule, there must be no explicit initial stub. If there is an explicit
   * final stub, then this convention is considered to be matched and the remaining period is
   * calculated using the stub convention `None`.
   */
  case object SHORT_FINAL extends StubConvention("ShortFinal") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitInitialStub) {
        rejected(
          "Dates specify an explicit initial stub, but stub convention is 'ShortFinal'",
          definition)
      } else if (explicitFinalStub) {
        Right(NONE)
      } else {
        Right(SHORT_FINAL)
      }

    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean = false
  }

  /**
   * A long final stub.
   *
   * The schedule periods will be determined forwards from the regular period start date. Any
   * remaining period, shorter than the standard frequency, will be allocated at the end and
   * combined with the previous period, making a total period longer than the standard frequency.
   * For example, an 8 month trade with a 3 month periodic frequency would result in one period of
   * 3 months followed by a 5 month final long stub.
   *
   * If there is no remaining period when calculating, then there is no stub. For example, a 6
   * month trade can be exactly divided by a 3 month frequency.
   *
   * When creating a schedule, there must be no explicit initial stub. If there is an explicit
   * final stub, then this convention is considered to be matched and the remaining period is
   * calculated using the stub convention `None`.
   */
  case object LONG_FINAL extends StubConvention("LongFinal") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitInitialStub) {
        rejected(
          "Dates specify an explicit initial stub, but stub convention is 'LongFinal'",
          definition)
      } else if (explicitFinalStub) {
        Right(NONE)
      } else {
        Right(LONG_FINAL)
      }

    /** A long final stub is always merged into the period next to it, whatever the dates are. */
    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean = true
  }

  /**
   * A smart final stub.
   *
   * The schedule periods will be determined forwards from the regular period start date. Any
   * remaining period, shorter than the standard frequency, will be allocated at the end. If this
   * results in a stub of less than 7 days, the stub will be combined with the next period. If this
   * results in a stub of 7 days or more, the stub will be retained. This is the equivalent of
   * [[LONG_FINAL]] up to 7 days and [[SHORT_FINAL]] beyond that. The 7 days are calculated based
   * on unadjusted dates. This convention appears to match that used by Bloomberg.
   *
   * If there is no remaining period when calculating, then there is no stub. For example, a 6
   * month trade can be exactly divided by a 3 month frequency.
   *
   * If there is an explicit final stub, then this convention is considered to be matched and the
   * remaining period is calculated using the stub convention `None`. An explicit initial stub is
   * accepted rather than rejected, and combines with an explicit final stub to give `Both`.
   */
  case object SMART_FINAL extends StubConvention("SmartFinal") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitInitialStub) {
        Right(if (explicitFinalStub) BOTH else SMART_FINAL)
      } else {
        Right(if (explicitFinalStub) NONE else SMART_FINAL)
      }

    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean =
      isShorterThanSmartThreshold(date1, date2)
  }

  /**
   * Both ends of the schedule have a stub.
   *
   * The schedule periods will be determined from two dates - the regular period start date and
   * the regular period end date. Days before the first regular period start date form the initial
   * stub. Days after the last regular period end date form the final stub.
   *
   * When creating a schedule, there must be both an explicit initial and final stub.
   */
  case object BOTH extends StubConvention("Both") {

    override private[schedule] def toImplicit(
        definition: => String,
        explicitInitialStub: Boolean,
        explicitFinalStub: Boolean): Either[Failure, StubConvention] =
      if (explicitInitialStub && explicitFinalStub) {
        Right(NONE)
      } else {
        rejected("Stub convention is 'Both' but explicit dates not specified", definition)
      }

    override private[schedule] def isStubLong(date1: LocalDate, date2: LocalDate): Boolean = false
  }


  //-------------------------------------------------------------------------
  /**
   * The complete set of stub conventions, in declaration order.
   *
   * The order is the declaration order of the enum constants being ported - `None`, the three
   * initial conventions, the three final conventions, then `Both` - and it is part of what this
   * file preserves: it is the order the members claim their lookup keys in, the order the captured
   * reference-data manifest lists them in, and the order a report over the family follows. It is
   * not the order the `Order` instance below imposes, which is alphabetical by name. The list is
   * non-empty by construction and holds exactly eight members.
   *
   * @return the eight conventions, in declaration order
   */
  val values: NonEmptyList[StubConvention] =
    NonEmptyList.of(
      NONE,
      SHORT_INITIAL,
      LONG_INITIAL,
      SMART_INITIAL,
      SHORT_FINAL,
      LONG_FINAL,
      SMART_FINAL,
      BOTH
    )

  /**
   * The spellings accepted in addition to the two keys every member is registered under.
   *
   * The name helper of the type being ported accepted six spellings of each member: the constant
   * identifier, the rendered name, and each of those folded to upper and to lower case. For
   * `SHORT_INITIAL` those are `SHORT_INITIAL`, `short_initial`, `ShortInitial`, `SHORTINITIAL`
   * and `shortinitial` - five distinct strings, the identifier already being upper case. The name
   * lookup of a family registers each member under its rendered name and that name folded to
   * upper case, so this table supplies precisely the remainder, and nothing beyond it:
   *
   *  - the constant identifier and its lower-case form, for the six members whose identifier
   *    differs from their rendered name by more than case - the underscore of `SHORT_INITIAL`
   *    survives no case folding;
   *  - the lower-case form of the rendered name, for every member.
   *
   * `NONE` and `BOTH` therefore need one row each: their identifiers are the upper-case forms of
   * their rendered names and are already derived, leaving only `none` and `both`.
   *
   * Each row maps a spelling to a canonical name rather than to a member, which is the shape the
   * lookup takes, and the lookup expands the table with the upper-case form of every spelling in
   * it. That expansion lands only on keys which already resolve to the same member, so no row here
   * can displace another or redirect a spelling the family already accepted.
   *
   * This is the only table the family declares. It has '''no lenient patterns and no external
   * name groups''', because the configuration of the library being ported declared neither - this
   * family had no configuration resource at all, the whole of its name space having always been
   * derived from its own constants. The two empty tables are passed explicitly below so that the
   * absence is visible here rather than looked for elsewhere.
   */
  private val AlternateNames: Map[String, String] =
    Map(
      "none" -> "None",
      "SHORT_INITIAL" -> "ShortInitial",
      "short_initial" -> "ShortInitial",
      "shortinitial" -> "ShortInitial",
      "LONG_INITIAL" -> "LongInitial",
      "long_initial" -> "LongInitial",
      "longinitial" -> "LongInitial",
      "SMART_INITIAL" -> "SmartInitial",
      "smart_initial" -> "SmartInitial",
      "smartinitial" -> "SmartInitial",
      "SHORT_FINAL" -> "ShortFinal",
      "short_final" -> "ShortFinal",
      "shortfinal" -> "ShortFinal",
      "LONG_FINAL" -> "LongFinal",
      "long_final" -> "LongFinal",
      "longfinal" -> "LongFinal",
      "SMART_FINAL" -> "SmartFinal",
      "smart_final" -> "SmartFinal",
      "smartfinal" -> "SmartFinal",
      "both" -> "Both"
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a convention, and it is built from [[values]]
   * and the alternate spellings above alone. Nothing is read from a class or from the class path,
   * so the name space of the family is fixed when this file is compiled.
   *
   * @return the name lookup for the eight conventions
   */
  implicit val namedEnum: NamedEnum[StubConvention] =
    NamedEnum.of(values, AlternateNames, Nil, Map.empty, "StubConvention")

  //-------------------------------------------------------------------------
  /**
   * Obtains the convention that the specified name identifies, if one does.
   *
   * This is the exact lookup, and it accepts the six spellings of each member that the type being
   * ported accepted, and only those:
   *
   * {{{
   * valueOf("ShortInitial")   // Some(SHORT_INITIAL) - the rendered name
   * valueOf("SHORTINITIAL")   // Some(SHORT_INITIAL) - that name in upper case
   * valueOf("shortinitial")   // Some(SHORT_INITIAL) - and in lower case
   * valueOf("SHORT_INITIAL")  // Some(SHORT_INITIAL) - the constant identifier
   * valueOf("short_initial")  // Some(SHORT_INITIAL) - and in lower case
   * valueOf("Short Initial")  // None - no convention has that name
   * }}}
   *
   * Text of any other shape, including a mixed case the table above does not name, resolves only
   * through [[parse]], which tolerates the case of its input.
   *
   * @param name  the name to look up
   * @return the convention with that name, or `None` when no convention has it
   */
  def valueOf(name: String): Option[StubConvention] = namedEnum.valueOf(name)

  /**
   * Parses a convention from text, tolerating the case of the input.
   *
   * The exact lookup of [[valueOf]] is tried first, so every spelling the type being ported
   * accepted is resolved by it. When that finds nothing, the text is folded to upper case and
   * looked up once more, which is the whole of the leniency available to this family, since it
   * declares no rewrite pattern for the step between the two lookups. The observable result is a
   * lookup that ignores case while respecting every other character, so `sHoRtInItIaL` resolves
   * here where the original rejected it, and `Short Initial` is rejected as it always was.
   *
   * Where the original signalled an unrecognised name by raising an error, this method reports it
   * as a value: the result is `Left` of a chain holding one
   * [[com.opengamma.strata.collect.result.Failure.Parsing]] whose message names both this family
   * and the text that could not be resolved. The returned type is the same as
   * `collect.ResultNec[StubConvention]`, spelled out here for readability.
   *
   * This method replaces the ported `of(String)`, which raised an error; no throwing factory is
   * published by this companion.
   *
   * @param name  the text to parse
   * @return the convention the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, StubConvention] = namedEnum.parse(name)

  //-------------------------------------------------------------------------
  /**
   * The ordering and hashing of conventions.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. All three are derived from `name`, which is sound because the eight names are
   * distinct and each member exists exactly once, so two conventions compare equal if, and only
   * if, they are the same convention - the law the combined instance has to satisfy, and one that
   * also makes this instance agree with `==` and with reference equality. Comparison by name makes
   * the ordering alphabetical rather than the declaration ordering of the enum being ported.
   *
   * @return the ordering of conventions by name, which is also their hashing
   */
  implicit val order: Order[StubConvention] with Hash[StubConvention] =
    NamedEnum.orderByName[StubConvention]

  /**
   * The rendering of conventions as text.
   *
   * A convention renders as its name, which is what `toString` produces as well, so the two ways
   * of putting a convention into a message agree.
   *
   * @return the rendering of a convention as its name
   */
  implicit val show: Show[StubConvention] = NamedEnum.showByName[StubConvention]

  /**
   * The JSON codec for conventions.
   *
   * A convention is written as the bare string of its name - `"ShortInitial"` - and never as an
   * object, which is the single-string form the type being ported wrote through its
   * string-conversion annotations, so a document written before this port is read after it as the
   * same member. This is also the form a [[PeriodicSchedule]] carries its optional stub convention
   * in, its derived product codec picking this instance up. A string is read back through
   * [[parse]], so a document is accepted whatever the case of the name it holds, and one naming no
   * convention of this family is rejected with a decoding failure carrying the message of the
   * parse failure.
   *
   * The codec is assembled by the compiler from the family's own name lookup, taken from the
   * shared JSON helpers of the collect module; nothing about it inspects a type, a class path or a
   * configuration source while the program runs.
   *
   * @return the codec reading and writing a convention as its name
   */
  implicit val codec: Codec[StubConvention] = Codecs.namedEnumCodec[StubConvention]

  //-------------------------------------------------------------------------
  /** The attribute under which a rejected stub declaration carries the schedule definition. */
  private val DefinitionAttribute: String = "definition"

  /**
   * The length below which the two smart conventions absorb a stub into its neighbour.
   *
   * Seven days is the threshold the library being ported used, and it is exclusive: a stub of
   * exactly seven days is retained. It is held as a `Long` because that is the type
   * `LocalDate.plusDays` takes, so no numeric widening occurs at the one call site.
   */
  private val SmartStubThresholdDays: Long = 7L

  /**
   * Reports an invalid stub declaration.
   *
   * Every rejection of this family takes this shape: the reason is `INVALID`, the message is the
   * one the ported exception carried, and the rendered schedule definition is attached under
   * [[DefinitionAttribute]] so that a report can name the definition that was rejected without
   * the message having to embed it. The definition is taken by name and is evaluated here, which
   * is to say only on the path that actually rejects.
   *
   * @param message  the message of the ported exception, verbatim
   * @param definition  the text the rejected schedule definition renders to
   * @return the failure on the left of an `Either`
   */
  private def rejected(message: String, definition: => String): Either[Failure, StubConvention] =
    Left(Failure.Invalid(message).withAttribute(DefinitionAttribute, definition))

  /**
   * Decides whether two dates leave the day-of-month of a month-based schedule undetermined.
   *
   * This is the condition of the one special case in [[StubConvention.toRollConvention]], and it
   * holds when all three of the following do: the two dates disagree about the day of the month,
   * they lie in different months, and at least one of them is the last day of its month. Such a
   * pair describes a schedule rolling on the end of the month rather than on a fixed day, which is
   * why neither day-of-month can be taken at face value. The proleptic month - the count of months
   * since the epoch - is what distinguishes 2016-03-16 to 2016-03-31, where the two dates share a
   * month and the start date settles the convention, from 2016-03-16 to 2017-03-31, where they do
   * not.
   *
   * @param start  the start date of the schedule
   * @param end  the end date of the schedule
   * @return true if the pair implies an end-of-month schedule rather than a fixed day-of-month one
   */
  private def straddlesMonthEnd(start: LocalDate, end: LocalDate): Boolean =
    start.getDayOfMonth != end.getDayOfMonth &&
      start.getLong(ChronoField.PROLEPTIC_MONTH) != end.getLong(ChronoField.PROLEPTIC_MONTH) &&
      (start.getDayOfMonth == start.lengthOfMonth || end.getDayOfMonth == end.lengthOfMonth)

  /**
   * Derives the roll convention implied by one date and a frequency.
   *
   * The date is the one the schedule rolls from - the start date when rolling forwards and the end
   * date when rolling backwards - and it is the only date the derivation reads. The ported helper
   * took the other date as well and never used it, so it is not a parameter here.
   *
   * @param date  the date the schedule rolls from
   * @param frequency  the periodic frequency of the schedule
   * @param preferEndOfMonth  whether to prefer the end-of-month when rolling
   * @return the roll convention the date and frequency imply
   */
  private def impliedRollConvention(
      date: LocalDate,
      frequency: Frequency,
      preferEndOfMonth: Boolean): RollConvention =
    if (frequency.isMonthBased) {
      if (preferEndOfMonth && date.getDayOfMonth == date.lengthOfMonth) {
        RollConventions.EOM
      } else {
        // A day-of-month read from a LocalDate is between 1 and 31, so the total factory applies.
        RollConvention.ofDayOfMonthUnsafe(date.getDayOfMonth)
      }
    } else if (frequency.isWeekBased) {
      RollConvention.ofDayOfWeek(date.getDayOfWeek)
    } else {
      // Neither monthly nor weekly means no known roll convention.
      RollConventions.NONE
    }

  /**
   * Decides whether the gap between two dates is shorter than the smart threshold.
   *
   * This is the rule both smart conventions apply, written once: the stub is absorbed when the
   * second date falls before the seventh day after the first, so a gap of exactly seven days is
   * retained and any shorter gap - including a reversed pair, whose second date is earlier still -
   * is absorbed. The dates are unadjusted, as they are in the library being ported.
   *
   * @param date1  the first date of the candidate stub
   * @param date2  the second date of the candidate stub
   * @return true if the gap is shorter than seven days
   */
  private def isShorterThanSmartThreshold(date1: LocalDate, date2: LocalDate): Boolean =
    date1.plusDays(SmartStubThresholdDays).isAfter(date2)
}

