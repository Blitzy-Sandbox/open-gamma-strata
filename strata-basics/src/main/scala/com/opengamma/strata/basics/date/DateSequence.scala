/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth
import java.time.temporal.TemporalAdjuster
import java.time.temporal.TemporalAdjusters

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.syntax.apply._

import _root_.io.circe.Codec
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.semiauto.deriveDecoder
import _root_.io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A sequence of dates, such as the third Wednesday of March, June, September and December.
 *
 * Exchange-traded contracts settle on dates drawn from a published sequence rather than on
 * dates chosen freely: an interest rate future is identified by the quarterly IMM date it
 * expires on, and a serial future by one of the nearby monthly dates. A sequence is the rule
 * that produces those dates, and this type is the operation of stepping through them - given
 * any date, it answers which sequence date comes next, which comes on or after it, which is
 * the nth from it, and which one belongs to a given month.
 *
 * ===A closed family===
 *
 * The six sequences defined in the companion of this class are the whole family. The class is
 * `sealed`, its constructor is visible only inside this package, and each sequence exists
 * exactly once as a value in the companion, so no further sequence can come into being - not
 * by subclassing from another file, and not by adding one while the program runs. The members
 * are therefore fixed, a match over them can be checked for exhaustiveness, and text naming no
 * member is rejected by `parse` as a value.
 *
 * ===The two families of method, and the one difference between them===
 *
 * Every sequence answers two closely related questions, and the difference between them is
 * whether the input date itself counts as a result:
 *
 *   - `next` and `nth` always return a date strictly later than the input, so a date that is
 *     itself in the sequence is stepped over.
 *   - `nextOrSame` and `nthOrSame` return the input date when it is a date of the sequence.
 *
 * {{{
 * val imm = DateSequences.QUARTERLY_IMM
 * imm.nextOrSame(LocalDate.of(2024, 3, 20))  // 2024-03-20, the third Wednesday itself
 * imm.next(LocalDate.of(2024, 3, 20))        // 2024-06-19, the following quarterly date
 * imm.nth(LocalDate.of(2024, 1, 1), 1)       // 2024-03-20, the first date after 1 January
 * }}}
 *
 * The distinction is one character wide in each implementation and a whole quarter wide in the
 * result, which is why every member spells it out rather than deriving one method from the
 * other.
 *
 * ===Base and full sequences===
 *
 * Several sequences come in pairs: a quarterly "base" sequence of March, June, September and
 * December, and a "full" sequence that interleaves the nearest serial months with it.
 * `baseSequence` names the base of such a pair, and returns the sequence itself for the four
 * members that are not one half of a pair. [[SequenceDate]] chooses between the two, which is
 * how a caller asks for "the second quarterly date" or "the second date of any kind" without
 * holding two sequences.
 *
 * ===Failure===
 *
 * Stepping through a sequence cannot fail on the data it is given: every date has a next
 * sequence date, so these methods are total in their signature. A sequence number that is
 * zero or negative is a different matter - it is not a value the sequence could interpret, it
 * is a broken precondition of the call - and it is reported through `ArgCheck` rather than as a
 * returned failure. The one place a sequence number arrives as data is a [[SequenceDate]] built
 * from user input, and there it is checked by the validating factory before any sequence sees
 * it.
 *
 * @param name  the unique name of the sequence, as it appears in text and in JSON
 */
sealed abstract class DateSequence private[date] (val name: String)
    extends Named
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to the package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be a seventh sequence, outside the
  // six this type publishes - is refused here instead.
  //
  // No invariant accompanies the check, and none is needed: every member of this family is a
  // `case object`, and the class of a `case object` takes no argument, so a class file naming one
  // of them directly has no field to supply and no state to disagree with its name. A family that
  // builds members from a hidden implementation class has to state one, because there the fields
  // are the caller's to choose; [[SequenceDate]], below, is that shape and states its own.
  JvmClosure.requireDeclaredMember(this, classOf[DateSequence])

  /**
   * Returns the name of this sequence, which is how a sequence renders as text.
   *
   * It is the text `parse` reads back, so the rendering of a sequence names that sequence.
   *
   * @return the unique name of this sequence
   */
  override def toString: String = name

  /**
   * Gets the base sequence of this sequence.
   *
   * Where a sequence interleaves serial dates with a quarterly sequence, this returns that
   * quarterly sequence; where it does not, it returns this sequence. A caller that wants only
   * the principal dates of a contract therefore asks for the base sequence without needing to
   * know which of the six sequences it holds.
   *
   * @return the base sequence, or this sequence when it has no separate base
   */
  def baseSequence: DateSequence = this

  /**
   * Finds the next date in the sequence after the input date.
   *
   * The result is always later than the input date, so a date that is itself in the sequence
   * is stepped over rather than returned.
   *
   * @param date  the input date
   * @return the next sequence date strictly after the input date
   */
  def next(date: LocalDate): LocalDate = nextOrSame(LocalDateUtils.plusDays(date, 1))

  /**
   * Finds the next date in the sequence, returning the input date if it is a date in the
   * sequence.
   *
   * @param date  the input date
   * @return the input date when it is a sequence date, otherwise the next sequence date
   */
  def nextOrSame(date: LocalDate): LocalDate

  /**
   * Finds the nth date in the sequence after the input date, always later than the input date.
   *
   * A sequence number of 1 selects the first sequence date after the input date, and a larger
   * sequence number counts on from there, one sequence date at a time.
   *
   * @param date  the input date
   * @param sequenceNumber  the 1-based index of the date to find, not zero or negative
   * @return the nth sequence date after the input date
   * @throws IllegalArgumentException if the sequence number is zero or negative, which is a
   *   broken precondition of the call rather than a property of the data
   */
  def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
    ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
    advance(next(date), sequenceNumber - 1)
  }

  /**
   * Finds the nth date in the sequence on or after the input date, returning the input date if
   * it is a date in the sequence.
   *
   * A sequence number of 1 selects the input date when it is a sequence date and the following
   * sequence date otherwise; a larger sequence number counts on from that first date.
   *
   * @param date  the input date
   * @param sequenceNumber  the 1-based index of the date to find, not zero or negative
   * @return the nth sequence date on or after the input date
   * @throws IllegalArgumentException if the sequence number is zero or negative, which is a
   *   broken precondition of the call rather than a property of the data
   */
  def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
    ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
    advance(nextOrSame(date), sequenceNumber - 1)
  }

  /**
   * Steps forward through the sequence a fixed number of times.
   *
   * The general form of `nth` and `nthOrSame` counts by stepping one sequence date at a time.
   * Recursing on the overridable public method would not be a tail call, so the walk is written
   * here, in a private method where it is one, and a large sequence number therefore costs no
   * stack. Each step still goes through `next`, so a member that overrides `next` is the member
   * that answers.
   *
   * @param date  the sequence date the walk has reached
   * @param remaining  the number of further steps to take, never negative
   * @return the sequence date reached after taking every remaining step
   */
  @tailrec
  private def advance(date: LocalDate, remaining: Int): LocalDate =
    if (remaining == 0) date else advance(next(date), remaining - 1)

  /**
   * Finds the date in the sequence that corresponds to the specified year-month.
   *
   * The date returned is ordinarily in the month asked for, which is what makes a contract
   * identified by its delivery month resolvable to a date. It is not guaranteed to be: a
   * quarterly sequence asked for a month of no quarter answers with the date of the next
   * quarter.
   *
   * @param yearMonth  the input year-month
   * @return the sequence date associated with that year-month
   */
  def dateMatching(yearMonth: YearMonth): LocalDate

  /**
   * Selects a date from the sequence, always later than the input date.
   *
   * The [[SequenceDate]] carries the instruction - which sequence number, counted from which
   * starting point, over the base sequence or the full one - and this applies it to this
   * sequence. Where the instruction names a year-month the count starts from the first day of
   * that month; otherwise it starts from the day after the input date.
   *
   * @param inputDate  the input date
   * @param sequenceDate  the instruction specifying which date to select
   * @return the selected sequence date
   */
  def selectDate(inputDate: LocalDate, sequenceDate: SequenceDate): LocalDate =
    sequenceDate.selectDate(inputDate, this, allowSame = false)

  /**
   * Selects a date from the sequence, returning the input date if it is a date in the sequence.
   *
   * This differs from `selectDate` only in that the count starts from the input date itself
   * rather than from the day after it, so an input date that is already a sequence date can be
   * the answer.
   *
   * @param inputDate  the input date
   * @param sequenceDate  the instruction specifying which date to select
   * @return the selected sequence date
   */
  def selectDateOrSame(inputDate: LocalDate, sequenceDate: SequenceDate): LocalDate =
    sequenceDate.selectDate(inputDate, this, allowSame = true)
}

/**
 * The six date sequences, and the lookup that resolves one from its name.
 *
 * Each sequence exists here exactly once, and the two tables a named family may declare - a
 * map of alternate spellings and a list of patterns that rewrite text before it is looked up -
 * are both empty for this family. The whole name space of the family is therefore its six
 * canonical names.
 *
 * The same six values are published as constants of [[DateSequences]], so a call site reaches
 * a sequence through either object and holds the same value.
 */
object DateSequence {

  /**
   * The third Wednesday of the month of the date being adjusted.
   *
   * Adjusting a date with this moves it to the third Wednesday of its own month, which is the
   * IMM date of that month. The adjuster is computed once and shared, being immutable and
   * stateless.
   */
  private val ThirdWednesday: TemporalAdjuster =
    TemporalAdjusters.dayOfWeekInMonth(3, DayOfWeek.WEDNESDAY)

  /**
   * Calculates the number of months to add to a base month to reach the nth quarterly month.
   *
   * The quarterly months are March, June, September and December. Counting from the base month,
   * the first step is the distance to the next quarterly month - zero when the base month is
   * already one of them - and each further step is a whole quarter.
   *
   * @param baseMonth  the month of the base date, from 1 to 12
   * @param sequenceNumber  the 1-based index of the quarterly date to reach
   * @return the number of months to add to the base date
   */
  private def monthsToAdd(baseMonth: Int, sequenceNumber: Int): Int = {
    val monthInQuarter = (baseMonth + 2) % 3 // Jan/Apr/Jul/Oct is 0, Feb/May/Aug/Nov is 1, Mar/Jun/Sep/Dec is 2
    val monthsUntilNextQuarter = 2 - monthInQuarter
    monthsUntilNextQuarter + (sequenceNumber - 1) * 3
  }

  /**
   * The 'Quarterly-IMM' sequence, the third Wednesday of March, June, September and December.
   *
   * This is the sequence that dates the principal listed interest rate futures, and it is the
   * base sequence of both serial sequences below.
   */
  case object QUARTERLY_IMM extends DateSequence("Quarterly-IMM") {

    override def next(date: LocalDate): LocalDate = nth(date, 1)

    override def nextOrSame(date: LocalDate): LocalDate = nthOrSame(date, 1)

    override def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val immOfMonth = date.`with`(ThirdWednesday)
      // the IMM date of the input month is only a candidate when it is strictly later
      val base = if (!immOfMonth.isAfter(date)) immOfMonth.plusMonths(1L) else immOfMonth
      shift(base, sequenceNumber)
    }

    override def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val immOfMonth = date.`with`(ThirdWednesday)
      val base = if (immOfMonth.isBefore(date)) immOfMonth.plusMonths(1L) else immOfMonth
      shift(base, sequenceNumber)
    }

    private def shift(base: LocalDate, sequenceNumber: Int): LocalDate =
      base.plusMonths(monthsToAdd(base.getMonthValue, sequenceNumber).toLong).`with`(ThirdWednesday)

    override def dateMatching(yearMonth: YearMonth): LocalDate = nextOrSame(yearMonth.atDay(1))
  }

  /**
   * The 'Quarterly-IMM-6-Serial' sequence, the quarterly IMM dates with the nearest serial
   * months interleaved so that the first six dates are consecutive months.
   *
   * The first six dates are the IMM dates of six consecutive months, which subsumes the first
   * two quarterly dates; from the seventh onwards the sequence is quarterly again, which is why
   * the quarterly count resumes at the sequence number less four.
   */
  case object QUARTERLY_IMM_6_SERIAL extends DateSequence("Quarterly-IMM-6-Serial") {

    override def baseSequence: DateSequence = QUARTERLY_IMM

    override def next(date: LocalDate): LocalDate = nth(date, 1)

    override def nextOrSame(date: LocalDate): LocalDate = nthOrSame(date, 1)

    override def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val immOfMonth = date.`with`(ThirdWednesday)
      val baseImm = if (!immOfMonth.isAfter(date)) immOfMonth.plusMonths(1L) else immOfMonth
      shift(baseImm, sequenceNumber)
    }

    override def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val immOfMonth = date.`with`(ThirdWednesday)
      val baseImm = if (immOfMonth.isBefore(date)) immOfMonth.plusMonths(1L) else immOfMonth
      shift(baseImm, sequenceNumber)
    }

    private def shift(base: LocalDate, sequenceNumber: Int): LocalDate =
      // first 4 serial can be expanded to first 6 serial by subsuming first two quarterly
      if (sequenceNumber <= 6) {
        base.plusMonths((sequenceNumber - 1).toLong).`with`(ThirdWednesday)
      } else {
        val effectiveQuarterlySequenceNumber = sequenceNumber - 4
        base
          .plusMonths(monthsToAdd(base.getMonthValue, effectiveQuarterlySequenceNumber).toLong)
          .`with`(ThirdWednesday)
      }

    override def dateMatching(yearMonth: YearMonth): LocalDate = nextOrSame(yearMonth.atDay(1))
  }

  /**
   * The 'Quarterly-IMM-3-Serial' sequence, the quarterly IMM dates with the nearest serial
   * months interleaved so that the first three dates are consecutive months.
   *
   * The first three dates are the IMM dates of three consecutive months, which subsumes the
   * first quarterly date; from the fourth onwards the sequence is quarterly again, which is why
   * the quarterly count resumes at the sequence number less two.
   */
  case object QUARTERLY_IMM_3_SERIAL extends DateSequence("Quarterly-IMM-3-Serial") {

    override def baseSequence: DateSequence = QUARTERLY_IMM

    override def next(date: LocalDate): LocalDate = nth(date, 1)

    override def nextOrSame(date: LocalDate): LocalDate = nthOrSame(date, 1)

    override def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val immOfMonth = date.`with`(ThirdWednesday)
      val baseImm = if (!immOfMonth.isAfter(date)) immOfMonth.plusMonths(1L) else immOfMonth
      shift(baseImm, sequenceNumber)
    }

    override def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val immOfMonth = date.`with`(ThirdWednesday)
      val baseImm = if (immOfMonth.isBefore(date)) immOfMonth.plusMonths(1L) else immOfMonth
      shift(baseImm, sequenceNumber)
    }

    private def shift(base: LocalDate, sequenceNumber: Int): LocalDate =
      // first 2 serial can be expanded to first 3 serial by subsuming first quarterly
      if (sequenceNumber <= 3) {
        base.plusMonths((sequenceNumber - 1).toLong).`with`(ThirdWednesday)
      } else {
        val effectiveQuarterlySequenceNumber = sequenceNumber - 2
        base
          .plusMonths(monthsToAdd(base.getMonthValue, effectiveQuarterlySequenceNumber).toLong)
          .`with`(ThirdWednesday)
      }

    override def dateMatching(yearMonth: YearMonth): LocalDate = nextOrSame(yearMonth.atDay(1))
  }

  /**
   * The 'Monthly-IMM' sequence, the third Wednesday of every month.
   *
   * Unlike the sequences above, this one associates a year-month with the IMM date of that very
   * month rather than with the next sequence date on or after its first day; the two agree for
   * every month, because every month has a third Wednesday.
   */
  case object MONTHLY_IMM extends DateSequence("Monthly-IMM") {

    override def next(date: LocalDate): LocalDate = nth(date, 1)

    override def nextOrSame(date: LocalDate): LocalDate = nthOrSame(date, 1)

    override def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val base = date.`with`(ThirdWednesday)
      if (!base.isAfter(date)) {
        base.plusMonths(sequenceNumber.toLong).`with`(ThirdWednesday)
      } else {
        base.plusMonths((sequenceNumber - 1).toLong).`with`(ThirdWednesday)
      }
    }

    override def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val base = date.`with`(ThirdWednesday)
      if (base.isBefore(date)) {
        base.plusMonths(sequenceNumber.toLong).`with`(ThirdWednesday)
      } else {
        base.plusMonths((sequenceNumber - 1).toLong).`with`(ThirdWednesday)
      }
    }

    override def dateMatching(yearMonth: YearMonth): LocalDate =
      yearMonth.atDay(1).`with`(ThirdWednesday)
  }

  /**
   * The 'Quarterly-10th' sequence, the tenth day of March, June, September and December.
   */
  case object QUARTERLY_10TH extends DateSequence("Quarterly-10th") {

    override def next(date: LocalDate): LocalDate = nth(date, 1)

    override def nextOrSame(date: LocalDate): LocalDate = nthOrSame(date, 1)

    override def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val tenthOfMonth = date.withDayOfMonth(10)
      val base = if (!tenthOfMonth.isAfter(date)) tenthOfMonth.plusMonths(1L) else tenthOfMonth
      shift(base, sequenceNumber)
    }

    override def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val tenthOfMonth = date.withDayOfMonth(10)
      val base = if (tenthOfMonth.isBefore(date)) tenthOfMonth.plusMonths(1L) else tenthOfMonth
      shift(base, sequenceNumber)
    }

    private def shift(base: LocalDate, sequenceNumber: Int): LocalDate =
      base.plusMonths(monthsToAdd(base.getMonthValue, sequenceNumber).toLong).withDayOfMonth(10)

    override def dateMatching(yearMonth: YearMonth): LocalDate = nextOrSame(yearMonth.atDay(1))
  }

  /**
   * The 'Monthly-1st' sequence, the first day of every month.
   */
  case object MONTHLY_1ST extends DateSequence("Monthly-1st") {

    override def next(date: LocalDate): LocalDate = nth(date, 1)

    override def nextOrSame(date: LocalDate): LocalDate = nthOrSame(date, 1)

    override def nth(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val firstOfMonth = date.withDayOfMonth(1)
      val base = if (!firstOfMonth.isAfter(date)) firstOfMonth.plusMonths(1L) else firstOfMonth
      base.plusMonths((sequenceNumber - 1).toLong)
    }

    override def nthOrSame(date: LocalDate, sequenceNumber: Int): LocalDate = {
      ArgCheck.notNegativeOrZero(sequenceNumber, "sequenceNumber")
      val firstOfMonth = date.withDayOfMonth(1)
      val base = if (firstOfMonth.isBefore(date)) firstOfMonth.plusMonths(1L) else firstOfMonth
      base.plusMonths((sequenceNumber - 1).toLong)
    }

    override def dateMatching(yearMonth: YearMonth): LocalDate = nextOrSame(yearMonth.atDay(1))
  }

  /**
   * The complete set of date sequences, in declaration order.
   *
   * The order is the order the members are declared in above, and it is the order a report over
   * the family follows. It is not the order the `Order` instance below imposes, which is
   * alphabetical by name. The list is non-empty by construction, which is what lets every
   * operation over the family - a name table, a generator, an exhaustive report - be written
   * without a case for a family that has no members.
   *
   * @return the six sequences, in declaration order
   */
  val values: NonEmptyList[DateSequence] =
    NonEmptyList.of(
      QUARTERLY_IMM,
      QUARTERLY_IMM_6_SERIAL,
      QUARTERLY_IMM_3_SERIAL,
      MONTHLY_IMM,
      QUARTERLY_10TH,
      MONTHLY_1ST
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a sequence, and it is built from `values`
   * alone. All three tables a named family may declare are empty here: this family has no
   * alternate spelling of any sequence, no pattern that rewrites text before it is looked up,
   * and no group of names published for an external protocol. The name space of the family is
   * therefore exactly its six canonical names.
   *
   * @return the name lookup for the six sequences
   */
  implicit val namedEnum: NamedEnum[DateSequence] =
    NamedEnum.of(values, Map.empty, Nil, Map.empty, "DateSequence")

  /**
   * Obtains the sequence with the specified canonical name, if one exists.
   *
   * The match is exact against the canonical names as they are declared, so `Quarterly-IMM`
   * resolves while `QUARTERLY-IMM` does not. Use `parse` to accept text whose case is not known
   * in advance.
   *
   * @param name  the name to look up
   * @return the sequence with that name, or `None` when no sequence has it
   */
  def valueOf(name: String): Option[DateSequence] = namedEnum.valueOf(name)

  /**
   * Parses a sequence from text, tolerating the case of the input.
   *
   * The lookup first tries the exact match of `valueOf`. When that finds nothing, the input is
   * folded to upper case and looked up once more, which is the whole of the leniency available
   * to this family, since it declares no rewrite pattern for the step between the two lookups:
   *
   * {{{
   * parse("Quarterly-IMM")  // Right(QUARTERLY_IMM) - the canonical name
   * parse("QUARTERLY-IMM")  // Right(QUARTERLY_IMM) - folded to upper case
   * parse("Quarterly IMM")  // Left - a name this family has never had
   * }}}
   *
   * Text that neither lookup resolves is reported as a value rather than raised: the result is
   * `Left` of a chain holding one [[Failure]] that names this family and the text it could not
   * resolve.
   *
   * @param name  the text to parse
   * @return the sequence the text names, or the failure naming the broken condition: the text
   *   must be one of the six canonical names, in that spelling or folded to upper case
   */
  def parse(name: String): EitherNec[Failure, DateSequence] = namedEnum.parse(name)

  /**
   * The ordering and hashing of sequences.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. Comparison is over `name`, which makes the ordering alphabetical rather than the
   * declaration order of `values`, and equality follows it - names are unique across the
   * family, so two sequences compare equal if, and only if, they are the same sequence.
   *
   * @return the ordering of sequences by name, which is also their hashing
   */
  implicit val order: Order[DateSequence] with Hash[DateSequence] = NamedEnum.orderByName

  /**
   * The rendering of sequences as text.
   *
   * A sequence renders as its canonical name, which is what `toString` produces as well, so the
   * two ways of putting a sequence into a message agree.
   *
   * @return the rendering of a sequence as its canonical name
   */
  implicit val show: Show[DateSequence] = NamedEnum.showByName

  /**
   * The JSON codec for sequences.
   *
   * A sequence is written as the bare string of its canonical name - `"Quarterly-IMM"` - and
   * never as an object. Decoding goes through `parse`, so a document is read with exactly the
   * leniency text is, and a string naming no sequence is reported as a decoding failure rather
   * than raised.
   *
   * @return the codec reading and writing a sequence as its canonical name
   */
  implicit val codec: Codec[DateSequence] = Codecs.namedEnumCodec
}

/**
 * The standard date sequences, as constants.
 *
 * Every constant here is one of the members of [[DateSequence]] and is that same value, so a
 * constant taken from here and the matching member of the companion are indistinguishable.
 */
object DateSequences {

  /**
   * The 'Quarterly-IMM' sequence, the third Wednesday of March, June, September and December.
   */
  val QUARTERLY_IMM: DateSequence = DateSequence.QUARTERLY_IMM

  /**
   * The 'Quarterly-IMM-6-Serial' sequence, quarterly IMM dates whose first six dates are the
   * IMM dates of six consecutive months.
   */
  val QUARTERLY_IMM_6_SERIAL: DateSequence = DateSequence.QUARTERLY_IMM_6_SERIAL

  /**
   * The 'Quarterly-IMM-3-Serial' sequence, quarterly IMM dates whose first three dates are the
   * IMM dates of three consecutive months.
   */
  val QUARTERLY_IMM_3_SERIAL: DateSequence = DateSequence.QUARTERLY_IMM_3_SERIAL

  /** The 'Monthly-IMM' sequence, the third Wednesday of every month. */
  val MONTHLY_IMM: DateSequence = DateSequence.MONTHLY_IMM

  /** The 'Quarterly-10th' sequence, the tenth day of March, June, September and December. */
  val QUARTERLY_10TH: DateSequence = DateSequence.QUARTERLY_10TH

  /** The 'Monthly-1st' sequence, the first day of every month. */
  val MONTHLY_1ST: DateSequence = DateSequence.MONTHLY_1ST
}

/**
 * An instruction that selects one date from a [[DateSequence]].
 *
 * A contract is rarely identified by the date it settles on. It is identified as "the second
 * quarterly future", "the front month", or "the first future at least three months out", and
 * the date follows from that description once a sequence and an evaluation date are known.
 * This type is that description, held apart from the sequence it will be applied to, so that
 * one instruction can be carried through a convention and resolved against whichever sequence
 * the instrument turns out to use.
 *
 * ===What an instruction says===
 *
 * Four pieces of information, of which the first two are alternatives:
 *
 *   - `yearMonth` - count from the first day of this month, ignoring the input date entirely.
 *     This is how a contract named by its delivery month is resolved.
 *   - `minimumPeriod` - count from the input date once this period has been added to it. This
 *     is how "at least three months out" is expressed: the period moves the starting point
 *     forward, and the sequence number then counts from there.
 *   - `sequenceNumber` - the 1-based index of the date to take, 1 being the first.
 *   - `fullSequence` - whether to count over the full sequence or over its base sequence. A
 *     future whose full sequence interleaves serial months with quarterly ones is counted over
 *     the quarterly dates alone when this is `false`.
 *
 * A year-month and a minimum period are mutually exclusive, because each names a different
 * starting point and there is no meaning in supplying both.
 *
 * ===Construction===
 *
 * There is no public constructor and no `copy`. Every instruction comes from one of the
 * factories, which are named after the sequence they count over - `base` for the base sequence
 * and `full` for the full one - and each answers with `EitherNec[Failure, SequenceDate]`
 * because all three of its rejections are properties of the data supplied:
 *
 * {{{
 * SequenceDate.base(2)                              // Right: the second base-sequence date
 * SequenceDate.base(YearMonth.of(2020, 3))          // Right: the first base date of March 2020
 * SequenceDate.full(Period.ofMonths(3), 1)          // Right: the first full date 3 months out
 * SequenceDate.base(Period.ofMonths(-1), 1)         // Left: minimum period cannot be negative
 * SequenceDate.base(0)                              // Left: sequence number must be positive
 * }}}
 *
 * The factories accumulate: an instruction wrong in two ways reports both failures rather than
 * only the first one found. The three conditions are named on [[SequenceDate.of]], the general
 * factory every one of the eight goes through.
 *
 * ===Normalisation===
 *
 * A minimum period of zero is no minimum period at all, and is normalised away rather than
 * rejected. Two instructions written as `base(Period.ZERO, 1)` and `base(1)` are therefore the
 * same value, they encode to the same JSON, and rebuilding either one from its own fields
 * yields itself.
 *
 * @param yearMonth  the month to count from, used instead of the input date when present
 * @param minimumPeriod  the period added to the input date before counting begins, never zero
 *   and never negative
 * @param sequenceNumber  the 1-based index of the date to select, always positive
 * @param fullSequence  whether to count over the full sequence rather than the base sequence
 */
sealed abstract case class SequenceDate private (
    yearMonth: Option[YearMonth],
    minimumPeriod: Option[Period],
    sequenceNumber: Int,
    fullSequence: Boolean)
    extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry a starting point, a minimum period and a sequence number no factory had checked
  // or normalised - can be stopped is here. The single implementation is the companion's hidden
  // `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[SequenceDate.Impl])

  // The invariant of this type, stated over the four fields the instance actually holds rather
  // than over the arguments a factory was given, because the class file of the implementation
  // carries a public constructor whatever the source asked for: a class compiled outside this
  // library can reach it directly, and the check above would admit what it built, its runtime
  // class being the one class that check admits. What is left to state is the three conditions
  // [[SequenceDate.of]] checks and the normalisation it then performs, so an instruction that
  // exists by any route is one that describes a date: two starting points would leave
  // [[selectDate]] choosing between them, a minimum period running backwards would move the
  // starting point behind the input date, and a sequence number of zero or less would count to no
  // date at all, 1 being the first.
  JvmClosure.requireInvariant(
    "it names a starting month or a minimum period, and not both",
    !(yearMonth.isDefined && minimumPeriod.isDefined))
  JvmClosure.requireInvariant(
    "its minimum period does not run backwards",
    !minimumPeriod.exists(period => period.isNegative))
  JvmClosure.requireInvariant(
    "its sequence number is positive, 1 being the first date of the sequence",
    sequenceNumber > 0)
  JvmClosure.requireInvariant(
    "a minimum period of zero is held as no minimum period at all",
    !minimumPeriod.contains(Period.ZERO))

  /**
   * Applies this instruction to a sequence, producing the date it describes.
   *
   * This is the other half of [[DateSequence.selectDate]] and [[DateSequence.selectDateOrSame]]:
   * the sequence knows how to step through its dates and this instruction knows where to start
   * and how far to count, so the two are written against one another and neither is useful
   * alone. It is visible only within this package for that reason - a caller selects a date
   * through the sequence, not through the instruction.
   *
   * The sequence actually counted over is this sequence when `fullSequence` is set and its base
   * sequence otherwise. The starting point is the first day of `yearMonth` when a month is
   * given, in which case the input date is not consulted and the count always admits that first
   * day itself; otherwise it is the input date moved on by `minimumPeriod`, if any, and whether
   * the starting point itself may be the answer is then decided by `allowSame`.
   *
   * @param inputDate  the date the count starts from, unless a year-month was given
   * @param sequence  the sequence to select from
   * @param allowSame  whether the starting point may itself be the selected date
   * @return the selected date
   */
  private[date] def selectDate(
      inputDate: LocalDate,
      sequence: DateSequence,
      allowSame: Boolean): LocalDate = {

    val selectedSequence = if (fullSequence) sequence else sequence.baseSequence
    yearMonth match {
      case Some(month) =>
        selectedSequence.nthOrSame(month.atDay(1), sequenceNumber)
      case None =>
        val startDate = minimumPeriod.fold(inputDate)(period => inputDate.plus(period))
        if (allowSame) {
          selectedSequence.nthOrSame(startDate, sequenceNumber)
        } else {
          selectedSequence.nth(startDate, sequenceNumber)
        }
    }
  }

  /**
   * Returns a string describing the instruction.
   *
   * All four fields are named, in the declaration order of the type, and a field holding
   * nothing is rendered with the companion's `AbsentFieldMarker` rather than left out:
   *
   * {{{
   * SequenceDate{yearMonth=[absent], minimumPeriod=P2M, sequenceNumber=3, fullSequence=true}
   * SequenceDate{yearMonth=2020-02, minimumPeriod=[absent], sequenceNumber=2, fullSequence=false}
   * }}}
   *
   * `[absent]` stands in for the text of `AbsentFieldMarker`, which these examples name rather
   * than print: that text is the host platform's rendering of an absent reference, which the
   * domain code of this library writes nowhere, and the constant carries the reason why.
   *
   * The field set is fixed rather than derived from which fields are present, because a reader
   * of a log line or a snapshot compares renderings of different instructions against one
   * another, and a layout that changes with the data cannot be read that way. It is also what
   * the `Show` instance renders, which is obtained from this method so the two cannot diverge.
   *
   * Note that this is not the shape of the JSON this type writes, where an absent field is
   * dropped from the document entirely: the two serve different readers.
   *
   * @return the descriptive string, naming all four fields
   */
  override def toString: String = {
    val renderedYearMonth = yearMonth.fold(SequenceDate.AbsentFieldMarker)(_.toString)
    val renderedMinimumPeriod = minimumPeriod.fold(SequenceDate.AbsentFieldMarker)(_.toString)
    s"SequenceDate{yearMonth=$renderedYearMonth, minimumPeriod=$renderedMinimumPeriod, " +
      s"sequenceNumber=$sequenceNumber, fullSequence=$fullSequence}"
  }
}

/**
 * The factories, instances and codec of [[SequenceDate]].
 *
 * The eight factories are the only way to build an instruction. They divide into two groups of
 * four that differ solely in the sequence they count over - `base` over the base sequence and
 * `full` over the full one - and within each group by where the count starts: a year-month, a
 * year-month with a sequence number, a sequence number alone, or a minimum period with a
 * sequence number. The general `of` underlies all eight and is also what the JSON decoder calls,
 * so text, code and serialized documents are all validated by one implementation.
 */
object SequenceDate {

  /** The rejection of an instruction that names both a starting month and a minimum period. */
  private val BothStartingPointsMessage = "Minimum period cannot be set when year-month is present"

  /** The rejection of an instruction whose minimum period runs backwards. */
  private val NegativeMinimumPeriodMessage = "Minimum period cannot be negative"

  /**
   * The text an absent optional field is rendered with by [[SequenceDate.toString]].
   *
   * It is obtained from the way the host platform renders an absent reference - converting an
   * empty `Option` to a reference and asking the platform for its text - rather than written
   * out as a string literal, which keeps the one place that needs the marker free of the token
   * itself.
   */
  private val AbsentFieldMarker: String = String.valueOf(Option.empty[AnyRef].orNull)

  /**
   * The raw field shape the JSON codec is derived from.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps run backwards - an instruction is taken apart into these fields and the
   * derived encoder writes them. It exists only for those purposes: it is private, it is never
   * returned, and nothing but the codec below builds one.
   *
   * The field names and their declaration order are therefore the wire shape, and holding them
   * in one product is what keeps the two directions of the codec describing the same document.
   *
   * @param yearMonth  the month to count from, if the document carried one
   * @param minimumPeriod  the minimum period, if the document carried one
   * @param sequenceNumber  the 1-based sequence number, unvalidated
   * @param fullSequence  whether to count over the full sequence
   */
  private final case class Raw(
      yearMonth: Option[YearMonth],
      minimumPeriod: Option[Period],
      sequenceNumber: Int,
      fullSequence: Boolean)
      extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * Obtains an instruction selecting the next base sequence date on or after the start of the
   * specified month.
   *
   * @param yearMonth  the month to count from
   * @return the instruction, which this factory always produces: it names no minimum period
   *   alongside the month and supplies the sequence number itself, so none of the conditions
   *   [[of]] checks can be broken
   */
  def base(yearMonth: YearMonth): EitherNec[Failure, SequenceDate] =
    of(Some(yearMonth), None, 1, fullSequence = false)

  /**
   * Obtains an instruction selecting the nth base sequence date on or after the start of the
   * specified month.
   *
   * @param yearMonth  the month to count from
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @return the instruction, or the failure naming the broken condition: the sequence number
   *   must be positive
   */
  def base(yearMonth: YearMonth, sequenceNumber: Int): EitherNec[Failure, SequenceDate] =
    of(Some(yearMonth), None, sequenceNumber, fullSequence = false)

  /**
   * Obtains an instruction selecting the nth base sequence date after the input date.
   *
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @return the instruction, or the failure naming the broken condition: the sequence number
   *   must be positive
   */
  def base(sequenceNumber: Int): EitherNec[Failure, SequenceDate] =
    of(None, None, sequenceNumber, fullSequence = false)

  /**
   * Obtains an instruction selecting the nth base sequence date after the input date once the
   * minimum period has been added to it.
   *
   * @param minimumPeriod  the minimum period between the input date and the first sequence date,
   *   not negative
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @return the instruction, or the failures naming every broken condition: the minimum period
   *   must have no negative years, months or days, and the sequence number must be positive
   */
  def base(minimumPeriod: Period, sequenceNumber: Int): EitherNec[Failure, SequenceDate] =
    of(None, Some(minimumPeriod), sequenceNumber, fullSequence = false)

  /**
   * Obtains an instruction selecting the next full sequence date on or after the start of the
   * specified month.
   *
   * @param yearMonth  the month to count from
   * @return the instruction, which this factory always produces: it names no minimum period
   *   alongside the month and supplies the sequence number itself, so none of the conditions
   *   [[of]] checks can be broken
   */
  def full(yearMonth: YearMonth): EitherNec[Failure, SequenceDate] =
    of(Some(yearMonth), None, 1, fullSequence = true)

  /**
   * Obtains an instruction selecting the nth full sequence date on or after the start of the
   * specified month.
   *
   * @param yearMonth  the month to count from
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @return the instruction, or the failure naming the broken condition: the sequence number
   *   must be positive
   */
  def full(yearMonth: YearMonth, sequenceNumber: Int): EitherNec[Failure, SequenceDate] =
    of(Some(yearMonth), None, sequenceNumber, fullSequence = true)

  /**
   * Obtains an instruction selecting the nth full sequence date after the input date.
   *
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @return the instruction, or the failure naming the broken condition: the sequence number
   *   must be positive
   */
  def full(sequenceNumber: Int): EitherNec[Failure, SequenceDate] =
    of(None, None, sequenceNumber, fullSequence = true)

  /**
   * Obtains an instruction selecting the nth full sequence date after the input date once the
   * minimum period has been added to it.
   *
   * @param minimumPeriod  the minimum period between the input date and the first sequence date,
   *   not negative
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @return the instruction, or the failures naming every broken condition: the minimum period
   *   must have no negative years, months or days, and the sequence number must be positive
   */
  def full(minimumPeriod: Period, sequenceNumber: Int): EitherNec[Failure, SequenceDate] =
    of(None, Some(minimumPeriod), sequenceNumber, fullSequence = true)

  /**
   * Obtains an instruction from every field, validating the combination.
   *
   * This is the general factory the eight named ones delegate to, and the one the JSON decoder
   * calls. Three conditions are checked, and all three are checked together rather than in
   * sequence, so an instruction wrong in more than one way reports every reason at once:
   *
   *   - a starting month and a minimum period may not both be given, since each names a
   *     different starting point;
   *   - a minimum period may not run backwards, in any of its years, months or days;
   *   - a sequence number must be positive, 1 being the first date of the sequence.
   *
   * A minimum period of zero passes the checks and is then normalised away, which is what makes
   * this factory idempotent over its own output: rebuilding an instruction from the fields of an
   * existing one yields that same instruction.
   *
   * @param yearMonth  the month to count from, if the count starts from a month
   * @param minimumPeriod  the minimum period added to the input date, if there is one
   * @param sequenceNumber  the 1-based sequence number, not zero or negative
   * @param fullSequence  whether to count over the full sequence rather than the base sequence
   * @return the instruction, or the failures naming every broken condition: a starting month and
   *   a minimum period may not both be given, the minimum period must have no negative years,
   *   months or days, and the sequence number must be positive
   */
  def of(
      yearMonth: Option[YearMonth],
      minimumPeriod: Option[Period],
      sequenceNumber: Int,
      fullSequence: Boolean): EitherNec[Failure, SequenceDate] = {

    val checkedStartingPoint: ValidatedFailures[Unit] =
      Validate.isFalse(yearMonth.isDefined && minimumPeriod.isDefined, BothStartingPointsMessage)
    val checkedMinimumPeriod: ValidatedFailures[Option[Period]] =
      Validate.cond(
        !minimumPeriod.exists(_.isNegative),
        minimumPeriod,
        Failure.Invalid(NegativeMinimumPeriodMessage))
    val checkedSequenceNumber: ValidatedFailures[Int] =
      Validate.notNegativeOrZero(sequenceNumber, "sequenceNumber")

    Validate.toResult(
      (checkedStartingPoint, checkedMinimumPeriod, checkedSequenceNumber).mapN {
        (_, checkedPeriod, checkedNumber) =>
          new Impl(
            yearMonth,
            checkedPeriod.filterNot(_ == Period.ZERO),
            checkedNumber,
            fullSequence)
      })
  }

  /**
   * The one implementation of an instruction.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[SequenceDate]] refuse in its own constructor to be any other implementation.
   *
   * @param yearMonth  the month to count from, if the count starts from a month
   * @param minimumPeriod  the minimum period, already normalised so that it is never zero
   * @param sequenceNumber  the 1-based sequence number, already checked to be positive
   * @param fullSequence  whether to count over the full sequence rather than the base sequence
   */
  private final class Impl(
      yearMonth: Option[YearMonth],
      minimumPeriod: Option[Period],
      sequenceNumber: Int,
      fullSequence: Boolean)
      extends SequenceDate(yearMonth, minimumPeriod, sequenceNumber, fullSequence)

  /**
   * The hashing and equality of instructions.
   *
   * This is the only equality-bearing instance of the type. Two instructions are equal when
   * every field is equal, which is the equality of the data they hold and nothing else; there
   * is no ordering, because no order over instructions has a meaning of its own.
   *
   * @return the hashing of instructions, which is also their equality
   */
  implicit val hash: Hash[SequenceDate] = Hash.fromUniversalHashCode

  /**
   * The rendering of instructions as text.
   *
   * Renders what [[SequenceDate.toString]] renders, field for field: all four fields, always,
   * in declaration order, an absent optional field carrying the `AbsentFieldMarker` text, which
   * the examples below stand in for as `[absent]`:
   *
   * {{{
   * SequenceDate{yearMonth=[absent], minimumPeriod=P3M, sequenceNumber=2, fullSequence=true}
   * SequenceDate{yearMonth=2020-03, minimumPeriod=[absent], sequenceNumber=1, fullSequence=false}
   * }}}
   *
   * Taking the rendering from `toString` rather than writing it a second time is what keeps the
   * two ways of putting an instruction into a message in agreement, since neither can be changed
   * without the other following.
   *
   * @return the rendering of an instruction, naming all four of its fields
   */
  implicit val show: Show[SequenceDate] = Show.show(_.toString)

  /**
   * The JSON encoder for instructions.
   *
   * An instruction is written as an object of its four fields, the two optional ones being
   * omitted rather than written as explicitly empty, with the month as `2020-03` and the period
   * as `P3M` - the ISO forms:
   *
   * {{{
   * {"minimumPeriod":"P2M","sequenceNumber":3,"fullSequence":true}
   * {"yearMonth":"2020-02","sequenceNumber":2,"fullSequence":false}
   * }}}
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. It is derived for the raw field shape rather than for the type itself,
   * because the type is a normalising one whose constructor is not public: an instruction is
   * taken apart into those fields and the derived encoder writes them, which is the exact
   * counterpart of the decoder below reading them and handing them to the factory. Deriving the
   * field set instead of listing it is what keeps the written document and the product in step -
   * a field added to the product appears in the document without anything here being changed.
   *
   * The derived encoder is then wrapped so that a field holding nothing is dropped from the
   * output rather than written as explicitly empty, as every product encoding of this library
   * is.
   *
   * @return the encoder writing an instruction as an object of the fields it carries
   */
  implicit val encoder: Encoder[SequenceDate] =
    Codecs.dropNulls(rawEncoder.contramap[SequenceDate] { value =>
      Raw(value.yearMonth, value.minimumPeriod, value.sequenceNumber, value.fullSequence)
    })

  /**
   * The JSON decoder for instructions.
   *
   * The fields are read into a raw product and handed to `of`, so a document is validated by
   * exactly the checks a caller's inputs are: a document naming both a starting month and a
   * minimum period, or a negative period, or a sequence number of zero, is rejected as a
   * decoding failure carrying every reason it was rejected for, rather than decoded into a value
   * the factory would never have built. An absent optional field is read as holding nothing,
   * which is what makes the round trip with the encoder above exact.
   *
   * @return the decoder reading and validating an instruction
   */
  implicit val decoder: Decoder[SequenceDate] =
    Codecs.validatedDecoder[Raw, SequenceDate] { raw =>
      of(raw.yearMonth, raw.minimumPeriod, raw.sequenceNumber, raw.fullSequence)
    }(rawDecoder)
}
