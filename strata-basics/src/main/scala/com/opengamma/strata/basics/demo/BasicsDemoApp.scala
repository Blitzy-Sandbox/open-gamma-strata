/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.demo

import java.time.LocalDate

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._

import io.circe.syntax.EncoderOps

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxMatrix
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.PeriodicSchedule
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.ResultNec

/**
 * A runnable program that generates a periodic date schedule adjusted against a holiday
 * calendar, converts a two-currency exposure into a single currency through an FX rate matrix,
 * and prints both as JSON. It is started with `sbt "strata-basics/run"`.
 *
 * Four steps, in order, each printed under its own heading:
 *
 *  1. '''define''' a quarterly [[PeriodicSchedule]] whose dates are adjusted against the London
 *     holiday calendar, using the named convention constants rather than any parsed text;
 *  1. '''generate''' the [[Schedule]] from that definition by resolving it against
 *     [[ReferenceData.standard]], which is passed explicitly - there is no ambient lookup of
 *     reference data anywhere in this library. That store answers per identifier, generating a
 *     built-in calendar on the first lookup that names it, so this run costs the one calendar it
 *     asks for - London - and not the thirty the store can answer for;
 *  1. '''convert''' a two-currency [[MultiCurrencyAmount]] into US dollars through an
 *     [[FxMatrix]] built from two quoted rates;
 *  1. '''serialize''' the schedule, the exposure as it stood before conversion and the converted
 *     amount to JSON with the circe codecs the types carry, and print all three.
 *
 * ===Effects at the edge===
 *
 * This object is the '''only''' place in the main sources of `strata-basics` that uses
 * `cats.effect`. Every calculation in this library is a pure function returning `Either` (or
 * `EitherNec`) over [[Failure]], and effects exist only where a program meets the outside world,
 * which for this module is here. The two lifts from the error channel into `IO` are therefore
 * declared privately below rather than shared: no exception type is added to `strata-collect` or
 * to any `strata-basics` package for the sake of a demonstration, and no library type gains an
 * `IO`-returning member.
 *
 * ===Determinism===
 *
 * Every input is a literal or a named constant: no clock, no random source, no environment
 * variable, no system property and no external file is read. Two runs therefore print
 * byte-identical output, which is what makes the output reviewable and diffable.
 *
 * The dates are also chosen so that the calendar visibly does work. The schedule rolls on the
 * 25th of the month, so one of its unadjusted boundaries falls on 25 December 2024 - Christmas
 * Day, a London holiday immediately followed by Boxing Day. The 'ModifiedFollowing' convention
 * therefore moves that boundary to the 27th, and the two schedule periods that share it print an
 * adjusted date differing from their unadjusted one. That difference is the visible proof that
 * the reference data was threaded through and the calendar resolved from it.
 */
object BasicsDemoApp extends IOApp.Simple {

  // Step 1 inputs. Literals, so that the run is reproducible, and named so that the output can
  // state what was asked for beside what came back.

  /** The start of the first schedule period, and the first unadjusted date. */
  private val StartDate: LocalDate = LocalDate.of(2024, 3, 25)

  /** The end of the last schedule period, twelve months after the start, so P3M divides evenly. */
  private val EndDate: LocalDate = LocalDate.of(2025, 3, 25)

  /**
   * The business day adjustment applied to every generated date.
   *
   * 'ModifiedFollowing' moves a date that is not a business day forward to the next one, unless
   * that would cross into the next month, in which case it moves backward instead. The calendar
   * is London, which is where the Christmas and Boxing Day holidays below come from.
   */
  private val Adjustment: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)

  /**
   * Whether the end-of-month is preferred when the roll day is ambiguous.
   *
   * False here: the schedule rolls on the 25th, which is unambiguous in every month, so asking
   * for end-of-month rolling would only obscure what the generated dates demonstrate.
   */
  private val PreferEndOfMonth: Boolean = false

  // Step 3 inputs. Rates are quoted in the market convention of the pair: a GBP/USD rate of
  // 1.27 means one pound buys 1.27 dollars, which is the same orientation the matrix stores.

  /** The GBP/USD rate: one pound in dollars. */
  private val GbpUsdRate: Double = 1.27d

  /** The EUR/USD rate: one euro in dollars. */
  private val EurUsdRate: Double = 1.09d

  /** The sterling leg of the exposure being converted. */
  private val GbpExposure: Double = 1000000d

  /** The euro leg of the exposure being converted. */
  private val EurExposure: Double = 500000d

  /** The width every label is padded to, so that the printed values line up in one column. */
  private val LabelWidth: Int = 26

  /** The width of the period-number column of the schedule table. */
  private val NumberWidth: Int = 8

  /** The width of a date-range column of the schedule table: two ISO dates and an arrow, spaced. */
  private val DateRangeWidth: Int = 26

  /** The heading of the schedule table, laid out on the same columns as its rows. */
  private val PeriodHeader: String =
    "  " + "period".padTo(NumberWidth, ' ') + "unadjusted".padTo(DateRangeWidth, ' ') + "adjusted"

  /** The marker drawn against a schedule period whose dates the calendar moved. */
  private val MovedMarker: String = "<- moved by GBLO"

  /** The marker drawn against a schedule period the calendar left alone. */
  private val UnmovedMarker: String = "-"

  /** What is printed where an optional property is absent. */
  private val Absent: String = "none"

  /**
   * Runs the four steps.
   *
   * Each failable call is threaded through the error channel and lifted into `IO` at the point of
   * use, so the first step that cannot be completed ends the program with a non-zero exit code
   * and the message of the failure that stopped it; a successful run ends with exit code 0.
   *
   * @return the effect of printing the four sections
   */
  override def run: IO[Unit] =
    for {
      _ <- IO.println("== OpenGamma Strata - strata-basics Scala port demo ==")
      _ <- IO.println("")
      // Step 1: the definition. `of` accumulates every invariant it can report, so it answers a
      // chain of failures and is lifted with `raiseNec`.
      definition <- raiseNec(
        PeriodicSchedule.of(
          StartDate,
          EndDate,
          Frequency.P3M,
          Adjustment,
          StubConvention.NONE,
          PreferEndOfMonth))
      _ <- printSection("1. Schedule definition", definitionLines(definition))
      // Step 2: generation. The reference data is supplied here, explicitly, and is the only
      // source of the holiday calendar the adjustment names.
      schedule <- raise(definition.createSchedule(ReferenceData.standard))
      _ <- printSection("2. Generated schedule, adjusted against ReferenceData.standard",
        scheduleLines(schedule))
      // Step 3: the FX conversion. `FxRate.of` rejects a non-positive rate and accumulates, so it
      // too is lifted with `raiseNec`; everything else here reports a single failure.
      gbpUsd <- raiseNec(FxRate.of(CurrencyPair.of(Currency.GBP, Currency.USD), GbpUsdRate))
      eurUsd <- raiseNec(FxRate.of(CurrencyPair.of(Currency.EUR, Currency.USD), EurUsdRate))
      matrix <- raise(FxMatrix.of(List(gbpUsd, eurUsd)))
      gbpAmount <- raise(CurrencyAmount.of(Currency.GBP, GbpExposure))
      eurAmount <- raise(CurrencyAmount.of(Currency.EUR, EurExposure))
      exposure <- raise(MultiCurrencyAmount.of(List(gbpAmount, eurAmount)))
      converted <- raise(exposure.convertedTo(Currency.USD, matrix))
      _ <- printSection("3. FX conversion", fxLines(gbpUsd, eurUsd, matrix, exposure, converted))
      // Step 4: serialization, through the codecs the types themselves carry.
      _ <- printSection("4. JSON, encoded with the types' own circe codecs",
        jsonLines(schedule, exposure, converted))
    } yield ()

  /**
   * Lifts a result carrying one failure into `IO`.
   *
   * This is the whole of the escalation from this library's error channel to a thrown error, and
   * it exists only because a program has to end somehow: the exception is built as a '''value'''
   * inside the mapping and handed to `IO.fromEither`, so nothing here throws and nothing in the
   * library gains an exception type.
   *
   * @param result  the result to lift
   * @tparam A  the type of the value the result carries
   * @return the value, or an effect that fails with the failure's message
   */
  private def raise[A](result: FailureOr[A]): IO[A] =
    IO.fromEither(result.left.map(failure => new IllegalStateException(failure.message)))

  /**
   * Lifts a result carrying a chain of failures into `IO`.
   *
   * The chain is reduced with [[Failure.collapse]], which folds equal failures together and joins
   * the remaining messages, so a definition rejected for several reasons reports all of them
   * rather than whichever one happened to be first. It is a separate name from [[raise]] because
   * the two argument types erase to the same JVM signature and could not be overloads.
   *
   * @param result  the result to lift
   * @tparam A  the type of the value the result carries
   * @return the value, or an effect that fails with the joined messages of every failure
   */
  private def raiseNec[A](result: ResultNec[A]): IO[A] =
    IO.fromEither(
      result.left.map(failures => new IllegalStateException(Failure.collapse(failures).message)))

  /**
   * Prints one titled section followed by a blank line.
   *
   * Every line is sequenced into the returned effect - `traverse_` rather than `foreach`, so that
   * nothing is discarded and the prints happen in order.
   *
   * @param title  the section heading
   * @param lines  the lines of the section, already rendered
   * @return the effect of printing the section
   */
  private def printSection(title: String, lines: List[String]): IO[Unit] =
    for {
      _ <- IO.println(s"-- $title --")
      _ <- lines.traverse_(line => IO.println(line))
      _ <- IO.println("")
    } yield ()

  /**
   * Renders one labelled field, padded so that a section's values share a column.
   *
   * @param label  the field name
   * @param value  the field value
   * @return the rendered line
   */
  private def field(label: String, value: String): String =
    s"  ${label.padTo(LabelWidth, ' ')}: $value"

  /**
   * Renders the schedule definition: what was asked for, and the constant each convention came
   * from, so that a reader can find the same identifiers in the source.
   *
   * @param definition  the definition to render
   * @return the lines describing it
   */
  private def definitionLines(definition: PeriodicSchedule): List[String] =
    List(
      field("unadjusted start date", definition.startDate.toString),
      field("unadjusted end date", definition.endDate.toString),
      field("periodic frequency", s"${definition.frequency} (Frequency.P3M)"),
      field(
        "business day convention",
        s"${definition.businessDayAdjustment.convention} " +
          "(BusinessDayConventions.MODIFIED_FOLLOWING)"),
      field(
        "holiday calendar",
        s"${definition.businessDayAdjustment.calendar} (HolidayCalendarIds.GBLO)"),
      field(
        "stub convention",
        definition.stubConvention.fold(Absent)(convention =>
          s"$convention (StubConvention.NONE)")),
      field(
        "roll convention",
        definition.rollConvention.fold("implied from the start date")(_.toString)),
      field("prefer end of month", PreferEndOfMonth.toString),
      field("definition", definition.toString))

  /**
   * Renders the generated schedule: its own dates, then one line per period showing the
   * unadjusted dates beside the adjusted ones, with the periods the calendar moved marked.
   *
   * @param schedule  the schedule to render
   * @return the lines describing it
   */
  private def scheduleLines(schedule: Schedule): List[String] = {
    // Both stubs in one call. The flag decides only which way a schedule of a single stub period
    // is read, and this schedule divides evenly by its frequency, so it has neither stub.
    val (initialStub, finalStub) = schedule.stubs(preferFinal = false)
    List(
      field("unadjusted start date", schedule.unadjustedStartDate.toString),
      field("unadjusted end date", schedule.unadjustedEndDate.toString),
      field("adjusted start date", schedule.adjustedStartDate.toString),
      field("adjusted end date", schedule.adjustedEndDate.toString),
      field("roll convention", schedule.rollConvention.toString),
      field("periods", schedule.size.toString),
      field("initial stub", initialStub.fold(Absent)(_.toString)),
      field("final stub", finalStub.fold(Absent)(_.toString)),
      "",
      PeriodHeader) :::
      schedule.periods.toList.zipWithIndex.map { case (period, index) =>
        periodLine(index + 1, period)
      }
  }

  /**
   * Renders one schedule period.
   *
   * @param number  the one-based period number
   * @param period  the period to render
   * @return the rendered line
   */
  private def periodLine(number: Int, period: SchedulePeriod): String = {
    val moved =
      period.startDate != period.unadjustedStartDate || period.endDate != period.unadjustedEndDate
    val unadjusted = s"${period.unadjustedStartDate} -> ${period.unadjustedEndDate}"
    val adjusted = s"${period.startDate} -> ${period.endDate}"
    "  " + number.toString.padTo(NumberWidth, ' ') + unadjusted.padTo(DateRangeWidth, ' ') +
      adjusted.padTo(DateRangeWidth, ' ') + (if (moved) MovedMarker else UnmovedMarker)
  }

  /**
   * Renders the FX step: the rates placed into the matrix, the exposure, and the single amount
   * the conversion produced.
   *
   * @param gbpUsd  the sterling rate placed into the matrix
   * @param eurUsd  the euro rate placed into the matrix
   * @param matrix  the matrix built from those two rates
   * @param exposure  the multi-currency exposure being converted
   * @param converted  the converted total
   * @return the lines describing the step
   */
  private def fxLines(
      gbpUsd: FxRate,
      eurUsd: FxRate,
      matrix: FxMatrix,
      exposure: MultiCurrencyAmount,
      converted: CurrencyAmount): List[String] =
    List(
      field("quoted rate 1", gbpUsd.toString),
      field("quoted rate 2", eurUsd.toString),
      field("fx matrix", matrix.toString),
      field("exposure", exposure.toString),
      field("target currency", Currency.USD.toString),
      field("converted total", converted.toString))

  /**
   * Renders the three JSON documents, each on one line.
   *
   * The codecs are the ones the types carry in their companions, derived at compile time; no
   * automatic derivation is imported here and nothing on this path reflects over a class.
   *
   * @param schedule  the generated schedule, whose JSON carries both date sets of every period
   * @param exposure  the multi-currency exposure before conversion
   * @param converted  the converted total
   * @return the lines describing the documents
   */
  private def jsonLines(
      schedule: Schedule,
      exposure: MultiCurrencyAmount,
      converted: CurrencyAmount): List[String] =
    List(
      field("Schedule", schedule.asJson.noSpaces),
      field("MultiCurrencyAmount", exposure.asJson.noSpaces),
      field("CurrencyAmount (converted)", converted.asJson.noSpaces))
}
