/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek

import scala.collection.immutable.Set

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.ReferenceDataId
import com.opengamma.strata.collect.NoJavaSerialization

/**
 * Reference data that supplies a holiday calendar for every calendar identifier.
 *
 * This decorates another set of reference data so that a [[HolidayCalendarId]] the underlying
 * data does not hold still resolves: the identifier yields a calendar whose only holidays are
 * Saturday and Sunday. A calculation can therefore proceed against reference data that is
 * incomplete, which is what makes this useful for exploratory work and demonstrations - and
 * what makes it unsuitable for production, where a calendar that is missing is a fault worth
 * reporting rather than papering over. [[HolidayCalendars.defaultingReferenceData]] is how a
 * caller normally obtains one.
 *
 * Every other identifier is passed through untouched. A request for something that is not a
 * holiday calendar - a security, a curve, anything an application identifies for itself - is
 * answered by the underlying data alone and reports its absence exactly as that data would,
 * because defaulting an identifier whose data this module knows nothing about could only
 * invent a value.
 *
 * ===The defaulted calendar carries the identifier that was asked for===
 *
 * A request for `GBXX` yields a calendar named `GBXX`, not the shared `Sat/Sun` calendar. The
 * distinction matters because a calendar's identity is observable: it is what the calendar's
 * `name` reports, what its equality compares, what appears in the name of a calendar combined
 * with it, and what its JSON form records. Defaulting to the shared weekend calendar would
 * quietly rewrite the identifier a caller resolved, so a schedule built against incomplete
 * data would stop naming the centre it was written for.
 *
 * The defaulted calendar holds no holiday dates at all, so it has no range of years and every
 * query it is asked falls through to its weekend test. That is what makes it answer uniformly
 * for any date the library accepts, rather than only for the years some data set happened to
 * cover.
 *
 * ===A composite identifier is deliberately left unresolved===
 *
 * An identifier such as `GBLO+USNY` or `GBLO~USNY` is not defaulted as a whole. Answering it
 * here would produce a single weekend-only calendar named `GBLO+USNY` and would discard the
 * parts, which matters when the underlying data holds some of them: a calculation asking for
 * London combined with New York would silently lose London's holidays.
 *
 * Reporting nothing for it is what sends the request on to
 * [[HolidayCalendarId.resolve]], which - having already tried the whole name - resolves each
 * part in turn against this same reference data. Each part is then a simple identifier, so a
 * part the underlying data holds is used as it stands and a part it does not hold is defaulted
 * individually, and the pieces are combined or linked as the identifier's name directs. The
 * result is a calendar identified by the whole composite name in which every known part still
 * contributes its own holidays.
 *
 * ===Two calls, two answers for a composite identifier===
 *
 * [[findValue]] is the single query every [[ReferenceData]] implements, and it is a plain
 * lookup in a store: for a composite identifier it reports nothing, by the rule above. The
 * combined calendar is what `HolidayCalendarId.resolve(refData)` answers - the call an
 * application makes, directly or through an adjustment - because that is the call which
 * resolves a composite name part by part. Code reaching for a defaulted composite calendar
 * therefore resolves the identifier rather than querying the store.
 *
 * The type is public. Its one field is immutable reference data, so there is no invariant to
 * protect, and code that means to assert the decoration itself needs to name the type;
 * [[com.opengamma.strata.basics.CombinedReferenceData]] is public for the same reason.
 *
 * As with every other implementation of [[ReferenceData]], this type has no JSON codec and no
 * typeclass instances. It stands for a heterogeneous store of values reached by identifiers of
 * differing types, so there is nothing for an encoder to write; the one kind of reference data
 * that has a JSON form - a holiday calendar - carries its own codec.
 *
 * @param underlying  the reference data to decorate, consulted before any calendar is defaulted
 * @see [[HolidayCalendars.defaultingReferenceData]] for the way to obtain an instance
 * @see [[HolidayCalendarId.resolve]] for the resolution a composite identifier falls through to
 */
final case class HolidaySafeReferenceData(underlying: ReferenceData)
    extends ReferenceData
    with NoJavaSerialization {

  /**
   * Finds the reference data value associated with the specified identifier.
   *
   * The underlying data is consulted first, so a calendar it holds is the calendar returned
   * and nothing is defaulted over data that exists. Only where it holds nothing is a
   * weekend-only calendar supplied, and only for a simple holiday calendar identifier; for any
   * other identifier this reports exactly what the underlying data reported.
   *
   * `orElse` takes its argument by name, so the defaulted calendar is built only when the
   * lookup comes back empty.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier to find
   * @return the value held for the identifier, the defaulted calendar where the identifier
   *   names a single holiday calendar the underlying data does not hold, or empty otherwise
   */
  override def findValue[T](id: ReferenceDataId[T]): Option[T] =
    underlying.findValue(id).orElse(defaultValue(id))

  /**
   * Checks if this reference data contains a value for the specified identifier.
   *
   * This is `true` for '''every''' holiday calendar identifier, composite ones included, and
   * otherwise reports whether the underlying data holds the identifier. Composites are
   * included deliberately: this reference data does answer for `GBLO+USNY`, by way of the
   * part-by-part resolution described on the type, even though [[findValue]] reports nothing
   * for the composite name itself - so answering `false` here would understate what a caller
   * can obtain.
   *
   * The override is required rather than incidental. The default on [[ReferenceData]] is
   * defined in terms of `findValue`, which for a composite identifier is empty, and no
   * membership test defined that way could report what this implementation actually answers.
   *
   * @param id  the identifier to find
   * @return true if the identifier names a holiday calendar, or the underlying data holds a
   *   value for it
   */
  override def containsValue(id: ReferenceDataId[_]): Boolean =
    underlying.findValue(id).isDefined || id.isInstanceOf[HolidayCalendarId]

  /**
   * Combines this reference data with another.
   *
   * The decoration is re-applied around the combined data, so defaulting survives combining
   * however many times reference data is layered. That is what a caller who asked for
   * defaulting expects: without the re-wrap, combining would return a plain combination of a
   * decorated set and another set, in which an identifier neither side holds would fail again
   * - the defaulting having been applied to only one of the two sources consulted.
   *
   * Values held here still win a clash, since the combination consults this side first and
   * this side consults the data it decorates before it defaults anything.
   *
   * @param other  the other reference data
   * @return reference data answering from both sources and still defaulting calendars
   */
  override def combinedWith(other: ReferenceData): ReferenceData =
    HolidaySafeReferenceData(underlying.combinedWith(other))

  /**
   * Returns the calendar supplied for an identifier the underlying data does not hold.
   *
   * A simple holiday calendar identifier yields a calendar of that identifier whose only
   * holidays are Saturday and Sunday. A composite identifier yields nothing, which is what
   * sends its resolution to its parts, and so does any identifier that names something other
   * than a holiday calendar.
   *
   * The one cast in this file is confined to the expression below and is sound: the pattern
   * establishes that the identifier is a [[HolidayCalendarId]], and a `HolidayCalendarId` is a
   * `ReferenceDataId[HolidayCalendar]`, so `T` is `HolidayCalendar` and the calendar produced
   * is of the type the identifier asks for. The cast is unavoidable only because the value is
   * produced from a runtime test on an identifier whose type parameter is otherwise unknown
   * here - the same situation, and the same justification, as the single cast in
   * `ImmutableReferenceData.findValue`.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier the underlying data does not hold
   * @return the defaulted calendar, or empty where nothing should be defaulted for the
   *   identifier
   */
  private def defaultValue[T](id: ReferenceDataId[T]): Option[T] =
    id match {
      case calendarId: HolidayCalendarId if !HolidayCalendarId.isCompositeCalendar(calendarId) =>
        Some(
          ImmutableHolidayCalendar
            .of(calendarId, Nil, HolidaySafeReferenceData.WeekendDays)
            .asInstanceOf[T])
      case _ => None
    }
}

/**
 * Provides the weekend a defaulted calendar observes.
 *
 * The companion holds no factory. An instance is obtained either from
 * [[HolidayCalendars.defaultingReferenceData]], which is what a caller normally wants, or from
 * the `apply` the case class synthesises, which is what a test asserting the decoration itself
 * uses.
 */
object HolidaySafeReferenceData {

  /**
   * The days of the week that a defaulted calendar treats as holidays.
   *
   * Saturday and Sunday. The set is held once here rather than built per lookup, because a
   * lookup that defaults a calendar happens on the resolution path of every date adjustment
   * made against incomplete data.
   */
  private val WeekendDays: Set[DayOfWeek] = Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
}
