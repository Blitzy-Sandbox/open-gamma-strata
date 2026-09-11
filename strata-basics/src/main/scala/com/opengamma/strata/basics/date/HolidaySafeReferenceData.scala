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

/**
 * Reference data that supplies a holiday calendar for every calendar identifier.
 *
 * This decorates another set of reference data so that a [[HolidayCalendarId]] the underlying
 * data does not hold still resolves: the identifier yields a calendar whose only holidays are
 * Saturday and Sunday. A calculation can therefore proceed against reference data that is
 * incomplete, which is what makes this useful for exploratory work, demonstrations and tests -
 * and what makes it unsuitable for production, where a calendar that is missing is a fault
 * worth reporting rather than papering over. [[HolidayCalendars.defaultingReferenceData]] is
 * how a caller normally obtains one.
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
 * data would no longer say which centre it had meant.
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
 * ===Divergences from the type being ported===
 *
 * The type being ported was a bean carrying one property, with generated builder, equality and
 * serialization support. None of that machinery is ported: this is a case class, whose
 * structural equality over the single underlying set is what the generated equality did, and
 * neither the serialization support nor the builder has a counterpart in this port.
 *
 * Where the original implemented a low-level query that signalled absence by returning a
 * reference to nothing, this implements [[findValue]] returning an `Option`, which is the one
 * primitive of [[ReferenceData]] in this port. The consequence is worth stating, because it
 * changes which call a caller observes composite behaviour through: in the library being
 * ported the identifier drove the lookup, so asking that library's reference data for
 * `GBLO+USNY` returned the combined calendar; here the lookup is a plain query of a store and
 * `findValue` reports nothing for a composite identifier, while
 * `HolidayCalendarId.resolve(refData)` - the call an application makes, directly or through an
 * adjustment - returns the combined calendar.
 *
 * The type is public where the original was visible only within its package. A case class
 * synthesises a public `apply` from its constructor regardless, so restricting the type would
 * take a hand-written companion and buy nothing: there is no invariant to protect, the single
 * field being immutable reference data, and a test that means to assert the decoration itself
 * needs to name the type. [[com.opengamma.strata.basics.CombinedReferenceData]] is public in
 * this port for the same reason.
 *
 * As with every other implementation of [[ReferenceData]], this type has no JSON codec and no
 * typeclass instances. It stands for a heterogeneous store of values reached by identifiers of
 * differing types, so there is nothing for an encoder to write; the one kind of reference data
 * that is serializable - a holiday calendar - carries its own codec.
 *
 * @param underlying  the reference data to decorate, consulted before any calendar is defaulted
 * @see [[HolidayCalendars.defaultingReferenceData]] for the way to obtain an instance
 * @see [[HolidayCalendarId.resolve]] for the resolution a composite identifier falls through to
 */
final case class HolidaySafeReferenceData(underlying: ReferenceData) extends ReferenceData {

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
   * Saturday and Sunday, which is the weekend the library being ported defaulted to. It is
   * held once here rather than built per lookup, because a lookup that defaults a calendar
   * happens on the resolution path of every date adjustment made against incomplete data.
   */
  private val WeekendDays: Set[DayOfWeek] = Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
}
