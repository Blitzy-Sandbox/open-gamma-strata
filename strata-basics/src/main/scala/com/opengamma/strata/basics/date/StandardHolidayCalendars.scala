/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import com.opengamma.strata.basics.ReferenceDataId

/**
 * The holiday calendars built into this library, keyed by their identifiers.
 *
 * A holiday calendar is reference data: code that adjusts a date asks for a calendar by
 * identifier and is given one by the [[com.opengamma.strata.basics.ReferenceData]] it was
 * passed, so that an application can supply its own holidays where its own holidays are what
 * matter. Most applications, and every test and demonstration, want the calendars this
 * library already knows about instead, and this object is where those live: it is the single
 * place the built-in set is written down, and
 * `com.opengamma.strata.basics.ReferenceData.standard` is exactly this set presented as
 * reference data.
 *
 * Two views are published, because the library draws a line between them:
 *
 *   - [[all]] is every built-in calendar - the rule-generated national calendars, the
 *     published Thai calendar, and the weekend and no-holiday calendars.
 *   - [[minimal]] is the calendars that are not really market data at all: the no-holidays
 *     calendar and the Saturday/Sunday, Friday/Saturday and Thursday/Friday weekend
 *     calendars. No reasonable application defines these differently, so they are the set
 *     `ReferenceData.of` places underneath a caller's own entries.
 *
 * ===Why the views are typed as they are===
 *
 * Both are iterables of identifier-to-value pairs rather than a map of calendars. That is
 * what keeps the dependency between this package and the root package pointing one way: the
 * root package defines [[ReferenceDataId]] and knows nothing of holiday calendars, while this
 * package defines the calendars and their identifiers and reads that abstraction from there.
 * A `Map[HolidayCalendarId, HolidayCalendar]` is an iterable of its pairs and both `Iterable`
 * and a pair are covariant, so a view of that type satisfies this one by ordinary subtyping,
 * with neither a cast nor a rebuild at the call site.
 */
object StandardHolidayCalendars {

  /**
   * Every holiday calendar built into this library, keyed by its own identifier.
   *
   * This is what `ReferenceData.standard` presents, and it is what resolves an identifier
   * such as `GBLO` for code that was given no reference data of its own.
   *
   * @return the identifier-to-calendar pairs of every built-in calendar
   */
  val all: Iterable[(ReferenceDataId[_], Any)] = Nil

  /**
   * The holiday calendars that carry no market convention of their own.
   *
   * These are the no-holidays calendar and the three weekend calendars: the ones whose
   * contents follow from their names, so that an application has nothing to disagree with.
   * They are the set `ReferenceData.of` supplies underneath a caller's own entries, and they
   * are a subset of [[all]].
   *
   * @return the identifier-to-calendar pairs of the weekend and no-holiday calendars
   */
  val minimal: Iterable[(ReferenceDataId[_], Any)] = Nil
}
