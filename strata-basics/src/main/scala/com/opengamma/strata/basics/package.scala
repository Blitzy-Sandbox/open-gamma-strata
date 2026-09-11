/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata

import cats.data.Kleisli

import com.opengamma.strata.collect.FailureOr

/**
 * Root package for the basic financial concepts the library is built from - currencies and
 * amounts, dates and the conventions that adjust them, indices, schedules and value schedules.
 *
 * Each of those is reached through the type or object that defines it, in this package or in a
 * nested one, and this package object adds nothing to them. Its whole purpose is the alias
 * below.
 *
 * ===Why this package object exists===
 *
 * Much of this module computes a date, a rate or a schedule that depends on reference data -
 * the holiday calendars and securities an application supplies. That dependency is explicit
 * everywhere in this library: a method that needs reference data takes it as a parameter, so
 * nothing is read from ambient state and the same call always answers the same way. The cost
 * of that discipline is that the data has to be on hand at the moment of the call, which is
 * awkward when several such calls are to be assembled into one operation and the data belongs
 * to the caller of that operation rather than to its author.
 *
 * `RefDataReader` is the answer to that: it is the type of an operation awaiting reference
 * data. A value of it composes with `map`, `flatMap`, `mapN` and the rest of the `cats`
 * vocabulary while the data is still unknown, and is run once, against the data actually
 * available, when the answer is wanted:
 *
 * {{{
 * val schedule: RefDataReader[Schedule] = periodic.toReader
 * val fixing: RefDataReader[LocalDate] = adjustment.toReader(tradeDate)
 * val both = (schedule, fixing).tupled.run(ReferenceData.standard)
 * }}}
 *
 * Every type in this module that resolves or adjusts against reference data offers both
 * forms - the direct method taking a `ReferenceData` and a `toReader` returning this type -
 * so a caller chooses between them on whether it has the data yet, never on what the operation
 * can express.
 *
 * ===The shape of the alias===
 *
 * A reader is a `Kleisli` over `FailureOr`: a function from reference data to either the value
 * or the single failure explaining its absence. `FailureOr` is the one-parameter alias the
 * `collect` module publishes for exactly this position, with the failure type already applied.
 * That is what makes this declaration compile: `Kleisli` requires a type constructor of one
 * parameter, this build carries no compiler plugin supplying type-lambda syntax, and so a
 * failure type applied at the use site could not be written here at all.
 *
 * Failures do not accumulate in a reader, and that is deliberate rather than an omission. A
 * reader fails because a piece of reference data is missing, and the lookups are sequential -
 * the second cannot be attempted until the first has answered - so there is never more than
 * one cause to report. Accumulation belongs to the validating factories, which check
 * independent parts of one input and answer with `ResultNec`.
 *
 * @see [[com.opengamma.strata.basics.ReferenceData]] for the data a reader is run against
 * @see [[com.opengamma.strata.basics.ReferenceDataId]] for the identifiers it resolves
 */
package object basics {

  /**
   * An operation awaiting reference data: a function from reference data to either the value
   * or the failure explaining its absence.
   *
   * @tparam A  the type of the value the operation produces
   */
  type RefDataReader[A] = Kleisli[FailureOr, ReferenceData, A]
}
