/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.data.Kleisli

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.result.Failure

/**
 * An identifier for a unique item of reference data.
 *
 * Reference data - holiday calendars, securities and the like - is obtained from an instance
 * of [[ReferenceData]] using an identifier of this type. The identifier is parameterized with
 * the type of the data it refers to, so a `ReferenceDataId[HolidayCalendar]` yields a holiday
 * calendar and nothing else. That parameter is what makes a lookup type-safe at the call site:
 * neither the identifier nor the reference data has to be asked what it holds.
 *
 * ===The contract an implementation must honour===
 *
 * An implementation of this trait is an identity, not a container: it carries just enough
 * information to name one item of reference data. Two consequences follow, and both are
 * requirements rather than recommendations.
 *
 * Implementations must be immutable and thread-safe, because a single identifier is shared
 * freely across threads and calculations.
 *
 * Implementations must supply value-based `equals` and `hashCode`. [[ReferenceData]] holds its
 * entries in a map keyed by identifier, so an identifier that inherits reference equality can
 * never find the value stored under an equal-but-not-identical instance. Declaring the
 * implementation a `final case class` satisfies this for free, which is what the identifier
 * families of this module and the test fixtures do; anything that cannot be a case class must
 * write the two methods by hand. The Java original relied on exactly the same contract.
 *
 * The trait is deliberately open rather than sealed. Reference data is an extension point of
 * this library: an identifier family such as `HolidayCalendarId` lives in its own file, and
 * applications and tests define identifiers of their own for data this module never sees.
 * Sealing the trait would make both impossible.
 *
 * ===Implementing it is nearly free===
 *
 * Both members below have a default implementation, expressed in terms of [[ReferenceData]]
 * alone. An identifier that is no more than an identity therefore implements no member at all:
 *
 * {{{
 * final case class TestingReferenceDataId(id: String) extends ReferenceDataId[Number]
 * }}}
 *
 * A family with a richer resolution rule overrides `resolve` and inherits the rest.
 * `HolidayCalendarId` does precisely that, because a composite identifier such as
 * `GBLO+USNY` is first looked up whole - letting a host supply one pre-combined calendar -
 * and only then resolved component by component.
 *
 * ===Two members of the Java interface are deliberately absent===
 *
 * The accessor that reported the runtime type of the data - `getReferenceDataType` - is not
 * ported. It existed so that the reference data implementation could check a stored value
 * against its identifier by reflection at run time, and the port achieves the same guarantee
 * at compile time: reference data enters a store either as a `ReferenceData.Entry[T]`, which
 * can only pair an identifier with a value of its own type, or through a factory whose key
 * type is required to be an identifier of its value type, and `ImmutableReferenceData` keeps
 * its store in a private field of a final class with a private constructor, so there is no
 * route by which a value could be filed under an identifier of another type. That is what an
 * implementation of this trait relies on when it hands back the value a lookup found without
 * checking it. Carrying a runtime type token on an identifier again, in any form, would
 * reintroduce precisely the reflection this migration removes.
 *
 * The low-level query primitive - `queryValueOrNull` - is not ported either. It signalled the
 * absence of a value by returning a reference to nothing, a convention this port does not use
 * anywhere: an absence is an `Option` returned by `ReferenceData.findValue`, and a failure to
 * resolve is a `Left`. The method therefore has no counterpart.
 *
 * This trait has no JSON codec, and neither has any identifier family other than
 * `HolidayCalendarId`. An identifier is a lookup token rather than serializable data, and the
 * reference data it addresses is excluded from the port's codec inventory for the same reason.
 *
 * @tparam T the type of the reference data this identifier refers to
 */
trait ReferenceDataId[T] {

  /**
   * Resolves this identifier against the supplied reference data.
   *
   * This is the ordinary way to obtain a value: the identifier is asked to find itself in the
   * data it is given, so reference data is threaded explicitly through every call that needs
   * it rather than read from ambient state.
   *
   * A missing value is reported, not thrown. Where the Java original raised a
   * `ReferenceDataNotFoundException`, this method returns
   * `Left(Failure.MissingData(...))` describing the identifier that could not be found, which
   * leaves the decision of what to do about it with the caller that has the context to make
   * it. The exception type is not ported.
   *
   * The default implementation delegates to `ReferenceData.getValue`, which is correct for any
   * identifier that is simply an identity. A family whose resolution involves more than a
   * single map lookup - a composite holiday calendar identifier, for instance - overrides
   * this method; because `toReader` is defined in terms of `resolve`, such an override is
   * picked up there as well.
   *
   * @param refData the reference data to resolve this identifier against
   * @return the reference data value, or the failure explaining why it could not be resolved
   */
  def resolve(refData: ReferenceData): Either[Failure, T] =
    refData.getValue(this)

  /**
   * Expresses resolution of this identifier as a function awaiting reference data.
   *
   * `resolve` needs its reference data at the moment it is called. This method returns the
   * same resolution as a value - a `Kleisli` over `FailureOr` - so that several lookups and
   * adjustments can be composed with `map`, `flatMap` and `mapN` while the data is still
   * unknown, and the composed reader is then run once against the reference data actually
   * available:
   *
   * {{{
   * val both = (calendarId.toReader, otherId.toReader).tupled
   * val resolved = both.run(ReferenceData.standard)
   * }}}
   *
   * The `Kleisli` type arguments are spelled out rather than inferred, over the single-parameter
   * `FailureOr` alias. That alias exists for this position: the build carries no compiler plugin
   * supplying type-lambda syntax, so a failure type applied at the use site could not be written
   * here at all.
   *
   * @return the resolution of this identifier as a function from reference data to the value
   */
  def toReader: RefDataReader[T] =
    Kleisli[FailureOr, ReferenceData, T](resolve)
}
