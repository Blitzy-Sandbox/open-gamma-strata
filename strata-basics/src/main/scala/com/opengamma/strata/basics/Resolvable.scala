/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.data.Kleisli

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.result.Failure

/**
 * An object that can be resolved against reference data.
 *
 * This trait marks those objects that can be resolved using [[ReferenceData]]. An
 * implementation of it describes something - a trade, a leg, a payment schedule - in terms of
 * [[ReferenceDataId]] identifiers, which refer to key concepts such as holiday calendars and
 * securities without holding them. Describing it that way is what lets the description be
 * written, stored and moved around by code that has no reference data to hand.
 *
 * When `resolve` is called, those identifiers are resolved: each one is looked up in the
 * `ReferenceData` supplied and a new "resolved" instance is returned in place of this one.
 * The result is typically of a type optimized for pricing, which is the point of the
 * conversion - the resolved form has already done the lookups, so the calculation that
 * follows does none.
 *
 * ===A resolved object is bound to a moment in time===
 *
 * This is the one caveat of the whole trait, and it is carried over from the type being
 * ported because it has not stopped being true. A resolved object may be bound to data that
 * changes over time, such as a holiday calendar. If that data changes - a new holiday is
 * added, say - the resolved form is '''not''' updated: it continues to hold the calendar it
 * was resolved against. Care must therefore be taken when placing a resolved form in a cache
 * or a persistence layer, because what is being stored is an answer computed against one
 * version of the reference data rather than a description that can be re-resolved against the
 * next.
 *
 * The unresolved form has no such caveat, which is the reason both forms exist.
 *
 * ===Failure is returned, not thrown===
 *
 * Resolution can fail, and it says so in its return type. Where the Java original declared
 * `throws ReferenceDataNotFoundException` for an identifier that the reference data cannot
 * satisfy, an implementation here returns `Left(Failure.MissingData(...))` naming the
 * identifier it could not find; where the original declared `throws RuntimeException` for a
 * definition that does not describe anything resolvable, an implementation returns
 * `Left(Failure.Invalid(...))` describing what it rejected. Neither exception type is ported,
 * and this part of the library defines no failure classes of its own - both of those are
 * members of the closed [[com.opengamma.strata.collect.result.Failure]] set that the whole
 * library reports through.
 *
 * ===Implementing this trait===
 *
 * Implementations must be immutable and thread-safe, as the Java original required: an
 * unresolved description is shared freely across threads and calculations, and `resolve` must
 * be a function of its argument and the instance alone.
 *
 * Only `resolve` has to be written. `toReader` is defined in terms of it and needs overriding
 * for no reason, so an implementation is one method long:
 *
 * {{{
 * final case class LazyTrade(calendarId: HolidayCalendarId) extends Resolvable[ResolvedTrade] {
 *   def resolve(refData: ReferenceData): Either[Failure, ResolvedTrade] =
 *     calendarId.resolve(refData).map(ResolvedTrade(_))
 * }
 * }}}
 *
 * The trait is deliberately open rather than sealed, and invariant in `T`, in both respects
 * matching the interface it replaces. Resolution is an extension point: the types that
 * implement it are the trades, positions and products of the modules built on this one, each
 * in its own file, so sealing it would be incorrect as well as impossible. It is one of the
 * forward-path types of this port - nothing inside this module consumes a `Resolvable`, and it
 * is ported now because it is part of the contract the modules migrated in later slices
 * implement.
 *
 * This trait has no JSON codec. It describes a capability and carries no data of its own, so
 * there is nothing for an encoder to write; the types that implement it supply their own
 * codecs where they are serializable at all.
 *
 * @tparam T the type of the resolved result
 * @see [[ReferenceData]] for the data an instance is resolved against
 * @see [[ReferenceDataId]] for the identifiers it resolves
 */
trait Resolvable[T] {

  /**
   * Resolves this object using the specified reference data.
   *
   * This converts the object implementing this trait to the equivalent resolved form. Every
   * [[ReferenceDataId]] identifier held by this instance is looked up in the reference data
   * supplied, and the result is typically of a type optimized for pricing.
   *
   * The resolved form is bound to the data it was resolved against and will not follow
   * subsequent changes to that data, so care must be taken when placing it in a cache or a
   * persistence layer.
   *
   * An identifier that the reference data cannot satisfy yields
   * `Left(Failure.MissingData(...))`, and a definition that cannot be resolved because it is
   * not self-consistent yields `Left(Failure.Invalid(...))`. Neither case throws.
   *
   * @param refData the reference data to use when resolving
   * @return the resolved instance, or the failure explaining why it could not be resolved
   */
  def resolve(refData: ReferenceData): Either[Failure, T]

  /**
   * Expresses resolution of this object as a function awaiting reference data.
   *
   * `resolve` needs its reference data at the moment it is called. This method returns the
   * same resolution as a value - a `Kleisli` over `FailureOr` - so that several resolutions
   * and adjustments can be composed with `map`, `flatMap` and `mapN` while the data is still
   * unknown, and the composed reader is then run once against the reference data actually
   * available:
   *
   * {{{
   * import cats.syntax.apply._
   *
   * val both = (trade.toReader, otherTrade.toReader).tupled
   * val resolved = both.run(ReferenceData.standard)
   * }}}
   *
   * The import is part of the example: `tupled` is `cats` syntax on the pair of readers
   * rather than a member of either, so the composition above does not compile without it.
   *
   * The default implementation delegates to `resolve`, so an implementation that overrides
   * `resolve` - which is every implementation, the method being abstract - has that override
   * picked up here without doing anything further. The method is not `final`, leaving an
   * implementation free to supply a reader built some other way, but there is no reason to.
   *
   * The `Kleisli` type arguments are spelled out rather than inferred, over the
   * single-parameter `FailureOr` alias. That alias exists for this position: the build carries
   * no compiler plugin supplying type-lambda syntax, so a failure type applied at the use site
   * could not be written here at all.
   *
   * @return the resolution of this object as a function from reference data to the resolved
   *   instance
   */
  def toReader: RefDataReader[T] =
    Kleisli[FailureOr, ReferenceData, T](resolve)
}

/**
 * A calculation target that can be resolved using reference data.
 *
 * This is implemented by those [[CalculationTarget]] instances that must be resolved against
 * reference data at the start of the calculation process. It allows, for example, the security
 * on a trade or a position to be resolved before the decision is made about which function
 * performs the processing - the choice of function depends on what the security turns out to
 * be, so it cannot be made while the security is still only an identifier.
 *
 * The resulting target is bound to the reference data it was resolved against. If that data
 * changes, the resolved target form is '''not''' updated, so care must be taken when placing
 * it in a cache or a persistence layer. This is the same caveat that applies to
 * [[Resolvable]], for the same reason.
 *
 * ===Why this is not a `Resolvable[CalculationTarget]`===
 *
 * The two contracts are kept separate, exactly as the Java interfaces they are ported from
 * keep them separate. The method here is named `resolveTarget` rather than `resolve`, and it
 * is declared to return a `CalculationTarget` - some other target, not a resolved form of any
 * particular type - so a target is free to resolve to a target of an entirely different type.
 * Deriving this trait from `Resolvable[CalculationTarget]` would merge two distinct contracts
 * and change the public surface, so it is not done; a type that genuinely offers both
 * capabilities mixes in both traits.
 *
 * ===Failure is returned, not thrown===
 *
 * As with [[Resolvable]], the Java `throws ReferenceDataNotFoundException` becomes
 * `Left(Failure.MissingData(...))` for an identifier the reference data cannot satisfy, and
 * the Java `throws RuntimeException` becomes `Left(Failure.Invalid(...))` for a definition that
 * cannot be resolved. Neither exception type is ported.
 *
 * The trait is deliberately open rather than sealed, and, like `CalculationTarget` itself,
 * carries no data: implementations are the trades and positions of the modules built on this
 * one, they must be immutable and thread-safe, and nothing inside this module consumes one
 * today. It is ported now because it is part of the contract the modules migrated in later
 * slices implement, and it has no JSON codec for the same reason `CalculationTarget` has none.
 *
 * @see [[CalculationTarget]] for the marker trait every calculation target implements
 * @see [[Resolvable]] for the general resolution contract this one parallels
 */
trait ResolvableCalculationTarget extends CalculationTarget {

  /**
   * Resolves this target, returning the resolved instance.
   *
   * For example, if this represents a position where the security is referred to only by
   * identifier, this method converts the position to an equivalent instance with the security
   * looked up from reference data.
   *
   * The resulting target is bound to data from the reference data supplied. If that data
   * changes, the resulting target form will not be updated, so care must be taken when placing
   * the resolved form in a cache or a persistence layer.
   *
   * An identifier that the reference data cannot satisfy yields
   * `Left(Failure.MissingData(...))`, and a definition that cannot be resolved yields
   * `Left(Failure.Invalid(...))`. Neither case throws.
   *
   * @param refData the reference data to use when resolving
   * @return the resolved target, or the failure explaining why it could not be resolved
   */
  def resolveTarget(refData: ReferenceData): Either[Failure, CalculationTarget]
}
