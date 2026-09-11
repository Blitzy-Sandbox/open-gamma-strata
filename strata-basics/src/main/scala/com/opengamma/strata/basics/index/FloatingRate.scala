/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.result.Failure

/**
 * An index, or a group of indices, used to provide floating rates, typically in interest
 * rate swaps.
 *
 * A floating rate is identified in one of two ways in this library, and this trait is the
 * abstraction over both. The first is a concrete index - an Ibor index, an Overnight index or
 * a Price index - which carries its own currency, calendars, offsets and conventions, and can
 * be used for pricing as it stands. The second is a floating rate ''name'': the FpML/ISDA
 * style identifier of a whole family of rates, such as `GBP-LIBOR`, which names the family
 * without fixing a tenor and therefore has to be turned into a concrete index before it can
 * be used. Both kinds are named values, and both report the family they belong to, which is
 * precisely what this trait asks of them.
 *
 * The two kinds share a name space: a piece of text may name either, and which of the two it
 * names is frequently not known to the code holding the text. [[FloatingRate.parse]] is the
 * resolution that does not have to know - it searches both kinds, in a fixed order, and hands
 * back whichever it found. A caller that needs a concrete index whatever the text named uses
 * `FloatingRateIndex.parse` instead, which is built on top of this one and adds the step that
 * converts a name into an index for a tenor.
 *
 * ===This trait is deliberately open===
 *
 * Unlike the index hierarchy beneath it, this trait is not `sealed`, and that is a decision
 * rather than an oversight. Scala 2 requires every direct subtype of a sealed type to be
 * declared in the same file as the type itself, and the two implementor kinds of this trait
 * cannot satisfy that rule together: the sealed `FloatingRateIndex` hierarchy is declared in
 * `Index.scala`, which is what keeps the index family closed, while `FloatingRateName` is a
 * family in its own right declared in `FloatingRateName.scala`. Sealing here would force
 * those two independent families into a single file for no gain. The family this library
 * commits to closing is `Index` together with its leaves; the abstraction above them is open,
 * so a `match` over this trait is not checked for exhaustiveness and code that narrows a
 * `FloatingRate` should allow for a subtype it does not know.
 *
 * ===The currency is not declared here===
 *
 * The interface this is ported from declares an abstract currency accessor alongside the
 * floating-rate-name one. This port declares only the latter, because the two implementor
 * kinds cannot agree on the shape of the former. An index carries its currency as a field, so
 * its accessor is total. A floating rate name does not: it derives a currency by converting
 * itself into an index first, an operation that can fail - the name may have no index for the
 * tenor used - and which therefore reports a [[com.opengamma.strata.collect.result.Failure]]
 * in this port where the original raised an error. A single member cannot be both a total
 * `Currency` and a failure-or-`Currency`, so each implementor kind declares the accessor in
 * the shape that is honest for it: `currency: Currency` on the sealed `FloatingRateIndex`,
 * and `currency: Either[Failure, Currency]` on `FloatingRateName`. A caller holding a
 * `FloatingRate` reaches either by narrowing to the kind in hand:
 *
 * {{{
 * FloatingRate.parse(text).flatMap {
 *   case index: FloatingRateIndex => Right(index.currency)
 *   case name: FloatingRateName => name.currency
 * }
 * }}}
 *
 * Narrowing is the usage the original documents for this abstraction - it exists so that code
 * can work either with a specific index or with the index group - so the shape of the port
 * follows the shape of the intended use. The divergence is recorded in `SCALA_MIGRATION.md`
 * among the accessors that became failure-reporting.
 *
 * ===Implementation notes===
 *
 * Implementations are expected to be immutable and are therefore safe to share between
 * threads, and to report the same name for their whole lifetime, since the name is the
 * identity of a named value. This abstraction holds no data of its own - every value reaching
 * it is a member of one of the two implementor families - so no JSON serialization instances
 * are declared for it; those are declared by, and belong to, the leaf families that do hold
 * data.
 *
 * @see [[FloatingRateName]] for the family identifier and the conversion to a concrete index
 * @see [[FloatingRateType]] for the kind of rate a floating rate name describes
 */
trait FloatingRate extends Named {

  /**
   * Gets the floating rate name identifying the family that this floating rate belongs to.
   *
   * For a concrete index this is the family the index is a member of, which loses the index's
   * tenor: the three-month GBP Libor index and the six-month one report the same name. For a
   * floating rate name it is the value itself, so the operation is the identity there.
   *
   * @return the floating rate name of this floating rate
   */
  def floatingRateName: FloatingRateName
}

/**
 * Resolves text that names a floating rate of either kind.
 *
 * The two operations below differ only in how they report text that names nothing:
 * [[FloatingRate.tryParse]] answers with an `Option`, for a caller that has something else to
 * try, and [[FloatingRate.parse]] with an `Either` carrying a failure that explains what was
 * rejected, for a caller that does not. Neither raises an error, where the interface being
 * ported raised one from `parse`; the message that error carried is now the message of the
 * failure, unchanged, so a log line written from it reads as it did before.
 *
 * ===The search order===
 *
 * Both operations search four families in one fixed order and return the first value found:
 * Ibor index, Overnight index, Price index, then floating rate name. That order is part of
 * the contract rather than an implementation detail, because the name spaces of the families
 * overlap - `GB-RPI` names both a price index and a floating rate name - and the order is
 * what decides which of the two answers. It reproduces the order of the interface being
 * ported and must not be rearranged, for performance or for anything else. The closedness
 * specification of the port asserts that no other name is claimed by two families today, so
 * the order is currently observable only for the overlaps it was written for.
 *
 * ===Exact lookup, not lenient===
 *
 * Each family is probed through its own `valueOf`, the exact lookup: it applies the family's
 * alternate-name table and matches the result against the family's canonical and upper-case
 * keys. `EUR-ESTER` therefore resolves, through the Overnight alternate names, to the
 * `EUR-ESTR` index. What a probe deliberately does not do is apply a family's lenient
 * rewrites, which is what calling `parse` on the family instead would add: with four families
 * searched in turn, a lenient probe of an earlier family could claim text that an exact probe
 * of a later family would have matched precisely, silently changing which family answers. The
 * interface being ported probes exactly for that reason, and this port keeps to it.
 *
 * {{{
 * FloatingRate.tryParse("GBP-LIBOR-3M")   // Some(IborIndices.GBP_LIBOR_3M)
 * FloatingRate.tryParse("GBP-SONIA")      // Some(OvernightIndices.GBP_SONIA)
 * FloatingRate.tryParse("EUR-ESTER")      // Some(OvernightIndices.EUR_ESTR), through an alternate name
 * FloatingRate.tryParse("GB-RPI")         // Some(PriceIndices.GB_RPI), the index, not the name
 * FloatingRate.tryParse("GBP-LIBOR-BBA")  // Some(the GBP-LIBOR-BBA floating rate name)
 * FloatingRate.tryParse("rubbish")        // None
 * }}}
 *
 * ===Initialization order===
 *
 * This object names the companions of all four families, and every one of them declares types
 * that extend the trait above, so the dependency between this file and theirs is mutual -
 * exactly as it is in the sources being ported. It is safe because each reference sits inside
 * a method body: nothing here is a `val` or a `lazy val` reading another companion, so loading
 * this object forces none of the four, and each is initialized when a program first reaches it,
 * in whatever order that happens to be. Anything added here has to preserve that property. A
 * field holding, say, the four lookups pre-assembled into a list would introduce an
 * initialization cycle, and the symptom of such a cycle - a member observed as missing while a
 * class initializer is still running - depends on which of the classes the program touches
 * first, which makes it a defect that testing can easily miss.
 */
object FloatingRate {

  /**
   * Parses text naming a floating rate, of either kind, reporting a failure when it names
   * nothing.
   *
   * The four families are searched in the order documented above, and the first value found
   * is returned. Text that names no value of any of them is reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]] carrying the message the ported
   * interface used for the error it raised in the same situation. The returned type is the
   * same as `collect.FailureOr[FloatingRate]`, spelled out here for readability.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR-BBA`
   * @return the floating rate that the text names, or a failure describing the text that
   *   named none
   */
  def parse(indexStr: String): Either[Failure, FloatingRate] =
    tryParse(indexStr).toRight(Failure.Parsing(s"Floating rate index not known: $indexStr"))

  /**
   * Tries to parse text naming a floating rate, of either kind, answering with nothing when
   * it names none.
   *
   * The four families are searched in the order documented above, and the first value found
   * is returned. Each probe is evaluated only if the probes before it found nothing, since the
   * alternatives of `orElse` are passed by name, so resolving an Ibor index costs one lookup
   * rather than four. The result of each probe is widened to `FloatingRate` explicitly: the
   * widening is then checked against the declaration of each family rather than inferred from
   * it, so a family that ceased to be a floating rate would be reported here, by the compiler,
   * instead of quietly leaving the searched set.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR-BBA`
   * @return the floating rate that the text names, or nothing if it names none
   */
  def tryParse(indexStr: String): Option[FloatingRate] =
    IborIndex
      .valueOf(indexStr)
      .orElse[FloatingRate](OvernightIndex.valueOf(indexStr))
      .orElse[FloatingRate](PriceIndex.valueOf(indexStr))
      .orElse[FloatingRate](FloatingRateName.valueOf(indexStr))
}
