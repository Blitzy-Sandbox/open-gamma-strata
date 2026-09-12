/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

/**
 * A single observation of an index.
 *
 * Implementations of this trait represent observations of an index. For example, an observation
 * of `GBP-LIBOR-3M` at a specific fixing date.
 *
 * An [[Index]] is the agreed mechanism for determining a financial indicator; an observation is
 * one use of that mechanism - the index together with the point at which its figure is fixed,
 * and whatever dates follow from that point. Where an index is reference data shared by every
 * trade that refers to it, an observation belongs to the instrument that made it, which is why
 * an observation is an ordinary value built on demand rather than a member of a published set.
 *
 * What identifies the point of fixing, and what follows from it, differs by the kind of index,
 * and this abstraction therefore asks for nothing but the index itself. The four implementations
 * are [[IborIndexObservation]] (a fixing date, with the deposit period and year fraction it
 * implies), [[OvernightIndexObservation]] (a fixing date, with its publication and effective
 * dates), [[FxIndexObservation]] (a fixing date and the maturity date it implies) and
 * [[PriceIndexObservation]] (a fixing month, price indices publishing monthly). Code that needs
 * one of those specifics holds the implementation; code that only needs to know which index was
 * observed - to look up market data for it, or to report on it - holds this trait.
 *
 * ===This trait is deliberately open===
 *
 * Unlike the index hierarchy it refers to, this trait is not `sealed`, and that is a decision
 * rather than an oversight. Scala 2 requires every direct subtype of a sealed type to be
 * declared in the same file as the type itself, and the four implementations of this trait are
 * four separate files, one per kind of observation, each sitting beside the index family it
 * observes. Sealing here would collapse those four files into this one for no gain, since the
 * set this port commits to closing is the index family - [[Index]] and its leaves, sealed
 * together inside `Index.scala` - and not the observations made of it. The same treatment, and
 * the same reasoning, applies to the other abstractions of this port whose implementations are
 * spread across files: `FloatingRate`, `FxConvertible` and `ReferenceData`.
 *
 * Two consequences follow, and neither is a departure from the interface being ported, which is
 * likewise a plain, implementable interface. First, a `match` over this trait is not checked for
 * exhaustiveness by the compiler, so code that narrows an observation should carry a default
 * branch - exactly as code over the original tests one reference with a sequence of type tests.
 * Second, an application may add an observation of its own, which is the extension point the
 * original offers and this port keeps. Do not tighten this declaration: doing so breaks the file
 * layout of the package and removes that extension point.
 *
 * ===Only the index is declared here===
 *
 * This trait declares one member, the index, because that is the one thing every observation
 * has. In particular it declares no currency accessor. The interface being ported declares none
 * either, and the implementations could not agree on one: three of them report a single
 * `currency` taken from their index, while [[FxIndexObservation]] reports a `currencyPair`,
 * an exchange rate being a relation between two currencies rather than an amount in one. Each
 * implementation therefore declares the accessor that is honest for it, and a caller holding
 * this trait reaches it by narrowing to the kind in hand.
 *
 * ===Implementation notes===
 *
 * An implementation is expected to be an immutable value, and is therefore safe to share between
 * threads, and to report the same index for its whole lifetime, since the index is what the
 * observation is of. An implementation is also expected to be internally consistent: any date it
 * derives from its fixing point is derived through the index it reports, so that the observation
 * and the index cannot disagree.
 *
 * Each implementation satisfies `index` with the index type of its own family - `IborIndex`,
 * `OvernightIndex`, `FxIndex` or `PriceIndex`, every one of them a subtype of [[Index]] - which
 * narrows the result for a caller holding the implementation and is what lets such a caller
 * reach the fields of the index without a cast, while still satisfying this declaration.
 *
 * This trait holds no data of its own: it is an abstraction over values that carry their own,
 * and every value reaching it is one of the four implementations. No JSON serialization
 * instances are declared for it, accordingly; those are declared by, and belong to, the
 * implementations that do hold data.
 *
 * @see [[Index]] for the indices that can be observed
 * @see [[IborIndexObservation]], [[OvernightIndexObservation]], [[FxIndexObservation]] and
 *      [[PriceIndexObservation]] for the four kinds of observation
 */
trait IndexObservation {

  /**
   * Gets the index to be observed.
   *
   * The index is the identity of what was observed and is never absent: an observation cannot
   * exist without the index it was made of.
   *
   * @return the index
   */
  def index: Index
}
