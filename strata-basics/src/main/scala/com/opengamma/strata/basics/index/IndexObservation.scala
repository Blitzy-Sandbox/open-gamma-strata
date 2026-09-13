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
 * and this abstraction therefore asks for nothing but the index itself. The implementations this
 * module provides are [[IborIndexObservation]] (a fixing date, with the deposit period and year
 * fraction it implies), [[OvernightIndexObservation]] (a fixing date, with its publication and
 * effective dates), [[PriceIndexObservation]] (a fixing month, price indices publishing monthly)
 * and [[FxIndexObservation]] (a fixing date and the maturity date it implies), each in a file of
 * its own. Code that needs one of those specifics holds the implementation; code that only needs
 * to know which index was observed - to look up market data for it, or to report on it - holds
 * this trait.
 *
 * ===This trait is open===
 *
 * The interface being ported is a plain Java interface, and this trait is open for the same
 * reason: an application that observes an indicator of its own can implement it and be carried
 * by every signature written in terms of observations. The four implementations above are the
 * ones this module publishes, not the only ones a program may hold, so they are described here
 * as what is built in rather than as an exhaustive set.
 *
 * Two consequences follow. First, the set of observations is not closed, so a `match` over this
 * trait is not checked for exhaustiveness by the compiler: code that narrows an observation
 * supplies a default branch, exactly as code over the Java interface tested with `instanceof`
 * did. Second, an implementation this module has never seen reaches every operation declared
 * over the trait, which is the extension point the interface exists to offer.
 *
 * Openness is deliberate rather than incidental, and it is the reason the four implementations
 * are four sources. Scala 2 admits a direct subtype of a `sealed` type only in the file that
 * declares the type, so sealing this trait would drag all four into this one file; the module's
 * file layout gives each its own, alongside its factories, typeclass instances and JSON codec.
 * The closed hierarchies of this port are the ones whose closedness is worth that price and are
 * named as closed by the request - the [[Index]] families in `Index.scala`, the calendars in
 * `HolidayCalendar.scala`, `Rounding` and the failure model - and an observation is not among
 * them: it is a value an instrument makes, not a published set of values with a lookup. The
 * other traits left open are open for reasons of the same kind: `ReferenceData`, because
 * supplying reference data is what an application does with it, and `FloatingRate`, because it
 * is implemented by `FloatingRateName` as well as by the index families.
 *
 * ===Only the index is declared here===
 *
 * This trait declares one member, the index, because that is the one thing every observation
 * has. In particular it declares no currency accessor, because the implementations could not
 * agree on one: three of them report a single `currency` taken from their index, while
 * [[FxIndexObservation]] reports a `currencyPair`, an exchange rate being a relation between
 * two currencies rather than an amount in one. Each
 * implementation therefore declares the accessor that is honest for it, and a caller holding
 * this trait reaches it by narrowing to the kind in hand.
 *
 * ===Implementation notes===
 *
 * An implementation is expected to be an immutable value, and is therefore safe to share between
 * threads, and to report the same index for its whole lifetime, since the index is what the
 * observation is of. An implementation is also expected to be internally consistent: any date it
 * derives from its fixing point is derived through the index it reports, so that the observation
 * and the index cannot disagree. The four implementations this module publishes hold themselves
 * to that: the three with derived dates are built only through a factory that computes them.
 *
 * Each of them satisfies `index` with the index type of its own family - `IborIndex`,
 * `OvernightIndex`, `PriceIndex` or `FxIndex`, every one of them a subtype of [[Index]] - which
 * narrows the result for a caller holding the implementation and is what lets such a caller
 * reach the fields of the index without a cast, while still satisfying this declaration.
 *
 * This trait holds no data of its own: it is an abstraction over values that carry their own.
 * No JSON serialization instances are declared for it, accordingly; those are declared by, and
 * belong to, the implementations that do hold data.
 *
 * ===Why a class and not a trait===
 *
 * The four implementations are the whole of this family, and `sealed` says so to the Scala
 * compiler - which is what makes a match over them exhaustive. It says nothing at all in the
 * class file: a trait compiles to a plain JVM interface, and a class file compiled elsewhere may
 * implement an interface without running any constructor of this library, so a fifth kind of
 * observation could be presented to code the compiler had proved could only meet four. Declaring
 * the root as an abstract class closes that: a type cannot be claimed without extending it, and
 * extending it means running the constructor below, which refuses any subtype outside the four.
 * The constructor is `private[index]` so that only this package can declare a subtype in the
 * first place, and the guard is what holds where that modifier does not survive compilation.
 *
 * ===Serialization===
 *
 * An observation is written as JSON through the codec its own implementation publishes and in no
 * other form. All four implementations are `case class`es, which the compiler makes
 * `java.io.Serializable` whether or not the library wants it, so this trait mixes in
 * [[NoJavaSerialization]]: every observation refuses to be written to or read from an object
 * stream, and dates assembled by a stream rather than derived from an index by a factory cannot
 * be presented as an observation of this library. Carrying the refusal here rather than on each
 * implementation is what makes it one statement about the whole family, including the total
 * [[PriceIndexObservation]], whose fields no factory derives.
 *
 * @see [[Index]] for the indices that can be observed
 * @see [[IborIndexObservation]], [[OvernightIndexObservation]], [[PriceIndexObservation]] and
 *      [[FxIndexObservation]] for the four kinds of observation this module builds
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
