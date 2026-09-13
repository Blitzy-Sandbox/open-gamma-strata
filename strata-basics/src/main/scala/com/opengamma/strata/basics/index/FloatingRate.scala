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
 * The index hierarchy beneath it is `sealed`; this trait is not, and that is a decision rather
 * than an oversight. Scala 2 requires every direct subtype of a sealed type to be
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
 * This trait declares the floating rate name and no currency accessor, because the two
 * implementor kinds cannot agree on the shape of one. An index carries its currency as a
 * field, so its accessor is total. A floating rate name does not: it derives a currency by
 * converting itself into an index first, an operation that can fail - the name may have no
 * index for the tenor used - and which therefore reports a
 * [[com.opengamma.strata.collect.result.Failure]]. A single member cannot be both a total
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
 * Narrowing is the intended usage of this abstraction: it exists so that code can work either
 * with a specific index or with the index group.
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
 * rejected, for a caller that does not. Neither raises an error, so text of any shape can be
 * handed to either one.
 *
 * ===The search order===
 *
 * Both operations search four families in one fixed order and return the first value found:
 * Ibor index, Overnight index, Price index, then floating rate name. That order is part of
 * the contract rather than an implementation detail, because the name spaces of the families
 * overlap and the order is what decides which of them answers. In the built-in data `GB-RPI`
 * names both a price index and a floating rate name, and `EUR-ESTR` both an Overnight index
 * and a floating rate name, so the order above is what makes each of them resolve to the
 * index. The order must not be rearranged, for performance or for anything else.
 *
 * ===Exact lookup, not lenient===
 *
 * Each family is probed through its own `valueOf`, the exact lookup: it applies the family's
 * alternate-name table and matches the result against the family's canonical and upper-case
 * keys. `EUR-ESTER` therefore resolves, through the Overnight alternate names, to the
 * `EUR-ESTR` index. What a probe deliberately does not do is apply a family's lenient
 * rewrites, which is what calling `parse` on the family instead would add: with four families
 * searched in turn, a lenient probe of an earlier family could claim text that an exact probe
 * of a later family would have matched precisely, silently changing which family answers.
 * Probing exactly is what keeps the search order above meaningful.
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
 * ===The search and the families it searches are declared apart===
 *
 * The union is two things, and they are declared separately on purpose.
 * [[FloatingRate.tryParseWith]] is the search: it is handed the probes it is to try, in the
 * order it is to try them, and it names no family at all - it is stated over any probe type,
 * so it can be read, reasoned about and tested without a single family in existence, and a
 * caller with its own composition (a subset of the families below, or a probe of its own in
 * addition to them) hands that composition over instead of writing the search again.
 * [[FloatingRate.standardLookups]] is the composition this library ships: the four families in
 * the order documented above, and the one place in this file that names them.
 *
 * That composition is a compile-time dependency of this file on the two files that declare the
 * families - the sealed index hierarchy of `Index.scala` and the family identifiers of
 * `FloatingRateName.scala` - and the dependency belongs to the contract rather than to the way
 * the search happens to be written. Two things put it there, and neither is negotiable from
 * here.
 *
 * The first is the shape of the entry points. `parse` and `tryParse` take the text and nothing
 * else, and both sibling parsers are built on them: the parse of a concrete floating rate
 * index calls `tryParse` and then converts a family identifier for a tenor, and the parse of a
 * family identifier falls back through that one. An entry point taking the text alone has to
 * reach its families through something, and nothing in this library resolves a family by
 * reflection or lets a family announce itself while the program runs, so the four families are
 * reached by being named. The second is the trait above: it declares `floatingRateName`, whose
 * type is declared in `FloatingRateName.scala`, so this file names that file whether or not it
 * searches anything.
 *
 * What the separation buys, then, is not the absence of the dependency but its shape: it is
 * declared once, in a value whose whole extent a reader can see, rather than spread through
 * the body of the search; the search itself carries none of it and can be read in isolation;
 * and a caller who wants a different set of families is served by the search rather than
 * having to reimplement it.
 *
 * ===Initialization order===
 *
 * Every family the composition names declares a type that extends the trait above, so the
 * dependency between this file and theirs is mutual. It is safe because nothing here holds a
 * family: the elements of [[FloatingRate.standardLookups]] are supplied by name, so holding the
 * composition forces none of the four companions - producing a probe creates the function and
 * does not apply it - and each companion is forced only when its own probe is reached and
 * applied. Loading this object therefore forces none of them, and each is initialized when a
 * program first reaches it, in whatever order that happens to be. The composition is memoised, so
 * a parse assembles it at most once, and memoising it changes only where the by-name elements are
 * stored: it is not evaluated while this object is being loaded, and evaluating it produces no
 * probe.
 *
 * Anything added here has to preserve that property, and the elements of the composition in
 * particular have to stay by-name. A strict sequence of the four lookups, built in place, would
 * produce all four probes where it is written, and an element produced there is free to reach
 * into its family - an eta-expansion of a family's lookup, `IborIndex.valueOf _`, does exactly
 * that, because obtaining the function evaluates the companion it is taken from - so all four
 * families would be forced on the first parse, reintroducing an initialization cycle whose
 * symptom, a member observed as missing while a class initializer is still running, surfaces only
 * for the class the program happens to touch first. Each element is therefore written out as a
 * function of the name, `(name: String) => IborIndex.valueOf(name)`, whose body names the family
 * without evaluating it until the probe is applied.
 */
object FloatingRate {

  /**
   * One family's probe, as the search consumes it: the family's own exact, alias-aware lookup
   * by name, widened to this trait.
   *
   * A family declares its lookup over its own type - the Ibor index family answers with an
   * `Option[IborIndex]` - and conforms to this type by that type being a floating rate, which
   * is checked at compile time where the probe is supplied to a composition. A family that
   * ceased to be a floating rate is therefore rejected at [[FloatingRate.standardLookups]]
   * rather than quietly dropping out of the searched set.
   */
  type Lookup = String => Option[FloatingRate]

  /**
   * Parses text naming a floating rate, of either kind, reporting a failure when it names
   * nothing.
   *
   * The four families are searched in the order documented above, and the first value found
   * is returned. Text that names no value of any of them is reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]]. The returned type is the same as
   * `collect.FailureOr[FloatingRate]`, spelled out here for readability.
   *
   * The failure names the text as it stands, so a caller correcting its input is handed the
   * whole of what was refused. The text came from outside the library, so making it safe to
   * write out
   * belongs to the writing: the text form of a failure and
   * [[com.opengamma.strata.collect.result.Failure.show]] bound every part they write and escape
   * anything a line-oriented reader could act on.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR-BBA`
   * @return the floating rate that the text names, or a failure describing the text that
   *   named none
   */
  def parse(indexStr: String): Either[Failure, FloatingRate] =
    tryParse(indexStr).toRight(
      Failure.Parsing(s"Floating rate index not known: $indexStr"))

  /**
   * Tries to parse text naming a floating rate, of either kind, answering with nothing when
   * it names none.
   *
   * This is [[FloatingRate.tryParseWith]] applied to [[FloatingRate.standardLookups]]: the
   * four families are searched in the order documented above, and the first value found is
   * returned. A probe is reached only if the probes before it found nothing, so resolving an
   * Ibor index costs one lookup rather than four.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR-BBA`
   * @return the floating rate that the text names, or nothing if it names none
   */
  def tryParse(indexStr: String): Option[FloatingRate] =
    tryParseWith(indexStr, standardLookups)

  /**
   * Searches an explicitly supplied composition of probes and answers the first value one of
   * them finds.
   *
   * This is the union rule itself, and nothing more: the probes are tried in the order they
   * are supplied, each is reached only if the ones before it found nothing, and the first
   * value found is the answer. The operation is stated over any probe type rather than over
   * this trait, because the rule has nothing to do with what a floating rate is, and stating
   * it this way is what lets it be read and tested with no family involved. Supplying a
   * sequence whose elements are themselves by-name - the `LazyList` that
   * [[FloatingRate.standardLookups]] holds is one - additionally defers producing each probe
   * until it is reached, which is what keeps a family's companion from being loaded by a
   * search that never consults it.
   *
   * {{{
   * // the four families this library ships, which is what `tryParse` searches
   * FloatingRate.tryParseWith(text, FloatingRate.standardLookups)
   *
   * // the concrete indices only, leaving the family identifiers out of the search
   * FloatingRate.tryParseWith(text, FloatingRate.standardLookups.take(3))
   * }}}
   *
   * The rule is written once for the whole package, in `Index.firstMatch`, and this is the entry
   * point to it for a caller composing probes of floating rates; the unions of the index
   * hierarchy - `Index`, `RateIndex` and `FloatingRateIndex`, each over its own
   * `standardLookups` - reach the same code.
   *
   * @tparam A  the type of value the probes answer with
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR-BBA`
   * @param lookups  the probes to search, in the order they are to be tried
   * @return the value the first matching probe found, or nothing if none of them matched
   */
  def tryParseWith[A](indexStr: String, lookups: Seq[String => Option[A]]): Option[A] =
    Index.firstMatch(indexStr, lookups)

  /**
   * The composition of family probes this library searches, in the order documented above:
   * Ibor index, Overnight index, Price index, then floating rate name.
   *
   * This is the one place in this file that names the four families, and it is what
   * [[FloatingRate.tryParse]] and [[FloatingRate.parse]] search. Each probe is a family's own
   * exact, alias-aware lookup, so `EUR-ESTER` resolves through the Overnight family's
   * alternate names and no family's lenient rewriting takes part - which is what keeps an
   * earlier family from claiming text that a later one matches precisely. A caller that needs
   * a different set composes one and passes it to [[FloatingRate.tryParseWith]] rather than
   * reimplementing the search.
   *
   * It is a memoised holder of by-name elements: the composition is assembled once, on the first
   * parse that needs it, and every later parse searches that same value, while each probe is
   * still produced only when it is reached. The note on initialization order above explains why
   * the elements being by-name matters and must not be changed, and why memoising the value they
   * are held in does not weaken it.
   *
   * @return the four family probes, in probe order, each produced when it is first reached
   */
  lazy val standardLookups: LazyList[Lookup] =
    ((name: String) => IborIndex.valueOf(name)) #::
      ((name: String) => OvernightIndex.valueOf(name)) #::
      ((name: String) => PriceIndex.valueOf(name)) #::
      ((name: String) => FloatingRateName.valueOf(name)) #::
      LazyList.empty[Lookup]
}
