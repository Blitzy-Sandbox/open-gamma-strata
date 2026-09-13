/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata

/**
 * Root package for the common data structures the library is built from.
 *
 * The package itself is a collection of independent pieces - a named-instance abstraction and
 * the enumeration support built on it, fixed-scale decimal arithmetic, wrapped numeric arrays,
 * argument checking, serialization helpers - and each of them is reached through the type or
 * object that defines it, in this package or in a nested one. This package object adds nothing
 * to that. Its whole purpose is the error vocabulary below.
 *
 * ===Why this package object exists===
 *
 * An operation in this library that cannot produce a value returns a description of what
 * stopped it rather than abandoning the call stack, so the four types that describe such an
 * outcome are named in the signature of a large proportion of the library's methods. They are
 * defined in the nested `result` package, alongside the failure they carry and the combinators
 * over them, because that is where they belong. Requiring every caller to import them from
 * there in addition to whatever it needs from the module root, however, would put a second
 * import line in almost every file that uses this library for no benefit.
 *
 * Re-exporting the four names here removes that second line: one wildcard import
 *
 * {{{
 * import com.opengamma.strata.collect._
 * }}}
 *
 * supplies the error vocabulary of the module together with everything else it offers, which
 * is why this is the import a file in this library or in a module built on it conventionally
 * takes.
 *
 * ===The four names===
 *
 * Each alias forwards to the definition in the `result` package and adds nothing of its own:
 *
 *   - `FailureOr[A]` - a value, or the single failure that explains why there is no value.
 *   - `ResultNec[A]` - a value, or a non-empty chain of failures. This is what a validating
 *     factory returns, so it is the most frequently seen of the four.
 *   - `ValidatedFailures[A]` - the same pair of possibilities in accumulating form, used while
 *     several checks over one input are being combined.
 *   - `ValueWithFailures[A]` - a value, failures, or both, for an operation that can partly
 *     succeed.
 *
 * The name reached through this package object and the name reached through the `result`
 * package denote the same type in every case, and a value of one is a value of the other
 * without conversion. Which path a file takes is therefore a matter of convenience, with one
 * caveat: a file that wildcard-imports both this package and the `result` package sees each of
 * the four names twice and every use of one becomes an ambiguous reference. A file should
 * import them from exactly one of the two - conventionally this one - and reach the
 * combinators, the failure type and the reasons through their own names in `result`.
 *
 * ===The arity of the aliases===
 *
 * Every one of the four takes a single type parameter, with the failure type already applied.
 * That is deliberate and is part of the contract of this module rather than an incidental
 * convenience: it is what allows one of them to stand in a position that requires a type
 * constructor of one parameter. A function that has to read some ambient context before it can
 * produce a value is expressed as a `Kleisli` over `FailureOr`, and the modules built on this
 * one define exactly that:
 *
 * {{{
 * type RefDataReader[A] = Kleisli[FailureOr, ReferenceData, A]
 * }}}
 *
 * Written with the failure type spelled out at the use site instead, the alias would take two
 * type parameters and that declaration would no longer compile, because this build deliberately
 * carries no compiler plugin supplying type-lambda syntax. So none of these four aliases should
 * be "simplified" by inlining its failure type, however redundant the indirection looks from a
 * position that happens to apply both parameters at once.
 *
 * @see [[com.opengamma.strata.collect.result.Failure]] for the failure each of these outcomes carries
 * @see [[com.opengamma.strata.collect.result.FailureReason]] for the reasons a failure can carry
 */
package object collect {

  /**
   * The outcome of an operation that either produces a value or fails for a single reason.
   *
   * This is the module root's name for [[com.opengamma.strata.collect.result.FailureOr]], which
   * is where the type is defined and documented; the two names denote the same type.
   *
   * @tparam A  the type of the value produced when the operation succeeds
   */
  type FailureOr[A] = result.FailureOr[A]

  /**
   * The outcome of an operation that either produces a value or fails for one or more reasons.
   *
   * This is the module root's name for [[com.opengamma.strata.collect.result.ResultNec]], which
   * is where the type is defined and documented; the two names denote the same type.
   *
   * @tparam A  the type of the value produced when the operation succeeds
   */
  type ResultNec[A] = result.ResultNec[A]

  /**
   * The accumulating form of a result, used while several checks over one input are combined.
   *
   * This is the module root's name for
   * [[com.opengamma.strata.collect.result.ValidatedFailures]], which is where the type is
   * defined and documented; the two names denote the same type.
   *
   * @tparam A  the type of the value produced when every check passes
   */
  type ValidatedFailures[A] = result.ValidatedFailures[A]

  /**
   * A value, failures, or both: the outcome of an operation that can partly succeed.
   *
   * This is the module root's name for
   * [[com.opengamma.strata.collect.result.ValueWithFailures]], which is where the type is
   * defined and documented; the two names denote the same type.
   *
   * @tparam A  the type of the value, typically a collection type
   */
  type ValueWithFailures[A] = result.ValueWithFailures[A]
}
