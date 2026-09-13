/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

/**
 * A named instance.
 *
 * This is the smallest and most widely mixed-in abstraction of the library: it marks a
 * value that is identified by a single unique name. The name is the value's identity in
 * text form and carries enough information to be able to recreate the instance, which is
 * what allows a named value to be written to a configuration file, a log line or a JSON
 * document as a bare string and read back later without loss.
 *
 * ===Universal trait===
 *
 * `Named` extends `Any` rather than `AnyRef`, which makes it a ''universal trait''. Only a
 * universal trait can be mixed into a value class, so a wrapper that erases to a plain
 * `String` at run time is still able to present itself as a named value:
 *
 * {{{
 * final class TickerSymbol private (val name: String) extends AnyVal with Named
 * }}}
 *
 * A conventional class or a member of a closed family mixes it in just as directly:
 *
 * {{{
 * sealed abstract class Flavour private (val name: String) extends Named
 *
 * object Flavour {
 *   case object Sweet extends Flavour("Sweet")
 *   case object Savoury extends Flavour("Savoury")
 * }
 * }}}
 *
 * Being universal is also a constraint on this trait, and the reason it is kept as small
 * as it is: a universal trait may declare `def` members only. It can hold no field, no
 * initialization statement, no self-type and no constructor, so every implementation is
 * free to satisfy `name` in whatever way suits its own representation - a constructor
 * parameter, a stored value, or a computation over other fields.
 *
 * ===Obtaining an instance from a name===
 *
 * The reverse direction - name to instance - is deliberately not part of this trait,
 * because it belongs to a type's companion rather than to its values, and because the
 * lookup differs between the two kinds of named type in this library:
 *
 *   - a closed family of named values is looked up through the `NamedEnum` typeclass
 *     instance published by its companion, whose `valueOf` and `parse` operations resolve
 *     a name against the family's fixed set of values;
 *   - a typed string, whose name space is open, is built through the `of` factory that its
 *     companion inherits from `TypedStringCompanion`, which validates the text first.
 *
 * Both routes are resolved by the compiler against a concrete companion, so a name lookup
 * is a typed call with an explicit outcome. The name-keyed lookup helper carried as a
 * static method by the Java interface this trait is ported from is therefore intentionally
 * absent here: it searched an arbitrary type for a factory method dynamically at run time,
 * a technique this port does not use anywhere.
 *
 * ===Implementation notes===
 *
 * The single member is spelled `name` rather than the `getName` of the Java original,
 * following the Scala convention for accessors. Implementations are expected to be
 * immutable and to return the same name for the lifetime of the value, since equality,
 * ordering, text rendering and serialization of named types are all derived from it. By
 * design this trait refers to no concept from any domain module, so every later module of
 * the port can depend on it without depending on any of them.
 */
trait Named extends Any {

  /**
   * Gets the unique name of the instance.
   *
   * The name contains enough information to be able to recreate the instance, using the
   * companion of the implementing type as described above.
   *
   * @return the unique name
   */
  def name: String
}
