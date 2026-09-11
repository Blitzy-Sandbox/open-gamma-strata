/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.named

import java.util.Locale

import scala.collection.immutable.SortedMap
import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.result.Failure

/**
 * The name lookup of a closed family of named values.
 *
 * A named family is a set of values, fixed at compile time, each identified by a name. This
 * type is the lookup for one such family: given text, it produces the value the text names,
 * and it does so using only the tables handed to it when it was built.
 *
 * ===What this replaces===
 *
 * The Java original resolved a name through a registry assembled while the program ran. It
 * read the constants of a family back from its class by reflection, then merged in any
 * further members, alternate spellings and lenient patterns declared in configuration files
 * found on the class path, so the set of resolvable names was not known until the class path
 * was. This port keeps the resolution rules and discards the machinery: the members arrive
 * as a `NonEmptyList`, the three tables arrive as maps and lists written in Scala, and
 * nothing is read from a class or from the class path. What a family can resolve is
 * therefore visible in its companion and fixed at compile time.
 *
 * ===The four tables===
 *
 * A family declares its members and up to three tables over them, each reproducing one
 * section of the configuration the original read:
 *
 *  - '''members''' - the canonical values, in declaration order, available as `values`.
 *  - '''alternate names''' - a map from an alternate spelling to a canonical name, applied
 *    before any lookup. These are the `[alternates]` rows, such as the overnight index
 *    alias `EUR-ESTER` for `EUR-ESTR`.
 *  - '''lenient patterns''' - ordered regular-expression rewrites tried only after an exact
 *    lookup has failed. These are the `[lenientPatterns]` rows, such as the rule that lets
 *    `ACT/360` resolve as the day count named `Act/360`.
 *  - '''external names''' - named groups of spellings used by an external protocol, each
 *    group a map from the external spelling to a canonical name. These are the
 *    `[externals.FpML]` and `[externals.SWIFT]` rows, and they are opt-in: they take part in
 *    no lookup and are read only through `externalNames`.
 *
 * ===How a name resolves===
 *
 * `valueOf` is the exact lookup. The name is first replaced by its canonical form if the
 * alternate-name table has an entry for it, and the result is then looked up among the
 * members. Two keys resolve for every member: its canonical name, and that name folded to
 * upper case in the English locale. Where two members would claim the same key the first in
 * declaration order keeps it, which is the rule the original followed when merging its
 * providers.
 *
 * `parse` is the lenient lookup. It tries `valueOf` first. When that finds nothing, the
 * input is folded to upper case and every lenient pattern is then applied in order, each
 * pattern that matches the whole of the current text replacing it with its replacement -
 * which may refer to captured groups as `$1` - before the next pattern is considered. The
 * text that survives that sequence is looked up once more, again through the alternate-name
 * table. The whole of the leniency is therefore declared by the family; a family that
 * declares no pattern, as most do, gets a lookup that tolerates nothing beyond the case of
 * the input.
 *
 * Both steps are total. `valueOf` answers with an `Option`, and `parse` answers with a
 * `Failure.Parsing` on the left of an `EitherNec` where the original raised an error.
 *
 * ===Instances===
 *
 * The typeclass instances of a named family all derive from its names, so they are the same
 * for every family and are built here rather than written out by each: `orderByName` gives
 * the ordering, which is also the hashing and hence the equality, and `showByName` gives the
 * rendering. A family publishes them from its own companion, so that summoning an instance
 * for the family finds exactly one.
 *
 * @tparam A  the type of the named values of the family
 */
final class NamedEnum[A <: Named] private (
    val values: NonEmptyList[A],
    val alternateNames: Map[String, String],
    private val lenientPatterns: List[(Regex, String)],
    private val externalNameRows: Map[String, Map[String, String]],
    private val family: Option[String]) {

  /**
   * The members of the family keyed by their canonical name.
   *
   * Where two members share a canonical name the first in declaration order is kept. This
   * is the normalised view of the family: every key is a name exactly as the value renders
   * it, which is what makes the map usable for writing a name out as well as reading one in.
   *
   * @return the members keyed by canonical name
   */
  val byCanonicalName: Map[String, A] =
    values.foldLeft(Map.empty[String, A]) { (acc, value) =>
      if (acc.contains(value.name)) acc else acc.updated(value.name, value)
    }

  /**
   * The members of the family keyed by their canonical name folded to upper case.
   *
   * This is the second key every member is registered under, reproducing the upper-case
   * registration of the original, and it is what makes an exact lookup insensitive to the
   * case of a name whose canonical form is mixed case. Where two members fold to the same
   * key the first in declaration order is kept.
   *
   * @return the members keyed by upper-case name
   */
  val byUpperName: Map[String, A] =
    values.foldLeft(Map.empty[String, A]) { (acc, value) =>
      val key = value.name.toUpperCase(Locale.ENGLISH)
      if (acc.contains(key)) acc else acc.updated(key, value)
    }

  private val externalsByGroup: Map[String, Map[String, A]] =
    externalNameRows.map { case (group, rows) =>
      val resolved = rows.flatMap { case (externalName, canonicalName) =>
        lookupExact(canonicalName).map(value => externalName -> value)
      }
      ArgCheck.isTrue(
        resolved.size == rows.size,
        s"External name group '$group' refers to names that are not members of the family: " +
          rows.keySet.diff(resolved.keySet).toList.sorted.mkString(", "))
      group -> resolved
    }

  private def lookupExact(name: String): Option[A] =
    byCanonicalName.get(name).orElse(byUpperName.get(name))

  /**
   * Obtains the member of the family with the specified name, if one has it.
   *
   * The lookup is the exact one described on the type: the alternate-name table is consulted
   * first, and the resulting name is matched against the canonical names of the members and
   * against those names folded to upper case. No lenient pattern is applied, so text that
   * differs from a name in any way other than its case resolves only if an alternate name
   * says it should.
   *
   * @param name  the name to look up
   * @return the member with that name, or `None` when no member has it
   */
  def valueOf(name: String): Option[A] =
    lookupExact(alternateNames.getOrElse(name, name))

  /**
   * Parses text into a member of the family, applying the leniency the family declares.
   *
   * The exact lookup of `valueOf` is tried first. Failing that, the text is folded to upper
   * case, the lenient patterns are applied in order, and the exact lookup is tried once more
   * on the result. Text that names no member after all of that is reported as a
   * `Failure.Parsing` naming the text - and the family, when the family gave its name -
   * rather than raising an error, and carries the text as the `name` attribute so that a
   * caller assembling a report does not have to recover it from the message.
   *
   * @param name  the text to parse
   * @return the member the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, A] =
    valueOf(name) match {
      case Some(value) => Right(value)
      case None =>
        val rewritten = lenientPatterns.foldLeft(name.toUpperCase(Locale.ENGLISH)) {
          case (current, (pattern, replacement)) =>
            if (pattern.matches(current)) pattern.replaceFirstIn(current, replacement) else current
        }
        valueOf(rewritten) match {
          case Some(value) => Right(value)
          case None => Left(NonEmptyChain.one(parseFailure(name)))
        }
    }

  private def parseFailure(name: String): Failure = {
    val message = family match {
      case Some(label) => s"Unknown $label name: '$name'"
      case None => s"Unknown name: '$name'"
    }
    Failure.Parsing(message, SortedMap("name" -> name))
  }

  /**
   * Returns true when the specified name resolves to a member of the family.
   *
   * The test is that of `valueOf`, so it is exact.
   *
   * @param name  the name to test
   * @return true when a member has that name
   */
  def contains(name: String): Boolean = valueOf(name).isDefined

  /**
   * Obtains the members published for the specified group of external names.
   *
   * The group is the name the family gave the table, such as `FpML` or `SWIFT`. The map
   * returned is keyed by the external spelling, and `None` is the answer for a group the
   * family does not publish, which distinguishes a group that is absent from one that is
   * present and empty.
   *
   * @param group  the name of the group of external names
   * @return the members keyed by their external spelling, or `None` when the family
   *   publishes no such group
   */
  def externalNames(group: String): Option[Map[String, A]] = externalsByGroup.get(group)

  /**
   * The ordering of the members of the family, which is also their hashing.
   *
   * The order is alphabetical by name, not the declaration order that `values` preserves.
   *
   * @return the ordering of the members by name
   */
  def order: Order[A] with Hash[A] = NamedEnum.orderByName[A]

  /**
   * The rendering of the members of the family.
   *
   * A member renders as its canonical name.
   *
   * @return the rendering of a member as its name
   */
  def show: Show[A] = NamedEnum.showByName[A]
}

/**
 * Provides the means to build the name lookup of a family, and the name-derived typeclass
 * instances every family shares.
 */
object NamedEnum {

  /**
   * Obtains the name lookup of a family from its members and its three tables.
   *
   * A family that declares no alternate names, no lenient patterns or no external names
   * passes the empty table for each - `Map.empty`, `Nil`, `Map.empty` - which is the common
   * case.
   *
   * @param values  the members of the family, in declaration order
   * @param alternateNames  the alternate spellings, each mapped to a canonical name
   * @param lenientPatterns  the lenient rewrites, in the order they are applied, each a
   *   regular expression matched against the whole of the text and the replacement for it
   * @param externalNames  the groups of external spellings, each group mapping an external
   *   spelling to a canonical name
   * @tparam A  the type of the named values of the family
   * @return the name lookup for the family
   */
  def of[A <: Named](
      values: NonEmptyList[A],
      alternateNames: Map[String, String],
      lenientPatterns: List[(String, String)],
      externalNames: Map[String, Map[String, String]]): NamedEnum[A] =
    new NamedEnum[A](values, alternateNames, compile(lenientPatterns), externalNames, None)

  /**
   * Obtains the name lookup of a family, labelling the family for its parse failures.
   *
   * This is `of` with one addition: the label is the name of the family as it should appear
   * in the message of a parse failure, so that a failure reported out of context - in a
   * chain of failures from several families, say - says which family rejected the text.
   *
   * It carries a name of its own rather than being a second `of`, because an overloaded
   * method is typed without the expected types of its arguments, which would oblige every
   * caller to write out `Map.empty[String, String]` and `List.empty[(String, String)]` in
   * full for the tables it does not declare.
   *
   * @param family  the name of the family, as it appears in a parse failure
   * @param values  the members of the family, in declaration order
   * @param alternateNames  the alternate spellings, each mapped to a canonical name
   * @param lenientPatterns  the lenient rewrites, in the order they are applied
   * @param externalNames  the groups of external spellings
   * @tparam A  the type of the named values of the family
   * @return the name lookup for the family
   */
  def ofFamily[A <: Named](
      family: String,
      values: NonEmptyList[A],
      alternateNames: Map[String, String],
      lenientPatterns: List[(String, String)],
      externalNames: Map[String, Map[String, String]]): NamedEnum[A] =
    new NamedEnum[A](values, alternateNames, compile(lenientPatterns), externalNames, Some(family))

  private def compile(lenientPatterns: List[(String, String)]): List[(Regex, String)] =
    lenientPatterns.map { case (pattern, replacement) => (pattern.r, replacement) }

  /**
   * The ordering of named values by name, which is also their hashing and their equality.
   *
   * `Order` and `Hash` both extend `Eq`, so a family publishing this one value as its
   * instance cannot end up with two notions of equality that disagree. Comparison, equality
   * and hashing are all over the name, which is sound for a closed family because a name
   * identifies a member within it.
   *
   * @tparam A  the type of the named values
   * @return the ordering of named values by name
   */
  def orderByName[A <: Named]: Order[A] with Hash[A] =
    new Order[A] with Hash[A] {
      override def compare(x: A, y: A): Int = x.name.compareTo(y.name)

      override def eqv(x: A, y: A): Boolean = x.name == y.name

      override def hash(x: A): Int = x.name.hashCode
    }

  /**
   * The rendering of named values as their name.
   *
   * @tparam A  the type of the named values
   * @return the rendering of a named value as its name
   */
  def showByName[A <: Named]: Show[A] = Show.show(_.name)
}
