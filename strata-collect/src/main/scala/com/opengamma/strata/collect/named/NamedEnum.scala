/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.named

import java.util.Locale

import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.result.Failure

/**
 * The name lookup of a closed family of named values.
 *
 * A named family is a set of values, fixed when the family is compiled, each identified by
 * a name. This typeclass is the lookup for one such family: given text, it produces the
 * value that the text names, using nothing but the members and the tables handed to it when
 * it was built. A family builds its lookup in its own companion and publishes it there
 * implicitly, so that resolving a name is an ordinary typed call and `NamedEnum[Sample]`
 * summons the lookup of the family:
 *
 * {{{
 * sealed abstract class Sample private (val name: String) extends Named
 *
 * object Sample {
 *   case object Standard extends Sample("Standard")
 *   case object Extra extends Sample("Extra")
 *
 *   val namedEnum: NamedEnum[Sample] =
 *     NamedEnum.of(
 *       values = NonEmptyList.of(Standard, Extra),
 *       alternates = Map("Alternate" -> "Standard"),
 *       familyName = "Sample")
 * }
 *
 * Sample.namedEnum.valueOf("Standard") // Some(Standard)
 * Sample.namedEnum.parse("alternate") // Right(Standard)
 * }}}
 *
 * ===What this replaces===
 *
 * The type being ported resolved a name through a registry assembled while the program ran.
 * It read the constants of a family back from its own class reflectively, then merged in
 * any further members, alternate spellings, lenient rewrites and external mappings declared
 * in configuration found on the class path, so the set of resolvable names was not known
 * until the class path was. This port keeps every resolution rule of that mechanism and
 * discards the mechanism itself: the members arrive as a `NonEmptyList`, the three tables
 * arrive as Scala values, and nothing is read from a class or from the class path. What a
 * family can resolve is therefore visible in its companion and settled at compile time,
 * which is what makes the family closed.
 *
 * ===What a family declares===
 *
 * A family declares its members and up to three tables over them, each reproducing one
 * section of the configuration that the previous configuration-driven runtime registry
 * read:
 *
 *  - '''members''' - the canonical values, in declaration order, published as `values`.
 *  - '''alternate names''' - a map from an alternate spelling to a canonical name, applied
 *    before every lookup, exact and lenient alike. An alternate name is how a family
 *    accepts a spelling it has retired, or a second spelling it never owned.
 *  - '''lenient patterns''' - ordered regular-expression rewrites, tried only after an
 *    exact lookup has failed. They are how a family accepts a whole shape of text rather
 *    than one spelling at a time.
 *  - '''external names''' - named groups of spellings used by other systems, each group a
 *    map from an external spelling to a canonical name. These are opt-in: they take part in
 *    no lookup and are read only through `externalNames` and `externalNamesRaw`.
 *
 * Every table is permitted to be empty, and for most families all three are.
 *
 * ===How a name resolves===
 *
 * `valueOf` is the exact lookup and `parse` the lenient one. Between them they apply four
 * steps, in this order:
 *
 *  1. '''alias substitution''' - the text is replaced by the canonical name the
 *     alternate-name table gives for it, when it gives one. The substitution happens once;
 *     the result is not itself looked up as an alternate name, so alternate names do not
 *     chain.
 *  1. '''exact lookup''' - the result is matched against the keys the members are
 *     registered under. Each member is registered under two: its canonical name, and that
 *     name folded to upper case in the English locale. The match is case sensitive, so for
 *     a member named `Standard` both `Standard` and `STANDARD` resolve while `standard`
 *     does not. `valueOf` stops here.
 *  1. '''lenient rewriting''' - reached by `parse` alone, and only when the two steps above
 *     found nothing. The original text is folded to upper case, then every lenient pattern
 *     is applied in the order the family declared them: a pattern whose expression matches
 *     the whole of the current text replaces that text with its replacement, which may
 *     refer back to captured groups, and the pattern after it sees the replacement rather
 *     than the original. The rewrites therefore compose into a chain.
 *  1. '''repeat lookup''' - steps 1 and 2 are applied once more to the text that survived
 *     the chain.
 *
 * Two consequences of that order are worth stating. An alternate name takes precedence
 * over every lenient pattern, because step 1 is reached before step 3 and `parse` returns
 * as soon as the exact lookup succeeds. And a family that declares no lenient pattern still
 * gets something from `parse`: the fold to upper case in step 3 survives, so `parse` on such
 * a family is an exact lookup that tolerates the case of its input, which is what makes it
 * resolve `standard` where `valueOf` does not.
 *
 * Both operations are total. `valueOf` answers with an `Option`, and `parse` answers with a
 * `Failure.Parsing` on the left of an `EitherNec` where the type being ported raised an
 * error.
 *
 * ===Names that collide===
 *
 * Two members of one family can only collide over a key if one of their names is the
 * upper-case form of the other, and a collision is not reported: the member earlier in
 * declaration order keeps the key, exactly as the merge of the previous registry did. The
 * later member is then unreachable by name, and absent from `byCanonicalName`. Rather than
 * being validated here, this is ruled out for every family by the closedness specification,
 * which asserts that each member resolves to itself.
 *
 * ===Instances===
 *
 * The typeclass instances of a named family all derive from its names, so they are the same
 * for every family and are built once here rather than written out by each: `orderByName`
 * gives the ordering, which is also the hashing and hence the equality, `hashByName` gives
 * the hashing alone for a family that should not be ordered, and `showByName` gives the
 * rendering. They are plain methods rather than instances published from here: a family
 * declares the one it wants in its own companion, so that summoning an instance for that
 * family finds exactly one. Ordering by name agrees with equality by name - `compare` is zero exactly when `eqv`
 * holds - while the names of a family are distinct, which is the same condition the
 * closedness specification already enforces.
 *
 * @tparam A  the type of the named values of the family
 */
trait NamedEnum[A <: Named] {

  /**
   * The name of the family, as it appears when the family rejects text.
   *
   * This is a label rather than a derived value: it is supplied when the instance is built
   * and is never recovered from the type of the members. A family that supplies none is
   * labelled `NamedEnum`, so the label is never empty.
   *
   * @return the name of the family
   */
  def familyName: String

  /**
   * The members of the family, in declaration order.
   *
   * The order is the one the family listed, not the order of the names, and it is the order
   * in which the members claim their lookup keys. A family has at least one member, which
   * is what the type of this member states.
   *
   * @return the members of the family
   */
  def values: NonEmptyList[A]

  /**
   * Obtains the member of the family that the specified name identifies, if one does.
   *
   * This is the exact lookup: the alternate-name table is consulted once, and the resulting
   * name is matched against the canonical names of the members and against those names
   * folded to upper case. No lenient pattern is applied, so text that differs from a name
   * in any way other than its case resolves only if an alternate name says it should.
   *
   * @param name  the name to look up
   * @return the member with that name, or `None` when no member has it
   */
  def valueOf(name: String): Option[A]

  /**
   * Parses text into a member of the family, applying the leniency that the family declares.
   *
   * The exact lookup of `valueOf` is tried first, so an alternate name is honoured ahead of
   * any rewrite. Failing that, the text is folded to upper case, the lenient patterns are
   * applied in order, and the exact lookup is tried once more on the result.
   *
   * Text that names no member is reported rather than raised, as a single `Failure.Parsing`
   * whose message names the family and the text it rejected. The failure deliberately
   * carries no attributes, so that two failures from the same family over the same text are
   * equal and can be compared directly.
   *
   * @param name  the text to parse
   * @return the member the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, A]

  /**
   * The alternate spellings of the family, each mapped to a canonical name.
   *
   * The map is the expanded one, not the table as the family supplied it: alongside each
   * supplied spelling it holds that spelling folded to upper case, unless the family
   * already supplied a spelling identical to it. The supplied spellings are therefore
   * authoritative and the folded ones only fill the gaps, which is what lets an alternate
   * name be reached from `parse` - where the text has already been folded to upper case -
   * as well as from `valueOf`.
   *
   * @return the alternate spellings mapped to canonical names
   */
  def alternateNames: Map[String, String]

  /**
   * The members of the family keyed by their canonical name folded to upper case.
   *
   * This is the second of the two keys every member is registered under, and it is what
   * makes the exact lookup insensitive to the case of a name whose canonical form is mixed
   * case. A member whose canonical name is already upper case is registered under that one
   * key and appears here under it.
   *
   * @return the members keyed by upper-case name
   */
  def byUpperName: Map[String, A]

  /**
   * The members of the family keyed by their canonical name.
   *
   * This is the normalised view of the family: every key is a name exactly as its member
   * renders it, which is what makes the map usable for writing a name out as well as for
   * reading one in. A member that lost both of its keys to an earlier member is absent.
   *
   * @return the members keyed by canonical name
   */
  def byCanonicalName: Map[String, A]

  /**
   * The names of the groups of external spellings that the family publishes.
   *
   * @return the names of the groups
   */
  def externalNameGroups: Set[String]

  /**
   * Obtains the members that the specified group of external spellings identifies.
   *
   * The group is the name the family gave the table. The map returned is keyed by the
   * external spelling, and each spelling is mapped to the member that its canonical name
   * identifies, resolved through the same alias-aware exact lookup as `valueOf`. A row
   * whose canonical name identifies no member is omitted, so this map can be smaller than
   * the table `externalNamesRaw` returns; comparing the two key sets is how the closedness
   * specification detects a row that names a member the family does not have.
   *
   * @param group  the name of the group of external spellings
   * @return the members keyed by their external spelling, or `None` when the family
   *   publishes no such group
   */
  def externalNames(group: String): Option[Map[String, A]]

  /**
   * Obtains the specified group of external spellings as the family supplied it.
   *
   * The map returned is keyed by the external spelling and holds canonical names as text,
   * unresolved and unaltered - external tables are used verbatim, and are not expanded with
   * upper-case spellings the way the alternate-name table is.
   *
   * @param group  the name of the group of external spellings
   * @return the canonical names keyed by their external spelling, or `None` when the family
   *   publishes no such group
   */
  def externalNamesRaw(group: String): Option[Map[String, String]]

  /**
   * The lenient rewrites of the family, in the order they are applied.
   *
   * The list is the one the family supplied, so that it can be compared against the table
   * the family declares. The expressions actually used by `parse` are copies of these
   * made insensitive to case, which is how the previous mechanism compiled them.
   *
   * @return the lenient rewrites, each an expression and the replacement for it
   */
  def lenientPatterns: List[(Regex, String)]

  /**
   * Renders this lookup as the family it belongs to.
   *
   * @return the description of this lookup
   */
  override def toString: String
}

/**
 * Provides the means to build the name lookup of a closed family, together with the
 * name-derived typeclass instances that every named family shares.
 */
object NamedEnum {

  /** The label given to a family that supplies none of its own. */
  private val DefaultFamilyName: String = "NamedEnum"

  /** The inline flag that makes an expression insensitive to case. */
  private val CaseInsensitiveFlag: String = "(?i)"

  /**
   * Summons the name lookup of a family.
   *
   * {{{
   * NamedEnum[Sample].valueOf("Standard")
   * }}}
   *
   * @param ev  the name lookup published by the companion of the family
   * @tparam A  the type of the named values of the family
   * @return the name lookup of the family
   */
  def apply[A <: Named](implicit ev: NamedEnum[A]): NamedEnum[A] = ev

  /**
   * Obtains the name lookup of a family from its members and its three tables.
   *
   * Every parameter after the members has a default, so a family that declares no table
   * writes only `NamedEnum.of(values)`, and one that declares some may name the rest:
   *
   * {{{
   * NamedEnum.of(values = NonEmptyList.of(Standard), familyName = "Sample")
   * }}}
   *
   * The tables are used as described on the type: the alternate names are expanded with
   * their upper-case spellings, the lenient rewrites are copied into expressions
   * insensitive to case, and the external groups are held verbatim. Nothing is validated,
   * because nothing here can fail: a member that collides with an earlier one loses its
   * key, and an external row that names no member is omitted from the resolved group.
   *
   * @param values  the members of the family, in declaration order
   * @param alternates  the alternate spellings, each mapped to a canonical name
   * @param lenient  the lenient rewrites, in the order they are applied, each an
   *   expression matched against the whole of the text and the replacement for it
   * @param externals  the groups of external spellings, each group mapping an external
   *   spelling to a canonical name
   * @param familyName  the name of the family as it appears when the family rejects text,
   *   defaulting to a generic label
   * @tparam A  the type of the named values of the family
   * @return the name lookup of the family
   */
  def of[A <: Named](
      values: NonEmptyList[A],
      alternates: Map[String, String] = Map.empty,
      lenient: List[(Regex, String)] = Nil,
      externals: Map[String, Map[String, String]] = Map.empty,
      familyName: String = ""): NamedEnum[A] =
    new Impl[A](values, alternates, lenient, externals, familyName)

  /**
   * The ordering of named values by name, which is also their hashing and their equality.
   *
   * `Order` and `Hash` both extend `Eq`, so a family publishing this single value as its
   * instance cannot end up with two notions of equality that disagree. Comparison is that
   * of the names as text, which is the comparison the ported types performed, and equality
   * follows it: `compare` is zero exactly when the names are equal.
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
   * The hashing of named values by name, which is also their equality.
   *
   * This is the instance for a family whose members carry no meaningful order, and it
   * agrees with `orderByName` on equality and on hashing.
   *
   * @tparam A  the type of the named values
   * @return the hashing of named values by name
   */
  def hashByName[A <: Named]: Hash[A] =
    new Hash[A] {
      override def eqv(x: A, y: A): Boolean = x.name == y.name

      override def hash(x: A): Int = x.name.hashCode
    }

  /**
   * The rendering of named values as their name.
   *
   * A named value renders as its canonical name and as nothing else, which is the text
   * form the ported types produced and the form a name-keyed encoding writes.
   *
   * @tparam A  the type of the named values
   * @return the rendering of a named value as its name
   */
  def showByName[A <: Named]: Show[A] =
    new Show[A] {
      override def show(value: A): String = value.name
    }

  /**
   * The name lookup of a family, built over the members and tables it declared.
   *
   * Every table derived from those inputs is computed once, by a `lazy val`: these lookups
   * sit on the hot path of every name resolution in the library, so none of them may be
   * rebuilt per call, and deferring them also keeps a companion that defines its members
   * and its lookup side by side safe to initialize in either order.
   *
   * @param values  the members of the family, in declaration order
   * @param alternates  the alternate spellings as supplied, before expansion
   * @param lenientPatterns  the lenient rewrites as supplied, before they are copied
   * @param externals  the groups of external spellings, held verbatim
   * @param suppliedFamilyName  the label supplied for the family, possibly empty
   * @tparam A  the type of the named values of the family
   */
  private final class Impl[A <: Named](
      val values: NonEmptyList[A],
      alternates: Map[String, String],
      val lenientPatterns: List[(Regex, String)],
      externals: Map[String, Map[String, String]],
      suppliedFamilyName: String) extends NamedEnum[A] {

    override val familyName: String =
      if (suppliedFamilyName.isEmpty) DefaultFamilyName else suppliedFamilyName

    /**
     * The lookup keys of the members, first claimant of each key winning.
     *
     * Each member offers two keys in turn - its canonical name, then that name folded to
     * upper case - and the pairs are formed member by member rather than key by key, so
     * that a member claims both of its keys before the next member claims any. Keeping the
     * first pair for each key then reproduces the registration of the type being ported
     * exactly, including the case where a member named in mixed case shadows a later member
     * named in upper case.
     */
    private lazy val entries: List[(String, A)] =
      values.toList.flatMap { value =>
        List(value.name -> value, value.name.toUpperCase(Locale.ENGLISH) -> value)
      }.distinctBy { case (key, _) => key }

    /** The members keyed by every key they are registered under. */
    private lazy val byName: Map[String, A] = entries.toMap

    /**
     * The alternate spellings, expanded with their upper-case forms.
     *
     * The supplied table is the starting point, so a supplied spelling is never displaced,
     * and each upper-case form is added only where the table does not already hold that
     * exact spelling. The supplied spellings are visited in order of their text: a `Map`
     * has no order of its own, and sorting removes the only way this table could otherwise
     * come out differently from one build to the next. It can only differ from the order
     * the ported type used when two spellings differ from each other in case alone, which
     * no ported table does.
     */
    override lazy val alternateNames: Map[String, String] =
      alternates.toList.sortBy { case (spelling, _) => spelling }.foldLeft(alternates) {
        case (expanded, (spelling, canonicalName)) =>
          val upper = spelling.toUpperCase(Locale.ENGLISH)
          if (expanded.contains(upper)) expanded else expanded.updated(upper, canonicalName)
      }

    /** The registered keys that are already upper case, which are the folded ones. */
    override lazy val byUpperName: Map[String, A] =
      entries.foldLeft(Map.empty[String, A]) { case (acc, (key, value)) =>
        if (key == key.toUpperCase(Locale.ENGLISH)) acc.updated(key, value) else acc
      }

    /**
     * The members keyed by canonical name.
     *
     * The registered keys are split into those that are already a canonical name and those
     * that are not. The first group is taken as it stands, and only then does a member from
     * the second group contribute its canonical name, and only if that name is still free.
     * A member keyed canonically therefore wins over one that is not, which matters when a
     * member's canonical name was claimed by an earlier member: that earlier member holds
     * the name here, and the later one is absent.
     */
    override lazy val byCanonicalName: Map[String, A] = {
      val (canonicallyKeyed, others) = entries.partition { case (key, value) => key == value.name }
      val fromCanonical = canonicallyKeyed.foldLeft(Map.empty[String, A]) {
        case (acc, (key, value)) => acc.updated(key, value)
      }
      others.foldLeft(fromCanonical) { case (acc, (_, value)) =>
        if (acc.contains(value.name)) acc else acc.updated(value.name, value)
      }
    }

    /**
     * The lenient expressions, copied to be insensitive to case.
     *
     * The previous mechanism compiled every lenient expression insensitively, and the
     * expressions depend on it: `parse` has already folded its input to upper case by the
     * time they are applied, so an expression written in mixed case could never match
     * otherwise, and a class of characters written as upper case would never match a lower
     * case letter. The copy is made by prefixing the inline flag to the source of the
     * supplied expression, and is skipped where the source already begins with it.
     */
    private lazy val lenientMatchers: List[(Regex, String)] =
      lenientPatterns.map { case (expression, replacement) =>
        val source = expression.pattern.pattern()
        val insensitive =
          if (source.startsWith(CaseInsensitiveFlag)) source else CaseInsensitiveFlag + source
        (insensitive.r, replacement)
      }

    /**
     * The external groups with their rows resolved to members.
     *
     * Each row's canonical name is resolved through the exact lookup, so a row may point at
     * an alternate spelling as well as at a canonical name. A row that resolves to no
     * member is dropped, which leaves the group smaller than the table it came from.
     */
    private lazy val resolvedExternals: Map[String, Map[String, A]] =
      externals.map { case (group, rows) =>
        group -> rows.flatMap { case (externalSpelling, canonicalName) =>
          valueOf(canonicalName).map(value => externalSpelling -> value)
        }
      }

    override def valueOf(name: String): Option[A] =
      byName.get(alternateNames.getOrElse(name, name))

    override def parse(name: String): EitherNec[Failure, A] =
      valueOf(name)
        .orElse(valueOf(rewriteLeniently(name.toUpperCase(Locale.ENGLISH))))
        .toRight(notFound(name))

    /**
     * Applies every lenient rewrite to the specified text, in order.
     *
     * An expression that matches the whole of the text replaces it, and the expression
     * after it is applied to the replacement, so the rewrites chain. The replacement may
     * refer back to the groups the expression captured.
     *
     * @param name  the text to rewrite, already folded to upper case
     * @return the text that survives every rewrite
     */
    private def rewriteLeniently(name: String): String =
      lenientMatchers.foldLeft(name) { case (current, (expression, replacement)) =>
        if (expression.pattern.matcher(current).matches()) {
          expression.replaceFirstIn(current, replacement)
        } else {
          current
        }
      }

    /**
     * The failure reported for text that names no member.
     *
     * @param name  the text that was rejected, as it was supplied
     * @return the failure naming the family and the text
     */
    private def notFound(name: String): NonEmptyChain[Failure] =
      NonEmptyChain.one(Failure.Parsing(s"$familyName name not found: $name"))

    override def externalNameGroups: Set[String] = externals.keySet

    override def externalNames(group: String): Option[Map[String, A]] =
      resolvedExternals.get(group)

    override def externalNamesRaw(group: String): Option[Map[String, String]] =
      externals.get(group)

    override def toString: String = s"NamedEnum[$familyName]"
  }
}

