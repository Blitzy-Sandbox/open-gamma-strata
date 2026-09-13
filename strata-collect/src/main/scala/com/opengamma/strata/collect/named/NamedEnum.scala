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
 * ===What makes the family closed===
 *
 * The members arrive as a `NonEmptyList` and the three tables arrive as Scala values, held
 * by the lookup and read from nowhere else: no name is resolved by interrogating a class, a
 * class path or a configuration source, and a lookup consults nothing but what its family
 * handed it. What a family can resolve is therefore visible in its companion and settled
 * when that companion is compiled, which is what makes the family closed - the set of
 * resolvable names is the same in every program that links the family.
 *
 * ===What a family declares===
 *
 * A family declares its members and up to three tables over them:
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
 *     found nothing and the text is within the family's `lenientLengthCeiling`. The original
 *     text is folded to upper case, then every lenient pattern is applied in the order the
 *     family declared them: a pattern whose expression matches the whole of the current text
 *     replaces that text with its replacement, which may refer back to captured groups, and
 *     the pattern after it sees the replacement rather than the original. The rewrites
 *     therefore compose into a chain.
 *  1. '''repeat lookup''' - steps 1 and 2 are applied once more to the text that survived
 *     the chain.
 *
 * Steps 1 and 2 are applied to text of any length; steps 3 and 4 are the bounded ones, and
 * the ceiling that bounds them is derived from the family's own data, so no text either
 * exact lookup could resolve is refused for its size.
 *
 * Two consequences of that order are worth stating. An alternate name takes precedence
 * over every lenient pattern, because step 1 is reached before step 3 and `parse` returns
 * as soon as the exact lookup succeeds. And a family that declares no lenient pattern still
 * gets something from `parse`: the fold to upper case in step 3 survives, so `parse` on such
 * a family is an exact lookup that tolerates the case of its input, which is what makes it
 * resolve `standard` where `valueOf` does not.
 *
 * Both operations are total. `valueOf` answers with an `Option`, and `parse` answers with a
 * `Failure.Parsing` on the left of an `EitherNec`, so text that names no member is reported
 * rather than raised.
 *
 * ===Names that collide===
 *
 * Two members of one family can only collide over a key if one of their names is the
 * upper-case form of the other, and the canonical name wins: every member is registered
 * under its own name unconditionally, and the folded keys are added afterwards only where
 * the name space still has room for them. So each member of such a pair resolves to itself
 * by the name it publishes, `byCanonicalName` holds every member of every family, and the
 * only view that can be narrower than the member list is `byUpperName`, where the two
 * members of a pair genuinely share one folded key and the earlier of them keeps it.
 *
 * The order of the two registrations is what settles that: the canonical one is
 * unconditional and the folded one is conditional, so a member can lose its folded key but
 * never the name it publishes. One family of this library declares such a pair, so the
 * precedence is exercised rather than hypothetical.
 *
 * ===Closure on the JVM, not only in the source===
 *
 * A family is closed because its base class is `sealed` and its constructor is private to its
 * package, so no other Scala source can declare a member of it. Neither modifier reaches the
 * class file: Scala 2.13 records `sealed` nowhere in the bytecode, and a constructor private to a
 * package is emitted as public, because the JVM has no equivalent access level. A class compiled
 * against this library by other means could therefore declare itself a member of a family and
 * resolve nothing through this lookup while still being one of its values everywhere the family's
 * type is accepted - which is how a "closed" set of currencies, indices or conventions would
 * regain the dynamic members this port deliberately removed.
 *
 * The lookup cannot see that, since it only ever answers with the members it was given. The
 * closure is therefore completed where the JVM does give a hook: the constructor of the family's
 * own base class, which every subtype must call. A family of this port opens its base class with
 *
 * {{{
 * sealed abstract class Sample private[pkg] (val name: String) extends Named with NoJavaSerialization {
 *   JvmClosure.requireDeclaredMember(this, classOf[Sample])
 * }
 * }}}
 *
 * so an instance whose class was not declared inside the family's own companion cannot be
 * constructed at all, and `values` stays the whole of the family at run time. The mixin alongside
 * it closes the other route around a factory - `java.io.ObjectInputStream`, which the `case
 * object` members would otherwise take part in through the `Serializable` supertype the compiler
 * gives them.
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
   * It is not bounded in length. Two hashed lookups over the text cost one pass over it,
   * which is what the text costs to hold, so an alternate spelling longer than every
   * canonical name of the family resolves here exactly - and it is counted in
   * [[lenientLengthCeiling]], so the lenient stage admits its folded forms too.
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
   * ===The length of text the lenient stage accepts===
   *
   * The first two steps are applied to text of whatever length arrives: the exact lookup is a
   * pair of hashed lookups and costs one pass over the text, so a canonical name, the
   * upper-case form of one and an alternate spelling resolve however long they are. The last
   * two are bounded by [[lenientLengthCeiling]], which is the family's own longest datum plus
   * a margin: text beyond the ceiling is reported with the family's ordinary not-found
   * failure, without the fold to upper case that would copy it and without a single
   * expression being applied to it. Because the ceiling is at least the longest key and the
   * longest alternate spelling the family holds, no text either exact lookup could resolve is
   * ever beyond it.
   *
   * Within the ceiling the length of the text still may not cost more than the text is worth,
   * and that is settled where the expressions are handed over rather than where the text is:
   * each expression is examined once, when the family declares it, for the character every
   * full match of it must end with, and an expression whose character the text does not end
   * with is a match that cannot happen and is not attempted. The condition is a consequence of
   * the expression - it is derived only where the shape of the expression proves it, and no
   * condition at all otherwise - so it decides nothing about which text resolves, and leaves
   * one pass over the expressions costing what a pass over the text costs.
   *
   * ===The text the failure quotes back===
   *
   * The failure names the text it was handed as that text stands, character for character and
   * neither folded nor rewritten: a caller correcting its input is given back exactly what was
   * refused. Text that came from outside the library is consequently inside the failure, and
   * making it safe to write out belongs to the writing: the text form of a failure and
   * [[com.opengamma.strata.collect.result.Failure.show]] bound every part they write and escape
   * anything a line-oriented reader could act on, so a name from outside cannot forge a line of
   * a log or make that line as large as itself.
   *
   * Every family resolves its names through this one operation, so the message is the same
   * shape for each of them rather than for some of them, and a family added later inherits it.
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
   * This is the one view that can hold fewer entries than the family has members: where two
   * members' names differ in case alone they fold to a single key, which the earlier of them
   * keeps unless it is the canonical name of the other, in which case that other holds it.
   *
   * @return the members keyed by upper-case name
   */
  def byUpperName: Map[String, A]

  /**
   * The members of the family keyed by their canonical name.
   *
   * This is the normalised view of the family: every key is a name exactly as its member
   * renders it, which is what makes the map usable for writing a name out as well as for
   * reading one in. Every member appears, because a canonical name is registered
   * unconditionally and so cannot be taken by another member; the map therefore always holds
   * as many entries as the family has members.
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
   * external spelling, and each spelling is mapped to the value that its canonical name
   * identifies. The name is resolved through the alias-aware exact lookup of `valueOf`,
   * unless the family supplied a resolution of its own when it built this lookup, in which
   * case that one is used: a family that layers a second provider over its closed members -
   * a convention parameterised by a calendar, say - carries external rows naming values that
   * `values` does not hold, and the family's own lookup is the only thing that can reach
   * them. A row that resolves either way to nothing is omitted, so this map can be smaller
   * than the table `externalNamesRaw` returns; comparing the two key sets is how the
   * closedness specification detects a row that names a value nothing can reach.
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
   * The lenient rewrites of the family as text, in the order they are applied.
   *
   * This is the raw table: the source of each expression exactly as the family declared it,
   * beside its replacement, which is what a caller comparing the table against the resource
   * it was transcribed from needs. Reading it compiles nothing.
   *
   * @return the lenient rewrites, each the source of an expression and the replacement for it
   */
  def lenientSources: List[(String, String)]

  /**
   * The lenient rewrites of the family as expressions, in the order they are applied.
   *
   * The compiled projection of `lenientSources`, for a caller that wants to examine or apply
   * an expression rather than read its text; the source of each expression here is the row as
   * the family declared it. It is neither the table the family handed over nor the expressions
   * `parse` applies - those are compiled once, from the same sources, to be insensitive to
   * case - so a caller that only needs the rows should read `lenientSources` and compile
   * nothing.
   *
   * @return the lenient rewrites, each an expression and the replacement for it
   */
  def lenientPatterns: List[(Regex, String)]

  /**
   * The greatest length of text the lenient stage of the family is applied to.
   *
   * The ceiling is a constant of the family, derived from the family's own data and from
   * nothing else: the longest key a member is registered under - canonical names and their
   * upper-case forms alike - the longest spelling and the longest target of the expanded
   * alternate-name table, and the longest expression source the family declared, plus a
   * margin. A family with longer names therefore gets a longer ceiling, and one whose
   * expressions are written out at length gets a ceiling that covers them.
   *
   * Both sides of the alternate-name table count, because the text that survives the rewrites
   * is looked up through that table as well: a rewrite may produce an alternate spelling, and
   * a table row may name a value the family's own `values` does not hold, in which case its
   * target is longer than any key. Taking the greatest length of all four sources therefore
   * bounds the text that either of the two lookups could still resolve, which is what makes
   * the ceiling safe to apply before the rewrites rather than after them.
   *
   * The property that matters of it is one a specification can state: the ceiling of a family
   * can never refuse text that family could resolve. It is published for exactly that reason -
   * a caller has no need of it, and the two operations it bounds, `parse` and
   * `rewriteLeniently`, apply it themselves.
   *
   * @return the greatest length of text the rewrites of this family are applied to
   */
  def lenientLengthCeiling: Int

  /**
   * Applies every lenient rewrite of the family to the specified text, in order.
   *
   * This is the third step of `parse`, exposed on its own. An expression whose match covers
   * the whole of the current text replaces that text with its replacement, which may refer
   * back to the groups the expression captured, and the expression after it is applied to
   * what the one before it produced, so the rewrites chain. Text that no expression matches
   * comes back unchanged, and a family that declares no expression is the identity.
   *
   * Text longer than [[lenientLengthCeiling]] comes back unchanged as well, no expression
   * having been applied to it, which is the same bound `parse` applies to its own lenient
   * stage and is applied here for the same reason: the caller this operation exists for runs
   * the chain itself, so the bound has to live with the chain rather than with one of its
   * callers. A family whose ceiling refuses the text is left to answer the text through its
   * own exact lookups, which is exactly what the rewrites coming back empty-handed leaves it
   * to do.
   *
   * The text is expected to have been folded to upper case already, as `parse` folds it: the
   * expressions are applied without regard to case, but a rewrite that produces a canonical
   * name does so from the folded shape of a spelling rather than from any shape of it.
   *
   * It is public for one reason: a family whose name space is wider than its closed members -
   * a convention parameterised by a calendar, say - has to run the chain itself, between its
   * own exact lookup and its own repeat lookup, and running the family's own copy of the
   * table instead would be a second implementation of this algorithm and a second set of
   * compiled expressions to keep in step with it.
   *
   * @param name  the text to rewrite, folded to upper case
   * @return the text that survives every rewrite
   */
  def rewriteLeniently(name: String): String

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
   * The room allowed above the longest text a family knows, bounding its lenient stage.
   *
   * The ceiling a family applies before rewriting text is derived from its own data - the
   * longest lookup key, the longest alternate spelling, the longest alternate target and the
   * longest expression source it holds - and this margin is added to it, so that a spelling
   * longer than anything the family declares is still rewritten as long as it is within reach
   * of one. The tables this library transcribes size the margin: the longest expression source
   * any of them declares is fifty characters and the longest name or alternate spelling any of
   * them realises is under sixty, while the longest spelling a caller can sensibly offer one of
   * those rows - a screaming-snake or spaced form of a name - runs a handful of characters
   * beyond the name itself. Thirty-two characters of room therefore admits every declared row
   * and every spelling of one with room to spare, while leaving the ceiling of every family a
   * small constant.
   */
  private val LenientLengthMargin: Int = 32

  /**
   * The characters of an expression that stand for something other than themselves.
   *
   * Used only to decide whether the tail of an expression source is a plain literal, which
   * is a question about the text of the expression rather than about the language it
   * matches; anything in this set, and anything after a backslash, ends the enquiry with no
   * answer rather than with a guess.
   */
  private val Metacharacters: Set[Char] = Set('.', '\\', '+', '*', '?', '[', ']', '^', '$', '(', ')', '{', '}', '|')

  private val ClassOpen: Char = '['

  private val ClassClose: Char = ']'

  private val ClassNegate: Char = '^'

  private val Escape: Char = '\\'

  private val Alternation: Char = '|'

  /**
   * Opens a construct that can change how the rest of an expression is read.
   *
   * Every inline flag group, every non-capturing and every look-around group begins this way,
   * and one of those flags - the one that turns comments on - makes the tail of a source stop
   * being part of the expression at all. A source holding any of them is therefore not read
   * further.
   */
  private val GroupWithMeaning: String = "(?"

  private val QuoteOpen: String = "\\Q"

  private val SingleCharacterClassLength: Int = 3

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

    new Impl[A](
      values,
      alternates,
      lenient.map { case (expression, replacement) => (expression.pattern.pattern(), replacement) },
      externals,
      familyName,
      None)

  /**
   * Obtains the name lookup of a family whose lenient rewrites are handed over as text.
   *
   * The same lookup as [[of]] in every respect but two, and the form a family of this library
   * uses.
   *
   * The rewrites arrive as the '''source''' of each expression rather than as a compiled
   * expression. An expression has to be compiled insensitively to case to be applied, so a
   * family that compiles its own table hands over a compiled expression that is then compiled
   * a second time; handing over the source leaves exactly one compiled expression per rule in
   * the program, made when the family first parses a name and not before, so a family whose
   * callers only ever do arithmetic with its members compiles nothing at all.
   *
   * A resolution for the external tables may be supplied. External rows are resolved through
   * the exact lookup of the family by default, which is right for a family whose name space
   * is exactly its members; a family that layers a second provider over them - a convention
   * parameterised by a calendar, say - passes its own lookup here, so that a row naming a
   * value outside `values` resolves rather than being dropped.
   *
   * @param values  the members of the family, in declaration order
   * @param alternates  the alternate spellings, each mapped to a canonical name
   * @param lenient  the lenient rewrites, in the order they are applied, each the source of
   *   an expression matched against the whole of the text and the replacement for it
   * @param externals  the groups of external spellings, each group mapping an external
   *   spelling to a canonical name
   * @param familyName  the name of the family as it appears when the family rejects text,
   *   defaulting to a generic label
   * @param externalTargets  resolves the canonical name of an external row, where the family
   *   needs a wider resolution than its own exact lookup
   * @tparam A  the type of the named values of the family
   * @return the name lookup of the family
   */
  def ofSources[A <: Named](
      values: NonEmptyList[A],
      alternates: Map[String, String] = Map.empty,
      lenient: List[(String, String)] = Nil,
      externals: Map[String, Map[String, String]] = Map.empty,
      familyName: String = "",
      externalTargets: Option[String => Option[A]] = None): NamedEnum[A] =
    new Impl[A](values, alternates, lenient, externals, familyName, externalTargets)

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
   * @param suppliedSources  the lenient rewrites as supplied, each the source of an
   *   expression and the replacement for it
   * @param externals  the groups of external spellings, held verbatim
   * @param suppliedFamilyName  the label supplied for the family, possibly empty
   * @param externalTargets  resolves the canonical name of an external row, where the family
   *   supplied a wider resolution than the exact lookup of this one
   * @tparam A  the type of the named values of the family
   */
  private final class Impl[A <: Named](
      val values: NonEmptyList[A],
      alternates: Map[String, String],
      suppliedSources: List[(String, String)],
      externals: Map[String, Map[String, String]],
      suppliedFamilyName: String,
      externalTargets: Option[String => Option[A]]) extends NamedEnum[A] {

    override val familyName: String =
      if (suppliedFamilyName.isEmpty) DefaultFamilyName else suppliedFamilyName

    /**
     * The members keyed by the name each of them publishes.
     *
     * A canonical name is claimed unconditionally, which is the property every member of
     * every family depends on: the name a member renders is the name that reaches it, whatever
     * the other members of the family are called. Two members sharing one canonical name would
     * be a family with two identities for one name, which the closedness specification rules
     * out; were it to happen the earlier would keep the name, and this fold rather than a
     * conversion to a map is what makes that so.
     */
    private lazy val canonicalEntries: Map[String, A] =
      values.toList.foldLeft(Map.empty[String, A]) {
        case (claimed, value) =>
          if (claimed.contains(value.name)) claimed else claimed.updated(value.name, value)
      }

    /**
     * The members keyed by every key they are registered under.
     *
     * The canonical names above, and then the folded spelling of each member's name added
     * only where the name space has room for it - so a folded key is taken by the member
     * whose canonical name it is if there is one, and otherwise by the first member to offer
     * it. The two registrations are ordered, the canonical one unconditional and the folded
     * one conditional, which is the reason a member can lose its folded key but never its own
     * name.
     */
    private lazy val byName: Map[String, A] =
      values.toList.foldLeft(canonicalEntries) {
        case (claimed, value) =>
          val folded = value.name.toUpperCase(Locale.ENGLISH)
          if (claimed.contains(folded)) claimed else claimed.updated(folded, value)
      }

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

    /**
     * The registered keys that are already upper case, which are the folded ones.
     *
     * A folded key is held by the member whose canonical name it is where the family has
     * one, and by the first member to offer it otherwise, so this view has one entry per
     * distinct folded name and can therefore be smaller than the member list.
     */
    override lazy val byUpperName: Map[String, A] =
      byName.filter { case (key, _) => key == key.toUpperCase(Locale.ENGLISH) }

    /**
     * The members keyed by canonical name.
     *
     * This is the canonical registration exactly: a name is claimed by its own member and by
     * nothing else, so every member of the family appears here under the name it renders.
     */
    override lazy val byCanonicalName: Map[String, A] = canonicalEntries

    /**
     * The lenient rewrites, each compiled once and examined once.
     *
     * The compiled expression of a rule is made insensitive to case, and the rows depend on
     * that: `parse` has folded its input to upper case by the time a rule is applied, so an
     * expression whose row is written in mixed case could never match otherwise, and a class
     * of characters written as upper case would never match a lower-case letter. One
     * expression per rule exists in the program, made here from the source the family handed
     * over and used by every parse thereafter.
     */
    private lazy val lenientRules: List[LenientRule] =
      suppliedSources.map { case (source, replacement) => new LenientRule(source, replacement) }

    override def lenientSources: List[(String, String)] = suppliedSources

    /**
     * The lenient rewrites as expressions, for a caller that wants to apply one.
     *
     * Compiled from the same sources as the rules and without the inline flag the rules
     * carry, so that the source of each expression here is the row as the family declared
     * it. Nothing in the resolution path reads this and neither does the raw view above,
     * which is why it is derived lazily: a caller that reads the rows rather than the
     * expressions - every table comparison in this library - compiles nothing at all.
     */
    override lazy val lenientPatterns: List[(Regex, String)] =
      suppliedSources.map { case (source, replacement) => (source.r, replacement) }

    /**
     * The greatest length of text this family hands to its lenient rewrites.
     *
     * The longest text the family could plausibly be asked to rewrite, taken from the
     * family's own data and nothing else: the longest key a member is registered under, the
     * longest spelling and the longest target of the expanded alternate-name table, and the
     * longest expression source the family declared, plus
     * [[NamedEnum.LenientLengthMargin]]. The sources are read as the family supplied them
     * rather than through `lenientPatterns`, so computing the ceiling compiles no expression.
     *
     * Computed once, like every other table derived here: the value is a constant of the
     * family and is read on the miss path of every parse.
     */
    override lazy val lenientLengthCeiling: Int = {
      val keyLengths = byName.iterator.map { case (key, _) => key.length }
      val aliasLengths = alternateNames.iterator.flatMap { case (spelling, canonicalName) =>
        Iterator(spelling.length, canonicalName.length)
      }
      val sourceLengths = suppliedSources.iterator.map { case (source, _) => source.length }
      val longest = (keyLengths ++ aliasLengths ++ sourceLengths)
        .foldLeft(0)((widest, length) => math.max(widest, length))
      longest + LenientLengthMargin
    }

    /**
     * The external groups with their rows resolved to values.
     *
     * Each row's canonical name is resolved through the resolution the family supplied, or
     * through the exact lookup of this one where it supplied none - so a row may point at an
     * alternate spelling as well as at a canonical name, and a family whose own lookup is
     * wider than its closed members resolves a row naming one of those wider values. A row
     * that resolves to nothing is dropped, which leaves the group smaller than the table it
     * came from.
     */
    private lazy val resolvedExternals: Map[String, Map[String, A]] = {
      val resolve: String => Option[A] = externalTargets.getOrElse(canonicalName => valueOf(canonicalName))
      externals.map { case (group, rows) =>
        group -> rows.flatMap { case (externalSpelling, canonicalName) =>
          resolve(canonicalName).map(value => externalSpelling -> value)
        }
      }
    }

    override def valueOf(name: String): Option[A] =
      byName.get(alternateNames.getOrElse(name, name))

    /**
     * Parses text into a member, applying the leniency the family declares.
     *
     * The exact lookup runs first and unbounded, so an alternate spelling and a canonical
     * name of any length resolve as they always did. Only when it misses is the lenient stage
     * reached: the text is folded to upper case, every rewrite is applied to it in the order
     * the family declared them, and the exact lookup is tried once more on what survives.
     * This is the lenient lookup of the type being ported, step for step, bounded by
     * [[lenientLengthCeiling]] - text beyond the ceiling is reported with the failure the
     * rewrites would have reported for it, without the fold to upper case that would copy it
     * and without a single expression being applied to it.
     *
     * ===Why the lenient stage is bounded and the exact lookup is not===
     *
     * The rewrites are the only part of this algorithm whose cost is a function of the length
     * of the text rather than of the size of the family, and an expression that consumes text
     * of unbounded length - a greedy or repeated group followed by a literal, say - can make
     * that cost grow faster than the text does. Bounding the text the rewrites see makes the
     * cost of rejecting a name a constant of the family, and bounding it here, where the fold
     * to upper case would otherwise copy the whole of it, removes the copy as well.
     *
     * The ceiling is derived from the family's own data - its longest key, its longest
     * alternate spelling, its longest alternate target, its longest expression source, plus a
     * margin - so a family with longer names is given a longer ceiling, and no text that
     * either exact lookup could resolve is ever beyond it. What it narrows is therefore one
     * deliberate case: an expression that rewrites arbitrarily long text into the name of a
     * member, handed text longer than the family's ceiling, now reports that text instead of
     * rewriting it.
     *
     * Three facts place that narrowing. The rewrites are reached only after the alias-aware
     * exact lookup has missed, so nothing a family resolves exactly depends on them. The
     * lenient lookup of the type being ported applied its configured expressions to text of
     * any length in exactly the same way, so the exposure is inherited here rather than
     * introduced. And no table this library transcribes declares such an expression: every
     * source in them is anchored to a literal shape of a fixed size, which is why the ceiling
     * closes the case while it is still unrealised rather than after a family realises it.
     *
     * A full pass over the expressions for text within the ceiling is not narrowed and is not
     * meant to be: it is the cost the ported algorithm has, and the order of the pass is
     * behaviour. What bounds that pass is the requirement each rule reads from its own
     * expression - see [[LenientRule]] - which decides nothing about which text resolves.
     *
     * The comparison is written into the lookup rather than left to `rewriteLeniently`, which
     * applies the same ceiling itself: the fold to upper case happens between the two, so a
     * ceiling applied only inside the chain would copy the text before refusing it.
     *
     * @param name  the text to parse
     * @return the member the text names, or the failure describing why it names none
     */
    override def parse(name: String): EitherNec[Failure, A] =
      valueOf(name)
        .orElse(
          if (name.length > lenientLengthCeiling) None
          else valueOf(rewriteLeniently(name.toUpperCase(Locale.ENGLISH))))
        .toRight(notFound(name))

    override def rewriteLeniently(name: String): String =
      if (name.length > lenientLengthCeiling) {
        name
      } else {
        lenientRules.foldLeft(name)((current, rule) => rule.rewrite(current))
      }

    /**
     * The failure reported for text that names no member.
     *
     * The message is the name of the family, then the text as it was supplied, so the failure
     * names exactly what was rejected. Bounding that text and escaping what it may hold is the
     * business of writing a failure out, which [[Failure.show]] and the text form of a failure
     * do for every part they write.
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

  /**
   * One lenient rewrite of a family: an expression, its replacement, and what it needs of
   * the text before it is worth applying.
   *
   * ===The expression===
   *
   * Compiled once, here, from the source the family declared, with the inline flag that
   * makes it insensitive to case - prefixed unless the source already carries it. The rows
   * depend on that flag: the text a rule sees has been folded to upper case, so a row
   * written in mixed case would never match without it.
   *
   * ===What it needs of the text===
   *
   * A rewrite is applied with a whole-text match, so every character of the text takes part
   * in it, and the cost of deciding a match is therefore a function of the length of the
   * text. For most expressions that cost is one pass; for an expression holding a group that
   * can consume text of unbounded length it is a pass per position the group could end at,
   * which is a cost that grows faster than the text does. Text arriving from outside the
   * library reaches this - a name to parse, a name in a document being read - and while
   * `lenientLengthCeiling` bounds how much of it a rule is ever handed, that difference is
   * the difference between one pass over a name and a pass per character of it, which is
   * work a family declaring several dozen rules does several dozen times.
   *
   * It is closed by asking of the expression, once, a question about the language it
   * matches: which character must a full match end with? Where the tail of the source is a
   * plain literal, or a class holding exactly one character, the answer is that character
   * and every full match ends with it. Where the tail is anything else - a group, a
   * quantifier, a class of several characters, an anchor, an escape - there is no answer and
   * none is guessed. An expression holding an alternation is not asked at all, since either
   * branch may end the match.
   *
   * Text that does not end with a character the expression requires cannot match it, so
   * declining to run the expression over such text removes no match: the two spellings of
   * this rule agree on every input, and the specification of this port - which fixes the
   * lenient algorithm as that of the type being ported - is satisfied either way. What
   * changes is only the work: the one expression among the transcribed tables that can
   * consume unbounded text, `(.*)[(](.*)[)]`, requires a closing bracket at the end, so text
   * crafted to make it backtrack is declined on its last character rather than matched. The
   * requirement and the ceiling are independent of each other and both are kept: the ceiling
   * decides how much text a rule may be handed, this requirement decides which rules are
   * worth handing it to, and only the second of the two leaves every answer untouched by
   * construction.
   *
   * The comparison of characters ignores case, because the expression does. A source that
   * turns case sensitivity off again is therefore tested more weakly than it needs to be,
   * which costs a match that the expression then declines itself and cannot admit a match it
   * would have refused.
   *
   * @param source  the source of the expression, as the family declared it
   * @param replacement  the replacement for text this rule matches, which may refer back to
   *   the groups the expression captured
   */
  private final class LenientRule(source: String, replacement: String) {

    private val expression: Regex =
      (if (source.startsWith(CaseInsensitiveFlag)) source else CaseInsensitiveFlag + source).r

    private val requiredFinalCharacter: Option[Char] = finalCharacterOf(source)

    /**
     * Applies this rule to the specified text, where it matches the whole of it.
     *
     * One matcher serves both purposes: it decides whether the rule applies and then performs
     * the replacement. Asking the expression to replace within the text a second time would
     * match that text a second time, which is a scan and an allocation this needs not - a
     * matcher resets itself when it replaces, so the replacement sees the whole of the text
     * exactly as the test did.
     *
     * @param current  the text as the rules before this one left it
     * @return the replacement where this rule matches the whole of the text, the text
     *   unchanged otherwise
     */
    def rewrite(current: String): String =
      if (!couldMatch(current)) {
        current
      } else {
        val matcher = expression.pattern.matcher(current)
        if (matcher.matches()) matcher.replaceFirst(replacement) else current
      }

    /**
     * Whether the text satisfies what this rule requires of its final character.
     *
     * A rule that requires nothing could match anything and is always applied.
     *
     * @param text  the text a match is being considered for
     * @return false only where a full match is impossible
     */
    private def couldMatch(text: String): Boolean =
      requiredFinalCharacter.forall(required =>
        text.nonEmpty && equalIgnoringCase(text.charAt(text.length - 1), required))
  }

  /**
   * The character every full match of the specified expression source must end with, where
   * the source proves one.
   *
   * Read from the text of the source and nothing else, and conservative at every turn: an
   * answer is returned only where the shape of the source proves it, so a source this reader
   * does not understand yields no answer rather than a wrong one.
   *
   * Four shapes stop the reading before it begins, each because it can make the tail of a
   * source mean something other than what it spells:
   *
   *  - an '''alternation''', because a match may take either branch and only one of them ends
   *    the source;
   *  - an '''inline construct''' `(?...`, which covers every flag group, non-capturing group
   *    and look-around. The flag that matters most is the one turning comments on: under it
   *    `A # X` matches `A`, the `# X` being a comment, so reading `X` as required would refuse
   *    text the expression accepts. Rather than track which flags are in force at which
   *    position, a source holding any such construct is left without a requirement;
   *  - a '''quoted run''' `\Q`, inside which no character means what it otherwise would;
   *  - anything the two readers below decline.
   *
   * Only a leading `(?i)` is exempt, because that is the flag this lookup prefixes to a source
   * itself and it changes nothing about how the source is read.
   *
   * @param source  the source of an expression
   * @return the character a full match must end with, where the source proves one
   */
  private def finalCharacterOf(source: String): Option[Char] = {
    val body =
      if (source.startsWith(CaseInsensitiveFlag)) source.substring(CaseInsensitiveFlag.length) else source
    if (body.isEmpty || body.contains(Alternation) || body.contains(GroupWithMeaning) ||
      body.contains(QuoteOpen)) {
      None
    } else if (body.charAt(body.length - 1) == ClassClose) {
      singleCharacterClassOf(body)
    } else {
      finalLiteralOf(body)
    }
  }

  /**
   * The character of a class of exactly one character closing the specified source.
   *
   * The shape read is three characters: an unescaped opening bracket, one character that is
   * neither the negation nor an escape, and the closing bracket. A class of any other shape,
   * and a closing bracket that is part of something else, yields no answer. The negation is
   * excluded because a negated class of one character matches every character except that
   * one, which makes its single character say nothing useful about how a match ends.
   *
   * @param body  the source, without any leading inline flag
   * @return the single character of the closing class, where that is what closes the source
   */
  private def singleCharacterClassOf(body: String): Option[Char] =
    if (body.length < SingleCharacterClassLength) {
      None
    } else {
      val open = body.length - SingleCharacterClassLength
      val inner = body.charAt(body.length - 2)
      if (body.charAt(open) == ClassOpen && !isEscaped(body, open) && inner != ClassNegate && inner != Escape) {
        Some(inner)
      } else {
        None
      }
    }

  /**
   * The literal character closing the specified source.
   *
   * A character that stands for itself, is not escaped, and carries no quantifier - a
   * quantifier follows the character it applies to, so a source whose last character is a
   * plain literal has none applying to that literal.
   *
   * @param body  the source, without any leading inline flag
   * @return the closing character, where it stands for itself
   */
  private def finalLiteralOf(body: String): Option[Char] = {
    val last = body.charAt(body.length - 1)
    if (Metacharacters.contains(last) || isEscaped(body, body.length - 1)) None else Some(last)
  }

  /**
   * Whether the character at the specified position of a source is escaped.
   *
   * An odd number of escapes before a character escapes it; an even number of them is that
   * many literal escapes, leaving the character itself unescaped.
   *
   * @param body  the source
   * @param index  the position of the character
   * @return whether the character stands for something other than itself
   */
  private def isEscaped(body: String, index: Int): Boolean =
    body.substring(0, index).reverseIterator.takeWhile(character => character == Escape).size % 2 == 1

  /**
   * Whether two characters are the same character, disregarding their case.
   *
   * Folded both ways, since a single fold is not enough for every character a name can hold.
   *
   * @param left  one character
   * @param right  the other character
   * @return whether the two are the same character in some case
   */
  private def equalIgnoringCase(left: Char, right: Char): Boolean =
    left == right ||
      Character.toUpperCase(left) == Character.toUpperCase(right) ||
      Character.toLowerCase(left) == Character.toLowerCase(right)
}
