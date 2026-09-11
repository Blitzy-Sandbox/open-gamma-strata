/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.immutable.ListMap
import scala.collection.immutable.Map
import scala.collection.immutable.SortedMap

import cats.Order
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import com.opengamma.strata.collect.result.Failure

/**
 * The handful of collection helpers this port keeps, and the record of the many it does not.
 *
 * Two static helper classes are being ported here: one a set of stream collectors and small
 * collection utilities, the other a view over the entries of a map. Between them they run to
 * some two and a half thousand lines, and almost none of it has a counterpart in this file.
 * That is the point of this file rather than an omission from it. Those classes existed to
 * give a language without an immutable collection library, without a sum type and without an
 * accumulating applicative the operations it lacked; the language this port targets has all
 * three, so what those classes supplied by hand is now either in the standard library, in
 * `cats`, or expressible at the call site in one expression. Only the helpers that are
 * genuinely still missing are ported, and the list of what is deliberately absent is part of
 * the contract - it is what tells a later port that reaching for one of those members is a
 * sign the call site should be rewritten, not that this file is incomplete.
 *
 * What remains is four groups:
 *
 *   - `ensureOnlyOne`, for reading the single element that a collection is required to hold;
 *   - `toSortedMap`, for building a sorted map by extracting a key, and optionally a value,
 *     from each element;
 *   - `groupByPreservingOrder`, for grouping elements by key with the order of the input kept;
 *   - the chain helpers, for building a non-empty chain from a collection that may be empty
 *     and for concatenating chains.
 *
 * ===A failure is returned, never thrown===
 *
 * Both conditions that these helpers can report - a collection that holds more than the one
 * element expected of it, and two elements that produce the same key - depend on the data the
 * caller supplied rather than on the caller having broken a contract. Under the convention of
 * this library that makes them data-dependent failures, so they are returned as a
 * [[com.opengamma.strata.collect.result.Failure Failure]] on the left of an `Either` and the
 * caller decides what to do about them. The originals threw `IllegalArgumentException` from
 * inside a collector, which left the caller of a stream pipeline with nothing to inspect and
 * no way to accumulate the problem alongside others; the message text is reproduced word for
 * word so that logs and expectations carry over, but the shape is not.
 *
 * The helpers that cannot fail - grouping, and the chain helpers - return their result
 * directly, and the merging form of `toSortedMap` is total in the same way as the merging
 * collector it replaces.
 *
 * ===Ordering===
 *
 * Every member that produces a sorted map takes a `cats.Order` for the key type and derives
 * the standard library ordering from it, rather than taking the ordering itself. That follows
 * the convention of this port: a type that has an order declares a single `Order` instance in
 * its companion, and the Java comparison interface that the original relied on is not part of
 * this API. A key type that has only a standard library ordering reaches these members
 * through `cats.Order.fromOrdering`.
 *
 * ===What has no counterpart here===
 *
 * The members of the two classes that the dependent module uses, and what each becomes:
 *
 *   - the whole entry-view type, whose every use is `of(map)` followed by key or value
 *     filtering, mapping, iteration or collection back into a map - all of them operations
 *     that `scala.collection.immutable.Map` already has, so there is nothing to port;
 *   - `toImmutableList`, `toImmutableSet` and `toImmutableSortedSet`, which become `toList`,
 *     `toSet` and `to(SortedSet)` on any collection or iterator;
 *   - `stream` and `list`, which become `iterator` and `toList`;
 *   - `inOptional` and `filteringOptional`, which become `map` over an `Option` and `flatten`
 *     over a collection of them;
 *   - `tryCatchToOptional`, which becomes `scala.util.Try(...).toOption`;
 *   - the multimap collectors, whose one use in the dependent module is the ordered grouping
 *     that `groupByPreservingOrder` performs;
 *   - `only` and `toOnly`, which returned an empty optional both for an empty input and for
 *     an input with several elements. `ensureOnlyOne` distinguishes the two, which is what
 *     every call site of them in this port needs.
 *
 * Everything else in either class is not used by the module being ported and has no member
 * here. A later port that needs one adds it to this file, rather than reaching back into the
 * original.
 *
 * ===Thread safety===
 *
 * Every member is a pure function of its arguments and this object holds no state, so it is
 * safe to use from any number of threads. A member that takes an `IterableOnce` consumes it
 * once and never retains it, so a caller that passes an iterator must not use that iterator
 * afterwards - the usual contract of a single-use collection.
 *
 * @see [[com.opengamma.strata.collect.result.Failure Failure]] for the failure these helpers report
 */
object Collections {

  //-------------------------------------------------------------------------
  /**
   * Returns the single element of the specified collection, failing if it holds more than one.
   *
   * The three outcomes are distinguished, which is the whole purpose of the member:
   *
   *   - an empty collection produces `Right(None)` - there is no element, and that is not by
   *     itself an error;
   *   - a collection of exactly one element produces `Right(Some(element))`;
   *   - a collection of two or more elements produces a `Left`, naming the first two elements
   *     found so that the message says what was actually in the collection.
   *
   * A caller that requires an element to be present combines the two right-hand cases in the
   * shape it wants, which keeps the decision with the caller:
   *
   * {{{
   * // exactly one, reported as a failure when absent
   * Collections.ensureOnlyOne(candidates).flatMap {
   *   case Some(value) => Right(value)
   *   case None => Left(Failure.Invalid("No candidate was found"))
   * }
   * }}}
   *
   * The element type is unconstrained, so two elements that are equal to one another still
   * count as two: this member checks how many elements there are and deliberately does not
   * de-duplicate them. The call site being ported de-duplicated first, and reads as such -
   * the distinct currencies of a list of amounts having to number exactly one:
   *
   * {{{
   * Collections.ensureOnlyOne(amounts.iterator.map(_.currency).distinct)
   * }}}
   *
   * At most two elements are ever read, so this is safe to apply to a long or expensively
   * computed collection: the traversal stops as soon as a second element proves the check
   * has failed.
   *
   * @tparam A  the type of the elements
   * @param items  the collection to examine, consumed once
   * @return the only element, or none if the collection was empty, or a failure if it held
   *   more than one element
   */
  def ensureOnlyOne[A](items: IterableOnce[A]): Either[Failure, Option[A]] =
    onlyOne(items)((first, second) =>
      s"Multiple values found where only one was expected: $first and $second")

  /**
   * Returns the single element of the specified collection, failing with the specified message
   * if it holds more than one.
   *
   * This behaves exactly as the single-argument form and differs only in the message that a
   * failure carries, which is supplied by the caller where the wording of the default says
   * less than the context could:
   *
   * {{{
   * Collections.ensureOnlyOne(matches, s"More than one convention matches '$name'")
   * }}}
   *
   * The message is taken by name, so the text - and any value interpolated into it - is built
   * only if the collection turns out to hold more than one element. That replaces the message
   * template and its array of arguments that the original took: interpolation says the same
   * thing, costs nothing when the check passes, and is checked by the compiler.
   *
   * @tparam A  the type of the elements
   * @param items  the collection to examine, consumed once
   * @param message  the message the failure carries, evaluated only if the check fails
   * @return the only element, or none if the collection was empty, or a failure if it held
   *   more than one element
   */
  def ensureOnlyOne[A](items: IterableOnce[A], message: => String): Either[Failure, Option[A]] =
    onlyOne(items)((_, _) => message)

  /**
   * Reads at most the first two elements of a collection and decides between the three
   * outcomes.
   *
   * Both public forms are written in terms of this method, so they cannot drift apart. The
   * message is a function of the two offending elements rather than a string, which is what
   * lets the form with a caller-supplied message ignore them while the default form uses
   * them, and which keeps either message unbuilt unless a second element is actually found.
   *
   * @tparam A  the type of the elements
   * @param items  the collection to examine, consumed once
   * @param message  builds the failure message from the first two elements found
   * @return the only element, or none, or a failure
   */
  private def onlyOne[A](items: IterableOnce[A])(message: (A, A) => String): Either[Failure, Option[A]] = {
    val remaining = items.iterator
    if (!remaining.hasNext) {
      Right(None)
    } else {
      val first = remaining.next()
      if (!remaining.hasNext) {
        Right(Some(first))
      } else {
        Left(Failure.Invalid(message(first, remaining.next())))
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a sorted map from the specified collection, keyed by the result of the specified
   * function, failing if two elements produce the same key.
   *
   * The value held against each key is the element itself, and the map is ordered by the
   * `Order` of the key type rather than by the order the elements arrived in:
   *
   * {{{
   * Collections.toSortedMap(conventions, (c: Convention) => c.name)
   * // Right(SortedMap("Following" -> …, "ModifiedFollowing" -> …, "Preceding" -> …))
   * }}}
   *
   * Keys are required to be unique, as they are in the collector being ported. A repeated key
   * is a statement about the data rather than about the calling code, so it is reported as a
   * failure naming that key, and the first repeat stops the traversal - no later element is
   * examined and no later failure is reported.
   *
   * @tparam A  the type of the elements, which is also the type of the values of the map
   * @tparam K  the type of the keys of the map, which must have an order
   * @param items  the collection to build the map from, consumed once
   * @param key  extracts the key of each element
   * @return the map of key to element, or a failure if two elements produced the same key
   */
  def toSortedMap[A, K: Order](items: IterableOnce[A], key: A => K): Either[Failure, SortedMap[K, A]] =
    collectUnique(items.iterator.map(item => (key(item), item)), emptySortedMap[K, A])

  /**
   * Builds a sorted map from the specified collection, keyed and valued by the specified
   * functions, failing if two elements produce the same key.
   *
   * This is the form the port uses most, because it turns a collection of values that each
   * know their own key into the map that a type holds internally. Reading a multi-currency
   * amount as a map of currency to amount is exactly that:
   *
   * {{{
   * Collections.toSortedMap(amounts, (a: CurrencyAmount) => a.currency, (a: CurrencyAmount) => a.amount)
   * // Right(SortedMap(EUR -> 200.0, GBP -> 100.0)): SortedMap[Currency, Double]
   * }}}
   *
   * Keys are required to be unique and a repeat is reported as a failure, as for the form
   * above.
   *
   * @tparam A  the type of the elements
   * @tparam K  the type of the keys of the map, which must have an order
   * @tparam V  the type of the values of the map
   * @param items  the collection to build the map from, consumed once
   * @param key  extracts the key of each element
   * @param value  extracts the value of each element
   * @return the map of key to value, or a failure if two elements produced the same key
   */
  def toSortedMap[A, K: Order, V](
      items: IterableOnce[A],
      key: A => K,
      value: A => V): Either[Failure, SortedMap[K, V]] =
    collectUnique(items.iterator.map(item => (key(item), value(item))), emptySortedMap[K, V])

  /**
   * Builds a sorted map from the specified collection, combining the values of elements that
   * produce the same key.
   *
   * This form cannot fail: a repeated key is what the merge function is for, so the map holds
   * one value per distinct key however many elements contributed to it. Summing the amounts
   * of a collection that may name a currency more than once is the canonical use:
   *
   * {{{
   * Collections.toSortedMap(
   *   amounts,
   *   (a: CurrencyAmount) => a.currency,
   *   (a: CurrencyAmount) => a.amount,
   *   (first: Double, second: Double) => first + second)
   * // (EUR 100, EUR 200, CAD 100) becomes SortedMap(CAD -> 100.0, EUR -> 300.0)
   * }}}
   *
   * The merge function receives the value accumulated so far as its first argument and the
   * value of the element being added as its second, which is the order the collector being
   * ported used. That matters for any combination that is not commutative - subtraction,
   * string concatenation, or simply keeping the first or the last of the two.
   *
   * @tparam A  the type of the elements
   * @tparam K  the type of the keys of the map, which must have an order
   * @tparam V  the type of the values of the map
   * @param items  the collection to build the map from, consumed once
   * @param key  extracts the key of each element
   * @param value  extracts the value of each element
   * @param merge  combines the value accumulated for a key with the value of a later element
   * @return the map of key to combined value
   */
  def toSortedMap[A, K: Order, V](
      items: IterableOnce[A],
      key: A => K,
      value: A => V,
      merge: (V, V) => V): SortedMap[K, V] =
    items.iterator.foldLeft(emptySortedMap[K, V]) { (accumulated, item) =>
      val itemKey = key(item)
      val itemValue = value(item)
      accumulated.updated(itemKey, accumulated.get(itemKey).fold(itemValue)(merge(_, itemValue)))
    }

  /**
   * Returns the empty sorted map for a key type that has an order.
   *
   * This is the single place where the order of the key type is turned into the standard
   * library ordering that a sorted map is built with, so every member above sorts its result
   * the same way. The ordering is passed explicitly rather than made implicit locally, which
   * keeps the conversion visible at the one point it happens.
   *
   * @tparam K  the type of the keys, which must have an order
   * @tparam V  the type of the values
   * @return the empty map, ordered by the order of the key type
   */
  private def emptySortedMap[K: Order, V]: SortedMap[K, V] =
    SortedMap.empty[K, V](Order[K].toOrdering)

  /**
   * Accumulates key and value pairs into a sorted map, stopping at the first repeated key.
   *
   * The recursion is in tail position and is compiled to a loop, so this holds no mutable
   * state and traverses a collection of any size without consuming stack. Emptiness of the
   * map is not enough to detect a repeat - `contains` is asked of each key before it is
   * added, because a later pair would otherwise silently replace an earlier one, which is
   * precisely the condition being reported.
   *
   * @tparam K  the type of the keys
   * @tparam V  the type of the values
   * @param remaining  the pairs still to add
   * @param accumulated  the map built from the pairs already added
   * @return the completed map, or a failure naming the first repeated key
   */
  @tailrec
  private def collectUnique[K, V](
      remaining: Iterator[(K, V)],
      accumulated: SortedMap[K, V]): Either[Failure, SortedMap[K, V]] =
    if (!remaining.hasNext) {
      Right(accumulated)
    } else {
      val (itemKey, itemValue) = remaining.next()
      if (accumulated.contains(itemKey)) {
        Left(duplicateKey(itemKey))
      } else {
        collectUnique(remaining, accumulated.updated(itemKey, itemValue))
      }
    }

  /**
   * Returns the failure reported when two elements produce the same key.
   *
   * The key appears both in the message, for a person reading a log, and as an attribute, for
   * code that has to act on the failure without parsing the message.
   *
   * @tparam K  the type of the key
   * @param key  the key that two elements produced
   * @return the failure describing the repeated key
   */
  private def duplicateKey[K](key: K): Failure =
    Failure.Invalid(s"Multiple entries found with the same key: $key").withAttribute("key", s"$key")

  //-------------------------------------------------------------------------
  /**
   * Groups the elements of the specified collection by key, keeping the order of the input.
   *
   * Two orders are preserved, and both are part of the contract. The keys of the result
   * appear in the order in which they were first encountered, and the elements of each group
   * appear in the order in which they arrived:
   *
   * {{{
   * Collections.groupByPreservingOrder(List("bb", "a", "cc", "b", "aa"))(_.length)
   * // ListMap(2 -> NonEmptyList("bb", "cc", "aa"), 1 -> NonEmptyList("a", "b"))
   * }}}
   *
   * That determinism is why this member exists at all. Grouping with the standard library
   * produces a map whose iteration order is unspecified, which is enough to make a rendered
   * or serialized form of the result differ between runs on the same input; this port treats
   * the serialized form of a value as something that can be compared byte for byte, so an
   * unspecified order cannot be allowed into it. The result type says so: `ListMap` iterates
   * in insertion order, and the insertion order here is the order of first encounter.
   *
   * Each group is a `NonEmptyList`, because a group only exists once an element has been put
   * in it. The invariant is therefore carried by the type and a caller never has to consider
   * an empty group, which is what the multimap of the original made it do.
   *
   * The grouping itself is a single pass with constant-time lookup per element. The result is
   * then assembled once from the keys in encounter order, so the cost of the member is
   * governed by the number of distinct keys rather than by the number of elements; `ListMap`
   * is a linked structure and is meant for the modest number of groups that the call sites of
   * this port produce, not as a general-purpose map to be queried repeatedly.
   *
   * @tparam A  the type of the elements
   * @tparam K  the type of the keys
   * @param items  the collection to group, consumed once
   * @param key  extracts the key of each element
   * @return the groups, keyed in order of first encounter, each in order of arrival
   */
  def groupByPreservingOrder[A, K](items: IterableOnce[A])(key: A => K): ListMap[K, NonEmptyList[A]] = {
    // The fold carries the keys in reverse order of first encounter alongside the groups, so
    // that neither prepending a key nor prepending an element to its group costs more than a
    // constant. Both are reversed once, at the end.
    val (reversedKeys, groups) =
      items.iterator.foldLeft((List.empty[K], Map.empty[K, NonEmptyList[A]])) {
        case ((keysSoFar, groupsSoFar), item) =>
          val itemKey = key(item)
          groupsSoFar.get(itemKey) match {
            case Some(group) => (keysSoFar, groupsSoFar.updated(itemKey, item :: group))
            case None => (itemKey :: keysSoFar, groupsSoFar.updated(itemKey, NonEmptyList.one(item)))
          }
      }
    // Every key in the list was inserted into the groups by the same step that recorded it,
    // so the lookup below always finds a group; it is written as a lookup that may find
    // nothing so that the member is total whatever happens to the fold above.
    ListMap.from(
      reversedKeys.reverseIterator
        .flatMap(groupKey => groups.get(groupKey).map(group => groupKey -> group.reverse)))
  }

  //-------------------------------------------------------------------------
  /**
   * Returns the elements of the specified collection as a non-empty chain, or none if the
   * collection was empty.
   *
   * This is the one point at which a collection that may be empty meets a type that may not
   * be, and it is a member rather than an expression at the call site because that meeting
   * happens wherever a chain of accumulated failures is built from whatever a caller happened
   * to collect:
   *
   * {{{
   * Collections.toNonEmptyChain(failures).fold(Right(value): Either[NonEmptyChain[Failure], A])(Left(_))
   * }}}
   *
   * A chain is the accumulating shape of this library - concatenation is constant time, which
   * is what makes it the failure side of a validated result - and this member is how a plain
   * collection reaches it. The elements keep their order.
   *
   * @tparam A  the type of the elements
   * @param items  the collection to convert, consumed once
   * @return the elements as a chain, or none if there were no elements
   */
  def toNonEmptyChain[A](items: IterableOnce[A]): Option[NonEmptyChain[A]] =
    NonEmptyChain.fromSeq(items.iterator.toVector)

  /**
   * Concatenates the specified chains, or returns none if there were no chains to concatenate.
   *
   * The elements of the result are those of every chain, in the order the chains were given
   * and in the order they hold within each chain. The result is optional because the input
   * collection may be empty, which no chain can express - a single chain concatenated with
   * nothing is that chain, but nothing concatenated with nothing is not a chain at all:
   *
   * {{{
   * Collections.concatNonEmptyChains(results.map(_.failures))  // Option[NonEmptyChain[Failure]]
   * }}}
   *
   * @tparam A  the type of the elements
   * @param chains  the chains to concatenate, consumed once
   * @return the concatenation, or none if no chains were given
   */
  def concatNonEmptyChains[A](chains: IterableOnce[NonEmptyChain[A]]): Option[NonEmptyChain[A]] = {
    val remaining = chains.iterator
    if (!remaining.hasNext) {
      None
    } else {
      val first = remaining.next()
      Some(concatNonEmptyChains(first, remaining))
    }
  }

  /**
   * Concatenates the specified chain with the specified further chains.
   *
   * This differs from the form above only in taking the first chain separately, which makes
   * the result a chain rather than an optional one: there is at least the first chain to
   * return, whether or not any further chain follows. It is the form to use where one chain
   * is already in hand and others are being folded into it:
   *
   * {{{
   * Collections.concatNonEmptyChains(reported, laterReports)  // NonEmptyChain[Failure]
   * }}}
   *
   * Concatenation of a chain is constant time, so folding a collection of them is linear in
   * the number of chains and independent of how many elements they hold.
   *
   * @tparam A  the type of the elements
   * @param first  the chain the result starts with
   * @param others  the chains to append to it, in order, consumed once
   * @return the concatenation of the first chain and the others
   */
  def concatNonEmptyChains[A](
      first: NonEmptyChain[A],
      others: IterableOnce[NonEmptyChain[A]]): NonEmptyChain[A] =
    others.iterator.foldLeft(first)((earlier, later) => earlier ++ later)
}
