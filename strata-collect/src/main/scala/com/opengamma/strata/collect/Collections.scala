/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.VectorMap

import cats.Order
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import com.opengamma.strata.collect.result.Failure

/**
 * The collection helpers of this library that the standard library and `cats` do not supply.
 *
 * This object is deliberately small. An immutable collection library, a sum type and an
 * accumulating applicative between them cover almost everything a call site needs, so a
 * member earns a place here only when the operation it performs is unavailable from them or
 * when the shape of its result - an ordering, a failure channel or a non-empty type - is part
 * of a contract this library states. Anything a call site can express in one expression over
 * `scala.collection.immutable` or `cats` is written there rather than wrapped here.
 *
 * The members fall into four groups:
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
 * caller decides what to do about them: the failure is a value, so it can be inspected,
 * accumulated alongside others and reported. Its message text is stable, because it reaches
 * logs and test expectations.
 *
 * The helpers that cannot fail - grouping, and the chain helpers - return their result
 * directly, and the merging form of `toSortedMap` is total, because combining the values of
 * two elements with the same key is exactly what the caller asked for.
 *
 * ===Ordering===
 *
 * Every member that produces a sorted map takes a `cats.Order` for the key type and derives
 * the standard library ordering from it, rather than taking the ordering itself. That follows
 * the convention of this library: a type that has an order declares a single `Order` instance
 * in its companion, and that instance is what every member asking for an order receives. A
 * key type that has only a standard library ordering reaches these members through
 * `cats.Order.fromOrdering`.
 *
 * ===How a map-shaped result is assembled===
 *
 * Four members build a map one element at a time - the three forms of `toSortedMap` and
 * `groupByPreservingOrder` - and each of them accumulates into a mutable map of the standard
 * library that is created inside the method body, is reachable from nothing else, and is read
 * exactly once at the end to freeze the immutable result the member returns. That is the only
 * mutability in this file and it is deliberate: an immutable map updated once per element
 * copies the path to the entry it changes on every element, which measured close to a kilobyte
 * of garbage for every element of an input of ten thousand distinct keys, where building the
 * map once costs one node per distinct key. Nothing about the result changes - the same type,
 * the same ordering, the same iteration order, the same failure on a repeated key - and the
 * accumulator is an implementation detail in the strictest sense: it appears in no signature,
 * no field and no returned value, so no caller can observe that it existed. Each member says
 * below which mutable map it uses and what that map contributes beyond a cheaper assembly.
 *
 * ===Thread safety===
 *
 * Every member is a pure function of its arguments and this object holds no state, so it is
 * safe to use from any number of threads. A member that takes an `IterableOnce` consumes it
 * once and never retains it, so a caller that passes an iterator must not use that iterator
 * afterwards - the usual contract of a single-use collection.
 *
 * The accumulators described above do not qualify that. Each one is created by the invocation
 * that fills it, is never published, and is unreachable the moment the member returns, so two
 * threads calling the same member at the same time share nothing and two successive calls
 * cannot see one another's work.
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
    collectUnique(items.iterator.map(item => (key(item), item)))

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
    collectUnique(items.iterator.map(item => (key(item), value(item))))

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
      merge: (V, V) => V): SortedMap[K, V] = {
    val ordering = orderingOf[K]
    // The accumulator is a sorted mutable map of this invocation's own, ordered by the same
    // ordering as the result and frozen into it on the last line. Merging is a read of the
    // value held for the key followed by a write of the combined value, which is one search of
    // the tree and one write into it rather than a copy of the path to that entry.
    val accumulated = scala.collection.mutable.TreeMap.empty[K, V](ordering)
    val remaining = items.iterator
    // The traversal is a tail-recursive local rather than a `foreach`, and for a reason that
    // is not style: a function literal that captured the accumulator would be compiled to a
    // synthetic method taking it as a public argument, putting a mutable type into a member
    // signature of this object, whereas this local is compiled to a loop inside a private
    // one. Nothing here is a mutable variable - the only thing that changes is the map.
    @tailrec
    def mergeRemaining(): Unit =
      if (remaining.hasNext) {
        val item = remaining.next()
        val itemKey = key(item)
        val itemValue = value(item)
        accumulated.update(itemKey, accumulated.get(itemKey).fold(itemValue)(merge(_, itemValue)))
        mergeRemaining()
      }
    mergeRemaining()
    SortedMap.from(accumulated)(ordering)
  }

  /**
   * Returns the standard library ordering of a key type from the order it publishes.
   *
   * This is the single place where the order of the key type is turned into the standard
   * library ordering that a sorted map is built with, so every member above sorts its result
   * the same way - and, where a member builds its result through a sorted accumulator, that
   * accumulator and the result it is frozen into are given the very same ordering. The
   * ordering is passed explicitly rather than made implicit locally, which keeps the
   * conversion visible at the one point it happens.
   *
   * @tparam K  the type of the keys, which must have an order
   * @return the standard library ordering derived from the order of the key type
   */
  private def orderingOf[K: Order]: Ordering[K] = Order[K].toOrdering

  /**
   * Accumulates key and value pairs into a sorted map, stopping at the first repeated key.
   *
   * Both unique forms of `toSortedMap` are written in terms of this method, so neither the
   * assembly nor the failure can drift between them. The pairs arrive as an iterator that the
   * caller built by mapping over its own collection, and they are pulled one at a time: a
   * repeat stops the traversal where it is found, and neither the element that produced the
   * repeat nor anything after it is read again - which is what makes the promise that a
   * failing call leaves the rest of a single-use collection untouched.
   *
   * Emptiness of the map is not enough to detect a repeat - `contains` is asked of each key
   * before it is added, because a later pair would otherwise silently replace an earlier one,
   * which is precisely the condition being reported.
   *
   * The pairs accumulate into a sorted mutable map created here, which nothing outside this
   * method can reach and which is read once to freeze the immutable result; an immutable
   * sorted map updated once per pair would instead copy the path to the entry it added for
   * every pair. The recursion that drives it is in tail position and is compiled to a loop, so
   * a collection of any size is traversed without consuming stack, and the local it updates is
   * the accumulator itself rather than a mutable variable.
   *
   * @tparam K  the type of the keys, which must have an order
   * @tparam V  the type of the values
   * @param remaining  the pairs to add, pulled one at a time and consumed no further than the
   *   first repeated key
   * @return the completed map, or a failure naming the first repeated key
   */
  private def collectUnique[K: Order, V](
      remaining: Iterator[(K, V)]): Either[Failure, SortedMap[K, V]] = {
    val ordering = orderingOf[K]
    val accumulated = scala.collection.mutable.TreeMap.empty[K, V](ordering)
    @tailrec
    def addRemaining(): Either[Failure, SortedMap[K, V]] =
      if (!remaining.hasNext) {
        Right(SortedMap.from(accumulated)(ordering))
      } else {
        val (itemKey, itemValue) = remaining.next()
        if (accumulated.contains(itemKey)) {
          Left(duplicateKey(itemKey))
        } else {
          accumulated.update(itemKey, itemValue)
          addRemaining()
        }
      }
    addRemaining()
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
   * // VectorMap(2 -> NonEmptyList("bb", "cc", "aa"), 1 -> NonEmptyList("a", "b"))
   * }}}
   *
   * That determinism is why this member exists at all. Grouping with the standard library
   * produces a map whose iteration order is unspecified, which is enough to make a rendered
   * or serialized form of the result differ between runs on the same input; the serialized
   * form of a value in this library is something that can be compared byte for byte, so an
   * unspecified order cannot be allowed into it. The result type says so: `VectorMap` is an
   * immutable map that iterates in insertion order, and the insertion order here is the order
   * of first encounter.
   *
   * Each group is a `NonEmptyList`, because a group only exists once an element has been put
   * in it. The invariant is therefore carried by the type, and a caller never has to consider
   * an empty group.
   *
   * The grouping itself is a single pass with constant-time lookup per element, and the result
   * is assembled once from the keys in encounter order, so the member is linear in the number
   * of elements and its assembly is linear in the number of distinct keys. `VectorMap` is what
   * makes the second half of that true: it is built by appending each key to a vector and
   * recording it in a hashed map, so a key costs a constant amount to add however many keys
   * precede it. The other insertion-ordered map of the standard library, `ListMap`, is a
   * linked structure whose builder searches the entries it has already accumulated for every
   * key it is given, which makes assembling `k` groups quadratic in `k` - and quadratic in the
   * number of elements in the case where every element has a key of its own. Reading the
   * result is constant time per key for the same reason: a key is looked up by hash rather
   * than by walking a chain of entries whose length is the number of groups.
   *
   * The pass that precedes that assembly is over a mutable insertion-ordered map of this
   * invocation's own - a `LinkedHashMap`, created here, updated only from here and read once
   * to build the `VectorMap` returned. The choice of a mutable map is for the same reason as
   * the choice of `VectorMap` over `ListMap`: an immutable map updated once per element copies
   * the path to the entry it changes on every element, which is an allocation per element
   * proportional to the depth of the map rather than to the group it adds to - close to a
   * kilobyte per element on an input of ten thousand distinct keys, where this pass allocates
   * one entry per distinct key and one cell per element. Insertion order is what the mutable
   * map contributes beyond that: updating the group of a key already present leaves that key
   * where it was, so the order the map iterates in is the order of first encounter and the
   * keys no longer have to be carried, reversed and looked up again alongside the groups.
   *
   * @tparam A  the type of the elements
   * @tparam K  the type of the keys
   * @param items  the collection to group, consumed once
   * @param key  extracts the key of each element
   * @return the groups, keyed in order of first encounter, each in order of arrival
   */
  def groupByPreservingOrder[A, K](items: IterableOnce[A])(key: A => K): VectorMap[K, NonEmptyList[A]] = {
    // Each group accumulates in reverse order of arrival, so that prepending an element to it
    // costs a constant however long the group already is. Every group is reversed once, in the
    // single pass over the accumulator that assembles the result below.
    val groups = scala.collection.mutable.LinkedHashMap.empty[K, NonEmptyList[A]]
    val remaining = items.iterator
    // Tail-recursive rather than a `foreach` for the reason given in the merging form of
    // `toSortedMap`: a function literal capturing the accumulator would carry a mutable type
    // into a synthetic member signature of this object, and a local compiled to a loop does
    // not. There is no mutable variable in it either - the map is the only thing that changes.
    @tailrec
    def groupRemaining(): Unit =
      if (remaining.hasNext) {
        val item = remaining.next()
        val itemKey = key(item)
        groups.get(itemKey) match {
          case Some(group) => groups.update(itemKey, item :: group)
          case None => groups.update(itemKey, NonEmptyList.one(item))
        }
        groupRemaining()
      }
    groupRemaining()
    // The accumulator iterates its keys in the order they were first inserted, which is the
    // order of first encounter, so the result takes them in the order it receives them. The
    // accumulator itself is not reachable from the result and is unreachable altogether once
    // this method returns - the groups are immutable, so the ones handed over unchanged are
    // shared rather than copied. A group of one element is in arrival order already and is
    // handed over as it is: reversing it would answer an equal list and allocate a second one
    // for every element of an input whose keys are all distinct, which is the shape that costs
    // the most to begin with.
    VectorMap.from(groups.iterator.map { case (groupKey, group) =>
      (groupKey, if (group.tail.isEmpty) group else group.reverse)
    })
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
