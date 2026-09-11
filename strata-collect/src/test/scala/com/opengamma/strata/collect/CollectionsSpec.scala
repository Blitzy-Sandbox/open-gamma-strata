/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.time.LocalDate

import scala.collection.immutable.List
import scala.collection.immutable.ListMap
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.immutable.Vector
import scala.util.Try

import cats.Order
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests [[Collections]], the four groups of collection helper this port keeps.
 *
 * ===Scope===
 *
 * The two static helper classes being ported - a set of stream collectors with small
 * collection utilities, and a view over the entries of a map - run to some two and a half
 * thousand lines between them and are covered by a hundred and fifty test methods. Almost
 * none of that has a counterpart here, because almost none of it has a counterpart in
 * [[Collections]]: the language this port targets supplies the immutable collections, the
 * sum type and the accumulating applicative that those classes existed to simulate. This
 * spec therefore covers the nine members that were ported and nothing else, which is the
 * whole of the unit under test:
 *
 *   - the two `ensureOnlyOne` forms, which read the single element a collection is required
 *     to hold and distinguish an empty collection from an ambiguous one;
 *   - the three `toSortedMap` forms - keyed, keyed and valued, and merging;
 *   - `groupByPreservingOrder`, which groups by key with both orders of the input kept;
 *   - the three chain helpers, which build a chain from a collection that may be empty and
 *     concatenate chains.
 *
 * A member of either original class that was not ported is covered here by two kinds of test
 * rather than by none, so that the decision is recorded and stays true: an `assertDoesNotCompile`
 * proof that the member does not exist on [[Collections]], and a test of the standard library
 * or `cats` expression that replaces it at the call site. Those are the last two sections of
 * this spec. Every member the dependent module actually used is in one of them - the
 * single-element reducer, the list factory, the list, set and sorted-set collectors, the two
 * optional adapters, the exception-to-optional wrapper, and the entry view of a map.
 *
 * ===What is asserted, and why===
 *
 * Three properties of the port are behaviour rather than detail, and each is asserted
 * directly rather than inferred:
 *
 *   - '''A failure is returned, never thrown.''' Both conditions these helpers report - more
 *     than one element where one was expected, and two elements that produce the same key -
 *     depend on the caller's data, so they arrive as a
 *     [[com.opengamma.strata.collect.result.Failure Failure]] on the left of an `Either`.
 *     There is no exception assertion anywhere in this spec: every failing case is asserted
 *     through the matchers of [[com.opengamma.strata.collect.testkit.ResultMatchers ResultMatchers]],
 *     down to the reason, the message text and the attribute the failure carries.
 *   - '''The sorted maps are sorted, and immutable.''' Each result is bound to
 *     `scala.collection.immutable.SortedMap`, which the compiler checks, and its key order is
 *     asserted explicitly - including the case where the `cats.Order` supplied for the key
 *     type is not the natural one, which is what shows that the order of the result comes
 *     from that instance. The serialized form of a value elsewhere in this port is compared
 *     byte for byte, and that rests on these orders.
 *   - '''Grouping preserves the order of the input.''' The key order of a grouped result is
 *     asserted against inputs whose order of first encounter differs from the sorted order of
 *     their keys, so an implementation that grouped with the standard library - whose map
 *     iteration order is unspecified - fails these tests rather than passing them by
 *     coincidence. The larger of the two inputs was checked to iterate in a different order
 *     under standard grouping.
 *
 * Reading a collection once is part of the contract of every member, so the members that
 * promise to stop early are given an iterator and the state of that iterator afterwards is
 * asserted: `ensureOnlyOne` reads at most two elements, and the unique forms of
 * `toSortedMap` stop at the first repeated key.
 *
 * @see [[Collections]] for the helpers under test and the record of those deliberately absent
 */
final class CollectionsSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /**
   * The collection of the ported sorted-map cases: one element per distinct length, given in
   * an order that is not the order of their keys.
   */
  private val DistinctLengths: List[String] = List("bob", "a", "ab")

  /**
   * The collection of the ported duplicate-key cases, in which two elements share a length.
   * The first repeat is the third element, whose key is 1.
   */
  private val RepeatedLengths: List[String] = List("a", "ab", "c", "bb", "b", "a")

  /**
   * The collection of the ported grouping case, which holds four elements of length one -
   * two of them equal - and two of length two.
   */
  private val GroupingInput: List[String] = List("a", "ab", "b", "bb", "c", "a")

  /**
   * A collection whose keys are first encountered in the reverse of their sorted order, and
   * with enough distinct keys that grouping it with the standard library produces a
   * different key order. It is the input of the tests that pin the encounter order down.
   */
  private val DescendingLengths: List[String] =
    List("aaaaaa", "bbbbb", "cccc", "ddd", "ee", "f", "g")

  /**
   * Extracts the key of the ported cases. Every member that takes a key function is
   * overloaded on arity, so the function is held in a typed value rather than written as a
   * lambda at each call: the parameter type is then known without an annotation.
   */
  private val lengthOf: String => Int = string => string.length

  /** Extracts the value of the ported cases, which marks the element it came from. */
  private val marked: String => String = string => s"!$string"

  /** Combines two values by concatenation, the merge function of the ported merging case. */
  private val concatenated: (String, String) => String = (first, second) => first + second

  /**
   * Returns the failure an outcome holds, if it holds one.
   *
   * The matchers assert the reason and the message of a failure, which is what most cases
   * need; this reads the failure itself for the few that assert its attributes too. It goes
   * through `swap` rather than through a projection, so nothing here can reach for the value
   * of an outcome that has none.
   *
   * @tparam A  the type of the value the outcome carries when it succeeds
   * @param outcome  the outcome to inspect
   * @return the failure of the outcome, or `None` when it succeeded
   */
  private def failureOf[A](outcome: FailureOr[A]): Option[Failure] = outcome.swap.toOption

  /**
   * The key type of the case that exercises a key with no `cats.Order` of its own. It is
   * declared in the companion of this spec rather than in the class, so that it carries no
   * reference to the enclosing instance.
   */
  import CollectionsSpec.Code

  //-------------------------------------------------------------------------
  // ensureOnlyOne, with the message of the helper being ported

  test("ensureOnlyOne reports no element for an empty collection") {
    val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(List.empty[String])
    outcome should beSuccess
    outcome should haveValue(None)
  }

  test("ensureOnlyOne reports the element of a collection of one") {
    val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a"))
    outcome should beSuccess
    outcome should haveValue(Some("a"))
  }

  test("ensureOnlyOne fails for a collection of two, naming both elements") {
    val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a", "b"))
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)
    failureOf(outcome).map(_.message) shouldBe
      Some("Multiple values found where only one was expected: a and b")
  }

  test("ensureOnlyOne fails for a longer collection, naming the first two elements it found") {
    val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a", "b", "c", "d"))
    outcome should beFailureWith(FailureReason.INVALID)
    failureOf(outcome).map(_.message) shouldBe
      Some("Multiple values found where only one was expected: a and b")
    failureOf(outcome).map(_.attributes) shouldBe Some(SortedMap.empty[String, String])
  }

  test("ensureOnlyOne counts the elements of a collection rather than its distinct values") {
    val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a", "a"))
    outcome should beFailureWith(FailureReason.INVALID)
    failureOf(outcome).map(_.message) shouldBe
      Some("Multiple values found where only one was expected: a and a")
  }

  test("ensureOnlyOne accepts the call-site idiom of de-duplicating first") {
    val single: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List("GBP", "GBP", "GBP").iterator.distinct)
    single should haveValue(Some("GBP"))
    val mixed: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List("GBP", "USD", "GBP").iterator.distinct)
    mixed should beFailureWith(FailureReason.INVALID)
    failureOf(mixed).map(_.message) shouldBe
      Some("Multiple values found where only one was expected: GBP and USD")
  }

  test("ensureOnlyOne reads every single-use collection shape alike") {
    val shapes = Table[String, IterableOnce[String], IterableOnce[String], IterableOnce[String]](
      ("shape", "none", "one", "two"),
      ("List", List.empty[String], List("a"), List("a", "b")),
      ("Vector", Vector.empty[String], Vector("a"), Vector("a", "b")),
      ("Set", Set.empty[String], Set("a"), Set("a", "b")),
      ("Iterator", Iterator.empty[String], Iterator("a"), Iterator("a", "b"))
    )
    forAll(shapes) { (shape, none, one, two) =>
      withClue(s"$shape holding no element: ") {
        val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(none)
        outcome should haveValue(None)
      }
      withClue(s"$shape holding one element: ") {
        val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(one)
        outcome should haveValue(Some("a"))
      }
      withClue(s"$shape holding two elements: ") {
        val outcome: FailureOr[Option[String]] = Collections.ensureOnlyOne(two)
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }
  }

  test("ensureOnlyOne reads at most two elements of the collection it is given") {
    val remaining = List(1, 2, 3, 4, 5).iterator
    val outcome: FailureOr[Option[Int]] = Collections.ensureOnlyOne(remaining)
    outcome should beFailureWith(FailureReason.INVALID)
    failureOf(outcome).map(_.message) shouldBe
      Some("Multiple values found where only one was expected: 1 and 2")
    remaining.toList shouldBe List(3, 4, 5)
  }

  test("ensureOnlyOne consumes exactly the elements it reports on") {
    val one = List("only").iterator
    val single: FailureOr[Option[String]] = Collections.ensureOnlyOne(one)
    single should haveValue(Some("only"))
    one.hasNext shouldBe false
    val none = Iterator.empty[String]
    val empty: FailureOr[Option[String]] = Collections.ensureOnlyOne(none)
    empty should haveValue(None)
    none.hasNext shouldBe false
  }

  //-------------------------------------------------------------------------
  // ensureOnlyOne, with a message supplied by the caller

  test("ensureOnlyOne with a caller message reports the same three outcomes") {
    val none: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List.empty[String], "More than one letter was found")
    none should haveValue(None)
    val one: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List("a"), "More than one letter was found")
    one should haveValue(Some("a"))
    val two: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List("a", "b"), "More than one letter was found")
    two should beFailureWith(FailureReason.INVALID)
  }

  test("ensureOnlyOne with a caller message carries that message into the failure") {
    val date = LocalDate.of(2024, 4, 24)
    val outcome: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List("a", "b"), s"Expected one letter but found multiple for date $date")
    outcome should beFailureWith(FailureReason.INVALID)
    failureOf(outcome).map(_.message) shouldBe
      Some("Expected one letter but found multiple for date 2024-04-24")
    outcome should haveFailureMessageMatching("Expected one letter but found multiple for date .*")
  }

  test("ensureOnlyOne does not build a caller message that no failure will carry") {
    val none: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List.empty[String], fail("the message of a passing check was built"))
    none should haveValue(None)
    val one: FailureOr[Option[String]] =
      Collections.ensureOnlyOne(List("a"), fail("the message of a passing check was built"))
    one should haveValue(Some("a"))
  }

  //-------------------------------------------------------------------------
  // toSortedMap, keyed by a function

  test("toSortedMap keyed by a function holds each element against its key, in key order") {
    val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(DistinctLengths, lengthOf)
    built should beSuccess
    built should haveValue(SortedMap(1 -> "a", 2 -> "ab", 3 -> "bob"))
    built.map(_.keys.toList) shouldBe Right(List(1, 2, 3))
    built.map(_.values.toList) shouldBe Right(List("a", "ab", "bob"))
  }

  test("toSortedMap keyed by a function fails where two elements produce the same key") {
    val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(RepeatedLengths, lengthOf)
    built should beFailure
    built should beFailureWith(FailureReason.INVALID)
    failureOf(built).map(_.message) shouldBe Some("Multiple entries found with the same key: 1")
    failureOf(built).map(_.attributes) shouldBe Some(SortedMap("key" -> "1"))
  }

  test("toSortedMap keyed by a function names the key that first repeated") {
    val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(List("ab", "cd"), lengthOf)
    failureOf(built).map(_.message) shouldBe Some("Multiple entries found with the same key: 2")
    failureOf(built).map(_.attributes) shouldBe Some(SortedMap("key" -> "2"))
  }

  test("toSortedMap keyed by a function stops reading at the first repeated key") {
    val remaining = List("a", "b", "cc", "ddd").iterator
    val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(remaining, lengthOf)
    built should beFailureWith(FailureReason.INVALID)
    remaining.toList shouldBe List("cc", "ddd")
  }

  test("toSortedMap keyed by a function handles an empty and a single-element collection") {
    val none: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(List.empty[String], lengthOf)
    none should haveValue(SortedMap.empty[Int, String])
    none.map(_.isEmpty) shouldBe Right(true)
    val one: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(List("a"), lengthOf)
    one should haveValue(SortedMap(1 -> "a"))
  }

  test("toSortedMap keyed by a function sorts by the order of the key type, not by arrival") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(List("ccc", "bb", "dddd", "a"), lengthOf)
    built.map(_.iterator.toList) shouldBe Right(List(1 -> "a", 2 -> "bb", 3 -> "ccc", 4 -> "dddd"))
  }

  test("toSortedMap keyed by a function sorts by the supplied order where that is not the natural one") {
    // The member takes a cats Order for the key type and derives the ordering of the map from
    // it, so an Order in scope that reverses the natural one reverses the map. A local
    // instance takes precedence over the one the key type publishes.
    implicit val descending: Order[Int] = Order.fromOrdering(Ordering.Int.reverse)
    val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(DistinctLengths, lengthOf)
    built should haveValue(SortedMap(1 -> "a", 2 -> "ab", 3 -> "bob"))
    built.map(_.keys.toList) shouldBe Right(List(3, 2, 1))
  }

  test("toSortedMap keyed by a function accepts a key type that has only a standard library ordering") {
    // A key type of this spec's own, which publishes no cats Order, reaches the member by the
    // documented route: an Order derived from the standard library ordering it does publish.
    implicit val codeOrder: Order[Code] = Order.fromOrdering(Code.ordering)
    val built: FailureOr[SortedMap[Code, String]] =
      Collections.toSortedMap(DistinctLengths, (string: String) => Code(string.length.toString))
    built should beSuccess
    built.map(_.keys.toList) shouldBe Right(List(Code("1"), Code("2"), Code("3")))
    built should haveValue(SortedMap(Code("1") -> "a", Code("2") -> "ab", Code("3") -> "bob")(Code.ordering))
  }

  //-------------------------------------------------------------------------
  // toSortedMap, keyed and valued by functions

  test("toSortedMap keyed and valued by functions applies both projections") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(DistinctLengths, lengthOf, marked)
    built should beSuccess
    built should haveValue(SortedMap(1 -> "!a", 2 -> "!ab", 3 -> "!bob"))
    built.map(_.keys.toList) shouldBe Right(List(1, 2, 3))
  }

  test("toSortedMap keyed and valued by functions values the map independently of its elements") {
    val built: FailureOr[SortedMap[Int, Int]] =
      Collections.toSortedMap(DistinctLengths, lengthOf, lengthOf)
    built should haveValue(SortedMap(1 -> 1, 2 -> 2, 3 -> 3))
  }

  test("toSortedMap keyed and valued by functions fails where two elements produce the same key") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(RepeatedLengths, lengthOf, marked)
    built should beFailureWith(FailureReason.INVALID)
    failureOf(built).map(_.message) shouldBe Some("Multiple entries found with the same key: 1")
    failureOf(built).map(_.attributes) shouldBe Some(SortedMap("key" -> "1"))
  }

  test("toSortedMap keyed and valued by functions stops reading at the first repeated key") {
    val remaining = List("a", "b", "cc", "ddd").iterator
    val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(remaining, lengthOf, marked)
    built should beFailure
    remaining.toList shouldBe List("cc", "ddd")
  }

  test("toSortedMap keyed and valued by functions handles an empty and a single-element collection") {
    val none: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(List.empty[String], lengthOf, marked)
    none should haveValue(SortedMap.empty[Int, String])
    val one: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(List("ab"), lengthOf, marked)
    one should haveValue(SortedMap(2 -> "!ab"))
  }

  //-------------------------------------------------------------------------
  // toSortedMap, merging the values of a repeated key

  test("toSortedMap with a merge function combines the values of a repeated key") {
    val built: SortedMap[Int, String] =
      Collections.toSortedMap(RepeatedLengths, lengthOf, marked, concatenated)
    built shouldBe SortedMap(1 -> "!a!c!b!a", 2 -> "!ab!bb")
    built.keys.toList shouldBe List(1, 2)
  }

  test("toSortedMap with a merge function passes the accumulated value first") {
    // The order of the two arguments is observable through any combination that is not
    // commutative, which is what this and the next case rest on.
    val built: SortedMap[Int, String] = Collections.toSortedMap(
      RepeatedLengths,
      lengthOf,
      marked,
      (accumulated: String, added: String) => s"$accumulated>$added")
    built shouldBe SortedMap(1 -> "!a>!c>!b>!a", 2 -> "!ab>!bb")
  }

  test("toSortedMap with a merge function can keep the first or the last value of a key") {
    val first: SortedMap[Int, String] = Collections.toSortedMap(
      RepeatedLengths,
      lengthOf,
      marked,
      (accumulated: String, _: String) => accumulated)
    first shouldBe SortedMap(1 -> "!a", 2 -> "!ab")
    val last: SortedMap[Int, String] = Collections.toSortedMap(
      RepeatedLengths,
      lengthOf,
      marked,
      (_: String, added: String) => added)
    // The last element of the input repeats its first, so the last value of key 1 reads the
    // same as the first; key 2 is what separates the two merge functions.
    last shouldBe SortedMap(1 -> "!a", 2 -> "!bb")
  }

  test("toSortedMap with a merge function sums the values of a repeated key") {
    val built: SortedMap[Int, Int] = Collections.toSortedMap(
      RepeatedLengths,
      lengthOf,
      lengthOf,
      (accumulated: Int, added: Int) => accumulated + added)
    built shouldBe SortedMap(1 -> 4, 2 -> 4)
  }

  test("toSortedMap with a merge function cannot fail") {
    Collections.toSortedMap(List.empty[String], lengthOf, marked, concatenated) shouldBe
      SortedMap.empty[Int, String]
    Collections.toSortedMap(List("a"), lengthOf, marked, concatenated) shouldBe SortedMap(1 -> "!a")
    Collections.toSortedMap(List("a", "b", "c"), lengthOf, marked, concatenated) shouldBe
      SortedMap(1 -> "!a!b!c")
  }

  test("toSortedMap with a merge function sorts its keys and reads its collection once") {
    val remaining = List("ccc", "a", "bb", "dd").iterator
    val built: SortedMap[Int, String] =
      Collections.toSortedMap(remaining, lengthOf, marked, concatenated)
    built.keys.toList shouldBe List(1, 2, 3)
    built shouldBe SortedMap(1 -> "!a", 2 -> "!bb!dd", 3 -> "!ccc")
    remaining.hasNext shouldBe false
  }


  //-------------------------------------------------------------------------
  // groupByPreservingOrder

  test("groupByPreservingOrder keys the result in order of first encounter") {
    // The keys of this input are first encountered as 2 then 1, the reverse of their sorted
    // order, so the assertion below distinguishes the member from grouping that does not
    // promise an order.
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(List("bb", "a", "cc", "b", "aa"))(lengthOf)
    grouped.keys.toList shouldBe List(2, 1)
    grouped.get(2).map(_.toList) shouldBe Some(List("bb", "cc", "aa"))
    grouped.get(1).map(_.toList) shouldBe Some(List("a", "b"))
  }

  test("groupByPreservingOrder keeps the encounter order of many keys") {
    // Seven elements over six keys, encountered in the reverse of their sorted order. Grouping
    // this input with the standard library yields the keys in an order that is neither of
    // those, so this case fails against any implementation that does not preserve encounter
    // order rather than passing by coincidence.
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(DescendingLengths)(lengthOf)
    grouped.keys.toList shouldBe List(6, 5, 4, 3, 2, 1)
    grouped.iterator.map { case (key, group) => (key, group.toList) }.toList shouldBe List(
      6 -> List("aaaaaa"),
      5 -> List("bbbbb"),
      4 -> List("cccc"),
      3 -> List("ddd"),
      2 -> List("ee"),
      1 -> List("f", "g")
    )
  }

  test("groupByPreservingOrder keeps the arrival order within each group, duplicates included") {
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(GroupingInput)(lengthOf)
    grouped.keys.toList shouldBe List(1, 2)
    grouped.get(1).map(_.toList) shouldBe Some(List("a", "b", "c", "a"))
    grouped.get(2).map(_.toList) shouldBe Some(List("ab", "bb"))
    grouped.values.map(_.size).sum shouldBe GroupingInput.size
  }

  test("groupByPreservingOrder makes a group of one element a list of one element") {
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(List("a"))(lengthOf)
    grouped.keys.toList shouldBe List(1)
    grouped.get(1) shouldBe Some(NonEmptyList.one("a"))
    grouped.get(1).map(_.size) shouldBe Some(1)
    grouped.get(1).map(_.head) shouldBe Some("a")
  }

  test("groupByPreservingOrder of an empty collection is the empty map") {
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(List.empty[String])(lengthOf)
    grouped shouldBe ListMap.empty[Int, NonEmptyList[String]]
    grouped.keys.toList shouldBe List.empty[Int]
    grouped.isEmpty shouldBe true
  }

  test("groupByPreservingOrder groups by a key that is not derived from the element order") {
    val grouped: ListMap[Boolean, NonEmptyList[Int]] =
      Collections.groupByPreservingOrder(List(1, 2, 3, 4, 5))(value => value % 2 == 0)
    grouped.keys.toList shouldBe List(false, true)
    grouped.get(false).map(_.toList) shouldBe Some(List(1, 3, 5))
    grouped.get(true).map(_.toList) shouldBe Some(List(2, 4))
  }

  test("groupByPreservingOrder reads a single-use collection once") {
    val remaining = List("a", "ab", "b").iterator
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(remaining)(lengthOf)
    grouped.keys.toList shouldBe List(1, 2)
    grouped.get(1).map(_.toList) shouldBe Some(List("a", "b"))
    remaining.hasNext shouldBe false
  }

  //-------------------------------------------------------------------------
  // toNonEmptyChain and concatNonEmptyChains

  test("toNonEmptyChain of an empty collection is no chain") {
    Collections.toNonEmptyChain(List.empty[Int]) shouldBe None
    Collections.toNonEmptyChain(Vector.empty[Int]) shouldBe None
    Collections.toNonEmptyChain(Iterator.empty[Int]) shouldBe None
    Collections.toNonEmptyChain(Set.empty[Int]) shouldBe None
  }

  test("toNonEmptyChain of one element is the chain of that element") {
    Collections.toNonEmptyChain(List("only")) shouldBe Some(NonEmptyChain.one("only"))
    Collections.toNonEmptyChain(List("only")).map(_.length) shouldBe Some(1L)
  }

  test("toNonEmptyChain keeps the order of the elements it was given") {
    Collections.toNonEmptyChain(List(1, 2, 3)).map(_.toChain.toList) shouldBe Some(List(1, 2, 3))
    Collections.toNonEmptyChain(Vector(3, 2, 1)).map(_.toChain.toList) shouldBe Some(List(3, 2, 1))
    Collections.toNonEmptyChain(Iterator(1, 1, 2)).map(_.toChain.toList) shouldBe Some(List(1, 1, 2))
  }

  test("concatNonEmptyChains of no chain at all is no chain") {
    Collections.concatNonEmptyChains(List.empty[NonEmptyChain[Int]]) shouldBe None
    Collections.concatNonEmptyChains(Iterator.empty[NonEmptyChain[Int]]) shouldBe None
  }

  test("concatNonEmptyChains of a single chain is that chain") {
    Collections.concatNonEmptyChains(List(NonEmptyChain(1, 2))).map(_.toChain.toList) shouldBe
      Some(List(1, 2))
  }

  test("concatNonEmptyChains concatenates chains in the order they were given") {
    val chains = List(NonEmptyChain(1, 2), NonEmptyChain(3), NonEmptyChain(4, 5))
    Collections.concatNonEmptyChains(chains).map(_.toChain.toList) shouldBe Some(List(1, 2, 3, 4, 5))
    Collections.concatNonEmptyChains(chains.reverse).map(_.toChain.toList) shouldBe
      Some(List(4, 5, 3, 1, 2))
  }

  test("concatNonEmptyChains with a leading chain returns a chain whatever follows it") {
    val first = NonEmptyChain("a")
    Collections.concatNonEmptyChains(first, List.empty[NonEmptyChain[String]]).toChain.toList shouldBe
      List("a")
    Collections
      .concatNonEmptyChains(first, List(NonEmptyChain("b", "c"), NonEmptyChain("d")))
      .toChain
      .toList shouldBe List("a", "b", "c", "d")
  }

  test("concatNonEmptyChains builds the accumulated failures of several outcomes") {
    // The chain is the accumulating shape of the library, so this is the shape the helpers
    // exist for: the failures several checks reported become the one chain a caller reads.
    val reported = List(
      NonEmptyChain.one(Failure.Invalid("first")),
      NonEmptyChain.of(Failure.Parsing("second"), Failure.MissingData("third")))
    val combined: Option[NonEmptyChain[Failure]] = Collections.concatNonEmptyChains(reported)
    combined.map(_.toChain.toList.map(_.message)) shouldBe Some(List("first", "second", "third"))
    combined.map(_.toChain.toList.map(_.reason)) shouldBe
      Some(List(FailureReason.INVALID, FailureReason.PARSING, FailureReason.MISSING_DATA))
    combined.map(_.length) shouldBe Some(3L)
  }

  //-------------------------------------------------------------------------
  // Properties

  test("ensureOnlyOne succeeds exactly when the collection holds at most one element") {
    forAll { (elements: List[Int]) =>
      val outcome: FailureOr[Option[Int]] = Collections.ensureOnlyOne(elements)
      outcome.isRight shouldBe (elements.sizeIs <= 1)
      outcome.toOption.flatten shouldBe (if (elements.sizeIs == 1) elements.headOption else None)
    }
  }

  test("groupByPreservingOrder keys the result by the distinct keys in encounter order") {
    forAll { (elements: List[String]) =>
      val grouped = Collections.groupByPreservingOrder(elements)(lengthOf)
      grouped.keys.toList shouldBe elements.map(lengthOf).distinct
    }
  }

  test("groupByPreservingOrder loses no element and invents none") {
    forAll { (elements: List[String]) =>
      val grouped = Collections.groupByPreservingOrder(elements)(lengthOf)
      grouped.values.toList.flatMap(_.toList) should contain theSameElementsAs elements
      grouped.values.map(_.size).sum shouldBe elements.size
    }
  }

  test("groupByPreservingOrder puts each element in the group of its key, in order") {
    forAll { (elements: List[String]) =>
      val grouped = Collections.groupByPreservingOrder(elements)(lengthOf)
      grouped.foreach { case (key, group) =>
        group.toList shouldBe elements.filter(element => lengthOf(element) == key)
      }
      succeed
    }
  }

  test("toSortedMap keyed by a function succeeds exactly when the keys are distinct") {
    forAll { (elements: List[String]) =>
      val keys = elements.map(lengthOf)
      val built: FailureOr[SortedMap[Int, String]] = Collections.toSortedMap(elements, lengthOf)
      built.isRight shouldBe (keys.distinct.sizeIs == keys.size)
      built.foreach(map => map.keys.toList shouldBe keys.sorted)
      succeed
    }
  }

  test("toSortedMap with a merge function keys the result by the sorted distinct keys") {
    forAll { (elements: List[String]) =>
      val built = Collections.toSortedMap(elements, lengthOf, marked, concatenated)
      built.keys.toList shouldBe elements.map(lengthOf).distinct.sorted
      (built.sizeIs <= elements.size) shouldBe true
    }
  }

  test("toNonEmptyChain round-trips the elements of any collection") {
    forAll { (elements: List[Int]) =>
      Collections.toNonEmptyChain(elements).map(_.toChain.toList) shouldBe
        (if (elements.isEmpty) None else Some(elements))
    }
  }

  test("concatNonEmptyChains concatenates the elements of every chain it was given") {
    forAll { (groups: List[List[Int]]) =>
      val chains = groups.flatMap(group => Collections.toNonEmptyChain(group))
      Collections.concatNonEmptyChains(chains).map(_.toChain.toList) shouldBe
        (if (chains.isEmpty) None else Some(groups.flatten))
    }
  }


  //-------------------------------------------------------------------------
  // The members that were not ported, proved absent

  test("the entry view of a map has no counterpart on Collections") {
    assertDoesNotCompile("Collections.of(Map.empty[String, Int])")
    assertDoesNotCompile("Collections.mapStream(Map.empty[String, Int])")
    assertDoesNotCompile("Collections.entries(Map.empty[String, Int])")
    assertDoesNotCompile("Collections.mapValues(Map.empty[String, Int])((value: Int) => value)")
  }

  test("the optional adapters have no counterpart on Collections") {
    assertDoesNotCompile("Collections.inOptional((value: Int) => value + 1)")
    assertDoesNotCompile("Collections.filteringOptional(List(Option(1)))")
    assertDoesNotCompile("Collections.tryCatchToOptional(() => 1)")
  }

  test("no collector of any shape has a counterpart on Collections") {
    // The collectors of the original built an immutable collection of a third-party library
    // from a stream. Every collection of the standard library already converts to every other,
    // so there is no member here that collects into one - by any name.
    assertDoesNotCompile("Collections.toList(List(1))")
    assertDoesNotCompile("Collections.toSet(List(1))")
    assertDoesNotCompile("Collections.toSortedSet(List(1))")
    assertDoesNotCompile("Collections.toMap(List(1))((value: Int) => value)")
    assertDoesNotCompile("Collections.toListMap(List(1))((value: Int) => value)")
    assertDoesNotCompile("Collections.toGroupedMap(List(1))((value: Int) => value)")
  }

  test("the members that conflated an empty collection with an ambiguous one have no counterpart") {
    // Both of them answered an empty optional for an empty collection and for a collection of
    // several elements alike. `ensureOnlyOne` distinguishes the two, which every call site of
    // this port needs, so neither member was ported.
    assertDoesNotCompile("Collections.only(List(1))")
    assertDoesNotCompile("Collections.toOnly(List(1))")
  }

  test("the collection and stream factories have no counterpart on Collections") {
    assertDoesNotCompile("Collections.list(1, 2, 3)")
    assertDoesNotCompile("Collections.stream(List(1))")
    assertDoesNotCompile("Collections.combineMaps(Map.empty[String, Int], Map.empty[String, Int])")
    assertDoesNotCompile("Collections.concatToList(List(1), List(2))")
  }

  //-------------------------------------------------------------------------
  // The expressions that replace the members that were not ported

  test("collecting to an immutable list is toList on any collection or iterator") {
    List("a", "ab", "b").iterator.filter(string => lengthOf(string) == 1).toList shouldBe List("a", "b")
    Vector("a", "b").toList shouldBe List("a", "b")
    Set("a").toList shouldBe List("a")
    val collected: List[String] = List("b", "a").sorted
    collected shouldBe List("a", "b")
  }

  test("collecting to an immutable set is toSet, and to a sorted set is to(SortedSet)") {
    List("b", "a", "b").toSet shouldBe Set("a", "b")
    val sorted: SortedSet[String] = List("b", "a", "b").to(SortedSet)
    sorted.toList shouldBe List("a", "b")
    Vector(3, 1, 2).to(SortedSet).toList shouldBe List(1, 2, 3)
  }

  test("a list of known elements is the standard library list literal") {
    val listed: List[String] = List("a", "b", "c")
    listed shouldBe List("a", "b", "c")
    listed.size shouldBe 3
    List.empty[String] shouldBe List()
  }

  test("mapping inside an optional is map, and dropping the absent values of many is flatten") {
    Option("a").map(marked) shouldBe Some("!a")
    Option.empty[String].map(marked) shouldBe None
    List(Option("a"), Option.empty[String], Option("b")).flatten shouldBe List("a", "b")
    List(Option.empty[String]).flatten shouldBe List.empty[String]
  }

  test("turning a thrown exception into an absent value is Try") {
    // The wrapper being replaced ran a block and answered an empty optional if it threw.
    Try("12".toInt).toOption shouldBe Some(12)
    Try("x".toInt).toOption shouldBe None
    Try(List.empty[Int].head).toOption shouldBe None
  }

  test("the operations of the entry view are the operations of an immutable map") {
    val rates: Map[String, Int] = Map("a" -> 1, "b" -> 2, "c" -> 3)
    rates.view.mapValues(rate => rate * 10).toMap shouldBe Map("a" -> 10, "b" -> 20, "c" -> 30)
    rates.filter { case (key, _) => key == "a" } shouldBe Map("a" -> 1)
    rates.filter { case (_, rate) => rate > 1 }.keys.toList.sorted shouldBe List("b", "c")
    rates.map { case (key, rate) => s"$key$rate" }.toList.sorted shouldBe List("a1", "b2", "c3")
    rates.keys.toList.sorted shouldBe List("a", "b", "c")
    rates.values.toList.sorted shouldBe List(1, 2, 3)
  }

  test("the grouping collectors of the original are the ordered grouping of Collections") {
    // The one use the dependent module made of a grouping collector was grouping that had to
    // keep the order of its input, which is the member this spec covers above.
    val grouped: ListMap[Int, NonEmptyList[String]] =
      Collections.groupByPreservingOrder(GroupingInput)(lengthOf)
    val asMapOfLists: Map[Int, List[String]] =
      grouped.iterator.map { case (key, group) => (key, group.toList) }.toMap
    asMapOfLists shouldBe Map(1 -> List("a", "b", "c", "a"), 2 -> List("ab", "bb"))
  }

  //-------------------------------------------------------------------------
  // The cases of the two Java test classes that map onto this spec, under the names the
  // test-mapping manifest of this port points at.
  //
  // Traceability in this port is by test method: every method of every Java test class has a
  // row in the manifest naming either the test that carries it over or the reason it does not,
  // and a row that names one must name a test that exists. The eight rows that point at this
  // spec use the names below, so each of them is a test of its own here, holding the case of
  // the Java method exactly as that method wrote it - the same inputs, the same expectations,
  // and the outcome translated from a thrown exception to a returned failure. The sections
  // above cover the same members more thoroughly; these are the anchors that make the
  // mapping checkable, and they are written to be read against the Java methods they name.
  //
  // The names are quoted from the manifest and cannot be chosen here: the check matches them
  // literally against the test report. They are the only place in this spec where the name of
  // an original class appears, and they name a test of this port - nothing of the library the
  // original depended on is referenced by this file, in code, in a name or in a comment.

  test("Guavate_test_ensureOnlyOne") {
    val none: FailureOr[Option[String]] = Collections.ensureOnlyOne(List.empty[String])
    none should haveValue(None)
    val one: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a"))
    one should haveValue(Some("a"))
    // The Java case asserts that an IllegalArgumentException is thrown here; the port returns
    // the same condition as a failure, because it depends on the caller's data.
    val two: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a", "b"))
    two should beFailureWith(FailureReason.INVALID)
    failureOf(two).map(_.message) shouldBe
      Some("Multiple values found where only one was expected: a and b")
  }

  test("Guavate_test_ensureOnlyOne_withCustomMessage") {
    val date = LocalDate.of(2024, 4, 24)
    val message = s"Expected one letter but found multiple for date $date"
    val none: FailureOr[Option[String]] = Collections.ensureOnlyOne(List.empty[String], message)
    none should haveValue(None)
    val one: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a"), message)
    one should haveValue(Some("a"))
    val two: FailureOr[Option[String]] = Collections.ensureOnlyOne(List("a", "b"), message)
    two should beFailureWith(FailureReason.INVALID)
    failureOf(two).map(_.message) shouldBe
      Some("Expected one letter but found multiple for date 2024-04-24")
  }

  test("Guavate_test_toImmutableSortedMap_key") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(List("bob", "a", "ab"), lengthOf)
    built should haveValue(SortedMap(1 -> "a", 2 -> "ab", 3 -> "bob"))
    built.map(_.iterator.toList) shouldBe Right(List(1 -> "a", 2 -> "ab", 3 -> "bob"))
  }

  test("Guavate_test_toImmutableSortedMap_key_duplicateKeys") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(List("a", "ab", "c", "bb", "b", "a"), lengthOf)
    built should beFailureWith(FailureReason.INVALID)
    failureOf(built).map(_.message) shouldBe Some("Multiple entries found with the same key: 1")
  }

  test("Guavate_test_toImmutableSortedMap_keyValue") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(List("bob", "a", "ab"), lengthOf, marked)
    built should haveValue(SortedMap(1 -> "!a", 2 -> "!ab", 3 -> "!bob"))
    built.map(_.iterator.toList) shouldBe Right(List(1 -> "!a", 2 -> "!ab", 3 -> "!bob"))
  }

  test("Guavate_test_toImmutableSortedMap_keyValue_duplicateKeys") {
    val built: FailureOr[SortedMap[Int, String]] =
      Collections.toSortedMap(List("a", "ab", "c", "bb", "b", "a"), lengthOf, marked)
    built should beFailureWith(FailureReason.INVALID)
    failureOf(built).map(_.message) shouldBe Some("Multiple entries found with the same key: 1")
  }

  test("Guavate_test_toImmutableSortedMap_keyValue_duplicateKeys_merge") {
    val built: SortedMap[Int, String] =
      Collections.toSortedMap(List("a", "ab", "c", "bb", "b", "a"), lengthOf, marked, concatenated)
    built shouldBe SortedMap(1 -> "!a!c!b!a", 2 -> "!ab!bb")
  }

  test("MapStream_toMapGroupingRetainsOrder") {
    // The Java case shortens the keys of a map, sums the values that collide, and asserts the
    // entries of the result in order. The entry view it went through has no counterpart, so
    // the port reads the map as its entries, groups them with the order-preserving member and
    // reduces each group - and the order the Java case asserted is the order of the result.
    val entries: List[(String, Int)] = List("d" -> 1, "dd" -> 2, "b" -> 10, "bb" -> 20, "c" -> 1)
    val grouped: ListMap[String, NonEmptyList[(String, Int)]] =
      Collections.groupByPreservingOrder(entries) { case (key, _) => key.substring(0, 1) }
    val summed: List[(String, Int)] =
      grouped.iterator.map { case (key, group) => (key, group.toList.map { case (_, value) => value }.sum) }.toList
    summed shouldBe List("d" -> 3, "b" -> 30, "c" -> 1)
    summed.toMap shouldBe Map("d" -> 3, "b" -> 30, "c" -> 1)
  }
}


/**
 * Provides the key type of [[CollectionsSpec]], which the spec keeps out of its own class body.
 *
 * A case class nested in a class carries a reference to the instance that declared it, which
 * makes the pattern match of its generated `unapply` impossible to check completely at run
 * time. Declaring it here, in the companion, removes that reference: the type belongs to the
 * spec by name alone and is visible nowhere else in the module.
 */
private object CollectionsSpec {

  /**
   * A key type that publishes a standard library ordering and deliberately no `cats.Order`.
   *
   * Every key type of the library publishes an `Order` of its own, so only a type introduced
   * here can exercise the route documented for a key type that does not - an order derived
   * from its ordering, supplied at the call site.
   *
   * @param value  the text of the code
   */
  final case class Code(value: String)

  /**
   * Provides the ordering of [[Code]].
   *
   * The ordering is a plain value rather than an implicit one, so the case using it has to
   * name it - which is the point: nothing about [[Code]] reaches a sorted map by itself.
   */
  object Code {

    /** Orders codes by their text. */
    val ordering: Ordering[Code] = Ordering.by[Code, String](code => code.value)
  }
}
