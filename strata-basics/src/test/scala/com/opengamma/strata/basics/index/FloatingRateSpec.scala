/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import scala.collection.mutable.ListBuffer

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1

import com.opengamma.strata.collect.result.FailureReason

/**
 * Test [[FloatingRate]].
 *
 * The Java sources have no test class for this abstraction: the union it declares is exercised
 * through the specs of its consumers, which are ported together with the index families those
 * specs assert against. So this spec is the port's own, and the test mapping manifest maps no
 * Java method to it.
 *
 * What it asserts is what the abstraction owns by itself, stated so that it holds both today and
 * once the index families carry members:
 *
 *  - the contract of the trait, exercised through an implementation declared in this spec;
 *  - the agreement of the three entry points with each other, so `parse`, `tryParse` and
 *    `tryParseWith` applied to the shipped composition can never answer differently;
 *  - the shape of that shipped composition - four probes, each of them a family's own exact
 *    lookup, in the documented order - asserted by comparing each probe with the family lookup
 *    it is required to be, which is a property of the composition rather than of any family's
 *    membership;
 *  - the search rule itself, over probes this spec supplies, including the laziness that keeps a
 *    family's companion from being loaded by a search that never consults it.
 *
 * Two things are deliberately not asserted here. Which family answers a given piece of text is a
 * property of the families and is asserted with them; and the wording of the failure a name that
 * resolves to nothing produces is not pinned, because the reason and the text that could not be
 * resolved are the contract while the diagnostic prose around them is not. At the milestone this
 * spec was written the four families are closed sets with no members, so no text resolves to a
 * floating rate at all; every assertion below is written to be indifferent to that, and none of
 * them has to be revisited when the families land.
 */
class FloatingRateSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * An implementation of the trait, which is what lets the contract of the trait be asserted
   * before any family has members.
   *
   * It is an implementation of [[FloatingRate]] and of nothing else: it is not a member of
   * either implementor family, which is the point - the trait is deliberately open, so a value
   * that is a floating rate without being an index or a published family identifier has to be
   * expressible.
   *
   * The family is supplied by name, because at the milestone this spec was written no family
   * identifier can be built - the published family is a closed set with no members yet - and the
   * rest of the contract can be asserted without one.
   *
   * @param name  the name this floating rate reports
   * @param family  the family this floating rate reports belonging to, evaluated when it is asked for
   */
  private final class ProbeFloatingRate(val name: String, family: => FloatingRateName) extends FloatingRate {

    override def floatingRateName: FloatingRateName = family
  }

  /**
   * Text the search is applied to: names of every shape the families use, names that are not,
   * and the hostile shapes a caller can arrive with.
   *
   * Every assertion that reads this table has to hold whatever the families currently answer,
   * which is what makes the table safe to state over real index names.
   */
  private val sampleText: TableFor1[String] =
    Table(
      "text",
      "GBP-LIBOR-3M",
      "GBP-LIBOR-BBA",
      "GBP-SONIA",
      "EUR-ESTER",
      "EUR-ESTR",
      "USD-SOFR",
      "GB-RPI",
      "US-CPI-U",
      "EUR/USD-ECB",
      "Rubbish",
      "",
      "   ",
      "gbp-libor-3m",
      "GBP-LIBOR-3M ",
      "\u0000",
      "\ud83d\ude00",
      "' OR 1=1 --")

  //-------------------------------------------------------------------------
  test("test_trait_reportsItsNameAndItsFamily") {
    // The trait asks for two things: the name, which it inherits from being a named value, and
    // the family the rate belongs to. An implementation supplying both is a floating rate and is
    // usable as one - including as the answer of a probe in the search, which is asserted here
    // because that is how a value of this trait actually reaches a caller.
    val rate: FloatingRate =
      new ProbeFloatingRate("GBP-LIBOR-3M", FloatingRateName.valueOf("GBP-LIBOR-BBA").get)
    rate.name shouldBe "GBP-LIBOR-3M"
    FloatingRate.tryParseWith(
      "GBP-LIBOR-3M",
      List((text: String) => Option.when(text == rate.name)(rate))) shouldBe Some(rate)

    // The family it reports is only evaluated when it is asked for. Once the published family
    // identifiers exist, asking is part of the contract, and so is the identity the family
    // itself satisfies: a family identifier is a floating rate that reports itself.
    FloatingRateName.valueOf("GBP-LIBOR-BBA") match {
      case Some(family) =>
        rate.floatingRateName shouldBe family
        family.floatingRateName shouldBe family
        (family: FloatingRate).name shouldBe family.name
      case None =>
        // At this milestone the family is a closed set with no members, so there is nothing to
        // report and asking raises rather than answering with a value that does not exist. The
        // assertion is that the family is genuinely empty, which is the state this branch
        // exists for; when the members land the branch above takes over with no edit here.
        FloatingRateName.valueOf("GBP-LIBOR-BBA") shouldBe None
        a[NoSuchElementException] should be thrownBy rate.floatingRateName
    }
  }

  test("test_entryPoints_agreeWithEachOther") {
    // The three entry points are one search reported three ways, so they can never disagree:
    // `tryParse` is the search applied to the shipped composition, and `parse` is `tryParse` with
    // the absent case turned into a failure.
    forAll(sampleText) { (text: String) =>
      withClue(s"'$text': ") {
        val found = FloatingRate.tryParse(text)
        found shouldBe FloatingRate.tryParseWith(text, FloatingRate.standardLookups)
        FloatingRate.parse(text).toOption shouldBe found
        FloatingRate.parse(text).isRight shouldBe found.isDefined
      }
    }
  }

  test("test_unknownTextIsReportedNotRaised") {
    // Where the interface being ported raised an error from `parse`, the port reports it as a
    // value: the reason is `PARSING`, and the message quotes the text that resolved to nothing.
    // The reason and the quoted text are asserted; the prose around them is not, because it is
    // diagnostic rather than contract.
    FloatingRate.tryParse("Rubbish") shouldBe None
    FloatingRate.parse("Rubbish") match {
      case Left(failure) =>
        failure.reason shouldBe FailureReason.PARSING
        failure.message should include("Rubbish")
      case Right(floatingRate) =>
        fail(s"expected a parsing failure, found $floatingRate")
    }
  }

  test("test_hostileTextIsAnswered_notRaised") {
    // The search is total in its input. Every shape below is text a caller can arrive with, and
    // each is answered - with a value or with a failure - rather than raising, including the
    // payload long enough to matter.
    val hostile: List[String] =
      List("", "   ", "\u0000", "\n", "\t", "\ud83d\ude00", "' OR 1=1 --", "../../etc/passwd", "%s%n{}", "A" * 10000)

    hostile.foreach { text =>
      withClue(s"length ${text.length}: ") {
        val found = FloatingRate.tryParse(text)
        FloatingRate.parse(text).toOption shouldBe found
        if (found.isEmpty) {
          FloatingRate.parse(text).left.map(_.reason) shouldBe Left(FailureReason.PARSING)
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_standardLookups_isTheFourFamilyProbesInOrder") {
    // The shipped composition is four probes, and each is required to be one family's own exact
    // lookup, in the documented order: Ibor index, Overnight index, Price index, then floating
    // rate name. That requirement is asserted as an identity between each probe and the family
    // lookup it has to be, over every piece of text in the table - which pins the order without
    // reading any family's membership, and so holds equally before and after the families carry
    // members.
    val probes = FloatingRate.standardLookups.toList
    probes should have size 4

    forAll(sampleText) { (text: String) =>
      withClue(s"'$text': ") {
        probes.head.apply(text) shouldBe IborIndex.valueOf(text)
        probes(1).apply(text) shouldBe OvernightIndex.valueOf(text)
        probes(2).apply(text) shouldBe PriceIndex.valueOf(text)
        probes(3).apply(text) shouldBe FloatingRateName.valueOf(text)
      }
    }
  }

  test("test_standardLookups_isProducedFreshlyOnEveryCall") {
    // The composition has to stay a method whose elements are supplied by name. A value holding
    // the four lookups pre-assembled would force all four companions the first time anything
    // parsed, and the families and this object depend on each other, so that would reintroduce
    // an initialization cycle whose symptom depends on which class a program happens to touch
    // first. Being a method is observable, and asserted here, because the property is easy to
    // lose in a refactor and expensive to diagnose afterwards.
    val first = FloatingRate.standardLookups
    val second = FloatingRate.standardLookups
    first should not be theSameInstanceAs(second)
    first should have size 4
    second should have size 4
    // a subset composes, which is how a caller searches fewer families without reimplementing
    // the search
    FloatingRate.standardLookups.take(3) should have size 3
    val indexFamiliesOnly: List[FloatingRate.Lookup] =
      List(
        (name: String) => IborIndex.valueOf(name),
        (name: String) => OvernightIndex.valueOf(name),
        (name: String) => PriceIndex.valueOf(name))
    FloatingRate.tryParseWith("GBP-LIBOR-3M", FloatingRate.standardLookups.take(3)) shouldBe
      FloatingRate.tryParseWith("GBP-LIBOR-3M", indexFamiliesOnly)
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a probe that records when it is produced and when it is applied.
   *
   * The production entry is what makes laziness observable: the elements of a composition may be
   * supplied by name, and a search that stops early must never produce the probes after the one
   * that answered.
   *
   * @param label  the name this probe records itself under
   * @param claims  the text this probe answers for
   * @param log  the record of production and application, in the order they happened
   * @return the probe
   */
  private def recordingProbe(
      label: String,
      claims: Set[String],
      log: ListBuffer[String]): String => Option[String] = {
    log += s"produced $label"
    text => {
      log += s"probed $label"
      if (claims.contains(text)) Some(s"$label:$text") else None
    }
  }

  /**
   * A three-probe composition in the shape of the shipped one: by-name elements, and two probes
   * claiming one piece of text so that precedence is observable.
   *
   * @param log  the record the probes write to
   * @return the composition
   */
  private def recordingComposition(log: ListBuffer[String]): LazyList[String => Option[String]] =
    recordingProbe("first", Set("a", "shared"), log) #::
      recordingProbe("second", Set("b", "shared"), log) #::
      recordingProbe("third", Set("c"), log) #::
      LazyList.empty[String => Option[String]]

  test("test_search_triesProbesInOrderAndStopsAtTheFirstHit") {
    val log = ListBuffer.empty[String]
    val composition = recordingComposition(log)
    // obtaining the composition produces no probe at all
    log.toList shouldBe empty

    FloatingRate.tryParseWith("b", composition) shouldBe Some("second:b")
    // the first probe was produced and applied, then the second, which answered; the third was
    // never produced, so a family behind it would not have been loaded
    log.toList shouldBe List("produced first", "probed first", "produced second", "probed second")
  }

  test("test_search_earlierProbeClaimsSharedText") {
    // The property the fixed probe order exists for. The name spaces of the families overlap -
    // `GB-RPI` names both a price index and a published family identifier - and the order is what
    // decides which of the two a caller gets.
    val log = ListBuffer.empty[String]
    FloatingRate.tryParseWith("shared", recordingComposition(log)) shouldBe Some("first:shared")
    log.toList shouldBe List("produced first", "probed first")
  }

  test("test_search_consultsEveryProbeOnAMiss") {
    val log = ListBuffer.empty[String]
    FloatingRate.tryParseWith("Rubbish", recordingComposition(log)) shouldBe None
    log.toList shouldBe List(
      "produced first",
      "probed first",
      "produced second",
      "probed second",
      "produced third",
      "probed third")
  }

  test("test_search_isTotalInTheCompositionItIsGiven") {
    // including the empty one, and whatever ordered sequence a caller has to hand
    FloatingRate.tryParseWith("a", LazyList.empty[FloatingRate.Lookup]) shouldBe None
    FloatingRate.tryParseWith("a", List.empty[FloatingRate.Lookup]) shouldBe None
    FloatingRate.tryParseWith("a", Vector.empty[FloatingRate.Lookup]) shouldBe None

    val log = ListBuffer.empty[String]
    val strict: List[String => Option[String]] =
      List(recordingProbe("only", Set("a"), log), recordingProbe("unreached", Set("a"), log))
    FloatingRate.tryParseWith("a", strict) shouldBe Some("only:a")
    // a strict sequence produces both probes when it is built, but the search still applies only
    // the first
    log.toList.filter(_.startsWith("probed")) shouldBe List("probed only")
  }

  test("test_search_isStatedOverAnyValueAProbeAnswersWith") {
    // The rule has nothing to do with what a floating rate is, and the operation says so: it is
    // stated over the value type of the probes, which is what lets it be read and tested with no
    // family involved. A caller composing probes over its own type is served by the same search.
    val numeric: List[String => Option[Int]] =
      List(text => Option.when(text == "one")(1), text => Option.when(text.nonEmpty)(text.length))
    FloatingRate.tryParseWith("one", numeric) shouldBe Some(1)
    FloatingRate.tryParseWith("four", numeric) shouldBe Some(4)
    FloatingRate.tryParseWith("", numeric) shouldBe None
  }
}
