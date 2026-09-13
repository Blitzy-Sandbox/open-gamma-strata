/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[IborIndexObservation]], ported from the Java `IborIndexObservationTest`.
 *
 * The original held three test methods and this suite holds three, each under the name the
 * original gave it, because the migration is traced method by method and a Java test method and a
 * test of this suite are joined on the pair of suite class and test name. Every value the original
 * asserted is kept - the fixing date `2016-02-18` of `USD-LIBOR-3M` with the effective date
 * `2016-02-22` and the maturity date `2016-05-23` it implies, and the two `GBP-LIBOR` subjects of
 * the coverage and serialization methods. What changed is how each is asserted, and each change is
 * stated here once rather than argued again in every test.
 *
 * ===The direct constructor is replaced by field assertions===
 *
 * `test_of` built the value it expected with the constructor of the bean, `new
 * IborIndexObservation(index, fixingDate, effectiveDate, maturityDate, yearFraction)`, which the
 * generated bean declared package-scoped and the Java test could therefore reach because it sat in
 * the same package. The ported type is a validated value (AAP 0.3.3): it is a
 * `sealed abstract case class` with a private constructor, so it has no public `apply` and no
 * public `copy`, and the derived dates are not a caller's to supply at all. Comparing against a
 * hand-built expectation is therefore impossible by design, and the five fields are asserted one
 * by one instead. That is not a weaker claim than the original made - the equality of this type
 * compares exactly those five fields - and it is a more specific one, because a mismatch names the
 * field that is wrong rather than printing two whole values.
 *
 * The year fraction is expected twice over. Once from `index.dayCount.yearFraction(effectiveDate,
 * maturityDate)`, which is the call the Java test made of it and says the stored value is the one
 * the index derives; and once from the arithmetic the day count of the fixture prescribes, stated
 * in this suite from the fixture and not read back out of the subject - `USD-LIBOR-3M` counts on
 * `Act/360`, and the actual days from `2016-02-22` to `2016-05-23` are 91, so the year fraction is
 * `91.0 / 360.0`. Neither comparison carries a tolerance and neither needs one: an actual/360 year
 * fraction is a count of days divided by 360, so the division the day count performs and the
 * division written here are the same double to the bit. The 1e-9 parity discipline of the request
 * (AAP Rule 2) governs the captured baseline fixtures under `parity/`, where a Java-produced
 * number is compared with a Scala-produced one.
 *
 * ===A failure the original could not express===
 *
 * The Java factory raised `ReferenceDataNotFoundException` when the fixing calendar of the index
 * was absent from the reference data, so the Java test asserted nothing about it. The ported
 * factory reports that as a value - `Failure.MissingData` in the left of an `Either` - and
 * `test_of` asserts it, which is the explicit-error-handling content of this file (AAP Rule 5).
 * Every outcome in this suite is read through the matchers of the ported test kit, so an
 * unexpected failure is reported with its reason and message instead of raising from a partial
 * accessor, and the reason is compared as a value of the closed family of reasons rather than by
 * matching text.
 *
 * ===The reflective coverage sweep is replaced by named assertions===
 *
 * `coverage` called the two reflective bean-coverage helpers of the Java test helper: one walked
 * every property of an immutable bean through its meta-bean, the other compared two beans property
 * by property. There is no meta-bean here and nothing in this port reads a class while the program
 * runs, so neither helper has a target - the retained test helper of the ported collect module has
 * five members and neither of these is among them. Their substance does have a target: they stood
 * for the claims that two instances built independently from equal parts are equal and hash
 * equally, that instances differing in a field are not equal - the hash contract running one way
 * only, so nothing is claimed of the hashes of two unequal instances - and that an instance
 * renders itself faithfully. Those claims are asserted directly, over the same two subjects the
 * Java call used, on the type's own members and on its two typeclass instances.
 *
 * The absence of a `copy` is proved there too, by compiling a snippet and requiring it to fail,
 * because it is the fact that makes the field assertions of `test_of` the faithful port rather
 * than a weakening. The wider proofs that no `apply` exists and that no subtype can be declared
 * outside the sealed family belong to the module's API-surface spec, which owns them for every
 * type at once; this suite proves only the fact it depends on.
 *
 * ===Java serialization is replaced by the JSON codec===
 *
 * `test_serialization` asserted a Java-serialization round trip. Java serialization is not part of
 * this port at all (AAP 0.2.2); the codec derived when [[IborIndexObservation]] is compiled is its
 * single serialized form, so the round trip asserted below is `decode(encode(x)) == x` over the
 * Java subject, together with the exact shape of the document (AAP 0.6.4). The property-based
 * sweep over every codec-bearing type of the module belongs to the module's JSON round-trip spec,
 * to which the migration manifest routes this method as well; what is not duplicated anywhere else
 * is the consistency check of this decoder - a document whose derived dates or year fraction
 * disagree with what the index derives is rejected - and this suite is its witness.
 *
 * ===What is specific to this observation===
 *
 * Two properties of this type differ from its two siblings and are easy to cross-contaminate, so
 * both are asserted here rather than assumed. Its maturity date is derived from its '''effective'''
 * date and not from its fixing date, which `test_of` pins with the two dates the Java test used.
 * And its equality spans all five fields, where `OvernightIndexObservation` and
 * `FxIndexObservation` reduce theirs to the index and the fixing date; the year fraction takes part
 * in it through `java.lang.Double.compare`, the bit-pattern comparison the bean performed on a
 * `double`, which `coverage` states explicitly.
 *
 * No ordering is asserted: the bean does not implement `Comparable` and the port declares no
 * `Order`. [[IndexObservation]] is a deliberately open trait in this port - sealing it would pull
 * its four implementations into its own file, Scala 2 admitting a subtype of a sealed type only
 * from the file that type is declared in, and would deny an application the observation of its own
 * that the interface being ported allows it to supply. This suite asserts nothing about that
 * openness: the module's API-surface spec carries an open-contract row for the trait, implements
 * it from outside its file and compiles a further implementation to show the openness is a
 * property of the trait rather than of that one host, and owns the property for every type of the
 * module at once.
 *
 * @see [[IborIndexObservation]] for the type under test
 */
final class IborIndexObservationSpec extends AnyFunSuite with Matchers {

  /**
   * The reference data every successful factory call of this suite resolves against, which is the
   * fixture the Java test declared and holds the built-in holiday calendars of the port.
   */
  private val RefData: ReferenceData = ReferenceData.standard

  /**
   * Reference data holding nothing, against which no fixing calendar can be resolved.
   *
   * This is the fixture of the failure the Java test had no way to assert: the factory of the
   * original raised when a calendar was absent, so its absence was an error rather than an
   * outcome, and there was nothing to compare. It is deliberately empty rather than merely
   * incomplete, because an empty store is the one input that is certain to lack the fixing
   * calendar of every index, whichever index a later reading of this suite chooses.
   */
  private val NoRefData: ReferenceData = ReferenceData.empty

  /** The fixing date of the `USD-LIBOR-3M` observation of `test_of`, as the Java test gave it. */
  private val UsdFixingDate: LocalDate = date(2016, 2, 18)

  /** The effective date the index derives from [[UsdFixingDate]], two business days later. */
  private val UsdEffectiveDate: LocalDate = date(2016, 2, 22)

  /** The maturity date the index derives from [[UsdEffectiveDate]], three months on. */
  private val UsdMaturityDate: LocalDate = date(2016, 5, 23)

  /**
   * The fixing date of the `GBP-LIBOR-3M` subject of `coverage` and `test_serialization`, which is
   * the one date the Java test used for both.
   */
  private val GbpFixingDate: LocalDate = date(2014, 6, 30)

  /** The fixing date of the second `coverage` subject, the `GBP-LIBOR-1M` one month later. */
  private val GbpSecondFixingDate: LocalDate = date(2014, 7, 30)

  /**
   * The document the `GBP-LIBOR-3M` observation of [[GbpFixingDate]] encodes to.
   *
   * An object holding the five fields under the names the bean declared and in the order the type
   * declares them: the index as its bare name, the three dates as the ISO-8601 strings of the
   * platform, and the year fraction as a JSON number - the form the double policy of this port
   * writes for a finite value. The effective date of a sterling LIBOR fixing is the fixing date
   * itself, the index having no effective offset, which is why two of the three dates are equal
   * here; the year fraction is the 92 days to the maturity date over the 365 of the `Act/365F` day
   * count of the index.
   */
  private val ExpectedJson: String =
    """{"index":"GBP-LIBOR-3M","fixingDate":"2014-06-30","effectiveDate":"2014-06-30",""" +
      """"maturityDate":"2014-09-30","yearFraction":0.25205479452054796}"""

  //-------------------------------------------------------------------------
  test("test_of") {
    val outcome: Either[Failure, IborIndexObservation] =
      IborIndexObservation.of(IborIndices.USD_LIBOR_3M, UsdFixingDate, RefData)
    outcome should beSuccess
    val test = unwrap(outcome)

    // The Java test compared the whole observation against one it built with the package-scoped
    // constructor of the bean. The ported type is validated (AAP 0.3.3) - a `sealed abstract case
    // class` with a private constructor, and so without a public `apply` or `copy` - so there is
    // no expectation to build, and its five fields are asserted one by one instead. The claim is
    // the same one, the equality of this type being over exactly these five fields, and a mismatch
    // now names the field rather than printing two whole values.
    test.index shouldBe IborIndices.USD_LIBOR_3M
    test.fixingDate shouldBe UsdFixingDate

    // The two derived dates, which are the substance of the factory: the effective date is two
    // business days after the fixing date, and the maturity date is three months after the
    // *effective* date. That the maturity date is derived from the effective date and not from the
    // fixing date is the property of this observation most easily lost in a port - the FX
    // observation derives its maturity date from its fixing date - so it is pinned here with the
    // two dates the Java test stated.
    test.effectiveDate shouldBe UsdEffectiveDate
    test.maturityDate shouldBe UsdMaturityDate
    val fromEffective =
      IborIndices.USD_LIBOR_3M.calculateMaturityFromEffective(UsdEffectiveDate, RefData)
    unwrap(fromEffective) shouldBe UsdMaturityDate

    // The year fraction, against the call the Java test made of it: this is the claim that the
    // stored value is the one the index derives from the two dates above, rather than a number
    // computed some other way and carried along.
    val expectedYearFraction: Double =
      IborIndices.USD_LIBOR_3M.dayCount.yearFraction(UsdEffectiveDate, UsdMaturityDate)
    test.yearFraction shouldBe expectedYearFraction

    // and against the arithmetic that day count prescribes, stated here rather than obtained from
    // the subject - which is what makes the assertion evidence and not a comparison of one
    // computation with itself. `USD-LIBOR-3M` counts days on `Act/360`, so the year fraction is
    // the actual days from the effective date to the maturity date over 360. The days are the 7
    // left in the leap February of 2016 after the 22nd, the 31 of March, the 30 of April and the
    // 23 of May to the maturity date: 91. The day span and the day count of the index are each
    // asserted as well, so the numerator and the divisor of the expectation are pinned and not
    // only the quotient. No tolerance is taken and none is needed: an actual/360 year fraction is
    // one division of a day count by 360, which is the division written below, so the two sides
    // are the same double to the bit.
    java.time.temporal.ChronoUnit.DAYS.between(UsdEffectiveDate, UsdMaturityDate) shouldBe 91L
    IborIndices.USD_LIBOR_3M.dayCount shouldBe DayCounts.ACT_360
    test.yearFraction shouldBe 91.0d / 360.0d

    // The currency of an observation is the currency of the index it observes, which is the last
    // fact the Java test asserted.
    test.currency shouldBe Currency.USD

    // The failure the original could not express. Its factory raised when the fixing calendar of
    // the index was absent from the reference data; this one reports it as a value, and reference
    // data holding nothing is the input that is certain to lack it. The reason is compared as a
    // member of the closed family of reasons - the same reason `HolidayCalendarId.resolve` reports
    // for an identifier the store does not hold - rather than by matching the message text
    // (AAP Rule 5).
    val missing: Either[Failure, IborIndexObservation] =
      IborIndexObservation.of(IborIndices.USD_LIBOR_3M, UsdFixingDate, NoRefData)
    missing should beFailureWith(FailureReason.MISSING_DATA)
    missing should not(beSuccess)
  }

  //-------------------------------------------------------------------------
  test("test_resolve") {
    // The batch route into the type, which the index being ported published as `resolve`: the
    // fixing calendar and both date offsets are resolved once and the function that comes back
    // observes any fixing without consulting reference data again. Two entry points reach it -
    // `IborIndexObservation.resolve`, where this port implements it, and `IborIndex.resolve`,
    // where the original declared it - and both are asserted to produce what the per-fixing
    // factory produces, field by field, because the equality of an observation reads the index
    // and the fixing date alone and would hide a wrongly derived date.
    val index = IborIndices.USD_LIBOR_3M
    val observe = unwrap(IborIndexObservation.resolve(index, RefData))
    val observeFromIndex = unwrap(index.resolve(RefData))

    List(UsdFixingDate, UsdFixingDate.plusDays(1L), UsdFixingDate.plusDays(2L), UsdFixingDate.plusMonths(1L))
      .foreach { fixingDate =>
        withClue(s"$fixingDate: ") {
          val direct = unwrap(IborIndexObservation.of(index, fixingDate, RefData))
          List(observe(fixingDate), observeFromIndex(fixingDate)).foreach { resolved =>
            resolved shouldBe direct
            resolved.index shouldBe index
            resolved.fixingDate shouldBe fixingDate
            resolved.effectiveDate shouldBe direct.effectiveDate
            resolved.maturityDate shouldBe direct.maturityDate
            resolved.yearFraction shouldBe direct.yearFraction
          }
        }
      }

    // A fixing date that is not a fixing date of the index is carried as given while the derived
    // dates follow from the fixing date the index would use - the same treatment the per-fixing
    // factory gives it, here through the resolved function.
    val saturday = UsdFixingDate.`with`(java.time.DayOfWeek.SATURDAY)
    saturday.getDayOfWeek shouldBe java.time.DayOfWeek.SATURDAY
    observe(saturday).fixingDate shouldBe saturday
    observe(saturday) shouldBe unwrap(IborIndexObservation.of(index, saturday, RefData))

    // Reference data that cannot supply the calendar is reported once, by the resolution itself,
    // rather than by each fixing - which is the reason the operation exists.
    IborIndexObservation.resolve(index, NoRefData) should beFailureWith(FailureReason.MISSING_DATA)
    index.resolve(NoRefData) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val first: Either[Failure, IborIndexObservation] =
      IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, GbpFixingDate, RefData)
    val second: Either[Failure, IborIndexObservation] =
      IborIndexObservation.of(IborIndices.GBP_LIBOR_1M, GbpSecondFixingDate, RefData)
    first should beSuccess
    second should beSuccess
    val test = unwrap(first)
    val test2 = unwrap(second)

    // An observation built independently from the same arguments is equal to the first - which is
    // what makes the equality structural rather than by reference - and hashes equally. `Hash` is
    // the type's single equality-bearing instance, so it is asserted to agree with the equality and
    // the hash of the platform. This replaces the first of the two reflective sweeps the Java test
    // called, which walked the properties of the bean through its meta-bean.
    val same = unwrap(IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, GbpFixingDate, RefData))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[IborIndexObservation].eqv(same, test) shouldBe true
    Hash[IborIndexObservation].hash(same) shouldBe test.hashCode

    // The pair of the Java coverage call is unequal, under the equality of the platform and under
    // the typeclass alike. This replaces the second sweep, which compared two beans property by
    // property. Nothing is asserted about the two hashes being different, because the hash
    // contract runs one way only: equal values must hash equally, which is what the pair above
    // asserts, while unequal values are permitted to collide. Requiring these two to hash apart
    // would assert a property the type does not promise and would fail this suite for a field or
    // a seed change that is no defect. What the unequal subject does owe is the agreement of its
    // `Hash` instance with its own hash, so that is what is asserted of it.
    test2 should not be test
    Hash[IborIndexObservation].eqv(test2, test) shouldBe false
    Hash[IborIndexObservation].hash(test2) shouldBe test2.hashCode

    // Each of the two subjects differs from the other in the index and in the fixing date at once,
    // so each field is varied on its own as well, which is what proves that all five participate
    // rather than only the one read first. Varying either of the two fields a caller supplies
    // varies the three derived from them too: the third observation here is of the same index one
    // month later, and the fourth of the one-month index on the same date, and both differ from the
    // first in every derived field as well as in the field varied.
    unwrap(IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, GbpSecondFixingDate, RefData)) should
      not be test
    unwrap(IborIndexObservation.of(IborIndices.GBP_LIBOR_1M, GbpFixingDate, RefData)) should
      not be test

    // The year fraction takes part in the equality of this type, which is what makes its equality
    // wider than that of its two siblings: `OvernightIndexObservation` and `FxIndexObservation`
    // reduce theirs to the index and the fixing date, because every other field of those two
    // follows from that pair alone, whereas a year fraction follows from the day count as well.
    // The comparison is `java.lang.Double.compare` and not `==`, which is the bit-pattern
    // comparison the bean performed on a `double`, so it holds for a value that is not a number and
    // separates the two signed zeroes. Neither of those two values is reachable through this
    // factory - a year fraction here is a count of days over a count of days - so what is asserted
    // is the comparison this type's equality is stated over, alongside the fact that the two
    // subjects do differ in their year fractions and therefore exercise it.
    java.lang.Double.compare(test.yearFraction, test2.yearFraction) should not be 0
    java.lang.Double.compare(same.yearFraction, test.yearFraction) shouldBe 0
    java.lang.Double.compare(Double.NaN, Double.NaN) shouldBe 0
    java.lang.Double.compare(0.0d, -0.0d) should not be 0

    // `Show` renders what `toString` renders, and what `toString` renders is the form the bean
    // generated - the five fields named in declaration order between braces, the index by its name
    // and each date in its ISO form. The literals are pinned so that a change to the rendering
    // cannot pass unnoticed, and the year fraction printed in the first of them is the one the day
    // count of the index computes.
    Show[IborIndexObservation].show(test) shouldBe test.toString
    Show[IborIndexObservation].show(test2) shouldBe test2.toString
    test.toString shouldBe
      "IborIndexObservation{index=GBP-LIBOR-3M, fixingDate=2014-06-30, " +
        "effectiveDate=2014-06-30, maturityDate=2014-09-30, yearFraction=0.25205479452054796}"
    test2.toString shouldBe
      "IborIndexObservation{index=GBP-LIBOR-1M, fixingDate=2014-07-30, " +
        "effectiveDate=2014-07-30, maturityDate=2014-08-29, yearFraction=0.0821917808219178}"

    // The fact the field assertions of `test_of` rest on: an observation has no `copy`, so no
    // route exists to one whose derived dates disagree with its index. It is proved by compiling a
    // snippet and requiring it to fail, with the same snippet and an accessor in place of the
    // missing member as the control that the failure is the missing `copy` and not a mistake in the
    // snippet. The wider proofs - that no `apply` exists, and that no subtype of a sealed family can
    // be declared outside its file - belong to the module's API-surface spec, which makes them for
    // every type at once.
    assertDoesNotCompile(
      """IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, date(2014, 6, 30), ReferenceData.standard)
           .map(observation => observation.copy(fixingDate = date(2014, 7, 30)))""")
    assertCompiles(
      """IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, date(2014, 6, 30), ReferenceData.standard)
           .map(observation => observation.fixingDate)""")
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    val test = unwrap(IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, GbpFixingDate, RefData))

    // The document, compared as a parsed document rather than as printed text: what is claimed is
    // the fields present, their names and their values, not the whitespace or the order a printer
    // happens to choose. Java serialization, which the original asserted a round trip of, is no
    // part of this port (AAP 0.2.2); the codec compiled with the type is its serialized form.
    test.asJson shouldBe json(ExpectedJson)

    // and the round trip, in both directions. An encoding that is right and a decoding that is
    // wrong would still round-trip if only the round trip were asserted, and the other way about.
    decode[IborIndexObservation](test.asJson.noSpaces) shouldBe Right(test)

    // The shape is pinned member by member as well, because the whole-document comparison above
    // would also be satisfied by a literal that had drifted with the codec: the five field names
    // are the ones the bean declared, in declaration order; the index is its bare name and not an
    // object of its own fields; each date is the ISO-8601 string of the platform; and the year
    // fraction is a JSON number whose value is the one the day count of the index computes.
    test.asJson.asObject.map(obj => obj.keys.toList) shouldBe
      Some(List("index", "fixingDate", "effectiveDate", "maturityDate", "yearFraction"))
    test.asJson.hcursor.get[String]("index") shouldBe Right("GBP-LIBOR-3M")
    test.asJson.hcursor.get[String]("fixingDate") shouldBe Right("2014-06-30")
    test.asJson.hcursor.get[String]("effectiveDate") shouldBe Right("2014-06-30")
    test.asJson.hcursor.get[String]("maturityDate") shouldBe Right("2014-09-30")
    test.asJson.hcursor.downField("yearFraction").focus.map(value => value.isNumber) shouldBe
      Some(true)
    test.asJson.hcursor.get[Double]("yearFraction") shouldBe
      Right(
        IborIndices.GBP_LIBOR_3M.dayCount.yearFraction(date(2014, 6, 30), date(2014, 9, 30)))

    // The consistency check of the decoder, which is why it rebuilds the observation from the two
    // fields a document really determines - the index and the fixing date - and compares the three
    // it derives against the three the document states. Each of the three is contradicted in turn
    // and each contradiction is rejected, naming the field it was found in. This is the only place
    // in the package where that check is exercised: the module's JSON round-trip spec sweeps
    // consistent documents of every codec-bearing type, so an inconsistent one has to be written
    // here by hand.
    val document = test.asJson
    val wrongEffective = Json.obj("effectiveDate" -> Json.fromString("2014-07-01"))
    val wrongMaturity = Json.obj("maturityDate" -> Json.fromString("2014-09-29"))
    val wrongYearFraction = Json.obj("yearFraction" -> Json.fromDoubleOrNull(0.25d))
    rejectionOf(document.deepMerge(wrongEffective)) should include("effectiveDate")
    rejectionOf(document.deepMerge(wrongMaturity)) should include("maturityDate")
    rejectionOf(document.deepMerge(wrongYearFraction)) should include("yearFraction")

    // All five fields are required, so a document holding only the two the rebuilding needs cannot
    // be read either - the derived fields are what the check above has to compare against, and a
    // document omitting them states nothing to check.
    json("""{"index":"GBP-LIBOR-3M","fixingDate":"2014-06-30"}""")
      .as[IborIndexObservation]
      .isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the observation out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite. It exists because the only public factory
   * of the type reports the failure of resolving reference data as a value rather than by raising,
   * while the Java test - whose factory raised - asserted on the observation directly. The outcome
   * is folded rather than opened by a partial accessor, so a fixture or a call that unexpectedly
   * fails is reported as a test failure naming the reason and the message instead of raising an
   * error from somewhere else in the suite.
   *
   * @tparam A  the type of the value the outcome is expected to carry
   * @param outcome  the outcome expected to carry a value
   * @return the value it carries
   */
  private def unwrap[A](outcome: Either[Failure, A]): A =
    outcome.fold(
      failure =>
        fail(s"Expected a value but the call failed with ${failure.reason}: ${failure.message}"),
      value => value)

  /**
   * Parses the expected JSON form of this suite into the JSON model.
   *
   * A literal in this file that is not itself valid JSON is a defect in the suite rather than a
   * failure of the subject, so it is reported as one.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text) match {
      case Right(document) => document
      case Left(failure) =>
        fail(s"the expected JSON of this spec is not itself valid JSON: $text (${failure.message})")
    }

  /**
   * Reads the reason a document was rejected, failing the test if it was accepted.
   *
   * The decoder of this type rebuilds an observation from the index and fixing date of a document
   * and rejects one whose derived fields disagree, naming each field that does. Asserting the
   * rejection and the field it names together is what makes the claim specific: a document that
   * failed to decode for an unrelated reason - a field of the wrong type, say - would satisfy a
   * bare test of rejection while proving nothing about the check.
   *
   * @param payload  the document expected to be rejected
   * @return the message of the rejection
   */
  private def rejectionOf(payload: Json): String =
    payload.as[IborIndexObservation] match {
      case Left(failure) => failure.message
      case Right(value) =>
        fail(s"Expected the document to be rejected but it decoded to: $value")
    }
}
