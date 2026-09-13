/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor6

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[PriceIndex]].
 *
 * A price index is the most regular of the four index families: nine published members, nine
 * constants covering them exactly, every one active and published monthly, and no alternate name
 * at all, the alternate-name section of the published data being commented out in full. That
 * complete cover of the data by the constants is false of the Ibor, Overnight and exchange-rate
 * families, whose constants name a subset of their rows, which is why it is asserted here.
 */
class PriceIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The nine published indices, each paired with the name it renders as and is looked up by, in
   * the declaration order of the published data. The rows are written out rather than derived
   * from `PriceIndex.values`, so the table and the family's enumeration are two independent
   * statements of the same nine members; `test_extendedEnum` compares them.
   */
  private val dataName: TableFor2[PriceIndex, String] = Table(
    ("index", "name"),
    (PriceIndices.GB_HICP, "GB-HICP"),
    (PriceIndices.GB_RPI, "GB-RPI"),
    (PriceIndices.GB_RPIX, "GB-RPIX"),
    (PriceIndices.CH_CPI, "CH-CPI"),
    (PriceIndices.EU_AI_CPI, "EU-AI-CPI"),
    (PriceIndices.EU_EXT_CPI, "EU-EXT-CPI"),
    (PriceIndices.JP_CPI_EXF, "JP-CPI-EXF"),
    (PriceIndices.US_CPI_U, "US-CPI-U"),
    (PriceIndices.FR_EXT_CPI, "FR-EXT-CPI")
  )

  /**
   * The five fields the published data maps onto each member, row for row. The expectations are
   * transcribed from the published reference data, whose header reads
   * `Name,Currency,Country,Active,Publication Frequency`, rather than from the `PriceIndexData`
   * object of this module; the `Country` column of the data is the `region` of the index.
   */
  private val dataFields
      : TableFor6[PriceIndex, String, Currency, Country, Boolean, Frequency] = Table(
    ("index", "name", "currency", "region", "active", "publicationFrequency"),
    (PriceIndices.GB_HICP, "GB-HICP", Currency.GBP, Country.GB, true, Frequency.P1M),
    (PriceIndices.GB_RPI, "GB-RPI", Currency.GBP, Country.GB, true, Frequency.P1M),
    (PriceIndices.GB_RPIX, "GB-RPIX", Currency.GBP, Country.GB, true, Frequency.P1M),
    (PriceIndices.CH_CPI, "CH-CPI", Currency.CHF, Country.CH, true, Frequency.P1M),
    (PriceIndices.EU_AI_CPI, "EU-AI-CPI", Currency.EUR, Country.EU, true, Frequency.P1M),
    (PriceIndices.EU_EXT_CPI, "EU-EXT-CPI", Currency.EUR, Country.EU, true, Frequency.P1M),
    (PriceIndices.JP_CPI_EXF, "JP-CPI-EXF", Currency.JPY, Country.JP, true, Frequency.P1M),
    (PriceIndices.US_CPI_U, "US-CPI-U", Currency.USD, Country.US, true, Frequency.P1M),
    (PriceIndices.FR_EXT_CPI, "FR-EXT-CPI", Currency.EUR, Country.FR, true, Frequency.P1M)
  )

  //-------------------------------------------------------------------------
  test("test_gbpHicp") {
    // The index is reached by name rather than through its constant, so what is asserted is the
    // lookup answering with a fully populated index, through both of its entry points.
    val test: PriceIndex =
      PriceIndex.valueOf("GB-HICP").getOrElse(fail("the price index family publishes no GB-HICP"))
    PriceIndex.parse("GB-HICP") should haveValue(test)

    test.name shouldBe "GB-HICP"
    test.currency shouldBe Currency.GBP
    test.region shouldBe Country.GB
    test.active shouldBe true
    test.publicationFrequency shouldBe Frequency.P1M
    FloatingRateName.valueOf("GB-HICP") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "GB-HICP"
  }

  test("test_rowMappedFields") {
    // A name alone is not enough: a mapping that carried one row's currency, region, active flag
    // or publication frequency across to another would answer every name with the right member
    // and still be wrong about what it is, so all nine rows are asserted one at a time.
    forEvery(dataFields) {
      (index: PriceIndex,
          name: String,
          currency: Currency,
          region: Country,
          active: Boolean,
          publicationFrequency: Frequency) =>
        withClue(s"$name: ") {
          index.name shouldBe name
          index.currency shouldBe currency
          index.region shouldBe region
          index.active shouldBe active
          index.publicationFrequency shouldBe publicationFrequency

          val looked: PriceIndex = PriceIndex
            .valueOf(name)
            .getOrElse(fail(s"the price index family publishes no $name"))
          looked shouldBe index
          looked.name shouldBe name
          looked.currency shouldBe currency
          looked.region shouldBe region
          looked.active shouldBe active
          looked.publicationFrequency shouldBe publicationFrequency
        }
    }

    val all: List[PriceIndex] = PriceIndex.values.toList
    val expectedNames: List[String] = dataFields.toList.map { case (_, name, _, _, _, _) => name }
    all.map(_.name) shouldBe expectedNames

    all.map(_.currency).distinct should have size 5
    all.map(_.region).distinct should have size 6
    all.map(_.active).distinct shouldBe List(true)
    all.map(_.publicationFrequency).distinct shouldBe List(Frequency.P1M)
  }

  test("test_getFloatingRateName") {
    // For a price index the floating rate family is the WHOLE index name: one level is published
    // rather than one figure per tenor, so there is no tenor suffix to strip. The Ibor family's
    // own spec asserts the stripped form.
    val all: List[PriceIndex] = PriceIndex.values.toList
    all.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRateName.valueOf(index.name) shouldBe Some(index.floatingRateName)
        index.floatingRateName.name shouldBe index.name
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // `valueOf` is the exact lookup and `parse` reports text naming no member; both are asserted
    // over every row, so the two entry points answer the same inputs.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      withClue(s"$name: ") {
        PriceIndex.valueOf(name) shouldBe Some(index)
        PriceIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // Which members this family has: each name of the table resolves to the index it is paired
    // with, and the enumeration is exactly nine distinct members, named as the constants are.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      withClue(s"$name: ") {
        PriceIndex.valueOf(name) shouldBe Some(index)
      }
    }

    val all: List[PriceIndex] = PriceIndex.values.toList
    all should have size 9
    all.distinct should have size 9
    all.map(_.name).distinct should have size 9

    val rows: List[(PriceIndex, String)] = dataName.toList
    val constants: List[PriceIndex] = rows.map { case (index, _) => index }
    val providedNames: List[String] = rows.map { case (_, name) => name }
    all.map(_.name).toSet shouldBe constants.map(_.name).toSet
    all.map(_.name) shouldBe providedNames
  }

  test("test_of_lookup_notFound") {
    // Text naming no member is reported as a value, and the reason is compared rather than the
    // message, so the diagnostic wording stays free to change.
    PriceIndex.valueOf("Rubbish") shouldBe None
    PriceIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)

    // These spellings are not inert: `UK-RPI` and its siblings are live alternate spellings of a
    // FloatingRateName, and this family declares no alternate name of its own.
    val commentedOutAliases: List[String] =
      List("UK-HICP", "UK-RPI", "UK-RPIX", "SWF-CPI", "EUR-AI-CPI", "JPY-CPI-EXF", "USA-CPI-U")
    commentedOutAliases.foreach { alias =>
      withClue(s"$alias: ") {
        PriceIndex.valueOf(alias) shouldBe None
        PriceIndex.parse(alias) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  test("test_of_lookup_null") {
    // The absent name in the two spellings a lookup that takes a name can be given: the empty
    // name and a blank one, neither of which names a member.
    PriceIndex.valueOf("") shouldBe None
    PriceIndex.parse("") should beFailureWith(FailureReason.PARSING)
    PriceIndex.valueOf("   ") shouldBe None
    PriceIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  test("test_gb_rpi") {
    // The two day counts are fixed by the type rather than supplied by the published data: the
    // one-to-one convention, and a fixed leg defaulting to the convention of the index itself.
    PriceIndices.GB_RPI.currency shouldBe Currency.GBP
    PriceIndices.GB_RPI.dayCount shouldBe DayCounts.ONE_ONE
    PriceIndices.GB_RPI.defaultFixedLegDayCount shouldBe DayCounts.ONE_ONE
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The directory and the family agree: the nine constants are distinct values, and each is the
    // value the family answers with for its own name through both entry points of the lookup.
    val constants: List[PriceIndex] = dataName.toList.map { case (index, _) => index }
    constants should have size 9
    constants.distinct should have size 9
    constants.foreach { index =>
      withClue(s"${index.name}: ") {
        PriceIndex.valueOf(index.name) shouldBe Some(index)
        PriceIndex.parse(index.name) should haveValue(index)
        Show[PriceIndex].show(index) shouldBe index.name
        index.toString shouldBe index.name
      }
    }

    // Equality over two distinct configured members. The companion publishes one equality-bearing
    // implicit, an ordering that is also a hashing, which is what the assertions below read.
    val left: PriceIndex = PriceIndices.US_CPI_U
    val right: PriceIndex = PriceIndices.GB_RPI
    left should not be right
    Eq[PriceIndex].eqv(left, right) shouldBe false
    Hash[PriceIndex].eqv(left, right) shouldBe false
    Hash[PriceIndex].hash(left) should not be Hash[PriceIndex].hash(right)
    Show[PriceIndex].show(left) shouldBe "US-CPI-U"
    Show[PriceIndex].show(right) shouldBe "GB-RPI"
    Order[PriceIndex].compare(left, right) should not be 0

    val all: List[PriceIndex] = PriceIndex.values.toList
    for (first <- all; second <- all) {
      val sameValue = first == second
      withClue(s"${first.name} against ${second.name}: ") {
        Eq[PriceIndex].eqv(first, second) shouldBe sameValue
        Hash[PriceIndex].eqv(first, second) shouldBe sameValue
        (Order[PriceIndex].compare(first, second) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[PriceIndex].hash(first) shouldBe Hash[PriceIndex].hash(second)
        } else {
          Order[PriceIndex].compare(first, second) should not be 0
        }
      }
    }
  }

  test("test_jodaConvert") {
    // The text round trip: an index renders as its name, and that rendering reads back as the
    // same index. The JSON round trip is `test_serialization` below.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      val rendered = Show[PriceIndex].show(index)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe index.name
        PriceIndex.parse(rendered) should haveValue(index)
      }
    }
  }

  test("test_serialization") {
    // The JSON shape required of this family: an index is written as the bare string of its name
    // and never as an object. The sweep over every codec-bearing type is json.JsonRoundTripSpec.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      val encoded = index.asJson
      withClue(s"$name: ") {
        encoded shouldBe Json.fromString(name)
        encoded.as[PriceIndex] shouldBe Right(index)
      }
    }
    Json.fromString("Rubbish").as[PriceIndex].isLeft shouldBe true
    Json.fromString("UK-RPI").as[PriceIndex].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("US-CPI-U")).as[PriceIndex].isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // The table the family is created from, `PriceIndexData`, is asserted below: distinct names, a
  // derived lookup agreeing with the rows, and two properties of the data as it stands.
  //-------------------------------------------------------------------------
  test("data_names_areDistinct") {
    PriceIndexData.rows should have size 9
    PriceIndexData.rows.map(_.name).distinct should have size 9
  }

  test("data_byName_holdsEveryRowUnderItsCanonicalNameOnly") {
    PriceIndexData.byName should have size 9
    PriceIndexData.rows.foreach { row =>
      withClue(s"${row.name}: ") {
        PriceIndexData.byName.get(row.name) shouldBe Some(row)
      }
    }
    PriceIndexData.byName.keySet shouldBe PriceIndexData.rows.map(_.name).toSet

    // The keys are the canonical names alone; a case-insensitive lookup is the named enum
    // support's responsibility, so a folded name is absent here.
    PriceIndexData.byName.get("gb-rpi") shouldBe None
    PriceIndexData.byName.get("GB_RPI") shouldBe None
    PriceIndexData.byName.get("Rubbish") shouldBe None
  }

  test("data_everyIndexIsActiveAndPublishedMonthly") {
    // Both are properties of the data as it stands rather than constraints on the row type, which
    // admits a discontinued index and any publication frequency.
    PriceIndexData.rows.forall(_.active) shouldBe true
    PriceIndexData.rows.map(_.publicationFrequency).distinct shouldBe Vector(Frequency.P1M)
  }

  test("data_regionsAndCurrencies") {
    PriceIndexData.rows.filter(_.currency == Currency.GBP).map(_.name) shouldBe
      Vector("GB-HICP", "GB-RPI", "GB-RPIX")
    PriceIndexData.rows.filter(_.region == Country.EU).map(_.name) shouldBe Vector("EU-AI-CPI", "EU-EXT-CPI")

    // The French index is the one row whose region is not the region of its currency - euro,
    // France - which makes it the row a transcription is most easily wrong about.
    PriceIndexData.byName.get("FR-EXT-CPI").map(row => (row.currency, row.region)) shouldBe
      Some((Currency.EUR, Country.FR))
    PriceIndexData.rows.count(row => row.currency == Currency.EUR) shouldBe 3
  }

  test("data_iterationOrderIsStable") {
    // Both are immutable values built once, so every read of them traverses the same elements in
    // the same order within this process: `rows` in the declared order of the table, `byName` in
    // the iteration order of that single map. Nothing here observes a second process.
    PriceIndexData.rows shouldBe PriceIndexData.rows
    PriceIndexData.byName.toVector shouldBe PriceIndexData.byName.toVector
  }
}
