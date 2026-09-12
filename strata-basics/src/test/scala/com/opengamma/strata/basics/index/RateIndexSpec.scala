/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[RateIndex]].
 *
 * [[RateIndex]] is not a family of its own: it is the union of the two families whose figure is
 * an interest rate, searched in one fixed order - Ibor index, then Overnight index - and
 * answering with the first member found. That order is the order of the combined lookup being
 * ported, which a configuration file declared by listing exactly those two types. What this
 * spec owns is therefore the union rather than any member of it: that each family's members are
 * reachable through it, and - the assertion that gives this spec its point - that the members of
 * the other two families are not.
 *
 * ===The exclusion is the contract===
 *
 * The three union abstractions of this package form a ladder, and what keeps them distinct is
 * what each one refuses. This union admits an Ibor and an Overnight index and refuses a price
 * index and an exchange-rate index; [[FloatingRateIndex]] admits a price index as well and
 * refuses only an exchange-rate index; [[Index]] admits all four. So `test_of_lookup_notFound`
 * below asserts against `GB-RPI`, the name the Java method chose: it is a perfectly valid price
 * index and a perfectly valid [[Index]], and it does resolve through [[FloatingRateIndex]], so
 * the failure it produces here can only be this union's membership talking and not the text
 * being unknown. Text naming nothing at all would satisfy the same assertion while proving none
 * of that, which is why the contrast is asserted alongside each refusal.
 *
 * ===Two methods of the Java class are reinterpreted===
 *
 *  - `test_of_convert` asserted the round trip of the reflective string-conversion library that
 *    the Java type registered two annotations with. That library is not on the classpath of
 *    this port, so the guarantee it gave is asserted directly instead: a member renders as its
 *    name, and that rendering reads back through the union as the same member. It is the text
 *    round trip alone - this Java class has no serialization method, and this spec accordingly
 *    asserts no JSON form; the union is a trait and carries no codec, so the JSON form of a
 *    member is asserted in the spec of the family that owns it.
 *  - `test_of_lookup_null` asserted that the factory rejected an absent reference. No such
 *    reference is written anywhere in this port, so the case is asserted as the spellings of an
 *    absent name that can actually be supplied - the empty name and a blank one.
 *
 * The Java class has neither a `coverage` method nor a serialization method, and this spec
 * accordingly has exactly the six tests the Java class has, under the Java method names: the
 * test inventory manifest joins every ported Java method to a test of this suite by name, so a
 * test invented here would answer to no row of it. The properties a `coverage` sweep would have
 * stood in for are not lost - the closedness of each family is swept across the module by
 * `NamedEnumClosedSpec` and the fidelity of the index data by `ReferenceDataManifestSpec`, and
 * the membership of this union is asserted in `test_of_lookup` below.
 */
class RateIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared provider, transcribed row for row from the Java data provider.
   *
   * The rows are the Java provider's, in its order and its grouping: five Ibor indices, one per
   * currency it covers, then the five Overnight indices of the same currencies. The first
   * column is typed as the union rather than as either family, which is what holds every
   * assertion below to the union's own abstraction - a row typed by the leaf type it happens to
   * have could be asserted through that family and the union would never be exercised.
   */
  private val dataName: TableFor2[RateIndex, String] = Table(
    ("index", "name"),
    (IborIndices.GBP_LIBOR_6M, "GBP-LIBOR-6M"),
    (IborIndices.CHF_LIBOR_6M, "CHF-LIBOR-6M"),
    (IborIndices.EUR_LIBOR_6M, "EUR-LIBOR-6M"),
    (IborIndices.JPY_LIBOR_6M, "JPY-LIBOR-6M"),
    (IborIndices.USD_LIBOR_6M, "USD-LIBOR-6M"),
    (OvernightIndices.GBP_SONIA, "GBP-SONIA"),
    (OvernightIndices.CHF_SARON, "CHF-SARON"),
    (OvernightIndices.EUR_EONIA, "EUR-EONIA"),
    (OvernightIndices.JPY_TONAR, "JPY-TONAR"),
    (OvernightIndices.USD_FED_FUND, "USD-FED-FUND")
  )

  /**
   * Renders a rate index through the text instance published by the family it belongs to.
   *
   * The union is a trait, and a trait of this port carries no type class instance of its own: an
   * instance belongs to a closed family, and this union is two of them. The rendering of a
   * member is therefore reached where it is defined, by selecting on which family the member
   * belongs to. The match needs no default case because the union is sealed and these are the
   * only two types that extend it, which is what also makes this helper a witness of the
   * union's membership rather than a convenience.
   *
   * @param index  the index to render
   * @return the text the index renders as, which is its name
   */
  private def renderedName(index: RateIndex): String =
    index match {
      case ibor: IborIndex => Show[IborIndex].show(ibor)
      case overnight: OvernightIndex => Show[OvernightIndex].show(overnight)
    }

  //-------------------------------------------------------------------------
  test("test_name") {
    // every row is evaluated, so a transcription mistake reports every offending row at once
    // rather than only the first
    forEvery(dataName) { (index: RateIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: RateIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The Java factory both looked a name up exactly and reported an unknown one; the port
    // splits those into `valueOf`, the exact lookup, and `parse`, which reports the failure as
    // a value. Both are asserted, so the two entry points cannot drift apart. The lookup is
    // annotated with the union type, which is what holds `valueOf` to answering with the union
    // rather than with one of the two families it searches.
    forEvery(dataName) { (index: RateIndex, name: String) =>
      withClue(s"$name: ") {
        val resolved: Option[RateIndex] = RateIndex.valueOf(name)
        resolved shouldBe Some(index)
        RateIndex.parse(name) should haveValue(index)
      }
    }

    // The ten rows above sample the union; its membership is the whole of both families, so
    // every member of each is asserted to be reachable through it as itself. That also settles
    // the one thing the fixed search order could disturb: were a name to belong to both
    // families, the Ibor probe would answer for it and the Overnight loop would fail here.
    IborIndex.values.toList.foreach { ibor =>
      withClue(s"${ibor.name}: ") {
        RateIndex.valueOf(ibor.name) shouldBe Some(ibor)
      }
    }
    OvernightIndex.values.toList.foreach { overnight =>
      withClue(s"${overnight.name}: ") {
        RateIndex.valueOf(overnight.name) shouldBe Some(overnight)
      }
    }
  }

  test("test_of_convert") {
    // Ruling: this replaces the round trip of the reflective string-conversion library, which
    // is not on the classpath of this port. The guarantee that library gave was that its two
    // annotations agreed - a value rendered to text, and that text read back to the same value
    // - so that is what is asserted here, in both directions, through the union. It is
    // deliberately the text round trip and not a JSON one: this Java class has no serialization
    // method, the union is a trait and so carries no codec, and the JSON form of a member is
    // asserted in the spec of the family that owns it.
    forEvery(dataName) { (index: RateIndex, name: String) =>
      val rendered = renderedName(index)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe index.name
        RateIndex.valueOf(rendered) shouldBe Some(index)
        RateIndex.parse(rendered) should haveValue(index)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // The failure is the point of this test. `GB-RPI` names a price index, which is a floating
    // rate index and an index but not a rate index, so this union has to refuse it - and the
    // positive contrast asserted after the refusal is what proves the refusal is the union's
    // membership talking rather than the text being unknown.
    val priceName = PriceIndices.GB_RPI.name
    RateIndex.valueOf(priceName) shouldBe None
    RateIndex.parse(priceName) should beFailureWith(FailureReason.PARSING)
    FloatingRateIndex.valueOf(priceName) shouldBe Some(PriceIndices.GB_RPI)
    Index.valueOf(priceName) shouldBe Some(PriceIndices.GB_RPI)

    // An exchange-rate index is outside this union too, and for the same reason: its figure is
    // a rate of exchange rather than a rate of interest. It remains reachable as an index.
    val fxName = FxIndices.EUR_USD_ECB.name
    RateIndex.valueOf(fxName) shouldBe None
    RateIndex.parse(fxName) should beFailureWith(FailureReason.PARSING)
    Index.valueOf(fxName) shouldBe Some(FxIndices.EUR_USD_ECB)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed an absent reference to the factory and asserted
    // that it raised an error. This port writes no such reference and its lookups take a name
    // they resolve as a value, so the case is asserted as the two spellings of an absent name
    // that can actually be supplied - the empty name and a blank one - neither of which names a
    // member of either family.
    RateIndex.valueOf("") shouldBe None
    RateIndex.parse("") should beFailureWith(FailureReason.PARSING)
    RateIndex.valueOf("   ") shouldBe None
    RateIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }
}
