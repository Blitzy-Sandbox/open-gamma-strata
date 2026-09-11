/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDate

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.parser.parse

import org.scalatest.Assertion
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.result.Failure

/**
 * The contract of [[ParityHarness]] itself.
 *
 * ===Why this suite exists===
 *
 * The harness is the apparatus every parity measurement of this module runs through, and an
 * apparatus that measures its own subject cannot also vouch for itself: a fixture row either agrees
 * with the port or it does not, and either way it says nothing about whether the comparison that
 * decided it was the comparison the rule calls for. Turn the conjunction of the two tolerance
 * bounds into a disjunction, drop the sign of a presence mismatch, count discrepancies as rows, or
 * accept a fixture with no rows at all, and every parity spec in this module still passes while
 * measuring something weaker than Rule 2 requires - in the last case, nothing at all.
 *
 * So each case here fixes one decision of the harness against inputs chosen for that decision, and
 * every one of them is deterministic: fixed values, fixed fixtures, no generated data, no timing
 * and no ordering that depends on the runner.
 *
 * ===The name===
 *
 * This suite deliberately does not end in `ParitySpec`. Gate 3 selects the measurements with
 * `testOnly *ParitySpec`, and this file measures the measurer rather than the port, so it must not
 * be part of that selection - while still running under an ordinary `sbt test`.
 *
 * ===Where reports go===
 *
 * The cases that exercise the driver publish through [[ParityHarness.runFixtureIn]] into a
 * temporary directory of their own, never into the directory named by `parity.report.dir`. A report
 * this suite wrote into the gate's directory would be indistinguishable from a measurement of the
 * port, which is precisely the confusion the harness's report exists to avoid. The one thing this
 * suite asserts about the configured directory is the decision that validates it, which is a
 * function of text and needs no directory at all.
 */
final class ParityHarnessSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import ParityHarnessSpec._

  //-------------------------------------------------------------------------
  // The tolerance rule
  //-------------------------------------------------------------------------
  test("assertParity passes a pair inside both bounds") {
    IO {
      ParityHarness.assertParity("equal", 1.0d, 1.0d) shouldBe Nil
      ParityHarness.assertParity("equal zero", 0.0d, 0.0d) shouldBe Nil
      // A difference of two to the power of minus thirty-one: exactly representable next to
      // one, so the difference computed here is exact rather than approximate. It is inside
      // the absolute bound of 1e-9, and inside the relative bound because the scale is one.
      ParityHarness.assertParity("just inside", 1.0d, 1.0d + JustInsideBothBounds) shouldBe Nil
      ParityHarness.assertParity("negative, just inside", -1.0d, -1.0d - JustInsideBothBounds) shouldBe Nil
    }
  }

  test("assertParity rejects a difference that only the relative bound admits") {
    IO {
      // A thousandth, between values of the order of 1e12: a relative difference of 1e-15,
      // which the relative bound admits comfortably, and an absolute difference a million
      // times the absolute bound. Were the rule a disjunction, this pair would pass, and a
      // parity measurement of large quantities would be worthless.
      val discrepancies = ParityHarness.assertParity("large magnitude", 1e12d, 1e12d + 1e-3d)
      discrepancies should have size 1L
      discrepancies.head should include("met: false")
      discrepancies.head should include("met: true")
    }
  }

  test("assertParity rejects a difference that only the absolute bound admits") {
    IO {
      // One value twice the other, both of the order of 1e-15: an absolute difference far
      // inside 1e-9 and a relative difference of fifty percent. Were the rule a disjunction,
      // this pair would pass, and a parity measurement of small quantities would be worthless.
      val discrepancies = ParityHarness.assertParity("near zero", 1e-15d, 2e-15d)
      discrepancies should have size 1L
      discrepancies.head should include("met: true")
      discrepancies.head should include("met: false")
    }
  }

  test("assertParity rejects a difference outside both bounds") {
    IO {
      // Two to the power of minus twenty-nine next to one: outside the absolute bound, and
      // outside the relative bound at a scale of one. Both conjuncts reject it.
      val discrepancies = ParityHarness.assertParity("outside", 1.0d, 1.0d + JustOutsideBothBounds)
      discrepancies should have size 1L
      discrepancies.head should include("met: false")
    }
  }

  test("assertParity treats identical values outside the finite range as parity") {
    IO {
      // Bit identity is decided before any arithmetic, which is the only way these can pass:
      // the difference of two infinities is not a number, and every comparison against a value
      // that is not a number is false.
      ParityHarness.assertParity("nan", Double.NaN, Double.NaN) shouldBe Nil
      ParityHarness.assertParity("positive infinity", Double.PositiveInfinity, Double.PositiveInfinity) shouldBe Nil
      ParityHarness.assertParity("negative infinity", Double.NegativeInfinity, Double.NegativeInfinity) shouldBe Nil
    }
  }

  test("assertParity reports a pair involving a value outside the finite range as a discrepancy") {
    IO {
      val opposedInfinities =
        ParityHarness.assertParity("opposed", Double.PositiveInfinity, Double.NegativeInfinity)
      opposedInfinities should have size 1L
      opposedInfinities.head should include("outside the finite range")
      ParityHarness.assertParity("nan against finite", Double.NaN, 1.0d) should have size 1L
      ParityHarness.assertParity("finite against infinity", 1.0d, Double.PositiveInfinity) should have size 1L
      // No difference is computed for such a pair, so no arithmetic on a value outside the
      // range appears in the message.
      opposedInfinities.head should not include "difference NaN"
    }
  }

  test("assertParity treats positive and negative zero as parity") {
    IO {
      // The two zeros are not bit identical, so they reach the tolerance test, where their
      // difference is zero and both bounds hold. Signed zero is a representation rather than a
      // quantity, and the captured baselines carry negative zero.
      ParityHarness.assertParity("signed zero", -0.0d, 0.0d) shouldBe Nil
      ParityHarness.assertParity("signed zero, reversed", 0.0d, -0.0d) shouldBe Nil
    }
  }

  test("assertParity names the field, both values and both differences in a discrepancy") {
    IO {
      val discrepancies = ParityHarness.assertParity("yearFraction", 0.5d, 0.25d)
      discrepancies should have size 1L
      val message = discrepancies.head
      // A report has to be diagnosable without re-running the suite that wrote it.
      message should include("yearFraction")
      message should include("0.5")
      message should include("0.25")
      message should include("absolute difference")
      message should include("relative difference")
    }
  }

  //-------------------------------------------------------------------------
  // The other comparators
  //-------------------------------------------------------------------------
  test("assertParityOpt compares the presence of a value as an expectation in its own right") {
    IO {
      ParityHarness.assertParityOpt("absent", None, None) shouldBe Nil
      ParityHarness.assertParityOpt("present", Some(1.0d), Some(1.0d)) shouldBe Nil
      ParityHarness.assertParityOpt("beyond tolerance", Some(1.0d), Some(2.0d)) should have size 1L
      // A stub the port produced where the baseline has none, and the reverse, are different
      // facts and are reported differently.
      val unexpected = ParityHarness.assertParityOpt("unexpected", Some(1.0d), None)
      unexpected should have size 1L
      unexpected.head should include("expected no value")
      val missing = ParityHarness.assertParityOpt("missing", None, Some(1.0d))
      missing should have size 1L
      missing.head should include("actual no value")
    }
  }

  test("assertParitySeq reports a length difference once and otherwise compares by index") {
    IO {
      ParityHarness.assertParitySeq("equal", Vector(1.0d, 2.0d), Vector(1.0d, 2.0d)) shouldBe Nil
      // A shifted sequence differs at every index; reporting the length once is what keeps the
      // one fact worth knowing visible.
      val lengths = ParityHarness.assertParitySeq("lengths", Vector(1.0d), Vector(1.0d, 2.0d))
      lengths should have size 1L
      lengths.head should include("1 elements")
      lengths.head should include("2 elements")
      // Equal lengths are compared element by element, each under its own indexed label, so
      // two differing positions are two discrepancies rather than one.
      val elements =
        ParityHarness.assertParitySeq("elements", Vector(1.0d, 9.0d, 3.0d), Vector(1.0d, 2.0d, 4.0d))
      elements should have size 2L
      elements.head should include("elements[1]")
      elements(1) should include("elements[2]")
    }
  }

  test("assertExact compares dates, strings and whole numbers exactly") {
    IO {
      ParityHarness.assertExact("date", LocalDate.of(2024, 1, 31), LocalDate.of(2024, 1, 31)) shouldBe Nil
      ParityHarness.assertExact("dates", List(LocalDate.of(2024, 1, 31)), List(LocalDate.of(2024, 1, 31))) shouldBe Nil
      ParityHarness.assertExact("name", "Act/365F", "Act/365F") shouldBe Nil
      ParityHarness.assertExact("days", 31, 31) shouldBe Nil
      // No tolerance anywhere near these: a money amount out by one unit in the last place is
      // wrong rather than close, which is why the amounts are captured as their decimal text.
      ParityHarness.assertExact("amount", "12.34", "12.35") should have size 1L
      val dates = ParityHarness.assertExact("date", LocalDate.of(2024, 1, 31), LocalDate.of(2024, 2, 1))
      dates should have size 1L
      dates.head should include("2024-01-31")
      dates.head should include("2024-02-01")
    }
  }

  test("assertLeft passes only when the operation failed") {
    IO {
      ParityHarness.assertLeft("refused", Left(Failure.Invalid("refused"))) shouldBe Nil
      val accepted = ParityHarness.assertLeft("accepted", Right(1.5d))
      accepted should have size 1L
      // The value that should not have been produced is named, because that is the evidence.
      accepted.head should include("1.5")
    }
  }

  test("assertRight runs the check on success and reports the failure instead on failure") {
    IO {
      // On success the check decides, so a spec never reaches into an Either for the value.
      ParityHarness.assertRight("succeeded", Right(2.0d))(value =>
        ParityHarness.assertParity("value", value, 2.0d)) shouldBe Nil
      ParityHarness.assertRight("succeeded, differing", Right(2.0d))(value =>
        ParityHarness.assertParity("value", value, 3.0d)) should have size 1L
      // On failure the check is not run at all: its comparisons would have nothing to compare,
      // and reporting them as further discrepancies would multiply one fact into several.
      val failed = ParityHarness.assertRight("failed", Left(Failure.MissingData("no calendar")))(_ =>
        List("the check must not run"))
      failed should have size 1L
      failed.head should include("no calendar")
    }
  }

  test("attemptArgCheck passes only when the precondition was refused") {
    for {
      refused <- ParityHarness.attemptArgCheck("refused")(
        throw new IllegalArgumentException("dates supplied out of order"))
      wrongType <- ParityHarness.attemptArgCheck("wrong type")(throw new IllegalStateException("something else"))
      returned <- ParityHarness.attemptArgCheck("returned")(42)
    } yield {
      refused shouldBe Nil
      // A precondition enforced by a different failure is not the precondition the baseline
      // recorded, so the type that did refuse is named.
      wrongType should have size 1L
      wrongType.head should include("IllegalStateException")
      // A call that returned refused nothing at all.
      returned should have size 1L
      returned.head should include("42")
    }
  }

  test("the raise lifts carry every failure message into the effect") {
    for {
      value <- ParityHarness.raise(Right(7))
      single <- ParityHarness.raise(Left(Failure.Invalid("one cause"))).attempt
      several <- ParityHarness
        .raiseNec(Left(NonEmptyChain(Failure.Invalid("first cause"), Failure.Parsing("second cause"))))
        .attempt
    } yield {
      value shouldBe 7
      single.left.map(_.getMessage) shouldBe Left("one cause")
      // Every cause survives the lift, joined, because an input the port refuses for two
      // reasons is not diagnosable from one of them.
      several.left.map(_.getMessage) shouldBe Left("first cause; second cause")
    }
  }

  //-------------------------------------------------------------------------
  // Loading a fixture
  //-------------------------------------------------------------------------
  test("load reads a committed baseline into its rows, in fixture order") {
    for {
      rows <- ParityHarness.load[ProbeRow](FxFixture)
      larger <- ParityHarness.load[ProbeRow](ScheduleFixture)
    } yield {
      rows should have size 14L
      rows.map(_.id).head shouldBe "cross-rate-triangulating-matrix"
      rows.foreach(row => row.id should not be empty)
      // A fixture of several hundred rows goes through the same ceilings untouched, so the
      // bounds are not quietly refusing real data.
      larger.size should be > 100
    }
  }

  test("load fails for a resource that is not on the classpath") {
    ParityHarness.load[ProbeRow]("parity/no-such-baseline.json").attempt.map { outcome =>
      outcome.isLeft shouldBe true
      outcome.left.map(failure => Option(failure.getMessage).getOrElse("")) match {
        case Left(message) => message should include("no-such-baseline.json")
        case Right(rows) => fail(s"expected a failed effect, got ${rows.size} rows")
      }
    }
  }

  test("load refuses an empty fixture, so an emptied baseline cannot pass the gate") {
    // The finding this case exists for: an empty array decodes perfectly well into no rows,
    // and a report of zero rows, zero passed and zero failed is exactly the shape of a
    // measurement that succeeded. Replacing a baseline with an empty array would therefore
    // retire every comparison in it while Gate 3 still read a pass.
    ParityHarness.decodeRows[ProbeRow]("probe", "[]", ParityHarness.MaxFixtureCharacters, 10).attempt.map {
      case Left(failure) => failure.getMessage should include("holds no rows")
      case Right(rows) => fail(s"expected an empty fixture to be refused, got ${rows.size} rows")
    }
  }

  test("load refuses a document that is not an array of rows") {
    for {
      anObject <- ParityHarness
        .decodeRows[ProbeRow]("probe", "{}", ParityHarness.MaxFixtureCharacters, 10)
        .attempt
      malformed <- ParityHarness
        .decodeRows[ProbeRow]("probe", "[{\"id\":", ParityHarness.MaxFixtureCharacters, 10)
        .attempt
      wrongRows <- ParityHarness
        .decodeRows[ProbeRow]("probe", "[{\"unexpected\":1}]", ParityHarness.MaxFixtureCharacters, 10)
        .attempt
    } yield {
      // Each of the three is a different fact about the fixture, and none of them is the port
      // disagreeing with Java; absorbing any of them into a parity result would make the
      // gate's verdict worthless.
      anObject.left.map(_.getMessage) match {
        case Left(message) => message should include("not a top-level JSON array")
        case Right(rows) => fail(s"expected a JSON object to be refused, got ${rows.size} rows")
      }
      malformed.isLeft shouldBe true
      wrongRows.isLeft shouldBe true
    }
  }

  test("load refuses a fixture beyond the character ceiling before parsing it") {
    val document = "[{\"id\":\"a\",\"source\":\"a\"}]"
    ParityHarness.decodeRows[ProbeRow]("probe", document, document.length - 1, 10).attempt.map {
      case Left(failure) =>
        failure.getMessage should include(s"${document.length} characters")
        failure.getMessage should include(s"${document.length - 1}")
      case Right(rows) => fail(s"expected an oversized fixture to be refused, got ${rows.size} rows")
    }
  }

  test("load refuses a fixture with more rows than the ceiling before decoding them") {
    val document = "[{\"id\":\"a\",\"source\":\"a\"},{\"id\":\"b\",\"source\":\"b\"}]"
    ParityHarness.decodeRows[ProbeRow]("probe", document, ParityHarness.MaxFixtureCharacters, 1).attempt.map {
      case Left(failure) => failure.getMessage should include("holds 2 rows")
      case Right(rows) => fail(s"expected an over-long fixture to be refused, got ${rows.size} rows")
    }
  }

  test("load decodes a bounded fixture into its rows in document order") {
    val document = "[{\"id\":\"first\",\"source\":\"a\"},{\"id\":\"second\",\"source\":\"b\"}]"
    ParityHarness
      .decodeRows[ProbeRow]("probe", document, ParityHarness.MaxFixtureCharacters, 10)
      .map(rows => rows.map(_.id) shouldBe Vector("first", "second"))
  }

  test("the fixture ceilings admit every committed baseline") {
    CommittedFixtures
      .traverse(resource => ParityHarness.load[ProbeRow](resource).map(rows => (resource, rows.size)))
      .map { measured =>
        measured.foreach { case (resource, rows) =>
          withClue(s"$resource: ") {
            rows should be > 0
            rows should be <= ParityHarness.MaxFixtureRows
          }
        }
        // The largest baseline committed today is the day-count document, at roughly 12.7 MiB
        // and 18,356 rows. Both ceilings are set clear of it, so enforcing them cannot start
        // refusing legitimate data, and the largest fixture is loaded here rather than assumed.
        ParityHarness.MaxFixtureCharacters should be > 16 * 1024 * 1024
        ParityHarness.MaxFixtureRows should be > 20000
        measured.map(_._2).max should be > 18000
      }
  }

  //-------------------------------------------------------------------------
  // Where the report goes
  //-------------------------------------------------------------------------
  test("the report directory must be configured, and must be absolute") {
    IO {
      // Nothing configured has no fallback: a default would let a misconfigured run publish
      // where the gate never looks, and a gate that finds no report cannot tell that apart
      // from a measurement nobody made.
      ParityHarness.reportDirectoryFrom(None) match {
        case Left(message) => message should include(ParityHarness.ReportDirectoryProperty)
        case Right(directory) => fail(s"an unset property was accepted as $directory")
      }
      ParityHarness.reportDirectoryFrom(Some("   ")).isLeft shouldBe true
      // A relative path resolves against the working directory of a forked test JVM, which is
      // not the one directory the gate collects from.
      ParityHarness.reportDirectoryFrom(Some("target/parity-report")) match {
        case Left(message) => message should include("relative")
        case Right(directory) => fail(s"a relative property was accepted as $directory")
      }
      ParityHarness.reportDirectoryFrom(Some("/tmp/parity-report-probe")) shouldBe
        Right(Paths.get("/tmp/parity-report-probe"))
      // The build supplies exactly that: an absolute path, which is what the specs then use.
      ParityHarness.reportDirectoryFrom(sys.props.get(ParityHarness.ReportDirectoryProperty)) match {
        case Right(directory) => directory.isAbsolute shouldBe true
        case Left(message) => fail(s"the build's own report directory was refused: $message")
      }
    }
  }

  test("a fixture stem must name one file inside the report directory") {
    withTempDirectory { directory =>
      for {
        nested <- ParityHarness.writeReportTo(directory, reportOf("nested/stem")).attempt
        windows <- ParityHarness.writeReportTo(directory, reportOf("nested\\stem")).attempt
        blank <- ParityHarness.writeReportTo(directory, reportOf("   ")).attempt
        plain <- ParityHarness.writeReportTo(directory, reportOf("probe"))
      } yield {
        // A stem becomes part of a path, so one carrying a separator would publish outside the
        // directory the gate collects.
        nested.left.map(_.getClass.getName) shouldBe Left("java.lang.IllegalArgumentException")
        windows.isLeft shouldBe true
        blank.isLeft shouldBe true
        plain.getFileName.toString shouldBe "probe.json"
        plain.getParent shouldBe directory
      }
    }
  }

  //-------------------------------------------------------------------------
  // The driver
  //-------------------------------------------------------------------------
  test("runFixture counts rows that matched and discrepancies that did not") {
    withTempDirectory { directory =>
      for {
        rows <- ParityHarness.load[ProbeRow](FxFixture)
        firstId = rows.head.id
        report <- ParityHarness.runFixtureIn[ProbeRow](directory, "probe-counts", FxFixture) { row =>
          IO.pure(if (row.id == firstId) List("first difference", "second difference") else Nil)
        }
      } yield {
        // The counting convention: `passed` counts rows with no message at all, `failed`
        // counts discrepancies rather than rows, so one row differing twice makes their sum
        // exceed the number of rows.
        report.fixture shouldBe "probe-counts"
        report.rows shouldBe rows.size
        report.passed shouldBe rows.size - 1
        report.failed shouldBe 2
        report.failures.map(_.id) shouldBe Vector(firstId, firstId)
        report.failures.map(_.message) shouldBe Vector("first difference", "second difference")
      }
    }
  }

  test("runFixture publishes the report before the failure gate can end the test") {
    withTempDirectory { directory =>
      for {
        report <- ParityHarness.runFixtureIn[ProbeRow](directory, "probe-published", FxFixture)(_ =>
          IO.pure(List("every row differs")))
        published <- IO.blocking(new String(Files.readAllBytes(directory.resolve("probe-published.json")), Utf8))
        gate <- ParityHarness.failIfAny(report).attempt
        clean <- ParityHarness.failIfAny(report.copy(failed = 0, failures = Vector.empty)).attempt
      } yield {
        // The counts of a failing run are the reason the report exists, so they are on disk
        // before anything can decide the run failed. Reading the file back here is what proves
        // the order: the gate below has not run yet.
        report.failed shouldBe report.rows
        parse(published).map(_.asObject.map(_.keys.toVector)) shouldBe
          Right(Some(Vector("fixture", "rows", "passed", "failed", "failures")))
        published should include("probe-published")
        published should include("every row differs")
        gate.left.map(_.getClass.getName) shouldBe Left("java.lang.AssertionError")
        gate.left.map(failure => Option(failure.getMessage).getOrElse("")) match {
          case Left(message) =>
            message should include("probe-published")
            message should include(s"${report.failed} discrepancies")
          case Right(_) => fail("a report holding discrepancies did not fail the gate")
        }
        clean shouldBe Right(())
      }
    }
  }

  test("runFixture records a row whose check raises and keeps measuring the rows after it") {
    withTempDirectory { directory =>
      for {
        rows <- ParityHarness.load[ProbeRow](FxFixture)
        firstId = rows.head.id
        report <- ParityHarness.runFixtureIn[ProbeRow](directory, "probe-raised", FxFixture) { row =>
          if (row.id == firstId) IO.raiseError(new RuntimeException("the check itself broke")) else IO.pure(Nil)
        }
      } yield {
        // An exception escaping a row would take the counts of every row after it with it, so
        // it becomes one discrepancy of that row and the run continues.
        report.rows shouldBe rows.size
        report.failed shouldBe 1
        report.passed shouldBe rows.size - 1
        report.failures.head.id shouldBe firstId
        report.failures.head.message should include("the check itself broke")
      }
    }
  }

  test("the failure gate quotes the first twenty discrepancies and says how many it left out") {
    val failures = Vector.tabulate(25)(index => ParityFailure(s"row-$index", s"difference $index"))
    ParityHarness.failIfAny(ParityReport("probe-truncation", 25, 0, failures.size, failures)).attempt.map {
      case Left(failure) =>
        val message = Option(failure.getMessage).getOrElse("")
        message should include("difference 0")
        message should include("difference 19")
        // Everything beyond the quota stays in the report document rather than in the console.
        message should not include "difference 20"
        message should include("and 5 more")
      case Right(_) => fail("a report holding discrepancies did not fail the gate")
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Creates an empty directory, hands it to the case, and removes it and anything the case
   * wrote afterwards.
   *
   * Reports published by this suite go here rather than into the directory the gate reads, and
   * the finalizer runs on every outcome so a failing case leaves nothing behind for the next
   * run to find.
   *
   * @param use  the case, given the absolute path of the directory
   * @return the effect of the case, with creation and removal around it
   */
  private def withTempDirectory(use: Path => IO[Assertion]): IO[Assertion] =
    IO.blocking(Files.createTempDirectory("parity-harness-spec-")).flatMap { directory =>
      use(directory).guarantee(IO.blocking {
        Option(directory.toFile.listFiles())
          .map(_.toVector)
          .getOrElse(Vector.empty)
          .foreach(entry => Files.deleteIfExists(entry.toPath))
        val _ = Files.deleteIfExists(directory)
      })
    }
}

/** The row model and the fixed inputs of the cases above. */
private object ParityHarnessSpec {

  /**
   * The least a row model can be: the identity of a fixture row and the case it came from.
   *
   * The cases here measure the harness rather than the port, so they need no expectation
   * fields; this decoder reads the two keys it names and ignores the rest of the row, which
   * lets a real committed baseline stand in for a fixture of this suite's own.
   *
   * The identity follows the rule the baselines are captured under: a row carries an `id`
   * exactly where its `source` cannot identify it alone, so an absent `id` means the `source`
   * is the identity. `schedule-baseline.json` is the one committed document of that shape.
   */
  final case class ProbeRow(id: String, source: String) extends ParityRow

  object ProbeRow {

    implicit val probeRowDecoder: Decoder[ProbeRow] = Decoder.instance { cursor =>
      for {
        source <- cursor.get[String]("source")
        id <- cursor.getOrElse[String]("id")(source)
      } yield ProbeRow(id, source)
    }
  }

  /** The smallest committed baseline that carries row identities, used by the driver cases. */
  val FxFixture: String = "parity/fx-baseline.json"

  /** A committed baseline of several hundred rows, so the ceilings are exercised on real data. */
  val ScheduleFixture: String = "parity/schedule-baseline.json"

  /** Every baseline this module commits, in the order the gate reports them. */
  val CommittedFixtures: Vector[String] =
    Vector(
      "parity/daycount-baseline.json",
      "parity/schedule-baseline.json",
      "parity/fx-baseline.json",
      "parity/currency-math-baseline.json",
      "parity/holiday-baseline.json")

  /** The character set of a published report. */
  val Utf8: java.nio.charset.Charset = StandardCharsets.UTF_8

  /**
   * A difference that both bounds admit next to a value of one: two to the power of minus
   * thirty-one, which is exactly representable there, so the difference under test is exact.
   */
  val JustInsideBothBounds: Double = math.pow(2.0d, -31.0d)

  /** A difference next to one that neither bound admits: two to the power of minus twenty-nine. */
  val JustOutsideBothBounds: Double = math.pow(2.0d, -29.0d)

  /** A report with the given stem and nothing in it, for the cases that only publish one. */
  def reportOf(fixture: String): ParityReport = ParityReport(fixture, 1, 1, 0, Vector.empty)
}
