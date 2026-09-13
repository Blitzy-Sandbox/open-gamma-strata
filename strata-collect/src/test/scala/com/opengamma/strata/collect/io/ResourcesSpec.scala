/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import java.io.OutputStream
import java.io.PrintStream
import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import io.circe.parser.parse

import org.scalatest.Assertion
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Tests `Resources`, the single effectful edge of this module.
 *
 * ===The two subjects===
 *
 * `readClasspathText` is exercised against the parity baseline fixture committed under
 * `parity/`, which is the only test resource this module owns; using it rather than a
 * fixture of this spec's own keeps the classpath cases honest, because that file is read
 * over the same name (`parity/double-array-baseline.json`) by the parity harness that
 * actually depends on it. The fixture is treated purely as text here - its size, its first
 * character and the fact that it parses as a non-empty JSON array. Its rows, their fields
 * and the numbers in them belong to the parity spec, which owns the tolerance that gives
 * them meaning, and are deliberately not asserted here.
 *
 * What that reader reads is a '''file''' of the classpath, and the three cases under "What the
 * classpath reader reads" are what hold it to that: the directory holding the fixture, a
 * package directory and the empty name all resolve for a class loader and denote no file, so
 * each of them fails the read instead of returning the shape of the classpath as content, while
 * the fixture itself still reads whole under both spellings of its name. Whether a class loader
 * resolves such a name at all depends on the classpath in force, so those cases read the
 * location first and assert the refusal it implies - never cancelling, because content is the
 * one outcome no classpath may produce for a name like that.
 *
 * `readFileText` is exercised against files this spec creates under the temporary
 * directory of the platform and deletes again. Tests run in a forked process, so nothing
 * here may lean on the working directory: every path handed to the subject is absolute and
 * comes from `Files.createTempFile` or `Files.createTempDirectory`, and every one of them
 * is removed by a finalizer that runs whether the case passed, failed or was cancelled.
 *
 * ===A name is literal text===
 *
 * Each reader takes one name and uses it exactly as supplied. `readClasspathText` asks the
 * class loader for that resource name, dropping one leading separator so that the
 * slash-prefixed and the bare spelling of a resource yield one content; `readFileText` asks
 * the platform for a file at that path. Nothing inside a name is interpreted: a name opening
 * with `file:`, `classpath:` or `url:`, and a name whose tail reaches inside an archive,
 * name a resource or a file spelled exactly that way and nothing else. Neither reader
 * resolves a name against a class, opens an archive, or fetches a URL, and neither takes a
 * charset or a locator value. The two cases under "Names this reader does not interpret"
 * assert that behaviour over names of each of those shapes, each against a positive control
 * that reads the same file and the same resource under their plain names, so a failed read
 * is attributable to the shape of the name rather than to absent or unreadable content; the
 * surface case denies the rest of that API at compile time.
 *
 * Most of the cases here are additive rather than ported, and the note at the foot of this
 * file counts them and says why each family exists. The port promises a byte ceiling, a
 * strict decode, a handle released on every outcome, a read that happens only when the
 * effect is run, a failed effect for any source it cannot obtain, a failure whose text names
 * that source in one bounded line however the source was named, and a read that can be
 * cancelled while it waits; the original could promise none of those, because its reads
 * happened where they were written and it reported failure by throwing. Every failure here is
 * observed through `attempt` as a value.
 *
 * A read materialises at most 64 MiB and refuses a larger source, decodes strictly as UTF-8
 * and refuses malformed bytes rather than substituting for them, releases its handle on
 * every outcome, performs nothing until the effect is run, and fails the effect - never
 * yields a sentinel or empty text - for a source it cannot obtain. Each of those is asserted
 * below, on both readers where both can reach it, and every failure is observed through
 * `attempt` as a value rather than caught.
 *
 * ===Everything here goes through the two public readers===
 *
 * The subject's surface is `readClasspathText` and `readFileText`, and this spec uses
 * nothing else: the byte ceiling, the stream acquisitions and the shared read they feed are
 * private to the subject, and the surface case below asserts at compile time that they are
 * unreachable from here even though this spec shares their package. Nothing is given up by
 * that: the ceiling is established from outside, by handing `readFileText` a sparse file of
 * exactly 64 MiB and another of one byte more, and stream lifetime is established end to end
 * over the descriptor table of this process - after a read that succeeds, after a read that
 * fails once its stream is open, and after a read the ceiling refuses.
 *
 * Release when a read is '''cancelled''' is asserted as well, and it needs no seam in the
 * subject either. The source it is asserted over is the endless device `/dev/urandom`: a whole
 * read of it costs a few hundred milliseconds of real time, and the case '''measures''' that
 * cost first, so what follows is compared against this machine rather than against a constant
 * guessed at while writing the case. It then starts a second read, lets it get under way,
 * cancels it, and times the cancellation. Three things must hold: the read ends '''cancelled'''
 * rather than with a result, the cancellation costs a small fraction of a whole read - a
 * cancellation that waited for the blocking call to return would cost nearly all of one,
 * because it is delivered in the first tenth of it - and no descriptor of this process names
 * the source afterwards that did not name it before. Where the source is absent, where the read
 * ends before the cancellation reaches it, or where the machine reads so fast that the
 * comparison would mean nothing, the case cancels itself with the reason stated; every wait in
 * it is bounded, so a regression fails it rather than hanging the suite.
 *
 * A named pipe would be the obvious source for that case, since one opens and then never
 * yields, and the case after it records why it is not used: the subject reads its source with
 * one bulk call, that call requires a seekable channel, and a pipe is therefore '''refused'''
 * the moment it is read rather than blocking on it. That is worth establishing rather than
 * merely noting - it says that a source of this shape cannot make a read of this subject wait
 * at all, and that the handle of the refused read comes back like any other. The case makes its
 * pipe with `mkfifo`, opens the writing end on a fiber of its own so that the subject's open
 * can complete, removes both by a finalizer, bounds every wait, and cancels itself with the
 * reason stated where `mkfifo` cannot be run or where a platform turns out to read pipes after
 * all.
 *
 * ===Cases about the runner rather than about the subject===
 *
 * Four of the cases here cancel themselves where the machine cannot carry them - too little
 * heap for the case at the ceiling, no descriptor table to read, no endless device, no usable
 * `mkfifo` - and printing the reason is the right thing for such a case to do. On its own,
 * though, it hides the loss: a cancelled case is not a failed one, so a run that stopped
 * exercising the ceiling or the descriptor table still reports itself green. The capability
 * case at the foot of this file is the answer to that. It asserts those five properties of the
 * runner directly, each naming what it observed, so a machine that lacks one of them fails this
 * file and says which; the cases themselves keep their cancellations, so what is lost is still
 * printed where the machine is genuinely incapable. Three further conditions those cases cancel
 * on are outcomes of a race rather than properties of a machine, and the capability case states
 * why it deliberately does not assert them.
 *
 * ===Where a failure goes===
 *
 * A failure of this subject is a value its caller receives, and the diagnostics case holds it
 * to being '''only''' that: while two reads are refused, the error stream of this process is
 * captured, and nothing naming the subject may appear in it. The capture is proved live by a
 * canary the case writes itself, the original stream is restored by a finalizer, and the
 * assertion is narrow - no line naming the subject, rather than an empty capture - because the
 * suites here share one forked process and run in parallel.
 *
 * One property of the implementation is still not asserted, deliberately rather than by
 * omission: release when the '''acquisition''' itself fails. That belongs to the same pairing
 * of acquisition with release, and it cannot be observed from outside for a reason no seam
 * would fix - an acquisition that fails never produced a handle, so there is nothing to look
 * for in the descriptor table and nothing a caller could distinguish.
 */
final class ResourcesSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  /** The committed parity fixture, named exactly as its own consumers name it. */
  private val FixturePath = "parity/double-array-baseline.json"

  /**
   * The size of that fixture in characters, which is also its size in bytes: it is ASCII JSON.
   *
   * Stated as a number rather than measured from the file, because measuring the source the
   * subject reads and comparing it with itself would assert nothing. It is the one case that
   * pins the '''whole''' of an ordinary read, so a reader that began truncating, or that
   * refused a name it used to read, is caught here rather than by a case that only asks
   * whether the text is non-empty. A fixture regenerated at another size is a deliberate
   * change to a committed baseline, and updating this number is part of making it.
   */
  private val FixtureCharacters: Int = 90821

  /** A resource name the classpath does not hold, used by the failure and laziness cases. */
  private val AbsentResource = "parity/no-such-fixture.json"

  /**
   * The directory holding the fixture above, which denotes no file.
   *
   * It is the name a mistyped fixture name reduces to, and it resolves as readily as an entry
   * does - a class loader locates a directory without complaint - so reading it would yield the
   * shape of the classpath rather than content. That is why the subject refuses it.
   */
  private val FixtureDirectory = "parity"

  /**
   * A package directory, which denotes no file either and exists on any classpath this module
   * is loaded from, so the property is asserted over a second name that nothing about this
   * spec's own fixtures could explain.
   */
  private val PackageDirectory = "com/opengamma"

  /**
   * The empty resource name, which denotes the root of the classpath where it denotes anything.
   *
   * It is the degenerate case of the two above, and worth its own case because it is what a
   * name assembled from an empty configured value comes to.
   */
  private val EmptyResource = ""

  /** The wording the subject uses for a name it resolved to something that is not a file. */
  private val DirectoryRefusal = "Classpath resource is a directory rather than a file: "

  /** The wording the subject uses for a name the classpath does not hold at all. */
  private val AbsentRefusal = "Classpath resource absent: "

  /** A file path nothing on this platform bears, used by the laziness case of the file reader. */
  private val AbsentFilePath = "/no-such-directory/no-such-file.txt"

  /**
   * The text the diagnostics case writes to the error stream itself, to prove the capture works.
   *
   * A case asserting that nothing of the subject reached the error stream is worthless if the
   * capture was never connected to it, so the case writes this and asserts that '''it''' was
   * captured. The value is distinctive enough that no other suite sharing this process could
   * produce it.
   */
  private val StandardErrorCanary = "resources-spec-standard-error-canary"

  /**
   * How long the diagnostics case lets a report that is not the subject's own arrive.
   *
   * The duplicate report the case is about is written by the runtime rather than by the read,
   * so it need not be written by the time the read's value is in hand. The case therefore waits
   * before it looks, and this is that wait: long enough for a report that was going to be
   * written to have been written, and short enough to cost the suite nothing that matters.
   */
  private val DiagnosticSettle: FiniteDuration = 250.millis

  /**
   * A name carrying the characters a forged record is made of: a line feed, a carriage
   * return, a tab and a control character with no short escape.
   *
   * The text after the line feed is written to look like a record of its own, because that is
   * the property the two rendering cases are about - a name is chosen by a caller, a caller's
   * name may itself have arrived from somewhere else, and a message that carried these
   * characters through would let the choice of name state something the library never
   * reported. The prefix is an ordinary resource name, so the same message can be shown to
   * keep ordinary text exactly as it was.
   */
  private val ForgedName = "parity/fixture\nWARN  baseline replaced\rtab\there\u0007.json"

  /** The ordinary prefix of `ForgedName`, which rendering must leave character for character. */
  private val ForgedNamePrefix = "parity/fixture"

  /**
   * A name far beyond the 512-character bound of the diagnostic renderer, and a second one ten
   * times longer again.
   *
   * Two lengths rather than one: that a message is shorter than the name it names would also
   * be true of a message that grew with the name, so the two cases compare the two messages
   * with each other. A bounded message is the same size for both.
   */
  private val OverlongName = "parity/" + ("x" * 4000) + ".json"

  /** The second, ten times longer name of the pair described above. */
  private val FarOverlongName = "parity/" + ("x" * 40000) + ".json"

  /** The three characters the renderer appends when it has left part of a name out. */
  private val TruncationMarker = "..."

  /** The name the pipe case gives its named pipe inside its own directory. */
  private val PipeName = "stalled-source"

  /**
   * The endless device the cancellation case reads: a source that yields without ever ending.
   *
   * It is what makes a read observably '''in flight''' from outside the subject. A file of any
   * size this spec could write is read in a moment, and a named pipe - the obvious stalling
   * source - is refused rather than read, as the case after the cancellation one establishes.
   * This device is neither: the subject reads it exactly as it reads a file, the read takes
   * long enough to be interrupted part way through, and it needs nothing created or removed.
   */
  private val ContinuousSource = "/dev/urandom"

  /**
   * The absolute bound on the cancellation of a read that is in flight.
   *
   * The case asserts promptness by comparison rather than against a constant, because how long
   * a read takes is a property of the machine; this is the second, absolute bound that keeps a
   * regression from hanging the suite while the comparison decides whether it passed. Five
   * seconds is orders of magnitude above the milliseconds a prompt cancellation needs and far
   * below the subject's own two-minute bound.
   */
  private val PromptCancellation: FiniteDuration = 5.seconds

  /**
   * How much faster than a whole read the cancellation of one in flight has to be.
   *
   * A cancellation that waited for the blocking call to return would cost what remains of the
   * read, and the cancellation is delivered in the first tenth of one, so a factor of four
   * separates the two outcomes with room to spare in both directions - it does not demand that
   * a loaded machine cancel in any particular number of milliseconds, and it is not satisfied
   * by a cancellation that waited.
   */
  private val PromptnessFactor: Long = 4L

  /**
   * The shortest whole read the comparison above is drawn from.
   *
   * Below this, a quarter of a whole read is so small that the case would be measuring
   * scheduling noise rather than the subject, so it cancels itself instead of asserting
   * something it cannot see.
   */
  private val MeasurableRead: FiniteDuration = 100.millis

  /**
   * The pause between starting a read and cancelling it.
   *
   * Long enough that the read is inside its blocking call rather than still being set up -
   * cancelling the effect around a read is a different path and not the one the finding is
   * about - and short enough to leave nine tenths of the read ahead of it, which is what makes
   * the comparison above decisive.
   */
  private val ReadEntryPause: FiniteDuration = 50.millis

  /**
   * How long the writing end of the named pipe may take to open.
   *
   * Opening it completes only when the subject opens the reading end, so this bounds the
   * subject's own open together with the scheduling of two fibers.
   */
  private val PipeOpenBound: FiniteDuration = 30.seconds

  /** How long `mkfifo` may take, before the case concludes this platform cannot run it. */
  private val MakePipeBound: FiniteDuration = 10.seconds

  /**
   * How long the read of the named pipe may take before the case draws no conclusion from it.
   *
   * A refusal is immediate, so reaching this bound means the platform read the pipe instead of
   * refusing it - on which the case has nothing to say and cancels itself, rather than failing
   * over a platform difference in a call the subject makes.
   */
  private val PipeReadBound: FiniteDuration = 10.seconds

  /** The content of the fixture file the original read, byte for byte. */
  private val HelloWorld = "HelloWorld\n"

  /** The file name of the classpath fixture, used by the descriptor-reclamation case. */
  private val FixtureFileName = "double-array-baseline.json"

  /** Where this platform exposes the open descriptors of the running process, if it does. */
  private val ProcessDescriptors: Path = Paths.get("/proc/self/fd")

  /** Bytes that are not valid UTF-8: a continuation byte with nothing to continue. */
  private val MalformedUtf8: Array[Byte] = Array[Byte](0x41.toByte, 0xc3.toByte, 0x28.toByte)

  /**
   * The bytes an archive begins with, followed by bytes that are not valid UTF-8.
   *
   * The first four are the local-file header of the archive format, in the order it writes
   * them (80, 75, 3, 4); the two after them are a malformed UTF-8 sequence, which is what
   * the rest of an archive looks like to a text reader. Written as bytes rather than as a
   * string because encoding a string would destroy the property under test - an archive is
   * not text, and this file is deliberately not decodable.
   */
  private val ArchiveBytes: Array[Byte] =
    Array[Byte](0x50.toByte, 0x4b.toByte, 0x03.toByte, 0x04.toByte, 0xc3.toByte, 0x28.toByte)

  /**
   * The suffix that names an entry inside an archive, in the form archive tooling writes it
   * (`...TestFile.zip!/TestFile.txt`): the separator `!/` followed by the entry's own path.
   */
  private val ArchiveEntrySuffix = "!/TestFile.txt"

  /**
   * The byte ceiling both readers apply, as the subject documents it: 64 MiB.
   *
   * The subject holds this bound privately, so it is restated here rather than read from it -
   * which is the point of the two cases below: they establish through the public readers alone
   * that the documented ceiling is the one enforced, and that it is enforced beyond itself
   * rather than at itself.
   */
  private val DocumentedCeiling: Long = 64L * 1024L * 1024L

  /**
   * The heap the case at the ceiling needs, being several times the text it decodes.
   *
   * Reading exactly the ceiling materialises sixty-four mebibytes of bytes and the text decoded
   * from them at once. A runner given less heap than this cancels that case with its reason
   * stated, rather than failing on memory and reading as a defect in the reader.
   */
  private val CeilingCaseHeap: Long = 512L * 1024L * 1024L

  //-------------------------------------------------------------------------
  // readClasspathText
  //-------------------------------------------------------------------------
  test("readClasspathText reads the committed parity fixture from the test classpath") {
    // The relative form: a class loader resource name carries no leading separator, and
    // this is the form the fixture's consumers use.
    Resources.readClasspathText(FixturePath).map { text =>
      text should not be empty
      // The original asserted the first byte of the content; the fixture is a JSON array,
      // so the character that opens one is the equivalent property here.
      text.charAt(0) shouldBe '['
    }
  }

  test("readClasspathText decodes the fixture as UTF-8 text that parses as a non-empty JSON array") {
    Resources.readClasspathText(FixturePath).map { text =>
      // Structure only. The rows, their fields and their numbers belong to the parity spec.
      parse(text) match {
        case Right(json) => json.asArray.map(_.nonEmpty) shouldBe Some(true)
        case Left(failure) => fail(s"the fixture did not parse as JSON: ${failure.message}")
      }
    }
  }

  test("readClasspathText accepts an absolute, slash-prefixed resource path") {
    // The port removes a single leading separator, so the absolute and relative forms name
    // one resource and must return one content - the same normalisation the original applied.
    for {
      absolute <- Resources.readClasspathText("/" + FixturePath)
      relative <- Resources.readClasspathText(FixturePath)
    } yield {
      absolute should not be empty
      absolute shouldBe relative
    }
  }

  test("readClasspathText fails the IO for a resource that is not on the classpath") {
    // Additive: the original had no counterpart. The contract promises a failed effect, and
    // `attempt` turns that failure into a value so it can be inspected rather than caught.
    Resources.readClasspathText(AbsentResource).attempt.map {
      case Left(failure) =>
        failure shouldBe an[IOException]
        failure.getMessage should not be empty
        failure.getMessage should include(AbsentResource)
        // An ordinary name reaches the message character for character. The subject renders
        // every name it reports, so this is what says that the rendering of an ordinary name
        // is the name: the message is the fixed wording followed by exactly what was asked
        // for, on one line, with nothing escaped, elided or added.
        failure.getMessage should startWith("Classpath resource absent: ")
        failure.getMessage should endWith(AbsentResource)
        failure.getMessage.linesIterator.size shouldBe 1
      case Right(text) =>
        fail(s"expected a failed IO, got ${text.length} characters")
    }
  }

  //-------------------------------------------------------------------------
  // What the classpath reader reads: files, and nothing else
  //
  // A class loader resolves a name to a location, and a location need not be a file. A name
  // denoting a directory of the classpath resolves exactly as an entry does, and reading it
  // yields whatever the platform makes of a directory - the entry names it holds under exploded
  // class directories, empty text from an archive - neither of which anybody stored there. The
  // subject's contract is the classpath subset that names files, so the first two cases below
  // assert that a name outside it is refused where an absent name is refused, rather than read,
  // and the third asserts that refusing them narrowed nothing an ordinary read relies on.
  //
  // Whether the loader resolves such a name at all is a property of the classpath in force, so
  // each refusal case reads the location first and asserts against the refusal that location
  // implies. Both branches assert a failure: the one thing no classpath may do for a name like
  // that is return content.
  //-------------------------------------------------------------------------
  test("readClasspathText fails the IO for a name that denotes a directory of the classpath") {
    // Two such names: the directory holding the fixture, which is what a mistyped fixture name
    // reduces to, and a package directory, which exists on any classpath this module loads from.
    refusalOfANameThatDenotesNoFile(FixtureDirectory)
      .flatMap(_ => refusalOfANameThatDenotesNoFile(PackageDirectory))
  }

  test("readClasspathText fails the IO for the empty resource name") {
    // The same property at the root of the classpath. Under exploded class directories the
    // empty name resolves to the directory the classes sit in; from an archive it resolves to
    // nothing. Either way there is no file, so either way the read fails.
    refusalOfANameThatDenotesNoFile(EmptyResource)
  }

  test("readClasspathText returns the whole committed fixture under both spellings of its name") {
    // The positive half of the two cases above: refusing names that denote no file must not
    // narrow what the reader accepts. Both spellings of the fixture's name return its content
    // whole - every character of it, not merely a non-empty prefix - and return the same text.
    for {
      relative <- Resources.readClasspathText(FixturePath)
      absolute <- Resources.readClasspathText("/" + FixturePath)
    } yield withClue(
      s"the fixture read as ${relative.length} characters by its bare name and " +
        s"${absolute.length} by its slash-prefixed name: ") {
      relative.length shouldBe FixtureCharacters
      absolute.length shouldBe FixtureCharacters
      absolute shouldBe relative
    }
  }

  //-------------------------------------------------------------------------
  // How a failure names its source
  //
  // A caller chooses the name a read is given and the subject quotes that name back in
  // whatever failure the read produces. The three cases below are what holds that quoting to
  // the two properties the subject documents, over both readers rather than one: a message is
  // one line whatever the name contains, and its size does not follow the size of the name.
  // The case above is the third property of the same family - an ordinary name is quoted back
  // unaltered - which is why it is strengthened there rather than repeated here.
  //-------------------------------------------------------------------------
  test("neither reader lets a control character in a name forge a line in the failure it reports") {
    for {
      resource <- Resources.readClasspathText(ForgedName).attempt
      file <- Resources.readFileText(ForgedName).attempt
    } yield {
      List(refusalMessageOf(resource), refusalMessageOf(file)).foreach { message =>
        withClue(s"the message was '$message': ") {
          // One line, and not by luck: none of the three characters that would end a line or
          // move a column survives into the message.
          message.linesIterator.size shouldBe 1
          message should not include "\n"
          message should not include "\r"
          message should not include "\t"
          // What stands in their place. The first three have short escapes; the fourth is a
          // control character with none, so it is written as the six-character form.
          message should include("\\n")
          message should include("\\r")
          message should include("\\t")
          message should include("\\u0007")
          // And the ordinary part of the name is still there, so the rendering escaped what
          // it had to and nothing else.
          message should include(ForgedNamePrefix)
        }
      }
      succeed
    }
  }

  test("neither reader lets an overlong name inflate the failure it reports") {
    for {
      resource <- Resources.readClasspathText(OverlongName).attempt
      longerResource <- Resources.readClasspathText(FarOverlongName).attempt
      file <- Resources.readFileText(OverlongName).attempt
      longerFile <- Resources.readFileText(FarOverlongName).attempt
    } yield {
      val bounded = List(
        (refusalMessageOf(resource), refusalMessageOf(longerResource)),
        (refusalMessageOf(file), refusalMessageOf(longerFile)))
      bounded.foreach { case (message, messageForLongerName) =>
        withClue(s"the message was ${message.length} characters long: ") {
          // The name does not reach the message whole, and the marker says as much rather
          // than the message simply ending mid-name.
          message should not include OverlongName
          message should include(TruncationMarker)
          message.length should be < OverlongName.length
          // The bound is a bound rather than a proportion: a name ten times longer produces a
          // message of exactly the same size, so nothing a caller can do makes the diagnostic
          // grow. A message that merely shortened its name would fail here.
          messageForLongerName.length shouldBe message.length
          messageForLongerName should not include FarOverlongName
        }
      }
      succeed
    }
  }

  test("readFileText reports a path it cannot open as a failure of its own, with the platform failure as the cause") {
    // The platform's exception carries the raw path as its own message, so publishing it is
    // publishing the path: the subject wraps it instead, and attaches it as the cause, which
    // is where a diagnostic under this process's control still finds everything it had.
    withTempDirectory { directory =>
      val absent = directory.resolve("no-such-file.txt")
      Resources.readFileText(absent.toString).attempt.map {
        case Left(failure) =>
          failure shouldBe an[IOException]
          // The public text: the source, named the way every other message of this read names
          // it, and what the platform reported about it, on one line.
          failure.getMessage should include(s"file '${absent.toString}'")
          failure.getMessage should include("could not be opened")
          failure.getMessage should include("NoSuchFileException")
          failure.getMessage.linesIterator.size shouldBe 1
          // The cause: the platform exception itself, unaltered, so nothing is lost by the
          // wrapping - this is what "retain raw causes for controlled diagnostics" means.
          failure.getCause shouldBe a[NoSuchFileException]
          Option(failure.getCause.getMessage) shouldBe Some(absent.toString)
        case Right(text) =>
          fail(s"expected a failed IO, got ${text.length} characters")
      }
    }
  }

  //-------------------------------------------------------------------------
  // What a failure reaches, and what it does not
  //
  // The subject reports a failure by failing its effect, and that is meant to be the whole of
  // what a failure does: the caller holds the exception and nothing else has been told about
  // it. A reader that also wrote the failure somewhere would be reporting it twice - once as a
  // value the caller can act on and once as text in a log nobody asked for - and the second
  // report is the misleading one, because it looks like an error that escaped.
  //-------------------------------------------------------------------------
  test("a refused read reports its failure to its caller alone and prints nothing about itself") {
    // Two reads in one composition, the second failing inside the read rather than in the
    // acquisition: that is the pair a duplicate report needs, because the read is the part that
    // runs on a fiber of its own and a fiber which ends errored is handed to the runtime's
    // failure reporter as well as to whoever joins it.
    //
    // Two things about this case are worth stating. It asserts narrowly - that no captured line
    // names the subject - rather than that the capture is empty, because suites here share one
    // forked process and run in parallel, so another suite's output can arrive in the window
    // this case holds the stream. And a duplicate report of this kind is written at most once
    // per process, so inside a suite that has already refused a read this case can pass without
    // having had the chance to fail; what establishes it is a run of these two reads in a
    // process of their own, which is how the behaviour was found. The case earns its place by
    // failing if the report ever comes back in the first refused read of a process.
    withTempDirectory { directory =>
      withCapturedStandardError(
        for {
          absent <- Resources.readClasspathText(AbsentResource).attempt
          unreadable <- Resources.readFileText(directory.toString).attempt
        } yield (absent, unreadable)
      ).map { case ((absent, unreadable), capture) =>
        // Both reads failed, so the case is about a failure that happened rather than about a
        // stream nothing was written to.
        val refusedAcquisition = refusalMessageOf(absent)
        val refusedRead = refusalMessageOf(unreadable)
        val framesOfTheSubject = capture.linesIterator
          .filter(line => line.contains("Resources$") || line.contains("readBoundedText"))
          .toList
        withClue(
          s"the acquisition was refused with '$refusedAcquisition', the read with " +
            s"'$refusedRead', and ${capture.length} characters reached the error stream: ") {
          // The canary first: it proves the capture is live and that a stack trace written to
          // this stream would have been seen, so the emptiness asserted below is a fact about
          // the subject rather than about the capture.
          capture should include(StandardErrorCanary)
          capture.linesIterator.count(line => line.trim.startsWith("at ")) should be >= 1
          // And the subject wrote nothing: no frame of it, and no frame of the bounded read
          // the fiber runs, reached the error stream while its two failures were produced.
          framesOfTheSubject shouldBe empty
        }
      }
    }
  }

  test("readClasspathText suspends the read so that constructing the IO never throws") {
    // Building the description of a read of an absent resource is not itself a read, so it
    // cannot fail. What construction yields is a value, and this case asserts that value rather
    // than only that construction returned: two constructions of the same read are two
    // descriptions, and one description evaluated twice fails the same way both times, so
    // nothing was read, memoised or cached while the description was being built.
    val described = Resources.readClasspathText(AbsentResource)
    val describedAgain = Resources.readClasspathText(AbsentResource)
    for {
      first <- described.attempt
      again <- described.attempt
      other <- describedAgain.attempt
    } yield {
      described should not be theSameInstanceAs(describedAgain)
      val firstRefusal = refusalOf(first, AbsentResource)
      firstRefusal shouldBe a[FileNotFoundException]
      firstRefusal.getMessage should startWith(AbsentRefusal)
      // The same description, evaluated a second time, and a second description of the same
      // read: both fail exactly as the first did.
      refusalMessageOf(again) shouldBe firstRefusal.getMessage
      refusalMessageOf(other) shouldBe firstRefusal.getMessage
    }
  }

  test(
    "readClasspathText is referentially transparent: the same path yields the same content on repeated evaluation"
  ) {
    // One value, evaluated twice. This is what replaces the value equality of the locator
    // type: the port has no locator to compare, and the property that mattered - naming a
    // resource twice describes the same read - belongs to the effect instead.
    val read = Resources.readClasspathText(FixturePath)
    read.flatMap(first =>
      read.map { second =>
        first should not be empty
        second shouldBe first
      }
    )
  }

  //-------------------------------------------------------------------------
  // readFileText
  //-------------------------------------------------------------------------
  test("readFileText reads a temporary UTF-8 file created with java.nio.file.Files") {
    withTempFile(HelloWorld) { path =>
      Resources.readFileText(path.toString).map { text =>
        text shouldBe HelloWorld
        // The decoded-lines property of the original, over the same content.
        text.linesIterator.toList shouldBe List("HelloWorld")
      }
    }
  }

  test("readFileText decodes multi-byte UTF-8 content without loss") {
    // Escapes rather than literal characters, so this case depends on nothing but the
    // encoding the subject itself states: an acute e, a euro sign and an em dash, each of
    // which occupies more than one byte in UTF-8 and is corrupted by most other encodings.
    val content = "caf\u00e9 12,50\u20ac \u2014 fin\n"
    withTempFile(content) { path =>
      Resources.readFileText(path.toString).map { text =>
        text shouldBe content
        // Without this the case could pass over content that is plain ASCII and prove
        // nothing: more bytes than characters is what makes the encoding observable.
        content.getBytes(StandardCharsets.UTF_8).length should be > content.length
      }
    }
  }

  test("readFileText fails the IO for a path that does not exist") {
    // Additive, as above. The path is inside a directory that does exist, so the only
    // reason the read can fail is the absence of the file itself.
    withTempDirectory { directory =>
      val absent = directory.resolve("no-such-file.txt")
      Resources.readFileText(absent.toString).attempt.map {
        case Left(failure) =>
          failure shouldBe an[IOException]
          failure.getMessage should not be empty
        case Right(text) =>
          fail(s"expected a failed IO, got ${text.length} characters")
      }
    }
  }

  test("readFileText fails the IO for a path that cannot be read as a file") {
    // A directory is the deterministic way to name something that exists and yet cannot be
    // read as a file. Withdrawing read permission from a file is not: it is silently
    // ineffective for a privileged process, which is the normal case in a container, and
    // the case would then fail for a reason that has nothing to do with the subject.
    withTempDirectory { directory =>
      Resources.readFileText(directory.toString).attempt.map {
        case Left(failure) =>
          failure shouldBe an[IOException]
          failure.getMessage should not be empty
        case Right(text) =>
          fail(s"expected a failed IO, got ${text.length} characters")
      }
    }
  }

  test("readFileText suspends the read so that constructing the IO never throws") {
    // The file reader can be held to this more sharply than the classpath reader, and this is
    // how: the file '''exists''' when the description is built and is gone before the
    // description is evaluated. A reader that had read at construction time would hand back the
    // content it captured then; a reader that describes a read fails, because the file is no
    // longer there when the read finally happens. The second description, of a path nothing has
    // ever borne, adds the documented shape of that failure - the subject's own message with
    // the platform's report kept as the cause.
    withTempFile(HelloWorld) { path =>
      val describedWhilePresent = Resources.readFileText(path.toString)
      val describedAbsent = Resources.readFileText(AbsentFilePath)
      for {
        control <- describedWhilePresent
        _ <- IO.blocking(Files.delete(path))
        afterDeletion <- describedWhilePresent.attempt
        absent <- describedAbsent.attempt
      } yield {
        // The control: the very same description read the file whole while it was there, so
        // what follows is attributable to the deletion and to nothing else.
        control shouldBe HelloWorld
        val refusedAfterDeletion = refusalOf(afterDeletion, path.toString)
        refusedAfterDeletion shouldBe an[IOException]
        refusedAfterDeletion.getCause shouldBe a[NoSuchFileException]
        val refusedAbsent = refusalOf(absent, AbsentFilePath)
        refusedAbsent shouldBe an[IOException]
        refusedAbsent.getCause shouldBe a[NoSuchFileException]
      }
    }
  }

  //-------------------------------------------------------------------------
  test("Resources exposes only the two text-reading members, with no charset, locator or prefix-parsing API") {
    // Absence proofs. Every identifier inside these snippets resolves except the member
    // being denied, so each of them fails to compile for exactly one reason. Were any of
    // them to compile, this file would not compile either, and the object would have grown
    // a surface its documented contract rules out.
    //
    // The surface is exactly these two methods: `readClasspathText` and `readFileText`.
    // The second group of snippets below is what makes that a compile-time fact rather than
    // a claim - this spec sits in the same package as the subject, so a member the subject
    // shared with its package would be reachable from here, and each of those snippets is
    // rejected precisely because no member of the object other than the two readers is
    // reachable at all.
    IO {
      // No charset parameter: the character set is fixed at UTF-8 and never negotiated.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.readFileText("x", java.nio.charset.StandardCharsets.UTF_8)"""
      )
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.readClasspathText("x", java.nio.charset.StandardCharsets.UTF_8)"""
      )
      // No prefix parsing: "classpath:", "file:" and "url:" are not a language here.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.of("classpath:x")""")
      // No locator factories, and so no locator value type behind them.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.ofFile(new java.io.File("x"))""")
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.ofPath(java.nio.file.Paths.get("x"))""")
      // No URL support, and no archive support reached through one.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.ofUrl("file:/x")""")
      // Nor the classpath URL factory, which was the one remaining way into the locator
      // type: the argument is a string here, so this snippet is refused for the absence of
      // the member and for nothing else.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.ofClasspathUrl("file:/x")""")
      // And neither reader accepts a URL-typed argument. The URL expression itself is
      // well-typed, so what this snippet is refused for is the parameter type of the
      // reader: a name is text here, never a resolved address.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.readClasspathText(java.net.URI.create("file:/x").toURL())"""
      )
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.readFileText(java.net.URI.create("file:/x").toURL())"""
      )
      // No caller-sensitive lookup: the calling class plays no part in resolution.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.readClasspathText(classOf[String], "x")""")
      // The two readers themselves are reachable, which is what makes the denials above and
      // below denials of visibility and arity rather than of the path they are written with.
      assertCompiles("""com.opengamma.strata.collect.io.Resources.readClasspathText("x")""")
      assertCompiles("""com.opengamma.strata.collect.io.Resources.readFileText("x")""")
      // Nothing but those two. The byte ceiling is an implementation bound, so it cannot be
      // read; were it public, this snippet would compile and this case would fail.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.MaxBytes""")
      // The time bound is an implementation bound for the same reason, and is denied the same
      // way: a caller meets it only as the failure that names it.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.ReadTimeLimit""")
      // The shared read is not reachable, so no caller can perform a read under a limit of
      // its own choosing, nor with a stream of its own in place of the two acquisitions.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.readManaged("x", cats.effect.IO.pure(java.io.InputStream.nullInputStream()), 1)"""
      )
      // Nor is the acquisition-and-release pairing, so the stream lifetime of a read cannot
      // be taken apart and re-assembled by anything outside the object.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.managedStream(cats.effect.IO.pure(java.io.InputStream.nullInputStream()))"""
      )
      // Nor the cancellation protocol around that read: the fiber the read runs on, and the
      // close its cancellation brings forward, are the subject's own and cannot be composed
      // differently from outside.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.cancelableRead("x", java.io.InputStream.nullInputStream(), 1)"""
      )
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.closeTolerantly(java.io.InputStream.nullInputStream())"""
      )
      // Nor is the acquisition-and-release pairing, so the stream lifetime of a read cannot
      // be taken apart and re-assembled by anything outside the object.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.managedStream(cats.effect.IO.pure(java.io.InputStream.nullInputStream()))"""
      )
      // Nor either acquisition: a stream of the subject's own making never escapes it, so
      // the only way to obtain content through this object is a complete read. Each takes the
      // rendering of the name it reports alongside the name it looks up, and both arguments
      // here are well-typed strings, so these two snippets are refused for the visibility of
      // the member and for nothing else.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.openClasspathStream("parity/double-array-baseline.json", "parity/double-array-baseline.json")"""
      )
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.openFileStream("x", "x")""")
      // Nor the two halves of the classpath acquisition: how a name is located and opened, and
      // how a located name is judged to be a directory rather than a file, are the subject's own
      // decisions. Both arguments below are well-typed - two strings, and a URL - so each
      // snippet is refused for the visibility of the member and for nothing else.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.classpathEntry("x", "x")""")
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.denotesDirectory(java.net.URI.create("file:/x").toURL())"""
      )
      // Nor the wording a failure uses to name its source, nor the wrapping of a platform
      // failure in it: a caller cannot compose a diagnostic that looks like one of this
      // object's own, and cannot reach past the rendering by asking for the label directly.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.classpathSource("x")""")
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.fileSource("x")""")
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.unopenable("x", new java.io.IOException("y"))"""
      )
    }
  }

  //-------------------------------------------------------------------------
  // Names this reader does not interpret
  //
  // The absence proofs above deny the API that would interpret a name; the two cases below
  // are the behavioural half of the same statement. A name is used exactly as supplied, so a
  // name of a shape that elsewhere carries meaning - a URL, a prefixed locator string, a
  // path reaching inside an archive - names a resource or a file spelled exactly that way,
  // and the read of it fails because nothing bears that name. Each case carries a positive
  // control - the very same file and the very same resource, read under their plain names -
  // so the failure is attributable to the shape of the name rather than to absent or
  // unreadable content.
  //-------------------------------------------------------------------------
  test("a URL or a prefixed locator name is not interpreted and the read of it fails") {
    withTempFile(HelloWorld) { path =>
      // The same file that reads below, named with a scheme-like prefix in front of it.
      val prefixedFile = "file:" + path.toString
      // The same resource that reads below, named with the other such prefix.
      val prefixedResource = "classpath:" + FixturePath
      for {
        plainFile <- Resources.readFileText(path.toString)
        plainResource <- Resources.readClasspathText(FixturePath)
        urlName <- Resources.readClasspathText("url:file:/x").attempt
        prefixedResourceRead <- Resources.readClasspathText(prefixedResource).attempt
        prefixedFileRead <- Resources.readFileText(prefixedFile).attempt
      } yield {
        // The controls first: both sources exist and are readable under the names this
        // reader does take, so nothing below can be explained by missing content.
        plainFile shouldBe HelloWorld
        plainResource should not be empty
        // A name of URL shape is a resource name in its entirety, and the classpath holds no
        // entry called that - which is the absence failure, not a failure of anything that
        // was found.
        refusalOf(urlName, "url:file:/x") shouldBe a[FileNotFoundException]
        // "classpath:" is part of the resource name rather than a selector of the classpath,
        // which is why the read fails over a resource that plainly exists under the name the
        // control just read it by.
        refusalOf(prefixedResourceRead, prefixedResource) shouldBe a[FileNotFoundException]
        // "file:" likewise: the prefix belongs to the file name, so the platform reports
        // the file as absent even though the control read it a moment ago. The file reader
        // does not hand that report on as it stands, because its message is the raw path -
        // it names the source itself and carries the report as the cause, so the absence is
        // read there. Which of the two failures this is remains exactly as observable as it
        // was, and the case still turns on it.
        refusalOf(prefixedFileRead, prefixedFile).getCause shouldBe a[NoSuchFileException]
      }
    }
  }

  test("an archive is not opened and a path inside one is not resolved") {
    withTempBytes(ArchiveBytes) { path =>
      // An entry-inside-an-archive name, over a file that really begins with an archive header.
      val entryInsideArchive = path.toString + ArchiveEntrySuffix
      val entryInsideClasspathArchive = "parity/double-array-baseline.zip" + ArchiveEntrySuffix
      for {
        archive <- Resources.readFileText(path.toString).attempt
        insideArchive <- Resources.readFileText(entryInsideArchive).attempt
        insideClasspathArchive <- Resources.readClasspathText(entryInsideClasspathArchive).attempt
      } yield {
        // The archive itself: the only reader that takes a file path decodes strictly, and
        // an archive is not text, so its bytes never reach a caller as they stand. The file
        // is found and read, and it is the decode that ends the read, which the message
        // states.
        refusalOf(archive, path.toString).getMessage should include("UTF-8")
        // A name reaching inside the archive is not resolved: no archive is opened, so the
        // whole string, separator and all, is a file name that nothing bears. The failure
        // is therefore the platform's absence report rather than the decode above - the
        // read never reaches any content, which is what "not resolved" means here and what
        // tells this case apart from the one over the archive itself. The report is read
        // through the cause, for the reason given in the case above: the file reader names
        // its own source and keeps the platform's exception as the cause of that.
        refusalOf(insideArchive, entryInsideArchive).getCause shouldBe a[NoSuchFileException]
        // The same on the classpath side, where the original reached an entry through a
        // "jar:file:" URL: the archive-entry name is an ordinary resource name, and the
        // classpath holds no entry under it.
        refusalOf(insideClasspathArchive, entryInsideClasspathArchive) shouldBe a[FileNotFoundException]
      }
    }
  }

  //-------------------------------------------------------------------------
  // The byte ceiling
  //
  // The reader bounds what it will materialise, so the size of a source cannot decide the
  // memory of this process. The bound is private, so both cases below establish it the way any
  // caller would: by handing a public reader a source of a known size and observing what comes
  // back. The sources are sparse files, created by setting a length rather than by writing
  // bytes, so a case at sixty-four mebibytes costs no committed fixture and no disk.
  //
  // Both directions matter and each case catches one of them: a reader that stopped enforcing
  // the bound, or enforced it a byte too late, is caught by the refusal case, and a reader that
  // enforced it a byte too early - refusing a source of exactly the ceiling - is caught by the
  // boundary case.
  //-------------------------------------------------------------------------
  test("readFileText refuses a source one byte beyond the documented ceiling and reclaims its descriptor") {
    withSparseFile(DocumentedCeiling + 1L) { path =>
      for {
        outcome <- Resources.readFileText(path.toString).attempt
        open <- descriptorTargets
      } yield withClue(s"open descriptor targets after the refused read: ${open.mkString(", ")}: ") {
        outcome match {
          case Left(failure) =>
            failure shouldBe an[IOException]
            // the message has to name both the source and the limit, or a run that hit the
            // ceiling could not be told apart from a run that hit a corrupt file
            failure.getMessage should include(path.toString)
            failure.getMessage should include(s"$DocumentedCeiling byte limit")
            // the refusal happens after the stream is open, so the handle still has to come
            // back: this is release on the ceiling path, observed end to end
            open.count(target => target == path.toString) shouldBe 0
          case Right(text) =>
            fail(s"expected the ceiling to refuse the source, got ${text.length} characters")
        }
      }
    }
  }

  test("readFileText accepts a source of exactly the documented ceiling") {
    IO(Runtime.getRuntime.maxMemory).flatMap { heap =>
      if (heap < CeilingCaseHeap) {
        IO(
          cancel(
            s"this runner allows $heap bytes of heap, and decoding a source of " +
              s"$DocumentedCeiling bytes needs at least $CeilingCaseHeap; the refusal case " +
              "above still establishes that the ceiling is enforced"))
      } else {
        ceilingBoundaryCase
      }
    }
  }

  //-------------------------------------------------------------------------
  // The decode is strict
  //-------------------------------------------------------------------------
  test("readFileText fails the IO for content that is not valid UTF-8") {
    // Deliberately not a lenient substitution. The reader ingests captured baselines whose
    // numbers are the thing being measured, and a substitution character inside one is an
    // expectation nobody captured - so malformed input ends the read instead.
    withTempBytes(MalformedUtf8) { path =>
      Resources.readFileText(path.toString).attempt.map {
        case Left(failure) =>
          failure shouldBe an[IOException]
          failure.getMessage should include(path.toString)
          failure.getMessage should include("UTF-8")
        case Right(text) =>
          fail(s"expected malformed input to be refused, got '$text'")
      }
    }
  }

  //-------------------------------------------------------------------------
  // Resource lifetime
  //
  // Release is observed the only way the public surface allows: through the descriptors
  // this process holds after a read has finished. A reader that stopped closing what it
  // opens would leave a descriptor pointing at the source it had just read, and the two
  // cases below look for exactly that - once after reads that succeed, and once after a
  // read that acquires its stream and then fails in the decode, which is the outcome a
  // release tied to success alone would miss. Both cancel themselves, with the reason
  // printed, on a platform that does not expose the descriptor table.
  //-------------------------------------------------------------------------
  test("both public readers leave no open descriptor behind") {
    // After a read of a classpath resource and a read of a file, no descriptor of this
    // process still refers to either of them. This is the handle-reclamation property
    // itself rather than a proxy for it, and it holds only because both readers close what
    // they open.
    IO.blocking(Files.isDirectory(ProcessDescriptors)).flatMap { exposed =>
      if (!exposed) {
        IO(
          cancel(
            "this platform does not expose the descriptors of the running process, so " +
              "reclamation cannot be observed directly here; every other property of the " +
              "two readers is asserted by the cases around this one"))
      } else {
        IO.blocking(Option(getClass.getClassLoader.getResource(FixturePath))).flatMap { located =>
          located.map(_.getProtocol) match {
            case Some("file") => descriptorReclamationCase
            case other =>
              IO(
                cancel(
                  s"the classpath fixture resolves to $other rather than a file, so a " +
                    "descriptor that refers to it cannot be distinguished from one that refers " +
                    "to the archive holding it"))
          }
        }
      }
    }
  }

  test("a read that fails in the decode leaves no open descriptor behind either") {
    // The failure path of the same property. A file of malformed UTF-8 is opened and read
    // before the decode refuses it, so the stream is acquired and then abandoned by a
    // failing effect - the case a release that ran only on success would leak. Nothing here
    // waits or sleeps: the read is complete before the descriptor table is inspected,
    // because release happens inside the effect the case has already sequenced.
    IO.blocking(Files.isDirectory(ProcessDescriptors)).flatMap { exposed =>
      if (!exposed) {
        IO(
          cancel(
            "this platform does not expose the descriptors of the running process, so " +
              "reclamation on the failure path cannot be observed directly here; the " +
              "refusal itself is asserted by the malformed-UTF-8 case above"))
      } else {
        failedReadReclamationCase
      }
    }
  }

  //-------------------------------------------------------------------------
  // Cancellation of a read that is still under way
  //
  // The three descriptor observations above all look at a read that has finished. The two
  // cases below look at one that has not, which is the outcome the subject's cancellation
  // protocol exists for and the one that pairing acquisition with release cannot deliver on
  // its own: a read still inside its blocking call holds a thread and a descriptor until that
  // call returns, so a cancellation which merely queues behind it is no cancellation at all.
  //
  // Both cases are about sources rather than about fixtures, because that is what decides what
  // can be seen from outside the subject: one source yields endlessly, which makes a read
  // observably in flight, and the other is refused outright, which is why it cannot be used
  // for the first.
  //-------------------------------------------------------------------------
  test("readFileText cancels a read that is in flight promptly rather than waiting for the source") {
    IO.blocking(Files.isReadable(Paths.get(ContinuousSource))).flatMap { readable =>
      if (!readable) {
        IO(
          cancel(
            s"this platform offers no readable $ContinuousSource, so a read cannot be caught " +
              "in flight here; release after a read has finished is asserted by the three " +
              "cases above"))
      } else {
        inFlightCancellationCase
      }
    }
  }

  test("a named pipe is refused rather than read, so a source of that shape cannot make a read wait") {
    withNamedPipe(pipeRefusalCase)
  }

  //-------------------------------------------------------------------------
  // What this runner has to provide
  //
  // Four of the cases above cancel themselves where the machine cannot carry them, and each
  // prints the reason when it does. That is the right behaviour for the case - it has nothing to
  // say about a subject it could not exercise - but on its own it is invisible: a cancelled case
  // is not a failed one, and a run that quietly stopped exercising the ceiling, the descriptor
  // table, the endless device or a named pipe still reports itself as green. The case below is
  // what makes that visible. It asserts the five '''capabilities''' those cancellations rest on,
  // separately and each naming what it observed, so a runner that lacks one fails here and says
  // which one and why it matters, while the cases themselves still cancel with their reasons
  // rather than failing on a machine that was never going to carry them.
  //
  // Three other conditions those cases cancel on are deliberately '''not''' asserted here,
  // because they are outcomes of a race rather than properties of the machine: that a whole read
  // of the endless device is slow enough to measure, that a read has not ended on its own inside
  // the pause before it is cancelled, and that a read of a named pipe ends inside its bound. Any
  // of the three can go either way on a machine that provides every capability below, so
  // asserting one would make this suite fail intermittently for a reason that is not a defect.
  // They remain cancellations, with the measurement printed in the reason, which is what lets a
  // run that hit one be recognised by reading it.
  //-------------------------------------------------------------------------
  test("this runner provides every capability the cases above cancel themselves for the absence of") {
    for {
      heap <- IO(Runtime.getRuntime.maxMemory)
      descriptorsExposed <- IO.blocking(Files.isDirectory(ProcessDescriptors))
      fixtureProtocol <- IO.blocking(
        Option(getClass.getClassLoader.getResource(FixturePath)).map(_.getProtocol))
      continuousReadable <- IO.blocking(Files.isReadable(Paths.get(ContinuousSource)))
      pipeCapability <- namedPipeCapability
    } yield {
      // The ceiling case decodes sixty-four mebibytes and the text made from them at once.
      withClue(s"this runner allows $heap bytes of heap against the $CeilingCaseHeap the case " +
        "at the documented ceiling needs; run the suite with a larger -Xmx: ") {
        heap should be >= CeilingCaseHeap
      }
      // The three descriptor cases read the descriptor table of this process directly.
      withClue(s"$ProcessDescriptors is not a directory on this runner, so the descriptor " +
        "table cannot be read and handle reclamation cannot be observed; run the suite on a " +
        "platform that exposes it: ") {
        descriptorsExposed shouldBe true
      }
      // The descriptor case over a successful read distinguishes a handle on the fixture from a
      // handle on an archive holding it, which it can only do where the fixture is an ordinary
      // file. The protocol is read rather than assumed, because it is not the same under every
      // way of running this suite: a staged archive resolves it to "jar".
      withClue(s"the classpath fixture resolves to ${fixtureProtocol.getOrElse("nothing")} on " +
        "this runner rather than to a file, so run the suite as a forked test over exploded " +
        "class directories: ") {
        fixtureProtocol shouldBe Some("file")
      }
      // The cancellation case needs a source that yields without ending.
      withClue(s"$ContinuousSource is not readable on this runner, so no read can be caught in " +
        "flight and prompt cancellation cannot be observed: ") {
        continuousReadable shouldBe true
      }
      // And the pipe case needs a named pipe, which is made by running mkfifo.
      pipeCapability
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Creates a file holding the given text as UTF-8 bytes, hands its path to the case, and
   * deletes it afterwards.
   *
   * The write is kept inside the effect so that the file is created when the case runs
   * rather than when it is described, and the deletion is registered as a finalizer so it
   * happens on every outcome, a failed assertion included. Repeated runs therefore neither
   * accumulate files nor see one another's.
   *
   * @param contents  the text to write, which the case is expected to read back exactly
   * @param use  the case, given the absolute path of the file
   * @return the effect of the case, with creation and deletion around it
   */
  private def withTempFile(contents: String)(use: Path => IO[Assertion]): IO[Assertion] =
    IO(Files.createTempFile("resources-spec-", ".txt")).flatMap { path =>
      IO(Files.write(path, contents.getBytes(StandardCharsets.UTF_8)))
        .void
        .flatMap(_ => use(path))
        .guarantee(IO(Files.deleteIfExists(path)).void)
    }

  /**
   * Creates an empty directory, hands its path to the case, and deletes it afterwards.
   *
   * The directory is left empty, so the deletion in the finalizer always succeeds; a case
   * that needs a path inside it resolves one without creating it.
   *
   * @param use  the case, given the absolute path of the directory
   * @return the effect of the case, with creation and deletion around it
   */
  private def withTempDirectory(use: Path => IO[Assertion]): IO[Assertion] =
    IO(Files.createTempDirectory("resources-spec-dir-")).flatMap { directory =>
      use(directory).guarantee(IO(Files.deleteIfExists(directory)).void)
    }

  /**
   * Creates a file holding the given bytes verbatim, hands its path to the case, and deletes
   * it afterwards.
   *
   * The byte-level counterpart of `withTempFile`, for content that is deliberately not text:
   * writing it as a string would encode it and so destroy the very property under test.
   *
   * @param contents  the bytes to write
   * @param use  the case, given the absolute path of the file
   * @return the effect of the case, with creation and deletion around it
   */
  private def withTempBytes(contents: Array[Byte])(use: Path => IO[Assertion]): IO[Assertion] =
    IO(Files.createTempFile("resources-spec-bytes-", ".bin")).flatMap { path =>
      IO(Files.write(path, contents))
        .void
        .flatMap(_ => use(path))
        .guarantee(IO(Files.deleteIfExists(path)).void)
    }

  /**
   * Asserts that a read refused the name it was given and named it back, then yields the
   * failure so the caller can pin down which refusal it was.
   *
   * Both cases above make this assertion over several names, and the message matters as
   * much as the failure: a refusal that did not say what it refused could not be told from
   * any other failed read. Returning the failure rather than asserting its type here is
   * what lets each caller distinguish the two ways a name can be unusable - a source that
   * does not exist under it, and a source that exists and is not text - which is the
   * distinction those cases turn on. The outcome is inspected as a value, as everywhere
   * else in this file, so nothing here catches an exception.
   *
   * @param outcome  the outcome of the read, as produced by `attempt`
   * @param named  the name handed to the reader, which its failure must name back
   * @return the failure the read produced
   */
  private def refusalOf(outcome: Either[Throwable, String], named: String): Throwable =
    outcome match {
      case Left(failure) =>
        failure shouldBe an[IOException]
        failure.getMessage should include(named)
        failure
      case Right(text) =>
        fail(s"expected the read of '$named' to fail, got ${text.length} characters")
    }

  /**
   * Asserts that a read failed and yields the text of its failure.
   *
   * The counterpart of `refusalOf` for the cases about how a failure names its source.
   * `refusalOf` cannot serve those: it asserts that the message holds the name it was given,
   * which is the very thing they are about - a name carrying a line feed, or a name of some
   * thousands of characters, must '''not''' reach the message as it stands. So this asserts
   * only that the read failed, and hands the message back to be examined.
   *
   * @param outcome  the outcome of the read, as produced by `attempt`
   * @return the message of the failure, which every failure of this subject carries
   */
  private def refusalMessageOf(outcome: Either[Throwable, String]): String =
    outcome match {
      case Left(failure) =>
        failure shouldBe an[IOException]
        val message = Option(failure.getMessage).getOrElse("")
        message should not be empty
        message
      case Right(text) =>
        fail(s"expected the read to fail, got ${text.length} characters")
    }

  /**
   * Runs a case with the error stream of this process replaced by a capture, and hands back
   * what reached it.
   *
   * Three things make the capture usable as evidence. It settles before it is read, because a
   * report written by the runtime rather than by the read need not have been written by the
   * time the read's value is in hand. It then writes a '''canary''' - a stack trace of this
   * spec's own making - so that a case asserting the capture holds nothing of the subject can
   * first assert that the capture holds something, which is what tells an empty capture apart
   * from a capture that was never connected. And the original stream is restored by a
   * finalizer, so it is restored on every outcome: a case that failed while the stream was
   * swapped would otherwise take the error stream of the whole process with it.
   *
   * @param use  the case whose error output is to be captured
   * @return the value of the case, and the text that reached the error stream
   */
  private def withCapturedStandardError[A](use: IO[A]): IO[(A, String)] =
    IO.blocking((new ByteArrayOutputStream(), System.err)).flatMap { case (captured, original) =>
      IO.blocking(System.setErr(new PrintStream(captured, true, StandardCharsets.UTF_8)))
        .flatMap(_ => use)
        .flatMap(value =>
          IO.sleep(DiagnosticSettle)
            .flatMap(_ =>
              IO.blocking {
                new RuntimeException(StandardErrorCanary).printStackTrace()
                System.err.flush()
              }
            )
            .map(_ => (value, captured.toString(StandardCharsets.UTF_8)))
        )
        .guarantee(IO.blocking(System.setErr(original)))
    }

  /**
   * Asserts that a classpath name which denotes no readable file is refused, in the wording the
   * classpath in force implies.
   *
   * Whether a class loader resolves such a name is a property of the classpath rather than of
   * the subject: exploded class directories resolve a directory name to a location, an archive
   * resolves some and not others, and the empty name resolves to the root of the first kind and
   * to nothing on the second. So the location is read first and the assertion follows it - the
   * directory wording where there was something to reject, the absence wording where the name
   * reached nothing at all. Both branches assert a '''failure''': the one outcome no classpath
   * may produce for a name like this is content, which is exactly what the subject used to
   * produce. Neither branch cancels, so this case reports on every runner.
   *
   * @param name  the resource name, which must denote no readable file
   * @return the effect of the assertions
   */
  private def refusalOfANameThatDenotesNoFile(name: String): IO[Assertion] =
    for {
      located <- IO.blocking(Option(getClass.getClassLoader.getResource(name)).map(_.getProtocol))
      outcome <- Resources.readClasspathText(name).attempt
    } yield {
      val message = refusalMessageOf(outcome)
      withClue(
        s"the name '$name' resolved to ${located.getOrElse("nothing")} and the message was " +
          s"'$message': ") {
        // One line and naming the name, as every message of this subject is.
        message.linesIterator.size shouldBe 1
        message should endWith(name)
        located match {
          // A location the loader resolved, so the refusal says what was found rather than
          // claiming the name reached nothing.
          case Some(_) => message should startWith(DirectoryRefusal)
          // Nothing resolved, so this is the absence the reader has always reported.
          case None => message should startWith(AbsentRefusal)
        }
      }
    }

  /**
   * Creates a file of the given length whose every byte is zero, hands its path to the case,
   * and deletes it afterwards.
   *
   * The length is '''set''' rather than written, so the file is sparse: it occupies no space on
   * a file system that supports holes, it is created in constant time, and a read of it yields
   * that many zero bytes. Those bytes are valid UTF-8 - each is the NUL character - so the
   * content is text the reader accepts, which is what the case at the ceiling needs. This is
   * how a source of sixty-four mebibytes is exercised without committing a fixture of that
   * size or writing one byte at a time.
   *
   * @param length  the length to give the file, in bytes
   * @param use  the case, given the absolute path of the file
   * @return the effect of the case, with creation and deletion around it
   */
  private def withSparseFile(length: Long)(use: Path => IO[Assertion]): IO[Assertion] =
    IO.blocking(Files.createTempFile("resources-spec-sparse-", ".bin")).flatMap { path =>
      IO.blocking {
        val handle = new RandomAccessFile(path.toFile, "rw")
        try handle.setLength(length)
        finally handle.close()
      }.flatMap(_ => use(path))
        .guarantee(IO.blocking(Files.deleteIfExists(path)).void)
    }

  /**
   * Reads a source of exactly the documented ceiling, which the reader must accept whole.
   *
   * Decoding sixty-four mebibytes of NUL bytes costs a few hundred mebibytes of heap while the
   * text exists, which is why the case that calls this asks about the heap first rather than
   * risking a memory failure that would look like a defect in the reader.
   */
  private def ceilingBoundaryCase: IO[Assertion] =
    withSparseFile(DocumentedCeiling) { path =>
      Resources.readFileText(path.toString).map { text =>
        // read whole: the boundary is inclusive, so a reader that refused at the ceiling
        // rather than beyond it would fail here
        text.length.toLong shouldBe DocumentedCeiling
        // and the content is the text those bytes name, decoded rather than substituted
        text.charAt(0) shouldBe '\u0000'
        text.charAt(text.length - 1) shouldBe '\u0000'
      }
    }

  /**
   * Reads a classpath resource and a file through the public API, then asserts that no
   * descriptor of this process refers to either of them.
   *
   * The descriptor table is compared by the target each entry points at rather than by how
   * many entries there are, because loading a class or touching an archive opens descriptors
   * of its own and a count would drift with them. The absolute path of the temporary file and
   * the file name of the fixture are both specific enough to identify a leaked handle.
   */
  private def descriptorReclamationCase: IO[Assertion] =
    withTempFile(HelloWorld) { path =>
      for {
        _ <- Resources.readClasspathText(FixturePath)
        _ <- Resources.readFileText(path.toString)
        open <- descriptorTargets
      } yield withClue(s"open descriptor targets after both reads: ${open.mkString(", ")}: ") {
        open.count(target => target == path.toString) shouldBe 0
        open.count(target => target.endsWith(FixtureFileName)) shouldBe 0
      }
    }

  /**
   * Reads a file of malformed UTF-8 through the public API, asserts that the read failed,
   * and then asserts that no descriptor of this process refers to that file.
   *
   * The failure-path counterpart of `descriptorReclamationCase`, and the same mechanism:
   * one absolute temporary path, matched against the targets of the descriptor table by
   * equality. Malformed content is what makes the read fail after its stream has been
   * acquired - the bytes are read, then refused by the decoder - so a release that ran only
   * on success would show up here as a descriptor still pointing at the file.
   */
  private def failedReadReclamationCase: IO[Assertion] =
    withTempBytes(MalformedUtf8) { path =>
      for {
        outcome <- Resources.readFileText(path.toString).attempt
        open <- descriptorTargets
      } yield withClue(s"open descriptor targets after the failed read: ${open.mkString(", ")}: ") {
        // Without this the case could pass over a read that never opened anything at all.
        outcome.isLeft shouldBe true
        open.count(target => target == path.toString) shouldBe 0
      }
    }

  /**
   * Creates a named pipe, hands its path to the case, and removes it afterwards.
   *
   * A named pipe is the one source this spec can point the subject at that opens and then
   * never yields, which is what the cancellation case needs and what no ordinary file can
   * provide. Making one is not something the platform exposes through its file API, so it is
   * made by running `mkfifo`; a platform that has no usable `mkfifo` cancels the case with the
   * reason stated rather than failing it, exactly as the descriptor cases cancel themselves
   * where the descriptor table is not exposed.
   *
   * The pipe and the directory holding it are removed by a finalizer, so they are removed on
   * every outcome - a cancelled case, a failed assertion and a passing run alike - and the
   * removal tolerates a writing end that is still open, because unlinking a pipe another
   * handle refers to is permitted and is what a finalizer has to be able to do.
   *
   * @param use  the case, given the absolute path of the pipe
   * @return the effect of the case, with creation and removal around it
   */
  private def withNamedPipe(use: Path => IO[Assertion]): IO[Assertion] =
    IO.blocking(Files.createTempDirectory("resources-spec-pipe-")).flatMap { directory =>
      val pipe = directory.resolve(PipeName)
      makeNamedPipe(pipe)
        .flatMap {
          case Some(reason) => IO(cancel(reason))
          case None => use(pipe)
        }
        .guarantee(
          IO.blocking(Files.deleteIfExists(pipe)).void *>
            IO.blocking(Files.deleteIfExists(directory)).void)
    }

  /**
   * Asserts that this runner can make a named pipe, and removes the one it made.
   *
   * The capability is asserted by exercising it rather than by looking for the command, because
   * a command that exists and cannot be run is the case the pipe case above cancels on. It uses
   * the same `mkfifo` call that case uses, so the two cannot disagree, and it asserts three
   * things about the result: that nothing was reported as a reason not to proceed, that the pipe
   * is there, and that it is not an ordinary file - a platform whose `mkfifo` quietly produced a
   * regular file would satisfy the first two and would not give the pipe case the source it
   * needs.
   *
   * The pipe and the directory holding it are removed by a finalizer, so they are removed on
   * every outcome, a failed assertion included.
   *
   * @return the effect of the assertions
   */
  private def namedPipeCapability: IO[Assertion] =
    IO.blocking(Files.createTempDirectory("resources-spec-capability-")).flatMap { directory =>
      val pipe = directory.resolve(PipeName)
      makeNamedPipe(pipe)
        .flatMap(refusal =>
          IO.blocking((Files.exists(pipe), Files.isRegularFile(pipe))).map {
            case (present, ordinary) =>
              withClue(
                s"mkfifo on this runner reported '${refusal.getOrElse("no reason not to proceed")}' " +
                  s"and left present=$present, ordinaryFile=$ordinary at $pipe; the pipe case " +
                  "cannot run without a usable mkfifo: ") {
                refusal shouldBe None
                present shouldBe true
                ordinary shouldBe false
              }
          }
        )
        .guarantee(
          IO.blocking(Files.deleteIfExists(pipe)).void *>
            IO.blocking(Files.deleteIfExists(directory)).void)
    }

  /**
   * Runs `mkfifo` for the given path, and says why the case cannot run if it could not.
   *
   * The wait is bounded, so a platform on which the command hangs cancels the case instead of
   * hanging the suite, and every way it can fail - absent command, non-zero status, an
   * interrupted wait - is reported as a reason rather than as an exception, because none of
   * them is a defect in the subject. The child's output is discarded, so nothing of this
   * process waits on a stream the case never reads.
   *
   * @param pipe  where the pipe should be created
   * @return the reason the case cannot run, or nothing when the pipe was created
   */
  private def makeNamedPipe(pipe: Path): IO[Option[String]] =
    IO.interruptible {
      new ProcessBuilder("mkfifo", pipe.toString)
        .redirectOutput(ProcessBuilder.Redirect.DISCARD)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
        .waitFor()
    }.timeout(MakePipeBound)
      .attempt
      .map {
        case Right(0) => None
        case Right(status) =>
          Some(
            s"mkfifo exited with status $status here, so no named pipe could be created and " +
              "what this subject does with one is not established by this run; the case above " +
              "establishes prompt cancellation, and it needs no pipe")
        case Left(failure) =>
          Some(
            s"this platform has no usable mkfifo (${failure.getClass.getName}: " +
              s"${Option(failure.getMessage).getOrElse("")}), so no named pipe could be created " +
              "and what this subject does with one is not established by this run; the case " +
              "above establishes prompt cancellation, and it needs no pipe")
      }

  /**
   * Measures a whole read of the endless device, then cancels one in flight and compares.
   *
   * The first read is the yardstick, and measuring it is what makes the case independent of
   * the machine: it is the cost of reading the subject's ceiling from this source here and
   * now, and it ends in the ceiling refusal because the device never ends. A cancellation that
   * waited for the blocking call to return would cost most of that, since it is delivered in
   * the first tenth of a read; a cancellation that interrupts the call and closes the handle
   * costs a fraction of it. A machine on which a whole read is too quick to measure is told
   * so, rather than being asserted against noise.
   *
   * @return the effect of the case
   */
  private def inFlightCancellationCase: IO[Assertion] =
    for {
      startedWhole <- IO.monotonic
      _ <- Resources.readFileText(ContinuousSource).attempt
      finishedWhole <- IO.monotonic
      whole = finishedWhole - startedWhole
      assertion <-
        if (whole < MeasurableRead) {
          IO(
            cancel(
              s"a whole read of $ContinuousSource took $whole here, which is under the " +
                s"$MeasurableRead this case compares against, so the time a cancellation takes " +
                "cannot be told from scheduling noise on this machine"))
        } else {
          cancelReadInFlight(whole)
        }
    } yield assertion

  /**
   * Cancels a read of the endless device part way through and asserts what the cancellation
   * did.
   *
   * The order of the steps is the substance of the case:
   *
   *  - the descriptor table is read '''before''' the read starts, because this source is one
   *    the runtime itself holds open, so what the case can assert is that the cancelled read
   *    left nothing '''new''' behind rather than that nothing refers to the device at all;
   *  - the pause lets the read reach its blocking call, so the cancellation is delivered to a
   *    read in flight rather than to the effect around one. Both are cancellation paths; only
   *    the first is the one that could not be prompt;
   *  - `cancel` completes only once the read's finalizers have run, so the time it takes
   *    '''is''' the time the handle takes to come back. It is bounded as well as measured, so
   *    a protocol that waited for the read would fail this case rather than hang it;
   *  - the outcome is examined before anything is concluded from the timing, because a read
   *    that had already ended would be "cancelled" instantly and would prove nothing.
   *
   * @param whole  the measured cost of a whole read of the same source
   * @return the effect of the assertions
   */
  private def cancelReadInFlight(whole: FiniteDuration): IO[Assertion] =
    for {
      held <- descriptorTargets.map(_.count(target => target == ContinuousSource))
      reader <- Resources.readFileText(ContinuousSource).start
      _ <- IO.sleep(ReadEntryPause)
      startedAt <- IO.monotonic
      _ <- reader.cancel.timeout(PromptCancellation)
      finishedAt <- IO.monotonic
      outcome <- reader.join
      open <- descriptorTargets
      cancellation = finishedAt - startedAt
      ended = !outcome.fold(
        canceled = true,
        errored = (_: Throwable) => false,
        completed = (_: IO[String]) => false)
      assertion <-
        if (ended) {
          IO(
            cancel(
              s"the read ended on its own inside the $ReadEntryPause before the cancellation " +
                s"reached it - a whole read of $ContinuousSource was measured at $whole - so " +
                "nothing was cancelled and this run observes nothing about cancellation"))
        } else {
          IO {
            info(
              s"a whole read of $ContinuousSource took $whole; cancelling one in flight took " +
                s"$cancellation")
            // The clue names the two measurements and the two descriptor counts rather than
            // the whole descriptor table: a table of a hundred entries in a failure message
            // buries the three numbers that say what went wrong.
            withClue(
              s"a whole read took $whole, the cancellation took $cancellation, and " +
                s"${open.count(target => target == ContinuousSource)} descriptors named " +
                s"$ContinuousSource afterwards against $held before: ") {
              // Prompt relative to the source itself, which is the comparison that separates a
              // cancellation that interrupted the read from one that queued behind it.
              (cancellation.toNanos * PromptnessFactor) should be < whole.toNanos
              // And release while the read was still under way: no descriptor names the source
              // that did not name it before the read started.
              open.count(target => target == ContinuousSource) shouldBe held
            }
          }
        }
    } yield assertion

  /**
   * Reads a named pipe through the public API and asserts that it is refused, promptly, with
   * its handle reclaimed.
   *
   * The writing end is opened '''before''' the read starts and is never written to, because a
   * pipe's open completes only once both ends are open: without it the subject would wait in
   * its own open, which is a different path from the one this case is about. Joining that
   * fiber is therefore the proof that the subject got past its open and reached the read, with
   * no delay guessed at. The join is bounded, so a subject that never opens fails this case
   * rather than hanging it; if that happens the writer fiber is left inside a native open,
   * which no platform makes interruptible, and it costs one daemon thread of a run that has
   * already failed.
   *
   * What the read then does is the point: the subject reads its source with one bulk call, that
   * call needs a seekable channel, and a pipe has none - so the read is refused where a file
   * would have been read. A platform that reads pipes instead reaches the bound below and
   * cancels the case, since nothing here is a claim about the subject in that event.
   *
   * @param pipe  the named pipe to read
   * @return the effect of the case
   */
  private def pipeRefusalCase(pipe: Path): IO[Assertion] =
    for {
      opening <- IO.interruptible(Files.newOutputStream(pipe)).start
      reader <- Resources.readFileText(pipe.toString).attempt.start
      sink <- opening.joinWithNever.timeout(PipeOpenBound)
      // The close in the finalizer is the second one: the case closes the writing end itself,
      // in its own order, and this closes it on the paths where the case never got that far.
      // Closing an already closed stream is tolerated, as it is in the subject.
      assertion <- pipeRefusal(reader, sink, pipe).guarantee(closeQuietly(sink))
    } yield assertion

  /**
   * Waits for the refusal, closes the writing end and inspects the descriptor table.
   *
   * The writing end is closed only once the read has ended, because closing it would end the
   * read by itself; once it is closed, nothing of this process should refer to the pipe, which
   * is what the descriptor table is inspected for.
   *
   * @param reader  the fiber running the read of the pipe
   * @param sink  the writing end of the pipe, which keeps the subject's open from waiting
   * @param pipe  the path of the pipe, as it appears in the descriptor table
   * @return the effect of the assertions
   */
  private def pipeRefusal(
      reader: FiberIO[Either[Throwable, String]],
      sink: OutputStream,
      pipe: Path): IO[Assertion] =
    for {
      outcome <- reader.joinWithNever.map(Option(_)).timeoutTo(PipeReadBound, IO.pure(None))
      _ <- closeQuietly(sink)
      exposed <- IO.blocking(Files.isDirectory(ProcessDescriptors))
      open <- descriptorTargets
      assertion <- outcome match {
        case None =>
          IO(
            cancel(
              s"the read of a named pipe here did not end within $PipeReadBound, so this " +
                "platform reads pipes rather than refusing them and the case has nothing to " +
                "conclude; the cancellation case above is where promptness is established"))
        case Some(refused) =>
          IO {
            withClue(
              s"${open.count(target => target == pipe.toString)} descriptors named the pipe " +
                "after the read of it: ") {
              refused match {
                case Left(failure) =>
                  // A refusal, and one this subject reports as a failed effect like any other.
                  failure shouldBe an[IOException]
                  Option(failure.getMessage) should not be empty
                case Right(text) =>
                  fail(s"expected the pipe to be refused, got ${text.length} characters")
              }
              // Release on that path as on every other: the read opened the pipe before it was
              // refused, so a handle that was not given back would show up here.
              if (exposed) open.count(target => target == pipe.toString) shouldBe 0
              else succeed
            }
          }
      }
    } yield assertion

  /** Closes a stream of this spec's own, treating a failure to close as nothing to report. */
  private def closeQuietly(stream: OutputStream): IO[Unit] =
    IO.blocking(stream.close()).handleError(_ => ())

  /** The targets of the descriptors this process currently has open. */
  private def descriptorTargets: IO[Vector[String]] =
    IO.blocking {
      Option(new File(ProcessDescriptors.toString).listFiles())
        .map(_.toVector)
        .getOrElse(Vector.empty[File])
        .flatMap(entry => Try(Files.readSymbolicLink(entry.toPath).toString).toOption)
    }
}

// ---------------------------------------------------------------------------
// Mapping from the original test class
// ---------------------------------------------------------------------------
//
// Every one of the fifteen cases of the original is accounted for below. Six had
// counterparts. The other nine tested API the port does not have, and they divide in two:
// five of them - the four prefixed forms and the value-equality case - performed a read or
// asserted a property that is still reachable another way, and each of those is
// consolidated onto the case here that covers the part of its behaviour which survives;
// the remaining four - two archive cases and two URL cases - performed reads that no
// member here can perform at all, and each is consolidated onto the case that asserts what
// the port does with a name of that shape instead, which is refuse it.
//
// ---- Reading a file -------------------------------------------------------
//
// test_ofFile
//   -> "readFileText reads a temporary UTF-8 file created with java.nio.file.Files"
//      ported. The original read a fixture file checked in beside it, through a relative
//      path resolved against the working directory. Tests here run in a forked process, so
//      the file is created under the temporary directory of the platform instead and named
//      absolutely; its content is the content of that fixture, byte for byte.
// test_ofPath
//   -> "readFileText reads a temporary UTF-8 file created with java.nio.file.Files"
//      consolidated. The original distinguished a File from a Path because the locator had
//      a factory for each. The port takes the path as text, so both factories reduce to one
//      call and there is one case rather than two.
//
// ---- Reading a classpath resource -----------------------------------------
//
// test_ofClasspath_relativeConvertedToAbsolute
//   -> "readClasspathText reads the committed parity fixture from the test classpath"
//      ported. The relative form, which is the form a class loader takes.
// test_ofClasspath_absolute
//   -> "readClasspathText accepts an absolute, slash-prefixed resource path"
//      ported. The port removes one leading separator, exactly as the original did, so the
//      case asserts that both forms return one content.
// test_ofClasspath_withClass_absolute
//   -> "readClasspathText accepts an absolute, slash-prefixed resource path"
//      consolidated. The class argument selected the loader and anchored a relative name.
//      Resolution here is neither caller-sensitive nor class-relative - which the absence
//      proofs deny directly - so what remains of this case is the absolute form above.
// test_ofClasspath_withClass_relative
//   -> "readClasspathText reads the committed parity fixture from the test classpath"
//      consolidated, for the same reason: what remains is the relative form.
//
// ---- The prefixed locator forms -------------------------------------------
//
// test_of_fileNoPrefix
//   -> "readFileText reads a temporary UTF-8 file created with java.nio.file.Files"
//      consolidated. An unprefixed string named a file, which is what the file reader now
//      takes; the locator string it also asserted has no counterpart.
// test_of_filePrefixed
//   -> "readFileText reads a temporary UTF-8 file created with java.nio.file.Files"
//      consolidated. The "file:" prefix is not parsed; the read it selected is this one.
// test_of_classpath
//   -> "readClasspathText reads the committed parity fixture from the test classpath"
//      consolidated. The "classpath:" prefix is not parsed; the read it selected is this one.
// test_of_invalid
//   -> "readClasspathText fails the IO for a resource that is not on the classpath"
//      consolidated. A locator string that could not be interpreted was rejected eagerly by
//      throwing. There is nothing to interpret now, so the failure that replaces it is the
//      one an unusable name actually produces: a name the classpath does not hold, reported
//      as a failed effect and observed as a value.
//
// ---- Archives -------------------------------------------------------------
//
// test_ofPath_zipFile
//   -> "an archive is not opened and a path inside one is not resolved"
//      consolidated. The original obtained the archive as a byte source and asserted the
//      four bytes of its local-file header. Neither the byte source nor archive support is
//      ported, so those bytes are unreachable through this API and no case here can assert
//      them. What replaces the case is the port's refusal: the case named above writes a
//      file beginning with those same four bytes and shows that the only reader which
//      takes it ends the read rather than yielding them, because it decodes strictly and
//      an archive is not text.
// test_ofPath_fileInZipFile
//   -> "an archive is not opened and a path inside one is not resolved"
//      consolidated. The original opened an archive file system, resolved an entry inside
//      it and reached that entry through a "url:jar:file:" locator. None of the three -
//      the archive file system, the locator, the URL form - is ported, so what replaces the
//      case is the port's refusal to resolve such a name: the entry name is a file name
//      like any other, nothing bears it, and the read fails over it while the same archive
//      under its own name fails for the different reason above.
//
// ---- URLs -----------------------------------------------------------------
//
// test_ofUrl
//   -> "a URL or a prefixed locator name is not interpreted and the read of it fails"
//      consolidated. The URL factory is not ported, and neither reader accepts a URL-typed
//      argument; the absence proofs deny both. No case here can therefore perform the read
//      this one performed, and what stands in its place is the behaviour that replaces it:
//      a name of URL shape is used literally, so the read of it fails, against the control
//      of the same file read by its plain name in the same case.
// test_ofClasspathUrl
//   -> "a URL or a prefixed locator name is not interpreted and the read of it fails"
//      consolidated, for the same reason and with an absence proof of its own. The nearest
//      name to the classpath URL this case built is the prefixed classpath name, which the
//      case named above shows is refused while the control reads the very same resource
//      under its plain name.
//
// ---- Value equality -------------------------------------------------------
//
// test_equalsHashCode
//   -> "readClasspathText is referentially transparent: the same path yields the same
//      content on repeated evaluation"
//      consolidated. There is no locator value type to compare, so the contract this case
//      asserted does not exist. The property worth keeping is the one that replaced it:
//      naming a resource twice describes the same read, and evaluating that description
//      twice yields the same content.
//
// ---------------------------------------------------------------------------
// The seven cases named above are every case in this file that an original case landed on.
// The other twenty-two of the twenty-nine have no origin in the original class and are
// additive, because they hold the port to promises the original could not make - its reads
// happened where they were written and it reported failure by throwing. They are:
//
//   * the two laziness cases, one per reader: describing a read performs none of it. Each
//     asserts the description it built rather than only that building it returned - two
//     constructions are two values and one description fails identically however often it is
//     evaluated, and on the file side the file exists when the description is built and is
//     deleted before it is evaluated, which is what a reader that read at construction time
//     would fail;
//   * the two remaining failure cases - a file that does not exist, and a path that is not
//     a readable file - which hold the port to its promise that a source it cannot obtain
//     fails the effect rather than yielding a sentinel or empty text;
//   * the JSON-structure case, which shows the fixture survives the read as text a parser
//     accepts, and the multi-byte case, which shows a decode of more bytes than characters;
//   * the absence proofs, which deny the whole unported surface at compile time;
//   * the two ceiling cases - a source of one byte beyond the documented limit, which is
//     refused, and a source of exactly the limit, which is read;
//   * the strict-decode case, which refuses malformed input instead of substituting for it;
//   * the two descriptor cases, which show that nothing of this process refers to a source
//     once both public readers have read one each, and once a read has failed with its
//     stream already open. The refused ceiling case above observes the same table on the
//     third path, so release is established on every outcome a caller can reach;
//   * the three cases about how a failure names its source - a name carrying the characters a
//     forged record is made of, a pair of overlong names, and the wrapping of the platform's
//     own failure of an open - which hold the port to a diagnostic that is one line and does
//     not grow with the name it quotes, while keeping the platform's report reachable as the
//     cause. The absent-resource case above carries the third property of that family, that
//     an ordinary name is quoted back character for character;
//   * the two cases about a read that has not finished: the cancellation case, which catches a
//     read of an endless device in flight and shows its cancellation costing a fraction of
//     what the source needs for a whole read, with no descriptor left behind - release while a
//     read is still under way, which the three descriptor observations above cannot reach
//     because each of them looks at a read that has ended - and the pipe case, which
//     establishes that a named pipe is refused rather than read, which is why the first uses
//     a device and not a pipe;
//   * the three cases about what the classpath reader reads: a name denoting a directory of the
//     classpath and the empty name, each refused rather than read - the reader takes the subset
//     of the classpath that names files, and a name outside it fails where an absent name fails
//     instead of yielding the shape of the classpath as content - and the whole-fixture case,
//     which shows that refusing those names narrowed nothing, by reading every character of the
//     committed fixture under both spellings of its name;
//   * the diagnostics case, which holds a failure to one destination: a refused read reports
//     itself to its caller as a value and writes nothing about itself to the error stream, so a
//     caller that handles the failure is not also reading it in a log;
//   * the capability case, which asserts the five properties of the runner that the four
//     cancelling cases above rest on, so a machine that cannot carry one of them fails this
//     file rather than passing it with a case silently skipped.
// ---------------------------------------------------------------------------
