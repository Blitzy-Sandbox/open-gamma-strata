/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

import scala.util.Try

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
 * `readFileText` is exercised against files this spec creates under the temporary
 * directory of the platform and deletes again. Tests run in a forked process, so nothing
 * here may lean on the working directory: every path handed to the subject is absolute and
 * comes from `Files.createTempFile` or `Files.createTempDirectory`, and every one of them
 * is removed by a finalizer that runs whether the case passed, failed or was cancelled.
 *
 * ===What is ported, and what has no counterpart===
 *
 * The original of this spec tested a locator value type whose surface was far wider than
 * the port keeps. Six of its fifteen cases have counterparts here: the two that read a
 * file through a `File` and through a `Path`, and the four that read a classpath resource
 * by absolute name, by relative name, and by either form resolved against a class. Each of
 * those asserted the same pair of properties - the first byte of the content, then the
 * decoded lines - and that pair is what the "reads it" and "decodes it" case of each member
 * below preserves. The remaining nine cases tested the prefixed locator forms
 * ("classpath:", "file:", "url:"), archives, URLs and the value equality of the locator
 * type itself. None of that API is ported, so no case here can perform the reads those
 * nine performed; what the port offers in their place is its refusal to interpret such a
 * name at all, and the two cases under "Names the port does not interpret" assert exactly
 * that - each against a positive control that reads the same file and the same resource
 * under their plain names, so a refusal cannot be mistaken for missing content. The mapping
 * at the foot of this file records where every one of the fifteen landed.
 *
 * Most of the cases here are additive rather than ported, and the note at the foot of this
 * file counts them and says why each family exists. The port promises a byte ceiling, a
 * strict decode, a handle released on every outcome, a read that happens only when the
 * effect is run, and a failed effect for any source it cannot obtain; the original could
 * promise none of those, because its reads happened where they were written and it reported
 * failure by throwing. Every failure here is observed through `attempt` as a value.
 *
 * The two negative cases are additive for a reason of their own. The original had none for
 * a missing classpath resource or an unreadable file - its only negative case concerned
 * parsing a prefixed locator string - whereas the port promises that a resource it cannot
 * obtain makes the effect fail rather than yielding a sentinel or empty text. That promise
 * is part of the contract, so it is tested.
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
 * Two properties of the implementation are not asserted, deliberately rather than by
 * omission, because observing either would mean the subject shipping a seam for the purpose:
 * release when a read is '''cancelled''', and release when the '''acquisition''' itself fails,
 * where there is no handle to reclaim. Both belong to the pairing of acquisition with release
 * that the subject delegates to `cats.effect.Resource`, and neither can be reached from
 * outside the object: a cancellation would have to be delivered while a blocking read is in
 * flight, which no caller can observe or time deterministically, and a failed acquisition
 * leaves nothing to look for.
 */
final class ResourcesSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  /** The committed parity fixture, named exactly as its own consumers name it. */
  private val FixturePath = "parity/double-array-baseline.json"

  /** A resource name the classpath does not hold, used by the failure and laziness cases. */
  private val AbsentResource = "parity/no-such-fixture.json"

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
   * The first four are the local-file header the original's archive case asserted, in the
   * order it asserted them (80, 75, 3, 4); the two after them are a malformed UTF-8
   * sequence, which is what the rest of an archive looks like to a text reader. Written as
   * bytes rather than as a string because encoding a string would destroy the property
   * under test - an archive is not text, and this file is deliberately not decodable.
   */
  private val ArchiveBytes: Array[Byte] =
    Array[Byte](0x50.toByte, 0x4b.toByte, 0x03.toByte, 0x04.toByte, 0xc3.toByte, 0x28.toByte)

  /**
   * The suffix the original used to name an entry inside an archive, taken verbatim from
   * the locator its archive case produced (`...TestFile.zip!/TestFile.txt`).
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
      case Right(text) =>
        fail(s"expected a failed IO, got ${text.length} characters")
    }
  }

  test("readClasspathText suspends the read so that constructing the IO never throws") {
    IO {
      // Building the description of a read of an absent resource is not itself a read, so
      // it cannot fail; only evaluating it can, which the case above evaluates.
      val _ = Resources.readClasspathText(AbsentResource)
      succeed
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
    IO {
      val _ = Resources.readFileText("/no-such-directory/no-such-file.txt")
      succeed
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
      // Nor either acquisition: a stream of the subject's own making never escapes it, so
      // the only way to obtain content through this object is a complete read.
      assertDoesNotCompile(
        """com.opengamma.strata.collect.io.Resources.openClasspathStream("parity/double-array-baseline.json")"""
      )
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.openFileStream("x")""")
    }
  }

  //-------------------------------------------------------------------------
  // Names the port does not interpret
  //
  // The absence proofs above deny the factories; the two cases below are the behavioural
  // half of the same statement, and they are what the original's URL and archive cases
  // become here. Each of those cases turned a name of a particular shape - a URL, a
  // prefixed locator string, a path reaching inside an archive - into bytes. None of that
  // API is ported, so no case here can perform those reads; what the port offers in their
  // place is the refusal itself. A name is used exactly as supplied, so a name of any of
  // those shapes fails the read, and each case carries a positive control - the very same
  // file and the very same resource, read under their plain names - so the failure is
  // attributable to the shape of the name rather than to absent or unreadable content.
  //-------------------------------------------------------------------------
  test("a URL or a prefixed locator name is not interpreted and the read of it fails") {
    withTempFile(HelloWorld) { path =>
      // The same file that reads below, named with the prefix the original parsed away.
      val prefixedFile = "file:" + path.toString
      // The same resource that reads below, named with the other prefix it parsed away.
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
        // "url:" selected a URL in the original. Here the whole string is a resource name,
        // and the classpath holds no entry called that - which is the absence failure, not
        // a failure of anything that was found.
        refusalOf(urlName, "url:file:/x") shouldBe a[FileNotFoundException]
        // "classpath:" selected the classpath lookup in the original. Here it is part of
        // the resource name, which is why the read fails over a resource that plainly
        // exists under the name the control just read it by.
        refusalOf(prefixedResourceRead, prefixedResource) shouldBe a[FileNotFoundException]
        // "file:" likewise: the prefix belongs to the file name, so the platform reports
        // the file as absent even though the control read it a moment ago.
        refusalOf(prefixedFileRead, prefixedFile) shouldBe a[NoSuchFileException]
      }
    }
  }

  test("an archive is not opened and a path inside one is not resolved") {
    withTempBytes(ArchiveBytes) { path =>
      // The shape of name the original's archive case produced, over a real archive header.
      val entryInsideArchive = path.toString + ArchiveEntrySuffix
      val entryInsideClasspathArchive = "parity/double-array-baseline.zip" + ArchiveEntrySuffix
      for {
        archive <- Resources.readFileText(path.toString).attempt
        insideArchive <- Resources.readFileText(entryInsideArchive).attempt
        insideClasspathArchive <- Resources.readClasspathText(entryInsideClasspathArchive).attempt
      } yield {
        // The archive itself: the original read it as bytes and asserted its header. No
        // byte-source reader is ported, so those bytes are unreachable here - the only
        // reader that takes this path decodes strictly, and an archive is not text. The
        // file is found and read, and it is the decode that ends the read, which the
        // message states.
        refusalOf(archive, path.toString).getMessage should include("UTF-8")
        // A name reaching inside the archive is not resolved: no archive is opened, so the
        // whole string, separator and all, is a file name that nothing bears. The failure
        // is therefore the platform's absence report rather than the decode above - the
        // read never reaches any content, which is what "not resolved" means here and what
        // tells this case apart from the one over the archive itself.
        refusalOf(insideArchive, entryInsideArchive) shouldBe a[NoSuchFileException]
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
// The other twelve of the nineteen have no origin in the original class and are additive,
// because they hold the port to promises the original could not make - its reads happened
// where they were written and it reported failure by throwing. They are:
//
//   * the two laziness cases, one per reader: describing a read performs none of it;
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
//     third path, so release is established on every outcome a caller can reach.
// ---------------------------------------------------------------------------
