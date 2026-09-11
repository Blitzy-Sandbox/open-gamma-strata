/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.ByteArrayInputStream
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicInteger

import scala.util.Try

import cats.effect.IO
import cats.effect.Outcome
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
 * type itself; none of that API is ported, so inventing cases for it would test nothing.
 * The mapping at the foot of this file records where every one of the fifteen landed.
 *
 * Two cases here are additive rather than ported. The original had no negative case for a
 * missing classpath resource or an unreadable file - its only negative case concerned
 * parsing a prefixed locator string - whereas the port promises that a resource it cannot
 * obtain makes the effect fail rather than yielding a sentinel or empty text. That promise
 * is part of the contract, so it is tested, and every failure is observed through `attempt`
 * as a value.
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
    // The reading surface is these two methods. The one other public member is the
    // documented byte ceiling `MaxBytes`, a constant rather than an operation, which the
    // ceiling cases below assert the value of.
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
      // No caller-sensitive lookup: the calling class plays no part in resolution.
      assertDoesNotCompile("""com.opengamma.strata.collect.io.Resources.readClasspathText(classOf[String], "x")""")
    }
  }

  //-------------------------------------------------------------------------
  // The byte ceiling
  //
  // Both readers hand the shared read a limit, and that limit is what stops the size of a
  // source from deciding the memory of this process. The cases below drive the production
  // read - the same method the two public readers call, with the production acquisition of
  // a real file and a real classpath resource - and vary only the limit, because a case
  // that varied the source instead would have to build one of sixty-four megabytes to
  // observe the same decision. The value of the limit the public readers actually pass is
  // asserted separately, immediately below.
  //-------------------------------------------------------------------------
  test("the bounded read refuses a source that is one byte beyond its limit") {
    withTempFile("abcde") { path =>
      Resources
        .readManaged(s"file '$path'", Resources.openFileStream(path.toString), 4)
        .attempt
        .map {
          case Left(failure) =>
            failure shouldBe an[IOException]
            // The message has to name both the source and the limit, or a run that hits the
            // ceiling cannot be told apart from a run that hit a corrupt file.
            failure.getMessage should include(path.toString)
            failure.getMessage should include("4 byte limit")
          case Right(text) =>
            fail(s"expected the ceiling to refuse the source, got ${text.length} characters")
        }
    }
  }

  test("the bounded read accepts a source of exactly its limit") {
    // The boundary itself is inclusive: a source of precisely the limit is read whole, so
    // the ceiling rejects only what is genuinely beyond it.
    withTempFile("abcd") { path =>
      Resources
        .readManaged(s"file '$path'", Resources.openFileStream(path.toString), 4)
        .map(text => text shouldBe "abcd")
    }
  }

  test("the bounded read applies the same ceiling to a classpath resource") {
    // The classpath acquisition goes through the same read, so the ceiling cannot hold for
    // one source and not the other.
    Resources
      .readManaged(s"classpath resource '$FixturePath'", Resources.openClasspathStream(FixturePath), 16)
      .attempt
      .map {
        case Left(failure) =>
          failure shouldBe an[IOException]
          failure.getMessage should include(FixturePath)
          failure.getMessage should include("16 byte limit")
        case Right(text) =>
          fail(s"expected the ceiling to refuse the resource, got ${text.length} characters")
      }
  }

  test("MaxBytes is the documented ceiling and is far above the data this module reads") {
    Resources.readClasspathText(FixturePath).map { text =>
      Resources.MaxBytes shouldBe 64 * 1024 * 1024
      // The largest text read anywhere in this repository is the day-count parity baseline
      // of the dependent module, at roughly 12.7 MiB. The ceiling must stay clear of it, or
      // enforcing it would start refusing legitimate data.
      Resources.MaxBytes should be > 16 * 1024 * 1024
      // ... and this module's own fixture is nowhere near it, so the ceiling never affects
      // an ordinary read.
      text.length should be < Resources.MaxBytes / 4
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

  test("the bounded read refuses malformed UTF-8 from a classpath resource too") {
    // The same decode serves both sources, driven here through the production read with a
    // stream of this spec's own, because every committed classpath resource is valid UTF-8.
    val stream = new CountingStream(MalformedUtf8)
    Resources.readManaged("classpath resource 'malformed'", IO.pure(stream), Resources.MaxBytes).attempt.map {
      case Left(failure) =>
        failure shouldBe an[IOException]
        failure.getMessage should include("UTF-8")
        stream.closeCount shouldBe 1
      case Right(text) =>
        fail(s"expected malformed input to be refused, got '$text'")
    }
  }

  //-------------------------------------------------------------------------
  // Resource lifetime
  //
  // The one property of these readers that no caller can observe through their public
  // surface: a classpath read chooses its own class loader, so nothing outside this object
  // can hand it a stream whose closes it counts. Without the cases below, deleting the
  // release finalizer would leave every other case in this file green while every read
  // leaked its handle. They therefore drive the production read and the production managed
  // stream with a counting stream, and assert that release ran exactly once - not merely at
  // least once - on each outcome a read can have.
  //-------------------------------------------------------------------------
  test("the managed read closes the stream exactly once when the read succeeds") {
    val stream = new CountingStream(HelloWorld.getBytes(StandardCharsets.UTF_8))
    Resources.readManaged("counting stream", IO.pure(stream), Resources.MaxBytes).map { text =>
      text shouldBe HelloWorld
      stream.closeCount shouldBe 1
    }
  }

  test("the managed read closes the stream exactly once when the read fails") {
    val stream = new CountingStream(HelloWorld.getBytes(StandardCharsets.UTF_8), failOnRead = true)
    Resources.readManaged("counting stream", IO.pure(stream), Resources.MaxBytes).attempt.map { outcome =>
      outcome.isLeft shouldBe true
      stream.closeCount shouldBe 1
    }
  }

  test("the managed read closes the stream exactly once when the ceiling refuses the source") {
    // The refusal happens after the bytes have been read, so it is a failure of the body of
    // the managed read rather than of its acquisition, and the handle still has to come back.
    val stream = new CountingStream("abcde".getBytes(StandardCharsets.UTF_8))
    Resources.readManaged("counting stream", IO.pure(stream), 4).attempt.map { outcome =>
      outcome.isLeft shouldBe true
      stream.closeCount shouldBe 1
    }
  }

  test("the managed stream closes the stream exactly once when the use is cancelled") {
    // Cancellation is the outcome a bare `try`/`finally` around a read would not cover, and
    // it is observed deterministically here: the body cancels itself, so no timing is
    // involved, and the fiber is joined to prove the cancellation was the outcome.
    val stream = new CountingStream(HelloWorld.getBytes(StandardCharsets.UTF_8))
    Resources
      .managedStream(IO.pure(stream))
      .use(_ => IO.canceled)
      .start
      .flatMap(_.join)
      .map {
        case Outcome.Canceled() => stream.closeCount shouldBe 1
        case other => fail(s"expected the use to be cancelled, got $other")
      }
  }

  test("the managed read releases nothing when the stream cannot be acquired") {
    // An acquisition that fails has nothing to release, so the failure must surface as it is
    // rather than being replaced by one from a finalizer with no handle to close.
    val expected = new IOException("acquisition refused")
    Resources.readManaged("counting stream", IO.raiseError[InputStream](expected), Resources.MaxBytes).attempt.map {
      case Left(failure) => failure shouldBe expected
      case Right(text) => fail(s"expected the acquisition to fail, got ${text.length} characters")
    }
  }

  test("both public readers leave no open descriptor behind") {
    // The end-to-end counterpart of the cases above, over the public API: after a read of a
    // classpath resource and a read of a file, no descriptor of this process still refers to
    // either of them. This is the handle-reclamation property itself rather than a proxy for
    // it, and it holds only because both readers close what they open.
    IO.blocking(Files.isDirectory(ProcessDescriptors)).flatMap { exposed =>
      if (!exposed) {
        IO(
          cancel(
            "this platform does not expose the descriptors of the running process, so " +
              "reclamation cannot be observed directly here; the counting-stream cases above " +
              "cover release on every outcome"))
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

  /** The targets of the descriptors this process currently has open. */
  private def descriptorTargets: IO[Vector[String]] =
    IO.blocking {
      Option(new File(ProcessDescriptors.toString).listFiles())
        .map(_.toVector)
        .getOrElse(Vector.empty[File])
        .flatMap(entry => Try(Files.readSymbolicLink(entry.toPath).toString).toOption)
    }
}

/**
 * A stream that counts how often it was closed, and can fail a read on demand.
 *
 * This is the seam the closure cases need. A classpath read selects its own class loader, so
 * no caller can give one a stream to observe; handing this stream to the shared read instead
 * exercises the production acquisition-and-release pairing over a stream whose closes are
 * countable. `AtomicInteger` rather than a plain counter because release may run on a
 * different thread from the read.
 *
 * @param bytes  the content the stream yields
 * @param failOnRead  whether a read fails instead of yielding content
 */
private final class CountingStream(bytes: Array[Byte], failOnRead: Boolean = false) extends InputStream {

  private val delegate: ByteArrayInputStream = new ByteArrayInputStream(bytes)

  private val closes: AtomicInteger = new AtomicInteger(0)

  /** How often this stream has been closed. */
  def closeCount: Int = closes.get()

  override def read(): Int =
    if (failOnRead) throw new IOException("the stream refused to be read") else delegate.read()

  override def read(buffer: Array[Byte], offset: Int, length: Int): Int =
    if (failOnRead) {
      throw new IOException("the stream refused to be read")
    } else {
      delegate.read(buffer, offset, length)
    }

  override def close(): Unit = {
    val _ = closes.incrementAndGet()
    delegate.close()
  }
}

// ---------------------------------------------------------------------------
// Mapping from the original test class
// ---------------------------------------------------------------------------
//
// Every one of the fifteen cases of the original is accounted for below. Six had
// counterparts; the other nine tested API the port does not have, and each of those is
// consolidated onto the case here that covers the part of its behaviour which survives.
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
//   -> "readFileText reads a temporary UTF-8 file created with java.nio.file.Files"
//      consolidated. Reading the archive itself was a plain file read, asserted over its
//      header bytes; what is not ported is treating it as an archive.
// test_ofPath_fileInZipFile
//   -> "readFileText fails the IO for a path that cannot be read as a file"
//      consolidated. Naming an entry inside an archive is unsupported, and the general
//      property that replaces it is that a path which is not a readable file fails the
//      effect rather than yielding text.
//
// ---- URLs -----------------------------------------------------------------
//
// test_ofUrl
//   -> "readClasspathText reads the committed parity fixture from the test classpath"
//      consolidated. URL support is not ported - the absence proofs deny it - and the read
//      this case performed is available by resource name.
// test_ofClasspathUrl
//   -> "readClasspathText reads the committed parity fixture from the test classpath"
//      consolidated, for the same reason: a classpath URL is reached by name here.
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
// Three cases here have no origin in the original class and are additive: the two failure
// cases above, which hold the port to its promise that a resource it cannot obtain makes
// the effect fail, and the laziness cases, which hold it to the promise that describing a
// read performs none of it. The original could promise neither, because its reads happened
// where they were written and reported failure by throwing.
// ---------------------------------------------------------------------------
