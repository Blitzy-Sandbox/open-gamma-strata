/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.CharacterCodingException
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import cats.effect.Resource

import com.opengamma.strata.collect.result.Failure

/**
 * Loads text resources as effects.
 *
 * This object is the single effectful edge of this module: every other member is a pure
 * function of its arguments. It exists so that the JSON fixtures behind the parity
 * harness, and any text the demonstration application needs, can be read without ambient
 * global state. Each method returns a description of a read, and nothing touches the
 * classpath or the file system until that description is run.
 *
 * The surface is those two reads and nothing else. Every other member of this object is
 * private, so the byte ceiling, the time bound, the two stream acquisitions, the shared read
 * they feed and the cancellation protocol around it are neither readable nor replaceable from
 * outside: a caller chooses the source and nothing about how it is read.
 *
 * ===Both reads are bounded===
 *
 * A reader that materialises whatever it is pointed at is a way to exhaust a heap with a
 * choice of argument, so neither method here does. The ceiling is 64 MiB, it applies to
 * both sources, and it is enforced '''while''' reading rather than checked beforehand: one
 * byte more than the ceiling is read, and a source that yields it fails the effect naming
 * the source and the limit. Consulting the size of a file first would prove nothing,
 * because a file can grow between the question and the read, and a classpath entry has no
 * size to consult at all. The ceiling therefore also bounds the decoded text, since a
 * character costs at least one byte.
 *
 * How much a source may yield is only one of the two ways it can cost this process without
 * limit; how long it may take to yield it is the other, and it is bounded as well. The whole
 * of a managed read - the open, the read and the decode - is given two minutes, for the
 * reasons set out at `ReadTimeLimit`, and a read that outlasts that is abandoned with a
 * failure naming the source and the bound. Nothing legitimate this module reads comes close to
 * it; what it bounds is a source that has stopped making progress.
 *
 * ===The decode is strict===
 *
 * Both readers decode UTF-8 '''strictly''': a byte sequence that is not valid UTF-8 fails
 * the effect rather than being replaced by a substitution character. The character set is
 * always stated, so the platform default is never consulted, and both sources decode
 * identically, so a classpath read and a file read of the same bytes agree exactly.
 *
 * Strictness is a deliberate narrowing of the reader being replaced, which substituted a
 * replacement character for malformed input. What this reader ingests is the captured
 * parity baselines that the port's numerical results are measured against, and a
 * substitution inside one of those is a silently altered expectation: the measurement would
 * still run, against a value nobody captured. Failing loudly is the only useful outcome, so
 * that is what happens.
 *
 * ===Failure, and how a failure names its source===
 *
 * A resource that cannot be obtained makes the returned effect fail. A missing classpath
 * resource fails with a [[java.io.FileNotFoundException]] naming it; a source the platform
 * refuses to open, and a path the platform declines to interpret at all, fail with a
 * [[java.io.IOException]] naming the source and what the platform reported; a source beyond
 * the ceiling and text that is not valid UTF-8 each fail with a [[java.io.IOException]] that
 * explains which of the two it was; and a read that does not complete inside the time bound
 * described below fails with a [[java.io.IOException]] naming the source and that bound.
 * Neither reader ever substitutes a sentinel or empty text for content.
 *
 * Every one of those messages names its source in a '''bounded, single-line''' form, produced
 * by [[Failure.renderDiagnostic]] - the one renderer these two modules hold for text on its
 * way to a line-oriented reader. A caller chooses the name a read is given, and that name may
 * itself have reached the caller from outside the process, so a message that interpolated it as
 * it stands would let the choice of name write a line of its own into whatever records the
 * failure, or make that record as large as the name (CWE-117, CWE-209). An ordinary name
 * renders to itself character for character, so an ordinary diagnostic reads exactly as it did.
 *
 * The exception the platform reports is for that reason never the failure a caller sees, since
 * its own message '''is''' the raw path: it is attached as the '''cause''' of the failure above,
 * where a diagnostic that is under this process's control can still reach it, while the text
 * this object composes carries the rendered form and the class name of that exception and
 * nothing else.
 *
 * ===Resource lifetime, and cancellation that does not wait for the source===
 *
 * Each read owns a stream, and that stream is closed on every outcome - success, failure and
 * cancellation alike - because acquisition is paired with release through
 * [[cats.effect.Resource]]. Both readers share the one implementation that makes that pairing,
 * so neither can drift from the other.
 *
 * Pairing alone decides '''that''' a handle comes back, not '''when'''. A read that is
 * cancelled while it waits on a source releases its handle only once the blocked call returns,
 * and a source that never yields never returns it: the read holds a thread of the blocking pool
 * and a descriptor of this process indefinitely, and no cancellation of it can be prompt
 * (CWE-400, CWE-772). Three mechanisms together make it prompt, and each of them answers a
 * different part of that:
 *
 *  1. '''The blocking is interruptible.''' Both acquisitions and the read itself are lifted
 *     with `IO.interruptible` and `IO.interruptibleMany` rather than with ordinary blocking, so
 *     cancelling a read delivers a thread interrupt to the call that is waiting. A stream from
 *     the file system is channel backed, and a channel read that is interrupted closes its
 *     channel and raises instead of continuing to wait for data that is not coming - so an
 *     interrupt ends a stalled read rather than merely marking it to be ended later.
 *  1. '''Cancellation closes the stream first.''' The bounded read runs on a fiber of its own,
 *     and the effect that waits for that fiber closes the stream '''before''' it asks the
 *     reader to stop. The handle is therefore reclaimed at the moment of cancellation instead
 *     of at the moment the source gives way, which matters for a source that ignores an
 *     interrupt entirely. Release still owns the close on every ordinary outcome; the close on
 *     this path tolerates finding a stream that is already closed.
 *  1. '''The read has a time bound.''' A read that neither completes nor is cancelled is
 *     abandoned after two minutes, so a caller that never cancels is bounded as well. The
 *     bound is applied with `timeoutAndForget` rather than `timeout`: `timeout` waits for the
 *     read it has cancelled to finish, which is a wait on precisely the source the bound exists
 *     for. Forgetting the read lets the failure arrive on time, and the read that was forgotten
 *     still gives its handle back, because release runs for a cancelled use and a cancelled
 *     acquisition alike.
 *
 * The scope is deliberately narrow. The byte and character source hierarchy of the
 * original, its locator value type together with the prefixed forms ("classpath:",
 * "file:" and "url:") that were parsed into one, its caller-sensitive classpath search,
 * its byte-order-mark handling and its archive support are all outside this port.
 */
object Resources {

  /**
   * The largest source either reader accepts, in bytes: 64 MiB.
   *
   * The ceiling is a resource bound rather than a statement about the data: it is set well
   * above anything this module is expected to read, so that a legitimate source is never
   * refused, while an arbitrarily large one still cannot be materialised. The largest text
   * read in this repository today is the day-count parity baseline at roughly 12.7 MiB, so
   * the ceiling leaves it a factor of five of headroom. Peak memory during a read is
   * therefore bounded too, at the bytes read plus the text decoded from them.
   *
   * It is an implementation bound and not part of the surface: a caller has nothing to do
   * with it beyond receiving the failure that names it, so it is private to this object and
   * documented here rather than exposed to be read.
   */
  private val MaxBytes: Int = 64 * 1024 * 1024

  /**
   * The longest either reader waits for a source to be opened, read and decoded: two minutes.
   *
   * The ceiling above bounds how much a source may yield; this bounds how long it may take to
   * yield it, which is the other way a source can cost this process without limit. A named
   * pipe, a device, a stream whose other end has stopped writing without closing: each of them
   * leaves a read waiting for data that may never arrive, and without a bound that read holds a
   * thread of the blocking pool and a handle of this process for as long as the source chooses.
   *
   * Two minutes is chosen the same way the byte ceiling is - far above any legitimate read, so
   * that the bound can only ever be reached by a source that has stopped making progress. The
   * largest text read in this repository today is the day-count parity baseline at roughly
   * 12.7 MiB; reading and decoding that costs well under a second from a local file, so even a
   * heavily loaded machine, a cold page cache or a slow network file system has two orders of
   * magnitude of headroom before a read that is progressing normally could be refused.
   *
   * Like the ceiling it is an implementation bound rather than part of the surface: a caller
   * has nothing to choose about it and only ever meets the failure that names it.
   */
  private val ReadTimeLimit: FiniteDuration = 2.minutes

  /**
   * Reads a classpath resource as UTF-8 text.
   *
   * The name is resolved against the class loader of this module, falling back to the
   * context class loader of the calling thread and then to the platform loader, so a
   * loader is always selected. Class loader resource names are absolute and carry no
   * leading separator, so one leading `/` is removed when present: both
   * `parity/example.json` and `/parity/example.json` name the same resource. The name is
   * otherwise used exactly as supplied: the calling class plays no part in resolution.
   *
   * The stream is closed once the read finishes, and equally if the read fails or is
   * cancelled.
   *
   * @param path  the resource name, with or without a leading `/`
   * @return the content of the resource decoded as UTF-8; the effect fails with a
   *         [[java.io.FileNotFoundException]] naming the resource when the classpath
   *         holds no such entry, and with a [[java.io.IOException]] when the resource
   *         exceeds the 64 MiB ceiling described above, is not valid UTF-8, could not be
   *         opened at all or did not arrive inside the time bound; every one of those
   *         messages names the resource in the bounded, single-line form described above
   */
  def readClasspathText(path: String): IO[String] = {
    val name = canonicalResourceName(path)
    // The name is rendered once, here, and that rendering is what every message about this
    // read is given: no message can bypass it, because nothing further in holds the raw name
    // for any purpose other than the lookup itself.
    val rendered = Failure.renderDiagnostic(name)
    readManaged(classpathSource(rendered), openClasspathStream(rendered, name), MaxBytes)
  }

  /**
   * Reads a file as UTF-8 text.
   *
   * The path is interpreted by the platform exactly as supplied, so a relative path is
   * resolved against the working directory of the process. The file is opened once, read
   * under the same ceiling as a classpath resource, and closed on every outcome.
   *
   * @param path  the path of the file to read
   * @return the content of the file decoded as UTF-8; the effect fails with a
   *         [[java.io.IOException]] whose message names the file in the bounded,
   *         single-line form described above and which carries the exception the platform
   *         reported as its '''cause''' - for an absent file that cause is
   *         [[java.nio.file.NoSuchFileException]], and for a path the platform declines to
   *         interpret it is the rejection of the path itself - and with a
   *         [[java.io.IOException]] of this object's own for a source beyond the 64 MiB
   *         ceiling, for text that is not valid UTF-8, and for a read that outlasts the
   *         time bound
   */
  def readFileText(path: String): IO[String] = {
    // Rendered once, as on the classpath side, and passed to the acquisition so that the
    // failure of the open names the source in exactly the form the rest of the read does.
    val rendered = Failure.renderDiagnostic(path)
    readManaged(fileSource(rendered), openFileStream(rendered, path), MaxBytes)
  }

  //-------------------------------------------------------------------------
  // The shared read, the pairing that owns its stream, the cancellation protocol around it,
  // the two acquisitions and the text a failure of one of them carries.
  //
  // Each public reader is these members composed: one read, bounded in size and in time and
  // decoded the same way whichever source it is given, over a stream whose close is paired
  // with its acquisition and brought forward when the read is cancelled, and one acquisition
  // per source, each naming its source through the same two labels. Every one of them is
  // private to this object, as everything here other than the two readers is, so the readers
  // cannot drift from one another and no caller reaches the read with a stream, a limit, a
  // time bound or a source label of its own.
  //-------------------------------------------------------------------------

  /**
   * Reads one source to its end, under a ceiling and under the time bound, and decodes it.
   *
   * This is where the three mechanisms of the cancellation protocol described at the head of
   * this object meet: the acquisition and the read it wraps are interruptible, the read runs
   * on a fiber whose cancellation closes the stream before it waits for the reader, and the
   * whole managed read is given the time bound. The bound is applied '''outside''' the
   * pairing with release, so that expiring it cancels the use and the acquisition together and
   * therefore reclaims the handle.
   *
   * `timeoutAndForget` rather than `timeout`: `timeout` cancels the read and then '''waits'''
   * for that cancellation to finish, which on the one source this bound exists for - a source
   * that never yields - is a wait on exactly the read that is not returning. Forgetting the
   * read instead makes the failure arrive at the bound whatever the source does. The forgotten
   * read is not abandoned: its cancellation continues in the background, and release runs on a
   * cancelled use and a cancelled acquisition alike, so the stream is still closed.
   *
   * @param source  how the source is named in a failure message, already rendered
   * @param open  the acquisition of the stream to read
   * @param maxBytes  the largest number of bytes to accept; a source yielding more fails
   * @return the decoded text
   */
  private def readManaged(source: String, open: IO[InputStream], maxBytes: Int): IO[String] =
    managedStream(open)
      .use(stream => cancelableRead(source, stream, maxBytes))
      .timeoutAndForget(ReadTimeLimit)
      .handleErrorWith {
        case expired: TimeoutException =>
          IO.raiseError(
            new IOException(
              s"$source did not complete within $ReadTimeLimit, so the read was abandoned; " +
                "the bound exists so that a source which never yields cannot hold a thread " +
                "and a handle of this process for as long as it chooses",
              expired))
        case other => IO.raiseError(other)
      }

  /**
   * Pairs the acquisition of a stream with its close.
   *
   * Release runs on every outcome of whatever uses the stream, so a read that fails part
   * way through, and one that is cancelled, both reclaim the handle.
   */
  private def managedStream(open: IO[InputStream]): Resource[IO, InputStream] =
    Resource.fromAutoCloseable(open)

  /**
   * Reads the stream on a fiber of its own, so that cancelling the read closes the stream
   * rather than waiting for it.
   *
   * Interruptible blocking alone leaves one gap: the cancellation of a read is complete only
   * when the blocked call has returned, and the close that release performs can only happen
   * after that. A source that ignores an interrupt would therefore hold its handle for as long
   * as it held the thread. Putting the read on its own fiber closes that gap - the effect that
   * waits for the fiber is cancelled promptly, and its finalizer closes the stream '''first'''
   * and only then cancels the reader, so the handle comes back at the moment of cancellation
   * instead of at the moment the read gives up.
   *
   * The close in that finalizer tolerates its own failure, because it races the ordinary close
   * of release: whichever runs second finds a stream that is already closed, and a failure
   * raised from a finalizer would replace the cancellation it was reporting. Release keeps
   * owning the close on every other outcome, so the stream is closed exactly once whenever the
   * read completes, fails or is refused.
   *
   * @param source  how the source is named in a failure message, already rendered
   * @param stream  the stream to read, owned by the pairing above
   * @param maxBytes  the largest number of bytes to accept
   * @return the decoded text
   */
  private def cancelableRead(source: String, stream: InputStream, maxBytes: Int): IO[String] =
    readBoundedText(source, stream, maxBytes).start.flatMap(reader =>
      reader.joinWithNever.onCancel(closeTolerantly(stream) *> reader.cancel)
    )

  /**
   * Closes a stream, treating a failure to close as nothing to report.
   *
   * Used only from the cancellation path above, where the close is an attempt to reclaim a
   * handle early and a second close of the same stream is the expected case rather than a
   * fault.
   */
  private def closeTolerantly(stream: InputStream): IO[Unit] =
    IO.blocking(stream.close()).handleError(_ => ())

  /**
   * Acquires a classpath resource as a stream.
   *
   * Acquisition is the lookup itself, so an entry the classpath does not hold is reported
   * as a failed effect rather than as a stream that yields no bytes. The absence is named
   * with the rendering of the resource name that the reader produced, never with the name as
   * it was supplied, and a lookup the classpath machinery itself refuses is wrapped by
   * `unopenable` for the same reason.
   *
   * @param rendered  the resource name in its bounded, single-line rendering, for the message
   * @param name  the same name as a class loader expects it, for the lookup itself
   */
  private def openClasspathStream(rendered: String, name: String): IO[InputStream] =
    IO.interruptible(Option(classLoader.getResourceAsStream(name)))
      .handleErrorWith(cause => IO.raiseError(unopenable(classpathSource(rendered), cause)))
      .flatMap(opened =>
        IO.fromOption(opened)(new FileNotFoundException(s"Classpath resource absent: $rendered"))
      )

  /**
   * Acquires a file as a stream, naming the source rather than repeating what the platform
   * said about it.
   *
   * Interpreting the path is part of the acquisition, because a path can be rejected without
   * the file system being touched at all, and that rejection is as caller-controlled as any
   * other: it is wrapped exactly like a refused open.
   *
   * @param rendered  the path in its bounded, single-line rendering, for the message
   * @param path  the path to interpret and open, exactly as it was supplied
   */
  private def openFileStream(rendered: String, path: String): IO[InputStream] =
    IO.interruptible(Files.newInputStream(Paths.get(path)))
      .handleErrorWith(cause => IO.raiseError(unopenable(fileSource(rendered), cause)))

  /**
   * How a classpath resource is named in a message, given the rendering of its name.
   *
   * The wording of each label lives in one place, so the label a failure of the acquisition
   * carries is the same label the read, the ceiling and the decode carry, and a caller reading
   * two failures of one read sees one name for the source rather than two spellings of it.
   */
  private def classpathSource(rendered: String): String = s"classpath resource '$rendered'"

  /** How a file is named in a message, given the rendering of its path. */
  private def fileSource(rendered: String): String = s"file '$rendered'"

  /**
   * The failure of an acquisition, naming the source and what the platform reported.
   *
   * The message a platform exception carries is the path it was given, so handing that
   * exception to a caller would publish the raw path however carefully this object had
   * rendered its own text. What is published instead is the rendered source label, the class
   * name of the platform exception - which this process chose, not its caller - and the
   * rendering of its message, with the exception itself attached as the cause so that a
   * diagnostic under this process's control loses nothing.
   *
   * @param source  the rendered source label
   * @param cause  the exception the platform raised
   * @return the failure to raise in its place
   */
  private def unopenable(source: String, cause: Throwable): IOException =
    new IOException(
      s"$source could not be opened: the platform reported ${cause.getClass.getName}: " +
        Failure.renderDiagnostic(Option(cause.getMessage).getOrElse("")),
      cause)

  //-------------------------------------------------------------------------
  /**
   * Reads at most one byte beyond the ceiling, off the compute pool, and refuses a source
   * that supplies that byte.
   *
   * Reading `maxBytes + 1` is what makes the ceiling an enforced bound rather than an
   * assumption: the read stops there, so a source of any size costs the ceiling and no
   * more, and the extra byte is the evidence that there was more to come.
   *
   * The read is '''interruptible''', and that is load bearing rather than decorative: a
   * stream obtained from the file system is backed by a channel, and a channel read that is
   * interrupted closes the channel and raises [[java.nio.channels.ClosedByInterruptException]]
   * instead of continuing to wait. Cancelling a read therefore ends the blocked call rather
   * than queueing behind it, which ordinary blocking cannot do: ordinary blocking defers the
   * cancellation until the call it is running has returned, and a call waiting on a source
   * that never yields never returns. `interruptibleMany` rather than `interruptible` because
   * the interrupt is repeated until the call actually gives way, so a read that swallows one
   * interrupt still ends.
   */
  private def readBoundedText(source: String, stream: InputStream, maxBytes: Int): IO[String] = {
    // Saturating rather than wrapping, so that a ceiling at the top of the range stays a
    // ceiling instead of becoming a request for a negative number of bytes.
    val probe = if (maxBytes < Int.MaxValue) maxBytes + 1 else Int.MaxValue
    IO.interruptibleMany(stream.readNBytes(probe)).flatMap { bytes =>
      if (bytes.length > maxBytes) {
        IO.raiseError(
          new IOException(
            s"$source exceeds the $maxBytes byte limit this reader accepts, so it was not " +
              "read; the limit exists so that the size of a source cannot determine the " +
              "memory this process uses"))
      } else {
        decodeUtf8(source, bytes)
      }
    }
  }

  /**
   * Decodes bytes as UTF-8, shared by both readers so that they agree exactly.
   *
   * The decoder reports malformed and unmappable input instead of substituting for it, and
   * is built per call because a decoder carries the state of the decode it is performing.
   */
  private def decodeUtf8(source: String, bytes: Array[Byte]): IO[String] =
    IO.delay(strictUtf8Decoder.decode(ByteBuffer.wrap(bytes)).toString)
      .handleErrorWith {
        case coding: CharacterCodingException =>
          IO.raiseError(
            new IOException(
              s"$source is not valid UTF-8 text, so it was not decoded; this reader " +
                "refuses malformed input rather than substituting for it, because a " +
                "substitution inside captured data is an expectation nobody captured",
              coding))
        case other => IO.raiseError(other)
      }

  /** A decoder that refuses malformed and unmappable input rather than substituting. */
  private def strictUtf8Decoder: CharsetDecoder =
    StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)

  /**
   * Converts a resource name to the form a class loader expects, by removing a single
   * leading separator when one is present.
   */
  private def canonicalResourceName(path: String): String =
    if (path.startsWith("/")) path.substring(1) else path

  /**
   * Selects a class loader, preferring the one that loaded this module, then the context
   * loader of the calling thread, and finally the platform loader, which is always
   * available. The result is therefore total, so selecting a loader is never itself a way for
   * a read to fail: a lookup ends in content, in the absence above, or in the wrapped failure
   * of an acquisition, and in nothing else.
   */
  private def classLoader: ClassLoader =
    Option(getClass.getClassLoader)
      .orElse(Option(Thread.currentThread().getContextClassLoader))
      .getOrElse(ClassLoader.getPlatformClassLoader)
}
