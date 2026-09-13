/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.net.JarURLConnection
import java.net.URL
import java.nio.ByteBuffer
import java.nio.charset.CharacterCodingException
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
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
 * reasons set out at `ReadTimeLimit`, and a caller whose read outlasts that is freed with a
 * failure naming the source and the bound. Nothing legitimate this module reads comes close to
 * it; what it bounds is a source that has stopped making progress.
 *
 * A bound on how long a caller waits is not by itself a bound on what a stalled read costs
 * this process, and the sources whose waiting cannot be ended at all are answered before they
 * are opened rather than by that bound: '''neither reader opens a source of the file system
 * that is not a regular file'''. A directory, a named pipe, a socket and a device are refused
 * with a failure naming the source - by the file reader for the path it is given, and by the
 * classpath reader for a name it resolved onto the file system, which is what a classpath
 * resolves a name onto; a location under a protocol this object cannot interpret is opened as
 * it always was, as `locationRefusal` records. The section on cancellation below sets out why
 * the open in particular has to be answered that way and what is left over once it is.
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
 * A resource that cannot be obtained makes the returned effect fail. A classpath resource that
 * is absent fails with a [[java.io.FileNotFoundException]] naming it, and so does a classpath
 * name that resolves to a '''directory''' rather than to a readable file, because this object
 * reads files and a directory is not one: what a directory yields when it is read is the shape
 * of the classpath rather than content, so it is refused as a name instead of handed back as
 * text. A source the platform refuses to open, and a path the platform declines to interpret at
 * all, fail with a [[java.io.IOException]] naming the source and what the platform reported; a
 * path the platform describes as something other than a regular file fails with a
 * [[java.io.IOException]] of this object's own, naming the source, saying what the platform
 * described it as and why only a regular file is opened; a source beyond the ceiling and text
 * that is not valid UTF-8 each fail with a [[java.io.IOException]] that explains which of the
 * two it was; and a read that does not complete inside the time bound described below fails
 * with a [[java.io.IOException]] naming the source and that bound. Neither reader ever
 * substitutes a sentinel or empty text for content.
 *
 * Every one of those messages names its source in a '''bounded, single-line''' form, produced
 * by `Failure.renderDiagnostic` - the one renderer these two modules hold for text on its
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
 * (CWE-400, CWE-772). Four mechanisms together answer that, and each of them answers a
 * different part of it:
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
 *     abandoned after two minutes, so a '''caller''' that never cancels is not made to wait on
 *     a source that has stopped making progress. The bound is applied with `timeoutAndForget`
 *     rather than `timeout`: `timeout` waits for the read it has cancelled to finish, which is
 *     a wait on precisely the source the bound exists for. Forgetting the read lets the failure
 *     arrive on time, and the read that was forgotten still gives its handle back, because
 *     release runs for a cancelled use and a cancelled acquisition alike. What the bound does
 *     '''not''' do is reclaim a thread: an abandoned read that is inside a call the platform
 *     will not interrupt stays there, which is what the mechanism below is for.
 *  1. '''Neither reader opens a source of a kind whose open waits.''' The three mechanisms
 *     above all act on a call that is already running, and the one call none of them reaches is
 *     the '''open''': it happens in the acquisition, which cats-effect runs uncancelable, and
 *     for a source that is not a regular file the platform defines the open itself to wait - a
 *     named pipe waits for a writer, a socket for a peer, some devices for data - in a native
 *     call that a thread interrupt does not abort. So each reader asks what the source is
 *     before it opens anything, with the one call that answers immediately for every kind of
 *     source, and refuses what is not a regular file: the file reader describes the path
 *     (`regularFileStream`) and the classpath reader asks the same of a location it resolved on
 *     the file system (`locationRefusal`). The kinds of source whose open is designed to wait
 *     are therefore refused before the uninterruptible acquisition begins, rather than met
 *     inside it.
 *
 * What remains after all four is worth stating plainly rather than leaving to be discovered. A
 * '''regular''' file on a file system that stops answering, and a path replaced between the
 * moment it was described and the moment it was opened, can each still hold one thread of the
 * blocking pool for as long as the platform takes to return from that open. In both of those
 * the caller is still freed at the time bound, what is held is the single thread that read's
 * own open is waiting in, and the descriptor is still never leaked: there is none until the
 * open returns, and when it returns the pairing with release closes it even though the use it
 * was acquired for was abandoned long before. A caller's own bound cannot be shorter than such
 * an open either, because acquisition is uncancelable by design, and that is the exchange which
 * guarantees the handle comes back at all: a read whose acquisition could be cancelled part way
 * through could be cancelled between obtaining a descriptor and pairing it with its close.
 *
 * ===What a read costs===
 *
 * A read of this object carries a fixed cost before it moves a single byte, and it is worth
 * stating plainly because it is large next to a small source. That floor is two hops onto the
 * blocking pool - the interruptible acquisition and the interruptible read - one fiber started
 * and joined, and one timer registered for the bound; measured on a loaded twelve-core machine
 * in a forked process, twenty-five samples each, it comes to roughly half a millisecond at
 * best and three quarters of one typically. A one-kibibyte file costs 0.48 ms at best and
 * 0.70 ms typically; a ninety-kibibyte classpath resource 1.10 ms and 1.23 ms; a bare round
 * trip through this effect system, which reads nothing at all, 0.02 ms and 0.04 ms. So the
 * floor is between ten and thirty times the cost of scheduling alone and it does not move with
 * the size of the source: a one-kibibyte read and a ninety-kibibyte read differ by far less
 * than either differs from doing nothing, because what separates them is scheduling rather
 * than input and output.
 *
 * Above the floor the cost is monotone in the payload and quickly dominated by it: a twelve
 * mebibyte file costs 40.7 ms at best and 47.9 ms typically, where the floor is under two
 * percent of the read. The largest text this repository holds is the day-count parity baseline
 * at roughly 12.7 MiB, so for the reads this object exists to serve the floor is about one
 * percent of the work.
 *
 * That floor '''is''' the mechanisms set out just above, and it is paid for nothing else.
 * Interruptible blocking is what lets a cancellation end a call that is waiting; the fiber is
 * what lets a cancellation reclaim the handle without waiting for that call; the timer is what
 * bounds how long a caller waits on a source that has stopped making progress; and describing
 * the source before opening it is what keeps an open that cannot be interrupted out of the
 * acquisition. The last of those is the only one that costs no scheduling: the description and
 * the open are one call after another inside the hop the acquisition already pays, so what it
 * adds to a read is one system call rather than a hop, and the figures above - measured before
 * it - do not resolve it. A read without any of them would be a shade quicker and could not be
 * cancelled promptly, bounded at all, nor kept away from a source whose open never returns, and
 * this object reads fixtures and demonstration text - work measured in tens of reads, not in
 * millions - so the exchange is settled here once for both readers rather than offered as a
 * choice. A caller reads whole text from a named source and pays a bounded, cancelable read for
 * it; there is no second, cheaper reader to choose, and the two-member surface above is
 * deliberate.
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
   * yield it, which is the other way a source can cost this process without limit. A stream
   * whose other end has stopped writing without closing, a file system that has stopped
   * answering: each of them leaves a read waiting for data that may never arrive, and without a
   * bound the '''caller''' of that read waits with it for as long as the source chooses.
   *
   * What the bound delivers is exactly that: the caller is not held. It is applied with
   * `timeoutAndForget`, so the failure arrives at the bound whatever the source is doing, and
   * the handle of the abandoned read still comes back, because release runs for a cancelled use
   * and a cancelled acquisition alike. It does not recover the '''thread''' of a read stalled
   * in a call the platform will not interrupt, and it is not the mechanism that answers the
   * sources whose blocking cannot be ended - those are refused before they are opened, by the
   * fourth mechanism described at the head of this object, which also records what is left
   * over. Two minutes therefore bounds how long a caller waits on a source that stopped making
   * progress part way through a read this object had begun.
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
   * What is read is a '''file''' of the classpath. A name that resolves to something other
   * than a readable file - a directory of the classpath, which resolves as readily as an entry
   * does - fails the effect rather than being read, because what a directory yields when it is
   * read is the shape of the classpath and not content anybody stored there. So a mistyped
   * fixture name is refused as a name, in the same place and in the same form as a name nothing
   * bears at all, instead of failing later as unparseable text.
   *
   * The stream is closed once the read finishes, and equally if the read fails or is
   * cancelled.
   *
   * @param path  the resource name, with or without a leading `/`
   * @return the content of the resource decoded as UTF-8; the effect fails with a
   *         [[java.io.FileNotFoundException]] naming the resource when the classpath
   *         holds no such entry, and equally when the name resolves to a directory rather
   *         than to a readable file, and with a [[java.io.IOException]] when the resource
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
   * What is read is a '''regular file'''. The path is described before it is opened, and a
   * path the platform describes as anything else - a directory, a named pipe, a socket, a
   * device - fails the effect instead of being opened, because the open of such a source is
   * defined to wait and that wait cannot be interrupted; `regularFileStream` sets out the
   * reasoning and what the description cannot promise. Nothing a caller can name that the
   * platform calls a regular file is narrowed by that, symbolic links being followed.
   *
   * @param path  the path of the file to read
   * @return the content of the file decoded as UTF-8; the effect fails with a
   *         [[java.io.IOException]] whose message names the file in the bounded,
   *         single-line form described above and which carries the exception the platform
   *         reported as its '''cause''' - for an absent file that cause is
   *         [[java.nio.file.NoSuchFileException]], and for a path the platform declines to
   *         interpret it is the rejection of the path itself - and with a
   *         [[java.io.IOException]] of this object's own for a path that is not a regular
   *         file, for a source beyond the 64 MiB ceiling, for text that is not valid UTF-8,
   *         and for a read that outlasts the time bound
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
   * This is where three of the four mechanisms of the cancellation protocol described at the
   * head of this object meet: the acquisition and the read it wraps are interruptible, the read
   * runs on a fiber whose cancellation closes the stream before it waits for the reader, and
   * the whole managed read is given the time bound. The bound is applied '''outside''' the
   * pairing with release, so that expiring it cancels the use and the acquisition together and
   * therefore reclaims the handle. The fourth mechanism sits in the acquisitions this is handed
   * rather than here, because what it answers is the open and not the read.
   *
   * `timeoutAndForget` rather than `timeout`: `timeout` cancels the read and then '''waits'''
   * for that cancellation to finish, which on the source this bound exists for - one that has
   * stopped making progress - is a wait on exactly the read that is not returning. Forgetting
   * the read instead makes the failure arrive at the bound whatever the source does, which is
   * what makes the bound a bound '''for the caller''': the caller is freed at it, and the
   * forgotten read is not thereby recovered. That read is not dropped either: its cancellation
   * continues in the background, and release runs on a cancelled use and a cancelled
   * acquisition alike, so the stream is still closed.
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
                "the bound exists so that a source which has stopped making progress cannot " +
                "make its caller wait for as long as it chooses, and the handle of the " +
                "abandoned read is given back",
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
   * The read is carried on that fiber as an '''`Either`''' - the failure is caught before the
   * fiber can end in it, and raised again here, in the effect the caller is waiting on. A fiber
   * that ends errored is not only joined: the runtime hands its outcome to the failure reporter
   * as well, which prints it, and a reader whose whole contract is to return failures '''as
   * values''' would then both return a failure and print one. Catching it inside the fiber
   * leaves that fiber ending successfully with a failure it is carrying, so nothing is reported
   * anywhere but to the caller. The value is not changed by the detour: the same exception
   * instance is raised, in the same place in the composition, so a caller observing the read
   * through `attempt` sees exactly what it saw before. Nor is the cancellation protocol
   * changed - catching a failure does not catch a cancellation, so a cancelled read still
   * reaches the finalizer above, which closes the stream and only then cancels the reader.
   *
   * @param source  how the source is named in a failure message, already rendered
   * @param stream  the stream to read, owned by the pairing above
   * @param maxBytes  the largest number of bytes to accept
   * @return the decoded text
   */
  private def cancelableRead(source: String, stream: InputStream, maxBytes: Int): IO[String] =
    readBoundedText(source, stream, maxBytes).attempt.start.flatMap(reader =>
      reader.joinWithNever.onCancel(closeTolerantly(stream) *> reader.cancel).rethrow
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
   * Acquisition is the lookup itself, so a name the classpath does not hold, and a name it
   * holds without a readable file behind it, are both reported as a failed effect rather than
   * as a stream that yields bytes a caller would read as content. Either refusal is named with
   * the rendering of the resource name that the reader produced, never with the name as it was
   * supplied, and a lookup the classpath machinery itself refuses is wrapped by `unopenable`
   * for the same reason.
   *
   * The order of the two steps is what keeps the wrapping right: the lookup and the open are
   * one interruptible region, so anything the platform raises inside it - a location it
   * declines to interpret, an open it refuses - becomes the wrapped `unopenable` failure,
   * while the refusals this object decides for itself are raised afterwards from the `Either`
   * that region yields and so carry their own wording unwrapped.
   *
   * @param rendered  the resource name in its bounded, single-line rendering, for the message
   * @param name  the same name as a class loader expects it, for the lookup itself
   */
  private def openClasspathStream(rendered: String, name: String): IO[InputStream] =
    IO.interruptible(classpathEntry(rendered, name))
      .handleErrorWith(cause => IO.raiseError(unopenable(classpathSource(rendered), cause)))
      .flatMap {
        case Right(stream) => IO.pure(stream)
        case Left(refusal) => IO.raiseError(new FileNotFoundException(refusal))
      }

  /**
   * Locates a classpath resource and opens it, or says why it cannot be read as a file.
   *
   * A class loader resolves a name to a location, and a location is not necessarily a file: a
   * name that denotes a '''directory''' of the classpath resolves as readily as one that
   * denotes an entry, and reading it yields whatever the platform makes of a directory - the
   * names it holds, one per line, under exploded class directories, and empty text from an
   * archive. Both of those are content a caller never stored, so a reader that handed them
   * back would turn a mistyped name into a parse failure of the text it was given instead of a
   * refusal of the name, and would publish the shape of the classpath along the way (CWE-209).
   * This object's contract is the classpath subset that names files, so a name outside it is
   * refused here, where the absent name is refused, rather than read - and a location of a kind
   * whose open would '''wait''' is refused here too, for the reason `regularFileStream` gives,
   * so neither reader of this object issues an open against a source the platform has just
   * described as something other than a regular file.
   *
   * The location is resolved once and the stream is opened from it, which is what the class
   * loader's own combined lookup-and-open does internally: an ordinary read therefore costs
   * exactly what it did, and the decision below is a question asked of a location already in
   * hand rather than a second traversal of the classpath.
   *
   * This is deliberately one thunk rather than two effects. The lookup, the decision and the
   * open belong to the single interruptible region of the acquisition, so a classpath read
   * pays one hop to the blocking pool whatever the outcome; splitting them would add a hop to
   * every read to sharpen a failure, which is the wrong trade at the cost described at the head
   * of this object.
   *
   * @param rendered  the resource name in its bounded, single-line rendering, for the message
   * @param name  the same name as a class loader expects it, for the lookup itself
   * @return the opened stream, or the message of the refusal to raise in its place
   */
  private def classpathEntry(rendered: String, name: String): Either[String, InputStream] =
    Option(classLoader.getResource(name)) match {
      case None => Left(s"Classpath resource absent: $rendered")
      case Some(located) =>
        locationRefusal(rendered, located) match {
          case Some(refusal) => Left(refusal)
          case None => Right(located.openStream())
        }
    }

  /**
   * Decides whether a resolved classpath location can be read as a file, by asking the one
   * question each protocol can actually answer.
   *
   * A location carries the protocol that produced it, and only the protocol knows what it has
   * resolved to:
   *
   *  1. A location on the file system is a path, so the file system is asked directly - and it
   *     is asked the same question the file reader asks, because a classpath that resolves a
   *     name onto a directory can resolve one onto a source of some other kind too, and the
   *     open of such a source waits for the reasons set out at `regularFileStream`. A
   *     directory is refused as the directory it is, since that is what a mistyped fixture
   *     name resolves to; a location that is neither a directory nor a regular file is
   *     refused as not being one.
   *  1. A location inside an archive is an entry, and an archive marks a directory entry as
   *     such. The entry is read through [[java.net.JarURLConnection]], the connection type this
   *     protocol produces, whose own lookup resolves a name without a trailing separator onto
   *     the directory entry that carries one - which is precisely the name a caller would have
   *     mistyped. An entry of an archive is a span of bytes of that archive rather than a
   *     source of the platform's own, so an open of it waits for nothing and the kind question
   *     does not arise.
   *  1. Any other protocol is one this object cannot interpret, and a guess about it would
   *     refuse a location that reads perfectly well. Such a location is therefore accepted and
   *     opened, and if the open or the read then fails it fails as it would have before.
   *
   * The two predicates asked of a file location are the non-raising ones rather than the
   * description `regularFileStream` takes, which is deliberate: each of them answers false for
   * a location it cannot describe instead of raising, so a location this object has no reading
   * of is still opened exactly as it was before, and the leniency of this side is unchanged.
   * The file reader cannot use them, because it has to tell an absent path apart from a path
   * of the wrong kind and only the description does that.
   *
   * The connection is only asked for its entry and never for its own stream, so nothing is
   * opened that the caller would have to close: the stream a read owns is opened once, by the
   * acquisition above, and closed by the pairing that owns it.
   *
   * @param rendered  the resource name in its bounded, single-line rendering, for the message
   * @param located  the location a class loader resolved the resource name to
   * @return the message of the refusal to raise in its place, or nothing where the location
   *         can be read as a file
   */
  private def locationRefusal(rendered: String, located: URL): Option[String] =
    located.getProtocol match {
      case "file" =>
        val location = Paths.get(located.toURI)
        if (Files.isDirectory(location)) {
          Some(s"Classpath resource is a directory rather than a file: $rendered")
        } else if (Files.isRegularFile(location)) {
          None
        } else {
          Some(s"Classpath resource is not a regular file: $rendered")
        }
      case "jar" =>
        located.openConnection() match {
          case archive: JarURLConnection =>
            Option(archive.getJarEntry)
              .filter(_.isDirectory)
              .map(_ => s"Classpath resource is a directory rather than a file: $rendered")
          case _ => None
        }
      case _ => None
    }

  /**
   * Acquires a file as a stream, naming the source rather than repeating what the platform
   * said about it.
   *
   * Interpreting the path is part of the acquisition, because a path can be rejected without
   * the file system being touched at all, and that rejection is as caller-controlled as any
   * other: it is wrapped exactly like a refused open.
   *
   * The order of the steps is the same as on the classpath side and keeps the wrapping right:
   * the interpretation, the description of the source and the open are one interruptible
   * region, so anything the platform raises inside it - a path it declines to interpret, a
   * path it holds nothing at, an open it refuses - becomes the wrapped `unopenable` failure,
   * while the refusal this object decides for itself is raised afterwards from the `Either`
   * that region yields and so carries its own wording unwrapped.
   *
   * @param rendered  the path in its bounded, single-line rendering, for the message
   * @param path  the path to interpret and open, exactly as it was supplied
   */
  private def openFileStream(rendered: String, path: String): IO[InputStream] =
    IO.interruptible(regularFileStream(rendered, path))
      .handleErrorWith(cause => IO.raiseError(unopenable(fileSource(rendered), cause)))
      .flatMap {
        case Right(stream) => IO.pure(stream)
        case Left(refusal) => IO.raiseError(new IOException(refusal))
      }

  /**
   * Describes a path and opens it only where the platform describes a '''regular file''', or
   * says why it was not opened.
   *
   * This is the one question that has to be asked '''before''' the open rather than after it,
   * and the reason is the difference between the two calls. Describing a path is `stat`, which
   * answers about the entry itself and returns whatever kind of source it names; opening one is
   * `open`, and for a source that is not a regular file `open` is defined to '''wait''' - a
   * named pipe with no writer waits for one, a socket waits for a peer, some devices wait for
   * data - and that wait is in a native call that a thread interrupt does not abort. An open
   * of that kind is therefore unbounded in a way no bound of this object can shorten, because
   * acquisition is uncancelable by design, so the source is refused here instead, where the
   * question costs one `stat` and answers immediately whatever kind of source it is.
   *
   * What that narrows is only the '''kind''' of source, not which files a caller may name. A
   * regular file is opened wherever the platform reports one, symbolic links being followed,
   * so a link to a file and a pseudo-file the platform describes as regular read exactly as
   * they did; a directory, a named pipe, a socket, a device and anything else the platform
   * calls other than regular are refused with the wording below rather than opened.
   *
   * The description and the open are '''not''' atomic, and that race is stated rather than
   * hidden: a path can be replaced between the two, so a source described as a regular file
   * can be something else by the time it is opened, and such an open can still wait. What the
   * pair removes is the case a caller can arrange by choosing a name - which is the whole of
   * what a reader of caller-supplied paths can remove - and the residual is one open of a
   * source that was a regular file when it was described. The time bound still frees the
   * caller in that case, and the residual is recorded at the head of this object.
   *
   * @param rendered  the path in its bounded, single-line rendering, for the message
   * @param path  the path to interpret, describe and open, exactly as it was supplied
   * @return the opened stream, or the message of the refusal to raise in its place
   */
  private def regularFileStream(rendered: String, path: String): Either[String, InputStream] = {
    val file = Paths.get(path)
    // `stat` rather than a predicate: a predicate answers false for a path the platform holds
    // nothing at, which would report an absent file as a refusal of this object's own instead
    // of as the platform's report carried as a cause.
    val described = Files.readAttributes(file, classOf[BasicFileAttributes])
    if (described.isRegularFile) Right(Files.newInputStream(file))
    else Left(notARegularFile(rendered, described.isDirectory))
  }

  /**
   * The refusal of a source that is not a regular file, naming the source and what the
   * platform described it as.
   *
   * A directory is named as one, because that is the mistake a caller is most likely to have
   * made and it is what the classpath side of this object already says of a name that resolved
   * to one; every other kind - a named pipe, a socket, a device - is named by what it is not,
   * since the distinction between them changes nothing about the refusal.
   *
   * @param rendered  the path in its bounded, single-line rendering
   * @param directory  whether the platform described the source as a directory
   * @return the message of the refusal
   */
  private def notARegularFile(rendered: String, directory: Boolean): String = {
    val described =
      if (directory) "is a directory rather than a regular file" else "is not a regular file"
    s"${fileSource(rendered)} $described, so it was not opened; this reader opens regular " +
      "files only, because the open of a source of another kind - a named pipe with no " +
      "writer, a socket, some devices - can wait for a peer that never arrives and cannot " +
      "be interrupted"
  }

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
