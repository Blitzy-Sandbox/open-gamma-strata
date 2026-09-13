/*
 * ===========================================================================
 *  capture-baseline.jsh - Java parity baseline capture
 * ===========================================================================
 *
 *  PURPOSE
 *  -------
 *  A JShell script (JDK 21) that runs against the Maven-built *Java* Strata
 *  jars and emits the six numerical parity baseline fixtures plus the
 *  reference-data manifest that the Scala test suite asserts against.
 *
 *  Those SEVEN JSON documents are the deliverable and the whole of it; the
 *  script is retained so that they can be regenerated and audited by a third
 *  party. `manifest/java-test-mapping.csv` is NOT one of them and is neither
 *  written nor read here: it records mapping decisions a scanner cannot derive
 *  - which Scala spec absorbed a consolidated Java test, why a test was
 *  dropped - so it is owned alongside the Scala test suite and verified by the
 *  test-scope gate that consumes it.
 *
 *  The procedure, the fixture schemas and the manifest schema are in
 *  `tools/parity-capture/README.md`.
 *
 *  TOOLCHAIN AND INVOCATION
 *  ------------------------
 *  `jshell` ships with JDK 21, so nothing is installed. This script is on no
 *  sbt source root: it is compiled by nothing, shipped in nothing and run by no
 *  CI job. Its extension is `.jsh`, never `.java` - that is load-bearing,
 *  because the deliverable forbids `.java` files - and it emits, generates and
 *  compiles no Java source and adds no build step.
 *
 *      jshell --class-path "<the six jars below>" -R-Xmx900m \
 *             tools/parity-capture/capture-baseline.jsh
 *
 *  `parity.out.dir` (default `.`, expected to be the repository root) is the
 *  only property the capture READS; the seven output paths are relative to it.
 *  Pass it through JShell's `-R` prefix. Run it from the repository root, and
 *  see the README for the two ways of assembling the classpath.
 *
 *  The capture WRITES one property, `capture.exit.status`, and it is not an
 *  input: it is the channel the run's verdict travels on. SECTION 0 clears it
 *  before anything can fail, SECTION 14 sets it to "0" only once every
 *  document has been built and every check has passed, and the final `/exit`
 *  reads it with `Integer.getInteger`, defaulting to 1. A value supplied by
 *  the caller is reported and discarded, so `-R-Dcapture.exit.status=0`
 *  cannot pre-authorise a success this run has not earned. Behaviour 4 below
 *  is why the verdict cannot simply be computed in the exit expression.
 *
 *  CLASSPATH - READ BEFORE "FIXING" IT
 *  -----------------------------------
 *  Six jars: the `strata-basics` and `strata-collect` Java jars, plus the
 *  Guava, Guava `failureaccess`, Joda-Beans and Joda-Convert jars the Java
 *  implementation requires. Carrying Guava and Joda here is NOT a
 *  dependency-purity (Rule 1 / Gate 2) violation: that gate measures the sbt
 *  `Compile` and `Test` classpaths of the two Scala modules, and `tools/` is on
 *  neither of them. Removing them does not improve purity; it makes the capture
 *  impossible, because the implementation being measured depends on both.
 *
 *  WRITE BOUNDARY
 *  --------------
 *  This script READS `modules/**` - through the classpath and the resources
 *  inside the jars - and NEVER writes there. It writes exactly two things, and
 *  both boundaries are enforced at run time rather than by convention:
 *
 *    * the seven documents, under the output root. `outputRoot` REFUSES A ROOT
 *      THAT SITS INSIDE A CHECKOUT WITHOUT BEING ITS ROOT BEFORE IT CREATES
 *      ANYTHING (so `-Dparity.out.dir=modules` is rejected, not obeyed, and
 *      leaves no directory behind), refuses a symbolic link at ANY component of
 *      it, and re-asserts the placement after canonicalisation;
 *      `guardedOutputTarget` then requires each path to be one of the seven
 *      declared literals, to stay under the canonical root, and to have no
 *      symbolic link at any component;
 *    * one empty lock file, under the JVM temporary directory, which must be
 *      absolute, already present and outside every checkout - so the lock
 *      cannot reach the Maven tree either. The lock path creates NO DIRECTORY
 *      at all: the file is created relative to a handle on a directory that
 *      already exists (`openLockRoot`, `acquireOutputLock`).
 *
 *  Every directory actually written to is reached through a HANDLE opened
 *  component by component from the filesystem root, never by name a second
 *  time, so the directory the bytes land in is the directory that was validated
 *  (see `OutputDirectory` and `openOrCreateNoFollowDirectory`). Every write,
 *  rename and delete goes through that handle, and a filesystem that cannot
 *  provide one refuses the run rather than falling back to pathnames.
 *
 *  CONCURRENCY
 *  -----------
 *  One capture at a time per output root. The run takes an exclusive lock keyed
 *  on the canonical root before it validates or creates anything and holds it
 *  through publication and cleanup, so two captures cannot interleave their
 *  publish moves and leave a mixed generation behind (`acquireOutputLock`).
 *
 *  DETERMINISM CONTRACT
 *  --------------------
 *  Same inputs => byte-identical output, on any JDK 21:
 *    * every pseudo-random value comes from one `java.util.Random` seeded with
 *      the fixed literal `RANDOM_SEED` (the JDK specifies the algorithm, so it
 *      reproduces exactly);
 *    * no wall-clock, no locale-sensitive formatting (never `String.format` or
 *      `DecimalFormat` for numbers), no unordered `HashMap` / `HashSet` on any
 *      output path - only `LinkedHashMap` / `TreeMap` / explicitly ordered
 *      lists;
 *    * doubles are written with `Double.toString`, the shortest exactly
 *      round-tripping representation (this is what "full precision" means for
 *      the 1e-9 absolute AND relative comparison of Rule 2 / Gate 3, and it
 *      preserves `-0.0`);
 *    * non-finite doubles are written as the tagged strings "NaN",
 *      "Infinity" and "-Infinity" per the non-finite JSON policy;
 *    * strings are escaped to pure ASCII, so the bytes do not depend on the
 *      platform charset.
 *
 *  FAIL-FAST CONTRACT
 *  ------------------
 *  Every captured value that has a hard-coded Java test constant is
 *  cross-checked against that constant using THAT TEST'S OWN tolerance (exact
 *  equality for the DayCount tables, 1e-6 for FxMatrix, 1e-14 for DoubleArray).
 *  Reference-data row counts are asserted against independently verified
 *  expected counts.
 *
 *  A REJECTION IS AN EXPECTATION TOO, and it is held to the same standard:
 *  every failure a Java test states carries the exception TYPE that test names
 *  and is compared by assignability (see the `Expect` banner), so a rejection
 *  that changes type cannot be recaptured as the new truth. What a measured
 *  call may throw at all is bounded separately by `requireCapturable`: a closed
 *  list of domain rejections is recorded, and an `Error`, a checked exception
 *  or a runtime type outside that list (NullPointerException,
 *  ClassCastException, ...) aborts the capture instead of becoming a fixture
 *  value. A row whose outcome NO Java test states is counted as capture-only in
 *  the summary, so the size of that set is visible rather than implicit.
 *
 *  All documents are built in memory and written only after every check has
 *  passed, so a failed run cannot leave a half-valid fixture on disk; the
 *  publication itself is a staged transaction that either replaces every
 *  document or leaves the previous generation in place (see `flushDocuments`).
 *  Any failure prints an actionable diagnostic and the script exits with a
 *  non-zero status.
 *
 *  REFLECTION IS PERMITTED HERE
 *  ----------------------------
 *  The "no reflection" rule applies to the Scala codec path, not to this
 *  developer tool. Reflection is used only to enumerate public constants
 *  holders (`DayCounts`, `HolidayCalendarIds`, ...), which is the cleanest way
 *  to obtain a provably complete constant list. Where an `ExtendedEnum`
 *  accessor gives the same answer, the accessor is preferred.
 *
 *  FOUR JSHELL BEHAVIOURS THIS SCRIPT RELIES ON (all verified on JDK 21)
 *  --------------------------------------------------------------------
 *  1. `/set feedback silent` is NOT used. In script-file (non-interactive)
 *     mode JShell registers no predefined feedback modes, so that command
 *     fails and prints "Does not match any current feedback mode: silent" to
 *     stdout - it would itself be the pollution it is meant to prevent. File
 *     mode already echoes no snippet values, so the convention this script
 *     follows instead is: never write a bare expression statement, and print
 *     only deliberately via System.out.
 *  2. Every Strata type is imported with an explicit SINGLE-TYPE import, never
 *     on demand. JShell auto-imports `java.util.*`, so an on-demand import of
 *     `com.opengamma.strata.basics.currency.*` makes `Currency` ambiguous with
 *     `java.util.Currency` and fails to compile. A single-type import shadows
 *     the auto-imported on-demand one.
 *  3. A TYPE MUST BE DECLARED BEFORE IT IS USED. JShell defers a snippet that
 *     names an undeclared method until the method arrives, but rejects one that
 *     names an undeclared type, so every helper class precedes the snippets
 *     that mention it - which is why the lock code follows `OutputDirectory`.
 *  4. AN `/exit` WHOSE EXPRESSION FAILS TO COMPILE DOES NOT EXIT. JShell
 *     reports the compilation error and then falls through to its interactive
 *     REPL, which reads EOF from a redirected stdin and terminates with status
 *     ZERO - or, when stdin is a terminal, waits at a prompt and never
 *     terminates at all. The exit expression must therefore be compilable
 *     under every load this script can suffer, and the only loads it has to
 *     survive are the ones where its own declarations are gone: an incomplete
 *     classpath takes down whole sections at once. So the expression names
 *     `java.lang.Integer` and two literals and nothing else, the verdict
 *     travels in the `capture.exit.status` property, and the default is
 *     failure. Nothing else in this script may be added to that expression.
 *     Two related behaviours are ruled out for the same reason and must not be
 *     reintroduced: a JShell COMMAND cannot be made conditional, so the
 *     preflight cannot `/exit 1` on its own; and `System.exit` inside a
 *     snippet does not propagate - it kills the remote execution JVM, upon
 *     which JShell prints "State engine terminated." and again falls through
 *     to the REPL with status zero.
 *
 *  A snippet that throws does not stop JShell - it prints a trace and the next
 *  snippet runs. The whole capture is therefore performed by one guarded
 *  driver invoked from a single snippet at the end of this file.
 * ===========================================================================
 */

/* ===========================================================================
 * SECTION 0 - THE EXIT-STATUS CHANNEL, THEN THE CLASSPATH PREFLIGHT
 *
 * Runs before anything else and fails loudly on the most common environment
 * error: an incomplete --class-path.
 *
 * It is written with Class.forName on STRING literals, referencing no Strata
 * type, so that it still compiles and still runs when those types are absent -
 * which is precisely the case it has to diagnose. Without it a developer sees
 * only JShell's wall of "package does not exist" errors; with it, the first
 * thing printed names the missing classes and the classpath to fix.
 *
 * The preflight cannot end the run itself, and that is a property of JShell
 * rather than a choice: a JShell command cannot be made conditional, so there
 * is no `/exit 1` to put here, and `System.exit` inside a snippet does not
 * propagate (header behaviour 4). The preflight therefore prints, sets
 * PREFLIGHT_OK and returns; the run fails closed through the exit-status
 * channel opened immediately below, and PREFLIGHT_OK is checked again by the
 * driver so that nothing is built or written.
 *
 * THE EXIT-STATUS CHANNEL. `capture.exit.status` is cleared here - first, and
 * by a snippet that names nothing but `java.lang` and so cannot itself be
 * lost - and is set to "0" by SECTION 14 only once every document has been
 * built and every check has passed. The final `/exit` reads it with
 * `Integer.getInteger`, defaulting to 1, which is what makes failure the
 * default for every load of this file. Clearing it also means a
 * caller-supplied `-R-Dcapture.exit.status=0` cannot pre-authorise a success
 * this run has not earned: such a value is reported and discarded.
 * ===========================================================================
 */

String PRESET_EXIT_STATUS = System.clearProperty("capture.exit.status");

if (PRESET_EXIT_STATUS != null) {
  System.out.println("note: discarding the caller-supplied capture.exit.status="
      + PRESET_EXIT_STATUS);
  System.out.println("  that property is this run's own verdict, not an input.");
}

String[] REQUIRED_CLASSES = {
    "com.opengamma.strata.basics.ReferenceData",
    "com.opengamma.strata.basics.currency.Currency",
    "com.opengamma.strata.basics.currency.FxMatrix",
    "com.opengamma.strata.basics.date.DayCount",
    "com.opengamma.strata.basics.date.HolidayCalendars",
    "com.opengamma.strata.basics.index.IborIndex",
    "com.opengamma.strata.basics.location.Country",
    "com.opengamma.strata.basics.schedule.PeriodicSchedule",
    "com.opengamma.strata.collect.array.DoubleArray",
    "com.opengamma.strata.collect.io.ResourceConfig",
    "com.opengamma.strata.collect.named.ExtendedEnum",
    // Guava and Joda are implementation dependencies of the Java modules being
    // measured. They are required HERE and are not a Rule 1 concern - see the
    // header.
    "com.google.common.collect.ImmutableMap",
    "org.joda.beans.ImmutableBean",
};

List<String> MISSING_CLASSES = new ArrayList<>();

for (String className : REQUIRED_CLASSES) {
  try {
    Class.forName(className);
  } catch (Throwable thrown) {
    // The ONE place that deliberately catches every Throwable: this is the
    // classpath preflight, and an absent class arrives either as a checked
    // ClassNotFoundException or as a NoClassDefFoundError - an Error - when a
    // class is present but its supertype is not. Both mean the same thing
    // here, and PREFLIGHT_OK aborts the run before any capture happens.
    // Everywhere else, an Error is infrastructure failure: see
    // requireCapturable.
    MISSING_CLASSES.add(className);
  }
}

boolean PREFLIGHT_OK = MISSING_CLASSES.isEmpty();

if (!PREFLIGHT_OK) {
  System.out.println("ABORTED: the JShell --class-path is incomplete.");
  System.out.println("  " + MISSING_CLASSES.size() + " required class(es) are missing:");
  for (String className : MISSING_CLASSES) {
    System.out.println("    " + className);
  }
  System.out.println("  The classpath must contain the Maven-built strata-basics and");
  System.out.println("  strata-collect jars together with their Guava and Joda dependencies.");
  System.out.println("  Build them with:");
  System.out.println("    mvn -B -pl modules/collect,modules/basics -am -DskipTests \\");
  System.out.println("        -Dcheckstyle.skip=true -Dmaven.javadoc.skip=true package");
  System.out.println("  then pass the dependency classpath as section 3 of");
  System.out.println("  tools/parity-capture/README.md shows.");
  System.out.println("  Nothing was written.");
}

// --- Strata: root, currency -------------------------------------------------
import com.opengamma.strata.basics.ReferenceData;
import com.opengamma.strata.basics.currency.BigMoney;
import com.opengamma.strata.basics.currency.Currency;
import com.opengamma.strata.basics.currency.CurrencyAmount;
import com.opengamma.strata.basics.currency.CurrencyAmountArray;
import com.opengamma.strata.basics.currency.CurrencyPair;
import com.opengamma.strata.basics.currency.FxMatrix;
import com.opengamma.strata.basics.currency.FxMatrixBuilder;
import com.opengamma.strata.basics.currency.FxRate;
import com.opengamma.strata.basics.currency.Money;
import com.opengamma.strata.basics.currency.MultiCurrencyAmount;
import com.opengamma.strata.basics.currency.MultiCurrencyAmountArray;

// --- Strata: date ----------------------------------------------------------
import com.opengamma.strata.basics.date.AdjustableDate;
import com.opengamma.strata.basics.date.BusinessDayAdjustment;
import com.opengamma.strata.basics.date.BusinessDayConvention;
import com.opengamma.strata.basics.date.BusinessDayConventions;
import com.opengamma.strata.basics.date.DateSequences;
import com.opengamma.strata.basics.date.DayCount;
import com.opengamma.strata.basics.date.DayCounts;
import com.opengamma.strata.basics.date.HolidayCalendar;
import com.opengamma.strata.basics.date.HolidayCalendarId;
import com.opengamma.strata.basics.date.HolidayCalendarIds;
import com.opengamma.strata.basics.date.HolidayCalendars;
import com.opengamma.strata.basics.date.PeriodAdditionConventions;
import com.opengamma.strata.basics.date.Tenor;

// --- Strata: index, location, schedule -------------------------------------
import com.opengamma.strata.basics.index.FloatingRateName;
import com.opengamma.strata.basics.index.FloatingRateNames;
import com.opengamma.strata.basics.index.FxIndex;
import com.opengamma.strata.basics.index.FxIndices;
import com.opengamma.strata.basics.index.IborIndex;
import com.opengamma.strata.basics.index.IborIndices;
import com.opengamma.strata.basics.index.OvernightIndex;
import com.opengamma.strata.basics.index.OvernightIndices;
import com.opengamma.strata.basics.index.PriceIndex;
import com.opengamma.strata.basics.index.PriceIndices;
import com.opengamma.strata.basics.location.Country;
import com.opengamma.strata.basics.schedule.Frequency;
import com.opengamma.strata.basics.schedule.PeriodicSchedule;
import com.opengamma.strata.basics.schedule.RollConvention;
import com.opengamma.strata.basics.schedule.RollConventions;
import com.opengamma.strata.basics.schedule.Schedule;
import com.opengamma.strata.basics.schedule.ScheduleException;
import com.opengamma.strata.basics.schedule.SchedulePeriod;
import com.opengamma.strata.basics.schedule.StubConvention;

// --- Strata: collect -------------------------------------------------------
import com.opengamma.strata.collect.Decimal;
import com.opengamma.strata.collect.array.DoubleArray;
import com.opengamma.strata.collect.array.DoubleMatrix;
import com.opengamma.strata.collect.io.CsvFile;
import com.opengamma.strata.collect.io.CsvRow;
import com.opengamma.strata.collect.io.IniFile;
import com.opengamma.strata.collect.io.PropertiesFile;
import com.opengamma.strata.collect.io.PropertySet;
import com.opengamma.strata.collect.io.ResourceConfig;
import com.opengamma.strata.collect.io.ResourceLocator;
import com.opengamma.strata.collect.named.ExtendedEnum;
import com.opengamma.strata.collect.named.Named;

// --- JDK -------------------------------------------------------------------
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SecureDirectoryStream;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributeView;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileOwnerAttributeView;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.nio.file.attribute.UserPrincipal;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.DateTimeException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.Month;
import java.time.MonthDay;
import java.time.Period;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

// --- Static imports used by the captured cases ---
import static com.opengamma.strata.basics.date.BusinessDayConventions.FOLLOWING;
import static com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_FOLLOWING;
import static com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_PRECEDING;
import static com.opengamma.strata.basics.date.BusinessDayConventions.PRECEDING;
import static com.opengamma.strata.basics.date.DayCounts.ACT_360;
import static com.opengamma.strata.basics.date.DayCounts.ACT_364;
import static com.opengamma.strata.basics.date.DayCounts.ACT_365F;
import static com.opengamma.strata.basics.date.DayCounts.ACT_365L;
import static com.opengamma.strata.basics.date.DayCounts.ACT_365_25;
import static com.opengamma.strata.basics.date.DayCounts.ACT_365_ACTUAL;
import static com.opengamma.strata.basics.date.DayCounts.ACT_ACT_AFB;
import static com.opengamma.strata.basics.date.DayCounts.ACT_ACT_ICMA;
import static com.opengamma.strata.basics.date.DayCounts.ACT_ACT_ISDA;
import static com.opengamma.strata.basics.date.DayCounts.ACT_ACT_YEAR;
import static com.opengamma.strata.basics.date.DayCounts.NL_360;
import static com.opengamma.strata.basics.date.DayCounts.NL_365;
import static com.opengamma.strata.basics.date.DayCounts.ONE_ONE;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_360_ISDA;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_360_PSA;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_EPLUS_360;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_E_360;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_E_360_ISDA;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_E_365;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_U_360;
import static com.opengamma.strata.basics.date.DayCounts.THIRTY_U_360_EOM;
import static com.opengamma.strata.basics.date.HolidayCalendarIds.JPTO;
import static com.opengamma.strata.basics.date.HolidayCalendarIds.NO_HOLIDAYS;
import static com.opengamma.strata.basics.date.HolidayCalendarIds.SAT_SUN;
import static com.opengamma.strata.basics.schedule.Frequency.P12M;
import static com.opengamma.strata.basics.schedule.Frequency.P1D;
import static com.opengamma.strata.basics.schedule.Frequency.P1M;
import static com.opengamma.strata.basics.schedule.Frequency.P1W;
import static com.opengamma.strata.basics.schedule.Frequency.P2M;
import static com.opengamma.strata.basics.schedule.Frequency.P3M;
import static com.opengamma.strata.basics.schedule.Frequency.P6M;
import static com.opengamma.strata.basics.schedule.Frequency.TERM;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_11;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_17;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_22;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_24;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_28;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_29;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_30;
import static com.opengamma.strata.basics.schedule.RollConventions.DAY_4;
import static com.opengamma.strata.basics.schedule.RollConventions.EOM;
import static com.opengamma.strata.basics.schedule.RollConventions.IMM;
import static com.opengamma.strata.basics.schedule.RollConventions.SFE;
import static com.opengamma.strata.basics.schedule.StubConvention.LONG_FINAL;
import static com.opengamma.strata.basics.schedule.StubConvention.LONG_INITIAL;
import static com.opengamma.strata.basics.schedule.StubConvention.SHORT_FINAL;
import static com.opengamma.strata.basics.schedule.StubConvention.SHORT_INITIAL;
import static com.opengamma.strata.basics.schedule.StubConvention.SMART_FINAL;
import static com.opengamma.strata.basics.schedule.StubConvention.SMART_INITIAL;
import static java.time.Month.APRIL;
import static java.time.Month.AUGUST;
import static java.time.Month.FEBRUARY;
import static java.time.Month.JULY;
import static java.time.Month.JUNE;
import static java.time.Month.MAY;
import static java.time.Month.NOVEMBER;
import static java.time.Month.OCTOBER;
import static java.time.Month.SEPTEMBER;

/* ===========================================================================
 * SECTION 1 - DETERMINISTIC JSON WRITER
 *
 * No JSON library may be assumed on the capture classpath, so the writer is
 * hand-rolled. Design constraints, all of them load-bearing for the
 * determinism contract in the header:
 *   * object key order is insertion order (LinkedHashMap), and every builder
 *     below inserts in a fixed declared order - never a HashMap;
 *   * numbers are rendered by Double.toString / Long.toString only;
 *   * strings are escaped to pure ASCII;
 *   * indentation is two spaces and line endings are '\n';
 *   * arrays of scalars are chunked at a fixed number of items per line, so a
 *     431,914-element holiday array is neither one unreadable 6 MB line nor
 *     431,914 separate lines.
 * ===========================================================================
 */

/** Number of scalar array items per line. Fixed, so output is reproducible. */
int JSON_ARRAY_ITEMS_PER_LINE = 10;

abstract class Jn {
  abstract boolean isScalar();

  abstract void write(StringBuilder sb, int indent);

  /**
   * Renders with no newlines, no indentation and no space after a separator.
   *
   * Used only by the row-per-line document form (Section 3), which
   * holiday-baseline.json uses because its 3,846 rows carry roughly 440,000
   * date strings: pretty-printing them costs about 2 MB of pure indentation
   * for no diff-readability gain, since the interesting unit there is the row,
   * not the field. Every other document is pretty-printed as Section 5 of the
   * README describes.
   */
  abstract void writeCompact(StringBuilder sb);
}

void jsonIndent(StringBuilder sb, int indent) {
  for (int i = 0; i < indent; i++) {
    sb.append("  ");
  }
}

/**
 * A pre-rendered scalar token: string, number, boolean or null. Holding the
 * rendered text means the escaping and number-formatting rules are applied in
 * exactly one place, at construction time.
 */
class JScalar extends Jn {
  private final String token;

  JScalar(String token) {
    this.token = token;
  }

  boolean isScalar() {
    return true;
  }

  void write(StringBuilder sb, int indent) {
    sb.append(token);
  }

  void writeCompact(StringBuilder sb) {
    sb.append(token);
  }
}

class JArray extends Jn {
  private final List<Jn> items = new ArrayList<>();

  JArray add(Jn item) {
    items.add(item);
    return this;
  }

  int size() {
    return items.size();
  }

  boolean isScalar() {
    return false;
  }

  private boolean allScalar() {
    for (Jn item : items) {
      if (!item.isScalar()) {
        return false;
      }
    }
    return true;
  }

  void write(StringBuilder sb, int indent) {
    if (items.isEmpty()) {
      sb.append("[]");
      return;
    }
    if (allScalar()) {
      // Scalars: one line when short, otherwise chunked at a fixed width.
      if (items.size() <= JSON_ARRAY_ITEMS_PER_LINE) {
        sb.append('[');
        for (int i = 0; i < items.size(); i++) {
          if (i > 0) {
            sb.append(", ");
          }
          items.get(i).write(sb, indent);
        }
        sb.append(']');
        return;
      }
      sb.append("[\n");
      for (int i = 0; i < items.size(); i++) {
        if (i % JSON_ARRAY_ITEMS_PER_LINE == 0) {
          jsonIndent(sb, indent + 1);
        }
        items.get(i).write(sb, indent + 1);
        if (i < items.size() - 1) {
          sb.append(',');
        }
        boolean endOfLine = (i % JSON_ARRAY_ITEMS_PER_LINE == JSON_ARRAY_ITEMS_PER_LINE - 1)
            || i == items.size() - 1;
        sb.append(endOfLine ? "\n" : " ");
      }
      jsonIndent(sb, indent);
      sb.append(']');
      return;
    }
    // Non-scalar items (objects, nested arrays): one per line.
    sb.append("[\n");
    for (int i = 0; i < items.size(); i++) {
      jsonIndent(sb, indent + 1);
      items.get(i).write(sb, indent + 1);
      if (i < items.size() - 1) {
        sb.append(',');
      }
      sb.append('\n');
    }
    jsonIndent(sb, indent);
    sb.append(']');
  }

  void writeCompact(StringBuilder sb) {
    sb.append('[');
    for (int i = 0; i < items.size(); i++) {
      if (i > 0) {
        sb.append(',');
      }
      items.get(i).writeCompact(sb);
    }
    sb.append(']');
  }

  /**
   * Renders this array as a document whose rows sit one per line: the opening
   * bracket, then every element compacted onto its own indented line, then the
   * closing bracket. Element order is insertion order, so the bytes are as
   * reproducible as the pretty-printed form.
   */
  void writeRowsPerLine(StringBuilder sb) {
    if (items.isEmpty()) {
      sb.append("[]");
      return;
    }
    sb.append("[\n");
    for (int i = 0; i < items.size(); i++) {
      sb.append("  ");
      items.get(i).writeCompact(sb);
      if (i < items.size() - 1) {
        sb.append(',');
      }
      sb.append('\n');
    }
    sb.append(']');
  }
}

class JObject extends Jn {
  // LinkedHashMap: key order is the fixed order in which fields are declared.
  private final Map<String, Jn> fields = new LinkedHashMap<>();

  JObject set(String key, Jn value) {
    fields.put(key, value);
    return this;
  }

  boolean has(String key) {
    return fields.containsKey(key);
  }

  boolean isScalar() {
    return false;
  }

  void write(StringBuilder sb, int indent) {
    if (fields.isEmpty()) {
      sb.append("{}");
      return;
    }
    sb.append("{\n");
    int i = 0;
    int last = fields.size() - 1;
    for (Map.Entry<String, Jn> entry : fields.entrySet()) {
      jsonIndent(sb, indent + 1);
      sb.append(jsonQuote(entry.getKey()));
      sb.append(": ");
      entry.getValue().write(sb, indent + 1);
      if (i < last) {
        sb.append(',');
      }
      sb.append('\n');
      i++;
    }
    jsonIndent(sb, indent);
    sb.append('}');
  }

  void writeCompact(StringBuilder sb) {
    sb.append('{');
    int i = 0;
    for (Map.Entry<String, Jn> entry : fields.entrySet()) {
      if (i > 0) {
        sb.append(',');
      }
      sb.append(jsonQuote(entry.getKey()));
      sb.append(':');
      entry.getValue().writeCompact(sb);
      i++;
    }
    sb.append('}');
  }
}

String jsonQuote(String raw) {
  StringBuilder sb = new StringBuilder(raw.length() + 2);
  sb.append('"');
  for (int i = 0; i < raw.length(); i++) {
    char c = raw.charAt(i);
    switch (c) {
      case '"':
        sb.append("\\\"");
        break;
      case '\\':
        sb.append("\\\\");
        break;
      case '\b':
        sb.append("\\b");
        break;
      case '\f':
        sb.append("\\f");
        break;
      case '\n':
        sb.append("\\n");
        break;
      case '\r':
        sb.append("\\r");
        break;
      case '\t':
        sb.append("\\t");
        break;
      default:
        if (c < 0x20 || c > 0x7e) {
          // Control characters and everything non-ASCII: \\uXXXX, so the
          // emitted bytes never depend on the platform charset.
          sb.append("\\u");
          String hex = Integer.toHexString(c);
          for (int pad = hex.length(); pad < 4; pad++) {
            sb.append('0');
          }
          sb.append(hex);
        } else {
          sb.append(c);
        }
        break;
    }
  }
  sb.append('"');
  return sb.toString();
}

Jn jNull() {
  return new JScalar("null");
}

Jn jStr(String value) {
  return value == null ? jNull() : new JScalar(jsonQuote(value));
}

Jn jBool(boolean value) {
  return new JScalar(value ? "true" : "false");
}

Jn jInt(long value) {
  return new JScalar(Long.toString(value));
}

/**
 * Renders a double at full precision. Finite values use Double.toString, the
 * shortest representation that round-trips exactly (and which preserves
 * -0.0); the three non-finite values become the tagged strings required by the
 * non-finite JSON policy. String.format and DecimalFormat are never used -
 * they are locale-sensitive.
 */
Jn jDbl(double value) {
  if (Double.isNaN(value)) {
    return new JScalar("\"NaN\"");
  }
  if (value == Double.POSITIVE_INFINITY) {
    return new JScalar("\"Infinity\"");
  }
  if (value == Double.NEGATIVE_INFINITY) {
    return new JScalar("\"-Infinity\"");
  }
  return new JScalar(Double.toString(value));
}

Jn jDate(LocalDate date) {
  return date == null ? jNull() : jStr(date.toString());
}

/**
 * The canonical name of a Strata value, which is the identity every fixture
 * carries for it.
 *
 * A `Named` type is written through getName() and every other
 * identity-bearing type through toString(), so a document holds "Act/365F",
 * "GBP-LIBOR-3M", "EUR/USD", "P3M" for a Frequency, "3M" for a Tenor and
 * "GBLO+USNY" - the exact strings a consumer parses back.
 */
Jn jName(Object value) {
  if (value == null) {
    return jNull();
  }
  if (value instanceof Named) {
    return jStr(((Named) value).getName());
  }
  return jStr(value.toString());
}

Jn jDates(Iterable<LocalDate> dates) {
  JArray array = new JArray();
  for (LocalDate date : dates) {
    array.add(jDate(date));
  }
  return array;
}

Jn jDoubleArray(DoubleArray values) {
  JArray array = new JArray();
  for (int i = 0; i < values.size(); i++) {
    array.add(jDbl(values.get(i)));
  }
  return array;
}

Jn jDoubles(double[] values) {
  JArray array = new JArray();
  for (double value : values) {
    array.add(jDbl(value));
  }
  return array;
}

Jn jDoubleMatrix(DoubleMatrix matrix) {
  JArray rows = new JArray();
  for (int r = 0; r < matrix.rowCount(); r++) {
    rows.add(jDoubleArray(matrix.row(r)));
  }
  return rows;
}

/** Renders a document: the tree, then exactly one trailing newline. */
String jsonDocument(Jn root) {
  StringBuilder sb = new StringBuilder(1 << 16);
  root.write(sb, 0);
  sb.append('\n');
  return sb.toString();
}

/**
 * Renders a row-array document with one row object per line, then exactly one
 * trailing newline. Reserved for holiday-baseline.json - see Jn.writeCompact.
 */
String jsonRowsPerLineDocument(JArray rows) {
  StringBuilder sb = new StringBuilder(1 << 24);
  rows.writeRowsPerLine(sb);
  sb.append('\n');
  return sb.toString();
}

/* ===========================================================================
 * SECTION 2 - SELF-CHECK AND ABORT FRAMEWORK
 *
 * This is what makes the fixtures trustworthy. Every value that has a
 * hard-coded Java test constant is compared against it here, using that
 * test's own tolerance; every reference-data table is compared against an
 * independently verified expected count. Failures accumulate, each is printed
 * as one actionable line, and the run ends with a non-zero status without
 * having written anything.
 *
 * TOLERANCES ARE NOT INTERCHANGEABLE (this is a real trap). The fixtures store
 * full-precision doubles, which the fixture consumer compares at 1e-9 absolute
 * AND relative; a capture-time check instead uses the tolerance of the Java
 * test that states the value:
 *   * DayCountTest.data_yearFraction / data_days -> EXACT equality;
 *   * FxMatrixTest                               -> 1e-6;
 *   * DoubleArrayTest (its DELTA)                -> 1e-14.
 * Conflating them either aborts spuriously or silently accepts drift.
 * ===========================================================================
 */

/** Per-fixture counters: rows emitted, checks performed, capture-only rows. */
class FixtureStats {
  int rows;
  int checks;
  int captureOnly;
  int errorRows;
}

class Checker {
  final List<String> failures = new ArrayList<>();
  // TreeMap: the summary prints in a stable, alphabetical fixture order.
  final Map<String, FixtureStats> stats = new TreeMap<>();

  private FixtureStats stats(String fixture) {
    FixtureStats s = stats.get(fixture);
    if (s == null) {
      s = new FixtureStats();
      stats.put(fixture, s);
    }
    return s;
  }

  void countRow(String fixture) {
    stats(fixture).rows++;
  }

  void countErrorRow(String fixture) {
    stats(fixture).errorRows++;
  }

  /**
   * Records a row that deliberately has no Java test constant to compare
   * against - a seeded-random row, or one of the JPY 0-decimal rows added to
   * close the minor-unit-digits gap. Marking them explicitly is what stops the
   * summary from hiding a row that SHOULD have been checked.
   */
  void countCaptureOnly(String fixture) {
    stats(fixture).captureOnly++;
  }

  void fail(String fixture, String rowId, String detail) {
    failures.add(fixture + " | " + rowId + " | " + detail);
  }

  /** Compares against a Java test constant with an explicit tolerance. */
  void checkClose(String fixture, String rowId, double expected, double actual, double tolerance,
      String toleranceDescription) {
    stats(fixture).checks++;
    double absDelta = Math.abs(actual - expected);
    double scale = Math.max(Math.max(Math.abs(actual), Math.abs(expected)), 1e-300);
    double relDelta = absDelta / scale;
    boolean bothNaN = Double.isNaN(expected) && Double.isNaN(actual);
    boolean identical = Double.compare(expected, actual) == 0;
    boolean withinTolerance = tolerance > 0d && absDelta <= tolerance;
    if (bothNaN || identical || withinTolerance) {
      return;
    }
    fail(fixture, rowId, "expected=" + Double.toString(expected)
        + " actual=" + Double.toString(actual)
        + " absDelta=" + Double.toString(absDelta)
        + " relDelta=" + Double.toString(relDelta)
        + " tol=" + Double.toString(tolerance) + " (" + toleranceDescription + ")");
  }

  /** Exact double equality, as the DayCount tables assert it. */
  void checkExact(String fixture, String rowId, double expected, double actual) {
    checkClose(fixture, rowId, expected, actual, 0d, "exact");
  }

  void checkInt(String fixture, String rowId, long expected, long actual) {
    stats(fixture).checks++;
    if (expected != actual) {
      fail(fixture, rowId, "expected=" + expected + " actual=" + actual + " delta="
          + (actual - expected) + " tol=0 (exact int)");
    }
  }

  void checkEquals(String fixture, String rowId, Object expected, Object actual) {
    stats(fixture).checks++;
    if (expected == null ? actual != null : !expected.equals(actual)) {
      fail(fixture, rowId, "expected=" + expected + " actual=" + actual + " tol=n/a (equals)");
    }
  }

  void checkTrue(String fixture, String rowId, boolean condition, String detail) {
    stats(fixture).checks++;
    if (!condition) {
      fail(fixture, rowId, detail);
    }
  }

  /**
   * Asserts a reference-data row count.
   *
   * An unexpected count aborts the capture: the resource behind it has changed,
   * and publishing the new count would silently reshape the manifest.
   */
  void checkCount(String fixture, String what, int expected, int actual) {
    stats(fixture).checks++;
    if (expected != actual) {
      fail(fixture, what, "expected count=" + expected + " actual count=" + actual
          + " tol=0 (reference-data count)");
    }
  }

  boolean ok() {
    return failures.isEmpty();
  }

  /**
   * Pads with spaces by hand rather than through String.format / printf.
   * Those are locale-sensitive, and this script uses no locale-sensitive
   * formatting anywhere - not even for console output, so that the claim needs
   * no exception.
   */
  private String pad(String value, int width, boolean left) {
    StringBuilder sb = new StringBuilder();
    if (left) {
      sb.append(value);
    }
    for (int i = value.length(); i < width; i++) {
      sb.append(' ');
    }
    if (!left) {
      sb.insert(0, value);
      sb.setLength(Math.max(width, value.length()));
      String padded = sb.toString();
      return padded.substring(value.length()) + value;
    }
    return sb.toString();
  }

  private String row(String fixture, String rows, String checks, String errorRows,
      String captureOnly) {
    return pad(fixture, 26, true) + " " + pad(rows, 8, false) + " " + pad(checks, 8, false) + " "
        + pad(errorRows, 8, false) + " " + pad(captureOnly, 8, false);
  }

  void printSummary() {
    System.out.println();
    System.out.println("--- capture summary -------------------------------------------------");
    System.out.println(row("fixture", "rows", "checks", "errRows", "capOnly"));
    int totalRows = 0;
    int totalChecks = 0;
    for (Map.Entry<String, FixtureStats> entry : stats.entrySet()) {
      FixtureStats s = entry.getValue();
      System.out.println(row(entry.getKey(), Integer.toString(s.rows), Integer.toString(s.checks),
          Integer.toString(s.errorRows), Integer.toString(s.captureOnly)));
      totalRows += s.rows;
      totalChecks += s.checks;
    }
    System.out.println(row("TOTAL", Integer.toString(totalRows), Integer.toString(totalChecks), "",
        ""));
    System.out.println("---------------------------------------------------------------------");
  }

  void printFailures() {
    if (failures.isEmpty()) {
      return;
    }
    System.out.println();
    System.out.println("!!! " + failures.size() + " CHECK FAILURE(S) - NOTHING WAS WRITTEN !!!");
    for (String failure : failures) {
      System.out.println("FAIL " + failure);
    }
  }
}

Checker CHECK = new Checker();

// Tolerances, each named after the Java test that defines it.
double TOL_EXACT = 0d;
double TOL_FX_MATRIX = 1e-6;            // FxMatrixTest.TOLERANCE
double TOL_DOUBLE_ARRAY = 1e-14;        // DoubleArrayTest.DELTA

/** The single seeded source of every pseudo-random value in this script. */
long RANDOM_SEED = 20240117L;
Random RND = new Random(RANDOM_SEED);

double nextRandomRate() {
  // Positive, well-scaled FX-style rates.
  return Math.round(RND.nextDouble() * 1e8 * 2d) / 1e8 + 0.005d;
}

double nextRandomAmount() {
  return Math.round((RND.nextDouble() - 0.5d) * 2e10) / 100d;
}

/* ===========================================================================
 * SECTION 3 - OUTPUT PATHS AND THE WRITE GUARD
 * ===========================================================================
 */

String OUT_ROOT = System.getProperty("parity.out.dir", ".");

String OUTPUT_DAYCOUNT = "strata-basics/src/test/resources/parity/daycount-baseline.json";
String OUTPUT_SCHEDULE = "strata-basics/src/test/resources/parity/schedule-baseline.json";
String OUTPUT_FX = "strata-basics/src/test/resources/parity/fx-baseline.json";
String OUTPUT_CURRENCY_MATH = "strata-basics/src/test/resources/parity/currency-math-baseline.json";
String OUTPUT_HOLIDAY = "strata-basics/src/test/resources/parity/holiday-baseline.json";
String OUTPUT_DOUBLE_ARRAY = "strata-collect/src/test/resources/parity/double-array-baseline.json";
String OUTPUT_MANIFEST = "strata-basics/src/test/resources/manifest/reference-data-manifest.json";

/**
 * Directories this script must never write into. The repository boundary is
 * enforced here, in code, rather than by convention: `git status --porcelain --
 * modules examples eclipse pom.xml src .github` has to stay empty.
 */
String[] FORBIDDEN_OUTPUT_PREFIXES = {"modules", "examples", "eclipse", "src", ".github", "project"};

Path OUTPUT_ROOT_PATH = null;

/**
 * True when `directory` is the root of a Strata checkout.
 *
 * BOTH markers are required - `build.sbt` and `modules/` - so that a directory
 * merely holding one of the two is not mistaken for a checkout root. `modules/`
 * alone is a common directory name, and a `build.sbt` alone sits at the root of
 * any sbt project.
 */
boolean isCheckoutRoot(Path directory) {
  return Files.isRegularFile(directory.resolve("build.sbt"), LinkOption.NOFOLLOW_LINKS)
      && Files.isDirectory(directory.resolve("modules"), LinkOption.NOFOLLOW_LINKS);
}

/**
 * Rejects an output root that sits INSIDE a checkout without being its root.
 *
 * The seven output paths are relative, so a root of `modules` would write to
 * `modules/strata-basics/src/test/resources/...` - inside a tree this project
 * must leave untouched, and creating directories there breaks the repository
 * boundary check (`git status --porcelain -- modules examples eclipse pom.xml
 * src .github` has to stay empty). Testing the first path component against a
 * name list cannot catch `modules/basics` or `strata-basics`; walking upward
 * from the root catches every one of them, and still allows the two roots
 * that make sense: the checkout root itself, and any directory outside a
 * checkout (which is how a dry run into a scratch directory works).
 *
 * It asks only whether an ANCESTOR is a checkout root, through
 * `Files.isRegularFile` / `Files.isDirectory` - which answer false for what
 * does not exist - so it is safe on a path no component of which exists yet.
 * That is what lets `outputRoot` apply it BEFORE it creates anything: a
 * refused root must leave no directory behind, least of all inside the tree
 * the refusal exists to protect.
 */
void requireOutputRootPlacement(Path root) {
  for (Path cursor = root.getParent(); cursor != null; cursor = cursor.getParent()) {
    if (isCheckoutRoot(cursor)) {
      throw new IllegalStateException("parity.out.dir is inside the checkout rooted at " + cursor
          + ", which this script must not write into: " + root
          + " - use the checkout root itself, or a directory outside it");
    }
  }
}

/**
 * Resolves `parity.out.dir` to a canonical, contained, link-free directory.
 *
 * Canonicalising alone is not containment (CWE-59). `toRealPath` would happily
 * report `/run/lock` for a declared `/var/run/lock` and carry on, so the whole
 * generation would land somewhere the caller never named - silently, because
 * the resolved path is perfectly valid. So the root is checked BEFORE
 * anything is created, and the checks are ordered so that a REFUSAL CREATES
 * NOTHING:
 *
 *  1. the DECLARED root must not sit inside a checkout without being its
 *     root. This is first because it is the one refusal that would otherwise
 *     leave a footprint: the check below it creates the missing components,
 *     so asserting placement after them would refuse `parity.out.dir=modules`
 *     correctly and still have created `modules/qa-probe` inside the tree
 *     this script must not write into. The test needs no component to exist
 *     (see `requireOutputRootPlacement`), so nothing is lost by doing it on
 *     the declared path;
 *  2. every component of the declared root, from the filesystem root down, is
 *     opened RELATIVE TO ITS PARENT'S HANDLE and rejected if it is a symbolic
 *     link - including the root's own last component, and including ancestors,
 *     which is the case canonicalisation hides;
 *  3. a missing component is created individually, never through
 *     `createDirectories`, and the creation is then verified through the
 *     parent's handle, so a component created outside the validated parent
 *     refuses the run instead of being written under
 *     (`openOrCreateNoFollowDirectory`);
 *  4. the result must equal its own canonical form, which after 2 it does -
 *     the equality is asserted rather than assumed, so a filesystem that
 *     aliases paths some other way (a case-insensitive mount, a bind mount)
 *     is refused instead of quietly redirecting the output;
 *  5. the placement is asserted AGAIN, on the canonical path. 1 and 5 are not
 *     redundant: canonicalisation can move a path into a checkout, and only
 *     the canonical form of the root is what the documents are written under.
 *     Keeping both means neither a declared nor a resolved path can reach a
 *     forbidden tree, and the declared one cannot even create a directory in
 *     it on the way to being refused.
 */
Path outputRoot() throws IOException {
  if (OUTPUT_ROOT_PATH != null) {
    return OUTPUT_ROOT_PATH;
  }
  Path declared = Paths.get(OUT_ROOT).toAbsolutePath().normalize();
  // Step 1: before a single directory is created.
  requireOutputRootPlacement(declared);
  if (declared.getRoot() == null) {
    throw new IllegalStateException("parity.out.dir has no filesystem root: " + declared);
  }
  // Steps 2 and 3: descend from the filesystem root through trusted handles,
  // creating the missing components and VERIFYING each one through its
  // parent's handle. A component that is a link, or that is not a directory,
  // or that was created anywhere other than inside the parent this descent is
  // holding, refuses the run rather than being written under.
  openOrCreateNoFollowDirectory(declared).close();
  Path canonical = declared.toRealPath();
  if (!canonical.equals(declared)) {
    throw new IllegalStateException("parity.out.dir is not canonical: " + declared
        + " resolves to " + canonical + " - pass the resolved path instead");
  }
  // Step 5: the same assertion on the canonical path, which is the one the
  // documents are actually written under.
  requireOutputRootPlacement(canonical);
  OUTPUT_ROOT_PATH = canonical;
  return OUTPUT_ROOT_PATH;
}

/**
 * Validates one declared output path and returns the absolute file it names.
 *
 * FOUR INDEPENDENT CHECKS, because a string test is not containment (CWE-59):
 *
 *  1. the relative string must be plain - no absolute prefix, no `..`, no
 *     backslash - and its first component must not be a repository area this
 *     script must never write into;
 *  2. it must be one of the seven declared output literals, so a typo cannot
 *     invent a ninth destination;
 *  3. resolved against the CANONICAL root and normalised, it must still start
 *     with that root, so no combination of root and relative path can escape;
 *  4. no existing component of the path may be a symbolic link, and the target
 *     itself must be absent or a regular file. Without 4, checks 1-3 all pass
 *     while the write lands wherever the link points - the resolved path never
 *     leaves the root, but the bytes do.
 *
 * Nothing here writes, and nothing here is relied on for containment at write
 * time: these are the cheap early diagnostics on the DECLARED path, and the
 * write itself goes through the directory handle that
 * `openOrCreateNoFollowDirectory` descends to.
 */
Path guardedOutputTarget(String relativePath) throws IOException {
  if (relativePath.startsWith("/") || relativePath.contains("..") || relativePath.contains("\\")) {
    throw new IllegalStateException("Refusing to write outside the repository: " + relativePath);
  }
  String first = relativePath.replace('\\', '/').split("/")[0];
  for (String forbidden : FORBIDDEN_OUTPUT_PREFIXES) {
    if (first.equals(forbidden)) {
      throw new IllegalStateException("Refusing to write into " + forbidden + "/: " + relativePath);
    }
  }
  boolean declared = relativePath.equals(OUTPUT_DAYCOUNT)
      || relativePath.equals(OUTPUT_SCHEDULE)
      || relativePath.equals(OUTPUT_FX)
      || relativePath.equals(OUTPUT_CURRENCY_MATH)
      || relativePath.equals(OUTPUT_HOLIDAY)
      || relativePath.equals(OUTPUT_DOUBLE_ARRAY)
      || relativePath.equals(OUTPUT_MANIFEST);
  if (!declared) {
    throw new IllegalStateException("Not a declared output path: " + relativePath);
  }
  Path root = outputRoot();
  Path target = root.resolve(relativePath).normalize();
  if (!target.startsWith(root)) {
    throw new IllegalStateException("Refusing to write outside " + root + ": " + target);
  }
  Path cursor = root;
  for (Path element : root.relativize(target)) {
    cursor = cursor.resolve(element);
    if (Files.isSymbolicLink(cursor)) {
      throw new IllegalStateException("Refusing to write through a symbolic link: " + cursor);
    }
    boolean isTarget = cursor.equals(target);
    if (!isTarget && Files.exists(cursor, LinkOption.NOFOLLOW_LINKS)
        && !Files.isDirectory(cursor, LinkOption.NOFOLLOW_LINKS)) {
      throw new IllegalStateException("Output parent is not a directory: " + cursor);
    }
    if (isTarget && Files.exists(cursor, LinkOption.NOFOLLOW_LINKS)
        && !Files.isRegularFile(cursor, LinkOption.NOFOLLOW_LINKS)) {
      throw new IllegalStateException("Output target is not a regular file: " + cursor);
    }
  }
  return target;
}

/**
 * Documents are accumulated here and flushed only after every check has
 * passed, so a failed run cannot leave a partially written fixture behind.
 * LinkedHashMap, so they are written in a fixed order.
 */
Map<String, String> PENDING_DOCUMENTS = new LinkedHashMap<>();

void stageDocument(String relativePath, Jn root) throws IOException {
  guardedOutputTarget(relativePath);
  PENDING_DOCUMENTS.put(relativePath, jsonDocument(root));
}

void stageRowsPerLineDocument(String relativePath, JArray rows) throws IOException {
  guardedOutputTarget(relativePath);
  PENDING_DOCUMENTS.put(relativePath, jsonRowsPerLineDocument(rows));
}

/**
 * The suffix of every temporary this run creates. The pid keeps two capture
 * runs in one output tree from colliding; no temporary name ever reaches a
 * deliverable, so this does not weaken the determinism contract.
 */
String FLUSH_TEMP_SUFFIX = ".capture-tmp-" + ProcessHandle.current().pid();

/**
 * Marks a temporary or a backup left behind by ANY capture run, not just this
 * one: every name this script creates under an output directory carries it.
 */
String FLUSH_ARTEFACT_MARKER = ".capture-tmp-";

/**
 * A VALIDATED OUTPUT DIRECTORY, OPERATED THROUGH A HANDLE RATHER THAN A NAME.
 *
 * Validating a path and then writing to that path are two different acts, and
 * between them the directory can be replaced by a symbolic link: the checks
 * pass, the bytes land elsewhere (CWE-59, the classic time-of-check to
 * time-of-use window). A handle closes it. Every operation below - create,
 * read back, move, delete - is performed RELATIVE to a directory that was
 * opened once, after validation, so it reaches the directory that was
 * validated rather than whatever its name points at now.
 *
 * That is what `SecureDirectoryStream` provides, and the JDK implements it on
 * every platform this capture runs on (Linux, macOS, Solaris - anywhere
 * `openat` exists). WHERE IT IS ABSENT THE RUN REFUSES. There is no path-based
 * fallback, deliberately: `NOFOLLOW_LINKS` on a path-based write only refuses
 * a link at the FINAL component, so a parent exchanged after validation still
 * redirects the bytes - a fallback that looks safe and is not. Refusing keeps
 * the guarantee absolute: every byte this script writes, every rename and
 * every delete is performed relative to a directory handle, and no I/O
 * operation resolves a pathname at all.
 *
 * THE HANDLE IS ACQUIRED BY DESCENT, NOT BY NAME. Opening the directory by
 * pathname would reopen the very window the handle exists to close: the
 * validation walked the components, and `Files.newDirectoryStream(path)`
 * resolves them AGAIN, following any link that appeared in between (CWE-367
 * and CWE-59 together - the check and the use look at different objects). So
 * `openChild` opens each component RELATIVE TO ITS PARENT'S HANDLE with
 * NOFOLLOW_LINKS, and `openOrCreateNoFollowDirectory` chains that from the filesystem
 * root - the one directory no descent can reach any other way, and the one
 * that cannot be a symbolic link. A component swapped after validation is then
 * not followed but REFUSED, and once the handle is held no name is resolved
 * again at all.
 *
 * The ONE by-name open left is the filesystem root, which no descent can
 * reach any other way and which cannot be a symbolic link; `openVerified`
 * still brackets it with a NOFOLLOW identity read - `fileKey` before and after
 * - so even that open refuses if the object under the name changed.
 *
 * Names passed here are single-element file names, never paths: a handle has
 * no notion of `..`, which is the point.
 */
final class OutputDirectory implements AutoCloseable {
  final Path path;
  private final SecureDirectoryStream<Path> secure;

  private OutputDirectory(Path path, SecureDirectoryStream<Path> secure) {
    this.path = path;
    this.secure = secure;
  }

  /**
   * Wraps a freshly opened stream, refusing it unless it is secure.
   *
   * This is the single place the guarantee is enforced, so no operation below
   * has to ask whether it holds: if a handle exists, it is handle-relative.
   */
  private static OutputDirectory of(Path directory, DirectoryStream<Path> opened)
      throws IOException {
    if (!(opened instanceof SecureDirectoryStream)) {
      opened.close();
      throw new IllegalStateException("Refusing to publish into " + directory
          + ": this filesystem does not support SecureDirectoryStream, so a write cannot be"
          + " bound to the directory that was validated. Point -Dparity.out.dir at a filesystem"
          + " that does (any local POSIX filesystem on Linux, macOS or Solaris) and re-run.");
    }
    return new OutputDirectory(directory, (SecureDirectoryStream<Path>) opened);
  }

  /**
   * Opens `directory` by pathname, bracketed by a no-follow identity read.
   *
   * Used for the filesystem root, which every descent starts from. The two
   * `fileKey` reads are what makes a by-name open defensible: the object that
   * carried the name before the open must be the object that carries it after,
   * or the run refuses rather than writing to whatever took its place.
   */
  static OutputDirectory openVerified(Path directory) throws IOException {
    BasicFileAttributes before =
        Files.readAttributes(directory, BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
    if (!before.isDirectory()) {
      throw new IllegalStateException(
          "Refusing to open an output component that is not a directory: " + directory);
    }
    Object key = before.fileKey();
    if (key == null) {
      throw new IllegalStateException("Refusing to open " + directory
          + " by name: this filesystem reports no file key, so it cannot be proved that the"
          + " directory opened is the directory that was validated. Run the capture on a"
          + " filesystem that supports secure directory streams or file keys.");
    }
    DirectoryStream<Path> opened = Files.newDirectoryStream(directory);
    boolean keep = false;
    try {
      BasicFileAttributes after =
          Files.readAttributes(directory, BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
      if (!after.isDirectory() || !key.equals(after.fileKey())) {
        throw new IllegalStateException("Refusing to write to " + directory
            + ": it was replaced while it was being opened");
      }
      keep = true;
      return of(directory, opened);
    } finally {
      if (!keep) {
        opened.close();
      }
    }
  }

  /**
   * Opens the single-element `name` as a subdirectory of this handle, without
   * following a symbolic link at that name.
   */
  OutputDirectory openChild(Path name) throws IOException {
    return of(path.resolve(name), secure.newDirectoryStream(name, LinkOption.NOFOLLOW_LINKS));
  }

  /**
   * Creates the single-element `name` as a subdirectory and returns the handle
   * on what was created, or refuses.
   *
   * `SecureDirectoryStream` cannot create a directory, so this is the one
   * operation that has to name a path. What makes it safe is the verification
   * rather than the creation: the new directory is then opened THROUGH THIS
   * HANDLE, so a creation that landed anywhere other than inside the directory
   * this handle holds - because the name was exchanged for a link on the way -
   * cannot be found here and refuses the run. A racing capture or a
   * concurrently created directory is not an error: the name already existing
   * simply means the open decides what is there.
   */
  OutputDirectory createChild(Path name) throws IOException {
    try {
      Files.createDirectory(path.resolve(name));
    } catch (FileAlreadyExistsException raced) {
      // Someone else created it between the open that failed and this call;
      // the open below is what decides whether what is there is usable.
    }
    try {
      return openChild(name);
    } catch (NoSuchFileException notWhereItWasAsked) {
      throw new IllegalStateException("Refusing to write under " + path.resolve(name)
          + ": the directory was created but is not present inside the directory this run"
          + " validated, so the name was redirected while it was being created",
          notWhereItWasAsked);
    }
  }

  /**
   * Opens `name` inside this directory for writing as a channel that can carry
   * a file lock, creating it when absent with owner-only permissions.
   *
   * The creation is HANDLE-RELATIVE. `SecureDirectoryStream.newByteChannel`
   * creates the file inside the directory this handle holds, so no pathname is
   * resolved and a parent exchanged for a link after validation cannot redirect
   * it - the difference from `createChild`, which the NIO API forces to name a
   * path because a secure stream cannot create a DIRECTORY. The file is asked
   * for as `rw-------`, so no other account can write through it; a filesystem
   * that does not keep POSIX permissions cannot be asked for them, and there the
   * ownership read on the file is what stands.
   *
   * `SecureDirectoryStream` answers with a `SeekableByteChannel`, while a
   * `FileLock` needs a `FileChannel`; every platform that provides a secure
   * stream returns one here. Where one is not returned the run REFUSES, because
   * reopening the file by pathname would put back the window this class exists
   * to close.
   */
  FileChannel openOrCreatePrivateChannel(Path name) throws IOException {
    SeekableByteChannel opened;
    try {
      opened = secure.newByteChannel(name,
          options(StandardOpenOption.CREATE, StandardOpenOption.WRITE),
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
    } catch (UnsupportedOperationException noPosixPermissions) {
      opened = secure.newByteChannel(name,
          options(StandardOpenOption.CREATE, StandardOpenOption.WRITE));
    }
    if (opened instanceof FileChannel) {
      return (FileChannel) opened;
    }
    String answered = opened.getClass().getName();
    opened.close();
    throw new IllegalStateException("Refusing to lock " + path.resolve(name) + ": this filesystem"
        + " answers with " + answered + " rather than a FileChannel, so the lock cannot be taken"
        + " on the directory handle this run validated.");
  }

  /**
   * The owner of `name` inside this directory, read through the handle and
   * without following a link.
   *
   * Handle-bound rather than by name, so the answer describes the object inside
   * the directory this run opened and not whatever the path points at now.
   */
  UserPrincipal ownerOf(Path name) throws IOException {
    FileOwnerAttributeView view =
        secure.getFileAttributeView(name, FileOwnerAttributeView.class, LinkOption.NOFOLLOW_LINKS);
    if (view == null) {
      throw new IllegalStateException("Refusing to use " + path.resolve(name)
          + ": this filesystem does not report file ownership.");
    }
    return view.getOwner();
  }

  /**
   * The POSIX permissions of `name` inside this directory, read without
   * following a link, or null where the filesystem does not keep them.
   */
  Set<PosixFilePermission> posixPermissionsOf(Path name) throws IOException {
    PosixFileAttributeView view =
        secure.getFileAttributeView(name, PosixFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
    if (view == null) {
      return null;
    }
    try {
      return view.readAttributes().permissions();
    } catch (UnsupportedOperationException notKept) {
      return null;
    }
  }

  /**
   * The names in THIS directory that carry `marker`, sorted.
   *
   * Reads the stream this handle already holds, so the scan looks at the
   * directory that was opened rather than resolving its name a second time.
   * A `DirectoryStream` may be iterated once, and this is the only iteration
   * any handle is subjected to.
   */
  List<String> namesContaining(String marker) throws IOException {
    List<String> found = new ArrayList<>();
    for (Path entry : secure) {
      String name = entry.getFileName().toString();
      if (name.contains(marker)) {
        found.add(name);
      }
    }
    Collections.sort(found);
    return found;
  }

  private Set<OpenOption> options(OpenOption... requested) {
    Set<OpenOption> options = new LinkedHashSet<>();
    for (OpenOption option : requested) {
      options.add(option);
    }
    // A no-follow open is the whole point: if the name is a link, fail rather
    // than write through it.
    options.add(LinkOption.NOFOLLOW_LINKS);
    return options;
  }

  /** Creates `name` and writes `bytes`; fails if anything already exists there. */
  void writeNew(Path name, byte[] bytes) throws IOException {
    try (SeekableByteChannel channel =
        secure.newByteChannel(name, options(StandardOpenOption.CREATE_NEW,
            StandardOpenOption.WRITE))) {
      ByteBuffer buffer = ByteBuffer.wrap(bytes);
      while (buffer.hasRemaining()) {
        channel.write(buffer);
      }
    }
  }

  /**
   * Reads `name` back, at most `limit + 1` bytes.
   *
   * The extra byte is deliberate: a file that is LONGER than the document it
   * should hold has to fail verification too, and reading exactly `limit`
   * bytes could not tell the two apart.
   */
  byte[] readBack(Path name, int limit) throws IOException {
    ByteBuffer buffer = ByteBuffer.allocate(limit + 1);
    try (SeekableByteChannel channel =
        secure.newByteChannel(name, options(StandardOpenOption.READ))) {
      while (buffer.hasRemaining() && channel.read(buffer) > 0) { }
    }
    buffer.flip();
    byte[] read = new byte[buffer.remaining()];
    buffer.get(read);
    return read;
  }

  boolean exists(Path name) throws IOException {
    try {
      secure.getFileAttributeView(name, BasicFileAttributeView.class, LinkOption.NOFOLLOW_LINKS)
          .readAttributes();
      return true;
    } catch (NoSuchFileException absent) {
      return false;
    }
  }

  boolean isRegularFile(Path name) throws IOException {
    try {
      return secure
          .getFileAttributeView(name, BasicFileAttributeView.class, LinkOption.NOFOLLOW_LINKS)
          .readAttributes()
          .isRegularFile();
    } catch (NoSuchFileException absent) {
      return false;
    }
  }

  /** Renames within this directory, atomically. */
  void move(Path from, Path to) throws IOException {
    secure.move(from, secure, to);
  }

  void deleteIfExists(Path name) throws IOException {
    try {
      secure.deleteFile(name);
    } catch (NoSuchFileException absent) {
      // nothing to delete
    }
  }

  public void close() throws IOException {
    secure.close();
  }
}

/**
 * Acquires a handle on `directory` by descending from the filesystem root, one
 * component at a time, creating what is missing and never following a symbolic
 * link.
 *
 * `directory` must be absolute. Each component is opened RELATIVE TO THE
 * HANDLE ON ITS PARENT, so the only name this resolves is the filesystem root:
 * a component replaced between validation and this call is refused rather than
 * followed, and once the handle exists no name is resolved again.
 *
 * A component that does not exist yet is created through `createChild`, which
 * verifies the creation through the parent's handle - the one operation the
 * NIO API cannot perform handle-relative, made safe by checking the result
 * where it must have landed rather than by trusting the name. This is why the
 * function both creates and opens: splitting them would put a window back
 * between the two.
 *
 * The intermediate handles are closed on the way out; only the handle on
 * `directory` itself is returned, and the caller owns it.
 */
OutputDirectory openOrCreateNoFollowDirectory(Path directory) throws IOException {
  Path filesystemRoot = directory.getRoot();
  if (filesystemRoot == null) {
    throw new IllegalStateException("Output directory is not absolute: " + directory);
  }
  List<OutputDirectory> chain = new ArrayList<>();
  try {
    OutputDirectory current = OutputDirectory.openVerified(filesystemRoot);
    chain.add(current);
    for (Path element : filesystemRoot.relativize(directory)) {
      OutputDirectory child;
      try {
        child = current.openChild(element);
      } catch (NoSuchFileException absent) {
        child = current.createChild(element);
      }
      current = child;
      chain.add(current);
    }
    // The last handle is the caller's; everything above it is closed below.
    return chain.remove(chain.size() - 1);
  } finally {
    for (OutputDirectory intermediate : chain) {
      try {
        intermediate.close();
      } catch (IOException failed) {
        System.out.println("NOTE: could not close the handle on " + intermediate.path + " - "
            + errorMessage(failed));
      }
    }
  }
}

/**
 * Acquires a handle on an EXISTING absolute `directory` by the same descent,
 * creating nothing.
 *
 * A missing component refuses the run. That is what a write root supplied by
 * the environment needs: a temporary directory that does not exist is an
 * environment error, and creating a path the caller named is precisely the
 * write the placement guards exist to prevent.
 */
OutputDirectory openExistingNoFollowDirectory(Path directory) throws IOException {
  Path filesystemRoot = directory.getRoot();
  if (filesystemRoot == null) {
    throw new IllegalStateException("Directory is not absolute: " + directory);
  }
  List<OutputDirectory> chain = new ArrayList<>();
  try {
    OutputDirectory current = OutputDirectory.openVerified(filesystemRoot);
    chain.add(current);
    for (Path element : filesystemRoot.relativize(directory)) {
      current = current.openChild(element);
      chain.add(current);
    }
    // The last handle is the caller's; everything above it is closed below.
    return chain.remove(chain.size() - 1);
  } finally {
    for (OutputDirectory intermediate : chain) {
      try {
        intermediate.close();
      } catch (IOException failed) {
        System.out.println("NOTE: could not close the handle on " + intermediate.path + " - "
            + errorMessage(failed));
      }
    }
  }
}

/* ---------------------------------------------------------------------------
 * ONE CAPTURE AT A TIME PER OUTPUT ROOT.
 *
 * A pid-suffixed temporary keeps two runs from colliding on a FILENAME; it
 * does not serialise the two TRANSACTIONS. Without a lock, two captures over
 * one output root both pass the stale-artefact check (neither has staged
 * anything yet), then interleave their publish moves: each file ends up whole,
 * each run exits 0, and the tree holds documents from two generations with
 * nothing on disk to say so. Because both runs succeed, no `.new`/`.old`
 * leftover is there for the next run to refuse - which is the one way this
 * mixed state would otherwise be detected.
 *
 * So the run holds an exclusive OS-level lock for the whole of validation,
 * staging, publication and cleanup, and a second capture over the same root is
 * REFUSED rather than queued: a capture takes minutes, and a caller who
 * launched two by mistake needs to be told, not made to wait.
 *
 * WHERE THE LOCK LIVES, AND WHY NOT IN THE OUTPUT ROOT. The lock file is kept
 * under the JVM temporary directory, named for the canonical output root, for
 * two reasons. First, the output root is normally the repository checkout, and
 * a lock file there would be an untracked artefact in the deliverable tree.
 * Second, a lock file that is deleted after use is not a lock: a process that
 * opened it before the delete and locked it afterwards would hold a lock on an
 * unlinked inode while the next process locked a fresh one, and both would
 * believe they owned the root. The file is therefore created once and never
 * removed, and it carries NO CONTENT at all - the lock is the file lock, so
 * there is nothing to write and nothing to truncate. The OS releases the lock
 * if a capture is killed, so a leftover file blocks nothing.
 *
 * THE LOCK PATH IS ITSELF A WRITE, SO IT IS GUARDED AS ONE. A predictable name
 * in a world-writable temporary directory is a place another local process can
 * plant a symbolic link, and an open that followed it would point this run's
 * file operations at whatever it named (CWE-22, CWE-59). SO THE LOCK PATH
 * CREATES NO DIRECTORY: the one file it needs is created relative to a
 * directory handle, and the directory it goes in must already exist.
 *
 *  1. `java.io.tmpdir` must be set, ABSOLUTE and already present. A relative
 *     value resolves against the working directory - normally the checkout -
 *     so it is refused rather than interpreted, and a missing directory is an
 *     environment error rather than something this run creates;
 *  2. neither the declared temporary directory NOR its canonical form may be a
 *     checkout root or sit inside one, so `-Djava.io.tmpdir=<checkout>/modules`
 *     is refused before anything is opened and the repository boundary
 *     (`git status --porcelain -- modules examples eclipse pom.xml src .github`
 *     staying empty) cannot be broken through the lock path. The canonical form
 *     is the one used: unlike `parity.out.dir`, a non-canonical spelling is
 *     RESOLVED rather than refused, because no document is written here and the
 *     platform's own default (`/var/folders/...` on macOS, reached through the
 *     `/var` link) would otherwise refuse every run. Both forms are checked, so
 *     neither a declared nor a resolved temporary directory reaches a checkout;
 *  3. every component of that canonical path is opened RELATIVE TO ITS PARENT'S
 *     HANDLE with NOFOLLOW (`openExistingNoFollowDirectory`), so a symbolic
 *     link anywhere in it - not only at the last component - refuses the run
 *     instead of redirecting the write. Nothing on that path is created: a
 *     component that is absent, or that cannot be opened as a directory without
 *     following a link, stops the run;
 *  4. the lock file is CREATED THROUGH THAT HANDLE, never by pathname, with
 *     owner-only permissions. That is the whole reason no private lock
 *     DIRECTORY is made first: Java has no handle-relative `mkdir` - a
 *     `SecureDirectoryStream` offers `newByteChannel`, `newDirectoryStream`,
 *     `move` and the two deletes, and nothing that creates a directory - so a
 *     directory could only be made by naming its path, and a parent renamed
 *     between the validation and that call would leave a directory created
 *     somewhere else. That is precisely the write these guards exist to
 *     prevent, and a file needs no such call, so none is made;
 *  5. whatever is at the lock name must then be THIS RUN'S OWN regular file,
 *     writable by nobody else. A directory, a symbolic link (refused by the
 *     no-follow open itself), a foreign-owned file and a group- or
 *     other-writable file each stop the run rather than being locked, because a
 *     file another account controls can be replaced under this run - which
 *     would leave two captures each believing they held the output root.
 *     Ownership comes from the OS and not from configuration: a probe file
 *     created through the same handle supplies this process's effective
 *     identity, so the check cannot be defeated with `-Duser.name=...`.
 *
 * Declared after the handle machinery it uses: JShell resolves a type only
 * once its declaration has been read, so the lock code - which operates
 * through an `OutputDirectory` - follows that class rather than preceding it.
 *
 * The key is the DECLARED absolute, normalised root rather than the
 * canonicalised one, because the lock has to be held before `outputRoot`
 * creates anything. That is not a weaker key: `outputRoot` REFUSES any root
 * whose canonical form differs from its declared form, so every run that gets
 * past it had declared == canonical, and two spellings of one root cannot both
 * proceed.
 * ------------------------------------------------------------------------- */

Path OUTPUT_LOCK_PATH = null;
FileChannel OUTPUT_LOCK_CHANNEL = null;
FileLock OUTPUT_LOCK = null;

/** Lower-case hex of the SHA-256 of `text`; the lock file name is built from it. */
String sha256Hex(String text) {
  try {
    byte[] digest = MessageDigest.getInstance("SHA-256")
        .digest(text.getBytes(StandardCharsets.UTF_8));
    StringBuilder hex = new StringBuilder(digest.length * 2);
    for (byte b : digest) {
      hex.append(Character.forDigit((b >> 4) & 0xf, 16)).append(Character.forDigit(b & 0xf, 16));
    }
    return hex.toString();
  } catch (NoSuchAlgorithmException impossible) {
    // Every JDK is required to provide SHA-256; a JVM without it cannot be
    // reasoned about, so this is fatal rather than degraded.
    throw new IllegalStateException("this JVM provides no SHA-256 digest", impossible);
  }
}

/**
 * Rejects a lock root that IS a checkout root or sits inside one.
 *
 * Stricter than `requireOutputRootPlacement`, which permits the checkout root
 * itself because that is where the documents belong. A lock file belongs in no
 * checkout at all: it is not a deliverable, and an untracked file in the tree
 * is what the repository-boundary check exists to catch. Like that function it
 * asks only whether a directory carries the two checkout markers, so it is
 * safe on a path no component of which exists.
 */
void requireLockRootOutsideCheckout(Path root, String configured) {
  for (Path cursor = root; cursor != null; cursor = cursor.getParent()) {
    if (isCheckoutRoot(cursor)) {
      throw new IllegalStateException("Refusing to keep the capture lock inside the checkout"
          + " rooted at " + cursor + ": java.io.tmpdir=" + configured + " gives " + root
          + ". Point -R-Djava.io.tmpdir at a directory outside every checkout.");
    }
  }
}

/**
 * Requires whatever is at `lockName` to be this run's own regular file, writable
 * by nobody else - requirement 5 of the banner above - read through the handle
 * on the directory that holds it.
 *
 * OWNERSHIP COMES FROM THE OS, NOT FROM CONFIGURATION. `user.name` is a system
 * property the same caller who sets `java.io.tmpdir` can set, so comparing an
 * owner against it proves nothing. A probe file created through this handle is
 * used instead, and the owner the kernel gave it - this process's effective
 * identity - is what the lock file's owner is compared with. The probe is
 * removed whatever the outcome, and is also the writability test, so a
 * directory this run cannot write refuses here with a message about the
 * directory.
 */
void requireLockFileIsOurs(OutputDirectory lockRoot, Path lockName) throws IOException {
  Path lockFile = lockRoot.path.resolve(lockName);
  if (!lockRoot.isRegularFile(lockName)) {
    throw new IllegalStateException("Refusing to lock " + lockFile + ": what is at that name is"
        + " not a regular file. Remove it and re-run.");
  }
  Path probe = Paths.get(".parity-capture-owner-probe-" + ProcessHandle.current().pid());
  try {
    lockRoot.writeNew(probe, new byte[0]);
  } catch (IOException refused) {
    throw new IllegalStateException("Refusing to lock " + lockFile + ": this run cannot create a"
        + " file in " + lockRoot.path + " (" + errorMessage(refused) + "), so its own identity"
        + " there cannot be established. Point -R-Djava.io.tmpdir at a directory you own.",
        refused);
  }
  UserPrincipal ours;
  try {
    ours = lockRoot.ownerOf(probe);
  } finally {
    lockRoot.deleteIfExists(probe);
  }
  UserPrincipal owner = lockRoot.ownerOf(lockName);
  if (!owner.equals(ours)) {
    throw new IllegalStateException("Refusing to lock " + lockFile + ": it is owned by "
        + owner.getName() + " rather than by this run's own " + ours.getName() + ", so another"
        + " account controls the file this capture would serialise on. Remove it and re-run.");
  }
  Set<PosixFilePermission> permissions = lockRoot.posixPermissionsOf(lockName);
  if (permissions != null && (permissions.contains(PosixFilePermission.GROUP_WRITE)
      || permissions.contains(PosixFilePermission.OTHERS_WRITE))) {
    throw new IllegalStateException("Refusing to lock " + lockFile + ": it is writable by others ("
        + PosixFilePermissions.toString(permissions) + "), so another account could replace the"
        + " file this capture serialises on. chmod 600 it and re-run.");
  }
}

/**
 * Opens the existing temporary directory the lock file lives in, applying
 * requirements 1 to 3 of the banner above, and returns the HANDLE on it.
 *
 * Creates nothing. The handle is the caller's, and the lock file is created and
 * opened through it; closing it afterwards does not disturb the channel that
 * holds the lock, because that channel carries its own descriptor on the file
 * rather than on the directory.
 */
OutputDirectory openLockRoot() throws IOException {
  String configured = System.getProperty("java.io.tmpdir");
  if (configured == null || configured.isBlank()) {
    throw new IllegalStateException("java.io.tmpdir is not set, so there is nowhere outside the"
        + " checkout to keep the capture lock. Pass -R-Djava.io.tmpdir=<an absolute existing"
        + " directory outside every checkout> and re-run.");
  }
  Path declared = Paths.get(configured);
  if (!declared.isAbsolute()) {
    throw new IllegalStateException("Refusing a relative java.io.tmpdir for the capture lock: "
        + configured + " would resolve against the working directory, which is normally the"
        + " checkout. Pass an absolute path.");
  }
  declared = declared.normalize();
  // Requirement 2 on the DECLARED path, before any component is opened, so a
  // refusal leaves no footprint in a tree this script must not write into.
  requireLockRootOutsideCheckout(declared, configured);
  Path canonical;
  try {
    canonical = declared.toRealPath();
  } catch (IOException unresolvable) {
    throw new IllegalStateException("Refusing to create java.io.tmpdir for the capture lock: "
        + declared + " does not exist or cannot be resolved (" + errorMessage(unresolvable)
        + "). Point -R-Djava.io.tmpdir at an existing absolute directory outside every checkout.",
        unresolvable);
  }
  // Requirement 2 again on the canonical path: canonicalisation can move a path
  // into a checkout, and the canonical path is the one the lock file goes in.
  requireLockRootOutsideCheckout(canonical, configured);
  try {
    return openExistingNoFollowDirectory(canonical);
  } catch (IOException notUsable) {
    throw new IllegalStateException("Refusing to keep the capture lock in " + canonical + ": it"
        + " cannot be opened as a directory without following a link (" + errorMessage(notUsable)
        + "). A symbolic link or a file on that path stops the run rather than being written"
        + " through. Point -R-Djava.io.tmpdir at a real directory outside every checkout.",
        notUsable);
  }
}

/**
 * Takes the exclusive lock for the output root, or fails saying who holds it.
 *
 * FAIL-CLOSED in both directions: a lock already held by another capture and a
 * lock that cannot be created at all both stop the run. The second case
 * matters - a capture that silently proceeded unlocked because the temporary
 * directory was unwritable would be exactly the unserialised run this guards
 * against, so the message names the file and the root instead.
 *
 * Nothing is ever written to or truncated in the lock file: the lock is the
 * file lock. That is deliberate, so that an operation on this path can never
 * destroy anything even if the guards above were somehow bypassed. The file is
 * created and opened relative to the handle on the directory that was
 * validated, so nothing on this path is reached by name.
 */
void acquireOutputLock() throws IOException {
  Path root = Paths.get(OUT_ROOT).toAbsolutePath().normalize();
  Path lockName = Paths.get("parity-capture-baseline-" + sha256Hex(root.toString()) + ".lock");
  Path lockFile;
  FileChannel channel;
  try (OutputDirectory lockRoot = openLockRoot()) {
    lockFile = lockRoot.path.resolve(lockName);
    try {
      channel = lockRoot.openOrCreatePrivateChannel(lockName);
    } catch (IOException failed) {
      throw new IllegalStateException("cannot create the capture lock " + lockFile + " for output"
          + " root " + root + " - the capture will not run unserialised: " + errorMessage(failed),
          failed);
    }
    boolean keep = false;
    try {
      requireLockFileIsOurs(lockRoot, lockName);
      keep = true;
    } finally {
      if (!keep) {
        closeQuietly(channel, lockFile);
      }
    }
  }
  FileLock lock;
  String contention;
  try {
    lock = channel.tryLock();
    contention = "another capture holds the exclusive lock " + lockFile;
  } catch (OverlappingFileLockException alreadyHeldHere) {
    // Not another process: this JVM holds it, which can only mean the driver
    // was entered twice. Reported distinctly, because the remedy differs.
    lock = null;
    contention = "this JVM already holds " + lockFile + ", so the capture driver ran twice";
  } catch (IOException failed) {
    closeQuietly(channel, lockFile);
    throw new IllegalStateException("cannot lock " + lockFile + " for output root " + root + ": "
        + errorMessage(failed), failed);
  }
  if (lock == null) {
    closeQuietly(channel, lockFile);
    throw new IllegalStateException("another capture is writing to " + root + " (" + contention
        + "). Two captures over one output root can interleave their publish moves and leave a"
        + " mixed generation, so this run stops. Wait for the other capture to finish, or pass a"
        + " different -Dparity.out.dir.");
  }
  OUTPUT_LOCK_PATH = lockFile;
  OUTPUT_LOCK_CHANNEL = channel;
  OUTPUT_LOCK = lock;
  System.out.println("  output lock     = " + lockFile);
}

/** Closes a channel without masking the failure that is already being reported. */
void closeQuietly(FileChannel channel, Path what) {
  try {
    channel.close();
  } catch (IOException failed) {
    System.out.println("NOTE: could not close " + what + " - " + errorMessage(failed));
  }
}

/**
 * Releases the lock. Idempotent, and never throws: it runs in the `finally` of
 * the driver, where a failure of its own would hide the failure being reported.
 */
void releaseOutputLock() {
  if (OUTPUT_LOCK != null) {
    try {
      OUTPUT_LOCK.release();
    } catch (IOException failed) {
      System.out.println("NOTE: could not release the capture lock " + OUTPUT_LOCK_PATH + " - "
          + errorMessage(failed));
    }
    OUTPUT_LOCK = null;
  }
  if (OUTPUT_LOCK_CHANNEL != null) {
    closeQuietly(OUTPUT_LOCK_CHANNEL, OUTPUT_LOCK_PATH);
    OUTPUT_LOCK_CHANNEL = null;
  }
}

/**
 * One document's place in the publication transaction.
 *
 * The three flags are the journal. They are set as each step COMPLETES, so
 * rollback knows exactly which steps happened - in particular `backedUp` is
 * set the moment the previous generation has been moved aside, before the new
 * file is moved in, because the window between those two moves is precisely
 * where a failure would otherwise strand the previous generation under a
 * `.old` name with no record that it was there.
 */
final class FlushEntry {
  final String relativePath;
  final OutputDirectory directory;
  final Path fileName;
  final Path temporaryName;
  final Path backupName;
  final byte[] bytes;
  boolean staged;
  boolean backedUp;
  boolean published;

  FlushEntry(String relativePath, OutputDirectory directory, Path fileName, byte[] bytes) {
    this.relativePath = relativePath;
    this.directory = directory;
    this.fileName = fileName;
    this.temporaryName = Paths.get(fileName.toString() + FLUSH_TEMP_SUFFIX + ".new");
    this.backupName = Paths.get(fileName.toString() + FLUSH_TEMP_SUFFIX + ".old");
    this.bytes = bytes;
  }

  Path target() {
    return directory.path.resolve(fileName);
  }
}

/**
 * Refuses to publish into a directory that still holds an interrupted run's
 * artefacts.
 *
 * A `.new` file is a document that was staged and never published; a `.old`
 * file is a previous generation that was moved aside and never deleted. Either
 * one means a capture was killed between two moves, so the tree may hold a
 * MIXED generation - some files new, some old - and that is the one state this
 * transaction cannot detect from the published files alone. Overwriting it
 * would erase the evidence and the operator's only copy of the previous
 * generation, so the run stops and says what is there instead.
 *
 * The scan runs through the directory's own HANDLE rather than its name, so it
 * reads the directory that will be published into - a name-based scan could
 * report a clean directory and then publish into a different one.
 */
void requireNoStaleFlushArtefacts(OutputDirectory directory) throws IOException {
  List<String> stale = directory.namesContaining(FLUSH_ARTEFACT_MARKER);
  if (!stale.isEmpty()) {
    throw new IllegalStateException("Refusing to publish into " + directory.path
        + ": a previous capture left " + stale
        + ". A `.new` file was staged but never published and can be deleted; a `.old` file IS the"
        + " previous generation of the target named before the suffix and must be moved back onto"
        + " it (or deleted, if the current file is the one you want). Resolve it, then re-run.");
  }
}

/**
 * Undoes a partial publication: every target already replaced goes back to the
 * bytes it had, and every temporary is removed.
 *
 * FAIL-CLOSED. Each step is attempted, every failure is printed with both
 * paths - that is the one case a human has to finish by hand - and the method
 * then THROWS, because a rollback that only logs turns "all or nothing" into
 * "nothing, probably", and the run would report success on a tree nobody
 * checked.
 */
void rollbackFlush(List<FlushEntry> entries) {
  List<String> failures = new ArrayList<>();
  for (int i = entries.size() - 1; i >= 0; i--) {
    FlushEntry entry = entries.get(i);
    try {
      if (entry.published) {
        entry.directory.deleteIfExists(entry.fileName);
        entry.published = false;
      }
      if (entry.backedUp) {
        entry.directory.move(entry.backupName, entry.fileName);
        entry.backedUp = false;
      }
      if (entry.staged) {
        entry.directory.deleteIfExists(entry.temporaryName);
        entry.staged = false;
      }
    } catch (Throwable failed) {
      // Deliberately everything: this is the rollback, not a capture handler.
      // Whatever went wrong - an IOException, a SecurityException, an Error
      // from a full disk - the remaining targets still have to be attempted,
      // and the combined failure is thrown at the end.
      String message = entry.target() + " - " + errorMessage(failed);
      failures.add(message);
      System.out.println("ROLLBACK FAILED: could not restore " + message);
      if (entry.backedUp) {
        System.out.println("    the previous generation is still at "
            + entry.directory.path.resolve(entry.backupName));
      }
    }
  }
  if (!failures.isEmpty()) {
    throw new IllegalStateException("rollback incomplete, " + failures.size()
        + " target(s) need manual attention: " + failures);
  }
}

/**
 * Publishes the staged documents as ONE GENERATION, or leaves every file as it
 * was.
 *
 * `Files.writeString` straight onto each target cannot do that: it truncates
 * before it writes, so an I/O error or an interrupt half way through the seven
 * files leaves earlier files new, later files old, and the file being written
 * truncated - a tree that looks like a baseline and is not one.
 *
 * The phases below are the transaction, and phase 3 ends at the COMMIT
 * BOUNDARY:
 *
 *  1. validate all seven destinations before a single byte is written, create
 *     their parents one link-checked component at a time, take a no-follow
 *     handle on each directory by descending to it, and refuse to proceed if
 *     an earlier run left artefacts there;
 *  2. write each document to `<target>.capture-tmp-<pid>.new` through that
 *     handle with CREATE_NEW (so an existing file or link at that name is
 *     never followed or reused), then verify it by reading the bytes back
 *     through the same handle and comparing them;
 *  3. publish: move any existing target aside to `.old` - RECORDING that
 *     before the next move - then move the verified temporary into place. Both
 *     moves are renames within one directory, so both are atomic, and a reader
 *     sees either the whole old file or the whole new one, never a truncated
 *     one. Any failure in 2 or 3 rolls back to the previous generation and
 *     rethrows, so the driver records a failure and the exit status is
 *     non-zero;
 *  ---- the commit boundary: after 3, the new generation IS the generation ----
 *  4. with every target published, delete the `.old` backups. THIS CANNOT ROLL
 *     BACK, and the structure is what guarantees it: phase 4 sits outside the
 *     block whose `catch` calls `rollbackFlush`, because phase 4 CLEARS the
 *     `backedUp` journal flag as each backup is deleted. A rollback entered
 *     from here would find the flag already cleared for the backups it had
 *     removed, delete the published target it was meant to restore, and have
 *     nothing to put back - it would destroy the very generation the run had
 *     just verified. So a cleanup failure keeps the published documents,
 *     names every backup it could not remove, and fails the run without
 *     touching a published target; the next run refuses to publish until
 *     those leftovers are resolved.
 *
 * WHAT THIS DOES NOT CLAIM. Seven files in three directories cannot be
 * replaced in one filesystem operation: a directory swap is the only primitive
 * that would, and it is unavailable here because the three directories are
 * separate trees in two sbt modules - no single swap reaches all of them - and
 * because a swap replaces a directory WHOLE, so it would delete whatever else
 * those directories come to hold. So a process KILLED
 * between two of the publish moves - SIGKILL, a power loss - leaves a mixed
 * generation on disk. What the transaction guarantees is that such a state is
 * never silent: the interrupted run's `.old` and `.new` files stay where they
 * are, phase 1 of the NEXT run refuses to publish over them and names them,
 * and because the fixtures are tracked, `git status` shows the same thing.
 */
void flushDocuments() throws Exception {
  List<FlushEntry> entries = new ArrayList<>();
  Map<Path, OutputDirectory> handles = new LinkedHashMap<>();
  try {
    // Phase 1: validate, descend to each directory creating what is missing,
    // and refuse stale artefacts.
    for (Map.Entry<String, String> document : PENDING_DOCUMENTS.entrySet()) {
      Path target = guardedOutputTarget(document.getKey());
      Path directory = target.getParent();
      OutputDirectory handle = handles.get(directory);
      if (handle == null) {
        // Descended to and created on the way, never opened by name: see
        // `openOrCreateNoFollowDirectory`.
        handle = openOrCreateNoFollowDirectory(directory);
        handles.put(directory, handle);
        requireNoStaleFlushArtefacts(handle);
      }
      if (handle.exists(target.getFileName()) && !handle.isRegularFile(target.getFileName())) {
        throw new IllegalStateException("Output target is not a regular file: " + target);
      }
      entries.add(new FlushEntry(document.getKey(), handle, target.getFileName(),
          document.getValue().getBytes(StandardCharsets.UTF_8)));
    }
    try {
      // Phase 2: write and verify every temporary.
      for (FlushEntry entry : entries) {
        entry.directory.writeNew(entry.temporaryName, entry.bytes);
        entry.staged = true;
        byte[] readBack = entry.directory.readBack(entry.temporaryName, entry.bytes.length);
        if (!Arrays.equals(readBack, entry.bytes)) {
          throw new IOException("staged document does not match what was written: "
              + entry.directory.path.resolve(entry.temporaryName) + " (" + readBack.length
              + " bytes read back, " + entry.bytes.length + " written)");
        }
      }
      // Phase 3: publish, keeping the previous generation aside until all are in place.
      for (FlushEntry entry : entries) {
        if (entry.directory.exists(entry.fileName)) {
          entry.directory.deleteIfExists(entry.backupName);
          entry.directory.move(entry.fileName, entry.backupName);
          // Recorded HERE, between the two moves: if the next move fails, the
          // previous generation is under `backupName` and rollback knows it.
          entry.backedUp = true;
        }
        entry.directory.move(entry.temporaryName, entry.fileName);
        entry.staged = false;
        entry.published = true;
        System.out.println("wrote " + entry.target() + "  (" + entry.bytes.length + " bytes)");
      }
    } catch (Throwable thrown) {
      // The rollback arm, and it guards phases 2 and 3 ONLY. Deliberately
      // everything: a publication that fails half way through must be undone
      // whatever failed it, and the original failure is rethrown afterwards so
      // the driver reports it and the exit status is non-zero.
      try {
        rollbackFlush(entries);
      } catch (Throwable rollbackFailed) {
        // A rollback that could not finish is louder than the failure that
        // caused it: it is printed here AND attached to the original, because
        // it is the case that needs a human.
        System.out.println("ROLLBACK INCOMPLETE: " + errorMessage(rollbackFailed));
        thrown.addSuppressed(rollbackFailed);
      }
      throw thrown;
    }
    // ----------------------------- COMMIT -----------------------------------
    // Every document is published and every one of them was verified before it
    // moved. From here the new generation is the generation, and NOTHING below
    // may roll back: phase 4 is deliberately outside the block above, because
    // rolling back from here would delete published targets whose backups this
    // phase had already removed. See the phase 4 note in the banner.
    List<String> undeleted = new ArrayList<>();
    for (FlushEntry entry : entries) {
      if (entry.backedUp) {
        try {
          entry.directory.deleteIfExists(entry.backupName);
          entry.backedUp = false;
        } catch (IOException failed) {
          undeleted.add(entry.directory.path.resolve(entry.backupName) + " - "
              + errorMessage(failed));
        }
      }
    }
    if (!undeleted.isEmpty()) {
      // The published generation stays exactly as it is - complete and
      // verified. What is wrong is only that the PREVIOUS generation is still
      // beside it under `.old` names, which the next run will refuse to
      // publish over, so the operator has to hear about it and the run fails.
      System.out.println("PUBLISHED, BUT NOT CLEANED UP: every document was written and is"
          + " intact; the previous generation could not be removed and is still on disk.");
      for (String leftover : undeleted) {
        System.out.println("    " + leftover);
      }
      throw new IllegalStateException("every document was published and left in place, but "
          + undeleted.size() + " backup(s) of the previous generation could not be removed: "
          + undeleted + ". Delete them by hand - the published documents are the new baseline"
          + " and must NOT be reverted - then the next capture will run.");
    }
  } finally {
    for (OutputDirectory handle : handles.values()) {
      try {
        handle.close();
      } catch (IOException failed) {
        System.out.println("NOTE: could not close the handle on " + handle.path + " - "
            + errorMessage(failed));
      }
    }
  }
}

/* ===========================================================================
 * SECTION 4 - FIXTURE HELPERS OVER THE PUBLIC API
 *
 * The date, list, holiday and `ScheduleInfo` helpers that the data tables of
 * Section 5 and the emitters of Sections 7 to 13 are written in terms of.
 *
 * A `.jsh` script runs in the UNNAMED PACKAGE, so nothing package-private is
 * reachable from here - neither `com.opengamma.strata.collect.TestHelper`,
 * which lives in a test jar, nor a Java test's own package-private stub. Every
 * helper below is built from public API, and the ones whose behaviour differs
 * from the obvious reading of their signature say so at their declaration.
 * ===========================================================================
 */

LocalDate date(int year, int month, int day) {
  return LocalDate.of(year, month, day);
}

LocalDate date(int year, Month month, int day) {
  return LocalDate.of(year, month, day);
}

List<LocalDate> list(LocalDate... dates) {
  List<LocalDate> result = new ArrayList<>();
  for (LocalDate date : dates) {
    result.add(date);
  }
  return result;
}

/*
 * `md` names a month and a day; `mds` resolves a list of them into the dates of
 * one year, which is the form the expected-holiday tables of Section 5 are
 * written in (GlobalHolidayCalendarsTest.java:1200-1210).
 */
MonthDay md(int month, int day) {
  return MonthDay.of(month, day);
}

List<LocalDate> mds(int year, MonthDay... monthDays) {
  List<LocalDate> holidays = new ArrayList<>();
  for (MonthDay md : monthDays) {
    holidays.add(md.atYear(year));
  }
  return holidays;
}

/*
 * The SIMPLE_30_360 sentinel (Trap 2).
 *
 * DayCountTest declares `SIMPLE_30_360 = Double.NaN` and
 * `SIMPLE_30_360DAYS = 0` as MARKERS, not as expected values, and resolves them
 * through calc360 / calc360Days. A row taken literally would record NaN or 0 as
 * its expectation, which is exactly the silent corruption the self-checks exist
 * to prevent.
 *
 * The two markers are compared in two different ways, matching the comparisons
 * DayCountTest itself makes:
 *   * `value == SIMPLE_30_360` compares boxed Double REFERENCES. The constant
 *     is declared once, so every table literal shares that one autoboxed
 *     instance and reference identity selects exactly the sentinel rows
 *     (measured: 59 of the 201 data_yearFraction rows).
 *   * `value == SIMPLE_30_360DAYS` compares int VALUES, so a row whose expected
 *     day count is literally 0 is routed through calc360Days as well.
 */
final Double SIMPLE_30_360 = Double.NaN;
final int SIMPLE_30_360DAYS = 0;

double calc360(int y1, int m1, int d1, int y2, int m2, int d2) {
  return ((y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)) / 360d;
}

int calc360Days(int y1, int m1, int d1, int y2, int m2, int d2) {
  return (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1);
}

/**
 * An implementation of the PUBLIC nested interface DayCount.ScheduleInfo whose
 * accessors are nullable and non-throwing, matching the stub DayCountTest
 * declares rather than the interface defaults (Trap 7).
 *
 * The distinction is load-bearing. The interface's own defaults THROW
 * UnsupportedOperationException for getStartDate / getEndDate /
 * getPeriodEndDate / getFrequency and default isEndOfMonthConvention to true;
 * DayCountTest's stub returns NULL from every accessor and answers
 * getPeriodEndDate(date) with its fixed `periodEnd`, IGNORING the argument.
 *
 * That is what the fixture's `scheduleInfo` object encodes: a nullable field
 * per accessor plus a single fixed `periodEnd`, where a `null` field states
 * that no schedule information is supplied and the interface default therefore
 * applies to the evaluation the row records.
 */
class Info implements DayCount.ScheduleInfo {
  private final LocalDate start;
  private final LocalDate end;
  private final LocalDate periodEnd;
  private final boolean eom;
  private final Frequency frequency;

  Info(boolean eom) {
    this(null, null, null, eom, null);
  }

  Info(LocalDate start, LocalDate end, LocalDate periodEnd, boolean eom, Frequency frequency) {
    this.start = start;
    this.end = end;
    this.periodEnd = periodEnd;
    this.eom = eom;
    this.frequency = frequency;
  }

  public boolean isEndOfMonthConvention() {
    return eom;
  }

  public Frequency getFrequency() {
    return frequency;
  }

  public LocalDate getStartDate() {
    return start;
  }

  public LocalDate getEndDate() {
    return end;
  }

  /** Returns the fixed period end, ignoring the argument, exactly as the stub does. */
  public LocalDate getPeriodEndDate(LocalDate date) {
    return periodEnd;
  }

  LocalDate rawStart() {
    return start;
  }

  LocalDate rawEnd() {
    return end;
  }

  LocalDate rawPeriodEnd() {
    return periodEnd;
  }

  boolean rawEom() {
    return eom;
  }

  Frequency rawFrequency() {
    return frequency;
  }
}

/**
 * Renders an `Info` as the fixture's `scheduleInfo` object: the four nullable
 * fields plus the fixed `periodEnd`. The alternative `periodEnds` shape, used
 * for rows evaluated against a real Schedule, is produced by
 * scheduleInfoOfSchedule below. A row always carries exactly one of the two.
 *
 * A null `Info` means the row used the two-argument overload, so the schedule
 * information is Java's own default. That is rendered as the same object with
 * every field null - NOT as JSON null - so `scheduleInfo` has one type across
 * the document and `scheduleInfo.periodEnd == null` says "the default" in the
 * same way for every row.
 */
Jn jScheduleInfo(Info info) {
  if (info == null) {
    return new JObject()
        .set("start", jNull())
        .set("end", jNull())
        .set("frequency", jNull())
        .set("eom", jNull())
        .set("periodEnd", jNull());
  }
  return new JObject()
      .set("start", jDate(info.rawStart()))
      .set("end", jDate(info.rawEnd()))
      .set("frequency", jName(info.rawFrequency()))
      .set("eom", jBool(info.rawEom()))
      .set("periodEnd", jDate(info.rawPeriodEnd()));
}

/**
 * Renders a real Schedule as the fixture's `scheduleInfo` object using the
 * `periodEnds` shape.
 *
 * `periodEnds` is the sorted list of the ADJUSTED end dates of every period.
 * Schedule.getPeriodEndDate(d) answers with the end date of the first period
 * that contains d, comparing against the adjusted start and end dates with the
 * start included and the end excluded; for the contiguous periods of a schedule
 * that is the first boundary strictly after d when start <= d < end. Outside
 * that range Java throws, and the encoded list has no boundary to offer, so a
 * consumer reading it has no period end for such a date.
 */
Jn scheduleInfoOfSchedule(Schedule schedule) {
  JArray periodEnds = new JArray();
  for (SchedulePeriod period : schedule.getPeriods()) {
    periodEnds.add(jDate(period.getEndDate()));
  }
  return new JObject()
      .set("start", jDate(schedule.getStartDate()))
      .set("end", jDate(schedule.getEndDate()))
      .set("frequency", jName(schedule.getFrequency()))
      .set("eom", jBool(schedule.isEndOfMonthConvention()))
      .set("periodEnds", periodEnds);
}

/** Unwraps a Throwable to the message the fixture records in an `error` row. */
String errorMessage(Throwable thrown) {
  String message = thrown.getMessage();
  return thrown.getClass().getSimpleName() + ": " + (message == null ? "" : message);
}

/**
 * THE LINE BETWEEN AN EXPECTATION AND A BROKEN CAPTURE.
 *
 * A capture helper that catches everything cannot tell the two apart. The Java
 * APIs measured here reject an input they cannot serve with one of a small,
 * closed set of runtime exceptions - IllegalArgumentException (and its Strata
 * subtypes ScheduleException, ParseFailureException, IllegalArgFailureException),
 * IllegalStateException, UnsupportedOperationException,
 * IndexOutOfBoundsException, ArithmeticException, DateTimeException - or with a
 * runtime exception Strata declares itself, such as
 * ReferenceDataNotFoundException. Each of those IS the expectation the fixture
 * records - the row states that this input is rejected, and a consumer that
 * accepts it fails the comparison.
 *
 * Everything else means the measurement itself failed, and there are two
 * families of it. A LinkageError or NoClassDefFoundError says the classpath is
 * wrong; an ExceptionInInitializerError says a Strata class could not
 * initialise; an OutOfMemoryError or StackOverflowError says the run is broken.
 * A NullPointerException, ClassCastException, ArrayStoreException or
 * ConcurrentModificationException says either that the measured code has a
 * defect or that this script called it wrongly - a domain API answers "I
 * cannot serve this input" with a rejection, never by dereferencing null.
 * Serialising any of those as the expected behaviour of a day count would bake
 * a broken environment, or a bug, into the baseline and still exit 0.
 *
 * So this method is called first in every capture handler: a domain rejection
 * returns and is recorded, and everything else is rethrown - an Error
 * unchanged, anything else wrapped in an IllegalStateException that names the
 * type - so it reaches the driver, which records a `driver` failure and exits
 * non-zero. A checked exception lands in the second group too: no measured API
 * declares one, so its appearance is itself a defect.
 *
 * The allow-list is closed on purpose, and it is wide enough for the behaviour
 * that exists: the seven JSON documents this script writes contain exactly
 * six distinct throwable types - UnsupportedOperationException (1028 occurrences),
 * ScheduleException (565), IllegalArgumentException (103),
 * IllegalStateException (2), IndexOutOfBoundsException (1) and
 * ArrayIndexOutOfBoundsException (1) - every one of them on the list. Widening
 * it is therefore never needed to capture today's behaviour, and a type that
 * is not on it is news.
 */
List<Class<? extends Throwable>> CAPTURABLE_JDK_FAILURES = List.of(
    IllegalArgumentException.class,
    IllegalStateException.class,
    UnsupportedOperationException.class,
    IndexOutOfBoundsException.class,
    ArithmeticException.class,
    DateTimeException.class);

/**
 * True when `thrown` is a rejection a measured API is allowed to answer with.
 *
 * Strata's own runtime exceptions are domain rejections by construction - they
 * exist to say "this input cannot be served" - so they are accepted by package
 * rather than enumerated one at a time, which keeps this predicate from going
 * stale against a Strata version that introduces one. ScheduleException,
 * ParseFailureException and IllegalArgFailureException are already covered as
 * IllegalArgumentException subtypes; ReferenceDataNotFoundException, which
 * extends RuntimeException directly, is covered by the package rule and is
 * thrown by the manifest's calendar-resolvability probe.
 */
boolean isCapturableFailure(Throwable thrown) {
  if (!(thrown instanceof RuntimeException)) {
    return false;
  }
  for (Class<? extends Throwable> allowed : CAPTURABLE_JDK_FAILURES) {
    if (allowed.isInstance(thrown)) {
      return true;
    }
  }
  return thrown.getClass().getName().startsWith("com.opengamma.strata.");
}

void requireCapturable(Throwable thrown) {
  if (thrown instanceof Error) {
    throw (Error) thrown;
  }
  if (isCapturableFailure(thrown)) {
    return;
  }
  throw new IllegalStateException(
      "capture aborted: a measured call threw " + thrown.getClass().getName()
          + ", which is not a rejection a measured API may answer with: " + errorMessage(thrown),
      thrown);
}

/**
 * The weaker form, for the handful of places that probe with reflection.
 *
 * A reflective lookup legitimately throws CHECKED exceptions - the class or
 * the method may be absent - and those are recorded as check failures with a
 * useful message. An `Error` still is not: it says the JVM or the classpath is
 * broken, so it goes to the driver rather than being reported as "the date
 * rules are unreachable".
 */
void rethrowIfError(Throwable thrown) {
  // A reflective invocation wraps whatever the target threw, so the wrapper is
  // unwrapped before the test - otherwise an Error raised inside the invoked
  // method would arrive as a checked InvocationTargetException and be reported
  // as a check failure.
  Throwable cause = thrown instanceof InvocationTargetException ? thrown.getCause() : thrown;
  if (cause instanceof Error) {
    throw (Error) cause;
  }
}

/**
 * WHAT A JAVA TEST SAYS ABOUT ONE MEASURED CALL.
 *
 * `requireCapturable` bounds what a measured call may throw. This bounds what
 * it may throw HERE, on this row, which is the difference between a fixture
 * that records behaviour and a fixture that asserts it. Three states:
 *
 *   MUST_SUCCEED - the call has to produce a value. A rejection aborts the
 *       capture, however well-formed that rejection is.
 *   failsWith(T) - the call has to be rejected, with a throwable assignable to
 *       T, which is the type the Java test names. A rejection of another type
 *       aborts the capture, and so does a value.
 *   CAPTURE_ONLY - no Java test states this outcome: a generated combination,
 *       a seeded random input. Whatever happens is recorded and the row is
 *       counted as capture-only in the summary, so the count of unasserted
 *       rows is visible rather than implicit.
 *
 * Without the type, an expected failure only asserts that SOMETHING was
 * thrown: a change from IllegalArgumentException to IllegalStateException, or
 * to a NullPointerException from a new bug, would be recaptured as the new
 * truth and the capture would still exit 0. That is the hole this closes.
 *
 * The type is compared by ASSIGNABILITY, not by name, because that is what the
 * Java assertions mean - `assertThatIllegalArgumentException()` and
 * `assertThatExceptionOfType(ScheduleException.class)` are both satisfied by a
 * subtype. Comparing names would make this check stricter than the tests it
 * measures and fail on correct behaviour: DoubleArrayTest names
 * IndexOutOfBoundsException where the implementation throws
 * ArrayIndexOutOfBoundsException.
 */
final class Expect {
  /** The type the Java test names, or null when no failure is expected. */
  final Class<? extends Throwable> failureType;
  /** False only for capture-only rows, where no Java test states the outcome. */
  final boolean stated;

  private Expect(Class<? extends Throwable> failureType, boolean stated) {
    this.failureType = failureType;
    this.stated = stated;
  }

  static Expect success() {
    return new Expect(null, true);
  }

  static Expect captureOnly() {
    return new Expect(null, false);
  }

  static Expect failsWith(Class<? extends Throwable> failureType) {
    return new Expect(failureType, true);
  }

  boolean mustSucceed() {
    return stated && failureType == null;
  }

  boolean mustFail() {
    return failureType != null;
  }
}

/** The call must produce a value; any rejection aborts the capture. */
Expect MUST_SUCCEED = Expect.success();

/** No Java test states this outcome; it is recorded and counted, not asserted. */
Expect CAPTURE_ONLY = Expect.captureOnly();

/**
 * The call must be rejected with an IllegalArgumentException.
 *
 * This is the shape of `assertThatIllegalArgumentException()`, which is how
 * every ArgChecker-guarded rejection in the measured API is asserted - and it
 * accepts the Strata subtypes (ScheduleException, ParseFailureException)
 * because a subtype satisfies that assertion.
 */
Expect MUST_REJECT_ARGUMENT = Expect.failsWith(IllegalArgumentException.class);

/**
 * Asserts that an observed rejection is the rejection the Java test names.
 *
 * One check is counted per call, so the summary's check total covers expected
 * failures exactly as it covers expected values.
 */
void checkExpectedFailureType(String fixture, String id, Class<? extends Throwable> expected,
    Throwable thrown) {
  CHECK.checkTrue(fixture, id, expected.isInstance(thrown),
      "expected " + expected.getSimpleName() + " but Java threw " + errorMessage(thrown));
}

/* =========================================================================
 * SECTION 5 - THE JAVA TEST DATA TABLES
 *
 * Each table below is the `public static Object[][] data_*()` body of the Java
 * test its own banner names, so every row remains traceable to a source line
 * there; the banner is the authoritative statement of where the rows come
 * from. The bodies are valid Java array initialisers over the imports, the
 * constants and the date/list helpers declared above.
 *
 * The row counts asserted in Section 6 are the counts measured in those Java
 * sources, so a table edited upstream without this script being updated aborts
 * the capture instead of silently shrinking a fixture.
 * =========================================================================
 */

/* --- The constants the PeriodicScheduleTest tables are written in terms of ---
 * Source: modules/basics/src/test/java/com/opengamma/strata/basics/schedule/PeriodicScheduleTest.java:85-127
 */
final ReferenceData REF_DATA = ReferenceData.standard();
final RollConvention ROLL_NONE = RollConventions.NONE;
final StubConvention STUB_NONE = StubConvention.NONE;
final StubConvention STUB_BOTH = StubConvention.BOTH;
final BusinessDayAdjustment BDA = BusinessDayAdjustment.of(MODIFIED_FOLLOWING, SAT_SUN);
final BusinessDayAdjustment BDA_JPY_MF = BusinessDayAdjustment.of(MODIFIED_FOLLOWING, JPTO);
final BusinessDayAdjustment BDA_JPY_P = BusinessDayAdjustment.of(PRECEDING, JPTO);
final BusinessDayAdjustment BDA_NONE = BusinessDayAdjustment.NONE;
final LocalDate NOV_29_2013 = date(2013, NOVEMBER, 29);  // Fri
final LocalDate NOV_30_2013 = date(2013, NOVEMBER, 30);  // Sat
final LocalDate FEB_28 = date(2014, FEBRUARY, 28); // Fri
final LocalDate APR_01 = date(2014, APRIL, 1); // Tue
final LocalDate MAY_17 = date(2014, MAY, 17);  // Sat
final LocalDate MAY_19 = date(2014, MAY, 19);  // Mon
final LocalDate MAY_30 = date(2014, MAY, 30);  // Fri
final LocalDate MAY_31 = date(2014, MAY, 31);  // Sat
final LocalDate JUN_03 = date(2014, JUNE, 3);  // Tue
final LocalDate JUN_04 = date(2014, JUNE, 4);  // Wed
final LocalDate JUN_10 = date(2014, JUNE, 10);  // Tue
final LocalDate JUN_11 = date(2014, JUNE, 11);  // Wed
final LocalDate JUN_17 = date(2014, JUNE, 17);  // Tue
final LocalDate JUL_04 = date(2014, JULY, 4); // Fri
final LocalDate JUL_11 = date(2014, JULY, 11); // Fri
final LocalDate JUL_17 = date(2014, JULY, 17); // Thu
final LocalDate JUL_30 = date(2014, JULY, 30);  // Wed
final LocalDate AUG_04 = date(2014, AUGUST, 4); // Mon
final LocalDate AUG_11 = date(2014, AUGUST, 11); // Mon
final LocalDate AUG_17 = date(2014, AUGUST, 17); // Sun
final LocalDate AUG_18 = date(2014, AUGUST, 18); // Mon
final LocalDate AUG_29 = date(2014, AUGUST, 29);  // Fri
final LocalDate AUG_30 = date(2014, AUGUST, 30);  // Sat
final LocalDate AUG_31 = date(2014, AUGUST, 31);  // Sun
final LocalDate SEP_04 = date(2014, SEPTEMBER, 4); // Thu
final LocalDate SEP_05 = date(2014, SEPTEMBER, 5); // Fri
final LocalDate SEP_10 = date(2014, SEPTEMBER, 10); // Wed
final LocalDate SEP_11 = date(2014, SEPTEMBER, 11); // Thu
final LocalDate SEP_17 = date(2014, SEPTEMBER, 17); // Wed
final LocalDate SEP_18 = date(2014, SEPTEMBER, 18); // Thu
final LocalDate SEP_30 = date(2014, SEPTEMBER, 30);  // Tue
final LocalDate OCT_17 = date(2014, OCTOBER, 17); // Fri
final LocalDate OCT_30 = date(2014, OCTOBER, 30);  // Thu
final LocalDate NOV_28 = date(2014, NOVEMBER, 28);  // Fri
final LocalDate NOV_30 = date(2014, NOVEMBER, 30);  // Sun

/* --- data_yearFraction: 201 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:132 --- */
Object[][] data_yearFraction() {
  return new Object[][] {
        {ONE_ONE, 2011, 12, 28, 2012, 2, 28, 1d},
        {ONE_ONE, 2011, 12, 28, 2012, 2, 29, 1d},
        {ONE_ONE, 2011, 12, 28, 2012, 3, 1, 1d},
        {ONE_ONE, 2011, 12, 28, 2016, 2, 28, 1d},
        {ONE_ONE, 2011, 12, 28, 2016, 2, 29, 1d},
        {ONE_ONE, 2011, 12, 28, 2016, 3, 1, 1d},
        {ONE_ONE, 2012, 2, 29, 2012, 3, 29, 1d},
        {ONE_ONE, 2012, 2, 29, 2012, 3, 28, 1d},
        {ONE_ONE, 2012, 3, 1, 2012, 3, 28, 1d},

        // ACT_ACT_ISDA
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, (4d / 365d + 58d / 366d)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, (4d / 365d + 59d / 366d)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, (4d / 365d + 60d / 366d)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, (4d / 365d + 58d / 366d + 4)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, (4d / 365d + 59d / 366d + 4)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, (4d / 365d + 60d / 366d + 4)},
        {ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 29, 29d / 366d},
        {ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 28, 28d / 366d},
        {ACT_ACT_ISDA, 2012, 3, 1, 2012, 3, 28, 27d / 366d},

        // ACT_ACT_AFB
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, (63d / 365d)},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, (64d / 366d)},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, (62d / 365d) + 4},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, (63d / 365d) + 4},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, (64d / 366d) + 4},
        {ACT_ACT_AFB, 2012, 2, 28, 2012, 3, 28, 29d / 366d},
        {ACT_ACT_AFB, 2012, 2, 29, 2012, 3, 28, 28d / 366d},
        {ACT_ACT_AFB, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        // ACT_ACT_YEAR
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 28, (62d / 366d)},
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 29, (63d / 366d)},
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 3, 1, (64d / 366d)},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 28, (62d / 366d) + 4},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 29, (63d / 366d) + 4},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 3, 1, (64d / 366d) + 4},
        {ACT_ACT_YEAR, 2012, 2, 28, 2012, 3, 28, 29d / 366d},
        {ACT_ACT_YEAR, 2012, 2, 29, 2012, 3, 28, 28d / 365d},
        {ACT_ACT_YEAR, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        {ACT_ACT_YEAR, 2011, 2, 28, 2011, 3, 2, (2d / 365d)},
        {ACT_ACT_YEAR, 2011, 3, 1, 2011, 3, 2, (1d / 366d)},

        {ACT_ACT_YEAR, 2012, 2, 28, 2016, 3, 2, (3d / 366d) + 4},
        {ACT_ACT_YEAR, 2012, 2, 29, 2016, 3, 2, (2d / 365d) + 4},

        // ACT_365_ACTUAL
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, (63d / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, (64d / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 366d)},
        {ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29d / 366d},
        {ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28d / 365d},
        {ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        // ACT_360
        {ACT_360, 2011, 12, 28, 2012, 2, 28, (62d / 360d)},
        {ACT_360, 2011, 12, 28, 2012, 2, 29, (63d / 360d)},
        {ACT_360, 2011, 12, 28, 2012, 3, 1, (64d / 360d)},
        {ACT_360, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 360d)},
        {ACT_360, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 360d)},
        {ACT_360, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 360d)},
        {ACT_360, 2012, 2, 28, 2012, 3, 28, 29d / 360d},
        {ACT_360, 2012, 2, 29, 2012, 3, 28, 28d / 360d},
        {ACT_360, 2012, 3, 1, 2012, 3, 28, 27d / 360d},

        // ACT_364
        {ACT_364, 2011, 12, 28, 2012, 2, 28, (62d / 364d)},
        {ACT_364, 2011, 12, 28, 2012, 2, 29, (63d / 364d)},
        {ACT_364, 2011, 12, 28, 2012, 3, 1, (64d / 364d)},
        {ACT_364, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 364d)},
        {ACT_364, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 364d)},
        {ACT_364, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 364d)},
        {ACT_364, 2012, 2, 28, 2012, 3, 28, 29d / 364d},
        {ACT_364, 2012, 2, 29, 2012, 3, 28, 28d / 364d},
        {ACT_364, 2012, 3, 1, 2012, 3, 28, 27d / 364d},

        // ACT_365F
        {ACT_365F, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {ACT_365F, 2011, 12, 28, 2012, 2, 29, (63d / 365d)},
        {ACT_365F, 2011, 12, 28, 2012, 3, 1, (64d / 365d)},
        {ACT_365F, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365d)},
        {ACT_365F, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365d)},
        {ACT_365F, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365d)},
        {ACT_365F, 2012, 2, 28, 2012, 3, 28, 29d / 365d},
        {ACT_365F, 2012, 2, 29, 2012, 3, 28, 28d / 365d},
        {ACT_365F, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        // ACT_365_25
        {ACT_365_25, 2011, 12, 28, 2012, 2, 28, (62d / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2012, 2, 29, (63d / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2012, 3, 1, (64d / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365.25d)},
        {ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29d / 365.25d},
        {ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28d / 365.25d},
        {ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27d / 365.25d},

        // NL_360
        {NL_360, 2011, 12, 28, 2012, 2, 28, (62d / 360d)},
        {NL_360, 2011, 12, 28, 2012, 2, 29, (62d / 360d)},
        {NL_360, 2011, 12, 28, 2012, 3, 1, (63d / 360d)},
        {NL_360, 2011, 12, 28, 2016, 2, 28, ((62d + 365d + 365d + 365d + 365d) / 360d)},
        {NL_360, 2011, 12, 28, 2016, 2, 29, ((62d + 365d + 365d + 365d + 365d) / 360d)},
        {NL_360, 2011, 12, 28, 2016, 3, 1, ((63d + 365d + 365d + 365d + 365d) / 360d)},
        {NL_360, 2012, 2, 28, 2012, 3, 28, 28d / 360d},
        {NL_360, 2012, 2, 29, 2012, 3, 28, 28d / 360d},
        {NL_360, 2012, 3, 1, 2012, 3, 28, 27d / 360d},
        {NL_360, 2011, 12, 1, 2012, 12, 1, 365d / 360d},

        // NL_365
        {NL_365, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {NL_365, 2011, 12, 28, 2012, 2, 29, (62d / 365d)},
        {NL_365, 2011, 12, 28, 2012, 3, 1, (63d / 365d)},
        {NL_365, 2011, 12, 28, 2016, 2, 28, ((62d + 365d + 365d + 365d + 365d) / 365d)},
        {NL_365, 2011, 12, 28, 2016, 2, 29, ((62d + 365d + 365d + 365d + 365d) / 365d)},
        {NL_365, 2011, 12, 28, 2016, 3, 1, ((63d + 365d + 365d + 365d + 365d) / 365d)},
        {NL_365, 2012, 2, 28, 2012, 3, 28, 28d / 365d},
        {NL_365, 2012, 2, 29, 2012, 3, 28, 28d / 365d},
        {NL_365, 2012, 3, 1, 2012, 3, 28, 27d / 365d},
        {NL_365, 2011, 12, 1, 2012, 12, 1, 365d / 365d},

        // THIRTY_360_ISDA
        {THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360},

        {THIRTY_360_ISDA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360},

        {THIRTY_360_ISDA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360},
        {THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)},

        // THIRTY_360_PSA
        {THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_360_PSA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360},
        {THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360},
        {THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360},
        {THIRTY_360_PSA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360},

        {THIRTY_360_PSA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_360_PSA, 2012, 2, 29, 2012, 3, 28, calc360(2012, 2, 30, 2012, 3, 28)},
        {THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 28, calc360(2011, 2, 30, 2012, 2, 28)},
        {THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 29, calc360(2011, 2, 30, 2012, 2, 29)},
        {THIRTY_360_PSA, 2012, 2, 29, 2016, 2, 29, calc360(2012, 2, 30, 2016, 2, 29)},

        {THIRTY_360_PSA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360},
        {THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360},
        {THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)},

        // THIRTY_E_360
        {THIRTY_E_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360},

        {THIRTY_E_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_E_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_E_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_E_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360},

        {THIRTY_E_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_E_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360},
        {THIRTY_E_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_E_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_E_360, 2012, 5, 29, 2013, 8, 31, calc360(2012, 5, 29, 2013, 8, 30)},
        {THIRTY_E_360, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_E_360, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_E_360, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)},

        // THIRTY_EPLUS_360
        {THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360},

        {THIRTY_EPLUS_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360},

        {THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360},
        {THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 31, calc360(2012, 5, 29, 2013, 9, 1)},
        {THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 9, 1)},
        {THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 9, 1)},

        // THIRTY_E_365
        {THIRTY_E_365, 2011, 12, 28, 2012, 2, 28, calc360Days(2011, 12, 28, 2012, 2, 28) / 365d},
        {THIRTY_E_365, 2011, 12, 28, 2012, 2, 29, calc360Days(2011, 12, 28, 2012, 2, 30) / 365d},
        {THIRTY_E_365, 2011, 12, 28, 2012, 3, 1, calc360Days(2011, 12, 28, 2012, 3, 1) / 365d},
        {THIRTY_E_365, 2011, 12, 28, 2016, 2, 28, calc360Days(2011, 12, 28, 2016, 2, 28) / 365d},
        {THIRTY_E_365, 2011, 12, 28, 2016, 2, 29, calc360Days(2011, 12, 28, 2016, 2, 30) / 365d},
        {THIRTY_E_365, 2011, 12, 28, 2016, 3, 1, calc360Days(2011, 12, 28, 2016, 3, 1) / 365d},

        {THIRTY_E_365, 2012, 2, 28, 2012, 3, 28, calc360Days(2012, 2, 28, 2012, 3, 28) / 365d},
        {THIRTY_E_365, 2012, 2, 29, 2012, 3, 28, calc360Days(2012, 2, 30, 2012, 3, 28) / 365d},
        {THIRTY_E_365, 2011, 2, 28, 2012, 2, 28, calc360Days(2011, 2, 30, 2012, 2, 28) / 365d},
        {THIRTY_E_365, 2011, 2, 28, 2012, 2, 29, calc360Days(2011, 2, 30, 2012, 2, 30) / 365d},
        {THIRTY_E_365, 2012, 2, 29, 2016, 2, 29, calc360Days(2012, 2, 30, 2016, 2, 30) / 365d},

        {THIRTY_E_365, 2012, 3, 1, 2012, 3, 28, calc360Days(2012, 3, 1, 2012, 3, 28) / 365d},
        {THIRTY_E_365, 2012, 5, 30, 2013, 8, 29, calc360Days(2012, 5, 30, 2013, 8, 29) / 365d},
        {THIRTY_E_365, 2012, 5, 29, 2013, 8, 30, calc360Days(2012, 5, 29, 2013, 8, 30) / 365d},
        {THIRTY_E_365, 2012, 5, 30, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30) / 365d},
        {THIRTY_E_365, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 8, 30) / 365d},
        {THIRTY_E_365, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30) / 365d},
        {THIRTY_E_365, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30) / 365d},
        {THIRTY_E_365, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30) / 365d},
  };
}

/* --- data_days: 185 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:418 --- */
Object[][] data_days() {
  return new Object[][] {
        {ONE_ONE, 2011, 12, 28, 2012, 2, 28, 1},
        {ONE_ONE, 2011, 12, 28, 2012, 2, 29, 1},
        {ONE_ONE, 2011, 12, 28, 2012, 3, 1, 1},
        {ONE_ONE, 2011, 12, 28, 2016, 2, 28, 1},
        {ONE_ONE, 2011, 12, 28, 2016, 2, 29, 1},
        {ONE_ONE, 2011, 12, 28, 2016, 3, 1, 1},
        {ONE_ONE, 2012, 2, 29, 2012, 3, 29, 1},
        {ONE_ONE, 2012, 2, 29, 2012, 3, 28, 1},
        {ONE_ONE, 2012, 3, 1, 2012, 3, 28, 1},

        // ACT_ACT_ISDA
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, 1523},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, 1524},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, 1525},

        // ACT_ACT_AFB
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, 1523},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, 1524},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, 1525},

        // ACT_ACT_YEAR
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 28, 1523},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 29, 1524},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 3, 1, 1525},

        // ACT_365_ACTUAL
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27},

        // ACT_360
        {ACT_360, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_360, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_360, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_360, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_360, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_360, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},

        // ACT_364
        {ACT_364, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_364, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_364, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_364, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_364, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_364, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_364, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_364, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_364, 2012, 3, 1, 2012, 3, 28, 27},

        // ACT_365F
        {ACT_365F, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_365F, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_365F, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_365F, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_365F, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_365F, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_365F, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_365F, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_365F, 2012, 3, 1, 2012, 3, 28, 27},

        // ACT_365_25
        {ACT_365_25, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_365_25, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_365_25, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_365_25, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27},

        // NL_360
        {NL_360, 2011, 12, 28, 2012, 2, 28, 62},
        {NL_360, 2011, 12, 28, 2012, 2, 29, 62},
        {NL_360, 2011, 12, 28, 2012, 3, 1, 63},
        {NL_360, 2011, 12, 28, 2016, 2, 28, 62 + 365 + 365 + 365 + 365},
        {NL_360, 2011, 12, 28, 2016, 2, 29, 62 + 365 + 365 + 365 + 365},
        {NL_360, 2011, 12, 28, 2016, 3, 1, 63 + 365 + 365 + 365 + 365},
        {NL_360, 2012, 2, 28, 2012, 3, 28, 28},
        {NL_360, 2012, 2, 29, 2012, 3, 28, 28},
        {NL_360, 2012, 3, 1, 2012, 3, 28, 27},
        {NL_360, 2011, 12, 1, 2012, 12, 1, 365},

        // NL_365
        {NL_365, 2011, 12, 28, 2012, 2, 28, 62},
        {NL_365, 2011, 12, 28, 2012, 2, 29, 62},
        {NL_365, 2011, 12, 28, 2012, 3, 1, 63},
        {NL_365, 2011, 12, 28, 2016, 2, 28, 62 + 365 + 365 + 365 + 365},
        {NL_365, 2011, 12, 28, 2016, 2, 29, 62 + 365 + 365 + 365 + 365},
        {NL_365, 2011, 12, 28, 2016, 3, 1, 63 + 365 + 365 + 365 + 365},
        {NL_365, 2012, 2, 28, 2012, 3, 28, 28},
        {NL_365, 2012, 2, 29, 2012, 3, 28, 28},
        {NL_365, 2012, 3, 1, 2012, 3, 28, 27},
        {NL_365, 2011, 12, 1, 2012, 12, 1, 365},

        // THIRTY_360_ISDA
        {THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS},

        {THIRTY_360_ISDA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360DAYS},

        {THIRTY_360_ISDA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360DAYS},
        {THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},

        // THIRTY_360_PSA
        {THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS},

        {THIRTY_360_PSA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2012, 2, 29, 2012, 3, 28, calc360Days(2012, 2, 30, 2012, 3, 28)},
        {THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 28, calc360Days(2011, 2, 30, 2012, 2, 28)},
        {THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 29, calc360Days(2011, 2, 30, 2012, 2, 29)},
        {THIRTY_360_PSA, 2012, 2, 29, 2016, 2, 29, calc360Days(2012, 2, 30, 2016, 2, 29)},

        {THIRTY_360_PSA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360DAYS},
        {THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},

        // THIRTY_E_360
        {THIRTY_E_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS},

        {THIRTY_E_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360DAYS},

        {THIRTY_E_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_E_360, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 8, 30)},
        {THIRTY_E_360, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_E_360, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_E_360, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},

        // THIRTY_EPLUS_360
        {THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS},

        {THIRTY_EPLUS_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360DAYS},

        {THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 9, 1)},
        {THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 9, 1)},
        {THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 9, 1)},

        // THIRTY_E_365
        {THIRTY_E_365, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2011, 12, 28, 2012, 2, 29, calc360Days(2011, 12, 28, 2012, 2, 30)},
        {THIRTY_E_365, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2011, 12, 28, 2016, 2, 29, calc360Days(2011, 12, 28, 2016, 2, 30)},
        {THIRTY_E_365, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS},

        {THIRTY_E_365, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2012, 2, 29, 2012, 3, 28, calc360Days(2012, 2, 30, 2012, 3, 28)},
        {THIRTY_E_365, 2011, 2, 28, 2012, 2, 28, calc360Days(2011, 2, 30, 2012, 2, 28)},
        {THIRTY_E_365, 2011, 2, 28, 2012, 2, 29, calc360Days(2011, 2, 30, 2012, 2, 30)},
        {THIRTY_E_365, 2012, 2, 29, 2016, 2, 29, calc360Days(2012, 2, 30, 2012, 2, 30)},

        {THIRTY_E_365, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS},
        {THIRTY_E_365, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 8, 30)},
        {THIRTY_E_365, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_E_365, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)},
        {THIRTY_E_365, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)},
  };
}

/* --- data_30U360: 22 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:658 --- */
Object[][] data_30U360() {
  return new Object[][] {
        {2011, 12, 28, 2012, 2, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2012, 2, 29, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2012, 3, 1, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2016, 2, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2016, 2, 29, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2016, 3, 1, SIMPLE_30_360, SIMPLE_30_360},

        {2012, 2, 28, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 2, 29, 2012, 3, 28, SIMPLE_30_360, calc360(2012, 2, 30, 2012, 3, 28)},
        {2012, 2, 29, 2012, 3, 30, SIMPLE_30_360, calc360(2012, 2, 30, 2012, 3, 30)},
        {2012, 2, 29, 2012, 3, 31, SIMPLE_30_360, calc360(2012, 2, 30, 2012, 3, 30)},
        {2012, 2, 29, 2013, 2, 28, SIMPLE_30_360, calc360(2012, 2, 30, 2013, 2, 30)},
        {2011, 2, 28, 2012, 2, 28, SIMPLE_30_360, calc360(2011, 2, 30, 2012, 2, 28)},
        {2011, 2, 28, 2012, 2, 29, SIMPLE_30_360, calc360(2011, 2, 30, 2012, 2, 30)},
        {2012, 2, 29, 2016, 2, 29, SIMPLE_30_360, calc360(2012, 2, 30, 2016, 2, 30)},

        {2012, 3, 1, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 30, 2013, 8, 29, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 29, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 30, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 29, 2013, 8, 31, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)},
        {2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)},
        {2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)},
  };
}

/* --- data_30E360ISDA: 19 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:732 --- */
Object[][] data_30E360ISDA() {
  return new Object[][] {
        {2011, 12, 28, 2012, 2, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2012, 2, 29, calc360(2011, 12, 28, 2012, 2, 30), SIMPLE_30_360},
        {2011, 12, 28, 2012, 3, 1, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2016, 2, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2011, 12, 28, 2016, 2, 29, calc360(2011, 12, 28, 2016, 2, 30), SIMPLE_30_360},
        {2011, 12, 28, 2016, 3, 1, SIMPLE_30_360, SIMPLE_30_360},

        {2012, 2, 28, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 2, 29, 2012, 3, 28, calc360(2012, 2, 30, 2012, 3, 28), calc360(2012, 2, 30, 2012, 3, 28)},
        {2011, 2, 28, 2012, 2, 28, calc360(2011, 2, 30, 2012, 2, 28), calc360(2011, 2, 30, 2012, 2, 28)},
        {2011, 2, 28, 2012, 2, 29, calc360(2011, 2, 30, 2012, 2, 30), calc360(2011, 2, 30, 2012, 2, 29)},
        {2012, 2, 29, 2016, 2, 29, calc360(2012, 2, 30, 2016, 2, 30), calc360(2012, 2, 30, 2016, 2, 29)},

        {2012, 3, 1, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 30, 2013, 8, 29, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 29, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 30, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360},
        {2012, 5, 29, 2013, 8, 31, calc360(2012, 5, 29, 2013, 8, 30), calc360(2012, 5, 29, 2013, 8, 30)},
        {2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)},
        {2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)},
        {2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)},
  };
}

/* --- data_ACTACTAFB: 57 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:794 --- */
Object[][] data_ACTACTAFB() {
  return new Object[][] {
        // example from the original French specification
        {1994, 2, 10, 1997, 6, 30, 140d / 365d + 3},
        {1994, 2, 10, 1994, 6, 30, 140d / 365d},

        // simple examples that are less than one year long
        {2004, 2, 10, 2005, 2, 10, 1d},
        {2004, 2, 28, 2005, 2, 28, 1d},
        {2004, 2, 29, 2005, 2, 28, 365d / 366d},
        {2004, 3, 1, 2005, 3, 1, 1d},

        // examples over one year, from a fixed start date
        // from Feb28 2003
        {2003, 2, 28, 2005, 2, 27, 1d + (364d / 365d)},
        {2003, 2, 28, 2005, 2, 28, 2d},
        {2003, 2, 28, 2005, 3, 1, 2d + (1d / 365d)},
        {2003, 2, 28, 2008, 2, 27, 4d + (364d / 365d)},
        {2003, 2, 28, 2008, 2, 28, 5d},
        {2003, 2, 28, 2008, 2, 29, 5d},
        {2003, 2, 28, 2008, 3, 1, 5d + (1d / 365d)},
        // from Feb28 2004
        {2004, 2, 28, 2005, 2, 27, (365d / 366d)},
        {2004, 2, 28, 2005, 2, 28, 1d},
        {2004, 2, 28, 2005, 3, 1, 1d + (2d / 366d)},
        {2004, 2, 28, 2008, 2, 27, 3d + (365d / 366d)},
        {2004, 2, 28, 2008, 2, 28, 4d},                   // ISDA end-of-February would give (4d + (1d / 365d))
        {2004, 2, 28, 2008, 2, 29, 4d + (1d / 365d)},
        {2004, 2, 28, 2008, 3, 1, 4d + (2d / 366d)},
        // from Feb29 2004
        {2004, 2, 29, 2005, 2, 28, 365d / 366d},
        {2004, 2, 29, 2005, 3, 1, 1d + (1d / 366d)},
        {2004, 2, 29, 2008, 2, 27, 3d + (364d / 366d)},
        {2004, 2, 29, 2008, 2, 28, 3d + (365d / 366d)},   // ISDA end-of-February would give (4d)
        {2004, 2, 29, 2008, 2, 29, 4d},
        {2004, 2, 29, 2008, 3, 1, 4d + (1d / 366d)},
        // from Mar01 2004
        {2004, 3, 1, 2005, 2, 28, 364d / 365d},
        {2004, 3, 1, 2005, 3, 1, 1d},
        {2004, 3, 1, 2008, 2, 27, 3d + (363d / 365d)},
        {2004, 3, 1, 2008, 2, 28, 3d + (364d / 365d)},
        {2004, 3, 1, 2008, 2, 29, 3d + (364d / 365d)},
        {2004, 3, 1, 2008, 3, 1, 4d},
        // from Mar01 2003
        {2003, 3, 1, 2005, 2, 27, 1d + (363d / 365d)},
        {2003, 3, 1, 2005, 2, 28, 1d + (364d / 365d)},    // ISDA end-of-February would give (2d)
        {2003, 3, 1, 2005, 3, 1, 2d},
        {2003, 3, 1, 2008, 2, 27, 4d + (363d / 365d)},    // ISDA end-of-February would give (5d)
        {2003, 3, 1, 2008, 2, 28, 4d + (364d / 365d)},
        {2003, 3, 1, 2008, 2, 29, 5d},
        {2003, 3, 1, 2008, 3, 1, 5d},

        // examples over one year, up to a fixed end date (not relevant in real life)
        // up to Mar01 from leap year
        {2004, 2, 28, 2006, 3, 1, 2d + (2d / 366d)},
        {2004, 2, 29, 2006, 3, 1, 2d + (1d / 366d)},
        {2004, 3, 1, 2006, 3, 1, 2d},
        // up to Mar01 from non leap year
        {2005, 2, 28, 2007, 3, 1, 2d + (1d / 365d)},
        {2005, 3, 1, 2007, 3, 1, 2d},
        // up to Feb28 in leap year from leap year
        {2004, 2, 27, 2008, 2, 28, 4d + (1d / 365d)},     // ISDA end-of-February would give (4d + (2d / 365d))
        {2004, 2, 28, 2008, 2, 28, 4d},                   // ISDA end-of-February would give (4d + (1d / 365d))
        {2004, 2, 29, 2008, 2, 28, 3d + (365d / 366d)},   // ISDA end-of-February would give (4d)
        {2004, 3, 1, 2008, 2, 28, 3d + (364d / 365d)},
        // up to Feb28 in leap year from non leap year
        {2006, 2, 27, 2008, 2, 28, 2d + (1d / 365d)},
        {2006, 2, 28, 2008, 2, 28, 2d},
        {2006, 3, 1, 2008, 2, 28, 1d + (364d / 365d)},
        // up to Feb29 in leap year from leap year
        {2004, 2, 28, 2008, 2, 29, 4d + (1d / 365d)},
        {2004, 2, 29, 2008, 2, 29, 4d},
        {2004, 3, 1, 2008, 2, 29, 3d + (364d / 365d)},
        // up to Feb29 in leap year from non leap year
        {2006, 2, 27, 2008, 2, 29, 2d + (1d / 365d)},
        {2006, 2, 28, 2008, 2, 29, 2d},
        {2006, 3, 1, 2008, 2, 29, 1d + (364d / 365d)},
  };
}

/* --- data_ACT365L: 12 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:884 --- */
Object[][] data_ACT365L() {
  return new Object[][] {
        {2011, 12, 28, 2012, 2, 28, P12M, 2012, 2, 28, 62d / 365d},
        {2011, 12, 28, 2012, 2, 28, P12M, 2012, 2, 29, 62d / 366d},
        {2011, 12, 28, 2012, 2, 28, P12M, 2012, 3, 1, 62d / 366d},

        {2011, 12, 28, 2012, 2, 29, P12M, 2012, 2, 29, 63d / 366d},
        {2011, 12, 28, 2012, 2, 29, P12M, 2012, 3, 1, 63d / 366d},

        {2011, 12, 28, 2012, 2, 28, P6M, 2012, 2, 28, 62d / 366d},
        {2011, 12, 28, 2012, 2, 28, P6M, 2012, 2, 29, 62d / 366d},
        {2011, 12, 28, 2012, 2, 28, P6M, 2012, 3, 1, 62d / 366d},

        {2011, 12, 28, 2012, 2, 29, P6M, 2012, 2, 29, 63d / 366d},
        {2011, 12, 28, 2012, 2, 29, P6M, 2012, 3, 1, 63d / 366d},

        {2010, 12, 28, 2011, 2, 28, P6M, 2011, 2, 28, 62d / 365d},
        {2010, 12, 28, 2011, 2, 28, P6M, 2011, 3, 1, 62d / 365d},
  };
}

/* --- data_generation: 87 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/schedule/PeriodicScheduleTest.java:287 --- */
Object[][] data_generation() {
  return new Object[][] {
        // stub null
        {JUN_17, SEP_17, P1M, null, null, BDA, null, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},

        // stub NONE
        {JUN_17, SEP_17, P1M, STUB_NONE, null, BDA, null, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, JUL_17, P1M, STUB_NONE, null, BDA, null, null, null,
            list(JUN_17, JUL_17),
            list(JUN_17, JUL_17), DAY_17},

        // stub SHORT_INITIAL
        {JUN_04, SEP_17, P1M, SHORT_INITIAL, null, BDA, null, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, SEP_17, P1M, SHORT_INITIAL, null, BDA, null, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, JUL_04, P1M, SHORT_INITIAL, null, BDA, null, null, null,
            list(JUN_17, JUL_04),
            list(JUN_17, JUL_04), DAY_4},
        {date(2011, 6, 28), date(2011, 6, 30), P1M, SHORT_INITIAL, EOM, BDA, null, null, null,
            list(date(2011, 6, 28), date(2011, 6, 30)),
            list(date(2011, 6, 28), date(2011, 6, 30)), EOM},
        {date(2014, 12, 12), date(2015, 8, 24), P3M, SHORT_INITIAL, null, BDA, null, null, null,
            list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 24), date(2015, 8, 24)),
            list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 25), date(2015, 8, 24)), DAY_24},
        {date(2014, 12, 12), date(2015, 8, 24), P3M, SHORT_INITIAL, RollConventions.NONE, BDA, null, null, null,
            list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 24), date(2015, 8, 24)),
            list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 25), date(2015, 8, 24)), DAY_24},
        {date(2014, 11, 24), date(2015, 8, 24), P3M, null, RollConventions.NONE, BDA, null, null, null,
            list(date(2014, 11, 24), date(2015, 2, 24), date(2015, 5, 24), date(2015, 8, 24)),
            list(date(2014, 11, 24), date(2015, 2, 24), date(2015, 5, 25), date(2015, 8, 24)), DAY_24},

        // stub LONG_INITIAL
        {JUN_04, SEP_17, P1M, LONG_INITIAL, null, BDA, null, null, null,
            list(JUN_04, JUL_17, AUG_17, SEP_17),
            list(JUN_04, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, SEP_17, P1M, LONG_INITIAL, null, BDA, null, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, JUL_04, P1M, LONG_INITIAL, null, BDA, null, null, null,
            list(JUN_17, JUL_04),
            list(JUN_17, JUL_04), DAY_4},
        {JUN_17, AUG_04, P1M, LONG_INITIAL, null, BDA, null, null, null,
            list(JUN_17, AUG_04),
            list(JUN_17, AUG_04), DAY_4},

        // stub SMART_INITIAL
        {JUN_04, SEP_17, P1M, SMART_INITIAL, null, BDA, null, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_10, SEP_17, P1M, SMART_INITIAL, null, BDA, null, null, null,
            list(JUN_10, JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_10, JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_11, SEP_17, P1M, SMART_INITIAL, null, BDA, null, null, null,
            list(JUN_11, JUL_17, AUG_17, SEP_17),
            list(JUN_11, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, JUL_04, P1M, SMART_INITIAL, null, BDA, null, null, null,
            list(JUN_17, JUL_04),
            list(JUN_17, JUL_04), DAY_4},

        // stub SHORT_FINAL
        {JUN_04, SEP_17, P1M, SHORT_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17), DAY_4},
        {JUN_17, SEP_17, P1M, SHORT_FINAL, null, BDA, null, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, JUL_04, P1M, SHORT_FINAL, null, BDA, null, null, null,
            list(JUN_17, JUL_04),
            list(JUN_17, JUL_04), DAY_17},
        {date(2011, 6, 28), date(2011, 6, 30), P1M, SHORT_FINAL, EOM, BDA, null, null, null,
            list(date(2011, 6, 28), date(2011, 6, 30)),
            list(date(2011, 6, 28), date(2011, 6, 30)), DAY_28},
        {date(2014, 11, 29), date(2015, 9, 2), P3M, SHORT_FINAL, null, BDA, null, null, null,
            list(date(2014, 11, 29), date(2015, 2, 28), date(2015, 5, 29), date(2015, 8, 29), date(2015, 9, 2)),
            list(date(2014, 11, 28), date(2015, 2, 27), date(2015, 5, 29), date(2015, 8, 31), date(2015, 9, 2)),
            DAY_29},
        {date(2014, 11, 29), date(2015, 9, 2), P3M, SHORT_FINAL, RollConventions.NONE, BDA, null, null, null,
            list(date(2014, 11, 29), date(2015, 2, 28), date(2015, 5, 29), date(2015, 8, 29), date(2015, 9, 2)),
            list(date(2014, 11, 28), date(2015, 2, 27), date(2015, 5, 29), date(2015, 8, 31), date(2015, 9, 2)),
            DAY_29},

        // stub LONG_FINAL
        {JUN_04, SEP_17, P1M, LONG_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, SEP_17),
            list(JUN_04, JUL_04, AUG_04, SEP_17), DAY_4},
        {JUN_17, SEP_17, P1M, LONG_FINAL, null, BDA, null, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, JUL_04, P1M, LONG_FINAL, null, BDA, null, null, null,
            list(JUN_17, JUL_04),
            list(JUN_17, JUL_04), DAY_17},
        {JUN_17, AUG_04, P1M, LONG_FINAL, null, BDA, null, null, null,
            list(JUN_17, AUG_04),
            list(JUN_17, AUG_04), DAY_17},

        // stub SMART_FINAL
        {JUN_04, SEP_17, P1M, SMART_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17), DAY_4},
        {JUN_04, SEP_11, P1M, SMART_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_11),
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_11), DAY_4},
        {JUN_04, SEP_10, P1M, SMART_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, SEP_10),
            list(JUN_04, JUL_04, AUG_04, SEP_10), DAY_4},
        {JUN_17, JUL_04, P1M, SMART_FINAL, null, BDA, null, null, null,
            list(JUN_17, JUL_04),
            list(JUN_17, JUL_04), DAY_17},

        // explicit initial stub
        {JUN_04, SEP_17, P1M, null, null, BDA, JUN_17, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_04, SEP_17, P1M, SHORT_INITIAL, null, BDA, JUN_17, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_17, SEP_17, P1M, null, null, BDA, JUN_17, null, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_04, SEP_04, P1M, SMART_FINAL, null, BDA, JUN_17, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_04),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_04), DAY_17},

        // explicit final stub
        {JUN_04, SEP_17, P1M, null, null, BDA, null, AUG_04, null,
            list(JUN_04, JUL_04, AUG_04, SEP_17),
            list(JUN_04, JUL_04, AUG_04, SEP_17), DAY_4},
        {JUN_04, SEP_17, P1M, SHORT_FINAL, null, BDA, null, AUG_04, null,
            list(JUN_04, JUL_04, AUG_04, SEP_17),
            list(JUN_04, JUL_04, AUG_04, SEP_17), DAY_4},
        {JUN_17, SEP_17, P1M, null, null, BDA, null, AUG_17, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_04, SEP_04, P1M, SMART_INITIAL, null, BDA, null, AUG_17, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_04),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_04), DAY_17},

        // explicit double stub
        {JUN_04, SEP_17, P1M, null, null, BDA, JUL_11, AUG_11, null,
            list(JUN_04, JUL_11, AUG_11, SEP_17),
            list(JUN_04, JUL_11, AUG_11, SEP_17), DAY_11},
        {JUN_04, OCT_17, P1M, STUB_BOTH, null, BDA, JUL_11, SEP_11, null,
            list(JUN_04, JUL_11, AUG_11, SEP_11, OCT_17),
            list(JUN_04, JUL_11, AUG_11, SEP_11, OCT_17), DAY_11},
        {JUN_17, SEP_17, P1M, null, null, BDA, JUN_17, SEP_17, null,
            list(JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},

        // stub null derive from roll convention
        {JUN_04, SEP_17, P1M, null, DAY_17, BDA, null, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
            list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17), DAY_17},
        {JUN_04, SEP_17, P1M, null, DAY_4, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
            list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17), DAY_4},

        // near end of month
        // EOM flag false, thus roll on 30th
        {NOV_30_2013, NOV_30, P3M, STUB_NONE, null, BDA, null, null, null,
            list(NOV_30_2013, FEB_28, MAY_30, AUG_30, NOV_30),
            list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28), DAY_30},
        // EOM flag true and is EOM, thus roll at EOM
        {NOV_30_2013, NOV_30, P3M, STUB_NONE, EOM, BDA, null, null, null,
            list(NOV_30_2013, FEB_28, MAY_31, AUG_31, NOV_30),
            list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28), EOM},
        // EOM flag true, and last business day, thus roll at EOM (stub convention defined)
        {MAY_30, NOV_30, P3M, STUB_NONE, EOM, BDA, null, null, null,
            list(MAY_31, AUG_31, NOV_30),
            list(MAY_30, AUG_29, NOV_28), EOM},
        // EOM flag true, and last business day, thus roll at EOM
        {MAY_30, NOV_30, P3M, null, EOM, BDA, null, null, null,
            list(MAY_31, AUG_31, NOV_30),
            list(MAY_30, AUG_29, NOV_28), EOM},
        // EOM flag true, and last business day, thus roll at EOM (start adjustment none)
        {MAY_30, NOV_30, P3M, null, EOM, BDA, null, null, BDA_NONE,
            list(MAY_31, AUG_31, NOV_30),
            list(MAY_30, AUG_29, NOV_28), EOM},
        // roll date set to 30th, so roll on 30th
        {MAY_30, NOV_30, P3M, null, DAY_30, BDA, null, null, null,
            list(MAY_30, AUG_30, NOV_30),
            list(MAY_30, AUG_29, NOV_28), DAY_30},
        // EOM flag true, but not EOM, thus roll on 30th
        {JUL_30, OCT_30, P1M, null, EOM, BDA, null, null, null,
            list(JUL_30, AUG_30, SEP_30, OCT_30),
            list(JUL_30, AUG_29, SEP_30, OCT_30), DAY_30},
        // EOM flag true and is EOM, double stub, thus roll at EOM
        {date(2014, 1, 3), SEP_17, P3M, STUB_BOTH, EOM, BDA, FEB_28, AUG_31, null,
            list(date(2014, 1, 3), FEB_28, MAY_31, AUG_31, SEP_17),
            list(date(2014, 1, 3), FEB_28, MAY_30, AUG_29, SEP_17), EOM},
        // EOM flag true plus start date as last business day of month with start date adjust of NONE
        {NOV_29_2013, NOV_30, P3M, STUB_NONE, EOM, BDA, null, null, BDA_NONE,
            list(NOV_30_2013, FEB_28, MAY_31, AUG_31, NOV_30),
            list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28), EOM},
        // EOM flag true plus start date as last business day of month with start date adjust of NONE
        {NOV_29_2013, NOV_30, P3M, null, EOM, BDA, null, null, BDA_NONE,
            list(NOV_30_2013, FEB_28, MAY_31, AUG_31, NOV_30),
            list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28), EOM},
        // EOM flag false, short initial, implies EOM true
        {date(2011, 6, 2), date(2011, 8, 31), P1M, SHORT_INITIAL, null, BDA, null, null, null,
            list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 31), date(2011, 8, 31)),
            list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 29), date(2011, 8, 31)), EOM},
        // EOM flag false, explicit stub, implies EOM true
        {date(2011, 6, 2), date(2011, 8, 31), P1M, null, null, BDA, date(2011, 6, 30), null, null,
            list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 31), date(2011, 8, 31)),
            list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 29), date(2011, 8, 31)), EOM},
        // EOM flag false, explicit stub, implies EOM true
        {date(2011, 7, 31), date(2011, 10, 10), P1M, null, null, BDA, null, date(2011, 9, 30), null,
            list(date(2011, 7, 31), date(2011, 8, 31), date(2011, 9, 30), date(2011, 10, 10)),
            list(date(2011, 7, 29), date(2011, 8, 31), date(2011, 9, 30), date(2011, 10, 10)), EOM},
        // EOM flag false, explicit stub, implies EOM true
        {date(2011, 2, 2), date(2011, 5, 30), P1M, null, null, BDA, date(2011, 2, 28), null, null,
            list(date(2011, 2, 2), date(2011, 2, 28), date(2011, 3, 30), date(2011, 4, 30), date(2011, 5, 30)),
            list(date(2011, 2, 2), date(2011, 2, 28), date(2011, 3, 30), date(2011, 4, 29), date(2011, 5, 30)),
            DAY_30},
        // EOM flag true and is EOM, but end date equals start day rather than EOM
        {date(2018, 2, 28), date(2024, 2, 28), Frequency.ofYears(2), STUB_NONE, EOM, BDA, null, null, null,
            list(date(2018, 2, 28), date(2020, 2, 29), date(2022, 2, 28), date(2024, 2, 28)),
            list(date(2018, 2, 28), date(2020, 2, 28), date(2022, 2, 28), date(2024, 2, 28)), EOM},
        // EOM flag true and is EOM, but end date equals start day rather than EOM
        {date(2018, 4, 30), date(2018, 10, 30), P2M, STUB_NONE, EOM, BDA, null, null, null,
            list(date(2018, 4, 30), date(2018, 6, 30), date(2018, 8, 31), date(2018, 10, 30)),
            list(date(2018, 4, 30), date(2018, 6, 29), date(2018, 8, 31), date(2018, 10, 30)), EOM},

        // pre-adjusted start date, no change needed
        {JUL_17, OCT_17, P1M, null, DAY_17, BDA, null, null, BDA_NONE,
            list(JUL_17, AUG_17, SEP_17, OCT_17),
            list(JUL_17, AUG_18, SEP_17, OCT_17), DAY_17},
        // pre-adjusted start date, change needed
        {AUG_18, OCT_17, P1M, null, DAY_17, BDA, null, null, BDA_NONE,
            list(AUG_17, SEP_17, OCT_17),
            list(AUG_18, SEP_17, OCT_17), DAY_17},
        // pre-adjusted first regular, change needed
        {JUL_11, OCT_17, P1M, null, DAY_17, BDA, AUG_18, null, BDA_NONE,
            list(JUL_11, AUG_17, SEP_17, OCT_17),
            list(JUL_11, AUG_18, SEP_17, OCT_17), DAY_17},
        // pre-adjusted last regular, change needed
        {JUL_17, OCT_17, P1M, null, DAY_17, BDA, null, AUG_18, BDA_NONE,
            list(JUL_17, AUG_17, OCT_17),
            list(JUL_17, AUG_18, OCT_17), DAY_17},
        // pre-adjusted first+last regular, change needed
        {APR_01, OCT_17, P1M, null, DAY_17, BDA, MAY_19, AUG_18, BDA_NONE,
            list(APR_01, MAY_17, JUN_17, JUL_17, AUG_17, OCT_17),
            list(APR_01, MAY_19, JUN_17, JUL_17, AUG_18, OCT_17), DAY_17},
        // pre-adjusted end date, change needed
        {JUL_17, AUG_18, P1M, null, DAY_17, BDA, null, null, BDA_NONE,
            list(JUL_17, AUG_17),
            list(JUL_17, AUG_18), DAY_17},
        // pre-adjusted end date, change needed, with adjustment
        {JUL_17, AUG_18, P1M, null, DAY_17, BDA, null, null, BDA,
            list(JUL_17, AUG_17),
            list(JUL_17, AUG_18), DAY_17},

        // TERM period
        {JUN_04, SEP_17, TERM, STUB_NONE, null, BDA, null, null, null,
            list(JUN_04, SEP_17),
            list(JUN_04, SEP_17), ROLL_NONE},
        // TERM period defined as a stub and no regular periods
        {JUN_04, SEP_17, P12M, SHORT_INITIAL, null, BDA, SEP_17, null, null,
            list(JUN_04, SEP_17),
            list(JUN_04, SEP_17), DAY_17},
        {JUN_04, SEP_17, P12M, SHORT_INITIAL, null, BDA, null, JUN_04, null,
            list(JUN_04, SEP_17),
            list(JUN_04, SEP_17), DAY_4},
        {date(2014, 9, 24), date(2016, 11, 24), Frequency.ofYears(2), SHORT_INITIAL, null, BDA, null, null, null,
            list(date(2014, 9, 24), date(2014, 11, 24), date(2016, 11, 24)),
            list(date(2014, 9, 24), date(2014, 11, 24), date(2016, 11, 24)), DAY_24},

        // IMM
        {date(2014, 9, 17), date(2014, 10, 15), P1M, STUB_NONE, IMM, BDA, null, null, null,
            list(date(2014, 9, 17), date(2014, 10, 15)),
            list(date(2014, 9, 17), date(2014, 10, 15)), IMM},
        {date(2014, 9, 17), date(2014, 10, 15), TERM, STUB_NONE, IMM, BDA, null, null, null,
            list(date(2014, 9, 17), date(2014, 10, 15)),
            list(date(2014, 9, 17), date(2014, 10, 15)), IMM},
        // IMM with an extremely short period - two days - still works
        {date(2014, 9, 17), date(2014, 10, 15), Frequency.ofDays(2), STUB_NONE, IMM, BDA, null, null, null,
            list(date(2014, 9, 17), date(2014, 10, 15)),
            list(date(2014, 9, 17), date(2014, 10, 15)), IMM},
        {date(2014, 9, 17), date(2014, 10, 1), Frequency.ofDays(2), STUB_NONE, IMM, BDA, null, null, null,
            list(date(2014, 9, 17), date(2014, 10, 1)),
            list(date(2014, 9, 17), date(2014, 10, 1)), IMM},

        // IMM with adjusted start dates and various conventions
        // Modified Following, no stub
        {date(2018, 3, 22), date(2020, 3, 18), P6M, STUB_NONE, IMM, BDA_JPY_MF, null, null, BDA_NONE,
            list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20), date(2019, 9, 18), date(2020, 3, 18)),
            list(date(2018, 3, 22), date(2018, 9, 19), date(2019, 3, 20), date(2019, 9, 18), date(2020, 3, 18)), IMM},
        // Preceding, no stub
        {date(2018, 3, 20), date(2019, 3, 20), P6M, STUB_NONE, IMM, BDA_JPY_P, null, null, BDA_NONE,
            list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20)),
            list(date(2018, 3, 20), date(2018, 9, 19), date(2019, 3, 20)), IMM},
        // Modified Following, null stub
        {date(2018, 3, 22), date(2019, 3, 20), P6M, null, IMM, BDA_JPY_MF, null, null, BDA_NONE,
            list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20)),
            list(date(2018, 3, 22), date(2018, 9, 19), date(2019, 3, 20)), IMM},
        // Explicit long front stub with (adjusted) first regular start date
        {date(2017, 9, 2), date(2018, 9, 19), P6M, LONG_INITIAL, IMM, BDA_JPY_MF, date(2018, 3, 22), null, BDA_NONE,
            list(date(2017, 9, 2), date(2018, 3, 21), date(2018, 9, 19)),
            list(date(2017, 9, 2), date(2018, 3, 22), date(2018, 9, 19)), IMM},
        // Implicit short front stub with (adjusted) first regular start date
        {date(2018, 1, 2), date(2018, 9, 19), P6M, null, IMM, BDA_JPY_MF, date(2018, 3, 22), null, BDA_NONE,
            list(date(2018, 1, 2), date(2018, 3, 21), date(2018, 9, 19)),
            list(date(2018, 1, 2), date(2018, 3, 22), date(2018, 9, 19)), IMM},
        // Implicit back stub with (adjusted) last regular start date
        {date(2017, 3, 15), date(2018, 5, 19), P6M, null, IMM, BDA_JPY_MF, null, date(2018, 3, 22), BDA_NONE,
            list(date(2017, 3, 15), date(2017, 9, 20), date(2018, 3, 21), date(2018, 5, 19)),
            list(date(2017, 3, 15), date(2017, 9, 20), date(2018, 3, 22), date(2018, 5, 21)), IMM},

        // Day30 rolling with February
        {date(2015, 1, 30), date(2015, 4, 30), P1M, STUB_NONE, DAY_30, BDA, null, null, null,
            list(date(2015, 1, 30), date(2015, 2, 28), date(2015, 3, 30), date(2015, 4, 30)),
            list(date(2015, 1, 30), date(2015, 2, 27), date(2015, 3, 30), date(2015, 4, 30)), DAY_30},
        {date(2015, 2, 28), date(2015, 4, 30), P1M, STUB_NONE, DAY_30, BDA, null, null, null,
            list(date(2015, 2, 28), date(2015, 3, 30), date(2015, 4, 30)),
            list(date(2015, 2, 27), date(2015, 3, 30), date(2015, 4, 30)), DAY_30},
        {date(2015, 2, 28), date(2015, 4, 30), P1M, SHORT_INITIAL, DAY_30, BDA, null, null, null,
            list(date(2015, 2, 28), date(2015, 3, 30), date(2015, 4, 30)),
            list(date(2015, 2, 27), date(2015, 3, 30), date(2015, 4, 30)), DAY_30},

        // Two stubs no regular
        {date(2019, 1, 16), date(2020, 10, 22), P12M, null, DAY_22, BDA, date(2020, 1, 22), date(2020, 1, 22), null,
            list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)),
            list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)), DAY_22},
        {date(2019, 1, 16), date(2020, 10, 22), P12M, STUB_BOTH, DAY_22, BDA, date(2020, 1, 22), date(2020, 1, 22),
            null,
            list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)),
            list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)), DAY_22},
  };
}

/* --- data_replace: 11 rows --- Source: modules/basics/src/test/java/com/opengamma/strata/basics/schedule/PeriodicScheduleTest.java:931 --- */
Object[][] data_replace() {
  return new Object[][] {
        // SmartInitial is set
        {JUN_11, JUN_17, AUG_17, P1M, null, DAY_17, BDA, null, null, BDA_JPY_P,
            list(JUN_11, JUL_17, AUG_17), SMART_INITIAL, null, DAY_17},
        // SmartInitial not set
        {MAY_19, JUN_17, AUG_17, P1M, LONG_INITIAL, DAY_17, BDA, JUN_17, AUG_17, BDA_JPY_P,
            list(MAY_19, JUL_17, AUG_17), LONG_INITIAL, AUG_17, DAY_17},
        // start set to be later
        {JUL_04, JUN_17, AUG_17, P1M, null, DAY_17, BDA, JUN_17, AUG_17, BDA_JPY_P,
            list(JUL_04, JUL_17, AUG_17), SMART_INITIAL, AUG_17, DAY_17},
        // original schedule had no stubs and NONE, new schedule uses SmartInitial instead
        {JUN_04, JUN_17, AUG_17, P1M, STUB_NONE, null, BDA, null, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17), SMART_INITIAL, null, null},
        // original schedule had double stubs with stub convention and first regular date to determine roll of 17th
        // new schedule uses SmartInitial and calculated last regular
        {JUN_04, JUN_03, AUG_30, P1M, SMART_FINAL, null, BDA, JUN_17, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, AUG_30), SMART_INITIAL, AUG_17, null},
        // original schedule had double explicit stubs, new schedule uses SmartInitial instead of first regular
        {JUN_04, JUN_03, AUG_30, P1M, null, null, BDA, JUN_17, AUG_17, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, AUG_30), SMART_INITIAL, AUG_17, null},
        // original schedule had double explicit stubs and BOTH, new schedule uses SmartInitial instead of first regular
        {JUN_04, JUN_03, AUG_30, P1M, STUB_BOTH, null, BDA, JUN_17, AUG_17, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17, AUG_30), SMART_INITIAL, AUG_17, null},
        // original schedule had first regular date, new schedule just uses SmartInitial
        {JUN_04, JUN_03, AUG_17, P1M, null, null, BDA, JUN_17, null, null,
            list(JUN_04, JUN_17, JUL_17, AUG_17), SMART_INITIAL, null, null},
        // original schedule had last regular date and unnecessary final stub convention
        {JUN_04, JUN_17, AUG_04, P1M, SHORT_FINAL, null, BDA, null, JUL_17, null,
            list(JUN_04, JUN_17, JUL_17, AUG_04), SMART_INITIAL, JUL_17, null},
        // original schedule was final, but resulted in Term schedule, new schedule retains the stub convention
        {JUN_04, JUL_17, AUG_17, P1M, SHORT_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, AUG_17), SHORT_FINAL, null, null},
        // cannot set start after end
        {SEP_04, JUN_17, AUG_17, P1M, null, DAY_17, BDA, JUN_17, AUG_17, BDA_JPY_P, null, null, null, null},
  };
}


/* --- GlobalHolidayCalendarsTest expected-holiday tables --------------------
 *
 * Source: the 25 `data_*()` providers of
 * modules/basics/src/test/java/com/opengamma/strata/basics/date/GlobalHolidayCalendarsTest.java:245-1188,
 * with `ImmutableList.of(` written as the `list(` helper of Section 4.
 *
 * WHY THEY ARE HERE. Without them the holiday fixture's only cross-checks are
 * the implementation talking to itself: `holidays(from, to)` agreeing with a
 * loop over `isHoliday`, and a sorted list being sorted. Both hold however
 * wrong the calendar is. These tables are the independent statement - 201
 * year-rows of dates sourced from legislation, exchange notices and
 * central-bank calendars, each cited in the Java test - and
 * `checkHolidayAgainstJavaTable` applies that test's own rule to them:
 *
 *     isHoliday = (holidays.contains(date) || Saturday || Sunday)
 *                 && !workingDays.contains(date)
 *
 * The Saturday/Sunday term belongs to the rule and not to the calendar:
 * Budapest declares Sunday as its only weekend day and carries its Saturdays as
 * explicit holidays, while its Java test asserts against Sat+Sun minus its six
 * working Saturdays. That is the comparison this check makes for every
 * calendar.
 * ---------------------------------------------------------------------------
 */

  public static Object[][] data_gblo() {
    return new Object[][] {
        // Whitsun, Last Mon Aug - http://hansard.millbanksystems.com/commons/1964/mar/04/staggered-holidays
        {1965, mds(1965, md(4, 16), md(4, 19), md(6, 7), md(8, 30), md(12, 27), md(12, 28))},
        // Whitsun May - http://hansard.millbanksystems.com/commons/1964/mar/04/staggered-holidays
        // 29th Aug - http://hansard.millbanksystems.com/written_answers/1965/nov/25/august-bank-holiday
        {1966, mds(1966, md(4, 8), md(4, 11), md(5, 30), md(8, 29), md(12, 26), md(12, 27))},
        // 29th May, 28th Aug - http://hansard.millbanksystems.com/written_answers/1965/jun/03/bank-holidays-1967-and-1968
        {1967, mds(1967, md(3, 24), md(3, 27), md(5, 29), md(8, 28), md(12, 25), md(12, 26))},
        // 3rd Jun, 2nd Sep - http://hansard.millbanksystems.com/written_answers/1965/jun/03/bank-holidays-1967-and-1968
        {1968, mds(1968, md(4, 12), md(4, 15), md(6, 3), md(9, 2), md(12, 25), md(12, 26))},
        // 26th May, 1st Sep - http://hansard.millbanksystems.com/written_answers/1967/mar/21/bank-holidays-1969-dates
        {1969, mds(1969, md(4, 4), md(4, 7), md(5, 26), md(9, 1), md(12, 25), md(12, 26))},
        // 25th May, 31st Aug - http://hansard.millbanksystems.com/written_answers/1967/jul/28/bank-holidays
        {1970, mds(1970, md(3, 27), md(3, 30), md(5, 25), md(8, 31), md(12, 25), md(12, 28))},
        // applying rules
        {1971, mds(1971, md(4, 9), md(4, 12), md(5, 31), md(8, 30), md(12, 27), md(12, 28))},
        {2009, mds(2009, md(1, 1), md(4, 10), md(4, 13), md(5, 4), md(5, 25), md(8, 31), md(12, 25), md(12, 28))},
        {2010, mds(2010, md(1, 1), md(4, 2), md(4, 5), md(5, 3), md(5, 31), md(8, 30), md(12, 27), md(12, 28))},
        // https://www.gov.uk/bank-holidays
        {2012, mds(2012, md(1, 2), md(4, 6), md(4, 9), md(5, 7), md(6, 4), md(6, 5), md(8, 27), md(12, 25), md(12, 26))},
        {2013, mds(2013, md(1, 1), md(3, 29), md(4, 1), md(5, 6), md(5, 27), md(8, 26), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 5), md(5, 26), md(8, 25), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 4), md(5, 25), md(8, 31), md(12, 25), md(12, 28))},
        {2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 2), md(5, 30), md(8, 29), md(12, 26), md(12, 27))},
        {2020, mds(2020, md(1, 1), md(4, 10), md(4, 13), md(5, 8), md(5, 25), md(8, 31), md(12, 25), md(12, 28))},
        {2022, mds(2022, md(1, 3), md(4, 15), md(4, 18), md(5, 2), md(6, 2), md(6, 3), md(8, 29), md(9, 19), md(12, 26), md(12, 27))},
        {2023, mds(2023, md(1, 2), md(4, 7), md(4, 10), md(5, 1), md(5, 8), md(5, 29), md(8, 28), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_frpa() {
    return new Object[][] {
        // dates not shifted if fall on a weekend
        {2003, mds(2003, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(5, 8), md(5, 29),
            md(6, 9), md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2004, mds(2004, md(1, 1), md(4, 9), md(4, 12), md(5, 1), md(5, 8), md(5, 20), md(5, 31),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2005, mds(2005, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 8),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2006, mds(2006, md(1, 1), md(4, 14), md(4, 17), md(5, 1), md(5, 8), md(5, 25),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2007, mds(2007, md(1, 1), md(4, 6), md(4, 9), md(5, 1), md(5, 8), md(5, 17),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2008, mds(2008, md(1, 1), md(3, 21), md(3, 24), md(5, 1), md(5, 8), md(5, 12), md(5, 24),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},

        {2012, mds(2012, md(1, 1), md(4, 6), md(4, 9), md(5, 1), md(5, 8), md(5, 17),
            md(5, 28), md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2013, mds(2013, md(1, 1), md(3, 29), md(4, 1), md(5, 1), md(5, 8), md(5, 9), md(5, 20),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(5, 8), md(5, 29),
            md(6, 9), md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 1), md(5, 8), md(5, 14), md(5, 25),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
        {2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 8), md(5, 16),
            md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_defr() {
    return new Object[][] {
        // dates not shifted if fall on a weekend
        {2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(5, 29), md(6, 9), md(6, 19),
            md(10, 3), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 1), md(5, 14), md(5, 25), md(6, 4),
            md(10, 3), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 16), md(5, 26),
            md(10, 3), md(12, 25), md(12, 26), md(12, 31))},
        {2017, mds(2017, md(1, 1), md(4, 14), md(4, 17), md(5, 1), md(5, 25), md(6, 5), md(6, 15),
            md(10, 3), md(10, 31), md(12, 25), md(12, 26), md(12, 31))},
    };
  }

  public static Object[][] data_chzu() {
    return new Object[][] {
        // dates not shifted if fall on a weekend
        {2012, mds(2012, md(1, 1), md(1, 2), md(4, 6), md(4, 9), md(5, 1), md(5, 17), md(5, 28),
            md(8, 1), md(12, 25), md(12, 26))},
        {2013, mds(2013, md(1, 1), md(1, 2), md(3, 29), md(4, 1), md(5, 1), md(5, 9), md(5, 20),
            md(8, 1), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(1, 2), md(4, 18), md(4, 21), md(5, 1), md(5, 29), md(6, 9),
            md(8, 1), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(1, 2), md(4, 3), md(4, 6), md(5, 1), md(5, 14), md(5, 25),
            md(8, 1), md(12, 25), md(12, 26))},
        {2016, mds(2016, md(1, 1), md(1, 2), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 16),
            md(8, 1), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_euta() {
    return new Object[][] {
        // 1997 - 1998 (testing phase), Jan 1, christmas day
        {1997, mds(1997, md(1, 1), md(12, 25))},
        {1998, mds(1998, md(1, 1), md(12, 25))},
        // in 1999, Jan 1, christmas day, Dec 26, Dec 31
        {1999, mds(1999, md(1, 1), md(12, 25), md(12, 31))},
        // in 2000, Jan 1, good friday, easter monday, May 1, christmas day, Dec 26
        {2000, mds(2000, md(1, 1), md(4, 21), md(4, 24), md(5, 1), md(12, 25), md(12, 26))},
        // in 2001, Jan 1, good friday, easter monday, May 1, christmas day, Dec 26, Dec 31
        {2001, mds(2001, md(1, 1), md(4, 13), md(4, 16), md(5, 1), md(12, 25), md(12, 26), md(12, 31))},
        // from 2002, Jan 1, good friday, easter monday, May 1, christmas day, Dec 26
        {2002, mds(2002, md(1, 1), md(3, 29), md(4, 1), md(5, 1), md(12, 25), md(12, 26))},
        {2003, mds(2003, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(12, 25), md(12, 26))},
        // http://www.ecb.europa.eu/home/html/holidays.en.html
        {2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 1), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_usgs() {
    return new Object[][] {
        // http://www.sifma.org/uploadedfiles/research/statistics/statisticsfiles/misc-us-historical-holiday-market-recommendations-sifma.pdf?n=53384
        {1996, mds(1996, md(1, 1), md(1, 15), md(2, 19), md(4, 5), md(5, 27), md(7, 4),
            md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))},
        {1997, mds(1997, md(1, 1), md(1, 20), md(2, 17), md(3, 28), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {1998, mds(1998, md(1, 1), md(1, 19), md(2, 16), md(4, 10), md(5, 25), md(7, 3),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
        {1999, mds(1999, md(1, 1), md(1, 18), md(2, 15), md(4, 2), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 24))},
        {2000, mds(2000, md(1, 17), md(2, 21), md(4, 21), md(5, 29), md(7, 4),
            md(9, 4), md(10, 9), md(11, 23), md(12, 25))},
        {2001, mds(2001, md(1, 1), md(1, 15), md(2, 19), md(4, 13), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))},
        {2002, mds(2002, md(1, 1), md(1, 21), md(2, 18), md(3, 29), md(5, 27), md(7, 4),
            md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))},
        {2003, mds(2003, md(1, 1), md(1, 20), md(2, 17), md(4, 18), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2004, mds(2004, md(1, 1), md(1, 19), md(2, 16), md(4, 9), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 24))},
        {2005, mds(2005, md(1, 17), md(2, 21), md(3, 25), md(5, 30), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
        {2006, mds(2006, md(1, 2), md(1, 16), md(2, 20), md(4, 14), md(5, 29), md(7, 4),
            md(9, 4), md(10, 9), md(11, 23), md(12, 25))},
        {2007, mds(2007, md(1, 1), md(1, 15), md(2, 19), md(4, 6), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))},
        {2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(3, 21), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(4, 10), md(5, 25), md(7, 3),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
        {2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(4, 2), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 24))},
        {2011, mds(2011, md(1, 17), md(2, 21), md(4, 22), md(5, 30), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
        {2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(4, 6), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(10, 30), md(11, 12), md(11, 22), md(12, 25))},
        {2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(3, 29), md(5, 27), md(7, 4),
            md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))},
        {2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(4, 18), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(4, 3), md(5, 25), md(7, 3),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
    };
  }

  public static Object[][] data_usny() {
    return new Object[][] {
        // http://www.cs.ny.gov/attendance_leave/2012_legal_holidays.cfm
        {2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(5, 25), md(7, 4),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
        {2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 25))},
        {2011, mds(2011, md(1, 1), md(1, 17), md(2, 21), md(5, 30), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
        {2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))},
        {2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(5, 27), md(7, 4),
            md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))},
        {2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(5, 25), md(7, 4),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
        {2021, mds(2021, md(1, 1), md(1, 18), md(2, 15), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 25))},
        {2022, mds(2022, md(1, 1), md(1, 17), md(2, 21), md(5, 30), md(6, 20), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
    };
  }

  public static Object[][] data_nyfd() {
    return new Object[][] {
        // http://www.ny.frb.org/aboutthefed/holiday_schedule.html
        // http://web.archive.org/web/20080403230805/http://www.ny.frb.org/aboutthefed/holiday_schedule.html
        // http://web.archive.org/web/20100827003740/http://www.ny.frb.org/aboutthefed/holiday_schedule.html
        // http://web.archive.org/web/20031007222458/http://www.ny.frb.org/aboutthefed/holiday_schedule.html
        // http://www.federalreserve.gov/aboutthefed/k8.htm
        {2003, mds(2003, md(1, 1), md(1, 20), md(2, 17), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2004, mds(2004, md(1, 1), md(1, 19), md(2, 16), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25))},
        {2005, mds(2005, md(1, 17), md(2, 21), md(5, 30), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
        {2006, mds(2006, md(1, 2), md(1, 16), md(2, 20), md(5, 29), md(7, 4),
            md(9, 4), md(10, 9), md(11, 23), md(12, 25))},
        {2007, mds(2007, md(1, 1), md(1, 15), md(2, 19), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))},
        {2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(5, 25),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
        {2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(5, 31), md(7, 5),
            md(9, 6), md(10, 11), md(11, 11), md(11, 25))},
        {2011, mds(2011, md(1, 17), md(2, 21), md(5, 30), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
        {2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))},
        {2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(5, 27), md(7, 4),
            md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))},
        {2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(5, 26), md(7, 4),
            md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))},
        {2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(5, 25),
            md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))},
        {2016, mds(2016, md(1, 1), md(1, 18), md(2, 15), md(5, 30), md(7, 4),
            md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))},
        {2017, mds(2017, md(1, 2), md(1, 16), md(2, 20), md(5, 29), md(7, 4),
            md(9, 4), md(10, 9), md(11, 23), md(12, 25))},
        {2018, mds(2018, md(1, 1), md(1, 15), md(2, 19), md(5, 28), md(7, 4),
            md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))},
    };
  }

  public static Object[][] data_nyse() {
    return new Object[][] {
        // https://www.nyse.com/markets/hours-calendars
        // http://web.archive.org/web/20110320011340/http://www.nyse.com/about/newsevents/1176373643795.html?sa_campaign=/internal_ads/homepage/08262008holidays
        // http://web.archive.org/web/20080901164729/http://www.nyse.com/about/newsevents/1176373643795.html?sa_campaign=/internal_ads/homepage/08262008holidays
        {2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(3, 21), md(5, 26), md(7, 4),
            md(9, 1), md(11, 27), md(12, 25))},
        {2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(4, 10), md(5, 25), md(7, 3),
            md(9, 7), md(11, 26), md(12, 25))},
        {2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(4, 2), md(5, 31), md(7, 5),
            md(9, 6), md(11, 25), md(12, 24))},
        {2011, mds(2011, md(1, 1), md(1, 17), md(2, 21), md(4, 22), md(5, 30), md(7, 4),
            md(9, 5), md(11, 24), md(12, 26))},
        {2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(4, 6), md(5, 28), md(7, 4),
            md(9, 3), md(10, 30), md(11, 22), md(12, 25))},
        {2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(3, 29), md(5, 27), md(7, 4),
            md(9, 2), md(11, 28), md(12, 25))},
        {2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(4, 18), md(5, 26), md(7, 4),
            md(9, 1), md(11, 27), md(12, 25))},
        {2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(4, 3), md(5, 25), md(7, 3),
            md(9, 7), md(11, 26), md(12, 25))},
    };
  }

  public static Object[][] data_jpto() {
    return new Object[][] {
        // https://www.boj.or.jp/en/about/outline/holi.htm/
        // http://web.archive.org/web/20110513190217/http://www.boj.or.jp/en/about/outline/holi.htm/
        // https://www.japanspecialist.co.uk/travel-tips/national-holidays-in-japan/
        {1999, mds(1999, md(1, 1), md(1, 2), md(1, 3), md(1, 15), md(2, 11), md(3, 22), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 20), md(9, 15), md(9, 23), md(10, 11), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2000, mds(2000, md(1, 1), md(1, 2), md(1, 3), md(1, 10), md(2, 11), md(3, 20), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 20), md(9, 15), md(9, 23), md(10, 9), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2001, mds(2001, md(1, 1), md(1, 2), md(1, 3), md(1, 8), md(2, 12), md(3, 20), md(4, 30), md(5, 3), md(5, 4), md(5, 5),
            md(7, 20), md(9, 15), md(9, 24), md(10, 8), md(11, 3), md(11, 23), md(12, 24), md(12, 31))},
        {2002, mds(2002, md(1, 1), md(1, 2), md(1, 3), md(1, 14), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 6),
            md(7, 20), md(9, 16), md(9, 23), md(10, 14), md(11, 4), md(11, 23), md(12, 23), md(12, 31))},
        {2003, mds(2003, md(1, 1), md(1, 2), md(1, 3), md(1, 13), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 21), md(9, 15), md(9, 23), md(10, 13), md(11, 3), md(11, 24), md(12, 23), md(12, 31))},
        {2004, mds(2004, md(1, 1), md(1, 2), md(1, 3), md(1, 12), md(2, 11), md(3, 20), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 19), md(9, 20), md(9, 23), md(10, 11), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2005, mds(2005, md(1, 1), md(1, 2), md(1, 3), md(1, 10), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 18), md(9, 19), md(9, 23), md(10, 10), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2006, mds(2006, md(1, 1), md(1, 2), md(1, 3), md(1, 9), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 17), md(9, 18), md(9, 23), md(10, 9), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2011, mds(2011, md(1, 1), md(1, 2), md(1, 3), md(1, 10), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
            md(7, 18), md(9, 19), md(9, 23), md(10, 10), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2012, mds(2012, md(1, 1), md(1, 2), md(1, 3), md(1, 9), md(2, 11), md(3, 20), md(4, 30), md(5, 3), md(5, 4), md(5, 5),
            md(7, 16), md(9, 17), md(9, 22), md(10, 8), md(11, 3), md(11, 23), md(12, 24), md(12, 31))},
        {2013, mds(2013, md(1, 1), md(1, 2), md(1, 3), md(1, 14), md(2, 11), md(3, 20), md(4, 29),
            md(5, 3), md(5, 4), md(5, 5), md(5, 6),
            md(7, 15), md(9, 16), md(9, 23), md(10, 14), md(11, 4), md(11, 23), md(12, 23), md(12, 31))},
        {2014, mds(2014, md(1, 1), md(1, 2), md(1, 3), md(1, 13), md(2, 11), md(3, 21), md(4, 29),
            md(5, 3), md(5, 4), md(5, 5), md(5, 6),
            md(7, 21), md(9, 15), md(9, 23), md(10, 13), md(11, 3), md(11, 24), md(12, 23), md(12, 31))},
        {2015, mds(2015, md(1, 1), md(1, 2), md(1, 3), md(1, 12), md(2, 11), md(3, 21), md(4, 29),
            md(5, 3), md(5, 4), md(5, 5), md(5, 6),
            md(7, 20), md(9, 21), md(9, 22), md(9, 23), md(10, 12), md(11, 3), md(11, 23), md(12, 23), md(12, 31))},
        {2018, mds(2018, md(1, 1), md(1, 2), md(1, 3), md(1, 8), md(2, 12), md(3, 21), md(4, 30),
            md(5, 3), md(5, 4), md(5, 5), md(7, 16), md(8, 11), md(9, 17), md(9, 24),
            md(10, 8), md(11, 3), md(11, 23), md(12, 23), md(12, 24), md(12, 31))},
        {2019, mds(2019, md(1, 1), md(1, 2), md(1, 3), md(1, 14), md(2, 11), md(3, 21), md(4, 29), md(4, 30),
            md(5, 1), md(5, 2), md(5, 3), md(5, 4), md(5, 5), md(5, 6), md(7, 15), md(8, 12), md(9, 16), md(9, 23),
            md(10, 14), md(10, 22), md(11, 4), md(11, 23), md(12, 31))},
        {2020, mds(2020, md(1, 1), md(1, 2), md(1, 3), md(1, 13), md(2, 11), md(2, 24), md(3, 20), md(4, 29),
            md(5, 3), md(5, 4), md(5, 5), md(5, 6), md(7, 23), md(7, 24), md(8, 10), md(9, 21), md(9, 22),
            md(11, 3), md(11, 23), md(12, 31))},
        {2021, mds(2021, md(1, 1), md(1, 11), md(2, 11), md(2, 23), md(3, 20), md(4, 29),
            md(5, 3), md(5, 4), md(5, 5), md(7, 22), md(7, 23), md(8, 9), md(9, 20),
            md(9, 23), md(11, 3), md(11, 23), md(12, 31))},
    };
  }

  public static Object[][] data_ausy() {
    return new Object[][] {
        {2012, mds(2012, md(1, 1), md(1, 2), md(1, 26), md(4, 6), md(4, 7), md(4, 8), md(4, 9),
            md(4, 25), md(6, 11), md(8, 6), md(10, 1), md(12, 25), md(12, 26))},
        {2013, mds(2013, md(1, 1), md(1, 26), md(1, 28), md(3, 29), md(3, 30), md(3, 31), md(4, 1),
            md(4, 25), md(6, 10), md(8, 5), md(10, 7), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(1, 26), md(1, 27), md(4, 18), md(4, 19), md(4, 20), md(4, 21),
            md(4, 25), md(6, 9), md(8, 4), md(10, 6), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(1, 26), md(4, 3), md(4, 4), md(4, 5), md(4, 6), md(4, 25),
            md(6, 8), md(8, 3), md(10, 5), md(12, 25), md(12, 26), md(12, 27), md(12, 28))},
        {2016, mds(2016, md(1, 1), md(1, 26), md(3, 25), md(3, 26), md(3, 27), md(3, 28),
            md(4, 25), md(6, 13), md(8, 1), md(10, 3), md(12, 25), md(12, 26), md(12, 27))},
        {2017, mds(2017, md(1, 1), md(1, 2), md(1, 26), md(4, 14), md(4, 15), md(4, 16), md(4, 17),
            md(4, 25), md(6, 12), md(8, 7), md(10, 2), md(12, 25), md(12, 26))},
        {2022, mds(2022, md(1, 3), md(1, 26), md(4, 15), md(4, 18),
            md(4, 25), md(6, 13), md(8, 1), md(9, 22), md(10, 3), md(12, 26), md(12, 27))},
        {2026, mds(2026, md(1, 1), md(1, 26), md(4, 3), md(4, 6),
            md(4, 27), md(6, 8), md(8, 3), md(10, 5), md(12, 25), md(12, 28))},
    };
  }

  public static Object[][] data_brbd() {
    // http://www.planalto.gov.br/ccivil_03/leis/2002/L10607.htm
    return new Object[][] {
        {2013, mds(2013, md(1, 1), md(2, 11), md(2, 12), md(3, 29), md(4, 21), md(5, 1),
            md(5, 30), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))},
        {2014, mds(2014, md(1, 1), md(3, 3), md(3, 4), md(4, 18), md(4, 21), md(5, 1),
            md(6, 19), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))},
        {2015, mds(2015, md(1, 1), md(2, 16), md(2, 17), md(4, 3), md(4, 21), md(5, 1),
            md(6, 4), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))},
        {2016, mds(2016, md(1, 1), md(2, 8), md(2, 9), md(3, 25), md(4, 21), md(5, 1),
            md(5, 26), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))},
        {2024, mds(2024, md(1, 1), md(2, 12), md(2, 13), md(3, 29), md(4, 21), md(5, 1),
            md(5, 30), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(11,20), md(12, 25))}
    };
  }

  public static Object[][] data_camo() {
    // https://www.bankofcanada.ca/about/contact-information/bank-of-canada-holiday-schedule/
    // also indicate day after new year and boxing day, but no other sources for this
    return new Object[][] {
        {2017, mds(2017, md(1, 2), md(4, 14),
            md(5, 22), md(6, 26), md(7, 3), md(9, 4), md(10, 9), md(12, 25))},
        {2018, mds(2018, md(1, 1), md(3, 30),
            md(5, 21), md(6, 25), md(7, 2), md(9, 3), md(10, 8), md(12, 25))},
        {2022, mds(2022, md(1, 3), md(4, 15),
            md(5, 23), md(6, 24), md(7, 1), md(9, 5), md(9, 30), md(10, 10), md(12, 26))},
    };
  }

  public static Object[][] data_cato() {
    return new Object[][] {
        {2009, mds(2009, md(1, 1), md(2, 16), md(4, 10),
            md(5, 18), md(7, 1), md(8, 3), md(9, 7), md(10, 12), md(11, 11), md(12, 25), md(12, 28))},
        {2010, mds(2010, md(1, 1), md(2, 15), md(4, 2),
            md(5, 24), md(7, 1), md(8, 2), md(9, 6), md(10, 11), md(11, 11), md(12, 27), md(12, 28))},
        {2011, mds(2011, md(1, 3), md(2, 21), md(4, 22),
            md(5, 23), md(7, 1), md(8, 1), md(9, 5), md(10, 10), md(11, 11), md(12, 26), md(12, 27))},
        {2012, mds(2012, md(1, 2), md(2, 20), md(4, 6),
            md(5, 21), md(7, 2), md(8, 6), md(9, 3), md(10, 8), md(11, 12), md(12, 25), md(12, 26))},
        {2013, mds(2013, md(1, 1), md(2, 18), md(3, 29),
            md(5, 20), md(7, 1), md(8, 5), md(9, 2), md(10, 14), md(11, 11), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(2, 17), md(4, 18),
            md(5, 19), md(7, 1), md(8, 4), md(9, 1), md(10, 13), md(11, 11), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(2, 16), md(4, 3),
            md(5, 18), md(7, 1), md(8, 3), md(9, 7), md(10, 12), md(11, 11), md(12, 25), md(12, 28))},
        {2016, mds(2016, md(1, 1), md(2, 15), md(3, 25),
            md(5, 23), md(7, 1), md(8, 1), md(9, 5), md(10, 10), md(11, 11), md(12, 26), md(12, 27))},
        {2025, mds(2025, md(1, 1), md(2, 17), md(4, 18), md(5, 19),
            md(7, 1), md(8, 4), md(9, 1), md(9, 30), md(10, 13), md(11, 11), md(12, 25), md(12, 26))}
    };
  }

  public static Object[][] data_czpr() {
    // official data from Czech National Bank
    // https://www.cnb.cz/en/public/media_service/schedules/media_svatky.html
    return new Object[][] {
        {2008, mds(2008, md(1, 1), md(3, 24), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2009, mds(2009, md(1, 1), md(4, 13), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2010, mds(2010, md(1, 1), md(4, 5), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2011, mds(2011, md(1, 1), md(4, 25), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2012, mds(2012, md(1, 1), md(4, 9), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2013, mds(2013, md(1, 1), md(4, 1), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(4, 21), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(4, 6), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
        {2017, mds(2017, md(1, 1), md(4, 14), md(4, 17), md(5, 1), md(5, 8),
            md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_dkco() {
    // official data from Danish Bankers association via web archive
    return new Object[][] {
        {2013, mds(2013, md(1, 1), md(3, 28), md(3, 29), md(4, 1),
            md(4, 26), md(5, 9), md(5, 10), md(5, 20), md(6, 5), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2014, mds(2014, md(1, 1), md(4, 17), md(4, 18), md(4, 21),
            md(5, 16), md(5, 29), md(5, 30), md(6, 5), md(6, 9), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2015, mds(2015, md(1, 1), md(4, 2), md(4, 3), md(4, 6),
            md(5, 1), md(5, 14), md(5, 15), md(5, 25), md(6, 5), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2016, mds(2016, md(1, 1), md(3, 24), md(3, 25), md(3, 28),
            md(4, 22), md(5, 5), md(5, 6), md(5, 16), md(6, 5), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
    };
  }

  public static Object[][] data_hubu() {
    // http://www.mnb.hu/letoltes/bubor2.xls
    // http://holidays.kayaposoft.com/public_holidays.php?year=2013&country=hun&region=#
    return new Object[][] {
        {2012, mds(2012, md(3, 15), md(3, 16), md(4, 9), md(4, 30), md(5, 1), md(5, 28),
            md(8, 20), md(10, 22), md(10, 23), md(11, 1), md(11, 2), md(12, 24), md(12, 25), md(12, 26), md(12, 31)),
            list(date(2012, 3, 24), date(2012, 5, 5), date(2012, 10, 27),
                date(2012, 11, 10), date(2012, 12, 15), date(2012, 12, 29))},
        {2013, mds(2013, md(1, 1), md(3, 15), md(4, 1), md(5, 1), md(5, 20),
            md(8, 19), md(8, 20), md(10, 23), md(11, 1), md(12, 24), md(12, 25), md(12, 26), md(12, 27)),
            list(date(2013, 8, 24), date(2013, 12, 7), date(2013, 12, 21))},
        {2014, mds(2014, md(1, 1), md(3, 15), md(4, 21), md(5, 1), md(5, 2),
            md(6, 9), md(8, 20), md(10, 23), md(10, 24), md(12, 24), md(12, 25), md(12, 26)),
            list(date(2014, 5, 10), date(2014, 10, 18))},
        {2015, mds(2015, md(1, 1), md(1, 2), md(3, 15), md(4, 6), md(5, 1), md(5, 25),
            md(8, 20), md(8, 21), md(10, 23), md(12, 24), md(12, 25), md(12, 26)),
            list(date(2015, 1, 10), date(2015, 8, 8), date(2015, 12, 12))},
        {2016, mds(2016, md(1, 1), md(3, 14), md(3, 15), md(3, 28), md(5, 1), md(5, 16),
            md(10, 31), md(11, 1), md(12, 24), md(12, 25), md(12, 26)),
            list(date(2016, 3, 5), date(2016, 10, 15))},
        {2020, mds(2020, md(1, 1), md(3, 15), md(4, 10), md(4, 13), md(5, 1), md(6, 1),
            md(8, 20), md(8, 21), md(10, 23), md(12, 24), md(12, 25), md(12, 26)),
            list(date(2020, 8, 29), date(2020, 12, 12))},
    };
  }

  public static Object[][] data_mxmc() {
    // http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadro&idCuadro=CF111&locale=en
    return new Object[][] {
        {2012, mds(2012, md(1, 1), md(2, 6), md(3, 19), md(4, 5), md(4, 6),
            md(5, 1), md(9, 16), md(11, 2), md(11, 19), md(12, 12), md(12, 25))},
        {2013, mds(2013, md(1, 1), md(2, 4), md(3, 18), md(3, 28), md(3, 29),
            md(5, 1), md(9, 16), md(11, 2), md(11, 18), md(12, 12), md(12, 25))},
        {2014, mds(2014, md(1, 1), md(2, 3), md(3, 17), md(4, 17), md(4, 18),
            md(5, 1), md(9, 16), md(11, 2), md(11, 17), md(12, 12), md(12, 25))},
        {2015, mds(2015, md(1, 1), md(2, 2), md(3, 16), md(4, 2), md(4, 3),
            md(5, 1), md(9, 16), md(11, 2), md(11, 16), md(12, 12), md(12, 25))},
        {2016, mds(2016, md(1, 1), md(2, 1), md(3, 21), md(3, 24), md(3, 25),
            md(5, 1), md(9, 16), md(11, 2), md(11, 21), md(12, 12), md(12, 25))},
        {2024, mds(2024, md(1, 1), md(2, 5), md(3, 18), md(3, 28), md(3, 29),
            md(5, 1), md(9, 16), md(10, 1), md(11, 2), md(11, 18), md(12, 12), md(12, 25))},
    };
  }

  public static Object[][] data_noos() {
    // official data from Oslo Bors via web archive
    return new Object[][] {
        {2009, mds(2009, md(1, 1), md(4, 9), md(4, 10), md(4, 13),
            md(5, 1), md(5, 21), md(6, 1), md(12, 24), md(12, 25), md(12, 31))},
        {2011, mds(2011, md(4, 21), md(4, 22), md(4, 25),
            md(5, 17), md(6, 2), md(6, 13), md(12, 26))},
        {2012, mds(2012, md(4, 5), md(4, 6), md(4, 9),
            md(5, 1), md(5, 17), md(5, 28), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2013, mds(2013, md(1, 1), md(3, 28), md(3, 29), md(4, 1),
            md(5, 1), md(5, 9), md(5, 17), md(5, 20), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2014, mds(2014, md(1, 1), md(4, 17), md(4, 18), md(4, 21),
            md(5, 1), md(5, 17), md(5, 29), md(6, 9), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2015, mds(2015, md(1, 1), md(4, 2), md(4, 3), md(4, 6),
            md(5, 1), md(5, 14), md(5, 25), md(12, 24), md(12, 25), md(12, 31))},
        {2016, mds(2016, md(1, 1), md(3, 24), md(3, 25), md(3, 28),
            md(5, 5), md(5, 16), md(5, 17), md(12, 26))},
        {2017, mds(2017, md(4, 13), md(4, 14), md(4, 17),
            md(5, 1), md(5, 17), md(5, 25), md(6, 5), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_nzau() {
    // https://www.govt.nz/browse/work/public-holidays-and-work/public-holidays-and-anniversary-dates/
    // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/
    return new Object[][] {
        {2015, mds(2015, md(1, 1), md(1, 2), md(1, 26), md(2, 6), md(4, 3), md(4, 6),
            md(4, 27), md(6, 1), md(10, 26), md(12, 25), md(12, 28))},
        {2016, mds(2016, md(1, 1), md(1, 4), md(2, 1), md(2, 8), md(3, 25), md(3, 28),
            md(4, 25), md(6, 6), md(10, 24), md(12, 26), md(12, 27))},
        {2017, mds(2017, md(1, 2), md(1, 3), md(1, 30), md(2, 6), md(4, 14), md(4, 17),
            md(4, 25), md(6, 5), md(10, 23), md(12, 25), md(12, 26))},
        {2018, mds(2018, md(1, 1), md(1, 2), md(1, 29), md(2, 6), md(3, 30), md(4, 2),
            md(4, 25), md(6, 4), md(10, 22), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_nzwe() {
    // https://www.govt.nz/browse/work/public-holidays-and-work/public-holidays-and-anniversary-dates/
    // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/
    return new Object[][] {
        {2015, mds(2015, md(1, 1), md(1, 2), md(1, 19), md(2, 6), md(4, 3), md(4, 6),
            md(4, 27), md(6, 1), md(10, 26), md(12, 25), md(12, 28))},
        {2016, mds(2016, md(1, 1), md(1, 4), md(1, 25), md(2, 8), md(3, 25), md(3, 28),
            md(4, 25), md(6, 6), md(10, 24), md(12, 26), md(12, 27))},
        {2017, mds(2017, md(1, 2), md(1, 3), md(1, 23), md(2, 6), md(4, 14), md(4, 17),
            md(4, 25), md(6, 5), md(10, 23), md(12, 25), md(12, 26))},
        {2018, mds(2018, md(1, 1), md(1, 2), md(1, 22), md(2, 6), md(3, 30), md(4, 2),
            md(4, 25), md(6, 4), md(10, 22), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_nzbd() {
    // https://www.govt.nz/browse/work/public-holidays-and-work/public-holidays-and-anniversary-dates/
    // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/
    return new Object[][] {
        {2015, mds(2015, md(1, 1), md(1, 2), md(2, 6), md(4, 3), md(4, 6),
            md(4, 27), md(6, 1), md(10, 26), md(12, 25), md(12, 28))},
        {2016, mds(2016, md(1, 1), md(1, 4), md(2, 8), md(3, 25), md(3, 28),
            md(4, 25), md(6, 6), md(10, 24), md(12, 26), md(12, 27))},
        {2017, mds(2017, md(1, 2), md(1, 3), md(2, 6), md(4, 14), md(4, 17),
            md(4, 25), md(6, 5), md(10, 23), md(12, 25), md(12, 26))},
        {2018, mds(2018, md(1, 1), md(1, 2), md(2, 6), md(3, 30), md(4, 2),
            md(4, 25), md(6, 4), md(10, 22), md(12, 25), md(12, 26))},
        {2025, mds(2025, md(1, 1), md(1, 2), md(2, 6), md(4, 18),
            md(4, 21), md(4, 25), md(6, 2), md(6, 20), md(10, 27), md(12, 25), md(12, 26))},
    };
  }

  public static Object[][] data_plwa() {
    // based on government law data and stock exchange holidays
    return new Object[][] {
        {2013, mds(2013, md(1, 1), md(4, 1),
            md(5, 1), md(5, 3), md(5, 30), md(8, 15), md(11, 1), md(11, 11), md(12, 24), md(12, 25), md(12, 26))},
        {2014, mds(2014, md(1, 1), md(1, 6), md(4, 21),
            md(5, 1), md(6, 19), md(8, 15), md(11, 11), md(12, 24), md(12, 25), md(12, 26))},
        {2015, mds(2015, md(1, 1), md(1, 6), md(4, 6),
            md(5, 1), md(6, 4), md(11, 11), md(12, 24), md(12, 25), md(12, 31))},
        {2016, mds(2016, md(1, 1), md(1, 6), md(3, 28),
            md(5, 3), md(5, 26), md(8, 15), md(11, 1), md(11, 11), md(12, 26))},
        {2017, mds(2017, md(1, 6), md(4, 17),
            md(5, 1), md(5, 3), md(6, 15), md(8, 15), md(11, 1), md(12, 25), md(12, 26))},
        {2018, mds(2018, md(1, 1), md(1, 6), md(4, 1), md(4, 2), md(5, 1), md(5, 3),
            md(5, 20), md(5, 31), md(8, 15), md(11, 1), md(11, 11), md(11, 12), md(12, 24), md(12, 25), md(12, 26), md(12, 31))}
    };
  }

  public static Object[][] data_sest() {
    // official data from published fixing dates
    return new Object[][] {
        {2014, mds(2014, md(1, 1), md(1, 6), md(4, 18), md(4, 21),
            md(5, 1), md(5, 29), md(6, 6), md(6, 20), md(12, 24), md(12, 25), md(12, 26), md(12, 31))},
        {2015, mds(2015, md(1, 1), md(1, 6), md(4, 3), md(4, 6),
            md(5, 1), md(5, 14), md(6, 19), md(12, 24), md(12, 25), md(12, 31))},
        {2016, mds(2016, md(1, 1), md(1, 6), md(3, 25), md(3, 28),
            md(5, 5), md(6, 6), md(6, 24), md(12, 26))},
    };
  }

  public static Object[][] data_zajo() {
    // http://www.gov.za/about-sa/public-holidays
    // https://web.archive.org/web/20151230214958/http://www.gov.za/about-sa/public-holidays
    return new Object[][] {
        {2015, mds(2015, md(1, 1), md(3, 21), md(4, 3), md(4, 6), md(4, 27), md(5, 1),
            md(6, 16), md(8, 10), md(9, 24), md(12, 16), md(12, 25), md(12, 26))},
        {2016, mds(2016, md(1, 1), md(3, 21), md(3, 25), md(3, 28), md(4, 27), md(5, 2),
            md(6, 16), md(8, 3), md(8, 9), md(9, 24), md(12, 16), md(12, 26), md(12, 27))},
        {2017, mds(2017, md(1, 1), md(1, 2), md(3, 21), md(4, 14), md(4, 17), md(4, 27), md(5, 1),
            md(6, 16), md(8, 9), md(9, 25), md(12, 16), md(12, 16), md(12, 25), md(12, 26))},
    };
  }
/* Row counts measured in the Java sources at the lines cited above. */
Map<String, Integer> EXPECTED_TABLE_ROWS = new TreeMap<>();
EXPECTED_TABLE_ROWS.put("data_yearFraction", 201);
EXPECTED_TABLE_ROWS.put("data_days", 185);
EXPECTED_TABLE_ROWS.put("data_30U360", 22);
EXPECTED_TABLE_ROWS.put("data_30E360ISDA", 19);
EXPECTED_TABLE_ROWS.put("data_ACTACTAFB", 57);
EXPECTED_TABLE_ROWS.put("data_ACT365L", 12);
EXPECTED_TABLE_ROWS.put("data_generation", 87);
EXPECTED_TABLE_ROWS.put("data_replace", 11);

/* ===========================================================================
 * SECTION 6 - EVALUATION HELPERS
 *
 * Every Java call that can fail is funnelled through `Eval`, so that a row
 * either carries a value or carries an `error` field with the Java message.
 * Per the harness contract an `error` row must produce a Left on the Scala
 * side for an Either-returning API, or the documented ArgCheck exception for a
 * precondition API - so these rows are expectations in their own right, not
 * omissions.
 * ===========================================================================
 */

class Eval {
  final double value;
  final String error;
  /**
   * The type of the rejection, kept alongside its message so an expectation
   * can be stated as a TYPE rather than as a message prefix. A prefix match on
   * "IllegalArgumentException" rejects ScheduleException, which
   * `assertThatIllegalArgumentException()` accepts, so it would be stricter
   * than the Java test it stands for.
   */
  final Class<? extends Throwable> thrownType;

  private Eval(double value, String error, Class<? extends Throwable> thrownType) {
    this.value = value;
    this.error = error;
    this.thrownType = thrownType;
  }

  static Eval of(double value) {
    return new Eval(value, null, null);
  }

  static Eval failed(Throwable thrown) {
    return new Eval(Double.NaN, errorMessage(thrown), thrown.getClass());
  }

  boolean isError() {
    return error != null;
  }

  /** True when the rejection is assignable to the type the Java test names. */
  boolean failedWith(Class<? extends Throwable> expected) {
    return thrownType != null && expected.isAssignableFrom(thrownType);
  }
}

Eval evalYearFraction(DayCount dayCount, LocalDate start, LocalDate end, DayCount.ScheduleInfo info) {
  try {
    return Eval.of(info == null ? dayCount.yearFraction(start, end)
        : dayCount.yearFraction(start, end, info));
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    return Eval.failed(thrown);
  }
}

Eval evalRelativeYearFraction(DayCount dayCount, LocalDate start, LocalDate end,
    DayCount.ScheduleInfo info) {
  try {
    return Eval.of(info == null ? dayCount.relativeYearFraction(start, end)
        : dayCount.relativeYearFraction(start, end, info));
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    return Eval.failed(thrown);
  }
}

Eval evalDays(DayCount dayCount, LocalDate start, LocalDate end) {
  try {
    return Eval.of(dayCount.days(start, end));
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    return Eval.failed(thrown);
  }
}

/**
 * A slug for a row id: lower case, `+` spelled out so it survives, every other
 * run of non-alphanumeric characters collapsed to a single dash.
 *
 * `30E+/360` -> `30eplus-360` (NOT `30e-360`, which is a different day count),
 * `Bus/252 BRBD` -> `bus-252-brbd`, `Act/Act ICMA` -> `act-act-icma`.
 */
String slug(String raw) {
  StringBuilder sb = new StringBuilder();
  String lower = raw.toLowerCase(Locale.ENGLISH).replace("+", "plus");
  for (int i = 0; i < lower.length(); i++) {
    char c = lower.charAt(i);
    if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) {
      sb.append(c);
    } else if (sb.length() > 0 && sb.charAt(sb.length() - 1) != '-') {
      sb.append('-');
    }
  }
  while (sb.length() > 0 && sb.charAt(sb.length() - 1) == '-') {
    sb.setLength(sb.length() - 1);
  }
  return sb.toString();
}

/**
 * How many times each base id has been used, so a repeated base can be made
 * unique. LinkedHashMap because nothing here may depend on hash order.
 */
Map<String, Integer> DAYCOUNT_ID_USES = new LinkedHashMap<>();

/**
 * Makes a base id unique WITHIN THE DOCUMENT: the first use is the base
 * itself, the nth use is `base-n`.
 *
 * Bases do repeat - `data_ACTACTAFB` evaluates the same day count over the
 * same dates more than once, with different expectations - and an id has to
 * identify a row, so the emission-ordered counter is part of the id contract
 * rather than a workaround.
 */
String dayCountId(String base) {
  Integer previous = DAYCOUNT_ID_USES.get(base);
  int use = previous == null ? 1 : previous.intValue() + 1;
  DAYCOUNT_ID_USES.put(base, Integer.valueOf(use));
  return use == 1 ? base : base + "-" + use;
}

String dcId(String prefix, DayCount dayCount, LocalDate start, LocalDate end) {
  return prefix + "-" + slug(dayCount.getName()) + "-" + start + "-" + end;
}

String dcId(String prefix, DayCount dayCount, LocalDate start) {
  return prefix + "-" + slug(dayCount.getName()) + "-" + start;
}

/**
 * The skeleton every daycount row shares: TWELVE FIELDS, ALWAYS ALL TWELVE, in
 * this order.
 *
 * A field with nothing to say is `null`, never absent. That is what lets the
 * consumer decode one uniform schema instead of branching on which keys a row
 * happens to carry, and it makes the difference between "this row has no
 * relative year fraction" and "this row was emitted by an older script"
 * visible rather than silent. `set` on an existing key replaces the value and
 * keeps its position, so the four evaluated fields below are filled in by
 * `setYearFraction` / `setDays` without disturbing the order.
 */
JObject dayCountRow(String idBase, String source, DayCount dayCount, LocalDate start,
    LocalDate end, Jn scheduleInfo) {
  return new JObject()
      .set("id", jStr(dayCountId(idBase)))
      .set("source", jStr(source))
      .set("dayCount", jName(dayCount))
      .set("start", jDate(start))
      .set("end", jDate(end))
      .set("scheduleInfo", scheduleInfo)
      .set("yearFraction", jNull())
      .set("relativeYearFraction", jNull())
      .set("relativeYearFractionReversed", jNull())
      .set("days", jNull())
      .set("error", jNull())
      .set("daysError", jNull());
}

/**
 * Adds the evaluated year fraction to a row, as either `yearFraction` or
 * `error`, and cross-checks it when the Java table supplies an expectation.
 * An error where the Java table expects a value is itself a failure.
 */
void setYearFraction(String fixture, JObject row, String rowId, Eval eval, Double expected) {
  if (eval.isError()) {
    row.set("error", jStr(eval.error));
    CHECK.countErrorRow(fixture);
    if (expected != null) {
      CHECK.fail(fixture, rowId, "expected=" + expected + " but Java threw " + eval.error);
    }
    return;
  }
  row.set("yearFraction", jDbl(eval.value));
  if (expected != null) {
    CHECK.checkExact(fixture, rowId, expected.doubleValue(), eval.value);
  }
}

/**
 * Adds the evaluated day count to a row, as either `days` or `daysError`, and
 * cross-checks it when the Java table supplies an expectation.
 *
 * EVERY row carries this, not only the rows of `data_days`: `days` and
 * `yearFraction` are separate Java methods with separate preconditions - the
 * wrong-order rows fail both, the missing-schedule-information rows fail only
 * `yearFraction` - and a row that recorded just one of them could not state
 * that difference.
 */
void setDays(String fixture, JObject row, String rowId, Eval eval, Integer expected) {
  if (eval.isError()) {
    row.set("daysError", jStr(eval.error));
    CHECK.countErrorRow(fixture);
    if (expected != null) {
      CHECK.fail(fixture, rowId, "expected days=" + expected + " but Java threw " + eval.error);
    }
    return;
  }
  row.set("days", jInt((long) eval.value));
  if (expected != null) {
    CHECK.checkInt(fixture, rowId, expected.intValue(), (long) eval.value);
  }
}

void setDays(String fixture, JObject row, String rowId, DayCount dayCount, LocalDate start,
    LocalDate end) {
  setDays(fixture, row, rowId, evalDays(dayCount, start, end), null);
}

/* ===========================================================================
 * SECTION 7 - FIXTURE 1 OF 6: daycount-baseline.json
 * ===========================================================================
 */

String FX_DAYCOUNT = "daycount";

/**
 * The 21 standard day counts, in DayCounts declaration order.
 *
 * THE ORDER IS DECLARED HERE, NOT DISCOVERED. It decides the order of ~11,000
 * rows of this fixture, and neither available way of discovering it is
 * specified: `Class.getDeclaredFields` returns fields in no defined order (the
 * JLS and the javadoc both say so - today's JVM happens to return declaration
 * order, which is exactly the kind of accident that makes a baseline
 * irreproducible on another JDK 21 build), and
 * `DayCount.extendedEnum().lookupAllNormalized()` is keyed by name in the
 * registry's own order, which is neither declaration order nor alphabetical
 * (measured: it starts `Act/365L, Act/Act ISDA, NL/360, ...`).
 *
 * So the list below states the order explicitly - the constant declarations of
 * DayCounts.java in file order - and each name is resolved through the public
 * registry. Reflection then verifies SET EQUALITY against the holder: a
 * constant this list does not name, or a name the holder no longer declares,
 * aborts the capture instead of silently dropping or reordering a day count.
 *
 * Source: modules/basics/src/main/java/com/opengamma/strata/basics/date/DayCounts.java
 * (StandardDayCounts.values(), which the Java test uses, is package-private and
 * unreachable from a .jsh script - Trap 1.)
 */
String[] STANDARD_DAY_COUNT_NAMES = {
    "1/1",
    "Act/Act ISDA",
    "Act/Act ICMA",
    "Act/Act AFB",
    "Act/Act Year",
    "Act/365 Actual",
    "Act/365L",
    "Act/360",
    "Act/364",
    "Act/365F",
    "Act/365.25",
    "NL/360",
    "NL/365",
    "30/360 ISDA",
    "30U/360",
    "30U/360 EOM",
    "30/360 PSA",
    "30E/360 ISDA",
    "30E/360",
    "30E+/360",
    "30E/365",
};

/** Resolved once: the list is read on every row of the data_types sweeps. */
List<DayCount> STANDARD_DAY_COUNTS = null;

List<DayCount> standardDayCounts() {
  if (STANDARD_DAY_COUNTS != null) {
    return STANDARD_DAY_COUNTS;
  }
  List<DayCount> ordered = new ArrayList<>();
  for (String name : STANDARD_DAY_COUNT_NAMES) {
    ordered.add(DayCount.of(name));
  }
  // Completeness: the declared list and the holder's public constants must be
  // the same set. Sorted names on both sides, so the comparison itself does
  // not depend on either order.
  List<String> declared = new ArrayList<>();
  for (DayCount dayCount : ordered) {
    declared.add(dayCount.getName());
  }
  List<String> reflected = new ArrayList<>();
  for (Field field : DayCounts.class.getDeclaredFields()) {
    if (Modifier.isPublic(field.getModifiers()) && Modifier.isStatic(field.getModifiers())
        && DayCount.class.equals(field.getType())) {
      try {
        reflected.add(((DayCount) field.get(null)).getName());
      } catch (IllegalAccessException ex) {
        throw new IllegalStateException("Cannot read DayCounts." + field.getName(), ex);
      }
    }
  }
  Collections.sort(declared);
  Collections.sort(reflected);
  CHECK.checkEquals(FX_DAYCOUNT, "DayCounts constants", reflected, declared);
  STANDARD_DAY_COUNTS = ordered;
  return STANDARD_DAY_COUNTS;
}

/** The 21 standard day counts plus Bus/252 BRBD, the grid's 22 subjects. */
List<DayCount> gridDayCounts() {
  List<DayCount> result = new ArrayList<>(standardDayCounts());
  // DayCount.ofBus252(HolidayCalendarId) is public and resolves the calendar
  // against ReferenceData.standard() internally.
  result.add(DayCount.ofBus252(HolidayCalendarId.of("BRBD")));
  return result;
}

/** data_yearFraction: 2-arg yearFraction, relative, and reversed relative. */
void emitDataYearFraction(JArray rows) {
  Object[][] table = data_yearFraction();
  CHECK.checkCount(FX_DAYCOUNT, "data_yearFraction rows",
      EXPECTED_TABLE_ROWS.get("data_yearFraction").intValue(), table.length);
  for (Object[] r : table) {
    DayCount dayCount = (DayCount) r[0];
    int y1 = (Integer) r[1];
    int m1 = (Integer) r[2];
    int d1 = (Integer) r[3];
    int y2 = (Integer) r[4];
    int m2 = (Integer) r[5];
    int d2 = (Integer) r[6];
    Object rawValue = r[7];
    // Sentinel resolution, identical to the Java consumer.
    double expected = (rawValue == SIMPLE_30_360) ? calc360(y1, m1, d1, y2, m2, d2)
        : ((Number) rawValue).doubleValue();
    LocalDate start = LocalDate.of(y1, m1, d1);
    LocalDate end = LocalDate.of(y2, m2, d2);
    String rowId = dayCount.getName() + " " + start + ".." + end;
    JObject row = dayCountRow(dcId("yf", dayCount, start, end),
        "DayCountTest.data_yearFraction", dayCount, start, end, jScheduleInfo(null));
    Eval yf = evalYearFraction(dayCount, start, end, null);
    setYearFraction(FX_DAYCOUNT, row, rowId, yf, Double.valueOf(expected));
    Eval rel = evalRelativeYearFraction(dayCount, start, end, null);
    if (!rel.isError()) {
      row.set("relativeYearFraction", jDbl(rel.value));
      CHECK.checkExact(FX_DAYCOUNT, rowId + " relative", expected, rel.value);
    }
    Eval relReversed = evalRelativeYearFraction(dayCount, end, start, null);
    if (!relReversed.isError()) {
      row.set("relativeYearFractionReversed", jDbl(relReversed.value));
      CHECK.checkExact(FX_DAYCOUNT, rowId + " relative reversed", -expected, relReversed.value);
    }
    setDays(FX_DAYCOUNT, row, rowId, dayCount, start, end);
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/** data_days: the integer day count, with the int-valued sentinel resolved. */
void emitDataDays(JArray rows) {
  Object[][] table = data_days();
  CHECK.checkCount(FX_DAYCOUNT, "data_days rows",
      EXPECTED_TABLE_ROWS.get("data_days").intValue(), table.length);
  for (Object[] r : table) {
    DayCount dayCount = (DayCount) r[0];
    int y1 = (Integer) r[1];
    int m1 = (Integer) r[2];
    int d1 = (Integer) r[3];
    int y2 = (Integer) r[4];
    int m2 = (Integer) r[5];
    int d2 = (Integer) r[6];
    int rawValue = ((Number) r[7]).intValue();
    // int VALUE comparison, exactly as the Java consumer does it.
    int expected = (rawValue == SIMPLE_30_360DAYS) ? calc360Days(y1, m1, d1, y2, m2, d2) : rawValue;
    LocalDate start = LocalDate.of(y1, m1, d1);
    LocalDate end = LocalDate.of(y2, m2, d2);
    String rowId = dayCount.getName() + " days " + start + ".." + end;
    JObject row = dayCountRow(dcId("days", dayCount, start, end), "DayCountTest.data_days",
        dayCount, start, end, jScheduleInfo(null));
    setYearFraction(FX_DAYCOUNT, row, rowId, evalYearFraction(dayCount, start, end, null), null);
    setDays(FX_DAYCOUNT, row, rowId, evalDays(dayCount, start, end), Integer.valueOf(expected));
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/**
 * data_30U360: four evaluations per row. The third one is the trap - the
 * THIRTY_360_ISDA case is asserted against the NON-EOM column even though it
 * is evaluated with Info(true), because that day count ignores the flag.
 */
void emitData30U360(JArray rows) {
  Object[][] table = data_30U360();
  CHECK.checkCount(FX_DAYCOUNT, "data_30U360 rows",
      EXPECTED_TABLE_ROWS.get("data_30U360").intValue(), table.length);
  for (Object[] r : table) {
    int y1 = (Integer) r[0];
    int m1 = (Integer) r[1];
    int d1 = (Integer) r[2];
    int y2 = (Integer) r[3];
    int m2 = (Integer) r[4];
    int d2 = (Integer) r[5];
    Object rawNotEom = r[6];
    Object rawEom = r[7];
    double expectedNotEom = (rawNotEom == SIMPLE_30_360) ? calc360(y1, m1, d1, y2, m2, d2)
        : ((Number) rawNotEom).doubleValue();
    double expectedEom = (rawEom == SIMPLE_30_360) ? calc360(y1, m1, d1, y2, m2, d2)
        : ((Number) rawEom).doubleValue();
    LocalDate start = LocalDate.of(y1, m1, d1);
    LocalDate end = LocalDate.of(y2, m2, d2);
    Info infoNotEom = new Info(false);
    Info infoEom = new Info(true);
    Object[][] cases = {
        {"DayCountTest.test_yearFraction_30U360_notEom", THIRTY_U_360, infoNotEom, expectedNotEom},
        {"DayCountTest.test_yearFraction_30U360_eom", THIRTY_U_360, infoEom, expectedEom},
        {"DayCountTest.test_yearFraction_30360ISDA", THIRTY_360_ISDA, infoEom, expectedNotEom},
        {"DayCountTest.test_yearFraction_30U360EOM", THIRTY_U_360_EOM, infoEom, expectedEom},
    };
    for (Object[] c : cases) {
      String source = (String) c[0];
      DayCount dayCount = (DayCount) c[1];
      Info info = (Info) c[2];
      double expected = (Double) c[3];
      String rowId = source + " " + dayCount.getName() + " " + start + ".." + end + " eom="
          + info.rawEom();
      JObject row = dayCountRow(dcId(info.rawEom() ? "u360-eom" : "u360-noteom", dayCount, start,
          end), source, dayCount, start, end, jScheduleInfo(info));
      setYearFraction(FX_DAYCOUNT, row, rowId, evalYearFraction(dayCount, start, end, info),
          Double.valueOf(expected));
      setDays(FX_DAYCOUNT, row, rowId, dayCount, start, end);
      rows.add(row);
      CHECK.countRow(FX_DAYCOUNT);
    }
  }
}

/** data_30E360ISDA: the non-maturity and maturity evaluations. */
void emitData30E360Isda(JArray rows) {
  Object[][] table = data_30E360ISDA();
  CHECK.checkCount(FX_DAYCOUNT, "data_30E360ISDA rows",
      EXPECTED_TABLE_ROWS.get("data_30E360ISDA").intValue(), table.length);
  for (Object[] r : table) {
    int y1 = (Integer) r[0];
    int m1 = (Integer) r[1];
    int d1 = (Integer) r[2];
    int y2 = (Integer) r[3];
    int m2 = (Integer) r[4];
    int d2 = (Integer) r[5];
    Object rawNotMaturity = r[6];
    Object rawMaturity = r[7];
    LocalDate start = LocalDate.of(y1, m1, d1);
    LocalDate end = LocalDate.of(y2, m2, d2);
    double expectedNotMaturity = (rawNotMaturity == SIMPLE_30_360)
        ? calc360(y1, m1, d1, y2, m2, d2) : ((Number) rawNotMaturity).doubleValue();
    double expectedMaturity = (rawMaturity == SIMPLE_30_360)
        ? calc360(y1, m1, d1, y2, m2, d2) : ((Number) rawMaturity).doubleValue();
    // The Java test passes `new Info(false)` for the not-maturity case, whose
    // getEndDate() returns null - and 30E/360 ISDA only compares the second
    // date against it, so a null end date means "not the maturity date" and the
    // value is computed.
    //
    // A row cannot state a numeric expectation against ABSENT schedule
    // information, because absent schedule information is itself an expectation
    // of failure for this day count (see the missing-schedule-information
    // rows). So the not-maturity case is captured with a CONCRETE end date one
    // day AFTER the period end: still not the maturity date, so Java takes the
    // same branch and produces exactly the value its table asserts, while the
    // row states schedule information a consumer can evaluate.
    Info infoNotMaturity = new Info(null, end.plusDays(1), null, false, null);
    Info infoMaturity = new Info(null, end, null, false, P3M);
    JObject rowNotMaturity = dayCountRow(
        dcId("e360i-notmaturity", THIRTY_E_360_ISDA, start, end),
        "DayCountTest.test_yearFraction_30E360ISDA_notMaturity",
        THIRTY_E_360_ISDA, start, end, jScheduleInfo(infoNotMaturity));
    setYearFraction(FX_DAYCOUNT, rowNotMaturity,
        "30E360ISDA notMaturity " + start + ".." + end,
        evalYearFraction(THIRTY_E_360_ISDA, start, end, infoNotMaturity),
        Double.valueOf(expectedNotMaturity));
    setDays(FX_DAYCOUNT, rowNotMaturity, "30E360ISDA notMaturity " + start + ".." + end,
        THIRTY_E_360_ISDA, start, end);
    rows.add(rowNotMaturity);
    CHECK.countRow(FX_DAYCOUNT);
    JObject rowMaturity = dayCountRow(dcId("e360i-maturity", THIRTY_E_360_ISDA, start, end),
        "DayCountTest.test_yearFraction_30E360ISDA_maturity",
        THIRTY_E_360_ISDA, start, end, jScheduleInfo(infoMaturity));
    setYearFraction(FX_DAYCOUNT, rowMaturity,
        "30E360ISDA maturity " + start + ".." + end,
        evalYearFraction(THIRTY_E_360_ISDA, start, end, infoMaturity),
        Double.valueOf(expectedMaturity));
    setDays(FX_DAYCOUNT, rowMaturity, "30E360ISDA maturity " + start + ".." + end,
        THIRTY_E_360_ISDA, start, end);
    rows.add(rowMaturity);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/** data_ACTACTAFB: 2-arg year fraction on ACT_ACT_AFB. */
void emitDataActActAfb(JArray rows) {
  Object[][] table = data_ACTACTAFB();
  CHECK.checkCount(FX_DAYCOUNT, "data_ACTACTAFB rows",
      EXPECTED_TABLE_ROWS.get("data_ACTACTAFB").intValue(), table.length);
  for (Object[] r : table) {
    LocalDate start = LocalDate.of((Integer) r[0], (Integer) r[1], (Integer) r[2]);
    LocalDate end = LocalDate.of((Integer) r[3], (Integer) r[4], (Integer) r[5]);
    double expected = ((Number) r[6]).doubleValue();
    JObject row = dayCountRow(dcId("afb", ACT_ACT_AFB, start, end),
        "DayCountTest.data_ACTACTAFB", ACT_ACT_AFB, start, end, jScheduleInfo(null));
    setYearFraction(FX_DAYCOUNT, row, "ACTACTAFB " + start + ".." + end,
        evalYearFraction(ACT_ACT_AFB, start, end, null), Double.valueOf(expected));
    setDays(FX_DAYCOUNT, row, "ACTACTAFB " + start + ".." + end, ACT_ACT_AFB, start, end);
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/** data_ACT365L: evaluated with Info(null, null, periodEnd, false, frequency). */
void emitDataAct365L(JArray rows) {
  Object[][] table = data_ACT365L();
  CHECK.checkCount(FX_DAYCOUNT, "data_ACT365L rows",
      EXPECTED_TABLE_ROWS.get("data_ACT365L").intValue(), table.length);
  for (Object[] r : table) {
    LocalDate start = LocalDate.of((Integer) r[0], (Integer) r[1], (Integer) r[2]);
    LocalDate end = LocalDate.of((Integer) r[3], (Integer) r[4], (Integer) r[5]);
    Frequency frequency = (Frequency) r[6];
    LocalDate periodEnd = LocalDate.of((Integer) r[7], (Integer) r[8], (Integer) r[9]);
    double expected = ((Number) r[10]).doubleValue();
    Info info = new Info(null, null, periodEnd, false, frequency);
    String rowId = "ACT365L " + start + ".." + end + " " + frequency + " pe=" + periodEnd;
    JObject row = dayCountRow(
        dcId("l365-" + slug(frequency.toString()) + "-pe-" + periodEnd, ACT_365L, start, end),
        "DayCountTest.data_ACT365L", ACT_365L, start, end, jScheduleInfo(info));
    setYearFraction(FX_DAYCOUNT, row, rowId,
        evalYearFraction(ACT_365L, start, end, info), Double.valueOf(expected));
    setDays(FX_DAYCOUNT, row, rowId, ACT_365L, start, end);
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/**
 * Adds one Info-based case.
 *
 * `source` names the DayCountTest `@Test` method the case belongs to. A null
 * `info` selects the two-argument `yearFraction` overload, which is the one the
 * ACT_ACT_ISDA and ACT_ACT_AFB assertions of the ISDA cases make.
 */
void addInfoCase(JArray rows, String source, DayCount dayCount, LocalDate start, LocalDate end,
    Info info, double expected) {
  String method = source.substring(source.indexOf(".test_") + ".test_".length());
  // Two families of the same shape: the ICMA stub cases and the official ISDA
  // test cases. The id says which, so a row is traceable to its Java method
  // without reading `source`.
  String prefix = method.startsWith("actAct_isdaTestCase") ? "isda" : "icma";
  String rowId = source + " " + dayCount.getName() + " " + start + ".." + end;
  JObject row = dayCountRow(dcId(prefix + "-" + slug(method), dayCount, start, end), source,
      dayCount, start, end, jScheduleInfo(info));
  setYearFraction(FX_DAYCOUNT, row, rowId,
      evalYearFraction(dayCount, start, end, info), Double.valueOf(expected));
  setDays(FX_DAYCOUNT, row, rowId, dayCount, start, end);
  rows.add(row);
  CHECK.countRow(FX_DAYCOUNT);
}

/**
 * The Info-based ICMA and official-ISDA cases: one row per assertion of
 * DayCountTest:917-1112.
 *
 * Each expected value is the arithmetic expression that test states, left as an
 * expression so a row stays readable against its source, and every one is
 * checked for exact equality.
 */
void emitInfoCases(JArray rows) {
  // test_actActIcma_singlePeriod (2 assertions)
  LocalDate spStart = LocalDate.of(2003, 11, 1);
  LocalDate spEnd = LocalDate.of(2004, 5, 1);
  Info spInfo = new Info(spStart, spEnd, spEnd, true, P6M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_singlePeriod", ACT_ACT_ICMA, spStart,
      spEnd.minusDays(1), spInfo, 181d / (182d * 2d));
  addInfoCase(rows, "DayCountTest.test_actActIcma_singlePeriod", ACT_ACT_ICMA, spStart, spEnd,
      spInfo, 182d / (182d * 2d));

  // test_actActIcma_longInitialStub_eomFlagEom_short / _long
  LocalDate lisStart = LocalDate.of(2011, 10, 1);
  LocalDate lisPeriodEnd = LocalDate.of(2012, 2, 29);
  Info lisInfoEom = new Info(lisStart, lisPeriodEnd.plus(P3M), lisPeriodEnd, true, P3M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_longInitialStub_eomFlagEom_short", ACT_ACT_ICMA,
      lisStart, LocalDate.of(2011, 11, 12), lisInfoEom, 42d / (91d * 4d));
  addInfoCase(rows, "DayCountTest.test_actActIcma_longInitialStub_eomFlagEom_long", ACT_ACT_ICMA,
      lisStart, LocalDate.of(2012, 1, 12), lisInfoEom, (60d / (91d * 4d)) + (43d / (91d * 4d)));

  // test_actActIcma_veryLongInitialStub_eomFlagEom_short / _mid
  LocalDate vlisStart = LocalDate.of(2011, 7, 1);
  LocalDate vlisPeriodEnd = LocalDate.of(2012, 2, 29);
  Info vlisInfoEom = new Info(vlisStart, vlisPeriodEnd.plus(P3M), vlisPeriodEnd, true, P3M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_veryLongInitialStub_eomFlagEom_short",
      ACT_ACT_ICMA, vlisStart, LocalDate.of(2011, 8, 12), vlisInfoEom, 42d / (92d * 4d));
  addInfoCase(rows, "DayCountTest.test_actActIcma_veryLongInitialStub_eomFlagEom_mid",
      ACT_ACT_ICMA, vlisStart, LocalDate.of(2011, 11, 12), vlisInfoEom,
      (61d / (92d * 4d)) + (73d / (91d * 4d)));

  // test_actActIcma_longInitialStub_notEomFlagEom_short / _long
  Info lisInfoNotEom = new Info(lisStart, lisPeriodEnd.plus(P3M), lisPeriodEnd, false, P3M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_longInitialStub_notEomFlagEom_short",
      ACT_ACT_ICMA, lisStart, LocalDate.of(2011, 11, 12), lisInfoNotEom, 42d / (92d * 4d));
  addInfoCase(rows, "DayCountTest.test_actActIcma_longInitialStub_notEomFlagEom_long",
      ACT_ACT_ICMA, lisStart, LocalDate.of(2012, 1, 12), lisInfoNotEom,
      (59d / (92d * 4d)) + (44d / (92d * 4d)));

  // test_actActIcma_longFinalStub_eomFlagEom_short / _long
  LocalDate lfsStart = LocalDate.of(2011, 8, 31);
  LocalDate lfsPeriodEnd = LocalDate.of(2012, 1, 31);
  Info lfsInfoEom = new Info(lfsStart.minus(P3M), lfsPeriodEnd, lfsPeriodEnd, true, P3M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_longFinalStub_eomFlagEom_short", ACT_ACT_ICMA,
      lfsStart, LocalDate.of(2011, 11, 12), lfsInfoEom, 73d / (91d * 4d));
  addInfoCase(rows, "DayCountTest.test_actActIcma_longFinalStub_eomFlagEom_long", ACT_ACT_ICMA,
      lfsStart, LocalDate.of(2012, 1, 12), lfsInfoEom, (91d / (91d * 4d)) + (43d / (91d * 4d)));

  // test_actActIcma_longFinalStub_notEomFlagEom_short / _long
  LocalDate lfsnStart = LocalDate.of(2012, 2, 29);
  LocalDate lfsnPeriodEnd = LocalDate.of(2012, 7, 31);
  Info lfsnInfo = new Info(lfsnStart.minus(P3M), lfsnPeriodEnd, lfsnPeriodEnd, false, P3M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_longFinalStub_notEomFlagEom_short", ACT_ACT_ICMA,
      lfsnStart, LocalDate.of(2012, 4, 1), lfsnInfo, 32d / (90d * 4d));
  addInfoCase(rows, "DayCountTest.test_actActIcma_longFinalStub_notEomFlagEom_long", ACT_ACT_ICMA,
      lfsnStart, LocalDate.of(2012, 6, 1), lfsnInfo, (90d / (90d * 4d)) + (3d / (92d * 4d)));

  // test_actActIcma_middle
  Info middleInfo = new Info(LocalDate.of(2011, 12, 30), LocalDate.of(2012, 9, 30),
      LocalDate.of(2012, 6, 30), false, P3M);
  addInfoCase(rows, "DayCountTest.test_actActIcma_middle", ACT_ACT_ICMA, LocalDate.of(2012, 4, 10),
      LocalDate.of(2012, 5, 10), middleInfo, 30d / (4 * 92d));

  // test_actAct_isdaTestCase_normal (3 assertions)
  LocalDate normStart = LocalDate.of(2003, 11, 1);
  LocalDate normEnd = LocalDate.of(2004, 5, 1);
  Info normInfo = new Info(normStart, normEnd.plus(P6M), normEnd, true, P6M);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_normal", ACT_ACT_ISDA, normStart,
      normEnd, null, (61d / 365d) + (121d / 366d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_normal", ACT_ACT_ICMA, normStart,
      normEnd, normInfo, 182d / (182d * 2d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_normal", ACT_ACT_AFB, normStart,
      normEnd, null, 182d / 366d);

  // test_actAct_isdaTestCase_shortInitialStub (6 assertions)
  LocalDate sisStart = LocalDate.of(1999, 2, 1);
  LocalDate sisFirstRegular = LocalDate.of(1999, 7, 1);
  LocalDate sisEnd = LocalDate.of(2000, 7, 1);
  Info sisInfo1 = new Info(sisStart, sisEnd.plus(P12M), sisFirstRegular, true, P12M);
  Info sisInfo2 = new Info(sisStart, sisEnd.plus(P12M), sisEnd, true, P12M);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortInitialStub", ACT_ACT_ISDA,
      sisStart, sisFirstRegular, null, 150d / 365d);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortInitialStub", ACT_ACT_ICMA,
      sisStart, sisFirstRegular, sisInfo1, 150d / (365d * 1d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortInitialStub", ACT_ACT_AFB,
      sisStart, sisFirstRegular, null, 150d / 365d);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortInitialStub", ACT_ACT_ISDA,
      sisFirstRegular, sisEnd, null, (184d / 365d) + (182d / 366d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortInitialStub", ACT_ACT_ICMA,
      sisFirstRegular, sisEnd, sisInfo2, 366d / (366d * 1d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortInitialStub", ACT_ACT_AFB,
      sisFirstRegular, sisEnd, null, 366d / 366d);

  // test_actAct_isdaTestCase_longInitialStub (6 assertions)
  LocalDate lonStart = LocalDate.of(2002, 8, 15);
  LocalDate lonFirstRegular = LocalDate.of(2003, 7, 15);
  LocalDate lonEnd = LocalDate.of(2004, 1, 15);
  Info lonInfo1 = new Info(lonStart, lonEnd, lonFirstRegular, true, P6M);
  Info lonInfo2 = new Info(lonStart, lonEnd, lonEnd, true, P6M);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longInitialStub", ACT_ACT_ISDA,
      lonStart, lonFirstRegular, null, 334d / 365d);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longInitialStub", ACT_ACT_ICMA,
      lonStart, lonFirstRegular, lonInfo1, (181d / (181d * 2d)) + (153d / (184d * 2d)));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longInitialStub", ACT_ACT_AFB,
      lonStart, lonFirstRegular, null, 334d / 365d);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longInitialStub", ACT_ACT_ISDA,
      lonFirstRegular, lonEnd, null, (170d / 365d) + (14d / 366d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longInitialStub", ACT_ACT_ICMA,
      lonFirstRegular, lonEnd, lonInfo2, 184d / (184d * 2d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longInitialStub", ACT_ACT_AFB,
      lonFirstRegular, lonEnd, null, 184d / 365d);

  // test_actAct_isdaTestCase_shortFinalStub (6 assertions)
  LocalDate sfsStart = LocalDate.of(1999, 7, 30);
  LocalDate sfsLastRegular = LocalDate.of(2000, 1, 30);
  LocalDate sfsEnd = LocalDate.of(2000, 6, 30);
  Info sfsInfo1 = new Info(sfsStart, sfsEnd, sfsLastRegular, true, P6M);
  Info sfsInfo2 = new Info(sfsStart, sfsEnd, sfsEnd, true, P6M);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortFinalStub", ACT_ACT_ISDA, sfsStart,
      sfsLastRegular, null, (155d / 365d) + (29d / 366d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortFinalStub", ACT_ACT_ICMA, sfsStart,
      sfsLastRegular, sfsInfo1, 184d / (184d * 2d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortFinalStub", ACT_ACT_AFB, sfsStart,
      sfsLastRegular, null, 184d / 365d);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortFinalStub", ACT_ACT_ISDA,
      sfsLastRegular, sfsEnd, null, 152d / 366d);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortFinalStub", ACT_ACT_ICMA,
      sfsLastRegular, sfsEnd, sfsInfo2, 152d / (182d * 2d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_shortFinalStub", ACT_ACT_AFB,
      sfsLastRegular, sfsEnd, null, 152d / 366d);

  // test_actAct_isdaTestCase_longFinalStub (3 assertions)
  LocalDate lfStart = LocalDate.of(1999, 11, 30);
  LocalDate lfEnd = LocalDate.of(2000, 4, 30);
  Info lfInfo = new Info(lfStart.minus(P3M), lfEnd, lfEnd, true, P3M);
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longFinalStub", ACT_ACT_ISDA, lfStart,
      lfEnd, null, (32d / 365d) + (120d / 366d));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longFinalStub", ACT_ACT_ICMA, lfStart,
      lfEnd, lfInfo, (91d / (91d * 4d)) + (61d / (92d * 4)));
  addInfoCase(rows, "DayCountTest.test_actAct_isdaTestCase_longFinalStub", ACT_ACT_AFB, lfStart,
      lfEnd, null, 152d / 366d);
}

/**
 * test_actActYearVsIcma asserts that ACT_ACT_ICMA with a one-year Info equals
 * ACT_ACT_YEAR, over 400 start dates x 365 end dates.
 *
 * The full ~146,000-iteration equivalence is run here as a capture-time
 * self-check, because it is cheap and it is the strongest statement the Java
 * test makes. Only a deterministic subsample (every ACT_ACT_YEAR_SAMPLE_STRIDE
 * iterations) is emitted into the fixture, so 146,000 near-identical rows do
 * not swamp it. Both the stride and the resulting row count are reported.
 */
int ACT_ACT_YEAR_SAMPLE_STRIDE = 1000;

void emitActActYearVsIcma(JArray rows) {
  LocalDate start = LocalDate.of(2011, 1, 1);
  int iteration = 0;
  for (int i = 0; i < 400; i++) {
    for (int j = 0; j < 365; j++) {
      LocalDate end = start.plusDays(j);
      Info info = new Info(start, end, start.plusYears(1), false, P12M);
      Eval icma = evalYearFraction(ACT_ACT_ICMA, start, end, info);
      Eval year = evalYearFraction(ACT_ACT_YEAR, start, end, null);
      String rowId = "actActYearVsIcma " + start + ".." + end;
      if (icma.isError() || year.isError()) {
        CHECK.fail(FX_DAYCOUNT, rowId, "unexpected error icma=" + icma.error + " year=" + year.error);
      } else {
        CHECK.checkExact(FX_DAYCOUNT, rowId, year.value, icma.value);
        if (iteration % ACT_ACT_YEAR_SAMPLE_STRIDE == 0) {
          JObject row = dayCountRow(dcId("yvi", ACT_ACT_ICMA, start, end),
              "DayCountTest.test_actActYearVsIcma", ACT_ACT_ICMA, start, end,
              jScheduleInfo(info));
          row.set("yearFraction", jDbl(icma.value));
          setDays(FX_DAYCOUNT, row, rowId, ACT_ACT_ICMA, start, end);
          rows.add(row);
          CHECK.countRow(FX_DAYCOUNT);
        }
      }
      iteration++;
    }
    start = start.plusDays(1);
  }
}

/**
 * The generated grid: all 21 standard day counts plus Bus/252 BRBD, over
 * month-end and mid-month date pairs from 2010 to 2030, evaluated with simple
 * (default) schedule information.
 *
 * A day count that requires schedule information the default cannot supply
 * throws here. Such a row is emitted with `error`, and that IS its expectation:
 * a consumer must observe the same precondition failure for that input rather
 * than a value.
 */
void emitGeneratedDateGrid(JArray rows) {
  List<LocalDate> monthEnds = new ArrayList<>();
  List<LocalDate> midMonths = new ArrayList<>();
  for (int year = 2010; year <= 2030; year++) {
    for (int month = 1; month <= 12; month++) {
      monthEnds.add(LocalDate.of(year, month, 1).with(TemporalAdjusters.lastDayOfMonth()));
      midMonths.add(LocalDate.of(year, month, 15));
    }
  }
  List<List<LocalDate>> series = new ArrayList<>();
  series.add(monthEnds);
  series.add(midMonths);
  // The id tag of each series, so a row names which grid it belongs to.
  String[] seriesTags = {"me", "mid"};
  for (DayCount dayCount : gridDayCounts()) {
    for (int s = 0; s < series.size(); s++) {
      List<LocalDate> dates = series.get(s);
      for (int i = 0; i + 1 < dates.size(); i++) {
        LocalDate start = dates.get(i);
        LocalDate end = dates.get(i + 1);
        String rowId = "grid " + dayCount.getName() + " " + start;
        JObject row = dayCountRow(dcId("grid-" + seriesTags[s], dayCount, start),
            "grid.simpleInfo", dayCount, start, end, jScheduleInfo(null));
        Eval yf = evalYearFraction(dayCount, start, end, null);
        // Capture-only: generated inputs have no Java test constant.
        setYearFraction(FX_DAYCOUNT, row, rowId, yf, null);
        setDays(FX_DAYCOUNT, row, rowId, dayCount, start, end);
        rows.add(row);
        CHECK.countRow(FX_DAYCOUNT);
        CHECK.countCaptureOnly(FX_DAYCOUNT);
      }
    }
  }
}

/**
 * The generated grid, part two: every period of P1M / P3M / P6M / P12M
 * schedules running 2015-01-15 to 2020-01-15, in three shapes - regular, a
 * SHORT_INITIAL stub with firstRegularStartDate, and a SHORT_FINAL stub with
 * lastRegularEndDate - each period evaluated with the REAL Schedule as the
 * ScheduleInfo, which is the case the `periodEnds` shape exists for.
 */
void emitGeneratedScheduleGrid(JArray rows) {
  LocalDate gridStart = LocalDate.of(2015, 1, 15);
  LocalDate gridEnd = LocalDate.of(2020, 1, 15);
  Frequency[] frequencies = {P1M, P3M, P6M, P12M};
  List<DayCount> dayCounts = gridDayCounts();
  for (Frequency frequency : frequencies) {
    // shape 1: regular; shape 2: SHORT_INITIAL with firstRegularStartDate;
    // shape 3: SHORT_FINAL with lastRegularEndDate.
    List<PeriodicSchedule> definitions = new ArrayList<>();
    definitions.add(PeriodicSchedule.builder()
        .startDate(gridStart)
        .endDate(gridEnd)
        .frequency(frequency)
        .businessDayAdjustment(BusinessDayAdjustment.NONE)
        .stubConvention(StubConvention.NONE)
        .build());
    definitions.add(PeriodicSchedule.builder()
        .startDate(gridStart.minusDays(10))
        .endDate(gridEnd)
        .frequency(frequency)
        .businessDayAdjustment(BusinessDayAdjustment.NONE)
        .stubConvention(SHORT_INITIAL)
        .firstRegularStartDate(gridStart)
        .build());
    definitions.add(PeriodicSchedule.builder()
        .startDate(gridStart)
        .endDate(gridEnd.plusDays(10))
        .frequency(frequency)
        .businessDayAdjustment(BusinessDayAdjustment.NONE)
        .stubConvention(SHORT_FINAL)
        .lastRegularEndDate(gridEnd)
        .build());
    String[] shapes = {"regular", "shortInitial", "shortFinal"};
    for (int s = 0; s < definitions.size(); s++) {
      Schedule schedule = definitions.get(s).createSchedule(ReferenceData.standard());
      Jn info = scheduleInfoOfSchedule(schedule);
      for (DayCount dayCount : dayCounts) {
        for (SchedulePeriod period : schedule.getPeriods()) {
          LocalDate start = period.getStartDate();
          LocalDate end = period.getEndDate();
          String rowId = "gridSchedule " + dayCount.getName() + " " + frequency + " " + start;
          JObject row = dayCountRow(
              dcId("sched-" + slug(frequency.toString()) + "-" + slug(shapes[s]), dayCount, start),
              "grid.schedule." + frequency + "." + shapes[s], dayCount, start, end, info);
          Eval yf = evalYearFraction(dayCount, start, end, schedule);
          setYearFraction(FX_DAYCOUNT, row, rowId, yf, null);
          setDays(FX_DAYCOUNT, row, rowId, dayCount, start, end);
          rows.add(row);
          CHECK.countRow(FX_DAYCOUNT);
          CHECK.countCaptureOnly(FX_DAYCOUNT);
        }
      }
    }
  }
}

/* ---------------------------------------------------------------------------
 * The four @MethodSource("data_types") sweeps of DayCountTest, over the same
 * subjects as the generated grid: the 21 standard day counts plus Bus/252
 * BRBD. Sources: DayCountTest.java:88-124.
 *
 * DayCountTest guards three of the four with `if (type != ONE_ONE)`, because
 * `1/1` answers 1 for any two dates, and it runs over the 21 standard day
 * counts only. Both limits are enforced exactly as that test sets them: the row
 * is always captured, while the ASSERTION is applied only where the test makes
 * it - ONE_ONE is recorded without a year-fraction expectation, and the extra
 * Bus/252 BRBD row is marked capture-only rather than being held to a tolerance
 * the test never claimed for it (a business-day count of 251 for a whole year is
 * not "365 within 5", and is correct).
 * ---------------------------------------------------------------------------
 */

/** The dates DayCountTest declares for these four sweeps (:62-65). */
LocalDate DAYCOUNT_JAN_01 = LocalDate.of(2010, 1, 1);
LocalDate DAYCOUNT_JAN_02 = LocalDate.of(2010, 1, 2);
LocalDate DAYCOUNT_JUL_01 = LocalDate.of(2010, 7, 1);
LocalDate DAYCOUNT_JAN_01_NEXT = LocalDate.of(2011, 1, 1);

/** True for the 21 standard day counts the Java sweeps actually cover. */
boolean isStandardDayCount(DayCount dayCount) {
  return standardDayCounts().contains(dayCount);
}

/**
 * test_same: every day count except `1/1` answers 0 for a zero-length period,
 * in both year fraction and days.
 */
void emitDataTypesSame(JArray rows) {
  for (DayCount dayCount : gridDayCounts()) {
    boolean asserted = isStandardDayCount(dayCount) && !dayCount.equals(ONE_ONE);
    String rowId = "test_same " + dayCount.getName();
    JObject row = dayCountRow(dcId("same", dayCount, DAYCOUNT_JAN_02),
        "DayCountTest.test_same", dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_02,
        jScheduleInfo(null));
    setYearFraction(FX_DAYCOUNT, row, rowId,
        evalYearFraction(dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_02, null),
        asserted ? Double.valueOf(0d) : null);
    setDays(FX_DAYCOUNT, row, rowId, evalDays(dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_02),
        asserted ? Integer.valueOf(0) : null);
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
    if (!asserted) {
      CHECK.countCaptureOnly(FX_DAYCOUNT);
    }
  }
}

/**
 * test_halfYear and test_wholeYear: the sanity bounds the Java test states,
 * with its own tolerances - half a year within 0.01 of 0.5 and 182 days within
 * 2, a whole year within 0.02 of 1 and 365 days within 5.
 *
 * These are tolerance checks rather than equalities, so they are asserted with
 * checkClose / an explicit integer window instead of the exact comparison the
 * data tables use.
 */
void emitDataTypesYearBounds(JArray rows, boolean halfYear) {
  LocalDate start = DAYCOUNT_JAN_01;
  LocalDate end = halfYear ? DAYCOUNT_JUL_01 : DAYCOUNT_JAN_01_NEXT;
  String source = halfYear ? "DayCountTest.test_halfYear" : "DayCountTest.test_wholeYear";
  String prefix = halfYear ? "half" : "whole";
  double expectedFraction = halfYear ? 0.5d : 1d;
  double fractionTolerance = halfYear ? 0.01d : 0.02d;
  String toleranceName = halfYear ? "DayCountTest.test_halfYear within(0.01)"
      : "DayCountTest.test_wholeYear within(0.02)";
  int expectedDays = halfYear ? 182 : 365;
  int daysTolerance = halfYear ? 2 : 5;
  for (DayCount dayCount : gridDayCounts()) {
    boolean asserted = isStandardDayCount(dayCount) && !dayCount.equals(ONE_ONE);
    String rowId = source + " " + dayCount.getName();
    Info info = new Info(DAYCOUNT_JAN_01, DAYCOUNT_JAN_01_NEXT, DAYCOUNT_JAN_01_NEXT, false, P12M);
    JObject row = dayCountRow(dcId(prefix, dayCount, start, end), source, dayCount, start, end,
        jScheduleInfo(info));
    Eval fraction = evalYearFraction(dayCount, start, end, info);
    if (fraction.isError()) {
      row.set("error", jStr(fraction.error));
      CHECK.countErrorRow(FX_DAYCOUNT);
      CHECK.fail(FX_DAYCOUNT, rowId, "expected a year fraction but Java threw " + fraction.error);
    } else {
      row.set("yearFraction", jDbl(fraction.value));
      if (asserted) {
        CHECK.checkClose(FX_DAYCOUNT, rowId, expectedFraction, fraction.value, fractionTolerance,
            toleranceName);
      }
    }
    Eval days = evalDays(dayCount, start, end);
    setDays(FX_DAYCOUNT, row, rowId, days, null);
    if (asserted && !days.isError()) {
      long actualDays = (long) days.value;
      CHECK.checkTrue(FX_DAYCOUNT, rowId + " days",
          Math.abs(actualDays - expectedDays) <= daysTolerance,
          "expected days=" + expectedDays + " +/-" + daysTolerance + " actual=" + actualDays);
    }
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
    if (!asserted) {
      CHECK.countCaptureOnly(FX_DAYCOUNT);
    }
  }
}

/**
 * test_wrongOrder: for every day count, both `yearFraction` and `days` reject a
 * pair of dates supplied out of order, and the row states that rejection as its
 * expectation. `relativeYearFraction` accepts them and negates the result, and
 * is captured on the same row, so the two outcomes sit side by side.
 */
void emitDataTypesWrongOrder(JArray rows) {
  for (DayCount dayCount : gridDayCounts()) {
    boolean asserted = isStandardDayCount(dayCount);
    String rowId = "test_wrongOrder " + dayCount.getName();
    JObject row = dayCountRow(dcId("order", dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_01),
        "DayCountTest.test_wrongOrder", dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_01,
        jScheduleInfo(null));
    Eval fraction = evalYearFraction(dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_01, null);
    if (fraction.isError()) {
      row.set("error", jStr(fraction.error));
      CHECK.countErrorRow(FX_DAYCOUNT);
      if (asserted) {
        CHECK.checkTrue(FX_DAYCOUNT, rowId + " yearFraction",
            fraction.failedWith(IllegalArgumentException.class),
            "expected IllegalArgumentException, got " + fraction.error);
      }
    } else {
      row.set("yearFraction", jDbl(fraction.value));
      CHECK.fail(FX_DAYCOUNT, rowId,
          "expected yearFraction to reject reversed dates but it returned " + fraction.value);
    }
    Eval relative = evalRelativeYearFraction(dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_01, null);
    if (!relative.isError()) {
      row.set("relativeYearFraction", jDbl(relative.value));
    }
    Eval relativeReversed = evalRelativeYearFraction(dayCount, DAYCOUNT_JAN_01, DAYCOUNT_JAN_02,
        null);
    if (!relativeReversed.isError()) {
      row.set("relativeYearFractionReversed", jDbl(relativeReversed.value));
      if (!relative.isError()) {
        // relativeYearFraction negates when the dates are reversed.
        CHECK.checkExact(FX_DAYCOUNT, rowId + " relative negates", -relativeReversed.value,
            relative.value);
      }
    }
    Eval days = evalDays(dayCount, DAYCOUNT_JAN_02, DAYCOUNT_JAN_01);
    if (days.isError()) {
      row.set("daysError", jStr(days.error));
      CHECK.countErrorRow(FX_DAYCOUNT);
      if (asserted) {
        CHECK.checkTrue(FX_DAYCOUNT, rowId + " days",
            days.failedWith(IllegalArgumentException.class),
            "expected IllegalArgumentException, got " + days.error);
      }
    } else {
      row.set("days", jInt((long) days.value));
      CHECK.fail(FX_DAYCOUNT, rowId,
          "expected days to reject reversed dates but it returned " + days.value);
    }
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
    if (!asserted) {
      CHECK.countCaptureOnly(FX_DAYCOUNT);
    }
  }
}

/**
 * The five day counts that READ schedule information, evaluated with no
 * schedule information supplied - the case where the DayCount.ScheduleInfo
 * defaults throw UnsupportedOperationException.
 *
 * The expectation of each row is written out, so this is an assertion and not
 * a recording: three of the five must fail with the message named here, and
 * the other two must return the exact fraction named here. `days` never reads
 * schedule information, so it answers on all five.
 *
 * Rows: {day count, expected fraction or null, expected error or null}.
 */
Object[][] missingScheduleInfoCases() {
  return new Object[][] {
      {ACT_ACT_ICMA, null, "UnsupportedOperationException: The end date of the schedule is required"},
      // Act/365 Actual reads the period end only to decide 365 vs 366 and
      // falls back to the actual dates, so it answers: 29/366.
      {ACT_365_ACTUAL, Double.valueOf(29d / 366d), null},
      {THIRTY_E_360_ISDA, null,
          "UnsupportedOperationException: The end date of the schedule is required"},
      // 30U/360 reads only the end-of-month flag, which defaults to true: 29/360.
      {THIRTY_U_360, Double.valueOf(29d / 360d), null},
      {ACT_365L, null,
          "UnsupportedOperationException: The end date of the schedule period is required"},
  };
}

void emitMissingScheduleInfo(JArray rows) {
  LocalDate start = LocalDate.of(2012, 1, 31);
  LocalDate end = LocalDate.of(2012, 2, 29);
  for (Object[] testCase : missingScheduleInfoCases()) {
    DayCount dayCount = (DayCount) testCase[0];
    Double expectedFraction = (Double) testCase[1];
    String expectedError = (String) testCase[2];
    String rowId = "missingScheduleInfo " + dayCount.getName();
    JObject row = dayCountRow(dcId("missing", dayCount, start, end),
        "DayCountTest.data_types.missingScheduleInfo", dayCount, start, end, jScheduleInfo(null));
    Eval fraction = evalYearFraction(dayCount, start, end, null);
    setYearFraction(FX_DAYCOUNT, row, rowId, fraction, expectedFraction);
    if (expectedError != null) {
      CHECK.checkEquals(FX_DAYCOUNT, rowId + " error", expectedError,
          fraction.isError() ? fraction.error : null);
    }
    setDays(FX_DAYCOUNT, row, rowId, dayCount, start, end);
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/**
 * Bus/252 BRBD across and outside the calendar's stored range.
 *
 * Business252DayCountTest states its expectation as an identity rather than a
 * number (:82-92): `yearFraction` is the calendar's own `daysBetween` over
 * 252, and `days` IS that `daysBetween`. Both are asserted here, on the BRBD
 * calendar the fixture uses, which makes every value in these rows checked
 * against something other than itself.
 *
 * Six of the nine probes sit outside BRBD's 1950-2099 range, where
 * ImmutableHolidayCalendar falls back to a weekend-only test. For those the
 * business-day count is also computed independently - by counting the days
 * that are neither Saturday nor Sunday - so the fallback is proven rather than
 * assumed.
 *
 * Rows: {label, start, end, true if the whole span is outside the range}.
 */
Object[][] business252CalendarRangeCases() {
  return new Object[][] {
      {"inrange", LocalDate.of(2020, 1, 15), LocalDate.of(2020, 4, 15), Boolean.FALSE},
      {"firstyear", LocalDate.of(1950, 1, 2), LocalDate.of(1950, 12, 29), Boolean.FALSE},
      {"lastyear", LocalDate.of(2099, 1, 2), LocalDate.of(2099, 12, 31), Boolean.FALSE},
      {"below", LocalDate.of(1949, 1, 3), LocalDate.of(1949, 12, 30), Boolean.TRUE},
      {"farbelow", LocalDate.of(1940, 1, 31), LocalDate.of(1945, 6, 30), Boolean.TRUE},
      {"spanlower", LocalDate.of(1948, 12, 31), LocalDate.of(1952, 6, 30), Boolean.FALSE},
      {"above", LocalDate.of(2100, 1, 4), LocalDate.of(2100, 12, 31), Boolean.TRUE},
      {"farabove", LocalDate.of(2105, 2, 27), LocalDate.of(2110, 3, 31), Boolean.TRUE},
      {"spanupper", LocalDate.of(2098, 12, 31), LocalDate.of(2102, 6, 30), Boolean.FALSE},
  };
}

/** Days that are neither Saturday nor Sunday, counted independently. */
int weekdayCount(LocalDate startInclusive, LocalDate endExclusive) {
  int count = 0;
  LocalDate cursor = startInclusive;
  while (cursor.isBefore(endExclusive)) {
    DayOfWeek dayOfWeek = cursor.getDayOfWeek();
    if (dayOfWeek != DayOfWeek.SATURDAY && dayOfWeek != DayOfWeek.SUNDAY) {
      count++;
    }
    cursor = cursor.plusDays(1);
  }
  return count;
}

void emitBusiness252CalendarRange(JArray rows) {
  HolidayCalendarId brbdId = HolidayCalendarId.of("BRBD");
  DayCount bus252 = DayCount.ofBus252(brbdId);
  HolidayCalendar brbd = brbdId.resolve(ReferenceData.standard());
  for (Object[] testCase : business252CalendarRangeCases()) {
    String label = (String) testCase[0];
    LocalDate start = (LocalDate) testCase[1];
    LocalDate end = (LocalDate) testCase[2];
    boolean outsideRange = ((Boolean) testCase[3]).booleanValue();
    String rowId = "bus252 " + label + " " + start + ".." + end;
    int businessDays = brbd.daysBetween(start, end);
    JObject row = dayCountRow(dcId("bus252-" + label, bus252, start, end),
        "Business252DayCountTest.calendarRange", bus252, start, end, jScheduleInfo(null));
    // Business252DayCountTest: yearFraction == daysBetween / 252, days == daysBetween.
    setYearFraction(FX_DAYCOUNT, row, rowId, evalYearFraction(bus252, start, end, null),
        Double.valueOf(businessDays / 252d));
    setDays(FX_DAYCOUNT, row, rowId, evalDays(bus252, start, end),
        Integer.valueOf(businessDays));
    if (outsideRange) {
      // Outside 1950-2099 the calendar answers weekend-only, which is the one
      // thing about these probes that is not the implementation's own word.
      CHECK.checkInt(FX_DAYCOUNT, rowId + " weekend-only fallback", weekdayCount(start, end),
          businessDays);
    }
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/** The verified anchor: ACT_ACT_ISDA 2011-12-28 -> 2012-02-28. */
void checkDayCountAnchor() {
  double actual = ACT_ACT_ISDA.yearFraction(LocalDate.of(2011, 12, 28), LocalDate.of(2012, 2, 28));
  CHECK.checkExact(FX_DAYCOUNT, "ANCHOR ACT_ACT_ISDA 2011-12-28..2012-02-28",
      4d / 365d + 58d / 366d, actual);
}

Jn buildDayCountFixture() {
  JArray rows = new JArray();
  checkDayCountAnchor();
  emitDataYearFraction(rows);
  emitDataDays(rows);
  emitData30U360(rows);
  emitData30E360Isda(rows);
  emitDataActActAfb(rows);
  emitDataAct365L(rows);
  emitInfoCases(rows);
  emitActActYearVsIcma(rows);
  emitDataTypesSame(rows);
  emitDataTypesYearBounds(rows, true);
  emitDataTypesYearBounds(rows, false);
  emitDataTypesWrongOrder(rows);
  emitMissingScheduleInfo(rows);
  emitBusiness252CalendarRange(rows);
  emitGeneratedDateGrid(rows);
  emitGeneratedScheduleGrid(rows);
  // The row count is asserted rather than reported: 201 + 185 year-fraction
  // and day tables, 88 30U/360, 38 30E/360 ISDA, 57 AFB, 12 ACT/365L, 35
  // Info-based ICMA and ISDA cases, 146 sampled ACT_ACT_YEAR rows, 4 x 22
  // data_types sweeps, 5 missing-schedule-information probes, 9 Bus/252 range
  // probes, 11,044 simple-info grid rows and 6,578 schedule-grid rows.
  CHECK.checkCount(FX_DAYCOUNT, "daycount rows", 18356, rows.size());
  CHECK.checkCount(FX_DAYCOUNT, "daycount row ids", 18356, DAYCOUNT_ID_USES.size()
      + countDuplicateIdBases());
  return rows;
}

/** The number of extra uses of a repeated base id, so ids can be counted. */
int countDuplicateIdBases() {
  int extra = 0;
  for (Integer uses : DAYCOUNT_ID_USES.values()) {
    extra += uses.intValue() - 1;
  }
  return extra;
}

/* ===========================================================================
 * SECTION 8 - FIXTURE 2 OF 6: schedule-baseline.json
 * ===========================================================================
 */

String FX_SCHEDULE = "schedule";

Jn jBusinessDayAdjustment(BusinessDayAdjustment adjustment) {
  if (adjustment == null) {
    return jNull();
  }
  return new JObject()
      .set("convention", jName(adjustment.getConvention()))
      .set("calendar", jName(adjustment.getCalendar()));
}

Jn jAdjustableDate(AdjustableDate date) {
  if (date == null) {
    return jNull();
  }
  return new JObject()
      .set("unadjusted", jDate(date.getUnadjusted()))
      .set("adjustment", jBusinessDayAdjustment(date.getAdjustment()));
}

/**
 * Reads a `List<LocalDate>` column out of an Object[] table row.
 *
 * Written as an element-by-element copy through `List<?>` rather than a cast
 * to `List<LocalDate>`: the latter is an unchecked cast, and javac reports it
 * on stdout, which would break the "print only deliberately" rule this script
 * relies on for clean output.
 */
List<LocalDate> toDateList(Object raw) {
  if (raw == null) {
    return null;
  }
  List<LocalDate> result = new ArrayList<>();
  for (Object element : (List<?>) raw) {
    result.add((LocalDate) element);
  }
  return result;
}

Jn jSchedulePeriod(SchedulePeriod period) {
  return new JObject()
      .set("unadjustedStart", jDate(period.getUnadjustedStartDate()))
      .set("unadjustedEnd", jDate(period.getUnadjustedEndDate()))
      .set("start", jDate(period.getStartDate()))
      .set("end", jDate(period.getEndDate()));
}

/** Renders the eleven PeriodicSchedule fields that define a row's input. */
JObject jPeriodicScheduleInputs(PeriodicSchedule definition) {
  return new JObject()
      .set("startDate", jDate(definition.getStartDate()))
      .set("endDate", jDate(definition.getEndDate()))
      .set("frequency", jName(definition.getFrequency()))
      .set("businessDayAdjustment", jBusinessDayAdjustment(definition.getBusinessDayAdjustment()))
      .set("startDateBusinessDayAdjustment",
          jBusinessDayAdjustment(definition.getStartDateBusinessDayAdjustment().orElse(null)))
      .set("endDateBusinessDayAdjustment",
          jBusinessDayAdjustment(definition.getEndDateBusinessDayAdjustment().orElse(null)))
      .set("stubConvention", jName(definition.getStubConvention().orElse(null)))
      .set("rollConvention", jName(definition.getRollConvention().orElse(null)))
      .set("firstRegularStartDate", jDate(definition.getFirstRegularStartDate().orElse(null)))
      .set("lastRegularEndDate", jDate(definition.getLastRegularEndDate().orElse(null)))
      .set("overrideStartDate", jAdjustableDate(definition.getOverrideStartDate().orElse(null)));
}

/**
 * Renders the same eleven input fields from the RAW values, for a definition
 * the PeriodicSchedule builder itself rejects.
 *
 * `jPeriodicScheduleInputs` cannot be used there: it reads the fields off a
 * built definition, and for these rows no definition exists - the validation
 * in `PeriodicSchedule.validate()` (`PeriodicSchedule.java:361-390`) throws
 * before `build()` returns. Without this renderer such a row would carry only
 * the handful of fields the caller happened to name, and the fixture would no
 * longer have one uniform input key set across every row - which is the
 * property that lets the Scala decoder use a single case class for the whole
 * document. The key order below is identical to `jPeriodicScheduleInputs`.
 */
JObject jPeriodicScheduleRawInputs(
    LocalDate startDate,
    LocalDate endDate,
    Frequency frequency,
    BusinessDayAdjustment businessDayAdjustment,
    BusinessDayAdjustment startDateBusinessDayAdjustment,
    BusinessDayAdjustment endDateBusinessDayAdjustment,
    StubConvention stubConvention,
    RollConvention rollConvention,
    LocalDate firstRegularStartDate,
    LocalDate lastRegularEndDate,
    AdjustableDate overrideStartDate) {
  return new JObject()
      .set("startDate", jDate(startDate))
      .set("endDate", jDate(endDate))
      .set("frequency", jName(frequency))
      .set("businessDayAdjustment", jBusinessDayAdjustment(businessDayAdjustment))
      .set("startDateBusinessDayAdjustment", jBusinessDayAdjustment(startDateBusinessDayAdjustment))
      .set("endDateBusinessDayAdjustment", jBusinessDayAdjustment(endDateBusinessDayAdjustment))
      .set("stubConvention", jName(stubConvention))
      .set("rollConvention", jName(rollConvention))
      .set("firstRegularStartDate", jDate(firstRegularStartDate))
      .set("lastRegularEndDate", jDate(lastRegularEndDate))
      .set("overrideStartDate", jAdjustableDate(overrideStartDate));
}

/**
 * Writes the seven expectation fields that every successfully resolved row
 * carries, in their fixed declared order, and returns the unadjusted and
 * adjusted date lists it derived so the caller can cross-check them against a
 * Java table column.
 *
 * Both date lists are read back off the resolved periods rather than from
 * `createUnadjustedDates()` / `createAdjustedDates()`, so the dates, the
 * periods and the stubs in a row are guaranteed to be one self-consistent view
 * of one Schedule - which is what makes the structural invariants of the
 * fixture (`periods[i].start == adjustedDates[i]` and its three siblings) hold
 * by construction instead of by coincidence.
 */
List<List<LocalDate>> setResolvedScheduleExpectations(JObject row, Schedule schedule) {
  List<LocalDate> unadjusted = new ArrayList<>();
  List<LocalDate> adjusted = new ArrayList<>();
  JArray periods = new JArray();
  for (int i = 0; i < schedule.size(); i++) {
    SchedulePeriod period = schedule.getPeriod(i);
    periods.add(jSchedulePeriod(period));
    if (i == 0) {
      unadjusted.add(period.getUnadjustedStartDate());
      adjusted.add(period.getStartDate());
    }
    unadjusted.add(period.getUnadjustedEndDate());
    adjusted.add(period.getEndDate());
  }
  row.set("unadjustedDates", jDates(unadjusted));
  row.set("adjustedDates", jDates(adjusted));
  row.set("periods", periods);
  row.set("initialStub",
      schedule.getInitialStub().isPresent() ? jSchedulePeriod(schedule.getInitialStub().get())
          : jNull());
  row.set("finalStub",
      schedule.getFinalStub().isPresent() ? jSchedulePeriod(schedule.getFinalStub().get())
          : jNull());
  row.set("resolvedRollConvention", jName(schedule.getRollConvention()));
  row.set("resolvedFrequency", jName(schedule.getFrequency()));
  List<List<LocalDate>> result = new ArrayList<>();
  result.add(unadjusted);
  result.add(adjusted);
  return result;
}

/**
 * Resolves a PeriodicSchedule and adds either the full expectation set or an
 * `error` field carrying the Java ScheduleException / IllegalArgumentException
 * message.
 *
 * `expectedUnadjusted` and `expectedAdjusted` are the Java table's own columns
 * when the row came from a table, and null for generated combinations; when
 * present they are cross-checked, so a table row that stops matching aborts
 * the capture.
 *
 * `expect` states what the Java test says about resolution itself: a schedule,
 * a rejection of a named type, or nothing (a generated combination). A named
 * rejection is checked by type - see the Expect banner - so a definition that
 * starts failing differently cannot be recaptured as the expectation.
 */
void addScheduleRow(JArray rows, String source, PeriodicSchedule definition, String rowId,
    List<LocalDate> expectedUnadjusted, List<LocalDate> expectedAdjusted,
    RollConvention expectedRoll, Expect expect) {
  JObject row = jPeriodicScheduleInputs(definition);
  row.set("source", jStr(source));
  Schedule schedule = null;
  Throwable thrownByResolution = null;
  try {
    schedule = definition.createSchedule(ReferenceData.standard());
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    thrownByResolution = thrown;
  }
  if (thrownByResolution != null) {
    row.set("error", jStr(errorMessage(thrownByResolution)));
    CHECK.countErrorRow(FX_SCHEDULE);
    if (expect.mustFail()) {
      checkExpectedFailureType(FX_SCHEDULE, rowId, expect.failureType, thrownByResolution);
    } else if (expect.mustSucceed()) {
      CHECK.fail(FX_SCHEDULE, rowId,
          "expected a schedule but Java threw " + errorMessage(thrownByResolution));
    }
    rows.add(row);
    CHECK.countRow(FX_SCHEDULE);
    return;
  }
  if (expect.mustFail()) {
    CHECK.fail(FX_SCHEDULE, rowId, "expected " + expect.failureType.getSimpleName()
        + " but a schedule was produced");
  }
  List<List<LocalDate>> dates = setResolvedScheduleExpectations(row, schedule);
  List<LocalDate> unadjusted = dates.get(0);
  List<LocalDate> adjusted = dates.get(1);
  if (expectedUnadjusted != null) {
    CHECK.checkEquals(FX_SCHEDULE, rowId + " unadjusted", expectedUnadjusted, unadjusted);
  }
  if (expectedAdjusted != null) {
    CHECK.checkEquals(FX_SCHEDULE, rowId + " adjusted", expectedAdjusted, adjusted);
  }
  if (expectedRoll != null) {
    CHECK.checkEquals(FX_SCHEDULE, rowId + " rollConvention", expectedRoll,
        schedule.getRollConvention());
  }
  rows.add(row);
  CHECK.countRow(FX_SCHEDULE);
}

/**
 * data_generation: 87 rows of twelve columns - start, end, frequency,
 * stubConvention, rollConvention, businessDayAdjustment, firstRegular,
 * lastRegular, startBusinessDayAdjustment, expected unadjusted dates,
 * expected adjusted dates, expected roll convention. Built through
 * PeriodicSchedule.builder() exactly as PeriodicScheduleTest does.
 */
void emitDataGeneration(JArray rows) {
  Object[][] table = data_generation();
  CHECK.checkCount(FX_SCHEDULE, "data_generation rows",
      EXPECTED_TABLE_ROWS.get("data_generation").intValue(), table.length);
  for (Object[] r : table) {
    LocalDate start = (LocalDate) r[0];
    LocalDate end = (LocalDate) r[1];
    Frequency frequency = (Frequency) r[2];
    StubConvention stubConvention = (StubConvention) r[3];
    RollConvention rollConvention = (RollConvention) r[4];
    BusinessDayAdjustment businessDayAdjustment = (BusinessDayAdjustment) r[5];
    LocalDate firstRegular = (LocalDate) r[6];
    LocalDate lastRegular = (LocalDate) r[7];
    BusinessDayAdjustment startBusinessDayAdjustment = (BusinessDayAdjustment) r[8];
    List<LocalDate> expectedUnadjusted = toDateList(r[9]);
    List<LocalDate> expectedAdjusted = toDateList(r[10]);
    RollConvention expectedRoll = (RollConvention) r[11];
    PeriodicSchedule definition = PeriodicSchedule.builder()
        .startDate(start)
        .endDate(end)
        .frequency(frequency)
        .startDateBusinessDayAdjustment(startBusinessDayAdjustment)
        .businessDayAdjustment(businessDayAdjustment)
        .stubConvention(stubConvention)
        .rollConvention(rollConvention)
        .firstRegularStartDate(firstRegular)
        .lastRegularEndDate(lastRegular)
        .build();
    addScheduleRow(rows, "PeriodicScheduleTest.data_generation", definition,
        "generation " + start + ".." + end + " " + frequency + " stub=" + stubConvention,
        expectedUnadjusted, expectedAdjusted, expectedRoll, MUST_SUCCEED);
  }
}

/**
 * data_replace: 11 rows. Each builds a base definition, applies
 * replaceStartDate and compares createUnadjustedDates(). A null expected list
 * means the Java test asserts IllegalArgumentException.
 */
void emitDataReplace(JArray rows) {
  Object[][] table = data_replace();
  CHECK.checkCount(FX_SCHEDULE, "data_replace rows",
      EXPECTED_TABLE_ROWS.get("data_replace").intValue(), table.length);
  for (Object[] r : table) {
    LocalDate replaceStart = (LocalDate) r[0];
    LocalDate start = (LocalDate) r[1];
    LocalDate end = (LocalDate) r[2];
    Frequency frequency = (Frequency) r[3];
    StubConvention stubConvention = (StubConvention) r[4];
    RollConvention rollConvention = (RollConvention) r[5];
    BusinessDayAdjustment businessDayAdjustment = (BusinessDayAdjustment) r[6];
    LocalDate firstRegular = (LocalDate) r[7];
    LocalDate lastRegular = (LocalDate) r[8];
    BusinessDayAdjustment startBusinessDayAdjustment = (BusinessDayAdjustment) r[9];
    List<LocalDate> expectedUnadjusted = toDateList(r[10]);
    StubConvention expectedStub = (StubConvention) r[11];
    LocalDate expectedLastRegular = (LocalDate) r[12];
    RollConvention expectedRoll = (RollConvention) r[13];
    PeriodicSchedule base = PeriodicSchedule.builder()
        .startDate(start)
        .endDate(end)
        .frequency(frequency)
        .startDateBusinessDayAdjustment(startBusinessDayAdjustment)
        .businessDayAdjustment(businessDayAdjustment)
        .stubConvention(stubConvention)
        .rollConvention(rollConvention)
        .firstRegularStartDate(firstRegular)
        .lastRegularEndDate(lastRegular)
        .build();
    String rowId = "replace " + replaceStart + " over " + start + ".." + end;
    // The row records the BASE definition plus the replacement start date,
    // which together state the whole operation to perform: build the base
    // definition, apply replaceStartDate, then create the unadjusted dates.
    JObject row = jPeriodicScheduleInputs(base);
    row.set("source", jStr("PeriodicScheduleTest.data_replace"));
    row.set("replacedStartDate", jDate(replaceStart));
    // replaceStartDate itself throws when the replacement start is after the
    // end date, so it belongs inside the guarded block - the Java test wraps
    // the whole `replaceStartDate(...).createSchedule(...)` chain in one
    // assertThatIllegalArgumentException lambda, not just the last call.
    PeriodicSchedule replaced = null;
    List<LocalDate> unadjusted = null;
    Schedule schedule = null;
    Throwable thrownByReplace = null;
    try {
      replaced = base.replaceStartDate(replaceStart);
      unadjusted = new ArrayList<>(replaced.createUnadjustedDates());
      schedule = replaced.createSchedule(ReferenceData.standard());
    } catch (Throwable thrown) {
      requireCapturable(thrown);
      thrownByReplace = thrown;
    }
    // A null expected list is the table's way of saying "this row is the
    // rejection case", and PeriodicScheduleTest states it as
    // `assertThatIllegalArgumentException()` (:999), so the row must both fail
    // AND fail with that type; a row that carries dates must not fail at all.
    Expect expect = expectedUnadjusted == null ? MUST_REJECT_ARGUMENT : MUST_SUCCEED;
    if (thrownByReplace != null) {
      row.set("error", jStr(errorMessage(thrownByReplace)));
      CHECK.countErrorRow(FX_SCHEDULE);
      if (expect.mustFail()) {
        checkExpectedFailureType(FX_SCHEDULE, rowId, expect.failureType, thrownByReplace);
      } else {
        CHECK.fail(FX_SCHEDULE, rowId,
            "expected dates but Java threw " + errorMessage(thrownByReplace));
      }
    } else {
      if (expect.mustFail()) {
        CHECK.fail(FX_SCHEDULE, rowId, "expected " + expect.failureType.getSimpleName()
            + " but replaceStartDate produced a schedule");
      }
      // `replacedDefinition` is the post-replacement definition itself, whose
      // eleven fields are exactly what the Java test asserts one by one
      // (PeriodicScheduleTest.java:1002-1013: the override start date and the
      // first regular start date are cleared, the start date becomes the
      // replacement, and the start-date adjustment becomes BDA_NONE). Carrying
      // it lets a consumer assert the whole `replaceStartDate` operation rather
      // than only the dates it happens to produce.
      row.set("replacedDefinition", jPeriodicScheduleInputs(replaced));
      // `createUnadjustedDates()` on the replaced definition is what the Java
      // table asserts, and it is NOT always the unadjusted view of the resolved
      // Schedule - the two genuinely differ on the LONG_INITIAL / DAY_17 row
      // (data_replace row 2, MAY_19 over JUN_17..AUG_17), where
      // createUnadjustedDates() gives [2014-05-19, 2014-07-17, 2014-08-17] - a
      // long initial stub from the replacement start - while createSchedule()
      // rolls the start onto the 17th and gives
      // [2014-05-17, 2014-06-17, 2014-07-17, 2014-08-17]. Both are real Java
      // answers for the same definition, so the row carries each under its own
      // key and both are validated, instead of asserting that they agree.
      row.set("replacedUnadjustedDates", jDates(unadjusted));
      // Then the same full expectation set every resolved row carries, so a
      // replace row is shaped like the rest of the document rather than
      // carrying only the one column the Java test happens to assert.
      setResolvedScheduleExpectations(row, schedule);
      row.set("expectedStubConvention", jName(expectedStub));
      row.set("expectedLastRegularEndDate", jDate(expectedLastRegular));
      row.set("expectedRollConvention", jName(expectedRoll));
      // The rejection case is reported above, by type, so here the row is one
      // that carries dates and the table's own expectations are asserted.
      if (expectedUnadjusted != null) {
        CHECK.checkEquals(FX_SCHEDULE, rowId + " unadjusted", expectedUnadjusted, unadjusted);
        CHECK.checkEquals(FX_SCHEDULE, rowId + " stubConvention",
            Optional.ofNullable(expectedStub), replaced.getStubConvention());
        CHECK.checkEquals(FX_SCHEDULE, rowId + " lastRegularEndDate",
            Optional.ofNullable(expectedLastRegular), replaced.getLastRegularEndDate());
        // The Java test asserts the replaced definition's roll convention too
        // (:1013), so the captured column is checked rather than merely copied.
        CHECK.checkEquals(FX_SCHEDULE, rowId + " rollConvention",
            Optional.ofNullable(expectedRoll), replaced.getRollConvention());
        // No check that `unadjusted` equals the schedule-derived
        // `dates.get(0)`: as the comment above records, Java itself does not
        // guarantee that, so asserting it here would abort the capture on
        // correct behaviour.
      }
    }
    rows.add(row);
    CHECK.countRow(FX_SCHEDULE);
  }
}

/**
 * The generated combination grid: four frequencies x all eight stub
 * conventions x six roll conventions x three holiday-calendar adjustments,
 * resolved against ReferenceData.standard().
 *
 * Many combinations are mutually inconsistent, and that is the point: each
 * combination records whether Java resolved it or rejected it - a success row
 * or an `error` row - and that classification is what every row asserts.
 */
void emitScheduleCombinations(JArray rows) {
  Frequency[] frequencies = {P1M, P3M, P6M, P12M};
  StubConvention[] stubs = StubConvention.values();
  // IMMCAD, IMMAUD and TBILL are mandatory here, and not for variety: these
  // three StandardRollConventions members adjust with a built-in holiday
  // calendar (StandardRollConventions.java:60-63,74-75,103-104,133-134 - IMMCAD
  // with GBLO and CATO.combinedWith(CAMO), IMMAUD with AUSY, TBILL with USNY),
  // so they are the only rows that exercise a calendar-bearing roll convention.
  // SFE (second Friday) and IMMNZD use no calendar at all and are the
  // calendar-free control beside them.
  Object[][] rollConventions = {
      {"EOM", EOM}, {"IMM", IMM}, {"IMMCAD", RollConventions.IMMCAD},
      {"IMMAUD", RollConventions.IMMAUD}, {"IMMNZD", RollConventions.IMMNZD},
      {"SFE", SFE}, {"TBILL", RollConventions.TBILL},
      {"Day15", RollConventions.DAY_15},
  };
  Object[][] calendars = {
      {"GBLO", HolidayCalendarIds.GBLO}, {"USNY", HolidayCalendarIds.USNY},
      {"GBLO+USNY", HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY)},
  };
  LocalDate start = LocalDate.of(2015, 1, 15);
  LocalDate end = LocalDate.of(2018, 1, 15);
  for (Frequency frequency : frequencies) {
    for (StubConvention stub : stubs) {
      for (Object[] roll : rollConventions) {
        for (Object[] calendar : calendars) {
          BusinessDayAdjustment adjustment =
              BusinessDayAdjustment.of(MODIFIED_FOLLOWING, (HolidayCalendarId) calendar[1]);
          PeriodicSchedule definition;
          try {
            definition = PeriodicSchedule.builder()
                .startDate(start)
                .endDate(end)
                .frequency(frequency)
                .businessDayAdjustment(adjustment)
                .stubConvention(stub)
                .rollConvention((RollConvention) roll[1])
                .build();
          } catch (Throwable thrown) {
            requireCapturable(thrown);
            // A definition the builder itself rejects is still an expectation.
            // The full input key set is rendered from the raw values so this
            // row is shaped exactly like every other row in the document.
            JObject row = jPeriodicScheduleRawInputs(start, end, frequency, adjustment, null, null,
                stub, (RollConvention) roll[1], null, null, null);
            row.set("source", jStr("grid.combinations"));
            row.set("error", jStr(errorMessage(thrown)));
            rows.add(row);
            CHECK.countRow(FX_SCHEDULE);
            CHECK.countErrorRow(FX_SCHEDULE);
            CHECK.countCaptureOnly(FX_SCHEDULE);
            continue;
          }
          addScheduleRow(rows, "grid.combinations", definition,
              "grid " + frequency + " " + stub + " " + roll[0] + " " + calendar[0], null, null,
              null, CAPTURE_ONLY);
          CHECK.countCaptureOnly(FX_SCHEDULE);
        }
      }
    }
  }
}

/**
 * Emits one row from raw field values, covering the case where the
 * PeriodicSchedule builder itself rejects the definition.
 *
 * `addScheduleRow` takes an already-built definition and so cannot express a
 * definition that never builds; the validation at `PeriodicSchedule.java:361-390`
 * throws inside `build()`. Here the build and the resolution sit inside one
 * guarded block, exactly as the Java tests wrap the whole chain in a single
 * `assertThatIllegalArgumentException` lambda, and the input fields are
 * rendered from the raw values so the row keeps the document's uniform key set
 * whether or not a definition was produced.
 */
void addScheduleCaseRow(JArray rows, String source, String rowId,
    LocalDate startDate,
    LocalDate endDate,
    Frequency frequency,
    BusinessDayAdjustment businessDayAdjustment,
    BusinessDayAdjustment startDateBusinessDayAdjustment,
    BusinessDayAdjustment endDateBusinessDayAdjustment,
    StubConvention stubConvention,
    RollConvention rollConvention,
    LocalDate firstRegularStartDate,
    LocalDate lastRegularEndDate,
    AdjustableDate overrideStartDate,
    List<LocalDate> expectedUnadjusted,
    List<LocalDate> expectedAdjusted,
    Expect expect) {
  JObject row = jPeriodicScheduleRawInputs(startDate, endDate, frequency, businessDayAdjustment,
      startDateBusinessDayAdjustment, endDateBusinessDayAdjustment, stubConvention, rollConvention,
      firstRegularStartDate, lastRegularEndDate, overrideStartDate);
  row.set("source", jStr(source));
  Schedule schedule = null;
  Throwable thrownByCase = null;
  try {
    PeriodicSchedule definition = PeriodicSchedule.builder()
        .startDate(startDate)
        .endDate(endDate)
        .frequency(frequency)
        .businessDayAdjustment(businessDayAdjustment)
        .startDateBusinessDayAdjustment(startDateBusinessDayAdjustment)
        .endDateBusinessDayAdjustment(endDateBusinessDayAdjustment)
        .stubConvention(stubConvention)
        .rollConvention(rollConvention)
        .firstRegularStartDate(firstRegularStartDate)
        .lastRegularEndDate(lastRegularEndDate)
        .overrideStartDate(overrideStartDate)
        .build();
    schedule = definition.createSchedule(ReferenceData.standard());
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    thrownByCase = thrown;
  }
  if (thrownByCase != null) {
    row.set("error", jStr(errorMessage(thrownByCase)));
    CHECK.countErrorRow(FX_SCHEDULE);
    if (expect.mustFail()) {
      checkExpectedFailureType(FX_SCHEDULE, rowId, expect.failureType, thrownByCase);
    } else if (expect.mustSucceed()) {
      CHECK.fail(FX_SCHEDULE, rowId,
          "expected a schedule but Java threw " + errorMessage(thrownByCase));
    }
    rows.add(row);
    CHECK.countRow(FX_SCHEDULE);
    return;
  }
  if (expect.mustFail()) {
    CHECK.fail(FX_SCHEDULE, rowId, "expected " + expect.failureType.getSimpleName()
        + " but a schedule was produced");
  }
  List<List<LocalDate>> dates = setResolvedScheduleExpectations(row, schedule);
  if (expectedUnadjusted != null) {
    CHECK.checkEquals(FX_SCHEDULE, rowId + " unadjusted", expectedUnadjusted, dates.get(0));
  }
  if (expectedAdjusted != null) {
    CHECK.checkEquals(FX_SCHEDULE, rowId + " adjusted", expectedAdjusted, dates.get(1));
  }
  if (expectedUnadjusted == null && expectedAdjusted == null) {
    CHECK.countCaptureOnly(FX_SCHEDULE);
  }
  rows.add(row);
  CHECK.countRow(FX_SCHEDULE);
}

/**
 * The named feature cases: the PeriodicSchedule inputs that the two tables and
 * the combination grid never populate, and the definitions the builder
 * rejects.
 *
 * Every row here is taken from a PeriodicScheduleTest method, named in its
 * `source`, and every expectation is that test's own literal.
 *
 * Three inputs are covered nowhere else in this fixture: `overrideStartDate`,
 * which neither data table sets; `endDateBusinessDayAdjustment`, likewise; and
 * the five builder-time validation branches, whose captured messages are the
 * only statement this fixture carries of what a definition is rejected for.
 */
void emitScheduleFeatures(JArray rows) {
  BusinessDayAdjustment bda = BusinessDayAdjustment.of(MODIFIED_FOLLOWING, SAT_SUN);
  BusinessDayAdjustment bdaNone = BusinessDayAdjustment.NONE;
  BusinessDayAdjustment followingSatSun = BusinessDayAdjustment.of(FOLLOWING, SAT_SUN);
  LocalDate jun04 = LocalDate.of(2014, 6, 4);
  LocalDate jun17 = LocalDate.of(2014, 6, 17);
  LocalDate jul04 = LocalDate.of(2014, 7, 4);
  LocalDate jul11 = LocalDate.of(2014, 7, 11);
  LocalDate jul17 = LocalDate.of(2014, 7, 17);
  LocalDate aug04 = LocalDate.of(2014, 8, 4);
  LocalDate aug17 = LocalDate.of(2014, 8, 17);
  LocalDate aug18 = LocalDate.of(2014, 8, 18);
  LocalDate sep04 = LocalDate.of(2014, 9, 4);
  LocalDate sep05 = LocalDate.of(2014, 9, 5);
  LocalDate sep17 = LocalDate.of(2014, 9, 17);
  LocalDate oct17 = LocalDate.of(2014, 10, 17);

  // test_startEndAdjust (PeriodicScheduleTest.java:910) - the only Java case
  // that sets BOTH the start-date and the end-date business day adjustment.
  // Its literals are createUnadjustedDates() and createAdjustedDates(REF_DATA).
  addScheduleCaseRow(rows, "PeriodicScheduleTest.test_startEndAdjust", "startEndAdjust",
      LocalDate.of(2014, 10, 4), LocalDate.of(2015, 4, 4), P3M, bda,
      BusinessDayAdjustment.of(PRECEDING, SAT_SUN),
      BusinessDayAdjustment.of(MODIFIED_PRECEDING, SAT_SUN),
      StubConvention.NONE, null, null, null, null,
      List.of(LocalDate.of(2014, 10, 4), LocalDate.of(2015, 1, 4), LocalDate.of(2015, 4, 4)),
      List.of(LocalDate.of(2014, 10, 3), LocalDate.of(2015, 1, 5), LocalDate.of(2015, 4, 3)),
      MUST_SUCCEED);

  // test_firstPaymentDate_before_effectiveDate (:212) - an override start date
  // earlier than the start date, combined with a first regular start date. The
  // Java test asserts five periods, period i running from
  // overrideStartDate.plusMonths(3 * i) to three months later, unadjusted
  // equalling adjusted because every one of those dates is a weekday.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.test_firstPaymentDate_before_effectiveDate",
      "override before effective date",
      LocalDate.of(2018, 7, 26), LocalDate.of(2019, 6, 20), P3M, bda, null, null, null, null,
      LocalDate.of(2018, 6, 20), null, AdjustableDate.of(LocalDate.of(2018, 3, 20)),
      List.of(LocalDate.of(2018, 3, 20), LocalDate.of(2018, 6, 20), LocalDate.of(2018, 9, 20),
          LocalDate.of(2018, 12, 20), LocalDate.of(2019, 3, 20), LocalDate.of(2019, 6, 20)),
      List.of(LocalDate.of(2018, 3, 20), LocalDate.of(2018, 6, 20), LocalDate.of(2018, 9, 20),
          LocalDate.of(2018, 12, 20), LocalDate.of(2019, 3, 20), LocalDate.of(2019, 6, 20)),
      MUST_SUCCEED);

  // test_override_fallbackWhenStartDateMismatch (:851) - an adjusted override
  // start date that does not line up with the start date.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.test_override_fallbackWhenStartDateMismatch",
      "override fallback", jul04, sep17, P1M, bda, null, null, null, DAY_17, null, null,
      AdjustableDate.of(jun17, followingSatSun),
      List.of(jun17, jul17, aug17, sep17), List.of(jun17, jul17, aug18, sep17), MUST_SUCCEED);

  // test_override_fallbackWhenStartDateMismatchEndStub (:880) - the same, with
  // an explicit last regular end date producing a final stub.
  addScheduleCaseRow(rows,
      "PeriodicScheduleTest.test_override_fallbackWhenStartDateMismatchEndStub",
      "override fallback end stub", jul04, sep04, P1M, bda, null, null, null, DAY_17, null, aug17,
      AdjustableDate.of(jun17, followingSatSun),
      List.of(jun17, jul17, aug17, sep04), List.of(jun17, jul17, aug18, sep04), MUST_SUCCEED);

  // coverage_builder (:1447) - the one Java definition with all eleven fields
  // populated at once, so the fixture carries a row in which no input is null.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.coverage_builder", "coverage builder",
      jul17, sep17, P2M, bdaNone, bdaNone, bdaNone, StubConvention.NONE, EOM, jul17, sep17,
      AdjustableDate.of(jul11, bdaNone), null, null, MUST_SUCCEED);

  // The builder-time validation branches. Each is a definition Java rejects,
  // and the captured message is what the row asserts about that rejection:
  // PeriodicSchedule.java:361 (start not before end), :363
  // (override start date not before end date), :367 and :378 (first regular
  // start date outside the schedule), :370 (last regular end date before the
  // first regular start date).
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.startAfterEnd", "start after end",
      sep17, jun04, P1M, bda, null, null, null, null, null, null, null, null, null, MUST_REJECT_ARGUMENT);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.startEqualsEnd", "start equals end",
      jun04, jun04, P1M, bda, null, null, null, null, null, null, null, null, null, MUST_REJECT_ARGUMENT);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.overrideStartAfterEnd",
      "override start after end", jun04, sep17, P1M, bda, null, null, null, null, null, null,
      AdjustableDate.of(oct17), null, null, MUST_REJECT_ARGUMENT);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.firstRegularAfterEnd",
      "first regular after end", jun04, sep17, P1M, bda, null, null, null, null, oct17, null, null,
      null, null, MUST_REJECT_ARGUMENT);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.firstRegularBeforeStart",
      "first regular before start", jul17, sep17, P1M, bda, null, null, null, null, jun04, null,
      null, null, null, MUST_REJECT_ARGUMENT);
  // createDates(JUN_04, SEP_17, SEP_05, SEP_04) at :263 - last regular end date
  // before the first regular start date.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.lastRegularBeforeFirstRegular",
      "last regular before first regular", jun04, sep17, P1M, bda, null, null, null, null, sep05,
      sep04, null, null, null, MUST_REJECT_ARGUMENT);
  // The first-regular vs override-start-date conflict at :265-272.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.firstRegularWithOverride",
      "first regular with override", jun04, sep17, P1M, bda, null, null, null, null, jul17, null,
      AdjustableDate.of(aug04), null, null, MUST_REJECT_ARGUMENT);
  // Term frequency with an explicit regular date: a stub convention that
  // contradicts the explicit dates, rejected at resolution rather than at
  // build time with "Explicit stubs must not be specified when using 'Term'
  // frequency" (PeriodicSchedule.java:591).
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.termWithExplicitStub",
      "term with explicit stub", jun04, sep17, TERM, bda, null, null, null, null, jul17, null,
      null, null, null, Expect.failsWith(ScheduleException.class));
}

Jn buildScheduleFixture() {
  JArray rows = new JArray();
  emitDataGeneration(rows);
  emitDataReplace(rows);
  emitScheduleFeatures(rows);
  emitScheduleCombinations(rows);
  return rows;
}

/* ===========================================================================
 * SECTION 9 - FIXTURE 3 OF 6: fx-baseline.json
 *
 * One row per scenario, and every row carries the same nine keys: `id`,
 * `source`, `matrix`, `matrixState` and the five query lists of the row schema
 * (queries, conversions, multi, crosses, merges), each list entry carrying
 * either its value or an `error`. A list a scenario does not exercise is
 * emitted empty rather than omitted, so the fixture has one shape throughout;
 * nesting each expectation inside the input that produced it also makes the
 * input/expectation counts equal by construction rather than by convention.
 *
 * `id` is the short kebab-case handle the parity report names a scenario by,
 * and `source` stays the Java test attribution. Ids are checked for
 * uniqueness during capture.
 *
 * The zero-rate connected matrix of FxMatrixTest is included deliberately:
 * its JPY/CAD rate of 0.0 makes the reciprocal CAD/JPY infinite, which is
 * exactly the case the tagged-double policy exists for. It appears twice, once
 * as supplied and once with the x1.01 shift of streamPairsToMatrix, and the
 * nine-currency definition of addMultipleRatesSingle is present because that
 * is the only one that grows the builder's matrix past its initial size of 8.
 * ===========================================================================
 */

String FX_FX = "fx";

/** A currency pair and its rate. FxRate.getRate() is private, so the rate is
 * read through the public fxRate(base, counter). */
Jn jFxRate(FxRate rate) {
  if (rate == null) {
    return jNull();
  }
  CurrencyPair pair = rate.getPair();
  return new JObject()
      .set("pair", jName(pair))
      .set("rate", jDbl(rate.fxRate(pair.getBase(), pair.getCounter())));
}

Jn jCurrencyAmount(CurrencyAmount amount) {
  if (amount == null) {
    return jNull();
  }
  return new JObject()
      .set("currency", jName(amount.getCurrency()))
      .set("amount", jDbl(amount.getAmount()));
}

/** A matrix definition: the ordered list of pair/rate entries used to build it. */
class RateEntry {
  final CurrencyPair pair;
  final double rate;

  RateEntry(Currency base, Currency counter, double rate) {
    this.pair = CurrencyPair.of(base, counter);
    this.rate = rate;
  }
}

List<RateEntry> rateEntries(Object... baseCounterRate) {
  List<RateEntry> result = new ArrayList<>();
  for (int i = 0; i < baseCounterRate.length; i += 3) {
    result.add(new RateEntry((Currency) baseCounterRate[i], (Currency) baseCounterRate[i + 1],
        ((Number) baseCounterRate[i + 2]).doubleValue()));
  }
  return result;
}

/** A stable label for a rate list, used in check ids (never in a fixture). */
String rateEntryLabel(List<RateEntry> entries) {
  if (entries.isEmpty()) {
    return "[empty]";
  }
  StringBuilder sb = new StringBuilder();
  for (RateEntry entry : entries) {
    if (sb.length() > 0) {
      sb.append(',');
    }
    sb.append(entry.pair).append('@').append(Double.toString(entry.rate));
  }
  return sb.toString();
}

Jn jRateEntries(List<RateEntry> entries) {
  JArray array = new JArray();
  for (RateEntry entry : entries) {
    array.add(new JObject().set("pair", jName(entry.pair)).set("rate", jDbl(entry.rate)));
  }
  return array;
}

/** Builds a matrix, or returns null when the builder rejects the definition. */
FxMatrix buildMatrix(List<RateEntry> entries) {
  FxMatrixBuilder builder = FxMatrix.builder();
  for (RateEntry entry : entries) {
    builder.addRate(entry.pair, entry.rate);
  }
  return builder.build();
}

/** The matrix's own state: currencies in insertion order, and the rate matrix. */
JObject jMatrixState(FxMatrix matrix) {
  JArray currencies = new JArray();
  for (Currency currency : matrix.getCurrencies()) {
    currencies.add(jName(currency));
  }
  return new JObject()
      .set("currencies", currencies)
      .set("rates", jDoubleMatrix(matrix.getRates()));
}

/**
 * An expected query result: a null expectation with no expected failure means
 * capture-only, while an `expectedFailure` type asserts that Java rejects the
 * query with that type - which is what the `assertThatIllegalArgumentException`
 * cases of FxRateTest.test_fxRate_forPair state, so the capture states them
 * too rather than recording whatever happens.
 *
 * The type matters: without it an expected failure only asserts that something
 * was thrown, and a rejection that changed type - or a NullPointerException
 * from a new defect - would be recaptured as the expectation.
 */
class FxQuery {
  final Currency base;
  final Currency counter;
  final Double expected;
  final double tolerance;
  final String toleranceName;
  final Class<? extends Throwable> expectedFailure;

  FxQuery(Currency base, Currency counter, Double expected, double tolerance, String toleranceName) {
    this(base, counter, expected, tolerance, toleranceName, null);
  }

  FxQuery(Currency base, Currency counter, Double expected, double tolerance, String toleranceName,
      Class<? extends Throwable> expectedFailure) {
    this.base = base;
    this.counter = counter;
    this.expected = expected;
    this.tolerance = tolerance;
    this.toleranceName = toleranceName;
    this.expectedFailure = expectedFailure;
  }

  static FxQuery of(Currency base, Currency counter) {
    return new FxQuery(base, counter, null, 0d, "capture-only");
  }

  static FxQuery exact(Currency base, Currency counter, double expected) {
    return new FxQuery(base, counter, Double.valueOf(expected), 0d, "exact");
  }

  static FxQuery close(Currency base, Currency counter, double expected) {
    return new FxQuery(base, counter, Double.valueOf(expected), TOL_FX_MATRIX,
        "FxMatrixTest.TOLERANCE");
  }

  /**
   * A query the Java implementation must reject, captured as an `error`.
   *
   * FxMatrix.fxRate and FxRate.fxRate both reject an unknown currency through
   * ArgChecker, and FxRateTest.test_fxRate_forPair / FxMatrixTest assert that
   * with `assertThatIllegalArgumentException()`, so that is the type required
   * here.
   */
  static FxQuery failing(Currency base, Currency counter) {
    return new FxQuery(base, counter, null, 0d, "expected-failure",
        IllegalArgumentException.class);
  }
}

/**
 * One merge and WHAT FxMatrixTest SAYS ABOUT IT.
 *
 * Recording the outcome of a merge is not an expectation: whatever Java did
 * becomes the fixture, so a change in merge semantics would be recaptured as
 * the new truth and the capture would still exit 0. Every merge therefore
 * carries the assertion of the Java test it comes from:
 *
 *   cannotMergeDisjointMatrices (FxMatrixTest:490-504) -> `failing`
 *   mergeAddsInAdditionalCurrencies     (:525-546)     -> `extending`, with
 *       that test's currency set and all six of its rate constants
 *   mergeIgnoresDuplicateCurrencies     (:506-523)     -> `extending`, with
 *       the receiver's own rates as the expectation, which is what "ignores
 *       duplicates" MEANS for these inputs; the test's literal
 *       `result equals matrix1` case needs its own receiver and is asserted
 *       by checkFxMergeConstants below
 *   merging an empty matrix                            -> `failing`
 *
 * `extending` carries both halves of the assertion: the currencies the result
 * must contain, and the rates it must answer - including the triangulated
 * ones, which are the part a broken merge gets wrong while still producing a
 * matrix.
 */
class MergeQuery {
  final List<RateEntry> other;
  final Class<? extends Throwable> expectedFailure;
  final List<Currency> expectedCurrencies;
  final List<FxQuery> rateChecks;

  MergeQuery(List<RateEntry> other, Class<? extends Throwable> expectedFailure,
      List<Currency> expectedCurrencies, List<FxQuery> rateChecks) {
    this.other = other;
    this.expectedFailure = expectedFailure;
    this.expectedCurrencies = expectedCurrencies;
    this.rateChecks = rateChecks;
  }

  /**
   * Java rejects this merge; the rejection, AND ITS TYPE, is the assertion.
   *
   * FxMatrixTest.cannotMergeDisjointMatrices (:490-504) states it as
   * `assertThatIllegalArgumentException()`, so a merge that starts failing
   * some other way - an IllegalStateException, or a NullPointerException from
   * a defect in the merge itself - is a capture failure and not a new
   * expectation.
   */
  static MergeQuery failing(List<RateEntry> other) {
    return new MergeQuery(other, IllegalArgumentException.class, new ArrayList<>(),
        new ArrayList<>());
  }

  /** The merge succeeds: assert the currency set and every named rate. */
  static MergeQuery extending(List<RateEntry> other, List<Currency> expectedCurrencies,
      List<FxQuery> rateChecks) {
    return new MergeQuery(other, null, expectedCurrencies, rateChecks);
  }
}

/**
 * Varargs list builders. Unlike `merges`, these are safe as varargs: Currency
 * and FxQuery are not generic, so there is no possible-heap-pollution warning.
 */
List<Currency> currencyList(Currency... currencies) {
  List<Currency> result = new ArrayList<>();
  for (Currency currency : currencies) {
    result.add(currency);
  }
  return result;
}

List<FxQuery> rateChecks(FxQuery... checks) {
  List<FxQuery> result = new ArrayList<>();
  for (FxQuery check : checks) {
    result.add(check);
  }
  return result;
}

class ConversionQuery {
  final CurrencyAmount amount;
  final Currency target;
  final Double expected;
  final double tolerance;
  final String toleranceName;

  ConversionQuery(CurrencyAmount amount, Currency target, Double expected, double tolerance,
      String toleranceName) {
    this.amount = amount;
    this.target = target;
    this.expected = expected;
    this.tolerance = tolerance;
    this.toleranceName = toleranceName;
  }

  static ConversionQuery of(CurrencyAmount amount, Currency target) {
    return new ConversionQuery(amount, target, null, 0d, "capture-only");
  }

  static ConversionQuery exact(CurrencyAmount amount, Currency target, double expected) {
    return new ConversionQuery(amount, target, Double.valueOf(expected), 0d, "exact");
  }

  static ConversionQuery close(CurrencyAmount amount, Currency target, double expected) {
    return new ConversionQuery(amount, target, Double.valueOf(expected), TOL_FX_MATRIX,
        "FxMatrixTest.TOLERANCE");
  }
}

class MultiQuery {
  final MultiCurrencyAmount amount;
  final Currency target;
  final Double expected;
  final double tolerance;
  final String toleranceName;

  MultiQuery(MultiCurrencyAmount amount, Currency target, Double expected, double tolerance,
      String toleranceName) {
    this.amount = amount;
    this.target = target;
    this.expected = expected;
    this.tolerance = tolerance;
    this.toleranceName = toleranceName;
  }

  static MultiQuery exact(MultiCurrencyAmount amount, Currency target, double expected) {
    return new MultiQuery(amount, target, Double.valueOf(expected), 0d, "exact");
  }

  static MultiQuery close(MultiCurrencyAmount amount, Currency target, double expected) {
    return new MultiQuery(amount, target, Double.valueOf(expected), TOL_FX_MATRIX,
        "FxMatrixTest.TOLERANCE");
  }
}

/**
 * One FxRate.crossRate case: the two rates to cross, and either the expected
 * cross rate or the statement that Java must reject the combination. Both
 * outcomes are assertions of FxRateTest.test_crossRate, so both are checked.
 */
class CrossQuery {
  final FxRate rate1;
  final FxRate rate2;
  final FxRate expected;
  final Class<? extends Throwable> expectedFailure;

  CrossQuery(FxRate rate1, FxRate rate2, FxRate expected,
      Class<? extends Throwable> expectedFailure) {
    this.rate1 = rate1;
    this.rate2 = rate2;
    this.expected = expected;
    this.expectedFailure = expectedFailure;
  }

  static CrossQuery of(FxRate rate1, FxRate rate2, FxRate expected) {
    return new CrossQuery(rate1, rate2, expected, null);
  }

  /**
   * FxRateTest.test_crossRate asserts every rejected combination - no common
   * currency, or a rate against itself - with
   * `assertThatIllegalArgumentException()`, so the type is required here too.
   */
  static CrossQuery failing(FxRate rate1, FxRate rate2) {
    return new CrossQuery(rate1, rate2, null, IllegalArgumentException.class);
  }
}

/**
 * The amounts of a MultiCurrencyAmount as a bare array, which is the shape the
 * row schema's `amounts` field uses. getAmounts() is an ImmutableSortedSet
 * ordered by currency, so the order is stable.
 */
Jn jMultiCurrencyAmounts(MultiCurrencyAmount amount) {
  JArray amounts = new JArray();
  for (CurrencyAmount currencyAmount : amount.getAmounts()) {
    amounts.add(jCurrencyAmount(currencyAmount));
  }
  return amounts;
}

/**
 * The row ids emitted so far. Every row carries a short, stable, kebab-case
 * `id` that is unique within the fixture, because that id is the handle the
 * parity report and any failure message use to name a scenario; two rows
 * sharing one would make a report ambiguous, so a collision aborts the
 * capture rather than being emitted.
 */
Set<String> FX_ROW_IDS = new LinkedHashSet<>();

void addFxScenario(JArray rows, String id, String source, List<RateEntry> definition,
    List<FxQuery> queries, List<ConversionQuery> conversions, List<MultiQuery> multi,
    List<CrossQuery> crosses, List<MergeQuery> merges, boolean captureOnly) {
  CHECK.checkTrue(FX_FX, "row id " + id, FX_ROW_IDS.add(id), "row ids are unique");
  JObject row = new JObject()
      .set("id", jStr(id))
      .set("source", jStr(source))
      .set("matrix", jRateEntries(definition));
  FxMatrix matrix = null;
  String buildError = null;
  try {
    matrix = buildMatrix(definition);
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    buildError = errorMessage(thrown);
  }
  if (buildError != null) {
    // A definition the builder rejects has no matrix to query, so the five
    // query sections are emitted empty and `matrixState` is null: the row keeps
    // the shape of every other row and adds `error` to it.
    //
    // No scenario in this document declares such a rejection - every rate list
    // here is one FxMatrixTest builds successfully - so a build that starts
    // failing means the definition list or the builder changed, and that is a
    // capture failure rather than a fixture row to be believed.
    CHECK.fail(FX_FX, id, "the matrix definition was rejected: " + buildError);
    row.set("matrixState", jNull())
        .set("queries", new JArray())
        .set("conversions", new JArray())
        .set("multi", new JArray())
        .set("crosses", new JArray())
        .set("merges", new JArray())
        .set("error", jStr(buildError));
    CHECK.countErrorRow(FX_FX);
    rows.add(row);
    CHECK.countRow(FX_FX);
    return;
  }
  row.set("matrixState", jMatrixState(matrix));

  JArray queryArray = new JArray();
  for (FxQuery query : queries) {
    JObject entry = new JObject()
        .set("base", jName(query.base))
        .set("counter", jName(query.counter));
    String queryId = source + " fxRate " + query.base + "/" + query.counter;
    try {
      double actual = matrix.fxRate(query.base, query.counter);
      entry.set("fxRate", jDbl(actual));
      if (query.expected != null) {
        CHECK.checkClose(FX_FX, queryId, query.expected.doubleValue(), actual, query.tolerance,
            query.toleranceName);
      } else if (query.expectedFailure != null) {
        CHECK.fail(FX_FX, queryId, "expected " + query.expectedFailure.getSimpleName()
            + " but Java produced " + actual);
      }
    } catch (Throwable thrown) {
      requireCapturable(thrown);
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
      if (query.expected != null) {
        CHECK.fail(FX_FX, queryId,
            "expected=" + query.expected + " but Java threw " + errorMessage(thrown));
      } else if (query.expectedFailure != null) {
        checkExpectedFailureType(FX_FX, queryId, query.expectedFailure, thrown);
      }
    }
    queryArray.add(entry);
  }
  row.set("queries", queryArray);

  JArray conversionArray = new JArray();
  for (ConversionQuery conversion : conversions) {
    JObject entry = new JObject()
        .set("currency", jName(conversion.amount.getCurrency()))
        .set("amount", jDbl(conversion.amount.getAmount()))
        .set("target", jName(conversion.target));
    try {
      CurrencyAmount converted = matrix.convert(conversion.amount, conversion.target);
      entry.set("converted", jCurrencyAmount(converted));
      if (conversion.expected != null) {
        CHECK.checkClose(FX_FX, source + " convert " + conversion.amount + "->" + conversion.target,
            conversion.expected.doubleValue(), converted.getAmount(), conversion.tolerance,
            conversion.toleranceName);
      }
    } catch (Throwable thrown) {
      requireCapturable(thrown);
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
      if (conversion.expected != null) {
        CHECK.fail(FX_FX, source + " convert " + conversion.amount + "->" + conversion.target,
            "expected=" + conversion.expected + " but Java threw " + errorMessage(thrown));
      }
    }
    conversionArray.add(entry);
  }
  row.set("conversions", conversionArray);

  JArray multiArray = new JArray();
  for (MultiQuery query : multi) {
    JObject entry = new JObject()
        .set("amounts", jMultiCurrencyAmounts(query.amount))
        .set("target", jName(query.target));
    try {
      CurrencyAmount converted = matrix.convert(query.amount, query.target);
      entry.set("multiConverted", jCurrencyAmount(converted));
      if (query.expected != null) {
        CHECK.checkClose(FX_FX, source + " convertMulti ->" + query.target,
            query.expected.doubleValue(), converted.getAmount(), query.tolerance,
            query.toleranceName);
      }
    } catch (Throwable thrown) {
      requireCapturable(thrown);
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
      if (query.expected != null) {
        CHECK.fail(FX_FX, source + " convertMulti ->" + query.target,
            "expected=" + query.expected + " but Java threw " + errorMessage(thrown));
      }
    }
    multiArray.add(entry);
  }
  row.set("multi", multiArray);

  JArray crossArray = new JArray();
  for (CrossQuery cross : crosses) {
    JObject entry = new JObject()
        .set("rate1", jFxRate(cross.rate1))
        .set("rate2", jFxRate(cross.rate2));
    String crossId = source + " crossRate " + cross.rate1 + " x " + cross.rate2;
    try {
      FxRate actual = cross.rate1.crossRate(cross.rate2);
      entry.set("crossRate", jFxRate(actual));
      if (cross.expected != null) {
        CHECK.checkEquals(FX_FX, crossId, cross.expected, actual);
      } else if (cross.expectedFailure != null) {
        CHECK.fail(FX_FX, crossId, "expected " + cross.expectedFailure.getSimpleName()
            + " but Java produced " + actual);
      }
    } catch (Throwable thrown) {
      requireCapturable(thrown);
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
      if (cross.expected != null) {
        CHECK.fail(FX_FX, crossId,
            "expected=" + cross.expected + " but Java threw " + errorMessage(thrown));
      } else if (cross.expectedFailure != null) {
        checkExpectedFailureType(FX_FX, crossId, cross.expectedFailure, thrown);
      }
    }
    crossArray.add(entry);
  }
  row.set("crosses", crossArray);

  JArray mergeArray = new JArray();
  for (MergeQuery merge : merges) {
    JObject entry = new JObject().set("other", jRateEntries(merge.other));
    String mergeId = source + " merge " + rateEntryLabel(merge.other);
    FxMatrix merged = null;
    // The throwable itself is kept, not just its message: the assertion the
    // Java test makes is about its TYPE.
    Throwable mergeThrown = null;
    try {
      merged = matrix.merge(buildMatrix(merge.other));
    } catch (Throwable thrown) {
      requireCapturable(thrown);
      mergeThrown = thrown;
    }
    if (mergeThrown != null) {
      entry.set("error", jStr(errorMessage(mergeThrown)));
      CHECK.countErrorRow(FX_FX);
      if (merge.expectedFailure != null) {
        checkExpectedFailureType(FX_FX, mergeId, merge.expectedFailure, mergeThrown);
      } else {
        CHECK.fail(FX_FX, mergeId,
            "expected a merged matrix but Java threw " + errorMessage(mergeThrown));
      }
    } else {
      entry.set("merged", jMatrixState(merged));
      if (merge.expectedFailure != null) {
        CHECK.fail(FX_FX, mergeId, "expected " + merge.expectedFailure.getSimpleName()
            + " but Java produced a merged matrix over " + merged.getCurrencies());
      }
      CHECK.checkTrue(FX_FX, mergeId + " has an expectation",
          merge.expectedCurrencies.size() + merge.rateChecks.size() > 0,
          "a successful merge must carry the currencies and rates its Java test asserts");
      for (Currency currency : merge.expectedCurrencies) {
        CHECK.checkTrue(FX_FX, mergeId + " contains " + currency,
            merged.getCurrencies().contains(currency),
            "merged matrix is missing " + currency + ": " + merged.getCurrencies());
      }
      for (FxQuery rate : merge.rateChecks) {
        String rateId = mergeId + " fxRate " + rate.base + "/" + rate.counter;
        try {
          double actual = merged.fxRate(rate.base, rate.counter);
          CHECK.checkClose(FX_FX, rateId, rate.expected.doubleValue(), actual, rate.tolerance,
              rate.toleranceName);
        } catch (Throwable thrown) {
          requireCapturable(thrown);
          CHECK.fail(FX_FX, rateId,
              "expected=" + rate.expected + " but Java threw " + errorMessage(thrown));
        }
      }
    }
    mergeArray.add(entry);
  }
  row.set("merges", mergeArray);

  rows.add(row);
  CHECK.countRow(FX_FX);
  if (captureOnly) {
    CHECK.countCaptureOnly(FX_FX);
  }
}

List<FxQuery> noQueries() {
  return new ArrayList<>();
}

List<ConversionQuery> noConversions() {
  return new ArrayList<>();
}

List<MultiQuery> noMulti() {
  return new ArrayList<>();
}

List<CrossQuery> noCrosses() {
  return new ArrayList<>();
}

List<MergeQuery> noMerges() {
  return new ArrayList<>();
}

/*
 * Fixed-arity helpers rather than a generic varargs method: `merges(MergeQuery...)`
 * is a parameterized vararg and javac reports possible heap pollution for it,
 * which would put a warning on stdout.
 */
List<MergeQuery> merges(MergeQuery first) {
  List<MergeQuery> result = new ArrayList<>();
  result.add(first);
  return result;
}

List<MergeQuery> merges(MergeQuery first, MergeQuery second) {
  List<MergeQuery> result = merges(first);
  result.add(second);
  return result;
}

List<MergeQuery> merges(MergeQuery first, MergeQuery second, MergeQuery third) {
  List<MergeQuery> result = merges(first, second);
  result.add(third);
  return result;
}

List<MergeQuery> merges(MergeQuery first, MergeQuery second, MergeQuery third,
    MergeQuery fourth) {
  List<MergeQuery> result = merges(first, second, third);
  result.add(fourth);
  return result;
}

/**
 * The merge assertions of FxMatrixTest that need their own receiver, checked
 * without emitting a row.
 *
 * `mergeIgnoresDuplicateCurrencies` (FxMatrixTest:506-523) states its result
 * as `result equals matrix1`, which only holds when EVERY currency of the
 * other matrix is already present - its matrix1 carries EUR/CHF for exactly
 * that reason. The emitted merge row uses a two-rate receiver, so the literal
 * equality case is asserted here, on that test's own matrices, at its own
 * rates. A merge that started adding the other matrix's rates on top of the
 * receiver's would fail this check and abort the capture.
 */
void checkFxMergeConstants() {
  FxMatrix matrix1 = buildMatrix(rateEntries(
      Currency.GBP, Currency.USD, 1.6,
      Currency.EUR, Currency.USD, 1.4,
      Currency.EUR, Currency.CHF, 1.2));
  FxMatrix matrix2 = buildMatrix(rateEntries(
      Currency.GBP, Currency.USD, 1.7,
      Currency.EUR, Currency.USD, 1.5,
      Currency.EUR, Currency.CHF, 1.3));
  CHECK.checkEquals(FX_FX, "FxMatrixTest.mergeIgnoresDuplicateCurrencies", matrix1,
      matrix1.merge(matrix2));
}

/**
 * The FxRate cross-rate cases of FxRateTest.test_crossRate (8 + 5).
 *
 * The row goes through addFxScenario like every other, over an empty matrix
 * definition, so it carries the same nine keys as the rest of the fixture -
 * `crosses` is simply the only populated query section. The cross rates are a
 * property of FxRate alone and need no matrix.
 */
void emitCrossRates(JArray rows) {
  FxRate gbpUsd = FxRate.of(Currency.GBP, Currency.USD, 5d / 4d);
  FxRate usdGbp = FxRate.of(Currency.USD, Currency.GBP, 4d / 5d);
  FxRate eurUsd = FxRate.of(Currency.EUR, Currency.USD, 8d / 7d);
  FxRate usdEur = FxRate.of(Currency.USD, Currency.EUR, 7d / 8d);
  FxRate eurGbp = FxRate.of(Currency.EUR, Currency.GBP, (8d / 7d) * (4d / 5d));
  FxRate gbpGbp = FxRate.of(Currency.GBP, Currency.GBP, 1d);
  FxRate usdUsd = FxRate.of(Currency.USD, Currency.USD, 1d);
  FxRate eurCad = FxRate.of(Currency.EUR, Currency.CAD, 12d / 5d);
  // The eight combinations the Java test asserts all equal eurGbp, followed by
  // the five documented IllegalArgumentException cases: two identity pairs, two
  // same-currency pairs and one with no common currency.
  List<CrossQuery> crosses = new ArrayList<>();
  crosses.add(CrossQuery.of(eurUsd, usdGbp, eurGbp));
  crosses.add(CrossQuery.of(eurUsd, gbpUsd, eurGbp));
  crosses.add(CrossQuery.of(usdEur, usdGbp, eurGbp));
  crosses.add(CrossQuery.of(usdEur, gbpUsd, eurGbp));
  crosses.add(CrossQuery.of(gbpUsd, usdEur, eurGbp));
  crosses.add(CrossQuery.of(gbpUsd, eurUsd, eurGbp));
  crosses.add(CrossQuery.of(usdGbp, usdEur, eurGbp));
  crosses.add(CrossQuery.of(usdGbp, eurUsd, eurGbp));
  crosses.add(CrossQuery.failing(gbpGbp, gbpUsd));
  crosses.add(CrossQuery.failing(usdUsd, gbpUsd));
  crosses.add(CrossQuery.failing(gbpUsd, gbpUsd));
  crosses.add(CrossQuery.failing(gbpUsd, usdGbp));
  crosses.add(CrossQuery.failing(gbpUsd, eurCad));
  addFxScenario(rows, "fx-rate-cross-rates", "FxRateTest.test_crossRate", rateEntries(),
      noQueries(), noConversions(), noMulti(), crosses, noMerges(), false);
}

Jn buildFxFixture() {
  JArray rows = new JArray();

  // FxMatrixTest.matrixCalculatesCrossRates - the tolerances below are the
  // ones the Java test itself uses: exact for the directly supplied rates and
  // their reciprocals, 1e-6 for the triangulated cross rates.
  List<RateEntry> crossMatrix =
      rateEntries(Currency.GBP, Currency.USD, 1.6, Currency.EUR, Currency.USD, 1.4,
          Currency.EUR, Currency.CHF, 1.2);
  List<FxQuery> crossQueries = new ArrayList<>();
  crossQueries.add(FxQuery.exact(Currency.GBP, Currency.USD, 1.6));
  crossQueries.add(FxQuery.exact(Currency.USD, Currency.GBP, 1 / 1.6));
  crossQueries.add(FxQuery.exact(Currency.EUR, Currency.USD, 1.4));
  crossQueries.add(FxQuery.exact(Currency.USD, Currency.EUR, 1 / 1.4));
  crossQueries.add(FxQuery.close(Currency.EUR, Currency.GBP, 1.4 / 1.6));
  crossQueries.add(FxQuery.close(Currency.GBP, Currency.EUR, 1.6 / 1.4));
  crossQueries.add(FxQuery.exact(Currency.EUR, Currency.CHF, 1.2));
  crossQueries.add(FxQuery.of(Currency.CHF, Currency.GBP));
  crossQueries.add(FxQuery.of(Currency.USD, Currency.CHF));
  crossQueries.add(FxQuery.exact(Currency.USD, Currency.USD, 1.0));
  addFxScenario(rows, "cross-rate-triangulating-matrix", "FxMatrixTest.matrixCalculatesCrossRates",
      crossMatrix, crossQueries, noConversions(), noMulti(), noCrosses(), noMerges(), false);

  // FxMatrixTest.convertMultipleCurrencyAmountWithMultipleEntries and
  // convertMultipleCurrencyAmountWithSingleEntry.
  List<RateEntry> convertMatrix =
      rateEntries(Currency.GBP, Currency.EUR, 1.4, Currency.GBP, Currency.USD, 1.6);
  MultiCurrencyAmount single = MultiCurrencyAmount.of(CurrencyAmount.of(Currency.GBP, 1600));
  MultiCurrencyAmount multiple = MultiCurrencyAmount.of(
      CurrencyAmount.of(Currency.GBP, 1600),
      CurrencyAmount.of(Currency.EUR, 1200),
      CurrencyAmount.of(Currency.USD, 1500));
  List<MultiQuery> multiQueries = new ArrayList<>();
  multiQueries.add(MultiQuery.exact(single, Currency.GBP, 1600));
  multiQueries.add(MultiQuery.exact(single, Currency.USD, 2560));
  multiQueries.add(MultiQuery.exact(single, Currency.EUR, 2240));
  multiQueries.add(MultiQuery.close(multiple, Currency.GBP,
      1600d + (1200 / 1.4) + (1500 / 1.6)));
  multiQueries.add(MultiQuery.exact(multiple, Currency.USD,
      (1600d * 1.6) + ((1200 / 1.4) * 1.6) + 1500));
  multiQueries.add(MultiQuery.exact(multiple, Currency.EUR,
      (1600d * 1.4) + 1200 + ((1500 / 1.6) * 1.4)));
  List<ConversionQuery> convertConversions = new ArrayList<>();
  convertConversions.add(ConversionQuery.exact(CurrencyAmount.of(Currency.GBP, 1600),
      Currency.USD, 2560));
  convertConversions.add(ConversionQuery.of(CurrencyAmount.of(Currency.EUR, 1200), Currency.GBP));
  convertConversions.add(ConversionQuery.of(CurrencyAmount.of(Currency.USD, 1500), Currency.EUR));
  addFxScenario(rows, "convert-multi-currency-amount",
      "FxMatrixTest.convertMultipleCurrencyAmount", convertMatrix, noQueries(), convertConversions,
      multiQueries, noCrosses(), noMerges(), false);

  // FxMatrixTest.streamEntriesToMatrix - the zero-rate connected matrix. The
  // JPY/CAD rate of 0.0 makes CAD/JPY infinite; both are captured as tagged
  // doubles, which is the whole reason that policy exists.
  List<RateEntry> zeroMatrix = rateEntries(
      Currency.GBP, Currency.USD, 1.6,
      Currency.EUR, Currency.USD, 1.4,
      Currency.CHF, Currency.AUD, 1.2,
      Currency.SEK, Currency.AUD, 0.1,
      Currency.JPY, Currency.CAD, 0.0,
      Currency.EUR, Currency.CHF, 1.2,
      Currency.JPY, Currency.USD, 0.008);
  List<FxQuery> zeroQueries = new ArrayList<>();
  zeroQueries.add(FxQuery.exact(Currency.GBP, Currency.USD, 1.6));
  zeroQueries.add(FxQuery.exact(Currency.EUR, Currency.USD, 1.4));
  zeroQueries.add(FxQuery.exact(Currency.JPY, Currency.CAD, 0.0));
  zeroQueries.add(FxQuery.exact(Currency.CAD, Currency.JPY, Double.POSITIVE_INFINITY));
  zeroQueries.add(FxQuery.of(Currency.CAD, Currency.USD));
  zeroQueries.add(FxQuery.of(Currency.SEK, Currency.CHF));
  List<ConversionQuery> zeroConversions = new ArrayList<>();
  zeroConversions.add(ConversionQuery.exact(CurrencyAmount.of(Currency.JPY, 1000), Currency.CAD, 0.0));
  zeroConversions.add(ConversionQuery.of(CurrencyAmount.of(Currency.CAD, 1000), Currency.JPY));
  addFxScenario(rows, "zero-rate-connected-matrix", "FxMatrixTest.streamEntriesToMatrix",
      zeroMatrix, zeroQueries, zeroConversions, noMulti(), noCrosses(), noMerges(), false);

  // FxMatrixTest.streamPairsToMatrix - the same rate set, with every rate
  // shifted by x1.01 before it is collected, which is the map step of that
  // test. The two rates it asserts, 1.6 * 1.01 -> 1.616 and 1.4 * 1.01 ->
  // 1.414, are checked exactly, so the shift is pinned rather than assumed.
  // The zero rate survives the shift (0.0 * 1.01 == 0.0), so this row carries
  // the infinite reciprocal too.
  List<RateEntry> shiftedMatrix = new ArrayList<>();
  for (RateEntry entry : zeroMatrix) {
    shiftedMatrix.add(
        new RateEntry(entry.pair.getBase(), entry.pair.getCounter(), entry.rate * 1.01));
  }
  List<FxQuery> shiftedQueries = new ArrayList<>();
  shiftedQueries.add(FxQuery.exact(Currency.GBP, Currency.USD, 1.616));
  shiftedQueries.add(FxQuery.exact(Currency.EUR, Currency.USD, 1.414));
  shiftedQueries.add(FxQuery.exact(Currency.JPY, Currency.CAD, 0.0));
  shiftedQueries.add(FxQuery.exact(Currency.CAD, Currency.JPY, Double.POSITIVE_INFINITY));
  shiftedQueries.add(FxQuery.of(Currency.SEK, Currency.CHF));
  shiftedQueries.add(FxQuery.of(Currency.CHF, Currency.AUD));
  addFxScenario(rows, "shifted-zero-rate-connected-matrix", "FxMatrixTest.streamPairsToMatrix",
      shiftedMatrix, shiftedQueries, noConversions(), noMulti(), noCrosses(), noMerges(), false);

  // FxMatrixTest.addMultipleRatesSingle - "By adding more than 8 currencies we
  // force a resizing operation". FxMatrixBuilder starts at MINIMAL_MATRIX_SIZE
  // = 8 and grows when a ninth currency arrives, so this nine-currency
  // definition is the row that exercises the grown matrix. The seven rates the
  // Java test asserts carry that test's own tolerances; the remaining queries
  // and conversions triangulate through the currencies added after the resize.
  List<RateEntry> resizeMatrix = rateEntries(
      Currency.GBP, Currency.USD, 1.6,
      Currency.EUR, Currency.USD, 1.4,
      Currency.EUR, Currency.CHF, 1.2,
      Currency.EUR, Currency.CHF, 1.2,
      Currency.CHF, Currency.AUD, 1.2,
      Currency.SEK, Currency.AUD, 0.16,
      Currency.JPY, Currency.USD, 0.0084,
      Currency.JPY, Currency.CAD, 0.01,
      Currency.USD, Currency.NZD, 1.3);
  List<FxQuery> resizeQueries = new ArrayList<>();
  resizeQueries.add(FxQuery.exact(Currency.GBP, Currency.USD, 1.6));
  resizeQueries.add(FxQuery.exact(Currency.USD, Currency.GBP, 1 / 1.6));
  resizeQueries.add(FxQuery.close(Currency.EUR, Currency.USD, 1.4));
  resizeQueries.add(FxQuery.close(Currency.USD, Currency.EUR, 1 / 1.4));
  resizeQueries.add(FxQuery.close(Currency.EUR, Currency.GBP, 1.4 / 1.6));
  resizeQueries.add(FxQuery.close(Currency.GBP, Currency.EUR, 1.6 / 1.4));
  resizeQueries.add(FxQuery.exact(Currency.EUR, Currency.CHF, 1.2));
  resizeQueries.add(FxQuery.of(Currency.NZD, Currency.SEK));
  resizeQueries.add(FxQuery.of(Currency.CAD, Currency.NZD));
  resizeQueries.add(FxQuery.of(Currency.JPY, Currency.NZD));
  resizeQueries.add(FxQuery.exact(Currency.NZD, Currency.NZD, 1.0));
  List<ConversionQuery> resizeConversions = new ArrayList<>();
  resizeConversions.add(ConversionQuery.of(CurrencyAmount.of(Currency.NZD, 1000), Currency.SEK));
  resizeConversions.add(ConversionQuery.of(CurrencyAmount.of(Currency.SEK, 1000), Currency.NZD));
  addFxScenario(rows, "resize-nine-currency-matrix", "FxMatrixTest.addMultipleRatesSingle",
      resizeMatrix, resizeQueries, resizeConversions, noMulti(), noCrosses(), noMerges(), false);

  // FxMatrixTest.emptyMatrixCanHandleTrivialRate and
  // emptyMatrixCannotDoConversion: the trivial rate works, any real query is
  // rejected.
  List<FxQuery> emptyQueries = new ArrayList<>();
  emptyQueries.add(FxQuery.exact(Currency.USD, Currency.USD, 1.0));
  emptyQueries.add(FxQuery.failing(Currency.USD, Currency.EUR));
  addFxScenario(rows, "empty-matrix", "FxMatrixTest.emptyMatrix", rateEntries(), emptyQueries,
      noConversions(), noMulti(), noCrosses(), noMerges(), false);

  // FxMatrixTest.singleRateMatrixByOfCurrencyPairFactory: a query for an
  // absent currency fails.
  List<FxQuery> singleQueries = new ArrayList<>();
  singleQueries.add(FxQuery.exact(Currency.GBP, Currency.USD, 1.6));
  singleQueries.add(FxQuery.exact(Currency.USD, Currency.GBP, 0.625));
  singleQueries.add(FxQuery.failing(Currency.USD, Currency.EUR));
  addFxScenario(rows, "single-pair-matrix", "FxMatrixTest.singlePairMatrix",
      rateEntries(Currency.GBP, Currency.USD, 1.6), singleQueries, noConversions(), noMulti(),
      noCrosses(), noMerges(), false);

  // FxRateTest.test_fxRate_forPair and test_convert_double, over the single
  // GBP/USD 1.25 rate those tests use. An identity query is answered before
  // any lookup happens, so GBP/GBP, USD/USD and even AUD/AUD - AUD being
  // absent from the matrix entirely - are all exactly 1, while every genuine
  // query involving AUD is rejected. The GBP -> GBP conversion is the
  // same-currency case, which returns the amount unchanged.
  List<FxQuery> identityQueries = new ArrayList<>();
  identityQueries.add(FxQuery.exact(Currency.GBP, Currency.USD, 1.25));
  identityQueries.add(FxQuery.exact(Currency.USD, Currency.GBP, 1d / 1.25d));
  identityQueries.add(FxQuery.exact(Currency.GBP, Currency.GBP, 1.0));
  identityQueries.add(FxQuery.exact(Currency.USD, Currency.USD, 1.0));
  identityQueries.add(FxQuery.exact(Currency.AUD, Currency.AUD, 1.0));
  identityQueries.add(FxQuery.failing(Currency.AUD, Currency.GBP));
  identityQueries.add(FxQuery.failing(Currency.GBP, Currency.AUD));
  identityQueries.add(FxQuery.failing(Currency.AUD, Currency.USD));
  identityQueries.add(FxQuery.failing(Currency.USD, Currency.AUD));
  identityQueries.add(FxQuery.failing(Currency.EUR, Currency.AUD));
  List<ConversionQuery> identityConversions = new ArrayList<>();
  identityConversions.add(
      ConversionQuery.exact(CurrencyAmount.of(Currency.GBP, 100), Currency.GBP, 100d));
  identityConversions.add(
      ConversionQuery.exact(CurrencyAmount.of(Currency.GBP, 100), Currency.USD, 125d));
  identityConversions.add(
      ConversionQuery.exact(CurrencyAmount.of(Currency.USD, 100), Currency.GBP, 100d / 1.25d));
  addFxScenario(rows, "identity-and-single-rate-conversion", "FxRateTest.test_fxRate_forPair",
      rateEntries(Currency.GBP, Currency.USD, 1.25), identityQueries, identityConversions,
      noMulti(), noCrosses(), noMerges(), false);

  // The four merge cases: disjoint (fails - FxMatrixTest:503), duplicate
  // currencies (keeps the receiver's rates), additional currencies (extends
  // the matrix) and an empty other matrix. The last one fails as well, and the
  // row asserts that failure: merge looks for a currency common to both
  // matrices and an empty matrix has none, so merging with it is an error
  // rather than a no-op - an asymmetry of the API worth pinning.
  //
  // Each merge carries the assertion of its Java test rather than just its
  // outcome; see MergeQuery. The rate constants below are written as the same
  // arithmetic expressions FxMatrixTest uses, at that test's own TOLERANCE.
  List<RateEntry> mergeBase =
      rateEntries(Currency.GBP, Currency.USD, 1.6, Currency.EUR, Currency.USD, 1.4);
  List<RateEntry> disjoint =
      rateEntries(Currency.CHF, Currency.AUD, 1.2, Currency.SEK, Currency.AUD, 0.16);
  List<RateEntry> duplicate = rateEntries(Currency.GBP, Currency.USD, 1.7,
      Currency.EUR, Currency.USD, 1.5, Currency.EUR, Currency.CHF, 1.3);
  List<RateEntry> additional =
      rateEntries(Currency.EUR, Currency.CHF, 1.2, Currency.CHF, Currency.AUD, 1.2);
  // mergeIgnoresDuplicateCurrencies, applied to this receiver: the rates for
  // the currencies the receiver already knows must be the RECEIVER's (1.6 and
  // 1.4), not the other matrix's (1.7 and 1.5), and CHF - which only the other
  // matrix carries - is added.
  MergeQuery duplicateMerge = MergeQuery.extending(duplicate,
      currencyList(Currency.GBP, Currency.USD, Currency.EUR, Currency.CHF),
      rateChecks(
          FxQuery.exact(Currency.GBP, Currency.USD, 1.6),
          FxQuery.exact(Currency.EUR, Currency.USD, 1.4),
          FxQuery.close(Currency.GBP, Currency.EUR, 1.6 / 1.4)));
  // mergeAddsInAdditionalCurrencies (FxMatrixTest:525-546): the receiver below
  // is that test's matrix1 and the other matrix its matrix2, and the six rates
  // asserted here are the six that test asserts.
  MergeQuery additionalMerge = MergeQuery.extending(additional,
      currencyList(Currency.USD, Currency.GBP, Currency.EUR, Currency.CHF, Currency.AUD),
      rateChecks(
          FxQuery.exact(Currency.GBP, Currency.USD, 1.6),
          FxQuery.close(Currency.GBP, Currency.EUR, 1.6 / 1.4),
          FxQuery.exact(Currency.EUR, Currency.CHF, 1.2),
          FxQuery.exact(Currency.CHF, Currency.AUD, 1.2),
          FxQuery.close(Currency.GBP, Currency.CHF, (1.6 / 1.4) * 1.2),
          FxQuery.close(Currency.GBP, Currency.AUD, (1.6 / 1.4) * 1.2 * 1.2)));
  addFxScenario(rows, "merge-cases", "FxMatrixTest.merge", mergeBase, noQueries(),
      noConversions(), noMulti(), noCrosses(),
      merges(MergeQuery.failing(disjoint), duplicateMerge, additionalMerge,
          MergeQuery.failing(rateEntries())), false);
  checkFxMergeConstants();

  // Seeded random matrices: capture-only, because no Java constant exists for
  // them. Every rate comes from the single seeded Random.
  Currency[] randomCurrencies = {Currency.USD, Currency.EUR, Currency.GBP, Currency.JPY,
      Currency.CHF, Currency.AUD, Currency.CAD, Currency.NZD};
  for (int scenario = 0; scenario < 4; scenario++) {
    List<RateEntry> entries = new ArrayList<>();
    for (int i = 1; i < randomCurrencies.length; i++) {
      entries.add(new RateEntry(randomCurrencies[0], randomCurrencies[i], nextRandomRate()));
    }
    List<FxQuery> queries = new ArrayList<>();
    for (int i = 0; i < randomCurrencies.length; i++) {
      for (int j = 0; j < randomCurrencies.length; j++) {
        queries.add(FxQuery.of(randomCurrencies[i], randomCurrencies[j]));
      }
    }
    List<ConversionQuery> conversions = new ArrayList<>();
    for (int i = 0; i < randomCurrencies.length; i++) {
      conversions.add(ConversionQuery.of(
          CurrencyAmount.of(randomCurrencies[i], nextRandomAmount()), randomCurrencies[0]));
    }
    addFxScenario(rows, "random-scenario-" + scenario,
        "random.seed" + RANDOM_SEED + ".scenario" + scenario, entries, queries, conversions,
        noMulti(), noCrosses(), noMerges(), true);
  }

  emitCrossRates(rows);
  return rows;
}

/* ===========================================================================
 * SECTION 10 - FIXTURE 4 OF 6: currency-math-baseline.json
 *
 * Rows carry the named input lists of the row schema plus one result list per
 * subject type. Each result entry names its `op`, so a consumer reads the
 * operation rather than inferring it from position.
 *
 * TWO OPERATIONS ARE COMPOSED, NOT INVENTED. The row schema names
 * `multipliedBy` and `mapAmounts` for CurrencyAmountArray and
 * MultiCurrencyAmountArray, and the Java types declare neither method. No Java
 * API is invented for them: both expectations are COMPOSED from two real public
 * calls, `of(currency, getValues().multipliedBy(s))` and
 * `of(currency, getValues().map(f))`, and every such entry is marked
 * `"composed": true` and carries the function it used in `mapAmountsFn`. That
 * function is x -> x * x, the same one the double-array fixture documents.
 * ===========================================================================
 */

String FX_CURRENCY_MATH = "currency-math";

/** The documented mapAmounts / map function, named in the emitted rows. */
String MAP_AMOUNTS_FN = "x -> x * x";

double mapAmountsFn(double value) {
  return value * value;
}

Jn jMoney(Money money) {
  if (money == null) {
    return jNull();
  }
  return new JObject()
      .set("currency", jName(money.getCurrency()))
      .set("amount", jStr(money.getValue().toString()))
      .set("toString", jStr(money.toString()));
}

Jn jBigMoney(BigMoney money) {
  if (money == null) {
    return jNull();
  }
  return new JObject()
      .set("currency", jName(money.getCurrency()))
      .set("amount", jStr(money.getValue().toString()))
      .set("toString", jStr(money.toString()));
}

Jn jCurrencyAmountArray(CurrencyAmountArray array) {
  if (array == null) {
    return jNull();
  }
  return new JObject()
      .set("currency", jName(array.getCurrency()))
      .set("values", jDoubleArray(array.getValues()));
}

Jn jMultiCurrencyAmountArray(MultiCurrencyAmountArray array) {
  if (array == null) {
    return jNull();
  }
  JArray arrays = new JArray();
  // getValues() is an ImmutableSortedMap keyed by currency, so stable.
  for (Map.Entry<Currency, DoubleArray> entry : array.getValues().entrySet()) {
    arrays.add(new JObject()
        .set("currency", jName(entry.getKey()))
        .set("values", jDoubleArray(entry.getValue())));
  }
  return new JObject().set("size", jInt(array.getSize())).set("arrays", arrays);
}

/*
 * Just the per-currency arrays of a MultiCurrencyAmountArray, without the
 * `size` wrapper. This is the `arrays[]` member of the row schema's
 * `multiArrays[{arrays[]}]` input field.
 */
Jn jMultiCurrencyAmountArrayValues(MultiCurrencyAmountArray array) {
  if (array == null) {
    return jNull();
  }
  JArray arrays = new JArray();
  for (Map.Entry<Currency, DoubleArray> entry : array.getValues().entrySet()) {
    arrays.add(new JObject()
        .set("currency", jName(entry.getKey()))
        .set("values", jDoubleArray(entry.getValue())));
  }
  return arrays;
}

/**
 * Runs one operation, adding either the rendered result or an `error`.
 *
 * `expect` states what the Java test says about the call - a value, a
 * rejection of a named type, or nothing at all. An expected rejection is
 * checked by type, so a change from one exception to another is a capture
 * failure rather than a new fixture value; see the Expect banner.
 */
interface ResultSupplier {
  Jn get() throws Throwable;
}

void addOperation(JArray results, String fixture, String rowId, JObject entry,
    ResultSupplier supplier, Expect expect) {
  try {
    Jn value = supplier.get();
    entry.set("result", value);
    if (expect.mustFail()) {
      CHECK.fail(fixture, rowId, "expected " + expect.failureType.getSimpleName()
          + " but Java produced a value");
    }
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    entry.set("error", jStr(errorMessage(thrown)));
    CHECK.countErrorRow(fixture);
    if (expect.mustFail()) {
      checkExpectedFailureType(fixture, rowId, expect.failureType, thrown);
    } else if (expect.mustSucceed()) {
      CHECK.fail(fixture, rowId, "unexpected Java failure " + errorMessage(thrown));
    }
  }
  results.add(entry);
}

JObject opEntry(String op) {
  return new JObject().set("op", jStr(op));
}

/*
 * Collector for the seven row-level input fields that the `currency-math`
 * row schema declares: `amounts`, `scalars`, `arrays`, `multiArrays`,
 * `money`, `bigMoney` and `rates`.
 *
 * WHY A COLLECTOR RATHER THAN A SECOND LIST OF LITERALS: the row schema wants
 * the inputs the expectations were computed from. Writing them out a second
 * time next to the operations would let the two copies drift apart silently,
 * and a drifted input field is worse than a missing one because it looks
 * authoritative. Every registering method below therefore RETURNS ITS
 * ARGUMENT, so an input is registered by wrapping the expression that
 * produces it - the registered object is, by construction, the very object
 * the operation consumed.
 *
 * Insertion order is preserved and duplicates are dropped, so the emitted
 * arrays are deterministic and free of repetition. `reset()` is called once
 * per row, so each row declares exactly its own inputs.
 */
class MathInputs {
  final List<CurrencyAmount> amounts = new ArrayList<>();
  final List<Double> scalars = new ArrayList<>();
  final List<CurrencyAmountArray> arrays = new ArrayList<>();
  final List<MultiCurrencyAmountArray> multiArrays = new ArrayList<>();
  final List<Money> money = new ArrayList<>();
  final List<BigMoney> bigMoney = new ArrayList<>();
  final List<FxRate> rates = new ArrayList<>();

  void reset() {
    amounts.clear();
    scalars.clear();
    arrays.clear();
    multiArrays.clear();
    money.clear();
    bigMoney.clear();
    rates.clear();
  }

  CurrencyAmount amount(CurrencyAmount value) {
    if (!amounts.contains(value)) {
      amounts.add(value);
    }
    return value;
  }

  /** Registers every component of a MultiCurrencyAmount as an `amounts` entry. */
  MultiCurrencyAmount multi(MultiCurrencyAmount value) {
    for (CurrencyAmount component : value.getAmounts()) {
      amount(component);
    }
    return value;
  }

  double scalar(double value) {
    Double boxed = Double.valueOf(value);
    if (!scalars.contains(boxed)) {
      scalars.add(boxed);
    }
    return value;
  }

  CurrencyAmountArray array(CurrencyAmountArray value) {
    if (!arrays.contains(value)) {
      arrays.add(value);
    }
    return value;
  }

  MultiCurrencyAmountArray multiArray(MultiCurrencyAmountArray value) {
    if (!multiArrays.contains(value)) {
      multiArrays.add(value);
    }
    return value;
  }

  Money money(Money value) {
    if (!money.contains(value)) {
      money.add(value);
    }
    return value;
  }

  BigMoney bigMoney(BigMoney value) {
    if (!bigMoney.contains(value)) {
      bigMoney.add(value);
    }
    return value;
  }

  FxRate rate(FxRate value) {
    if (!rates.contains(value)) {
      rates.add(value);
    }
    return value;
  }

  /*
   * Emits all seven declared input fields, always, in the schema's order. A
   * field a row does not use is emitted as an EMPTY ARRAY rather than being
   * omitted or set to null, so every row has the same shape and a decoder
   * needs no per-field optionality.
   */
  void emitInto(JObject row) {
    JArray amountJson = new JArray();
    for (CurrencyAmount value : amounts) {
      amountJson.add(jCurrencyAmount(value));
    }
    JArray scalarJson = new JArray();
    for (Double value : scalars) {
      scalarJson.add(jDbl(value.doubleValue()));
    }
    JArray arrayJson = new JArray();
    for (CurrencyAmountArray value : arrays) {
      arrayJson.add(jCurrencyAmountArray(value));
    }
    JArray multiArrayJson = new JArray();
    for (MultiCurrencyAmountArray value : multiArrays) {
      multiArrayJson.add(new JObject().set("arrays", jMultiCurrencyAmountArrayValues(value)));
    }
    JArray moneyJson = new JArray();
    for (Money value : money) {
      moneyJson.add(jMoney(value));
    }
    JArray bigMoneyJson = new JArray();
    for (BigMoney value : bigMoney) {
      bigMoneyJson.add(jBigMoney(value));
    }
    JArray rateJson = new JArray();
    for (FxRate value : rates) {
      rateJson.add(new JObject()
          .set("pair", jStr(value.getPair().toString()))
          .set("rate", jDbl(value.fxRate(value.getPair().getBase(), value.getPair().getCounter()))));
    }
    row.set("amounts", amountJson)
        .set("scalars", scalarJson)
        .set("arrays", arrayJson)
        .set("multiArrays", multiArrayJson)
        .set("money", moneyJson)
        .set("bigMoney", bigMoneyJson)
        .set("rates", rateJson);
  }
}

MathInputs MATH_IN = new MathInputs();

/*
 * One Currency.ini row: the currency instance plus its three configured
 * values.
 *
 * WHY THE INI RATHER THAN A HAND-TYPED CODE LIST: the row schema asks for
 * every currency, and `Currency.getAvailableCurrencies()` returns only the 55
 * ACTIVE ones - the 19 rows marked `historic = true` live in a separate map.
 * Reading the sections of Currency.ini yields all 74 codes in the file's own
 * order with no code written by hand here, and the counts asserted below
 * abort the capture if that resource ever changes.
 */
class CurrencyRow {
  final Currency currency;
  final int minorUnitDigits;
  final String triangulationCurrency;
  final boolean historic;

  CurrencyRow(Currency currency, int minorUnitDigits, String triangulationCurrency,
      boolean historic) {
    this.currency = currency;
    this.minorUnitDigits = minorUnitDigits;
    this.triangulationCurrency = triangulationCurrency;
    this.historic = historic;
  }
}

List<CurrencyRow> currencyIniRows() {
  IniFile ini = ResourceConfig.combinedIniFile("Currency.ini");
  List<CurrencyRow> rows = new ArrayList<>();
  for (String code : ini.sections()) {
    // CurrencyDataLoader accepts only three-letter upper-case sections.
    if (code.length() != 3) {
      continue;
    }
    PropertySet properties = ini.section(code);
    boolean historic = properties.keys().contains("historic")
        && Boolean.parseBoolean(properties.value("historic"));
    rows.add(new CurrencyRow(
        Currency.of(code),
        Integer.parseInt(properties.value("minorUnitDigits")),
        properties.value("triangulationCurrency"),
        historic));
  }
  return rows;
}

/*
 * Asserts that `Currency.of(code)` really does carry the configured data for
 * all 74 rows, the historic ones included.
 *
 * This is not obvious from the source and is exactly the kind of assumption a
 * sweep should not make silently: `Currency.of` consults the ACTIVE map and
 * otherwise falls through to `addCode`, which MINTS an instance with
 * minorUnitDigits 0 and USD triangulation for a code that is not configured -
 * but `Currency.DYNAMIC` is PRE-SEEDED with `loadCurrencies(true)`, so the 19
 * historic codes resolve to their configured instances (ATS -> 2 digits, EUR
 * triangulation) rather than to minted defaults. The per-currency sweeps in
 * the rows below depend on that, so it is checked, once, for every row of the
 * resource.
 */
void checkCurrencyIniAgainstCurrencyOf() {
  List<CurrencyRow> rows = currencyIniRows();
  int historicCount = 0;
  for (CurrencyRow row : rows) {
    String code = row.currency.getCode();
    CHECK.checkInt(FX_CURRENCY_MATH, "Currency.of(" + code + ") minorUnitDigits",
        row.minorUnitDigits, row.currency.getMinorUnitDigits());
    CHECK.checkEquals(FX_CURRENCY_MATH, "Currency.of(" + code + ") triangulationCurrency",
        row.triangulationCurrency, row.currency.getTriangulationCurrency().getCode());
    if (row.historic) {
      historicCount++;
    }
  }
  CHECK.checkCount(FX_CURRENCY_MATH, "Currency.ini rows", 74, rows.size());
  CHECK.checkCount(FX_CURRENCY_MATH, "Currency.ini historic rows", 19, historicCount);
  CHECK.checkCount(FX_CURRENCY_MATH, "Currency.getAvailableCurrencies()",
      rows.size() - historicCount, Currency.getAvailableCurrencies().size());
}

/*
 * The two literal amounts the per-currency sweeps use.
 *
 * They are LITERALS, not draws from the shared Random, and that is
 * deliberate: the seeded stream is consumed in fixture order, so a draw added
 * here would shift every later fixture's random values and change documents
 * this row has no business changing. `1234.56789` has five decimal places, so
 * rounding to 0, 2 and 3 minor units is visible in the result of every
 * currency; `1234.5678901234567` has more than twelve, so BigMoney's scale-12
 * HALF_UP rounding is exercised before the narrowing to minor units.
 */
double SWEEP_AMOUNT = 1234.56789d;
double SWEEP_AMOUNT_HIGH_PRECISION = 1234.5678901234567d;

/** CurrencyAmount operations, including the documented edge cases. */
Jn buildCurrencyAmountResults() {
  JArray results = new JArray();
  CurrencyAmount gbp100 = MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 100));
  CurrencyAmount gbp25 = MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 25.5));
  CurrencyAmount usd50 = MATH_IN.amount(CurrencyAmount.of(Currency.USD, 50));

  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.plus same currency",
      opEntry("plus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(gbp25)),
      () -> jCurrencyAmount(gbp100.plus(gbp25)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.minus same currency",
      opEntry("minus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(gbp25)),
      () -> jCurrencyAmount(gbp100.minus(gbp25)), MUST_SUCCEED);
  // CurrencyAmountTest: adding different currencies throws.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.plus different currency",
      opEntry("plus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(usd50)),
      () -> jCurrencyAmount(gbp100.plus(usd50)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.minus different currency",
      opEntry("minus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(usd50)),
      () -> jCurrencyAmount(gbp100.minus(usd50)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.multipliedBy",
      opEntry("multipliedBy").set("left", jCurrencyAmount(gbp100))
          .set("scalar", jDbl(MATH_IN.scalar(3.5))),
      () -> jCurrencyAmount(gbp100.multipliedBy(3.5)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.negated",
      opEntry("negated").set("left", jCurrencyAmount(gbp100)),
      () -> jCurrencyAmount(gbp100.negated()), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.positive",
      opEntry("positive").set("left", jCurrencyAmount(gbp100.negated())),
      () -> jCurrencyAmount(gbp100.negated().positive()), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.negative",
      opEntry("negative").set("left", jCurrencyAmount(gbp100)),
      () -> jCurrencyAmount(gbp100.negative()), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo rate",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.USD)).set("rate", jDbl(1.25)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.USD, 1.25)), MUST_SUCCEED);
  // CurrencyAmount.convertedTo to the SAME currency with a rate that is not 1
  // is rejected (the fixed-rate overload compares with a 1e-8 fuzzy equality).
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo same currency rate != 1",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.GBP)).set("rate", jDbl(1.25)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.GBP, 1.25)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo same currency rate 1",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.GBP)).set("rate", jDbl(1.0)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.GBP, 1.0)), MUST_SUCCEED);
  // CurrencyAmountTest.test_convertedTo_explicitRate uses 1.5 as its rejected
  // rate, so the literal the Java test names is captured as well as the 1.25
  // above. The fixed-rate overload compares with a 1e-8 fuzzy equality.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo same currency rate 1.5",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.GBP)).set("rate", jDbl(1.5)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.GBP, 1.5)), MUST_REJECT_ARGUMENT);

  // CurrencyAmountTest.test_of_Currency_negativeZero: -0.0 is normalised to
  // +0.0, which the Java test asserts through Double.doubleToLongBits.
  CurrencyAmount negativeZero = CurrencyAmount.of(Currency.GBP, -0.0);
  CHECK.checkInt(FX_CURRENCY_MATH, "CurrencyAmount.of(-0.0) doubleToLongBits", 0L,
      Double.doubleToLongBits(negativeZero.getAmount()));
  results.add(opEntry("of")
      .set("input", new JObject().set("currency", jName(Currency.GBP)).set("amount", jDbl(-0.0)))
      .set("result", jCurrencyAmount(negativeZero))
      .set("doubleToLongBits", jInt(Double.doubleToLongBits(negativeZero.getAmount()))));

  // CurrencyAmountTest.test_of_Currency_NaN: NaN is rejected.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.of(NaN)",
      opEntry("of").set("input",
          new JObject().set("currency", jName(Currency.GBP)).set("amount", jDbl(Double.NaN))),
      () -> jCurrencyAmount(CurrencyAmount.of(Currency.GBP, Double.NaN)), MUST_REJECT_ARGUMENT);

  // The normalisation is not confined to the factory: the private constructor
  // adds 0d to every amount, so an ARITHMETIC RESULT of -0.0 is normalised
  // too. 0.0 * -1.0 is -0.0 in IEEE-754 and +0.0 here, and the exact bit
  // pattern is asserted through doubleToLongBits, so this row is satisfied only
  // by a result whose sign bit matches.
  CurrencyAmount zeroTimesMinusOne = CurrencyAmount.of(Currency.GBP, 0.0).multipliedBy(-1.0);
  CHECK.checkInt(FX_CURRENCY_MATH, "CurrencyAmount 0.0 multipliedBy -1.0 doubleToLongBits", 0L,
      Double.doubleToLongBits(zeroTimesMinusOne.getAmount()));
  results.add(opEntry("multipliedBy")
      .set("left", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 0.0))))
      .set("scalar", jDbl(MATH_IN.scalar(-1.0)))
      .set("result", jCurrencyAmount(zeroTimesMinusOne))
      .set("doubleToLongBits", jInt(Double.doubleToLongBits(zeroTimesMinusOne.getAmount()))));

  // The documented numeric edge: infinities are accepted, but an arithmetic
  // result of NaN is rejected, so (+INF) + (-INF) throws.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount +INF plus -INF",
      opEntry("plus")
          .set("left", jCurrencyAmount(CurrencyAmount.of(Currency.GBP, Double.POSITIVE_INFINITY)))
          .set("right", jCurrencyAmount(CurrencyAmount.of(Currency.GBP, Double.NEGATIVE_INFINITY))),
      () -> jCurrencyAmount(CurrencyAmount.of(Currency.GBP, Double.POSITIVE_INFINITY)
          .plus(CurrencyAmount.of(Currency.GBP, Double.NEGATIVE_INFINITY))), MUST_REJECT_ARGUMENT);

  // Seeded random amounts across every configured currency.
  //
  // NOTE ON THE COUNT: the row schema calls for "all 74 currencies from
  // Currency.getAvailableCurrencies()", but that method returns 55. The 74
  // rows of Currency.ini include 19 marked `historic = true`, and only the
  // non-historic ones are loaded into the configured set. 55 is therefore the
  // correct count for this method, and the manifest separately records all 74
  // INI rows with their historic flags. Both counts are asserted.
  List<Currency> configured = new ArrayList<>(Currency.getAvailableCurrencies());
  CHECK.checkCount(FX_CURRENCY_MATH, "Currency.getAvailableCurrencies()", 55, configured.size());
  for (Currency currency : configured) {
    double amount = nextRandomAmount();
    CurrencyAmount value = CurrencyAmount.of(currency, amount);
    addOperation(results, FX_CURRENCY_MATH, "random CurrencyAmount " + currency,
        opEntry("multipliedBy").set("left", jCurrencyAmount(value)).set("scalar", jDbl(1.5)),
        () -> jCurrencyAmount(value.multipliedBy(1.5)), CAPTURE_ONLY);
    CHECK.countCaptureOnly(FX_CURRENCY_MATH);
  }
  // The remaining 19 currencies - the `historic = true` rows of Currency.ini,
  // which getAvailableCurrencies() excludes - complete this fixture's 74-code
  // inventory. They carry the LITERAL sweep amount rather than a draw from the
  // shared Random, because a draw here would shift the random values of every
  // fixture captured after this one.
  int historicSwept = 0;
  for (CurrencyRow row : currencyIniRows()) {
    if (!row.historic) {
      continue;
    }
    CurrencyAmount value = CurrencyAmount.of(row.currency, SWEEP_AMOUNT);
    addOperation(results, FX_CURRENCY_MATH, "historic CurrencyAmount " + row.currency,
        opEntry("multipliedBy").set("left", jCurrencyAmount(value)).set("scalar", jDbl(1.5))
            .set("minorUnitDigits", jInt(row.currency.getMinorUnitDigits()))
            .set("captureOnly", jBool(true)),
        () -> jCurrencyAmount(value.multipliedBy(1.5)), CAPTURE_ONLY);
    CHECK.countCaptureOnly(FX_CURRENCY_MATH);
    historicSwept++;
  }
  CHECK.checkCount(FX_CURRENCY_MATH, "historic currencies swept", 19, historicSwept);
  return results;
}

/** Money and BigMoney rounding, arithmetic and conversion. */
Jn buildMoneyResults(boolean bigMoney) {
  JArray results = new JArray();
  // MoneyTest and BigMoneyTest use only AUD, RON (2 minor-unit digits) and
  // BHD (3). The row schema requires every distinct minorUnitDigits, so JPY
  // (0 digits) is added here. The JPY rows are CAPTURE-ONLY: no Java test
  // constant exists for them, and the summary counts them as such rather than
  // pretending they were cross-checked.
  Object[][] inputs = {
      {Currency.AUD, 100.12, Boolean.FALSE},
      {Currency.AUD, 100.13, Boolean.FALSE},
      {Currency.AUD, 100.1249, Boolean.FALSE},
      {Currency.AUD, 100.125, Boolean.FALSE},
      {Currency.RON, 200.23, Boolean.FALSE},
      {Currency.RON, 200.2345, Boolean.FALSE},
      {Currency.RON, 1.005, Boolean.FALSE},
      {Currency.BHD, 100.12, Boolean.FALSE},
      {Currency.BHD, 100.1249, Boolean.FALSE},
      {Currency.BHD, 100.125, Boolean.FALSE},
      {Currency.BHD, 1.23456, Boolean.FALSE},
      {Currency.JPY, 123.456, Boolean.TRUE},
      {Currency.JPY, 0.5, Boolean.TRUE},
      {Currency.JPY, 123.4567890123456, Boolean.TRUE},
  };
  for (Object[] input : inputs) {
    Currency currency = (Currency) input[0];
    double amount = ((Number) input[1]).doubleValue();
    boolean captureOnly = ((Boolean) input[2]).booleanValue();
    JObject entry = opEntry("of")
        .set("currency", jName(currency))
        .set("amount", jDbl(amount))
        .set("minorUnitDigits", jInt(currency.getMinorUnitDigits()))
        .set("captureOnly", jBool(captureOnly));
    if (bigMoney) {
      MATH_IN.bigMoney(BigMoney.of(currency, amount));
      addOperation(results, FX_CURRENCY_MATH, "BigMoney.of " + currency + " " + amount, entry,
          () -> jBigMoney(BigMoney.of(currency, amount)), MUST_SUCCEED);
    } else {
      MATH_IN.money(Money.of(currency, amount));
      addOperation(results, FX_CURRENCY_MATH, "Money.of " + currency + " " + amount, entry,
          () -> jMoney(Money.of(currency, amount)), MUST_SUCCEED);
    }
    if (captureOnly) {
      CHECK.countCaptureOnly(FX_CURRENCY_MATH);
    }
  }
  if (bigMoney) {
    BigMoney left = MATH_IN.bigMoney(BigMoney.of(Currency.RON, 200.2345));
    BigMoney right = MATH_IN.bigMoney(BigMoney.of(Currency.RON, 100.1249));
    BigMoney other = MATH_IN.bigMoney(BigMoney.of(Currency.AUD, 100));
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.plus",
        opEntry("plus").set("left", jBigMoney(left)).set("right", jBigMoney(right)),
        () -> jBigMoney(left.plus(right)), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.minus",
        opEntry("minus").set("left", jBigMoney(left)).set("right", jBigMoney(right)),
        () -> jBigMoney(left.minus(right)), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.plus different currency",
        opEntry("plus").set("left", jBigMoney(left)).set("right", jBigMoney(other)),
        () -> jBigMoney(left.plus(other)), MUST_REJECT_ARGUMENT);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.minus different currency",
        opEntry("minus").set("left", jBigMoney(left)).set("right", jBigMoney(other)),
        () -> jBigMoney(left.minus(other)), MUST_REJECT_ARGUMENT);
    // multipliedBy takes a long, not a double.
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.multipliedBy",
        opEntry("multipliedBy").set("left", jBigMoney(left)).set("scalar", jInt(3L)),
        () -> jBigMoney(left.multipliedBy(3L)), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney",
        opEntry("toMoney").set("left", jBigMoney(left)),
        () -> jMoney(left.toMoney()), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney JPY",
        opEntry("toMoney").set("left", jBigMoney(BigMoney.of(Currency.JPY, 123.4567890123456))),
        () -> jMoney(BigMoney.of(Currency.JPY, 123.4567890123456).toMoney()), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.convertedTo",
        opEntry("convertedTo").set("left", jBigMoney(other)).set("target", jName(Currency.USD))
            .set("rate", jDbl(0.65)),
        () -> jBigMoney(other.convertedTo(Currency.USD, java.math.BigDecimal.valueOf(0.65))),
        MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.convertedTo same currency rate != 1",
        opEntry("convertedTo").set("left", jBigMoney(other)).set("target", jName(Currency.AUD))
            .set("rate", jDbl(1.25)),
        () -> jBigMoney(other.convertedTo(Currency.AUD, java.math.BigDecimal.valueOf(1.25))),
        MUST_REJECT_ARGUMENT);
    // The same-currency rule has a SUCCEEDING side too: a rate of exactly 1
    // is the no-conversion case and must return the value unchanged.
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.convertedTo same currency rate 1",
        opEntry("convertedTo").set("left", jBigMoney(left)).set("target", jName(Currency.RON))
            .set("rate", jDbl(1.0)),
        () -> jBigMoney(left.convertedTo(Currency.RON, java.math.BigDecimal.ONE)), MUST_SUCCEED);

    // BigMoneyTest's discriminating values. BigMoney.of rounds to scale 12
    // HALF_UP, so the first three survive unchanged and the fourth - which has
    // fifteen decimal places - is the row where the rounding actually bites
    // (BigMoneyTest asserts the same thing through parse: "AUD
    // 1.123456789012345" equals "AUD 1.123456789012").
    double[] bigMoneyScaleInputs = {1.000009d, 9.99999999d, 1.441d, 1.123456789012345d};
    for (double amount : bigMoneyScaleInputs) {
      BigMoney value = MATH_IN.bigMoney(BigMoney.of(Currency.GBP, amount));
      addOperation(results, FX_CURRENCY_MATH, "BigMoney.of GBP " + amount,
          opEntry("of").set("currency", jName(Currency.GBP)).set("amount", jDbl(amount))
              .set("minorUnitDigits", jInt(Currency.GBP.getMinorUnitDigits()))
              .set("captureOnly", jBool(false)),
          () -> jBigMoney(value), MUST_SUCCEED);
    }
    // ... and its narrowing to minor units, which is where the twelve-digit
    // value meets the currency's two.
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney high precision",
        opEntry("toMoney")
            .set("left", jBigMoney(MATH_IN.bigMoney(BigMoney.of(Currency.AUD, 1.123456789012345d)))),
        () -> jMoney(BigMoney.of(Currency.AUD, 1.123456789012345d).toMoney()), MUST_SUCCEED);

    // BigMoneyTest.test_roundToScale / test_roundToScaleNegative, row for row:
    // the six rounding modes at scale 2 and the three negative scales. Every
    // expectation here is also asserted in checkMoneyConstants.
    Object[][] roundings = {
        {1.441d, Integer.valueOf(2), RoundingMode.CEILING},
        {1.441d, Integer.valueOf(2), RoundingMode.UP},
        {1.446d, Integer.valueOf(2), RoundingMode.HALF_UP},
        {1.449d, Integer.valueOf(2), RoundingMode.FLOOR},
        {1.449d, Integer.valueOf(2), RoundingMode.DOWN},
        {1.444d, Integer.valueOf(2), RoundingMode.HALF_DOWN},
        {780001d, Integer.valueOf(-3), RoundingMode.CEILING},
        {780001d, Integer.valueOf(-2), RoundingMode.UP},
        {780005d, Integer.valueOf(-1), RoundingMode.HALF_UP},
        {780999d, Integer.valueOf(-3), RoundingMode.FLOOR},
        {780699d, Integer.valueOf(-2), RoundingMode.DOWN},
        {780234d, Integer.valueOf(-1), RoundingMode.HALF_DOWN},
    };
    for (Object[] rounding : roundings) {
      double amount = ((Number) rounding[0]).doubleValue();
      int scale = ((Integer) rounding[1]).intValue();
      RoundingMode mode = (RoundingMode) rounding[2];
      BigMoney value = MATH_IN.bigMoney(BigMoney.of(Currency.GBP, amount));
      addOperation(results, FX_CURRENCY_MATH,
          "BigMoney.roundToScale " + amount + " " + scale + " " + mode,
          opEntry("roundToScale").set("left", jBigMoney(value)).set("scale", jInt(scale))
              .set("roundingMode", jStr(mode.name())),
          () -> jBigMoney(value.roundToScale(scale, mode)), MUST_SUCCEED);
    }

    // Every currency, active and historic: the `left` operand shows
    // BigMoney.of rounding a fifteen-decimal amount to scale 12, and the
    // result shows toMoney narrowing that to the currency's minor units.
    // Capture-only: no Java test states these per-currency values.
    int sweptBig = 0;
    for (CurrencyRow row : currencyIniRows()) {
      BigMoney value = BigMoney.of(row.currency, SWEEP_AMOUNT_HIGH_PRECISION);
      addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney sweep " + row.currency,
          opEntry("toMoney").set("left", jBigMoney(value))
              .set("minorUnitDigits", jInt(row.currency.getMinorUnitDigits()))
              .set("captureOnly", jBool(true)),
          () -> jMoney(value.toMoney()), CAPTURE_ONLY);
      CHECK.countCaptureOnly(FX_CURRENCY_MATH);
      sweptBig++;
    }
    CHECK.checkCount(FX_CURRENCY_MATH, "BigMoney.toMoney currencies swept", 74, sweptBig);
  } else {
    Money left = MATH_IN.money(Money.of(Currency.RON, 200.23));
    Money right = MATH_IN.money(Money.of(Currency.RON, 100.12));
    Money other = MATH_IN.money(Money.of(Currency.AUD, 100.12));
    addOperation(results, FX_CURRENCY_MATH, "Money.plus",
        opEntry("plus").set("left", jMoney(left)).set("right", jMoney(right)),
        () -> jMoney(left.plus(right)), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "Money.minus",
        opEntry("minus").set("left", jMoney(left)).set("right", jMoney(right)),
        () -> jMoney(left.minus(right)), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "Money.plus different currency",
        opEntry("plus").set("left", jMoney(left)).set("right", jMoney(other)),
        () -> jMoney(left.plus(other)), MUST_REJECT_ARGUMENT);
    addOperation(results, FX_CURRENCY_MATH, "Money.minus different currency",
        opEntry("minus").set("left", jMoney(left)).set("right", jMoney(other)),
        () -> jMoney(left.minus(other)), MUST_REJECT_ARGUMENT);
    addOperation(results, FX_CURRENCY_MATH, "Money.multipliedBy",
        opEntry("multipliedBy").set("left", jMoney(left)).set("scalar", jInt(3L)),
        () -> jMoney(left.multipliedBy(3L)), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.USD))
            .set("rate", jDbl(0.65)),
        () -> jMoney(other.convertedTo(Currency.USD, java.math.BigDecimal.valueOf(0.65))),
        MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo same currency rate != 1",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.AUD))
            .set("rate", jDbl(1.25)),
        () -> jMoney(other.convertedTo(Currency.AUD, java.math.BigDecimal.valueOf(1.25))),
        MUST_REJECT_ARGUMENT);
    // MoneyTest.testConvertedToWithExplicitRateForSameCurrency rejects 1.1
    // with "FX rate must be 1 when no conversion required"; the succeeding
    // side of the same rule is a rate of exactly 1, from
    // MoneyTest.testConvertedToWithExplicitRate.
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo same currency rate 1.1",
        opEntry("convertedTo").set("left", jMoney(left)).set("target", jName(Currency.RON))
            .set("rate", jDbl(1.1)),
        () -> jMoney(left.convertedTo(Currency.RON, Decimal.of(1.1))), MUST_REJECT_ARGUMENT);
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo same currency rate 1",
        opEntry("convertedTo").set("left", jMoney(left)).set("target", jName(Currency.RON))
            .set("rate", jDbl(1.0)),
        () -> jMoney(left.convertedTo(Currency.RON, Decimal.of(1))), MUST_SUCCEED);
    // MoneyTest.testConvertedToWithExplicitRate: AUD 100.12 at 2.6 -> RON
    // 260.31, rounded to RON's two minor units.
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo cross currency explicit rate",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.RON))
            .set("rate", jDbl(2.6)),
        () -> jMoney(other.convertedTo(Currency.RON, Decimal.of(2.6d))), MUST_SUCCEED);
    // MoneyTest.testConvertedToWithRateProvider: the same conversion through a
    // provider, expressed here as the FxRate that provides it (FxRate IS an
    // FxRateProvider), so the fixture carries the rate in its `rates` list
    // rather than an opaque lambda.
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo rate provider",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.RON))
            .set("rates", jRateEntries(rateEntries(Currency.AUD, Currency.RON, 2.5))),
        () -> jMoney(other.convertedTo(Currency.RON,
            MATH_IN.rate(FxRate.of(Currency.AUD, Currency.RON, 2.5)))), MUST_SUCCEED);
    addOperation(results, FX_CURRENCY_MATH, "Money.toBigMoney",
        opEntry("toBigMoney").set("left", jMoney(left)),
        () -> jBigMoney(left.toBigMoney()), MUST_SUCCEED);

    // Every currency, active and historic, at one literal amount with five
    // decimal places: the result shows Money.of rounding HALF_UP to that
    // currency's minor units, so all three distinct minorUnitDigits values -
    // 0, 2 and 3 - are exercised on every currency that declares them rather
    // than on one representative. Capture-only: no Java test states these
    // per-currency values, though `checkCurrencyIniAgainstCurrencyOf` has
    // already asserted the digit counts they depend on.
    int swept = 0;
    for (CurrencyRow row : currencyIniRows()) {
      addOperation(results, FX_CURRENCY_MATH, "Money.of sweep " + row.currency,
          opEntry("of").set("currency", jName(row.currency)).set("amount", jDbl(SWEEP_AMOUNT))
              .set("minorUnitDigits", jInt(row.currency.getMinorUnitDigits()))
              .set("captureOnly", jBool(true)),
          () -> jMoney(Money.of(row.currency, SWEEP_AMOUNT)), CAPTURE_ONLY);
      CHECK.countCaptureOnly(FX_CURRENCY_MATH);
      swept++;
    }
    CHECK.checkCount(FX_CURRENCY_MATH, "Money.of currencies swept", 74, swept);
  }
  return results;
}

/** CurrencyAmountArray operations, including the two composed ones. */
Jn buildCurrencyAmountArrayResults() {
  JArray results = new JArray();
  CurrencyAmountArray gbpArray =
      MATH_IN.array(CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d)));
  CurrencyAmountArray gbpOther =
      MATH_IN.array(CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(0.5d, 1.5d, 2.5d)));
  CurrencyAmountArray usdArray =
      MATH_IN.array(CurrencyAmountArray.of(Currency.USD, DoubleArray.of(1d, 2d, 3d)));
  CurrencyAmountArray shortArray =
      MATH_IN.array(CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d)));

  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus array",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(gbpOther)),
      () -> jCurrencyAmountArray(gbpArray.plus(gbpOther)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus array",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(gbpOther)),
      () -> jCurrencyAmountArray(gbpArray.minus(gbpOther)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus amount",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 10)))),
      () -> jCurrencyAmountArray(gbpArray.plus(CurrencyAmount.of(Currency.GBP, 10))),
      MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus currency mismatch",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(usdArray)),
      () -> jCurrencyAmountArray(gbpArray.plus(usdArray)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus size mismatch",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(shortArray)),
      () -> jCurrencyAmountArray(gbpArray.plus(shortArray)), MUST_REJECT_ARGUMENT);
  // The three failing shapes of `minus` mirror those of `plus`: a different
  // currency, a different size, and a scalar amount in another currency. Each
  // is captured separately because the messages differ ("Currencies must be
  // equal ..." against "Sizes must be equal ..."), so every row pins its own
  // rejection category rather than merely "rejected".
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus currency mismatch",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(usdArray)),
      () -> jCurrencyAmountArray(gbpArray.minus(usdArray)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus size mismatch",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(shortArray)),
      () -> jCurrencyAmountArray(gbpArray.minus(shortArray)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus amount currency mismatch",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.USD, 10)))),
      () -> jCurrencyAmountArray(gbpArray.plus(CurrencyAmount.of(Currency.USD, 10))),
      MUST_REJECT_ARGUMENT);
  // CurrencyAmountArrayTest.test_minus_currencyAmount
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus amount",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 0.5)))),
      () -> jCurrencyAmountArray(gbpArray.minus(CurrencyAmount.of(Currency.GBP, 0.5))),
      MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus amount currency mismatch",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.USD, 0.5)))),
      () -> jCurrencyAmountArray(gbpArray.minus(CurrencyAmount.of(Currency.USD, 0.5))),
      MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.convertedTo",
      opEntry("convertedTo").set("left", jCurrencyAmountArray(gbpArray))
          .set("target", jName(Currency.USD)).set("rate", jDbl(1.6)),
      () -> jCurrencyAmountArray(gbpArray.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.GBP, Currency.USD, 1.6)))),
      MUST_SUCCEED);
  // Composed: the Java type has no multipliedBy, so the expectation is built
  // from of(currency, getValues().multipliedBy(scalar)).
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.multipliedBy (composed)",
      opEntry("multipliedBy").set("left", jCurrencyAmountArray(gbpArray))
          .set("scalar", jDbl(MATH_IN.scalar(2.5)))
          .set("composed", jBool(true)),
      () -> jCurrencyAmountArray(
          CurrencyAmountArray.of(gbpArray.getCurrency(), gbpArray.getValues().multipliedBy(2.5))),
      MUST_SUCCEED);
  // Composed: likewise mapAmounts, using the documented function.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.mapAmounts (composed)",
      opEntry("mapAmounts").set("left", jCurrencyAmountArray(gbpArray))
          .set("mapAmountsFn", jStr(MAP_AMOUNTS_FN)).set("composed", jBool(true)),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(gbpArray.getCurrency(),
          gbpArray.getValues().map(v -> mapAmountsFn(v)))), MUST_SUCCEED);
  // of(List<CurrencyAmount>) and its mixed-currency rejection.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of list",
      opEntry("of").set("input", jDoubles(new double[] {4d, 5d, 6d}))
          .set("currency", jName(Currency.GBP)),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(Arrays.asList(
          CurrencyAmount.of(Currency.GBP, 4), CurrencyAmount.of(Currency.GBP, 5),
          CurrencyAmount.of(Currency.GBP, 6)))), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of mixed currencies",
      opEntry("of").set("input", jStr("GBP 4, USD 5")),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(Arrays.asList(
          CurrencyAmount.of(Currency.GBP, 4), CurrencyAmount.of(Currency.USD, 5)))),
      MUST_REJECT_ARGUMENT);
  // of(size, valueFunction) - CurrencyAmountArrayTest.test_of_function and
  // test_of_function_mixedCurrency. The second rejects with a different
  // message from the list form ("Currencies differ: GBP and USD"), so both
  // paths are captured.
  List<CurrencyAmount> functionValues = Arrays.asList(
      CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.GBP, 2),
      CurrencyAmount.of(Currency.GBP, 3));
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of function",
      opEntry("of").set("size", jInt(3)).set("currency", jName(Currency.GBP))
          .set("input", jDoubles(new double[] {1d, 2d, 3d})),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(3, i -> functionValues.get(i))),
      MUST_SUCCEED);
  List<CurrencyAmount> mixedFunctionValues = Arrays.asList(
      CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.USD, 2),
      CurrencyAmount.of(Currency.GBP, 3));
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of function mixed currencies",
      opEntry("of").set("size", jInt(3)).set("input", jStr("GBP 1, USD 2, GBP 3")),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(3, i -> mixedFunctionValues.get(i))),
      MUST_REJECT_ARGUMENT);
  // CurrencyAmountArrayTest.test_convertedTo_missingFxRate: converting with a
  // rate for an unrelated pair fails rather than silently leaving the values
  // unconverted.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.convertedTo missing rate",
      opEntry("convertedTo").set("left", jCurrencyAmountArray(gbpArray))
          .set("target", jName(Currency.USD))
          .set("rates", jRateEntries(rateEntries(Currency.EUR, Currency.USD, 1.61))),
      () -> jCurrencyAmountArray(gbpArray.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.EUR, Currency.USD, 1.61)))), MUST_REJECT_ARGUMENT);

  // SIGNED ZERO THROUGH THE ARRAY TYPES - three different answers, all pinned.
  //
  // CurrencyAmount.of normalises -0.0 to +0.0, but CurrencyAmountArray stores
  // a DoubleArray and does NOT: the element keeps its sign bit. Multiplying by
  // -1.0 therefore flips the signs of both zeros, while `get(i)` hands the
  // element to CurrencyAmount.of and so normalises it. All three results are
  // asserted separately through doubleToLongBits, so none of them can be
  // satisfied by a sign-blind comparison.
  CurrencyAmountArray signedZeroArray =
      MATH_IN.array(CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(-0.0, 0.0, 1.0)));
  CHECK.checkInt(FX_CURRENCY_MATH, "CurrencyAmountArray keeps -0.0",
      Double.doubleToLongBits(-0.0), Double.doubleToLongBits(signedZeroArray.getValues().get(0)));
  CHECK.checkInt(FX_CURRENCY_MATH, "CurrencyAmountArray.get normalises -0.0", 0L,
      Double.doubleToLongBits(signedZeroArray.get(0).getAmount()));
  results.add(opEntry("of")
      .set("currency", jName(Currency.GBP))
      .set("input", jDoubles(new double[] {-0.0, 0.0, 1.0}))
      .set("result", jCurrencyAmountArray(signedZeroArray))
      .set("doubleToLongBits", jInt(Double.doubleToLongBits(signedZeroArray.getValues().get(0)))));
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.multipliedBy signed zero (composed)",
      opEntry("multipliedBy").set("left", jCurrencyAmountArray(signedZeroArray))
          .set("scalar", jDbl(MATH_IN.scalar(-1.0)))
          .set("composed", jBool(true)),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(signedZeroArray.getCurrency(),
          signedZeroArray.getValues().multipliedBy(-1.0))), MUST_SUCCEED);
  results.add(opEntry("get")
      .set("left", jCurrencyAmountArray(signedZeroArray))
      .set("index", jInt(0))
      .set("result", jCurrencyAmount(signedZeroArray.get(0)))
      .set("doubleToLongBits", jInt(Double.doubleToLongBits(signedZeroArray.get(0).getAmount()))));
  return results;
}

/** MultiCurrencyAmount operations. */
Jn buildMultiCurrencyAmountResults() {
  JArray results = new JArray();
  MultiCurrencyAmount base = MATH_IN.multi(MultiCurrencyAmount.of(
      CurrencyAmount.of(Currency.GBP, 100), CurrencyAmount.of(Currency.USD, 200)));
  MultiCurrencyAmount other = MATH_IN.multi(MultiCurrencyAmount.of(
      CurrencyAmount.of(Currency.USD, 50), CurrencyAmount.of(Currency.EUR, 75)));

  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.of",
      opEntry("of").set("amounts", jMultiCurrencyAmounts(base)),
      () -> jMultiCurrencyAmounts(base), MUST_SUCCEED);
  // of() rejects duplicate currencies; total() merges them.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.of duplicate currency",
      opEntry("of").set("input", jStr("GBP 100, GBP 200")),
      () -> jMultiCurrencyAmounts(MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 100), CurrencyAmount.of(Currency.GBP, 200))),
      MUST_REJECT_ARGUMENT);
  // The empty amount is a legal value rather than an error, and it is the
  // identity of MultiCurrencyAmount addition, so its encoding - an empty amount
  // list - is pinned here.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.empty",
      opEntry("of").set("input", jStr("empty")),
      () -> jMultiCurrencyAmounts(MultiCurrencyAmount.empty()), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.total duplicate currency",
      opEntry("total").set("input", jStr("GBP 100, GBP 200")),
      () -> jMultiCurrencyAmounts(MultiCurrencyAmount.total(Arrays.asList(
          CurrencyAmount.of(Currency.GBP, 100), CurrencyAmount.of(Currency.GBP, 200)))),
      MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.plus multi",
      opEntry("plus").set("left", jMultiCurrencyAmounts(base))
          .set("right", jMultiCurrencyAmounts(other)),
      () -> jMultiCurrencyAmounts(base.plus(other)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.minus multi",
      opEntry("minus").set("left", jMultiCurrencyAmounts(base))
          .set("right", jMultiCurrencyAmounts(other)),
      () -> jMultiCurrencyAmounts(base.minus(other)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.multipliedBy",
      opEntry("multipliedBy").set("left", jMultiCurrencyAmounts(base))
          .set("scalar", jDbl(MATH_IN.scalar(1.5))),
      () -> jMultiCurrencyAmounts(base.multipliedBy(1.5)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.mapAmounts",
      opEntry("mapAmounts").set("left", jMultiCurrencyAmounts(base))
          .set("mapAmountsFn", jStr(MAP_AMOUNTS_FN)),
      () -> jMultiCurrencyAmounts(base.mapAmounts(v -> mapAmountsFn(v))), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.getAmount known",
      opEntry("getAmount").set("left", jMultiCurrencyAmounts(base))
          .set("currency", jName(Currency.GBP)),
      () -> jCurrencyAmount(base.getAmount(Currency.GBP)), MUST_SUCCEED);
  // getAmount for a currency the amount does not contain throws.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.getAmount unknown",
      opEntry("getAmount").set("left", jMultiCurrencyAmounts(base))
          .set("currency", jName(Currency.CHF)),
      () -> jCurrencyAmount(base.getAmount(Currency.CHF)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.convertedTo",
      opEntry("convertedTo").set("left", jMultiCurrencyAmounts(base))
          .set("target", jName(Currency.USD))
          .set("rates", jRateEntries(rateEntries(Currency.GBP, Currency.USD, 1.6))),
      () -> jCurrencyAmount(base.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.GBP, Currency.USD, 1.6)))),
      MUST_SUCCEED);
  return results;
}

/** MultiCurrencyAmountArray operations, including the two composed ones. */
Jn buildMultiCurrencyAmountArrayResults() {
  JArray results = new JArray();
  Map<Currency, DoubleArray> baseValues = new LinkedHashMap<>();
  baseValues.put(Currency.GBP, DoubleArray.of(1d, 2d, 3d));
  baseValues.put(Currency.USD, DoubleArray.of(10d, 20d, 30d));
  MultiCurrencyAmountArray base = MATH_IN.multiArray(MultiCurrencyAmountArray.of(baseValues));
  Map<Currency, DoubleArray> otherValues = new LinkedHashMap<>();
  otherValues.put(Currency.USD, DoubleArray.of(1d, 1d, 1d));
  otherValues.put(Currency.EUR, DoubleArray.of(5d, 5d, 5d));
  MultiCurrencyAmountArray other = MATH_IN.multiArray(MultiCurrencyAmountArray.of(otherValues));
  Map<Currency, DoubleArray> shortValues = new LinkedHashMap<>();
  shortValues.put(Currency.GBP, DoubleArray.of(1d, 2d));
  MultiCurrencyAmountArray shortArray =
      MATH_IN.multiArray(MultiCurrencyAmountArray.of(shortValues));

  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of",
      opEntry("of").set("left", jMultiCurrencyAmountArray(base)),
      () -> jMultiCurrencyAmountArray(base), MUST_SUCCEED);
  // of() with unequal array lengths across currencies is rejected.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of unequal lengths",
      opEntry("of").set("input", jStr("GBP[1,2,3], USD[1,2]")),
      () -> {
        Map<Currency, DoubleArray> bad = new LinkedHashMap<>();
        bad.put(Currency.GBP, DoubleArray.of(1d, 2d, 3d));
        bad.put(Currency.USD, DoubleArray.of(1d, 2d));
        return jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(bad));
      }, MUST_REJECT_ARGUMENT);
  // RAGGED INPUT IS NOT AN ERROR ON THIS PATH, and the contrast with the
  // rejected map form above is the point. MultiCurrencyAmountArrayTest:62-79
  // builds the array from a LIST of MultiCurrencyAmount, where a currency
  // missing from one element is ZERO-FILLED for that index rather than
  // rejected - so GBP becomes [0, 21, 0] - while a currency that appears in no
  // element at all remains unknown and `getValues` rejects it.
  List<MultiCurrencyAmount> raggedAmounts = Arrays.asList(
      MultiCurrencyAmount.of(CurrencyAmount.of(Currency.EUR, 4)),
      MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 21),
          CurrencyAmount.of(Currency.USD, 32),
          CurrencyAmount.of(Currency.EUR, 43)),
      MultiCurrencyAmount.of(CurrencyAmount.of(Currency.EUR, 44)));
  MultiCurrencyAmountArray raggedArray =
      MATH_IN.multiArray(MultiCurrencyAmountArray.of(raggedAmounts));
  CHECK.checkInt(FX_CURRENCY_MATH, "ragged MultiCurrencyAmountArray size", 3,
      raggedArray.getSize());
  CHECK.checkEquals(FX_CURRENCY_MATH, "ragged MultiCurrencyAmountArray GBP",
      DoubleArray.of(0d, 21d, 0d), raggedArray.getValues(Currency.GBP));
  CHECK.checkEquals(FX_CURRENCY_MATH, "ragged MultiCurrencyAmountArray USD",
      DoubleArray.of(0d, 32d, 0d), raggedArray.getValues(Currency.USD));
  CHECK.checkEquals(FX_CURRENCY_MATH, "ragged MultiCurrencyAmountArray EUR",
      DoubleArray.of(4d, 43d, 44d), raggedArray.getValues(Currency.EUR));
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of ragged list",
      opEntry("of").set("input", jStr("[EUR 4], [GBP 21, USD 32, EUR 43], [EUR 44]")),
      () -> jMultiCurrencyAmountArray(raggedArray), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of ragged function",
      opEntry("of").set("size", jInt(3))
          .set("input", jStr("[EUR 4], [GBP 21, USD 32, EUR 43], [EUR 44]")),
      () -> jMultiCurrencyAmountArray(
          MultiCurrencyAmountArray.of(3, i -> raggedAmounts.get(i))), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.getValues unknown ragged",
      opEntry("getValues").set("left", jMultiCurrencyAmountArray(raggedArray))
          .set("currency", jName(Currency.AUD)),
      () -> jDoubleArray(raggedArray.getValues(Currency.AUD)), MUST_REJECT_ARGUMENT);
  // MultiCurrencyAmountArrayTest.test_empty_amounts: two entries with no
  // currencies at all, so the expected structure carries size 2 and an empty
  // currency list.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of empty amounts",
      opEntry("of").set("size", jInt(2)).set("input", jStr("[], []")),
      () -> jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(
          MultiCurrencyAmount.empty(), MultiCurrencyAmount.empty())), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.getValues known",
      opEntry("getValues").set("left", jMultiCurrencyAmountArray(base))
          .set("currency", jName(Currency.GBP)),
      () -> jDoubleArray(base.getValues(Currency.GBP)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.getValues unknown",
      opEntry("getValues").set("left", jMultiCurrencyAmountArray(base))
          .set("currency", jName(Currency.CHF)),
      () -> jDoubleArray(base.getValues(Currency.CHF)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.plus array",
      opEntry("plus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(other)),
      () -> jMultiCurrencyAmountArray(base.plus(other)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.minus array",
      opEntry("minus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(other)),
      () -> jMultiCurrencyAmountArray(base.minus(other)), MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.plus size mismatch",
      opEntry("plus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(shortArray)),
      () -> jMultiCurrencyAmountArray(base.plus(shortArray)), MUST_REJECT_ARGUMENT);
  // MultiCurrencyAmountArrayTest.test_minusArray / test_plusDifferentSize: the
  // subtracting side of both shapes, so both addition and subtraction are
  // captured.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.minus size mismatch",
      opEntry("minus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(shortArray)),
      () -> jMultiCurrencyAmountArray(base.minus(shortArray)), MUST_REJECT_ARGUMENT);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.minus multi",
      opEntry("minus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmounts(MultiCurrencyAmount.of(
              CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.CHF, 2)))),
      () -> jMultiCurrencyAmountArray(base.minus(MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.CHF, 2)))),
      MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.plus multi",
      opEntry("plus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmounts(MultiCurrencyAmount.of(
              CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.USD, 2)))),
      () -> jMultiCurrencyAmountArray(base.plus(MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.USD, 2)))),
      MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.total",
      opEntry("total").set("input", jStr("GBP[1,2,3] + USD[10,20,30]")),
      () -> jMultiCurrencyAmountArray(MultiCurrencyAmountArray.total(Arrays.asList(
          CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d)),
          CurrencyAmountArray.of(Currency.USD, DoubleArray.of(10d, 20d, 30d))))),
      MUST_SUCCEED);
  // The rate is captured ON THE OPERATION and registered in the row's `rates`
  // list, exactly as `MultiCurrencyAmount.convertedTo` does. Without it the
  // row would state a converted result whose provider appears nowhere in the
  // document, and the only way to replay it would be to borrow a rate from an
  // unrelated row - which is not a replay of what Java was given here.
  //
  // No Java test states this combination, so the expectation is tied to the
  // captured rate here instead: converting GBP[1,2,3] + USD[10,20,30] into USD
  // at GBP/USD 1.6 is the GBP leg scaled by the rate plus the USD leg
  // unchanged, element by element. A rate that did not produce the numbers
  // beside it would therefore fail the capture rather than be published.
  DoubleArray convertedAtOneSix = base
      .convertedTo(Currency.USD, FxRate.of(Currency.GBP, Currency.USD, 1.6))
      .getValues();
  for (int i = 0; i < 3; i++) {
    CHECK.checkClose(FX_CURRENCY_MATH,
        "MultiCurrencyAmountArray.convertedTo at GBP/USD 1.6 [" + i + "]",
        (i + 1) * 1.6 + (i + 1) * 10d, convertedAtOneSix.get(i), 1e-12,
        "GBP leg at the captured rate plus the USD leg");
  }
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.convertedTo",
      opEntry("convertedTo").set("left", jMultiCurrencyAmountArray(base))
          .set("target", jName(Currency.USD))
          .set("rates", jRateEntries(rateEntries(Currency.GBP, Currency.USD, 1.6))),
      () -> jCurrencyAmountArray(base.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.GBP, Currency.USD, 1.6)))),
      MUST_SUCCEED);
  // Composed, exactly as for CurrencyAmountArray.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.multipliedBy (composed)",
      opEntry("multipliedBy").set("left", jMultiCurrencyAmountArray(base)).set("scalar", jDbl(2.5))
          .set("composed", jBool(true)),
      () -> {
        Map<Currency, DoubleArray> scaled = new LinkedHashMap<>();
        for (Map.Entry<Currency, DoubleArray> entry : base.getValues().entrySet()) {
          scaled.put(entry.getKey(), entry.getValue().multipliedBy(2.5));
        }
        return jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(scaled));
      }, MUST_SUCCEED);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.mapAmounts (composed)",
      opEntry("mapAmounts").set("left", jMultiCurrencyAmountArray(base))
          .set("mapAmountsFn", jStr(MAP_AMOUNTS_FN)).set("composed", jBool(true)),
      () -> {
        Map<Currency, DoubleArray> mapped = new LinkedHashMap<>();
        for (Map.Entry<Currency, DoubleArray> entry : base.getValues().entrySet()) {
          mapped.put(entry.getKey(), entry.getValue().map(v -> mapAmountsFn(v)));
        }
        return jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(mapped));
      }, MUST_SUCCEED);
  return results;
}

/**
 * Cross-checks the Money / BigMoney rounding and arithmetic constants that
 * MoneyTest and BigMoneyTest hard-code, so the money rows are asserted rather
 * than merely captured. Every expectation below is one of those tests' own
 * assertions, restated against the same inputs.
 */
void checkMoneyConstants() {
  // MoneyTest.testOfCurrencyAndAmount, including its rounding block: two
  // minor-unit digits for AUD and three for BHD.
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(AUD, 100.1249) rounds to 100.12",
      Money.of(Currency.AUD, 100.12), Money.of(Currency.AUD, 100.1249));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(AUD, 100.125) rounds to 100.13",
      Money.of(Currency.AUD, 100.13), Money.of(Currency.AUD, 100.125));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(BHD, 100.1249) rounds to 100.125",
      Money.of(Currency.BHD, 100.125), Money.of(Currency.BHD, 100.1249));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(AUD, 200) amount",
      java.math.BigDecimal.valueOf(20000, 2), Money.of(Currency.AUD, 200).getAmount());
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(AUD, 100.12) amount",
      java.math.BigDecimal.valueOf(10012, 2), Money.of(Currency.AUD, 100.12).getAmount());
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(BHD, 100.12) amount",
      java.math.BigDecimal.valueOf(100120, 3), Money.of(Currency.BHD, 100.12).getAmount());
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(BHD, 100.125) amount",
      java.math.BigDecimal.valueOf(100125, 3), Money.of(Currency.BHD, 100.125).getAmount());
  // MoneyTest.test_toString
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.of(BHD, 100.125) toString", "BHD 100.125",
      Money.of(Currency.BHD, 100.125).toString());
  // MoneyTest.testPlus / testMinus / testMultipliedBy
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money GBP 1.23 plus 2.34", Money.of(Currency.GBP, 3.57),
      Money.of(Currency.GBP, 1.23).plus(Money.of(Currency.GBP, 2.34)));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money GBP 1.23 minus 0.34", Money.of(Currency.GBP, 0.89),
      Money.of(Currency.GBP, 1.23).minus(Money.of(Currency.GBP, 0.34)));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money GBP 0.34 minus 1.23", Money.of(Currency.GBP, -0.89),
      Money.of(Currency.GBP, 0.34).minus(Money.of(Currency.GBP, 1.23)));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money GBP 1.23 multipliedBy 2", Money.of(Currency.GBP, 2.46),
      Money.of(Currency.GBP, 1.23).multipliedBy(2));
  // MoneyTest.testMinus, the failing direction: its message differs from the
  // addition case, so both messages are captured.
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.minus different currency message",
      "IllegalArgumentException: Unable to subtract amounts in different currencies",
      moneyFailureMessage(() -> Money.of(Currency.RON, 200.23).minus(Money.of(Currency.AUD, 100))));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.minus different currency message",
      "IllegalArgumentException: Unable to subtract amounts in different currencies",
      moneyFailureMessage(
          () -> BigMoney.of(Currency.RON, 200.2345).minus(BigMoney.of(Currency.AUD, 100))));
  // MoneyTest.testConvertedToWithExplicitRate / testConvertedToWithRateProvider
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money RON 200.23 convertedTo RON rate 1",
      Money.of(Currency.RON, 200.23),
      Money.of(Currency.RON, 200.23).convertedTo(Currency.RON, Decimal.of(1)));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money AUD 100.12 convertedTo RON rate 2.6",
      Money.of(Currency.RON, 260.31),
      Money.of(Currency.AUD, 100.12).convertedTo(Currency.RON, Decimal.of(2.6d)));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money AUD 100.12 convertedTo RON provider 2.5",
      Money.of(Currency.RON, 250.30),
      Money.of(Currency.AUD, 100.12)
          .convertedTo(Currency.RON, FxRate.of(Currency.AUD, Currency.RON, 2.5)));
  CHECK.checkEquals(FX_CURRENCY_MATH, "Money.convertedTo same currency rate 1.1 message",
      "IllegalArgumentException: FX rate must be 1 when no conversion required",
      moneyFailureMessage(
          () -> Money.of(Currency.RON, 200.23).convertedTo(Currency.RON, Decimal.of(1.1))));
  // BigMoneyTest.test_toString: BigMoney keeps the full scale.
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.of(RON, 200.2345) toString", "RON 200.2345",
      BigMoney.of(Currency.RON, 200.2345).toString());
  // BigMoneyTest.testOfCurrencyAndAmount: BigMoney does NOT round to minor
  // units, so BHD 100.1249 keeps its fourth decimal where Money.of rounds it.
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.of(BHD, 100.1249) value",
      Decimal.of(100.1249), BigMoney.of(Currency.BHD, 100.1249).getValue());
  // BigMoneyTest.testParse: BigMoney.of / parse round to scale 12 HALF_UP, so
  // the fifteen-decimal amount and its twelve-decimal form are one value.
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney 15-decimal amount rounds to scale 12",
      BigMoney.parse("AUD 1.123456789012"), BigMoney.of(Currency.AUD, 1.123456789012345d));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.toMoney of 15-decimal amount",
      Money.of(Currency.AUD, 1.12), BigMoney.of(Currency.AUD, 1.123456789012345d).toMoney());
  // BigMoneyTest.test_roundToScale and test_roundToScaleNegative, row for row.
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(1.441, 2, CEILING)",
      BigMoney.of(Currency.GBP, 1.45),
      BigMoney.of(Currency.GBP, 1.441).roundToScale(2, RoundingMode.CEILING));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(1.441, 2, UP)",
      BigMoney.of(Currency.GBP, 1.45),
      BigMoney.of(Currency.GBP, 1.441).roundToScale(2, RoundingMode.UP));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(1.446, 2, HALF_UP)",
      BigMoney.of(Currency.GBP, 1.45),
      BigMoney.of(Currency.GBP, 1.446).roundToScale(2, RoundingMode.HALF_UP));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(1.449, 2, FLOOR)",
      BigMoney.of(Currency.GBP, 1.44),
      BigMoney.of(Currency.GBP, 1.449).roundToScale(2, RoundingMode.FLOOR));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(1.449, 2, DOWN)",
      BigMoney.of(Currency.GBP, 1.44),
      BigMoney.of(Currency.GBP, 1.449).roundToScale(2, RoundingMode.DOWN));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(1.444, 2, HALF_DOWN)",
      BigMoney.of(Currency.GBP, 1.44),
      BigMoney.of(Currency.GBP, 1.444).roundToScale(2, RoundingMode.HALF_DOWN));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(780001, -3, CEILING)",
      BigMoney.of(Currency.GBP, 781_000),
      BigMoney.of(Currency.GBP, 780_001).roundToScale(-3, RoundingMode.CEILING));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(780001, -2, UP)",
      BigMoney.of(Currency.GBP, 780_100),
      BigMoney.of(Currency.GBP, 780_001).roundToScale(-2, RoundingMode.UP));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(780005, -1, HALF_UP)",
      BigMoney.of(Currency.GBP, 780_010),
      BigMoney.of(Currency.GBP, 780_005).roundToScale(-1, RoundingMode.HALF_UP));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(780999, -3, FLOOR)",
      BigMoney.of(Currency.GBP, 780_000),
      BigMoney.of(Currency.GBP, 780_999).roundToScale(-3, RoundingMode.FLOOR));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(780699, -2, DOWN)",
      BigMoney.of(Currency.GBP, 780_600),
      BigMoney.of(Currency.GBP, 780_699).roundToScale(-2, RoundingMode.DOWN));
  CHECK.checkEquals(FX_CURRENCY_MATH, "BigMoney.roundToScale(780234, -1, HALF_DOWN)",
      BigMoney.of(Currency.GBP, 780_230),
      BigMoney.of(Currency.GBP, 780_234).roundToScale(-1, RoundingMode.HALF_DOWN));
  // CurrencyAmountArrayTest.test_of_function and
  // MultiCurrencyAmountArrayTest.test_of_function: the two array factories the
  // fixture captures, checked against the values those tests assert.
  List<CurrencyAmount> functionValues = Arrays.asList(
      CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.GBP, 2),
      CurrencyAmount.of(Currency.GBP, 3));
  CHECK.checkEquals(FX_CURRENCY_MATH, "CurrencyAmountArray.of(3, fn) values",
      DoubleArray.of(1d, 2d, 3d),
      CurrencyAmountArray.of(3, i -> functionValues.get(i)).getValues());
}

/**
 * Runs a Money / BigMoney call that must fail and returns the message the
 * fixture records for it, so the message itself can be checked against the
 * Java test that states it. A call that unexpectedly SUCCEEDS returns a
 * marker rather than throwing, which turns the surprise into a named check
 * failure instead of an aborted script.
 */
interface ThrowingCall {
  Object get() throws Throwable;
}

String moneyFailureMessage(ThrowingCall call) {
  try {
    call.get();
    return "no failure";
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    return errorMessage(thrown);
  }
}

/*
 * Adds one currency-math row.
 *
 * Row shape, following the `currency-math-baseline.json` row schema: the
 * seven declared input fields (`amounts`, `scalars`, `arrays`, `multiArrays`,
 * `money`, `bigMoney`, `rates`) followed by the expectation bucket for the
 * family this row covers. `MATH_IN` is reset before the builder runs and
 * emitted afterwards, so the input fields hold exactly the inputs that
 * builder consumed - see the note on MathInputs for why they are collected
 * rather than re-listed.
 *
 * The expectation buckets are grouped by type, which is how the row schema
 * groups them: CurrencyAmount plus/minus/multipliedBy/negated/positive/
 * negative/convertedTo; CurrencyAmountArray and MultiCurrencyAmountArray
 * element-wise plus/minus/multipliedBy/mapAmounts/total; MultiCurrencyAmount
 * of/total/plus/convertedTo; Money.of and BigMoney.of rounded amounts with
 * Money plus/multipliedBy/convertedTo and BigMoney.toMoney.
 */
/*
 * The six expectation buckets, in the order the row schema declares them.
 * EVERY row carries ALL SIX: the one its family covers, and an empty array
 * for the other five. Uniform rows are the point - the Scala decoder is then
 * one fixed-shape product with no optional fields, and a bucket that is
 * missing because a builder was never wired up cannot masquerade as a bucket
 * that is empty because the row does not exercise it.
 */
String[] CURRENCY_MATH_BUCKETS = {
    "currencyAmountResults",
    "moneyResults",
    "bigMoneyResults",
    "currencyAmountArrayResults",
    "multiCurrencyAmountResults",
    "multiCurrencyAmountArrayResults"};

void addCurrencyMathRow(JArray rows, String id, String source, String resultField,
    ResultSupplier builder) throws Throwable {
  MATH_IN.reset();
  Jn results = builder.get();
  JObject row = new JObject().set("id", jStr(id)).set("source", jStr(source));
  MATH_IN.emitInto(row);
  boolean matched = false;
  for (String bucket : CURRENCY_MATH_BUCKETS) {
    if (bucket.equals(resultField)) {
      row.set(bucket, results);
      matched = true;
    } else {
      row.set(bucket, new JArray());
    }
  }
  if (!matched) {
    throw new IllegalStateException("Not a declared currency-math bucket: " + resultField);
  }
  rows.add(row);
  CHECK.countRow(FX_CURRENCY_MATH);
}

Jn buildCurrencyMathFixture() throws Throwable {
  JArray rows = new JArray();
  checkMoneyConstants();
  checkCurrencyIniAgainstCurrencyOf();

  addCurrencyMathRow(rows, "currency-amount", "CurrencyAmountTest", "currencyAmountResults",
      () -> buildCurrencyAmountResults());
  addCurrencyMathRow(rows, "money", "MoneyTest", "moneyResults",
      () -> buildMoneyResults(false));
  addCurrencyMathRow(rows, "big-money", "BigMoneyTest", "bigMoneyResults",
      () -> buildMoneyResults(true));
  addCurrencyMathRow(rows, "currency-amount-array", "CurrencyAmountArrayTest",
      "currencyAmountArrayResults", () -> buildCurrencyAmountArrayResults());
  addCurrencyMathRow(rows, "multi-currency-amount", "MultiCurrencyAmountTest",
      "multiCurrencyAmountResults", () -> buildMultiCurrencyAmountResults());
  addCurrencyMathRow(rows, "multi-currency-amount-array", "MultiCurrencyAmountArrayTest",
      "multiCurrencyAmountArrayResults", () -> buildMultiCurrencyAmountArrayResults());

  return rows;
}

/* ===========================================================================
 * SECTION 11 - FIXTURE 5 OF 6: holiday-baseline.json
 *
 * One row per (calendar, year), written one row per line (Jn.writeCompact).
 *
 * WHAT `holidays` MEANS - this is the fixture's contract, so it is stated
 * precisely. It is EVERY date d in the year for which isHoliday(d) is true,
 * i.e. weekends INCLUDED, which is exactly
 * HolidayCalendar.holidays(startInclusive, endExclusive). Two reasons:
 * isHoliday treats a weekend as a holiday (both the javadoc and the runtime
 * agree), and a calendar's weekendDays are not reachable through the public
 * API - so the full isHoliday truth is the only contract both sides can
 * compute symmetrically. The Java tests assert the same thing in the other
 * direction, listing non-weekend holidays and OR-ing Saturday and Sunday.
 *
 * WHY THAT MATTERS, AND WHERE HUBU MAKES IT MATTER. HUBU is built as
 * ImmutableHolidayCalendar.of(id, holidays, SUNDAY, SUNDAY)
 * (GlobalHolidayCalendars.java:1204): Sunday is its ONLY weekend day and its
 * Saturdays are listed EXPLICITLY as holidays by addHungarianSaturdays
 * (:1239-1250). A fixture listing only non-weekend holidays would be ambiguous
 * between "not a holiday" and "a weekend day the list omitted", and the
 * ambiguity would land on exactly the calendar where it changes the answer. So
 * the full isHoliday truth is emitted, and the Java tables serve only as a
 * cross-check, in the direction those tests assert them.
 *
 * The out-of-range rows are deliberate: outside its stored year range an
 * ImmutableHolidayCalendar falls back to a weekend-only test rather than
 * throwing (ImmutableHolidayCalendar.java:397-415), and those rows record that
 * fallback. Beyond year 0000-9999 it throws instead, which the `yearRange` rows
 * carry as `error` expectations.
 * ===========================================================================
 */

String FX_HOLIDAY = "holiday";

/** The 24 calendars generated over 1950-2099. EUTA and THBA differ and are handled separately. */
String[] GENERATED_CALENDARS = {
    "GBLO", "FRPA", "DEFR", "CHZU", "USGS", "USNY", "NYFD", "NYSE", "JPTO", "AUSY", "BRBD",
    "CAMO", "CATO", "CZPR", "DKCO", "HUBU", "MXMC", "NOOS", "NZAU", "NZWE", "NZBD", "PLWA",
    "SEST", "ZAJO",
};

// The year ranges, each verified against the loop in the generator it names.
int HOLIDAY_GENERATED_FIRST_YEAR = 1950;      // GlobalHolidayCalendars.java:128 and 23 siblings
int HOLIDAY_GENERATED_LAST_YEAR = 2099;
int HOLIDAY_EUTA_FIRST_YEAR = 1997;           // generateEuropeanTarget, :296
int HOLIDAY_THBA_FIRST_YEAR = 2005;           // HolidayCalendarData.ini [THBA], :31
int HOLIDAY_THBA_LAST_YEAR = 2079;
/** The two years every weekend and composite calendar is sampled in; 2024 is a leap year. */
int[] HOLIDAY_SAMPLE_YEARS = {2020, 2024};

/**
 * THE SAMPLING RULE, which the harness cannot infer and the README therefore
 * states as well: three samples per row, at {month, day, shift amount, months
 * to endExclusive}.
 *
 *   Jan 1  shift -3  daysBetween to Jan 1 of the next year - the whole-year
 *                    business-day count, and a span crossing the year end;
 *                    the negative shift walks back into the previous year.
 *   Jun 15 shift +5  daysBetween over one month, wholly inside the year.
 *   Dec 24 shift +7  the positive shift and the one-month span both cross into
 *                    the next year.
 *
 * Three is a deliberate ceiling: `holidays` already pins isHoliday for every
 * day of the year, so a sample buys date ARITHMETIC coverage only, and each
 * one costs ~250 bytes across 3,846 rows.
 */
int[][] HOLIDAY_SAMPLE_SPEC = {
    {1, 1, -3, 12},
    {6, 15, 5, 1},
    {12, 24, 7, 1},
};

/**
 * The weekend days of every captured calendar, as declared by the constructor
 * call that builds it in GlobalHolidayCalendars - not derived from any date
 * table.
 *
 * They are not public API, so they are never emitted: they are the expectation
 * the out-of-range and weekend rows are checked against, which is what proves
 * the weekend-only fallback instead of assuming it.
 */
Set<DayOfWeek> weekendDaySet(DayOfWeek... days) {
  Set<DayOfWeek> set = new LinkedHashSet<>();
  for (DayOfWeek day : days) {
    set.add(day);
  }
  return set;
}

Map<String, Set<DayOfWeek>> buildHolidayWeekendDays() {
  Map<String, Set<DayOfWeek>> map = new TreeMap<>();
  // Every generator passes (SATURDAY, SUNDAY) ...
  for (String id : GENERATED_CALENDARS) {
    map.put(id, weekendDaySet(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY));
  }
  map.put("EUTA", weekendDaySet(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY));
  // ... except Budapest, whose only weekend day is Sunday (:1204).
  map.put("HUBU", weekendDaySet(DayOfWeek.SUNDAY));
  // THBA comes from the INI, whose [THBA] section declares `Weekend = Sat,Sun`.
  map.put("THBA", weekendDaySet(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY));
  map.put("NoHolidays", weekendDaySet());
  map.put("Sat/Sun", weekendDaySet(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY));
  map.put("Fri/Sat", weekendDaySet(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY));
  map.put("Thu/Fri", weekendDaySet(DayOfWeek.THURSDAY, DayOfWeek.FRIDAY));
  return map;
}

Map<String, Set<DayOfWeek>> HOLIDAY_WEEKEND_DAYS = buildHolidayWeekendDays();

/**
 * The expected-holiday tables of GlobalHolidayCalendarsTest, by calendar id.
 *
 * Row shape is the Java provider's: {year, expected holidays} and, for HUBU
 * alone, {year, expected holidays, working days}. A calendar absent from this
 * map has no Java table, so its rows carry no independent expectation - which
 * `checkHolidayTablesUsed` reports rather than hides.
 */
Map<String, Object[][]> buildHolidayExpectationTables() {
  Map<String, Object[][]> tables = new LinkedHashMap<>();
  tables.put("GBLO", data_gblo());
  tables.put("FRPA", data_frpa());
  tables.put("DEFR", data_defr());
  tables.put("CHZU", data_chzu());
  tables.put("EUTA", data_euta());
  tables.put("USGS", data_usgs());
  tables.put("USNY", data_usny());
  tables.put("NYFD", data_nyfd());
  tables.put("NYSE", data_nyse());
  tables.put("JPTO", data_jpto());
  tables.put("AUSY", data_ausy());
  tables.put("BRBD", data_brbd());
  tables.put("CAMO", data_camo());
  tables.put("CATO", data_cato());
  tables.put("CZPR", data_czpr());
  tables.put("DKCO", data_dkco());
  tables.put("HUBU", data_hubu());
  tables.put("MXMC", data_mxmc());
  tables.put("NOOS", data_noos());
  tables.put("NZAU", data_nzau());
  tables.put("NZWE", data_nzwe());
  tables.put("NZBD", data_nzbd());
  tables.put("PLWA", data_plwa());
  tables.put("SEST", data_sest());
  tables.put("ZAJO", data_zajo());
  return tables;
}

Map<String, Object[][]> HOLIDAY_EXPECTATION_TABLES = buildHolidayExpectationTables();

/** Row counts measured in GlobalHolidayCalendarsTest, 201 in total. */
Map<String, Integer> buildHolidayExpectationRowCounts() {
  Map<String, Integer> counts = new LinkedHashMap<>();
  counts.put("GBLO", Integer.valueOf(17));
  counts.put("FRPA", Integer.valueOf(11));
  counts.put("DEFR", Integer.valueOf(4));
  counts.put("CHZU", Integer.valueOf(5));
  counts.put("EUTA", Integer.valueOf(9));
  counts.put("USGS", Integer.valueOf(20));
  counts.put("USNY", Integer.valueOf(10));
  counts.put("NYFD", Integer.valueOf(16));
  counts.put("NYSE", Integer.valueOf(8));
  counts.put("JPTO", Integer.valueOf(17));
  counts.put("AUSY", Integer.valueOf(8));
  counts.put("BRBD", Integer.valueOf(5));
  counts.put("CAMO", Integer.valueOf(3));
  counts.put("CATO", Integer.valueOf(9));
  counts.put("CZPR", Integer.valueOf(10));
  counts.put("DKCO", Integer.valueOf(4));
  counts.put("HUBU", Integer.valueOf(6));
  counts.put("MXMC", Integer.valueOf(6));
  counts.put("NOOS", Integer.valueOf(8));
  counts.put("NZAU", Integer.valueOf(4));
  counts.put("NZWE", Integer.valueOf(4));
  counts.put("NZBD", Integer.valueOf(5));
  counts.put("PLWA", Integer.valueOf(6));
  counts.put("SEST", Integer.valueOf(3));
  counts.put("ZAJO", Integer.valueOf(3));
  return counts;
}

Map<String, Integer> HOLIDAY_EXPECTATION_ROW_COUNTS = buildHolidayExpectationRowCounts();

/** How many table rows were actually matched by a captured row. */
Map<String, Integer> HOLIDAY_TABLE_ROWS_USED = new TreeMap<>();

/**
 * THE INDEPENDENT CHECK on a captured (calendar, year) row.
 *
 * When GlobalHolidayCalendarsTest has a row for this year, the captured
 * holiday list must equal the set that test's own rule produces from its
 * hand-sourced dates. The dates come from outside the implementation -
 * legislation, exchange notices, central-bank calendars, each cited in the
 * Java test - so agreement here is a statement about the calendar rather than
 * about the capture.
 *
 * The test's rule, exactly as it states it: a date is a holiday when the table
 * names it or it
 * falls at the weekend, unless the row lists it as a working day. The weekend
 * is Saturday and Sunday for all 25 tables, including Budapest's - see the
 * table banner.
 */
void checkHolidayAgainstJavaTable(String rowId, String calendarName, int year,
    List<LocalDate> capturedHolidays) {
  Object[][] table = HOLIDAY_EXPECTATION_TABLES.get(calendarName);
  if (table == null) {
    return;
  }
  for (Object[] tableRow : table) {
    if (((Integer) tableRow[0]).intValue() != year) {
      continue;
    }
    // toDateList copies through a wildcard, so no unchecked cast is needed.
    List<LocalDate> expectedDates = toDateList(tableRow[1]);
    List<LocalDate> workingDays = tableRow.length > 2 ? toDateList(tableRow[2])
        : new ArrayList<LocalDate>();
    List<LocalDate> expected = new ArrayList<>();
    LocalDate date = LocalDate.of(year, 1, 1);
    int length = date.lengthOfYear();
    for (int i = 0; i < length; i++) {
      boolean weekend = date.getDayOfWeek() == DayOfWeek.SATURDAY
          || date.getDayOfWeek() == DayOfWeek.SUNDAY;
      if ((expectedDates.contains(date) || weekend) && !workingDays.contains(date)) {
        expected.add(date);
      }
      date = date.plusDays(1);
    }
    CHECK.checkEquals(FX_HOLIDAY,
        rowId + " GlobalHolidayCalendarsTest.data_" + calendarName.toLowerCase(Locale.ENGLISH),
        expected, capturedHolidays);
    Integer used = HOLIDAY_TABLE_ROWS_USED.get(calendarName);
    HOLIDAY_TABLE_ROWS_USED.put(calendarName,
        Integer.valueOf(used == null ? 1 : used.intValue() + 1));
  }
}

/**
 * Every table row must have been consumed by a captured row.
 *
 * This is the check that makes the one above load-bearing: without it, a year
 * dropped from the captured range - or a calendar renamed - would simply stop
 * being compared, and 201 expectations would quietly become none.
 */
void checkHolidayTablesUsed() {
  int total = 0;
  for (Map.Entry<String, Integer> entry : HOLIDAY_EXPECTATION_ROW_COUNTS.entrySet()) {
    String calendarName = entry.getKey();
    int expectedRows = entry.getValue().intValue();
    Object[][] table = HOLIDAY_EXPECTATION_TABLES.get(calendarName);
    CHECK.checkCount(FX_HOLIDAY, "data_" + calendarName.toLowerCase(Locale.ENGLISH) + " rows",
        expectedRows, table == null ? 0 : table.length);
    Integer used = HOLIDAY_TABLE_ROWS_USED.get(calendarName);
    CHECK.checkCount(FX_HOLIDAY, "data_" + calendarName.toLowerCase(Locale.ENGLISH)
        + " rows checked", expectedRows, used == null ? 0 : used.intValue());
    total += expectedRows;
  }
  CHECK.checkCount(FX_HOLIDAY, "GlobalHolidayCalendarsTest expected-year rows", 201, total);
}

/**
 * One emitted row, kept in Java form so the checks below read it as data
 * rather than by inspecting rendered JSON.
 */
class HolidayRow {
  String id;
  String source;
  String calendar;
  int year;
  /** Every date of the year for which isHoliday is true; null when rejected. */
  List<LocalDate> holidays;
  /** The rejection message, or null. */
  String error;
  /** One entry per sample: its rejection message, or null. */
  List<String> sampleErrors = new ArrayList<>();
}

/**
 * Every row by id. Row ids must be unique - the parity report names a failing
 * row by its id - and the whole-fixture checks look rows up here.
 */
Map<String, HolidayRow> HOLIDAY_ROWS = new LinkedHashMap<>();

/**
 * Kebab-cases a calendar name into an id segment. '+' and '~' must map to
 * DIFFERENT text, or a combined and a linked composite of the same two
 * calendars would collide.
 */
String holidayCalendarSlug(String calendarName) {
  StringBuilder sb = new StringBuilder();
  for (int i = 0; i < calendarName.length(); i++) {
    char c = calendarName.charAt(i);
    if (c == '+') {
      sb.append("-plus-");
    } else if (c == '~') {
      sb.append("-linked-");
    } else if (Character.isLetterOrDigit(c)) {
      sb.append(Character.toLowerCase(c));
    } else {
      sb.append('-');
    }
  }
  return sb.toString();
}

/** A '-' sign would read as an id separator, so a negative year spells it out. */
String holidayYearSlug(int year) {
  return year < 0 ? "minus" + Integer.toString(-year) : Integer.toString(year);
}

String holidayRowId(String calendarName, int year, String suffix) {
  return holidayCalendarSlug(calendarName) + "-" + holidayYearSlug(year)
      + (suffix.isEmpty() ? "" : "-" + suffix);
}

/** Every date of a year whose day-of-week is one of the given weekend days. */
List<LocalDate> weekendDatesOfYear(int year, Set<DayOfWeek> weekendDays) {
  List<LocalDate> dates = new ArrayList<>();
  LocalDate end = LocalDate.of(year + 1, 1, 1);
  for (LocalDate date = LocalDate.of(year, 1, 1); date.isBefore(end); date = date.plusDays(1)) {
    if (weekendDays.contains(date.getDayOfWeek())) {
      dates.add(date);
    }
  }
  return dates;
}

/** The emitted expectation: every date of the year for which isHoliday is true. */
List<LocalDate> holidayDatesOfYear(HolidayCalendar calendar, int year) {
  List<LocalDate> dates = new ArrayList<>();
  LocalDate end = LocalDate.of(year + 1, 1, 1);
  for (LocalDate date = LocalDate.of(year, 1, 1); date.isBefore(end); date = date.plusDays(1)) {
    if (calendar.isHoliday(date)) {
      dates.add(date);
    }
  }
  return dates;
}

/**
 * One sample. Every call is made first and published only if all of them
 * succeeded, so a sample either carries a complete set of expectations or
 * carries `error` with every value null - never a half-populated mixture.
 */
JObject jHolidaySample(HolidayRow row, HolidayCalendar calendar, LocalDate date, int shiftAmount,
    int endExclusiveMonths) {

  String rowId = row.id;
  LocalDate endExclusive = date.plusMonths(endExclusiveMonths);
  Boolean holiday = null;
  Boolean businessDay = null;
  LocalDate next = null;
  LocalDate previous = null;
  LocalDate nextOrSame = null;
  LocalDate previousOrSame = null;
  LocalDate shifted = null;
  Integer between = null;
  String error = null;
  try {
    boolean holiday0 = calendar.isHoliday(date);
    boolean businessDay0 = calendar.isBusinessDay(date);
    LocalDate next0 = calendar.next(date);
    LocalDate previous0 = calendar.previous(date);
    LocalDate nextOrSame0 = calendar.nextOrSame(date);
    LocalDate previousOrSame0 = calendar.previousOrSame(date);
    LocalDate shifted0 = calendar.shift(date, shiftAmount);
    int between0 = calendar.daysBetween(date, endExclusive);
    holiday = holiday0;
    businessDay = businessDay0;
    next = next0;
    previous = previous0;
    nextOrSame = nextOrSame0;
    previousOrSame = previousOrSame0;
    shifted = shifted0;
    between = between0;
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    error = errorMessage(thrown);
  }
  row.sampleErrors.add(error);
  if (error == null) {
    // isBusinessDay is the exact complement of isHoliday (HolidayCalendar.java:61).
    CHECK.checkEquals(FX_HOLIDAY, rowId + " sample " + date + " isBusinessDay",
        Boolean.valueOf(!holiday.booleanValue()), businessDay);
    // nextOrSame / previousOrSame agree with next / previous exactly when the
    // date itself is a holiday.
    CHECK.checkEquals(FX_HOLIDAY, rowId + " sample " + date + " nextOrSame",
        holiday.booleanValue() ? next : date, nextOrSame);
    CHECK.checkEquals(FX_HOLIDAY, rowId + " sample " + date + " previousOrSame",
        holiday.booleanValue() ? previous : date, previousOrSame);
  }
  JObject shift = new JObject()
      .set("amount", jInt(shiftAmount))
      .set("result", jDate(shifted));
  JObject daysBetween = new JObject()
      .set("endExclusive", jDate(endExclusive))
      .set("result", between == null ? jNull() : jInt(between.intValue()));
  return new JObject()
      .set("date", jDate(date))
      .set("isHoliday", holiday == null ? jNull() : jBool(holiday.booleanValue()))
      .set("isBusinessDay", businessDay == null ? jNull() : jBool(businessDay.booleanValue()))
      .set("next", jDate(next))
      .set("previous", jDate(previous))
      .set("nextOrSame", jDate(nextOrSame))
      .set("previousOrSame", jDate(previousOrSame))
      .set("shift", shift)
      .set("daysBetween", daysBetween)
      .set("error", jStr(error));
}

/**
 * Adds one (calendar, year) row. The key set is identical in every row -
 * `holidays` is null and `error` is set only on a row whose whole year is
 * outside the accepted 0000-9999 range.
 */
HolidayRow addHolidayRow(JArray rows, String source, String suffix, String calendarName,
    HolidayCalendar calendar, int year) {

  String rowId = holidayRowId(calendarName, year, suffix);
  HolidayRow row = new HolidayRow();
  row.id = rowId;
  row.source = source;
  row.calendar = calendarName;
  row.year = year;
  CHECK.checkTrue(FX_HOLIDAY, rowId, HOLIDAY_ROWS.put(rowId, row) == null, "duplicate row id");
  List<LocalDate> holidayDates = null;
  String error = null;
  try {
    holidayDates = holidayDatesOfYear(calendar, year);
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    error = errorMessage(thrown);
    // Only ONE kind of row may fail: the `yearRange` probes, whose whole year
    // lies outside the accepted 0000-9999 range and which checkYearRangeRow
    // then asserts message-first. Every other row - a generated calendar, a
    // year outside a calendar's stored range, a weekend-only calendar - has a
    // holiday list by construction, so a rejection there is a change in
    // behaviour rather than a fixture value, and it aborts the capture instead
    // of being recorded as the expectation.
    CHECK.checkTrue(FX_HOLIDAY, rowId + " may fail", source.equals("yearRange"),
        "only a year-range probe may be rejected, but this row was: " + error);
    CHECK.checkTrue(FX_HOLIDAY, rowId + " rejection type",
        IllegalArgumentException.class.isInstance(thrown),
        "expected IllegalArgumentException but Java threw " + error);
  }
  row.holidays = holidayDates;
  row.error = error;
  if (holidayDates != null) {
    // Cross-check 0, and the only one that comes from outside the
    // implementation: the hand-sourced expected-holiday tables of
    // GlobalHolidayCalendarsTest, where that test has a row for this year.
    checkHolidayAgainstJavaTable(rowId, calendarName, year, holidayDates);
    // Cross-check 1: the same truth through a different public API. `holidays`
    // is a Stream over the range, so agreement is not a tautology of the loop
    // above - the two public APIs are checked against each other.
    List<LocalDate> streamed = new ArrayList<>();
    calendar.holidays(LocalDate.of(year, 1, 1), LocalDate.of(year + 1, 1, 1)).forEach(streamed::add);
    CHECK.checkEquals(FX_HOLIDAY, rowId + " holidays(stream)", holidayDates, streamed);
    // Cross-check 2: strictly ascending, so the emitted array is sorted and
    // duplicate-free by construction rather than by convention.
    boolean ascending = true;
    for (int i = 1; i < holidayDates.size(); i++) {
      ascending = ascending && holidayDates.get(i - 1).isBefore(holidayDates.get(i));
    }
    CHECK.checkTrue(FX_HOLIDAY, rowId + " holidays ascending", ascending,
        "holidays are not strictly ascending");
    // Cross-check 3: a year outside the calendar's stored range, and every
    // weekend-only calendar, must answer with exactly its weekend dates.
    Set<DayOfWeek> weekendDays = HOLIDAY_WEEKEND_DAYS.get(calendarName);
    if (weekendDays != null && (source.equals("outOfRange") || source.equals("weekend"))) {
      CHECK.checkEquals(FX_HOLIDAY, rowId + " weekend-only fallback",
          weekendDatesOfYear(year, weekendDays), holidayDates);
    }
  } else {
    CHECK.countErrorRow(FX_HOLIDAY);
  }
  JArray samples = new JArray();
  for (int[] spec : HOLIDAY_SAMPLE_SPEC) {
    samples.add(jHolidaySample(row, calendar, LocalDate.of(year, spec[0], spec[1]), spec[2],
        spec[3]));
  }
  rows.add(new JObject()
      .set("id", jStr(rowId))
      .set("source", jStr(source))
      .set("calendar", jStr(calendarName))
      .set("year", jInt(year))
      .set("holidays", holidayDates == null ? jNull() : jDates(holidayDates))
      .set("samples", samples)
      .set("error", jStr(error)));
  CHECK.countRow(FX_HOLIDAY);
  return row;
}

/**
 * Resolves a calendar id against the standard reference data.
 *
 * This is the public route. The Java tests reach the generators directly
 * through GlobalHolidayCalendars.generateLondon() and friends, but that class
 * is package-private and a .jsh script runs in the unnamed package, so it is
 * unreachable here without reflection (Trap 1). Resolution through
 * ReferenceData.standard() yields the very same calendar instances.
 */
HolidayCalendar resolveCalendar(String id) {
  return HolidayCalendarId.of(id).resolve(ReferenceData.standard());
}

/**
 * The data_easter() rows of GlobalHolidayCalendarsTest as {day, month, year}
 * triples: 201 rows covering 1900-2099.
 *
 * That provider's duplicated 1900 row is kept, so the row count here equals the
 * count in the source and neither side has to explain a difference of one.
 */
int[][] EASTER_EXPECTED = {
    {15, 4, 1900}, {15, 4, 1900}, {7, 4, 1901}, {30, 3, 1902}, {12, 4, 1903}, {3, 4, 1904},
    {23, 4, 1905}, {15, 4, 1906}, {31, 3, 1907}, {19, 4, 1908}, {11, 4, 1909}, {27, 3, 1910},
    {16, 4, 1911}, {7, 4, 1912}, {23, 3, 1913}, {12, 4, 1914}, {4, 4, 1915}, {23, 4, 1916},
    {8, 4, 1917}, {31, 3, 1918}, {20, 4, 1919}, {4, 4, 1920}, {27, 3, 1921}, {16, 4, 1922},
    {1, 4, 1923}, {20, 4, 1924}, {12, 4, 1925}, {4, 4, 1926}, {17, 4, 1927}, {8, 4, 1928},
    {31, 3, 1929}, {20, 4, 1930}, {5, 4, 1931}, {27, 3, 1932}, {16, 4, 1933}, {1, 4, 1934},
    {21, 4, 1935}, {12, 4, 1936}, {28, 3, 1937}, {17, 4, 1938}, {9, 4, 1939}, {24, 3, 1940},
    {13, 4, 1941}, {5, 4, 1942}, {25, 4, 1943}, {9, 4, 1944}, {1, 4, 1945}, {21, 4, 1946},
    {6, 4, 1947}, {28, 3, 1948}, {17, 4, 1949}, {9, 4, 1950}, {25, 3, 1951}, {13, 4, 1952},
    {5, 4, 1953}, {18, 4, 1954}, {10, 4, 1955}, {1, 4, 1956}, {21, 4, 1957}, {6, 4, 1958},
    {29, 3, 1959}, {17, 4, 1960}, {2, 4, 1961}, {22, 4, 1962}, {14, 4, 1963}, {29, 3, 1964},
    {18, 4, 1965}, {10, 4, 1966}, {26, 3, 1967}, {14, 4, 1968}, {6, 4, 1969}, {29, 3, 1970},
    {11, 4, 1971}, {2, 4, 1972}, {22, 4, 1973}, {14, 4, 1974}, {30, 3, 1975}, {18, 4, 1976},
    {10, 4, 1977}, {26, 3, 1978}, {15, 4, 1979}, {6, 4, 1980}, {19, 4, 1981}, {11, 4, 1982},
    {3, 4, 1983}, {22, 4, 1984}, {7, 4, 1985}, {30, 3, 1986}, {19, 4, 1987}, {3, 4, 1988},
    {26, 3, 1989}, {15, 4, 1990}, {31, 3, 1991}, {19, 4, 1992}, {11, 4, 1993}, {3, 4, 1994},
    {16, 4, 1995}, {7, 4, 1996}, {30, 3, 1997}, {12, 4, 1998}, {4, 4, 1999}, {23, 4, 2000},
    {15, 4, 2001}, {31, 3, 2002}, {20, 4, 2003}, {11, 4, 2004}, {27, 3, 2005}, {16, 4, 2006},
    {8, 4, 2007}, {23, 3, 2008}, {12, 4, 2009}, {4, 4, 2010}, {24, 4, 2011}, {8, 4, 2012},
    {31, 3, 2013}, {20, 4, 2014}, {5, 4, 2015}, {27, 3, 2016}, {16, 4, 2017}, {1, 4, 2018},
    {21, 4, 2019}, {12, 4, 2020}, {4, 4, 2021}, {17, 4, 2022}, {9, 4, 2023}, {31, 3, 2024},
    {20, 4, 2025}, {5, 4, 2026}, {28, 3, 2027}, {16, 4, 2028}, {1, 4, 2029}, {21, 4, 2030},
    {13, 4, 2031}, {28, 3, 2032}, {17, 4, 2033}, {9, 4, 2034}, {25, 3, 2035}, {13, 4, 2036},
    {5, 4, 2037}, {25, 4, 2038}, {10, 4, 2039}, {1, 4, 2040}, {21, 4, 2041}, {6, 4, 2042},
    {29, 3, 2043}, {17, 4, 2044}, {9, 4, 2045}, {25, 3, 2046}, {14, 4, 2047}, {5, 4, 2048},
    {18, 4, 2049}, {10, 4, 2050}, {2, 4, 2051}, {21, 4, 2052}, {6, 4, 2053}, {29, 3, 2054},
    {18, 4, 2055}, {2, 4, 2056}, {22, 4, 2057}, {14, 4, 2058}, {30, 3, 2059}, {18, 4, 2060},
    {10, 4, 2061}, {26, 3, 2062}, {15, 4, 2063}, {6, 4, 2064}, {29, 3, 2065}, {11, 4, 2066},
    {3, 4, 2067}, {22, 4, 2068}, {14, 4, 2069}, {30, 3, 2070}, {19, 4, 2071}, {10, 4, 2072},
    {26, 3, 2073}, {15, 4, 2074}, {7, 4, 2075}, {19, 4, 2076}, {11, 4, 2077}, {3, 4, 2078},
    {23, 4, 2079}, {7, 4, 2080}, {30, 3, 2081}, {19, 4, 2082}, {4, 4, 2083}, {26, 3, 2084},
    {15, 4, 2085}, {31, 3, 2086}, {20, 4, 2087}, {11, 4, 2088}, {3, 4, 2089}, {16, 4, 2090},
    {8, 4, 2091}, {30, 3, 2092}, {12, 4, 2093}, {4, 4, 2094}, {24, 4, 2095}, {15, 4, 2096},
    {31, 3, 2097}, {20, 4, 2098}, {12, 4, 2099},
};

/**
 * test_christmas() of GlobalHolidayCalendarsTest, as {year, christmas day,
 * boxing day} - the four shapes of the bump: Christmas on a Friday, Saturday,
 * Sunday and Monday.
 */
int[][] CHRISTMAS_EXPECTED = {
    {2020, 25, 28},
    {2021, 27, 28},
    {2022, 27, 26},
    {2023, 25, 26},
};

/**
 * Runs the date-rule cross-checks against the Java test constants.
 *
 * `easter`, `christmasBumpedSatSun` and `boxingDayBumpedSatSun` are
 * package-private statics of the package-private GlobalHolidayCalendars, so a
 * script in the unnamed package reaches them only by reflection. That is
 * legitimate here for the reason Section 8 of the README gives: the
 * no-reflection requirement applies to the Scala codec path, not to a
 * developer tool under tools/. Reflecting is strictly better than the
 * alternative, which is leaving the rules that place every Easter-derived and
 * Christmas-derived holiday in 3,846 rows unchecked.
 */
void checkHolidayDateRules() {
  Class<?> generators;
  try {
    generators = Class.forName("com.opengamma.strata.basics.date.GlobalHolidayCalendars");
  } catch (Throwable thrown) {
    rethrowIfError(thrown);
    CHECK.fail(FX_HOLIDAY, "GlobalHolidayCalendars", "not loadable: " + errorMessage(thrown));
    return;
  }
  Method easter;
  Method christmas;
  Method boxingDay;
  try {
    easter = generators.getDeclaredMethod("easter", int.class);
    christmas = generators.getDeclaredMethod("christmasBumpedSatSun", int.class);
    boxingDay = generators.getDeclaredMethod("boxingDayBumpedSatSun", int.class);
    easter.setAccessible(true);
    christmas.setAccessible(true);
    boxingDay.setAccessible(true);
  } catch (Throwable thrown) {
    rethrowIfError(thrown);
    CHECK.fail(FX_HOLIDAY, "GlobalHolidayCalendars", "date rules unreachable: "
        + errorMessage(thrown));
    return;
  }
  try {
    for (int[] row : EASTER_EXPECTED) {
      LocalDate expected = LocalDate.of(row[2], row[1], row[0]);
      CHECK.checkEquals(FX_HOLIDAY, "GlobalHolidayCalendarsTest.data_easter " + row[2], expected,
          easter.invoke(null, row[2]));
    }
    for (int[] row : CHRISTMAS_EXPECTED) {
      CHECK.checkEquals(FX_HOLIDAY, "GlobalHolidayCalendarsTest.test_christmas christmas " + row[0],
          LocalDate.of(row[0], 12, row[1]), christmas.invoke(null, row[0]));
      CHECK.checkEquals(FX_HOLIDAY, "GlobalHolidayCalendarsTest.test_christmas boxingDay " + row[0],
          LocalDate.of(row[0], 12, row[2]), boxingDay.invoke(null, row[0]));
    }
  } catch (Throwable thrown) {
    rethrowIfError(thrown);
    CHECK.fail(FX_HOLIDAY, "GlobalHolidayCalendars", "date rule invocation threw "
        + errorMessage(thrown));
  }
}

/**
 * Asserts a composite calendar's combination law over a whole year, once per
 * date: '+' (combinedWith) is a holiday in EITHER calendar, '~' (linkedWith)
 * is a holiday in BOTH (HolidayCalendarId.java:280-316).
 */
void checkCompositeLaw(String rowId, HolidayCalendar composite, HolidayCalendar first,
    HolidayCalendar second, boolean combined, int year) {

  LocalDate end = LocalDate.of(year + 1, 1, 1);
  boolean agrees = true;
  LocalDate firstMismatch = null;
  for (LocalDate date = LocalDate.of(year, 1, 1); date.isBefore(end); date = date.plusDays(1)) {
    boolean expected = combined
        ? first.isHoliday(date) || second.isHoliday(date)
        : first.isHoliday(date) && second.isHoliday(date);
    if (composite.isHoliday(date) != expected) {
      agrees = false;
      firstMismatch = firstMismatch == null ? date : firstMismatch;
    }
  }
  CHECK.checkTrue(FX_HOLIDAY, rowId + (combined ? " combined law" : " linked law"), agrees,
      "composite disagrees with its components, first at " + firstMismatch);
}

/**
 * Reproduces GlobalHolidayCalendarsTest.test_combinedWith (:1189-1197): over
 * every date from 1950-01-01 up to 2040, the combination of JPTO and USNY is a
 * holiday exactly when either component is.
 */
void checkJptoUsnyCombinedSpan(HolidayCalendar composite, HolidayCalendar jpto,
    HolidayCalendar usny) {

  boolean agrees = true;
  LocalDate firstMismatch = null;
  LocalDate date = LocalDate.of(1950, 1, 1);
  while (date.getYear() < 2040) {
    boolean expected = jpto.isHoliday(date) || usny.isHoliday(date);
    if (composite.isHoliday(date) != expected) {
      agrees = false;
      firstMismatch = firstMismatch == null ? date : firstMismatch;
    }
    date = date.plusDays(1);
  }
  CHECK.checkTrue(FX_HOLIDAY, "GlobalHolidayCalendarsTest.test_combinedWith", agrees,
      "JPTO+USNY disagrees with JPTO || USNY, first at " + firstMismatch);
}

/**
 * Asserts that a year-range row was rejected, on the row and on every one of
 * its samples, with the message Java actually produces.
 */
void checkYearRangeRow(HolidayRow row, String expectedPrefix) {
  CHECK.checkTrue(FX_HOLIDAY, row.id + " holidays rejected", row.holidays == null,
      "a year outside 0000-9999 cannot enumerate holidays");
  CHECK.checkTrue(FX_HOLIDAY, row.id + " error message",
      row.error != null && row.error.startsWith(expectedPrefix),
      "expected the year-range rejection message, got: " + row.error);
  CHECK.checkInt(FX_HOLIDAY, row.id + " sample count", HOLIDAY_SAMPLE_SPEC.length,
      row.sampleErrors.size());
  boolean allRejected = true;
  for (String sampleError : row.sampleErrors) {
    allRejected = allRejected && sampleError != null && sampleError.startsWith(expectedPrefix);
  }
  CHECK.checkTrue(FX_HOLIDAY, row.id + " samples rejected", allRejected,
      "every date arithmetic call on an out-of-accepted-range year must be rejected, got "
          + row.sampleErrors);
}

/**
 * THE DISCRIMINATION CHECKS - the ones that prove `holidays` carries the full
 * isHoliday truth rather than a copy of a Java test table.
 *
 * HUBU is the case that distinguishes them. In range its Saturdays are
 * EXPLICIT holidays (addHungarianSaturdays, GlobalHolidayCalendars.java:1239)
 * even though Saturday is not one of its weekend days; out of range the
 * weekend-only fallback answers with Sundays alone, so no Saturday survives.
 * A fixture built from the Java test lists - which OR in Saturday and Sunday
 * for every calendar - could not show that difference.
 *
 * GBLO is the complementary case: a conventional Sat/Sun calendar must list
 * every weekend date of the year, which a weekend-filtered table would omit.
 *
 * Only rows that are deliberately rejected may carry an error, anywhere.
 */
void checkHolidayDiscrimination(JArray rows) {
  for (int year : HOLIDAY_SAMPLE_YEARS) {
    HolidayRow hubuInRange = HOLIDAY_ROWS.get(holidayRowId("HUBU", year, ""));
    CHECK.checkTrue(FX_HOLIDAY, "hubu-" + year + " present", hubuInRange != null,
        "the HUBU row for " + year + " is missing");
    if (hubuInRange != null && hubuInRange.holidays != null) {
      int saturdays = 0;
      for (LocalDate date : hubuInRange.holidays) {
        saturdays += date.getDayOfWeek() == DayOfWeek.SATURDAY ? 1 : 0;
      }
      CHECK.checkTrue(FX_HOLIDAY, "hubu-" + year + " explicit Saturdays", saturdays > 0,
          "HUBU lists its Saturdays explicitly, so an in-range year must contain some");
      CHECK.checkTrue(FX_HOLIDAY, "hubu-" + year + " Saturday is not a weekend day",
          !HOLIDAY_WEEKEND_DAYS.get("HUBU").contains(DayOfWeek.SATURDAY),
          "HUBU's only weekend day is Sunday, so those Saturdays are holidays, not weekends");
    }
    HolidayRow gblo = HOLIDAY_ROWS.get(holidayRowId("GBLO", year, ""));
    if (gblo != null && gblo.holidays != null) {
      CHECK.checkTrue(FX_HOLIDAY, "gblo-" + year + " weekends included",
          gblo.holidays.containsAll(weekendDatesOfYear(year, HOLIDAY_WEEKEND_DAYS.get("GBLO"))),
          "every Saturday and Sunday of the year must appear in a Sat/Sun calendar's holidays");
    }
  }
  HolidayRow hubuOutOfRange =
      HOLIDAY_ROWS.get(holidayRowId("HUBU", HOLIDAY_GENERATED_FIRST_YEAR - 1, "out-of-range"));
  CHECK.checkTrue(FX_HOLIDAY, "hubu out-of-range present", hubuOutOfRange != null,
      "the HUBU out-of-range row is missing");
  if (hubuOutOfRange != null && hubuOutOfRange.holidays != null) {
    int saturdays = 0;
    for (LocalDate date : hubuOutOfRange.holidays) {
      saturdays += date.getDayOfWeek() == DayOfWeek.SATURDAY ? 1 : 0;
    }
    CHECK.checkInt(FX_HOLIDAY, hubuOutOfRange.id + " no explicit Saturdays", 0, saturdays);
  }
  for (HolidayRow row : HOLIDAY_ROWS.values()) {
    boolean rejected = row.source.equals("yearRange");
    boolean clean = row.error == null;
    for (String sampleError : row.sampleErrors) {
      clean = clean && sampleError == null;
    }
    CHECK.checkTrue(FX_HOLIDAY, row.id + " unexpected error", rejected || clean,
        "only a yearRange row may carry an error, found row=" + row.error + " samples="
            + row.sampleErrors);
  }
}

JArray buildHolidayFixture() {
  JArray rows = new JArray();
  // The 24 generators run 1950-2099.
  for (String id : GENERATED_CALENDARS) {
    HolidayCalendar calendar = resolveCalendar(id);
    CHECK.checkEquals(FX_HOLIDAY, "calendar id " + id, id, calendar.getName());
    for (int year = HOLIDAY_GENERATED_FIRST_YEAR; year <= HOLIDAY_GENERATED_LAST_YEAR; year++) {
      addHolidayRow(rows, "generated", "", id, calendar, year);
    }
    // Out-of-range probes: Java falls back to weekend-only rather than throwing.
    addHolidayRow(rows, "outOfRange", "out-of-range", id, calendar,
        HOLIDAY_GENERATED_FIRST_YEAR - 1);
    addHolidayRow(rows, "outOfRange", "out-of-range", id, calendar,
        HOLIDAY_GENERATED_LAST_YEAR + 1);
  }
  // EUTA is generated over 1997-2099, so 1996 is its out-of-range probe.
  HolidayCalendar euta = resolveCalendar("EUTA");
  CHECK.checkEquals(FX_HOLIDAY, "calendar id EUTA", "EUTA", euta.getName());
  for (int year = HOLIDAY_EUTA_FIRST_YEAR; year <= HOLIDAY_GENERATED_LAST_YEAR; year++) {
    addHolidayRow(rows, "generated", "", "EUTA", euta, year);
  }
  addHolidayRow(rows, "outOfRange", "out-of-range", "EUTA", euta, HOLIDAY_EUTA_FIRST_YEAR - 1);
  addHolidayRow(rows, "outOfRange", "out-of-range", "EUTA", euta,
      HOLIDAY_GENERATED_LAST_YEAR + 1);
  // THBA is an explicit date table covering 2005-2079.
  HolidayCalendar thba = resolveCalendar("THBA");
  CHECK.checkEquals(FX_HOLIDAY, "calendar id THBA", "THBA", thba.getName());
  for (int year = HOLIDAY_THBA_FIRST_YEAR; year <= HOLIDAY_THBA_LAST_YEAR; year++) {
    addHolidayRow(rows, "dataTable", "", "THBA", thba, year);
  }
  addHolidayRow(rows, "outOfRange", "out-of-range", "THBA", thba, HOLIDAY_THBA_FIRST_YEAR - 1);
  addHolidayRow(rows, "outOfRange", "out-of-range", "THBA", thba, HOLIDAY_THBA_LAST_YEAR + 1);
  // The four weekend / no-holiday calendars.
  Object[][] weekendCalendars = {
      {"NoHolidays", HolidayCalendars.NO_HOLIDAYS},
      {"Sat/Sun", HolidayCalendars.SAT_SUN},
      {"Fri/Sat", HolidayCalendars.FRI_SAT},
      {"Thu/Fri", HolidayCalendars.THU_FRI},
  };
  for (Object[] entry : weekendCalendars) {
    String name = (String) entry[0];
    HolidayCalendar calendar = (HolidayCalendar) entry[1];
    // The identity string is the calendar's own id, asserted here so a rename
    // upstream cannot slip through.
    CHECK.checkEquals(FX_HOLIDAY, "weekend calendar id " + name, name,
        calendar.getId().getName());
    for (int year : HOLIDAY_SAMPLE_YEARS) {
      addHolidayRow(rows, "weekend", "", name, calendar, year);
    }
  }
  // Composite ids resolve component-wise and are part of the contract. Both
  // forms are captured: '+' combines (holiday in either) and '~' links
  // (holiday in both).
  HolidayCalendar gblo = resolveCalendar("GBLO");
  HolidayCalendar usny = resolveCalendar("USNY");
  HolidayCalendar jpto = resolveCalendar("JPTO");
  HolidayCalendar combined = resolveCalendar("GBLO+USNY");
  CHECK.checkEquals(FX_HOLIDAY, "composite calendar name", "GBLO+USNY", combined.getName());
  HolidayCalendar linked = resolveCalendar("GBLO~USNY");
  CHECK.checkEquals(FX_HOLIDAY, "linked calendar name", "GBLO~USNY", linked.getName());
  HolidayCalendar jptoUsny = resolveCalendar("JPTO+USNY");
  CHECK.checkEquals(FX_HOLIDAY, "composite calendar name JPTO+USNY", "JPTO+USNY",
      jptoUsny.getName());
  for (int year : HOLIDAY_SAMPLE_YEARS) {
    HolidayRow combinedRow = addHolidayRow(rows, "composite", "", "GBLO+USNY", combined, year);
    checkCompositeLaw(combinedRow.id, combined, gblo, usny, true, year);
    HolidayRow linkedRow = addHolidayRow(rows, "composite", "", "GBLO~USNY", linked, year);
    checkCompositeLaw(linkedRow.id, linked, gblo, usny, false, year);
    HolidayRow jptoRow = addHolidayRow(rows, "composite", "", "JPTO+USNY", jptoUsny, year);
    checkCompositeLaw(jptoRow.id, jptoUsny, jpto, usny, true, year);
  }
  checkJptoUsnyCombinedSpan(jptoUsny, jpto, usny);
  // Beyond year 0000-9999 the lookup neither answers nor falls back: it
  // throws, on both sides of the range.
  String yearRangePrefix = "IllegalArgumentException: Date is outside the accepted range "
      + "(year 0000 to 10,000): ";
  checkYearRangeRow(addHolidayRow(rows, "yearRange", "year-range", "GBLO", gblo, 10000),
      yearRangePrefix);
  checkYearRangeRow(addHolidayRow(rows, "yearRange", "year-range", "GBLO", gblo, -1),
      yearRangePrefix);

  checkHolidayDateRules();
  checkHolidayDiscrimination(rows);
  checkHolidayTablesUsed();

  // The row count is asserted rather than reported, so thinning the year
  // coverage cannot pass unnoticed: 24 generators x 150 years + EUTA's 103 +
  // THBA's 75 + 52 out-of-range probes + 4 weekend calendars x 2 years + 3
  // composites x 2 years + 2 year-range rejections.
  int generatedYears = HOLIDAY_GENERATED_LAST_YEAR - HOLIDAY_GENERATED_FIRST_YEAR + 1;
  int expectedRows = GENERATED_CALENDARS.length * generatedYears
      + (HOLIDAY_GENERATED_LAST_YEAR - HOLIDAY_EUTA_FIRST_YEAR + 1)
      + (HOLIDAY_THBA_LAST_YEAR - HOLIDAY_THBA_FIRST_YEAR + 1)
      + (GENERATED_CALENDARS.length + 2) * 2
      + weekendCalendars.length * HOLIDAY_SAMPLE_YEARS.length
      + 3 * HOLIDAY_SAMPLE_YEARS.length
      + 2;
  CHECK.checkCount(FX_HOLIDAY, "holiday rows", expectedRows, rows.size());
  CHECK.checkCount(FX_HOLIDAY, "holiday row ids", expectedRows, HOLIDAY_ROWS.size());
  return rows;
}

/* ===========================================================================
 * SECTION 12 - FIXTURE 6 OF 6: double-array-baseline.json
 *
 * THE ROW SHAPE IS FIXED AND UNIFORM: every row carries every input, every
 * index parameter and all TWENTY-TWO expectations, with no optional field, no
 * null and no `error` entry. This fixture is the whole of the 1e-9 numerical
 * comparison for DoubleArray and DoubleMatrix, so a row that omitted an
 * expectation would silently reduce what that comparison measures - the one
 * failure a golden baseline exists to prevent.
 *
 * BOTH OVERLOAD FAMILIES ARE CAPTURED. DoubleArray has a scalar plus, minus,
 * multipliedBy and dividedBy AND an element-wise overload of each
 * (DoubleArray.java:660,682,704,726 and :802,829,858,886). Capturing one of
 * the two would leave four public methods unmeasured and one of the row's two
 * declared operands unused. Each expectation is therefore
 * named after the overload that produced it - plusScalar and plusArray,
 * minusScalar and minusArray, and so on - so a reader can tell which operand
 * produced which value without consulting this script.
 *
 * subArray IS THE TWO-BOUND, HALF-OPEN OVERLOAD (DoubleArray.java:561). The
 * row carries `subArrayFrom` and `subArrayTo` because neither can be derived
 * from the inputs; the single-bound overload is the special case
 * subArray(from, a.length) and is covered by DoubleArraySpec as a unit test.
 *
 * DoubleMatrix HAS NO MATRIX PRODUCT. Its only multipliedBy takes a double
 * (DoubleMatrix.java:544), so `matrixMultipliedBy` is the SCALAR multiply and
 * no matrix-times-matrix product is invented here. `matrixB` is consumed only
 * by matrixPlus and matrixMinus (:624,:653), the two members that take a
 * second matrix.
 *
 * EVERY ROW IS A SUCCESS ROW. The schema has no error column, so the emitter
 * asserts each operation's preconditions before computing anything - equal and
 * non-empty array lengths, identically shaped non-empty matrices,
 * 0 <= subArrayFrom <= subArrayTo <= a.length, in-range `with` indices - and
 * THROWS rather than recording a failure as an expectation. The exception
 * paths (empty-array min/max, mismatched element-wise lengths, out-of-range
 * indices) belong to DoubleArraySpec and DoubleMatrixSpec as unit tests.
 *
 * THE JAVA-DERIVED ROWS ARE CROSS-CHECKED AGAINST THE JAVA TEST CONSTANTS.
 * Each `javatest-` row is built from inputs DoubleArrayTest or DoubleMatrixTest
 * uses, and the values the row EMITS - the very objects that are serialized,
 * not a recomputation of them - are compared against the literals those tests
 * assert for those inputs: arrays at the test's own DELTA of 1e-14, scalars
 * exactly. A wrong overload, a mis-mapped operation or a changed
 * implementation therefore aborts the capture before anything is written. The
 * seeded-random and IEEE-edge rows have no Java literal to be compared
 * against and are counted as capture-only, which is what keeps the summary
 * honest about how much of the fixture is independently pinned.
 *
 * The population floors are asserted in code (the DOUBLE_ARRAY_MIN_* constants
 * below), so thinning the fixture cannot pass unnoticed, and the rows
 * deliberately include signed zero, not-a-number and both infinities, so the
 * tagged-double policy is exercised end to end.
 * ===========================================================================
 */

String FX_DOUBLE_ARRAY = "double-array";

/*
 * Population floors, and the exact count the fixture is expected to carry.
 *
 * The floors are the minimum population the baseline is worth having: enough
 * Java-derived rows to validate the operations against the inputs and constants
 * DoubleArrayTest and DoubleMatrixTest state, as many seeded-random rows again
 * so the relative bound of the 1e-9 rule is exercised across magnitudes, and
 * the IEEE-edge rows without which the non-finite policy is never measured.
 * They are asserted rather than documented, so a later reduction fails the
 * capture instead of quietly shrinking the comparison.
 */
int DOUBLE_ARRAY_MIN_ROWS = 40;
int DOUBLE_ARRAY_MIN_JAVATEST_ROWS = 16;
int DOUBLE_ARRAY_MIN_RANDOM_ROWS = 16;
int DOUBLE_ARRAY_MIN_IEEE_ROWS = 4;
int DOUBLE_ARRAY_EXPECTED_ROWS = 43;

/**
 * The seeded source of the random rows of this fixture alone.
 *
 * A dedicated generator, seeded from the script's single documented seed,
 * rather than the shared RND: it makes the rows of this fixture independent of
 * how many values every earlier section happened to draw, so a change in one
 * fixture can never move the numbers of another.
 */
Random DOUBLE_ARRAY_RND = new Random(RANDOM_SEED);

/** The magnitudes the seeded rows span, so both bounds of the parity rule are exercised. */
double[] DOUBLE_ARRAY_SCALES = {1e-8d, 1e-4d, 1d, 1e4d, 1e8d};

/** Every emitted row id, so that uniqueness can be asserted against the row count. */
Set<String> DOUBLE_ARRAY_IDS = new LinkedHashSet<>();

/**
 * The values of one emitted row, held as the Java objects that were
 * serialized.
 *
 * The cross-checks below read these fields, so they compare what the fixture
 * actually contains rather than a second computation that could differ from
 * it.
 */
class DoubleArrayCase {
  String id;
  DoubleArray a;
  DoubleArray b;
  double scalar;
  DoubleArray plusScalar;
  DoubleArray plusArray;
  DoubleArray minusScalar;
  DoubleArray minusArray;
  DoubleArray multipliedByScalar;
  DoubleArray multipliedByArray;
  DoubleArray dividedByScalar;
  DoubleArray dividedByArray;
  DoubleArray mapSquared;
  DoubleArray sorted;
  DoubleArray concat;
  DoubleArray subArray;
  double reduceSum;
  double sum;
  double min;
  double max;
  double matrixTotal;
  DoubleMatrix matrixMultipliedBy;
  DoubleMatrix matrixPlus;
  DoubleMatrix matrixMinus;
  DoubleMatrix matrixTranspose;
  DoubleMatrix matrixWith;
}

/**
 * Rejects a row that cannot satisfy the schema.
 *
 * This is a defect in this script rather than a disagreement with Java, so it
 * throws instead of being recorded as a check failure: the guarded driver
 * turns it into a non-zero exit with a stack trace, and nothing is written.
 */
void requireDoubleArrayRow(boolean condition, String id, String detail) {
  if (!condition) {
    throw new IllegalStateException("double-array row '" + id + "': " + detail);
  }
}

/** Asserts a DoubleArray against values a Java test hard-codes, at its DELTA. */
void checkDoubleArrayValues(String rowId, DoubleArray actual, double... expected) {
  CHECK.checkInt(FX_DOUBLE_ARRAY, rowId + " size", expected.length, actual.size());
  if (actual.size() != expected.length) {
    return;
  }
  for (int i = 0; i < expected.length; i++) {
    CHECK.checkClose(FX_DOUBLE_ARRAY, rowId + "[" + i + "]", expected[i], actual.get(i),
        TOL_DOUBLE_ARRAY, "DoubleArrayTest.DELTA");
  }
}

/** Asserts a DoubleMatrix against values a Java test hard-codes, row by row. */
void checkDoubleMatrixValues(String rowId, DoubleMatrix actual, int rowCount, int columnCount,
    double... expected) {
  CHECK.checkInt(FX_DOUBLE_ARRAY, rowId + " rowCount", rowCount, actual.rowCount());
  CHECK.checkInt(FX_DOUBLE_ARRAY, rowId + " columnCount", columnCount, actual.columnCount());
  if (actual.rowCount() != rowCount || actual.columnCount() != columnCount) {
    return;
  }
  for (int i = 0; i < expected.length; i++) {
    int row = i / columnCount;
    int column = i % columnCount;
    CHECK.checkClose(FX_DOUBLE_ARRAY, rowId + "[" + row + "][" + column + "]", expected[i],
        actual.get(row, column), TOL_DOUBLE_ARRAY, "DoubleMatrixTest.DELTA");
  }
}

/**
 * Asserts that a call the Java test expects to reject does reject, with an
 * exception of the type that test names.
 *
 * The type is checked by ASSIGNABILITY, not by name: DoubleArrayTest asserts
 * `assertThatExceptionOfType(IndexOutOfBoundsException.class)` while the
 * implementation throws the more specific `ArrayIndexOutOfBoundsException`,
 * which satisfies that assertion. Comparing simple names would make this check
 * stricter than that test and fail on correct behaviour.
 */
void checkDoubleArrayThrows(String rowId, Class<? extends Throwable> expectedType,
    ThrowingCall call) {
  try {
    Object value = call.get();
    CHECK.fail(FX_DOUBLE_ARRAY, rowId,
        "expected " + expectedType.getSimpleName() + " but it returned " + value);
  } catch (Throwable thrown) {
    requireCapturable(thrown);
    CHECK.checkTrue(FX_DOUBLE_ARRAY, rowId, expectedType.isInstance(thrown),
        "expected " + expectedType.getSimpleName() + " but Java threw " + errorMessage(thrown));
  }
}

/**
 * THE HARD-CODED CONSTANTS OF DoubleArrayTest AND DoubleMatrixTest.
 *
 * The fixture rows are evaluated over the capture's own inputs, so they say
 * nothing about whether the Java implementation still does what its own tests
 * claim. These are those claims, with their inputs, expected values, tolerance
 * (DELTA = 1e-14) and expected exception types:
 *
 *   DoubleArrayTest  :192-201 subArray(from), :203-212 subArray(from,to),
 *                    :315-322 with, :325-364 scalar plus/minus/multipliedBy/
 *                    dividedBy/map/mapWithIndex, :368-397 the array-valued
 *                    forms and their length-mismatch rejections, :419-455
 *                    sorted/min/max/sum/reduce, :458-473 concat.
 *   DoubleMatrixTest :221-231 with, :234-241 multipliedBy, :257-280
 *                    plus/minus/combine and their dimension rejections,
 *                    :282-286 total.
 *
 * A mismatch in any of them aborts the capture, so a changed Java answer
 * cannot be published as the new baseline.
 */
void checkDoubleArrayJavaConstants() {
  DoubleArray oneTwoThree = DoubleArray.of(1d, 2d, 3d);
  DoubleArray halves = DoubleArray.of(0.5d, 0.6d, 0.7d);
  DoubleArray tens = DoubleArray.of(10d, 20d, 30d);
  // Scalar arithmetic (:325-353).
  checkDoubleArrayValues("DoubleArrayTest.test_plus(5)", oneTwoThree.plus(5), 6d, 7d, 8d);
  checkDoubleArrayValues("DoubleArrayTest.test_plus(-5)", oneTwoThree.plus(-5), -4d, -3d, -2d);
  checkDoubleArrayValues("DoubleArrayTest.test_minus(5)", oneTwoThree.minus(5), -4d, -3d, -2d);
  checkDoubleArrayValues("DoubleArrayTest.test_multipliedBy(5)", oneTwoThree.multipliedBy(5),
      5d, 10d, 15d);
  checkDoubleArrayValues("DoubleArrayTest.test_dividedBy(5)", tens.dividedBy(5), 2d, 4d, 6d);
  // map / mapWithIndex (:355-364).
  checkDoubleArrayValues("DoubleArrayTest.test_map", oneTwoThree.map(v -> 1 / v), 1d, 1d / 2d,
      1d / 3d);
  checkDoubleArrayValues("DoubleArrayTest.test_mapWithIndex",
      oneTwoThree.mapWithIndex((i, v) -> i * v), 0d, 2d, 6d);
  // Element-wise arithmetic and its length precondition (:368-397).
  checkDoubleArrayValues("DoubleArrayTest.test_plus_array", oneTwoThree.plus(halves), 1.5d, 2.6d,
      3.7d);
  checkDoubleArrayThrows("DoubleArrayTest.test_plus_array empty", IllegalArgumentException.class,
      () -> oneTwoThree.plus(DoubleArray.EMPTY));
  checkDoubleArrayValues("DoubleArrayTest.test_minus_array", oneTwoThree.minus(halves), 0.5d, 1.4d,
      2.3d);
  checkDoubleArrayThrows("DoubleArrayTest.test_minus_array empty", IllegalArgumentException.class,
      () -> oneTwoThree.minus(DoubleArray.EMPTY));
  checkDoubleArrayValues("DoubleArrayTest.test_multipliedBy_array", oneTwoThree.multipliedBy(halves),
      0.5d, 1.2d, 2.1d);
  checkDoubleArrayValues("DoubleArrayTest.test_dividedBy_array",
      tens.dividedBy(DoubleArray.of(2d, 5d, 10d)), 5d, 4d, 3d);
  // sorted / min / max / sum / reduce (:419-455).
  checkDoubleArrayValues("DoubleArrayTest.test_sorted", DoubleArray.of(2d, 1d, 3d, 0d).sorted(),
      0d, 1d, 2d, 3d);
  checkDoubleArrayValues("DoubleArrayTest.test_sorted empty", DoubleArray.of().sorted());
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleArrayTest.test_min", 1d,
      DoubleArray.of(2d, 1d, 3d).min());
  checkDoubleArrayThrows("DoubleArrayTest.test_min empty", IllegalStateException.class,
      () -> Double.valueOf(DoubleArray.EMPTY.min()));
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleArrayTest.test_max", 3d,
      DoubleArray.of(2d, 1d, 3d).max());
  checkDoubleArrayThrows("DoubleArrayTest.test_max empty", IllegalStateException.class,
      () -> Double.valueOf(DoubleArray.EMPTY.max()));
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleArrayTest.test_sum", 6d,
      DoubleArray.of(2d, 1d, 3d).sum());
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleArrayTest.test_sum empty", 0d, DoubleArray.EMPTY.sum());
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleArrayTest.test_reduce", 6d,
      DoubleArray.of(2d, 1d, 3d).reduce(1d, (r, v) -> r * v));
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleArrayTest.test_reduce empty", 2d,
      DoubleArray.EMPTY.reduce(2d, (r, v) -> r + v));
  // concat / subArray / with, including their index preconditions (:192-212,
  // :315-322, :458-473).
  checkDoubleArrayValues("DoubleArrayTest.test_concat_object", oneTwoThree.concat(halves), 1d, 2d,
      3d, 0.5d, 0.6d, 0.7d);
  checkDoubleArrayValues("DoubleArrayTest.test_subArray_from", oneTwoThree.subArray(1), 2d, 3d);
  checkDoubleArrayValues("DoubleArrayTest.test_subArray_from end", oneTwoThree.subArray(3));
  checkDoubleArrayThrows("DoubleArrayTest.test_subArray_from(4)", IndexOutOfBoundsException.class,
      () -> oneTwoThree.subArray(4));
  checkDoubleArrayValues("DoubleArrayTest.test_subArray_fromTo", oneTwoThree.subArray(1, 2), 2d);
  checkDoubleArrayThrows("DoubleArrayTest.test_subArray_fromTo(0,4)", IndexOutOfBoundsException.class,
      () -> oneTwoThree.subArray(0, 4));
  checkDoubleArrayValues("DoubleArrayTest.test_with", oneTwoThree.with(0, 2.6d), 2.6d, 2d, 3d);
  checkDoubleArrayThrows("DoubleArrayTest.test_with(3)", IndexOutOfBoundsException.class,
      () -> oneTwoThree.with(3, 2d));
  // DoubleMatrix (:221-286).
  DoubleMatrix threeByTwo = DoubleMatrix.copyOf(new double[][] {{1d, 2d}, {3d, 4d}, {5d, 6d}});
  checkDoubleMatrixValues("DoubleMatrixTest.test_with", threeByTwo.with(0, 0, 2.6d), 3, 2, 2.6d, 2d,
      3d, 4d, 5d, 6d);
  checkDoubleArrayThrows("DoubleMatrixTest.test_with(3,0)", IndexOutOfBoundsException.class,
      () -> threeByTwo.with(3, 0, 2d));
  checkDoubleMatrixValues("DoubleMatrixTest.test_multipliedBy", threeByTwo.multipliedBy(5), 3, 2,
      5d, 10d, 15d, 20d, 25d, 30d);
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleMatrixTest.test_total", 21d, threeByTwo.total());
  CHECK.checkExact(FX_DOUBLE_ARRAY, "DoubleMatrixTest.test_total empty", 0d,
      DoubleMatrix.EMPTY.total());
  DoubleMatrix twoByThree = DoubleMatrix.of(2, 3, 1d, 2d, 3d, 4d, 5d, 6d);
  DoubleMatrix twoByThreeSmall = DoubleMatrix.of(2, 3, 0.5d, 0.6d, 0.7d, 0.5d, 0.6d, 0.7d);
  checkDoubleMatrixValues("DoubleMatrixTest.test_plus", twoByThree.plus(twoByThreeSmall), 2, 3,
      1.5d, 2.6d, 3.7d, 4.5d, 5.6d, 6.7d);
  checkDoubleArrayThrows("DoubleMatrixTest.test_plus empty", IllegalArgumentException.class,
      () -> twoByThree.plus(DoubleMatrix.EMPTY));
  checkDoubleMatrixValues("DoubleMatrixTest.test_minus", twoByThree.minus(twoByThreeSmall), 2, 3,
      0.5d, 1.4d, 2.3d, 3.5d, 4.4d, 5.3d);
  checkDoubleMatrixValues("DoubleMatrixTest.test_combine",
      twoByThree.combine(twoByThreeSmall, (x, y) -> x * y), 2, 3, 0.5d, 2d * 0.6d, 3d * 0.7d,
      4d * 0.5d, 5d * 0.6d, 6d * 0.7d);
  checkDoubleArrayThrows("DoubleMatrixTest.test_combine empty", IllegalArgumentException.class,
      () -> twoByThree.combine(DoubleMatrix.EMPTY, (x, y) -> x * y));
}

/**
 * Emits one uniform row and returns the values it carries.
 *
 * Every operation is applied directly, with no exception handling: the
 * preconditions above guarantee that none of them can fail, so anything thrown
 * here is a defect that must abort the capture rather than become an
 * expectation.
 */
DoubleArrayCase addDoubleArrayRow(JArray rows, String id, double[] a, double[] b, double scalar,
    int subArrayFrom, int subArrayTo, double[][] matrixA, double[][] matrixB,
    int withRow, int withColumn, double withValue, boolean captureOnly) {
  requireDoubleArrayRow(DOUBLE_ARRAY_IDS.add(id), id, "duplicate row id");
  requireDoubleArrayRow(a.length > 0, id, "`a` must be non-empty, because min and max have no value for an empty array");
  requireDoubleArrayRow(a.length == b.length, id, "`a` and `b` must have the same length, because the element-wise operations require it");
  requireDoubleArrayRow(matrixA.length > 0 && matrixA[0].length > 0, id, "`matrixA` must have at least one row and one column");
  requireDoubleArrayRow(matrixB.length == matrixA.length, id, "`matrixB` must have the row count of `matrixA`");
  for (int r = 0; r < matrixA.length; r++) {
    requireDoubleArrayRow(matrixA[r].length == matrixA[0].length, id, "`matrixA` must be rectangular");
    requireDoubleArrayRow(matrixB[r].length == matrixA[0].length, id, "`matrixB` must have the shape of `matrixA`");
  }
  requireDoubleArrayRow(subArrayFrom >= 0 && subArrayFrom <= subArrayTo && subArrayTo <= a.length, id,
      "0 <= subArrayFrom <= subArrayTo <= a.length");
  requireDoubleArrayRow(withRow >= 0 && withRow < matrixA.length, id, "`withRow` must index a row of `matrixA`");
  requireDoubleArrayRow(withColumn >= 0 && withColumn < matrixA[0].length, id,
      "`withColumn` must index a column of `matrixA`");

  DoubleArrayCase emitted = new DoubleArrayCase();
  emitted.id = id;
  emitted.a = DoubleArray.copyOf(a);
  emitted.b = DoubleArray.copyOf(b);
  emitted.scalar = scalar;
  DoubleMatrix first = DoubleMatrix.copyOf(matrixA);
  DoubleMatrix second = DoubleMatrix.copyOf(matrixB);
  emitted.plusScalar = emitted.a.plus(scalar);
  emitted.plusArray = emitted.a.plus(emitted.b);
  emitted.minusScalar = emitted.a.minus(scalar);
  emitted.minusArray = emitted.a.minus(emitted.b);
  emitted.multipliedByScalar = emitted.a.multipliedBy(scalar);
  emitted.multipliedByArray = emitted.a.multipliedBy(emitted.b);
  emitted.dividedByScalar = emitted.a.dividedBy(scalar);
  emitted.dividedByArray = emitted.a.dividedBy(emitted.b);
  emitted.mapSquared = emitted.a.map(value -> value * value);
  // The identity of the reduction is zero, so `reduceSum` is `sum` computed
  // through the general member: both are left folds in index order, and the
  // check below holds them to each other as well as to the fixture.
  emitted.reduceSum = emitted.a.reduce(0d, (accumulated, value) -> accumulated + value);
  emitted.sum = emitted.a.sum();
  emitted.min = emitted.a.min();
  emitted.max = emitted.a.max();
  emitted.sorted = emitted.a.sorted();
  emitted.concat = emitted.a.concat(emitted.b);
  emitted.subArray = emitted.a.subArray(subArrayFrom, subArrayTo);
  emitted.matrixMultipliedBy = first.multipliedBy(scalar);
  emitted.matrixPlus = first.plus(second);
  emitted.matrixMinus = first.minus(second);
  emitted.matrixTranspose = first.transpose();
  emitted.matrixTotal = first.total();
  emitted.matrixWith = first.with(withRow, withColumn, withValue);

  rows.add(new JObject()
      .set("id", jStr(id))
      .set("a", jDoubleArray(emitted.a))
      .set("b", jDoubleArray(emitted.b))
      .set("scalar", jDbl(scalar))
      .set("subArrayFrom", jInt(subArrayFrom))
      .set("subArrayTo", jInt(subArrayTo))
      .set("matrixA", jDoubleMatrix(first))
      .set("matrixB", jDoubleMatrix(second))
      .set("withRow", jInt(withRow))
      .set("withColumn", jInt(withColumn))
      .set("withValue", jDbl(withValue))
      .set("plusScalar", jDoubleArray(emitted.plusScalar))
      .set("plusArray", jDoubleArray(emitted.plusArray))
      .set("minusScalar", jDoubleArray(emitted.minusScalar))
      .set("minusArray", jDoubleArray(emitted.minusArray))
      .set("multipliedByScalar", jDoubleArray(emitted.multipliedByScalar))
      .set("multipliedByArray", jDoubleArray(emitted.multipliedByArray))
      .set("dividedByScalar", jDoubleArray(emitted.dividedByScalar))
      .set("dividedByArray", jDoubleArray(emitted.dividedByArray))
      .set("mapSquared", jDoubleArray(emitted.mapSquared))
      .set("reduceSum", jDbl(emitted.reduceSum))
      .set("sum", jDbl(emitted.sum))
      .set("min", jDbl(emitted.min))
      .set("max", jDbl(emitted.max))
      .set("sorted", jDoubleArray(emitted.sorted))
      .set("concat", jDoubleArray(emitted.concat))
      .set("subArray", jDoubleArray(emitted.subArray))
      .set("matrixMultipliedBy", jDoubleMatrix(emitted.matrixMultipliedBy))
      .set("matrixPlus", jDoubleMatrix(emitted.matrixPlus))
      .set("matrixMinus", jDoubleMatrix(emitted.matrixMinus))
      .set("matrixTranspose", jDoubleMatrix(emitted.matrixTranspose))
      .set("matrixTotal", jDbl(emitted.matrixTotal))
      .set("matrixWith", jDoubleMatrix(emitted.matrixWith)));

  // `sum` and `reduceSum` are the same left fold from zero, so they must agree
  // on every row, including the rows whose fold produces a non-finite value -
  // which is checked here rather than only on the rows that have a Java
  // literal.
  CHECK.checkExact(FX_DOUBLE_ARRAY, id + " reduceSum equals sum", emitted.sum, emitted.reduceSum);
  CHECK.countRow(FX_DOUBLE_ARRAY);
  if (captureOnly) {
    CHECK.countCaptureOnly(FX_DOUBLE_ARRAY);
  }
  return emitted;
}

/**
 * Compares one emitted array against the literal a Java test asserts for it.
 *
 * The tolerance is DoubleArrayTest's own DELTA, because that is the tolerance
 * the literal was written to: the fixture stores the full-precision value, and
 * this check exists to prove that value is the one the Java test describes.
 */
void checkDoubleArrayAgainstJava(String id, String expectation, DoubleArray actual, double[] expected,
    String javaSource) {
  String label = id + " " + expectation + " (" + javaSource + ")";
  CHECK.checkInt(FX_DOUBLE_ARRAY, label + " length", expected.length, actual.size());
  if (expected.length != actual.size()) {
    return;
  }
  for (int i = 0; i < expected.length; i++) {
    CHECK.checkClose(FX_DOUBLE_ARRAY, label + "[" + i + "]", expected[i], actual.get(i),
        TOL_DOUBLE_ARRAY, "DoubleArrayTest.DELTA");
  }
}

/** Compares one emitted matrix, row-major, against the literal a Java test asserts for it. */
void checkDoubleMatrixAgainstJava(String id, String expectation, DoubleMatrix actual, int expectedRows,
    int expectedColumns, double[] expectedRowMajor, String javaSource) {
  String label = id + " " + expectation + " (" + javaSource + ")";
  CHECK.checkInt(FX_DOUBLE_ARRAY, label + " rowCount", expectedRows, actual.rowCount());
  CHECK.checkInt(FX_DOUBLE_ARRAY, label + " columnCount", expectedColumns, actual.columnCount());
  if (expectedRows != actual.rowCount() || expectedColumns != actual.columnCount()) {
    return;
  }
  for (int r = 0; r < expectedRows; r++) {
    for (int c = 0; c < expectedColumns; c++) {
      CHECK.checkClose(FX_DOUBLE_ARRAY, label + "[" + r + "][" + c + "]",
          expectedRowMajor[r * expectedColumns + c], actual.get(r, c), TOL_DOUBLE_ARRAY,
          "DoubleMatrixTest literal");
    }
  }
}

/** Compares one emitted scalar against the value a Java test asserts exactly. */
void checkDoubleArrayScalarAgainstJava(String id, String expectation, double expected, double actual,
    String javaSource) {
  CHECK.checkExact(FX_DOUBLE_ARRAY, id + " " + expectation + " (" + javaSource + ")", expected, actual);
}

/**
 * The rows built from the inputs of DoubleArrayTest and DoubleMatrixTest, each
 * cross-checked against the constants those tests assert.
 *
 * The inputs are the literal arrays and matrices of the Java tests - [1,2,3],
 * [0.5,0.6,0.7], [10,20,30], [2,5,10], [2,1,3], [2,1,3,0], [1,2,3,3,4], [2],
 * the tolerance pairs, the `filled` arrays, {{1,2},{3,4},{5,6}}, of(2,3,1..6),
 * of(2,3,0.5,0.6,0.7,...), {{1,2,3},{4,5,6},{7,8,9}}, the 3x6 transpose matrix,
 * {{2,3}}, {{2}}, {{1,2},{3,4}} and identity(2)/identity(3) - so that every
 * expectation with a Java literal is compared against it.
 */
void addJavaDerivedDoubleArrayRows(JArray rows) {
  double[] oneTwoThree = {1d, 2d, 3d};
  double[] halves = {0.5d, 0.6d, 0.7d};
  double[] tens = {10d, 20d, 30d};
  double[] divisors = {2d, 5d, 10d};
  double[][] threeByTwo = {{1d, 2d}, {3d, 4d}, {5d, 6d}};
  double[][] threeByTwoHalves = {{0.5d, 0.6d}, {0.7d, 0.5d}, {0.6d, 0.7d}};
  double[][] twoByThree = {{1d, 2d, 3d}, {4d, 5d, 6d}};
  double[][] twoByThreeHalves = {{0.5d, 0.6d, 0.7d}, {0.5d, 0.6d, 0.7d}};
  double[][] threeByThree = {{1d, 2d, 3d}, {4d, 5d, 6d}, {7d, 8d, 9d}};
  double[][] identityThree = DoubleMatrix.identity(3).toArray();
  double[][] identityTwo = DoubleMatrix.identity(2).toArray();
  double[][] twoByTwo = {{1d, 2d}, {3d, 4d}};
  double[][] oneByTwo = {{2d, 3d}};
  double[][] oneByTwoOnes = {{1d, 1d}};
  double[][] oneByOne = {{2d}};
  double[][] oneByOneThree = {{3d}};
  double[][] threeBySix = {
      {1d, 2d, 3d, 4d, 5d, 6d},
      {7d, 8d, 9d, 10d, 11d, 12d},
      {13d, 14d, 15d, 16d, 17d, 18d}};
  double[][] threeBySixHalves = {
      {0.5d, 0.6d, 0.7d, 0.5d, 0.6d, 0.7d},
      {0.5d, 0.6d, 0.7d, 0.5d, 0.6d, 0.7d},
      {0.5d, 0.6d, 0.7d, 0.5d, 0.6d, 0.7d}};

  // DoubleArrayTest.test_plus (:325-331), test_minus (:333-339),
  // test_multipliedBy (:341-346), test_map (:355-359), test_plus_array
  // (:368-374), test_minus_array (:376-382), test_multipliedBy_array
  // (:384-390), test_concat_object (:467-470), test_subArray_fromTo
  // (:203-213), test_sorted (:419-425); DoubleMatrixTest.test_multipliedBy
  // (:234-240), test_total (:282-286), test_with (:220-232).
  DoubleArrayCase plusFive = addDoubleArrayRow(rows, "javatest-plus-scalar-5", oneTwoThree, halves,
      5d, 1, 3, threeByTwo, threeByTwoHalves, 0, 0, 2.6d, false);
  checkDoubleArrayAgainstJava(plusFive.id, "plusScalar", plusFive.plusScalar,
      new double[] {6d, 7d, 8d}, "DoubleArrayTest.test_plus");
  checkDoubleArrayAgainstJava(plusFive.id, "minusScalar", plusFive.minusScalar,
      new double[] {-4d, -3d, -2d}, "DoubleArrayTest.test_minus");
  checkDoubleArrayAgainstJava(plusFive.id, "multipliedByScalar", plusFive.multipliedByScalar,
      new double[] {5d, 10d, 15d}, "DoubleArrayTest.test_multipliedBy");
  checkDoubleArrayAgainstJava(plusFive.id, "plusArray", plusFive.plusArray,
      new double[] {1.5d, 2.6d, 3.7d}, "DoubleArrayTest.test_plus_array");
  checkDoubleArrayAgainstJava(plusFive.id, "minusArray", plusFive.minusArray,
      new double[] {0.5d, 1.4d, 2.3d}, "DoubleArrayTest.test_minus_array");
  checkDoubleArrayAgainstJava(plusFive.id, "multipliedByArray", plusFive.multipliedByArray,
      new double[] {0.5d, 1.2d, 2.1d}, "DoubleArrayTest.test_multipliedBy_array");
  checkDoubleArrayAgainstJava(plusFive.id, "mapSquared", plusFive.mapSquared,
      new double[] {1d, 4d, 9d}, "DoubleArrayTest.test_map shape, x -> x * x");
  checkDoubleArrayAgainstJava(plusFive.id, "sorted", plusFive.sorted, oneTwoThree,
      "DoubleArrayTest.test_sorted");
  checkDoubleArrayAgainstJava(plusFive.id, "concat", plusFive.concat,
      new double[] {1d, 2d, 3d, 0.5d, 0.6d, 0.7d}, "DoubleArrayTest.test_concat_object");
  checkDoubleArrayAgainstJava(plusFive.id, "subArray", plusFive.subArray, new double[] {2d, 3d},
      "DoubleArrayTest.test_subArray_fromTo subArray(1, 3)");
  checkDoubleArrayScalarAgainstJava(plusFive.id, "sum", 6d, plusFive.sum,
      "DoubleArrayTest.test_sum");
  checkDoubleArrayScalarAgainstJava(plusFive.id, "min", 1d, plusFive.min,
      "DoubleArrayTest.test_min");
  checkDoubleArrayScalarAgainstJava(plusFive.id, "max", 3d, plusFive.max,
      "DoubleArrayTest.test_max");
  checkDoubleMatrixAgainstJava(plusFive.id, "matrixMultipliedBy", plusFive.matrixMultipliedBy, 3, 2,
      new double[] {5d, 10d, 15d, 20d, 25d, 30d}, "DoubleMatrixTest.test_multipliedBy");
  checkDoubleMatrixAgainstJava(plusFive.id, "matrixWith", plusFive.matrixWith, 3, 2,
      new double[] {2.6d, 2d, 3d, 4d, 5d, 6d}, "DoubleMatrixTest.test_with");
  checkDoubleArrayScalarAgainstJava(plusFive.id, "matrixTotal", 21d, plusFive.matrixTotal,
      "DoubleMatrixTest.test_total");

  // The zero scalar of test_plus / test_minus, which both leave the array
  // unchanged, and the second `with` assertion of DoubleMatrixTest.test_with.
  DoubleArrayCase plusZero = addDoubleArrayRow(rows, "javatest-plus-scalar-0", oneTwoThree, halves,
      0d, 0, 3, threeByTwo, threeByTwoHalves, 0, 0, 1d, false);
  checkDoubleArrayAgainstJava(plusZero.id, "plusScalar", plusZero.plusScalar, oneTwoThree,
      "DoubleArrayTest.test_plus plus(0)");
  checkDoubleArrayAgainstJava(plusZero.id, "minusScalar", plusZero.minusScalar, oneTwoThree,
      "DoubleArrayTest.test_minus minus(0)");
  checkDoubleArrayAgainstJava(plusZero.id, "subArray", plusZero.subArray, oneTwoThree,
      "DoubleArrayTest.test_subArray_fromTo subArray(0, 3)");
  checkDoubleMatrixAgainstJava(plusZero.id, "matrixWith", plusZero.matrixWith, 3, 2,
      new double[] {1d, 2d, 3d, 4d, 5d, 6d}, "DoubleMatrixTest.test_with with(0, 0, 1)");

  // The negative scalar of test_plus / test_minus, whose expectations are the
  // mirror image of the plus(5) row - which is what proves the two overloads
  // were not transposed.
  DoubleArrayCase plusMinusFive = addDoubleArrayRow(rows, "javatest-plus-scalar-minus-5",
      oneTwoThree, halves, -5d, 2, 3, threeByTwo, threeByTwoHalves, 2, 1, 2.6d, false);
  checkDoubleArrayAgainstJava(plusMinusFive.id, "plusScalar", plusMinusFive.plusScalar,
      new double[] {-4d, -3d, -2d}, "DoubleArrayTest.test_plus plus(-5)");
  checkDoubleArrayAgainstJava(plusMinusFive.id, "minusScalar", plusMinusFive.minusScalar,
      new double[] {6d, 7d, 8d}, "DoubleArrayTest.test_minus minus(-5)");
  checkDoubleArrayAgainstJava(plusMinusFive.id, "subArray", plusMinusFive.subArray,
      new double[] {3d}, "DoubleArrayTest.test_subArray_fromTo subArray(2, 3)");

  // The unit scalar, where both multipliedBy overloads short-circuit, and the
  // empty half-open slice subArray(3, 3).
  DoubleArrayCase timesOne = addDoubleArrayRow(rows, "javatest-multipliedby-scalar-1", oneTwoThree,
      halves, 1d, 3, 3, threeByTwo, threeByTwoHalves, 1, 0, 0d, false);
  checkDoubleArrayAgainstJava(timesOne.id, "multipliedByScalar", timesOne.multipliedByScalar,
      oneTwoThree, "DoubleArrayTest.test_multipliedBy multipliedBy(1)");
  checkDoubleArrayAgainstJava(timesOne.id, "subArray", timesOne.subArray, new double[] {},
      "DoubleArrayTest.test_subArray_fromTo subArray(3, 3)");
  checkDoubleMatrixAgainstJava(timesOne.id, "matrixMultipliedBy", timesOne.matrixMultipliedBy, 3, 2,
      new double[] {1d, 2d, 3d, 4d, 5d, 6d}, "DoubleMatrixTest.test_multipliedBy multipliedBy(1)");

  // DoubleArrayTest.test_dividedBy (:348-353) and test_dividedBy_array
  // (:392-398), the only two operations whose Java implementation differs
  // between its overloads: the scalar form multiplies by the reciprocal, the
  // element-wise form divides.
  DoubleArrayCase dividedByFive = addDoubleArrayRow(rows, "javatest-dividedby-scalar-5", tens,
      divisors, 5d, 1, 2, twoByThree, twoByThreeHalves, 0, 2, 2.6d, false);
  checkDoubleArrayAgainstJava(dividedByFive.id, "dividedByScalar", dividedByFive.dividedByScalar,
      new double[] {2d, 4d, 6d}, "DoubleArrayTest.test_dividedBy");
  checkDoubleArrayAgainstJava(dividedByFive.id, "dividedByArray", dividedByFive.dividedByArray,
      new double[] {5d, 4d, 3d}, "DoubleArrayTest.test_dividedBy_array");
  checkDoubleArrayScalarAgainstJava(dividedByFive.id, "sum", 60d, dividedByFive.sum,
      "DoubleArrayTest.test_sum shape");

  DoubleArrayCase dividedByOne = addDoubleArrayRow(rows, "javatest-dividedby-scalar-1", tens,
      divisors, 1d, 0, 2, twoByThree, twoByThreeHalves, 1, 1, -5d, false);
  checkDoubleArrayAgainstJava(dividedByOne.id, "dividedByScalar", dividedByOne.dividedByScalar,
      tens, "DoubleArrayTest.test_dividedBy dividedBy(1)");

  // DoubleArrayTest.test_sorted (:419-425) four-element case.
  DoubleArrayCase sortedFour = addDoubleArrayRow(rows, "javatest-sorted-four",
      new double[] {2d, 1d, 3d, 0d}, new double[] {1d, 1d, 1d, 1d}, 2.6d, 1, 4, twoByTwo,
      identityTwo, 1, 1, 2.6d, false);
  checkDoubleArrayAgainstJava(sortedFour.id, "sorted", sortedFour.sorted,
      new double[] {0d, 1d, 2d, 3d}, "DoubleArrayTest.test_sorted");

  // DoubleArrayTest.test_min (:427-432), test_max (:434-439), test_sum
  // (:441-446), test_reduce (:448-455).
  DoubleArrayCase minMaxSum = addDoubleArrayRow(rows, "javatest-min-max-sum-three",
      new double[] {2d, 1d, 3d}, new double[] {3d, 2d, 1d}, 2.6d, 0, 2, oneByTwo, oneByTwoOnes, 0,
      1, 5d, false);
  checkDoubleArrayScalarAgainstJava(minMaxSum.id, "min", 1d, minMaxSum.min,
      "DoubleArrayTest.test_min");
  checkDoubleArrayScalarAgainstJava(minMaxSum.id, "max", 3d, minMaxSum.max,
      "DoubleArrayTest.test_max");
  checkDoubleArrayScalarAgainstJava(minMaxSum.id, "sum", 6d, minMaxSum.sum,
      "DoubleArrayTest.test_sum");
  checkDoubleArrayScalarAgainstJava(minMaxSum.id, "reduceSum", 6d, minMaxSum.reduceSum,
      "DoubleArrayTest.test_reduce, identity 0 and addition");

  // The single-element array of test_min / test_max / test_sum / test_sorted,
  // where min and max short-circuit rather than folding, together with the
  // one-by-one matrix of DoubleMatrixTest.test_reduce (:288-295).
  DoubleArrayCase single = addDoubleArrayRow(rows, "javatest-single-element", new double[] {2d},
      new double[] {5d}, 2d, 0, 1, oneByOne, oneByOneThree, 0, 0, 2.6d, false);
  checkDoubleArrayScalarAgainstJava(single.id, "min", 2d, single.min, "DoubleArrayTest.test_min");
  checkDoubleArrayScalarAgainstJava(single.id, "max", 2d, single.max, "DoubleArrayTest.test_max");
  checkDoubleArrayScalarAgainstJava(single.id, "sum", 2d, single.sum, "DoubleArrayTest.test_sum");
  checkDoubleArrayAgainstJava(single.id, "sorted", single.sorted, new double[] {2d},
      "DoubleArrayTest.test_sorted");
  checkDoubleArrayScalarAgainstJava(single.id, "matrixTotal", 2d, single.matrixTotal,
      "DoubleMatrixTest.test_reduce one-by-one matrix");

  // The duplicate-bearing array of DoubleArrayTest (:36-38 assertContent
  // usage), which is the row that exercises `sorted` with equal elements.
  DoubleArrayCase duplicates = addDoubleArrayRow(rows, "javatest-duplicates-five",
      new double[] {1d, 2d, 3d, 3d, 4d}, new double[] {4d, 3d, 2d, 1d, 0d}, -1.5d, 1, 4, twoByTwo,
      identityTwo, 0, 1, 1d, false);
  checkDoubleArrayAgainstJava(duplicates.id, "sorted", duplicates.sorted,
      new double[] {1d, 2d, 3d, 3d, 4d}, "DoubleArrayTest sorted with duplicates");
  checkDoubleArrayScalarAgainstJava(duplicates.id, "min", 1d, duplicates.min,
      "DoubleArrayTest.test_min shape");
  checkDoubleArrayScalarAgainstJava(duplicates.id, "max", 4d, duplicates.max,
      "DoubleArrayTest.test_max shape");

  // The four pairs of DoubleArrayTest.test_equalWithTolerance (:496-514):
  // [1,2] against [1,2.02] and [1,2.009], and [0,0] against [0,0.02] and
  // [0,0.009]. They are the arrays whose differences sit either side of that
  // test's tolerance, which makes them the most informative inputs for the
  // element-wise operations.
  DoubleArrayCase tolerancePair = addDoubleArrayRow(rows, "javatest-tolerance-pair-2-02",
      new double[] {1d, 2d}, new double[] {1d, 2.02d}, 1d, 0, 2, oneByTwo, oneByTwoOnes, 0, 0, 0d,
      false);
  checkDoubleArrayAgainstJava(tolerancePair.id, "minusArray", tolerancePair.minusArray,
      new double[] {0d, -0.02d}, "DoubleArrayTest.test_equalWithTolerance difference");
  addDoubleArrayRow(rows, "javatest-tolerance-pair-2-009", new double[] {1d, 2.02d},
      new double[] {1d, 2.009d}, 2d, 1, 2, oneByTwo, new double[][] {{2d, 3d}}, 0, 1, 2.6d, false);
  addDoubleArrayRow(rows, "javatest-tolerance-zeros-02", new double[] {0d, 0d},
      new double[] {0d, 0.02d}, 0.5d, 0, 1, twoByTwo, identityTwo, 1, 0, -5d, false);
  addDoubleArrayRow(rows, "javatest-tolerance-zeros-009", new double[] {0d, 0.009d},
      new double[] {0d, 0d}, 2.6d, 0, 2, twoByTwo, identityTwo, 0, 0, 5d, false);

  // The two `filled` factories of DoubleArrayTest (:305-313): filled(3) is
  // three zeros and filled(3, 1.5) is three copies of 1.5.
  DoubleArrayCase filled = addDoubleArrayRow(rows, "javatest-filled-three",
      DoubleArray.filled(3).toArray(), DoubleArray.filled(3, 1.5d).toArray(), 1.5d, 0, 3,
      identityThree, threeByThree, 1, 2, 2.6d, false);
  checkDoubleArrayAgainstJava(filled.id, "a", filled.a, new double[] {0d, 0d, 0d},
      "DoubleArrayTest filled(3)");
  checkDoubleArrayAgainstJava(filled.id, "b", filled.b, new double[] {1.5d, 1.5d, 1.5d},
      "DoubleArrayTest filled(3, 1.5)");
  checkDoubleArrayScalarAgainstJava(filled.id, "matrixTotal", 3d, filled.matrixTotal,
      "DoubleMatrix.identity(3) total");

  // The nine-element array and the square matrix of
  // DoubleMatrixTest.testTransposeMatrix (:298-310).
  DoubleArrayCase nine = addDoubleArrayRow(rows, "javatest-nine-elements",
      new double[] {1d, 2d, 3d, 4d, 5d, 6d, 7d, 8d, 9d},
      new double[] {9d, 8d, 7d, 6d, 5d, 4d, 3d, 2d, 1d}, 3d, 2, 7, threeByThree, identityThree, 2,
      2, 5d, false);
  checkDoubleMatrixAgainstJava(nine.id, "matrixTranspose", nine.matrixTranspose, 3, 3,
      new double[] {1d, 4d, 7d, 2d, 5d, 8d, 3d, 6d, 9d}, "DoubleMatrixTest.testTransposeMatrix");
  checkDoubleArrayScalarAgainstJava(nine.id, "sum", 45d, nine.sum, "DoubleArrayTest.test_sum shape");

  // The non-square matrix of DoubleMatrixTest.testTransposeMatrix
  // (:311-321), whose transpose is six-by-three - the shape the transposition
  // is most likely to be got wrong on.
  DoubleArrayCase nonSquare = addDoubleArrayRow(rows, "javatest-matrix-three-by-six",
      new double[] {1d, 2d, 3d, 4d, 5d, 6d}, new double[] {13d, 14d, 15d, 16d, 17d, 18d}, 2d, 1, 5,
      threeBySix, threeBySixHalves, 2, 5, 2.6d, false);
  checkDoubleMatrixAgainstJava(nonSquare.id, "matrixTranspose", nonSquare.matrixTranspose, 6, 3,
      new double[] {
          1d, 7d, 13d,
          2d, 8d, 14d,
          3d, 9d, 15d,
          4d, 10d, 16d,
          5d, 11d, 17d,
          6d, 12d, 18d},
      "DoubleMatrixTest.testTransposeMatrix");

  // DoubleMatrixTest.test_plus (:256-263) and test_minus (:264-271), whose
  // operands are exactly these two two-by-three matrices.
  DoubleArrayCase matrixPair = addDoubleArrayRow(rows, "javatest-matrix-two-by-three-plus-minus",
      oneTwoThree, halves, 2.6d, 0, 3, twoByThree, twoByThreeHalves, 1, 2, 1d, false);
  checkDoubleMatrixAgainstJava(matrixPair.id, "matrixPlus", matrixPair.matrixPlus, 2, 3,
      new double[] {1.5d, 2.6d, 3.7d, 4.5d, 5.6d, 6.7d}, "DoubleMatrixTest.test_plus");
  checkDoubleMatrixAgainstJava(matrixPair.id, "matrixMinus", matrixPair.matrixMinus, 2, 3,
      new double[] {0.5d, 1.4d, 2.3d, 3.5d, 4.4d, 5.3d}, "DoubleMatrixTest.test_minus");

  // The one-by-two matrix of DoubleMatrixTest.test_reduce (:288-295) and the
  // remaining `assertContent` array of DoubleArrayTest, so that a row with a
  // single matrix row and a two-element array is present.
  DoubleArrayCase oneRow = addDoubleArrayRow(rows, "javatest-matrix-one-by-two",
      new double[] {2d, 3d}, new double[] {0.5d, 0.6d}, 2d, 0, 2, oneByTwo, oneByTwoOnes, 0, 0, 2d,
      false);
  checkDoubleArrayScalarAgainstJava(oneRow.id, "matrixTotal", 5d, oneRow.matrixTotal,
      "DoubleMatrixTest.test_reduce one-by-two matrix");
  checkDoubleMatrixAgainstJava(oneRow.id, "matrixTranspose", oneRow.matrixTranspose, 2, 1,
      new double[] {2d, 3d}, "DoubleMatrixTest.testTransposeMatrix shape");
}

/** The value of one seeded element at the given magnitude, signed either way. */
double nextDoubleArrayValue(double scale) {
  return (DOUBLE_ARRAY_RND.nextDouble() * 2d - 1d) * scale;
}

/**
 * The seeded-random rows.
 *
 * Every row draws its two operands, its scalar, its matrices and its
 * replacement value from a different combination of the declared magnitudes,
 * so the parity rule's relative bound is exercised across fifteen orders of
 * magnitude and with both signs rather than only near one. The generator is
 * seeded, so the rows are the same on every run; they carry no Java literal
 * and are counted as capture-only.
 */
void addSeededRandomDoubleArrayRows(JArray rows) {
  for (int scenario = 0; scenario < 18; scenario++) {
    double scale = DOUBLE_ARRAY_SCALES[scenario % DOUBLE_ARRAY_SCALES.length];
    double otherScale = DOUBLE_ARRAY_SCALES[(scenario + 2) % DOUBLE_ARRAY_SCALES.length];
    double scalarScale = DOUBLE_ARRAY_SCALES[(scenario + 1) % DOUBLE_ARRAY_SCALES.length];
    int length = 2 + (scenario % 6);
    double[] a = new double[length];
    double[] b = new double[length];
    for (int i = 0; i < length; i++) {
      a[i] = nextDoubleArrayValue(scale);
      b[i] = nextDoubleArrayValue(otherScale);
    }
    int matrixRows = 1 + (scenario % 3);
    int matrixColumns = 1 + ((scenario + 1) % 4);
    double[][] matrixA = new double[matrixRows][matrixColumns];
    double[][] matrixB = new double[matrixRows][matrixColumns];
    for (int r = 0; r < matrixRows; r++) {
      for (int c = 0; c < matrixColumns; c++) {
        matrixA[r][c] = nextDoubleArrayValue(scale);
        matrixB[r][c] = nextDoubleArrayValue(otherScale);
      }
    }
    int subArrayFrom = scenario % (length + 1);
    int subArrayTo = subArrayFrom + ((scenario * 3) % (length - subArrayFrom + 1));
    String id = "random-seeded-" + (scenario < 10 ? "0" : "") + scenario;
    addDoubleArrayRow(rows, id, a, b, nextDoubleArrayValue(scalarScale), subArrayFrom, subArrayTo,
        matrixA, matrixB, scenario % matrixRows, scenario % matrixColumns,
        nextDoubleArrayValue(scale), true);
  }
}

/**
 * The IEEE-edge rows, without which the tagged-double policy this fixture
 * exists to pin is never exercised end to end.
 *
 * Between them they drive: the total order of `sorted`, where negative zero
 * precedes positive zero and not-a-number sorts last; the propagation of
 * not-a-number through `min`, `max`, `sum` and `reduceSum`; a zero scalar,
 * whose reciprocal is an infinity and whose product with an infinity is
 * not-a-number; a zero element in the second operand, which makes
 * `dividedByArray` non-finite; and multiplication of large magnitudes, which
 * overflows to an infinity. None of them has a Java literal to be compared
 * against, so all are counted as capture-only.
 */
void addIeeeEdgeDoubleArrayRows(JArray rows) {
  double[][] onesTwoByTwo = {{1d, 1d}, {1d, 1d}};
  // Not-a-number, both infinities and both zeros in one array.
  addDoubleArrayRow(rows, "ieee-nonfinite-mix",
      new double[] {Double.NaN, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY, -0.0d, 0d},
      new double[] {1d, 2d, 3d, 4d, 5d}, 2.5d, 0, 5,
      new double[][] {{Double.NaN, Double.POSITIVE_INFINITY}, {Double.NEGATIVE_INFINITY, -0.0d}},
      onesTwoByTwo, 1, 1, 0d, true);
  // A zero scalar: the reciprocal is an infinity, and an infinite element
  // multiplied by zero is not-a-number.
  addDoubleArrayRow(rows, "ieee-scalar-zero",
      new double[] {1d, 0d, -2d, Double.POSITIVE_INFINITY}, new double[] {1d, 1d, 1d, 1d}, 0d, 1, 3,
      new double[][] {{1d, 0d}, {-1d, Double.POSITIVE_INFINITY}}, onesTwoByTwo, 0, 1, 0d, true);
  // A zero element in the second operand, so the element-wise division is
  // non-finite in two places and not-a-number where both elements are zero.
  addDoubleArrayRow(rows, "ieee-divisor-zero-element", new double[] {1d, 2d, 0d, 4d},
      new double[] {1d, 0d, -0.0d, 2d}, 2d, 2, 4, new double[][] {{1d, 2d}, {0d, -0.0d}},
      onesTwoByTwo, 1, 0, -0.0d, true);
  // Magnitudes whose products overflow the finite range.
  addDoubleArrayRow(rows, "ieee-multiply-overflow",
      new double[] {1e200d, -1e200d, 1.5e308d}, new double[] {1e200d, 1e200d, 2d}, 1e10d, 0, 2,
      new double[][] {{1e200d, -1e200d}, {1.5e308d, 1e-320d}}, onesTwoByTwo, 0, 0, 1e308d, true);
  // Signed zero on both sides: `sorted` orders them, and every expectation
  // carries the exact signed value, so -0.0 and 0.0 are never interchangeable.
  addDoubleArrayRow(rows, "ieee-signed-zero", new double[] {0d, -0.0d, 1d, -1d},
      new double[] {-0.0d, 0d, -1d, 1d}, -0.0d, 1, 4,
      new double[][] {{0d, -0.0d}, {-0.0d, 0d}}, onesTwoByTwo, 0, 1, -0.0d, true);
  // Both infinities as operands and as the scalar, so that the sum of opposite
  // infinities is captured as not-a-number.
  addDoubleArrayRow(rows, "ieee-infinities",
      new double[] {Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY, 1d},
      new double[] {1d, 1d, Double.POSITIVE_INFINITY}, Double.POSITIVE_INFINITY, 0, 3,
      new double[][] {{Double.POSITIVE_INFINITY, 1d}, {1d, Double.NEGATIVE_INFINITY}},
      onesTwoByTwo, 1, 1, Double.NEGATIVE_INFINITY, true);
}

Jn buildDoubleArrayFixture() {
  JArray rows = new JArray();
  addJavaDerivedDoubleArrayRows(rows);
  int javaDerived = rows.size();
  addSeededRandomDoubleArrayRows(rows);
  int seededRandom = rows.size() - javaDerived;
  addIeeeEdgeDoubleArrayRows(rows);
  int ieeeEdge = rows.size() - javaDerived - seededRandom;

  // The population floors, asserted rather than described. A fixture below any
  // of them measures less than the baseline is required to measure, so it
  // aborts the capture instead of being written.
  CHECK.checkTrue(FX_DOUBLE_ARRAY, "Java-derived row floor",
      javaDerived >= DOUBLE_ARRAY_MIN_JAVATEST_ROWS,
      "expected at least " + DOUBLE_ARRAY_MIN_JAVATEST_ROWS + " Java-derived rows, found "
          + javaDerived);
  CHECK.checkTrue(FX_DOUBLE_ARRAY, "seeded-random row floor",
      seededRandom >= DOUBLE_ARRAY_MIN_RANDOM_ROWS,
      "expected at least " + DOUBLE_ARRAY_MIN_RANDOM_ROWS + " seeded-random rows, found "
          + seededRandom);
  CHECK.checkTrue(FX_DOUBLE_ARRAY, "IEEE-edge row floor", ieeeEdge >= DOUBLE_ARRAY_MIN_IEEE_ROWS,
      "expected at least " + DOUBLE_ARRAY_MIN_IEEE_ROWS + " IEEE-edge rows, found " + ieeeEdge);
  CHECK.checkTrue(FX_DOUBLE_ARRAY, "total row floor", rows.size() >= DOUBLE_ARRAY_MIN_ROWS,
      "expected at least " + DOUBLE_ARRAY_MIN_ROWS + " rows, found " + rows.size());
  CHECK.checkCount(FX_DOUBLE_ARRAY, "double-array rows", DOUBLE_ARRAY_EXPECTED_ROWS, rows.size());
  // Every id was added to a set as its row was emitted, so an equal count is
  // the uniqueness the fixture's failure reporting depends on.
  CHECK.checkCount(FX_DOUBLE_ARRAY, "double-array row ids", DOUBLE_ARRAY_EXPECTED_ROWS,
      DOUBLE_ARRAY_IDS.size());

  checkDoubleArrayJavaConstants();
  // DoubleArrayTest asserts equalWithTolerance against its own DELTA of 1e-14;
  // that tolerance is checked here rather than being folded into the fixture.
  DoubleArray base = DoubleArray.of(1d, 2d, 3d);
  DoubleArray nudged = DoubleArray.of(1d + 1e-15, 2d, 3d);
  CHECK.checkTrue(FX_DOUBLE_ARRAY, "equalWithTolerance within DELTA",
      base.equalWithTolerance(nudged, TOL_DOUBLE_ARRAY),
      "expected equalWithTolerance to hold within " + TOL_DOUBLE_ARRAY);
  CHECK.checkTrue(FX_DOUBLE_ARRAY, "equalWithTolerance outside DELTA",
      !base.equalWithTolerance(DoubleArray.of(1.1d, 2d, 3d), TOL_DOUBLE_ARRAY),
      "expected equalWithTolerance to fail outside " + TOL_DOUBLE_ARRAY);
  return rows;
}

/* ===========================================================================
 * SECTION 13 - THE REFERENCE-DATA MANIFEST
 *
 * Enumerated from the Java reference data, with EVERY count asserted in code:
 * a count that differs from the independently verified expectation aborts the
 * capture, so an edited resource cannot silently reshape the manifest.
 *
 * The consumer asserts its own data tables against this document key by key and
 * row by row, so the key names, the nesting and the counts are the contract -
 * keep them explicit, self-describing and stable.
 * ===========================================================================
 */

String FX_MANIFEST = "manifest";

/**
 * Public static constants of a holder class, read reflectively and returned in
 * NAME ORDER.
 *
 * Reflection is deliberate and permitted in this tool (see the header): it is
 * the only way to get a provably COMPLETE constant list, which is the point of
 * a manifest. The type is matched by simple name so that the element types do
 * not all have to be imported.
 *
 * What reflection does NOT give is an order: `Class.getDeclaredFields` is
 * specified to return the fields in no particular order. Using it as the
 * serialized order would make the manifest depend on a JVM implementation
 * detail, so the constants are sorted by (name, field name) - a TOTAL order
 * the JDK cannot change.
 *
 * The field name is part of the key because a name on its own is not unique: a
 * holder may declare two constants for one value, as `OvernightIndices`
 * declares the deprecated `EUR_ESTER` alongside `EUR_ESTR` for the same
 * `EUR-ESTR` index. Both are public constants, the asserted counts count
 * fields, and sorting by name alone would leave their relative order to the
 * JVM again.
 */
List<Named> namedConstants(Class<?> holder, String typeSimpleName) {
  List<String[]> keys = new ArrayList<>();
  Map<String, Named> byKey = new LinkedHashMap<>();
  for (Field field : holder.getDeclaredFields()) {
    if (Modifier.isPublic(field.getModifiers()) && Modifier.isStatic(field.getModifiers())
        && field.getType().getSimpleName().equals(typeSimpleName)) {
      try {
        Named constant = (Named) field.get(null);
        String key = constant.getName() + "\u0000" + field.getName();
        keys.add(new String[] {constant.getName(), field.getName(), key});
        byKey.put(key, constant);
      } catch (IllegalAccessException ex) {
        throw new IllegalStateException("Cannot read " + holder.getSimpleName() + "."
            + field.getName(), ex);
      }
    }
  }
  keys.sort(new Comparator<String[]>() {
    public int compare(String[] left, String[] right) {
      int byName = left[0].compareTo(right[0]);
      return byName != 0 ? byName : left[1].compareTo(right[1]);
    }
  });
  List<Named> result = new ArrayList<>();
  for (String[] key : keys) {
    result.add(byKey.get(key[2]));
  }
  return result;
}

/** A named-constant group: its asserted count and its names in name order. */
Jn jNamedGroup(String what, Class<?> holder, String typeSimpleName, int expectedCount) {
  List<Named> constants = namedConstants(holder, typeSimpleName);
  CHECK.checkCount(FX_MANIFEST, what, expectedCount, constants.size());
  JArray names = new JArray();
  for (Named constant : constants) {
    names.add(jStr(constant.getName()));
  }
  return new JObject().set("count", jInt(constants.size())).set("names", names);
}

/**
 * The key/value rows of an INI section, in FILE ORDER.
 *
 * File order is preserved because the lenient-pattern lists are applied
 * sequentially - each matching pattern rewrites the string before the next is
 * tried - so their order is behaviour, not presentation. valueList is used
 * rather than asMap so that a repeated key stays lossless (asMap joins
 * repeated values with a comma).
 */
Jn jIniSectionRows(IniFile ini, String sectionName, String what, int expectedCount) {
  PropertySet section = ini.section(sectionName);
  JArray rows = new JArray();
  int count = 0;
  for (String key : section.keys()) {
    for (String value : section.valueList(key)) {
      rows.add(new JObject().set("key", jStr(key)).set("value", jStr(value)));
      count++;
    }
  }
  CHECK.checkCount(FX_MANIFEST, what, expectedCount, count);
  return new JObject().set("count", jInt(count)).set("rows", rows);
}

/** The 74 Currency.ini rows, with their historic flags. */
Jn buildCurrencyManifest() {
  IniFile ini = ResourceConfig.combinedIniFile("Currency.ini");
  JArray rows = new JArray();
  int total = 0;
  int historicCount = 0;
  for (String code : ini.sections()) {
    // CurrencyDataLoader accepts only three-letter upper-case sections.
    if (code.length() != 3) {
      continue;
    }
    PropertySet properties = ini.section(code);
    boolean historic = properties.keys().contains("historic")
        && Boolean.parseBoolean(properties.value("historic"));
    rows.add(new JObject()
        .set("code", jStr(code))
        .set("minorUnitDigits", jInt(Integer.parseInt(properties.value("minorUnitDigits"))))
        .set("triangulationCurrency", jStr(properties.value("triangulationCurrency")))
        .set("historic", jBool(historic)));
    total++;
    if (historic) {
      historicCount++;
    }
  }
  // All 74 INI rows, of which 19 are historic, leaving the 55 that
  // Currency.getAvailableCurrencies() returns. The manifest records all three
  // numbers because no single public method returns the 74: the active set
  // omits the historic rows, and the `historic` flag itself is not exposed at
  // all, so the resource is the only place both are stated. (Currency.of DOES
  // resolve a historic code to its configured instance - Currency.DYNAMIC is
  // pre-seeded with loadCurrencies(true) - and the currency-math fixture
  // relies on that, asserting it per row in
  // `checkCurrencyIniAgainstCurrencyOf`; minted defaults of 0 minor units and
  // USD triangulation apply only to a code this resource does not define.)
  CHECK.checkCount(FX_MANIFEST, "Currency.ini rows", 74, total);
  CHECK.checkCount(FX_MANIFEST, "Currency.ini historic rows", 19, historicCount);
  CHECK.checkCount(FX_MANIFEST, "Currency.getAvailableCurrencies()", total - historicCount,
      Currency.getAvailableCurrencies().size());
  return new JObject()
      .set("count", jInt(total))
      .set("historicCount", jInt(historicCount))
      .set("activeCount", jInt(total - historicCount))
      .set("rows", rows);
}

/** The ordered market-convention priority list. */
Jn buildMarketConventionPriority() {
  IniFile ini = ResourceConfig.combinedIniFile("CurrencyData.ini");
  String ordering = ini.section("marketConventionPriority").value("ordering");
  JArray list = new JArray();
  List<String> codes = new ArrayList<>();
  for (String code : ordering.split(",")) {
    String trimmed = code.trim();
    if (!trimmed.isEmpty()) {
      list.add(jStr(trimmed));
      codes.add(trimmed);
    }
  }
  // Emitted as a JSON ARRAY, never a set or a map: the order decides which
  // currency of a pair is the base, so it is semantically load-bearing.
  CHECK.checkCount(FX_MANIFEST, "marketConventionPriority entries", 9, codes.size());
  CHECK.checkEquals(FX_MANIFEST, "marketConventionPriority order",
      Arrays.asList("XAU", "EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "JPY"), codes);
  return list;
}

/** The 92 configured currency pairs with their rate digits. */
Jn buildCurrencyPairManifest() {
  IniFile ini = ResourceConfig.combinedIniFile("CurrencyPair.ini");
  JArray rows = new JArray();
  int count = 0;
  for (String section : ini.sections()) {
    PropertySet properties = ini.section(section);
    // The SECTION name is the pair ("EUR/AUD"); the only key inside it is
    // "rateDigits". Reading the pair from the key would label all 92 rows
    // "rateDigits" while still counting 92, so the count check alone would not
    // catch it.
    for (String key : properties.keys()) {
      rows.add(new JObject()
          .set("pair", jStr(section))
          .set("rateDigits", jInt(Integer.parseInt(properties.value(key).trim()))));
      count++;
    }
  }
  CHECK.checkCount(FX_MANIFEST, "CurrencyPair.ini rows", 92, count);
  CHECK.checkCount(FX_MANIFEST, "CurrencyPair.getAvailablePairs()", count,
      CurrencyPair.getAvailablePairs().size());
  return new JObject().set("count", jInt(count)).set("rows", rows);
}

/** The 251 alpha-3 to alpha-2 country rows, read the way Country itself reads them. */
Jn buildCountryManifest() {
  PropertiesFile file = PropertiesFile.of(
      ResourceLocator.ofClasspath(Country.class, "country.properties").getCharSource());
  PropertySet properties = file.getProperties();
  JArray rows = new JArray();
  int count = 0;
  for (String alpha3 : properties.keys()) {
    rows.add(new JObject()
        .set("alpha3", jStr(alpha3))
        .set("alpha2", jStr(properties.value(alpha3))));
    count++;
  }
  CHECK.checkCount(FX_MANIFEST, "country.properties rows", 251, count);
  return new JObject().set("count", jInt(count)).set("rows", rows);
}

/**
 * Index rows read from a CSV through the public CsvFile reader, with every
 * column preserved.
 *
 * CsvFile is used rather than line counting because these files are CRLF and
 * contain comma-only separator lines which CsvFile skips: counting lines gives
 * 306 / 36 / 10 / 19, while the real data-row counts are 271 / 35 / 9 / 16.
 */
Jn buildIndexCsvManifest(String resourceName, String what, int expectedCount) {
  JArray rows = new JArray();
  JArray headerArray = new JArray();
  int count = 0;
  boolean headersRecorded = false;
  for (ResourceLocator resource : ResourceConfig.orderedResources(resourceName)) {
    CsvFile csv = CsvFile.of(resource.getCharSource(), true);
    if (!headersRecorded) {
      for (String header : csv.headers()) {
        headerArray.add(jStr(header));
      }
      headersRecorded = true;
    }
    for (CsvRow row : csv.rows()) {
      JObject entry = new JObject();
      for (String header : csv.headers()) {
        entry.set(header, jStr(row.getValue(header)));
      }
      rows.add(entry);
      count++;
    }
  }
  CHECK.checkCount(FX_MANIFEST, what, expectedCount, count);
  return new JObject()
      .set("count", jInt(count))
      .set("headers", headerArray)
      .set("rows", rows);
}

/** The 41 FloatingRateName constants and the 404 alias rows in seven sections. */
Jn buildFloatingRateNameManifest() {
  IniFile ini = ResourceConfig.combinedIniFile("FloatingRateNameData.ini");
  // The seven sections and their independently verified row counts.
  Object[][] sections = {
      {"ibor", 159}, {"iborFixingDateOffset", 3}, {"overnightCompounded", 156},
      {"overnightAveraged", 6}, {"price", 30}, {"currencyDefaultIbor", 23},
      {"currencyDefaultOvernight", 27},
  };
  JObject sectionObject = new JObject();
  int totalRows = 0;
  for (Object[] section : sections) {
    String name = (String) section[0];
    int expected = ((Integer) section[1]).intValue();
    Jn rows = jIniSectionRows(ini, name, "FloatingRateNameData.ini [" + name + "]", expected);
    sectionObject.set(name, rows);
    totalRows += expected;
  }
  CHECK.checkCount(FX_MANIFEST, "FloatingRateNameData.ini total alias rows", 404, totalRows);
  return new JObject()
      .set("constants", jNamedGroup("FloatingRateNames constants", FloatingRateNames.class,
          "FloatingRateName", 41))
      .set("aliasRowCount", jInt(totalRows))
      .set("sections", sectionObject);
}

/**
 * Alternate names for one enum family.
 *
 * Two counts are recorded, because the two sources legitimately differ: the
 * INI `[alternates]` section holds the DECLARED rows, while
 * ExtendedEnum.alternateNames() also registers a derived UPPER-CASE key for
 * every mixed-case alternate. OvernightIndex is the case in point - 10 INI rows
 * against 13 API entries, because "DKK-Tom Next", "EUR-EuroSTR" and
 * "USD-Federal Funds" each gain an upper-cased twin.
 */
Jn buildAlternateNames(String resourceName, ExtendedEnum<?> extendedEnum, String what,
    int expectedIniRows, int expectedApiRows) {
  IniFile ini = ResourceConfig.combinedIniFile(resourceName);
  JArray iniRows = new JArray();
  int iniCount = 0;
  if (ini.contains("alternates")) {
    PropertySet section = ini.section("alternates");
    for (String key : section.keys()) {
      for (String value : section.valueList(key)) {
        iniRows.add(new JObject().set("alternateName", jStr(key)).set("standardName", jStr(value)));
        iniCount++;
      }
    }
  }
  CHECK.checkCount(FX_MANIFEST, what + " [alternates] INI rows", expectedIniRows, iniCount);
  // The API view, in sorted order so the manifest stays byte-stable.
  Map<String, String> apiSorted = new TreeMap<>(extendedEnum.alternateNames());
  JArray apiRows = new JArray();
  for (Map.Entry<String, String> entry : apiSorted.entrySet()) {
    apiRows.add(new JObject()
        .set("alternateName", jStr(entry.getKey()))
        .set("standardName", jStr(entry.getValue())));
  }
  CHECK.checkCount(FX_MANIFEST, what + " alternateNames() API rows", expectedApiRows,
      apiSorted.size());
  return new JObject()
      .set("iniRowCount", jInt(iniCount))
      .set("iniRows", iniRows)
      .set("apiExpandedRowCount", jInt(apiSorted.size()))
      .set("apiExpandedRows", apiRows);
}

/** External name groups (FpML, SWIFT) for one enum family. */
Jn buildExternalNames(ExtendedEnum<?> extendedEnum, String what, Object[][] expectedGroups) {
  JObject groups = new JObject();
  for (Object[] expectedGroup : expectedGroups) {
    String groupName = (String) expectedGroup[0];
    int expectedCount = ((Integer) expectedGroup[1]).intValue();
    Map<String, String> sorted = new TreeMap<>(extendedEnum.externalNames(groupName).externalNames());
    JArray rows = new JArray();
    for (Map.Entry<String, String> entry : sorted.entrySet()) {
      rows.add(new JObject()
          .set("externalName", jStr(entry.getKey()))
          .set("standardName", jStr(entry.getValue())));
    }
    CHECK.checkCount(FX_MANIFEST, what + " externals." + groupName, expectedCount, sorted.size());
    groups.set(groupName, new JObject().set("count", jInt(sorted.size())).set("rows", rows));
  }
  return groups;
}

/** The THBA date table: 75 year rows plus the separate weekend declaration. */
Jn buildHolidayCalendarDataManifest() {
  IniFile ini = ResourceConfig.combinedIniFile("HolidayCalendarData.ini");
  PropertySet section = ini.section("THBA");
  JArray rows = new JArray();
  int yearRows = 0;
  String weekend = null;
  for (String key : section.keys()) {
    if (key.equals("Weekend")) {
      // The weekend declaration is emitted separately: counting it as a year
      // row would report 76 instead of 75.
      weekend = section.value(key);
      continue;
    }
    rows.add(new JObject().set("year", jStr(key)).set("dates", jStr(section.value(key))));
    yearRows++;
  }
  CHECK.checkCount(FX_MANIFEST, "HolidayCalendarData.ini [THBA] year rows", 75, yearRows);
  CHECK.checkEquals(FX_MANIFEST, "HolidayCalendarData.ini [THBA] Weekend", "Sat,Sun", weekend);
  return new JObject()
      .set("THBA", new JObject()
          .set("yearRowCount", jInt(yearRows))
          .set("weekend", jStr(weekend))
          .set("rows", rows));
}

/**
 * The 31 default-by-currency calendar ids.
 *
 * Thirteen of them - CLSA CNBE COBO HKHK IDJA ILTA INMU KRSE RUMO SARI SGSI
 * TRIS TWTA - have no built-in calendar, so they are ids that are expected NOT
 * to resolve. The manifest records the mapping and each id's resolvability
 * without depending on resolution succeeding.
 */
Jn buildHolidayCalendarDefaultManifest() {
  IniFile ini = ResourceConfig.combinedIniFile("HolidayCalendarDefaultData.ini");
  PropertySet section = ini.section("defaultByCurrency");
  JArray rows = new JArray();
  int count = 0;
  int resolvable = 0;
  for (String currencyCode : section.keys()) {
    String calendarId = section.value(currencyCode);
    boolean canResolve;
    try {
      HolidayCalendarId.of(calendarId).resolve(ReferenceData.standard());
      canResolve = true;
    } catch (Throwable thrown) {
      // A calendar id with no built-in calendar is the expectation here; an
      // Error is not, and requireCapturable sends it to the driver.
      requireCapturable(thrown);
      canResolve = false;
    }
    rows.add(new JObject()
        .set("currency", jStr(currencyCode))
        .set("calendarId", jStr(calendarId))
        .set("resolvableAgainstStandardReferenceData", jBool(canResolve)));
    count++;
    if (canResolve) {
      resolvable++;
    }
  }
  CHECK.checkCount(FX_MANIFEST, "HolidayCalendarDefaultData.ini defaultByCurrency rows", 31, count);
  CHECK.checkCount(FX_MANIFEST, "defaultByCurrency ids without a built-in calendar", 13,
      count - resolvable);
  return new JObject()
      .set("count", jInt(count))
      .set("resolvableCount", jInt(resolvable))
      .set("unresolvableCount", jInt(count - resolvable))
      .set("rows", rows);
}

/** The ids of the built-in holiday calendars available in ReferenceData.standard(). */
Jn buildBuiltInCalendarManifest() {
  Map<String, HolidayCalendar> normalized =
      new TreeMap<>(HolidayCalendars.extendedEnum().lookupAllNormalized());
  JArray names = new JArray();
  for (String name : normalized.keySet()) {
    names.add(jStr(name));
  }
  // 30 = the four weekend / no-holiday calendars plus 26 dated ones (the 25
  // generated calendars and THBA).
  CHECK.checkCount(FX_MANIFEST, "built-in holiday calendars", 30, normalized.size());
  return new JObject().set("count", jInt(normalized.size())).set("names", names);
}

Jn buildManifest() {
  IniFile dayCountIni = ResourceConfig.combinedIniFile("DayCount.ini");
  IniFile rollConventionIni = ResourceConfig.combinedIniFile("RollConvention.ini");
  IniFile businessDayIni = ResourceConfig.combinedIniFile("BusinessDayConvention.ini");
  IniFile periodAdditionIni = ResourceConfig.combinedIniFile("PeriodAdditionConvention.ini");

  JObject externalNames = new JObject()
      .set("DayCount", buildExternalNames(DayCount.extendedEnum(), "DayCount",
          new Object[][] {{"FpML", 14}, {"SWIFT", 8}}))
      .set("RollConvention", buildExternalNames(RollConvention.extendedEnum(), "RollConvention",
          new Object[][] {{"FpML", 44}}))
      .set("BusinessDayConvention", buildExternalNames(BusinessDayConvention.extendedEnum(),
          "BusinessDayConvention", new Object[][] {{"FpML", 5}, {"SWIFT", 3}}));

  JObject lenientPatterns = new JObject()
      .set("DayCount",
          jIniSectionRows(dayCountIni, "lenientPatterns", "DayCount.ini [lenientPatterns]", 67))
      .set("RollConvention", jIniSectionRows(rollConventionIni, "lenientPatterns",
          "RollConvention.ini [lenientPatterns]", 11))
      .set("BusinessDayConvention", jIniSectionRows(businessDayIni, "lenientPatterns",
          "BusinessDayConvention.ini [lenientPatterns]", 11))
      .set("PeriodAdditionConvention", jIniSectionRows(periodAdditionIni, "lenientPatterns",
          "PeriodAdditionConvention.ini [lenientPatterns]", 3));

  JObject alternateNames = new JObject()
      .set("IborIndex", buildAlternateNames("IborIndex.ini", IborIndex.extendedEnum(),
          "IborIndex", 1, 1))
      .set("OvernightIndex", buildAlternateNames("OvernightIndex.ini",
          OvernightIndex.extendedEnum(), "OvernightIndex", 10, 13))
      .set("FxIndex", buildAlternateNames("FxIndex.ini", FxIndex.extendedEnum(), "FxIndex", 1, 1));

  JObject stubConventions = new JObject();
  JArray stubNames = new JArray();
  for (StubConvention stub : StubConvention.values()) {
    stubNames.add(jStr(stub.getName()));
  }
  CHECK.checkCount(FX_MANIFEST, "StubConvention values", 8, StubConvention.values().length);
  stubConventions.set("count", jInt(StubConvention.values().length)).set("names", stubNames);

  return new JObject()
      .set("schemaVersion", jInt(1L))
      .set("generator", jStr("tools/parity-capture/capture-baseline.jsh"))
      .set("randomSeed", jInt(RANDOM_SEED))
      .set("currencies", buildCurrencyManifest())
      .set("marketConventionPriority", buildMarketConventionPriority())
      .set("currencyPairs", buildCurrencyPairManifest())
      .set("countries", buildCountryManifest())
      .set("iborIndices", buildIndexCsvManifest("IborIndexData.csv", "IborIndexData.csv rows", 271))
      .set("overnightIndices",
          buildIndexCsvManifest("OvernightIndexData.csv", "OvernightIndexData.csv rows", 35))
      .set("priceIndices",
          buildIndexCsvManifest("PriceIndexData.csv", "PriceIndexData.csv rows", 9))
      .set("fxIndices", buildIndexCsvManifest("FxIndexData.csv", "FxIndexData.csv rows", 16))
      .set("iborIndexConstants",
          jNamedGroup("IborIndices constants", IborIndices.class, "IborIndex", 113))
      .set("overnightIndexConstants",
          jNamedGroup("OvernightIndices constants", OvernightIndices.class, "OvernightIndex", 21))
      .set("priceIndexConstants",
          jNamedGroup("PriceIndices constants", PriceIndices.class, "PriceIndex", 9))
      .set("fxIndexConstants", jNamedGroup("FxIndices constants", FxIndices.class, "FxIndex", 8))
      .set("floatingRateNames", buildFloatingRateNameManifest())
      .set("dayCounts", jNamedGroup("DayCounts constants", DayCounts.class, "DayCount", 21))
      .set("businessDayConventions", jNamedGroup("BusinessDayConventions constants",
          BusinessDayConventions.class, "BusinessDayConvention", 7))
      .set("rollConventions", jNamedGroup("RollConventions constants", RollConventions.class,
          "RollConvention", 45))
      .set("periodAdditionConventions", jNamedGroup("PeriodAdditionConventions constants",
          PeriodAdditionConventions.class, "PeriodAdditionConvention", 3))
      .set("dateSequences",
          jNamedGroup("DateSequences constants", DateSequences.class, "DateSequence", 6))
      .set("stubConventions", stubConventions)
      .set("holidayCalendarIds", jNamedGroup("HolidayCalendarIds constants",
          HolidayCalendarIds.class, "HolidayCalendarId", 29))
      .set("holidayCalendarDefaultByCurrency", buildHolidayCalendarDefaultManifest())
      .set("builtInHolidayCalendars", buildBuiltInCalendarManifest())
      .set("holidayCalendarData", buildHolidayCalendarDataManifest())
      .set("externalNames", externalNames)
      .set("lenientPatterns", lenientPatterns)
      .set("alternateNames", alternateNames);
}

/* ===========================================================================
 * SECTION 14 - THE DRIVER
 *
 * All seven documents are built in memory, every self-check runs, and only
 * then is anything written - so a failed run cannot leave a half-valid fixture
 * on disk.
 *
 * The whole capture runs under ONE EXCLUSIVE LOCK on the output root, taken
 * before the first path is validated and released only after publication and
 * cleanup have finished; see `acquireOutputLock` and the `finally` of `capture`.
 *
 * `CAPTURE_COMPLETED` is not redundant with `CHECK.ok()`. A JShell snippet
 * that fails to COMPILE is reported and skipped, and calling the method it
 * declared then throws SPIResolutionException at run time; that is caught
 * here, but it would leave the failure list empty and the run would exit 0 on
 * a capture that never happened. The flag closes that hole, so the exit status
 * means "every document was built and every check passed".
 *
 * Neither `CHECK` nor `CAPTURE_COMPLETED` is named by the `/exit` expression,
 * and that is the point. Both are snippet declarations, and an incomplete
 * classpath does not merely skip one snippet - it can take a whole section's
 * declarations down at once, leaving an exit expression that names something
 * which no longer exists. Such an expression does not exit: JShell falls
 * through to its REPL and terminates with status ZERO (header behaviour 4),
 * which would turn the worst environment error into a silent success. So this
 * section states its verdict POSITIVELY, once, into the `capture.exit.status`
 * property that SECTION 0 cleared, and the exit expression reads that property
 * with `Integer.getInteger` and defaults to 1. Every way of not reaching that
 * statement - a failed check, a driver exception, the preflight, a snippet
 * that failed to compile, the statement itself failing to compile - leaves the
 * property unset and the run exits non-zero.
 * ===========================================================================
 */

boolean CAPTURE_COMPLETED = false;

void capture() throws Throwable {
  if (!PREFLIGHT_OK) {
    throw new IllegalStateException(
        "classpath preflight failed: " + MISSING_CLASSES.size() + " class(es) missing");
  }
  System.out.println("capture-baseline.jsh - Java parity baseline capture");
  System.out.println("  java.version    = " + System.getProperty("java.version"));
  System.out.println("  output root     = " + Paths.get(OUT_ROOT).toAbsolutePath().normalize());
  System.out.println("  random seed     = " + RANDOM_SEED);
  System.out.println();

  // The lock is taken HERE, before the first `stageDocument` - which is the
  // first call that validates a path and creates a directory under the output
  // root - and released in this method's `finally` after publication and cleanup.
  acquireOutputLock();
  try {
    System.out.println("building daycount-baseline.json ...");
    stageDocument(OUTPUT_DAYCOUNT, buildDayCountFixture());
    System.out.println("building schedule-baseline.json ...");
    stageDocument(OUTPUT_SCHEDULE, buildScheduleFixture());
    System.out.println("building fx-baseline.json ...");
    stageDocument(OUTPUT_FX, buildFxFixture());
    System.out.println("building currency-math-baseline.json ...");
    stageDocument(OUTPUT_CURRENCY_MATH, buildCurrencyMathFixture());
    System.out.println("building holiday-baseline.json ...");
    // One row per line: see Jn.writeCompact for why this document alone.
    stageRowsPerLineDocument(OUTPUT_HOLIDAY, buildHolidayFixture());
    System.out.println("building double-array-baseline.json ...");
    stageDocument(OUTPUT_DOUBLE_ARRAY, buildDoubleArrayFixture());
    System.out.println("building reference-data-manifest.json ...");
    stageDocument(OUTPUT_MANIFEST, buildManifest());
    // The manifest is one document rather than a row array, so it contributes a
    // single row to the summary - reporting 0 would read like a failure.
    CHECK.countRow(FX_MANIFEST);

    CHECK.printSummary();

    if (!CHECK.ok()) {
      CHECK.printFailures();
      System.out.println();
      System.out.println("ABORTED: " + CHECK.failures.size()
          + " check(s) failed, so no file was written.");
      return;
    }
    System.out.println();
    System.out.println("all checks passed - writing " + PENDING_DOCUMENTS.size() + " document(s):");
    flushDocuments();
    CAPTURE_COMPLETED = true;
    System.out.println();
    System.out.println("capture complete.");
  } finally {
    releaseOutputLock();
  }
}

try {
  capture();
} catch (Throwable thrown) {
  // A failure here is itself a capture failure: record it so the exit status
  // is non-zero even if no individual check had failed.
  CHECK.fail("driver", "capture", "threw " + errorMessage(thrown));
  System.out.println("ABORTED: the capture driver threw " + errorMessage(thrown));
  for (StackTraceElement frame : thrown.getStackTrace()) {
    System.out.println("    at " + frame);
  }
}

// The verdict, published to the channel SECTION 0 cleared. This is the ONLY
// statement in the file that can make the run exit 0, and it says so
// positively: every document built (CAPTURE_COMPLETED) and every check passed
// (CHECK.ok()). Reaching it at all requires that this snippet compiled, which
// requires that the declarations it names survived the load - so an incomplete
// classpath, which destroys them, cannot reach it either.
if (CHECK.ok() && CAPTURE_COMPLETED) {
  System.setProperty("capture.exit.status", "0");
}

// A closing pointer for the preflight case, whose symptom is the thousands of
// JShell compilation errors standing between the preflight's own block and
// this line: without it the last thing a reader sees is a compilation error
// about a symbol they never wrote. It is guarded by PREFLIGHT_OK, which
// SECTION 0 declares without naming one Strata or Guava type and which
// therefore survives exactly the loads that destroy everything else. It
// decides nothing - the status is already settled by the property above, and
// this snippet failing to compile could only cost the message.
if (!PREFLIGHT_OK) {
  System.out.println();
  System.out.println("ABORTED: the classpath preflight failed - read the FIRST block of");
  System.out.println("  output above rather than the JShell errors that follow it. Nothing");
  System.out.println("  was written, and this run exits non-zero.");
}

// `java.lang.Integer` and two literals, and it must stay that way: an /exit
// expression that fails to compile does not exit (header behaviour 4), so
// naming a snippet-declared symbol here would surrender the exit status in
// precisely the case where it carries the most information. Property absent or
// unparseable => 1, so failure is the default and every path above it has to
// earn the zero.
/exit (Integer.getInteger("capture.exit.status", 1))

// ---------------------------------------------------------------------------
// THE TWO LINES BELOW ARE THE FALLBACK, AND THEY ARE REACHED ONLY WHEN THE
// /exit ABOVE COULD NOT BE EVALUATED AT ALL. Do not delete them as dead code:
// on every ordinary run they ARE dead, because the line above terminates the
// process - which is exactly why they cost nothing and why they are the only
// thing standing between an unusable classpath and a silent success.
//
// The case they exist for is not hypothetical. A classpath that carries the
// Strata jars WITHOUT their Guava dependency lets javac resolve the Strata
// signatures but not the Guava types inside them, and on JDK 21.0.12.1 that
// combination makes javac itself fail with an internal NullPointerException
// while generating code for one of this script's methods
// ("An exception has occurred in the compiler ... Type.getTag() because
// "type" is null"). After that crash JShell's compilation context is
// corrupted: EVERY later snippet is rejected with a spurious
// "cannot find symbol / symbol: class" pair, and that includes the /exit
// expression above - and, measured, a bare `/exit 1` as well. So no exit
// expression of any shape survives it, and the run would fall through to the
// REPL and terminate ZERO.
//
// Two measured JShell behaviours make the recovery possible. First, a failed
// /exit does not end the file: JShell reports the error and reads the NEXT
// line, so these lines run. Second, `/reset` is a command and needs no
// compilation, and it discards every snippet and restarts the execution
// engine - which throws the corrupted compilation context away with them - so
// the literal below is compiled in a pristine context and does exit.
//
// The status is the literal 1 rather than the property, because `/reset`
// restarts the JVM the property lived in, and because reaching this line is
// itself proof of failure: it means the capture's own declarations did not
// survive the load, so nothing was built and nothing was written.
/reset
/exit 1
