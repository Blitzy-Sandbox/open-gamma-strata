/*
 * ===========================================================================
 *  capture-baseline.jsh - Java parity baseline capture for the Scala port
 * ===========================================================================
 *
 *  WHAT THIS IS
 *  ------------
 *  A JShell script (JDK 21) that runs against the Maven-built *Java* Strata
 *  jars and emits the six numerical parity baseline fixtures plus the
 *  reference-data manifest that pin the behaviour of the Scala port of
 *  `strata-collect` / `strata-basics`.
 *
 *  The seven JSON documents are the deliverable; this script is retained so
 *  that the baseline can be regenerated and audited by a third party.
 *
 *  This is a developer / audit tool. It lives OUTSIDE both sbt modules and is
 *  on no sbt source root, so it is compiled by nothing and shipped in nothing.
 *  Its extension is `.jsh`, never `.java`: that is load-bearing, because the
 *  deliverable forbids `.java` files. This script never emits, generates or
 *  compiles Java source, and it adds no build step.
 *
 *  DEPENDENCY PURITY (Rule 1 / Gate 2) - READ BEFORE "FIXING" THE CLASSPATH
 *  -----------------------------------------------------------------------
 *  The JShell classpath below deliberately carries the Java Strata jars
 *  TOGETHER WITH Guava and Joda. That is intentional and is NOT a Rule 1
 *  violation: Gate 2 measures the sbt `Compile` / `Test` classpaths of the two
 *  Scala modules, and `tools/` is on neither of them. Removing Guava or Joda
 *  here does not improve dependency purity - it simply breaks the capture,
 *  because the Java implementation being measured depends on both.
 *
 *  REPOSITORY BOUNDARY
 *  -------------------
 *  This script READS `modules/**` (through the classpath and the classpath
 *  resources inside the jars) and NEVER writes there. Writes are confined to
 *  the seven output paths listed in `OUTPUT_*` below, and `guardOutputPath`
 *  enforces that at run time rather than by convention.
 *
 *  HOW TO RUN
 *  ----------
 *  1. Build the Java modules once (writes only into git-ignored `target/`):
 *
 *       mvn -B -pl modules/collect,modules/basics -am -DskipTests \
 *           -Dcheckstyle.skip=true -Dmaven.javadoc.skip=true package
 *
 *     `package` is preferred over `install`; `install` writes the shared local
 *     repository under coordinates other builds read, and is redundant here.
 *
 *  2. Capture, from the repository root:
 *
 *       mvn -q -pl modules/basics dependency:build-classpath \
 *           -Dmdep.outputFile=/tmp/basics-cp.txt
 *       jshell --class-path \
 *         "$(cat /tmp/basics-cp.txt):modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar" \
 *         -R-Xmx900m tools/parity-capture/capture-baseline.jsh
 *
 *     If the jars are already installed in the local Maven repository, the
 *     equivalent zero-rebuild classpath is the six entries
 *     strata-basics, strata-collect, guava, failureaccess, joda-beans and
 *     joda-convert. Either classpath produces byte-identical output.
 *
 *  3. Optional: choose where the fixtures are written (default: the current
 *     directory, which is expected to be the repository root):
 *
 *       jshell ... -R-Dparity.out.dir=/tmp/parity-out ...
 *
 *  See `tools/parity-capture/README.md` for the full procedure, the fixture
 *  schemas and the manifest schema.
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
 *  expected counts. All documents are built in memory and written only after
 *  every check has passed, so a failed run cannot leave a half-valid fixture on
 *  disk. Any failure prints an actionable diagnostic and the script exits with
 *  a non-zero status.
 *
 *  REFLECTION IS PERMITTED HERE
 *  ----------------------------
 *  The "no reflection" rule applies to the Scala codec path, not to this
 *  developer tool. This script uses reflection only to enumerate public
 *  constants holders (`DayCounts`, `HolidayCalendarIds`, ...), which is the
 *  cleanest way to obtain a provably complete constant list. Where an
 *  `ExtendedEnum` accessor gives the same answer, the accessor is preferred.
 *
 *  TWO JSHELL BEHAVIOURS THIS SCRIPT RELIES ON (both verified on JDK 21)
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
 *
 *  A snippet that throws does not stop JShell - it prints a trace and the next
 *  snippet runs. The whole capture is therefore performed by one guarded
 *  driver invoked from a single snippet at the end of this file.
 * ===========================================================================
 */

/* ===========================================================================
 * SECTION 0 - CLASSPATH PREFLIGHT
 *
 * Runs before anything else and fails loudly on the most common environment
 * error: an incomplete --class-path.
 *
 * It is written with Class.forName on STRING literals, referencing no Strata
 * type, so that it still compiles and still runs when those types are absent -
 * which is precisely the case it has to diagnose. Without it a developer sees
 * only JShell's wall of "package does not exist" errors; with it, the first
 * thing printed names the missing classes and the classpath to fix.
 * ===========================================================================
 */

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
  System.out.println("  then pass the dependency classpath as shown in this file's header.");
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
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.Month;
import java.time.Period;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

// --- Static imports, copied verbatim from the Java tests whose data tables
// --- this script reproduces, minus the test-only helpers (TestHelper,
// --- Guavate, AssertJ, JUnit) which are replaced by local equivalents.
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

/** Base class of the tiny JSON tree. */
abstract class Jn {
  /** True for values that are rendered as a single token (string, number, ...). */
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

/** Escapes a string to a pure-ASCII JSON string literal. */
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

/** ISO-8601 date, or null. */
Jn jDate(LocalDate date) {
  return date == null ? jNull() : jStr(date.toString());
}

/**
 * The canonical name of a Strata value. Every `Named` type is written through
 * getName(), and the remaining identity-bearing types through toString(), so
 * the emitted strings are exactly the identities the Scala port reproduces
 * (for example "Act/365F", "GBP-LIBOR-3M", "EUR/USD", "P3M", "3M",
 * "GBLO+USNY").
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
 * full-precision doubles for the Scala side's 1e-9 absolute AND relative
 * comparison, but a capture-time check must use the tolerance of the Java test
 * it is reproducing:
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
   * Asserts a reference-data row count. A mismatch means the resource changed
   * under the port, which must abort rather than silently reshape the manifest.
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

/** Rejects any output path that escapes the seven declared destinations. */
void guardOutputPath(String relativePath) {
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
}

/**
 * Documents are accumulated here and flushed only after every check has
 * passed, so a failed run cannot leave a partially written fixture behind.
 * LinkedHashMap, so they are written in a fixed order.
 */
Map<String, String> PENDING_DOCUMENTS = new LinkedHashMap<>();

void stageDocument(String relativePath, Jn root) {
  guardOutputPath(relativePath);
  PENDING_DOCUMENTS.put(relativePath, jsonDocument(root));
}

/** Stages a row-array document in the one-row-per-line form. */
void stageRowsPerLineDocument(String relativePath, JArray rows) {
  guardOutputPath(relativePath);
  PENDING_DOCUMENTS.put(relativePath, jsonRowsPerLineDocument(rows));
}

void flushDocuments() throws Exception {
  for (Map.Entry<String, String> entry : PENDING_DOCUMENTS.entrySet()) {
    String relativePath = entry.getKey();
    guardOutputPath(relativePath);
    Path target = Paths.get(OUT_ROOT).resolve(relativePath);
    Files.createDirectories(target.getParent());
    Files.writeString(target, entry.getValue(), StandardCharsets.UTF_8);
    System.out.println("wrote " + target + "  (" + entry.getValue().length() + " chars)");
  }
}

/* ===========================================================================
 * SECTION 4 - LOCAL EQUIVALENTS OF THE TEST-ONLY JAVA HELPERS
 *
 * The Java tests reach `TestHelper.date` / `TestHelper.list` and their own
 * package-private `Info` class. None of those are available here:
 * `com.opengamma.strata.collect.TestHelper` lives in a test jar, and a `.jsh`
 * script runs in the UNNAMED PACKAGE so it cannot touch package-private types
 * at all. The replacements below are behaviourally identical, which is what
 * lets the Java data tables in Section 5 be used verbatim.
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
 * The SIMPLE_30_360 sentinel (Trap 2).
 *
 * DayCountTest declares `SIMPLE_30_360 = Double.NaN` and
 * `SIMPLE_30_360DAYS = 0` as MARKERS, not as expected values, and its
 * consumers resolve them through calc360 / calc360Days. Transcribing the rows
 * literally would record NaN and 0 as the expectations, which is exactly the
 * silent corruption this script exists to avoid.
 *
 * Two different comparisons are reproduced, because Java uses two:
 *   * `value == SIMPLE_30_360` compares boxed Double REFERENCES. Declaring the
 *     constant once means every table literal shares that one autoboxed
 *     instance, so reference identity works here just as it does in the Java
 *     test (verified: it matches 59 of the 201 data_yearFraction rows).
 *   * `value == SIMPLE_30_360DAYS` compares int VALUES, so any row whose
 *     expected day count is literally 0 is also routed through calc360Days.
 *     That is Java's own behaviour and it is reproduced, not "fixed".
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
 * A local implementation of the PUBLIC nested interface DayCount.ScheduleInfo
 * carrying the semantics of DayCountTest's package-private `Info` stub rather
 * than those of the interface (Trap 7).
 *
 * The distinction matters. The interface's own defaults THROW
 * UnsupportedOperationException for getStartDate / getEndDate /
 * getPeriodEndDate / getFrequency, and default isEndOfMonthConvention to true.
 * The test stub instead returns NULL from every accessor and returns its fixed
 * `periodEnd` from getPeriodEndDate(date) while IGNORING the argument. That
 * stub behaviour is what the fixture's nullable `scheduleInfo` object with a
 * single fixed `periodEnd` describes, and `null` in the fixture means "the
 * Java default", which is `None` on the Scala side.
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
 */
Jn jScheduleInfo(Info info) {
  if (info == null) {
    return jNull();
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
 * Java's Schedule.getPeriodEndDate(d) returns the end date of the first period
 * that contains d, comparing against the adjusted start and end dates with the
 * start included and the end excluded; for the contiguous periods of a
 * schedule that is the first boundary strictly after d when
 * start <= d < end, and it throws otherwise - which the Scala port renders as
 * None.
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

/* =========================================================================
 * SECTION 5 - THE JAVA TEST DATA TABLES, VERBATIM
 *
 * Every table below is a byte-for-byte copy of the corresponding
 * `public static Object[][] data_*()` body from the Java test named in its
 * banner. They are copied rather than re-typed for one reason: a copy cannot
 * introduce a transcription error, and every row stays traceable to its
 * source line. The bodies are valid Java array initialisers, so they compile
 * here unchanged once the imports, the constants and the date/list helpers
 * above are in place.
 *
 * The row counts asserted in Section 6 are the counts measured in the Java
 * sources, so a table that is edited upstream without this script being
 * updated aborts the capture instead of silently shrinking a fixture.
 * =========================================================================
 */

/* --- PeriodicScheduleTest constants (verbatim, `private static final` ->
 * `final` only, so every initialiser expression is untouched) ---
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

        //-------------------------------------------------------
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, (4d / 365d + 58d / 366d)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, (4d / 365d + 59d / 366d)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, (4d / 365d + 60d / 366d)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, (4d / 365d + 58d / 366d + 4)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, (4d / 365d + 59d / 366d + 4)},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, (4d / 365d + 60d / 366d + 4)},
        {ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 29, 29d / 366d},
        {ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 28, 28d / 366d},
        {ACT_ACT_ISDA, 2012, 3, 1, 2012, 3, 28, 27d / 366d},

        //-------------------------------------------------------
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, (63d / 365d)},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, (64d / 366d)},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, (62d / 365d) + 4},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, (63d / 365d) + 4},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, (64d / 366d) + 4},
        {ACT_ACT_AFB, 2012, 2, 28, 2012, 3, 28, 29d / 366d},
        {ACT_ACT_AFB, 2012, 2, 29, 2012, 3, 28, 28d / 366d},
        {ACT_ACT_AFB, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        //-------------------------------------------------------
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

        //-------------------------------------------------------
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, (63d / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, (64d / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 366d)},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 366d)},
        {ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29d / 366d},
        {ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28d / 365d},
        {ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        //-------------------------------------------------------
        {ACT_360, 2011, 12, 28, 2012, 2, 28, (62d / 360d)},
        {ACT_360, 2011, 12, 28, 2012, 2, 29, (63d / 360d)},
        {ACT_360, 2011, 12, 28, 2012, 3, 1, (64d / 360d)},
        {ACT_360, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 360d)},
        {ACT_360, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 360d)},
        {ACT_360, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 360d)},
        {ACT_360, 2012, 2, 28, 2012, 3, 28, 29d / 360d},
        {ACT_360, 2012, 2, 29, 2012, 3, 28, 28d / 360d},
        {ACT_360, 2012, 3, 1, 2012, 3, 28, 27d / 360d},

        //-------------------------------------------------------
        {ACT_364, 2011, 12, 28, 2012, 2, 28, (62d / 364d)},
        {ACT_364, 2011, 12, 28, 2012, 2, 29, (63d / 364d)},
        {ACT_364, 2011, 12, 28, 2012, 3, 1, (64d / 364d)},
        {ACT_364, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 364d)},
        {ACT_364, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 364d)},
        {ACT_364, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 364d)},
        {ACT_364, 2012, 2, 28, 2012, 3, 28, 29d / 364d},
        {ACT_364, 2012, 2, 29, 2012, 3, 28, 28d / 364d},
        {ACT_364, 2012, 3, 1, 2012, 3, 28, 27d / 364d},

        //-------------------------------------------------------
        {ACT_365F, 2011, 12, 28, 2012, 2, 28, (62d / 365d)},
        {ACT_365F, 2011, 12, 28, 2012, 2, 29, (63d / 365d)},
        {ACT_365F, 2011, 12, 28, 2012, 3, 1, (64d / 365d)},
        {ACT_365F, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365d)},
        {ACT_365F, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365d)},
        {ACT_365F, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365d)},
        {ACT_365F, 2012, 2, 28, 2012, 3, 28, 29d / 365d},
        {ACT_365F, 2012, 2, 29, 2012, 3, 28, 28d / 365d},
        {ACT_365F, 2012, 3, 1, 2012, 3, 28, 27d / 365d},

        //-------------------------------------------------------
        {ACT_365_25, 2011, 12, 28, 2012, 2, 28, (62d / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2012, 2, 29, (63d / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2012, 3, 1, (64d / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365.25d)},
        {ACT_365_25, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365.25d)},
        {ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29d / 365.25d},
        {ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28d / 365.25d},
        {ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27d / 365.25d},

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, 1523},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, 1524},
        {ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, 1525},

        //-------------------------------------------------------
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, 1523},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, 1524},
        {ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, 1525},

        //-------------------------------------------------------
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_ACT_YEAR, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 28, 1523},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 29, 1524},
        {ACT_ACT_YEAR, 2011, 12, 28, 2016, 3, 1, 1525},

        //-------------------------------------------------------
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27},

        //-------------------------------------------------------
        {ACT_360, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_360, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_360, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_360, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_360, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_360, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},

        //-------------------------------------------------------
        {ACT_364, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_364, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_364, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_364, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_364, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_364, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_364, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_364, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_364, 2012, 3, 1, 2012, 3, 28, 27},

        //-------------------------------------------------------
        {ACT_365F, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_365F, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_365F, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_365F, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_365F, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_365F, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_365F, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_365F, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_365F, 2012, 3, 1, 2012, 3, 28, 27},

        //-------------------------------------------------------
        {ACT_365_25, 2011, 12, 28, 2012, 2, 28, 62},
        {ACT_365_25, 2011, 12, 28, 2012, 2, 29, 63},
        {ACT_365_25, 2011, 12, 28, 2012, 3, 1, 64},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365},
        {ACT_365_25, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365},
        {ACT_365_25, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365},
        {ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29},
        {ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28},
        {ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27},

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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

        //-------------------------------------------------------
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
        // IMM with stupid short period still works
        {date(2014, 9, 17), date(2014, 10, 15), Frequency.ofDays(2), STUB_NONE, IMM, BDA, null, null, null,
            list(date(2014, 9, 17), date(2014, 10, 15)),
            list(date(2014, 9, 17), date(2014, 10, 15)), IMM},
        {date(2014, 9, 17), date(2014, 10, 1), Frequency.ofDays(2), STUB_NONE, IMM, BDA, null, null, null,
            list(date(2014, 9, 17), date(2014, 10, 1)),
            list(date(2014, 9, 17), date(2014, 10, 1)), IMM},

        //IMM with adjusted start dates and various conventions
        //MF, no stub 
        {date(2018, 3, 22), date(2020, 3, 18), P6M, STUB_NONE, IMM, BDA_JPY_MF, null, null, BDA_NONE,
            list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20), date(2019, 9, 18), date(2020, 3, 18)),
            list(date(2018, 3, 22), date(2018, 9, 19), date(2019, 3, 20), date(2019, 9, 18), date(2020, 3, 18)), IMM},
        //Preceding, no stub
        {date(2018, 3, 20), date(2019, 3, 20), P6M, STUB_NONE, IMM, BDA_JPY_P, null, null, BDA_NONE,
            list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20)),
            list(date(2018, 3, 20), date(2018, 9, 19), date(2019, 3, 20)), IMM},
        //MF, null stub
        {date(2018, 3, 22), date(2019, 3, 20), P6M, null, IMM, BDA_JPY_MF, null, null, BDA_NONE,
            list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20)),
            list(date(2018, 3, 22), date(2018, 9, 19), date(2019, 3, 20)), IMM},
        //Explicit long front stub with (adjusted) first regular start date
        {date(2017, 9, 2), date(2018, 9, 19), P6M, LONG_INITIAL, IMM, BDA_JPY_MF, date(2018, 3, 22), null, BDA_NONE,
            list(date(2017, 9, 2), date(2018, 3, 21), date(2018, 9, 19)),
            list(date(2017, 9, 2), date(2018, 3, 22), date(2018, 9, 19)), IMM},
        //Implicit short front stub with (adjusted) first regular start date
        {date(2018, 1, 2), date(2018, 9, 19), P6M, null, IMM, BDA_JPY_MF, date(2018, 3, 22), null, BDA_NONE,
            list(date(2018, 1, 2), date(2018, 3, 21), date(2018, 9, 19)),
            list(date(2018, 1, 2), date(2018, 3, 22), date(2018, 9, 19)), IMM},
        //Implicit back stub with (adjusted) last regular start date
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
        // original schedule had last regular date and uneccessary final stub convention
        {JUN_04, JUN_17, AUG_04, P1M, SHORT_FINAL, null, BDA, null, JUL_17, null,
            list(JUN_04, JUN_17, JUL_17, AUG_04), SMART_INITIAL, JUL_17, null},
        // original schedule was final, but resulted in Term schedule, new schedule retains the stub convention
        {JUN_04, JUL_17, AUG_17, P1M, SHORT_FINAL, null, BDA, null, null, null,
            list(JUN_04, JUL_04, AUG_04, AUG_17), SHORT_FINAL, null, null},
        // cannot set start after end
        {SEP_04, JUN_17, AUG_17, P1M, null, DAY_17, BDA, JUN_17, AUG_17, BDA_JPY_P, null, null, null, null},
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

  private Eval(double value, String error) {
    this.value = value;
    this.error = error;
  }

  static Eval of(double value) {
    return new Eval(value, null);
  }

  static Eval failed(Throwable thrown) {
    return new Eval(Double.NaN, errorMessage(thrown));
  }

  boolean isError() {
    return error != null;
  }
}

Eval evalYearFraction(DayCount dayCount, LocalDate start, LocalDate end, DayCount.ScheduleInfo info) {
  try {
    return Eval.of(info == null ? dayCount.yearFraction(start, end)
        : dayCount.yearFraction(start, end, info));
  } catch (Throwable thrown) {
    return Eval.failed(thrown);
  }
}

Eval evalRelativeYearFraction(DayCount dayCount, LocalDate start, LocalDate end,
    DayCount.ScheduleInfo info) {
  try {
    return Eval.of(info == null ? dayCount.relativeYearFraction(start, end)
        : dayCount.relativeYearFraction(start, end, info));
  } catch (Throwable thrown) {
    return Eval.failed(thrown);
  }
}

Eval evalDays(DayCount dayCount, LocalDate start, LocalDate end) {
  try {
    return Eval.of(dayCount.days(start, end));
  } catch (Throwable thrown) {
    return Eval.failed(thrown);
  }
}

/** The skeleton every daycount row shares. */
JObject dayCountRow(String source, DayCount dayCount, LocalDate start, LocalDate end, Jn scheduleInfo) {
  return new JObject()
      .set("source", jStr(source))
      .set("dayCount", jName(dayCount))
      .set("start", jDate(start))
      .set("end", jDate(end))
      .set("scheduleInfo", scheduleInfo);
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

/* ===========================================================================
 * SECTION 7 - FIXTURE 1 OF 6: daycount-baseline.json
 * ===========================================================================
 */

String FX_DAYCOUNT = "daycount";

/** The 21 standard day counts, in DayCounts declaration order. */
List<DayCount> standardDayCounts() {
  // Reflection over the public constants holder gives a provably complete
  // list. StandardDayCounts.values(), which the Java test uses, is
  // package-private and therefore unreachable from a .jsh script (Trap 1).
  List<DayCount> result = new ArrayList<>();
  for (Field field : DayCounts.class.getDeclaredFields()) {
    if (Modifier.isPublic(field.getModifiers()) && Modifier.isStatic(field.getModifiers())
        && DayCount.class.equals(field.getType())) {
      try {
        result.add((DayCount) field.get(null));
      } catch (IllegalAccessException ex) {
        throw new IllegalStateException("Cannot read DayCounts." + field.getName(), ex);
      }
    }
  }
  return result;
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
    JObject row = dayCountRow("DayCountTest.data_yearFraction", dayCount, start, end, jNull());
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
    JObject row = dayCountRow("DayCountTest.data_days", dayCount, start, end, jNull());
    Eval days = evalDays(dayCount, start, end);
    if (days.isError()) {
      row.set("error", jStr(days.error));
      CHECK.countErrorRow(FX_DAYCOUNT);
      CHECK.fail(FX_DAYCOUNT, rowId, "expected=" + expected + " but Java threw " + days.error);
    } else {
      row.set("days", jInt((long) days.value));
      CHECK.checkInt(FX_DAYCOUNT, rowId, expected, (long) days.value);
    }
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
      JObject row = dayCountRow(source, dayCount, start, end, jScheduleInfo(info));
      setYearFraction(FX_DAYCOUNT, row, rowId, evalYearFraction(dayCount, start, end, info),
          Double.valueOf(expected));
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
    Info infoNotMaturity = new Info(false);
    Info infoMaturity = new Info(null, end, null, false, P3M);
    JObject rowNotMaturity = dayCountRow("DayCountTest.test_yearFraction_30E360ISDA_notMaturity",
        THIRTY_E_360_ISDA, start, end, jScheduleInfo(infoNotMaturity));
    setYearFraction(FX_DAYCOUNT, rowNotMaturity,
        "30E360ISDA notMaturity " + start + ".." + end,
        evalYearFraction(THIRTY_E_360_ISDA, start, end, infoNotMaturity),
        Double.valueOf(expectedNotMaturity));
    rows.add(rowNotMaturity);
    CHECK.countRow(FX_DAYCOUNT);
    JObject rowMaturity = dayCountRow("DayCountTest.test_yearFraction_30E360ISDA_maturity",
        THIRTY_E_360_ISDA, start, end, jScheduleInfo(infoMaturity));
    setYearFraction(FX_DAYCOUNT, rowMaturity,
        "30E360ISDA maturity " + start + ".." + end,
        evalYearFraction(THIRTY_E_360_ISDA, start, end, infoMaturity),
        Double.valueOf(expectedMaturity));
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
    JObject row = dayCountRow("DayCountTest.data_ACTACTAFB", ACT_ACT_AFB, start, end, jNull());
    setYearFraction(FX_DAYCOUNT, row, "ACTACTAFB " + start + ".." + end,
        evalYearFraction(ACT_ACT_AFB, start, end, null), Double.valueOf(expected));
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
    JObject row = dayCountRow("DayCountTest.data_ACT365L", ACT_365L, start, end, jScheduleInfo(info));
    setYearFraction(FX_DAYCOUNT, row,
        "ACT365L " + start + ".." + end + " " + frequency + " pe=" + periodEnd,
        evalYearFraction(ACT_365L, start, end, info), Double.valueOf(expected));
    rows.add(row);
    CHECK.countRow(FX_DAYCOUNT);
  }
}

/**
 * Adds one Info-based case, transcribed from a named `@Test` method of
 * DayCountTest. A null `info` selects the two-argument overload, which is what
 * the ACT_ACT_ISDA and ACT_ACT_AFB assertions of the ISDA test cases use.
 */
void addInfoCase(JArray rows, String source, DayCount dayCount, LocalDate start, LocalDate end,
    Info info, double expected) {
  JObject row = dayCountRow(source, dayCount, start, end,
      info == null ? jNull() : jScheduleInfo(info));
  setYearFraction(FX_DAYCOUNT, row, source + " " + dayCount.getName() + " " + start + ".." + end,
      evalYearFraction(dayCount, start, end, info), Double.valueOf(expected));
  rows.add(row);
  CHECK.countRow(FX_DAYCOUNT);
}

/**
 * The Info-based ICMA and official-ISDA cases, transcribed one row per
 * assertion from DayCountTest lines 917-1112. The expected values are written
 * as the same arithmetic expressions the Java test uses, so they stay readable
 * as the documents they come from and are checked for exact equality.
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
          JObject row = dayCountRow("DayCountTest.test_actActYearVsIcma", ACT_ACT_ICMA, start, end,
              jScheduleInfo(info));
          row.set("yearFraction", jDbl(icma.value));
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
 * Day counts that require schedule information the default cannot supply throw
 * here, exactly as they do in Java; those rows are emitted with `error` and
 * are expectations in their own right - the Scala port reproduces them as
 * documented ArgCheck throws.
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
  for (DayCount dayCount : gridDayCounts()) {
    for (List<LocalDate> dates : series) {
      for (int i = 0; i + 1 < dates.size(); i++) {
        LocalDate start = dates.get(i);
        LocalDate end = dates.get(i + 1);
        JObject row = dayCountRow("grid.simpleInfo", dayCount, start, end, jNull());
        Eval yf = evalYearFraction(dayCount, start, end, null);
        // Capture-only: generated inputs have no Java test constant.
        setYearFraction(FX_DAYCOUNT, row, "grid " + dayCount.getName() + " " + start, yf, null);
        Eval days = evalDays(dayCount, start, end);
        if (!days.isError()) {
          row.set("days", jInt((long) days.value));
        }
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
          JObject row = dayCountRow("grid.schedule." + frequency + "." + shapes[s], dayCount,
              start, end, info);
          Eval yf = evalYearFraction(dayCount, start, end, schedule);
          setYearFraction(FX_DAYCOUNT, row,
              "gridSchedule " + dayCount.getName() + " " + frequency + " " + start, yf, null);
          rows.add(row);
          CHECK.countRow(FX_DAYCOUNT);
          CHECK.countCaptureOnly(FX_DAYCOUNT);
        }
      }
    }
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
  emitGeneratedDateGrid(rows);
  emitGeneratedScheduleGrid(rows);
  return rows;
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
 */
void addScheduleRow(JArray rows, String source, PeriodicSchedule definition, String rowId,
    List<LocalDate> expectedUnadjusted, List<LocalDate> expectedAdjusted,
    RollConvention expectedRoll, boolean expectFailure) {
  JObject row = jPeriodicScheduleInputs(definition);
  row.set("source", jStr(source));
  Schedule schedule = null;
  String error = null;
  try {
    schedule = definition.createSchedule(ReferenceData.standard());
  } catch (Throwable thrown) {
    error = errorMessage(thrown);
  }
  if (error != null) {
    row.set("error", jStr(error));
    CHECK.countErrorRow(FX_SCHEDULE);
    if (!expectFailure && expectedUnadjusted != null) {
      CHECK.fail(FX_SCHEDULE, rowId, "expected a schedule but Java threw " + error);
    }
    rows.add(row);
    CHECK.countRow(FX_SCHEDULE);
    return;
  }
  if (expectFailure) {
    CHECK.fail(FX_SCHEDULE, rowId, "expected Java to throw but a schedule was produced");
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
        expectedUnadjusted, expectedAdjusted, expectedRoll, false);
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
    // The row records the BASE definition plus the replacement start date, so
    // the Scala side reproduces the whole operation: build the base, apply
    // replaceStartDate, then create the unadjusted dates.
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
    String error = null;
    try {
      replaced = base.replaceStartDate(replaceStart);
      unadjusted = new ArrayList<>(replaced.createUnadjustedDates());
      schedule = replaced.createSchedule(ReferenceData.standard());
    } catch (Throwable thrown) {
      error = errorMessage(thrown);
    }
    if (error != null) {
      row.set("error", jStr(error));
      CHECK.countErrorRow(FX_SCHEDULE);
      if (expectedUnadjusted != null) {
        CHECK.fail(FX_SCHEDULE, rowId, "expected dates but Java threw " + error);
      }
    } else {
      // `replacedDefinition` is the post-replacement definition itself, whose
      // eleven fields are exactly what the Java test asserts one by one
      // (PeriodicScheduleTest.java:1002-1013: the override start date and the
      // first regular start date are cleared, the start date becomes the
      // replacement, and the start-date adjustment becomes BDA_NONE). Carrying
      // it lets the Scala side assert the whole `replaceStartDate` operation
      // rather than only the dates it happens to produce.
      row.set("replacedDefinition", jPeriodicScheduleInputs(replaced));
      // `createUnadjustedDates()` on the replaced definition is what the Java
      // table asserts, and it is NOT always the unadjusted view of the resolved
      // Schedule - the two genuinely differ on the LONG_INITIAL / DAY_17 row
      // (data_replace row 2, MAY_19 over JUN_17..AUG_17), where
      // createUnadjustedDates() gives [2014-05-19, 2014-07-17, 2014-08-17] - a
      // long initial stub from the replacement start - while createSchedule()
      // rolls the start onto the 17th and gives
      // [2014-05-17, 2014-06-17, 2014-07-17, 2014-08-17]. Both are real Java
      // answers for the same definition and the port has to reproduce both, so
      // the row carries each under its own key instead of asserting that they
      // agree.
      row.set("replacedUnadjustedDates", jDates(unadjusted));
      // Then the same full expectation set every resolved row carries, so a
      // replace row is shaped like the rest of the document rather than
      // carrying only the one column the Java test happens to assert.
      setResolvedScheduleExpectations(row, schedule);
      row.set("expectedStubConvention", jName(expectedStub));
      row.set("expectedLastRegularEndDate", jDate(expectedLastRegular));
      row.set("expectedRollConvention", jName(expectedRoll));
      if (expectedUnadjusted == null) {
        CHECK.fail(FX_SCHEDULE, rowId, "expected Java to throw but dates were produced");
      } else {
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
 * resolved against ReferenceData.standard(). Many combinations are mutually
 * inconsistent and produce `error` rows; that is the point - the Scala port
 * must reject exactly the same ones.
 */
void emitScheduleCombinations(JArray rows) {
  Frequency[] frequencies = {P1M, P3M, P6M, P12M};
  StubConvention[] stubs = StubConvention.values();
  // IMMCAD, IMMAUD and TBILL are mandatory here, and not for variety: in Java
  // these three StandardRollConventions members capture built-in holiday
  // calendars at class-initialisation time through ReferenceData.standard()
  // (StandardRollConventions.java:60-63,74-75,103-104,133-134 - IMMCAD holds
  // GBLO and CATO.combinedWith(CAMO), IMMAUD holds AUSY, TBILL holds USNY),
  // whereas the Scala port binds the StandardHolidayCalendars constants
  // directly. These rows are the evidence that the substitution is
  // behaviour-preserving. SFE (second Friday) and IMMNZD use no calendar at
  // all, so they are the control group for that same comparison.
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
              null, false);
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
    boolean expectFailure) {
  JObject row = jPeriodicScheduleRawInputs(startDate, endDate, frequency, businessDayAdjustment,
      startDateBusinessDayAdjustment, endDateBusinessDayAdjustment, stubConvention, rollConvention,
      firstRegularStartDate, lastRegularEndDate, overrideStartDate);
  row.set("source", jStr(source));
  Schedule schedule = null;
  String error = null;
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
    error = errorMessage(thrown);
  }
  if (error != null) {
    row.set("error", jStr(error));
    CHECK.countErrorRow(FX_SCHEDULE);
    if (!expectFailure) {
      CHECK.fail(FX_SCHEDULE, rowId, "expected a schedule but Java threw " + error);
    }
    rows.add(row);
    CHECK.countRow(FX_SCHEDULE);
    return;
  }
  if (expectFailure) {
    CHECK.fail(FX_SCHEDULE, rowId, "expected Java to throw but a schedule was produced");
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
 * `source`, and every expectation is that test's own literal - nothing is
 * composed by hand. Three inputs of the port's validated smart constructor are
 * exercised only here: `overrideStartDate` (which the tables never set),
 * `endDateBusinessDayAdjustment` (likewise), and the five builder-time
 * validation branches, which resolve to `Failure.Invalid` in the port and so
 * need a captured message to compare against.
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
      false);

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
      false);

  // test_override_fallbackWhenStartDateMismatch (:851) - an adjusted override
  // start date that does not line up with the start date.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.test_override_fallbackWhenStartDateMismatch",
      "override fallback", jul04, sep17, P1M, bda, null, null, null, DAY_17, null, null,
      AdjustableDate.of(jun17, followingSatSun),
      List.of(jun17, jul17, aug17, sep17), List.of(jun17, jul17, aug18, sep17), false);

  // test_override_fallbackWhenStartDateMismatchEndStub (:880) - the same, with
  // an explicit last regular end date producing a final stub.
  addScheduleCaseRow(rows,
      "PeriodicScheduleTest.test_override_fallbackWhenStartDateMismatchEndStub",
      "override fallback end stub", jul04, sep04, P1M, bda, null, null, null, DAY_17, null, aug17,
      AdjustableDate.of(jun17, followingSatSun),
      List.of(jun17, jul17, aug17, sep04), List.of(jun17, jul17, aug18, sep04), false);

  // coverage_builder (:1447) - the one Java definition with all eleven fields
  // populated at once, so the fixture carries a row in which no input is null.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.coverage_builder", "coverage builder",
      jul17, sep17, P2M, bdaNone, bdaNone, bdaNone, StubConvention.NONE, EOM, jul17, sep17,
      AdjustableDate.of(jul11, bdaNone), null, null, false);

  // The builder-time validation branches. Each is a definition Java rejects,
  // and the captured message is the whole contract tying it to the port's
  // Failure.Invalid: PeriodicSchedule.java:361 (start not before end), :363
  // (override start date not before end date), :367 and :378 (first regular
  // start date outside the schedule), :370 (last regular end date before the
  // first regular start date).
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.startAfterEnd", "start after end",
      sep17, jun04, P1M, bda, null, null, null, null, null, null, null, null, null, true);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.startEqualsEnd", "start equals end",
      jun04, jun04, P1M, bda, null, null, null, null, null, null, null, null, null, true);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.overrideStartAfterEnd",
      "override start after end", jun04, sep17, P1M, bda, null, null, null, null, null, null,
      AdjustableDate.of(oct17), null, null, true);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.firstRegularAfterEnd",
      "first regular after end", jun04, sep17, P1M, bda, null, null, null, null, oct17, null, null,
      null, null, true);
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.firstRegularBeforeStart",
      "first regular before start", jul17, sep17, P1M, bda, null, null, null, null, jun04, null,
      null, null, null, true);
  // createDates(JUN_04, SEP_17, SEP_05, SEP_04) at :263 - last regular end date
  // before the first regular start date.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.lastRegularBeforeFirstRegular",
      "last regular before first regular", jun04, sep17, P1M, bda, null, null, null, null, sep05,
      sep04, null, null, null, true);
  // The first-regular vs override-start-date conflict at :265-272.
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.firstRegularWithOverride",
      "first regular with override", jun04, sep17, P1M, bda, null, null, null, null, jul17, null,
      AdjustableDate.of(aug04), null, null, true);
  // Term frequency with an explicit regular date: a stub convention that
  // contradicts the explicit dates, rejected at resolution rather than at
  // build time with "Explicit stubs must not be specified when using 'Term'
  // frequency" (PeriodicSchedule.java:591).
  addScheduleCaseRow(rows, "PeriodicScheduleTest.invalid.termWithExplicitStub",
      "term with explicit stub", jun04, sep17, TERM, bda, null, null, null, null, jul17, null,
      null, null, null, true);
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
 * An expected query result: a null expectation with expectFailure false means
 * capture-only, while expectFailure true asserts that Java rejects the query -
 * which is what the `assertThatIllegalArgumentException` cases of
 * FxRateTest.test_fxRate_forPair state, so the capture states them too rather
 * than recording whatever happens.
 */
class FxQuery {
  final Currency base;
  final Currency counter;
  final Double expected;
  final double tolerance;
  final String toleranceName;
  final boolean expectFailure;

  FxQuery(Currency base, Currency counter, Double expected, double tolerance, String toleranceName) {
    this(base, counter, expected, tolerance, toleranceName, false);
  }

  FxQuery(Currency base, Currency counter, Double expected, double tolerance, String toleranceName,
      boolean expectFailure) {
    this.base = base;
    this.counter = counter;
    this.expected = expected;
    this.tolerance = tolerance;
    this.toleranceName = toleranceName;
    this.expectFailure = expectFailure;
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

  /** A query the Java implementation must reject, captured as an `error`. */
  static FxQuery failing(Currency base, Currency counter) {
    return new FxQuery(base, counter, null, 0d, "expected-failure", true);
  }
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
  final boolean expectFailure;

  CrossQuery(FxRate rate1, FxRate rate2, FxRate expected, boolean expectFailure) {
    this.rate1 = rate1;
    this.rate2 = rate2;
    this.expected = expected;
    this.expectFailure = expectFailure;
  }

  static CrossQuery of(FxRate rate1, FxRate rate2, FxRate expected) {
    return new CrossQuery(rate1, rate2, expected, false);
  }

  static CrossQuery failing(FxRate rate1, FxRate rate2) {
    return new CrossQuery(rate1, rate2, null, true);
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

/** Emits one FX scenario row. Every row carries the same nine keys. */
void addFxScenario(JArray rows, String id, String source, List<RateEntry> definition,
    List<FxQuery> queries, List<ConversionQuery> conversions, List<MultiQuery> multi,
    List<CrossQuery> crosses, List<List<RateEntry>> merges, boolean captureOnly) {
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
    buildError = errorMessage(thrown);
  }
  if (buildError != null) {
    // A definition the builder rejects has no matrix to query, so the five
    // query sections are emitted empty and `matrixState` is null: the row keeps
    // the shape of every other row and adds `error` to it.
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
      } else if (query.expectFailure) {
        CHECK.fail(FX_FX, queryId, "expected Java to throw but produced " + actual);
      }
    } catch (Throwable thrown) {
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
      if (query.expected != null) {
        CHECK.fail(FX_FX, queryId,
            "expected=" + query.expected + " but Java threw " + errorMessage(thrown));
      } else if (query.expectFailure) {
        // The failure itself is the assertion the Java test makes.
        CHECK.checkTrue(FX_FX, queryId, true, "expected failure observed");
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
      } else if (cross.expectFailure) {
        CHECK.fail(FX_FX, crossId, "expected Java to throw but produced " + actual);
      }
    } catch (Throwable thrown) {
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
      if (cross.expected != null) {
        CHECK.fail(FX_FX, crossId,
            "expected=" + cross.expected + " but Java threw " + errorMessage(thrown));
      } else if (cross.expectFailure) {
        // The failure itself is the assertion the Java test makes.
        CHECK.checkTrue(FX_FX, crossId, true, "expected failure observed");
      }
    }
    crossArray.add(entry);
  }
  row.set("crosses", crossArray);

  JArray mergeArray = new JArray();
  for (List<RateEntry> other : merges) {
    JObject entry = new JObject().set("other", jRateEntries(other));
    try {
      FxMatrix merged = matrix.merge(buildMatrix(other));
      entry.set("merged", jMatrixState(merged));
    } catch (Throwable thrown) {
      entry.set("error", jStr(errorMessage(thrown)));
      CHECK.countErrorRow(FX_FX);
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

List<List<RateEntry>> noMerges() {
  return new ArrayList<>();
}

/*
 * Fixed-arity helpers rather than a generic varargs method: `merges(List<RateEntry>...)`
 * is a parameterized vararg and javac reports possible heap pollution for it,
 * which would put a warning on stdout.
 */
List<List<RateEntry>> merges(List<RateEntry> first) {
  List<List<RateEntry>> result = new ArrayList<>();
  result.add(first);
  return result;
}

List<List<RateEntry>> merges(List<RateEntry> first, List<RateEntry> second) {
  List<List<RateEntry>> result = merges(first);
  result.add(second);
  return result;
}

List<List<RateEntry>> merges(List<RateEntry> first, List<RateEntry> second,
    List<RateEntry> third) {
  List<List<RateEntry>> result = merges(first, second);
  result.add(third);
  return result;
}

List<List<RateEntry>> merges(List<RateEntry> first, List<RateEntry> second, List<RateEntry> third,
    List<RateEntry> fourth) {
  List<List<RateEntry>> result = merges(first, second, third);
  result.add(fourth);
  return result;
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
  // the matrix) and an empty other matrix. The last one fails as well, and
  // deliberately so: Java's merge looks for a currency common to both
  // matrices, and an empty matrix has none, so merging with it is an error
  // rather than a no-op. That is a real asymmetry of the Java API and the port
  // has to reproduce it.
  List<RateEntry> mergeBase =
      rateEntries(Currency.GBP, Currency.USD, 1.6, Currency.EUR, Currency.USD, 1.4);
  List<RateEntry> disjoint =
      rateEntries(Currency.CHF, Currency.AUD, 1.2, Currency.SEK, Currency.AUD, 0.16);
  List<RateEntry> duplicate = rateEntries(Currency.GBP, Currency.USD, 1.7,
      Currency.EUR, Currency.USD, 1.5, Currency.EUR, Currency.CHF, 1.3);
  List<RateEntry> additional =
      rateEntries(Currency.EUR, Currency.CHF, 1.2, Currency.CHF, Currency.AUD, 1.2);
  addFxScenario(rows, "merge-cases", "FxMatrixTest.merge", mergeBase, noQueries(),
      noConversions(), noMulti(), noCrosses(),
      merges(disjoint, duplicate, additional, rateEntries()), false);

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
 * subject type. Each result entry names its `op`, so the Scala side reads the
 * operation rather than inferring it from position.
 *
 * TWO OPERATIONS ARE COMPOSED, NOT INVENTED. The row schema names
 * `multipliedBy` and `mapAmounts` for CurrencyAmountArray and
 * MultiCurrencyAmountArray, but the JAVA types have neither method - they are
 * added by the Scala port. Rather than invent a Java API, those expectations
 * are composed from two real public calls, `of(currency, getValues().
 * multipliedBy(s))` and `of(currency, getValues().map(f))`, and each such
 * entry is marked `"composed": true` and carries the function it used in
 * `mapAmountsFn`. The mapping function is x -> x * x, the same one the
 * double-array fixture documents.
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
 * `expectFailure` records what the Java test asserts: true means the failure
 * IS the expectation, false means a failure aborts the capture, and null means
 * capture-only.
 */
interface ResultSupplier {
  Jn get() throws Throwable;
}

void addOperation(JArray results, String fixture, String rowId, JObject entry,
    ResultSupplier supplier, Boolean expectFailure) {
  try {
    Jn value = supplier.get();
    entry.set("result", value);
    if (Boolean.TRUE.equals(expectFailure)) {
      CHECK.fail(fixture, rowId, "expected Java to throw but produced a value");
    }
  } catch (Throwable thrown) {
    entry.set("error", jStr(errorMessage(thrown)));
    CHECK.countErrorRow(fixture);
    if (Boolean.FALSE.equals(expectFailure)) {
      CHECK.fail(fixture, rowId, "unexpected Java failure " + errorMessage(thrown));
    } else if (Boolean.TRUE.equals(expectFailure)) {
      CHECK.checkTrue(fixture, rowId, true, "expected failure observed");
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
      () -> jCurrencyAmount(gbp100.plus(gbp25)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.minus same currency",
      opEntry("minus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(gbp25)),
      () -> jCurrencyAmount(gbp100.minus(gbp25)), Boolean.FALSE);
  // CurrencyAmountTest: adding different currencies throws.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.plus different currency",
      opEntry("plus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(usd50)),
      () -> jCurrencyAmount(gbp100.plus(usd50)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.minus different currency",
      opEntry("minus").set("left", jCurrencyAmount(gbp100)).set("right", jCurrencyAmount(usd50)),
      () -> jCurrencyAmount(gbp100.minus(usd50)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.multipliedBy",
      opEntry("multipliedBy").set("left", jCurrencyAmount(gbp100))
          .set("scalar", jDbl(MATH_IN.scalar(3.5))),
      () -> jCurrencyAmount(gbp100.multipliedBy(3.5)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.negated",
      opEntry("negated").set("left", jCurrencyAmount(gbp100)),
      () -> jCurrencyAmount(gbp100.negated()), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.positive",
      opEntry("positive").set("left", jCurrencyAmount(gbp100.negated())),
      () -> jCurrencyAmount(gbp100.negated().positive()), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.negative",
      opEntry("negative").set("left", jCurrencyAmount(gbp100)),
      () -> jCurrencyAmount(gbp100.negative()), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo rate",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.USD)).set("rate", jDbl(1.25)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.USD, 1.25)), Boolean.FALSE);
  // CurrencyAmount.convertedTo to the SAME currency with a rate that is not 1
  // is rejected (the fixed-rate overload compares with a 1e-8 fuzzy equality).
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo same currency rate != 1",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.GBP)).set("rate", jDbl(1.25)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.GBP, 1.25)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo same currency rate 1",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.GBP)).set("rate", jDbl(1.0)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.GBP, 1.0)), Boolean.FALSE);
  // CurrencyAmountTest.test_convertedTo_explicitRate uses 1.5 as its rejected
  // rate, so the literal the Java test names is captured as well as the 1.25
  // above. The fixed-rate overload compares with a 1e-8 fuzzy equality.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmount.convertedTo same currency rate 1.5",
      opEntry("convertedTo").set("left", jCurrencyAmount(gbp100))
          .set("target", jName(Currency.GBP)).set("rate", jDbl(1.5)),
      () -> jCurrencyAmount(gbp100.convertedTo(Currency.GBP, 1.5)), Boolean.TRUE);

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
      () -> jCurrencyAmount(CurrencyAmount.of(Currency.GBP, Double.NaN)), Boolean.TRUE);

  // The normalisation is not confined to the factory: the private constructor
  // adds 0d to every amount, so an ARITHMETIC RESULT of -0.0 is normalised
  // too. 0.0 * -1.0 is -0.0 in IEEE-754 and +0.0 here, and the bit pattern is
  // captured so the port cannot satisfy this row with a sign-blind
  // comparison.
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
          .plus(CurrencyAmount.of(Currency.GBP, Double.NEGATIVE_INFINITY))), Boolean.TRUE);

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
        () -> jCurrencyAmount(value.multipliedBy(1.5)), null);
    CHECK.countCaptureOnly(FX_CURRENCY_MATH);
  }
  // The remaining 19 currencies - the `historic = true` rows of Currency.ini,
  // which getAvailableCurrencies() excludes and the closed Scala Currency set
  // still contains - complete the 74. They carry the LITERAL sweep amount
  // rather than a draw from the shared Random, because a draw here would
  // shift the random values of every fixture captured after this one.
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
        () -> jCurrencyAmount(value.multipliedBy(1.5)), null);
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
          () -> jBigMoney(BigMoney.of(currency, amount)), Boolean.FALSE);
    } else {
      MATH_IN.money(Money.of(currency, amount));
      addOperation(results, FX_CURRENCY_MATH, "Money.of " + currency + " " + amount, entry,
          () -> jMoney(Money.of(currency, amount)), Boolean.FALSE);
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
        () -> jBigMoney(left.plus(right)), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.minus",
        opEntry("minus").set("left", jBigMoney(left)).set("right", jBigMoney(right)),
        () -> jBigMoney(left.minus(right)), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.plus different currency",
        opEntry("plus").set("left", jBigMoney(left)).set("right", jBigMoney(other)),
        () -> jBigMoney(left.plus(other)), Boolean.TRUE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.minus different currency",
        opEntry("minus").set("left", jBigMoney(left)).set("right", jBigMoney(other)),
        () -> jBigMoney(left.minus(other)), Boolean.TRUE);
    // multipliedBy takes a long, not a double.
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.multipliedBy",
        opEntry("multipliedBy").set("left", jBigMoney(left)).set("scalar", jInt(3L)),
        () -> jBigMoney(left.multipliedBy(3L)), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney",
        opEntry("toMoney").set("left", jBigMoney(left)),
        () -> jMoney(left.toMoney()), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney JPY",
        opEntry("toMoney").set("left", jBigMoney(BigMoney.of(Currency.JPY, 123.4567890123456))),
        () -> jMoney(BigMoney.of(Currency.JPY, 123.4567890123456).toMoney()), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.convertedTo",
        opEntry("convertedTo").set("left", jBigMoney(other)).set("target", jName(Currency.USD))
            .set("rate", jDbl(0.65)),
        () -> jBigMoney(other.convertedTo(Currency.USD, java.math.BigDecimal.valueOf(0.65))),
        Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.convertedTo same currency rate != 1",
        opEntry("convertedTo").set("left", jBigMoney(other)).set("target", jName(Currency.AUD))
            .set("rate", jDbl(1.25)),
        () -> jBigMoney(other.convertedTo(Currency.AUD, java.math.BigDecimal.valueOf(1.25))),
        Boolean.TRUE);
    // The same-currency rule has a SUCCEEDING side too: a rate of exactly 1
    // is the no-conversion case and must return the value unchanged.
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.convertedTo same currency rate 1",
        opEntry("convertedTo").set("left", jBigMoney(left)).set("target", jName(Currency.RON))
            .set("rate", jDbl(1.0)),
        () -> jBigMoney(left.convertedTo(Currency.RON, java.math.BigDecimal.ONE)), Boolean.FALSE);

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
          () -> jBigMoney(value), Boolean.FALSE);
    }
    // ... and its narrowing to minor units, which is where the twelve-digit
    // value meets the currency's two.
    addOperation(results, FX_CURRENCY_MATH, "BigMoney.toMoney high precision",
        opEntry("toMoney")
            .set("left", jBigMoney(MATH_IN.bigMoney(BigMoney.of(Currency.AUD, 1.123456789012345d)))),
        () -> jMoney(BigMoney.of(Currency.AUD, 1.123456789012345d).toMoney()), Boolean.FALSE);

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
          () -> jBigMoney(value.roundToScale(scale, mode)), Boolean.FALSE);
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
          () -> jMoney(value.toMoney()), null);
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
        () -> jMoney(left.plus(right)), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "Money.minus",
        opEntry("minus").set("left", jMoney(left)).set("right", jMoney(right)),
        () -> jMoney(left.minus(right)), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "Money.plus different currency",
        opEntry("plus").set("left", jMoney(left)).set("right", jMoney(other)),
        () -> jMoney(left.plus(other)), Boolean.TRUE);
    addOperation(results, FX_CURRENCY_MATH, "Money.minus different currency",
        opEntry("minus").set("left", jMoney(left)).set("right", jMoney(other)),
        () -> jMoney(left.minus(other)), Boolean.TRUE);
    addOperation(results, FX_CURRENCY_MATH, "Money.multipliedBy",
        opEntry("multipliedBy").set("left", jMoney(left)).set("scalar", jInt(3L)),
        () -> jMoney(left.multipliedBy(3L)), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.USD))
            .set("rate", jDbl(0.65)),
        () -> jMoney(other.convertedTo(Currency.USD, java.math.BigDecimal.valueOf(0.65))),
        Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo same currency rate != 1",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.AUD))
            .set("rate", jDbl(1.25)),
        () -> jMoney(other.convertedTo(Currency.AUD, java.math.BigDecimal.valueOf(1.25))),
        Boolean.TRUE);
    // MoneyTest.testConvertedToWithExplicitRateForSameCurrency rejects 1.1
    // with "FX rate must be 1 when no conversion required"; the succeeding
    // side of the same rule is a rate of exactly 1, from
    // MoneyTest.testConvertedToWithExplicitRate.
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo same currency rate 1.1",
        opEntry("convertedTo").set("left", jMoney(left)).set("target", jName(Currency.RON))
            .set("rate", jDbl(1.1)),
        () -> jMoney(left.convertedTo(Currency.RON, Decimal.of(1.1))), Boolean.TRUE);
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo same currency rate 1",
        opEntry("convertedTo").set("left", jMoney(left)).set("target", jName(Currency.RON))
            .set("rate", jDbl(1.0)),
        () -> jMoney(left.convertedTo(Currency.RON, Decimal.of(1))), Boolean.FALSE);
    // MoneyTest.testConvertedToWithExplicitRate: AUD 100.12 at 2.6 -> RON
    // 260.31, rounded to RON's two minor units.
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo cross currency explicit rate",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.RON))
            .set("rate", jDbl(2.6)),
        () -> jMoney(other.convertedTo(Currency.RON, Decimal.of(2.6d))), Boolean.FALSE);
    // MoneyTest.testConvertedToWithRateProvider: the same conversion through a
    // provider, expressed here as the FxRate that provides it (FxRate IS an
    // FxRateProvider), so the fixture carries the rate in its `rates` list
    // rather than an opaque lambda.
    addOperation(results, FX_CURRENCY_MATH, "Money.convertedTo rate provider",
        opEntry("convertedTo").set("left", jMoney(other)).set("target", jName(Currency.RON))
            .set("rates", jRateEntries(rateEntries(Currency.AUD, Currency.RON, 2.5))),
        () -> jMoney(other.convertedTo(Currency.RON,
            MATH_IN.rate(FxRate.of(Currency.AUD, Currency.RON, 2.5)))), Boolean.FALSE);
    addOperation(results, FX_CURRENCY_MATH, "Money.toBigMoney",
        opEntry("toBigMoney").set("left", jMoney(left)),
        () -> jBigMoney(left.toBigMoney()), Boolean.FALSE);

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
          () -> jMoney(Money.of(row.currency, SWEEP_AMOUNT)), null);
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
      () -> jCurrencyAmountArray(gbpArray.plus(gbpOther)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus array",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(gbpOther)),
      () -> jCurrencyAmountArray(gbpArray.minus(gbpOther)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus amount",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 10)))),
      () -> jCurrencyAmountArray(gbpArray.plus(CurrencyAmount.of(Currency.GBP, 10))),
      Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus currency mismatch",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(usdArray)),
      () -> jCurrencyAmountArray(gbpArray.plus(usdArray)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus size mismatch",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(shortArray)),
      () -> jCurrencyAmountArray(gbpArray.plus(shortArray)), Boolean.TRUE);
  // The three failing shapes of `minus` mirror those of `plus`: a different
  // currency, a different size, and a scalar amount in another currency. They
  // are captured separately because the Java messages differ ("Currencies must
  // be equal ..." against "Sizes must be equal ...") and the port returns a
  // different Failure for each.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus currency mismatch",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(usdArray)),
      () -> jCurrencyAmountArray(gbpArray.minus(usdArray)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus size mismatch",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmountArray(shortArray)),
      () -> jCurrencyAmountArray(gbpArray.minus(shortArray)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.plus amount currency mismatch",
      opEntry("plus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.USD, 10)))),
      () -> jCurrencyAmountArray(gbpArray.plus(CurrencyAmount.of(Currency.USD, 10))),
      Boolean.TRUE);
  // CurrencyAmountArrayTest.test_minus_currencyAmount
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus amount",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.GBP, 0.5)))),
      () -> jCurrencyAmountArray(gbpArray.minus(CurrencyAmount.of(Currency.GBP, 0.5))),
      Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.minus amount currency mismatch",
      opEntry("minus").set("left", jCurrencyAmountArray(gbpArray))
          .set("right", jCurrencyAmount(MATH_IN.amount(CurrencyAmount.of(Currency.USD, 0.5)))),
      () -> jCurrencyAmountArray(gbpArray.minus(CurrencyAmount.of(Currency.USD, 0.5))),
      Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.convertedTo",
      opEntry("convertedTo").set("left", jCurrencyAmountArray(gbpArray))
          .set("target", jName(Currency.USD)).set("rate", jDbl(1.6)),
      () -> jCurrencyAmountArray(gbpArray.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.GBP, Currency.USD, 1.6)))),
      Boolean.FALSE);
  // Composed: the Java type has no multipliedBy, so the expectation is built
  // from of(currency, getValues().multipliedBy(scalar)).
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.multipliedBy (composed)",
      opEntry("multipliedBy").set("left", jCurrencyAmountArray(gbpArray))
          .set("scalar", jDbl(MATH_IN.scalar(2.5)))
          .set("composed", jBool(true)),
      () -> jCurrencyAmountArray(
          CurrencyAmountArray.of(gbpArray.getCurrency(), gbpArray.getValues().multipliedBy(2.5))),
      Boolean.FALSE);
  // Composed: likewise mapAmounts, using the documented function.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.mapAmounts (composed)",
      opEntry("mapAmounts").set("left", jCurrencyAmountArray(gbpArray))
          .set("mapAmountsFn", jStr(MAP_AMOUNTS_FN)).set("composed", jBool(true)),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(gbpArray.getCurrency(),
          gbpArray.getValues().map(v -> mapAmountsFn(v)))), Boolean.FALSE);
  // of(List<CurrencyAmount>) and its mixed-currency rejection.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of list",
      opEntry("of").set("input", jDoubles(new double[] {4d, 5d, 6d}))
          .set("currency", jName(Currency.GBP)),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(Arrays.asList(
          CurrencyAmount.of(Currency.GBP, 4), CurrencyAmount.of(Currency.GBP, 5),
          CurrencyAmount.of(Currency.GBP, 6)))), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of mixed currencies",
      opEntry("of").set("input", jStr("GBP 4, USD 5")),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(Arrays.asList(
          CurrencyAmount.of(Currency.GBP, 4), CurrencyAmount.of(Currency.USD, 5)))),
      Boolean.TRUE);
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
      Boolean.FALSE);
  List<CurrencyAmount> mixedFunctionValues = Arrays.asList(
      CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.USD, 2),
      CurrencyAmount.of(Currency.GBP, 3));
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.of function mixed currencies",
      opEntry("of").set("size", jInt(3)).set("input", jStr("GBP 1, USD 2, GBP 3")),
      () -> jCurrencyAmountArray(CurrencyAmountArray.of(3, i -> mixedFunctionValues.get(i))),
      Boolean.TRUE);
  // CurrencyAmountArrayTest.test_convertedTo_missingFxRate: converting with a
  // rate for an unrelated pair fails rather than silently leaving the values
  // unconverted.
  addOperation(results, FX_CURRENCY_MATH, "CurrencyAmountArray.convertedTo missing rate",
      opEntry("convertedTo").set("left", jCurrencyAmountArray(gbpArray))
          .set("target", jName(Currency.USD))
          .set("rates", jRateEntries(rateEntries(Currency.EUR, Currency.USD, 1.61))),
      () -> jCurrencyAmountArray(gbpArray.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.EUR, Currency.USD, 1.61)))), Boolean.TRUE);

  // SIGNED ZERO THROUGH THE ARRAY TYPES - three different answers, all pinned.
  //
  // CurrencyAmount.of normalises -0.0 to +0.0, but CurrencyAmountArray stores
  // a DoubleArray and does NOT: the element keeps its sign bit, which `of`
  // below records through doubleToLongBits. Multiplying by -1.0 therefore
  // flips the signs of both zeros, while `get(i)` hands the element to
  // CurrencyAmount.of and so normalises it. A port that treated -0.0 and 0.0
  // as interchangeable would satisfy none of the three.
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
          signedZeroArray.getValues().multipliedBy(-1.0))), Boolean.FALSE);
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
      () -> jMultiCurrencyAmounts(base), Boolean.FALSE);
  // of() rejects duplicate currencies; total() merges them.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.of duplicate currency",
      opEntry("of").set("input", jStr("GBP 100, GBP 200")),
      () -> jMultiCurrencyAmounts(MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 100), CurrencyAmount.of(Currency.GBP, 200))),
      Boolean.TRUE);
  // The empty amount is a legal value, not an error, and it is the identity of
  // the port's Monoid, so its encoding (an empty amount list) is pinned here.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.empty",
      opEntry("of").set("input", jStr("empty")),
      () -> jMultiCurrencyAmounts(MultiCurrencyAmount.empty()), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.total duplicate currency",
      opEntry("total").set("input", jStr("GBP 100, GBP 200")),
      () -> jMultiCurrencyAmounts(MultiCurrencyAmount.total(Arrays.asList(
          CurrencyAmount.of(Currency.GBP, 100), CurrencyAmount.of(Currency.GBP, 200)))),
      Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.plus multi",
      opEntry("plus").set("left", jMultiCurrencyAmounts(base))
          .set("right", jMultiCurrencyAmounts(other)),
      () -> jMultiCurrencyAmounts(base.plus(other)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.minus multi",
      opEntry("minus").set("left", jMultiCurrencyAmounts(base))
          .set("right", jMultiCurrencyAmounts(other)),
      () -> jMultiCurrencyAmounts(base.minus(other)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.multipliedBy",
      opEntry("multipliedBy").set("left", jMultiCurrencyAmounts(base))
          .set("scalar", jDbl(MATH_IN.scalar(1.5))),
      () -> jMultiCurrencyAmounts(base.multipliedBy(1.5)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.mapAmounts",
      opEntry("mapAmounts").set("left", jMultiCurrencyAmounts(base))
          .set("mapAmountsFn", jStr(MAP_AMOUNTS_FN)),
      () -> jMultiCurrencyAmounts(base.mapAmounts(v -> mapAmountsFn(v))), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.getAmount known",
      opEntry("getAmount").set("left", jMultiCurrencyAmounts(base))
          .set("currency", jName(Currency.GBP)),
      () -> jCurrencyAmount(base.getAmount(Currency.GBP)), Boolean.FALSE);
  // getAmount for a currency the amount does not contain throws.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.getAmount unknown",
      opEntry("getAmount").set("left", jMultiCurrencyAmounts(base))
          .set("currency", jName(Currency.CHF)),
      () -> jCurrencyAmount(base.getAmount(Currency.CHF)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmount.convertedTo",
      opEntry("convertedTo").set("left", jMultiCurrencyAmounts(base))
          .set("target", jName(Currency.USD))
          .set("rates", jRateEntries(rateEntries(Currency.GBP, Currency.USD, 1.6))),
      () -> jCurrencyAmount(base.convertedTo(Currency.USD,
          MATH_IN.rate(FxRate.of(Currency.GBP, Currency.USD, 1.6)))),
      Boolean.FALSE);
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
      () -> jMultiCurrencyAmountArray(base), Boolean.FALSE);
  // of() with unequal array lengths across currencies is rejected.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of unequal lengths",
      opEntry("of").set("input", jStr("GBP[1,2,3], USD[1,2]")),
      () -> {
        Map<Currency, DoubleArray> bad = new LinkedHashMap<>();
        bad.put(Currency.GBP, DoubleArray.of(1d, 2d, 3d));
        bad.put(Currency.USD, DoubleArray.of(1d, 2d));
        return jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(bad));
      }, Boolean.TRUE);
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
      () -> jMultiCurrencyAmountArray(raggedArray), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of ragged function",
      opEntry("of").set("size", jInt(3))
          .set("input", jStr("[EUR 4], [GBP 21, USD 32, EUR 43], [EUR 44]")),
      () -> jMultiCurrencyAmountArray(
          MultiCurrencyAmountArray.of(3, i -> raggedAmounts.get(i))), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.getValues unknown ragged",
      opEntry("getValues").set("left", jMultiCurrencyAmountArray(raggedArray))
          .set("currency", jName(Currency.AUD)),
      () -> jDoubleArray(raggedArray.getValues(Currency.AUD)), Boolean.TRUE);
  // MultiCurrencyAmountArrayTest.test_empty_amounts: a size with no currencies
  // at all, which the port must round-trip without inventing an entry.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.of empty amounts",
      opEntry("of").set("size", jInt(2)).set("input", jStr("[], []")),
      () -> jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(
          MultiCurrencyAmount.empty(), MultiCurrencyAmount.empty())), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.getValues known",
      opEntry("getValues").set("left", jMultiCurrencyAmountArray(base))
          .set("currency", jName(Currency.GBP)),
      () -> jDoubleArray(base.getValues(Currency.GBP)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.getValues unknown",
      opEntry("getValues").set("left", jMultiCurrencyAmountArray(base))
          .set("currency", jName(Currency.CHF)),
      () -> jDoubleArray(base.getValues(Currency.CHF)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.plus array",
      opEntry("plus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(other)),
      () -> jMultiCurrencyAmountArray(base.plus(other)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.minus array",
      opEntry("minus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(other)),
      () -> jMultiCurrencyAmountArray(base.minus(other)), Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.plus size mismatch",
      opEntry("plus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(shortArray)),
      () -> jMultiCurrencyAmountArray(base.plus(shortArray)), Boolean.TRUE);
  // MultiCurrencyAmountArrayTest.test_minusArray / test_plusDifferentSize: the
  // subtracting side of both shapes, so the port's `minus` is pinned as well
  // as its `plus`.
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.minus size mismatch",
      opEntry("minus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmountArray(shortArray)),
      () -> jMultiCurrencyAmountArray(base.minus(shortArray)), Boolean.TRUE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.minus multi",
      opEntry("minus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmounts(MultiCurrencyAmount.of(
              CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.CHF, 2)))),
      () -> jMultiCurrencyAmountArray(base.minus(MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.CHF, 2)))),
      Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.plus multi",
      opEntry("plus").set("left", jMultiCurrencyAmountArray(base))
          .set("right", jMultiCurrencyAmounts(MultiCurrencyAmount.of(
              CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.USD, 2)))),
      () -> jMultiCurrencyAmountArray(base.plus(MultiCurrencyAmount.of(
          CurrencyAmount.of(Currency.GBP, 1), CurrencyAmount.of(Currency.USD, 2)))),
      Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.total",
      opEntry("total").set("input", jStr("GBP[1,2,3] + USD[10,20,30]")),
      () -> jMultiCurrencyAmountArray(MultiCurrencyAmountArray.total(Arrays.asList(
          CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d)),
          CurrencyAmountArray.of(Currency.USD, DoubleArray.of(10d, 20d, 30d))))),
      Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.convertedTo",
      opEntry("convertedTo").set("left", jMultiCurrencyAmountArray(base))
          .set("target", jName(Currency.USD)),
      () -> jCurrencyAmountArray(
          base.convertedTo(Currency.USD, FxRate.of(Currency.GBP, Currency.USD, 1.6))),
      Boolean.FALSE);
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
      }, Boolean.FALSE);
  addOperation(results, FX_CURRENCY_MATH, "MultiCurrencyAmountArray.mapAmounts (composed)",
      opEntry("mapAmounts").set("left", jMultiCurrencyAmountArray(base))
          .set("mapAmountsFn", jStr(MAP_AMOUNTS_FN)).set("composed", jBool(true)),
      () -> {
        Map<Currency, DoubleArray> mapped = new LinkedHashMap<>();
        for (Map.Entry<Currency, DoubleArray> entry : base.getValues().entrySet()) {
          mapped.put(entry.getKey(), entry.getValue().map(v -> mapAmountsFn(v)));
        }
        return jMultiCurrencyAmountArray(MultiCurrencyAmountArray.of(mapped));
      }, Boolean.FALSE);
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
  // MoneyTest.testMinus, the failing direction: the message differs from the
  // addition case, and the port maps each to its own Failure.
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
 * WHY THAT MATTERS, AND WHY THE JAVA TEST LISTS ARE NOT COPIED HERE. HUBU is
 * built as ImmutableHolidayCalendar.of(id, holidays, SUNDAY, SUNDAY)
 * (GlobalHolidayCalendars.java:1204): Sunday is its ONLY weekend day and its
 * Saturdays are listed EXPLICITLY as holidays by addHungarianSaturdays
 * (:1239-1250). A fixture that copied the Java test tables would be ambiguous
 * between "not a holiday" and "a weekend day the table filtered out", and the
 * ambiguity would land on exactly the calendar where it changes the answer.
 * So the full isHoliday truth is emitted, and the Java tables are used only as
 * a cross-check, in the direction the tests assert them.
 *
 * The out-of-range rows are deliberate: outside its stored year range an
 * ImmutableHolidayCalendar falls back to a weekend-only test rather than
 * throwing (ImmutableHolidayCalendar.java:397-415), and the port must
 * reproduce that. Beyond year 0000-9999 it throws instead, which the
 * `yearRange` rows capture as `error` expectations.
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
 * The weekend days of every captured calendar, transcribed from the Java
 * source that constructs it - NOT from any date table. They are not public
 * API, so they are not emitted; they are the expectation the out-of-range and
 * weekend rows are checked against, which is what proves the weekend-only
 * fallback rather than assuming it.
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
    error = errorMessage(thrown);
  }
  row.holidays = holidayDates;
  row.error = error;
  if (holidayDates != null) {
    // Cross-check 1: the same truth through a different public API. `holidays`
    // is a Stream over the range, so agreement is not a tautology of the loop
    // above - it also pins the two APIs to each other for the port.
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
 * data_easter() of GlobalHolidayCalendarsTest, extracted mechanically from the
 * Java test source as {day, month, year} triples - 201 rows covering
 * 1900-2099, with its duplicated 1900 row kept so the extraction stays a copy
 * rather than an edit.
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
  // No row other than the deliberate year-range rejections may carry an error.
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
 * DoubleMatrix HAS NO MATRIX PRODUCT. The row schema says "matrix
 * multipliedBy", and the only multipliedBy the class exposes is the SCALAR
 * multipliedBy(double). No matrix-times-matrix product is invented here; the
 * captured matrix operations are the scalar multipliedBy plus transpose,
 * total, with, plus, minus and the element-wise combine.
 *
 * Rows deliberately include non-finite and signed-zero contents so the
 * tagged-double policy is exercised on both sides.
 * ===========================================================================
 */

String FX_DOUBLE_ARRAY = "double-array";

void addDoubleArrayRow(JArray rows, String source, double[] a, double[] b, double scalar,
    double[][] matrixA, double[][] matrixB, boolean captureOnly) {
  DoubleArray arrayA = DoubleArray.copyOf(a);
  DoubleArray arrayB = DoubleArray.copyOf(b);
  JObject row = new JObject()
      .set("source", jStr(source))
      .set("a", jDoubles(a))
      .set("b", jDoubles(b))
      .set("scalar", jDbl(scalar))
      .set("matrixA", matrixA == null ? jNull() : jDoubleMatrix(DoubleMatrix.copyOf(matrixA)))
      .set("matrixB", matrixB == null ? jNull() : jDoubleMatrix(DoubleMatrix.copyOf(matrixB)));
  JArray results = new JArray();
  // Element-wise and scalar array operations.
  addOperation(results, FX_DOUBLE_ARRAY, source + " plus", opEntry("plus"),
      () -> jDoubleArray(arrayA.plus(arrayB)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " minus", opEntry("minus"),
      () -> jDoubleArray(arrayA.minus(arrayB)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " multipliedBy", opEntry("multipliedBy"),
      () -> jDoubleArray(arrayA.multipliedBy(scalar)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " dividedBy", opEntry("dividedBy"),
      () -> jDoubleArray(arrayA.dividedBy(scalar)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " map", opEntry("map")
      .set("mapFn", jStr(MAP_AMOUNTS_FN)),
      () -> jDoubleArray(arrayA.map(v -> mapAmountsFn(v))), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " reduce", opEntry("reduce")
      .set("reduceFn", jStr("(acc, v) -> acc + v")).set("identity", jDbl(0d)),
      () -> jDbl(arrayA.reduce(0d, (acc, v) -> acc + v)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " sum", opEntry("sum"),
      () -> jDbl(arrayA.sum()), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " min", opEntry("min"),
      () -> jDbl(arrayA.min()), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " max", opEntry("max"),
      () -> jDbl(arrayA.max()), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " sorted", opEntry("sorted"),
      () -> jDoubleArray(arrayA.sorted()), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " concat", opEntry("concat"),
      () -> jDoubleArray(arrayA.concat(arrayB)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " subArray", opEntry("subArray")
      .set("fromIndex", jInt(1L)),
      () -> jDoubleArray(arrayA.subArray(1)), null);
  addOperation(results, FX_DOUBLE_ARRAY, source + " with", opEntry("with")
      .set("index", jInt(0L)).set("value", jDbl(scalar)),
      () -> jDoubleArray(arrayA.with(0, scalar)), null);
  if (matrixA != null) {
    DoubleMatrix mA = DoubleMatrix.copyOf(matrixA);
    // Scalar multipliedBy only - see the section banner.
    addOperation(results, FX_DOUBLE_ARRAY, source + " matrixMultipliedBy",
        opEntry("matrixMultipliedBy").set("scalar", jDbl(scalar)),
        () -> jDoubleMatrix(mA.multipliedBy(scalar)), null);
    addOperation(results, FX_DOUBLE_ARRAY, source + " transpose", opEntry("transpose"),
        () -> jDoubleMatrix(mA.transpose()), null);
    addOperation(results, FX_DOUBLE_ARRAY, source + " total", opEntry("total"),
        () -> jDbl(mA.total()), null);
    addOperation(results, FX_DOUBLE_ARRAY, source + " matrixWith", opEntry("matrixWith")
        .set("row", jInt(0L)).set("column", jInt(0L)).set("value", jDbl(scalar)),
        () -> jDoubleMatrix(mA.with(0, 0, scalar)), null);
    if (matrixB != null) {
      DoubleMatrix mB = DoubleMatrix.copyOf(matrixB);
      addOperation(results, FX_DOUBLE_ARRAY, source + " matrixPlus", opEntry("matrixPlus"),
          () -> jDoubleMatrix(mA.plus(mB)), null);
      addOperation(results, FX_DOUBLE_ARRAY, source + " matrixMinus", opEntry("matrixMinus"),
          () -> jDoubleMatrix(mA.minus(mB)), null);
      addOperation(results, FX_DOUBLE_ARRAY, source + " matrixCombine", opEntry("matrixCombine")
          .set("combineFn", jStr("(x, y) -> x * y")),
          () -> jDoubleMatrix(mA.combine(mB, (x, y) -> x * y)), null);
    }
  }
  row.set("results", results);
  rows.add(row);
  CHECK.countRow(FX_DOUBLE_ARRAY);
  if (captureOnly) {
    CHECK.countCaptureOnly(FX_DOUBLE_ARRAY);
  }
}

Jn buildDoubleArrayFixture() {
  JArray rows = new JArray();
  // The literal arrays DoubleArrayTest and DoubleMatrixTest use.
  addDoubleArrayRow(rows, "DoubleArrayTest.basic", new double[] {1d, 2d, 3d},
      new double[] {0.5d, 1.5d, 2.5d}, 2.5d,
      new double[][] {{1d, 2d, 3d}, {4d, 5d, 6d}},
      new double[][] {{6d, 5d, 4d}, {3d, 2d, 1d}}, false);
  addDoubleArrayRow(rows, "DoubleArrayTest.duplicates", new double[] {1d, 2d, 3d, 3d, 4d},
      new double[] {4d, 3d, 2d, 1d, 0d}, -1.5d,
      new double[][] {{1d, 2d}, {2d, 4d}}, new double[][] {{1d, 0d}, {0d, 1d}}, false);
  addDoubleArrayRow(rows, "DoubleArrayTest.nine",
      new double[] {1d, 2d, 3d, 4d, 5d, 6d, 7d, 8d, 9d},
      new double[] {9d, 8d, 7d, 6d, 5d, 4d, 3d, 2d, 1d}, 3d,
      new double[][] {{1d, 2d, 3d, 4d, 5d, 6d}}, null, false);
  addDoubleArrayRow(rows, "DoubleArrayTest.single", new double[] {1d}, new double[] {2d}, 0.5d,
      new double[][] {{42d}}, new double[][] {{-42d}}, false);
  // Empty arrays: min and max throw, which is itself an expectation.
  addDoubleArrayRow(rows, "DoubleArrayTest.empty", new double[] {}, new double[] {}, 1d, null,
      null, false);
  // Non-finite and signed-zero contents, so the tagged-double policy is
  // exercised end to end on both sides.
  addDoubleArrayRow(rows, "nonFinite.signedZero",
      new double[] {0d, -0.0d, 1d, -1d},
      new double[] {-0.0d, 0d, -1d, 1d}, -0.0d,
      new double[][] {{0d, -0.0d}, {-0.0d, 0d}}, new double[][] {{1d, 1d}, {1d, 1d}}, false);
  addDoubleArrayRow(rows, "nonFinite.infinities",
      new double[] {Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY, 1d},
      new double[] {1d, 1d, Double.POSITIVE_INFINITY}, Double.POSITIVE_INFINITY,
      new double[][] {{Double.POSITIVE_INFINITY, 1d}, {1d, Double.NEGATIVE_INFINITY}},
      new double[][] {{1d, 1d}, {1d, 1d}}, false);
  addDoubleArrayRow(rows, "nonFinite.nan",
      new double[] {Double.NaN, 1d, 2d}, new double[] {1d, Double.NaN, 3d}, Double.NaN,
      new double[][] {{Double.NaN, 1d}, {1d, Double.NaN}},
      new double[][] {{1d, 1d}, {1d, 1d}}, false);
  // Seeded random arrays: capture-only.
  for (int scenario = 0; scenario < 6; scenario++) {
    int length = 3 + scenario;
    double[] a = new double[length];
    double[] b = new double[length];
    for (int i = 0; i < length; i++) {
      a[i] = nextRandomAmount();
      b[i] = nextRandomAmount();
    }
    double[][] matrixA = new double[2][length];
    double[][] matrixB = new double[2][length];
    for (int r = 0; r < 2; r++) {
      for (int c = 0; c < length; c++) {
        matrixA[r][c] = nextRandomAmount();
        matrixB[r][c] = nextRandomAmount();
      }
    }
    addDoubleArrayRow(rows, "random.seed" + RANDOM_SEED + ".array" + scenario, a, b,
        nextRandomRate(), matrixA, matrixB, true);
  }
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
 * Enumerated from the Java side, with EVERY count asserted in code. A count
 * that differs from the independently verified expectation aborts the capture,
 * so a resource edited under the port cannot silently reshape the manifest.
 *
 * The Scala-side spec asserts that the ported data objects equal this document
 * exactly, so key names and nesting are part of the contract: keep them
 * explicit, self-describing and stable.
 * ===========================================================================
 */

String FX_MANIFEST = "manifest";

/**
 * Public static constants of a holder class, read reflectively.
 *
 * Reflection is deliberate and permitted in this tool (see the header): it is
 * the only way to get a provably COMPLETE constant list, which is the point of
 * a manifest. The type is matched by simple name so that the element types do
 * not all have to be imported.
 */
List<Named> namedConstants(Class<?> holder, String typeSimpleName) {
  List<Named> result = new ArrayList<>();
  for (Field field : holder.getDeclaredFields()) {
    if (Modifier.isPublic(field.getModifiers()) && Modifier.isStatic(field.getModifiers())
        && field.getType().getSimpleName().equals(typeSimpleName)) {
      try {
        result.add((Named) field.get(null));
      } catch (IllegalAccessException ex) {
        throw new IllegalStateException("Cannot read " + holder.getSimpleName() + "."
            + field.getName(), ex);
      }
    }
  }
  return result;
}

/** A named-constant group: its asserted count and its names in declaration order. */
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
 * Two counts are recorded, because they legitimately differ: the INI
 * `[alternates]` section is the table the port transcribes, while
 * ExtendedEnum.alternateNames() additionally registers a derived UPPER-CASE
 * key for every mixed-case alternate. OvernightIndex is the case in point -
 * 10 INI rows become 13 API entries, because "DKK-Tom Next", "EUR-EuroSTR" and
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
 * TRIS TWTA - have no built-in calendar in Java either, so they are ids that
 * legitimately fail to resolve. The manifest records the mapping and each id's
 * resolvability WITHOUT attempting to depend on resolution succeeding.
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
 * `CAPTURE_COMPLETED` is not redundant with `CHECK.ok()`. A JShell snippet
 * that fails to COMPILE is reported and skipped, and calling the method it
 * declared then throws SPIResolutionException at run time; that is caught
 * here, but it would leave the failure list empty and the run would exit 0 on
 * a capture that never happened. The flag closes that hole, so the exit status
 * means "every document was built and every check passed".
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

/exit ((CHECK.ok() && CAPTURE_COMPLETED) ? 0 : 1)
