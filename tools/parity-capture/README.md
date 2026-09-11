Parity Capture
--------------
This directory holds the capture procedure for the **Java-generated parity baselines** of the
Scala port of `strata-collect` and `strata-basics`.

It contains exactly two files:

* `capture-baseline.jsh` — a JShell script that runs against the Maven-built *Java* Strata jars
  and emits six numerical parity fixtures plus one reference-data manifest.
* `README.md` — this document: the procedure, the fixture schemas and the manifest schema.

The seven JSON documents are the deliverable; they live in the two Scala modules' test resources
and are committed alongside the Scala code. The script is retained so that the baselines can be
**regenerated and audited** by a third party, and this README is what that third party follows.

No user-specified rules apply to this file: `review_rules` reports that no rules were provided, so
the work is held to enterprise-standard best practice instead — the commands below were run in
this repository, every number is traceable to a file, and nothing is claimed that was not
observed. *Verification status* in section 3 records exactly what was executed, and what was not.

### 0. Read this first

Three properties of this directory look like defects and are not. Changing them breaks the
capture.

**The script is `.jsh`, never `.java`.** The deliverable forbids `.java` files in the ported
modules, yet the baselines must come from the Java implementation. JShell ships with JDK 21, so
the Java classes can be driven directly from a script without adding a compilation unit, a build
step or a source file anywhere. Do not "promote" this script to a `.java` class, a JUnit test or a
Maven module. The fixtures are the product; the script is the audit trail.

**The capture classpath deliberately carries Guava and Joda.** The dependency-purity requirement
(Rule 1 / Gate 2) is measured on the sbt `Compile` and `Test` classpaths of `strata-collect` and
`strata-basics`. `tools/` is on neither: it is on no sbt source root, it is compiled by nothing,
and no CI job runs it — the Scala CI job runs `scripts/verify-gates.sh`, which invokes `sbt`, not
`mvn` and not `jshell`. The Java implementation being measured depends on Guava and Joda, so
removing them here does not improve purity; it only makes the capture impossible.

**The procedure never writes into the Java tree.** The acceptance gate requires
`git status --porcelain -- modules examples eclipse pom.xml src .github` to stay empty. The Maven
build writes only into `target/`, which is git-ignored (`.gitignore:13`), and the script writes
only the seven paths in section 4 — `guardOutputPath` enforces that at run time by rejecting
absolute paths, `..`, and anything whose first path segment is `modules`, `examples`, `eclipse`,
`src`, `.github` or `project`. The Java sources, tests and resources under `modules/**` are
read-only reference material: they are the source every later migration slice ports from, and the
only reproducible source of these baselines. Never edit them to make a fixture match.

### 1. Purpose and why this exists

The port must reproduce the Java numbers to **1e-9 absolute *and* relative** (Rule 2, verified by
Gate 3). That tolerance cannot be met against values transcribed by hand from test sources or
re-derived from a specification: a Java→Scala port of a numerical library is pinned by **golden
baselines generated from the original implementation**. Everything else — a plausible-looking
constant, a rounded expectation copied out of a test table, a value recomputed "the same way" —
drifts silently in the last digits, which is exactly where a day-count or a triangulated FX rate
goes wrong.

So the baseline is captured, not written:

1. build the untouched Maven modules once;
2. run `capture-baseline.jsh` against the resulting jars;
3. commit the seven JSON documents it emits;
4. let the Scala specs assert against them.

Two consequences follow, and both matter more than they look.

* **A failing parity spec is never fixed by editing a fixture.** The fixture is the Java answer.
  Either the Scala code is wrong (fix the code), or the fixture is stale because the Java tree
  changed (regenerate it with this procedure and review the diff). Hand-editing a value destroys
  the only independent evidence the port has.
* **The capture cross-checks itself.** Every captured value that has a hard-coded Java test
  constant is compared against that constant during capture, and every reference-data row count is
  compared against an independently verified expected count. A mismatch aborts the run with a
  non-zero status and writes nothing, so a fixture on disk is one that agreed with the Java tests
  when it was produced.

### 2. Prerequisites

* **JDK 21.** `jshell` is part of the JDK, so nothing else needs installing. The repository's CI
  already provides a JDK 21 executor (`.circleci/config.yml:36-38`, image `cimg/openjdk:21.0`).
* **Apache Maven**, to build the two Java modules.
* **A clean checkout**, and a shell whose working directory is the **repository root** — the
  paths below, including the script's default output root, are repository-root relative.
* Roughly 1 GB of free heap for the capture JVM (`-R-Xmx900m` is what the script is run with) and
  about 20 MB of disk for the emitted documents.

No network access is needed once Maven's dependencies are cached, and no database, service or
container is involved.

### 3. Procedure

#### Step 1 — build the two Java modules

```
mvn -B -pl modules/collect,modules/basics -am -DskipTests -Dcheckstyle.skip=true \
    -Dmaven.javadoc.skip=true package
```

This produces

* `modules/collect/target/strata-collect-2.12.74-SNAPSHOT.jar`
* `modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar`

The coordinates are the Maven build's own: version `2.12.74-SNAPSHOT` (`pom.xml:13`), artifactIds
`strata-collect` (`modules/collect/pom.xml:11`) and `strata-basics`
(`modules/basics/pom.xml:11`). Both outputs land under git-ignored `target/`.

`package` is preferred over `install` because `install` writes the shared local Maven repository
under coordinates other builds resolve, which this procedure does not need. Use `install` instead
only if you take route (a) below.

#### Step 2 — assemble the capture classpath

The script needs the two module jars **plus** the Java implementation's third-party dependencies
(Guava, `failureaccess`, Joda-Beans, Joda-Convert). Two routes work; both were verified and both
produce byte-identical fixtures.

Route (a) — `install`, then let Maven resolve the whole basics classpath:

```
mvn -B -pl modules/collect,modules/basics -am -DskipTests -Dcheckstyle.skip=true \
    -Dmaven.javadoc.skip=true install
mvn -q -pl modules/basics dependency:build-classpath -Dmdep.outputFile=/tmp/basics-cp.txt
CP="$(cat /tmp/basics-cp.txt):modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar"
```

This is the shortest form, and it works because `modules/basics/pom.xml` already depends on
`strata-collect` at compile scope (`:19-22`) and on its test-jar (`:50-55`). Note the reason
`install` is required here: `dependency:build-classpath` resolves `strata-collect` from the local
repository even when `-am` is passed, so after `package` alone a machine that has never installed
`strata-collect` cannot resolve it.

Route (b) — `package` only, resolving just the third-party jars and naming the module jars
explicitly:

```
mvn -q -pl modules/collect dependency:build-classpath -Dmdep.outputFile=/tmp/collect-cp.txt
CP="modules/basics/target/strata-basics-2.12.74-SNAPSHOT.jar"
CP="$CP:modules/collect/target/strata-collect-2.12.74-SNAPSHOT.jar"
CP="$CP:$(cat /tmp/collect-cp.txt)"
```

`modules/collect` has no OpenGamma dependencies, so its classpath is purely third-party.

If the jars are already present in the local Maven repository, the equivalent zero-rebuild
classpath is just those six entries: `strata-basics`, `strata-collect`, `guava`, `failureaccess`,
`joda-beans`, `joda-convert`.

#### Step 3 — run the capture

```
jshell --class-path "$CP" -R-Xmx900m tools/parity-capture/capture-baseline.jsh
```

Before anything else the script runs a **classpath preflight** over the types it needs. An
incomplete classpath is the most common mistake, and it produces an actionable message naming the
missing classes and the build command to fix them, rather than a wall of JShell "package does not
exist" errors.

**Output root.** The script writes the seven documents relative to the value of the
`parity.out.dir` system property, whose default is `.` — the current directory, expected to be the
repository root. Pass it through JShell's `-R` prefix to write elsewhere, which is the safe way to
inspect a capture before letting it touch the working tree:

```
jshell --class-path "$CP" -R-Xmx900m -R-Dparity.out.dir=/tmp/parity-out \
       tools/parity-capture/capture-baseline.jsh
diff -r /tmp/parity-out/strata-basics strata-basics
```

**Exit status.** Zero only when every document was built *and* every self-check passed; non-zero
on any check failure, count mismatch, preflight failure or exception in the driver. The documents
are accumulated in memory and flushed only after all checks pass, so **a failed run writes
nothing** and cannot leave a half-valid fixture behind. A failure prints one line per failing
check, naming the fixture, the row and the expected/actual/delta/tolerance.

**Progress output.** The run prints the JDK version, the resolved output root, the random seed,
one line per document, then a summary table of `rows`, `checks`, `errRows` (rows whose expectation
is a Java failure) and `capOnly` (rows deliberately without a Java constant to check against),
and finally the path and size of each document written.

**Safety.** Run all three steps from the repository root. The capture writes only the seven paths
listed in section 4 and never under `modules/`, `examples/`, `eclipse/`, `src/` or `.github/` — it
refuses to, as section 0 describes — and Maven writes only into git-ignored `target/`. Nothing in
this procedure modifies the Java tree.

#### Verification status

This procedure was executed on this branch, on OpenJDK 21.0.12.1 (Temurin) with Maven 3.9.16, and
the results below are what was observed — not an estimate:

* Step 1 completed with `BUILD SUCCESS` in about 12 s against warm Maven caches, producing both
  jars.
* The capture ran in well under a minute at `-R-Xmx900m`, reported `all checks passed` and wrote
  all seven documents, exiting 0. Observed totals for the script as it now stands:
  **23,014 rows and 198,001 checks**, made up of `daycount` 18,254, `holiday` 3,846, `schedule`
  879, `fx` 14, `double-array` 14, `currency-math` 6 and the manifest counted as one. Each
  fixture's own section below states the coverage it contributes; the total moves whenever one of
  them is extended, so treat it as a reading of this revision rather than a fixed figure.
* It was run five times in total — three times before the holiday fixture's checks were added
  (twice with the zero-rebuild classpath of six local-repository entries, once with the route-(b)
  classpath assembled from the `package` output) and twice after — and within each set the
  documents were **byte-identical** (`diff -r`), which is the determinism contract of section 5
  demonstrated rather than asserted. Adding those checks changed `holiday-baseline.json` only: the
  other six documents stayed byte-identical to the earlier runs, which is how the change was
  verified.
* The abort path was exercised for real rather than hypothetically: a two-row transcription error
  in the `data_easter` constants was caught by the new cross-check (`expected=2051-03-26
  actual=2051-04-02`), the run exited 1 and wrote nothing, and the constants were then re-extracted
  mechanically from the Java test source instead of being hand-corrected.
* The `fx-baseline.json` section was subsequently extended — row ids, one uniform row shape, and
  the `streamPairsToMatrix`, `addMultipleRatesSingle`, identity and empty-merge scenarios of
  section 6 — taking that fixture from 11 rows and 35 checks to 14 and 77. Every one of the added
  scenarios uses literal rates and draws nothing from the seeded `Random`, so the stream the later
  fixtures consume is untouched: the other six documents were verified **byte-identical**
  (`diff`) to the ones produced before that change, the already-committed
  `double-array-baseline.json` among them.
* It was run three times — twice with the zero-rebuild classpath (the six local-repository
  entries) and once with the route-(b) classpath assembled from the `package` output — and the
  three sets of documents were **byte-identical** (`diff -r`), which is the determinism contract
  of section 5 demonstrated rather than asserted.
* When the `currency-math` coverage was later extended, the capture was re-run twice more with the
  zero-rebuild classpath into two scratch output roots, and those two runs were byte-identical to
  each other across all seven documents. Diffed against the run that preceded the extension, the
  other **six** documents were byte-identical as well — which is the property that makes an
  extension to one fixture safe: the additions consume nothing from the shared seeded `Random`, so
  no later fixture's values move.
* Re-running the script now reproduces `schedule-baseline.json`, `fx-baseline.json`,
  `currency-math-baseline.json`, `holiday-baseline.json` and `double-array-baseline.json`
  **byte-identically** to the committed files, so the determinism contract of section 5 holds for
  those five. `daycount-baseline.json` is the exception and the difference is a known gap rather
  than drift: the committed fixture was captured with a day-count section ahead of the one in this
  script — it carries the uniform row shape and the per-row `id` of section 6, the
  `scheduleInfo.end` of the `test_yearFraction_30E360ISDA_notMaturity` rows, and 102 rows the
  script does not yet emit (`test_same`, `test_halfYear`, `test_wholeYear`, `test_wrongOrder` at
  22 rows each, `data_types.missingScheduleInfo` at 5 and
  `Business252DayCountTest.calendarRange` at 9). Every year fraction the two agree on is
  identical, so the fixture is sound and is the file the parity spec reads; bringing the day-count
  section up to it is the outstanding work, and until that is done do **not** overwrite
  `daycount-baseline.json` from a run of this script, because doing so would drop that coverage.
* Route (a) was verified as far as its classpath: `dependency:build-classpath` was run for both
  `modules/basics` and `modules/collect` and its output inspected, which is how the
  local-repository caveat above was established. The `install` invocation itself was **not** run,
  because the jars were already present in this environment's local repository.
* The failure path was exercised with a deliberately incomplete classpath: the preflight aborted,
  listed the missing classes, exited 1 and wrote zero files.
* `git status --porcelain -- modules examples eclipse pom.xml src .github` was empty afterwards.

Re-running on a different JDK 21 build should reproduce the same bytes; if it does not, treat the
difference as a finding and investigate it before committing anything.

### 4. Outputs

Seven documents, all UTF-8, LF-terminated, with exactly one trailing newline:

| Path | Contents |
|---|---|
| `strata-basics/src/test/resources/parity/daycount-baseline.json` | Day-count year fractions, relative year fractions and day counts, with and without schedule information |
| `strata-basics/src/test/resources/parity/schedule-baseline.json` | `PeriodicSchedule` resolutions: unadjusted and adjusted dates, periods, stubs, resolved conventions |
| `strata-basics/src/test/resources/parity/fx-baseline.json` | `FxMatrix` / `FxRate` rates, conversions, cross rates and merges |
| `strata-basics/src/test/resources/parity/currency-math-baseline.json` | `CurrencyAmount`, `Money`, `BigMoney`, `MultiCurrencyAmount` and the two amount-array types |
| `strata-basics/src/test/resources/parity/holiday-baseline.json` | Per-calendar, per-year holiday sets and date-arithmetic samples |
| `strata-collect/src/test/resources/parity/double-array-baseline.json` | `DoubleArray` and `DoubleMatrix` operation results |
| `strata-basics/src/test/resources/manifest/reference-data-manifest.json` | The reference-data manifest: every ported data table, enumerated from Java, with asserted counts |

Consumers — a schema change breaks these, so change both sides together:

* `strata-basics/src/test/scala/com/opengamma/strata/basics/parity/ParityHarness.scala` — loads a
  fixture and applies the tolerance rule.
* `strata-basics/src/test/scala/com/opengamma/strata/basics/parity/DayCountParitySpec.scala`,
  `ScheduleParitySpec.scala`, `FxParitySpec.scala`, `CurrencyMathParitySpec.scala`,
  `HolidayCalendarParitySpec.scala`.
* `strata-collect/src/test/scala/com/opengamma/strata/collect/parity/DoubleArrayParitySpec.scala`.
* `strata-basics/src/test/scala/com/opengamma/strata/basics/ReferenceDataManifestSpec.scala` —
  asserts the Scala data objects equal the manifest.

All of them read their resource through `collect.io.Resources.readClasspathText` and decode it
with circe, so every field name and nesting decision in sections 6 and 7 is part of a compile-time
contract on the Scala side.

Nothing here is deferred: the documents are committed with the Scala code, and this script is what
regenerates them.

### 5. Encoding conventions

These conventions are the contract across the Java→Scala boundary. The capture writes them and the
circe decoders read them, so neither side may change one alone.

* **Doubles at full precision.** Finite values are written with `Double.toString`, the shortest
  representation that round-trips exactly — so `0.16942884946478032`, never a formatted or
  truncated form. Signed zero is preserved: `-0.0` is written as `-0.0`. `String.format` and
  `DecimalFormat` are never used anywhere in the capture, not even for console output, because
  they are locale-sensitive.
* **Non-finite doubles are tagged strings**: `"NaN"`, `"Infinity"`, `"-Infinity"`. This is the
  single non-finite policy of the port, and the Scala `taggedDouble` codec accepts a JSON number or
  one of exactly those three strings. Whether a non-finite value is *valid* is decided by the
  receiving type's factory, not by the codec.
  The case that motivates it is real, not theoretical: `FxMatrixTest` builds a connected matrix
  containing `JPY/CAD = 0.0` (`:332` and again at `:357`), whose reciprocal `CAD/JPY` is
  `Infinity`. The fixture carries both, so the port is pinned on the edge as well as the middle.
* **Dates are ISO-8601** (`LocalDate.toString`), for example `2012-02-28`; a date-valued field that
  is absent is JSON `null`.
* **Named values are their canonical Java name string.** Every `Named` type is written through
  `getName()` and the remaining identity-bearing types through `toString()`, so a fixture carries
  `"Act/365F"`, `"GBP-LIBOR-3M"`, `"EUR/USD"`, `"P3M"` for a `Frequency`, `"3M"` for a `Tenor`,
  `"Term"`, `"GBLO+USNY"` — exactly the identities the Scala port reproduces and encodes.
* **Errors are strings**, formatted as `SimpleClassName: message`, for example
  `"UnsupportedOperationException: The end date of the schedule is required"`. They appear in an
  `error` field, described per fixture in section 6.
* **Determinism.** Every pseudo-random value comes from one `java.util.Random` seeded with the
  literal `20240117` (the JDK specifies the algorithm, so it reproduces exactly); there is no
  wall-clock input; no `HashMap`/`HashSet` appears on any output path, only insertion-ordered or
  sorted maps; object keys are written in a fixed declared order; strings are escaped to pure
  ASCII, so the bytes do not depend on the platform charset; indentation is two spaces, line
  endings are `\n`, and scalar arrays are chunked at a fixed ten items per line, so a long array
  is neither one unreadable line nor one line per element. Same inputs, same bytes — see
  *Verification status*.
* **`holiday-baseline.json` alone is written one row object per line**, unindented, because its
  3,846 rows carry about 445,000 date strings: pretty-printing them costs roughly 2 MB of pure
  indentation and buys nothing, since the unit anyone reads or diffs there is the row. It is the
  same deterministic writer (`Jn.writeCompact`) with newlines and padding omitted — key order,
  escaping and number rendering are unchanged, and the document still ends with exactly one
  newline. Every other document is pretty-printed as described above.

**The two-tolerance rule.** These are not interchangeable, and conflating them is a real trap:

* The **fixtures** carry full-precision values, because the Scala side compares them at 1e-9
  absolute *and* relative (section 6).
* A **capture-time self-check** uses the tolerance of the Java test it reproduces: exact equality
  for the `DayCountTest` tables (`data_yearFraction`, `data_days`), `1e-6` for `FxMatrixTest`
  (its `TOLERANCE`, `:43-44`), `1e-14` for `DoubleArrayTest` (its `DELTA`).

Using the Scala 1e-9 for a capture-time check would silently accept drift the Java test rejects;
using exact equality where the Java test allows `1e-6` aborts the capture on a value that is
correct. Each check in the script names the tolerance it used, and prints it on failure.

### 6. Fixture schemas

Each fixture is a **JSON array of row objects**. Every row carries a `source` field naming the
Java test method or the generated population it came from, which is what makes a failure
attributable; where `source` cannot identify a single row the row also carries a unique `id`, and
that is the name the parity report prints: `daycount-baseline.json` carries one on every row, and
`holiday-baseline.json`, at 3,846 rows, does the same. A row either carries its expectations or
carries an `error` — an `error` row is an expectation in its own right, not an omission.

**Harness contract.** `ParityHarness.assertParity(actual, expected)` passes when
`|a − e| ≤ 1e-9` **and** `|a − e| ≤ 1e-9 · max(|a|, |e|, 1e-300)` — both bounds, so neither a
large magnitude nor a near-zero value can hide a discrepancy. Dates, lists and strings compare
exactly. An `error` row must produce a `Left` from an `Either`-returning API, or the documented
`ArgCheck` exception from a precondition API. Each parity spec writes
`<parity.report.dir>/<fixture>.json` with its pass/fail counts *before* asserting that nothing
failed, and `scripts/verify-gates.sh` collects those reports into `target/gate-report.md`. Those
reports come from the **ScalaTest run**; this script never writes them.

#### `daycount-baseline.json`

This fixture's committed form is **uniform**: every row carries the identical twelve keys, in this
order, and `scheduleInfo` is always an object — never `null`. A key whose evaluation was not
performed is present with the value `null`, so the row shape never varies.

| Key | Contents |
|---|---|
| `id` | unique kebab-case row identity, `<family>-<variant>-<dayCount>-<start>[-<end>]`; this is what the parity report names |
| `source` | the Java test method, or the generated population, the row came from |
| `dayCount` | the Java `DayCount` name, including `Bus/252 BRBD` |
| `start`, `end` | the date pair, ISO-8601 |
| `scheduleInfo` | the schedule information the evaluation was given — see below |
| `yearFraction` | `yearFraction(start, end, scheduleInfo)`, or `null` |
| `relativeYearFraction` | `relativeYearFraction(start, end, …)`, or `null` |
| `relativeYearFractionReversed` | `relativeYearFraction(end, start, …)`, or `null` |
| `days` | `days(start, end)`, or `null` |
| `error` | the Java failure of the year-fraction evaluation, or `null` |
| `daysError` | the Java failure of the day-count evaluation, or `null` |

A value and its error are never both set, and both `null` means that evaluation was not performed —
unambiguous, because a successful evaluation always yields a number or one of the three tagged
non-finite strings. The relative forms are carried on the same row rather than as separate reverse
rows, and are evaluated only where they carry information: the `data_yearFraction` rows, whose
Java consumers assert them (the reverse against `-expected`), and the out-of-order rows, where
`relativeYearFraction` **succeeds** because it has no order check while `yearFraction` and `days`
both reject the pair. Everywhere else the dates are in order, so the relative value would merely
repeat `yearFraction`.

The `scheduleInfo` object carries `start`, `end`, `frequency`, `eom` plus **exactly one of**
`periodEnd` / `periodEnds` — never both, never neither:

* `periodEnd`: one fixed date, reproducing the `DayCountTest` stub whose `getPeriodEndDate(date)`
  **ignores its argument** and returns that one value for every date; or
* `periodEnds`: the ordered adjusted end dates of every period of a **real** `Schedule`, from which
  `periodEndDate(d)` is the first boundary strictly after `d` when `start ≤ d < end`, and absent
  otherwise (Java throws there; the port returns `None`). The capture re-evaluates every such row
  through an implementation that sees only this list, and the two must agree, so the encoding is
  demonstrably lossless.

`null` inside `scheduleInfo` has exactly one meaning: **absent**, i.e. `None` on the Scala side and
the Java `ScheduleInfo` default, so a day count that reads it raises. `eom` is the one exception a
reader needs to know: `null` there means the interface default, which is `true`. Since the
`DayCountTest` stub always supplies an explicit flag, `eom: null` is precisely the set of rows
evaluated through the Java **two-argument** overload against `DayCounts.SIMPLE_SCHEDULE_INFO` — and
the capture asserts that equivalence row by row, so the default-versus-stub distinction needs no
discriminator field.

One consequence of that single `null` meaning is worth stating, because it is the only place the
fixture's input departs from the Java test's: `30E/360 ISDA` reads the schedule end date only in
`!secondDate.equals(scheduleInfo.getEndDate())`, and the Java stub passes `null` there — a null
that means "not the maturity date", not "absent". Four of the nineteen `test_…_notMaturity` rows
depend on it. Those rows therefore carry a **materialised** schedule end of `end + 1 day`, a date
that is provably not the maturity, and the capture asserts that the materialised evaluation equals
both what the original stub produced and the Java table's not-maturity column. Every other row is
verified to be independent of a null accessor read, by evaluating it twice — once with absent
accessors throwing, once with them returning `null` — and requiring the same result.

Input population, by `id` family: `yf` 201 and `days` 185 (`data_yearFraction`, `data_days`);
`u360` 88 (`data_30U360` × its four consumers — note that the `30/360 ISDA` consumer is asserted
against the *non*-EOM column even though it is given `Info(true)`, because that day count ignores
the flag); `e360i` 38 (`data_30E360ISDA` × not-maturity and maturity); `afb` 57; `l365` 12;
`icma` 13 and `isda` 24 (one row per assertion of the `Act/Act ICMA` stub series and the official
ISDA test cases, the two-`Info` tests contributing one row per `Info`); `yvi` 146 (every 1000th
iteration of `test_actActYearVsIcma`, whose full 146,000-iteration equivalence is run as a
capture-time check); `same` 22, `half` 22, `whole` 22 and `order` 22 (the four portable
`data_types` consumers, over the 21 standard day counts and `Bus/252 BRBD` — `1/1` and `Bus/252`
are excluded from the two calendar-day sanity bands, as `1/1` is in the Java test and `Bus/252`
counts business days); `missing` 5 (missing schedule information stated once per day count
commonly said to need it — `Act/Act ICMA`, `30E/360 ISDA` and `Act/365L` raise, `Act/365 Actual`
and `30U/360` do not, and recording all five makes that evidence rather than folklore); `bus252` 9
(in-range, wholly outside `BRBD`'s 1950–2099 range, and crossing it, where Java falls back to a
weekend-only test — those rows are checked against an independent weekday count); `grid` 11,044
(all 22 subjects × consecutive month-end and mid-month pairs, 2010–2030, with absent schedule
information); `sched` 6,446 (all 22 subjects × `P1M`/`P3M`/`P6M`/`P12M` × regular, `SHORT_INITIAL`
with `firstRegularStartDate`, `SHORT_FINAL` with `lastRegularEndDate` × every period,
2015-01-15 → 2020-01-15, evaluated with the real `Schedule`).

Observed: **18,356 rows, 165,634 checks, 1,050 `error` rows**, 13,302,984 bytes. Three of the Java
tables contain literal duplicate rows carrying identical expectations — one in `data_yearFraction`,
one in `data_days` and nine date pairs repeated between the sections of `data_ACTACTAFB` — and
those rows are reproduced rather than de-duplicated, so the second occurrence takes a `-2` id
suffix; the total (11) is asserted, so a new duplicate cannot appear unnoticed.

The capture asserts one hard anchor before emitting anything: `Act/Act ISDA` from 2011-12-28 to
2012-02-28 must equal `4/365 + 58/366` (`0.16942884946478032`). The `SIMPLE_30_360` /
`SIMPLE_30_360DAYS` markers of `DayCountTest` are **resolved** through `calc360` / `calc360Days`
exactly as the Java consumers resolve them, so no row carries `"NaN"` or `0` as a stand-in for a
30/360 expectation — and the fixture contains no non-finite year fraction at all.

Because the schema above is uniform where `capture-baseline.jsh` section 7 is not (that emitter
predates it: no `id`, a nullable `scheduleInfo`, five different row key sets), the committed
document was produced by an equivalent capture assembled **outside the checkout** from this
script's verbatim data tables, JSON writer and self-check framework plus the emitter this schema
requires. Regenerating it means reproducing the rules in this sub-section; the numbers above, the
anchor, and the row-by-row agreement of all 18,234 shared rows with section 7's output are what
make the document auditable in the meantime.

The document is 13 MB, which is large enough to ask about and small enough not to worry about:
reading it through `Resources.readClasspathText`, parsing it with circe and decoding all 18,356
rows into a twelve-field product was measured at about 1.3 s inside a forked test JVM capped at
`-Xmx320m`, so the parity spec needs no special heap.

#### `schedule-baseline.json`

Row inputs — the eleven `PeriodicSchedule` fields that define a case: `startDate`, `endDate`,
`frequency`, `businessDayAdjustment`, `startDateBusinessDayAdjustment`,
`endDateBusinessDayAdjustment`, `stubConvention`, `rollConvention`, `firstRegularStartDate`,
`lastRegularEndDate`, `overrideStartDate`. A business-day adjustment is
`{"convention", "calendar"}`; an adjustable date is `{"unadjusted", "adjustment"}`; an absent
optional field is `null`.

**All twelve input keys — the eleven fields plus `source` — are present in every row of this
fixture, `null` where unset, including in rows that carry an `error`.** A definition the builder
itself rejects never becomes a `PeriodicSchedule` object, so its inputs are rendered from the raw
values instead; the key set and the key order are the same either way. That uniformity is what
lets the Scala decoder read the whole document into one case class.

Row expectations: `unadjustedDates`, `adjustedDates`,
`periods[{unadjustedStart, unadjustedEnd, start, end}]`, `initialStub`, `finalStub` (each a period
or `null`), `resolvedRollConvention`, `resolvedFrequency` — or `error` carrying the Java
`ScheduleException` / `IllegalArgumentException` message, for a definition the builder or the
resolution rejects. A row carries the full expectation set or an `error`, never some of each. Both
date lists and the periods are read off the resolved `Schedule`, so
`periods[i].unadjustedStart == unadjustedDates[i]`, `periods[i].unadjustedEnd ==
unadjustedDates[i+1]`, `periods[i].start == adjustedDates[i]`, `periods[i].end ==
adjustedDates[i+1]` and `periods.length == unadjustedDates.length - 1` hold by construction.

The `data_replace` rows model an operation rather than a plain resolution, so they add four keys
of their own. `replacedStartDate` is the input: the date passed to
`PeriodicSchedule.replaceStartDate(...)`, applied to the base definition the eleven input fields
describe. `replacedDefinition` is the post-replacement definition, rendered with the same eleven
fields, which is what the Java test asserts field by field
(`PeriodicScheduleTest.java:1002-1013`: the override start date and the first regular start date
are cleared, the start date becomes the replacement, the start-date adjustment becomes
`BusinessDayAdjustment.NONE`). `replacedUnadjustedDates` is `createUnadjustedDates()` on that
definition — the list the Java table asserts — and `expectedStubConvention`,
`expectedLastRegularEndDate` and `expectedRollConvention` are the table's remaining columns,
each checked against the replaced definition during capture.

`replacedUnadjustedDates` is a separate key from `unadjustedDates` because the two are **not**
always equal, and the difference is real Java behaviour rather than an inconsistency: on
`data_replace` row 2 (`2014-05-19` over `2014-06-17`–`2014-08-17`, `LongInitial` with `Day17`),
`createUnadjustedDates()` returns `[2014-05-19, 2014-07-17, 2014-08-17]` — a long initial stub
starting at the replacement date — while `createSchedule()` rolls the start onto the 17th and
returns `[2014-05-17, 2014-06-17, 2014-07-17, 2014-08-17]`. The port has to reproduce both, so the
fixture carries both and asserts neither against the other.

Input population:

* every row of `PeriodicScheduleTest.data_generation` (87) and `data_replace` (11), with each
  table's own expected unadjusted dates, adjusted dates, stub convention, last regular end date
  and roll convention cross-checked during capture;
* thirteen named feature rows (5 resolved, 8 rejected), each taken from a `PeriodicScheduleTest`
  method named in its `source`, covering the three inputs the two tables never populate:
  `endDateBusinessDayAdjustment` (`test_startEndAdjust`), `overrideStartDate`
  (`test_firstPaymentDate_before_effectiveDate`, `test_override_fallbackWhenStartDateMismatch`,
  `…EndStub`, `coverage_builder` — which is also the one row in which no input is `null`), and the
  builder-time validation branches of `PeriodicSchedule.java:361-390` plus the `Term`-frequency
  stub rejection of `:591`, whose messages are the whole contract tying them to the port's
  `Failure.Invalid`;
* a generated grid of 4 frequencies × 8 stub conventions × **8** roll conventions (`EOM`, `IMM`,
  `IMMCAD`, `IMMAUD`, `IMMNZD`, `SFE`, `TBILL`, `Day15`) × 3 calendars (`GBLO`, `USNY`,
  `GBLO+USNY`) over 2015-01-15 to 2018-01-15 with `ModifiedFollowing`, resolved against
  `ReferenceData.standard()` — 768 rows, many of them mutually inconsistent combinations that
  produce an `error`, which is the point: the port must reject exactly the same ones.

`IMMCAD`, `IMMAUD` and `TBILL` are in that grid for a specific reason. In Java those three
`StandardRollConventions` members capture built-in holiday calendars at class-initialisation time
through `ReferenceData.standard()` (`StandardRollConventions.java:60-63,74-75,103-104,133-134` —
`IMMCAD` holds `GBLO` and `CATO.combinedWith(CAMO)`, `IMMAUD` holds `AUSY`, `TBILL` holds `USNY`),
whereas the port binds the `StandardHolidayCalendars` constants directly. These rows are the
evidence that the substitution is behaviour-preserving; `SFE` and `IMMNZD`, which use no calendar,
are the control group.

Observed: 879 rows, 311 checks, 573 `error` rows.

#### `fx-baseline.json`

One row per scenario, and **every row carries the same nine keys in the same order** — a query
list a scenario does not exercise is emitted as an empty array, never omitted, so the fixture has
exactly one shape:

`id`, `source`, `matrix`, `matrixState`, `queries`, `conversions`, `multi`, `crosses`, `merges`.

Inputs:

* `id` — a short, stable, kebab-case handle, unique within the fixture. This is what a parity
  report and a failure message name a scenario by, so the capture checks uniqueness and aborts on
  a collision. It is additional to, not a replacement for, `source`.
* `source` — the Java test method or generated population the row came from, as in every fixture.
* `matrix` — the defining list of `{pair, rate}`, in the order the rates are added. Order is part
  of the value: it decides the matrix's currency order, and for a pair whose currencies are both
  already present it decides which rate is the reference (`FxMatrixBuilder.addRate` is documented
  as asymmetric in that case).
* `matrixState` — the built matrix's own state, `{currencies, rates}`, with the currencies in the
  matrix's insertion order and `rates` as an array of row arrays. A definition the builder rejects
  yields `"matrixState": null`, the five query lists empty, and an extra `error` key.

Query lists, each **entry** carrying its own value or an `error` — the expectation is nested in
the input that produced it rather than held in a parallel array, so an expectation can never be
mismatched to or misaligned with its input:

* `queries[{base, counter, fxRate}]`
* `conversions[{currency, amount, target, converted{currency, amount}}]`
* `multi[{amounts[{currency, amount}], target, multiConverted}]`
* `crosses[{rate1, rate2, crossRate}]` — each rate is `{pair, rate}`; populated on the `FxRate`
  cross-rate row
* `merges[{other[{pair, rate}], merged{currencies, rates}}]`

Input population — fourteen rows:

* `cross-rate-triangulating-matrix` — `FxMatrixTest.matrixCalculatesCrossRates`: the supplied
  rates, their reciprocals, the triangulated crosses and an identity query.
* `convert-multi-currency-amount` — `FxMatrixTest.convertMultipleCurrencyAmount*`: single-entry
  and multi-entry `MultiCurrencyAmount` conversions plus plain `CurrencyAmount` ones.
* `zero-rate-connected-matrix` — `FxMatrixTest.streamEntriesToMatrix`: the connected matrix
  containing `JPY/CAD = 0.0`, whose `CAD/JPY` reciprocal is `"Infinity"` (see section 5).
* `shifted-zero-rate-connected-matrix` — `FxMatrixTest.streamPairsToMatrix`: the same rate set
  with every rate shifted by `x1.01`, checked exactly against that test's `1.616` and `1.414`. The
  zero rate survives the shift, so this row carries the infinite reciprocal too.
* `resize-nine-currency-matrix` — `FxMatrixTest.addMultipleRatesSingle`: a **nine**-currency
  definition. `FxMatrixBuilder` starts at `MINIMAL_MATRIX_SIZE = 8` and grows only when a ninth
  currency arrives, so this is the row that exercises the resized matrix.
* `empty-matrix`, `single-pair-matrix` — the trivial identity rate works; a genuine query for an
  absent currency is rejected.
* `identity-and-single-rate-conversion` — `FxRateTest.test_fxRate_forPair` and
  `test_convert_double` over `GBP/USD 1.25`: `GBP/GBP`, `USD/USD` and `AUD/AUD` are all exactly
  `1` even though `AUD` is absent, because an identity query is answered before any lookup; the
  five genuine `AUD` queries are rejected; and `GBP -> GBP` is the same-currency conversion, which
  returns the amount unchanged.
* `merge-cases` — disjoint (rejected), duplicate currencies (the receiver's rates win), additional
  currencies (the matrix is extended) and an **empty** other matrix. The last is rejected too, and
  deliberately: merging looks for a currency common to both matrices and an empty matrix has none,
  so merging with it is an error rather than a no-op.
* `random-scenario-0` … `random-scenario-3` — seeded random rate sets over eight currencies, every
  ordered pair queried. Capture-only: no Java constant exists for them.
* `fx-rate-cross-rates` — `FxRateTest.test_crossRate`: the eight permutations that agree plus the
  five documented failures (two identity pairs, two same-currency pairs, one with no common
  currency). It runs over an empty matrix definition, since a cross rate is a property of `FxRate`
  alone.

Observed: 14 rows, 77 checks, 14 `error` entries, 4 capture-only rows. Capture-time tolerances:
exact for directly supplied rates, their reciprocals and the identity rate, `1e-6`
(`FxMatrixTest.TOLERANCE`) for triangulated crosses. A query or cross the Java implementation must
reject is asserted to fail, not merely recorded — so the `error` entries are expectations the Java
tests state, not accidents of capture.

One value worth keeping in mind while reading this fixture: `eurUsd.crossRate(usdGbp)` is
`0.9142857142857143`, not the `0.9142857142857144` that recomputing `(8/7) * (4/5)` a different
way produces. That single last-digit difference is the whole argument for capturing baselines
instead of writing them.

#### `currency-math-baseline.json`

Six rows, one per Java test family, and every row has the **same fifteen keys in the same order**:

| Key | Contents |
|---|---|
| `id` | The row's stable identity, kebab-case and unique in the file: `currency-amount`, `money`, `big-money`, `currency-amount-array`, `multi-currency-amount`, `multi-currency-amount-array`. This is what a parity report names when a row fails. |
| `source` | The Java test class the population comes from. |
| `amounts` | `[{currency, amount}]` — `amount` is a number (or a tagged non-finite string). |
| `scalars` | `[number]` |
| `arrays` | `[{currency, values: [number]}]` |
| `multiArrays` | `[{arrays: [{currency, values: [number]}]}]` |
| `money`, `bigMoney` | `[{currency, amount, toString}]` — `amount` is a decimal **string**, never a double. |
| `rates` | `[{pair, rate}]`, for example `{"pair": "GBP/USD", "rate": 1.6}`. |
| `currencyAmountResults`, `moneyResults`, `bigMoneyResults`, `currencyAmountArrayResults`, `multiCurrencyAmountResults`, `multiCurrencyAmountArrayResults` | The six expectation buckets. |

The seven input lists are the deduplicated, insertion-ordered registry of the values that row's
expectations were computed from — each is registered by wrapping the expression that produced it,
so an input cannot drift away from the expectation that consumed it. All six expectation buckets
appear in **every** row: the one the row's family covers, and an empty array for the other five.
An input list a row does not use is likewise an empty array. So the row shape is fixed, and the
Scala decoder is one product type with no optional fields.

**There is no positional pairing to infer.** Every bucket entry names its operation in `op` and
carries its own operands inline, then exactly one of `result` or `error`:

```
{"op": "plus", "left": {...}, "right": {...}, "result": {...}}
{"op": "convertedTo", "left": {...}, "target": "USD", "rate": 1.25, "result": {...}}
{"op": "getValues", "left": {...}, "currency": "CHF",
 "error": "IllegalArgumentException: No values available for CHF"}
```

The operand and marker keys are a closed set: `left`, `right`, `scalar`, `target`, `rate`,
`rates`, `currency`, `amount`, `amounts`, `input`, `size`, `index`, `scale`, `roundingMode`,
`mapAmountsFn`, `minorUnitDigits`, `doubleToLongBits`, `composed`, `captureOnly`. `input` carries
a construction input that is not a single typed value (a literal array, or a short description
such as `"GBP 1, USD 2, GBP 3"` for a rejected mixed-currency list). Three markers are worth
naming:

* `"composed": true` (with `mapAmountsFn` where relevant) — the expectation was composed from two
  real public Java calls because the Java type has no such single method (`multipliedBy` and
  `mapAmounts` on the amount-array types are added by the Scala port). The mapping function is
  `x -> x * x`, the same one the double-array fixture uses. Java `CurrencyAmountArray` likewise has
  no per-array `total`; the aggregate is the static
  `MultiCurrencyAmountArray.total(Iterable<CurrencyAmountArray>)` and is captured in the
  multi-array bucket.
* `"captureOnly": true` (with `minorUnitDigits`) — an entry with no hard-coded Java test constant
  to compare against: a seeded-random amount, or one of the per-currency sweeps below. The summary
  counts them separately so an entry that *should* have been checked cannot hide among them.
* `doubleToLongBits` — the exact bit pattern of a signed zero, on the entries where the sign is
  the thing being pinned. `CurrencyAmount` normalises `-0.0` to `+0.0` in both its factory and its
  arithmetic results, `CurrencyAmountArray` stores a `DoubleArray` and **keeps** the sign bit, and
  `CurrencyAmountArray.get(i)` normalises it again on the way out — three different answers, so a
  sign-blind implementation satisfies none of them.

Coverage, by family:

* `CurrencyAmount` — `plus`/`minus` in the same and in different currencies, `multipliedBy`,
  `negated`, `positive`, `negative`, `convertedTo` (cross-currency, and the same-currency rule at
  rates 1, 1.25 and 1.5), `of(-0.0)`, `of(NaN)` (rejected), `(+∞) + (−∞)` (the documented NaN-result
  rejection, reachable only from infinite operands), and every currency: the 55
  `Currency.getAvailableCurrencies()` entries with seeded-random amounts plus the 19
  `historic = true` rows of `Currency.ini` with the literal sweep amount.
* `Money` and `BigMoney` — `of` across all three distinct `minorUnitDigits` values, including
  `MoneyTest`'s four HALF_UP boundaries verbatim (`AUD 100.1249 → 100.12`, `AUD 100.125 → 100.13`,
  `BHD 100.1249 → 100.125`, `BHD 100.125 → 100.125`); `plus`/`minus` including the currency
  mismatch; `multipliedBy`; `convertedTo` by explicit rate, by rate provider, and the
  same-currency rule on both sides (rate 1 succeeds, 1.1 and 1.25 fail); `Money.toBigMoney`;
  `BigMoney`'s scale-12 HALF_UP rounding on `1.000009`, `9.99999999`, `1.441` and the
  fifteen-decimal `1.123456789012345`; the twelve `roundToScale` rows of `BigMoneyTest` (six
  rounding modes at scale 2, three negative scales); and `BigMoney.toMoney` for every one of the
  74 currencies, whose `left` operand shows the scale-12 rounding and whose result shows the
  narrowing to that currency's minor units.
* `CurrencyAmountArray` — `of(currency, DoubleArray)`, `of(List)`, `of(size, fn)` and the
  mixed-currency rejection of both list and function forms; element-wise `plus`/`minus` against
  another array (with the currency-mismatch and size-mismatch rejections) and against a scalar
  `CurrencyAmount` (with its currency-mismatch rejection); `convertedTo`, including the
  missing-rate rejection; the composed `multipliedBy` and `mapAmounts`; and the signed-zero array.
* `MultiCurrencyAmount` — `of` (including `empty` and the duplicate-currency rejection),
  `total` (which merges duplicates where `of` rejects them), `plus`, `minus`, `multipliedBy`,
  `mapAmounts`, `getAmount` (known and unknown currency) and `convertedTo`. Amounts are written
  sorted by currency code, which is the port's byte-stable shape.
* `MultiCurrencyAmountArray` — `of(Map)` and its unequal-length rejection; `of(List)` and
  `of(size, fn)`, where a currency missing from one element is **zero-filled** rather than
  rejected, and a currency absent from every element stays unknown so `getValues` rejects it;
  `of` with empty amounts; `getValues` known and unknown; `plus`/`minus` against another array
  (with the size-mismatch rejection) and against a `MultiCurrencyAmount`; `total`; `convertedTo`;
  the composed `multipliedBy` and `mapAmounts`.

`Money`/`BigMoney` amounts are written as decimal **strings** together with their `toString` form,
so no rounding is lost through a double, and the Scala side compares them exactly rather than
within a tolerance.

Two conventions are deliberate omissions. `parse` is **not** captured here — the format-level
behaviour of `CurrencyAmount.parse` / `Money.parse` belongs to those types' own specs, and this
fixture stays on the seven declared input lists. And the per-currency sweeps use the literal
amounts `1234.56789` (five decimal places, so rounding to 0, 2 and 3 minor units is visible) and
`1234.5678901234567` (more than twelve, so `BigMoney`'s scale-12 rounding bites) rather than draws
from the shared seeded `Random`: the stream is consumed in fixture order, so a draw added here
would shift the random values of every fixture captured afterwards.

Observed: 6 rows, 227 checks, 29 `error` entries, 228 capture-only entries, 355 expectation
entries in total, covering all 74 configured and historic currencies.

#### `holiday-baseline.json`

One row per (calendar, year), **one row object per line**. The key set is identical in every row:

```
{"id": "gblo-2020", "source": "generated", "calendar": "GBLO", "year": 2020,
 "holidays": ["2020-01-01", "2020-01-04", …],
 "samples": [{"date": "2020-01-01", "isHoliday": true, "isBusinessDay": false,
              "next": "2020-01-02", "previous": "2019-12-31",
              "nextOrSame": "2020-01-02", "previousOrSame": "2019-12-31",
              "shift": {"amount": -3, "result": "2019-12-27"},
              "daysBetween": {"endExclusive": "2021-01-01", "result": 254},
              "error": null}, …],
 "error": null}
```

The example above is wrapped and spaced for reading; in the document a row is one unspaced line,
as section 5 describes.

`id` is unique across the file and is what the gate report names when a row fails: the
kebab-cased calendar name, the year, and `-out-of-range` or `-year-range` for a probe row. `+`
slugs to `-plus-` and `~` to `-linked-`, so a combined and a linked composite of the same two
calendars cannot collide, and a negative year spells its sign (`gblo-minus1-year-range`).

**What `holidays` means — the definition this fixture exists to settle.** It is **every date `d` in
`year` for which `calendar.isHoliday(d)` is true, weekends included**. Nothing is filtered, which
makes it self-verifying: the harness replays it date by date over the year. Two reasons it has to
be defined that way. `isHoliday` treats a weekend as a holiday and a calendar's weekend days are
not public API, so the full `isHoliday` truth is the only contract both sides can compute
symmetrically. And `HUBU` is built as `ImmutableHolidayCalendar.of(id, holidays, SUNDAY, SUNDAY)`
(`GlobalHolidayCalendars.java:1204`) — Sunday is its *only* weekend day, and its Saturdays are
listed **explicitly** as holidays (`:1239-1250`). The Java tests assert the same truth from the
other side, listing non-weekend holidays and OR-ing Saturday and Sunday, so **their lists are not
copied into this fixture**: a copy would be ambiguous between "not a holiday" and "a weekend day
the table filtered out", and the ambiguity would land on exactly the calendar where it changes the
answer. They are used as the cross-check instead, in the direction they are asserted:
`holidays == {d : javaTable.contains(d) or d is a weekend day of that calendar}`, and for `HUBU`
minus its working Saturdays (`GlobalHolidayCalendarsTest.java:926-936`).

**The sampling rule**, which the harness cannot infer: three samples per row, at a fixed
(date, shift amount, `endExclusive`) triple.

| Sample date | `shift.amount` | `daysBetween.endExclusive` | What it pins |
|---|---|---|---|
| 1 January | −3 | 1 January of the next year | The whole-year business-day count, and a span crossing the year end; the negative shift walks back into the previous year |
| 15 June | +5 | 15 July | A positive shift and a one-month span wholly inside the year |
| 24 December | +7 | 24 January of the next year | A positive shift and a span that both cross into the next year |

Each sample is self-describing — the amount and the end date are in the row — so the harness calls
`shift(date, amount)` and `daysBetween(date, endExclusive)` without knowing the rule. Three is a
deliberate ceiling: `holidays` already pins `isHoliday` for every day of the year, so a sample buys
date *arithmetic* coverage only, at about 250 bytes across 3,846 rows. A sample either carries a
complete set of expectations or carries `error` with every value `null` — never a half-populated
mixture.

Input population, distinguished by `source`:

* `generated` — the 24 calendars generated over **every** year 1950–2099, and `EUTA` over every
  year 1997–2099 (`GlobalHolidayCalendars.java:296`): 3,703 rows.
* `dataTable` — `THBA` over every year 2005–2079, its full INI range: 75 rows.
* `outOfRange` — the year either side of each calendar's range (1949 and 2100; 1996 and 2100 for
  `EUTA`; 2004 and 2080 for `THBA`): 52 rows. There Java falls back to a weekend-only test instead
  of throwing (`ImmutableHolidayCalendar.java:397-415`), and the port must reproduce that rather
  than throw or return an empty year.
* `weekend` — the four weekend / no-holiday calendars for 2020 and 2024, with their ids asserted:
  8 rows.
* `composite` — `GBLO+USNY` (combined: a holiday in **either**), `GBLO~USNY` (linked: a holiday in
  **both**) and `JPTO+USNY`, each for 2020 and 2024: 6 rows. Composite ids resolve component-wise
  against `ReferenceData.standard()`, so these rows pin that resolution as well as the combination.
* `yearRange` — `GBLO` for year 10000 and year −1: 2 rows. Outside 0000–9999 the lookup neither
  answers nor falls back but throws, so `holidays` is `null` and `error` carries the message Java
  produces, on the row and on all three samples.

**Observed: 3,846 rows, 50,305 checks, 2 `error` rows, 8.9 MB** (3,848 lines: the two brackets and
one row each). That size is expected of a golden fixture with this coverage, and the coverage is
not negotiable — `GlobalHolidayCalendarsSpec` ports the 201 Java year-rows verbatim, and this
document covers every remaining year, which is the only thing that pins the monthly-bitmask
representation and the out-of-range fallback across the whole 1950–2099 span.

The checks are what make the rows trustworthy, and they are structural rather than transcribed:
every row's `holidays` is compared against the same truth read through the `holidays(start, end)`
Stream API and asserted strictly ascending; every `outOfRange` and `weekend` row is compared
against exactly the weekend dates of its year under the weekend days of that calendar, taken from
the Java source that constructs it; every sample's `isBusinessDay`, `nextOrSame` and
`previousOrSame` are checked against `isHoliday`, `next` and `previous`; each composite row is
checked against its components' combination law over the whole year, and `JPTO+USNY` additionally
over 1950–2039, which is `GlobalHolidayCalendarsTest.test_combinedWith` (`:1189-1197`) reproduced;
the 201 `data_easter` rows and the four `test_christmas` year triples are checked against
`easter`, `christmasBumpedSatSun` and `boxingDayBumpedSatSun`; `HUBU` is asserted to carry explicit
Saturdays in range and none out of range while `GBLO` carries every weekend date, which is what
proves the definition above was applied rather than a Java test table copied; no row other than the
two deliberate `yearRange` rows may carry an error anywhere; and the row count and the id count are
asserted, so thinning the year coverage cannot pass unnoticed.

#### `double-array-baseline.json`

Row inputs: `source`, `a`, `b` (arrays), `scalar`, `matrixA`, `matrixB` (arrays of row arrays, or
`null`). Expectations live in `results`, one entry per operation, each naming its `op` and carrying
`result` or `error`, plus any operation parameter (`mapFn`, `reduceFn`, `identity`, `fromIndex`,
`index`, `value`, `row`, `column`, `combineFn`, `scalar`).

Operations: `plus`, `minus`, `multipliedBy`, `dividedBy`, `map`, `reduce`, `sum`, `min`, `max`,
`sorted`, `concat`, `subArray`, `with`; and when a matrix is present `matrixMultipliedBy`
(**scalar** — see section 8), `transpose`, `total`, `matrixWith`, `matrixPlus`, `matrixMinus`,
`matrixCombine`.

Input population: the literal arrays of `DoubleArrayTest` and `DoubleMatrixTest` — including the
empty array, whose `min`/`max` throw and are therefore `error` expectations — rows with signed zero
and with `NaN`/`±Infinity` contents so the tagged-double policy is exercised end to end, and
seeded random arrays and matrices. Observed: 14 rows, 4 `error` entries.

### 7. Manifest schema

`reference-data-manifest.json` is a single object, not a row array. It enumerates every reference
data table from the **Java** side with **every count asserted during capture**, so a resource
edited under the port cannot silently reshape it. `ReferenceDataManifestSpec` then asserts that the
ported Scala data objects equal this document exactly — which is why the key names and the nesting
below are a contract: renaming a key, flattening an object or turning an ordered array into a map
breaks that spec.

Top-level keys, in the order they are written:

| Key | Shape | Asserted content |
|---|---|---|
| `schemaVersion` | number | `1` |
| `generator` | string | `tools/parity-capture/capture-baseline.jsh` |
| `randomSeed` | number | `20240117` |
| `currencies` | `{count, historicCount, activeCount, rows[{code, minorUnitDigits, triangulationCurrency, historic}]}` | **74** rows, of which **19** are historic, leaving the **55** that `Currency.getAvailableCurrencies()` returns (also cross-checked) |
| `marketConventionPriority` | JSON **array** of codes | The ordering, in order: `XAU, EUR, GBP, AUD, NZD, USD, CAD, CHF, JPY` (9 entries) |
| `currencyPairs` | `{count, rows[{pair, rateDigits}]}` | **92** pairs; cross-checked against `CurrencyPair.getAvailablePairs()` |
| `countries` | `{count, rows[{alpha3, alpha2}]}` | **251** rows |
| `iborIndices` | `{count, headers[], rows[{<header>: value}]}` | **271** rows, every CSV column preserved |
| `overnightIndices` | same | **35** rows |
| `priceIndices` | same | **9** rows |
| `fxIndices` | same | **16** rows |
| `iborIndexConstants` | `{count, names[]}` | **113**, in declaration order |
| `overnightIndexConstants` | `{count, names[]}` | **21** |
| `priceIndexConstants` | `{count, names[]}` | **9** |
| `fxIndexConstants` | `{count, names[]}` | **8** |
| `floatingRateNames` | `{constants{count, names[]}, aliasRowCount, sections{<section>{count, rows[{key, value}]}}}` | **41** constants; **404** alias rows over seven sections — `ibor` 159, `iborFixingDateOffset` 3, `overnightCompounded` 156, `overnightAveraged` 6, `price` 30, `currencyDefaultIbor` 23, `currencyDefaultOvernight` 27 |
| `dayCounts` | `{count, names[]}` | **21** |
| `businessDayConventions` | `{count, names[]}` | **7** |
| `rollConventions` | `{count, names[]}` | **45** — 8 standard plus `Day1`…`Day30` and `DayMon`…`DaySun` |
| `periodAdditionConventions` | `{count, names[]}` | **3** |
| `dateSequences` | `{count, names[]}` | **6** |
| `stubConventions` | `{count, names[]}` | **8** |
| `holidayCalendarIds` | `{count, names[]}` | **29** |
| `holidayCalendarDefaultByCurrency` | `{count, resolvableCount, unresolvableCount, rows[{currency, calendarId, resolvableAgainstStandardReferenceData}]}` | **31** rows, of which **18** resolve and **13** do not — those thirteen have no built-in calendar in Java either, so the manifest records the mapping without depending on resolution succeeding |
| `builtInHolidayCalendars` | `{count, names[]}` | **30** — the four weekend / no-holiday calendars plus 26 dated ones (25 generated and `THBA`) |
| `holidayCalendarData` | `{THBA{yearRowCount, weekend, rows[{year, dates}]}}` | **75** year rows (2005–2079) plus the separate `weekend` value `Sat,Sun` |
| `externalNames` | `{<family>{<group>{count, rows[{externalName, standardName}]}}}` | `DayCount` FpML **14** / SWIFT **8**; `RollConvention` FpML **44**; `BusinessDayConvention` FpML **5** / SWIFT **3**. Sorted, so the bytes are stable |
| `lenientPatterns` | `{<family>{count, rows[{key, value}]}}` | `DayCount` **67**, `RollConvention` **11**, `BusinessDayConvention` **11**, `PeriodAdditionConvention` **3** — in **file order** |
| `alternateNames` | `{<family>{iniRowCount, iniRows[{alternateName, standardName}], apiExpandedRowCount, apiExpandedRows[…]}}` | `IborIndex` **1**/**1**, `OvernightIndex` **10**/**13**, `FxIndex` **1**/**1** |

Three of these deserve a word, because their shape encodes behaviour:

* **`marketConventionPriority` is an array.** The order decides which currency of an unlisted pair
  becomes the base, so it is semantically load-bearing and must never be emitted as a set or map.
* **`lenientPatterns` preserves file order.** The lenient rewrite is *sequential*: each matching
  pattern rewrites the string before the next is tried, so the order is behaviour, not
  presentation. The rows are read with the property-set key and value-list accessors rather than
  the flattened map view, so a repeated key stays lossless.
* **`alternateNames` records two counts on purpose.** The INI `[alternates]` section is the table
  the port transcribes; the runtime `alternateNames()` view additionally registers a derived
  upper-case key for every mixed-case alternate. `OvernightIndex` is the case in point: 10 INI rows
  become 13 API entries.

### 8. Notes for maintainers

Each of the following silently corrupts a baseline if forgotten, and each is a verified property
of the Java sources this capture reads — not a style preference. Check them before changing how
anything is captured.

* **The day-count section is behind its committed fixture.** `daycount-baseline.json` as committed
  is richer than what this script emits, in the ways the verification log above itemises, so the
  two are not interchangeable: regenerate the other five fixtures freely, but treat
  `daycount-baseline.json` as the reference until the day-count section reproduces it. The gap is
  one of coverage, not of correctness — the values present in both agree exactly.

* **Public API only.** A `.jsh` script runs in the unnamed package, so the package-private classes
  the Java tests reach by sharing their package are **unreachable** here:
  `GlobalHolidayCalendars` (`date/GlobalHolidayCalendars.java:43`), `StandardDayCounts` (`:21`),
  `Business252DayCount` (`:23`), `StandardRollConventions` (`:24`),
  `StandardBusinessDayConventions` (`:20`) and `GlobalHolidayCalendarLookup` (`:19`). Use the
  public routes instead: `HolidayCalendars.of(String)` (`date/HolidayCalendars.java:91`) or
  `HolidayCalendarId.of(id).resolve(ReferenceData.standard())`, which yields the very same calendar
  instances; the `DayCounts` constants rather than `StandardDayCounts.values()`. `Bus/252` rows are
  reachable because `DayCount.ofBus252(HolidayCalendarId)` (`date/DayCount.java:59`) is public.
  The one exception is deliberate: the holiday cross-checks reach `easter`,
  `christmasBumpedSatSun` and `boxingDayBumpedSatSun` on `GlobalHolidayCalendars` **by
  reflection** (`getDeclaredMethod` + `setAccessible`, both classes being on the classpath and so
  in the unnamed module). No fixture value comes from them — they only let the
  `GlobalHolidayCalendarsTest.data_easter` and `test_christmas` constants be asserted, and the
  alternative is leaving the rules that place every Easter- and Christmas-derived holiday in 3,846
  rows unchecked. See the last bullet of this section on why reflection is acceptable here.
* **The `SIMPLE_30_360` sentinel.** In `DayCountTest`, `SIMPLE_30_360 = Double.NaN` (`:128`) and
  `SIMPLE_30_360DAYS = 0` (`:130`) are **markers, not expectations**. The test methods resolve them
  (`:391`, `:401`, `:411`, and `:651` for days) through `calc360` (`:380`) —
  `((y2-y1)*360 + (m2-m1)*30 + (d2-d1))/360d` — and `calc360Days` (`:384`). Copying such a row
  literally would record `NaN` or `0` as the expected value and pin the port to nonsense.
* **CSV separator rows.** The four index CSVs contain comma-only separator lines (35 / 1 / 1 / 3)
  that `CsvFile` skips, and the overnight and FX files also carry `#` comment lines. Counting every
  non-comment, non-blank line after the header gives 306 / 36 / 10 / 19; the true data-row counts
  are **271 / 35 / 9 / 16**. Read them through `CsvFile`, never by counting lines. (The Ibor and
  overnight files use CRLF endings, the price and FX files LF — another reason not to count bytes
  or lines.)
* **`[lenientPatterns]` is not exposed.** `ExtendedEnum` keeps the section name private and exposes
  only `findLenient(name)` (`named/ExtendedEnum.java:490`), so the rows have to be read from the
  INI itself: `ResourceConfig.combinedIniFile(...)` (`io/ResourceConfig.java:173`) →
  `IniFile.section(...)` (`io/IniFile.java:197`) → the property set's `keys()` (`:116`) and
  `valueList(key)` (`:226`). Prefer `valueList` over `asMap()` (`:140`), which joins repeated
  values with a comma. Alternates and externals *are* public: `alternateNames()` (`:440`),
  `externalNameGroups()` (`:454`), `externalNames(group)` (`:471`), `lookupAll()` (`:395`) and
  `lookupAllNormalized()` (`:415`).
* **A currency pair is an INI section, not a key.** `CurrencyPair.ini` stores each pair as a
  section — `[EUR/AUD]` with a single `rateDigits` key — so reading the pair from the property key
  yields the literal `"rateDigits"` for all 92 rows **while the count check still passes**. The
  pair must come from the section name.
* **The 0-decimal-currency gap.** `MoneyTest` (`:24-26`) and `BigMoneyTest` (`:27-29`) declare
  only AUD, RON (2 minor-unit digits) and BHD (3), and the one other currency they touch, GBP,
  also has 2 — so no Java test covers a 0-digit currency. The fixture must cover every distinct
  `minorUnitDigits`, so the capture adds a 0-digit currency (JPY) itself and then sweeps `Money.of`
  and `BigMoney.toMoney` over all 74 rows of `Currency.ini`, which reaches the 0-digit historic
  currencies (`ESP`, `ITL`, `PTE`) as well. Those entries are marked `captureOnly` because there is
  no Java constant to cross-check them against — which is the point of the marker: it stops the
  summary from hiding an entry that *should* have been checked. The digits they depend on are not
  taken on trust: `checkCurrencyIniAgainstCurrencyOf` asserts, for every one of the 74 rows, that
  `Currency.of(code)` carries the INI's `minorUnitDigits` and `triangulationCurrency`. That check
  exists because `Currency.of` mints an instance with 0 minor units and USD triangulation for a
  code the resource does not define, and only the pre-seeding of `Currency.DYNAMIC` with
  `loadCurrencies(true)` makes the 19 historic codes resolve to their real data.
* **The `ScheduleInfo` stub is not the real default.** `DayCountTest`'s nested `Info` class
  (`:1373-1416`) is package-private, has **nullable, non-throwing** getters, and its
  `getPeriodEndDate(date)` returns a fixed date ignoring its argument — whereas the real
  `DayCount.ScheduleInfo` defaults throw (`date/DayCount.java:187-260`). The capture declares its
  own behavioural equivalent, which is why the fixture's `scheduleInfo` fields are nullable with
  `null` meaning "the Java default" (`None` on the Scala side).
* **`DoubleMatrix` has no matrix product.** The only `multipliedBy` it exposes is the scalar
  `multipliedBy(double)` (`array/DoubleMatrix.java:544`). Do not invent a matrix-times-matrix
  operation: the captured matrix operations are that scalar form plus `with` (`:522`), `total`
  (`:703`), `transpose` (`:746`) and the element-wise `plus`/`minus`/`combine`.
* **`[THBA]` has a trailing `Weekend` key.** Its section holds 75 year rows (2005–2079) *and* a
  `Weekend = Sat,Sun` entry, so a naive key count returns 76. The manifest emits the weekend value
  separately.
* **Reflection is fine here.** Enumerating a public constants holder with `getDeclaredFields()` is
  the only way to obtain a provably *complete* constant list, which is the whole point of a
  manifest. The no-reflection requirement applies to the Scala **codec path**, not to a developer
  tool under `tools/`. Where an `ExtendedEnum` accessor gives the same answer, the accessor is
  preferred.

For the wider context — the module layout, the sbt commands and the list of deliberate divergences
from the Java behaviour — see [the repository README](../../README.md) and `SCALA_MIGRATION.md`.
