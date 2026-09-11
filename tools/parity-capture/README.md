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
* The capture ran in about 31 s at `-R-Xmx900m`, reported `all checks passed` and wrote all seven
  documents, exiting 0. Observed totals: 22,800 rows and 147,446 checks.
* It was run three times — twice with the zero-rebuild classpath (the six local-repository
  entries) and once with the route-(b) classpath assembled from the `package` output — and the
  three sets of documents were **byte-identical** (`diff -r`), which is the determinism contract
  of section 5 demonstrated rather than asserted.
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
| `strata-basics/src/test/resources/parity/holiday-baseline.json` | Per-calendar, per-year holiday sets and date-arithmetic probes |
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
  endings are `\n`, and scalar arrays are chunked at a fixed ten items per line, so a long holiday
  array is neither one unreadable line nor one line per element. Same inputs, same bytes — see
  *Verification status*.

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
attributable. A row either carries its expectations or carries an `error` — an `error` row is an
expectation in its own right, not an omission.

**Harness contract.** `ParityHarness.assertParity(actual, expected)` passes when
`|a − e| ≤ 1e-9` **and** `|a − e| ≤ 1e-9 · max(|a|, |e|, 1e-300)` — both bounds, so neither a
large magnitude nor a near-zero value can hide a discrepancy. Dates, lists and strings compare
exactly. An `error` row must produce a `Left` from an `Either`-returning API, or the documented
`ArgCheck` exception from a precondition API. Each parity spec writes
`<parity.report.dir>/<fixture>.json` with its pass/fail counts *before* asserting that nothing
failed, and `scripts/verify-gates.sh` collects those reports into `target/gate-report.md`. Those
reports come from the **ScalaTest run**; this script never writes them.

#### `daycount-baseline.json`

Row inputs: `source`, `dayCount` (name), `start`, `end`, `scheduleInfo` (`null`, or an object).

The `scheduleInfo` object carries `start`, `end`, `frequency`, `eom` — each nullable, where `null`
means "the Java default for that accessor" (and `None` on the Scala side) — plus **exactly one of**

* `periodEnd`: a single fixed date, reproducing the `DayCountTest` stub whose
  `getPeriodEndDate(date)` ignores its argument; or
* `periodEnds`: the ordered list of the adjusted end dates of every period of a real `Schedule`,
  from which `periodEndDate(d)` is the first boundary after `d` when `start ≤ d < end` and absent
  otherwise.

Row expectations: `yearFraction`, `relativeYearFraction`, `relativeYearFractionReversed`, `days`
— whichever the row's evaluation covers — or `error`.

Input population: every row of the `DayCountTest` tables `data_yearFraction` (201 rows),
`data_days` (185), `data_30U360` (22), `data_30E360ISDA` (19), `data_ACTACTAFB` (57) and
`data_ACT365L` (12) — the `30U/360`, `30/360 ISDA`, `30U/360 EOM`, `30E/360 ISDA` and `Act/365L`
rows among them supplying the stub `scheduleInfo`; the `Act/Act ICMA` stub series and the
`Act/Act` ISDA test cases, which evaluate `Act/Act ISDA`, `Act/Act ICMA` and `Act/Act AFB` against
one schedule; a sampled sweep of `Act/Act Year` against ICMA; a generated grid over all 21
standard day counts plus `Bus/252 BRBD` on month-end and mid-month date pairs from 2010 to 2030
with default schedule information; and every period of `P1M`/`P3M`/`P6M`/`P12M` schedules from
2015-01-15 to 2020-01-15 in three shapes — regular, `SHORT_INITIAL` with `firstRegularStartDate`,
`SHORT_FINAL` with `lastRegularEndDate` — evaluated with the real `Schedule` as the schedule
information. Observed: 18,254 rows, 147,027 checks, 1,025 `error` rows.

The capture also asserts one hard anchor before emitting anything:
`Act/Act ISDA` from 2011-12-28 to 2012-02-28 must equal `4/365 + 58/366`.

#### `schedule-baseline.json`

Row inputs — the eleven `PeriodicSchedule` fields that define a case: `startDate`, `endDate`,
`frequency`, `businessDayAdjustment`, `startDateBusinessDayAdjustment`,
`endDateBusinessDayAdjustment`, `stubConvention`, `rollConvention`, `firstRegularStartDate`,
`lastRegularEndDate`, `overrideStartDate`. A business-day adjustment is
`{"convention", "calendar"}`; an adjustable date is `{"unadjusted", "adjustment"}`; an absent
optional field is `null`.

Row expectations: `unadjustedDates`, `adjustedDates`,
`periods[{unadjustedStart, unadjustedEnd, start, end}]`, `initialStub`, `finalStub` (each a period
or `null`), `resolvedRollConvention`, `resolvedFrequency` — or `error` carrying the Java
`ScheduleException` / `IllegalArgumentException` message, for a definition the builder or the
resolution rejects.

Input population: every row of `PeriodicScheduleTest.data_generation` (87) and `data_replace`
(11), plus a generated grid of 4 frequencies × 8 stub conventions × 6 roll conventions (`EOM`,
`IMM`, `IMMCAD`, `IMMAUD`, `TBILL`, `Day15`) × 3 calendars (`GBLO`, `USNY`, `GBLO+USNY`) over
2015-01-15 to 2018-01-15 with `ModifiedFollowing`, resolved against `ReferenceData.standard()`.
Observed: 674 rows, 293 checks, 373 `error` rows.

#### `fx-baseline.json`

One row per scenario. Inputs: `source`, `matrix` (the defining list of `{pair, rate}`), and
`matrixState` — the built matrix's own state, `{currencies, rates}`, with the currencies in the
matrix's insertion order and `rates` as an array of row arrays. A definition the builder rejects
yields a row with `error` and no `matrixState`.

Query lists, each entry carrying its value or an `error`:

* `queries[{base, counter, fxRate}]`
* `conversions[{currency, amount, target, converted{currency, amount}}]`
* `multi[{amounts[{currency, amount}], target, multiConverted}]`
* `merges[{other[{pair, rate}], merged{currencies, rates}}]`
* `crosses[{rate1, rate2, crossRate}]` — populated on the `FxRate` cross-rate row

Input population: the `FxMatrixTest` rate sets (the triangulating cross-rate matrix, the
multi-currency conversion matrix, the zero-rate connected matrix, the empty matrix, a single-pair
matrix, and the disjoint / duplicate / additional merge cases) and the `FxRateTest` cross-rate
cases — eight that agree plus five documented failures — followed by seeded random matrices.
Observed: 11 rows, 35 checks. Capture-time tolerances: exact for directly supplied rates and their
reciprocals, `1e-6` for triangulated crosses.

#### `currency-math-baseline.json`

Six rows, one per Java test family. Every row carries `source`, then the **seven declared input
lists** — `amounts`, `scalars`, `arrays`, `multiArrays`, `money`, `bigMoney`, `rates` — which are
always present and are empty arrays when unused, so every row has the same shape. Then one
expectation bucket, named for the family: `currencyAmountResults`, `moneyResults`,
`bigMoneyResults`, `currencyAmountArrayResults`, `multiCurrencyAmountResults`,
`multiCurrencyAmountArrayResults`.

Each bucket entry names its operation in `op` and carries its operands and either `result` or
`error`. Two markers appear:

* `"composed": true` with `mapAmountsFn` — the expectation was composed from two real public Java
  calls because the Java type has no such single method (`multipliedBy` and `mapAmounts` on the
  amount-array types are added by the Scala port). The mapping function is `x -> x * x`, the same
  one the double-array fixture uses.
* `"captureOnly": true` with `minorUnitDigits` — a row with no Java test constant to check
  against (see the 0-decimal-currency note in section 8).

Coverage: `CurrencyAmount` `plus`/`minus`/`multipliedBy`/`negated`/`positive`/`negative`/
`convertedTo`; `CurrencyAmountArray` and `MultiCurrencyAmountArray` element-wise
`plus`/`minus`/`multipliedBy`/`mapAmounts`/`total`; `MultiCurrencyAmount`
`of`/`total`/`plus`/`convertedTo`; `Money.of` and `BigMoney.of` rounded amounts with
`Money` `plus`/`multipliedBy`/`convertedTo` and `BigMoney.toMoney`. `Money`/`BigMoney` amounts are
written as decimal **strings** together with their `toString` form, so no rounding is lost through
a double. Observed: 6 rows, 32 checks, 17 `error` entries, 61 capture-only entries.

#### `holiday-baseline.json`

One row per (calendar, year): `source`, `calendar` (id or weekend-calendar name), `year`.

Expectations: `holidays` — **every** date in that year for which `isHoliday` is true, weekends
included, which is the only symmetric contract both sides can compute (a calendar's weekend days
are not public API, and `isHoliday` treats a weekend as a holiday); `probes`, two per row, each
`{date, isHoliday, isBusinessDay, next, previous, nextOrSame, previousOrSame, shift3,
shiftMinus3}`; and `daysBetween` over the whole year.

Input population, distinguished by `source`: `generated` — the 24 calendars generated over
1950–2099, and `EUTA` over 1997–2099; `dataTable` — `THBA` over 2005–2079; `outOfRange` — the year
either side of each calendar's range (1949 and 2100; 1996 for `EUTA`; 2004 and 2080 for `THBA`),
where Java falls back to a weekend-only test instead of throwing, which the port must reproduce;
`weekend` — the four weekend / no-holiday calendars for 2020 and 2024, with their ids asserted;
`composite` — `GBLO+USNY` for 2020 and 2024. Observed: 3,840 rows.

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

* **Public API only.** A `.jsh` script runs in the unnamed package, so the package-private classes
  the Java tests reach by sharing their package are **unreachable** here:
  `GlobalHolidayCalendars` (`date/GlobalHolidayCalendars.java:43`), `StandardDayCounts` (`:21`),
  `Business252DayCount` (`:23`), `StandardRollConventions` (`:24`),
  `StandardBusinessDayConventions` (`:20`) and `GlobalHolidayCalendarLookup` (`:19`). Use the
  public routes instead: `HolidayCalendars.of(String)` (`date/HolidayCalendars.java:91`) or
  `HolidayCalendarId.of(id).resolve(ReferenceData.standard())`, which yields the very same calendar
  instances; the `DayCounts` constants rather than `StandardDayCounts.values()`. `Bus/252` rows are
  reachable because `DayCount.ofBus252(HolidayCalendarId)` (`date/DayCount.java:59`) is public.
  `easter()` is not reachable, and belongs to no fixture.
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
  `minorUnitDigits`, so the capture adds a 0-digit currency (JPY) itself. Those rows are marked
  `captureOnly` because there is no Java constant to cross-check them against — which is the point
  of the marker: it stops the summary from hiding a row that *should* have been checked.
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
