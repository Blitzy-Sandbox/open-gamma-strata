#!/usr/bin/env bash
#
# verify-gates.sh - the single authoritative acceptance-gate runner for the
#                   Scala port of `strata-collect` and `strata-basics`.
#
# PROVENANCE
#   Every row this script runs, and every row's pass condition, comes from the
#   validation table of the technical specification (AAP section 0.10.1). The
#   nineteen automated rows are executed in that table's order, followed by the
#   one row that is reported rather than measured (Gate 7's manual approval,
#   which is an out-of-band pull-request review and never blocks this script).
#   No row may be relaxed, skipped or short-circuited: nothing else in the
#   repository enforces the deliverable, because `.github/mergify.yml` gates
#   auto-merge on `check-success=build` alone and the CircleCI `scala_build21`
#   job - which invokes this script exactly once - is informational for
#   auto-merge just like the existing `build11`/`build17`/`build21` jobs.
#
#   The ten numbered items the specification calls "Rule 1 ... Rule 10" are
#   requirements of the migration that this script MEASURES. They do not
#   constrain this script, which is why the literal strings `var`, `null`,
#   `throw new`, `java.util.List`, `guava` and `joda` appear below inside grep
#   patterns: `scripts/` sits outside every tree those patterns scan. Keep it
#   that way - this script writes nothing outside "$ROOT/target", so no
#   evidence file can ever land in a scanned path and trip a gate it measures.
#
# USAGE
#   scripts/verify-gates.sh            run every gate and write the report
#   scripts/verify-gates.sh -h|--help  print this usage and exit
#
#   There is deliberately no flag that selects a subset of gates: a partial run
#   is not an acceptance run.
#
# EXIT CODES
#   0  every automated row passed
#   1  one or more automated rows failed
#   2  usage error, or a required tool is missing (preflight failure)
#
# ARTIFACTS (all under $ROOT/target, all published by CI with `when: always`)
#   target/gate-report.md          the deliverable evidence: one row per gate,
#                                  a machine-readable summary line and the
#                                  appendices each row contributed
#   target/parity-report/*.json    the six parity reports the specs write
#   target/test-reports/           the JUnit XML and ScalaTest logs
#   target/audit/                  per-row evidence, sbt logs, class-load logs
#                                  and the snapshots the late rows read
#
# REQUIRED TOOLCHAIN
#   JDK 21 (`java`, `javap`) and sbt 1.13.0 on PATH, as installed by the
#   `scala_build21` CI job, plus git, python3 and the POSIX text utilities.
#   `jq`, `xmllint` and `shellcheck` are NOT required and NOT used: every JSON,
#   XML and CSV document is parsed with python3 and its standard library only.
#   On a small host export SBT_OPTS first, e.g.
#   SBT_OPTS="-Xmx1200m -Xss8m -XX:MaxMetaspaceSize=512m".
#
# DESIGN NOTES THAT MATTER WHEN EDITING THIS FILE
#   * `set -e` is suppressed inside a function called in an `if`, `&&` or `||`
#     context, and `run_gate` calls every gate function that way. No gate
#     function may rely on `set -e`: each inspects the status of every command
#     it runs and returns an explicit 0 or 1.
#   * grep exits 0 when it matched, 1 when it did not and >=2 on error. Every
#     "must find nothing" row goes through `assert_no_match`, which passes only
#     on 1 and reports 0 and >=2 differently.
#   * A row whose input artifact is missing FAILS. There is no path on which a
#     missing file, an absent marker or a failed command is read as a pass.
#
set -euo pipefail

# Deterministic, locale-independent sorting, grepping and character classes.
export LC_ALL=C

# A conservative default for hosts with little memory; an exported SBT_OPTS
# wins, so CI and developers can size the build themselves.
export SBT_OPTS="${SBT_OPTS:--Xmx1200m -Xss8m -XX:MaxMetaspaceSize=512m}"

#-----------------------------------------------------------------------------
# Locations.
#-----------------------------------------------------------------------------

SCRIPT_NAME="scripts/verify-gates.sh"

if ! ROOT="$(git rev-parse --show-toplevel 2>/dev/null)"; then
  printf 'FATAL: %s must be run inside the git working tree of the repository.\n' \
    "$SCRIPT_NAME" >&2
  exit 2
fi
cd "$ROOT"

TARGET_DIR="$ROOT/target"
PARITY_DIR="$TARGET_DIR/parity-report"
TEST_REPORT_DIR="$TARGET_DIR/test-reports"
AUDIT_DIR="$TARGET_DIR/audit"
LOG_DIR="$AUDIT_DIR/logs"
SNAPSHOT_DIR="$AUDIT_DIR/snapshot"
REPORT_FILE="$TARGET_DIR/gate-report.md"
APPENDIX_FILE="$AUDIT_DIR/appendices.md"

# The two Scala module roots, named once.
COLLECT_MAIN="strata-collect/src/main/scala"
BASICS_MAIN="strata-basics/src/main/scala"
COLLECT_CLASSES="strata-collect/target/scala-2.13/classes"
BASICS_CLASSES="strata-basics/target/scala-2.13/classes"
COLLECT_TEST_CLASSES="strata-collect/target/scala-2.13/test-classes"

#-----------------------------------------------------------------------------
# Usage and argument handling.
#-----------------------------------------------------------------------------

# Prints this file's leading comment block, which is the usage text. The file
# is located at its canonical path first and through BASH_SOURCE second, so
# that an invocation from a subdirectory - which `cd "$ROOT"` above has
# already made a relative BASH_SOURCE useless for - still prints it.
usage() {
  local self="$ROOT/$SCRIPT_NAME"
  if [[ ! -f "$self" ]]; then
    self="${BASH_SOURCE[0]}"
  fi
  if [[ ! -f "$self" ]]; then
    printf 'Usage: %s [-h|--help]\n' "$SCRIPT_NAME"
    printf 'Runs every acceptance gate of AAP section 0.10.1 and writes\n'
    printf 'target/gate-report.md. Exit 0 all passed, 1 a row failed, 2 usage.\n'
    return 0
  fi
  awk 'NR == 1 { next }
       /^#/ { sub(/^#[[:space:]]?/, ""); print; next }
       { exit }' "$self"
}

if [[ $# -gt 0 ]]; then
  case "$1" in
    -h | --help)
      usage
      exit 0
      ;;
    *)
      printf 'FATAL: unknown argument "%s".\n\n' "$1" >&2
      printf 'Usage: %s [-h|--help]\n' "$SCRIPT_NAME" >&2
      printf 'This script takes no options: it runs every gate, always.\n' >&2
      exit 2
      ;;
  esac
fi

#-----------------------------------------------------------------------------
# The output tree, created before anything can write into it - in particular
# before any JVM writes a class-load log, which the specification calls out.
#-----------------------------------------------------------------------------

mkdir -p "$TARGET_DIR" "$PARITY_DIR" "$TEST_REPORT_DIR" "$AUDIT_DIR" "$LOG_DIR" \
  "$SNAPSHOT_DIR/test-reports" "$SNAPSHOT_DIR/parity-report"
: >"$APPENDIX_FILE"

#-----------------------------------------------------------------------------
# Gate bookkeeping. Four ordered, parallel arrays: the row label, its verdict,
# a one-line detail and the relative path of its evidence.
#-----------------------------------------------------------------------------

GATE_LABEL=()
GATE_STATUS=()
GATE_DETAIL=()
GATE_EVIDENCE=()
GATE_TOTAL=0
GATE_FAILED=0
# Automated rows are the measured ones; the reported row (Gate 7's manual
# approval) is counted apart, so the summary cannot read as if a human
# approval had been measured.
GATE_AUTOMATED=0
GATE_REPORTED=0
REPORT_WRITTEN="no"
# The number of automated rows AAP section 0.10.1 defines, so that a report
# written by the EXIT trap after an interruption can say how much of the run
# it covers instead of presenting a partial result as an acceptance result.
GATE_EXPECTED_AUTOMATED=19
RUN_COMPLETED="no"

# Set by a gate function through `detail`/`evidence`; read by `run_gate`.
GATE_DETAIL_OUT=""
GATE_EVIDENCE_OUT=""

# Appends to the running row detail, so a row can report several findings.
detail() {
  if [[ -z "$GATE_DETAIL_OUT" ]]; then
    GATE_DETAIL_OUT="$1"
  else
    GATE_DETAIL_OUT="$GATE_DETAIL_OUT; $1"
  fi
}

# Names the evidence file of the current row, repository-root relative.
evidence() {
  GATE_EVIDENCE_OUT="${1#"$ROOT"/}"
}

# Starts a fresh evidence file for the current row and publishes its absolute
# path in EV. Not a command substitution: that would run in a subshell and the
# `evidence` assignment would be lost.
EV=""
new_evidence() {
  EV="$AUDIT_DIR/$1"
  : >"$EV"
  evidence "$EV"
}

# Appends one appendix section to the report's appendix file. Content arrives
# on stdin so that a caller can pipe a file, a command or a here-document.
add_appendix() {
  local title="$1"
  {
    printf '\n### %s\n\n' "$title"
    printf '```text\n'
    cat
    printf '```\n'
  } >>"$APPENDIX_FILE"
}

#-----------------------------------------------------------------------------
# The gate runner.
#
# Runs one row, records its verdict and CONTINUES: the report is the
# deliverable, so a failing row never stops the rows after it from producing
# their own evidence. The overall exit status is non-zero if any row failed.
#-----------------------------------------------------------------------------

run_gate() {
  local label="$1"
  local fn="$2"
  local rc=0

  GATE_DETAIL_OUT=""
  GATE_EVIDENCE_OUT=""

  printf '\n=== %s ===\n' "$label"

  # Deliberate: `|| rc=$?` disables `set -e` inside the callee, which is why
  # every gate function checks its own commands and returns explicitly.
  "$fn" || rc=$?

  local status
  if [[ "$rc" -eq 0 ]]; then
    status="PASS"
  else
    status="FAIL"
    GATE_FAILED=$((GATE_FAILED + 1))
  fi

  GATE_LABEL+=("$label")
  GATE_STATUS+=("$status")
  GATE_DETAIL+=("${GATE_DETAIL_OUT:-no detail recorded}")
  GATE_EVIDENCE+=("${GATE_EVIDENCE_OUT:-none}")
  GATE_TOTAL=$((GATE_TOTAL + 1))
  GATE_AUTOMATED=$((GATE_AUTOMATED + 1))

  printf '%s: %s -- %s\n' "$status" "$label" "${GATE_DETAIL_OUT:-no detail recorded}"
  return 0
}

# Records a row that is reported rather than measured. Its verdict text is
# fixed by the specification and is written verbatim into the report.
record_reported_row() {
  GATE_LABEL+=("$1")
  GATE_STATUS+=("REPORTED")
  GATE_DETAIL+=("$2")
  GATE_EVIDENCE+=("${3:-none}")
  GATE_TOTAL=$((GATE_TOTAL + 1))
  GATE_REPORTED=$((GATE_REPORTED + 1))
  printf '\n=== %s ===\nREPORTED: %s -- %s\n' "$1" "$1" "$2"
}

#-----------------------------------------------------------------------------
# Shared helpers.
#-----------------------------------------------------------------------------

# assert_no_match <description> <evidence file> <grep arguments...>
#
# The single implementation of every "this must find nothing" check. grep's
# three exit statuses are handled separately: 1 (nothing matched) is the only
# pass, 0 records the matched lines as the failure's evidence, and >=2 is an
# error - a missing path or a bad pattern - reported as such rather than read
# as an absence. Binary matches are captured too, because grep reports them on
# stderr, and they are treated as matches.
assert_no_match() {
  local description="$1"
  local evidence_file="$2"
  shift 2
  local matches
  local rc=0

  matches="$(grep "$@" 2>&1)" || rc=$?

  {
    printf '## %s\n' "$description"
    printf '# command: grep %s\n' "$*"
    printf '# grep exit status: %s (1 = nothing matched, which is the pass)\n' "$rc"
    if [[ -n "$matches" ]]; then
      printf '%s\n' "$matches"
    fi
    printf '\n'
  } >>"$evidence_file"

  case "$rc" in
    1)
      return 0
      ;;
    0)
      local count
      count="$(printf '%s\n' "$matches" | awk 'NF { n++ } END { print n + 0 }')"
      detail "$description: $count match(es)"
      return 1
      ;;
    *)
      detail "$description: grep failed with status $rc"
      return 1
      ;;
  esac
}

# count_matches <pattern file or '-'> ... reads stdin, prints the match count.
# Returns 1 only when grep itself failed (status >= 2), never for "no match".
count_matches() {
  local pattern="$1"
  local n
  local rc=0
  n="$(grep -c -e "$pattern" || rc=$?)"
  if [[ "$rc" -gt 1 ]]; then
    return 1
  fi
  printf '%s\n' "${n:-0}"
  return 0
}

# require_file <path> <what it is>; records the failure detail itself.
require_file() {
  if [[ ! -f "$1" ]]; then
    detail "missing $2: $1"
    return 1
  fi
  return 0
}

# run_sbt <log name> <sbt arguments...>
#
# The one way this script invokes sbt. `-batch` is what the specification
# prescribes (and it starts no sbt server, so nothing listens on a socket);
# `-Dsbt.log.noformat=true` removes ANSI colour codes, without which the
# `[info]`-anchored and classpath greps below would read escape sequences.
# That flag is a determinism measure only - it changes nothing any gate
# measures. Combined output is tee'd to the log and to stdout, so a CI log
# shows the build while the file keeps it for the report.
run_sbt() {
  local name="$1"
  shift
  local log="$LOG_DIR/$name.log"
  local rc=0

  printf '+ sbt -batch -Dsbt.log.noformat=true' >&2
  printf ' %q' "$@" >&2
  printf '\n' >&2

  sbt -batch -Dsbt.log.noformat=true "$@" >"$log" 2>&1 || rc=$?
  SBT_LOG="$log"
  if [[ -f "$log" ]]; then
    tail -n 40 "$log"
  fi
  return "$rc"
}

# Strips sbt's `[info] ` / `[warn] ` / `[error] ` line prefixes, so that a line
# printed by a forked JVM (which arrives unprefixed) and the same line relayed
# through sbt's logger (which arrives prefixed) are read identically.
strip_sbt_prefix() {
  sed -E 's/^\[(info|warn|error|success|debug)\][[:space:]]?//'
}

#-----------------------------------------------------------------------------
# Preflight: every external tool this script needs, checked before any gate
# runs. A missing tool is a hard stop - gates are never skipped because their
# tool is absent.
#-----------------------------------------------------------------------------

preflight() {
  local required=(git sbt java javap python3 awk sed grep find sort comm tr cut wc diff)
  local missing=()
  local tool
  for tool in "${required[@]}"; do
    if ! command -v "$tool" >/dev/null 2>&1; then
      missing+=("$tool")
    fi
  done

  local evidence_file="$AUDIT_DIR/preflight.txt"
  {
    printf '## preflight\n'
    printf '# repository root: %s\n' "$ROOT"
    for tool in "${required[@]}"; do
      printf '%-8s %s\n' "$tool" "$(command -v "$tool" 2>/dev/null || printf 'MISSING')"
    done
    printf '\n# versions\n'
    java -version 2>&1 || printf 'java -version failed\n'
    sbt --script-version 2>&1 || printf 'sbt --script-version failed\n'
    python3 --version 2>&1 || printf 'python3 --version failed\n'
  } >"$evidence_file"

  if [[ "${#missing[@]}" -gt 0 ]]; then
    printf 'FATAL: preflight failed - these required tools are not on PATH: %s\n' \
      "${missing[*]}" >&2
    printf 'A complete run needs JDK 21 and sbt 1.13.0, as installed by the\n' >&2
    printf 'CircleCI scala_build21 job. Install them and re-run; gates are never\n' >&2
    printf 'skipped because a tool is missing.\n' >&2
    GATE_LABEL+=("Preflight - required tools")
    GATE_STATUS+=("FAIL")
    GATE_DETAIL+=("missing tools: ${missing[*]}")
    GATE_EVIDENCE+=("${evidence_file#"$ROOT"/}")
    GATE_TOTAL=$((GATE_TOTAL + 1))
    GATE_FAILED=$((GATE_FAILED + 1))
    write_report
    exit 2
  fi

  JDK_VERSION="$(java -version 2>&1 | head -n 1)"
  SBT_VERSION="$(sbt --script-version 2>/dev/null || printf 'unknown')"
  printf 'preflight: all required tools present\n'
  printf '  %s\n  sbt %s\n' "$JDK_VERSION" "$SBT_VERSION"
}

#-----------------------------------------------------------------------------
# Snapshots.
#
# The late rows must not depend on artifacts an earlier row can destroy. The
# Rule 9 row runs `sbt clean`, and `build.sbt` registers target/test-reports
# and target/parity-report with `cleanFiles`, so both directories are emptied
# there. The JUnit XML of the FULL suite exists only after Gate 1 - the later
# `testOnly` rows rewrite the suites they run - so it is copied aside
# immediately, and the test-scope row reads the copy.
#-----------------------------------------------------------------------------

snapshot_junit_xml() {
  rm -f "$SNAPSHOT_DIR"/test-reports/*.xml
  local count=0
  if compgen -G "$TEST_REPORT_DIR/TEST-*.xml" >/dev/null; then
    cp "$TEST_REPORT_DIR"/TEST-*.xml "$SNAPSHOT_DIR/test-reports/"
    count="$(find "$SNAPSHOT_DIR/test-reports" -name 'TEST-*.xml' | wc -l | tr -d ' ')"
  fi
  SNAPSHOT_JUNIT_COUNT="$count"
}

snapshot_parity_reports() {
  rm -f "$SNAPSHOT_DIR"/parity-report/*.json
  local count=0
  if compgen -G "$PARITY_DIR/*.json" >/dev/null; then
    cp "$PARITY_DIR"/*.json "$SNAPSHOT_DIR/parity-report/"
    count="$(find "$SNAPSHOT_DIR/parity-report" -name '*.json' | wc -l | tr -d ' ')"
  fi
  SNAPSHOT_PARITY_COUNT="$count"
}

# Puts this run's reports back after the Rule 9 row's `clean` emptied the two
# published directories. The files restored are the ones this run produced -
# the snapshots taken in Gate 1 and Gate 3 - so the artifacts CI publishes are
# complete rather than empty. Nothing is regenerated and nothing is invented.
restore_snapshots() {
  if compgen -G "$SNAPSHOT_DIR/test-reports/TEST-*.xml" >/dev/null; then
    mkdir -p "$TEST_REPORT_DIR"
    cp "$SNAPSHOT_DIR"/test-reports/TEST-*.xml "$TEST_REPORT_DIR/"
  fi
  if compgen -G "$SNAPSHOT_DIR/parity-report/*.json" >/dev/null; then
    mkdir -p "$PARITY_DIR"
    cp "$SNAPSHOT_DIR"/parity-report/*.json "$PARITY_DIR/"
  fi
}

#=============================================================================
# Row 1 - Gate 1: builds and runs.
#
#   sbt -batch clean compile Test/compile test
#
# Pass: exit 0 and every spec in both modules green. The JUnit XML and the
# parity reports are snapshotted immediately afterwards, whatever the verdict,
# so the later rows and the report have them.
#=============================================================================

row_01_build_and_test() {
  local rc=0
  run_sbt gate01-build clean compile "Test/compile" test || rc=$?
  new_evidence gate01-build-and-test.txt

  {
    printf '## Gate 1 - builds and runs\n'
    printf '# command: sbt -batch clean compile Test/compile test\n'
    printf '# sbt exit status: %s\n' "$rc"
    printf '# log: %s\n\n' "${SBT_LOG#"$ROOT"/}"
    printf '# run summary lines from the sbt log\n'
    awk '/Total number of tests run|Suites: completed|Tests: succeeded|All tests passed|TESTS FAILED|^\[error\]/ {
           if (++shown <= 60) print
         }
         END { if (shown == 0) print "(no summary lines found)" }' "$SBT_LOG"
  } >>"$EV"

  snapshot_junit_xml
  snapshot_parity_reports

  local failed=0

  if [[ "$rc" -ne 0 ]]; then
    detail "sbt clean compile Test/compile test exited $rc"
    failed=1
  fi

  # Vacuity: a build that compiled nothing and discovered no suite would
  # otherwise exit 0 and pass this row.
  local runs
  runs="$(awk '/Total number of tests run/ { n++ } END { print n + 0 }' "$SBT_LOG")"
  if [[ "${runs:-0}" -lt 1 ]]; then
    detail "the sbt log reports no test run at all"
    failed=1
  fi
  if [[ "${SNAPSHOT_JUNIT_COUNT:-0}" -lt 1 ]]; then
    detail "no TEST-*.xml was written to target/test-reports"
    failed=1
  fi

  local total_tests
  total_tests="$(awk 'match($0, /Total number of tests run: [0-9]+/) {
      line = substr($0, RSTART, RLENGTH)
      sub(/.*: /, "", line)
      total += line
    }
    END { print total + 0 }' "$SBT_LOG")"

  {
    printf '\n# suites snapshotted: %s\n' "${SNAPSHOT_JUNIT_COUNT:-0}"
    printf '# parity reports snapshotted: %s\n' "${SNAPSHOT_PARITY_COUNT:-0}"
    printf '# tests reported by sbt: %s\n' "$total_tests"
  } >>"$EV"

  if [[ "$failed" -eq 0 ]]; then
    detail "both modules compiled and every spec passed (${total_tests} tests, ${SNAPSHOT_JUNIT_COUNT} suites)"
  fi
  return "$failed"
}

#=============================================================================
# Row 2 - Gate 2 / Rule 1: dependency purity.
#
# Three checks, all of which must find nothing, on both Compile and Test:
#   1. the four dependency trees, grepped for guava, joda and the UNSUFFIXED
#      Maven coordinate `com.opengamma.strata:strata-collect:` - the Java
#      artefact. The in-build Scala project appears as `strata-collect_2.13`
#      and must not be confused with it.
#   2. the exported full classpaths, grepped for guava, joda and a VERSIONED
#      strata-collect jar. The Scala module contributes a `classes` directory,
#      never a jar, which is exactly what distinguishes the two.
#   3. the build definition and both module trees, grepped for the Java
#      libraries by package and artifact name.
# A `scala-library` positive control keeps an empty classpath from passing.
#=============================================================================

row_02_dependency_purity() {
  new_evidence gate02-dependency-purity.txt
  local failed=0
  local rc=0

  # -- 1. dependency trees -------------------------------------------------
  run_sbt gate02-dependency-tree \
    "strata-basics/Compile/dependencyTree" \
    "strata-basics/Test/dependencyTree" \
    "strata-collect/Compile/dependencyTree" \
    "strata-collect/Test/dependencyTree" || rc=$?
  local tree_log="$SBT_LOG"

  {
    printf '## Gate 2 / Rule 1 - dependency purity\n'
    printf '# dependency tree log: %s (sbt exit %s)\n\n' "${tree_log#"$ROOT"/}" "$rc"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "dependencyTree failed with status $rc"
    failed=1
  fi
  # Vacuity: `scala-library` is marked [S] and omitted from the tree, so the
  # control is a declared dependency that must be there instead.
  if ! grep -q 'cats-core_2.13' "$tree_log"; then
    detail "dependency tree does not mention cats-core_2.13 (tree not produced?)"
    failed=1
  fi
  if ! assert_no_match "guava/joda/Java strata-collect in the four dependency trees" \
    "$EV" -Ei "guava|joda|com\.opengamma\.strata:strata-collect:" "$tree_log"; then
    failed=1
  fi

  # -- 2. exported classpaths ---------------------------------------------
  rc=0
  run_sbt gate02-classpath \
    "export strata-basics/Compile/fullClasspath" \
    "export strata-basics/Test/fullClasspath" || rc=$?
  local cp_log="$SBT_LOG"
  local entries="$AUDIT_DIR/gate02-classpath-entries.txt"

  # `export` prints the value unprefixed; sbt's own chatter is `[...]`-prefixed
  # and is dropped so that the entries file holds classpath entries only.
  awk '!/^\[/' "$cp_log" | tr ':' '\n' | sed -e 's/^[[:space:]]*//' -e '/^$/d' >"$entries"

  {
    printf '# classpath log: %s (sbt exit %s)\n' "${cp_log#"$ROOT"/}" "$rc"
    printf '# classpath entries: %s (see %s)\n\n' \
      "$(wc -l <"$entries" | tr -d ' ')" "${entries#"$ROOT"/}"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "export fullClasspath failed with status $rc"
    failed=1
  fi
  if ! grep -q 'scala-library' "$entries"; then
    detail "positive control failed: no scala-library on the exported classpath"
    failed=1
  fi
  if ! grep -qF "$COLLECT_CLASSES" "$entries"; then
    detail "the Scala strata-collect classes directory is not on the classpath"
    failed=1
  fi
  if ! assert_no_match "guava/joda/versioned strata-collect jar on the Compile and Test classpaths" \
    "$EV" -Ei "guava|joda|strata-collect-[0-9].*\.jar" "$entries"; then
    failed=1
  fi

  # -- 3. build definition and module sources -----------------------------
  if ! assert_no_match "Java libraries named in the build definition or either module tree" \
    "$EV" -rE "org\.joda|com\.google\.common|joda-beans|joda-convert|guava" \
    build.sbt project strata-collect strata-basics; then
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "no guava, joda or Java strata-collect artefact on either module's Compile or Test classpath"
  fi
  return "$failed"
}

#=============================================================================
# Row 3 - Gate 2a / Rule 1a: exactly two Scala-only modules, with the edge.
#
#   * `sbt projects` lists exactly `strata-basics` and `strata-collect` (the
#     root project IS strata-basics). Compared against a literal expectation,
#     not a count. sbt 1.13 emits a TAB after `[info]`, so the extraction is
#     whitespace-tolerant rather than space-anchored.
#   * the Compile internal dependency classpath of strata-basics carries
#     strata-collect's `classes`, and the Test one additionally its
#     `test-classes`: that is the proof of the directed edge.
#   * zero `.java` files under either module or the build definition.
#   * the file-extension histogram, reported.
#   * each project's unmanaged source directories hold only its own
#     src/main/scala.
#=============================================================================

row_03_two_scala_modules() {
  new_evidence gate02a-two-scala-modules.txt
  local failed=0
  local rc=0

  {
    printf '## Gate 2a / Rule 1a - exactly two Scala-only modules with the required edge\n\n'
  } >>"$EV"

  # -- project ids ---------------------------------------------------------
  run_sbt gate02a-projects projects || rc=$?
  local projects_log="$SBT_LOG"
  local ids="$AUDIT_DIR/gate02a-project-ids.txt"
  local expected_ids="$AUDIT_DIR/gate02a-project-ids-expected.txt"

  # sbt 1.13 emits a TAB after `[info]`, so the anchor is whitespace-tolerant
  # rather than space-anchored; awk exits 0 when nothing matches, so an empty
  # result reaches the comparison below instead of failing the pipeline.
  awk '/^\[info\][[:space:]]+[*]?[[:space:]]*strata-/ {
         if (match($0, /strata-[a-z]+/)) print substr($0, RSTART, RLENGTH)
       }' "$projects_log" | sort >"$ids"
  printf 'strata-basics\nstrata-collect\n' >"$expected_ids"

  {
    printf '# command: sbt -batch projects (exit %s)\n' "$rc"
    printf '# project ids found:\n'
    cat "$ids"
    printf '# expected:\n'
    cat "$expected_ids"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "sbt projects failed with status $rc"
    failed=1
  fi
  if ! diff -u "$expected_ids" "$ids" >>"$EV" 2>&1; then
    detail "the build's project ids are not exactly strata-basics and strata-collect"
    failed=1
  fi

  # -- the directed edge ---------------------------------------------------
  rc=0
  run_sbt gate02a-internal-classpath \
    "show strata-basics/Compile/internalDependencyClasspath" \
    "show strata-basics/Test/internalDependencyClasspath" || rc=$?
  local edge_log="$SBT_LOG"
  local compile_section="$AUDIT_DIR/gate02a-edge-compile.txt"
  local test_section="$AUDIT_DIR/gate02a-edge-test.txt"

  # The root project aggregates strata-collect, so `show` prints both
  # projects' values. Attribution is by the key header each value follows.
  awk '/Compile \/ internalDependencyClasspath[[:space:]]*$/ { section = "compile"; next }
       /Test \/ internalDependencyClasspath[[:space:]]*$/ { section = "test"; next }
       section == "compile" { print > compile_out; next }
       section == "test" { print > test_out }' \
    compile_out="$compile_section" test_out="$test_section" "$edge_log"
  : >>"$compile_section"
  : >>"$test_section"

  {
    printf '\n# command: sbt -batch "show strata-basics/Compile/internalDependencyClasspath" '
    printf '"show strata-basics/Test/internalDependencyClasspath" (exit %s)\n' "$rc"
    printf '# Compile section:\n'
    cat "$compile_section"
    printf '# Test section:\n'
    cat "$test_section"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "show internalDependencyClasspath failed with status $rc"
    failed=1
  fi
  if ! grep -qF "$COLLECT_CLASSES" "$compile_section"; then
    detail "Compile/internalDependencyClasspath lacks $COLLECT_CLASSES"
    failed=1
  fi
  if ! grep -qF "$COLLECT_TEST_CLASSES" "$test_section"; then
    detail "Test/internalDependencyClasspath lacks $COLLECT_TEST_CLASSES"
    failed=1
  fi

  # -- zero Java sources ---------------------------------------------------
  local java_count
  java_count="$(find strata-collect strata-basics project -name '*.java' | wc -l | tr -d ' ')"
  {
    printf '\n# command: find strata-collect strata-basics project -name "*.java" | wc -l\n'
    printf '%s\n' "$java_count"
    find strata-collect strata-basics project -name '*.java'
  } >>"$EV"
  if [[ "$java_count" -ne 0 ]]; then
    detail "$java_count .java file(s) inside the sbt build"
    failed=1
  fi

  # -- file-extension histogram (reported) --------------------------------
  local histogram="$AUDIT_DIR/gate02a-extension-histogram.txt"
  find strata-collect strata-basics -type f | sed 's/.*\.//' | sort | uniq -c |
    sort -rn >"$histogram"
  add_appendix "Gate 2a - file-extension histogram of strata-collect and strata-basics" \
    <"$histogram"

  # -- unmanaged source directories ---------------------------------------
  rc=0
  run_sbt gate02a-source-dirs \
    "show strata-basics/Compile/unmanagedSourceDirectories" \
    "show strata-collect/Compile/unmanagedSourceDirectories" || rc=$?
  local dirs_log="$SBT_LOG"
  local dirs="$AUDIT_DIR/gate02a-source-directories.txt"

  # Every source directory the two `show` commands printed, one per line. The
  # pattern admits a test root as well as a main one, so a project pointed at
  # the wrong root is reported rather than filtered away.
  awk '{
         line = $0
         while (match(line, /\/[^ ,()]+\/src\/(main|test)\/(scala|java|resources)/)) {
           print substr(line, RSTART, RLENGTH)
           line = substr(line, RSTART + RLENGTH)
         }
       }' "$dirs_log" | sort -u >"$dirs"

  {
    printf '\n# command: sbt -batch "show strata-basics/Compile/unmanagedSourceDirectories" '
    printf '"show strata-collect/Compile/unmanagedSourceDirectories" (exit %s)\n' "$rc"
    printf '# source directories reported:\n'
    cat "$dirs"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "show unmanagedSourceDirectories failed with status $rc"
    failed=1
  fi
  local expected_dirs="$AUDIT_DIR/gate02a-source-directories-expected.txt"
  printf '%s\n%s\n' "$ROOT/$BASICS_MAIN" "$ROOT/$COLLECT_MAIN" | sort >"$expected_dirs"
  if ! diff -u "$expected_dirs" "$dirs" >>"$EV" 2>&1; then
    detail "the projects' unmanaged Compile source directories are not exactly their own src/main/scala"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "exactly two Scala-only projects, 0 .java files, strata-basics -> strata-collect edge proven"
  fi
  return "$failed"
}

#=============================================================================
# Row 4 - Gate 3 / Rule 2: numerical parity.
#
#   sbt -batch "testOnly *ParitySpec"      (root aggregation runs all six)
#
# then every one of daycount, schedule, fx, currency-math, holiday and
# double-array must have a report under target/parity-report carrying the five
# agreed fields and `failed == 0`. A missing report is a failure, never a pass.
# The specs write their report BEFORE asserting, so the counts are read even
# when the run failed.
#=============================================================================

row_04_numerical_parity() {
  new_evidence gate03-numerical-parity.txt
  local failed=0
  local rc=0

  run_sbt gate03-parity "testOnly *ParitySpec" || rc=$?
  {
    printf '## Gate 3 / Rule 2 - numerical parity at 1e-9 absolute and relative\n'
    printf '# command: sbt -batch "testOnly *ParitySpec" (exit %s)\n' "$rc"
    printf '# log: %s\n\n' "${SBT_LOG#"$ROOT"/}"
  } >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "the parity specs exited $rc"
    failed=1
  fi

  local summary="$AUDIT_DIR/gate03-parity-summary.txt"
  if ! python3 - "$PARITY_DIR" "$summary" <<'PY' >>"$EV" 2>&1; then
import json
import sys

directory, summary_path = sys.argv[1], sys.argv[2]
fixtures = ["daycount", "schedule", "fx", "currency-math", "holiday", "double-array"]
expected_keys = {"fixture", "rows", "passed", "failed", "failures"}

problems = []
lines = ["fixture          rows      passed    failed"]
for fixture in fixtures:
    path = f"{directory}/{fixture}.json"
    try:
        with open(path, encoding="utf-8") as handle:
            report = json.load(handle)
    except FileNotFoundError:
        problems.append(f"{fixture}: report {path} is missing")
        lines.append(f"{fixture:<16} MISSING")
        continue
    except (OSError, ValueError) as error:
        problems.append(f"{fixture}: report {path} could not be read: {error}")
        lines.append(f"{fixture:<16} UNREADABLE")
        continue

    keys = set(report)
    if keys != expected_keys:
        problems.append(
            f"{fixture}: report keys are {sorted(keys)}, expected {sorted(expected_keys)}")
    if report.get("fixture") != fixture:
        problems.append(f"{fixture}: report names fixture {report.get('fixture')!r}")
    rows = report.get("rows", 0)
    passed = report.get("passed", 0)
    failed = report.get("failed", 0)
    lines.append(f"{fixture:<16} {rows:<9} {passed:<9} {failed}")
    # Vacuity: a report of no rows, or one that passed nothing, measured
    # nothing and must not be read as parity. The two counts are reported
    # rather than related to one another: the basics harness counts `passed`
    # in fixture rows while the collect spec counts it in expectations, and
    # the pass condition of this row is `failed == 0`.
    if not isinstance(rows, int) or rows < 1:
        problems.append(f"{fixture}: report evaluated {rows} rows")
    if not isinstance(passed, int) or passed < 1:
        problems.append(f"{fixture}: report records {passed} passed")
    if failed != 0:
        problems.append(f"{fixture}: {failed} row(s) failed parity")
        for failure in report.get("failures", [])[:5]:
            problems.append(f"    {failure}")

with open(summary_path, "w", encoding="utf-8") as handle:
    handle.write("\n".join(lines) + "\n")
    if problems:
        handle.write("\nproblems:\n" + "\n".join(problems) + "\n")

print("\n".join(lines))
if problems:
    print("\nproblems:")
    print("\n".join(problems))
    sys.exit(1)
PY
    detail "the six parity reports are not all present with zero failed rows"
    failed=1
  fi

  if [[ -f "$summary" ]]; then
    add_appendix "Gate 3 - parity row counts per fixture" <"$summary"
  fi
  snapshot_parity_reports

  if [[ "$failed" -eq 0 ]]; then
    local rows
    rows="$(python3 -c 'import json,sys;print(sum(json.load(open(f"{sys.argv[1]}/{n}.json"))["rows"] for n in ["daycount","schedule","fx","currency-math","holiday","double-array"]))' "$PARITY_DIR")"
    detail "six fixtures, $rows rows, 0 failed"
  fi
  return "$failed"
}


#=============================================================================
# Row 5 - Gate 4: serialization round-trip and the closed codec inventory.
#
#   sbt -batch "testOnly *JsonRoundTripSpec *CodecsSpec"
#
# `JsonRoundTripSpec` prints its inventory between the literal markers
# CODEC-COVERAGE-BEGIN and CODEC-COVERAGE-END: one `COVERED <category> <fqcn>`
# line per codec-bearing type, one `EXCLUDED <fqcn> <reason>` line per type
# that carries no codec, then the two count lines. The block is extracted,
# published as evidence, copied into the report and compared - symmetrically,
# with `comm -3` - against the closed inventory of AAP section 0.6.4, which is
# embedded below in sorted order. An absent marker fails the row.
#=============================================================================

# The 58 covered types of AAP section 0.6.4: 14 named-enum, 9 parsed-string,
# 2 hand-written, 29 semiauto-product and 4 explicit-shape, in the exact line
# form the spec prints and in sorted order.
expected_covered_lines() {
  cat <<'EOF'
COVERED explicit-shape com.opengamma.strata.basics.currency.FxMatrix
COVERED explicit-shape com.opengamma.strata.basics.currency.MultiCurrencyAmount
COVERED explicit-shape com.opengamma.strata.collect.array.DoubleArray
COVERED explicit-shape com.opengamma.strata.collect.array.DoubleMatrix
COVERED hand-written com.opengamma.strata.basics.date.DayCount
COVERED hand-written com.opengamma.strata.basics.date.HolidayCalendar
COVERED named-enum com.opengamma.strata.basics.currency.Currency
COVERED named-enum com.opengamma.strata.basics.date.BusinessDayConvention
COVERED named-enum com.opengamma.strata.basics.date.DateSequence
COVERED named-enum com.opengamma.strata.basics.date.PeriodAdditionConvention
COVERED named-enum com.opengamma.strata.basics.index.FloatingRateName
COVERED named-enum com.opengamma.strata.basics.index.FloatingRateType
COVERED named-enum com.opengamma.strata.basics.index.FxIndex
COVERED named-enum com.opengamma.strata.basics.index.IborIndex
COVERED named-enum com.opengamma.strata.basics.index.OvernightIndex
COVERED named-enum com.opengamma.strata.basics.index.PriceIndex
COVERED named-enum com.opengamma.strata.basics.schedule.RollConvention
COVERED named-enum com.opengamma.strata.basics.schedule.StubConvention
COVERED named-enum com.opengamma.strata.basics.value.ValueAdjustmentType
COVERED named-enum com.opengamma.strata.collect.result.FailureReason
COVERED parsed-string com.opengamma.strata.basics.StandardId
COVERED parsed-string com.opengamma.strata.basics.currency.CurrencyPair
COVERED parsed-string com.opengamma.strata.basics.date.HolidayCalendarId
COVERED parsed-string com.opengamma.strata.basics.date.MarketTenor
COVERED parsed-string com.opengamma.strata.basics.date.Tenor
COVERED parsed-string com.opengamma.strata.basics.location.Country
COVERED parsed-string com.opengamma.strata.basics.schedule.Frequency
COVERED parsed-string com.opengamma.strata.collect.Decimal
COVERED parsed-string com.opengamma.strata.collect.FixedScaleDecimal
COVERED semiauto-product com.opengamma.strata.basics.currency.AdjustablePayment
COVERED semiauto-product com.opengamma.strata.basics.currency.BigMoney
COVERED semiauto-product com.opengamma.strata.basics.currency.CurrencyAmount
COVERED semiauto-product com.opengamma.strata.basics.currency.CurrencyAmountArray
COVERED semiauto-product com.opengamma.strata.basics.currency.FxRate
COVERED semiauto-product com.opengamma.strata.basics.currency.Money
COVERED semiauto-product com.opengamma.strata.basics.currency.MultiCurrencyAmountArray
COVERED semiauto-product com.opengamma.strata.basics.currency.Payment
COVERED semiauto-product com.opengamma.strata.basics.date.AdjustableDate
COVERED semiauto-product com.opengamma.strata.basics.date.AdjustableDates
COVERED semiauto-product com.opengamma.strata.basics.date.BusinessDayAdjustment
COVERED semiauto-product com.opengamma.strata.basics.date.DaysAdjustment
COVERED semiauto-product com.opengamma.strata.basics.date.PeriodAdjustment
COVERED semiauto-product com.opengamma.strata.basics.date.SequenceDate
COVERED semiauto-product com.opengamma.strata.basics.date.TenorAdjustment
COVERED semiauto-product com.opengamma.strata.basics.index.FxIndexObservation
COVERED semiauto-product com.opengamma.strata.basics.index.IborIndexObservation
COVERED semiauto-product com.opengamma.strata.basics.index.OvernightIndexObservation
COVERED semiauto-product com.opengamma.strata.basics.index.PriceIndexObservation
COVERED semiauto-product com.opengamma.strata.basics.schedule.PeriodicSchedule
COVERED semiauto-product com.opengamma.strata.basics.schedule.Schedule
COVERED semiauto-product com.opengamma.strata.basics.schedule.SchedulePeriod
COVERED semiauto-product com.opengamma.strata.basics.value.Rounding
COVERED semiauto-product com.opengamma.strata.basics.value.ValueAdjustment
COVERED semiauto-product com.opengamma.strata.basics.value.ValueDerivatives
COVERED semiauto-product com.opengamma.strata.basics.value.ValueSchedule
COVERED semiauto-product com.opengamma.strata.basics.value.ValueStep
COVERED semiauto-product com.opengamma.strata.basics.value.ValueStepSequence
COVERED semiauto-product com.opengamma.strata.collect.result.Failure
EOF
}

# The excluded half of the same closed inventory: the reference-data store and
# everything that populates it, the behavioural and function types, the
# typeclasses, helpers and effect edges, the generic result aliases and the
# abstract heads of the index families. Names only - the reasons are prose.
expected_excluded_names() {
  cat <<'EOF'
com.opengamma.strata.basics.CalculationTarget
com.opengamma.strata.basics.CalculationTargetList
com.opengamma.strata.basics.CombinedReferenceData
com.opengamma.strata.basics.ImmutableReferenceData
com.opengamma.strata.basics.ReferenceData
com.opengamma.strata.basics.ReferenceData.Entry
com.opengamma.strata.basics.ReferenceDataId
com.opengamma.strata.basics.Resolvable
com.opengamma.strata.basics.ResolvableCalculationTarget
com.opengamma.strata.basics.currency.FxConvertible
com.opengamma.strata.basics.currency.FxRateProvider
com.opengamma.strata.basics.currency.LazyFxRateProvider
com.opengamma.strata.basics.date.DateAdjuster
com.opengamma.strata.basics.date.DayCount.ScheduleInfo
com.opengamma.strata.basics.date.HolidaySafeReferenceData
com.opengamma.strata.basics.index.FloatingRate
com.opengamma.strata.basics.index.FloatingRateIndex
com.opengamma.strata.basics.index.Index
com.opengamma.strata.basics.index.IndexObservation
com.opengamma.strata.basics.index.RateIndex
com.opengamma.strata.collect.ArgCheck
com.opengamma.strata.collect.Collections
com.opengamma.strata.collect.DoubleArrayMath
com.opengamma.strata.collect.Named
com.opengamma.strata.collect.TypedStringCompanion
com.opengamma.strata.collect.Validate
com.opengamma.strata.collect.array.Matrix
com.opengamma.strata.collect.io.Resources
com.opengamma.strata.collect.json.Codecs
com.opengamma.strata.collect.named.NamedEnum
com.opengamma.strata.collect.result.FailureOr
com.opengamma.strata.collect.result.ResultNec
com.opengamma.strata.collect.result.ValidatedFailures
com.opengamma.strata.collect.result.ValueWithFailures
EOF
}

row_05_serialization_round_trip() {
  new_evidence gate04-serialization.txt
  local failed=0
  local rc=0

  run_sbt gate04-codec "testOnly *JsonRoundTripSpec *CodecsSpec" || rc=$?
  local log="$SBT_LOG"
  local block="$AUDIT_DIR/codec-coverage.txt"

  {
    printf '## Gate 4 - serialization round-trip and codec coverage\n'
    printf '# command: sbt -batch "testOnly *JsonRoundTripSpec *CodecsSpec" (exit %s)\n' "$rc"
    printf '# log: %s\n\n' "${log#"$ROOT"/}"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "the codec specs exited $rc"
    failed=1
  fi

  # The first block between the two markers. Lines a forked JVM printed arrive
  # unprefixed; the same text relayed by sbt's logger arrives prefixed, so the
  # prefix is stripped before matching.
  strip_sbt_prefix <"$log" |
    awk '/^CODEC-COVERAGE-BEGIN$/ { inside = 1 }
         inside { print }
         /^CODEC-COVERAGE-END$/ { if (inside) exit }' >"$block"

  if [[ ! -s "$block" ]]; then
    detail "the CODEC-COVERAGE-BEGIN/END block is absent from the spec output"
    printf '# the marker block was not found in the log\n' >>"$EV"
    return 1
  fi

  local first last
  first="$(head -n 1 "$block")"
  last="$(tail -n 1 "$block")"
  if [[ "$first" != "CODEC-COVERAGE-BEGIN" || "$last" != "CODEC-COVERAGE-END" ]]; then
    detail "the coverage block is not delimited by its two markers"
    failed=1
  fi

  local actual_covered="$AUDIT_DIR/gate04-covered-actual.txt"
  local actual_excluded="$AUDIT_DIR/gate04-excluded-actual.txt"
  local want_covered="$AUDIT_DIR/gate04-covered-expected.txt"
  local want_excluded="$AUDIT_DIR/gate04-excluded-expected.txt"

  awk '/^COVERED / { print }' "$block" | sort >"$actual_covered"
  awk '/^EXCLUDED / { print $2 }' "$block" | sort >"$actual_excluded"
  expected_covered_lines | sort >"$want_covered"
  expected_excluded_names | sort >"$want_excluded"

  local covered_diff excluded_diff
  covered_diff="$(comm -3 "$want_covered" "$actual_covered")"
  excluded_diff="$(comm -3 "$want_excluded" "$actual_excluded")"

  {
    printf '# covered types printed: %s (expected %s)\n' \
      "$(wc -l <"$actual_covered" | tr -d ' ')" "$(wc -l <"$want_covered" | tr -d ' ')"
    printf '# excluded types printed: %s (expected %s)\n' \
      "$(wc -l <"$actual_excluded" | tr -d ' ')" "$(wc -l <"$want_excluded" | tr -d ' ')"
    printf '# comm -3 expected/actual, covered (empty means identical):\n%s\n' "$covered_diff"
    printf '# comm -3 expected/actual, excluded (empty means identical):\n%s\n' "$excluded_diff"
  } >>"$EV"

  if [[ -n "$covered_diff" ]]; then
    detail "the covered list differs from the closed section 0.6.4 inventory"
    failed=1
  fi
  if [[ -n "$excluded_diff" ]]; then
    detail "the excluded list differs from the closed section 0.6.4 inventory"
    failed=1
  fi

  # The two count lines must agree with the lines they count.
  local stated_covered stated_excluded
  stated_covered="$(awk '/^COVERED-COUNT / { print $2 }' "$block")"
  stated_excluded="$(awk '/^EXCLUDED-COUNT / { print $2 }' "$block")"
  if [[ "${stated_covered:-}" != "$(wc -l <"$actual_covered" | tr -d ' ')" ]]; then
    detail "COVERED-COUNT (${stated_covered:-absent}) disagrees with the covered lines"
    failed=1
  fi
  if [[ "${stated_excluded:-}" != "$(wc -l <"$actual_excluded" | tr -d ' ')" ]]; then
    detail "EXCLUDED-COUNT (${stated_excluded:-absent}) disagrees with the excluded lines"
    failed=1
  fi

  add_appendix "Gate 4 - codec coverage block printed by JsonRoundTripSpec" <"$block"

  if [[ "$failed" -eq 0 ]]; then
    detail "$(wc -l <"$actual_covered" | tr -d ' ') covered and $(wc -l <"$actual_excluded" | tr -d ' ') excluded types, identical to the closed inventory"
  fi
  return "$failed"
}

#=============================================================================
# Row 6 - Gate 5 / Rule 3: no `var` in domain code.
#
#   grep -rnw var strata-collect/src/main/scala strata-basics/src/main/scala
#
# must be empty. Whole-word match, and there are no file exclusions: every
# occurrence of the token in either module's main sources fails this row.
#=============================================================================

row_06_no_var() {
  new_evidence gate05-no-var.txt
  printf '## Gate 5 / Rule 3 - no `var` in either module main sources\n\n' >>"$EV"
  if assert_no_match "var in main sources" "$EV" -rnw var "$COLLECT_MAIN" "$BASICS_MAIN"; then
    detail "no occurrence of the token in $COLLECT_MAIN or $BASICS_MAIN"
    return 0
  fi
  return 1
}

#=============================================================================
# Row 7 - Gate 5 / Rule 3: no boxing in the numeric hot paths.
#
# A deterministic bytecode assertion, never a timing test. `javap -c -p` is
# run over exactly five class files, the disassembly is restricted to the
# bodies of the named hot methods - and of the helpers they delegate to, which
# Scala 2 emits either as private methods or as lifted `$anonfun$<name>$<n>`
# methods - and each body must contain no boxing call.
#
# Note the packages: DoubleArray and DoubleMatrix are in
# `com.opengamma.strata.collect.array`, DoubleArrayMath is in
# `com.opengamma.strata.collect`.
#
# Specialised Function1 calls such as `apply$mcDI$sp` are permitted: they are
# the unboxed path and do not match the forbidden pattern.
#=============================================================================

# The hot-method selector. Applied to the method name of each javap member
# declaration: an optional `$anonfun$` prefix (the lifted body of a lambda
# passed to one of these methods), one of the named hot methods, an optional
# CamelCase suffix (`plusInto`, `sumFrom`, `mapWithIndexInto`, ...) and any
# number of Scala's `$<n>` lifting suffixes.
BOXING_HOT_NAMES='plus|minus|multipliedBy|dividedBy|map|mapWithIndex|combine|reduce|sum|min|max|equalWithTolerance|tabulate|transpose|total'
BOXING_HOT_REGEX="^([$]anonfun[$])?($BOXING_HOT_NAMES)([A-Z][A-Za-z0-9]*)?([$][A-Za-z0-9]+)*$"
BOXING_FORBIDDEN='scala/runtime/BoxesRunTime\|java/lang/Double.valueOf\|Double.doubleValue'

row_07_no_boxing() {
  new_evidence gate05-no-boxing.txt
  local failed=0

  {
    printf '## Gate 5 / Rule 3 - no boxing in the numeric hot paths\n'
    printf '# hot-method selector: %s\n' "$BOXING_HOT_REGEX"
    printf '# forbidden: %s\n\n' "$BOXING_FORBIDDEN"
  } >>"$EV"

  local classes=(
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray\$.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleMatrix.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleMatrix\$.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/DoubleArrayMath\$.class"
  )

  local class_file
  for class_file in "${classes[@]}"; do
    if ! require_file "$class_file" "hot-path class file"; then
      failed=1
      printf '# MISSING: %s\n' "$class_file" >>"$EV"
      continue
    fi

    # The companion object's class file differs from the class's only by a
    # trailing `$`, which must survive into the evidence file name or the two
    # dumps would overwrite one another.
    local class_stem
    class_stem="$(basename "$class_file" .class)"
    local bodies="$AUDIT_DIR/gate05-boxing-${class_stem//\$/-object}.txt"
    local rc=0
    if ! javap -c -p "$class_file" >"$AUDIT_DIR/gate05-disasm.tmp" 2>>"$EV"; then
      detail "javap failed on $class_file"
      failed=1
      continue
    fi

    # State machine over the disassembly: a member declaration sits at exactly
    # two spaces of indent and carries a parameter list; everything more
    # deeply indented belongs to the member above it. `selected` is reset on
    # every declaration, so a body is never attributed to the wrong member.
    BOXING_HOT_REGEX="$BOXING_HOT_REGEX" awk '
      BEGIN { hot = ENVIRON["BOXING_HOT_REGEX"]; selected = 0; bodies = 0 }
      /^  [^ ]/ {
        selected = 0
        if (index($0, "(") > 0) {
          declaration = $0
          sub(/\(.*/, "", declaration)
          fields = split(declaration, parts, " ")
          name = parts[fields]
          sub(/^.*\./, "", name)
          if (name ~ hot) {
            selected = 1
            bodies++
            print "### method: " $0
          }
        }
        next
      }
      selected { print }
      END { print "### selected-bodies: " bodies + 0 }
    ' "$AUDIT_DIR/gate05-disasm.tmp" >"$bodies"
    rm -f "$AUDIT_DIR/gate05-disasm.tmp"

    local body_count hits
    body_count="$(awk '/^### selected-bodies: / { print $3 }' "$bodies")"
    hits="$(awk -v pattern="scala/runtime/BoxesRunTime|java/lang/Double.valueOf|Double.doubleValue" \
      '$0 ~ pattern { n++ } END { print n + 0 }' "$bodies")"

    {
      printf '# %s\n' "${class_file#"$ROOT"/}"
      printf '#   hot bodies selected: %s\n' "$body_count"
      printf '#   boxing calls found: %s\n' "$hits"
      printf '#   bodies: %s\n' "${bodies#"$ROOT"/}"
      if [[ "${hits:-0}" -ne 0 ]]; then
        awk -v pattern="scala/runtime/BoxesRunTime|java/lang/Double.valueOf|Double.doubleValue" \
          '/^### method: / { current = $0 } $0 ~ pattern { print current; print "    " $0 }' "$bodies"
      fi
      printf '\n'
    } >>"$EV"

    # Vacuity: a selector that matched nothing would report zero boxing calls.
    if [[ "${body_count:-0}" -lt 1 ]]; then
      detail "no hot-method body found in $(basename "$class_file")"
      failed=1
    fi
    if [[ "${hits:-0}" -ne 0 ]]; then
      detail "$hits boxing call(s) in the hot paths of $(basename "$class_file")"
      failed=1
    fi
  done

  if [[ "$failed" -eq 0 ]]; then
    detail "five hot-path classes disassembled, zero boxing calls in every selected body"
  fi
  return "$failed"
}

#=============================================================================
# Row 8 - Gate 5 / Rule 5: explicit error handling.
#
#   * no `null` in either module's main sources;
#   * no `throw new` outside ArgCheck.scala, which is where the documented
#     fail-fast invariant throws live;
#   * the four specs that prove the failable surface green.
#=============================================================================

row_08_explicit_error_handling() {
  new_evidence gate05-error-handling.txt
  local failed=0
  local rc=0

  printf '## Gate 5 / Rule 5 - explicit error handling\n\n' >>"$EV"

  if ! assert_no_match "null in main sources" "$EV" \
    -rnw null "$COLLECT_MAIN" "$BASICS_MAIN"; then
    failed=1
  fi

  # `throw new` is filtered rather than piped: with `pipefail` a pipeline
  # whose first grep matches nothing fails, and "nothing matched" is the pass.
  local throws
  local throw_rc=0
  throws="$(grep -rnE "throw new" "$COLLECT_MAIN" "$BASICS_MAIN" 2>&1)" || throw_rc=$?
  local outside
  outside="$(printf '%s\n' "$throws" | awk '!/\/ArgCheck\.scala/ && NF')"
  {
    printf '## throw new outside ArgCheck.scala\n'
    printf '# command: grep -rnE "throw new" %s %s (exit %s), excluding /ArgCheck.scala\n' \
      "$COLLECT_MAIN" "$BASICS_MAIN" "$throw_rc"
    printf '# all matches:\n%s\n' "$throws"
    printf '# matches outside ArgCheck.scala:\n%s\n\n' "$outside"
  } >>"$EV"
  case "$throw_rc" in
    0 | 1) ;;
    *)
      detail "the throw-new scan failed with status $throw_rc"
      failed=1
      ;;
  esac
  if [[ -n "$outside" ]]; then
    detail "$(printf '%s\n' "$outside" | wc -l | tr -d ' ') throw(s) outside ArgCheck.scala"
    failed=1
  fi

  run_sbt gate05-failable "testOnly *SmartConstructorSpec *FailableSurfaceSpec *ApiSurfaceSpec *FailureSpec" || rc=$?
  {
    printf '# command: sbt -batch "testOnly *SmartConstructorSpec *FailableSurfaceSpec '
    printf '*ApiSurfaceSpec *FailureSpec" (exit %s)\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    awk '/Total number of tests run|Tests: succeeded|All tests passed/ { print }' "$SBT_LOG"
  } >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "the failable-surface specs exited $rc"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "no null, no throw outside ArgCheck.scala, failable-surface specs green"
  fi
  return "$failed"
}

#=============================================================================
# Row 9 - Gate 5 / Rule 7: `IO` only at the edges.
#
# `cats.effect` and `IO[` may appear only under strata-basics' demo package
# and in strata-collect's io/Resources.scala. Both scans must be empty.
#=============================================================================

row_09_io_at_the_edges() {
  new_evidence gate05-io-edges.txt
  local failed=0
  printf '## Gate 5 / Rule 7 - IO only at the edges\n\n' >>"$EV"

  local files rc

  rc=0
  files="$(grep -rlE "cats\.effect|\bIO\[" "$BASICS_MAIN" 2>&1)" || rc=$?
  local basics_leak
  basics_leak="$(printf '%s\n' "$files" | awk '!/\/demo\// && NF')"
  {
    printf '## cats.effect or IO[ in strata-basics main sources outside /demo/\n'
    printf '# grep exit status: %s\n' "$rc"
    printf '# files matched:\n%s\n' "$files"
    printf '# outside /demo/:\n%s\n\n' "$basics_leak"
  } >>"$EV"
  if [[ "$rc" -gt 1 ]]; then
    detail "the strata-basics IO scan failed with status $rc"
    failed=1
  fi
  if [[ -n "$basics_leak" ]]; then
    detail "IO outside demo: $(printf '%s\n' "$basics_leak" | tr '\n' ' ')"
    failed=1
  fi

  rc=0
  files="$(grep -rlE "cats\.effect|\bIO\[" "$COLLECT_MAIN" 2>&1)" || rc=$?
  local collect_leak
  collect_leak="$(printf '%s\n' "$files" | awk '!/\/io\/Resources\.scala/ && NF')"
  {
    printf '## cats.effect or IO[ in strata-collect main sources outside io/Resources.scala\n'
    printf '# grep exit status: %s\n' "$rc"
    printf '# files matched:\n%s\n' "$files"
    printf '# outside io/Resources.scala:\n%s\n\n' "$collect_leak"
  } >>"$EV"
  if [[ "$rc" -gt 1 ]]; then
    detail "the strata-collect IO scan failed with status $rc"
    failed=1
  fi
  if [[ -n "$collect_leak" ]]; then
    detail "IO outside io/Resources.scala: $(printf '%s\n' "$collect_leak" | tr '\n' ' ')"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "cats.effect and IO confined to demo/ and collect io/Resources.scala"
  fi
  return "$failed"
}


#=============================================================================
# Row 10 - Gate 5: typeclass instances.
#
#   sbt -batch "testOnly *TypeclassLawsSpec"
#
# The spec's `InstanceInventory` summons every promised instance at COMPILE
# time, so the suite does not compile unless they exist, and prints the
# inventory - one `TYPECLASS-INVENTORY <typeclass> <type>` line per instance
# plus one summary line - which is copied into the gate report. The inventory
# not being locatable in the output fails the row.
#=============================================================================

row_10_typeclass_instances() {
  new_evidence gate05-typeclass-instances.txt
  local failed=0
  local rc=0

  run_sbt gate05-typeclass "testOnly *TypeclassLawsSpec" || rc=$?
  local inventory="$AUDIT_DIR/typeclass-inventory.txt"
  strip_sbt_prefix <"$SBT_LOG" | awk '/^TYPECLASS-INVENTORY/ { print }' >"$inventory"

  local lines summary
  lines="$(awk '/^TYPECLASS-INVENTORY / { n++ } END { print n + 0 }' "$inventory")"
  summary="$(awk '/^TYPECLASS-INVENTORY-SUMMARY/ { print; exit }' "$inventory")"

  {
    printf '## Gate 5 - typeclass instances and their laws\n'
    printf '# command: sbt -batch "testOnly *TypeclassLawsSpec" (exit %s)\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    printf '# instance lines: %s\n' "$lines"
    printf '# summary: %s\n\n' "${summary:-absent}"
    awk '/Total number of tests run|Tests: succeeded|All tests passed/ { print }' "$SBT_LOG"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "TypeclassLawsSpec exited $rc"
    failed=1
  fi
  if [[ "${lines:-0}" -lt 1 ]]; then
    detail "no TYPECLASS-INVENTORY line found in the spec output"
    failed=1
  fi
  if [[ -z "$summary" ]]; then
    detail "the TYPECLASS-INVENTORY-SUMMARY line is absent"
    failed=1
  fi

  if [[ -s "$inventory" ]]; then
    add_appendix "Gate 5 - typeclass instance inventory summoned by TypeclassLawsSpec" \
      <"$inventory"
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "$lines instances summoned and law-checked; $summary"
  fi
  return "$failed"
}

#=============================================================================
# Row 11 - Gate 5 / Rule 4: closed enums and reference-data fidelity.
#
#   sbt -batch "testOnly *NamedEnumClosedSpec *ReferenceDataManifestSpec"
#
# plus: no resource lookup in either module's main sources.
#
# The specification writes that second check as
#   grep -rn "\.ini\|\.csv\|\.properties" strata-collect/src/main strata-basics/src/main
# whose unescaped `.` wildcards and absent word boundary also match ordinary
# identifiers - `initialValue` matches `\.ini`. The check implemented here is
# the one the specification asks for, "no resource lookups in main sources",
# expressed as a file-extension match: `\.(ini|csv|properties)\b`, which still
# catches every reference to a resource FILE. The literal pattern is run too,
# and every line it reports is recorded in the evidence, annotated, so nothing
# is hidden by the narrowing.
#=============================================================================

row_11_closed_enums() {
  new_evidence gate05-closed-enums.txt
  local failed=0
  local rc=0

  run_sbt gate05-named-enum "testOnly *NamedEnumClosedSpec *ReferenceDataManifestSpec" || rc=$?
  {
    printf '## Gate 5 / Rule 4 - closed named families and reference-data fidelity\n'
    printf '# command: sbt -batch "testOnly *NamedEnumClosedSpec *ReferenceDataManifestSpec" (exit %s)\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    awk '/Total number of tests run|Tests: succeeded|All tests passed/ { print }' "$SBT_LOG"
    printf '\n'
  } >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "the closed-enum and manifest specs exited $rc"
    failed=1
  fi

  if ! assert_no_match "resource file references in main sources" "$EV" \
    -rnE "\.(ini|csv|properties)\b" strata-collect/src/main strata-basics/src/main; then
    failed=1
  fi

  # The specification's literal pattern, recorded for completeness. Its
  # matches are reported, not judged: see the comment above this function.
  local literal
  local literal_rc=0
  literal="$(grep -rn "\.ini\|\.csv\|\.properties" strata-collect/src/main strata-basics/src/main 2>&1)" ||
    literal_rc=$?
  {
    printf '## the specification'"'"'s literal pattern, for completeness\n'
    printf '# command: grep -rn "\\.ini\\|\\.csv\\|\\.properties" strata-collect/src/main strata-basics/src/main\n'
    printf '# grep exit status: %s\n' "$literal_rc"
    printf '# NOTE: an unescaped "." is a wildcard, so these lines include identifier\n'
    printf '#       matches such as "initialValue" that are not resource lookups. The\n'
    printf '#       row is decided by the extension-anchored check above.\n'
    printf '%s\n\n' "$literal"
  } >>"$EV"
  if [[ "$literal_rc" -gt 1 ]]; then
    detail "the literal resource scan failed with status $literal_rc"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "closed families and data tables verified; no resource lookup in main sources"
  fi
  return "$failed"
}

#=============================================================================
# Row 12 - Rule 6: no reflection on the codec path.
#
# Three parts, all required.
#
# (a) Every class file of both modules is disassembled and grepped for the
#     reflection API. The count must be 0.
#
# (b) The SAME spec is run twice under a class-loading log, differing only in
#     whether encode and decode execute: `-Dcodec.audit=baseline` generates
#     every value from the fixed seed and renders it, `-Dcodec.audit=codec`
#     additionally encodes and decodes it. The difference between the two sets
#     of loaded classes is therefore attributable to encoding, and it must
#     hold no class of any reflection package.
#
#     One correction is applied to the comparison, and it is what makes the
#     comparison mean anything: the JVM names a hidden class after its host
#     plus the ADDRESS at which it was defined, e.g.
#     `java.lang.reflect.Proxy$$Lambda/0x00007af...`. Those names never agree
#     between two JVM runs, so a raw difference reports every hidden class of
#     the second run - here about 1800 of them, including hidden classes of
#     the JDK's own Proxy and annotation machinery that the BASELINE run
#     loaded too. The address suffix is therefore stripped before the sets are
#     compared. Nothing real is hidden by that: a genuine reflective call
#     loads `java.lang.reflect.Method`, `jdk.internal.reflect.*` or
#     `scala.reflect.runtime.*` under their own, address-free names, and those
#     survive the normalisation.
#
#     The digest each run prints is also compared. Both runs do the same
#     baseline work, so the digests must be equal; were they not, the two runs
#     would have generated different values and their class-load difference
#     would no longer be attributable to encoding.
#
# (c) Every class in the difference is disassembled and grepped as in (a). A
#     JVM-generated hidden class cannot be disassembled under its own name, so
#     it is listed in the report as "not inspectable (JVM-generated)" - a
#     documented exception, never a silent skip - and its HOST class, which
#     the log's `source:` field names, is inspected in its place.
#=============================================================================

RULE6_REFLECTION_PATTERN='java/lang/reflect/|java/lang/Class.forName|getDeclaredMethod|getDeclaredField'
RULE6_REFLECTION_PACKAGES='^(java\.lang\.reflect|sun\.reflect|jdk\.internal\.reflect|scala\.reflect\.runtime)\.'

# Extracts `<class name><TAB><source>` from a -Xlog:class+load log, with the
# hidden-class address suffix removed. $1 log, $2 names file, $3 map file.
classload_index() {
  awk 'index($1, "[class,load]") > 0 && $3 == "source:" {
         name = $2
         sub(/\/0x[0-9a-f]+$/, "", name)
         source = $4
         for (i = 5; i <= NF; i++) source = source " " $i
         print name "\t" source
       }' "$1" | sort -u >"$3"
  cut -f1 "$3" | sort -u >"$2"
}

# Disassembles one class, resolving where to read it from its recorded source.
# $1 class name, $2 source; prints the disassembly.
javap_from_source() {
  local class_name="$1"
  local source="$2"
  case "$source" in
    'shared objects file' | jrt:* | __JVM* | '')
      javap -c -p "$class_name"
      ;;
    file:*)
      javap -c -p -cp "${source#file:}" "$class_name"
      ;;
    *)
      javap -c -p -cp "$source" "$class_name"
      ;;
  esac
}

row_12_no_reflection() {
  new_evidence rule6-no-reflection.txt
  local failed=0

  printf '## Rule 6 - no reflection on the codec path\n\n' >>"$EV"

  # -- (a) both modules' own classes ---------------------------------------
  local class_count
  class_count="$(find "$COLLECT_CLASSES" "$BASICS_CLASSES" -name '*.class' 2>/dev/null | wc -l | tr -d ' ')"
  local dump="$AUDIT_DIR/rule6-main-disassembly.tmp"
  local matches="$AUDIT_DIR/rule6a-matches.txt"
  local javap_rc=0

  if [[ "$class_count" -lt 1 ]]; then
    detail "(a) no class files under either module's classes directory"
    printf '# no class files found; was the build run?\n' >>"$EV"
    failed=1
  else
    find strata-collect/target strata-basics/target -path '*scala-2.13/classes/*.class' \
      -exec javap -c -p {} + >"$dump" 2>"$AUDIT_DIR/rule6a-javap-errors.txt" || javap_rc=$?
    awk -v pattern="$RULE6_REFLECTION_PATTERN" \
      '$0 ~ pattern { print; n++ } END { print "### matches: " n + 0 }' "$dump" >"$matches"
    local hits
    hits="$(awk '/^### matches: / { print $3 }' "$matches")"
    {
      printf '# (a) command: find strata-collect/target strata-basics/target '
      printf -- '-path "*scala-2.13/classes/*.class" -exec javap -c -p {} +\n'
      printf '#     class files: %s, javap exit %s, disassembly lines %s\n' \
        "$class_count" "$javap_rc" "$(wc -l <"$dump" | tr -d ' ')"
      printf '#     reflection references: %s (see %s)\n\n' "$hits" "${matches#"$ROOT"/}"
    } >>"$EV"
    rm -f "$dump"
    if [[ "$javap_rc" -ne 0 ]]; then
      detail "(a) javap failed with status $javap_rc"
      failed=1
    fi
    if [[ "${hits:-0}" -ne 0 ]]; then
      detail "(a) $hits reflection reference(s) in the modules' own classes"
      failed=1
    fi
  fi

  # -- (b) the class-load difference ---------------------------------------
  local baseline_log="$AUDIT_DIR/baseline-classload.log"
  local codec_log="$AUDIT_DIR/codec-classload.log"
  rm -f "$baseline_log" "$codec_log"

  local mode rc
  for mode in baseline codec; do
    rc=0
    run_sbt "rule6-audit-$mode" \
      "set \`strata-basics\` / Test / javaOptions ++= Seq(\"-Dcodec.audit=$mode\", \"-Xlog:class+load:file=$AUDIT_DIR/$mode-classload.log\")" \
      "strata-basics/testOnly com.opengamma.strata.basics.json.JsonRoundTripSpec" || rc=$?
    local digest
    digest="$(strip_sbt_prefix <"$SBT_LOG" | awk '/^CODEC-AUDIT-DIGEST / { print; exit }')"
    {
      printf '# (b) %s run: sbt exit %s\n' "$mode" "$rc"
      printf '#     digest line: %s\n' "${digest:-absent}"
      printf '#     log: %s\n' "${SBT_LOG#"$ROOT"/}"
    } >>"$EV"
    if [[ "$rc" -ne 0 ]]; then
      detail "(b) the $mode audit run exited $rc"
      failed=1
    fi
    if [[ -z "$digest" ]]; then
      detail "(b) the $mode audit run printed no CODEC-AUDIT-DIGEST line"
      failed=1
    fi
    if [[ "$mode" == "baseline" ]]; then
      RULE6_BASELINE_DIGEST="$digest"
    else
      RULE6_CODEC_DIGEST="$digest"
    fi
  done

  # Both runs do the same baseline work, so the value count and digest agree.
  local baseline_work codec_work
  baseline_work="$(printf '%s\n' "${RULE6_BASELINE_DIGEST:-}" | awk '{ print $3, $4 }')"
  codec_work="$(printf '%s\n' "${RULE6_CODEC_DIGEST:-}" | awk '{ print $3, $4 }')"
  if [[ -z "$baseline_work" || "$baseline_work" != "$codec_work" ]]; then
    detail "(b) the two audit runs disagree on their baseline work ('$baseline_work' vs '$codec_work'), so the class-load difference is not attributable to encoding"
    failed=1
  fi

  local baseline_names="$AUDIT_DIR/rule6-baseline-classes.txt"
  local codec_names="$AUDIT_DIR/rule6-codec-classes.txt"
  local baseline_map="$AUDIT_DIR/rule6-baseline-map.txt"
  local codec_map="$AUDIT_DIR/rule6-codec-map.txt"
  local delta="$AUDIT_DIR/rule6-delta-classes.txt"
  local combined_map="$AUDIT_DIR/rule6-classload-map.txt"

  if [[ ! -s "$baseline_log" || ! -s "$codec_log" ]]; then
    detail "(b) a class-load log is missing or empty"
    printf '# baseline log: %s bytes; codec log: %s bytes\n' \
      "$(wc -c <"$baseline_log" 2>/dev/null || printf 0)" \
      "$(wc -c <"$codec_log" 2>/dev/null || printf 0)" >>"$EV"
    return 1
  fi

  classload_index "$baseline_log" "$baseline_names" "$baseline_map"
  classload_index "$codec_log" "$codec_names" "$codec_map"
  comm -13 "$baseline_names" "$codec_names" >"$delta"
  sort -u "$baseline_map" "$codec_map" >"$combined_map"

  local raw_delta
  raw_delta="$(comm -13 \
    <(awk 'index($1, "[class,load]") > 0 { print $2 }' "$baseline_log" | sort -u) \
    <(awk 'index($1, "[class,load]") > 0 { print $2 }' "$codec_log" | sort -u) | wc -l | tr -d ' ')"

  RULE6_DELTA_SIZE="$(wc -l <"$delta" | tr -d ' ')"
  {
    printf '\n# (b) baseline classes: %s, codec classes: %s\n' \
      "$(wc -l <"$baseline_names" | tr -d ' ')" "$(wc -l <"$codec_names" | tr -d ' ')"
    printf '#     delta (codec only): %s classes -- see %s\n' \
      "$RULE6_DELTA_SIZE" "${delta#"$ROOT"/}"
    printf '#     delta before the hidden-class address suffix is normalised: %s (address noise)\n' \
      "$raw_delta"
  } >>"$EV"

  if [[ "$RULE6_DELTA_SIZE" -lt 1 ]]; then
    detail "(b) the class-load difference is empty, so the audit measured nothing"
    failed=1
  fi
  if ! assert_no_match "(b) reflection packages in the class-load difference" "$EV" \
    -E "$RULE6_REFLECTION_PACKAGES" "$delta"; then
    failed=1
  fi

  # -- (c) every class of the difference -----------------------------------
  local noninspectable="$AUDIT_DIR/rule6-noninspectable-classes.txt"
  local per_class="$AUDIT_DIR/rule6-delta-inspection.txt"
  : >"$noninspectable"
  : >"$per_class"

  local name source target out class_hits inspect_rc
  local inspected=0
  local unresolved=0
  local total_hits=0
  while IFS= read -r name; do
    [[ -n "$name" ]] || continue
    source="$(awk -F'\t' -v wanted="$name" '$1 == wanted { print $2; exit }' "$combined_map")"
    target="$name"
    if [[ "$name" == *'$$Lambda'* || "$source" == __JVM* ]]; then
      printf '%s\tnot inspectable (JVM-generated); host: %s\n' "$name" "$source" \
        >>"$noninspectable"
      if [[ "$source" =~ ^[A-Za-z_][A-Za-z0-9_.$]*$ ]]; then
        target="$source"
        source="$(awk -F'\t' -v wanted="$target" '$1 == wanted { print $2; exit }' "$combined_map")"
      else
        unresolved=$((unresolved + 1))
        printf '%s\tUNRESOLVED host, source: %s\n' "$name" "$source" >>"$per_class"
        continue
      fi
    fi

    inspect_rc=0
    out="$(javap_from_source "$target" "$source" 2>&1)" || inspect_rc=$?
    if [[ "$inspect_rc" -ne 0 || -z "$out" ]]; then
      printf '%s\tjavap FAILED (status %s, source %s)\n' "$target" "$inspect_rc" "$source" \
        >>"$per_class"
      detail "(c) javap could not disassemble $target"
      failed=1
      continue
    fi
    inspected=$((inspected + 1))
    class_hits="$(printf '%s\n' "$out" |
      awk -v pattern="$RULE6_REFLECTION_PATTERN" '$0 ~ pattern { n++ } END { print n + 0 }')"
    printf '%s\t%s reflection reference(s)\n' "$target" "$class_hits" >>"$per_class"
    if [[ "$class_hits" -ne 0 ]]; then
      total_hits=$((total_hits + class_hits))
      detail "(c) $class_hits reflection reference(s) in $target"
      failed=1
    fi
  done <"$delta"

  {
    printf '\n# (c) delta classes inspected: %s, JVM-generated (not inspectable under their own name): %s\n' \
      "$inspected" "$(wc -l <"$noninspectable" | tr -d ' ')"
    printf '#     hosts that could not be resolved: %s\n' "$unresolved"
    printf '#     reflection references across the difference: %s\n' "$total_hits"
    printf '#     per-class detail: %s\n' "${per_class#"$ROOT"/}"
    printf '#     JVM-generated list: %s\n' "${noninspectable#"$ROOT"/}"
  } >>"$EV"

  if [[ "$unresolved" -ne 0 ]]; then
    detail "(c) $unresolved JVM-generated class(es) could not be traced to a host class"
    failed=1
  fi

  {
    printf 'baseline class-load log : %s\n' "${baseline_log#"$ROOT"/}"
    printf 'codec class-load log    : %s\n' "${codec_log#"$ROOT"/}"
    printf 'baseline digest         : %s\n' "${RULE6_BASELINE_DIGEST:-absent}"
    printf 'codec digest            : %s\n' "${RULE6_CODEC_DIGEST:-absent}"
    printf 'delta size (normalised) : %s classes\n' "$RULE6_DELTA_SIZE"
    printf 'delta size (raw names)  : %s classes, dominated by hidden-class address noise\n' "$raw_delta"
    printf 'delta classes inspected : %s\n' "$inspected"
    printf 'reflection references   : %s\n' "$total_hits"
    printf '\nnot inspectable (JVM-generated), inspected through their host class instead:\n'
    cat "$noninspectable"
  } | add_appendix "Rule 6 - class-load audit of the codec path"

  if [[ "$failed" -eq 0 ]]; then
    detail "no reflection in either module; $RULE6_DELTA_SIZE-class encode/decode difference holds none either"
  fi
  return "$failed"
}

#=============================================================================
# Row 13 - Rule 8: JVM 21 bytecode.
#
# A positive assertion: both named class files must report class-file major
# version 65. A missing file or an absent version line fails the row.
#=============================================================================

row_13_jvm21_bytecode() {
  new_evidence rule8-bytecode-version.txt
  local failed=0
  printf '## Rule 8 - class-file major version 65 (JVM 21)\n\n' >>"$EV"

  local files=(
    "$BASICS_CLASSES/com/opengamma/strata/basics/currency/Currency.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray.class"
  )
  local file version
  for file in "${files[@]}"; do
    if ! require_file "$file" "class file for the bytecode-version check"; then
      printf '# MISSING: %s\n' "$file" >>"$EV"
      failed=1
      continue
    fi
    version="$(javap -v "$file" 2>/dev/null | awk '/major version:/ { print $3; exit }')"
    printf '# %-80s major version: %s\n' "${file#"$ROOT"/}" "${version:-absent}" >>"$EV"
    if [[ "${version:-}" != "65" ]]; then
      detail "$(basename "$file") reports major version ${version:-absent}, expected 65"
      failed=1
    fi
  done

  if [[ "$failed" -eq 0 ]]; then
    detail "both named classes report class-file major version 65"
  fi
  return "$failed"
}


#=============================================================================
# Row 14 - Rule 9: warning-clean.
#
#   * `-Werror` is present in build.sbt, exactly once, in the common settings;
#   * no `@nowarn`, `@SuppressWarnings`, `-Wconf` and no second `Werror`
#     occurrence anywhere in the build definition or either module's sources:
#     the option is never scoped away;
#   * `sbt -batch clean compile Test/compile` succeeds.
#
# Two refinements to the literal scan, both recorded in the evidence with
# every raw match listed, so nothing is hidden:
#
#   1. sbt's own build output (project/target, project/project) is excluded,
#      and binary files are skipped. Those directories hold the COMPILED form
#      of build.sbt, so they carry its `-Werror` text; they are derived
#      artifacts of the file this row already scans, they do not exist in a
#      clean checkout, and they appear only because Gate 1 ran sbt first.
#   2. A match inside a comment is not a suppression - `@nowarn`,
#      `@SuppressWarnings` and `-Wconf` only do anything as code or as a
#      compiler option - so prose that names the flag, of which the specs
#      contain several lines, is classified as allowed and listed as such.
#
# A removal of the option is checked for separately and explicitly, because
# that is the thing the row exists to catch.
#
# This row RUNS `clean`, which empties target/test-reports and
# target/parity-report (build.sbt registers both with `cleanFiles`). The rows
# after it read the snapshots taken in Gate 1 and Gate 3, and the class files
# Rule 10 needs are re-emitted by this row's own compile.
#=============================================================================

row_14_warning_clean() {
  new_evidence rule9-warning-clean.txt
  local failed=0
  local rc=0

  printf '## Rule 9 - warning-clean build\n\n' >>"$EV"

  # -- 1. the option is declared, once -------------------------------------
  local werror_lines werror_count
  werror_lines="$(awk '/"-Werror"/ { print FILENAME ":" FNR ":" $0 }' build.sbt)"
  werror_count="$(printf '%s\n' "$werror_lines" | awk 'NF { n++ } END { print n + 0 }')"
  {
    printf '# command: grep -n %s build.sbt\n' "'\"-Werror\"'"
    printf '%s\n' "$werror_lines"
    printf '# occurrences: %s (exactly 1 expected)\n\n' "$werror_count"
  } >>"$EV"
  if [[ "$werror_count" -ne 1 ]]; then
    detail "build.sbt declares \"-Werror\" $werror_count time(s), expected exactly 1"
    failed=1
  fi

  # -- 2. no suppression anywhere -----------------------------------------
  local build_definition=()
  while IFS= read -r file; do
    build_definition+=("$file")
  done < <(find project -maxdepth 1 -type f | sort)

  local raw raw_rc=0
  raw="$(grep -rnI -E "nowarn|SuppressWarnings|-Wconf|Werror" \
    build.sbt "${build_definition[@]}" strata-collect/src strata-basics/src 2>&1)" || raw_rc=$?

  local remainder
  remainder="$(printf '%s\n' "$raw" | awk '
    NF {
      line = $0
      first = index(line, ":")
      path = substr(line, 1, first - 1)
      rest = substr(line, first + 1)
      second = index(rest, ":")
      text = substr(rest, second + 1)
      # the one declaration of the option, in the common settings
      if (path == "build.sbt" && text ~ /^[[:space:]]*"-Werror"[,]?[[:space:]]*$/) next
      # prose: a comment cannot suppress a warning
      if (text ~ /^[[:space:]]*(\*|\/\/|\/\*)/) next
      print
    }')"

  {
    printf '# command: grep -rnI -E "nowarn|SuppressWarnings|-Wconf|Werror" build.sbt %s strata-collect/src strata-basics/src\n' \
      "${build_definition[*]}"
    printf '# grep exit status: %s\n' "$raw_rc"
    printf '# NOTE: sbt build output (project/target, project/project) is excluded and\n'
    printf '#       binary files are skipped: they hold the compiled form of build.sbt,\n'
    printf '#       which this row scans in source form.\n'
    printf '# all raw matches:\n%s\n' "$raw"
    printf '# matches that are neither the single build.sbt option nor prose in a comment:\n%s\n\n' \
      "$remainder"
  } >>"$EV"

  if [[ "$raw_rc" -gt 1 ]]; then
    detail "the suppression scan failed with status $raw_rc"
    failed=1
  fi
  if [[ -n "$remainder" ]]; then
    detail "$(printf '%s\n' "$remainder" | wc -l | tr -d ' ') suppression candidate(s) outside build.sbt's single option"
    failed=1
  fi

  # -- the option is never removed or filtered away ------------------------
  if ! assert_no_match "removal or filtering of a compiler option" "$EV" \
    -rnI -E "scalacOptions[^=]*(--=|[^-]-=)|filterNot[^\n]*W(error|conf)|W(error|conf)[^\n]*filterNot|excludeLintKeys[^\n]*scalacOptions" \
    build.sbt "${build_definition[@]}" strata-collect/src strata-basics/src; then
    failed=1
  fi

  # -- 3. the build itself --------------------------------------------------
  run_sbt rule9-compile clean compile "Test/compile" || rc=$?
  # `clean` empties target/test-reports and target/parity-report, which CI
  # publishes; this run's own reports are put back from the snapshots.
  restore_snapshots
  {
    printf '# command: sbt -batch clean compile Test/compile (exit %s)\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    printf '# NOTE: this row cleans, which empties target/test-reports and\n'
    printf '#       target/parity-report. The reports this run produced were restored\n'
    printf '#       from target/audit/snapshot afterwards so the published artifacts\n'
    printf '#       are complete; the late rows read the snapshot in either case.\n'
    awk '/^\[warn\]|^\[error\]/ { if (++shown <= 40) print }
         END { print "### warn/error lines: " shown + 0 }' "$SBT_LOG"
  } >>"$EV"
  if [[ "$rc" -ne 0 ]]; then
    detail "sbt clean compile Test/compile exited $rc"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "-Werror declared once, no suppression or scoped removal, clean compile of both modules"
  fi
  return "$failed"
}

#=============================================================================
# Row 15 - Rule 10: Scala collections in the public API.
#
#   * `javap -s -protected` over both modules' MAIN classes must show no
#     java.util collection, Optional, stream or function type and no mutable
#     Scala collection. `javap -s` prints declarations in dot form and
#     `descriptor:` lines in slash form, which is why the matcher accepts both
#     separators. The main-class glob deliberately excludes test-classes.
#   * the same pipeline over the test-scope Rule10NegativeControl, which
#     declares `def bad: java.util.List[String]` for this purpose, must report
#     at least one match: that is what proves the matcher sees descriptors.
#   * the source-level scan must be empty (java.util.Arrays, java.util.Locale
#     and java.time.* are permitted and are not matched).
#=============================================================================

RULE10_PATTERN='java[./]util[./](Collection|List|ArrayList|LinkedList|Map|HashMap|TreeMap|LinkedHashMap|SortedMap|NavigableMap|Set|HashSet|TreeSet|LinkedHashSet|SortedSet|NavigableSet|Queue|Deque|Iterator|ListIterator|Enumeration|Optional|OptionalInt|OptionalLong|OptionalDouble|stream[./]|function[./])|scala[./]collection[./]mutable'

row_15_scala_collections_api() {
  new_evidence rule10-scala-collections.txt
  local failed=0

  printf '## Rule 10 - no Java collection type in either module public API\n\n' >>"$EV"

  # -- main classes --------------------------------------------------------
  local dump="$AUDIT_DIR/rule10-main-signatures.tmp"
  local matches="$AUDIT_DIR/rule10-main-matches.txt"
  local javap_rc=0
  find strata-collect/target strata-basics/target -path '*scala-2.13/classes/*.class' \
    -exec javap -s -protected {} + >"$dump" 2>"$AUDIT_DIR/rule10-javap-errors.txt" || javap_rc=$?

  local descriptors hits
  descriptors="$(awk '/descriptor:/ { n++ } END { print n + 0 }' "$dump")"
  awk -v pattern="$RULE10_PATTERN" '$0 ~ pattern { print; n++ } END { print "### matches: " n + 0 }' \
    "$dump" >"$matches"
  hits="$(awk '/^### matches: / { print $3 }' "$matches")"

  {
    printf '# command: find strata-collect/target strata-basics/target '
    printf -- '-path "*scala-2.13/classes/*.class" -exec javap -s -protected {} +\n'
    printf '#   javap exit %s, signature lines %s, descriptor lines %s\n' \
      "$javap_rc" "$(wc -l <"$dump" | tr -d ' ')" "$descriptors"
    printf '#   matches: %s (see %s)\n\n' "$hits" "${matches#"$ROOT"/}"
  } >>"$EV"
  rm -f "$dump"

  if [[ "$javap_rc" -ne 0 ]]; then
    detail "javap over the main classes failed with status $javap_rc"
    failed=1
  fi
  # Vacuity: no descriptors means nothing was scanned.
  if [[ "${descriptors:-0}" -lt 1 ]]; then
    detail "the main-class signature scan produced no descriptor line"
    failed=1
  fi
  if [[ "${hits:-0}" -ne 0 ]]; then
    detail "$hits Java-collection signature(s) in the modules' public API"
    failed=1
  fi

  # -- negative control ----------------------------------------------------
  local control="$COLLECT_TEST_CLASSES/com/opengamma/strata/collect/audit/Rule10NegativeControl.class"
  if ! require_file "$control" "Rule 10 negative control class"; then
    printf '# MISSING negative control: %s\n' "$control" >>"$EV"
    failed=1
  else
    local control_hits
    control_hits="$(javap -s -protected "$control" 2>&1 |
      awk -v pattern="$RULE10_PATTERN" '$0 ~ pattern { n++ } END { print n + 0 }')"
    {
      printf '# negative control: %s\n' "${control#"$ROOT"/}"
      printf '#   matches: %s (at least 1 required, proving the matcher sees descriptors)\n\n' \
        "$control_hits"
      javap -s -protected "$control" 2>&1 |
        awk -v pattern="$RULE10_PATTERN" '$0 ~ pattern { print "    " $0 }'
    } >>"$EV"
    if [[ "${control_hits:-0}" -lt 1 ]]; then
      detail "the negative control reported no match, so the matcher is not working"
      failed=1
    fi
  fi

  # -- sources -------------------------------------------------------------
  if ! assert_no_match "Java collection types in the module main sources" "$EV" \
    -rnE "java\.util\.(Collection|List|Map|Set|SortedMap|SortedSet|Optional|OptionalInt|OptionalDouble|Iterator|stream|function)" \
    "$COLLECT_MAIN" "$BASICS_MAIN"; then
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "no Java collection, Optional, stream or function type in either public API; negative control caught"
  fi
  return "$failed"
}

#=============================================================================
# Row 16 - Gate 6: end-to-end demo.
#
#   sbt -batch "strata-basics/run"
#
# must exit 0 and print the JSON of an FX-converted MultiCurrencyAmount
# alongside the adjusted PeriodicSchedule dates. The output is captured as
# evidence and is asserted to carry both a currency/amount JSON fragment and
# at least one ISO-8601 date.
#=============================================================================

row_16_demo() {
  new_evidence gate06-demo.txt
  local failed=0
  local rc=0

  run_sbt gate06-demo "strata-basics/run" || rc=$?
  local output="$AUDIT_DIR/gate06-demo-output.txt"
  strip_sbt_prefix <"$SBT_LOG" >"$output"

  local json_hits date_hits
  json_hits="$(awk '/"currency"[[:space:]]*:[[:space:]]*"[A-Z][A-Z][A-Z]"/ && /"amount"[[:space:]]*:/ { n++ } END { print n + 0 }' "$output")"
  date_hits="$(awk '/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/ { n++ } END { print n + 0 }' "$output")"

  {
    printf '## Gate 6 - end-to-end demo\n'
    printf '# command: sbt -batch "strata-basics/run" (exit %s)\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    printf '# currency/amount JSON fragments: %s\n' "$json_hits"
    printf '# ISO-8601 dates: %s\n\n' "$date_hits"
    printf '# demo output:\n'
    cat "$output"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "sbt \"strata-basics/run\" exited $rc"
    failed=1
  fi
  if [[ "${json_hits:-0}" -lt 1 ]]; then
    detail "the demo printed no currency/amount JSON fragment"
    failed=1
  fi
  if [[ "${date_hits:-0}" -lt 1 ]]; then
    detail "the demo printed no ISO-8601 date"
    failed=1
  fi

  if [[ "$failed" -ne 0 ]]; then
    add_appendix "Gate 6 - demo output (row failed)" <"$output"
  else
    detail "the demo ran, printing $json_hits JSON amount fragment(s) and $date_hits date(s)"
  fi
  return "$failed"
}


#=============================================================================
# Row 17 - Gate 7: migration note, automated half.
#
#   * SCALA_MIGRATION.md exists;
#   * its six sections (a) to (f) are present as `## ` headings;
#   * the member table of section (a) has at least as many rows as there are
#     distinct `strata-collect` members referenced from modules/basics/src.
#     That count is COMPUTED by the specification's own grep, never hardcoded.
#
# The manual half - a CODEOWNERS review of the note's content - is the
# reported row that follows this one. This script never blocks on a human.
#=============================================================================

GATE7_MANUAL_TEXT="automated checks passed; manual approval: see PR review"

row_17_migration_note() {
  new_evidence gate07-migration-note.txt
  local failed=0
  local note="SCALA_MIGRATION.md"

  printf '## Gate 7 - migration note (automated half)\n\n' >>"$EV"

  if ! require_file "$note" "migration note"; then
    printf '# MISSING: %s\n' "$note" >>"$EV"
    return 1
  fi

  # -- the six sections ----------------------------------------------------
  local headings heading_count
  headings="$(awk '/^## / { print }' "$note")"
  heading_count="$(printf '%s\n' "$headings" | awk 'NF { n++ } END { print n + 0 }')"
  {
    printf '# command: grep -c "^## " %s\n' "$note"
    printf '%s\n' "$heading_count"
    printf '# headings:\n%s\n\n' "$headings"
  } >>"$EV"
  if [[ "$heading_count" -lt 6 ]]; then
    detail "$note carries $heading_count '## ' heading(s), expected at least the six sections (a)-(f)"
    failed=1
  fi

  # -- the member count, computed ------------------------------------------
  local member_count
  member_count="$(grep -rhoE "\b(ArgChecker|Guavate|MapStream|Messages|Decimal|FixedScaleDecimal|DoubleArray|DoubleMatrix|DoubleArrayMath|ExtendedEnum|EnumNames|NamedLookup|TypedString|ResourceLocator|ResourceConfig|IniFile|CsvFile|CsvRow|PropertySet|PropertiesFile|Result|FailureItem|FailureReason|ValueWithFailures|TestHelper|CollectProjectAssertions|Unchecked)\.[a-zA-Z]+" \
    modules/basics/src | sort -u | wc -l | tr -d ' ')"
  GATE7_MEMBER_COUNT="$member_count"

  # -- the section (a) table row count -------------------------------------
  local table="$AUDIT_DIR/gate07-section-a-table.txt"
  awk '/^## \(a\)/ { inside = 1; next }
       inside && /^## / { exit }
       inside && /^\|/ { print }' "$note" >"$table"
  local pipe_lines rows
  pipe_lines="$(awk 'NF { n++ } END { print n + 0 }' "$table")"
  rows=$((pipe_lines - 2))
  if [[ "$rows" -lt 0 ]]; then
    rows=0
  fi
  GATE7_TABLE_ROWS="$rows"

  {
    printf '# distinct collect members referenced from modules/basics/src: %s\n' "$member_count"
    printf '# section (a) table: %s pipe lines, %s data rows (header and separator excluded)\n' \
      "$pipe_lines" "$rows"
    printf '# first two table lines, which must be the header and the separator:\n'
    head -n 2 "$table"
    printf '\n'
  } >>"$EV"

  if [[ "$member_count" -lt 1 ]]; then
    detail "the collect-member grep over modules/basics/src found nothing, so the comparison is vacuous"
    failed=1
  fi
  if [[ "$pipe_lines" -lt 3 ]]; then
    detail "section (a) of $note carries no member table"
    failed=1
  elif ! sed -n '2p' "$table" | grep -qE '^\|[-| :]+\|?[[:space:]]*$'; then
    detail "the second line of the section (a) table is not a markdown separator"
    failed=1
  fi
  if [[ "$rows" -lt "$member_count" ]]; then
    detail "section (a) has $rows table rows, fewer than the $member_count referenced collect members"
    failed=1
  fi

  {
    printf 'headings (## )                     : %s (six sections (a)-(f) required)\n' "$heading_count"
    printf 'distinct collect members referenced: %s\n' "$member_count"
    printf 'section (a) member table rows      : %s\n' "$rows"
  } | add_appendix "Gate 7 - migration note content checks"

  if [[ "$failed" -eq 0 ]]; then
    detail "$note present, $heading_count sections, $rows table rows >= $member_count referenced members"
  fi
  return "$failed"
}

#=============================================================================
# Row 19 - Test scope at least equal to the Java suites.
#
# Everything here is parsed with python3 and its standard library: the JUnit
# XML snapshot taken after Gate 1 (the only run that exercised every suite),
# the method-level manifest, and the Java test sources themselves.
#
# Enforced:
#   * every @Test/@ParameterizedTest method of every *Test.java under
#     modules/basics/src/test/java, and of every collect test class the
#     manifest maps, has a manifest row - no unmapped Java method;
#   * every `ported` or `consolidated:<spec>` row names a <testcase> that
#     exists in the JUnit XML, matched on suite class and test name;
#   * every `partial:<reason>` row belongs to GuavateTest or MapStreamTest;
#   * every `dropped:<reason>` row belongs to one of the five class-level
#     exclusions of AAP section 0.2.2, or is the one method-level exclusion
#     that section also names - the `ImmutableHolidayCalendar-Old.json`
#     fixture, dropped with Joda wire compatibility, which SCALA_MIGRATION.md
#     records as a divergence and instructs this script to allow;
#   * the per-module test counts: strata-basics >= 1223 and
#     strata-collect >= 491 (643 methods of the mapped collect classes minus
#     the 90 of GuavateTest and the 62 of MapStreamTest).
#
# Vacuity guards: the Java enumeration must find at least the 1223 basics and
# 643 mapped-collect methods the specification counted. A wildly lower number
# means the annotation scan is broken, not that the manifest is complete.
#=============================================================================

row_19_test_scope() {
  new_evidence test-scope.txt
  local failed=0
  local summary="$AUDIT_DIR/test-scope-summary.txt"

  printf '## Test scope at least equal to the Java suites\n\n' >>"$EV"

  if ! python3 - \
    "$SNAPSHOT_DIR/test-reports" \
    strata-basics/src/test/resources/manifest/java-test-mapping.csv \
    modules/basics/src/test/java \
    modules/collect/src/test/java \
    "$summary" <<'PY' >>"$EV" 2>&1; then
import csv
import glob
import os
import re
import sys
import xml.etree.ElementTree as ET

xml_dir, manifest, basics_java, collect_java, summary_path = sys.argv[1:6]

BASICS_PREFIX = "com.opengamma.strata.basics."
COLLECT_PREFIX = "com.opengamma.strata.collect."
BASICS_FLOOR = 1223
COLLECT_FLOOR = 491
BASICS_METHOD_FLOOR = 1223
COLLECT_METHOD_FLOOR = 643
HEADER = ["java_test_class", "java_test_method", "scala_spec", "scala_test_name", "status"]
PARTIAL_CLASSES = {
    "com.opengamma.strata.collect.GuavateTest",
    "com.opengamma.strata.collect.MapStreamTest",
}
# AAP section 0.2.2: the five class-level exclusions, plus the one method-level
# exclusion the same section names (the legacy Joda JSON fixture).
DROPPED_CLASSES = {
    "com.opengamma.strata.basics.date.HolidayCalendarIniLookupTest",
    "com.opengamma.strata.collect.result.FailureExceptionTest",
    "com.opengamma.strata.collect.result.FailureItemExceptionTest",
    "com.opengamma.strata.collect.result.IllegalArgFailureExceptionTest",
    "com.opengamma.strata.collect.result.ParseFailureExceptionTest",
    "com.opengamma.strata.basics.date.ImmutableHolidayCalendarTest",
}

problems = []
lines = []


def note(text):
    lines.append(text)


# -- the JUnit XML snapshot --------------------------------------------------
suite_tests = {}
test_cases = set()
xml_files = sorted(glob.glob(os.path.join(xml_dir, "TEST-*.xml")))
for path in xml_files:
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError as error:
        problems.append(f"{path}: not parseable XML: {error}")
        continue
    suites = [root] if root.tag == "testsuite" else list(root.iter("testsuite"))
    for suite in suites:
        name = suite.attrib.get("name", "")
        try:
            count = int(suite.attrib.get("tests", "0"))
        except ValueError:
            problems.append(f"{path}: suite {name!r} has a non-numeric tests attribute")
            count = 0
        suite_tests[name] = suite_tests.get(name, 0) + count
        for case in suite.iter("testcase"):
            test_cases.add((case.attrib.get("classname", ""), case.attrib.get("name", "")))

basics_tests = sum(n for s, n in suite_tests.items() if s.startswith(BASICS_PREFIX))
collect_tests = sum(n for s, n in suite_tests.items() if s.startswith(COLLECT_PREFIX))
other = {s: n for s, n in suite_tests.items()
         if not s.startswith(BASICS_PREFIX) and not s.startswith(COLLECT_PREFIX)}

note(f"JUnit XML files read          : {len(xml_files)} (from {xml_dir})")
note(f"suites                        : {len(suite_tests)}")
note(f"test cases                    : {len(test_cases)}")
note(f"strata-basics tests            : {basics_tests} (floor {BASICS_FLOOR})")
note(f"strata-collect tests           : {collect_tests} (floor {COLLECT_FLOOR})")
if other:
    note(f"suites outside both packages  : {sorted(other)}")

if not xml_files:
    problems.append(f"no TEST-*.xml found in {xml_dir}: Gate 1 produced no report to read")
if basics_tests < BASICS_FLOOR:
    problems.append(f"strata-basics ran {basics_tests} tests, below the floor of {BASICS_FLOOR}")
if collect_tests < COLLECT_FLOOR:
    problems.append(f"strata-collect ran {collect_tests} tests, below the floor of {COLLECT_FLOOR}")

# -- the Java test sources ---------------------------------------------------
ANNOTATION = re.compile(r"@(?:Test|ParameterizedTest)\b")
SIGNATURE = re.compile(
    r"(?:public|private|protected|\s)*\s*(?:<[^>]*>\s*)?[\w.<>\[\],\s?]+\s+(\w+)\s*\(")


def enumerate_java(tree):
    found = {}
    for directory, _, files in os.walk(tree):
        for name in sorted(files):
            if not name.endswith("Test.java"):
                continue
            path = os.path.join(directory, name)
            with open(path, encoding="utf-8") as handle:
                source = handle.read()
            package = re.search(r"^package\s+([\w.]+);", source, re.M)
            if package is None:
                problems.append(f"{path}: no package declaration")
                continue
            fqcn = f"{package.group(1)}.{name[:-5]}"
            methods = found.setdefault(fqcn, set())
            for match in ANNOTATION.finditer(source):
                signature = SIGNATURE.search(source[match.end():match.end() + 2000])
                if signature is not None:
                    methods.add(signature.group(1))
    return found


java_basics = enumerate_java(basics_java)
java_collect = enumerate_java(collect_java)
java_all = dict(java_basics)
java_all.update(java_collect)

basics_methods = sum(len(v) for v in java_basics.values())
note(f"Java basics test classes      : {len(java_basics)}")
note(f"Java basics test methods      : {basics_methods} (expected at least {BASICS_METHOD_FLOOR})")
if basics_methods < BASICS_METHOD_FLOOR:
    problems.append(
        f"the Java annotation scan found only {basics_methods} basics test methods, below the "
        f"{BASICS_METHOD_FLOOR} the specification counted: the scan is broken, not the manifest")

# -- the manifest ------------------------------------------------------------
with open(manifest, newline="", encoding="utf-8") as handle:
    reader = csv.reader(handle)
    try:
        header = next(reader)
    except StopIteration:
        header = []
    rows = [row for row in reader if row]

note(f"manifest rows                 : {len(rows)} ({manifest})")
if header != HEADER:
    problems.append(f"manifest header is {header}, expected {HEADER}")

mapped = {}
statuses = {}
for index, row in enumerate(rows, start=2):
    if len(row) != len(HEADER):
        problems.append(f"{manifest}:{index}: {len(row)} fields, expected {len(HEADER)}")
        continue
    java_class, java_method, spec, test_name, status = row
    base_method = re.sub(r"\(.*\)$", "", java_method)
    mapped.setdefault(java_class, set()).add(base_method)
    kind = status.split(":", 1)[0]
    statuses[kind] = statuses.get(kind, 0) + 1

    if java_class not in java_all:
        problems.append(f"{manifest}:{index}: unknown Java test class {java_class}")
    elif base_method not in java_all[java_class]:
        problems.append(
            f"{manifest}:{index}: {java_class} has no @Test method {base_method}")

    if kind in ("ported", "consolidated"):
        if kind == "consolidated" and ":" not in status:
            problems.append(f"{manifest}:{index}: consolidated row names no target spec")
        if not spec or not test_name:
            problems.append(f"{manifest}:{index}: {kind} row names no Scala test")
        elif (spec, test_name) not in test_cases:
            problems.append(
                f"{manifest}:{index}: no test case {spec} / {test_name!r} in the JUnit XML "
                f"(Java {java_class}.{java_method})")
    elif kind == "partial":
        if java_class not in PARTIAL_CLASSES:
            problems.append(
                f"{manifest}:{index}: partial row belongs to {java_class}, which is not "
                f"GuavateTest or MapStreamTest")
        if ":" not in status:
            problems.append(f"{manifest}:{index}: partial row gives no reason")
    elif kind == "dropped":
        if java_class not in DROPPED_CLASSES:
            problems.append(
                f"{manifest}:{index}: dropped row belongs to {java_class}, which is not an "
                f"AAP section 0.2.2 exclusion")
        if ":" not in status or not status.split(":", 1)[1]:
            problems.append(f"{manifest}:{index}: dropped row gives no reason")
    else:
        problems.append(f"{manifest}:{index}: unknown status {status!r}")

note("manifest statuses             : " +
     ", ".join(f"{k}={v}" for k, v in sorted(statuses.items())))

# -- no unmapped Java method -------------------------------------------------
unmapped = []
for fqcn, methods in sorted(java_basics.items()):
    for method in sorted(methods):
        if method not in mapped.get(fqcn, set()):
            unmapped.append(f"{fqcn}.{method}")

mapped_collect = sorted(c for c in mapped if c.startswith(COLLECT_PREFIX))
collect_methods = sum(len(java_collect.get(c, set())) for c in mapped_collect)
note(f"collect test classes mapped   : {len(mapped_collect)}")
note(f"their Java test methods       : {collect_methods} (expected at least {COLLECT_METHOD_FLOOR})")
if collect_methods < COLLECT_METHOD_FLOOR:
    problems.append(
        f"the Java annotation scan found only {collect_methods} methods across the mapped "
        f"collect classes, below the {COLLECT_METHOD_FLOOR} the specification counted")
for fqcn in mapped_collect:
    if fqcn not in java_collect:
        problems.append(f"{manifest}: mapped collect class {fqcn} has no Java source")
        continue
    for method in sorted(java_collect[fqcn]):
        if method not in mapped[fqcn]:
            unmapped.append(f"{fqcn}.{method}")

note(f"unmapped Java test methods    : {len(unmapped)} (0 required)")
if unmapped:
    problems.append(f"{len(unmapped)} Java test method(s) have no manifest row")
    problems.extend(f"    {entry}" for entry in unmapped[:20])

with open(summary_path, "w", encoding="utf-8") as handle:
    handle.write("\n".join(lines) + "\n")
    if problems:
        handle.write("\nproblems:\n" + "\n".join(problems) + "\n")

print("\n".join(lines))
if problems:
    print("\nproblems:")
    print("\n".join(problems[:60]))
    if len(problems) > 60:
        print(f"... and {len(problems) - 60} more (see {summary_path})")
    sys.exit(1)
PY
    detail "method-level traceability or the per-module test counts are not satisfied"
    failed=1
  fi

  if [[ -f "$summary" ]]; then
    add_appendix "Test scope - per-module counts and manifest traceability" <"$summary"
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "every Java test method mapped, every ported row joined to a test case, both counts above their floors"
  fi
  return "$failed"
}

#=============================================================================
# Row 20 - Repository boundary, run last.
#
#   git status --porcelain -- modules examples eclipse pom.xml src .github
#
# must be empty: the Maven tree and the repository governance files are
# unchanged. Running it last also catches any accidental write by this script,
# which is why every artifact it produces lives under target/.
#=============================================================================

row_20_repository_boundary() {
  new_evidence repository-boundary.txt
  local status
  local rc=0

  status="$(git status --porcelain -- modules examples eclipse pom.xml src .github 2>&1)" || rc=$?
  {
    printf '## Repository boundary - the Maven tree and governance files are untouched\n'
    printf '# command: git status --porcelain -- modules examples eclipse pom.xml src .github\n'
    printf '# git exit status: %s\n' "$rc"
    printf '# output (empty is the pass):\n%s\n' "$status"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "git status failed with status $rc"
    return 1
  fi
  if [[ -n "$status" ]]; then
    detail "$(printf '%s\n' "$status" | wc -l | tr -d ' ') path(s) changed outside the sbt build"
    return 1
  fi
  detail "modules, examples, eclipse, pom.xml, src and .github are unchanged"
  return 0
}

#-----------------------------------------------------------------------------
# The report.
#
# Written on every exit path through the EXIT trap, because CI publishes it
# with `when: always` and it is the deliverable evidence. Deterministic except
# for the one timestamp in its header, which no gate compares.
#-----------------------------------------------------------------------------

write_report() {
  if [[ "$REPORT_WRITTEN" == "yes" ]]; then
    return 0
  fi
  REPORT_WRITTEN="yes"

  local passed=$((GATE_AUTOMATED - GATE_FAILED))
  local index

  {
    printf '# Acceptance gate report\n\n'
    printf -- '- script: `%s`\n' "$SCRIPT_NAME"
    printf -- '- repository root: `%s`\n' "$ROOT"
    printf -- '- JDK: `%s`\n' "${JDK_VERSION:-unknown}"
    printf -- '- sbt: `%s`\n' "${SBT_VERSION:-unknown}"
    printf -- '- run finished (UTC): `%s`\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
    printf -- '- provenance: every row below is a row of the validation table of the\n'
    printf '  technical specification (AAP section 0.10.1), executed in that order.\n\n'

    printf '## Gates\n\n'
    printf '| # | Gate / Rule | Verdict | Detail | Evidence |\n'
    printf '|---|-------------|---------|--------|----------|\n'
    if [[ "${#GATE_LABEL[@]}" -eq 0 ]]; then
      printf '| - | (no row completed) | - | the run ended before any row finished | `none` |\n'
    fi
    for index in "${!GATE_LABEL[@]}"; do
      printf '| %s | %s | %s | %s | `%s` |\n' \
        "$((index + 1))" \
        "$(printf '%s' "${GATE_LABEL[$index]}" | sed -e 's/|/\\|/g')" \
        "${GATE_STATUS[$index]}" \
        "$(printf '%s' "${GATE_DETAIL[$index]}" | sed -e 's/|/\\|/g')" \
        "${GATE_EVIDENCE[$index]}"
    done

    printf '\nGATES: %s total, %s passed, %s failed (automated rows; plus %s reported row)\n' \
      "$GATE_AUTOMATED" "$passed" "$GATE_FAILED" "$GATE_REPORTED"
    if [[ "$RUN_COMPLETED" != "yes" ]]; then
      printf '\nRESULT: INCOMPLETE - the run ended after %s of the %s automated rows, so this\n' \
        "$GATE_AUTOMATED" "$GATE_EXPECTED_AUTOMATED"
      printf 'is not an acceptance result. %s of the rows that did run failed.\n' "$GATE_FAILED"
    elif [[ "$GATE_FAILED" -eq 0 ]]; then
      printf '\nRESULT: every automated gate passed.\n'
    else
      printf '\nRESULT: %s automated gate(s) failed. This delivery is not acceptable yet.\n' \
        "$GATE_FAILED"
    fi

    printf '\n## Appendices\n'
    if [[ -s "$APPENDIX_FILE" ]]; then
      cat "$APPENDIX_FILE"
    else
      printf '\n(no appendix was produced: the run stopped before any row could report.)\n'
    fi

    printf '\n## Artifacts\n\n'
    printf -- '- `target/gate-report.md` - this report\n'
    printf -- '- `target/parity-report/` - the six parity reports the specs write\n'
    printf -- '- `target/test-reports/` - JUnit XML and the ScalaTest run logs\n'
    printf -- '- `target/audit/` - per-row evidence, sbt logs, class-load logs and the\n'
    printf '  snapshots the late rows read\n'
  } >"$REPORT_FILE"

  printf '\nreport written: %s\n' "${REPORT_FILE#"$ROOT"/}"
}

on_exit() {
  local status=$?
  # The handler must not be able to fail the run it is reporting on, so both
  # `errexit` and `nounset` are relaxed for the duration of the report.
  set +e
  set +u
  write_report
  return "$status"
}
trap on_exit EXIT

# An interrupted run still has to leave the report behind, and bash only
# guarantees the EXIT trap for a signal it handles, so the two signals a CI
# timeout or an operator sends are handled explicitly. Each exits with the
# conventional 128 + signal status, which runs the EXIT trap above.
trap 'printf "\ninterrupted (SIGINT): writing the partial report\n" >&2; exit 130' INT
trap 'printf "\nterminated (SIGTERM): writing the partial report\n" >&2; exit 143' TERM

#-----------------------------------------------------------------------------
# Main: every row of AAP section 0.10.1, in that table's order.
#-----------------------------------------------------------------------------

main() {
  printf '%s\n' '============================================================'
  printf 'acceptance gates for the Scala port of strata-collect/strata-basics\n'
  printf 'repository root: %s\n' "$ROOT"
  printf '%s\n' '============================================================'

  preflight

  run_gate "Gate 1 - builds and runs" row_01_build_and_test
  run_gate "Gate 2 / Rule 1 - dependency purity" row_02_dependency_purity
  run_gate "Gate 2a / Rule 1a - exactly two Scala-only modules" row_03_two_scala_modules
  run_gate "Gate 3 / Rule 2 - numerical parity" row_04_numerical_parity
  run_gate "Gate 4 - serialization round-trip" row_05_serialization_round_trip
  run_gate "Gate 5 / Rule 3 - no var in domain code" row_06_no_var
  run_gate "Gate 5 / Rule 3 - no boxing in the numeric hot paths" row_07_no_boxing
  run_gate "Gate 5 / Rule 5 - explicit error handling" row_08_explicit_error_handling
  run_gate "Gate 5 / Rule 7 - IO at the edges" row_09_io_at_the_edges
  run_gate "Gate 5 - typeclass instances" row_10_typeclass_instances
  run_gate "Gate 5 / Rule 4 - closed enums and data fidelity" row_11_closed_enums
  run_gate "Rule 6 - no reflection on the codec path" row_12_no_reflection
  run_gate "Rule 8 - JVM 21 bytecode" row_13_jvm21_bytecode
  run_gate "Rule 9 - warning-clean" row_14_warning_clean
  run_gate "Rule 10 - Scala collections in the public API" row_15_scala_collections_api
  run_gate "Gate 6 - end-to-end demo" row_16_demo
  run_gate "Gate 7 - migration note (automated)" row_17_migration_note

  # The manual half of Gate 7 is reported, never measured: the approval is an
  # out-of-band pull-request review by a CODEOWNERS owner, carried by
  # mergify's `#approved-reviews-by>=1` condition. The wording below is the
  # one the specification prescribes for this row; it is stated only when the
  # automated half above actually passed, because a report that claimed
  # otherwise would be false.
  # The verdict of the row that has just run is the last element of each
  # array, read positionally rather than by a fixed index so that inserting a
  # row cannot silently point this at another row's verdict.
  local last=$((${#GATE_STATUS[@]} - 1))
  local manual_text="$GATE7_MANUAL_TEXT"
  if [[ "${GATE_STATUS[$last]:-}" != "PASS" ]]; then
    manual_text="automated checks FAILED (see the Gate 7 row above); manual approval: see PR review"
  fi
  record_reported_row "Gate 7 - migration note (manual approval)" "$manual_text" \
    "${GATE_EVIDENCE[$last]:-none}"

  run_gate "Test scope >= Java" row_19_test_scope
  run_gate "Repository boundary" row_20_repository_boundary

  # Every row of the table has now run, so the report may state a result.
  # A report written before this point - by the EXIT trap after a failure or
  # an interruption - says how far the run got instead.
  if [[ "$GATE_AUTOMATED" -ne "$GATE_EXPECTED_AUTOMATED" ]]; then
    printf 'FATAL: %s automated rows ran, expected %s. The row list and\n' \
      "$GATE_AUTOMATED" "$GATE_EXPECTED_AUTOMATED" >&2
    printf 'GATE_EXPECTED_AUTOMATED disagree - this script is misconfigured.\n' >&2
    GATE_FAILED=$((GATE_FAILED + 1))
  else
    RUN_COMPLETED="yes"
  fi
  write_report

  printf '\n%s\n' '------------------------------------------------------------'
  printf 'GATES: %s total, %s passed, %s failed (automated rows; plus %s reported row)\n' \
    "$GATE_AUTOMATED" "$((GATE_AUTOMATED - GATE_FAILED))" "$GATE_FAILED" "$GATE_REPORTED"
  for index in "${!GATE_LABEL[@]}"; do
    printf '  %-8s %s\n' "${GATE_STATUS[$index]}" "${GATE_LABEL[$index]}"
  done
  printf '%s\n' '------------------------------------------------------------'

  if [[ "$GATE_FAILED" -eq 0 ]]; then
    printf 'RESULT: every automated gate passed.\n'
    exit 0
  fi
  printf 'RESULT: %s automated gate(s) failed; see %s\n' \
    "$GATE_FAILED" "${REPORT_FILE#"$ROOT"/}"
  exit 1
}

# Run the gates when executed, and define them without running when sourced.
# The guard exists so that the helpers and the row functions can be exercised
# individually - including their failure paths, which a whole-run test cannot
# reach - and it is the only condition under which `main` does not run.
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  main "$@"
fi

