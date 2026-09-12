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
#   Those are the only two accepted invocations: no argument other than the
#   help flag is understood, and no second argument is, so every other arity
#   is a usage error rather than a silently ignored word. There is deliberately
#   no flag that selects a subset of gates: a partial run is not an acceptance
#   run.
#
# SOURCING
#   `source scripts/verify-gates.sh` DEFINES the helpers and the row functions
#   and runs nothing: no argument is read, no directory is changed, no artifact
#   is truncated, no variable is exported and no trap is installed. Only the
#   read-only location lookup (`git rev-parse --show-toplevel`) happens, so the
#   path variables are populated. A caller that wants to exercise one row calls
#   `init_run` first - that is the function holding every side effect.
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
#   target/test-reports/TEST-*.xml the per-suite JUnit XML written by the one
#                                  ScalaTest `-u` reporter `build.sbt`
#                                  configures - the only test report this
#                                  build produces at the repository root
#   target/audit/                  per-row evidence, sbt logs, class-load logs
#                                  and the snapshots the late rows read
#
#   The report is written on every exit path, including an interruption. The
#   one exception is a rejected output path: if target/ cannot be created
#   safely there is nowhere to write the report, so the run stops with exit 2
#   and the reason on stderr.
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
#     it runs and returns an explicit 0 or 1. Because that suppression is easy
#     to forget, the text utilities this script transforms and writes evidence
#     with (awk, sed, sort, comm, tr, cut, uniq, wc, cp, rm, mkdir, mv, tee,
#     cmp) are wrapped by `checked_command`: a hard failure of any of them is
#     recorded in target/audit/framework-errors.txt and FAILS the row that was
#     running, so no transformation or write can fail unnoticed and leave empty
#     output to be read as agreement. grep and diff keep their meaningful
#     status 1 (no match / files differ); only >= 2 is a hard failure.
#   * grep exits 0 when it matched, 1 when it did not and >=2 on error. Every
#     "must find nothing" row goes through `assert_no_match`, which passes only
#     on 1 and reports 0 and >=2 differently.
#   * A row whose input artifact is missing FAILS. There is no path on which a
#     missing file, an absent marker or a failed command is read as a pass. A
#     row that records no evidence, or empty evidence, fails for the same
#     reason.
#   * Every side effect - argument handling, `cd`, the exports, creating and
#     truncating artifacts, installing the traps - lives in `init_run`, which
#     only `main` calls. Loading this file does none of it.
#   * Nothing is written outside $ROOT/target, and every output path is
#     verified to contain no symbolic-link component and to resolve inside the
#     canonical repository root before anything is created in it (CWE-59 /
#     CWE-22): target/ is git-ignored, so its contents are not trustworthy.
#   * target/gate-report.md is published by CI with `when: always` and is the
#     deliverable evidence, so it is written to a temporary file, verified and
#     then renamed atomically. Only a rename that succeeded marks the report
#     written, which is what lets the EXIT trap replace a partial one.
#

#-----------------------------------------------------------------------------
# Locations.
#
# Derived by `resolve_locations`, which is called once when this file loads.
# It is a read-only lookup: no directory is changed, nothing is created and
# nothing is exported, so sourcing this file to exercise one function stays
# free of side effects. Outside a git working tree ROOT is left empty and
# `require_repository_root` - part of `init_run` - reports it.
#-----------------------------------------------------------------------------

SCRIPT_NAME="scripts/verify-gates.sh"
ROOT=""
ROOT_REAL=""

resolve_locations() {
  ROOT="$(git rev-parse --show-toplevel 2>/dev/null || printf '')"

  TARGET_DIR="$ROOT/target"
  PARITY_DIR="$TARGET_DIR/parity-report"
  TEST_REPORT_DIR="$TARGET_DIR/test-reports"
  AUDIT_DIR="$TARGET_DIR/audit"
  LOG_DIR="$AUDIT_DIR/logs"
  SNAPSHOT_DIR="$AUDIT_DIR/snapshot"
  REPORT_FILE="$TARGET_DIR/gate-report.md"
  APPENDIX_FILE="$AUDIT_DIR/appendices.md"
  FRAMEWORK_ERROR_FILE="$AUDIT_DIR/framework-errors.txt"
}

resolve_locations

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
# is located at its canonical path under the repository root first and through
# BASH_SOURCE second, so that it prints from a subdirectory, from outside the
# repository, and before the working directory has been changed - the help
# flag is answered by `parse_arguments`, which runs before anything else. The
# last resort is a fixed summary, so `--help` says something useful even if
# this file cannot be located at all.
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

# Prints the usage error common to every rejected invocation.
usage_error() {
  printf 'FATAL: %s\n\n' "$1" >&2
  printf 'Usage: %s            run every gate and write the report\n' "$SCRIPT_NAME" >&2
  printf '       %s -h|--help  print the usage and exit\n\n' "$SCRIPT_NAME" >&2
  printf 'This script takes no options: it runs every gate, always. It accepts\n' >&2
  printf 'either no argument at all or exactly one help flag - nothing else,\n' >&2
  printf 'and never a second argument, because an argument it ignored would be\n' >&2
  printf 'an instruction it did not carry out.\n' >&2
}

# The complete argument contract: no argument, or exactly one help flag. Every
# other arity is rejected, so a misspelled or surplus word can never be
# silently discarded while the run reports success.
parse_arguments() {
  case "$#" in
    0)
      return 0
      ;;
    1)
      case "$1" in
        -h | --help)
          usage
          exit 0
          ;;
        *)
          usage_error "unknown argument \"$1\"."
          exit 2
          ;;
      esac
      ;;
    *)
      usage_error "$# arguments given; this script accepts at most one, the help flag. Unexpected: $*"
      exit 2
      ;;
  esac
}

#-----------------------------------------------------------------------------
# Output-path safety.
#
# target/ is git-ignored, so whatever is already inside it arrived from outside
# this script and is not trustworthy. `mkdir -p` creates a directory THROUGH an
# existing symbolic link without complaint, and every truncation, copy and log
# written afterwards would then land wherever that link points - outside the
# repository, on a path this script neither owns nor reports (CWE-59 symlink
# following, CWE-22 path traversal). So every output directory is created one
# component at a time, a symbolic link anywhere below the root is refused, and
# the result is canonicalised and asserted to be inside the canonical root.
# Every file this framework truncates is checked the same way, because a
# symbolic link left in place of an evidence file would redirect that write on
# its own.
#-----------------------------------------------------------------------------

path_fatal() {
  printf 'FATAL: unsafe output path: %s\n' "$1" >&2
  printf 'This script writes only inside %s/target and never through a\n' \
    "${ROOT:-the repository root}" >&2
  printf 'symbolic link. Remove or replace the offending path and re-run: no\n' >&2
  printf 'gate is skipped because an output path was rejected.\n' >&2
}

# canonical_dir <existing directory> - prints its physical path, empty on error.
canonical_dir() {
  (cd -- "$1" >/dev/null 2>&1 && pwd -P)
}

# is_inside_root <canonical path> - the canonical root itself, or below it.
is_inside_root() {
  [[ -n "$ROOT_REAL" && ("$1" == "$ROOT_REAL" || "$1" == "$ROOT_REAL"/*) ]]
}

# ensure_output_dir <absolute directory below $ROOT>
#
# Creates it if needed, refusing a symlinked or non-directory component on the
# way, and confirms the canonical result is inside the canonical repository
# root. Returns 1 - it never exits - so a caller inside a gate can record the
# failure as that row's verdict.
ensure_output_dir() {
  local dir="$1"

  if [[ -z "$ROOT" || -z "$ROOT_REAL" ]]; then
    path_fatal "$dir (the repository root has not been resolved yet)"
    return 1
  fi
  case "$dir" in
    "$ROOT"/*) ;;
    *)
      path_fatal "$dir is not below $ROOT"
      return 1
      ;;
  esac

  local remainder="${dir#"$ROOT"/}"
  local path="$ROOT"
  local component
  while [[ -n "$remainder" ]]; do
    component="${remainder%%/*}"
    if [[ "$component" == "$remainder" ]]; then
      remainder=""
    else
      remainder="${remainder#*/}"
    fi
    if [[ -z "$component" || "$component" == "." || "$component" == ".." ]]; then
      path_fatal "$dir contains the path element \"$component\""
      return 1
    fi
    path="$path/$component"
    if [[ -L "$path" ]]; then
      path_fatal "$path is a symbolic link"
      return 1
    fi
    if [[ -e "$path" && ! -d "$path" ]]; then
      path_fatal "$path exists and is not a directory"
      return 1
    fi
    if [[ ! -e "$path" ]] && ! mkdir "$path"; then
      path_fatal "$path could not be created"
      return 1
    fi
  done

  local real
  real="$(canonical_dir "$dir")"
  if ! is_inside_root "$real"; then
    path_fatal "$dir resolves to \"${real:-nothing}\", which is not inside $ROOT_REAL"
    return 1
  fi
  return 0
}

# ensure_output_file <absolute file below $ROOT> - checks its directory, then
# the file itself: an existing symbolic link or non-regular file is refused
# rather than written through.
ensure_output_file() {
  local file="$1"
  local dir="${file%/*}"

  if [[ "$dir" == "$file" || -z "$dir" ]]; then
    path_fatal "$file has no directory part"
    return 1
  fi
  ensure_output_dir "$dir" || return 1
  if [[ -L "$file" ]]; then
    path_fatal "$file is a symbolic link"
    return 1
  fi
  if [[ -e "$file" && ! -f "$file" ]]; then
    path_fatal "$file exists and is not a regular file"
    return 1
  fi
  return 0
}

# copy_through_shell <from> <to> - a copy that needs no external tool, used
# only where the report must exist and `mv` has just proved unavailable.
copy_through_shell() {
  local from="$1"
  local to="$2"
  local line

  ensure_output_file "$to" || return 1
  : >"$to" || return 1
  while IFS= read -r line || [[ -n "$line" ]]; do
    printf '%s\n' "$line" >>"$to" || return 1
  done <"$from"
  return 0
}

# safe_truncate <absolute file below $ROOT> - the only way this framework
# empties or creates a file, so that no write can be redirected by a link.
safe_truncate() {
  ensure_output_file "$1" || return 1
  if ! : >"$1"; then
    path_fatal "$1 could not be truncated"
    return 1
  fi
  return 0
}

# assert_no_symlinks_below <directory> - a symbolic link already inside a
# directory this run writes into is refused: nothing here placed it, and the
# writes that follow would follow it.
assert_no_symlinks_below() {
  local dir="$1"
  local links
  local rc=0

  links="$(command find "$dir" -type l -print 2>&1)" || rc=$?
  if [[ "$rc" -ne 0 ]]; then
    path_fatal "$dir could not be scanned for symbolic links (find exited $rc: $links)"
    return 1
  fi
  if [[ -n "$links" ]]; then
    path_fatal "$dir contains symbolic link(s): $(printf '%s' "$links" | tr '\n' ' ')"
    return 1
  fi
  return 0
}

#-----------------------------------------------------------------------------
# The output tree, created before anything can write into it - in particular
# before any JVM writes a class-load log, which the specification calls out.
#-----------------------------------------------------------------------------

init_output_tree() {
  local dir
  for dir in "$TARGET_DIR" "$PARITY_DIR" "$TEST_REPORT_DIR" "$AUDIT_DIR" "$LOG_DIR" \
    "$SNAPSHOT_DIR" "$SNAPSHOT_DIR/test-reports" "$SNAPSHOT_DIR/parity-report"; do
    ensure_output_dir "$dir" || return 1
  done
  # sbt owns other subdirectories of target/, so only the three this script
  # writes evidence into are swept.
  for dir in "$AUDIT_DIR" "$PARITY_DIR" "$TEST_REPORT_DIR"; do
    assert_no_symlinks_below "$dir" || return 1
  done
  ensure_output_file "$REPORT_FILE" || return 1
  safe_truncate "$APPENDIX_FILE" || return 1
  # Last, and only now: the record of unchecked failures is opened once its
  # own path has been validated and emptied. Everything above this line
  # reports on stderr alone, so no failure during validation can be written
  # through a path the validation has not yet cleared.
  safe_truncate "$FRAMEWORK_ERROR_FILE" || return 1
  FRAMEWORK_ERROR_READY="yes"
  return 0
}

# Resolves the repository root for a real run, which is the one place that
# `cd`s, and canonicalises it for the containment checks above.
require_repository_root() {
  if [[ -z "$ROOT" ]]; then
    printf 'FATAL: %s must be run inside the git working tree of the repository.\n' \
      "$SCRIPT_NAME" >&2
    return 1
  fi
  if [[ ! -d "$ROOT" ]]; then
    printf 'FATAL: the repository root %s is not a directory.\n' "$ROOT" >&2
    return 1
  fi
  if ! cd "$ROOT"; then
    printf 'FATAL: cannot enter the repository root %s.\n' "$ROOT" >&2
    return 1
  fi
  ROOT_REAL="$(canonical_dir "$ROOT")"
  if [[ -z "$ROOT_REAL" ]]; then
    printf 'FATAL: the repository root %s cannot be canonicalised.\n' "$ROOT" >&2
    return 1
  fi
  return 0
}

# Every side effect of this script, in one function that only `main` calls:
# the shell options, the exports, the argument contract, the working
# directory, the output tree and the traps. Loading this file performs none of
# it, so `source`ing it to exercise a single row function is safe.
init_run() {
  set -euo pipefail
  # Deterministic, locale-independent sorting, grepping and character classes.
  export LC_ALL=C
  # A conservative default for hosts with little memory; an exported SBT_OPTS
  # wins, so CI and developers can size the build themselves.
  export SBT_OPTS="${SBT_OPTS:--Xmx1200m -Xss8m -XX:MaxMetaspaceSize=512m}"

  parse_arguments "$@"
  resolve_locations
  require_repository_root || exit 2
  # The output tree is validated and created BEFORE the checked-command
  # wrappers exist, so nothing can be recorded through a path that has not
  # been proved safe yet; `init_output_tree` opens the record itself, as its
  # last step.
  init_output_tree || exit 2
  install_checked_commands
  # `install_traps` is defined with the traps themselves, next to the report.
  install_traps
}

#-----------------------------------------------------------------------------
# Gate bookkeeping. Five ordered, parallel arrays: the row label, its verdict,
# a one-line detail, the relative path of its evidence, and its kind -
# `automated` for a measured row, `reported` for the one row that is stated
# rather than measured, `preflight` for the tool check that precedes them all.
# The arrays are the single source of truth for every count in the report,
# which is computed by tallying their entries: no count is ever derived by
# subtracting one running total from another, because a row recorded under one
# tally and not the other then yields an impossible figure.
#-----------------------------------------------------------------------------

GATE_LABEL=()
GATE_STATUS=()
GATE_DETAIL=()
GATE_EVIDENCE=()
GATE_KIND=()
GATE_FAILED=0
REPORT_WRITTEN="no"
# The number of automated rows AAP section 0.10.1 defines, so that a report
# written by the EXIT trap after an interruption can say how much of the run
# it covers instead of presenting a partial result as an acceptance result.
GATE_EXPECTED_AUTOMATED=19
RUN_COMPLETED="no"

# Set by a gate function through `detail`/`evidence`; read by `run_gate`.
GATE_DETAIL_OUT=""
GATE_EVIDENCE_OUT=""

# Tallies of the arrays above, recomputed by `gate_counts`. One implementation
# feeds both the report and the closing summary, so the two cannot disagree.
GATE_COUNT_AUTOMATED=0
GATE_COUNT_PASSED=0
GATE_COUNT_FAILED=0
GATE_COUNT_REPORTED=0
GATE_COUNT_PREFLIGHT_FAILED=0
# Non-empty when the parallel arrays have diverged, which would make every
# tally above meaningless; reported rather than quietly tolerated.
GATE_ARRAY_PROBLEM=""

# Recomputes every tally from the recorded verdicts. Counting entries cannot
# produce a negative or contradictory figure the way subtracting two
# independently maintained totals can. Returns 1 when the arrays have
# diverged.
gate_counts() {
  GATE_COUNT_AUTOMATED=0
  GATE_COUNT_PASSED=0
  GATE_COUNT_FAILED=0
  GATE_COUNT_REPORTED=0
  GATE_COUNT_PREFLIGHT_FAILED=0
  GATE_ARRAY_PROBLEM=""

  local rows="${#GATE_LABEL[@]}"
  if [[ "${#GATE_STATUS[@]}" -ne "$rows" || "${#GATE_DETAIL[@]}" -ne "$rows" ||
    "${#GATE_EVIDENCE[@]}" -ne "$rows" || "${#GATE_KIND[@]}" -ne "$rows" ]]; then
    GATE_ARRAY_PROBLEM="$(printf 'the gate arrays have diverged: %s labels, %s statuses, %s details, %s evidence paths, %s kinds' \
      "$rows" "${#GATE_STATUS[@]}" "${#GATE_DETAIL[@]}" "${#GATE_EVIDENCE[@]}" "${#GATE_KIND[@]}")"
  fi

  local index
  for index in "${!GATE_LABEL[@]}"; do
    case "${GATE_KIND[$index]:-automated}" in
      automated)
        GATE_COUNT_AUTOMATED=$((GATE_COUNT_AUTOMATED + 1))
        if [[ "${GATE_STATUS[$index]:-}" == "PASS" ]]; then
          GATE_COUNT_PASSED=$((GATE_COUNT_PASSED + 1))
        else
          GATE_COUNT_FAILED=$((GATE_COUNT_FAILED + 1))
        fi
        ;;
      reported)
        GATE_COUNT_REPORTED=$((GATE_COUNT_REPORTED + 1))
        ;;
      *)
        # The preflight row, and anything else recorded outside the table: it
        # is blocking, but it is not one of the nineteen measured rows and is
        # never counted as one.
        if [[ "${GATE_STATUS[$index]:-}" != "PASS" ]]; then
          GATE_COUNT_PREFLIGHT_FAILED=$((GATE_COUNT_PREFLIGHT_FAILED + 1))
        fi
        ;;
    esac
  done

  [[ -z "$GATE_ARRAY_PROBLEM" ]]
}

#-----------------------------------------------------------------------------
# Unchecked-failure detection.
#
# `run_gate` runs every gate function in a `||` context, which suppresses
# `set -e` inside it. A transformation or a write that fails there returns a
# status nobody looks at and leaves empty output behind - and empty output is
# exactly what several rows read as agreement (an empty `comm -3` means "the
# inventories are identical"). The wrappers below therefore make such a
# failure impossible to miss: each records the command and its status here,
# and `run_gate` fails the row that was running when it happened.
#
# The record is a FILE rather than a variable because a wrapper invoked inside
# a pipeline or a command substitution runs in a subshell, where an array
# append would be discarded when the subshell exits.
#-----------------------------------------------------------------------------

# Counted by `run_gate` so that every recorded failure is attributed to the
# row that caused it; whatever is left over fails the run itself.
FRAMEWORK_ERRORS_ATTRIBUTED=0

# The record is only opened once its own path has been proved safe. Until
# then a failure is reported on stderr alone: the directory holding the record
# is itself one of the paths being validated, and appending to it beforehand
# would be the very write - through a link planted in the git-ignored target/
# tree - that the validation exists to prevent.
FRAMEWORK_ERROR_READY="no"

framework_error() {
  if [[ "$FRAMEWORK_ERROR_READY" == "yes" && -n "${FRAMEWORK_ERROR_FILE:-}" ]]; then
    printf '%s\n' "$1" >>"$FRAMEWORK_ERROR_FILE" 2>/dev/null || true
  fi
  printf 'FRAMEWORK ERROR: %s\n' "$1" >&2
}

# The number of failures recorded so far; 0 when nothing has been recorded.
framework_error_count() {
  if [[ "$FRAMEWORK_ERROR_READY" == "yes" && -f "${FRAMEWORK_ERROR_FILE:-}" ]]; then
    command awk 'END { print NR + 0 }' "$FRAMEWORK_ERROR_FILE" 2>/dev/null || printf '0\n'
  else
    printf '0\n'
  fi
}

# The failures recorded after the first <n> lines, i.e. those belonging to the
# row that has just run.
framework_errors_since() {
  local skip="$1"
  if [[ "$FRAMEWORK_ERROR_READY" == "yes" && -f "${FRAMEWORK_ERROR_FILE:-}" ]]; then
    command awk -v skip="$skip" 'NR > skip { print }' "$FRAMEWORK_ERROR_FILE" 2>/dev/null || true
  fi
}

# checked_command <tolerated maximum status> <command> [arguments...]
#
# Runs the command and records any status above the tolerated maximum. grep
# and diff tolerate 1 - "nothing matched" and "the files differ" are answers
# this script asks for - while everything else tolerates nothing. Death by
# SIGPIPE (128 + 13) is tolerated everywhere: it means the consumer of a
# pipeline stopped reading first, which is how `... | grep -q` ends normally
# and is never a transformation producing a wrong answer.
checked_command() {
  local tolerated="$1"
  shift
  local rc=0

  command "$@" || rc=$?
  if [[ "$rc" -gt "$tolerated" && "$rc" -ne 141 ]]; then
    framework_error "$(printf '%s exited %s: %s' "$1" "$rc" "$*")"
  fi
  return "$rc"
}

# Wraps the text utilities this script transforms data and writes evidence
# with. A hard failure of any of them is a defect in the run, never an answer,
# so each is routed through `checked_command`. Deliberately NOT wrapped:
# `python3` (whose exit status IS its verdict), `javap` (Rule 6 expects it to
# refuse JVM-generated classes), and `java`, `git` and `sbt`, whose statuses
# every caller already inspects. `find`, `cat`, `head` and `tail` are not
# wrapped either - a reader whose consumer closes early, and a scan of a path
# that may legitimately be absent, are normal here - so the rows that depend
# on such a command's output check it themselves.
#
# The definitions live inside this function, and `init_run` is what calls it,
# because a function named after a standard command changes how commands
# resolve. Defining them merely by LOADING this file would impose that on a
# caller that only wanted to source it, which is the one thing sourcing must
# not do.
install_checked_commands() {
  awk() { checked_command 0 awk "$@"; }
  sed() { checked_command 0 sed "$@"; }
  sort() { checked_command 0 sort "$@"; }
  comm() { checked_command 0 comm "$@"; }
  tr() { checked_command 0 tr "$@"; }
  cut() { checked_command 0 cut "$@"; }
  uniq() { checked_command 0 uniq "$@"; }
  wc() { checked_command 0 wc "$@"; }
  cp() { checked_command 0 cp "$@"; }
  rm() { checked_command 0 rm "$@"; }
  mkdir() { checked_command 0 mkdir "$@"; }
  mv() { checked_command 0 mv "$@"; }
  tee() { checked_command 0 tee "$@"; }
  cmp() { checked_command 1 cmp "$@"; }
  grep() { checked_command 1 grep "$@"; }
  diff() { checked_command 1 diff "$@"; }
}

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
  evidence "$EV"
  # Checked and symlink-safe: a row whose evidence file cannot be started has
  # nowhere to record what it measured, and the record makes that the row's
  # verdict even though every call site ignores this status.
  if ! safe_truncate "$EV"; then
    framework_error "the evidence file $1 could not be started"
    return 1
  fi
  return 0
}

# Appends one appendix section to the report's appendix file. Content arrives
# on stdin so that a caller can pipe a file, a command or a here-document. A
# failed append is recorded: an appendix silently missing from the report
# would be indistinguishable from a row that had nothing to report.
add_appendix() {
  local title="$1"
  if ! {
    printf '\n### %s\n\n' "$title"
    printf '```text\n'
    cat
    printf '```\n'
  } >>"$APPENDIX_FILE"; then
    framework_error "the appendix \"$title\" could not be appended to ${APPENDIX_FILE#"$ROOT"/}"
    return 1
  fi
  return 0
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

  # The number of unchecked command failures recorded before this row, so the
  # ones it causes can be told apart from every other row's.
  local errors_before
  errors_before="$(framework_error_count)"

  # Deliberate: `|| rc=$?` disables `set -e` inside the callee, which is why
  # every gate function checks its own commands and returns explicitly.
  "$fn" || rc=$?

  # A transformation or write that failed inside the row is that row's
  # failure, whether or not the row noticed: the evidence it produced is
  # incomplete, so its verdict cannot be trusted.
  local errors_after new_errors
  errors_after="$(framework_error_count)"
  new_errors=$((errors_after - errors_before))
  if [[ "$new_errors" -gt 0 ]]; then
    FRAMEWORK_ERRORS_ATTRIBUTED=$((FRAMEWORK_ERRORS_ATTRIBUTED + new_errors))
    # The first few are named in the row's detail - enough to act on without
    # making one table cell unreadable - and every one of them is in the file.
    local summary
    summary="$(framework_errors_since "$errors_before" | command head -n 3 |
      command tr '\n' '|' | command sed -e 's/|$//')"
    if [[ "$new_errors" -gt 3 ]]; then
      summary="$summary| (and $((new_errors - 3)) more in ${FRAMEWORK_ERROR_FILE#"$ROOT"/})"
    fi
    detail "$new_errors unchecked command failure(s) during this row: $summary"
    rc=1
  fi

  # A row that recorded no evidence, or empty evidence, has not measured
  # anything: its verdict would rest on output that was never produced. Every
  # automated row names its evidence file through `new_evidence`, so an
  # undeclared one is a defect in the row, not a row without findings.
  if [[ -z "$GATE_EVIDENCE_OUT" || "$GATE_EVIDENCE_OUT" == "none" ]]; then
    detail "this row declared no evidence file"
    rc=1
  else
    local evidence_path="$GATE_EVIDENCE_OUT"
    if [[ "$evidence_path" != /* ]]; then
      evidence_path="$ROOT/$evidence_path"
    fi
    if [[ ! -s "$evidence_path" ]]; then
      detail "this row's evidence file $GATE_EVIDENCE_OUT is missing or empty"
      rc=1
    fi
  fi

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
  GATE_KIND+=("automated")

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
  GATE_KIND+=("reported")
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
# measures.
#
# Combined output is tee'd: it reaches stdout as it is produced AND the log
# file keeps it for the report. Streaming matters for more than readability -
# a compile-and-test invocation runs for minutes, and a command that prints
# nothing for that long is indistinguishable from one that has hung, which is
# what CI timeouts and output watchdogs act on. sbt's own status is taken from
# PIPESTATUS[0], because the pipeline's status is `tee`'s.
run_sbt() {
  local name="$1"
  shift
  local log="$LOG_DIR/$name.log"
  local rc=0

  printf '+ sbt -batch -Dsbt.log.noformat=true' >&2
  printf ' %q' "$@" >&2
  printf '\n' >&2

  # The log is an output path like any other: refuse to write it through a
  # symbolic link, and start it empty so a failed run cannot inherit the
  # previous one's output.
  if ! safe_truncate "$log"; then
    framework_error "the sbt log $name.log could not be started; sbt was not run"
    SBT_LOG="$log"
    return 1
  fi

  # PIPESTATUS must be read by the first statement inside the branch: every
  # command that runs afterwards, an assignment included, replaces it.
  local -a pipe_status=()
  if sbt -batch -Dsbt.log.noformat=true "$@" 2>&1 | tee "$log"; then
    pipe_status=("${PIPESTATUS[@]}")
  else
    pipe_status=("${PIPESTATUS[@]}")
  fi
  rc="${pipe_status[0]:-1}"

  SBT_LOG="$log"
  printf '+ sbt exit %s (log: %s)\n' "$rc" "${log#"$ROOT"/}" >&2
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
  # Every external executable this script runs, in one list: the toolchain
  # (git, sbt, java, javap), the parsers (python3, awk, sed, grep), the text
  # utilities the rows transform evidence with (find, sort, comm, tr, cut, wc,
  # diff, cmp, uniq, cat, head, tail, basename, date) and the file operations
  # the snapshots, logs and the report depend on (cp, rm, mkdir, mv, tee). A
  # tool absent from this list is a tool whose absence would first be noticed
  # halfway through a row, as a failure of that row rather than of the
  # environment.
  local required=(
    git sbt java javap python3
    awk sed grep find sort comm tr cut wc diff cmp uniq
    cat head tail basename date
    cp rm mkdir mv tee
  )
  local missing=()
  local tool
  local path
  for tool in "${required[@]}"; do
    # `type -P` searches PATH for an executable, ignoring functions - which
    # matters here, because the checked-command wrappers above are functions
    # named after the very tools being verified, and `command -v` would report
    # those instead of the binaries they call.
    if [[ -z "$(type -P "$tool" 2>/dev/null)" ]]; then
      missing+=("$tool")
    fi
  done

  local evidence_file="$AUDIT_DIR/preflight.txt"
  {
    printf '## preflight\n'
    printf '# repository root: %s\n' "$ROOT"
    printf '# every external executable this script runs, resolved through PATH\n'
    for tool in "${required[@]}"; do
      path="$(type -P "$tool" 2>/dev/null || printf '')"
      printf '%-8s %s\n' "$tool" "${path:-MISSING}"
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
    # Recorded as a `preflight` row, not an automated one: no gate of the
    # specification's table ran, and counting it among them would make the
    # report claim a measurement that never happened.
    GATE_LABEL+=("Preflight - required tools")
    GATE_STATUS+=("FAIL")
    GATE_DETAIL+=("missing tools: ${missing[*]}")
    GATE_EVIDENCE+=("${evidence_file#"$ROOT"/}")
    GATE_KIND+=("preflight")
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
#
# A snapshot mirrors EVERY file of the directory it covers rather than the
# files of one name pattern. The published artifact set is whatever the build's
# reporters wrote, and keying the copy to a pattern is precisely how an
# artifact this script promises gets lost when that set changes. The two named
# counts stay pattern-specific because two rows measure them - Gate 1's
# vacuity check counts the JUnit XML (`TEST-*.xml`) and Gate 3's counts the
# parity reports (`*.json`) - and the total number of files is recorded beside
# each of them.
#
# `errexit` is suppressed inside every gate function, because `run_gate` calls
# them in an `||` context. Every rm, mkdir, cp and count below is therefore
# status-checked: a failure returns non-zero and describes itself in
# SNAPSHOT_ERROR, which the calling row puts into its own detail, so no row can
# pass on evidence a silent copy failure left stale.
#-----------------------------------------------------------------------------

# Why the most recent snapshot or restore failed; empty after a successful one.
SNAPSHOT_ERROR=""

# snapshot_failure <reason>
#
# Records the reason in SNAPSHOT_ERROR, for the row that called the helper to
# put into its own detail, AND in the runner's error ledger, and returns 1.
# The record is what makes such a failure a verdict even if a call site
# ignored the status: these helpers are called from more than one row.
snapshot_failure() {
  SNAPSHOT_ERROR="$1"
  framework_error "$1"
  return 1
}

# copy_directory_contents <source directory> <destination directory>
#
# Copies every file under the source into the destination, preserving the
# layout and the timestamps (`cp -a source/. destination/`) and leaving
# whatever the destination already held in place. A source that does not exist
# is not an error - it means the build wrote nothing there, which the counts
# then report as zero - and an existing but empty source copies nothing.
copy_directory_contents() {
  local source_dir="$1"
  local destination="$2"

  if [[ ! -d "$source_dir" ]]; then
    return 0
  fi
  if ! mkdir -p "$destination"; then
    snapshot_failure "could not create $destination"
    return 1
  fi
  if ! cp -a "$source_dir"/. "$destination"/; then
    snapshot_failure "could not copy $source_dir into $destination"
    return 1
  fi

  # The copy is PROVED rather than assumed: every regular file of the source
  # must exist at the destination and be byte-identical to it. `cp` runs here
  # with `errexit` suppressed, so a partial or truncated write would otherwise
  # be published as this run's evidence. The traversal itself is checked, so a
  # source that cannot be read is a failure and not an empty file set.
  local listing
  if ! listing="$(cd "$source_dir" && find . -type f -print)"; then
    snapshot_failure "could not enumerate $source_dir to verify the copy into $destination"
    return 1
  fi
  local relative
  while IFS= read -r relative; do
    [[ -n "$relative" ]] || continue
    if ! cmp -s -- "$source_dir/$relative" "$destination/$relative"; then
      snapshot_failure "the copy of ${relative#./} in $destination differs from its source"
      return 1
    fi
  done <<<"$listing"
  return 0
}

# mirror_directory <source directory> <destination directory>
#
# Makes the destination an exact copy of the source: it is emptied first, so a
# file an earlier run snapshotted can never be mistaken for one this run
# produced.
mirror_directory() {
  local source_dir="$1"
  local destination="$2"

  if ! rm -rf "$destination"; then
    snapshot_failure "could not clear $destination"
    return 1
  fi
  if ! mkdir -p "$destination"; then
    snapshot_failure "could not create $destination"
    return 1
  fi
  copy_directory_contents "$source_dir" "$destination"
}

# count_files <directory> <find -name pattern>
#
# Prints the number of regular files matching the pattern, and 0 for a
# directory that does not exist. It returns non-zero only when `find` itself
# failed, so a broken traversal is never read as an empty directory.
count_files() {
  local dir="$1"
  local pattern="$2"
  local listing

  if [[ ! -d "$dir" ]]; then
    printf '0\n'
    return 0
  fi
  if ! listing="$(find "$dir" -type f -name "$pattern")"; then
    return 1
  fi
  printf '%s\n' "$listing" | awk 'NF { n++ } END { print n + 0 }'
}

# Copies the whole of target/test-reports aside. SNAPSHOT_JUNIT_COUNT is the
# number of suite XML files, which Gate 1 reads as its vacuity check;
# SNAPSHOT_JUNIT_FILES is the number of files copied in total.
snapshot_junit_xml() {
  local files junit

  SNAPSHOT_ERROR=""
  SNAPSHOT_JUNIT_COUNT=0
  SNAPSHOT_JUNIT_FILES=0

  if ! mirror_directory "$TEST_REPORT_DIR" "$SNAPSHOT_DIR/test-reports"; then
    return 1
  fi
  if ! files="$(count_files "$SNAPSHOT_DIR/test-reports" '*')"; then
    snapshot_failure "could not count the files snapshotted from $TEST_REPORT_DIR"
    return 1
  fi
  if ! junit="$(count_files "$SNAPSHOT_DIR/test-reports" 'TEST-*.xml')"; then
    snapshot_failure "could not count the suite XML snapshotted from $TEST_REPORT_DIR"
    return 1
  fi
  SNAPSHOT_JUNIT_FILES="$files"
  SNAPSHOT_JUNIT_COUNT="$junit"
  return 0
}

# Copies the whole of target/parity-report aside. SNAPSHOT_PARITY_COUNT is the
# number of parity reports, which Gate 3 reads; SNAPSHOT_PARITY_FILES is the
# number of files copied in total.
snapshot_parity_reports() {
  local files reports

  SNAPSHOT_ERROR=""
  SNAPSHOT_PARITY_COUNT=0
  SNAPSHOT_PARITY_FILES=0

  if ! mirror_directory "$PARITY_DIR" "$SNAPSHOT_DIR/parity-report"; then
    return 1
  fi
  if ! files="$(count_files "$SNAPSHOT_DIR/parity-report" '*')"; then
    snapshot_failure "could not count the files snapshotted from $PARITY_DIR"
    return 1
  fi
  if ! reports="$(count_files "$SNAPSHOT_DIR/parity-report" '*.json')"; then
    snapshot_failure "could not count the parity reports snapshotted from $PARITY_DIR"
    return 1
  fi
  SNAPSHOT_PARITY_FILES="$files"
  SNAPSHOT_PARITY_COUNT="$reports"
  return 0
}

# Puts this run's reports back after the Rule 9 row's `clean` emptied the two
# published directories. Every snapshotted file is restored, not the files of
# one pattern: the snapshots taken in Gate 1 and Gate 3 are this run's own
# artifacts, so the set CI publishes is complete rather than partial. Nothing
# is regenerated and nothing is invented, and the two counts it publishes are
# what is on disk afterwards.
restore_snapshots() {
  local failed=0
  local errors=""

  SNAPSHOT_ERROR=""
  RESTORED_TEST_FILES=0
  RESTORED_PARITY_FILES=0

  if ! copy_directory_contents "$SNAPSHOT_DIR/test-reports" "$TEST_REPORT_DIR"; then
    errors="$SNAPSHOT_ERROR"
    failed=1
  fi
  if ! copy_directory_contents "$SNAPSHOT_DIR/parity-report" "$PARITY_DIR"; then
    if [[ -n "$errors" ]]; then
      errors="$errors; $SNAPSHOT_ERROR"
    else
      errors="$SNAPSHOT_ERROR"
    fi
    failed=1
  fi

  local restored
  if restored="$(count_files "$TEST_REPORT_DIR" '*')"; then
    RESTORED_TEST_FILES="$restored"
  else
    errors="${errors:+$errors; }could not count the files restored into $TEST_REPORT_DIR"
    failed=1
  fi
  if restored="$(count_files "$PARITY_DIR" '*')"; then
    RESTORED_PARITY_FILES="$restored"
  else
    errors="${errors:+$errors; }could not count the files restored into $PARITY_DIR"
    failed=1
  fi

  # What was snapshotted must come back. A restore that copied without error
  # but left fewer files than this run put aside is the silent loss the
  # snapshots exist to prevent, so the two counts are compared rather than
  # merely published.
  if [[ "$RESTORED_TEST_FILES" -lt "${SNAPSHOT_JUNIT_FILES:-0}" ]]; then
    errors="${errors:+$errors; }restored $RESTORED_TEST_FILES of the ${SNAPSHOT_JUNIT_FILES:-0} file(s) snapshotted from $TEST_REPORT_DIR"
    failed=1
  fi
  if [[ "$RESTORED_PARITY_FILES" -lt "${SNAPSHOT_PARITY_FILES:-0}" ]]; then
    errors="${errors:+$errors; }restored $RESTORED_PARITY_FILES of the ${SNAPSHOT_PARITY_FILES:-0} file(s) snapshotted from $PARITY_DIR"
    failed=1
  fi

  SNAPSHOT_ERROR="$errors"
  if [[ "$failed" -ne 0 ]]; then
    framework_error "restoring this run's snapshotted reports failed: ${errors:-reason not recorded}"
  fi
  return "$failed"
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

  # Both snapshots are status-checked: this row owns them, so a copy that
  # failed is this row's failure rather than a silently smaller artifact set.
  local snapshot_failed=0
  local snapshot_error=""
  snapshot_junit_xml || { snapshot_failed=1; snapshot_error="$SNAPSHOT_ERROR"; }
  snapshot_parity_reports || {
    snapshot_failed=1
    snapshot_error="${snapshot_error:+$snapshot_error; }$SNAPSHOT_ERROR"
  }

  local failed=0

  # The snapshots taken above are this row's evidence for every later row and
  # for the published artifacts, so an incomplete one fails the row that
  # produced it rather than being discovered as a missing file three rows
  # later. Their verdict is carried by `snapshot_failed` and reported below
  # with the reason the helper recorded.

  if [[ "$rc" -ne 0 ]]; then
    detail "sbt clean compile Test/compile test exited $rc"
    failed=1
  fi
  if [[ "$snapshot_failed" -ne 0 ]]; then
    detail "snapshotting this run's reports failed: ${snapshot_error:-reason not recorded}"
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
    printf '# files snapshotted from target/test-reports: %s\n' "${SNAPSHOT_JUNIT_FILES:-0}"
    printf '# parity reports snapshotted: %s\n' "${SNAPSHOT_PARITY_COUNT:-0}"
    printf '# files snapshotted from target/parity-report: %s\n' "${SNAPSHOT_PARITY_FILES:-0}"
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
#     strata-collect's `classes`, and the Test one carries BOTH that
#     `classes` directory and, additionally, its `test-classes` - on one and
#     the same classpath value: that is the proof of the directed edge
#     `"compile->compile;test->test"`, and neither half of it is optional.
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

  local headers="$AUDIT_DIR/gate02a-edge-headers.txt"

  # The root project aggregates strata-collect, so `show` prints BOTH
  # projects' values, each under its own key header: `strata-collect / Compile
  # / internalDependencyClasspath` for the aggregated project and an
  # unprefixed `Compile / internalDependencyClasspath` for the current project,
  # which is strata-basics (the root project). This row measures
  # strata-basics' internal classpath, so ONLY the value under its own header
  # is attributed to a section - strata-collect's value names its own
  # `classes` directory, and letting that land here would let the collect
  # project satisfy a check about the basics project.
  #
  # The three parsed files are emptied FIRST. Appending to whatever a previous
  # run left behind would let last run's content answer this run's checks on
  # any day the output format drifted and nothing parsed at all.
  if ! safe_truncate "$compile_section" ||
    ! safe_truncate "$test_section" ||
    ! safe_truncate "$headers"; then
    detail "the Gate 2a edge section files could not be started"
    failed=1
  fi

  # A value is the indented continuation of the header above it (sbt prints
  # `[info] <TAB>List(...)`), so the first unindented line closes the section
  # and sbt's own chatter cannot be read as part of a value.
  awk -v basics="strata-basics" '
       function trim(s) {
         gsub(/^[[:space:]]+|[[:space:]]+$/, "", s)
         return s
       }
       {
         text = $0
         sub(/^\[(info|warn|error|success|debug)\][[:space:]]?/, "", text)

         if (text ~ /(^|[[:space:]])(Compile|Test)[[:space:]]*\/[[:space:]]*internalDependencyClasspath[[:space:]]*$/) {
           parts_n = split(text, parts, "/")
           config = trim(parts[parts_n - 1])
           project = (parts_n >= 3) ? trim(parts[parts_n - 2]) : ""
           if (project == "" || project == basics) {
             section = tolower(config)
             headers[section]++
             printf "%s / %s (attributed to %s)\n", (project == "" ? basics : project), config, section > header_out
           } else {
             section = "other"
             printf "%s / %s (ignored: another project)\n", project, config > header_out
           }
           next
         }

         if (section != "" && text ~ /^[[:space:]]/) {
           if (section == "compile") { print text > compile_out }
           else if (section == "test") { print text > test_out }
           next
         }
         section = ""
       }
       END {
         printf "compile-headers %d\n", headers["compile"] + 0 > header_out
         printf "test-headers %d\n", headers["test"] + 0 > header_out
       }' \
    compile_out="$compile_section" test_out="$test_section" header_out="$headers" \
    "$edge_log"

  local compile_headers test_headers
  compile_headers="$(awk '/^compile-headers / { print $2 }' "$headers")"
  test_headers="$(awk '/^test-headers / { print $2 }' "$headers")"

  {
    printf '\n# command: sbt -batch "show strata-basics/Compile/internalDependencyClasspath" '
    printf '"show strata-basics/Test/internalDependencyClasspath" (exit %s)\n' "$rc"
    printf '# key headers seen (only strata-basics own values are attributed):\n'
    cat "$headers"
    printf '# Compile section (strata-basics only):\n'
    cat "$compile_section"
    printf '# Test section (strata-basics only):\n'
    cat "$test_section"
  } >>"$EV"

  if [[ "$rc" -ne 0 ]]; then
    detail "show internalDependencyClasspath failed with status $rc"
    failed=1
  fi
  # Attribution is part of the measurement: exactly one strata-basics header
  # per configuration, each with a value under it. Anything else means the
  # output was not what this parse assumes, and a section that parsed empty
  # can satisfy nothing.
  if [[ "${compile_headers:-0}" -ne 1 ]]; then
    detail "expected exactly 1 strata-basics Compile/internalDependencyClasspath header, parsed ${compile_headers:-0}"
    failed=1
  fi
  if [[ "${test_headers:-0}" -ne 1 ]]; then
    detail "expected exactly 1 strata-basics Test/internalDependencyClasspath header, parsed ${test_headers:-0}"
    failed=1
  fi
  if [[ ! -s "$compile_section" ]]; then
    detail "strata-basics' Compile/internalDependencyClasspath value did not parse"
    failed=1
  fi
  if [[ ! -s "$test_section" ]]; then
    detail "strata-basics' Test/internalDependencyClasspath value did not parse"
    failed=1
  fi
  if ! grep -qF "$COLLECT_CLASSES" "$compile_section"; then
    detail "Compile/internalDependencyClasspath lacks $COLLECT_CLASSES"
    failed=1
  fi
  # The Test configuration consumes BOTH of the collect module's outputs, and
  # the specification says so in those words: each classpath carries
  # `strata-collect/target/scala-2.13/classes` and the Test one carries
  # `test-classes` "additionally". Requiring only test-classes here would
  # accept a build wired `test->test` alone, which is not the edge
  # `"compile->compile;test->test"` this row exists to prove. The two fixed
  # strings cannot alias one another: `.../scala-2.13/test-classes` does not
  # contain `.../scala-2.13/classes` as a substring, so each check is its own.
  if ! grep -qF "$COLLECT_CLASSES" "$test_section"; then
    detail "Test/internalDependencyClasspath lacks $COLLECT_CLASSES"
    failed=1
  fi
  if ! grep -qF "$COLLECT_TEST_CLASSES" "$test_section"; then
    detail "Test/internalDependencyClasspath lacks $COLLECT_TEST_CLASSES"
    failed=1
  fi
  # And both of them on the SAME classpath value. `show` prints one value per
  # line, and the Test section holds the value of every project the root
  # aggregates, so two separate matches could in principle come from two
  # different projects' classpaths. One line carrying both directories is the
  # unambiguous proof that it is strata-basics' own Test classpath that
  # consumes the collect module twice over.
  if ! grep -F "$COLLECT_TEST_CLASSES" "$test_section" | grep -qF "$COLLECT_CLASSES"; then
    detail "no single Test classpath carries both $COLLECT_CLASSES and $COLLECT_TEST_CLASSES"
    failed=1
  fi

  # -- zero Java sources ---------------------------------------------------
  # `find` is the authority for this count, so its own status is inspected:
  # a scan that failed part-way prints fewer paths, and a count taken from
  # that output would read "0 .java files" as a pass.
  local java_list="$AUDIT_DIR/gate02a-java-sources.txt"
  local java_count=0
  local find_rc=0
  if ! safe_truncate "$java_list"; then
    detail "the Gate 2a Java-source list could not be started"
    failed=1
  fi
  command find strata-collect strata-basics project -name '*.java' -print \
    >"$java_list" 2>>"$EV" || find_rc=$?
  if [[ "$find_rc" -ne 0 ]]; then
    detail "find for .java files exited $find_rc, so the count is not authoritative"
    failed=1
  else
    java_count="$(awk 'END { print NR + 0 }' "$java_list")"
  fi
  {
    printf '\n# command: find strata-collect strata-basics project -name "*.java" (exit %s)\n' \
      "$find_rc"
    printf '%s\n' "${java_count:-0}"
    cat "$java_list"
  } >>"$EV"
  if [[ "${java_count:-0}" -ne 0 ]]; then
    detail "${java_count} .java file(s) inside the sbt build"
    failed=1
  fi

  # -- file-extension histogram (reported) --------------------------------
  # Reported rather than measured, but a reported figure that silently failed
  # to be produced is worse than none: the write and the appendix are checked.
  local histogram="$AUDIT_DIR/gate02a-extension-histogram.txt"
  if ! safe_truncate "$histogram"; then
    detail "the Gate 2a extension histogram could not be started"
    failed=1
  fi
  local histogram_rc=0
  command find strata-collect strata-basics -type f -print 2>>"$EV" |
    sed 's/.*\.//' | sort | uniq -c | sort -rn >"$histogram" || histogram_rc=$?
  if [[ "$histogram_rc" -ne 0 ]]; then
    detail "the file-extension histogram pipeline exited $histogram_rc"
    failed=1
  elif [[ ! -s "$histogram" ]]; then
    detail "the file-extension histogram is empty, so the two module trees were not scanned"
    failed=1
  elif ! add_appendix "Gate 2a - file-extension histogram of strata-collect and strata-basics" \
    <"$histogram"; then
    detail "the file-extension histogram could not be appended to the report"
    failed=1
  fi

  # -- unmanaged source directories ---------------------------------------
  rc=0
  run_sbt gate02a-source-dirs \
    "show strata-basics/Compile/unmanagedSourceDirectories" \
    "show strata-collect/Compile/unmanagedSourceDirectories" || rc=$?
  local dirs_log="$SBT_LOG"
  local dirs="$AUDIT_DIR/gate02a-source-directories.txt"

  # Every source directory the two `show` commands printed, one per line, and
  # nothing filtered out: a project pointed at a root it should not have is
  # reported by the comparison below rather than quietly dropped.
  #
  # sbt prints a `Seq[File]` in one of two shapes, and both are parsed
  # structurally - by line and by delimiter - never by splitting on
  # whitespace. A checkout path may legitimately contain a space (a developer
  # clone under "My Documents" is enough), and a parser that tokenised on
  # whitespace would find no directory at all and fail this row for a reason
  # that has nothing to do with the build:
  #   [info] <TAB>List(/a/src/main/scala, /b/src/main/scala)   aggregated form
  #   [info] * /a/src/main/scala                               single-value form
  # The only sequence a path may therefore not contain is the ", " that
  # separates the elements of the first shape, which is noted here because
  # nothing in the parse can distinguish it.
  if ! safe_truncate "$dirs"; then
    detail "the Gate 2a source-directory list could not be started"
    failed=1
  fi
  awk '
       function trim(s) {
         gsub(/^[[:space:]]+|[[:space:]]+$/, "", s)
         return s
       }
       function emit(value) {
         value = trim(value)
         if (value != "") print value
       }
       {
         text = $0
         sub(/^\[(info|warn|error|success|debug)\][[:space:]]?/, "", text)
         text = trim(text)

         # The single-value shape: one element per line after a bullet.
         if (text ~ /^\*[[:space:]]/) {
           emit(substr(text, 2))
           next
         }
         # The aggregated shape: a collection literal holding the elements.
         if (text ~ /^(List|Vector|Seq|ArrayBuffer|ArraySeq)\(.*\)$/) {
           inner = text
           sub(/^[A-Za-z]+\(/, "", inner)
           sub(/\)$/, "", inner)
           if (trim(inner) == "") next
           count = split(inner, elements, ", ")
           for (i = 1; i <= count; i++) emit(elements[i])
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
#
# `failed == 0` is only worth reading when the document that states it is
# internally consistent, so the parser below also holds each report to the
# contract of the spec that wrote it: the counts are true integers (a JSON
# boolean is not one, and Python would otherwise read `true` as 1), zero
# discrepancies means an empty list and a non-empty list means a non-zero
# count, and the five basics fixtures satisfy the harness rule `failed == 0`
# iff `passed == rows`.
#
# Each discrepancy is held to the shape of the producer that wrote it, field
# by field, because a list of empty or half-filled entries is an evidence gap
# dressed as evidence:
#
#   * ParityHarness writes `ParityFailure(id, message)` through a derived
#     encoder, so its entries carry exactly those two fields, both non-empty,
#     and its list is never capped - it records every discrepancy, which is
#     why the number of entries must equal `failed`;
#   * DoubleArrayParitySpec attributes every entry to a row, an expectation
#     and a kind of check, all three non-empty, and adds only the diagnostic
#     fields its two failure descriptions document. Its counts are per CHECK
#     and its list is capped at its reporting limit, so a list shorter than
#     `failed` is acceptable only when the marker at its end accounts for
#     exactly the discrepancies omitted - and is a failure without one.
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

# The five fixtures of the basics harness and the one of the collect spec.
# They count `passed` differently - the harness counts fixture ROWS that
# matched in every respect, the collect spec counts individual CHECKS, of
# which each row carries many - so the cross-field rules below are applied
# per producer rather than uniformly. Both producers document their contract
# in the spec that writes the report; this parser is the other half of it.
BASICS_FIXTURES = ["daycount", "schedule", "fx", "currency-math", "holiday"]
COLLECT_FIXTURES = ["double-array"]
FIXTURES = BASICS_FIXTURES + COLLECT_FIXTURES
EXPECTED_KEYS = {"fixture", "rows", "passed", "failed", "failures"}
# The keys of one discrepancy. ParityHarness writes `ParityFailure(id,
# message)` through a derived encoder, so its objects carry those two fields
# and nothing else; the collect spec writes an attributed object that always
# names the row, the expectation and the kind of check, and adds the
# diagnostic fields its two failure descriptions document - the position of
# the value inside a result, the two values and the two deltas, or the
# message of a non-numeric discrepancy.
BASICS_FAILURE_KEYS = {"id", "message"}
BASICS_OPTIONAL_FAILURE_KEYS = set()
COLLECT_FAILURE_KEYS = {"id", "expectation", "check"}
COLLECT_OPTIONAL_FAILURE_KEYS = {
    "index", "row", "column", "expected", "actual", "absDelta", "relDelta", "message"}
# The keys of the entry the collect spec appends when it capped the list.
TRUNCATION_KEYS = {"truncated", "reportedFailures", "omittedFailures", "message"}

problems = []
lines = ["fixture          rows      passed    failed    failures"]


def count(report, key, fixture):
    """The value of a count field, or None when it is not a true integer.

    `isinstance(True, int)` is True in Python, so a JSON boolean would sail
    through an `isinstance(value, int)` test and then compare equal to 0 or
    1. A report whose counts are booleans has not measured anything, so the
    type is rejected explicitly rather than coerced.
    """
    value = report.get(key)
    if isinstance(value, bool) or not isinstance(value, int):
        problems.append(
            f"{fixture}: '{key}' is {value!r} ({type(value).__name__}), not an integer")
        return None
    if value < 0:
        problems.append(f"{fixture}: '{key}' is negative ({value})")
        return None
    return value


def check_failure_elements(fixture, failures, failed, expected_keys, optional_keys,
                           allow_truncation):
    """Validates the shape of every discrepancy and the truncation contract.

    Returns the number of real discrepancies the list describes and whether a
    truncation marker closed it, so that the caller can relate both to
    `failed`. Every field the producer promises must be a non-empty string -
    an entry that names no row, no expectation or no check describes nothing
    and cannot be acted on, which is the same evidence gap as an entry that
    is missing - and no field the producer does not document may appear.

    A truncation marker is accepted only from a producer that documents one,
    only as the last element, only when `failed` genuinely exceeds the
    entries retained, and only with the three counts and the message that
    make the omission explicit.
    """
    described = 0
    marked = False
    for index, failure in enumerate(failures):
        where = f"{fixture}: failures[{index}]"
        if not isinstance(failure, dict):
            problems.append(f"{where} is {type(failure).__name__}, not an object")
            continue
        if "truncated" in failure:
            if not allow_truncation:
                problems.append(
                    f"{where} carries a truncation marker, but this producer records every "
                    "discrepancy and never truncates its list")
                continue
            if index != len(failures) - 1:
                problems.append(f"{where} is a truncation marker but is not the last entry")
                continue
            marked = True
            if set(failure) != TRUNCATION_KEYS:
                problems.append(
                    f"{where} truncation marker keys are {sorted(failure)}, "
                    f"expected {sorted(TRUNCATION_KEYS)}")
                continue
            if failure["truncated"] is not True:
                problems.append(f"{where} 'truncated' is {failure['truncated']!r}, expected true")
            reported, omitted = failure["reportedFailures"], failure["omittedFailures"]
            if isinstance(reported, bool) or not isinstance(reported, int) or reported != index:
                problems.append(
                    f"{where} 'reportedFailures' is {reported!r}, expected the {index} entries "
                    "that precede the marker")
            if isinstance(omitted, bool) or not isinstance(omitted, int) or omitted < 1:
                problems.append(f"{where} 'omittedFailures' is {omitted!r}, expected at least 1")
            elif failed is not None and index + omitted != failed:
                problems.append(
                    f"{where} accounts for {index} + {omitted} discrepancies, "
                    f"but 'failed' is {failed}")
            if failed is not None and failed <= index:
                problems.append(
                    f"{where} claims truncation, but 'failed' ({failed}) does not exceed the "
                    f"{index} entries retained")
            message = failure["message"]
            if not isinstance(message, str) or not message.strip():
                problems.append(
                    f"{where} truncation marker says nothing: 'message' is {message!r}")
            described = failed if failed is not None else index
            continue
        keys = set(failure)
        missing = expected_keys - keys
        if missing:
            problems.append(
                f"{where} is missing {sorted(missing)}; its keys are {sorted(keys)}")
        unknown = keys - expected_keys - optional_keys
        if unknown:
            problems.append(
                f"{where} carries {sorted(unknown)}, which this producer does not document; "
                f"it writes {sorted(expected_keys)}"
                + (f" and may add {sorted(optional_keys)}" if optional_keys else " and nothing else"))
        for key in sorted(expected_keys & keys):
            value = failure[key]
            if not isinstance(value, str) or not value.strip():
                problems.append(f"{where} has no '{key}': it is {value!r}")
        described += 1
    return described, marked


for fixture in FIXTURES:
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

    if not isinstance(report, dict):
        problems.append(f"{fixture}: report is a {type(report).__name__}, not an object")
        lines.append(f"{fixture:<16} MALFORMED")
        continue

    keys = set(report)
    if keys != EXPECTED_KEYS:
        problems.append(
            f"{fixture}: report keys are {sorted(keys)}, expected {sorted(EXPECTED_KEYS)}")
    if report.get("fixture") != fixture:
        problems.append(f"{fixture}: report names fixture {report.get('fixture')!r}")

    rows = count(report, "rows", fixture)
    passed = count(report, "passed", fixture)
    failed = count(report, "failed", fixture)
    failures = report.get("failures")
    lines.append(
        f"{fixture:<16} {str(rows):<9} {str(passed):<9} {str(failed):<9} "
        f"{len(failures) if isinstance(failures, list) else 'MALFORMED'}")

    # Vacuity: a report of no rows, or one that passed nothing, measured
    # nothing and must not be read as parity.
    if rows is not None and rows < 1:
        problems.append(f"{fixture}: report evaluated {rows} rows")
    if passed is not None and passed < 1:
        problems.append(f"{fixture}: report records {passed} passed")

    if not isinstance(failures, list):
        problems.append(
            f"{fixture}: 'failures' is {type(failures).__name__}, not an array")
        continue

    # Which contract the list is held to, and whether a truncation marker is
    # part of it: only the collect spec caps its list, so a marker in a
    # basics report is a report that does not match its producer.
    basics = fixture in BASICS_FIXTURES
    described, marked = check_failure_elements(
        fixture, failures, failed,
        BASICS_FAILURE_KEYS if basics else COLLECT_FAILURE_KEYS,
        BASICS_OPTIONAL_FAILURE_KEYS if basics else COLLECT_OPTIONAL_FAILURE_KEYS,
        allow_truncation=not basics)

    # Cross-field consistency, both directions: zero discrepancies means an
    # empty list, and a non-empty list means a non-zero count. Either
    # contradiction is a report that disagrees with itself, and reading one
    # of its two halves as the verdict is how a broken run passes this row.
    if failed == 0 and failures:
        problems.append(
            f"{fixture}: 'failed' is 0 but {len(failures)} failure(s) are listed")
    if failed is not None and failed > 0 and not failures:
        problems.append(f"{fixture}: {failed} discrepancy(ies) counted but none listed")

    if fixture in BASICS_FIXTURES:
        # The harness records EVERY discrepancy, so the list is the count.
        if failed is not None and described != failed:
            problems.append(
                f"{fixture}: 'failed' is {failed} but {described} discrepancy(ies) are described")
        # It counts `passed` in fixture rows, and documents the equivalence
        # `failed == 0` <=> `passed == rows`.
        if rows is not None and passed is not None and passed > rows:
            problems.append(f"{fixture}: {passed} rows passed of {rows} evaluated")
        if None not in (rows, passed, failed) and (failed == 0) != (passed == rows):
            problems.append(
                f"{fixture}: 'failed' is {failed} while {passed} of {rows} rows passed, "
                "which breaks the harness rule that failed == 0 iff passed == rows")
    else:
        # The collect spec counts checks, of which every row carries many, so
        # its totals must exceed its rows rather than equal them.
        #
        # Its list may be capped, and the cap is the whole reason this needs
        # its own rule: a list shorter than `failed` is only acceptable when
        # the marker at its end says how many were dropped. Without one, the
        # report has silently described fewer discrepancies than it counted,
        # and whoever reads it cannot tell which ones are missing.
        if failed is not None and not marked and described != failed:
            problems.append(
                f"{fixture}: 'failed' is {failed} but {described} discrepancy(ies) are described "
                "and no truncation marker accounts for the remainder")
        if None not in (rows, passed, failed) and passed + failed < rows:
            problems.append(
                f"{fixture}: {passed} + {failed} checks over {rows} rows, fewer than one "
                "check per row, so the run cannot have measured every row")
        if None not in (rows, passed) and failed == 0 and passed < rows:
            problems.append(
                f"{fixture}: {passed} checks passed over {rows} rows with no failure, so rows "
                "went unmeasured")

    if failed is not None and failed != 0:
        problems.append(f"{fixture}: {failed} discrepancy(ies) against the Java baseline")
        for failure in failures[:5]:
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
    detail "the six parity reports are not all present, self-consistent and free of discrepancies"
    failed=1
  fi

  if [[ -f "$summary" ]]; then
    add_appendix "Gate 3 - parity row counts per fixture" <"$summary"
  fi
  # This row re-snapshots the parity reports it just produced, so a failed copy
  # is its failure: the later rows and the published artifacts read that copy.
  if ! snapshot_parity_reports; then
    detail "snapshotting the parity reports failed: ${SNAPSHOT_ERROR:-reason not recorded}"
    failed=1
  fi
  {
    printf '\n# parity reports snapshotted: %s (files copied: %s)\n' \
      "${SNAPSHOT_PARITY_COUNT:-0}" "${SNAPSHOT_PARITY_FILES:-0}"
  } >>"$EV"

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
# abstract heads of the index families.
#
# The REASON is part of the inventory, not decoration. Section 0.6.4 states
# every exclusion together with the rationale that makes it legitimate, and
# this block is published as the authoritative record of why 39 public types
# of the two modules carry no codec - so an exclusion that arrived with the
# wrong rationale, or with none, would be a false statement in the deliverable
# evidence. The whole `EXCLUDED <fqcn> <reason>` line is therefore compared,
# which makes this an exact fqcn-to-reason map rather than a set of names.
# Five rationales are shared by construction (the store, the contract types,
# the machinery, the abstract heads and the generic containers) and five types
# state their own. The
# contract rationale covers the five single-abstract-method callbacks of
# `DoubleArray` and `DoubleMatrix` as well as the behavioural interfaces: each
# is a function surface with no data, which is the same reading section 0.6.4
# applies to `DateAdjuster`.
expected_excluded_lines() {
  cat <<'EOF'
EXCLUDED com.opengamma.strata.basics.CalculationTarget function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.CalculationTargetList its element type is excluded
EXCLUDED com.opengamma.strata.basics.CombinedReferenceData heterogeneous identifier-to-value store; only calendars are serializable and HolidayCalendar carries them
EXCLUDED com.opengamma.strata.basics.ImmutableReferenceData heterogeneous identifier-to-value store; only calendars are serializable and HolidayCalendar carries them
EXCLUDED com.opengamma.strata.basics.ReferenceData heterogeneous identifier-to-value store; only calendars are serializable and HolidayCalendar carries them
EXCLUDED com.opengamma.strata.basics.ReferenceData.Entry same reason as the store it populates
EXCLUDED com.opengamma.strata.basics.ReferenceDataId behavioural abstraction; HolidayCalendarId is the one identifier with a codec
EXCLUDED com.opengamma.strata.basics.Resolvable function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.ResolvableCalculationTarget function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.currency.FxConvertible function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.currency.FxRateProvider function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.currency.LazyFxRateProvider function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.date.DateAdjuster function or contract type with no data of its own
EXCLUDED com.opengamma.strata.basics.date.DayCount.ScheduleInfo behavioural interface, implemented by the covered Schedule
EXCLUDED com.opengamma.strata.basics.date.HolidaySafeReferenceData heterogeneous identifier-to-value store; only calendars are serializable and HolidayCalendar carries them
EXCLUDED com.opengamma.strata.basics.index.FloatingRate abstract head of a family whose leaf types are covered
EXCLUDED com.opengamma.strata.basics.index.FloatingRateIndex abstract head of a family whose leaf types are covered
EXCLUDED com.opengamma.strata.basics.index.Index abstract head of a family whose leaf types are covered
EXCLUDED com.opengamma.strata.basics.index.IndexObservation abstract head of a family whose leaf types are covered
EXCLUDED com.opengamma.strata.basics.index.RateIndex abstract head of a family whose leaf types are covered
EXCLUDED com.opengamma.strata.collect.ArgCheck typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.Collections typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.DoubleArrayMath typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.Named typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.TypedStringCompanion typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.Validate typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.array.DoubleArray.DoubleTernaryOperator function or contract type with no data of its own
EXCLUDED com.opengamma.strata.collect.array.DoubleMatrix.ElementAction function or contract type with no data of its own
EXCLUDED com.opengamma.strata.collect.array.DoubleMatrix.ElementFunction function or contract type with no data of its own
EXCLUDED com.opengamma.strata.collect.array.DoubleMatrix.RowArrayFunction function or contract type with no data of its own
EXCLUDED com.opengamma.strata.collect.array.DoubleMatrix.RowArrayObjectFunction function or contract type with no data of its own
EXCLUDED com.opengamma.strata.collect.array.Matrix trait; DoubleMatrix is covered
EXCLUDED com.opengamma.strata.collect.io.Resources typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.json.Codecs typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.named.NamedEnum typeclass, helper or effect rather than data
EXCLUDED com.opengamma.strata.collect.result.FailureOr generic container; no port-owned codec
EXCLUDED com.opengamma.strata.collect.result.ResultNec generic container; no port-owned codec
EXCLUDED com.opengamma.strata.collect.result.ValidatedFailures generic container; no port-owned codec
EXCLUDED com.opengamma.strata.collect.result.ValueWithFailures generic container; no port-owned codec
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

  # Whole lines on both sides: `COVERED <category> <fqcn>` and
  # `EXCLUDED <fqcn> <reason>`. Keeping only the fqcn of an exclusion would
  # compare half of the statement and publish the other half unchecked.
  awk '/^COVERED / { print }' "$block" | sort >"$actual_covered"
  awk '/^EXCLUDED / { print }' "$block" | sort >"$actual_excluded"
  expected_covered_lines | sort >"$want_covered"
  expected_excluded_lines | sort >"$want_excluded"

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
    detail "the excluded list, type or reason, differs from the closed section 0.6.4 inventory"
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
    detail "$(wc -l <"$actual_covered" | tr -d ' ') covered and $(wc -l <"$actual_excluded" | tr -d ' ') excluded types, identical to the closed inventory including every exclusion reason"
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
# run over exactly five class files and the disassembly is restricted to the
# bodies the specification names - the element-wise operations - "and their
# @tailrec helpers". Each selected body must contain no boxing call.
#
# HOW THE BODIES ARE SELECTED, AND WHY NOT BY NAME
#   Naming the helpers by pattern does not work. Scala 2 emits them as private
#   methods whose names have no reliable relationship to the operation that
#   calls them: `multipliedBy` and `dividedBy` both delegate to `scaledInto`,
#   `combineReduce` to `ternaryFoldFrom`, `DoubleMatrix$.tabulate` to `build`,
#   `buildRows` and `fillRow`, and `DoubleArrayMath$.applyAddition` to
#   `addInto`. A selector anchored on the operation names would miss every one
#   of those bodies, and boxing inside them would pass this row unseen.
#
#   So selection is a VERIFIED CALL GRAPH instead. From the roots named below
#   the analyser follows, transitively, every method reference the bytecode of
#   a selected body actually makes - both the same-class form javap prints
#   without an owner (`// Method scaledInto:([DDI)V`) and the qualified form
#   into any of the other four audited classes - and it attaches the lifted
#   `$anonfun$<enclosing>$<n>` body of every lambda those methods pass, which
#   `invokedynamic` does not name in the disassembly. It then asserts that the
#   graph is CLOSED: a method called from an audited body and not itself
#   audited fails the row, so the audit cannot silently stop at a delegation.
#
# WHAT IS ASSERTED, PER CLASS
#   * every hot method the specification names is declared by the class (a
#     renamed or removed operation fails the row rather than shrinking it);
#   * every known loop helper is reached individually, by name;
#   * at least the number of bodies the class is known to carry is selected;
#   * zero `BoxesRunTime`, `Double.valueOf` or `Double.doubleValue` calls in
#     every selected body.
#
# THE THREE DOCUMENTED EXCLUSIONS
#   `DoubleMatrix$.of`, `DoubleArrayMath$.toObject` and `.toPrimitive` are the
#   boxed boundary: converting a `Seq[Double]` or an `Array[java.lang.Double]`
#   to `double[]` and back IS boxing, and section 0.6.6 scopes Rule 3 to the
#   inner loops over the primitive arrays. They are named in the analyser with
#   their reason and reported in the evidence, never quietly skipped; if a
#   rewrite ever made one reachable from a root, its body would be audited
#   like any other and its boxing would fail this row.
#
# Note the packages: DoubleArray and DoubleMatrix are in
# `com.opengamma.strata.collect.array`, DoubleArrayMath is in
# `com.opengamma.strata.collect`.
#
# Specialised Function1 calls such as `apply$mcDI$sp` are permitted: they are
# the unboxed path and do not match the forbidden pattern.
#=============================================================================

BOXING_FORBIDDEN='scala/runtime/BoxesRunTime, java/lang/Double.valueOf, Double.doubleValue'

row_07_no_boxing() {
  new_evidence gate05-no-boxing.txt
  local failed=0

  {
    printf '## Gate 5 / Rule 3 - no boxing in the numeric hot paths\n'
    printf '# selection: the transitive, asserted-closed call graph of the hot methods\n'
    printf '# forbidden calls: %s\n\n' "$BOXING_FORBIDDEN"
  } >>"$EV"

  # The five class files of the specification, and the simple name each is
  # known by in the analyser's inventory. A companion object's class file
  # differs from its class's only by the trailing `$`, which must survive
  # into every derived file name or the two dumps would overwrite each other.
  local names=(DoubleArray 'DoubleArray$' DoubleMatrix 'DoubleMatrix$' 'DoubleArrayMath$')
  local files=(
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleArray\$.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleMatrix.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/array/DoubleMatrix\$.class"
    "$COLLECT_CLASSES/com/opengamma/strata/collect/DoubleArrayMath\$.class"
  )

  local arguments=()
  local index simple class_file dump
  for index in "${!names[@]}"; do
    simple="${names[$index]}"
    class_file="${files[$index]}"
    if ! require_file "$class_file" "hot-path class file"; then
      failed=1
      printf '# MISSING: %s\n' "$class_file" >>"$EV"
      continue
    fi
    dump="$AUDIT_DIR/gate05-disasm-${simple//\$/-object}.txt"
    if ! javap -c -p "$class_file" >"$dump" 2>>"$EV"; then
      detail "javap failed on $class_file"
      failed=1
      continue
    fi
    printf '# %-72s -> %s\n' "${class_file#"$ROOT"/}" "${dump#"$ROOT"/}" >>"$EV"
    arguments+=("$simple=$dump")
  done

  # Five dumps or nothing: an audit of four classes is not this row.
  if [[ "${#arguments[@]}" -ne "${#names[@]}" ]]; then
    detail "${#arguments[@]} of ${#names[@]} hot-path classes could be disassembled"
    failed=1
  fi

  if [[ "${#arguments[@]}" -eq 0 ]]; then
    printf '\n# no class was disassembled, so no body could be audited\n' >>"$EV"
    return 1
  fi

  printf '\n' >>"$EV"
  if ! python3 - "$AUDIT_DIR" "${arguments[@]}" <<'PY' >>"$EV" 2>&1; then
import re
import sys

audit_dir = sys.argv[1]
dumps = dict(argument.split("=", 1) for argument in sys.argv[2:])

# The inventory of this row, per class.
#
#   `hot`          the operations AAP section 0.10.1 names. Each must be
#                  declared by the class, so a renamed or deleted operation
#                  fails this row instead of quietly shrinking the audit.
#   `elementwise`  the remaining operations of the same kind on the same
#                  primitive arrays, which the specification's list does not
#                  enumerate one by one. They are roots because Rule 3 is
#                  about inner loops over `Array[Double]`, and these are
#                  inner loops over `Array[Double]`.
#   `helpers`      the private loop bodies the operations delegate to. Each
#                  is asserted individually: the call graph must REACH it, so
#                  a delegation the graph cannot see fails the row.
#   `floor`        the number of method names the class is known to carry
#                  through this audit - a vacuity guard against a selection
#                  that has collapsed.
INVENTORY = {
    "DoubleArray": {
        "hot": ["plus", "minus", "multipliedBy", "dividedBy", "map", "mapWithIndex", "combine",
                "reduce", "sum", "min", "max", "equalWithTolerance", "tabulate"],
        "elementwise": ["combineReduce", "equalZeroWithTolerance", "sorted", "concat", "subArray",
                        "with", "get", "contains", "indexOf", "lastIndexOf", "forEach", "toArray"],
        "helpers": ["plusInto", "minusInto", "scaledInto", "mapInto", "mapWithIndexInto",
                    "plusEachInto", "minusEachInto", "multipliedByEachInto", "dividedByEachInto",
                    "combineEachInto", "ternaryFoldFrom", "minFrom", "maxFrom", "sumFrom",
                    "reduceFrom", "concatArray", "firstIndexOfFrom", "lastIndexOfFrom",
                    "forEachFrom"],
        "floor": 30,
    },
    "DoubleArray$": {
        "hot": ["tabulate"],
        "elementwise": ["of", "copyOf", "filled"],
        "helpers": ["tabulateInto"],
        "floor": 4,
    },
    "DoubleMatrix": {
        "hot": ["plus", "minus", "multipliedBy", "map", "mapWithIndex", "combine", "reduce",
                "total", "transpose", "tabulate"],
        "elementwise": ["row", "column", "get", "with", "forEach", "toArray"],
        "helpers": ["reduceFrom", "totalFrom", "forEachFrom", "columnCopy", "fillColumn"],
        "floor": 20,
    },
    "DoubleMatrix$": {
        "hot": ["tabulate"],
        "elementwise": ["identity", "diagonal", "filled", "copyOf", "ofArrays", "ofArrayObjects"],
        "helpers": ["build", "buildRows", "fillRow", "fillWith", "fillDiagonal", "fillFromArrays",
                    "allocateRows", "cloneRows"],
        "floor": 12,
    },
    "DoubleArrayMath$": {
        "hot": ["combine", "sum"],
        "elementwise": ["apply", "applyAddition", "applyMultiplication", "combineByAddition",
                        "combineByMultiplication", "combineLenient", "allFuzzyEquals",
                        "allFuzzyEqualsZero", "fuzzyEquals", "fuzzyEqualsZero", "isAscending",
                        "isMathematicalInteger", "sortedOrder", "reorderedCopy", "selectDoubles",
                        "selectInts", "selectValues", "sortPairs", "mergePasses"],
        "helpers": ["addInto", "multiplyInto", "applyInto", "combineInto", "combineLenientInto",
                    "identityInto", "sumFrom", "mergeRunInto", "mergeRunsFrom", "reorderInto",
                    "selectDoublesInto", "selectIntsInto", "selectValuesInto", "boundedIndex",
                    "fuzzyEqualsUnchecked"],
        "floor": 25,
    },
}

# The boxed boundary: these three convert between the primitive arrays and
# boxed values, so boxing is the operation rather than a cost inside a loop,
# and section 0.6.6 scopes Rule 3 to the loops. They are not roots; they are
# reported with their reason, and if a rewrite ever made one reachable from a
# root its body would be audited and its boxing would fail this row.
EXCLUDED = {
    "DoubleMatrix$": {
        "of": "boxed varargs boundary: unboxes a Seq[Double] through drainInto",
    },
    "DoubleArrayMath$": {
        "toObject": "double[] -> Array[java.lang.Double]: boxing IS the operation",
        "toPrimitive": "Array[java.lang.Double] -> double[]: unboxing IS the operation",
    },
}

FORBIDDEN = re.compile(r"scala/runtime/BoxesRunTime|java/lang/Double\.valueOf|Double\.doubleValue")
# A javap member declaration sits at exactly two spaces of indent; everything
# more deeply indented belongs to the member above it.
DECLARATION = re.compile(r"^  \S")
# `// Method scaledInto:([DDI)V` (same class, no owner) and
# `// Method com/opengamma/.../DoubleArray$.copyOf:([D)L...;` (qualified).
REFERENCE = re.compile(r"//\s*(?:Method|InterfaceMethod)\s+(\S+)")
ANONYMOUS = re.compile(r"^\$anonfun\$(?P<enclosing>.+)\$\d+$")

problems = []
report = []


def parse(path):
    """Reads one disassembly into an ordered list of (name, declaration) and
    the body lines of each."""
    order = []
    bodies = {}
    current = None
    with open(path, encoding="utf-8", errors="replace") as handle:
        for raw in handle:
            line = raw.rstrip("\n")
            if DECLARATION.match(line):
                current = None
                declaration = line.strip()
                if "(" not in declaration:
                    continue  # a field, not a method
                name = declaration.split("(", 1)[0].split()[-1].rpartition(".")[2]
                current = (name, declaration)
                order.append(current)
                bodies[current] = []
                continue
            if current is not None:
                bodies[current].append(line)
    return order, bodies


def references(body):
    """Every method this body calls, as (owning simple name or None, name)."""
    found = set()
    for line in body:
        match = REFERENCE.search(line)
        if not match:
            continue
        target = match.group(1).rsplit(":", 1)[0]
        if "/" in target:
            owner, _, method = target.rpartition(".")
            found.add((owner.replace("/", ".").rpartition(".")[2], method))
        else:
            found.add((None, target))
    return found


classes = {}
for simple, path in sorted(dumps.items()):
    try:
        order, bodies = parse(path)
    except OSError as error:
        problems.append(f"{simple}: disassembly {path} could not be read: {error}")
        continue
    if not bodies:
        problems.append(f"{simple}: disassembly {path} declares no method")
        continue
    classes[simple] = {"order": order, "bodies": bodies, "names": {n for n, _ in order}}

for simple in INVENTORY:
    if simple not in classes:
        problems.append(f"{simple}: no usable disassembly, so none of its bodies were audited")

# The roots, and the assertion that the specification's own hot methods exist.
selected = {simple: set() for simple in classes}
frontier = []
for simple, spec in INVENTORY.items():
    if simple not in classes:
        continue
    for name in spec["hot"]:
        if name not in classes[simple]["names"]:
            problems.append(
                f"{simple}: the specification names '{name}' as a hot method, but the class "
                "declares no such member")
    for name in spec["hot"] + spec["elementwise"]:
        if name in classes[simple]["names"] and name not in selected[simple]:
            selected[simple].add(name)
            frontier.append((simple, name))

# The closure: every method an audited body calls, plus the lifted body of
# every lambda it passes, transitively.
while frontier:
    simple, name = frontier.pop()
    for candidate in classes[simple]["names"]:
        match = ANONYMOUS.match(candidate)
        if match and match.group("enclosing") == name and candidate not in selected[simple]:
            selected[simple].add(candidate)
            frontier.append((simple, candidate))
    for key, body in classes[simple]["bodies"].items():
        if key[0] != name:
            continue
        for owner, method in references(body):
            target = simple if owner is None else owner
            if target not in classes or method not in classes[target]["names"]:
                continue
            if method not in selected[target]:
                selected[target].add(method)
                frontier.append((target, method))

total_names = 0
total_bodies = 0
total_hits = 0
for simple in sorted(classes):
    spec = INVENTORY.get(simple)
    if spec is None:
        problems.append(f"{simple}: disassembled but absent from the inventory of this row")
        continue
    chosen = selected[simple]
    stem = simple.replace("$", "-object")
    body_path = f"{audit_dir}/gate05-boxing-{stem}.txt"
    hits = []
    unclosed = []
    written = 0
    with open(body_path, "w", encoding="utf-8") as handle:
        for key in classes[simple]["order"]:
            name, declaration = key
            if name not in chosen:
                continue
            written += 1
            body = classes[simple]["bodies"][key]
            handle.write(f"### method: {declaration}\n")
            for line in body:
                handle.write(line + "\n")
                if FORBIDDEN.search(line):
                    hits.append((declaration, line.strip()))
            for owner, method in references(body):
                target = simple if owner is None else owner
                if target in classes and method in classes[target]["names"] \
                        and method not in selected[target]:
                    unclosed.append(f"{declaration} -> {target}.{method}")
        handle.write(f"### selected-bodies: {written}\n")

    total_names += len(chosen)
    total_bodies += written
    total_hits += len(hits)
    roots = sorted(set(spec["hot"] + spec["elementwise"]) & classes[simple]["names"])
    report.append(
        f"{simple}: {len(chosen)} method name(s), {written} body(ies), {len(hits)} boxing call(s)")
    report.append(f"    roots    : {' '.join(roots)}")
    report.append(f"    selected : {' '.join(sorted(chosen))}")
    for name, reason in sorted(EXCLUDED.get(simple, {}).items()):
        state = "declared" if name in classes[simple]["names"] else "absent"
        reached = " AND reachable from a root, so its body IS audited" if name in chosen else ""
        report.append(f"    excluded by design: {name} [{state}]{reached} - {reason}")

    if len(chosen) < spec["floor"]:
        problems.append(
            f"{simple}: {len(chosen)} method name(s) selected, fewer than the {spec['floor']} this "
            "class is known to carry through the audit, so the selection has collapsed")
    for helper in spec["helpers"]:
        if helper not in classes[simple]["names"]:
            problems.append(
                f"{simple}: required loop helper '{helper}' is not declared by the class; name "
                "the body that replaced it in the inventory of this row")
        elif helper not in chosen:
            problems.append(
                f"{simple}: required loop helper '{helper}' is declared but was NOT reached from "
                "any audited root, so its body would go uninspected")
    for declaration, line in hits:
        problems.append(f"{simple}: boxing in {declaration}\n        {line}")
    for edge in unclosed:
        problems.append(
            f"{simple}: the call graph is not closed: {edge} is called from an audited body but "
            "was not audited itself")

report.append(
    f"TOTAL: classes={len(classes)} names={total_names} bodies={total_bodies} "
    f"boxing={total_hits}")

summary_path = f"{audit_dir}/gate05-boxing-selection.txt"
with open(summary_path, "w", encoding="utf-8") as handle:
    handle.write("\n".join(report) + "\n")
    if problems:
        handle.write("\nproblems:\n" + "\n".join(problems) + "\n")

print("\n".join(report))
if problems:
    print("\nproblems:")
    print("\n".join(problems))
    sys.exit(1)
PY
    detail "the no-boxing audit reported problems (see gate05-boxing-selection.txt)"
    failed=1
  fi

  local selection="$AUDIT_DIR/gate05-boxing-selection.txt"
  if [[ -f "$selection" ]]; then
    add_appendix "Gate 5 - no-boxing audit: the bodies selected, per class" <"$selection"
  fi

  if [[ "$failed" -eq 0 ]]; then
    local audited_names audited_bodies
    audited_names="$(awk -F'names=' '/^TOTAL: / { split($2, field, " "); print field[1] }' "$selection")"
    audited_bodies="$(awk -F'bodies=' '/^TOTAL: / { split($2, field, " "); print field[1] }' "$selection")"
    detail "five hot-path classes, ${audited_names:-0} method name(s) and ${audited_bodies:-0} body(ies) audited through the closed call graph, zero boxing calls"
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
# THE SPECIFICATION'S COMMAND IS THE ONE THAT DECIDES
#   The specification writes that second check as
#     grep -rn "\.ini\|\.csv\|\.properties" strata-collect/src/main strata-basics/src/main
#   and states its pass condition as "no resource lookups in main sources".
#   That exact command is what runs here, and its output is what decides this
#   row - not a narrower substitute run beside it.
#
#   The command is, however, a regular expression whose `.` are wildcards and
#   which has no word boundary, so it matches inside ordinary identifiers:
#   `initialValue` matches `\.ini`. Every line it reports is therefore
#   ADJUDICATED, one match at a time, against the single rule that separates a
#   file reference from an identifier:
#
#     a match is a RESOURCE REFERENCE when the character the wildcard matched
#     is a literal `.` AND the extension does not continue into an identifier
#     - `"Currency.ini"`, `foo.csv)`, `bar.properties` at end of line;
#
#     otherwise it is a candidate for the SANCTIONED list, because the token
#     is part of a longer word: either the extension continues
#     (`.initialValue`, `.csvRow`) or the wildcard matched a letter rather
#     than a dot (`minimum` for `\.ini`).
#
#   Being a longer word is not on its own enough to be excused, because a
#   rule that excuses a whole CLASS of matches would let a new one appear
#   unnoticed. The identifier matches that are excused are ENUMERATED, one
#   per (file, identifier) pair, in RULE4_SANCTIONED_MATCHES below, each with
#   the reason it is there. A match that is not a resource reference and not
#   in that inventory is UNSANCTIONED and fails this row: a new identifier
#   that trips the specification's pattern, or the same identifier appearing
#   in another file, forces a decision rather than inheriting one.
#
#   Every sanctioned line is listed in the evidence with the identifier and
#   the inventory reason that explains it, and the counts must reconcile with
#   the number of lines the command reported: a line that cannot be
#   adjudicated fails this row, so no output of the specification's command
#   can be dropped, and a real resource lookup fails it however the regex
#   happened to match.
#
#   The tighter `\.(ini|csv|properties)\b` scan is kept as a second, redundant
#   formulation of the same requirement. It must also be empty.
#=============================================================================

# The complete list of matches of the specification's resource scan that are
# not resource references: <path> TAB <identifier> TAB <why>. Nothing else is
# excused. Adding an entry is a deliberate act with a stated reason; removing
# the code that produced one simply leaves the entry unused, which is
# reported rather than failed.
RULE4_SANCTIONED_MATCHES=(
  "strata-basics/src/main/scala/com/opengamma/strata/basics/value/ValueSchedule.scala	initialValue	the Java property name ValueSchedule.initialValue, which AAP 0.8.2 preserves and 0.6.4 carries as a JSON key; the unescaped . of the specification pattern matches the l.ini inside .initialValue"
)

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

  # -- the specification's own command, and the adjudication of its output --
  local literal_output="$AUDIT_DIR/gate05-resource-scan.txt"
  local literal_rc=0
  grep -rn "\.ini\|\.csv\|\.properties" strata-collect/src/main strata-basics/src/main \
    >"$literal_output" 2>&1 || literal_rc=$?
  local literal_lines
  literal_lines="$(awk 'NF { n++ } END { print n + 0 }' "$literal_output")"
  {
    printf '## the specification'"'"'s resource scan, which decides this row\n'
    printf '# command: grep -rn "\\.ini\\|\\.csv\\|\\.properties" strata-collect/src/main strata-basics/src/main\n'
    printf '# grep exit status: %s (0 = it matched, 1 = nothing matched, >=2 = error)\n' "$literal_rc"
    printf '# lines reported: %s (raw output: %s)\n' "$literal_lines" "${literal_output#"$ROOT"/}"
  } >>"$EV"
  # The enumerated exceptions, written where the adjudication reads them and
  # left behind as evidence of exactly what was excused.
  local sanctioned_inventory="$AUDIT_DIR/gate05-resource-scan-sanctioned.txt"
  printf '%s\n' "${RULE4_SANCTIONED_MATCHES[@]}" >"$sanctioned_inventory"

  # grep >= 2 is an error - a missing path or an unreadable file - and is
  # never read as an absence of matches.
  if [[ "$literal_rc" -gt 1 ]]; then
    detail "the specification's resource scan failed with status $literal_rc"
    failed=1
  elif ! python3 - "$literal_output" "$sanctioned_inventory" <<'PY' >>"$EV" 2>&1; then
import re
import sys

path, inventory_path = sys.argv[1], sys.argv[2]
# The specification's pattern, with its `.` wildcards intact, so that this
# adjudication sees exactly the matches the specification's command saw.
PATTERN = re.compile(r".(ini|csv|properties)")
IDENTIFIER = re.compile(r"[A-Za-z0-9_$]")

resource_references = []
sanctioned = []
unsanctioned = []
unclassified = []
reported = 0

try:
    with open(path, encoding="utf-8", errors="replace") as handle:
        lines = handle.read().splitlines()
except OSError as error:
    print(f"the scan output {path} could not be read: {error}")
    sys.exit(1)

# The enumerated exceptions, keyed by the file and the identifier they excuse.
inventory = {}
try:
    with open(inventory_path, encoding="utf-8") as handle:
        for entry in handle:
            if not entry.strip():
                continue
            source, _, rest = entry.rstrip("\n").partition("\t")
            word, _, why = rest.partition("\t")
            inventory[(source, word)] = why or "no reason recorded"
except OSError as error:
    print(f"the sanctioned-match inventory {inventory_path} could not be read: {error}")
    sys.exit(1)
used = set()

for line in lines:
    if not line.strip():
        continue
    reported += 1
    # `grep -rn` prints `<path>:<line number>:<text>`. Anything else - a
    # "Binary file ... matches" notice, say - cannot be adjudicated and is
    # reported as such rather than assumed harmless.
    parts = line.split(":", 2)
    if len(parts) != 3 or not parts[1].isdigit():
        unclassified.append(line)
        continue
    source, number, text = parts
    verdicts = []
    for match in PATTERN.finditer(text):
        token = match.group(0)
        extension = match.group(1)
        following = text[match.end():match.end() + 1]
        continues = bool(following) and bool(IDENTIFIER.match(following))
        if token.startswith(".") and not continues:
            verdicts.append(("resource", f"'{token}{following}' reads as a file extension", ""))
            continue
        # The word the extension sits inside, for the record. The scan starts
        # at the extension rather than at the wildcard character, so that a
        # wildcard which matched a separator is not read into the word.
        start = match.start() + 1
        while start > 0 and IDENTIFIER.match(text[start - 1]):
            start -= 1
        end = match.end()
        while end < len(text) and IDENTIFIER.match(text[end]):
            end += 1
        word = text[start:end]
        if token.startswith("."):
            how = f"'.{extension}' continues into the identifier '{word}'"
        else:
            how = f"the wildcard matched '{token[0]}' rather than '.', inside the word '{word}'"
        # An identifier match is excused only where it is enumerated, for
        # this file and this identifier. Anything else is a new match and is
        # reported as one.
        if (source, word) in inventory:
            used.add((source, word))
            verdicts.append(("sanctioned", how, word))
        else:
            verdicts.append(
                ("unsanctioned",
                 f"{how}, which is not in RULE4_SANCTIONED_MATCHES for this file", word))
    if not verdicts:
        # The line was reported by grep, so the pattern matched it; if this
        # adjudication cannot see the match, the adjudication is wrong.
        unclassified.append(line)
        continue
    kinds = {kind for kind, _, _ in verdicts}
    if "resource" in kinds:
        why = "; ".join(reason for kind, reason, _ in verdicts if kind == "resource")
        resource_references.append(f"{source}:{number}: {why}\n        {text.strip()}")
    elif "unsanctioned" in kinds:
        why = "; ".join(reason for kind, reason, _ in verdicts if kind == "unsanctioned")
        unsanctioned.append(f"{source}:{number}: {why}\n        {text.strip()}")
    else:
        # The reason each match is excused, once per line however many
        # matches the line carried, so the record stays readable.
        why = "; ".join(reason for _, reason, _ in verdicts)
        reasons = []
        for _, _, word in verdicts:
            reason = inventory[(source, word)]
            if reason not in reasons:
                reasons.append(reason)
        sanctioned.append(f"{source}:{number}: {why} -- {'; '.join(reasons)}")

unused = sorted(key for key in inventory if key not in used)

print(f"# lines adjudicated: {reported}")
print(f"#   resource references: {len(resource_references)}")
print(f"#   sanctioned identifier matches: {len(sanctioned)} "
      f"(inventory: {len(inventory)} entry(ies), {len(used)} used)")
print(f"#   unsanctioned identifier matches: {len(unsanctioned)}")
print(f"#   unclassifiable: {len(unclassified)}")
for entry in sanctioned:
    print(f"    SANCTIONED {entry}")
for source, word in unused:
    print(f"    UNUSED INVENTORY ENTRY {source} '{word}' no longer matches")
for entry in unclassified:
    print(f"    UNCLASSIFIED {entry}")
for entry in unsanctioned:
    print(f"    UNSANCTIONED {entry}")
for entry in resource_references:
    print(f"    RESOURCE {entry}")

if (len(resource_references) + len(sanctioned) + len(unsanctioned) + len(unclassified)
        != reported):
    print("the adjudication did not account for every reported line")
    sys.exit(1)
if resource_references:
    print(f"{len(resource_references)} resource file reference(s) in the modules' main sources")
    sys.exit(1)
if unsanctioned:
    print(f"{len(unsanctioned)} match(es) of the specification's scan are neither a resource "
          "reference nor an enumerated exception")
    sys.exit(1)
if unclassified:
    print(f"{len(unclassified)} reported line(s) could not be adjudicated")
    sys.exit(1)
PY
    detail "the specification's resource scan reports a match this row does not excuse: a resource lookup, an identifier match outside RULE4_SANCTIONED_MATCHES, or a line that could not be adjudicated"
    failed=1
  fi

  # The same requirement, expressed as a file-extension match. Redundant by
  # construction - every line it can report is a resource reference the
  # adjudication above also rejects - and kept so that the row states the
  # requirement twice rather than relying on one formulation.
  if ! assert_no_match "resource file references in main sources (extension-anchored)" "$EV" \
    -rnE "\.(ini|csv|properties)\b" strata-collect/src/main strata-basics/src/main; then
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "closed families and data tables verified; the specification's resource scan reports no file reference in main sources, and every match it did report is one of the matches RULE4_SANCTIONED_MATCHES enumerates (count: ${#RULE4_SANCTIONED_MATCHES[@]})"
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
#     THE DECISION SET IS THE RAW DIFFERENCE. The JVM names a hidden class
#     after its host plus the ADDRESS at which it was defined, e.g.
#     `java.lang.reflect.Proxy$$Lambda/0x00007af...`, and those addresses
#     never agree between two runs - so the raw difference is large (of the
#     order of a thousand entries) and dominated by hidden classes whose
#     address-free identity the baseline run loaded too. It is tempting to
#     strip the address before comparing, and that is exactly what must NOT
#     happen: normalising first merges distinct hidden classes into one name
#     and then deletes the whole group whenever the baseline happened to load
#     any member of it, so a codec-only hidden class disappears from the
#     comparison before anything looks at it.
#
#     Every raw entry is therefore kept and ADJUDICATED into exactly one of
#     three buckets, and the buckets must add up to the raw difference:
#
#       plain                   an ordinary class, address-free. It must not
#                               belong to a reflection package, and it is
#                               disassembled and grepped as in (a).
#       hidden, new identity    a hidden class the baseline run's own
#                               occurrences do not account for: either its
#                               address-free identity is absent from the
#                               baseline entirely, or the codec run loaded
#                               MORE occurrences of that identity than the
#                               baseline did and this is one of the surplus.
#                               Either way it is attributable to
#                               encode/decode: its identity must not belong to
#                               a reflection package, and its host class is
#                               disassembled in its place.
#       hidden, known identity  one occurrence of an identity paired against
#                               one occurrence the baseline run loaded. The
#                               address differs because addresses always
#                               differ; the identity does not. Counted, listed
#                               and its host inspected, and reported
#                               separately if it sits in a reflection package
#                               - which would then be pre-existing rather than
#                               codec-attributable.
#
#     The pairing is by OCCURRENCE COUNT, not by membership: a single
#     baseline occurrence exempts a single codec occurrence and no more, so
#     twenty extra `Codecs$$$Lambda` definitions in the codec run are twenty
#     new entries rather than one known identity repeated. Membership alone
#     would reintroduce, inside the adjudication, the very collapse that
#     keeping the raw addresses avoided.
#
#     The digest each run prints is also compared. Both runs do the same
#     baseline work, so the digests must be equal; were they not, the two runs
#     would have generated different values and their class-load difference
#     would no longer be attributable to encoding.
#
# (c) Every entry of the difference is inspected through a class javap can
#     actually read, and the ones for which no such class exists are
#     documented rather than skipped.
#
#     A hidden class cannot be disassembled under its own name, and neither
#     can the marker the JVM records in place of a source: `source:
#     __JVM_LookupDefineClass__` is not a class name, and handing it to javap
#     would fail the row on a class that was never there. The host is
#     therefore derived in a fixed order - the `source:` field when it names a
#     real class (a lambda records its host there), else the identity up to
#     `$$Lambda`, else the enclosing class of the identity - and an entry for
#     which none of those resolves is recorded as "not inspectable
#     (JVM-generated)" with its reason. Those entries remain in the decision
#     set: their identities went through the reflection-package check above,
#     which is what the audit needs from them.
#=============================================================================

RULE6_REFLECTION_PATTERN='java/lang/reflect/|java/lang/Class.forName|getDeclaredMethod|getDeclaredField'
RULE6_REFLECTION_PACKAGES='^(java\.lang\.reflect|sun\.reflect|jdk\.internal\.reflect|scala\.reflect\.runtime)\.'

# Extracts `<class name><TAB><source>` from a -Xlog:class+load log, exactly as
# the JVM wrote it: names keep their `/0x<address>` suffix, because that
# suffix is part of a hidden class's identity in this log and the comparison
# that decides the row is made on the raw names. $1 log, $2 names file,
# $3 map file.
classload_index() {
  awk 'index($1, "[class,load]") > 0 && $3 == "source:" {
         source = $4
         for (i = 5; i <= NF; i++) source = source " " $i
         print $2 "\t" source
       }' "$1" | sort -u >"$3"
  cut -f1 "$3" | sort -u >"$2"
}

# Disassembles one class, resolving where to read it from its recorded source.
# $1 class name, $2 source; prints the disassembly.
#
# A source that is not a location - the JVM's `__JVM_LookupDefineClass__`
# marker, the CDS archive, the JRT image, or nothing at all - means "look this
# class up the ordinary way", with no classpath. Only a real location becomes
# a `-cp` argument. The marker itself is never passed as a CLASS NAME: the
# classifier resolves a host before anything reaches this function.
javap_from_source() {
  local class_name="$1"
  local source="$2"
  case "$source" in
    'shared objects file' | jrt:* | __JVM* | '-' | '')
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
  # The raw difference, on the names the JVM wrote. This is the decision set.
  comm -13 "$baseline_names" "$codec_names" >"$delta"
  sort -u "$baseline_map" "$codec_map" >"$combined_map"

  RULE6_DELTA_SIZE="$(awk 'NF { n++ } END { print n + 0 }' "$delta")"
  {
    printf '\n# (b) baseline classes: %s, codec classes: %s\n' \
      "$(wc -l <"$baseline_names" | tr -d ' ')" "$(wc -l <"$codec_names" | tr -d ' ')"
    printf '#     raw delta (codec only, addresses intact): %s entries -- see %s\n' \
      "$RULE6_DELTA_SIZE" "${delta#"$ROOT"/}"
  } >>"$EV"

  if [[ "$RULE6_DELTA_SIZE" -lt 1 ]]; then
    detail "(b) the class-load difference is empty, so the audit measured nothing"
    failed=1
  fi

  # -- the adjudication of the raw difference -------------------------------
  #
  # One line of `rule6-delta-adjudication.txt` per raw entry, and three
  # derived files: the identities that must be free of reflection packages,
  # the de-duplicated list of classes to disassemble, and the entries for
  # which no disassemblable class exists.
  local adjudication="$AUDIT_DIR/rule6-delta-adjudication.txt"
  local identities="$AUDIT_DIR/rule6-delta-identities.txt"
  local inspection="$AUDIT_DIR/rule6-delta-inspection-list.txt"
  local noninspectable="$AUDIT_DIR/rule6-noninspectable-classes.txt"
  local classification_rc=0
  python3 - "$delta" "$baseline_names" "$codec_names" "$combined_map" \
    "$adjudication" "$identities" "$inspection" "$noninspectable" <<'PY' >>"$EV" 2>&1 || classification_rc=$?
import re
import sys
from collections import Counter

(delta_path, baseline_path, codec_path, map_path, adjudication_path, identities_path,
 inspection_path, noninspectable_path) = sys.argv[1:9]

HIDDEN = re.compile(r"/0x[0-9a-fA-F]+$")
# A class name, as opposed to a location or one of the JVM's own markers.
CLASS_NAME = re.compile(r"^[A-Za-z_$][A-Za-z0-9_$]*(\.[A-Za-z0-9_$]+)*$")
NOT_A_CLASS = ("shared objects file", "-", "")


def read_lines(path):
    with open(path, encoding="utf-8", errors="replace") as handle:
        return [line.rstrip("\n") for line in handle if line.strip()]


def identity_of(name):
    """The address-free identity of a class name."""
    return HIDDEN.sub("", name)


try:
    delta = read_lines(delta_path)
    baseline_names = read_lines(baseline_path)
    codec_names = set(read_lines(codec_path))
    sources = {}
    for line in read_lines(map_path):
        name, _, source = line.partition("\t")
        sources.setdefault(name, source)
except OSError as error:
    print(f"# the class-load index could not be read: {error}")
    sys.exit(1)

# How many occurrences of each address-free identity the baseline run may
# account for, as a BUDGET rather than a yes-or-no membership test.
#
# A hidden class is renamed on every run - `Codecs$$$Lambda/0x000...a` in one
# and `/0x000...b` in the next - so the raw name of a codec-run hidden class
# is almost always absent from the baseline and lands in the difference even
# when the two runs loaded the same class. Pairing occurrence for occurrence
# is what separates that from a class the codec run loaded MORE of: if the
# baseline loaded three `Failure$anon$lazy$macro$189$1$$Lambda` and the codec
# run loaded thirty-three, thirty of them are codec-only and are charged as
# such. Treating one baseline occurrence as exempting every codec occurrence
# is exactly the collapse this audit exists to prevent.
#
# A codec name that is byte-identical to a baseline name is not in the
# difference at all, yet it has consumed one baseline occurrence, so it is
# subtracted from the budget before the difference is paired against it.
baseline_hidden = Counter(
    identity_of(name) for name in baseline_names if HIDDEN.search(name))
already_paired = Counter(
    identity_of(name) for name in baseline_names
    if HIDDEN.search(name) and name in codec_names)
budget = Counter()
for identity, count in baseline_hidden.items():
    remaining = count - already_paired.get(identity, 0)
    if remaining > 0:
        budget[identity] = remaining


def host_of(name, identity, source):
    """The class to disassemble in place of a hidden class, why, and its kind.

    The order is fixed and every step is checked, because the alternative -
    handing javap whatever the log recorded - fails the row on names that were
    never classes: `__JVM_LookupDefineClass__` is a marker, not a host.

    The kind matters to what the disassembly can be held to. A lambda's host
    is the code that created the lambda, so a reflection reference in it is a
    reflection reference on the path being audited. The enclosing class of a
    JVM-defined hidden class (`java.lang.invoke.LambdaForm$MH` -> LambdaForm)
    is the JVM's own method-handle machinery, not the code that ran, so its
    disassembly is reported rather than charged - and the identity itself has
    already been through the reflection-package check.
    """
    if source not in NOT_A_CLASS and not source.startswith("__JVM") \
            and not source.startswith("jrt:") and not source.startswith("file:") \
            and CLASS_NAME.match(source):
        return source, "the source field names its host class", "host"
    lambda_host = identity.split("$$Lambda", 1)[0]
    if lambda_host and lambda_host != identity and CLASS_NAME.match(lambda_host):
        return lambda_host, "the host of a lambda proxy, from its own name", "host"
    package, _, simple = identity.rpartition(".")
    outer = simple.split("$", 1)[0]
    if package and outer and outer != simple:
        return f"{package}.{outer}", "the enclosing class of a JVM-defined hidden class", "jvm"
    return (None,
            f"JVM-generated, with no host class to disassemble (source: {source or 'unknown'})",
            "none")


buckets = {"plain": 0, "hidden-known-identity": 0, "hidden-new-identity": 0}
rows = []
identity_lines = []
to_inspect = {}
noninspectable = []
# The two ways an entry reaches the new-identity bucket, counted apart so the
# evidence says which, and every new entry that the charge rule reports
# rather than charges, grouped by the class it points at and the reason - so
# none of them passes unseen, while the per-entry record stays in the
# adjudication file where each one is a line of its own.
baseline_identity_seen = set(baseline_hidden)
baseline_class_names = set(baseline_names)
surplus_entries = 0
absent_identity_entries = 0
reported_new = Counter()

for name in delta:
    identity = identity_of(name)
    source = sources.get(name, "")
    if identity == name:
        bucket = "plain"
        target, why, kind = name, "an ordinary class, disassembled under its own name", "self"
        if source in NOT_A_CLASS or source.startswith("__JVM"):
            source_for_javap = "-"
        else:
            source_for_javap = source
    else:
        # One occurrence of the baseline's budget for this identity, or none
        # left - in which case this is a codec-side surplus and is treated
        # exactly like an identity the baseline never loaded.
        if budget[identity] > 0:
            budget[identity] -= 1
            bucket = "hidden-known-identity"
        else:
            bucket = "hidden-new-identity"
        target, why, kind = host_of(name, identity, source)
        source_for_javap = sources.get(target, "-") if target else "-"
        if source_for_javap in NOT_A_CLASS or source_for_javap.startswith("__JVM"):
            source_for_javap = "-"
    # What a reflection reference in the disassembly means, decided per entry
    # and recorded with its reason, because the disassembly of a HOST is the
    # whole class and not just the lambda that was defined in it.
    #
    #   charged   an ordinary class of the difference that the port ships or
    #             depends on; or a lambda whose host class is itself absent
    #             from the baseline run, so the body being read exists only
    #             because encode/decode ran. A reflection reference in it is
    #             a reflection reference on the audited path.
    #   reported  a PLATFORM class of the JDK. Its body is the runtime's own
    #             implementation, not the port's: `java.util.Random` reads a
    #             field offset through `Class.getDeclaredField` to seed
    #             itself, and decoding a date pulls in a dozen
    #             `java.time.format` classes, so charging those bodies would
    #             fail this row for using `LocalDate.parse`. What it cannot
    #             hide is the port itself reflecting: that loads classes of
    #             `java.lang.reflect` and friends, and (b) above fails on any
    #             of those appearing in the difference, platform or not.
    #   reported  a lambda whose host the baseline run loaded too - the body
    #             being read is byte-identical in both runs, so a reference in
    #             it says nothing about encoding, while what IS new about the
    #             entry, its identity, has gone through the reflection-package
    #             check; or the JVM's own method-handle machinery, which has
    #             no host on the audited path at all.
    #
    # Every reported new-identity entry is named individually in the evidence
    # with that reason, so the distinction can be read rather than trusted.
    if bucket == "plain":
        # A platform class is one the JVM loaded from its own image - the CDS
        # archive or the JRT image - AND whose name is in a platform package.
        # Both, so that a class with a missing source cannot pass as one.
        platform = ((source_for_javap == "-" or source_for_javap.startswith("jrt:"))
                    and name.startswith(("java.", "javax.", "jdk.", "sun.", "com.sun.")))
        charged = not platform
        charge_reason = ("an ordinary class of the difference" if charged
                         else "a platform class of the JDK, read from the runtime image")
    elif bucket == "hidden-new-identity" and kind == "host":
        charged = target not in baseline_class_names
        charge_reason = ("its host class exists only in the codec run" if charged
                         else "its host class was loaded by the baseline run too")
    elif bucket == "hidden-new-identity":
        charged, charge_reason = False, "JVM machinery with no host on the audited path"
    else:
        charged, charge_reason = False, "paired against a baseline occurrence"
    charge = "charged" if charged else "reported"
    why = f"{why}; {charge_reason}"
    if bucket == "hidden-new-identity":
        if identity in baseline_identity_seen:
            surplus_entries += 1
            why = f"{why}; codec-side surplus of an identity the baseline also loaded"
        else:
            absent_identity_entries += 1
        if not charged:
            reported_new[(target or "-", charge_reason)] += 1
    buckets[bucket] += 1
    rows.append("\t".join(
        [bucket, charge, name, identity, target or "-", source_for_javap or "-", why]))
    identity_lines.append(f"{bucket}\t{identity}")
    if target is None:
        noninspectable.append(f"{name}\tnot inspectable ({why})")
    else:
        entry = to_inspect.setdefault(target, [charge, bucket, source_for_javap or "-", 0])
        entry[3] += 1
        # One charged entry is enough to charge the class it points at, and a
        # plain entry makes its inspection mandatory: a javap failure on an
        # ordinary class of the difference fails the row.
        if charged:
            entry[0] = "charged"
        if bucket == "plain":
            entry[1] = "plain"

with open(adjudication_path, "w", encoding="utf-8") as handle:
    handle.write("# bucket\tcharge\tname\tidentity\tinspect\tsource\twhy\n")
    handle.write("\n".join(rows) + ("\n" if rows else ""))
with open(identities_path, "w", encoding="utf-8") as handle:
    handle.write("\n".join(identity_lines) + ("\n" if identity_lines else ""))
with open(inspection_path, "w", encoding="utf-8") as handle:
    for target in sorted(to_inspect):
        charge, bucket, source, entries = to_inspect[target]
        handle.write(f"{charge}\t{bucket}\t{target}\t{source}\t{entries}\n")
with open(noninspectable_path, "w", encoding="utf-8") as handle:
    handle.write("\n".join(noninspectable) + ("\n" if noninspectable else ""))

accounted = sum(buckets.values())
covered = sum(entry[3] for entry in to_inspect.values()) + len(noninspectable)
charged_classes = sum(1 for entry in to_inspect.values() if entry[0] == "charged")
print(f"#     adjudicated: {accounted} entries of {len(delta)}")
for bucket in sorted(buckets):
    print(f"#       {bucket}: {buckets[bucket]}")
print(f"#         new-identity entries that are a codec-side surplus of a shared identity: "
      f"{surplus_entries}")
print(f"#         new-identity entries whose identity the baseline never loaded: "
      f"{absent_identity_entries}")
print(f"#         new-identity entries reported rather than charged: {sum(reported_new.values())} "
      f"in {len(reported_new)} group(s), every one a line of its own in the adjudication file")
for (target, reason), count in sorted(reported_new.items()):
    print(f"#           {count:4d} x {target}: {reason}")
print(f"#       classes to disassemble: {len(to_inspect)} "
      f"({charged_classes} charged, covering {covered - len(noninspectable)} entries)")
print(f"#       not inspectable, documented: {len(noninspectable)}")
print(f"#     adjudication: {adjudication_path}")

if accounted != len(delta) or covered != len(delta):
    print("#     the adjudication does not account for every entry of the raw difference")
    sys.exit(1)
PY
  if [[ "$classification_rc" -ne 0 ]]; then
    detail "(b) the class-load difference could not be adjudicated entry by entry"
    failed=1
  fi

  # The reflection-package check, over the identity of every entry of the
  # decision set that encode/decode introduced: every plain class, and every
  # hidden occurrence the baseline run's own occurrences do not account for -
  # a new identity or a surplus of a shared one.
  local new_identities="$AUDIT_DIR/rule6-delta-new-identities.txt"
  awk -F'\t' '$1 == "plain" || $1 == "hidden-new-identity" { print $2 }' "$identities" |
    sort -u >"$new_identities"
  {
    printf '#     identities introduced by encode/decode: %s\n' \
      "$(awk 'NF { n++ } END { print n + 0 }' "$new_identities")"
  } >>"$EV"
  if ! assert_no_match "(b) reflection packages among the identities encode/decode introduced" \
    "$EV" -E "$RULE6_REFLECTION_PACKAGES" "$new_identities"; then
    failed=1
  fi

  # A hidden occurrence paired against one of the baseline's own is
  # pre-existing, not codec-attributable - but if it sits in a reflection
  # package it is stated here, with its count, rather than left out of the
  # record.
  local known_reflective="$AUDIT_DIR/rule6-preexisting-reflective-identities.txt"
  awk -F'\t' '$1 == "hidden-known-identity" { print $2 }' "$identities" | sort -u |
    grep -E "$RULE6_REFLECTION_PACKAGES" >"$known_reflective" || true
  {
    printf '#     pre-existing reflective identities (paired against a baseline occurrence, reported not charged): %s\n' \
      "$(awk 'NF { n++ } END { print n + 0 }' "$known_reflective")"
    if [[ -s "$known_reflective" ]]; then
      sed 's/^/#       /' "$known_reflective"
    fi
  } >>"$EV"

  # -- (c) every entry of the difference, through a class javap can read ----
  local per_class="$AUDIT_DIR/rule6-delta-inspection.txt"
  : >"$per_class"

  # Entries the classifier could not give a host to at all, counted before
  # the loop appends to the same file. Every count below is in ENTRIES of the
  # raw difference, never in classes, so that they reconcile with its size.
  local hostless
  hostless="$(awk 'NF { n++ } END { print n + 0 }' "$noninspectable")"

  local charge bucket target source entries out class_hits inspect_rc
  local inspected=0
  local covered=0
  local unreadable=0
  local undisassemblable=0
  local total_hits=0
  local reported_hits=0
  while IFS=$'\t' read -r charge bucket target source entries; do
    [[ -n "$target" ]] || continue
    inspect_rc=0
    out="$(javap_from_source "$target" "$source" 2>&1)" || inspect_rc=$?
    if [[ "$inspect_rc" -ne 0 || -z "$out" ]]; then
      printf '%s\t%s\t%s\tjavap FAILED (status %s, source %s)\t%s entry(ies)\n' \
        "$charge" "$bucket" "$target" "$inspect_rc" "$source" "$entries" >>"$per_class"
      if [[ "$bucket" == "plain" ]]; then
        # An ordinary class of the difference must be readable: if it is not,
        # the audit has not seen it and the row does not pass.
        detail "(c) javap could not disassemble $target"
        unreadable=$((unreadable + entries))
        failed=1
      else
        # A derived host that cannot be read is recorded as what it is: a
        # JVM-generated class with no disassemblable body. Its identity has
        # already been through the reflection-package check above.
        printf '%s\tnot inspectable (the derived host could not be disassembled, status %s) - %s entry(ies)\n' \
          "$target" "$inspect_rc" "$entries" >>"$noninspectable"
        undisassemblable=$((undisassemblable + entries))
      fi
      continue
    fi
    inspected=$((inspected + 1))
    covered=$((covered + entries))
    class_hits="$(printf '%s\n' "$out" |
      awk -v pattern="$RULE6_REFLECTION_PATTERN" '$0 ~ pattern { n++ } END { print n + 0 }')"
    printf '%s\t%s\t%s\t%s reflection reference(s)\t%s entry(ies)\n' \
      "$charge" "$bucket" "$target" "$class_hits" "$entries" >>"$per_class"
    if [[ "$class_hits" -ne 0 ]]; then
      if [[ "$charge" == "charged" ]]; then
        total_hits=$((total_hits + class_hits))
        detail "(c) $class_hits reflection reference(s) in $target"
        failed=1
      else
        # Reported, not charged: the baseline run loaded this identity too,
        # or the class is the JVM's own method-handle machinery rather than
        # code the audited path executed. Either way it is in the evidence.
        reported_hits=$((reported_hits + class_hits))
      fi
    fi
  done <"$inspection"

  local accounted=$((covered + undisassemblable + hostless + unreadable))
  {
    printf '\n# (c) classes disassembled: %s, covering %s of the %s entries of the difference\n' \
      "$inspected" "$covered" "$RULE6_DELTA_SIZE"
    printf '#     entries whose class is JVM-generated with no host to read: %s\n' "$hostless"
    printf '#     entries whose derived host could not be disassembled: %s\n' "$undisassemblable"
    printf '#     entries of an ordinary class javap could not read (a failure): %s\n' "$unreadable"
    printf '#     entries accounted for: %s of %s\n' "$accounted" "$RULE6_DELTA_SIZE"
    printf '#     reflection references charged to encode/decode: %s\n' "$total_hits"
    printf '#     reflection references reported but not charged (pre-existing identities and\n'
    printf '#       the JVM'"'"'s own method-handle machinery): %s\n' "$reported_hits"
    printf '#     per-class detail: %s\n' "${per_class#"$ROOT"/}"
    printf '#     not-inspectable list: %s\n' "${noninspectable#"$ROOT"/}"
  } >>"$EV"

  # Reconciliation: every entry of the raw difference was either inspected
  # through a class, or documented as having none to inspect. An entry that is
  # neither would have left the audit silently, which is the whole failure
  # mode this row is built to prevent.
  if [[ "$accounted" -ne "$RULE6_DELTA_SIZE" ]]; then
    detail "(c) $accounted of the $RULE6_DELTA_SIZE entries of the difference were accounted for"
    failed=1
  fi

  {
    printf 'baseline class-load log  : %s\n' "${baseline_log#"$ROOT"/}"
    printf 'codec class-load log     : %s\n' "${codec_log#"$ROOT"/}"
    printf 'baseline digest          : %s\n' "${RULE6_BASELINE_DIGEST:-absent}"
    printf 'codec digest             : %s\n' "${RULE6_CODEC_DIGEST:-absent}"
    printf 'raw delta (the decision set, addresses intact): %s entries\n' "$RULE6_DELTA_SIZE"
    awk -F'\t' 'NR > 1 { n[$1]++ } END { for (b in n) printf "  %-22s %s entries\n", b, n[b] }' \
      "$adjudication" | sort
    printf 'occurrence pairing (a baseline occurrence exempts one codec occurrence, no more):\n'
    awk -F'\t' 'NR > 1 && $1 == "hidden-new-identity" {
                  if ($7 ~ /codec-side surplus/) surplus++; else absent++
                }
                END { printf "  %-22s %s entries\n  %-22s %s entries\n",
                        "surplus of a shared identity", surplus + 0,
                        "identity absent from baseline", absent + 0 }' "$adjudication"
    printf 'classes disassembled     : %s, covering %s entries\n' "$inspected" "$covered"
    printf 'documented not inspectable: %s entries (%s with no host, %s whose host could not be read)\n' \
      "$((hostless + undisassemblable))" "$hostless" "$undisassemblable"
    printf 'entries accounted for    : %s of %s\n' "$accounted" "$RULE6_DELTA_SIZE"
    printf 'reflection references    : %s charged, %s reported (a paired occurrence, a host the\n' \
      "$total_hits" "$reported_hits"
    printf '                           baseline run loaded too, or the JVM'"'"'s own machinery)\n'
    printf '\nnot inspectable, with the reason each was recorded under:\n'
    cat "$noninspectable"
  } | add_appendix "Rule 6 - class-load audit of the codec path"

  if [[ "$failed" -eq 0 ]]; then
    detail "no reflection in either module; the $RULE6_DELTA_SIZE-entry encode/decode difference holds none either"
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
# The specification measures this row with three commands:
#
#   grep -n '"-Werror"' build.sbt
#   grep -rnE "nowarn|SuppressWarnings|-Wconf|Werror" build.sbt project strata-*/src
#   sbt -batch clean compile Test/compile
#
# and its pass condition is: `-Werror` present exactly in the common settings;
# no `@nowarn`, `@SuppressWarnings`, `-Wconf` or any other `Werror`
# occurrence (no scoped removal); compile succeeds.
#
# The scan is run over the whole of that scope - build.sbt, the build
# definition under project/ RECURSIVELY, and both modules' src trees, main and
# test, recursively - with sbt's own generated output (project/target and
# project/project, which hold the COMPILED form of build.sbt, do not exist in a
# clean checkout and appear only because an earlier row ran sbt) the one thing
# left out. EVERY raw hit is written into the evidence, and the rule that
# decides the row is the pass condition read literally: the scan must return
# EXACTLY ONE hit, build.sbt's `-Werror` option element, and every other hit
# fails - whatever file it is in, whether it is code or a comment, and however
# harmless it looks. Nothing is dropped, no path is excluded, no line number is
# allow-listed and there is no exempt class. A comment that merely names the
# flag is a hit like any other, so the sources carry no such comment: the three
# scaladoc sentences that used to name it were reworded to say "warnings as
# errors" instead.
#
# Each hit is still LABELLED, because a label tells whoever has to fix a
# failure which clause of the pass condition it broke. The labels are all
# fatal except the first:
#
#   ALLOWED    the one build.sbt line whose text is just the "-Werror" option
#              element. The pass condition requires it to be present exactly
#              once, which check 1 measures on build.sbt independently.
#   VIOLATION  suppression-construct: a hit containing `nowarn`,
#              `SuppressWarnings` or `-Wconf`, wherever it is.
#   VIOLATION  build-definition-occurrence: any OTHER hit in build.sbt or in a
#              non-generated file under project/. This is the pass condition's
#              "no scoped removal" clause: a second mention of the flag in the
#              build definition is how the option gets re-scoped, filtered or
#              lint-excluded away.
#   VIOLATION  module-source-occurrence: any hit in either module's sources.
#   VIOLATION  unparseable-scan-line: a scan line that is not of the form
#              path:line:text - grep's "Binary file ... matches" among them.
#              Nothing this row cannot parse is quietly ignored.
#
# Two further checks cover the ways the option can be neutralised without
# naming a suppression construct: an explicit scan for a removal
# (`scalacOptions -=` / `--=`), for a `filterNot` over the option list, for any
# `-Wconf` addition and for ANY `excludeLintKeys` occurrence (sbt's own lint
# suppression, which this build carried once and must never carry again); and a
# positive control that the raw scan really did find build.sbt's option line,
# so the row cannot pass because its scope or its pattern silently stopped
# matching anything.
#
# This row RUNS `clean`, which empties target/test-reports and
# target/parity-report (build.sbt registers both with `cleanFiles`). Every file
# this run wrote into them is restored from the Gate 1 and Gate 3 snapshots
# afterwards, the rows after it read those snapshots, and the class files
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

  # -- 2. no suppression anywhere, over the whole scope --------------------
  # The build definition, enumerated so that project/ is scanned RECURSIVELY
  # with sbt's generated output - and nothing else - left out.
  local project_files=""
  if ! project_files="$(find project -type f \
    -not -path 'project/target/*' -not -path 'project/project/*' | sort)"; then
    detail "enumerating the build definition under project/ failed"
    failed=1
  fi
  local build_definition=()
  local file
  while IFS= read -r file; do
    if [[ -n "$file" ]]; then
      build_definition+=("$file")
    fi
  done <<<"$project_files"

  local raw raw_rc=0
  raw="$(grep -rn -E "nowarn|SuppressWarnings|-Wconf|Werror" \
    build.sbt "${build_definition[@]}" strata-collect/src strata-basics/src 2>&1)" || raw_rc=$?

  # Every raw hit, with its class in the first field. The classes and the
  # pass-condition clause each one implements are documented in this row's
  # header comment; the awk below is that table and nothing else.
  local classified
  classified="$(printf '%s\n' "$raw" | awk '
    NF {
      line = $0
      first = index(line, ":")
      rest = (first > 0) ? substr(line, first + 1) : ""
      second = index(rest, ":")
      if (first == 0 || second == 0) {
        printf "%-38s %s\n", "VIOLATION unparseable-scan-line", line
        next
      }
      path = substr(line, 1, first - 1)
      lineno = substr(rest, 1, second - 1)
      text = substr(rest, second + 1)
      if (lineno !~ /^[0-9]+$/) {
        printf "%-38s %s\n", "VIOLATION unparseable-scan-line", line
        next
      }

      is_build = (path == "build.sbt" || path ~ /^project\//)

      if (text ~ /nowarn/ || text ~ /SuppressWarnings/ || text ~ /-Wconf/) {
        verdict = "VIOLATION suppression-construct"
      } else if (is_build) {
        if (path == "build.sbt" && text ~ /^[[:space:]]*"-Werror"[,]?[[:space:]]*$/) {
          verdict = "ALLOWED option-declaration"
        } else {
          verdict = "VIOLATION build-definition-occurrence"
        }
      } else {
        verdict = "VIOLATION module-source-occurrence"
      }
      printf "%-38s %s:%s:%s\n", verdict, path, lineno, text
    }')"

  local raw_count allowed_count violation_count violations
  raw_count="$(printf '%s\n' "$raw" | awk 'NF { n++ } END { print n + 0 }')"
  allowed_count="$(printf '%s\n' "$classified" | awk '/^ALLOWED/ { n++ } END { print n + 0 }')"
  violation_count="$(printf '%s\n' "$classified" | awk '/^VIOLATION/ { n++ } END { print n + 0 }')"
  violations="$(printf '%s\n' "$classified" | awk '/^VIOLATION/')"

  {
    printf '# command: grep -rn -E "nowarn|SuppressWarnings|-Wconf|Werror" build.sbt %s strata-collect/src strata-basics/src\n' \
      "${build_definition[*]}"
    printf '# grep exit status: %s (0 = matched, 1 = nothing matched, >=2 = error)\n' "$raw_rc"
    printf '# scope: build.sbt; project/ recursively minus sbt generated output\n'
    printf '#        (project/target, project/project, the compiled form of build.sbt);\n'
    printf '#        strata-collect/src and strata-basics/src recursively, main and test.\n'
    printf '# pass: exactly one hit, build.sbt'"'"'s "-Werror" option element. Every other\n'
    printf '#       hit fails, in any file, in code or in a comment. No exempt class.\n'
    printf '# raw hits: %s (allowed %s, violations %s)\n' \
      "$raw_count" "$allowed_count" "$violation_count"
    printf '# every raw hit, labelled:\n%s\n' "$classified"
    printf '# violations:\n%s\n\n' "${violations:-(none)}"
  } >>"$EV"

  detail "Rule 9 scan: $raw_count raw hit(s), $allowed_count allowed, $violation_count violation(s)"

  if [[ "$raw_rc" -gt 1 ]]; then
    detail "the suppression scan failed with status $raw_rc"
    failed=1
  fi
  # Positive control: the scan must at least find the option line it exists to
  # protect. No hit at all means the scope or the pattern is broken, not that
  # the repository is clean.
  if [[ "$raw_count" -eq 0 ]]; then
    detail "the Rule 9 scan produced no hit at all: its scope or its pattern is broken"
    failed=1
  elif [[ "$allowed_count" -lt 1 ]]; then
    detail "the Rule 9 scan did not find build.sbt's \"-Werror\" option line among its $raw_count hit(s)"
    failed=1
  fi
  if [[ "$violation_count" -gt 0 ]]; then
    detail "$violation_count Rule 9 violation(s), all listed in the evidence; first: $(printf '%s\n' "$violations" | head -n 1)"
    failed=1
  fi

  # -- the option is never removed, filtered or lint-excluded away ---------
  if ! assert_no_match "removal, filtering or lint-exclusion of a compiler option" "$EV" \
    -rn -E "scalacOptions[^=]*(--=|[^-]-=)|scalacOptions.*filterNot|filterNot.*W(error|conf)|W(error|conf).*filterNot|excludeLintKeys|-Wconf" \
    build.sbt "${build_definition[@]}" strata-collect/src strata-basics/src; then
    failed=1
  fi

  # -- 3. the build itself --------------------------------------------------
  run_sbt rule9-compile clean compile "Test/compile" || rc=$?
  # `clean` empties target/test-reports and target/parity-report, which CI
  # publishes; every file this run wrote into them is put back from the
  # snapshots, and a restore that failed is this row's failure because this row
  # is what emptied them.
  local restore_error=""
  if ! restore_snapshots; then
    restore_error="$SNAPSHOT_ERROR"
    detail "restoring the published reports after clean failed: ${restore_error:-reason not recorded}"
    failed=1
  fi
  {
    printf '# command: sbt -batch clean compile Test/compile (exit %s)\n' "$rc"
    printf '# log: %s\n' "${SBT_LOG#"$ROOT"/}"
    printf '# NOTE: this row cleans, which empties target/test-reports and\n'
    printf '#       target/parity-report. Every file this run wrote into them was\n'
    printf '#       restored from target/audit/snapshot afterwards - the whole\n'
    printf '#       snapshot, not one name pattern - so the published artifacts are\n'
    printf '#       complete; the late rows read the snapshot in either case.\n'
    printf '# files restored into target/test-reports: %s\n' "${RESTORED_TEST_FILES:-0}"
    printf '# files restored into target/parity-report: %s\n' "${RESTORED_PARITY_FILES:-0}"
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
#
# The specification's pass condition has a second half: the command must be
# "documented in README.md and strata-basics/README.md". A demo nobody can
# find is not an end-to-end demo, so both files are required to carry the
# exact command line this row runs, and the lines that carry it are copied
# into the evidence.
#=============================================================================

# The demo command, written once: this row runs it and then requires the two
# READMEs to document it verbatim.
GATE6_DEMO_COMMAND='sbt "strata-basics/run"'

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

  # -- the command is documented where the specification requires ----------
  local documented_in=(README.md strata-basics/README.md)
  local readme matched
  printf '\n## the demo command, documented\n# required line: %s\n' "$GATE6_DEMO_COMMAND" >>"$EV"
  for readme in "${documented_in[@]}"; do
    if ! require_file "$readme" "documentation of the demo command"; then
      failed=1
      printf '# MISSING: %s\n' "$readme" >>"$EV"
      continue
    fi
    matched="$(grep -nF -- "$GATE6_DEMO_COMMAND" "$readme" || true)"
    {
      printf '# %s:\n' "$readme"
      if [[ -n "$matched" ]]; then
        printf '%s\n' "$matched"
      else
        printf '#   (the command does not appear in this file)\n'
      fi
    } >>"$EV"
    if [[ -z "$matched" ]]; then
      detail "$readme does not document the demo command $GATE6_DEMO_COMMAND"
      failed=1
    fi
  done

  if [[ "$failed" -ne 0 ]]; then
    add_appendix "Gate 6 - demo output (row failed)" <"$output"
  else
    detail "the demo ran, printing $json_hits JSON amount fragment(s) and $date_hits date(s); both READMEs document the command"
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

  # -- the six sections, identified rather than counted --------------------
  #
  # Six headings of any kind is not the condition: section 0.8.3 requires the
  # six NAMED sections (a) to (f), each present once and in order, so each is
  # looked for by its own label. A note with six headings and no section (d)
  # would otherwise pass while the covered-calendar set went undocumented.
  local headings heading_count
  headings="$(awk '/^## / { print }' "$note")"
  heading_count="$(printf '%s\n' "$headings" | awk 'NF { n++ } END { print n + 0 }')"
  {
    printf '# command: grep -c "^## " %s\n' "$note"
    printf '%s\n' "$heading_count"
    printf '# headings:\n%s\n\n' "$headings"
    printf '# the six required sections of section 0.8.3:\n'
  } >>"$EV"

  local label previous_line=0 required_found=0 out_of_order=0
  for label in a b c d e f; do
    local occurrences line_number
    occurrences="$(grep -c "^## ($label)" "$note" || true)"
    line_number="$(grep -n "^## ($label)" "$note" | head -n 1 | cut -d: -f1 || true)"
    {
      printf '#   (%s): %s occurrence(s)' "$label" "$occurrences"
      if [[ -n "$line_number" ]]; then
        printf ' at line %s -- %s' "$line_number" "$(sed -n "${line_number}p" "$note")"
      fi
      printf '\n'
    } >>"$EV"
    if [[ "$occurrences" -eq 0 ]]; then
      detail "$note has no '## ($label)' section"
      failed=1
      continue
    fi
    if [[ "$occurrences" -gt 1 ]]; then
      detail "$note has $occurrences '## ($label)' sections, expected exactly one"
      failed=1
    fi
    required_found=$((required_found + 1))
    if [[ "$line_number" -le "$previous_line" ]]; then
      out_of_order=$((out_of_order + 1))
    fi
    previous_line="$line_number"
  done
  if [[ "$out_of_order" -ne 0 ]]; then
    detail "the six sections of $note are not in the order (a) to (f)"
    failed=1
  fi
  printf '\n' >>"$EV"

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
    printf 'headings (## )                     : %s\n' "$heading_count"
    printf 'required sections (a)-(f) found    : %s of 6, in order\n' "$required_found"
    printf 'distinct collect members referenced: %s\n' "$member_count"
    printf 'section (a) member table rows      : %s\n' "$rows"
  } | add_appendix "Gate 7 - migration note content checks"

  if [[ "$failed" -eq 0 ]]; then
    detail "$note present, sections (a)-(f) all present and in order, $rows table rows >= $member_count referenced members"
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
# The decisive measurement is an identity, not a containment test. The Java
# sources and the two class inventories AAP sections 0.4.1 and 0.2.2 name are
# the authority; the manifest is the audited document and never supplies its
# own expectations. Enforced:
#   * the class set is exactly the 72 basics *Test.java classes found under
#     modules/basics/src/test/java, the 20 collect classes section 0.4.1 maps
#     and the 4 collect exception classes section 0.2.2 drops with their
#     subjects - so a whole class left out of the manifest is caught, which
#     reading the expected set out of the manifest could never do;
#   * every @Test/@ParameterizedTest method of each of those classes has
#     exactly one row, keyed the way the manifest keys it - by bare name, or
#     by `name(Type;Type)` where the class overloads that name, so
#     DecimalTest's two testValuesOfBigDecimal methods remain two rows;
#   * the row count is the derived 1223 + 643 + 10 = 1876, and no source key
#     or whole row repeats, so a removed row cannot pass;
#   * every `ported` or `consolidated:<spec>` row names a <testcase> that
#     exists in the JUnit XML, matched on suite class and test name, with the
#     `consolidated:` suffix equal to the row's own scala_spec; a `partial:`
#     or `dropped:` row names no Scala target at all;
#   * `partial:` is permitted only for the pinned (class, method) pairs of
#     GuavateTest and MapStreamTest whose subject members are not ported, so
#     the thirty-four that are ported keep their real test cases;
#   * `dropped:` is permitted only for the five test classes section 0.2.2
#     excludes, and each of those five must be dropped whole;
#   * the JUnit XML is checked for duplicated evidence first - exactly one
#     <testsuite> element per file wherever it sits, one occurrence of each
#     suite and of each (classname, name) pair, and a `tests` attribute that
#     agrees with the suite's own <testcase> elements - and only then are the
#     per-module counts summed from it: strata-basics >= 1223 and
#     strata-collect >= 491, the secondary signals of section 0.10.1, which
#     are reported alongside the traceability result and enforced.
#
# Vacuity guards: the Java enumeration must find exactly the 72 classes and
# 1223 methods of basics, the 643 methods of the twenty mapped collect classes
# and the 10 of the four dropped ones. A different number means this scan is
# broken, not that the manifest is complete, and the message says which side
# to look at.
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
# The two per-module run counts AAP section 0.10.1 calls secondary signals.
# Secondary means "reported alongside the decisive traceability result", not
# advisory: a threshold the specification states is enforced here.
BASICS_FLOOR = 1223
COLLECT_FLOOR = 491
# The exact Java inventory AAP section 0.10.1 counted, and the row identity it
# derives from it: 1223 basics methods + 643 methods across the twenty mapped
# collect classes + the 10 methods of the four dropped collect exception
# classes = 1876 manifest rows, one row per Java test method. These are
# equalities, not floors: `modules/**` is frozen (row 20 proves it), so a
# different number means either the manifest drifted or this scan is broken,
# and the message says which side to look at.
BASICS_CLASS_COUNT = 72
BASICS_METHODS = 1223
MAPPED_COLLECT_METHODS = 643
DROPPED_COLLECT_METHODS = 10
MANIFEST_ROWS = 1876
HEADER = ["java_test_class", "java_test_method", "scala_spec", "scala_test_name", "status"]

# The twenty collect test classes AAP section 0.4.1 maps into the Scala port,
# named here so the authoritative inventory is this reviewed script and never
# the manifest under audit: deriving the expected set from the audited file
# would let an omitted class hide every one of its methods.
MAPPED_COLLECT_CLASSES = frozenset({
    "com.opengamma.strata.collect.ArgCheckerTest",
    "com.opengamma.strata.collect.DecimalTest",
    "com.opengamma.strata.collect.DoubleArrayMathTest",
    "com.opengamma.strata.collect.FixedScaleDecimalTest",
    "com.opengamma.strata.collect.GuavateTest",
    "com.opengamma.strata.collect.MapStreamTest",
    "com.opengamma.strata.collect.TestHelperTest",
    "com.opengamma.strata.collect.TypedStringTest",
    "com.opengamma.strata.collect.array.DoubleArrayTest",
    "com.opengamma.strata.collect.array.DoubleMatrixTest",
    "com.opengamma.strata.collect.io.ResourceLocatorTest",
    "com.opengamma.strata.collect.named.CombinedExtendedEnumTest",
    "com.opengamma.strata.collect.named.EnumNamesTest",
    "com.opengamma.strata.collect.named.ExtendedEnumTest",
    "com.opengamma.strata.collect.named.NamedTest",
    "com.opengamma.strata.collect.result.FailureItemTest",
    "com.opengamma.strata.collect.result.FailureItemsTest",
    "com.opengamma.strata.collect.result.FailureReasonTest",
    "com.opengamma.strata.collect.result.ResultTest",
    "com.opengamma.strata.collect.result.ValueWithFailuresTest",
})
# The four collect exception-test classes AAP section 0.2.2 drops with their
# subjects. They are mapped only as `dropped:` rows, which is why their methods
# are counted apart from the 643.
DROPPED_COLLECT_CLASSES = frozenset({
    "com.opengamma.strata.collect.result.FailureExceptionTest",
    "com.opengamma.strata.collect.result.FailureItemExceptionTest",
    "com.opengamma.strata.collect.result.IllegalArgFailureExceptionTest",
    "com.opengamma.strata.collect.result.ParseFailureExceptionTest",
})
# `dropped:` is permitted for exactly the five test classes AAP section 0.2.2
# excludes, and only when the WHOLE class is dropped. A class-wide exception
# for a class that keeps any method would let a single dropped row retire a
# retained Java test method without a Scala counterpart, which is the one thing
# this row exists to catch. The legacy `ImmutableHolidayCalendar-Old.json`
# fixture is not such a class: section 0.2.2 drops the fixture, while
# ImmutableHolidayCalendarTest itself is retained by section 0.4.1, so its
# methods must name real Scala test cases.
DROPPED_CLASSES = DROPPED_COLLECT_CLASSES | frozenset({
    "com.opengamma.strata.basics.date.HolidayCalendarIniLookupTest",
})
# `partial:` is permitted per (class, method) pair, never per class. Both
# classes keep real coverage - the thirty-four Guavate/MapStream methods whose
# subjects are ported into collect `Collections.scala` map into
# `CollectionsSpec` - so a class-wide exception would let any of those thirty-
# four be retired to `partial` and its lost coverage pass unnoticed. Pinning
# the exact pairs whose subjects are NOT ported (AAP sections 0.2.2 and 0.4.1:
# the Guavate and MapStream members `strata-basics` does not use) closes it in
# both directions: a pair outside this list cannot be excused, and each of the
# thirty-four in the complement has no status left but `ported` or
# `consolidated:<spec>`, both of which must join to a JUnit test case below.
# Every pin is checked against the Java sources further down, so a stale entry
# cannot widen the permission either.
PARTIAL_ALLOWED = {
    # GuavateTest: 71 method(s) whose subject member has no Scala counterpart
    "com.opengamma.strata.collect.GuavateTest": frozenset({
        "test_boxed", "test_callerClass", "test_casting", "test_combineFuturesAsList",
        "test_combineFuturesAsList_Void", "test_combineFuturesAsList_Void_exception",
        "test_combineFuturesAsList_exception", "test_combineFuturesAsMap",
        "test_combineFuturesAsMap_exception", "test_combineListMultimaps",
        "test_combineMaps", "test_combineMapsOverwriting",
        "test_combineMapsOverwriting_entries", "test_combineMaps_differentTypes",
        "test_combineMaps_merge", "test_combineMaps_mergeDifferentTypes",
        "test_concatItemsToListItems", "test_concatToList",
        "test_concatToList_differentTypes", "test_concatToSet",
        "test_concatToSet_differentTypes", "test_entry", "test_filtering", "test_first",
        "test_firstNonEmpty_optionalMatch1", "test_firstNonEmpty_optionalMatch2",
        "test_firstNonEmpty_optionalMatchNone", "test_firstNonEmpty_supplierMatch1",
        "test_firstNonEmpty_supplierMatch2", "test_firstNonEmpty_supplierMatchNone",
        "test_genericClass", "test_inNullable_null", "test_inNullable_present",
        "test_inTryCatchIgnore_exception", "test_inTryCatchIgnore_null",
        "test_inTryCatchIgnore_present", "test_in_Stream",
        "test_mapEntriesToImmutableMap", "test_mapEntriesToImmutableMap_mergeFn",
        "test_namedThreadFactory", "test_namedThreadFactory_prefix", "test_not_Predicate",
        "test_only", "test_pairsToImmutableMap", "test_poll", "test_poll_exception",
        "test_set", "test_splittingBySize", "test_substring", "test_toCombinedFuture",
        "test_toCombinedFutureMap", "test_toCombinedFuture_Void",
        "test_toImmutableListMultimap_key", "test_toImmutableListMultimap_keyValue",
        "test_toImmutableMap_key", "test_toImmutableMap_keyValue",
        "test_toImmutableMap_keyValue_duplicateKeys",
        "test_toImmutableMap_key_duplicateKeys", "test_toImmutableMap_mergeFn",
        "test_toImmutableMultiset", "test_toImmutableSetMultimap_key",
        "test_toImmutableSetMultimap_keyValue", "test_toOnly", "test_toOptional",
        "test_validUtilityClass", "test_zip", "test_zipWithIndex",
        "test_zipWithIndex_empty", "test_zip_empty", "test_zip_firstLonger",
        "test_zip_secondLonger",
    }),
    # MapStreamTest: 47 method(s) whose subject member has no Scala counterpart
    "com.opengamma.strata.collect.MapStreamTest": frozenset({
        "allMatch", "anyMatch", "concat", "concatGeneric", "concatNumberValues",
        "coverage", "filter", "filterValues", "filterValues_byClass", "flatMap",
        "flatMapBoth", "flatMapKeysAndValuesToKeys", "flatMapKeysAndValuesToValues",
        "flatMapKeysToKeys", "flatMapToDouble", "flatMapToInt", "flatMapValuesToValues",
        "forEach", "inverse", "keys", "keys_fromList", "keys_fromStream", "map",
        "mapBoth", "mapKeysAndValuesToKeys", "mapKeysAndValuesToValues", "mapKeysToKeys",
        "mapToDouble", "mapToInt", "maxKeys", "maxValues", "minKeys", "minValues",
        "noneMatch", "sortedKeys", "sortedKeys_comparator", "sortedValues",
        "sortedValues_comparator", "toListMultimap", "toSetMultimap", "values",
        "values_fromList", "values_fromStream", "zip", "zipWithIndex", "zip_longerFirst",
        "zip_longerSecond",
    }),
}

# Status grammar. The vocabulary is closed and every value below is matched
# WHOLE (`fullmatch`), so `ported:anything` is not a ported row: `ported`
# carries no suffix, `consolidated:` names the very spec the row's scala_spec
# field names, and `partial:`/`dropped:` carry a bounded, non-empty reason of
# letters, digits and hyphens - no colon, comma or whitespace in it.
SPEC_FQCN = (r"com\.opengamma\.strata\.(?:basics|collect)"
             r"(?:\.[a-z][A-Za-z0-9]*)*\.[A-Z][A-Za-z0-9]*Spec")
REASON = r"[A-Za-z0-9][A-Za-z0-9-]{3,119}"
STATUS_PORTED = re.compile(r"ported")
STATUS_CONSOLIDATED = re.compile(r"consolidated:(" + SPEC_FQCN + r")")
STATUS_PARTIAL = re.compile(r"partial:(" + REASON + r")")
STATUS_DROPPED = re.compile(r"dropped:(" + REASON + r")")

problems = []
lines = []


def note(text):
    lines.append(text)


def problem(text):
    problems.append(text)


# -- the JUnit XML snapshot --------------------------------------------------
# ScalaTest's `-u` reporter writes one file per suite, named TEST-<suite>.xml,
# whose `tests` attribute counts the <testcase> elements it contains. This row
# depends on that identity, so it verifies it instead of assuming it: every
# suite occurrence and every test case is recorded together with the file it
# came from, a suite reported twice - in two files, or twice in one file, at
# the root or nested inside another suite - is a duplicate report rather than
# twice as many tests, a repeated (classname, name) pair is an integrity
# failure rather than one silent set entry, and a `tests` attribute that
# disagrees with the suite's own <testcase> elements is rejected. Both module
# counts are then summed over that validated inventory, so duplicated evidence
# cannot inflate a floor while still looking unique to the row join.
def own_testcases(suite):
    """The <testcase> elements of this suite, excluding a nested suite's own.

    Counting a nested suite's cases against its parent would let a second
    occurrence of a suite hide inside the first: the parent's `tests`
    attribute could be raised to cover both and still match what it holds.
    """
    found = []
    for child in suite:
        if child.tag == "testsuite":
            continue
        if child.tag == "testcase":
            found.append(child)
        else:
            found.extend(own_testcases(child))
    return found


suite_sources = {}
suite_counts = {}
case_sources = {}
xml_files = sorted(glob.glob(os.path.join(xml_dir, "TEST-*.xml")))
for path in xml_files:
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError as error:
        problem(f"{path}: not parseable XML: {error}")
        continue
    # `iter` yields the root element itself when it is a <testsuite>, so a
    # suite nested inside another one is enumerated rather than hidden by it.
    suites = list(root.iter("testsuite"))
    one_suite = len(suites) == 1
    if not one_suite:
        problem(f"{path}: holds {len(suites)} <testsuite> element(s), expected exactly one: "
                f"one report file describes one suite, so this file's tests are not counted")
    for suite in suites:
        name = suite.attrib.get("name", "")
        if os.path.basename(path) != f"TEST-{name}.xml":
            problem(f"{path}: reports suite {name!r}, whose own report file is "
                    f"TEST-{name}.xml - two files describing one suite double its count")
        suite_sources.setdefault(name, []).append(path)
        cases = own_testcases(suite)
        raw = suite.attrib.get("tests", "")
        try:
            count = int(raw)
        except ValueError:
            problem(f"{path}: suite {name!r} has a non-numeric tests attribute {raw!r}")
            count = 0
        else:
            if count != len(cases):
                problem(f"{path}: suite {name!r} declares tests={count} but holds "
                        f"{len(cases)} <testcase> element(s) of its own")
        # A file that does not describe exactly one suite contributes no count
        # at all: aggregating it is how a repeated suite inflates a floor.
        if one_suite:
            suite_counts.setdefault(name, []).append(count)
        for case in cases:
            pair = (case.attrib.get("classname", ""), case.attrib.get("name", ""))
            case_sources.setdefault(pair, []).append(path)

duplicate_suites = sorted(name for name, paths in suite_sources.items() if len(paths) > 1)
duplicate_cases = sorted(pair for pair, paths in case_sources.items() if len(paths) > 1)
# One count per suite, read from its single validated occurrence, so a
# duplicated report contributes once even while it is being reported as a
# failure below.
suite_tests = {name: counts[0] for name, counts in suite_counts.items()}
test_cases = set(case_sources)

basics_tests = sum(n for s, n in suite_tests.items() if s.startswith(BASICS_PREFIX))
collect_tests = sum(n for s, n in suite_tests.items() if s.startswith(COLLECT_PREFIX))
other = sorted(s for s in suite_tests
               if not s.startswith(BASICS_PREFIX) and not s.startswith(COLLECT_PREFIX))

note(f"JUnit XML files read          : {len(xml_files)} (from {xml_dir})")
note(f"suite occurrences             : {sum(len(p) for p in suite_sources.values())} "
     f"across {len(suite_sources)} suite(s) (one occurrence each)")
note(f"duplicate suite reports       : {len(duplicate_suites)} (0 required)")
note(f"test cases                    : {len(test_cases)} distinct (classname, name) pairs")
note(f"duplicate test case pairs     : {len(duplicate_cases)} (0 required)")
note(f"strata-basics tests           : {basics_tests} (floor {BASICS_FLOOR}, secondary signal)")
note(f"strata-collect tests          : {collect_tests} (floor {COLLECT_FLOOR}, secondary signal)")
if other:
    note(f"suites outside both packages  : {other}")

if not xml_files:
    problem(f"no TEST-*.xml found in {xml_dir}: Gate 1 produced no report to read")
for name in duplicate_suites:
    problem(f"suite {name} is reported {len(suite_sources[name])} times in "
            f"{sorted(set(suite_sources[name]))}: its tests would be counted more than once")
for pair in duplicate_cases:
    problem(f"test case {pair[0]} / {pair[1]!r} appears {len(case_sources[pair])} times "
            f"in {sorted(set(case_sources[pair]))}")
if basics_tests < BASICS_FLOOR:
    problem(f"strata-basics ran {basics_tests} tests, below the floor of {BASICS_FLOOR}")
if collect_tests < COLLECT_FLOOR:
    problem(f"strata-collect ran {collect_tests} tests, below the floor of {COLLECT_FLOOR}")


# -- the Java test sources ---------------------------------------------------
# The manifest keys a Java test method by its bare name when that name is
# unique within its class, and by `name(Type;Type)` - source-level simple type
# names, semicolon-separated, empty parentheses for a no-argument overload -
# when it is not. DecimalTest is the one class in the mapped inventory that
# needs the signature form: it declares testValuesOfBigDecimal twice, once
# @ParameterizedTest over (String, long, int) and once @Test with no
# arguments. Keying by bare name would collapse those two rows into one, so a
# removed row would still pass. This parser therefore reads the parameter list
# and reproduces the manifest's own key syntax, which makes the comparison
# below an exact set equality rather than a containment test.
ANNOTATION = re.compile(r"@(?:Test|ParameterizedTest)\b")
DECLARATION = re.compile(
    r"""(?:@\w+(?:\s*\([^()]*\))?\s*|public\s+|protected\s+|private\s+|static\s+
        |final\s+|synchronized\s+|default\s+|<[^<>]*>\s*)*
        [\w.$]+(?:\s*<[^;{}]*?>)?(?:\s*\[\s*\])*\s+(?P<name>\w+)\s*\(""",
    re.X)


def blank_noncode(source):
    """Blank comments and string literals, preserving every offset.

    An `@Test` inside a comment or a string is not an annotation; blanking
    rather than deleting keeps the offsets of the real declarations, so the
    declaration search below can still read forward from the annotation.
    """
    out = list(source)
    index = len(source)
    position = 0
    while position < index:
        char = source[position]
        if char == "/" and source[position:position + 2] == "//":
            end = source.find("\n", position)
            end = index if end < 0 else end
        elif char == "/" and source[position:position + 2] == "/*":
            end = source.find("*/", position + 2)
            end = index if end < 0 else end + 2
        elif char == '"' and source[position:position + 3] == '"""':
            end = source.find('"""', position + 3)
            end = index if end < 0 else end + 3
        elif char in "\"'":
            end = position + 1
            while end < index:
                if source[end] == "\\":
                    end += 2
                    continue
                if source[end] == char or source[end] == "\n":
                    end += 1
                    break
                end += 1
        else:
            position += 1
            continue
        for blank in range(position, min(end, index)):
            if source[blank] != "\n":
                out[blank] = " "
        position = max(end, position + 1)
    return "".join(out)


def parameter_text(source, open_paren):
    """The text between `open_paren` and its matching close parenthesis."""
    depth = 0
    position = open_paren
    while position < len(source):
        if source[position] == "(":
            depth += 1
        elif source[position] == ")":
            depth -= 1
            if depth == 0:
                return source[open_paren + 1:position]
        position += 1
    return ""


def split_parameters(text):
    """Split a parameter list on its top-level commas."""
    parts = []
    current = []
    depth = 0
    for char in text:
        if char in "<([":
            depth += 1
        elif char in ">)]":
            depth -= 1
        if char == "," and depth == 0:
            parts.append("".join(current))
            current = []
        else:
            current.append(char)
    parts.append("".join(current))
    return [part.strip() for part in parts if part.strip()]


def simple_type(parameter):
    """The manifest's simple-name form of one declared parameter type."""
    text = re.sub(r"@\w+(?:\s*\([^()]*\))?", " ", parameter)
    text = re.sub(r"\bfinal\b", " ", text)
    without_name = re.sub(r"\s+\w+\s*$", "", text).strip()
    text = without_name or text.strip()
    arrays = text.count("[") + (1 if "..." in text else 0)
    text = text.replace("...", " ").replace("[", " ").replace("]", " ")
    while "<" in text:
        reduced = re.sub(r"<[^<>]*>", "", text)
        if reduced == text:
            text = text.split("<")[0]
            break
        text = reduced
    words = text.split()
    head = words[0] if words else text.strip()
    return head.split(".")[-1] + "[]" * arrays


def enumerate_java(tree):
    """{fqcn: {method keys}} for every *Test.java under `tree`."""
    found = {}
    for directory, _, files in os.walk(tree):
        for name in sorted(files):
            if not name.endswith("Test.java"):
                continue
            path = os.path.join(directory, name)
            with open(path, encoding="utf-8") as handle:
                source = blank_noncode(handle.read())
            package = re.search(r"^package\s+([\w.]+);", source, re.M)
            if package is None:
                problem(f"{path}: no package declaration")
                continue
            fqcn = f"{package.group(1)}.{name[:-5]}"
            declared = []
            for match in ANNOTATION.finditer(source):
                declaration = DECLARATION.search(source, match.end())
                if declaration is None:
                    problem(f"{path}: the @Test annotation at offset {match.start()} "
                            f"is followed by no method declaration")
                    continue
                types = [simple_type(parameter) for parameter
                         in split_parameters(parameter_text(source, declaration.end() - 1))]
                declared.append((declaration.group("name"), tuple(types)))
            names = {}
            for method, _ in declared:
                names[method] = names.get(method, 0) + 1
            keys = set()
            for method, types in declared:
                key = method if names[method] == 1 else f"{method}({';'.join(types)})"
                if key in keys:
                    problem(f"{path}: two @Test methods share the key {key}")
                keys.add(key)
            found[fqcn] = keys
    return found


java_basics = enumerate_java(basics_java)
java_collect = enumerate_java(collect_java)

basics_methods = sum(len(keys) for keys in java_basics.values())
mapped_collect_methods = sum(len(java_collect.get(c, ())) for c in MAPPED_COLLECT_CLASSES)
dropped_collect_methods = sum(len(java_collect.get(c, ())) for c in DROPPED_COLLECT_CLASSES)

note(f"Java basics test classes      : {len(java_basics)} ({BASICS_CLASS_COUNT} required)")
note(f"Java basics test methods      : {basics_methods} ({BASICS_METHODS} required)")
note(f"mapped collect test classes   : {len(MAPPED_COLLECT_CLASSES)} (pinned, AAP 0.4.1)")
note(f"their Java test methods       : {mapped_collect_methods} "
     f"({MAPPED_COLLECT_METHODS} required)")
note(f"dropped collect test classes  : {len(DROPPED_COLLECT_CLASSES)} (pinned, AAP 0.2.2)")
note(f"their Java test methods       : {dropped_collect_methods} "
     f"({DROPPED_COLLECT_METHODS} required)")

for fqcn in sorted(MAPPED_COLLECT_CLASSES | DROPPED_COLLECT_CLASSES):
    if fqcn not in java_collect:
        problem(f"{fqcn} is named by the AAP but has no *Test.java under {collect_java}")
if len(java_basics) != BASICS_CLASS_COUNT:
    problem(f"the Java scan found {len(java_basics)} basics test classes under {basics_java}, "
            f"not the {BASICS_CLASS_COUNT} the specification counted: the scan is broken")
if basics_methods != BASICS_METHODS:
    problem(f"the Java scan found {basics_methods} basics test methods, not the "
            f"{BASICS_METHODS} the specification counted: the scan is broken, "
            f"not the manifest")
if mapped_collect_methods != MAPPED_COLLECT_METHODS:
    problem(f"the Java scan found {mapped_collect_methods} methods across the twenty mapped "
            f"collect classes, not the {MAPPED_COLLECT_METHODS} the specification counted")
if dropped_collect_methods != DROPPED_COLLECT_METHODS:
    problem(f"the Java scan found {dropped_collect_methods} methods across the four dropped "
            f"collect classes, not the {DROPPED_COLLECT_METHODS} the specification counted")

# The class inventory the manifest must match exactly, built from the Java
# tree and the two pinned lists - never from the manifest.
expected_classes = set(java_basics) | set(MAPPED_COLLECT_CLASSES) | set(DROPPED_COLLECT_CLASSES)
expected_keys = {fqcn: set(java_basics[fqcn]) for fqcn in java_basics}
for fqcn in MAPPED_COLLECT_CLASSES | DROPPED_COLLECT_CLASSES:
    expected_keys[fqcn] = set(java_collect.get(fqcn, ()))
expected_rows = basics_methods + mapped_collect_methods + dropped_collect_methods


# -- the manifest ------------------------------------------------------------
# Read once, then measured against the inventories above: the class set and
# every method key must match exactly in both directions, no source key may
# appear twice, and the row count is the derived one-row-per-method identity
# rather than a floor. A target (scala_spec, scala_test_name) pair may be
# named by several rows - that is what a documented consolidation is - so only
# the Java side of a row is required to be unique.
with open(manifest, newline="", encoding="utf-8") as handle:
    reader = csv.reader(handle)
    header = next(reader, [])
    rows = [(index, row) for index, row in enumerate(reader, start=2) if row]

if header != HEADER:
    problem(f"manifest header is {header}, expected {HEADER}")

row_keys = {}
whole_rows = {}
mapped = {}
statuses = {}
joined = 0
partial_rows = 0
dropped_rows = 0

for index, row in rows:
    if len(row) != len(HEADER):
        problem(f"{manifest}:{index}: {len(row)} field(s), expected {len(HEADER)}")
        continue
    java_class, java_method, spec, test_name, status = row
    row_keys.setdefault((java_class, java_method), []).append(index)
    whole_rows.setdefault(tuple(row), []).append(index)

    consolidated = STATUS_CONSOLIDATED.fullmatch(status)
    if STATUS_PORTED.fullmatch(status):
        kind = "ported"
    elif consolidated:
        kind = "consolidated"
    elif STATUS_PARTIAL.fullmatch(status):
        kind = "partial"
    elif STATUS_DROPPED.fullmatch(status):
        kind = "dropped"
    else:
        problem(f"{manifest}:{index}: status {status!r} is outside the vocabulary "
                f"`ported`, `consolidated:<spec fqcn>`, `partial:<reason>`, "
                f"`dropped:<reason>`, where a reason is 4 to 120 characters of "
                f"letters, digits and hyphens")
        kind = None
    if kind is not None:
        statuses[kind] = statuses.get(kind, 0) + 1
        mapped.setdefault(java_class, {})[java_method] = kind

    if java_class not in expected_classes:
        problem(f"{manifest}:{index}: {java_class} is outside the pinned inventory of "
                f"{BASICS_CLASS_COUNT} basics, {len(MAPPED_COLLECT_CLASSES)} mapped collect and "
                f"{len(DROPPED_COLLECT_CLASSES)} dropped collect test classes")
    elif java_method not in expected_keys[java_class]:
        problem(f"{manifest}:{index}: {java_class} declares no @Test method {java_method}")

    if kind in ("ported", "consolidated"):
        if consolidated and consolidated.group(1) != spec:
            problem(f"{manifest}:{index}: the status consolidates into "
                    f"{consolidated.group(1)} while scala_spec names {spec!r}")
        if not spec or not test_name:
            problem(f"{manifest}:{index}: a {kind} row must name both a spec and a test")
        elif (spec, test_name) not in test_cases:
            problem(f"{manifest}:{index}: no test case {spec} / {test_name!r} in the JUnit "
                    f"XML (Java {java_class}.{java_method})")
        else:
            joined += 1
    elif kind in ("partial", "dropped"):
        if spec or test_name:
            problem(f"{manifest}:{index}: a {kind} row names no Scala test, so scala_spec "
                    f"and scala_test_name must be empty, not {spec!r} / {test_name!r}")
        if kind == "partial":
            partial_rows += 1
            if java_method not in PARTIAL_ALLOWED.get(java_class, frozenset()):
                problem(f"{manifest}:{index}: {java_class}.{java_method} is not one of the "
                        f"{sum(len(v) for v in PARTIAL_ALLOWED.values())} pinned "
                        f"(class, method) pairs `partial:` may excuse")
        else:
            dropped_rows += 1
            if java_class not in DROPPED_CLASSES:
                problem(f"{manifest}:{index}: {java_class} is not one of the "
                        f"{len(DROPPED_CLASSES)} wholly excluded AAP 0.2.2 test classes, so "
                        f"{java_method} cannot be dropped - it must name a Scala test case")

note(f"manifest rows                 : {len(rows)} ({expected_rows} required = "
     f"{basics_methods} + {mapped_collect_methods} + {dropped_collect_methods})")
note(f"manifest classes              : {len(mapped)} ({len(expected_classes)} required)")
note("manifest statuses             : " +
     ", ".join(f"{k}={v}" for k, v in sorted(statuses.items())))
note(f"rows joined to a test case    : {joined} "
     f"({statuses.get('ported', 0) + statuses.get('consolidated', 0)} required)")
note(f"partial rows                  : {partial_rows} of "
     f"{sum(len(v) for v in PARTIAL_ALLOWED.values())} pinned pairs")
note(f"dropped rows                  : {dropped_rows} across "
     f"{len(DROPPED_CLASSES)} wholly excluded classes")

if expected_rows != MANIFEST_ROWS:
    problem(f"the Java inventory derives {expected_rows} rows, not the {MANIFEST_ROWS} the "
            f"specification states: the enumeration and the specification disagree")
if len(rows) != expected_rows:
    problem(f"the manifest has {len(rows)} data rows, not the {expected_rows} its one-row-"
            f"per-method contract derives from the Java sources")

duplicate_keys = sorted(key for key, indices in row_keys.items() if len(indices) > 1)
for key in duplicate_keys:
    problem(f"{manifest}: {key[0]}.{key[1]} is mapped by {len(row_keys[key])} rows "
            f"{row_keys[key]}: one row per Java test method")
duplicate_full = sorted(indices for whole, indices in whole_rows.items() if len(indices) > 1)
for indices in duplicate_full:
    problem(f"{manifest}: rows {indices} are identical")

missing_classes = sorted(expected_classes - set(mapped))
extra_classes = sorted(set(mapped) - expected_classes)
for fqcn in missing_classes:
    problem(f"{manifest}: {fqcn} has no row at all, so none of its "
            f"{len(expected_keys.get(fqcn, ()))} test method(s) is traced")
for fqcn in extra_classes:
    problem(f"{manifest}: {fqcn} is mapped but is not in the pinned class inventory")

unmapped = []
for fqcn in sorted(expected_classes):
    for key in sorted(expected_keys.get(fqcn, ()) - set(mapped.get(fqcn, {}))):
        unmapped.append(f"{fqcn}.{key}")
note(f"unmapped Java test methods    : {len(unmapped)} (0 required)")
if unmapped:
    problem(f"{len(unmapped)} Java test method(s) have no manifest row")
    problems.extend(f"    {entry}" for entry in unmapped[:20])

# A class-level exclusion is only sound when the whole class is excluded: any
# retained method of it must still name a Scala test case.
for fqcn in sorted(DROPPED_CLASSES):
    kinds = mapped.get(fqcn, {})
    not_dropped = sorted(key for key, kind in kinds.items() if kind != "dropped")
    if not_dropped:
        problem(f"{manifest}: {fqcn} is a wholly excluded class, but "
                f"{len(not_dropped)} of its rows are not dropped: {not_dropped[:5]}")

# A pinned pair that no longer exists in the Java sources would silently widen
# the `partial:` permission, so the pins are checked against the inventory.
for fqcn in sorted(PARTIAL_ALLOWED):
    if fqcn not in MAPPED_COLLECT_CLASSES:
        problem(f"the pinned partial list names {fqcn}, which is not one of the twenty "
                f"mapped collect test classes")
        continue
    for key in sorted(PARTIAL_ALLOWED[fqcn] - expected_keys.get(fqcn, set())):
        problem(f"the pinned partial list names {fqcn}.{key}, which the Java source "
                f"no longer declares")

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
    detail "one row per Java test method across the pinned class inventory, every ported and consolidated row joined to a test case, no duplicated row or report, both counts above their floors"
  fi
  return "$failed"
}

#=============================================================================
# Row 20 - Repository boundary, run last, with the publication-artifact
#          secret scan.
#
#   git status --porcelain -- modules examples eclipse pom.xml src .github
#
# must be empty: the Maven tree and the repository governance files are
# unchanged. Running it last also catches any accidental write by this script,
# which is why every artifact it produces lives under target/.
#
# THE SECRET SCAN, AND WHY IT IS HERE
#   CI publishes four trees from this run - `target/gate-report.md`,
#   `target/parity-report`, `target/test-reports` and `target/audit` - with
#   `when: always`, and the audit tree holds every sbt log, every class-load
#   log and every row's evidence. Publishing an artifact is a disclosure, so
#   the run checks its own output for credential signatures BEFORE it leaves
#   the machine. This is the last row, so by the time it executes every other
#   row has finished writing.
#
#   It is not the only time the scan runs, because a run that ends early
#   publishes just as much:
#
#     pre-run          before the first row, over whatever an earlier run
#                      left behind. A finding there stops the run before it
#                      adds to those trees, and a clean tree with nothing in
#                      it is not a failure.
#     publication      this row: all four paths, with the three trees the
#                      run produces required to exist.
#     composed-report  after the report is written, over the actual bytes of
#                      `target/gate-report.md` - header, rows and result line
#                      included. A finding cannot be written into a report
#                      that already exists, so it is stated on stderr, kept
#                      in its own summary under the audit tree (which CI also
#                      publishes) and counted, so the run exits non-zero.
#
#   The report's content is therefore covered twice: as material, before it
#   is composed - the header fields, the rows and the counts its result line
#   comes from are written to `target/audit/gate-report-material.txt`, its
#   appendices already being a file in the audit tree - and as the file
#   itself, afterwards.
#
#   The scan is fail-closed: a file it cannot read, a walk it cannot
#   complete, or a symlink it will not follow fails the row rather than being
#   skipped. Nothing inside these trees is built through a symlink, and a
#   symlink is the one way an artifact can publish content from outside the
#   tree, so every one found is refused and named. The scan is binary-safe -
#   bytes are decoded permissively rather than assumed to be UTF-8 - and its
#   findings are reported REDACTED: the rule, the file and the line, never the
#   value, because the report naming a secret would publish it.
#
#   Quoted keys and quoted values are matched as well as bare ones, because
#   two of these trees are JSON: `"password": "..."` is the same disclosure
#   as `password=...`.
#
#   Two allowlists, both explicit, both EXACT and both counted in the
#   evidence: a value that IS the sbt distribution checksum
#   `.circleci/config.yml` publishes (read from that file, never hardcoded
#   here), and a value that IS a hexadecimal digest on a line that says what
#   it is - `CODEC-AUDIT-DIGEST`, `sha256`, `checksum`. A value that merely
#   contains one is not excused. Nothing else is, and every excused match is
#   listed.
#=============================================================================

# The four trees CI publishes from a run, in the order .circleci/config.yml
# stores them.
PUBLICATION_PATHS=(
  "target/gate-report.md"
  "target/parity-report"
  "target/test-reports"
  "target/audit"
)

# The scan, as a function, because it is called from more than one place:
# before the first row (so that whatever a previous run left behind is
# checked even if this run aborts early), at the boundary row (the enforced
# invocation, over all four trees), and once more over the gate report after
# it has been composed - the one artifact that does not exist while the rows
# are still running.
#
# Arguments:
#   $1  a phase label, recorded in the summary
#   $2  `require-paths` when every path must already exist, `allow-missing`
#       when a path this phase has not produced yet is not an error
#   $3  `require-files` when at least one artifact must have been read,
#       `allow-empty` when an empty tree is legitimate for this phase
#   $4  the summary file to write
#   $5.. the paths to scan
#
# Prints the summary it writes; returns non-zero when the scan did not pass.
scan_publication_artifacts() {
  local phase="$1" paths_mode="$2" files_mode="$3" summary="$4"
  shift 4
  # The one checksum the repository publishes on purpose, read from the file
  # that publishes it so that this allowlist cannot drift from it.
  local published_checksum=""
  if [[ -f .circleci/config.yml ]]; then
    # `|| true` because a configuration that publishes no checksum is not an
    # error here: it simply leaves this allowlist empty.
    published_checksum="$(grep -oE '\b[0-9a-f]{64}\b' .circleci/config.yml | sort -u |
      tr '\n' ',' | sed 's/,$//' || true)"
  fi

  python3 - "$phase" "$paths_mode" "$files_mode" "$summary" "$published_checksum" "$@" <<'PY'
import os
import re
import sys

phase, paths_mode, files_mode, summary_path = sys.argv[1:5]
published = [value for value in sys.argv[5].split(",") if value]
roots = sys.argv[6:]

# High-signal credential signatures. Only the NAME of a rule is ever printed,
# never its pattern: the evidence of this scan is itself published, and a
# published pattern would be re-read by the next run's scan of this file.
RULES = [
    ("aws-access-key-id", re.compile(r"\b(?:AKIA|ASIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA)[0-9A-Z]{16}\b")),
    ("private-key-block", re.compile(r"-----BEGIN (?:[A-Z]+ )*PRIVATE KEY-----")),
    ("github-token", re.compile(r"\bgh[pousr]_[A-Za-z0-9]{36,}\b")),
    ("slack-token", re.compile(r"\bxox[abprs]-[A-Za-z0-9-]{10,}\b")),
    ("google-api-key", re.compile(r"\bAIza[0-9A-Za-z_\-]{35}\b")),
    ("json-web-token",
     re.compile(r"\beyJ[A-Za-z0-9_\-]{8,}\.[A-Za-z0-9_\-]{8,}\.[A-Za-z0-9_\-]{4,}")),
    # No word boundary before the keyword: a credential is routinely named by
    # a longer identifier - `aws_secret_access_key`, `sonatypePassword` - and
    # anchoring the start would miss every one of them.
    #
    # The keyword and the value may each be quoted, because two of the four
    # trees this scans are JSON and the fifth artifact is YAML-shaped:
    # `password=hunter2`, `"password": "hunter2"`, `'token' : 'hunter2'` and
    # `{"aws_secret_access_key":"hunter2"}` are the same disclosure and all
    # four must match. The value is captured on its own, so the allowlists
    # below compare the VALUE rather than the whole match.
    ("credential-assignment",
     re.compile(r"(?i)(?:pass(?:word|wd)|secret|token|api[_-]?key|access[_-]?key"
                r"|private[_-]?key|credentials?|authorization)"
                r"[\"']?[ \t]*[:=][ \t]*[\"']?"
                r"(?P<secret>[^\s\"',;)}\]]{6,})")),
    ("url-embedded-credentials",
     re.compile(r"\b[a-z][a-z0-9+.\-]*://[^/\s:@]+:(?P<secret>[^/\s:@]+)@")),
]

# The two allowlists, both explicit and both EXACT: the value must BE the
# published checksum, or BE a hexadecimal digest on a line that says it is
# one. A value that merely contains one - `password=<checksum>-suffix` - is
# not the checksum and is not excused.
HEX = re.compile(r"[0-9a-fA-F]{32,}")
DIGEST_CONTEXT = re.compile(
    r"(?i)(?:digest|sha1|sha256|sha512|md5|checksum|commit|hash)")

files = 0
lines_scanned = 0
bytes_scanned = 0
findings = []
allowed = []
errors = []
missing = []
symlinks = []


def describe(match):
    """What may be said about a match in a report that is itself published.

    Neither the value nor the text around it: a secret quoted into the
    evidence is a secret published, and a quoted line would also be re-read
    by the next run's scan of this very file and reported all over again. The
    path and the line number locate it for whoever has to act on it.
    """
    start, end = match.span()
    return f"matched {end - start} characters at column {start + 1} (value withheld)"


def scan_file(path):
    global files, lines_scanned, bytes_scanned
    try:
        with open(path, "rb") as handle:
            payload = handle.read()
    except OSError as error:
        errors.append(f"{path}: {error}")
        return
    files += 1
    bytes_scanned += len(payload)
    # Binary-safe by construction: latin-1 maps every byte to a character, so
    # no artifact can be skipped for not being text, and none can crash this.
    text = payload.decode("latin-1")
    for number, line in enumerate(text.splitlines(), start=1):
        lines_scanned += 1
        for name, pattern in RULES:
            for match in pattern.finditer(line):
                # The value the rule found, where the rule isolates one, and
                # the whole match where it does not. Quotes and the
                # separators of the surrounding document are not part of a
                # value, so they are stripped before it is compared.
                value = (match.groupdict().get("secret") or match.group(0)).strip("\"' \t,;")
                if value and value in published:
                    allowed.append(
                        f"{path}:{number} rule={name} reason=the sbt distribution checksum "
                        "that .circleci/config.yml publishes")
                    continue
                if HEX.fullmatch(value) and DIGEST_CONTEXT.search(line):
                    allowed.append(
                        f"{path}:{number} rule={name} reason=a hexadecimal digest on a line "
                        "that names it as one")
                    continue
                findings.append(f"{path}:{number} rule={name} {describe(match)}")


def walk(root):
    """Walks one publication tree without following a single symlink.

    A symlink inside an artifact tree is refused rather than resolved: a link
    to a file outside the tree would have its target's content published by
    `store_artifacts`, and a link to a directory can also close a loop. None
    of the four trees is built with one, so every symlink found is recorded
    and fails the scan - which is what fail-closed traversal means here.
    """
    for directory, names, files_here in os.walk(
            root, onerror=lambda error: errors.append(str(error)), followlinks=False):
        for name in sorted(names):
            candidate = os.path.join(directory, name)
            if os.path.islink(candidate):
                symlinks.append(f"{candidate} -> (directory symlink, not followed)")
        for name in sorted(files_here):
            candidate = os.path.join(directory, name)
            if os.path.islink(candidate):
                symlinks.append(f"{candidate} -> (file symlink, not read)")
                continue
            scan_file(candidate)


for root in roots:
    if os.path.islink(root):
        symlinks.append(f"{root} -> (publication path is itself a symlink)")
        continue
    if not os.path.exists(root):
        missing.append(root)
        continue
    if os.path.isfile(root):
        scan_file(root)
        continue
    walk(root)

report = [
    f"phase                    : {phase} ({paths_mode}, {files_mode})",
    f"publication paths scanned: {len(roots) - len(missing) - len(symlinks)} of {len(roots)}",
    f"  not present            : {', '.join(missing) if missing else 'none'}",
    f"  files                  : {files}",
    f"  lines                  : {lines_scanned}",
    f"  bytes                  : {bytes_scanned}",
    f"rules applied            : {', '.join(name for name, _ in RULES)}",
    f"allowlisted matches      : {len(allowed)}",
    f"findings                 : {len(findings)}",
    f"read errors              : {len(errors)}",
    f"symlinks refused         : {len(symlinks)}",
]
for entry in allowed:
    report.append(f"  ALLOWED {entry}")
for entry in errors:
    report.append(f"  ERROR {entry}")
for entry in symlinks:
    report.append(f"  SYMLINK {entry}")
for entry in findings:
    report.append(f"  FINDING {entry}")

with open(summary_path, "w", encoding="utf-8") as handle:
    handle.write("\n".join(report) + "\n")
print("\n".join(report))

# Fail-closed: an unreadable artifact, or a symlink that was refused rather
# than read, is a gap in the scan and not a pass. What "missing" and "empty"
# mean is the caller's to state, because a pre-run scan of a clean tree
# legitimately finds nothing while the boundary row must find all four trees.
if errors:
    print(f"{len(errors)} publication artifact(s) could not be read")
    sys.exit(1)
if symlinks:
    print(f"{len(symlinks)} symlink(s) inside the publication trees were refused, so their "
          "content is unscanned")
    sys.exit(1)
if missing and paths_mode == "require-paths":
    print(f"a publication path this phase requires is missing: {', '.join(missing)}")
    sys.exit(1)
if findings:
    print(f"{len(findings)} credential signature(s) in the artifacts CI publishes")
    sys.exit(1)
if files < 1 and files_mode == "require-files":
    print("no artifact was scanned, so the scan measured nothing")
    sys.exit(1)
PY
}

row_20_repository_boundary() {
  new_evidence repository-boundary.txt
  local failed=0
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
    failed=1
  elif [[ -n "$status" ]]; then
    detail "$(printf '%s\n' "$status" | wc -l | tr -d ' ') path(s) changed outside the sbt build"
    failed=1
  fi

  # -- the publication-artifact secret scan --------------------------------
  #
  # The material of the report that has not been written yet: the rows
  # themselves. Written where the scan will read it, and left behind as
  # evidence of what was scanned.
  local material="$AUDIT_DIR/gate-report-material.txt"
  local index
  # The counts below are the tallies of the recorded verdicts, recomputed here
  # so that the material names this row's own predecessors rather than a total
  # left over from an earlier row.
  gate_counts || true
  {
    printf '# everything the gate report is composed from that is not already on disk:\n'
    printf '# the header fields, the rows, and the counts its result line is derived from.\n'
    printf '# (Its appendices are %s, inside the audit tree this scan walks.)\n' \
      "${APPENDIX_FILE#"$ROOT"/}"
    printf 'script\t%s\n' "$SCRIPT_NAME"
    printf 'root\t%s\n' "$ROOT"
    printf 'jdk\t%s\n' "${JDK_VERSION:-unknown}"
    printf 'sbt\t%s\n' "${SBT_VERSION:-unknown}"
    printf 'automated\t%s of %s expected\n' "$GATE_COUNT_AUTOMATED" "$GATE_EXPECTED_AUTOMATED"
    printf 'failed\t%s\n' "$GATE_COUNT_FAILED"
    printf 'reported\t%s\n' "$GATE_COUNT_REPORTED"
    printf 'run completed\t%s\n' "$RUN_COMPLETED"
    for index in "${!GATE_LABEL[@]}"; do
      printf '%s\t%s\t%s\t%s\n' "${GATE_LABEL[$index]}" "${GATE_STATUS[$index]}" \
        "${GATE_DETAIL[$index]}" "${GATE_EVIDENCE[$index]}"
    done
  } >"$material"

  # The three trees this run produces must be there to be scanned. The fourth
  # publication path, the gate report, is composed after the last row, so its
  # absence here is expected and its content is covered twice over: by the
  # material file above, and by the scan of the composed report itself once
  # this row has returned.
  local tree
  for tree in "${PUBLICATION_PATHS[@]:1}"; do
    if [[ ! -d "$tree" ]]; then
      detail "the publication tree $tree does not exist, so its content cannot be scanned"
      printf '# MISSING publication tree: %s\n' "$tree" >>"$EV"
      failed=1
    fi
  done

  # The ENFORCED invocation, over all four publication paths.
  local scan_summary="$AUDIT_DIR/publication-secret-scan.txt"
  local scan_rc=0
  printf '\n## Publication-artifact secret scan (fail-closed)\n' >>"$EV"
  scan_publication_artifacts publication allow-missing require-files "$scan_summary" \
    "${PUBLICATION_PATHS[@]}" >>"$EV" 2>&1 || scan_rc=$?
  if [[ "$scan_rc" -ne 0 ]]; then
    detail "the publication-artifact secret scan did not pass (see ${scan_summary#"$ROOT"/})"
    failed=1
  fi
  if [[ -f "$scan_summary" ]]; then
    add_appendix "Repository boundary - publication-artifact secret scan" <"$scan_summary"
  else
    detail "the publication-artifact secret scan wrote no summary"
    failed=1
  fi

  if [[ "$failed" -eq 0 ]]; then
    detail "modules, examples, eclipse, pom.xml, src and .github are unchanged; $(awk -F': *' '/^  files /{ print $2 }' "$scan_summary") published artifact(s) carry no credential signature"
  fi
  return "$failed"
}

#-----------------------------------------------------------------------------
# The report.
#
# Written on every exit path through the EXIT trap, because CI publishes it
# with `when: always` and it is the deliverable evidence. Deterministic except
# for the one timestamp in its header, which no gate compares.
#
# It is built in a temporary file beside the final one, verified, and then
# renamed - a rename within one directory is atomic, so a reader either sees
# the previous report or a complete new one, never half of either. `REPORT_
# WRITTEN` is set only after that rename succeeds: marking the report written
# before the write completes is what would let an interruption leave a partial
# file that the EXIT trap then declines to replace.
#
# Every count in it is tallied from the recorded verdicts by `gate_counts`.
# Nothing is derived by subtraction, which is how a preflight failure - a row
# that is blocking but is not one of the nineteen measured rows - used to
# produce "0 total, -1 passed, 1 failed".
#-----------------------------------------------------------------------------

write_report() {
  if [[ "$REPORT_WRITTEN" == "yes" ]]; then
    return 0
  fi

  gate_counts || true
  local index
  local temp_file="$REPORT_FILE.partial"
  local rc=0

  if ! safe_truncate "$temp_file"; then
    printf 'WARNING: the gate report cannot be written to %s\n' \
      "${REPORT_FILE#"$ROOT"/}" >&2
    return 1
  fi

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
    # The cell separator is escaped with a parameter expansion rather than a
    # pipe through `sed`: the report is the one thing that must still be
    # producible when something about the environment is broken, so its own
    # assembly depends on no external tool.
    for index in "${!GATE_LABEL[@]}"; do
      printf '| %s | %s | %s | %s | `%s` |\n' \
        "$((index + 1))" \
        "${GATE_LABEL[$index]//|/\\|}" \
        "${GATE_STATUS[$index]}" \
        "${GATE_DETAIL[$index]//|/\\|}" \
        "${GATE_EVIDENCE[$index]}"
    done

    printf '\nGATES: %s total, %s passed, %s failed (automated rows; plus %s reported row)\n' \
      "$GATE_COUNT_AUTOMATED" "$GATE_COUNT_PASSED" "$GATE_COUNT_FAILED" "$GATE_COUNT_REPORTED"
    if [[ "$GATE_COUNT_PREFLIGHT_FAILED" -gt 0 ]]; then
      printf 'PREFLIGHT: FAILED - the required toolchain was incomplete, so none of the\n'
      printf '%s automated rows ran. The row above says which tools were missing.\n' \
        "$GATE_EXPECTED_AUTOMATED"
    fi
    if [[ -n "$GATE_ARRAY_PROBLEM" ]]; then
      printf 'BOOKKEEPING: %s - the counts above cannot be trusted.\n' "$GATE_ARRAY_PROBLEM"
    fi
    local unattributed=$(($(framework_error_count) - FRAMEWORK_ERRORS_ATTRIBUTED))
    if [[ "$unattributed" -gt 0 ]]; then
      printf 'FRAMEWORK: %s unchecked command failure(s) outside any row (see `%s`).\n' \
        "$unattributed" "${FRAMEWORK_ERROR_FILE#"$ROOT"/}"
    fi

    if [[ "$GATE_COUNT_PREFLIGHT_FAILED" -gt 0 ]]; then
      printf '\nRESULT: INCOMPLETE - preflight failed before any automated row could run,\n'
      printf 'so nothing was measured. This is not an acceptance result.\n'
    elif [[ "$RUN_COMPLETED" != "yes" ]]; then
      printf '\nRESULT: INCOMPLETE - the run ended after %s of the %s automated rows, so this\n' \
        "$GATE_COUNT_AUTOMATED" "$GATE_EXPECTED_AUTOMATED"
      printf 'is not an acceptance result. %s of the rows that did run failed.\n' \
        "$GATE_COUNT_FAILED"
    elif [[ "$GATE_FAILED" -eq 0 ]]; then
      printf '\nRESULT: every automated gate passed.\n'
    else
      printf '\nRESULT: %s automated gate(s) failed. This delivery is not acceptable yet.\n' \
        "$GATE_COUNT_FAILED"
      if [[ "$GATE_FAILED" -gt "$GATE_COUNT_FAILED" ]]; then
        printf 'A further %s blocking failure(s) were recorded outside the rows themselves;\n' \
          "$((GATE_FAILED - GATE_COUNT_FAILED))"
        printf 'the lines above this result name them.\n'
      fi
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
    printf -- '- `target/test-reports/` - the per-suite JUnit XML (`TEST-*.xml`) written\n'
    printf '  by the single ScalaTest `-u` reporter the build configures\n'
    printf -- '- `target/audit/` - per-row evidence, sbt logs, class-load logs and the\n'
    printf '  snapshots the late rows read\n'
  } >"$temp_file" || rc=$?

  if [[ "$rc" -ne 0 || ! -s "$temp_file" ]]; then
    printf 'WARNING: the gate report could not be assembled (status %s); %s is\n' \
      "$rc" "${REPORT_FILE#"$ROOT"/}" >&2
    printf 'left as it was, and this run will try again on its way out.\n' >&2
    # Best effort, and deliberately not a checked command: the failure that
    # matters has already been reported above.
    command rm -f -- "$temp_file" 2>/dev/null || true
    return 1
  fi

  # The publication step: a rename inside one directory is atomic, so no
  # reader ever sees a half-written report, and an interrupted attempt leaves
  # only the temporary file behind.
  if ! mv -f -- "$temp_file" "$REPORT_FILE"; then
    # The rename is the only publication step there is. Copying the assembly
    # over the deliverable instead would leave exactly the half-written
    # target/gate-report.md that renaming exists to rule out, so the previous
    # report is left untouched, the complete assembly is kept, and a copy of
    # it is placed under target/audit - which CI publishes as a directory -
    # so the evidence is still reachable. REPORT_WRITTEN stays unset, so a
    # later attempt on the way out may still publish properly.
    printf 'WARNING: the assembled gate report could not be renamed onto %s.\n' \
      "${REPORT_FILE#"$ROOT"/}" >&2
    printf 'The previous report is untouched and the complete assembly is kept at\n' >&2
    printf '%s; it is NOT published.\n' "${temp_file#"$ROOT"/}" >&2
    local unpublished="$AUDIT_DIR/gate-report-unpublished.md"
    if copy_through_shell "$temp_file" "$unpublished"; then
      printf 'A copy is also under %s for the CI artifacts.\n' \
        "${unpublished#"$ROOT"/}" >&2
    fi
    return 1
  fi

  # Only now: the report on disk is complete, so a later attempt has nothing
  # to add. Until this point REPORT_WRITTEN stays "no", which is what lets the
  # EXIT trap replace a report an interruption cut short.
  REPORT_WRITTEN="yes"
  printf '\nreport written: %s\n' "${REPORT_FILE#"$ROOT"/}"
  return 0
}

on_exit() {
  local status=$?
  # The handler must not be able to fail the run it is reporting on, so both
  # `errexit` and `nounset` are relaxed for the duration of the report, and
  # `write_report`'s own status is deliberately discarded: it has already said
  # what went wrong on stderr.
  set +e
  set +u
  write_report || true
  return "$status"
}

# Installing the traps is a side effect, so it happens in `init_run` and not
# when this file loads: sourcing it must not replace the caller's handlers.
#
# An interrupted run still has to leave the report behind, and bash only
# guarantees the EXIT trap for a signal it handles, so the two signals a CI
# timeout or an operator sends are handled explicitly. Each exits with the
# conventional 128 + signal status, which runs the EXIT trap.
install_traps() {
  trap on_exit EXIT
  trap 'printf "\ninterrupted (SIGINT): writing the partial report\n" >&2; exit 130' INT
  trap 'printf "\nterminated (SIGTERM): writing the partial report\n" >&2; exit 143' TERM
}

#-----------------------------------------------------------------------------
# Main: every row of AAP section 0.10.1, in that table's order.
#-----------------------------------------------------------------------------

main() {
  # Every side effect of a real run - the shell options, the exports, the
  # argument contract, the working directory, the output tree and the traps -
  # happens here and nowhere else.
  init_run "$@"

  printf '%s\n' '============================================================'
  printf 'acceptance gates for the Scala port of strata-collect/strata-basics\n'
  printf 'repository root: %s\n' "$ROOT"
  printf '%s\n' '============================================================'

  preflight

  # Before the first row: whatever a previous run left in the publication
  # trees is already publishable, because CI stores all four with
  # `when: always` - including from a run that ends early. Scanning them here
  # means an abort at any later point has had its inputs checked once, and a
  # finding is raised before any row has had the chance to add to them. On a
  # clean tree there is nothing to find and nothing to fail: this phase
  # allows both a missing tree and an empty one.
  #
  # What this does not cover, stated rather than left implicit: a run that
  # ABORTS between here and the boundary row leaves behind the evidence the
  # rows that did run had written, and that evidence is published without
  # having been scanned in this run. It is scanned by the pre-run scan of the
  # next run, before anything is added to it. Closing the window itself means
  # scanning from the two exit paths that write the report - the preflight
  # failure and the EXIT trap - which is a change to the runner's own exit
  # handling rather than to a gate row.
  local pre_scan_rc=0
  scan_publication_artifacts pre-run allow-missing allow-empty \
    "$AUDIT_DIR/publication-secret-scan-pre-run.txt" "${PUBLICATION_PATHS[@]}" ||
    pre_scan_rc=$?
  if [[ "$pre_scan_rc" -ne 0 ]]; then
    printf 'FATAL: the publication trees left by an earlier run carry a credential\n' >&2
    printf 'signature, a symlink or an unreadable file. CI publishes them with\n' >&2
    printf '`when: always`, so this run stops before adding to them. See\n' >&2
    printf '%s\n' "${AUDIT_DIR#"$ROOT"/}/publication-secret-scan-pre-run.txt" >&2
    GATE_LABEL+=("Preflight - publication trees of the previous run")
    GATE_STATUS+=("FAIL")
    GATE_DETAIL+=("an earlier run's publication artifacts did not pass the secret scan")
    GATE_EVIDENCE+=("${AUDIT_DIR#"$ROOT"/}/publication-secret-scan-pre-run.txt")
    GATE_TOTAL=$((GATE_TOTAL + 1))
    GATE_FAILED=$((GATE_FAILED + 1))
    write_report
    exit 2
  fi

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
  # mergify's `#approved-reviews-by>=1` condition.
  #
  # The wording is FIXED. The specification states what this row says -
  # "automated checks passed; manual approval: see PR review" - and it says it
  # verbatim, on every run. Substituting a different sentence when the
  # automated half failed would make this row's text depend on another row's
  # verdict, so a reader could no longer rely on finding the prescribed line,
  # and a tool that looks for it would not find it. Nothing is concealed by
  # that: the automated half is its own PASS/FAIL row immediately above, it is
  # counted in the summary line and in the exit status, and the evidence file
  # this row points at is that row's evidence.
  #
  # The evidence path is read positionally - the row that has just run is the
  # last element of each array - so that inserting a row above cannot silently
  # point this one at a different row's evidence.
  local last=$((${#GATE_STATUS[@]} - 1))
  record_reported_row "Gate 7 - migration note (manual approval)" "$GATE7_MANUAL_TEXT" \
    "${GATE_EVIDENCE[$last]:-none}"

  run_gate "Test scope >= Java" row_19_test_scope
  run_gate "Repository boundary" row_20_repository_boundary

  # Every row of the table has now run, so the report may state a result.
  # A report written before this point - by the EXIT trap after a failure or
  # an interruption - says how far the run got instead.
  if ! gate_counts; then
    printf 'FATAL: %s\n' "$GATE_ARRAY_PROBLEM" >&2
    GATE_FAILED=$((GATE_FAILED + 1))
  fi
  if [[ "$GATE_COUNT_AUTOMATED" -ne "$GATE_EXPECTED_AUTOMATED" ]]; then
    printf 'FATAL: %s automated rows ran, expected %s. The row list and\n' \
      "$GATE_COUNT_AUTOMATED" "$GATE_EXPECTED_AUTOMATED" >&2
    printf 'GATE_EXPECTED_AUTOMATED disagree - this script is misconfigured.\n' >&2
    GATE_FAILED=$((GATE_FAILED + 1))
  else
    RUN_COMPLETED="yes"
  fi

  # Anything the checked-command layer recorded that no row accounted for
  # happened outside a row - in the framework itself - and is blocking too: a
  # snapshot, an appendix or an evidence file that failed to be written makes
  # the published evidence incomplete.
  local recorded_errors unattributed_errors
  recorded_errors="$(framework_error_count)"
  unattributed_errors=$((recorded_errors - FRAMEWORK_ERRORS_ATTRIBUTED))
  if [[ "$unattributed_errors" -gt 0 ]]; then
    printf 'FATAL: %s unchecked command failure(s) outside any gate row; see %s\n' \
      "$unattributed_errors" "${FRAMEWORK_ERROR_FILE#"$ROOT"/}" >&2
    GATE_FAILED=$((GATE_FAILED + 1))
    RUN_COMPLETED="no"
  fi

  # Publishing the report is itself a step that can fail, and its failure is
  # recorded after the tally above - so the tally is taken again afterwards.
  # Without this, a report that could not be published, or a write that failed
  # while publishing it, would leave the run exiting 0.
  if ! write_report; then
    printf 'FATAL: the gate report could not be published; see the warnings above.\n' >&2
    GATE_FAILED=$((GATE_FAILED + 1))
  fi
  recorded_errors="$(framework_error_count)"
  unattributed_errors=$((recorded_errors - FRAMEWORK_ERRORS_ATTRIBUTED))
  if [[ "$unattributed_errors" -gt 0 && "$RUN_COMPLETED" == "yes" ]]; then
    printf 'FATAL: %s unchecked command failure(s) recorded while publishing the\n' \
      "$unattributed_errors" >&2
    printf 'report; see %s\n' "${FRAMEWORK_ERROR_FILE#"$ROOT"/}" >&2
    GATE_FAILED=$((GATE_FAILED + 1))
    RUN_COMPLETED="no"
  fi

  # The composed report is the one publication artifact that did not exist
  # while the rows were running, so it is scanned now that it does. Its
  # content came from material the boundary row already scanned; this reads
  # the actual bytes CI will publish, header and result line included.
  #
  # It runs after the report is written, so a finding here cannot be written
  # INTO the report: it is stated on stderr, recorded in its own summary
  # under the audit tree - which CI publishes too - and counted, so the run
  # exits non-zero and nobody reads the report as an acceptance.
  local report_scan_rc=0
  scan_publication_artifacts composed-report require-paths require-files \
    "$AUDIT_DIR/publication-secret-scan-report.txt" "${PUBLICATION_PATHS[0]}" ||
    report_scan_rc=$?
  if [[ "$report_scan_rc" -ne 0 ]]; then
    printf 'FATAL: the composed gate report did not pass the publication secret scan.\n' >&2
    printf 'See %s\n' "${AUDIT_DIR#"$ROOT"/}/publication-secret-scan-report.txt" >&2
    GATE_FAILED=$((GATE_FAILED + 1))
  fi

  printf '\n%s\n' '------------------------------------------------------------'
  printf 'GATES: %s total, %s passed, %s failed (automated rows; plus %s reported row)\n' \
    "$GATE_COUNT_AUTOMATED" "$GATE_COUNT_PASSED" "$GATE_COUNT_FAILED" "$GATE_COUNT_REPORTED"
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
# reach - and everything above it is a definition: `main` is what calls
# `init_run`, so sourcing this file reads no argument, changes no directory,
# truncates no artifact, exports nothing and installs no trap. A sourcing
# caller that wants a real run calls `init_run` itself first.
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  main "$@"
fi
